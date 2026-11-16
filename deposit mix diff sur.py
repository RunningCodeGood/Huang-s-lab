"""
Core Deposit Mix – SUR model on first differences of deposit shares
====================================================================
Model (each equation, estimated jointly by SUR feasible GLS, HAC Bartlett 12):

    dY(t) = a + phi * dY(t-1) + sum_k beta_k * x_k(t - L_k) + e(t)
    dY(t) = Y(t) - Y(t-1),  Y in {MMA_pct, NOW_pct, NIB_Comm_pct}  (shares of total deposits)

Steps
  1  Load deposit shares and macro data; build dY
  2  Candidate drivers: monthly change (D1) for rates / spreads / ratios / indices,
     month-over-month and year-over-year % growth (MOM / YOY) for quantities; lags 1, 3, 6, 12
  3  Single-equation OLS screen (HAC 12): <= 1 driver per economic family, |corr| <= 0.7,
     expected signs, |phi| < 1, all driver p < 0.05, Durbin-Watson in [1.5, 2.5];
     out-of-time = recursive forecast of the last 27 months (train through Sep 2023),
     dY(t-1) = own forecast, cumulated to the share level
  4  System screen: best 20 per equation by OOT level RMSE -> 20^3 = 8,000 systems,
     each estimated by SUR-GLS; gates re-checked on the GLS estimates
     (driver p < 0.05, signs, |phi| < 1, DW in [1.5, 2.5])
  5  Selection: best 5 systems by mean OOT level RMSE -> fewest drivers -> highest mean R2
  6  Final SUR-GLS estimation and diagnostics (Breusch-Pagan LM, Hausman OLS vs GLS reported only,
     Breusch-Godfrey reported only)
No seasonal dummies; no Breusch-Godfrey or Hausman gate.

Usage
    python deposit_mix_diff_sur.py --input input_1.xlsx --macro macro.xlsx

Outputs (in --outdir)
    diff_sur_run.log, diff_sur_candidates.csv, diff_sur_systems.csv,
    diff_sur_coefficients.csv, diff_sur_fitted.csv, diff_sur_backtest.csv

Requirements: pandas, numpy, scipy, statsmodels, linearmodels, openpyxl
Runtime: roughly 10-15 minutes (single-equation screen ~175k regressions, 8,000 SUR systems).
"""

import argparse
import itertools
import logging
import os
import re
import warnings

import numpy as np
import pandas as pd
import statsmodels.api as sm
from linearmodels.system import SUR
from scipy.stats import chi2

warnings.filterwarnings("ignore")
log = logging.getLogger("diff_sur")

# =============================================================================
# CONFIGURATION
# =============================================================================
CONFIG = {
    "est_start": "2013-01-31",   # every candidate available (YoY lagged 12 needs 24 months)
    "est_end": "2025-12-31",
    "train_end": "2023-09-30",   # out-of-time: last 27 months (9 CCAR quarters)
    "lags": [1, 3, 6, 12],
    "hac_lags": 12,
    "max_vars": 3,
    "p_max": 0.05,
    "max_abs_corr": 0.70,
    "dw_range": (1.5, 2.5),
    "sys_top_n": 20,             # best N single-equation models per equation feed the system screen
    "top_n_oot": 5,              # selection pool (systems)
    "first_window": 9,
}

DEP = {"d_mma": "MMA_pct", "d_now": "NOW_pct", "d_nib_comm": "NIB_Comm_pct"}

MNEMONIC_MAP = {
    "M_FBBABLLICBQ_B.IUSA": "CI_LOANS", "M_FCASHFLOW_B.IUSA": "CORP_CF", "M_FCBOEVIXQ_B.IUSA": "VIX",
    "M_FETP_B.IUSA": "EMP", "M_FGDP$_B.IUSA": "RGDP", "M_FIP_B.IUSA": "IP", "M_FIRHYBOASQ_B.IUSA": "HY_OAS",
    "M_FLBR_B.IUSA": "UNEMP", "M_FRGT10Y_B.IUSA": "UST10Y", "M_FRGT2Y_B.IUSA": "UST2Y",
    "M_FRTFS_B.IUSA": "RETAIL", "M_FXSLASCILQ_B.IUSA": "SLOOS_CI", "M_FYPDPIQ_B.IUSA": "DPI",
    "M_FRFED_B.IUSA": "FFR", "M_FYPSAVQ_B.IUSA": "SAVE_RATE",
}
LEVEL_VARS = ["FFR", "UST2Y", "UST10Y", "TERM_SPREAD", "HY_OAS", "UNEMP", "SAVE_RATE", "VIX", "SLOOS_CI"]
GROWTH_VARS = ["CI_LOANS", "CORP_CF", "EMP", "RGDP", "IP", "RETAIL", "DPI"]
FAMILY = {"FFR": "rate", "UST2Y": "rate", "UST10Y": "rate", "TERM_SPREAD": "curve", "HY_OAS": "stress",
          "VIX": "stress", "SLOOS_CI": "credit", "CI_LOANS": "credit", "UNEMP": "labor", "EMP": "labor",
          "SAVE_RATE": "income", "DPI": "income", "CORP_CF": "corp", "RGDP": "activity", "IP": "activity",
          "RETAIL": "activity"}
RATES = {"FFR", "UST2Y", "UST10Y"}
# Expected signs on the underlying variable (+1 / -1 enforced; missing = not restricted)
EXPECTED_SIGN = {
    "d_mma": {},
    "d_now": {**{r: +1 for r in RATES}, "TERM_SPREAD": -1},
    "d_nib_comm": {**{r: -1 for r in RATES}, "TERM_SPREAD": +1, "HY_OAS": +1, "VIX": +1, "UNEMP": +1,
                   "SLOOS_CI": +1, "SAVE_RATE": +1, "CORP_CF": +1},
}
DESC_PREFIX = "Scenario Consistent Baseline (August 2026): "


# =============================================================================
# STEP 1: DATA
# =============================================================================
def load_deposits(path):
    raw = pd.read_excel(path, sheet_name="deposit_bal_input")
    df = raw[["YYYYMM"] + list(DEP.values())].dropna().copy()
    df["YYYYMM"] = df["YYYYMM"].astype(int)
    df.index = pd.to_datetime(df["YYYYMM"].astype(str), format="%Y%m") + pd.offsets.MonthEnd(0)
    for d, lvl in DEP.items():
        df[d] = df[lvl].diff()
    return df


def load_macro(path):
    m = pd.read_excel(path, header=None)
    codes = m.iloc[0, 1:].tolist()
    mac = m.iloc[2:, :].copy()
    mac.columns = ["date"] + codes
    mac["date"] = pd.to_datetime(mac["date"]) + pd.offsets.MonthEnd(0)
    mac = mac.set_index("date").astype(float).rename(columns=MNEMONIC_MAP)
    mac["TERM_SPREAD"] = mac["UST10Y"] - mac["UST2Y"]
    return mac


# =============================================================================
# STEP 2: CANDIDATE DRIVERS
# =============================================================================
def build_candidates(mac):
    base = {}
    for v in LEVEL_VARS:
        base[f"{v}_D1"] = mac[v].diff()
    for v in GROWTH_VARS:
        base[f"{v}_MOM"] = mac[v].pct_change() * 100
        base[f"{v}_YOY"] = mac[v].pct_change(12) * 100
    X = {f"{k}_L{L}": s.shift(L) for k, s in base.items() for L in CONFIG["lags"]}
    return pd.DataFrame(X, index=mac.index)


def root(col):
    """Underlying macro variable of a candidate column, e.g. FFR_D1_L3 -> FFR."""
    return re.sub(r"_(D1|MOM|YOY)_L\d+$", "", col)


# =============================================================================
# HELPERS
# =============================================================================
def durbin_watson(e):
    e = np.asarray(e, float)
    return float(np.sum(np.diff(e) ** 2) / np.sum(e ** 2))


def bg_test(e, Z, p):
    """Breusch-Godfrey LM test (reported only)."""
    e = np.asarray(e, float)
    L = np.column_stack([np.r_[np.zeros(k), e[:-k]] for k in range(1, p + 1)])
    A = np.column_stack([np.asarray(Z, float), L])
    u = e - A @ np.linalg.lstsq(A, e, rcond=None)[0]
    lm = len(e) * (1 - (u @ u) / ((e - e.mean()) @ (e - e.mean())))
    return float(chi2.sf(lm, p))


def recursive_level_forecast(b, Xm, y_level0, dy0):
    """b = [const, phi, betas...]; dY(t-1) is the model's own forecast; returns level path and dY path."""
    lv, dl, prev, lev = [], [], dy0, y_level0
    for t in range(len(Xm)):
        d = b[0] + b[1] * prev + Xm[t] @ b[2:]
        prev, lev = d, lev + d
        dl.append(d)
        lv.append(lev)
    return np.array(lv), np.array(dl)


def design(dep, X, y, combo, start, end):
    i = dep.loc[start:end].index
    Z = pd.DataFrame({"const": 1.0, "DY_LAG1": dep[y].shift(1).loc[i]}, index=i)
    for c in combo:
        Z[c] = X.loc[i, c]
    return Z


def fit_sur(eqs, method="gls", hac=True):
    if hac:
        return SUR(eqs).fit(method=method, cov_type="kernel", kernel="bartlett", bandwidth=CONFIG["hac_lags"])
    return SUR(eqs).fit(method=method, cov_type="unadjusted")


def hausman(eqs):
    o, g = fit_sur(eqs, "ols", hac=False), fit_sur(eqs, "gls", hac=False)
    d = (o.params - g.params).values
    V = (o.cov - g.cov).values
    w = np.linalg.eigvalsh(V)
    k = int((w > 1e-14 * max(1.0, w.max())).sum())
    H = float(d @ np.linalg.pinv(V) @ d)
    return H, k, float(chi2.sf(H, k))


# =============================================================================
# STEP 3: SINGLE-EQUATION SCREEN
# =============================================================================
def screen(dep, X):
    s, e, tr_end = CONFIG["est_start"], CONFIG["est_end"], CONFIG["train_end"]
    idx = dep.loc[s:e].index
    cols = list(X.columns)
    Xs = X.loc[idx, cols]
    assert not Xs.isna().any().any(), "missing candidate values in the estimation window"
    C = np.corrcoef(Xs.values.T)
    pos = {c: i for i, c in enumerate(cols)}
    tr = idx <= tr_end
    te = ~tr
    lo, hi = CONFIG["dw_range"]
    rows, stats = [], {}
    for y, lvl in DEP.items():
        Y = dep.loc[idx, y].values
        ylag = dep[y].shift(1).loc[idx].values
        lev_te = dep.loc[idx[te], lvl].values
        y0, dy0 = dep.loc[tr_end, lvl], dep.loc[tr_end, y]
        n_t = n_s = n_dw = 0
        for k in range(1, CONFIG["max_vars"] + 1):
            for combo in itertools.combinations(cols, k):
                fams = [FAMILY[root(c)] for c in combo]
                if len(set(fams)) < k:
                    continue
                if k > 1 and any(abs(C[pos[a], pos[b]]) > CONFIG["max_abs_corr"]
                                 for a, b in itertools.combinations(combo, 2)):
                    continue
                Xc = Xs[list(combo)].values
                Z = np.column_stack([np.ones(len(Y)), ylag, Xc])
                beta = np.linalg.lstsq(Z, Y, rcond=None)[0]
                if not abs(beta[1]) < 1:
                    continue
                if any(EXPECTED_SIGN[y].get(root(c), 0) and np.sign(beta[2 + i]) != EXPECTED_SIGN[y][root(c)]
                       for i, c in enumerate(combo)):
                    continue
                n_t += 1
                f = sm.OLS(Y, Z).fit(cov_type="HAC", cov_kwds={"maxlags": CONFIG["hac_lags"]})
                if (f.pvalues[2:] >= CONFIG["p_max"]).any():
                    continue
                n_s += 1
                dw = durbin_watson(f.resid)
                if not lo <= dw <= hi:
                    continue
                n_dw += 1
                bt = np.linalg.lstsq(Z[tr], Y[tr], rcond=None)[0]
                lv, _ = recursive_level_forecast(bt, Xc[te], y0, dy0)
                el = lev_te - lv
                rows.append(dict(dep=y, n_vars=k, drivers=" + ".join(combo), combo=combo,
                                 adj_r2=f.rsquared_adj, phi=f.params[1], phi_p=f.pvalues[1],
                                 max_p=f.pvalues[2:].max(), dw=dw,
                                 oot_rmse_lvl=np.sqrt(np.mean(el ** 2)), oot_bias_lvl=el.mean(),
                                 oot_rmse_lvl9=np.sqrt(np.mean(el[:CONFIG["first_window"]] ** 2)),
                                 coefs=", ".join(f"{c}={b:+.6f}" for c, b in zip(combo, f.params[2:]))))
        stats[y] = dict(tested=n_t, significant=n_s, dw_pass=n_dw)
        log.info("Screen %-11s sign/phi-consistent: %d | significant: %d | DW pass: %d", y, n_t, n_s, n_dw)
    return pd.DataFrame(rows), stats


# =============================================================================
# STEP 4: SYSTEM SCREEN (SUR-GLS)
# =============================================================================
def equation_gate(res, y, Z):
    drivers = [n for n in Z.columns if n not in ("const", "DY_LAG1")]
    b, p = res.params, res.pvalues
    if any(p[f"{y}_{n}"] >= CONFIG["p_max"] for n in drivers):
        return "p"
    if any(EXPECTED_SIGN[y].get(root(n), 0) and np.sign(b[f"{y}_{n}"]) != EXPECTED_SIGN[y][root(n)] for n in drivers):
        return "sign"
    if not abs(b[f"{y}_DY_LAG1"]) < 1:
        return "phi"
    lo, hi = CONFIG["dw_range"]
    if not lo <= durbin_watson(res.resids[y].values) <= hi:
        return "dw"
    return None


def system_oot(dep, X, y, combo, params):
    """Recursive level forecast for the OOT window with given coefficients."""
    tr_end, e = CONFIG["train_end"], CONFIG["est_end"]
    te = dep.loc[pd.Timestamp(tr_end) + pd.offsets.MonthEnd(1):e].index
    b = np.r_[params["const"], params["DY_LAG1"], [params[c] for c in combo]]
    lv, dl = recursive_level_forecast(b, X.loc[te, list(combo)].values, dep.loc[tr_end, DEP[y]], dep.loc[tr_end, y])
    el = dep.loc[te, DEP[y]].values - lv
    return dict(rmse=float(np.sqrt(np.mean(el ** 2))), bias=float(el.mean()),
                rmse9=float(np.sqrt(np.mean(el[:CONFIG["first_window"]] ** 2)))), pd.Series(lv, index=te)


def system_screen(dep, X, cands):
    s, e, t = CONFIG["est_start"], CONFIG["est_end"], CONFIG["train_end"]
    pools = {y: cands[cands.dep == y].nsmallest(CONFIG["sys_top_n"], "oot_rmse_lvl") for y in DEP}
    dz = {y: {r.drivers: design(dep, X, y, list(r.combo), s, e) for r in pools[y].itertuples()} for y in DEP}
    fail = dict(p=0, sign=0, phi=0, dw=0)
    rows = []
    for combo in itertools.product(*[list(pools[y].drivers) for y in DEP]):
        eqs = {y: {"dependent": dep.loc[s:e, y], "exog": dz[y][d]} for y, d in zip(DEP, combo)}
        g = fit_sur(eqs)
        why = next((w for w in (equation_gate(g, y, dz[y][d]) for y, d in zip(DEP, combo)) if w), None)
        if why:
            fail[why] += 1
            continue
        gt = fit_sur({y: {"dependent": dep.loc[s:t, y], "exog": dz[y][d].loc[:t]} for y, d in zip(DEP, combo)}, hac=False)
        row = dict(n_vars=sum(len(d.split(" + ")) for d in combo))
        rm, r2 = [], []
        for y, d in zip(DEP, combo):
            names = list(dz[y][d].columns)
            m, _ = system_oot(dep, X, y, d.split(" + "), {n: gt.params[f"{y}_{n}"] for n in names})
            r = g.resids[y].values
            yy = dep.loc[s:e, y]
            r2.append(1 - (r ** 2).sum() / ((yy - yy.mean()) ** 2).sum())
            rm.append(m["rmse"])
            row.update({f"{y}__drivers": d, f"{y}__phi": float(g.params[f"{y}_DY_LAG1"]),
                        f"{y}__dw": durbin_watson(r), f"{y}__r2": r2[-1],
                        f"{y}__oot_rmse": m["rmse"], f"{y}__oot_bias": m["bias"]})
        row["oot_mean"], row["r2_mean"] = float(np.mean(rm)), float(np.mean(r2))
        rows.append(row)
    n_sys = int(np.prod([len(pools[y]) for y in DEP]))
    log.info("System screen: %d systems | first failing gate: %s | pass: %d", n_sys, fail, len(rows))
    return pd.DataFrame(rows), fail, pools


def select_system(sys_df):
    pool = sys_df.nsmallest(CONFIG["top_n_oot"], "oot_mean")
    best = pool.sort_values(["n_vars", "r2_mean"], ascending=[True, False]).iloc[0]
    return {y: best[f"{y}__drivers"].split(" + ") for y in DEP}, pool, best.name


# =============================================================================
# MAIN
# =============================================================================
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", default="input_1.xlsx")
    ap.add_argument("--macro", default="macro.xlsx")
    ap.add_argument("--outdir", default=".")
    args = ap.parse_args()
    os.makedirs(args.outdir, exist_ok=True)
    out = lambda f: os.path.join(args.outdir, f)
    logging.basicConfig(level=logging.INFO, format="%(message)s",
                        handlers=[logging.StreamHandler(), logging.FileHandler(out("diff_sur_run.log"), mode="w")])

    log.info("=== Step 1-2: data and candidates ===")
    dep = load_deposits(args.input)
    mac = load_macro(args.macro)
    X = build_candidates(mac)
    log.info("Deposit months %d, macro months %d, candidate columns %d", len(dep), len(mac), X.shape[1])

    log.info("=== Step 3: single-equation OLS screen ===")
    cands, stats = screen(dep, X)
    cands.drop(columns="combo").to_csv(out("diff_sur_candidates.csv"), index=False)

    log.info("=== Step 4: system screen (SUR-GLS) ===")
    sys_df, fail, pools = system_screen(dep, X, cands)
    if sys_df.empty:
        raise SystemExit("No system passes the gates under SUR-GLS.")

    log.info("=== Step 5: selection ===")
    spec, pool, best_id = select_system(sys_df)
    sys_df.assign(in_pool=sys_df.index.isin(pool.index), selected=sys_df.index == best_id) \
          .sort_values("oot_mean").to_csv(out("diff_sur_systems.csv"))
    log.info("Pool (best %d by mean OOT level RMSE):\n%s", CONFIG["top_n_oot"],
             pool[["n_vars", "oot_mean", "r2_mean"] + [f"{y}__drivers" for y in DEP]].round(4).to_string())
    log.info("Selected: %s", spec)

    log.info("=== Step 6: final SUR-GLS estimation and diagnostics ===")
    s, e, t = CONFIG["est_start"], CONFIG["est_end"], CONFIG["train_end"]
    eqs = {y: {"dependent": dep.loc[s:e, y], "exog": design(dep, X, y, c, s, e)} for y, c in spec.items()}
    res = fit_sur(eqs)
    res_tr = fit_sur({y: {"dependent": dep.loc[s:t, y], "exog": design(dep, X, y, c, s, t)} for y, c in spec.items()}, hac=False)
    log.info("%s", res.summary)

    rc = res.resids.corr()
    T = len(res.resids)
    lm = T * sum(rc.iloc[i, j] ** 2 for i, j in [(0, 1), (0, 2), (1, 2)])
    H = hausman(eqs)
    log.info("Residual correlation:\n%s", rc.round(3))
    log.info("Breusch-Pagan LM = %.2f, p = %.2e", lm, chi2.sf(lm, 3))
    log.info("Hausman (system OLS vs SUR-GLS, reported only): H = %.2f, df = %d, p = %.4f", *H)

    coef_rows, fitted, backtest = [], {}, {}
    for y, c in spec.items():
        Z = eqs[y]["exog"]
        names = list(Z.columns)
        r = res.resids[y].values
        yy = dep.loc[s:e, y]
        phi = res.params[f"{y}_DY_LAG1"]
        m, path = system_oot(dep, X, y, c, {n: res_tr.params[f"{y}_{n}"] for n in names})
        log.info("%-11s R2=%.3f DW=%.2f phi=%.3f BG p(3/6/12)=%.3f/%.3f/%.3f | OOT level RMSE=%.4f bias=%+.4f first9=%.4f",
                 y, 1 - (r ** 2).sum() / ((yy - yy.mean()) ** 2).sum(), durbin_watson(r), phi,
                 *[bg_test(r, Z.values, L) for L in (3, 6, 12)], m["rmse"], m["bias"], m["rmse9"])
        for n in names:
            pn = f"{y}_{n}"
            coef_rows.append(dict(equation=y, variable=n, coef=res.params[pn], se=res.std_errors[pn],
                                  t=res.tstats[pn], p=res.pvalues[pn], coef_train=res_tr.params[pn],
                                  long_run=(res.params[pn] / (1 - phi)) if n not in ("const", "DY_LAG1") else np.nan))
        fitted[f"{y}_actual"] = yy
        fitted[f"{y}_fitted"] = yy - res.resids[y]
        backtest[f"{DEP[y]}_actual"] = dep.loc[path.index, DEP[y]]
        backtest[f"{DEP[y]}_forecast"] = path
    pd.DataFrame(coef_rows).to_csv(out("diff_sur_coefficients.csv"), index=False)
    pd.DataFrame(fitted).to_csv(out("diff_sur_fitted.csv"))
    pd.DataFrame(backtest).to_csv(out("diff_sur_backtest.csv"))
    log.info("Outputs written to %s", os.path.abspath(args.outdir))


if __name__ == "__main__":
    main()
