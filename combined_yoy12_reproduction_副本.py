#!/usr/bin/env python3
"""Compute the statistical analyses in Commercial Card + Small Business Fee.

Combined = Small Business + Commercial Card.
The short-run target is 100 * (Combined[t] / Combined[t-12] - 1).
EC_lag12 is the 12-month lag of the frozen in-sample long-run residual.
All numerical results are estimated from the supplied monthly CSV inputs.
Only CSV result tables and JSON run metadata are written. No report is generated.
"""

# %% Configuration and imports
from pathlib import Path
from datetime import datetime, date
from importlib import metadata
import argparse
import json
import platform
import re
import sys
import traceback
import warnings

import numpy as np
import pandas as pd
from scipy import stats
import statsmodels.api as sm
from statsmodels.tsa.stattools import adfuller, kpss
from statsmodels.stats.stattools import durbin_watson, jarque_bera
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.stats.diagnostic import breaks_cusumolsresid

ROOT = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
DEFAULT_CARD = ROOT / "Cardfee.csv"
DEFAULT_MACRO = ROOT / "Macro_Data_201001-202612.csv"
CARD_HEADER, MACRO_HEADER = 0, 0
CSV_ENCODING = "utf-8-sig"
CSV_SEPARATOR = ","
DATE_COLUMN = "YYYYMM"
CARD, MACRO, OUT = DEFAULT_CARD, DEFAULT_MACRO, None


def runtime_info():
    """Record installed versions for provenance without enforcing an environment."""
    packages = {}
    for name in ("numpy", "pandas", "scipy", "statsmodels"):
        try:
            packages[name] = metadata.version(name)
        except metadata.PackageNotFoundError:
            packages[name] = None
    return dict(python=sys.version, executable=sys.executable,
                platform=platform.platform(), machine=platform.machine(),
                packages=packages)


START, CUT, END = '2011-01-01', '2023-09-01', '2025-12-01'
OOT_START = '2023-10-01'
ALPHA, SCREEN_ALPHA, VIF_LIMIT = 0.05, 0.20, 10.0
MAPPING = {
    'Unrate': 'M_FLBR_B.IUSA', 'CPI': 'M_FCPIU_B.IUSA',
    'DPI': 'M_FYPDPIQ_B.IUSA', 'SBOpt': 'M_FSBINQ_B.IUSA',
    'GDP_Wholesale': 'M_FGDP42Q_B.IUSA', 'IP': 'M_FIP_B.IUSA',
    'CorpProfits': 'M_FZA_B.IUSA', 'DJ_TotalMkt': 'M_FDJMIIUSDD_WCFTDQ_B.IUSA',
}


def save(name, frame):
    frame.to_csv(OUT / f'{name}.csv', index=False, encoding='utf-8-sig', float_format='%.17g')


def fit(d, target, terms):
    z = d[[target] + terms].dropna()
    r = sm.OLS(z[target], sm.add_constant(z[terms], has_constant='add')).fit()
    return r


def diagnostics(r, dw=False, jb=False):
    """Model statistics; residual diagnostics are requested only where used."""
    result = dict(n=int(r.nobs), start=str(r.resid.index.min())[:10],
                  end=str(r.resid.index.max())[:10], R2=r.rsquared,
                  adj_R2=r.rsquared_adj, AIC=r.aic)
    if dw:
        result['DW'] = durbin_watson(r.resid)
    if jb:
        statistic, pvalue, _, _ = jarque_bera(r.resid)
        result.update(JB=statistic, JB_p=pvalue)
    return result


def vifs(r):
    return {name: variance_inflation_factor(r.model.exog, j)
            for j, name in enumerate(r.params.index) if name != 'const'}


def coef(r, name):
    vf = vifs(r)
    return [dict(model=name, term=t, coefficient=r.params[t], SE=r.bse[t],
                 t=r.tvalues[t], p=r.pvalues[t], VIF=vf.get(t, np.nan))
            for t in r.params.index]


def stationarity(x):
    x = x.dropna()
    a = adfuller(x, regression='c', autolag='AIC', result_object=False)
    with warnings.catch_warnings(record=True):
        warnings.simplefilter('always')
        k = kpss(x, regression='c', nlags='auto', result_object=False)
    bound = '<=' if k[1] == .01 else '>=' if k[1] == .1 else '='
    return dict(n=len(x), start=str(x.index.min())[:10], end=str(x.index.max())[:10],
                ADF_stat=a[0], ADF_p=a[1], ADF_lags=a[2], ADF_nobs=a[3],
                KPSS_stat=k[0], KPSS_p=k[1], KPSS_bound=bound, KPSS_lags=k[2],
                OR_pass=bool(a[1] < ALPHA or k[1] > ALPHA))


def stationarity_table(d):
    """Sections 1.1 and 2.2; annual changes start after aligning 2011+ levels."""
    rows = []
    for name in ['Combined'] + list(MAPPING):
        result = stationarity(d.loc[START:CUT, name])
        # This is a levels-screen candidacy decision, not proof of I(1).
        candidate = result['ADF_p'] >= ALPHA and result['KPSS_p'] < ALPHA
        rows.append(dict(section='1.1', window='IS', transform='level', variable=name,
                         level_candidate=bool(candidate), **result))
    for name in MAPPING:
        for window, end, transform in [('IS', CUT, 'yoy12'),
                                       ('IS_plus_OOT', END, 'yoy12_full_sample')]:
            rows.append(dict(section='2.2', window=window, transform=transform,
                             variable=name, **stationarity(d.loc[START:end, name + '_yoy'])))
    return pd.DataFrame(rows)


# %% 1. Input data, calendar alignment, and macro dictionary
MACRO_DESCRIPTIONS = [
    dict(group="Core", variable="Unrate", source_column=MAPPING["Unrate"],
         description="Household Survey: Unemployment Rate, (%, SA)",
         short_run_transform="diff(12), percentage points"),
    dict(group="Core", variable="CPI", source_column=MAPPING["CPI"],
         description="CPI: Urban Consumer - All Items, (Index 1982-84=100, SA)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Core", variable="DPI", source_column=MAPPING["DPI"],
         description="Income: Disposable Personal, (Bil. USD, SAAR)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Small Business", variable="SBOpt", source_column=MAPPING["SBOpt"],
         description="Small business: Normalized optimism index, (Index 1986=100, SA)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Small Business", variable="GDP_Wholesale", source_column=MAPPING["GDP_Wholesale"],
         description="Gross Product Originating: Wholesale Trade, (Bil. USD, SAAR)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="IP", source_column=MAPPING["IP"],
         description="Industrial Production: Total, (Index 2017=100, SA)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="CorpProfits", source_column=MAPPING["CorpProfits"],
         description="Corporate Profits with IVA and CCAdj: Profits After Tax with IVA and CCAdj, (Bil. USD, SAAR)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="DJ_TotalMkt", source_column=MAPPING["DJ_TotalMkt"],
         description="Dow Jones U.S. total stock market index - End of period, (Index, NSA)",
         short_run_transform="pct_change(12) * 100"),
]


def normalize_month(value):
    """Normalize date objects, numeric/string YYYYMM, and ISO dates to month start."""
    if pd.isna(value):
        raise ValueError("YYYYMM contains a missing date.")
    if isinstance(value, (pd.Timestamp, datetime, date, np.datetime64)):
        ts = pd.Timestamp(value)
    else:
        token = str(value).strip()
        if re.fullmatch(r"\d{6}(\.0+)?", token):
            ts = pd.to_datetime(token[:6], format="%Y%m")
        elif re.fullmatch(r"\d{4}[-/]\d{1,2}([-/]\d{1,2})?([ T].*)?", token):
            ts = pd.Timestamp(token)
        else:
            raise ValueError(f"Unsupported YYYYMM value: {value!r}. Expected YYYYMM or an ISO date.")
    return ts.to_period("M").to_timestamp()


def read_monthly(path, header, columns, first_month, last_month):
    """Read monthly CSV data without imputation or duplicate aggregation."""
    path = Path(path)
    if not path.is_file():
        raise FileNotFoundError(f"Input file not found: {path}")
    if path.suffix.lower() != ".csv":
        raise ValueError(f"Expected a CSV file: {path}")
    raw = pd.read_csv(
        path,
        header=header,
        encoding=CSV_ENCODING,
        sep=CSV_SEPARATOR,
        engine="c",
        float_precision="round_trip",
        low_memory=False,
    ).dropna(how="all")
    raw.columns = raw.columns.str.strip()
    if raw.columns.duplicated().any():
        raise ValueError(f"{path.name}: duplicate column names after trimming whitespace.")
    required = [DATE_COLUMN] + list(columns)
    missing = [name for name in required if name not in raw.columns]
    if missing:
        raise ValueError(f"{path.name}: missing columns {missing}; available columns: {list(raw.columns)}")
    chosen = raw[required].copy()
    chosen.index = pd.DatetimeIndex([normalize_month(v) for v in chosen.pop(DATE_COLUMN)], name="date")
    if chosen.index.duplicated().any():
        duplicates = chosen.index[chosen.index.duplicated()].strftime("%Y-%m").tolist()
        raise ValueError(f"{path.name}: duplicate months are not aggregated: {duplicates}")
    was_sorted = chosen.index.is_monotonic_increasing
    chosen = chosen.sort_index()
    months = pd.date_range(first_month, last_month, freq="MS")
    absent = months.difference(chosen.index)
    if len(absent):
        raise ValueError(f"{path.name}: missing required months: {absent.strftime('%Y-%m').tolist()}")
    selected = chosen.loc[months].copy()
    for column in selected:
        values = selected[column]
        if not pd.api.types.is_numeric_dtype(values):
            values = values.astype('string').str.strip()
            # Accept properly grouped thousands, without silently changing malformed numbers.
            grouped = values.str.fullmatch(r'[+-]?\d{1,3}(?:[,\uff0c]\d{3})+(?:\.\d+)?', na=False)
            values = values.where(~grouped, values.str.replace(r'[,\uff0c]', '', regex=True))
        selected[column] = pd.to_numeric(values, errors="raise").astype(float)
    finite = np.isfinite(selected.to_numpy())
    if not finite.all():
        r, c = np.where(~finite)
        examples = [(str(selected.index[i].date()), selected.columns[j]) for i, j in zip(r[:10], c[:10])]
        raise ValueError(f"{path.name}: missing or infinite values at {examples}; no imputation applied.")
    record = dict(file=path.name, format="CSV", encoding=CSV_ENCODING, separator=CSV_SEPARATOR,
                  header_zero_based=header, rows_read=len(raw),
                  rows_used=len(selected), rows_outside_scope=len(raw)-len(selected),
                  first_month=first_month, last_month=last_month, input_sorted=was_sorted,
                  duplicate_months=0, missing_months=0, invalid_required_values=0)
    return selected, record


def load_data():
    c, cq = read_monthly(CARD, CARD_HEADER,
                         ["Commercial Card", "Small Business"], START, END)
    m, mq = read_monthly(MACRO, MACRO_HEADER,
                         list(MAPPING.values()), START, END)
    d = m.rename(columns={v: k for k, v in MAPPING.items()}).copy()
    d["Commercial Card"] = c["Commercial Card"]
    d["Small Business"] = c["Small Business"]
    d["Combined"] = d["Commercial Card"] + d["Small Business"]
    if not (d.loc[START:, "Combined"] > 0).all():
        raise ValueError("Combined must be positive for YoY and MAPE calculations.")
    if (d[[name for name in MAPPING if name != "Unrate"]] == 0).any().any():
        raise ValueError("A macro level used as a percentage-change denominator is zero.")
    # Align to fee history before transformation; no pre-2011 observations are used.
    for name in MAPPING:
        x = d.loc[START:, name]
        d[name + '_yoy'] = x.diff(12) if name == 'Unrate' else x.pct_change(12, fill_method=None) * 100
    d['y'] = d.Combined.pct_change(12, fill_method=None) * 100
    features = {}
    for name in MAPPING:
        for lag in range(13):
            features[f'{name}_lag{lag}'] = d[name + '_yoy'].shift(lag)
    for lag in range(1, 13):
        features[f'y_lag{lag}'] = d.y.shift(lag)
    d = pd.concat([d, pd.DataFrame(features)], axis=1)
    return d, [cq, mq]


def partial(d, candidate):
    """Section 2.3 residual-Pearson screen, retaining its n-2 convention."""
    z = d[['y', 'EC_lag12', candidate]].dropna()
    if len(z) <= 3:
        raise ValueError(f'Not enough complete observations for {candidate}.')
    control = sm.add_constant(z[['EC_lag12']], has_constant='add')
    ry = sm.OLS(z.y, control).fit().resid
    rx = sm.OLS(z[candidate], control).fit().resid
    rho = stats.pearsonr(ry, rx).statistic
    if not np.isfinite(rho):
        raise ValueError(f'Undefined residual correlation for {candidate}.')
    # This is residual-Pearson inference, not the control-adjusted n-3 test.
    dof = len(z) - 2
    t = np.copysign(np.inf, rho) if abs(rho) >= 1 else rho * np.sqrt(dof / (1 - rho * rho))
    return dict(rho=rho, p=2 * stats.t.sf(abs(t), dof), n=len(z), df=dof,
                start=str(z.index.min())[:10], end=str(z.index.max())[:10])


def screen(d):
    names = [f'{m}_lag{l}' for m in MAPPING for l in range(13)]
    rows = []
    for name in names:
        a = partial(d.loc[START:CUT], name)
        b = partial(d.loc[OOT_START:END], name)
        same = np.sign(a['rho']) == np.sign(b['rho'])
        rows.append(dict(candidate=name, family=name.rsplit('_lag', 1)[0], lag=int(name.rsplit('lag', 1)[1]),
                         **{f'{k}_IS': v for k, v in a.items()},
                         **{f'{k}_OOT': v for k, v in b.items()},
                         same_sign=same,
                         passed=bool(a['p'] < SCREEN_ALPHA and b['p'] < SCREEN_ALPHA and same)))
    return pd.DataFrame(rows)


def forward(d, target, candidates, base=(), check_residual_adf=False):
    """Sections 1.2 and 2.4: test every remaining candidate at each round.

    Added-term p < 0.05 and all slope VIFs < 10 are required. The long-run
    step also requires ordinary residual ADF p < 0.05. Rank eligible models
    by adjusted R2, with candidate-specific complete cases and a lexical tie break.
    """
    selected = list(base)
    pending = list(candidates)
    records, rounds = [], []
    for round_no in range(1, len(candidates) + 1):
        trials = []
        for candidate in pending:
            r = fit(d, target, selected + [candidate])
            vf = vifs(r)
            max_p = r.pvalues.drop('const').max()
            eligible = r.pvalues[candidate] < ALPHA and max(vf.values()) < VIF_LIMIT
            rec = dict(round=round_no, candidate=candidate, selected_before=' + '.join(selected),
                       beta_added=r.params[candidate], p_added=r.pvalues[candidate], max_p=max_p,
                       max_VIF=max(vf.values()), all_VIF=json.dumps(vf),
                       eligible=bool(eligible), p_pass=bool(r.pvalues[candidate] < ALPHA),
                       VIF_pass=bool(max(vf.values()) < VIF_LIMIT),
                       **diagnostics(r))
            if check_residual_adf:
                rec['residual_ADF_p'] = adfuller(r.resid, regression='c', autolag='AIC', result_object=False)[1]
                rec['residual_gate_pass'] = bool(rec['residual_ADF_p'] < ALPHA)
                rec['eligible'] = rec['eligible'] and rec['residual_gate_pass']
            trials.append(rec)
        good = [r for r in trials if r['eligible']]
        winner = sorted(good, key=lambda r: (-r['adj_R2'], r['candidate']))[0] if good else None
        for rec in trials:
            rec['chosen'] = bool(winner and rec['candidate'] == winner['candidate'])
            rec['reject_reason'] = ('selected' if rec['chosen'] else
                                    'p>=0.05; VIF fails' if not rec['p_pass'] and not rec['VIF_pass'] else
                                    'p>=0.05' if not rec['p_pass'] else
                                    'VIF fails' if not rec['VIF_pass'] else
                                    'residual stationarity gate fails' if not rec.get('residual_gate_pass', True) else
                                    'eligible but lower adjusted R2')
        records.extend(trials)
        rounds.append(dict(round=round_no, added=winner['candidate'] if winner else '(stop)',
                           adj_R2=winner['adj_R2'] if winner else np.nan,
                           p_added=winner['p_added'] if winner else np.nan,
                           n=winner['n'] if winner else np.nan))
        if not winner:
            break
        selected.append(winner['candidate'])
        pending.remove(winner['candidate'])
        if not pending:
            break
    return selected, pd.DataFrame(records), pd.DataFrame(rounds)


def cap_pool(scr, eligible_families):
    """Keep at most two passing lags per family, ranked by absolute OOT rho."""
    z = scr[scr.passed].copy()
    z = z[z.family.isin(eligible_families)]
    z['strength'] = z.rho_OOT.abs()
    z = z.sort_values(['strength','candidate'],ascending=[False,True]).groupby('family',sort=False).head(2)
    return z, z.candidate.tolist() + [f'y_lag{l}' for l in range(1,13)]


def metrics(actual, pred):
    err = np.asarray(pred) - np.asarray(actual)
    return dict(n=len(err), RMSE=np.sqrt(np.mean(err ** 2)), MAPE=np.mean(np.abs(err / np.asarray(actual))) * 100)


def forecast(d, sr, mode):
    if mode not in {'static', 'dynamic_full'}:
        raise ValueError(f'Unknown forecast mode: {mode}')
    levels = d.Combined.copy()
    growth = d.y.copy()
    ec = d.EC.copy()
    dates = d.loc['2023-10-01':END].index
    if mode != 'static':
        levels.loc[dates] = np.nan
        growth.loc[dates] = np.nan
        if mode == 'dynamic_full':
            ec.loc[dates] = np.nan
    rows = []
    for dt in dates:
        old = dt - pd.DateOffset(months=12)
        x = {}
        for name in sr.params.index:
            if name == 'const':
                x[name] = 1.
            elif name == 'EC_lag12':
                x[name] = ec.at[old]
            elif name.startswith('y_lag'):
                x[name] = growth.at[dt - pd.DateOffset(months=int(name[5:]))]
            else:
                x[name] = d.at[dt, name]
        yh = sum(sr.params[name] * x[name] for name in sr.params.index)
        pred = levels.at[old] * (1 + yh / 100)
        rows.append(dict(date=dt, mode=mode, actual=d.at[dt, 'Combined'],
                         y_actual=d.at[dt, 'y'], y_pred=yh, prediction=pred,
                         prior12_level_used=levels.at[old],
                         **{name + '_used': value for name, value in x.items() if name != 'const'}))
        if mode != 'static':
            levels.at[dt] = pred
            growth.at[dt] = yh
            if mode == 'dynamic_full':
                ec.at[dt] = pred - d.at[dt, 'baseline']
    return pd.DataFrame(rows)


# %% 3. Long-run stability and trend tests
def residual_adf_table(model):
    """Section 1.2: ordinary ADF on the fitted long-run residual."""
    a = adfuller(model.resid, regression='c', autolag='AIC', result_object=False)
    return pd.DataFrame([dict(
        section='1.2', test='ordinary residual ADF with constant', n=int(model.nobs),
        ADF_stat=a[0], ADF_p=a[1], ADF_lags=a[2], ADF_nobs=a[3],
        reject_unit_root=bool(a[1] < ALPHA),
    )])


def cusum_table(d, terms):
    """Section 5.1: re-estimate each window and adjust for all coefficients."""
    rows = []
    for label, end in [('IS', CUT), ('full', END)]:
        model = fit(d.loc[START:end], 'Combined', terms)
        ddof = len(model.params)
        statistic, pvalue, _ = breaks_cusumolsresid(model.resid, ddof=ddof)
        rows.append(dict(section='5.1', sample=label, ddof=ddof, stat=statistic,
                         p=pvalue, n=int(model.nobs),
                         start=str(model.resid.index.min())[:10],
                         end=str(model.resid.index.max())[:10],
                         reject_stability=bool(pvalue < ALPHA)))
    return pd.DataFrame(rows)


def chow_table(d, terms):
    """Section 5.2: pooled-versus-split OLS F tests at the four specified dates."""
    full = d.loc[START:END]
    pooled = fit(full, 'Combined', terms)
    rows = []
    for cutoff in ['2023-01-01', '2023-10-01', '2024-01-01', '2024-07-01']:
        pre = full.loc[full.index < cutoff]
        post = full.loc[full.index >= cutoff]
        k, n = len(terms) + 1, len(full)
        if min(len(pre), len(post)) <= k:
            raise ValueError(f'Insufficient observations for the Chow split at {cutoff}.')
        rp, rq = fit(pre, 'Combined', terms), fit(post, 'Combined', terms)
        f = ((pooled.ssr - rp.ssr - rq.ssr) / k) / ((rp.ssr + rq.ssr) / (n - 2 * k))
        rows.append(dict(section='5.2', break_date=cutoff, F=f,
                         p=stats.f.sf(f, k, n - 2 * k),
                         n_pre=len(pre), n_post=len(post), SSR_pooled=pooled.ssr,
                         SSR_pre=rp.ssr, SSR_post=rq.ssr, df_num=k, df_den=n - 2 * k))
    return pd.DataFrame(rows)


def rolling_coefficients(d, terms):
    """Section 5.3: exactly 30 consecutive months, including the endpoint."""
    full = d.loc[START:END]
    checkpoints = {'2022-06', '2023-06', '2023-09', '2024-06', '2025-06', '2025-12'}
    rows = []
    for i in range(29, len(full)):
        frame = full.iloc[i - 29:i + 1]
        model = fit(frame, 'Combined', terms)
        rows.append(dict(section='5.3', window_start=frame.index[0],
                         window_end=frame.index[-1], n=len(frame),
                         checkpoint=frame.index[-1].strftime('%Y-%m') in checkpoints,
                         **model.params.to_dict()))
    return pd.DataFrame(rows)


def trend_tests(d):
    """Sections 6.1 and 6.2: full-OOT trend and first-18/last-9 validation."""
    oot = d.loc[OOT_START:END]
    design = pd.DataFrame({'const': 1., 'month': np.arange(len(oot), dtype=float)},
                          index=oot.index)
    rows = []
    for section, label, nfit in [('6.1', 'full_oot', len(oot)), ('6.2', 'first18', 18)]:
        model = sm.OLS(oot.EC.iloc[:nfit], design.iloc[:nfit]).fit()
        row = dict(section=section, model=label, n_fit=nfit,
                   fit_start=str(oot.index[0])[:10], fit_end=str(oot.index[nfit - 1])[:10],
                   const=model.params['const'], slope=model.params['month'],
                   slope_p=model.pvalues['month'], R2=model.rsquared,
                   evaluation_role='retrospective_fit' if label == 'full_oot' else 'heldout')
        if label == 'first18':
            heldout = oot.iloc[nfit:]
            prediction = heldout.baseline + model.predict(design.iloc[nfit:])
            row.update(evaluation_n=len(heldout),
                       evaluation_start=str(heldout.index[0])[:10],
                       evaluation_end=str(heldout.index[-1])[:10],
                       evaluation_MAPE=float(np.mean(np.abs(
                           (prediction - heldout.Combined) / heldout.Combined)) * 100))
        rows.append(row)
    return pd.DataFrame(rows)


# %% 4. Statistical results only
def run_analysis():
    inputs = [dict(role='fee_source', path=str(CARD)),
              dict(role='macro_source', path=str(MACRO))]
    d, input_validation = load_data()

    # Sections 1.1 and 2.2. No first-difference or pre-2011-history experiments.
    st = stationarity_table(d)
    save('stationarity', st)
    raw = st.loc[st['transform'].eq('level')].set_index('variable')
    lr_candidates = [name for name in MAPPING if bool(raw.at[name, 'level_candidate'])]

    # Section 1.2. Selected terms are calculated, never copied from a results table.
    lr_terms, lr_trials, lr_rounds = forward(
        d.loc[START:CUT], 'Combined', lr_candidates, check_residual_adf=True)
    if not lr_terms:
        raise RuntimeError('No long-run candidate satisfies the selection rules.')
    lr = fit(d.loc[START:CUT], 'Combined', lr_terms)
    d['baseline'] = lr.predict(sm.add_constant(d[lr_terms], has_constant='add'))
    d['EC'] = d.Combined - d.baseline
    d['EC_lag12'] = d.EC.shift(12)
    save('long_run_forward_all_trials', lr_trials)
    save('long_run_forward_rounds', lr_rounds)
    save('long_run_residual_adf', residual_adf_table(lr))
    print('Long-run selected terms:', ' + '.join(lr_terms), flush=True)

    ins, oot = d.loc[START:CUT], d.loc[OOT_START:END]
    eligible_families = st.loc[
        st['transform'].eq('yoy12') & st.OR_pass, 'variable'].tolist()

    # Sections 2.3 and 2.4. Retain the existing pairwise n-2 screening convention.
    scr = screen(d)
    scr['stationarity_pass_IS'] = scr.family.isin(eligible_families)
    scr['passed_after_stationarity'] = scr.passed & scr.stationarity_pass_IS
    save('partial_correlations', scr)
    capped, pool = cap_pool(scr, eligible_families)
    save('short_run_capped_pool', capped)
    sr_terms, sr_trials, sr_rounds = forward(ins, 'y', pool, base=['EC_lag12'])
    sr = fit(ins, 'y', sr_terms)
    save('short_run_forward_all_trials', sr_trials)
    save('short_run_forward_rounds', sr_rounds)
    save('coefficients', pd.DataFrame(coef(lr, 'long_run') + coef(sr, 'short_run')))
    save('model_diagnostics', pd.DataFrame([
        dict(section='1.2', model='long_run', **diagnostics(lr)),
        dict(section='2.1', model='short_run_base',
             **diagnostics(fit(ins, 'y', ['EC_lag12']), dw=True)),
        dict(section='2.4', model='short_run_final', **diagnostics(sr, dw=True, jb=True)),
    ]))
    print('Short-run selected terms:', ' + '.join(sr_terms), flush=True)

    # Sections 2.4 and 3. Reconstruct fee levels before calculating RMSE and MAPE.
    backtests = []
    for mode in ['static', 'dynamic_full']:
        prediction = forecast(d, sr, mode)
        backtests.append(dict(section='3', mode=mode,
                              **metrics(prediction.actual, prediction.prediction)))
    dates = sr.resid.index
    fitted_levels = d.Combined.shift(12).loc[dates] * (1 + sr.fittedvalues / 100)
    backtests.append(dict(section='2.4', mode='in_sample',
                          **metrics(d.loc[dates, 'Combined'], fitted_levels)))
    save('backtest_metrics', pd.DataFrame(backtests))

    # Section 4. The 2023 OOT subset contains October-December, not the full year.
    periods = [('IS', ins.EC), ('OOT', oot.EC)]
    periods += [(str(year), frame.EC) for year, frame in oot.groupby(oot.index.year)]
    save('ec_summary', pd.DataFrame([
        dict(section='4', period=label, n=len(x), start=str(x.index.min())[:10],
             end=str(x.index.max())[:10], mean=x.mean(), std_sample=x.std(ddof=1))
        for label, x in periods
    ]))

    # Sections 5 and 6. No alternative windows, degrees of freedom or extra tests.
    save('cusum', cusum_table(d, lr_terms))
    save('chow', chow_table(d, lr_terms))
    save('rolling_30m_coefficients', rolling_coefficients(d, lr_terms))
    save('overlay_trend_metrics', trend_tests(d))

    manifest = dict(
        inputs=inputs, input_validation=input_validation, input_format='CSV',
        output_formats=['csv', 'json'], runtime=runtime_info(), macro_mapping=MAPPING,
        long_run_candidates=lr_candidates, long_run_selected=lr_terms,
        short_run_selected=sr_terms, eligible_macro_families=eligible_families,
        settings=dict(
            start=START, IS_end=CUT, OOT_start=OOT_START, OOT_end=END,
            target='100 * (Combined[t] / Combined[t-12] - 1)',
            EC='Combined - frozen IS long-run fitted level; lag12; no transform',
            unrate='Unrate[t] - Unrate[t-12], in percentage points',
            macro_history='Align levels to 2011-01 onward, then transform; no 2010 history',
            stationarity_windows=['IS', 'IS_plus_OOT'],
            ADF='constant, maxlag=None, autolag=AIC', KPSS='constant, nlags=auto',
            long_run_candidacy='Level ADF p >= 0.05 and KPSS p < 0.05; not proof of I(1)',
            forward='All remaining candidates each round; added p < 0.05; slope VIF < 10; '
                    'rank by adjusted R2; candidate-specific complete cases',
            LR_residual_gate='Ordinary residual ADF p < 0.05',
            residual_ADF_interpretation='Ordinary ADF p-values are not formal Engle-Granger p-values',
            stationarity_filter='IS annual-transform OR rule: ADF p < 0.05 or KPSS p > 0.05',
            partial='Eight families, lag0-lag12; pairwise complete cases; residual Pearson, df=n-2',
            partial_inference='The retained n-2 convention does not adjust for the estimated control',
            family_cap='At most two passing lags per family by absolute OOT rho',
            OOT_role='Used in screening and lag ranking; not an untouched validation sample',
            CUSUM_ddof='Number of estimated long-run coefficients, including the intercept',
            rolling_window='30 months including the endpoint',
            EC_standard_deviation='Sample standard deviation, ddof=1',
            overlay='Full OOT is retrospective; first 18 months fit the trend, last 9 evaluate it',
        ),
    )
    write_json('run_manifest.json', manifest)
    return d, lr, sr, manifest


# %% 5. Entry point
def write_json(name, value):
    (OUT / name).write_text(json.dumps(value, ensure_ascii=False, indent=2), encoding='utf-8')


def main(argv=None):
    """Save statistical result tables and machine-readable run metadata."""
    global CARD, MACRO, OUT
    parser = argparse.ArgumentParser(description='Combined fee YoY12 / EC_lag12 statistical analyses')
    parser.add_argument('--card', type=Path, default=DEFAULT_CARD, help='Fee CSV file')
    parser.add_argument('--macro', type=Path, default=DEFAULT_MACRO, help='Macro CSV file')
    parser.add_argument('--output', type=Path, help='New or empty output directory')
    args = parser.parse_args(argv)
    CARD, MACRO = args.card.expanduser().resolve(), args.macro.expanduser().resolve()
    for path in (CARD, MACRO):
        if not path.is_file():
            raise FileNotFoundError(f'Input file not found: {path}')
    OUT = (
        args.output
        or ROOT / 'outputs' / ('combined_statistics_' + datetime.now().strftime('%Y%m%d_%H%M%S_%f'))
    ).expanduser().resolve()
    if OUT.exists() and (not OUT.is_dir() or any(OUT.iterdir())):
        raise FileExistsError(f'Output path is not an empty directory; refusing to overwrite: {OUT}')
    OUT.mkdir(parents=True, exist_ok=True)
    write_json('run_status.json', dict(status='running', started=datetime.now().astimezone().isoformat()))
    try:
        print(f'Running statistical analyses. Output: {OUT}', flush=True)
        _, _, _, manifest = run_analysis()
        write_json('run_status.json', dict(status='complete', finished=datetime.now().astimezone().isoformat()))
        print(f'Completed. Statistical tables: {OUT}', flush=True)
        return manifest
    except Exception as exc:
        (OUT / 'error.log').write_text(traceback.format_exc(), encoding='utf-8')
        write_json('run_status.json', dict(
            status='failed', error=str(exc), finished=datetime.now().astimezone().isoformat(),
        ))
        raise


if __name__ == '__main__':
    # Interactive kernels supply their own process arguments.
    interactive = 'ipykernel' in sys.modules or hasattr(sys, 'ps1')
    try:
        main([] if interactive else None)
    except (ValueError, RuntimeError, OSError) as exc:
        print(f'ERROR: {exc}', file=sys.stderr)
        if not interactive:
            sys.exit(1)
