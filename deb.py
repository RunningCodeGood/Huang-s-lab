#!/usr/bin/env python3
"""Reproduce the fresh forward-selection study behind the Debit Diagnostics column.

Example:
  python debit_stepwise_research.py --card Cardfee.xlsx \
      --macro Macro_Data_201001-202612.xls --out debit_results

Read the original workbooks without prior scripts, caches, selected variables,
or predetermined results. The YoY route implements only the long-run candidate
screening performed in this study, not a complete EG/ECM workflow. The current
data failed that screen, so no cointegration test or ECM was estimated.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import platform
from pathlib import Path

import numpy as np
import pandas as pd
import scipy
from scipy.stats import chi2
import statsmodels
from statsmodels.regression.linear_model import OLS
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.stats.stattools import durbin_watson, jarque_bera
from statsmodels.tools.tools import add_constant

# Keep the study settings consistent with the completed Diagnostics column.
START, END = "2012-01-01", "2023-09-01"
MONTHLY_MAX_LAG, QUARTERLY_MAX_LAG = 12, 4
MACROS = {
    "Unrate": "M_FLBR_B.IUSA",
    "CPI": "M_FCPIU_B.IUSA",
    "DPI": "M_FYPDPIQ_B.IUSA",
    "RetailSalesTotal": "M_FRT_B.IUSA (Retail Sales Total_Bil.USD,CDASAAR)",
    "ConsumerConfidence": "M_FCBC_B.IUSA(Consumer confidence index)",
    "SBOpt": "M_FSBINQ_B.IUSA",
    "GDP_Wholesale": "M_FGDP42Q_B.IUSA",
    "IP": "M_FIP_B.IUSA",
    "CorpProfits": "M_FZA_B.IUSA",
    "DJ_TotalMkt": "M_FDJMIIUSDD_WCFTDQ_B.IUSA",
    "GrocerySales": "M_FRT4451_B.IUSA",
    "GasolineSales": "M_FRT447_B.IUSA",
    "NonstoreSales": "M_FRT454_B.IUSA",
    "PrivateEmployment": "M_FETP_B.IUSA",
}


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def monthly_index(values: pd.Series) -> pd.DatetimeIndex:
    """Accept Excel dates or YYYYMM numbers; do not guess other date formats."""
    if pd.api.types.is_datetime64_any_dtype(values):
        dates = pd.DatetimeIndex(values)
    else:
        text = values.astype(str).str.replace(r"\.0$", "", regex=True)
        dates = pd.DatetimeIndex(pd.to_datetime(text, format="%Y%m", errors="raise"))
    dates = dates.to_period("M").to_timestamp()
    if dates.hasnans or dates.has_duplicates:
        raise ValueError("Dates contain missing values or duplicate months.")
    return dates


def load_training(card_path: Path, macro_path: Path) -> tuple[pd.DataFrame, list[dict]]:
    # Preserve the original worksheet identifier using ASCII Unicode escapes.
    card = pd.read_excel(card_path, sheet_name="\u6c47\u603b\u6570\u636e")
    revenue = pd.DataFrame(
        {"Debit_raw": pd.to_numeric(card["Debit Card"], errors="raise").to_numpy()},
        index=monthly_index(card["YYYYMM"]),
    )
    macro = pd.read_excel(macro_path, sheet_name="Macro Data", header=1)
    macro.index = monthly_index(macro["YYYYMM"])
    macro = macro[list(MACROS.values())].rename(columns={v: k for k, v in MACROS.items()})
    macro = macro.apply(pd.to_numeric, errors="raise")
    # Exclude 2011 before computing growth rates or lagged inputs.
    train = revenue.join(macro).sort_index().loc[START:END].copy()
    expected = pd.date_range(START, END, freq="MS")
    if not train.index.equals(expected) or not np.isfinite(train.to_numpy()).all():
        raise ValueError("Training revenue or macro data contain missing months, missing values, or nonfinite values.")
    train["Debit"] = train["Debit_raw"].astype(float)
    adjustments = []
    for month in ["2012-03-01", "2012-12-01"]:
        t = pd.Timestamp(month)
        left, right = t - pd.offsets.MonthBegin(), t + pd.offsets.MonthBegin()
        # Use original neighboring values; both months must be in the training window.
        adjusted = (train.loc[left, "Debit_raw"] + train.loc[right, "Debit_raw"]) / 2
        train.loc[t, "Debit"] = adjusted
        adjustments.append({"date": month, "original": float(train.loc[t, "Debit_raw"]),
                            "adjusted": float(adjusted)})
    # Do not adjust 2014, add event dummies, or apply further seasonal adjustment.
    return train, adjustments


def make_design(frame: pd.DataFrame, horizon: int, max_lag: int,
                static: bool = False) -> tuple[pd.DataFrame, list[str]]:
    """Transform, create candidate lags, then fix a common sample for all candidates."""
    data = frame[["Debit", *MACROS]].copy()
    transformed = data if horizon == 0 else 100 * data.pct_change(horizon, fill_method=None)
    if horizon:
        # Unemployment changes use percentage points; other macros use percent growth.
        transformed["Unrate"] = data["Unrate"].diff(horizon)
    y = transformed["Debit"].rename("target")
    candidates = {}
    if not static:
        for lag in range(1, max_lag + 1):
            candidates[f"y_L{lag}"] = y.shift(lag)
    for macro in MACROS:
        for lag in range(1 if static else max_lag + 1):
            candidates[f"{macro}_L{lag}"] = transformed[macro].shift(lag)
    matrix = pd.concat([y, pd.DataFrame(candidates)], axis=1).dropna()
    if len(matrix) < 5 or not np.isfinite(matrix.to_numpy()).all():
        raise ValueError("Insufficient transformed observations or nonfinite growth rates, possibly from zero denominators.")
    return matrix, list(candidates)


def fit_model(frame: pd.DataFrame, terms: list[str]):
    x = add_constant(frame[terms], has_constant="add")
    if len(frame) <= x.shape[1] or np.linalg.matrix_rank(x) != x.shape[1]:
        raise ValueError("The design matrix is rank deficient or has insufficient residual degrees of freedom.")
    if np.var(frame["target"].to_numpy()) == 0:
        raise ValueError("The dependent variable is constant; regression diagnostics are not applicable.")
    return OLS(frame["target"], x, missing="raise").fit()


def diagnostics(model) -> dict:
    jb, jb_p, _, _ = jarque_bera(model.resid)
    vifs = [float(variance_inflation_factor(model.model.exog, i))
            for i in range(1, len(model.params))]
    return {
        "n": int(model.nobs), "R2": float(model.rsquared),
        "adj_R2": float(model.rsquared_adj),
        "DW": float(durbin_watson(model.resid)), "JB": float(jb),
        "JB_p": float(jb_p), "max_VIF": max(vifs, default=1.0),
    }


def entry_gates(p_new: float, dw: float, jb_p: float, max_vif: float) -> dict:
    """Apply only these four entry gates; DW endpoints are inclusive, other limits strict."""
    return {
        "significance": bool(np.isfinite(p_new) and p_new < 0.05),
        "DW": bool(np.isfinite(dw) and 1.5 <= dw <= 2.5),
        "JB": bool(np.isfinite(jb_p) and jb_p > 0.05),
        "VIF": bool(np.isfinite(max_vif) and max_vif < 10),
    }


def verify_fit(model, stats: dict) -> None:
    """Independently verify each candidate's OLS fitted values, DW, and JB."""
    x, y = model.model.exog, model.model.endog
    fitted = x @ np.linalg.lstsq(x, y, rcond=None)[0]
    np.testing.assert_allclose(fitted, model.fittedvalues, rtol=1e-8, atol=1e-7)
    e = y - fitted
    dw = np.sum(np.diff(e) ** 2) / np.sum(e ** 2)
    z = e - e.mean()
    variance = np.mean(z ** 2)
    skew = np.mean(z ** 3) / variance ** 1.5
    kurtosis = np.mean(z ** 4) / variance ** 2
    jb = len(z) / 6 * (skew ** 2 + (kurtosis - 3) ** 2 / 4)
    np.testing.assert_allclose(dw, stats["DW"], rtol=1e-9)
    np.testing.assert_allclose(jb, stats["JB"], rtol=1e-7, atol=1e-8)
    np.testing.assert_allclose(chi2.sf(jb, 2), stats["JB_p"], rtol=1e-7, atol=1e-250)


def forward_selection(frame: pd.DataFrame, pool: list[str], approach: str) -> dict:
    """Start with an intercept; test single additions with no forced AR terms or backward deletion."""
    selected, remaining, trials, rounds = [], list(pool), [], []
    while remaining:
        rows = []
        for term in remaining:
            try:
                model = fit_model(frame, selected + [term])
            except ValueError as exc:
                rows.append(dict(approach=approach, round=len(selected) + 1,
                    candidate=term, existing_terms=";".join(selected), n=len(frame),
                    estimable=False, passes=False, failed_gates="not_estimable", error=str(exc),
                    significance_pass=False, DW_pass=False, JB_pass=False, VIF_pass=False))
                continue
            stats = diagnostics(model)
            p_new = float(model.pvalues[term])
            flags = entry_gates(p_new, stats["DW"], stats["JB_p"], stats["max_VIF"])
            row = dict(approach=approach, round=len(selected) + 1, candidate=term,
                existing_terms=";".join(selected), estimable=True,
                coef=float(model.params[term]), p_new=p_new, **stats,
                **{k + "_pass": v for k, v in flags.items()},
                passes=all(flags.values()), failed_gates=";".join(k for k, v in flags.items() if not v))
            verify_fit(model, stats)
            rows.append(row)
        good = sorted((r for r in rows if r["passes"]),
                      key=lambda r: (r["p_new"], -r["adj_R2"], r["candidate"]))
        winner = good[0]["candidate"] if good else None
        for row in rows:
            row["chosen"] = row["candidate"] == winner
        trials.extend(rows)
        rounds.append(dict(round=len(selected) + 1, tested=len(rows), passing=len(good), added=winner))
        if winner is None:
            break
        selected.append(winner)
        remaining.remove(winner)
    final = fit_model(frame, selected) if selected else None
    first = [r for r in trials if r["round"] == 1]
    result = dict(approach=approach, start=str(frame.index[0].date()), end=str(frame.index[-1].date()),
        n=len(frame), candidate_count=len(pool), selected=selected, rounds=rounds,
        final_model_found=bool(selected),
        status="Selected path model" if selected else "No admissible first addition; no final model",
        first_step_counts={key: sum(r[key] for r in first)
            for key in ["significance_pass", "DW_pass", "JB_pass", "VIF_pass", "passes"]},
        final_diagnostics=diagnostics(final) if final is not None else None,
        final_existing_terms_not_significant=[] if final is None else
            [term for term in selected if final.pvalues[term] >= 0.05],
        trials=trials)
    if final is not None:
        result["coefficients"] = [dict(term=t, coef=float(final.params[t]), p_OLS=float(final.pvalues[t]))
                                  for t in final.params.index]
    if "screen" in approach.lower():
        result["status"] = ("Long-run candidate selected; EG/ECM not implemented in this screening reproduction"
                            if selected else "Stopped before EG/ECM: no admissible long-run candidate")
        result["EG_status"] = "not_performed"
        result["ECM_status"] = "not_estimated"
    return result


def json_safe(value):
    """Serialize nonfinite statistics as null, not nonstandard JSON NaN or Infinity."""
    if isinstance(value, dict):
        return {k: json_safe(v) for k, v in value.items()}
    if isinstance(value, list):
        return [json_safe(v) for v in value]
    if isinstance(value, float) and not np.isfinite(value):
        return None
    return value


def run(card_path: Path, macro_path: Path, output_dir: Path) -> dict:
    card_path, macro_path, output_dir = card_path.resolve(), macro_path.resolve(), output_dir.resolve()
    if output_dir.exists() and any(output_dir.iterdir()):
        raise FileExistsError(f"Output directory is not empty; choose a new directory to preserve existing results: {output_dir}")
    sources = {str(p): sha256(p) for p in [card_path, macro_path]}
    train, adjustments = load_training(card_path, macro_path)
    quarterly = train.resample("QE").last()
    quarterly["Debit"] = train["Debit"].resample("QE").sum(min_count=3)
    configs = [
        ("Level", train, 0, MONTHLY_MAX_LAG, False),
        ("YoY_static_screen", train, 12, 0, True),
        ("MoM", train, 1, MONTHLY_MAX_LAG, False),
        ("QoQ_quarterly", quarterly, 1, QUARTERLY_MAX_LAG, False),
        ("Level_EG_screen_sensitivity", train, 0, 0, True),
    ]
    output_dir.mkdir(parents=True, exist_ok=True)
    results, summary_rows = {}, []
    for label, frame, horizon, max_lag, static in configs:
        matrix, pool = make_design(frame, horizon, max_lag, static)
        result = forward_selection(matrix, pool, label)
        results[label] = result
        matrix.to_csv(output_dir / f"{label}_design_matrix.csv", index_label="date")
        pd.DataFrame(result["trials"]).to_csv(output_dir / f"{label}_candidate_trials.csv", index=False)
        summary_rows.append(dict(approach=label, n=result["n"], candidates=len(pool),
            **result["first_step_counts"], selected=";".join(result["selected"]), status=result["status"]))
    summary = pd.DataFrame(summary_rows)
    summary.to_csv(output_dir / "summary.csv", index=False)
    manifest = dict(sources=sources, macro_mapping=MACROS, adjustments=adjustments,
        training_start=START, training_end=END, monthly_max_lag=MONTHLY_MAX_LAG,
        quarterly_max_lag=QUARTERLY_MAX_LAG, script_sha256=sha256(Path(__file__)),
        selection="Intercept-only, single-term forward; minimum new-term OLS p among candidates passing all four gates",
        gates="p_new < .05; 1.5 <= DW <= 2.5; JB p > .05; all nonconstant VIF < 10",
        sample="Fixed all-candidate common sample per approach; no 2011 warm-up or OOT fitting",
        scope="Reproduces latest diagnostic screening, not complete EG/ECM estimation or baseline/stress-floor calibration",
        versions=dict(python=platform.python_version(), numpy=np.__version__, pandas=pd.__version__,
                      scipy=scipy.__version__, statsmodels=statsmodels.__version__))
    for name, value in [("results.json", results), ("manifest.json", manifest)]:
        (output_dir / name).write_text(json.dumps(json_safe(value), ensure_ascii=False,
                                                indent=2, allow_nan=False), encoding="utf-8")
    for p in [card_path, macro_path]:
        if sha256(p) != sources[str(p)]:
            raise RuntimeError("An input file changed during execution; verify the sources and rerun.")
    print(summary.to_string(index=False))
    print(f"\nOutput directory: {output_dir}")
    return results


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--card", required=True, type=Path, help="Cardfee.xlsx with the original revenue summary worksheet")
    parser.add_argument("--macro", required=True, type=Path, help="Macro workbook: Macro Data sheet, column headers on the second row")
    parser.add_argument("--out", type=Path, default=Path("debit_results"), help="New output directory")
    args = parser.parse_args()
    run(args.card, args.macro, args.out)


if __name__ == "__main__":
    main()
