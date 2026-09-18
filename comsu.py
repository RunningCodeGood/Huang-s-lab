#!/usr/bin/env python3
"""Estimate and validate the Consumer Card fee revenue model.

All coefficients, diagnostics, accuracy statistics, and stability results are
calculated from the source workbooks at run time. No benchmark model results
from an existing report are embedded in this file.

The run writes numerical CSV tables and a JSON manifest only. It does not
generate reports or figures. The statistical scope is limited to the original
Consumer Card Fee Model: ADF/KPSS, OLS inference and VIF, Durbin-Watson,
Jarque-Bera, MAPE/RMSE, CUSUM, Chow, rolling coefficients, and OOT reestimation.
Diagnostic values do not by themselves establish overall model validity.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
import sys
import warnings
from dataclasses import dataclass
from importlib import metadata
from pathlib import Path

import numpy as np
import pandas as pd
from scipy import stats
from statsmodels.regression.linear_model import OLS
from statsmodels.stats.diagnostic import breaks_cusumolsresid
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.stats.stattools import durbin_watson, jarque_bera
from statsmodels.tools.tools import add_constant
from statsmodels.tsa.stattools import adfuller, kpss


ROOT = Path(__file__).resolve().parent
DEFAULT_CARD = Path("/Users/hyx/Desktop/Cardfee/Cardfee.xlsx")
DEFAULT_MACRO = Path("/Users/hyx/Desktop/Cardfee/Revise Version/Macro_Data_201001-202612.xls")
DEFAULT_OUTPUT = ROOT / "output" / "consumer_model_clean"

IN_SAMPLE_START = pd.Timestamp("2011-01-01")
IN_SAMPLE_END = pd.Timestamp("2023-09-01")
OOT_START = pd.Timestamp("2023-10-01")
OOT_END = pd.Timestamp("2025-12-01")

DATE_COLUMN = "YYYYMM"
TARGET_COLUMN = "Consumer Card"

MACRO_SPECS = {
    "Unemployment": {
        "group": "Core",
        "source": "M_FLBR_B.IUSA",
        "transform": "Monthly first difference in percentage points",
    },
    "CPI": {
        "group": "Core",
        "source": "M_FCPIU_B.IUSA",
        "transform": "Monthly percentage change",
    },
    "DPI": {
        "group": "Core",
        "source": "M_FYPDPIQ_B.IUSA",
        "transform": "Monthly percentage change",
    },
    "RetailSales": {
        "group": "Consumer",
        "source": "M_FRT_B.IUSA (Retail Sales Total_Bil.USD,CDASAAR)",
        "transform": "Monthly percentage change",
    },
    "ConsumerConfidence": {
        "group": "Consumer",
        "source": "M_FCBC_B.IUSA(Consumer confidence index)",
        "transform": "Monthly percentage change",
    },
}

BASE_TERMS = ["y_lag1", "y_lag2", "January", "December"]
SELECTION_P_LIMIT = 0.05
VIF_LIMIT = 10.0
DW_LOWER = 1.5
DW_UPPER = 2.5
JB_P_LIMIT = 0.05
ROLLING_WINDOW = 60


@dataclass
class Results:
    data: pd.DataFrame
    in_sample: pd.DataFrame
    oot: pd.DataFrame
    model: object
    selected_terms: list[str]
    stationarity: pd.DataFrame
    screening: pd.DataFrame
    coefficients: pd.DataFrame
    predictions: pd.DataFrame
    accuracy: pd.DataFrame
    residual_diagnostics: pd.DataFrame
    cusum: pd.DataFrame
    chow: pd.DataFrame
    rolling: pd.DataFrame
    rolling_summary: pd.DataFrame
    oot_only: pd.DataFrame
    source_audit: dict


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def normalize_month(values: pd.Series) -> pd.DatetimeIndex:
    if pd.api.types.is_datetime64_any_dtype(values):
        parsed = pd.to_datetime(values)
    else:
        text = values.astype(str).str.replace(r"\.0$", "", regex=True).str.strip()
        parsed = pd.to_datetime(text, format="%Y%m", errors="coerce")
        if parsed.isna().any():
            parsed = pd.to_datetime(values, errors="coerce")
    if parsed.isna().any():
        raise ValueError("The date column contains values that cannot be parsed as monthly dates.")
    return pd.DatetimeIndex(parsed).to_period("M").to_timestamp()


def validate_calendar(frame: pd.DataFrame, name: str) -> dict:
    if frame.index.has_duplicates:
        duplicates = frame.index[frame.index.duplicated()].strftime("%Y-%m").tolist()
        raise ValueError(f"{name} contains duplicate months: {duplicates}")
    expected = pd.date_range(frame.index.min(), frame.index.max(), freq="MS")
    missing = expected.difference(frame.index)
    if len(missing):
        raise ValueError(f"{name} is missing months: {missing.strftime('%Y-%m').tolist()}")
    return {
        "rows": int(len(frame)),
        "start": frame.index.min().strftime("%Y-%m"),
        "end": frame.index.max().strftime("%Y-%m"),
        "duplicate_months": 0,
        "missing_calendar_months": 0,
    }


def read_sources(card_path: Path, macro_path: Path) -> tuple[pd.DataFrame, pd.DataFrame, dict]:
    if not card_path.is_file():
        raise FileNotFoundError(f"Card source not found: {card_path}")
    if not macro_path.is_file():
        raise FileNotFoundError(f"Macro source not found: {macro_path}")

    card = pd.read_excel(card_path, sheet_name="汇总数据")
    missing_card = {DATE_COLUMN, TARGET_COLUMN}.difference(card.columns)
    if missing_card:
        raise ValueError(f"Card source is missing columns: {sorted(missing_card)}")
    card = card[[DATE_COLUMN, TARGET_COLUMN]].copy()
    card.index = normalize_month(card.pop(DATE_COLUMN))
    card.index.name = "date"
    card[TARGET_COLUMN] = pd.to_numeric(card[TARGET_COLUMN], errors="coerce")

    macro = pd.read_excel(macro_path, sheet_name="Macro Data", header=1)
    macro_fields = [spec["source"] for spec in MACRO_SPECS.values()]
    missing_macro = {DATE_COLUMN, *macro_fields}.difference(macro.columns)
    if missing_macro:
        raise ValueError(f"Macro source is missing columns: {sorted(missing_macro)}")
    macro = macro[[DATE_COLUMN, *macro_fields]].copy()
    macro.index = normalize_month(macro.pop(DATE_COLUMN))
    macro.index.name = "date"
    for column in macro.columns:
        macro[column] = pd.to_numeric(macro[column], errors="coerce")

    card_audit = validate_calendar(card, "Card data")
    macro_audit = validate_calendar(macro, "Macro data")
    if card[TARGET_COLUMN].isna().any():
        raise ValueError("Consumer Card contains missing values.")
    if (card[TARGET_COLUMN] <= 0).any():
        raise ValueError("Consumer Card must be positive for the log transformation.")
    if macro[macro_fields].isna().any().any():
        counts = macro[macro_fields].isna().sum()
        raise ValueError(f"Macro data contain missing values: {counts[counts > 0].to_dict()}")

    return card, macro, {
        "card_filename": card_path.name,
        "macro_filename": macro_path.name,
        "card_path": str(card_path),
        "macro_path": str(macro_path),
        "card_sha256": sha256(card_path),
        "macro_sha256": sha256(macro_path),
        "card": card_audit,
        "macro": macro_audit,
    }


def engineer_data(card: pd.DataFrame, macro: pd.DataFrame) -> tuple[pd.DataFrame, list[str]]:
    data = card.join(macro, how="left")
    modeling_range = data.loc[IN_SAMPLE_START:OOT_END]
    if modeling_range.empty:
        raise ValueError("The modeling period is outside the source data range.")
    macro_fields = [spec["source"] for spec in MACRO_SPECS.values()]
    if modeling_range[macro_fields].isna().any().any():
        raise ValueError("Macro values are missing inside the modeling period.")

    data["y"] = 100.0 * np.log(data[TARGET_COLUMN]).diff()
    data["y_lag1"] = data["y"].shift(1)
    data["y_lag2"] = data["y"].shift(2)
    data["January"] = (data.index.month == 1).astype(int)
    data["December"] = (data.index.month == 12).astype(int)

    candidates: list[str] = []
    for name, spec in MACRO_SPECS.items():
        source = spec["source"]
        transformed = data[source].diff() if name == "Unemployment" else data[source].pct_change(fill_method=None) * 100.0
        current_name = f"{name}_chg"
        average_name = f"{current_name}_MA3"
        data[current_name] = transformed
        data[average_name] = transformed.rolling(3).mean()
        candidates.extend([current_name, average_name])

    expected = pd.date_range(IN_SAMPLE_START, OOT_END, freq="MS")
    missing = expected.difference(data.index)
    if len(missing):
        raise ValueError(f"Combined data are missing months: {missing.strftime('%Y-%m').tolist()}")
    return data, candidates


def fit_ols(frame: pd.DataFrame, terms: list[str]):
    sample = frame[["y", *terms]].dropna().copy()
    if len(sample) <= len(terms) + 1:
        raise ValueError("Insufficient observations for the requested regression.")
    x = add_constant(sample[terms], has_constant="add")
    return OLS(sample["y"], x).fit(), sample


def compute_vifs(model) -> dict[str, float]:
    x = pd.DataFrame(model.model.exog, columns=model.model.exog_names)
    return {
        name: float(variance_inflation_factor(x.values, index))
        for index, name in enumerate(x.columns)
        if name != "const"
    }


def model_diagnostics(model) -> dict[str, float]:
    jb = jarque_bera(model.resid)
    return {
        "n": int(model.nobs),
        "r2": float(model.rsquared),
        "adj_r2": float(model.rsquared_adj),
        "dw": float(durbin_watson(model.resid)),
        "jb_stat": float(jb[0]),
        "jb_p": float(jb[1]),
    }


def forward_selection(data: pd.DataFrame, candidates: list[str]) -> tuple[list[str], pd.DataFrame]:
    sample = data.loc[IN_SAMPLE_START:IN_SAMPLE_END]
    selected: list[str] = []
    remaining = list(candidates)
    records: list[dict] = []
    round_number = 1

    while remaining:
        trials = []
        for candidate in remaining:
            terms = BASE_TERMS + selected + [candidate]
            model, used = fit_ols(sample, terms)
            vifs = compute_vifs(model)
            diagnostics = model_diagnostics(model)
            trial = {
                "round": round_number,
                "candidate": candidate,
                "n": int(len(used)),
                "coefficient": float(model.params[candidate]),
                "p_value": float(model.pvalues[candidate]),
                "adj_r2": float(model.rsquared_adj),
                "dw": diagnostics["dw"],
                "jb_p": diagnostics["jb_p"],
                "max_vif": float(max(vifs.values())),
            }
            trial["passes"] = bool(
                trial["p_value"] < SELECTION_P_LIMIT
                and trial["max_vif"] < VIF_LIMIT
                and DW_LOWER <= trial["dw"] <= DW_UPPER
                and trial["jb_p"] > JB_P_LIMIT
            )
            trials.append(trial)
            records.append(trial.copy())

        passing = [trial for trial in trials if trial["passes"]]
        if not passing:
            break
        winner = min(passing, key=lambda item: (item["p_value"], -item["adj_r2"], item["candidate"]))
        selected.append(winner["candidate"])
        remaining.remove(winner["candidate"])
        round_number += 1

    return selected, pd.DataFrame(records)


def stationarity_table(data: pd.DataFrame) -> pd.DataFrame:
    level = data.loc[IN_SAMPLE_START:IN_SAMPLE_END, TARGET_COLUMN]
    difference = level.diff().dropna()
    rows = []
    for label, series in (("Consumer Card level", level), ("Consumer Card first difference", difference)):
        adf = adfuller(series, regression="c", autolag="AIC", result_object=False)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            kp = kpss(series, regression="c", nlags="auto")
        rows.append(
            {
                "series": label,
                "n": int(len(series)),
                "adf_stat": float(adf[0]),
                "adf_p": float(adf[1]),
                "adf_lags": int(adf[2]),
                "kpss_stat": float(kp[0]),
                "kpss_p": float(kp[1]),
                "kpss_lags": int(kp[2]),
            }
        )
    return pd.DataFrame(rows)


def coefficient_table(model) -> pd.DataFrame:
    vifs = compute_vifs(model)
    confidence = model.conf_int()
    return pd.DataFrame(
        [
            {
                "term": term,
                "coefficient": float(model.params[term]),
                "std_error": float(model.bse[term]),
                "t_stat": float(model.tvalues[term]),
                "p_value": float(model.pvalues[term]),
                "ci_low": float(confidence.loc[term, 0]),
                "ci_high": float(confidence.loc[term, 1]),
                "vif": vifs.get(term, np.nan),
            }
            for term in model.params.index
        ]
    )


def prediction_frame(data: pd.DataFrame, model, sample: pd.DataFrame, window: str) -> pd.DataFrame:
    terms = [name for name in model.params.index if name != "const"]
    x = add_constant(sample[terms], has_constant="add")
    predicted_y = model.predict(x)
    prior_level = data[TARGET_COLUMN].shift(1).reindex(sample.index)
    predicted_level = prior_level * np.exp(predicted_y / 100.0)
    actual_level = data[TARGET_COLUMN].reindex(sample.index)
    return pd.DataFrame(
        {
            "window": window,
            "actual_level": actual_level,
            "predicted_level": predicted_level,
            "actual_y": sample["y"],
            "predicted_y": predicted_y,
            "residual_y": sample["y"] - predicted_y,
        },
        index=sample.index,
    )


def accuracy_row(predictions: pd.DataFrame) -> dict:
    error = predictions["actual_level"] - predictions["predicted_level"]
    return {
        "window": predictions["window"].iloc[0],
        "n": int(len(predictions)),
        "mape_pct": float((error.abs() / predictions["actual_level"]).mean() * 100.0),
        "rmse": float(np.sqrt(np.mean(np.square(error)))),
    }


def residual_row(predictions: pd.DataFrame) -> dict:
    residual = predictions["residual_y"]
    jb = jarque_bera(residual)
    return {
        "window": predictions["window"].iloc[0],
        "n": int(len(predictions)),
        "mean": float(residual.mean()),
        "std": float(residual.std(ddof=1)),
        "dw": float(durbin_watson(residual)),
        "jb_p": float(jb[1]),
    }


def cusum_table(data: pd.DataFrame, terms: list[str]) -> pd.DataFrame:
    rows = []
    for label, end in (("In sample", IN_SAMPLE_END), ("Full sample", OOT_END)):
        model, sample = fit_ols(data.loc[IN_SAMPLE_START:end], terms)
        statistic, p_value, critical = breaks_cusumolsresid(model.resid, ddof=len(model.params))
        rows.append(
            {
                "sample": label,
                "n": int(len(sample)),
                "statistic": float(statistic),
                "p_value": float(p_value),
                "critical_1pct": float(critical[0][1]),
                "critical_5pct": float(critical[1][1]),
                "critical_10pct": float(critical[2][1]),
            }
        )
    return pd.DataFrame(rows)


def chow_table(data: pd.DataFrame, terms: list[str]) -> pd.DataFrame:
    sample = data.loc[IN_SAMPLE_START:OOT_END, ["y", *terms]].dropna()
    pooled, _ = fit_ols(sample, terms)
    parameter_count = len(pooled.params)
    break_dates = (OOT_START, OOT_START + pd.DateOffset(months=3), OOT_START + pd.DateOffset(months=9))
    rows = []
    for break_date in break_dates:
        pre = sample.loc[sample.index < break_date]
        post = sample.loc[sample.index >= break_date]
        pre_model, _ = fit_ols(pre, terms)
        post_model, _ = fit_ols(post, terms)
        numerator = (pooled.ssr - pre_model.ssr - post_model.ssr) / parameter_count
        denominator_df = len(pre) + len(post) - 2 * parameter_count
        denominator = (pre_model.ssr + post_model.ssr) / denominator_df
        f_stat = float(numerator / denominator)
        rows.append(
            {
                "break_date": break_date.strftime("%Y-%m-%d"),
                "n_pre": int(len(pre)),
                "n_post": int(len(post)),
                "f_stat": f_stat,
                "p_value": float(stats.f.sf(f_stat, parameter_count, denominator_df)),
                "df_num": int(parameter_count),
                "df_den": int(denominator_df),
            }
        )
    return pd.DataFrame(rows)


def rolling_coefficients(data: pd.DataFrame, terms: list[str]) -> tuple[pd.DataFrame, pd.DataFrame]:
    sample = data.loc[IN_SAMPLE_START:OOT_END, ["y", *terms]].dropna()
    if len(sample) < ROLLING_WINDOW:
        raise ValueError("The usable model sample is shorter than the rolling window.")
    rows = []
    for end_position in range(ROLLING_WINDOW - 1, len(sample)):
        current = sample.iloc[end_position - ROLLING_WINDOW + 1 : end_position + 1]
        model, _ = fit_ols(current, terms)
        row = {
            "window_end": current.index[-1],
            "window_start": current.index[0],
            "n": int(len(current)),
        }
        row.update({term: float(model.params[term]) for term in model.params.index})
        rows.append(row)
    rolling = pd.DataFrame(rows).set_index("window_end")

    summary = []
    for term in terms:
        values = rolling[term]
        initial_sign = np.sign(values.iloc[0])
        summary.append(
            {
                "term": term,
                "min": float(values.min()),
                "max": float(values.max()),
                "mean": float(values.mean()),
                "std": float(values.std(ddof=1)),
                "same_sign_count": int((np.sign(values) == initial_sign).sum()),
                "windows": int(len(values)),
            }
        )
    return rolling, pd.DataFrame(summary)


def run_analysis(card_path: Path, macro_path: Path) -> Results:
    card, macro, source_audit = read_sources(card_path, macro_path)
    data, candidates = engineer_data(card, macro)
    selected_macro, screening = forward_selection(data, candidates)
    if not selected_macro:
        raise RuntimeError("Forward selection did not retain a macro candidate under the configured gates.")
    selected_terms = BASE_TERMS + selected_macro

    model, in_sample = fit_ols(data.loc[IN_SAMPLE_START:IN_SAMPLE_END], selected_terms)
    _, oot = fit_ols(data.loc[OOT_START:OOT_END], selected_terms)
    predictions_is = prediction_frame(data, model, in_sample, "In sample")
    predictions_oot = prediction_frame(data, model, oot, "Out of time")
    predictions = pd.concat([predictions_is, predictions_oot]).sort_index()
    rolling, rolling_summary = rolling_coefficients(data, selected_terms)
    oot_model, oot_sample = fit_ols(data.loc[OOT_START:OOT_END], selected_terms)
    oot_only = coefficient_table(oot_model)
    oot_only["n"] = len(oot_sample)

    return Results(
        data=data,
        in_sample=in_sample,
        oot=oot,
        model=model,
        selected_terms=selected_terms,
        stationarity=stationarity_table(data),
        screening=screening,
        coefficients=coefficient_table(model),
        predictions=predictions,
        accuracy=pd.DataFrame([accuracy_row(predictions_is), accuracy_row(predictions_oot)]),
        residual_diagnostics=pd.DataFrame([residual_row(predictions_is), residual_row(predictions_oot)]),
        cusum=cusum_table(data, selected_terms),
        chow=chow_table(data, selected_terms),
        rolling=rolling,
        rolling_summary=rolling_summary,
        oot_only=oot_only,
        source_audit=source_audit,
    )


def save_outputs(results: Results, output_dir: Path) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    frames = {
        "stationarity.csv": results.stationarity,
        "macro_screening.csv": results.screening,
        "coefficients.csv": results.coefficients,
        "predictions.csv": results.predictions.reset_index(),
        "accuracy.csv": results.accuracy,
        "residual_diagnostics.csv": results.residual_diagnostics,
        "cusum.csv": results.cusum,
        "chow.csv": results.chow,
        "rolling_coefficients.csv": results.rolling.reset_index(),
        "rolling_summary.csv": results.rolling_summary,
        "oot_only_coefficients.csv": results.oot_only,
    }
    for filename, frame in frames.items():
        frame.to_csv(output_dir / filename, index=False, encoding="utf-8-sig", float_format="%.12g")

    manifest = {
        "periods": {
            "in_sample": [IN_SAMPLE_START.strftime("%Y-%m"), IN_SAMPLE_END.strftime("%Y-%m")],
            "out_of_time": [OOT_START.strftime("%Y-%m"), OOT_END.strftime("%Y-%m")],
        },
        "target": TARGET_COLUMN,
        "output_formats": ["csv", "json"],
        "interpretation_notes": [
            "The existing selection specification is preserved; no additional tests are run.",
            "Durbin-Watson is retained as a reported descriptive diagnostic and selection heuristic, not a formal no-autocorrelation conclusion for this autoregressive model.",
            "OOT predictions are conditional one-step predictions using the fixed in-sample model; OOT-only reestimation is a separate diagnostic.",
        ],
        "selected_terms": results.selected_terms,
        "selection_gates": {
            "p_value_below": SELECTION_P_LIMIT,
            "vif_below": VIF_LIMIT,
            "durbin_watson_range": [DW_LOWER, DW_UPPER],
            "jarque_bera_p_above": JB_P_LIMIT,
        },
        "model_diagnostics": model_diagnostics(results.model),
        "accuracy": results.accuracy.to_dict(orient="records"),
        "source_audit": results.source_audit,
        "runtime": {
            "python": sys.version,
            "platform": platform.platform(),
            "packages": {
                name: metadata.version(name)
                for name in ("numpy", "pandas", "scipy", "statsmodels")
            },
        },
    }
    (output_dir / "run_manifest.json").write_text(json.dumps(manifest, indent=2), encoding="utf-8")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--card-file", type=Path, default=DEFAULT_CARD)
    parser.add_argument("--macro-file", type=Path, default=DEFAULT_MACRO)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    output_dir = args.output_dir.resolve()
    results = run_analysis(args.card_file.resolve(), args.macro_file.resolve())
    save_outputs(results, output_dir)
    print(f"Statistical results: {output_dir}")
    print(f"Selected terms: {results.selected_terms}")
    print(results.coefficients.to_string(index=False))
    print(results.accuracy.to_string(index=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
