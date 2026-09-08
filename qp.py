#!/usr/bin/env python3
"""Reproduce the final QualPay report with one Python script.

DATA ARE NOT EMBEDDED. Read the external monthly_fees.csv supplied with the
earlier analysis, or pass another file with the same columns and date coverage.
Required columns: date, sponsorship, qualpay. Extra columns are ignored.
Dates must cover January 2011 through December 2025, one row per month.

Install dependencies (Python 3.10-3.12 recommended):
    python -m pip install numpy==2.0.2 pandas==2.3.3 scipy==1.13.1 \
        statsmodels==0.14.6 matplotlib==3.9.4

Run with an explicit input file:
    python qualpay_final_analysis.py --input /path/to/monthly_fees.csv --out results

Or place monthly_fees.csv beside this script and run:
    python qualpay_final_analysis.py

The default input is relative to this script. The output directory (default:
qualpay_results) and explicit relative paths are relative to the working
directory. --no-plot skips the figure and does not require matplotlib.

The script fits S1 to historical Sponsorship (2011-2020), compares three
QualPay revenue means with the same-month S1 extrapolations, and estimates
separate month-adjusted trends for historical Sponsorship, full QualPay and
QualPay excluding January-March 2025. Calendar time is never renumbered.

Inference uses Bartlett HAC, n/(n-k) covariance correction and a Student t
reference distribution with n-k degrees of freedom. Historical primary lag
is 6; QualPay primary lag is 3, with lags 6 and 12 as sensitivity checks.
Quarter exclusion is a sensitivity analysis, not a formal outlier diagnosis.
The full 31-month QualPay sample remains primary. Mean similarity is
descriptive; HAC inference is approximate, exploratory and unadjusted for
multiple comparisons.

Outputs: scale_comparison.csv, slope_results.csv, coefficients.csv,
monthly_s1_comparison.csv, fitted_values.csv, model_details.json,
report_summary.txt, and history_sponsorship_projection_en.png (unless skipped).
Existing files with these names in --out are replaced. The input is read-only.
No other project script or previously generated result file is required.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
from pathlib import Path

try:
    import numpy as np
    import pandas as pd
    import scipy
    from scipy import stats
    import statsmodels
    import statsmodels.api as sm
except ImportError as exc:
    raise SystemExit(
        "Missing analysis dependency. Install with: python -m pip install "
        "numpy pandas scipy statsmodels matplotlib"
    ) from exc


BASE = Path(__file__).resolve().parent
EXCLUDED_DATES = pd.to_datetime(["2025-01-01", "2025-02-01", "2025-03-01"])
CHART_NAME = "history_sponsorship_projection_en.png"


def load_data(path: Path) -> pd.DataFrame:
    """Validate the external panel; preserve the actual calendar month index."""
    if not path.is_file():
        raise ValueError(
            f"Input CSV not found: {path}. No data are embedded. "
            "Pass --input /path/to/monthly_fees.csv or place it beside this script."
        )
    data = pd.read_csv(path, encoding="utf-8-sig")
    missing = {"date", "sponsorship", "qualpay"} - set(data.columns)
    if missing:
        raise ValueError(f"Missing input columns: {', '.join(sorted(missing))}")
    data["date"] = pd.to_datetime(data["date"], errors="raise")
    if data["date"].isna().any() or data["date"].duplicated().any():
        raise ValueError("Dates must be nonmissing and unique.")
    data = data.sort_values("date").reset_index(drop=True)
    expected = pd.date_range("2011-01-01", "2025-12-01", freq="MS")
    if not pd.DatetimeIndex(data["date"]).equals(expected):
        raise ValueError("Expected all 180 monthly dates, January 2011-December 2025.")
    for column in ["sponsorship", "qualpay"]:
        data[column] = pd.to_numeric(data[column], errors="raise")
    historical = data["date"] < "2021-01-01"
    qualpay = data["date"] >= "2023-06-01"
    if not np.isfinite(data.loc[historical, "sponsorship"]).all():
        raise ValueError("All 120 historical Sponsorship observations must be finite.")
    if not np.isfinite(data.loc[qualpay, "qualpay"]).all():
        raise ValueError("All 31 QualPay observations must be finite.")
    data["t"] = 12 * (data["date"].dt.year - 2011) + data["date"].dt.month - 1
    return data


def design(data, seasonal=True):
    x = data[["t"]].astype(float).copy()
    if seasonal:
        for month in range(2, 13):
            x[f"month_{month}"] = (data["date"].dt.month == month).astype(float)
    return sm.add_constant(x, has_constant="add")


def calendar_score_meat(scores, month_index, maxlags):
    """Bartlett HAC score sum, with lags measured in calendar months.

    Missing months contribute zero score only to this covariance sum. No
    outcome is imputed, no observation is added to OLS, and n stays 28 in
    the exclusion sample. At lag h, only retained pairs exactly h calendar
    months apart are included. This equals padding missing months with zero
    score vectors and taking ordinary lag products, but not padding data.
    """
    scores = np.asarray(scores, dtype=float)
    month_index = np.asarray(month_index)
    if scores.ndim != 2 or len(month_index) != len(scores):
        raise ValueError("One month index is required for every score row.")
    if not np.isfinite(scores).all() or not np.isfinite(month_index).all():
        raise ValueError("Scores and month indices must be finite.")
    if not np.equal(month_index, np.floor(month_index)).all():
        raise ValueError("Month indices must be integers.")
    if (np.diff(month_index) <= 0).any():
        raise ValueError("Month indices must be strictly increasing.")
    if not isinstance(maxlags, (int, np.integer)) or maxlags < 0:
        raise ValueError("maxlags must be a nonnegative integer.")
    month_index = month_index.astype(int)
    meat = scores.T @ scores
    differences = month_index[:, None] - month_index[None, :]
    for lag in range(1, maxlags + 1):
        later, earlier = np.where(differences == lag)
        lag_product = scores[later].T @ scores[earlier]
        weight = 1.0 - lag / (maxlags + 1.0)
        meat += weight * (lag_product + lag_product.T)
    return meat


def calendar_hac_covariance(fit, month_index, maxlags):
    """OLS sandwich with n/(n-k) correction based on observed sample n."""
    x = np.asarray(fit.model.exog, dtype=float)
    n, k = x.shape
    if np.linalg.matrix_rank(x) != k or n <= k:
        raise ValueError("Design must have full column rank and n > k.")
    scores = x * np.asarray(fit.resid)[:, None]
    meat = calendar_score_meat(scores, month_index, maxlags)
    bread = np.asarray(fit.normalized_cov_params)
    covariance = (n / (n - k)) * (bread @ meat @ bread)
    return (covariance + covariance.T) / 2.0


def inference(fit, covariance):
    coefficients = np.asarray(fit.params)
    standard_errors = np.sqrt(np.diag(covariance))
    statistic = coefficients / standard_errors
    pvalue = 2 * stats.t.sf(np.abs(statistic), fit.df_resid)
    critical = stats.t.ppf(0.975, fit.df_resid)
    lower, upper = coefficients - critical * standard_errors, coefficients + critical * standard_errors
    return standard_errors, statistic, pvalue, lower, upper




def model_detail(fit, sample: pd.DataFrame) -> dict:
    """Keep full coefficients, design dimensions and dates for reproduction."""
    n, k = fit.model.exog.shape
    return {
        "n": n,
        "k": k,
        "design_rank": int(np.linalg.matrix_rank(fit.model.exog)),
        "df_resid": int(fit.df_resid),
        "r_squared": float(fit.rsquared),
        "coefficients": {name: float(value) for name, value in fit.params.items()},
        "dates": sample["date"].dt.strftime("%Y-%m-%d").tolist(),
        "calendar_t": sample["t"].astype(int).tolist(),
        "month_counts": {
            str(month): int(count)
            for month, count in sample.groupby(sample["date"].dt.month).size().items()
        },
    }


def analyze(data: pd.DataFrame) -> dict:
    """Compute exactly the scale and trend analyses used in the final report."""
    history = data.loc[data["date"] < "2021-01-01"].copy()
    full = data.loc[data["date"] >= "2023-06-01"].copy()
    trimmed = full.loc[~full["date"].isin(EXCLUDED_DATES)].copy()
    s1 = sm.OLS(history["sponsorship"], design(history, seasonal=False)).fit()

    monthly = full[["date", "t"]].copy()
    monthly["actual"] = full["qualpay"]
    monthly["s1_prediction"] = s1.params["const"] + s1.params["t"] * full["t"]
    monthly["signed_difference"] = monthly["actual"] - monthly["s1_prediction"]
    monthly["excluded_q1_2025"] = monthly["date"].isin(EXCLUDED_DATES)
    comparisons = [
        ("Initial 19 months", monthly.loc[monthly["date"] <= "2024-12-01"]),
        ("Excluding Q1 2025", monthly.loc[~monthly["excluded_q1_2025"]]),
        ("Full 31 months", monthly),
    ]
    scale_rows = []
    for label, sample in comparisons:
        actual_mean = float(sample["actual"].mean())
        predicted_mean = float(sample["s1_prediction"].mean())
        scale_rows.append({
            "sample": label,
            "n": len(sample),
            "actual_mean": actual_mean,
            "s1_extrapolated_mean": predicted_mean,
            "mean_signed_difference": float(sample["signed_difference"].mean()),
            "relative_difference_in_means_pct": 100 * (actual_mean / predicted_mean - 1),
        })

    models = {"S1": model_detail(s1, history)}
    models["S1"]["formula"] = "S_t = alpha + beta*t + error_t; no month effects"
    slope_rows, coefficient_rows, fitted_rows = [], [], []
    fits_for_rows = [("S1", history, "sponsorship", s1)]
    specifications = [
        ("Historical Sponsorship", history, "sponsorship", (6,), 6),
        ("QualPay full sample", full, "qualpay", (3, 6, 12), 3),
        ("QualPay excluding Q1 2025", trimmed, "qualpay", (3, 6, 12), 3),
    ]
    for label, sample, outcome, lags, primary_lag in specifications:
        x = design(sample, seasonal=True)
        fit = sm.OLS(sample[outcome], x).fit()
        n, k = x.shape
        detail = model_detail(fit, sample)
        detail["formula"] = "y_t = alpha + beta*t + sum_{m=2}^{12} delta_m*I(month=m) + error_t"
        detail["primary_hac_lag"] = primary_lag
        detail["covariance_correction"] = n / (n - k)
        detail["covariances_by_calendar_hac_lag"] = {}
        for lag in lags:
            covariance = calendar_hac_covariance(fit, sample["t"], lag)
            se, tstat, pvalue, lower, upper = inference(fit, covariance)
            detail["covariances_by_calendar_hac_lag"][str(lag)] = covariance.tolist()
            for j, term in enumerate(x.columns):
                coefficient_rows.append({
                    "sample": label, "hac_lag": lag, "primary": lag == primary_lag,
                    "term": term, "estimate": float(fit.params.iloc[j]),
                    "standard_error": float(se[j]), "t_statistic": float(tstat[j]),
                    "df_resid": n - k, "p_two_sided": float(pvalue[j]),
                    "ci95_lower": float(lower[j]), "ci95_upper": float(upper[j]),
                })
            j = list(x.columns).index("t")
            slope_rows.append({
                "sample": label, "n": n, "k": k, "df_resid": n - k,
                "hac_lag": lag, "primary": lag == primary_lag,
                "slope": float(fit.params["t"]), "standard_error": float(se[j]),
                "t_statistic": float(tstat[j]), "p_two_sided": float(pvalue[j]),
                "ci95_lower": float(lower[j]), "ci95_upper": float(upper[j]),
                "r_squared": float(fit.rsquared),
            })
        models[label] = detail
        fits_for_rows.append((label, sample, outcome, fit))

    for label, sample, outcome, fit in fits_for_rows:
        for date, t, actual, fitted, residual, leverage in zip(
            sample["date"], sample["t"], sample[outcome], fit.fittedvalues,
            fit.resid, fit.get_influence().hat_matrix_diag,
        ):
            fitted_rows.append({
                "model": label, "date": date.strftime("%Y-%m-%d"), "t": int(t),
                "actual": float(actual), "fitted": float(fitted),
                "residual": float(residual), "leverage": float(leverage),
            })
    return {
        "s1_fit": s1,
        "scale": pd.DataFrame(scale_rows),
        "slopes": pd.DataFrame(slope_rows),
        "coefficients": pd.DataFrame(coefficient_rows),
        "monthly": monthly,
        "fitted": pd.DataFrame(fitted_rows),
        "models": models,
    }


def create_figure(data: pd.DataFrame, s1_fit, out_dir: Path) -> Path:
    """Draw the final English figure directly from this run's data and S1 fit."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt

    d = data
    prediction = s1_fit.params["const"] + s1_fit.params["t"] * d["t"]
    pre = d.loc[d["date"] < "2021-01-01"]
    post = d.loc[d["date"] >= "2023-06-01"]
    monthly = pd.DataFrame({"prediction_trend": prediction.loc[post.index]})
    navy, blue, teal, grey, ink = "#24486B", "#7596B5", "#078578", "#E6EBEF", "#1E2D3A"
    plt.rcParams.update({
        "font.family": "DejaVu Sans", "font.size": 9, "axes.titlesize": 12,
        "axes.labelsize": 9, "xtick.labelsize": 8, "ytick.labelsize": 8,
        "savefig.facecolor": "white", "axes.unicode_minus": False,
    })
    fig, ax = plt.subplots(figsize=(6.5, 3.6))
    fig.subplots_adjust(left=.105, right=.975, top=.78, bottom=.26)
    ax.grid(axis="y", color=grey, linewidth=.7, zorder=0)
    ax.spines[["top", "right"]].set_visible(False)
    ax.spines[["bottom", "left"]].set_color("#CBD4DC")
    ax.tick_params(length=0, colors="#53606A", pad=5)
    ax.set_ylim(0, 2000)
    ax.set_yticks([0, 500, 1000, 1500, 2000])
    ax.set_ylabel("Original units", color="#53606A")

    fig.text(.105, .94, "QualPay revenue and the historical Sponsorship projection",
             fontsize=10.4, weight="bold", color=ink)
    full_difference = 100 * (post.qualpay.mean() / monthly.prediction_trend.mean() - 1)
    fig.text(.105, .884,
             f"31-month means: QualPay {post.qualpay.mean():,.0f} | Sponsorship projection "
             f"{monthly.prediction_trend.mean():,.0f} ({full_difference:+.2f}%)",
             fontsize=7.7, color="#53606A")
    ax.axvspan(pd.Timestamp("2021-01-01"), pd.Timestamp("2023-06-01"), color="#F0F3F5")
    ax.plot(pre.date, pre.sponsorship, lw=1.2, color=blue,
            label="Sponsorship actual (2011–2020)")
    ax.plot(pre.date, prediction.iloc[:120], lw=1.6, color=navy,
            label="Historical Sponsorship model fit")
    future = d[d.date >= "2020-12-01"]
    ax.plot(future.date, prediction.iloc[119:], lw=1.6, ls=(0, (5, 4)), color=navy,
            label="Historical Sponsorship projection")
    ax.plot(post.date, post.qualpay, lw=1.6, marker="o", ms=2.3, color=teal,
            label="QualPay actual (from June 2023)")
    ax.text(pd.Timestamp("2022-03-01"), 1910, "29 months\nexcluded", ha="center", va="top",
            color="#697783", fontsize=7.5)
    ax.set_xlim(pd.Timestamp("2010-10-01"), pd.Timestamp("2026-03-01"))
    ax.set_xticks([pd.Timestamp(f"{year}-01-01") for year in [2011, 2014, 2017, 2020, 2023, 2025]])
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.legend(loc="upper left", bbox_to_anchor=(-.015, -.095), ncol=2, frameon=False,
              fontsize=6.65, handlelength=2.4, columnspacing=1.25, labelspacing=.6)
    fig.text(.105, .075, "Benchmark fitted to 2011–2020 Sponsorship data only; values retain the original units.",
             fontsize=6.5, color="#53606A")
    fig.text(.105, .04, "Confidence and prediction intervals are not shown. Numerical fit cannot establish a transfer of business.",
             fontsize=6.25, color="#53606A")
    out_dir.mkdir(parents=True, exist_ok=True)
    target = out_dir / CHART_NAME
    fig.savefig(target, dpi=300)
    plt.close(fig)
    return target



def save_results(result: dict, data: pd.DataFrame, input_path: Path, out: Path, plot: bool) -> None:
    out.mkdir(parents=True, exist_ok=True)
    for key, filename in [
        ("scale", "scale_comparison.csv"), ("slopes", "slope_results.csv"),
        ("coefficients", "coefficients.csv"), ("monthly", "monthly_s1_comparison.csv"),
        ("fitted", "fitted_values.csv"),
    ]:
        result[key].to_csv(out / filename, index=False)
    metadata = {
        "input_file": str(input_path.resolve()),
        "input_sha256": hashlib.sha256(input_path.read_bytes()).hexdigest(),
        "data_embedded": False,
        "time_encoding": "12*(year - 2011) + month - 1; actual calendar gaps preserved",
        "excluded_dates": EXCLUDED_DATES.strftime("%Y-%m-%d").tolist(),
        "reference_month": "January",
        "hac_kernel": "Bartlett: weight at calendar lag h is 1 - h/(L+1)",
        "hac_correction": "n/(n-k), with n counting retained observations only",
        "inference": "Two-sided t test of beta=0 and 95% t interval, df=n-k; approximate HAC inference",
        "mean_difference": "mean(actual - same-month S1 extrapolation); signed original revenue units",
        "relative_difference_pct": "100 * (actual mean / same-month S1 extrapolated mean - 1)",
        "interpretation": [
            "S1 uses the 120 historical months only. Its coefficients are fixed for all scale comparisons.",
            "S1 has no month effects. The three trend models estimate their own intercept, time slope and 11 month effects.",
            "The full 31-month QualPay sample is primary. Exclusion of Q1 2025 is a sensitivity analysis, not a formal outlier diagnosis.",
            "The excluded sample has 28 observations and 15 residual degrees of freedom. January-March 2024 each have one observation, fitted exactly by month effects; repeated months identify the slope.",
            "Read exclusion-sample significance from the lag-specific p-values. Changing the HAC lag does not change the estimated OLS slope.",
            "Mean similarity is descriptive, and HAC inference is approximate, exploratory and unadjusted for multiple comparisons.",
        ],
        "models": result["models"],
        "versions": {"python": platform.python_version(), "numpy": np.__version__,
                     "pandas": pd.__version__, "scipy": scipy.__version__,
                     "statsmodels": statsmodels.__version__},
    }
    (out / "model_details.json").write_text(json.dumps(metadata, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    if plot:
        create_figure(data, result["s1_fit"], out)
    slope_columns = ["sample", "n", "df_resid", "hac_lag", "slope", "standard_error",
                     "ci95_lower", "ci95_upper", "p_two_sided"]
    primary = result["slopes"].loc[result["slopes"]["primary"], slope_columns]
    sensitivity = result["slopes"].loc[result["slopes"]["sample"].eq("QualPay excluding Q1 2025"), slope_columns]
    summary = (
        "Evidence 1: Revenue scale relative to the historical Sponsorship S1 extrapolation\n"
        + result["scale"].to_string(index=False, float_format=lambda value: f"{value:.6g}")
        + "\n\nEvidence 2: Trends controlling for month-of-year effects\n"
        + primary.to_string(index=False, float_format=lambda value: f"{value:.6g}")
        + "\n\nHAC sensitivity after excluding January-March 2025\n"
        + sensitivity.to_string(index=False, float_format=lambda value: f"{value:.6g}")
        + "\n\nQuarter exclusion is a sensitivity analysis, not a formal outlier classification.\n"
    )
    (out / "report_summary.txt").write_text(summary, encoding="utf-8")
    print(summary)
    print(f"Results saved to {out.resolve()}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--input", type=Path, default=BASE / "monthly_fees.csv", help="External monthly CSV; no data are embedded")
    parser.add_argument("--out", type=Path, default=Path("qualpay_results"), help="Output directory (default: ./qualpay_results)")
    parser.add_argument("--no-plot", action="store_true", help="Skip the figure; matplotlib is then unnecessary")
    args = parser.parse_args()
    try:
        data = load_data(args.input)
        result = analyze(data)
        save_results(result, data, args.input, args.out, plot=not args.no_plot)
    except (OSError, ValueError, KeyError, ImportError) as exc:
        parser.exit(2, f"Cannot reproduce the QualPay analysis: {exc}\n")


if __name__ == "__main__":
    main()
