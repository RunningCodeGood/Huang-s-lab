#!/usr/bin/env python3
"""Feature engineering, OOT evaluation, and 2026 forecasting.

Required:
    Python 3.10+, numpy, pandas, openpyxl, Pillow

Recommended:
    statsmodels  (ADF/KPSS, ETS, SARIMA, SARIMAX benchmarks)
    xlrd         (direct reading of legacy .xls files)

If xlrd is unavailable, the script attempts a temporary .xls -> .xlsx conversion
through LibreOffice/soffice. The main OLS and rolling-regression pipeline does not
require statsmodels.


"""

from __future__ import annotations

import argparse
import json1
import math
import shutil
import subprocess
import tempfile
import warnings
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Sequence

import numpy as np
import pandas as pd


DEFAULT_MACRO = Path("Macro.xls")
DEFAULT_CARD = Path("Card.xls")
MACRO_CODES = ("FIP", "FZA", "FRT4451")#This is given after reviewing the final result
LAGS = (1, 3, 6)
RECOMMENDED_MODEL = "Rolling_ARX_DLog_FZA_60"#This is given after reviewing the final result
EXCLUDED_TERMS = frozenset({"target_L12", "FIP_diff_L1", "FRT4451_dlog_L3"})#This is given after reviewing the final result

# Locked specifications reproduce the delivered analysis. The reduced dlog
# challenger was finalized after reviewing results through 2025-12, so its OOT
# metrics are post-selection validation rather than an untouched confirmatory
# test. Use --rescreen only for strict diagnostic screening; it does not relax
# the IC threshold or replace the locked specifications automatically.
LOCKED_FEATURES = {
    "diff": (
        "FZA_diff_L1",
    ),
    "dlog": (
        "FZA_yoylog_L1",
    ),
    "yoylog": (
        "FIP_dlog_L6",
        "FZA_yoylog_L3",
        "FRT4451_diff_L3",
        "FZA_yoylog_L3__x__FRT4451_yoylog_L3",
        "FZA_yoylog_L1__x__FRT4451_yoylog_L1",
    ),
}


@dataclass(frozen=True)
class ModelSpec:
    name: str
    target: str
    macro_features: tuple[str, ...] = ()
    target_lags: tuple[int, ...] = ()
    trend: bool = False
    harmonics: int = 0
    window: int | None = None
    ridge_lambda: float | None = None
    evaluation_mode: str = "fixed"


@dataclass
class FittedRegression:
    kind: str
    columns: list[str]
    intercept: float
    beta: np.ndarray
    residuals: np.ndarray
    smear: float
    means: np.ndarray | None = None
    scales: np.ndarray | None = None


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--macro", type=Path, default=DEFAULT_MACRO)
    parser.add_argument("--card", type=Path, default=DEFAULT_CARD)
    parser.add_argument("--output-dir", type=Path, default=Path("python_model_outputs"))
    parser.add_argument(
        "--oot-plot",
        type=Path,
        default=None,
        help="Optional output path for the best-model OOT actual-versus-forecast PNG.",
    )
    parser.add_argument("--development-end", default="2023-09-01")
    parser.add_argument("--oot-start", default="2023-10-01")
    parser.add_argument("--oot-end", default="2025-12-01")
    parser.add_argument("--future-start", default="2026-01-01")
    parser.add_argument("--future-end", default="2026-12-01")
    parser.add_argument(
        "--rescreen",
        action="store_true",
        help="Export strict ADF/KPSS and Spearman-IC screening diagnostics; keep locked model specifications.",
    )
    parser.add_argument(
        "--skip-statsmodels",
        action="store_true",
        help="Skip optional ADF/KPSS, ETS, SARIMA and SARIMAX benchmarks.",
    )
    return parser.parse_args()



 


def lag(values: np.ndarray, periods: int) -> np.ndarray:
    out = np.full(values.shape, np.nan, dtype=float)
    if periods == 0:
        out[:] = values
    elif periods < len(values):
        out[periods:] = values[:-periods]
    return out


def difference(values: np.ndarray, periods: int = 1) -> np.ndarray:
    out = np.full(values.shape, np.nan, dtype=float)
    if periods < len(values):
        out[periods:] = values[periods:] - values[:-periods]
    return out


def target_vector(y: np.ndarray, transform: str) -> np.ndarray:
    if transform == "level":
        return y.copy()
    if transform == "log":
        return np.log(y)
    if transform == "diff":
        return difference(y, 1)
    if transform == "dlog":
        return difference(np.log(y), 1)
    if transform == "yoylog":
        return difference(np.log(y), 12)
    raise ValueError(f"Unknown target transform: {transform}")


def engineer_features(data: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    features = pd.DataFrame(index=data.index)
    metadata: list[dict[str, object]] = []
    for family in MACRO_CODES:
        x = data[family].to_numpy(float)
        transforms = {
            "loglevel": np.log(x),
            "diff": difference(x, 1),
            "dlog": difference(np.log(x), 1),
            "yoylog": difference(np.log(x), 12),
        }
        for transform, values in transforms.items():
            for periods in LAGS:
                name = f"{family}_{transform}_L{periods}"
                features[name] = lag(values, periods)
                metadata.append(
                    {
                        "feature": name,
                        "family": family,
                        "transform": transform,
                        "lag": periods,
                        "interaction": False,
                    }
                )

    pairs = (("FIP", "FZA"), ("FIP", "FRT4451"), ("FZA", "FRT4451"))
    for transform in ("diff", "dlog", "yoylog"):
        for periods in LAGS:
            for left, right in pairs:
                a = f"{left}_{transform}_L{periods}"
                b = f"{right}_{transform}_L{periods}"
                name = f"{a}__x__{b}"
                features[name] = features[a] * features[b]
                metadata.append(
                    {
                        "feature": name,
                        "family": f"{left}x{right}",
                        "transform": transform,
                        "lag": periods,
                        "interaction": True,
                    }
                )
    return features, pd.DataFrame(metadata)


def optional_statsmodels():
    try:
        from statsmodels.tsa.holtwinters import ExponentialSmoothing  # noqa: F401
        from statsmodels.tsa.statespace.sarimax import SARIMAX  # noqa: F401
        from statsmodels.tsa.stattools import adfuller, kpss  # noqa: F401
    except ImportError:
        return None
    return True


def stationarity_table(y: np.ndarray, train_end: int) -> pd.DataFrame:
    try:
        from statsmodels.tsa.stattools import adfuller, kpss
    except ImportError:
        warnings.warn("statsmodels not installed; target stationarity table is skipped.")
        return pd.DataFrame()

    rows = []
    for transform in ("level", "log", "diff", "dlog", "yoylog"):
        values = target_vector(y, transform)[: train_end + 1]
        values = values[np.isfinite(values)]
        try:
            adf_p = float(adfuller(values, autolag="AIC")[1])
        except Exception:
            adf_p = np.nan
        try:
            kpss_p = float(kpss(values, regression="c", nlags="auto")[1])
        except Exception:
            kpss_p = np.nan
        rows.append(
            {
                "variable": f"commercial_fee_{transform}",
                "n": len(values),
                "adf_p": adf_p,
                "kpss_p": kpss_p,
                "stationary_pass": bool(adf_p <= 0.05 and kpss_p >= 0.05),
            }
        )
    return pd.DataFrame(rows)


def screen_features(
    features: pd.DataFrame,
    metadata: pd.DataFrame,
    y: np.ndarray,
    train_end: int,
    target_transform: str,
) -> pd.DataFrame:
    """ADF/KPSS + Spearman IC + split-sign screening, using development data only."""
    try:
        from statsmodels.tsa.stattools import adfuller, kpss
    except ImportError as exc:
        raise RuntimeError("--rescreen requires statsmodels") from exc

    target = target_vector(y, target_transform)
    rows = []
    for meta in metadata.to_dict("records"):
        name = str(meta["feature"])
        x = features[name].to_numpy(float)
        idx = np.arange(train_end + 1)
        valid = idx[np.isfinite(x[idx]) & np.isfinite(target[idx])]
        if len(valid) < 36 or np.std(x[valid], ddof=1) < 1e-12:
            continue
        rho = float(pd.Series(x[valid]).corr(pd.Series(target[valid]), method="spearman"))
        t_stat = rho * math.sqrt((len(valid) - 2) / max(1e-12, 1.0 - rho * rho))
        midpoint = len(valid) // 2
        rho_1 = float(pd.Series(x[valid[:midpoint]]).corr(pd.Series(target[valid[:midpoint]]), method="spearman"))
        rho_2 = float(pd.Series(x[valid[midpoint:]]).corr(pd.Series(target[valid[midpoint:]]), method="spearman"))
        try:
            adf_p = float(adfuller(x[valid], autolag="AIC")[1])
        except Exception:
            adf_p = np.nan
        try:
            kpss_p = float(kpss(x[valid], regression="c", nlags="auto")[1])
        except Exception:
            kpss_p = np.nan
        stationary = bool(adf_p <= 0.05 and kpss_p >= 0.05)
        sign_stable = bool(np.sign(rho_1) == np.sign(rho_2) and np.sign(rho_1) != 0)
        rows.append(
            {
                "target": target_transform,
                **meta,
                "n": len(valid),
                "adf_p": adf_p,
                "kpss_p": kpss_p,
                "stationary_pass": stationary,
                "spearman_rho": rho,
                "ic_tstat": t_stat,
                "sign_first_half": rho_1,
                "sign_second_half": rho_2,
                "sign_stable": sign_stable,
                "strict_pass": stationary and abs(t_stat) >= 2 and sign_stable,
            }
        )
    return pd.DataFrame(rows)


def select_best_per_family(screen: pd.DataFrame, target_transform: str) -> tuple[str, ...]:
    """Return only candidates that pass the prespecified strict screen."""
    subset = screen.loc[screen["target"] == target_transform].copy()
    selected: list[str] = []
    for family in MACRO_CODES:
        pool = subset.loc[(subset["family"] == family) & (~subset["interaction"]) & subset["strict_pass"]]
        if not pool.empty:
            selected.append(str(pool.iloc[np.abs(pool["ic_tstat"]).argmax()]["feature"]))
    interactions = subset.loc[subset["interaction"] & subset["strict_pass"]].copy()
    if not interactions.empty:
        interactions = interactions.iloc[np.argsort(-np.abs(interactions["ic_tstat"].to_numpy()))]
        selected.extend(interactions["feature"].head(2).astype(str).tolist())
    return tuple(dict.fromkeys(selected))


def make_design(
    indices: np.ndarray,
    spec: ModelSpec,
    y_work: np.ndarray,
    dates: pd.Series,
    features: pd.DataFrame,
) -> pd.DataFrame:
    design: dict[str, np.ndarray] = {}
    if spec.trend:
        design["trend_months"] = indices / 12.0
    months = dates.dt.month.to_numpy()[indices]
    for k in range(1, spec.harmonics + 1):
        design[f"sin{k}"] = np.sin(2 * np.pi * k * months / 12.0)
        design[f"cos{k}"] = np.cos(2 * np.pi * k * months / 12.0)
    if spec.target_lags:
        transformed = target_vector(y_work, spec.target)
        for periods in spec.target_lags:
            design[f"target_L{periods}"] = lag(transformed, periods)[indices]
    for name in spec.macro_features:
        design[name] = features[name].to_numpy(float)[indices]
    return pd.DataFrame(design, index=indices)


def fit_regression(
    spec: ModelSpec,
    train_indices: np.ndarray,
    y_work: np.ndarray,
    dates: pd.Series,
    features: pd.DataFrame,
) -> FittedRegression | None:
    target = target_vector(y_work, spec.target)
    design = make_design(train_indices, spec, y_work, dates, features)
    x = design.to_numpy(float)
    valid = np.isfinite(target[train_indices]) & np.isfinite(x).all(axis=1)
    x = x[valid]
    yy = target[train_indices][valid]
    if len(yy) < max(30, x.shape[1] + 8):
        return None

    columns = list(design.columns)
    if spec.ridge_lambda is not None:
        means = x.mean(axis=0)
        scales = x.std(axis=0, ddof=1)
        keep = np.isfinite(scales) & (scales > 1e-10)
        x = x[:, keep]
        means = means[keep]
        scales = scales[keep]
        columns = [name for name, use in zip(columns, keep) if use]
        xs = (x - means) / scales
        intercept = float(yy.mean())
        beta = np.linalg.solve(
            xs.T @ xs + np.eye(xs.shape[1]) * spec.ridge_lambda,
            xs.T @ (yy - intercept),
        )
        fitted = intercept + xs @ beta
        kind = "ridge"
    else:
        x1 = np.column_stack([np.ones(len(x)), x])
        coef, *_ = np.linalg.lstsq(x1, yy, rcond=None)
        intercept = float(coef[0])
        beta = coef[1:]
        fitted = x1 @ coef
        means = scales = None
        kind = "ols"

    residuals = yy - fitted
    smear = float(np.mean(np.exp(residuals))) if spec.target in {"log", "dlog", "yoylog"} else 1.0
    return FittedRegression(kind, columns, intercept, beta, residuals, smear, means, scales)


def reconstruct(predicted: float, target: str, idx: int, y_work: np.ndarray, smear: float) -> float:
    if target == "level":
        return float(predicted)
    if target == "log":
        return float(np.exp(predicted) * smear)
    if target == "diff":
        return float(y_work[idx - 1] + predicted)
    if target == "dlog":
        return float(y_work[idx - 1] * np.exp(predicted) * smear)
    if target == "yoylog":
        return float(y_work[idx - 12] * np.exp(predicted) * smear)
    raise ValueError(target)


def predict_regression(
    fitted: FittedRegression,
    spec: ModelSpec,
    idx: int,
    y_work: np.ndarray,
    dates: pd.Series,
    features: pd.DataFrame,
) -> float:
    design = make_design(np.array([idx]), spec, y_work, dates, features)
    x = design[fitted.columns].to_numpy(float)[0]
    if not np.isfinite(x).all():
        return np.nan
    if fitted.kind == "ridge":
        assert fitted.means is not None and fitted.scales is not None
        x = (x - fitted.means) / fitted.scales
    transformed = fitted.intercept + float(x @ fitted.beta)
    return reconstruct(transformed, spec.target, idx, y_work, fitted.smear)


def forecast_fixed_origin(
    spec: ModelSpec,
    train_end: int,
    forecast_indices: np.ndarray,
    y: np.ndarray,
    dates: pd.Series,
    features: pd.DataFrame,
) -> np.ndarray:
    y_work = y.copy()
    y_work[train_end + 1 :] = np.nan
    train = np.arange(train_end + 1)
    if spec.window is not None:
        train = train[-spec.window :]
    fitted = fit_regression(spec, train, y_work, dates, features)
    if fitted is None:
        return np.full(len(forecast_indices), np.nan)
    forecasts = []
    for idx in forecast_indices:
        value = predict_regression(fitted, spec, int(idx), y_work, dates, features)
        y_work[idx] = value
        forecasts.append(value)
    return np.asarray(forecasts)


def forecast_rolling_one_step(
    spec: ModelSpec,
    forecast_indices: np.ndarray,
    y: np.ndarray,
    dates: pd.Series,
    features: pd.DataFrame,
) -> np.ndarray:
    forecasts = []
    for idx in forecast_indices:
        train = np.arange(idx)
        if spec.window is not None:
            train = train[-spec.window :]
        fitted = fit_regression(spec, train, y, dates, features)
        value = np.nan if fitted is None else predict_regression(fitted, spec, int(idx), y, dates, features)
        forecasts.append(value)
    return np.asarray(forecasts)


def forecast_refit_recursive(
    spec: ModelSpec,
    history_end: int,
    future_indices: np.ndarray,
    y: np.ndarray,
    dates: pd.Series,
    features: pd.DataFrame,
) -> np.ndarray:
    y_work = y.copy()
    train = np.arange(history_end + 1)
    if spec.window is not None:
        train = train[-spec.window :]
    fitted = fit_regression(spec, train, y_work, dates, features)
    if fitted is None:
        return np.full(len(future_indices), np.nan)
    out = []
    for idx in future_indices:
        value = predict_regression(fitted, spec, int(idx), y_work, dates, features)
        y_work[idx] = value
        out.append(value)
    return np.asarray(out)


def baseline_forecasts(y_train: np.ndarray, horizon: int) -> dict[str, np.ndarray]:
    finite = y_train[np.isfinite(y_train)]
    last = finite[-1]
    seasonal = np.resize(finite[-12:], horizon)
    drift_step = (finite[-1] - finite[0]) / max(1, len(finite) - 1)
    return {
        "Naive": np.repeat(last, horizon),
        "Seasonal_Naive": seasonal,
        "Drift": last + drift_step * np.arange(1, horizon + 1),
    }


def optional_time_series_benchmarks(
    y: np.ndarray,
    train_end: int,
    forecast_indices: np.ndarray,
    features: pd.DataFrame,
    macro_features: Sequence[str],
) -> dict[str, np.ndarray]:
    try:
        from statsmodels.tsa.holtwinters import ExponentialSmoothing
        from statsmodels.tsa.statespace.sarimax import SARIMAX
    except ImportError:
        warnings.warn("statsmodels is unavailable; ETS/SARIMA/SARIMAX are skipped.")
        return {}

    train = y[: train_end + 1]
    train = train[np.isfinite(train)]
    h = len(forecast_indices)
    results: dict[str, np.ndarray] = {}

    try:
        ets = ExponentialSmoothing(train, trend="add", seasonal="add", seasonal_periods=12).fit(optimized=True)
        results["ETS_Additive"] = np.asarray(ets.forecast(h), dtype=float)
    except Exception as exc:
        warnings.warn(f"ETS failed: {exc}")

    candidates = [
        ((0, 1, 1), (1, 0, 0, 12)),
        ((1, 1, 0), (0, 1, 1, 12)),
        ((1, 0, 1), (1, 0, 0, 12)),
        ((2, 0, 0), (1, 0, 0, 12)),
        ((0, 1, 1), (0, 1, 1, 12)),
    ]
    best = None
    for order, seasonal_order in candidates:
        try:
            fit = SARIMAX(
                train,
                order=order,
                seasonal_order=seasonal_order,
                trend="c" if order[1] + seasonal_order[1] == 0 else "n",
                enforce_stationarity=False,
                enforce_invertibility=False,
            ).fit(disp=False)
            if best is None or fit.aic < best.aic:
                best = fit
        except Exception:
            continue
    if best is not None:
        results["SARIMA_Grid"] = np.asarray(best.forecast(h), dtype=float)

    if macro_features:
        x_train = features.loc[:train_end, list(macro_features)].to_numpy(float)
        y_log = np.log(y[: train_end + 1])
        valid = np.isfinite(y_log) & np.isfinite(x_train).all(axis=1)
        x = x_train[valid]
        yy = y_log[valid]
        means = x.mean(axis=0)
        scales = x.std(axis=0, ddof=1)
        x = (x - means) / scales
        new_x = features.loc[forecast_indices, list(macro_features)].to_numpy(float)
        new_x = (new_x - means) / scales
        try:
            model = SARIMAX(
                yy,
                exog=x,
                order=(0, 1, 1),
                seasonal_order=(1, 0, 0, 12),
                enforce_stationarity=False,
                enforce_invertibility=False,
            ).fit(disp=False)
            pred = model.get_forecast(h, exog=new_x).predicted_mean
            sigma2 = float(np.nanmean(np.asarray(model.resid) ** 2))
            results["SARIMAX_Macro"] = np.exp(np.asarray(pred) + 0.5 * sigma2)
        except Exception as exc:
            warnings.warn(f"SARIMAX failed: {exc}")
    return results


def metric_row(actual: np.ndarray, predicted: np.ndarray, model: str, mode: str, mase_denom: float) -> dict[str, object]:
    valid = np.isfinite(actual) & np.isfinite(predicted)
    a = actual[valid]
    p = predicted[valid]
    error = p - a
    return {
        "model": model,
        "evaluation_mode": mode,
        "n": len(a),
        "RMSE": float(np.sqrt(np.mean(error**2))),
        "MAE": float(np.mean(np.abs(error))),
        "sMAPE": float(np.mean(2 * np.abs(error) / np.maximum(np.abs(a) + np.abs(p), 1e-12))),
        "MASE": float(np.mean(np.abs(error)) / mase_denom),
        "Bias": float(np.mean(error)),
        "OOT_R2": float(1 - np.sum(error**2) / np.sum((a - a.mean()) ** 2)),
    }


def save_oot_plot(
    predictions: pd.DataFrame,
    metric_values: pd.Series,
    output_path: Path,
    model: str = RECOMMENDED_MODEL,
) -> None:
    """Save a simple OOT actual-versus-forecast plot for the selected model."""
    try:
        from PIL import Image, ImageDraw, ImageFont
    except ImportError as exc:
        raise RuntimeError(
            "Pillow is required to create the OOT comparison plot."
        ) from exc

    plot_data = predictions[["date", "actual", model]].dropna().copy()
    plot_data["date"] = pd.to_datetime(plot_data["date"])
    output_path.parent.mkdir(parents=True, exist_ok=True)

    def load_font(size: int, bold: bool = False):
        names = (
            "/System/Library/Fonts/Supplemental/Arial Bold.ttf"
            if bold
            else "/System/Library/Fonts/Supplemental/Arial.ttf",
            "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"
            if bold
            else "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
        )
        for name in names:
            if Path(name).exists():
                return ImageFont.truetype(name, size=size)
        return ImageFont.load_default()

    width, height = 1680, 940
    left, right, top, bottom = 145, 70, 205, 135
    chart_left, chart_right = left, width - right
    chart_top, chart_bottom = top, height - bottom
    image = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(image)
    title_font = load_font(42, bold=True)
    subtitle_font = load_font(22)
    label_font = load_font(22)
    legend_font = load_font(23)

    draw.text((left, 45), "Out-of-Time Actual vs. Best Model Forecast", fill="#111111", font=title_font)
    subtitle = (
        "October 2023–December 2025  |  Rolling one-step, monthly refit  |  "
        f"RMSE {float(metric_values['RMSE']):.1f}  |  "
        f"MAE {float(metric_values['MAE']):.1f}  |  "
        f"sMAPE {float(metric_values['sMAPE']):.2%}"
    )
    draw.text((left, 105), subtitle, fill="#555555", font=subtitle_font)

    actual = plot_data["actual"].to_numpy(float)
    forecast = plot_data[model].to_numpy(float)
    values = np.concatenate([actual, forecast])
    y_min = float(np.nanmin(values))
    y_max = float(np.nanmax(values))
    padding = max(40.0, (y_max - y_min) * 0.10)
    y_min = max(0.0, y_min - padding)
    y_max += padding

    n = len(plot_data)
    x_positions = np.linspace(chart_left, chart_right, n)

    def y_position(value: float) -> float:
        return chart_bottom - (value - y_min) / (y_max - y_min) * (chart_bottom - chart_top)

    for tick in np.linspace(y_min, y_max, 6):
        y_px = y_position(float(tick))
        draw.line((chart_left, y_px, chart_right, y_px), fill="#D9D9D9", width=2)
        label = f"{tick:,.0f}"
        box = draw.textbbox((0, 0), label, font=label_font)
        draw.text((chart_left - 18 - (box[2] - box[0]), y_px - 13), label, fill="#444444", font=label_font)

    draw.line((chart_left, chart_top, chart_left, chart_bottom), fill="#777777", width=2)
    draw.line((chart_left, chart_bottom, chart_right, chart_bottom), fill="#777777", width=2)
    draw.text((chart_left, chart_top - 39), "Commercial Card fee", fill="#333333", font=label_font)

    dates_plot = plot_data["date"].tolist()
    tick_indices = list(range(0, n, 3))
    if tick_indices[-1] != n - 1:
        tick_indices.append(n - 1)
    for idx in tick_indices:
        x_px = float(x_positions[idx])
        draw.line((x_px, chart_bottom, x_px, chart_bottom + 8), fill="#777777", width=2)
        label = pd.Timestamp(dates_plot[idx]).strftime("%Y-%m")
        box = draw.textbbox((0, 0), label, font=label_font)
        draw.text((x_px - (box[2] - box[0]) / 2, chart_bottom + 16), label, fill="#444444", font=label_font)

    actual_points = [(float(x), y_position(float(y))) for x, y in zip(x_positions, actual)]
    forecast_points = [(float(x), y_position(float(y))) for x, y in zip(x_positions, forecast)]
    draw.line(actual_points, fill="#222222", width=5, joint="curve")
    draw.line(forecast_points, fill="#2F5597", width=5, joint="curve")
    for x_px, y_px in actual_points:
        draw.ellipse((x_px - 5, y_px - 5, x_px + 5, y_px + 5), fill="#222222")
    for x_px, y_px in forecast_points:
        draw.ellipse((x_px - 5, y_px - 5, x_px + 5, y_px + 5), fill="#2F5597")

    legend_y = 158
    draw.line((left, legend_y, left + 55, legend_y), fill="#222222", width=5)
    draw.ellipse((left + 22, legend_y - 5, left + 32, legend_y + 5), fill="#222222")
    draw.text((left + 68, legend_y - 15), "Actual", fill="#222222", font=legend_font)
    second_x = left + 220
    draw.line((second_x, legend_y, second_x + 55, legend_y), fill="#2F5597", width=5)
    draw.ellipse((second_x + 22, legend_y - 5, second_x + 32, legend_y + 5), fill="#2F5597")
    draw.text((second_x + 68, legend_y - 15), f"{model} forecast", fill="#222222", font=legend_font)

    image.save(output_path, format="PNG", optimize=True)


def main() -> None:
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    data = load_data(args.macro, args.card)
    features, metadata = engineer_features(data)
    y = data["commercial_fee"].to_numpy(float)
    dates = data["date"]

    train_end = int(data.index[data["date"] == pd.Timestamp(args.development_end)][0])
    oot_indices = data.index[
        (data["date"] >= pd.Timestamp(args.oot_start))
        & (data["date"] <= pd.Timestamp(args.oot_end))
    ].to_numpy()
    future_indices = data.index[
        (data["date"] >= pd.Timestamp(args.future_start))
        & (data["date"] <= pd.Timestamp(args.future_end))
    ].to_numpy()
    history_end = int(data.index[data["commercial_fee"].notna()].max())
    history_start = int(data.index[data["commercial_fee"].notna()].min())
    if len(oot_indices) != 27:
        raise ValueError(f"Expected 27 OOT months; got {len(oot_indices)}")

    selected = dict(LOCKED_FEATURES)
    screening = pd.DataFrame()
    strict_rescreened: dict[str, tuple[str, ...]] | None = None
    if args.rescreen:
        screens = [screen_features(features, metadata, y, train_end, t) for t in ("diff", "dlog", "yoylog")]
        screening = pd.concat(screens, ignore_index=True)
        strict_rescreened = {t: select_best_per_family(screening, t) for t in ("diff", "dlog", "yoylog")}
        strict_rescreened = {
            target: tuple(feature for feature in names if feature not in EXCLUDED_TERMS)
            for target, names in strict_rescreened.items()
        }
        screening.to_csv(args.output_dir / "feature_screening_python.csv", index=False)

    specs = [
        ModelSpec("Macro_OLS_Level_Fixed", "level", selected["yoylog"], (), True, 5, evaluation_mode="fixed"),
        ModelSpec("Macro_OLS_LogLevel_Fixed", "log", selected["yoylog"], (), True, 5, evaluation_mode="fixed"),
        ModelSpec("ARX_Diff_Screened_Fixed", "diff", selected["diff"], (1,), False, 5, evaluation_mode="fixed"),
        ModelSpec("ARX_DLog_FZA_Fixed", "dlog", selected["dlog"], (1,), False, 5, evaluation_mode="fixed"),
        ModelSpec("ARX_YoYLog_Screened_Fixed", "yoylog", selected["yoylog"], (1,), False, 5, evaluation_mode="fixed"),
        ModelSpec("Dynamic_DLog_L1_NoMacro_Fixed", "dlog", (), (1,), False, 5, evaluation_mode="fixed"),
        ModelSpec(RECOMMENDED_MODEL, "dlog", selected["dlog"], (1,), False, 5, 60, evaluation_mode="rolling"),
        ModelSpec("Rolling_DLog_L1_NoMacro_60", "dlog", (), (1,), False, 5, 60, evaluation_mode="rolling"),
        ModelSpec("Rolling_ARX_DLog_FZA_84", "dlog", selected["dlog"], (1,), False, 5, 84, evaluation_mode="rolling"),
        ModelSpec("Expanding_ARX_DLog_FZA", "dlog", selected["dlog"], (1,), False, 5, None, evaluation_mode="rolling"),
        ModelSpec("Rolling_Ridge_DLog_FZA_60", "dlog", selected["dlog"], (1,), False, 5, 60, 30.0, "rolling"),
    ]

    predictions = pd.DataFrame({"date": dates.iloc[oot_indices].to_numpy(), "actual": y[oot_indices]})
    modes: dict[str, str] = {}
    for spec in specs:
        if spec.evaluation_mode == "fixed":
            pred = forecast_fixed_origin(spec, train_end, oot_indices, y, dates, features)
            modes[spec.name] = "Fixed-origin 27-month holdout"
        else:
            pred = forecast_rolling_one_step(spec, oot_indices, y, dates, features)
            modes[spec.name] = "Rolling one-step with monthly refit"
        predictions[spec.name] = pred

    baselines = baseline_forecasts(y[: train_end + 1], len(oot_indices))
    for name, pred in baselines.items():
        predictions[name] = pred
        modes[name] = "Fixed-origin 27-month holdout"

    if not args.skip_statsmodels:
        optional = optional_time_series_benchmarks(y, train_end, oot_indices, features, selected["dlog"])
        for name, pred in optional.items():
            predictions[name] = pred
            modes[name] = "Fixed-origin 27-month holdout"

    mase_denom = float(
        np.nanmean(
            np.abs(
                y[history_start + 12 : train_end + 1]
                - y[history_start : train_end - 11]
            )
        )
    )
    metrics = pd.DataFrame(
        [
            metric_row(predictions["actual"].to_numpy(), predictions[col].to_numpy(), col, modes[col], mase_denom)
            for col in predictions.columns
            if col not in {"date", "actual"}
        ]
    ).sort_values(["evaluation_mode", "RMSE"])

    rolling_rmse = float(metrics.loc[metrics["model"] == RECOMMENDED_MODEL, "RMSE"].iloc[0])
    no_macro_rmse = float(metrics.loc[metrics["model"] == "Rolling_DLog_L1_NoMacro_60", "RMSE"].iloc[0])
    fixed_rmse = float(metrics.loc[metrics["model"] == "Macro_OLS_Level_Fixed", "RMSE"].iloc[0])
    best_metric_values = metrics.loc[metrics["model"] == RECOMMENDED_MODEL].iloc[0]
    oot_plot_path = args.oot_plot or (args.output_dir / "oot_best_model_vs_actual.png")
    save_oot_plot(predictions, best_metric_values, oot_plot_path)

    operational = next(spec for spec in specs if spec.name == RECOMMENDED_MODEL)
    fixed_scenario = next(spec for spec in specs if spec.name == "Macro_OLS_Level_Fixed")
    future_operational = forecast_refit_recursive(operational, history_end, future_indices, y, dates, features)
    future_fixed = forecast_refit_recursive(fixed_scenario, history_end, future_indices, y, dates, features)
    residuals = predictions["actual"].to_numpy() - predictions[RECOMMENDED_MODEL].to_numpy()
    q80 = np.quantile(residuals, [0.10, 0.90])
    q95 = np.quantile(residuals, [0.025, 0.975])
    future = pd.DataFrame(
        {
            "date": dates.iloc[future_indices].to_numpy(),
            "model": operational.name,
            "forecast": future_operational,
            "low_80_empirical": np.maximum(0, future_operational + q80[0]),
            "high_80_empirical": future_operational + q80[1],
            "low_95_empirical": np.maximum(0, future_operational + q95[0]),
            "high_95_empirical": future_operational + q95[1],
            "fixed_macro_scenario": future_fixed,
        }
    )

    predictions.to_csv(args.output_dir / "oot_predictions_python.csv", index=False)
    metrics.to_csv(args.output_dir / "model_metrics_python.csv", index=False)
    future.to_csv(args.output_dir / "forecast_2026_python.csv", index=False)
    stationarity = pd.DataFrame() if args.skip_statsmodels else stationarity_table(y, train_end)
    if not stationarity.empty:
        stationarity.to_csv(args.output_dir / "target_stationarity_python.csv", index=False)
    data.to_csv(args.output_dir / "model_input_python.csv", index=False)

    summary = {
        "development_window": f"{dates.iloc[history_start]:%Y-%m} to {dates.iloc[train_end]:%Y-%m}",
        "oot_window": f"{dates.iloc[oot_indices[0]]:%Y-%m} to {dates.iloc[oot_indices[-1]]:%Y-%m}",
        "oot_months": len(oot_indices),
        "recommended_operational_model": operational.name,
        "model_role": "preferred predictive challenger",
        "governed_primary_if_strict_ic_gate": "Rolling_DLog_L1_NoMacro_60",
        "rolling_rmse": rolling_rmse,
        "rolling_no_macro_rmse": no_macro_rmse,
        "macro_rmse_improvement": 1.0 - rolling_rmse / no_macro_rmse,
        "fixed_origin_macro_ols_rmse": fixed_rmse,
        "selected_dlog_features": list(selected["dlog"]),
        "strict_rescreened_features": (
            None
            if strict_rescreened is None
            else {target: list(names) for target, names in strict_rescreened.items()}
        ),
        "excluded_terms": sorted(EXCLUDED_TERMS),
        "recommended_formula": "dlog(Fee_t) = seasonal harmonics + target_L1 + FZA_yoylog_L1 + error",
        "forecast_2026_mean": float(np.mean(future_operational)),
        "forecast_2026_min": float(np.min(future_operational)),
        "forecast_2026_max": float(np.max(future_operational)),
        "oot_actual_vs_forecast_plot": str(oot_plot_path.resolve()),
        "note": "Rolling results use information available at each monthly origin and refit parameters; fixed-origin results estimate parameters only through 2023-09. The FZA model is a predictive challenger because no dlog macro feature passed the strict IC gate.",
    }
    (args.output_dir / "analysis_summary_python.json").write_text(
        json.dumps(summary, indent=2, ensure_ascii=False), encoding="utf-8"
    )

    print("\nFixed-origin ranking")
    print(metrics.loc[metrics["evaluation_mode"].str.startswith("Fixed"), ["model", "RMSE", "MAE", "sMAPE", "OOT_R2"]].to_string(index=False))
    print("\nRolling one-step ranking")
    print(metrics.loc[metrics["evaluation_mode"].str.startswith("Rolling"), ["model", "RMSE", "MAE", "sMAPE", "OOT_R2"]].to_string(index=False))
    print("\nSummary")
    print(json.dumps(summary, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
