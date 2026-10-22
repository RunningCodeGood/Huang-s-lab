# Development and Validation Across Three Models

## 01 | Card Fee — Card Fee Revenue

**Objective:** Forecast card fee revenue

1. Reconciled revenue definitions and resolved historical data anomalies.
2. Applied segment-specific methods: an Engle–Granger error-correction model for Commercial, an ARDL model for Consumer, and a rule-based benchmark for Debit.
3. Validated performance through backtesting, sensitivity analysis, and stress scenarios.

**Deliverables:** Reproducible code, model documentation, and review deck.

## 02 | Comp — Bonus Ratio

**Objective:** Assess the feasibility of a quantitative bonus-ratio model

1. Reconstructed the bonus ratio and validated the historical inputs.
2. Implemented and compared ARDL specifications using alternative macroeconomic drivers, focusing on diagnostics, forecast performance, economic consistency, and interpretability.
3. Documented evidence supporting non-adoption of the quantitative model and the use of a qualitative approach.

**Deliverables:** Model comparison results, test code, and non-adoption rationale.

## 03 | Noncomp — Efficiency Ratio

**Objective:** Forecast the efficiency ratio

1. Corrected confirmed outliers and established trend-break stationarity after controlling for fixed 2014 level and slope shifts.
2. Developed and compared ARDL specifications using alternative macroeconomic drivers, selecting unemployment based on diagnostic quality, forecast performance, and economic interpretability.
3. Validated model stability through one-step and recursive backtesting, sensitivity analysis, and stress forecasts.

**Deliverables:** Reproducible code, model documentation, and review deck.

## Shared Development Framework

Define scope → Rebuild and validate data → Develop and compare models → Backtest and stress-test → Document and reproduce
