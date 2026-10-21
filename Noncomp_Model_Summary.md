# Noncomp Efficiency Ratio Model Summary

## Development Data

- Quarterly Noncomp Efficiency Ratio, or EF, and 22 macroeconomic series from 1Q11 through 4Q25.
- Development sample: 3Q11-3Q23, with 49 usable observations.
- Out of time period: 4Q23-4Q25, covering nine quarters.
- The fixed specification is refitted through 4Q25 using 58 observations. S1 and S4 unemployment paths cover 1Q26-1Q28.

## Methodology

### 1. Target Variable and Data Preparation

- The target variable is quarterly Noncomp EF, modeled in ratio levels.
- The 3Q17 and 4Q17 observations are replaced with documented local averages.
- A fixed period indicator equals zero through 4Q13 and one from 1Q14 onward.
- A separate level and trend break analysis describes the historical change around 1Q14. ADF and KPSS tests support stationary residuals around the fitted paths.

### 2. ARDL Model Development

- The starting equation includes a constant, the period indicator, a linear trend and lagged EF.
- The 22 macroeconomic candidates enter as quarterly changes or log changes with a one quarter lag.
- Forward selection requires coefficient p < 0.05, VIF < 10, Durbin-Watson between 1.5 and 2.5, and Jarque-Bera p > 0.05.
- Lagged unemployment change is selected. The linear trend is removed because its p value is 0.1719.
- The final specification is:

  `EF(t) = c + delta D(t) + phi EF(t-1) + lambda Delta U(t-1) + epsilon(t)`

- The fixed specification is refitted through 4Q25 and used recursively to produce the S1 and S4 forecasts.

## Assumptions

- The EF accounting definition and source data remain comparable over time.
- The post 2014 operating regime remains relevant over the forecast horizon.
- Lagged EF continues to capture persistence in operating efficiency.
- The positive conditional relationship between unemployment deterioration and EF remains stable.
- The 1Q14 boundary, 2017 smoothing treatment, macro transformations and one quarter lag remain fixed.
- Scenario labels, units and quarter end dates are accurate. Forecasts set future residuals to zero and contain no management overlay.

## Limitations and Weaknesses

- The development sample contains only 49 usable observations, and the out of time assessment covers nine quarters.
- The 2017 smoothing treatment introduces judgment and affects both current and lagged EF.
- Pandemic unemployment movements create influential observations that can affect coefficient significance.
- GDP growth and the output gap produce similar model fit and forecast performance, so the macroeconomic variable choice is not statistically decisive.
- The aggregate EF model cannot separately identify expense effects, income effects, business initiatives or accounting changes.
- Forecasts are conditional point estimates based on supplied unemployment paths and do not provide a probability range.
- The out of time results contributed to model selection. Future forecast vintages are needed to provide additional independent performance evidence.
