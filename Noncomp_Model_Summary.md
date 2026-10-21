# Noncomp Efficiency Ratio Model Summary

## Development Data

- L-SNV quarterly Noncomp EF data during 2011 through 2025.
- Moody's quarterly Macro data during 2011 through 2025, with S1 and S4 scenario paths through 1Q28.

## Methodology

### 1. Target Variable

Quarterly Noncomp Efficiency Ratio in levels: `y(t) = EF(t)`.

### 2. Modeling Approach

ARDL regression with one lag of the target variable, a post-1Q14 period indicator, and the one-quarter-lagged quarterly change in unemployment. A separate fixed 1Q14 level-and-trend break analysis supports the level specification.

### 3. Selection and Assessment

Screen 22 macro factors using development data. Evaluate fitted in-sample, one-step OOT and recursive OOT performance. Refit the fixed specification through 4Q25 for S1 and S4 forecasts.

## Assumptions

- Historical relationships among EF, lagged EF and unemployment changes remain applicable over the forecast horizon.
- The post-1Q14 operating regime and the one-quarter response to unemployment changes persist with the estimated magnitudes.

## Limitations and Weaknesses

- Unemployment change is the only selected macroeconomic driver. Separate expense and income effects, business initiatives, accounting changes and other economic transmission channels are not explicitly modeled.
