# Average Compensation Modeling: Engle-Granger Error-Correction Model

## PPNR / CCAR Stress Testing Documentation — `CCC_Pay_Avg`

> **Data note:** Numerical values below are reproduced exactly as provided. Values that appear as `.` or are otherwise blank in the source have been retained as placeholders and should be completed from the final model output before formal submission.

---

## 1. Executive Summary

### Objective

Model average compensation per employee (`CCC_Pay_Avg`) using a two-step Engle-Granger error-correction model (ECM) for PPNR / CCAR stress testing.

### Final Model

**Long-run equilibrium equation**

$$
Pay_t = -,. + . \cdot FYPCPIQ_t + u_t
$$

**Short-run error-correction equation**

$$
y_t = . - . \cdot EC_{t-1} + . \cdot \Delta\%FYPCPIQ_t
$$

The model uses a single macroeconomic driver—per-capita income (`FYPCPIQ`)—in both stages.

### Performance

| Metric | Result |
|---|---:|
| Long-run $R^2$ | . |
| Residual ADF p-value | .e- |
| Short-run $R^2$ | . |
| Short-run adjusted $R^2$ | . |
| Short-run sample size |  |
| Durbin-Watson statistic | . |
| Jarque-Bera p-value | . |
| Maximum VIF | . |
| Reconstructed-level MAPE | .% |
| Reconstructed-level RMSE | $ |

### Conclusion

- The `FYPCPIQ`-anchored specification is the only tested specification that clears every diagnostic gate simultaneously.
- Coefficients are stable and sign-consistent across rolling-quarter windows, with `/` significant at the `%` level.
- The model is robust to the Florida Community Bank (FCB) acquisition structural break; tested dummy variables are unnecessary.

> **Recommendation:** Adopt the `FYPCPIQ`-anchored two-step ECM as the production PPNR compensation model.

---

## 2. Methodology: Two-Step Engle-Granger ECM

Average compensation per employee is defined as:

$$
CCC\_Pay\_Avg = \frac{CCC\_Comp}{CCC\_HC}
$$

It is modeled using a two-step Engle-Granger error-correction framework:

1. **Long-run equilibrium regression in levels:** estimate the equilibrium relationship between compensation and non-stationary macroeconomic drivers.
2. **Short-run regression in year-over-year transformed terms:** model short-run compensation growth using the lagged long-run equilibrium gap, $EC_{t-1}$.

### Data Universe

-  quarters of `CCC_Comp`, `CCC_HC`, `CCC_Pay_Avg`, and  macroeconomic series.
- Two derived Finance & Insurance sector ratios:
  - `fin_avg_pay` = Wages & Salaries / Employment
  - `fin_avg_exp` = Wages & Salaries / GDP

### Modeling Challenges

- The  non-stationary macro candidates are extremely collinear, with pairwise correlations of `.–.`. They largely reflect the same growth and inflation trend.
- The FCB acquisition, which closed on `--`, produced a one-time headcount step-change of `+%` in a single quarter.

Forward selection is used instead of fitting all macro candidates simultaneously, thereby avoiding an ill-conditioned, near-singular design matrix.

### Diagnostic Acceptance Gates

The following gates are applied during forward selection, rather than merely reported after model selection:

| Gate | Acceptance criterion |
|---|---|
| Statistical significance | `%` level |
| Multicollinearity | VIF `< ` |
| Residual serial correlation | Durbin-Watson in `[., .]` |
| Residual normality | Jarque-Bera p-value `> %` |
| Long-run cointegration | Residual stationarity required |

---

## 3. Stationarity Testing: Two Opposite Requirements

The two stages impose different stationarity requirements:

- **Short-run candidates:** year-over-year transforms must be stationary because growth-rate transformations are expected to remove stochastic trends.
- **Long-run candidates:** raw levels must be non-stationary and integrated of order one, I(1), as a precondition for a meaningful cointegrating relationship.

### Short-Run Family Screen

| Result | Series |
|---|---|
| Failed—non-stationary | `FAHEFI_B.IUSA`, `FPCNBCPH_B.IUSA`, `FECICCQ_B.IUSA` |
| Passed—stationary and usable | `FLBR_B.IUSA`, `FLBOPQ_B.IUSA`, `FYPEWSQ_B.IUSA`, `FGDPQ_B.IUSA`, `FE_B.IUSA`, `FYPCPIQ_B.IUSA` |

### Long-Run Candidate Screen

| Result | Series |
|---|---|
| I(1)—valid long-run candidates | `CCC_Pay_Avg`, `FAHEFI_B.IUSA`, `FPCNBCPH_B.IUSA`, `FLBOPQ_B.IUSA`, `FYPEWSQ_B.IUSA`, `FGDPQ_B.IUSA`, `FE_B.IUSA`, `FYPCPIQ_B.IUSA`, `FECICCQ_B.IUSA` |
| Excluded—stationary in levels | `FLBR_B.IUSA` |

Screening based on in-sample versus out-of-time performance, `p < .`, and sign consistency is intentionally permissive. Final acceptance is determined by forward selection's actual `%` significance test.

---

## 4. Long-Run Regression: Engle-Granger Step 1

The long-run equation is:

$$
Pay_t = \beta_0 + \sum_i \beta_i X_{i,t} + u_t
$$

Forward selection is conducted over the  non-stationary macroeconomic candidates. Each candidate must satisfy all of the following:

- Significant at the `%` level.
- VIF `< `.
- Equation residual is stationary, with ADF `p < .`.

### Adopted Specification: `FYPCPIQ` Alone

$$
Pay_t = -,. + . \cdot FYPCPIQ_t + u_t
$$

| Metric | Result |
|---|---:|
| $R^2$ | . |
| Sample size |  |
| Residual ADF p-value | .e- |

The stationary residual confirms a genuine cointegrating relationship.

### Alternative Specification Considered

`FAHEFI + FLBOPQ + FCB step dummy` was also cointegrated, but it has higher VIFs and requires an additional structural-break dummy.

### Rationale for Selecting `FYPCPIQ`

- It is the simplest specification, using one economically intuitive driver.
- It satisfies every diagnostic gate that the alternative fails.
- It avoids a structural-break dummy that subsequent testing shows to be statistically redundant.

> The `FYPCPIQ`-anchored specification is the only tested specification that clears every diagnostic gate simultaneously.

---

## 5. Short-Run Error-Correction Regression

The dependent variable is year-over-year compensation growth:

$$
y_t = \Delta \ln(Pay_t)
$$

It is regressed on the lagged equilibrium gap, $EC_{t-1} = u_{t-1}$, and any short-run dynamics that survive the univariate screen:

$$
y_t = . - . \cdot EC_{t-1} + . \cdot \Delta\%FYPCPIQ_t
$$

### Coefficients and Fit

| Term or metric | Result |
|---|---:|
| Constant | . |
| $EC_{t-1}$ coefficient | -. |
| `FYPCPIQ` year-over-year change coefficient | . |
| Coefficient significance | All `p < .` |
| $R^2$ | . |
| Adjusted $R^2$ | . |
| Sample size |  |

None of the other  candidates—across any family or lag from  to —adds incremental explanatory power after applying the `%` significance and VIF gates on top of $EC_{t-1}$ and contemporaneous `FYPCPIQ` growth.

### Reconstructed-Level Accuracy

Forecast compensation levels are reconstructed as:

$$
Pay_t = Pay_{t-1} \times (1 + \widehat{forecast\%}_t)
$$

| Accuracy metric | Result |
|---|---:|
| MAPE | .% |
| RMSE | $ |

The equilibrium-correction coefficient is negative and statistically significant, confirming that compensation reverts toward the long-run level implied by `FYPCPIQ`.

---

## 6. Diagnostics

Durbin-Watson, Jarque-Bera, and VIF requirements are enforced as hard forward-selection acceptance gates.

| Diagnostic | Result | Threshold | Status |
|---|---:|---:|---|
| Durbin-Watson | . | `[., .]` | Pass |
| Jarque-Bera p-value | . | `> .` | Pass |
| Maximum VIF | . | `< ` | Pass |

Both residual diagnostics are notably worse for the `FAHEFI + FLBOPQ + fcb_step` alternative; in particular, its Durbin-Watson statistic of `.` fails the acceptance criterion.

### FCB Dummy-Variable Robustness Check

| Specification | Finding |
|---|---|
| `FYPCPIQ` alone, without FCB control—adopted | Best result across all diagnostics |
| `FYPCPIQ` plus `fcb_pulse` | `fcb_pulse` is insignificant (`p = .`) and redundant |
| `FAHEFI + FLBOPQ` plus FCB step/pulse controls | Durbin-Watson fails at `.`; worse on every metric |

A permanent FCB acquisition step dummy adds no incremental value once the `FYPCPIQ`-anchored equilibrium term captures the level shift.

---

## 7. Stability: Rolling and Expanding Window Testing

A single static in-sample/out-of-time split is a weak stability test when the out-of-time window contains only  quarters. Rolling and expanding-window tests use the available history more efficiently.

### 7.1 Rolling Window

**Minimum window length:  quarters**

The rule of thumb is:

$$
Minimum\ sample\ size \approx  + k
$$

where $k = $ predictors: $EC_{t-1}$ and the `FYPCPIQ` driver. This produces a minimum window of , which also matches the point where the empirical results stabilize.

**Results**

-  rolling -quarter windows.
- Coefficient range: `[., .]`.
- No coefficient sign reversals.
- Significant at the `%` level in  of  windows.

### 7.2 Expanding (Recursive) Window

Unlike a fixed-size rolling window, which drops the oldest observation at each step, an expanding window begins at a minimum size and adds observations without discarding early history. The final step therefore uses the entire sample.

Overall, coefficients remain stable without sign reversals across  rolling windows, and  of  estimates remain significant at the `%` level.

---

## 8. Model Risk Assessment

### Strengths

- **Economically intuitive:** per-capita income drives long-run compensation.
- **Parsimonious:** one long-run driver and two short-run terms.
- **Stable:** no coefficient sign reversals across  rolling -quarter windows.
- **Regulator-consistent:** genuine cointegration is confirmed by a residual ADF p-value of `.e-`.
- **Interpretable:** model mechanics and economic rationale are transparent.

### Risk Review

| Risk area | Assessment |
|---|---|
| Multicollinearity | Low; maximum VIF is `.` |
| Residual serial correlation | None indicated; Durbin-Watson is `.` |
| Residual normality | Acceptable; Jarque-Bera p-value is `.` |
| FCB acquisition structural break | Explicitly tested; controls are redundant |
| Overfitting | Low risk; – parameters against `n = –` |
| Interpretability | High |

All hard acceptance gates—statistical significance, VIF, Durbin-Watson, Jarque-Bera, and cointegration—were applied prospectively during forward selection rather than selected after the fact.

---

## 9. Conclusion and Recommendation

- Adopt the two-step Engle-Granger ECM anchored on per-capita income (`FYPCPIQ`) as the final compensation specification.
- Genuine cointegration is confirmed, and the short-run equation passes every diagnostic acceptance gate.
- Coefficients are stable across rolling and expanding windows spanning the full sample.
- The model is robust to the FCB acquisition structural break; tested dummy-variable alternatives are unnecessary.

> **Final recommendation:** Use the `FYPCPIQ`-anchored two-step ECM as the production model for PPNR / CCAR compensation forecasting.
