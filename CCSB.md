# Commercial Card and Small Business Fee

Target: [formula]. Sponsorship and QualPay are excluded from the target entirely. This document applies the Engle-Granger cointegration + error-correction approach: a long-run cointegrating levels regression, tested for a stationary residual, feeding an error-correction term into a short-run equation on the annual % change.

In-sample (estimation window, all steps): [variable] to [variable]. Out-of-time (backtest/validation only, never used to fit any coefficient): [variable] to [variable].

Engle-Granger workflow

- Long run

- Stationary test ADF and KPSS: combined and Macros mustn’t be stationary (no further transformation)

- Select macro factors

- Fit the model, calculate the residual EC

- EC must be stationary

- Shory run (modeling EC)

- Stationary test ADF and KPSS: Transformed combined and Macros must be stationary (diff, pct)

## Long-run step

### Screening raw levels for cointegration candidacy

Only variables that are non-stationary in levels ([formula]) are valid cointegration candidates — regressing a level on an already-stationary ([formula]) variable is not a cointegration test.

Raw-level ADF/KPSS (in-sample, [variable] to [variable]):

| Variable | ADF p | KPSS p | Classification |
| --- | --- | --- | --- |
| Combined (target) | [variable] | [variable] | [variable] |
| Unrate | [variable] | [variable] | [variable] |
| CPI | [variable] | [variable] | [variable] |
| DPI | [variable] | [variable] | [variable] |
| SBOpt | [variable] | [variable] | [variable] |
| GDP_Wholesale | [variable] | [variable] | [variable] |
| IP | [variable] | [variable] | [variable] |
| CorpProfits | [variable] | [variable] | [variable] |
| DJ_TotalMkt | [variable] | [variable] | [variable] |

Combined is confirmed non-stationary in levels (ADF p: [variable]). Unrate and Industrial Production are excluded from long-run candidacy; the remaining [variable] (CPI, DPI, SBOpt, GDP_Wholesale, CorpProfits, DJ_TotalMkt) are [formula] candidates.

### Forward selection

Forward selection proceeds directly on the [variable] [formula] candidates with no forced base term:

需要复现完整的 挨个试forward selection；变量顺序会影响p-value？

| Round | Added | New var<br>p-value | VIF ok | Residual ADF<br>p | Stationary? |
| --- | --- | --- | --- | --- | --- |
| [variable] | CPI | [variable] | [variable] | [variable] | [variable] |
| [variable] | SBOpt | [variable] | [variable] | [variable] | [variable] |

DJ_TotalMkt is not significant and all other failed VIF tests.

Adj R square ；significant；VIF

Final long-run model:

[formula]

| Term | Coefficient | p-value | VIF |
| --- | --- | --- | --- |
| const | [variable] | [variable] | - |
| CPI | [variable] | [variable] | [variable] |
| SBOpt | [variable] | [variable] | [variable] |

R² [variable], Adj. R² [variable], n [variable]. Residual (EC) ADF p [variable] → STATIONARY: genuine cointegration, not a spurious regression.

## Short-run step (ECM)

Note all the variables are either annual % change or level change (只有Unrate是level change，这里是[formula]). But please test month over month and quarter over quarter change for all variables including the dependent variable.

一致？

### Same mechanical DW problem

The base short-run equation ([formula], where y is Combined’s trailing-[variable]-month % change) fails Durbin-Watson (DW [variable]).

这里选EC_lag[variable]的原因就是因为这里的y是 [variable]-month%change

EC不用做diff或者%change

Y如果是Month over month %change选EC_lag[variable]

Y如果是Quarter over q %change 选EC_lag[variable]

### Stationarity of short-run transforms (relaxed OR rule)

| Variable | ADF p | KPSS p | Result |
| --- | --- | --- | --- |
| Unrate | [variable] | [variable] | [variable] |
| CPI | [variable] | [variable] | [variable] |
| DPI | [variable] | [variable] | [variable] |
| SBOpt | [variable] | [variable] | [variable] |
| GDP_Wholesale | [variable] | [variable] | [variable] |
| IP | [variable] | [variable] | [variable] |
| CorpProfits | [variable] | [variable] | [variable] |
| DJ_TotalMkt | [variable] | [variable] | [variable] |

### Partial-correlation screen (controlling for EC_lag[variable]), both windows, p: [variable], sign match  筛选那些在控制 EC_lag[variable] 后，仍与收入同比变化有关联的宏观变量

目前的列表中是[variable]month %change 和candidate的partial corr

还要添加 mom和qoq %change 和candidate的partial corr

[variable] of [variable] macro-lag candidates pass.

| Candidate | rho (IS) | p (IS) | rho (OOT) | p (OOT) |
| --- | --- | --- | --- | --- |
| Unrate_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| Unrate_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| Unrate_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| Unrate_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| Unrate_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| Unrate_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| Unrate_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| Unrate_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| DPI_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| GDP_Wholesale_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| GDP_Wholesale_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| IP_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| IP_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| CorpProfits_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| CorpProfits_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| CorpProfits_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| CorpProfits_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| CorpProfits_lag[variable] | [variable] | [variable] | [variable] | [variable] |
| CorpProfits_lag[variable] | [variable] | [variable] | [variable] | [variable] |

After capping at [variable] lags per family, [variable] macro candidates plus the [variable] target-lag candidates (y_lag[variable]–y_lag[variable]) form the forward-selection pool.

### Forward selection (base: [formula], always included)

复现全部的Forward selection？

| Round | Added | Adj. R² after |
| --- | --- | --- |
| [variable] | y_lag[variable] | [variable] |
| [variable] | y_lag[variable] | [variable] |
| [variable] | CorpProfits_lag[variable] | [variable] |
| [variable] | (none further pass at [variable]) | - |

Final short-run (ECM) model:

[formula]

[formula]

| Term | Coefficient | p-value | VIF |
| --- | --- | --- | --- |
| const | [variable] | [variable] | - |
| EC_lag[variable] | [variable] | [variable] | [variable] |
| y_lag[variable] | [variable] | [variable] | [variable] |
| y_lag[variable] | [variable] | [variable] | [variable] |
| CorpProfits_lag[variable] | [variable] | [variable] | [variable] |

R² [variable], Adj. R² [variable], n [variable], AIC [variable]. Durbin-Watson [variable] (passes cleanly). Jarque-Bera p [variable] (passes cleanly — no relaxation needed). All VIFs low. In-sample reconstructed-level accuracy: MAPE [variable], RMSE [variable].

## Out-of-time backtest

| Method | RMSE | MAPE |
| --- | --- | --- |
| Static, [variable]-step-ahead (true history) | [variable] | [variable] |
| Dynamic, fully recursive [variable]-month | [variable] | [variable] |

## Diagnosis: why EC underperforms out-of-time

The long-run relationship (long-run section), estimated only on [variable] data, does not hold going forward. The EC series is well-behaved in-sample (mean [variable], std [variable]) but drifts sharply and monotonically negative through the OOT window:

| Period | Mean EC |
| --- | --- |
| In-sample ([variable]) | [variable] (std [variable]) |
| OOT ([variable] to [variable]) | [variable] (std [variable]) |
| [variable] (partial) | [variable] |
| [variable] | [variable] |
| [variable] | [variable] |

## Formal structural break tests

[variable] independent formal tests confirm a genuine structural break in the CPI/SBOpt long-run relationship, not just noisy out-of-time performance: 这是对Long run模型做的

### CUSUM test (parameter stability, based on OLS residuals)

| Sample used | CUSUM statistic | p-value | Verdict |
| --- | --- | --- | --- |
| In-sample only ([variable]) | [variable] | [variable] | [variable] |
| Full sample ([variable]) | [variable] | [variable] | [variable] |

### Chow tests at candidate break dates

| Candidate break<br>date | F-statistic | p-value | n (pre / post) |
| --- | --- | --- | --- |
| [variable] | [variable] | [variable] | [variable] / [variable] |
| [variable] | [variable] | [variable] | [variable] / [variable] |
| [variable] | [variable] | [variable] | [variable] / [variable] |
| [variable] | [variable] | [variable] | [variable] / [variable] |

### Rolling-window coefficients: the relationship reverses sign, not just weakens

| Rolling [variable]-month windowending | CPI coefficient | SBOpt coefficient |
| --- | --- | --- |
| [variable] | [variable] | [variable] |
| [variable] | [variable] | [variable] |
| [variable] | [variable] | [variable] |
| [variable] | [variable] | [variable] |
| [variable] | [variable] | [variable] |
| [variable] | [variable] | [variable] |

## Baseline-plus-overlay approach

Given the strength of the break evidence in the structural break section, the short-run ECM’s approach is the wrong tool. The standard PPNR remedy for this situation is to separate them explicitly:

- Baseline: the long-run macro-implied level, using the CPI and SBOpt relationship as estimated on the confirmed-stable [variable] window.

- Overlay: a separately-modeled, explicitly documented adjustment capturing the post-[variable] deviation (EC).

### Is the deviation actually trend-like, or just noisy?

A simple linear trend fit to EC over the full OOT window ([variable] to [variable]) is highly significant and explains most of the variance: slope [variable]/month (p: [variable]), R² [variable].

### Genuine forward validation (not retrospective curve-fitting)

As a genuine test, the trend was instead fit using only the first [variable] OOT months ([variable] to [variable]) and extrapolated forward to predict the last [variable] months:

| Test | Result |
| --- | --- |
| Trend slope (fit on first [variable] months only) | [variable]/month, p: [variable] |
| MAPE on held-out last [variable] months (genuine forecast) | [variable] |
