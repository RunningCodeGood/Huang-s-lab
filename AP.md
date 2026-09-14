# Average Pay Error-Correction Model

Average compensation per employee modeled as an Engle-Granger [variable]-step error-correction model (ECM): a long-run equilibrium regression in levels, and a short-run regression in Y-o-Y transformed terms anchored on the long-run equilibrium gap.

## Feature Engineering, Stationarity (ADF/KPSS), and Univariate Screening

### Feature engineering

Quarters of total compensation, headcount, average pay ([formula]) and macro series are the candidates of regressors plus the target variable. [variable] additional ratios are derived: an implied avg-pay proxy ([formula]) and a comp share of output ([formula]).

- Y-o-Y % change (dependent variable and features): trending dollar/index-level series where a percentage growth rate is the natural unit.

- Y-o-Y level (point) change (features): the unemployment rate and the comp share of output - both are rates/shares, not trending levels, so a level change rather than a % change is the conventional transform.

- Each of the families is lagged [variable] quarters, giving candidate short-run regressors.

### Stationarity (ADF and KPSS)

[variable] distinct stationarity questions arise, and they point in OPPOSITE directions:

- Short-run candidates (the Y-o-Y transformed features above) should be stationary - differencing/growth-rate transforms are supposed to remove trends. Tested via ADF (null: unit root/non-stationary) and KPSS (null: stationary); a family is treated as usable unless BOTH tests agree it is non-stationary.

Long-run candidates (raw levels) should be the OPPOSITE - non-stationary ([formula], trending) is the precondition for a cointegrating relationship to be a meaningful concept:

### Univariate screening (Pearson correlation, in-sample vs. out-of-time)

In-sample window: through [variable]. Out-of-time (OOT) window: [variable] to [variable]. A candidate short-run regressor is screened by its PARTIAL correlation with the target after netting out the equilibrium gap (and any other base regressor already in the model) via residual-on-residual correlation. Note using Pearson so the screen tests the same LINEAR relationship the final OLS forward-selection tests.

The screen here uses a real p-value (both windows, sign-consistent) as a deliberately permissive PRE-filter - final acceptance is decided by forward selection's actual significance test.

## Long-Run Regression

Engle-Granger step [variable]: forward-selected over the non-stationary-in-levels macro candidates. Acceptance per candidate: significant, VIF for all included regressors, AND the resulting equation's residual must itself be stationary (ADF p). Levels of the non-stationary macros are themselves extremely collinear with each other (pairwise correlations - they are all riding the same growth/inflation trend), so forward selection (rather than fitting all at once) is used to avoid an ill-conditioned/near-singular design matrix.

### Result: Per Capita Income alone

### An alternative that was tested and set aside: [variable] other macro drivers (+ acquisition step dummy)

A separate run (candidate pool including a permanent acquisition level-shift dummy from the acquisition quarter onward) selected a multi-driver equation instead:

## Short-Run (Error-Correction) Regression

The Y-o-Y % change in pay is regressed on the lagged equilibrium gap, plus any short-run macro dynamics that survive the univariate screening section screen and forward selection.

### Result

No other of the candidates (any family, any lag) added incremental explanatory power that survived forward selection's significance/VIF gate on top of the equilibrium gap and the adopted driver's own contemporaneous growth.

In-sample reconstructed-level accuracy was assessed by comparing reconstructed levels against actuals.

## Diagnostics: Durbin-Watson and Jarque-Bera

Applied as hard forward-selection acceptance gates (short-run regression section), not just reported after the fact: Durbin-Watson (DW) (no material residual autocorrelation) and Jarque-Bera (JB) (residuals not significantly non-normal, relaxable if other diagnostics are strong).

Both diagnostics are notably worse in the alternative specification with the other [variable] macro drivers and the acquisition dummy (failing). The adopted specification is the only [variable] of those tested that clears every diagnostic gate simultaneously.

## Stability: Rolling and Expanding Window Testing

A [variable] static in-sample/OOT split is a weak test of stability when the OOT window is short (low statistical power). Rolling and expanding window tests use the full history far more efficiently.

### Window length: minimum [variable] quarters

Rule of thumb: [formula]. This is not just a theoretical floor - it matches exactly where the empirical results stabilize (below).

### Expanding (recursive) window - most efficient use of the data

Unlike a rolling window (fixed size W, drops the oldest observation each step), an expanding window starts at a minimum size and only adds observations, never discarding early history - by the final step it uses the entire sample.

## The FCB Acquisition: Dummy-Variable Experiment

The Florida Community Bank acquisition (closed at the start of the acquisition quarter) produces a [variable]-time step-change in both headcount (in a [variable] quarter) and average pay - a genuine structural break, documented in [variable] and revisited here. [variable] dummy specifications were tested as an alternative to excluding affected quarters outright (which would cost observations): a permanent step dummy (from the acquisition quarter onward) for the long-run/level equation, since the break is a permanent level shift, and a pulse dummy (only for the [variable] quarters following the acquisition) for the short-run/Y-o-Y equation, since that is the only window where a [variable]-time level shift distorts a quarter-differenced metric.
