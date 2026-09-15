#!/usr/bin/env python3
"""Reproduce and audit the Combined fee models in the reference document.

Combined = Small Business + Commercial Card.
The short-run target is 100 * (Combined[t] / Combined[t-12] - 1).
EC_lag12 is the 12-month lag of the frozen in-sample long-run residual.
Reference values are comparison targets only; coefficients are re-estimated.
"""

# %% Configuration and imports
from pathlib import Path
from datetime import datetime, date
from decimal import Decimal
from importlib import metadata
import argparse
import hashlib
import json
import platform
import re
import sys
import traceback
import warnings

import numpy as np
import pandas as pd
import scipy
from scipy import stats
import statsmodels
import statsmodels.api as sm
from statsmodels.tsa.stattools import adfuller, kpss, coint
from statsmodels.stats.stattools import durbin_watson, jarque_bera
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.stats.diagnostic import breaks_cusumolsresid, acorr_breusch_godfrey

ROOT = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
DEFAULT_CARD = ROOT / "Cardfee.csv"
DEFAULT_MACRO = ROOT / "Macro_Data_201001-202612.csv"
CARD_HEADER, MACRO_HEADER = 0, 0
CSV_ENCODING = "utf-8-sig"
CSV_SEPARATOR = ","
DATE_COLUMN = "YYYYMM"
CARD, MACRO, OUT = DEFAULT_CARD, DEFAULT_MACRO, None


def runtime_info():
    """Record installed versions for provenance without enforcing an environment."""
    packages = {}
    for name in ("numpy", "pandas", "scipy", "statsmodels"):
        try:
            packages[name] = metadata.version(name)
        except metadata.PackageNotFoundError:
            packages[name] = None
    return dict(python=sys.version, executable=sys.executable,
                platform=platform.platform(), machine=platform.machine(),
                packages=packages)


START, CUT, END = '2011-01-01', '2023-09-01', '2025-12-01'
MAPPING = {
    'Unrate': 'M_FLBR_B.IUSA', 'CPI': 'M_FCPIU_B.IUSA',
    'DPI': 'M_FYPDPIQ_B.IUSA', 'SBOpt': 'M_FSBINQ_B.IUSA',
    'GDP_Wholesale': 'M_FGDP42Q_B.IUSA', 'IP': 'M_FIP_B.IUSA',
    'CorpProfits': 'M_FZA_B.IUSA', 'DJ_TotalMkt': 'M_FDJMIIUSDD_WCFTDQ_B.IUSA',
}
LR_CANDIDATES = ['CPI', 'DPI', 'SBOpt', 'GDP_Wholesale', 'CorpProfits', 'DJ_TotalMkt']
LR_TERMS = ['CPI', 'SBOpt']
SR_TERMS = ['EC_lag12', 'y_lag1', 'y_lag6', 'CorpProfits_lag3']


def save(name, frame):
    frame.to_csv(OUT / f'{name}.csv', index=False, encoding='utf-8-sig', float_format='%.17g')


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def fit(d, target, terms):
    z = d[[target] + terms].dropna()
    r = sm.OLS(z[target], sm.add_constant(z[terms], has_constant='add')).fit()
    return r


def diagnostics(r):
    jb = jarque_bera(r.resid)
    return dict(n=int(r.nobs), start=str(r.resid.index.min())[:10],
                end=str(r.resid.index.max())[:10], R2=r.rsquared,
                adj_R2=r.rsquared_adj, AIC=r.aic, DW=durbin_watson(r.resid),
                JB=jb[0], JB_p=jb[1])


def vifs(r):
    return {name: variance_inflation_factor(r.model.exog, j)
            for j, name in enumerate(r.params.index) if name != 'const'}


def coef(r, name):
    vf = vifs(r)
    return [dict(model=name, term=t, coefficient=r.params[t], SE=r.bse[t],
                 t=r.tvalues[t], p=r.pvalues[t], VIF=vf.get(t, np.nan))
            for t in r.params.index]


def stationarity(x, **adf_options):
    x = x.dropna()
    a = adfuller(x, regression='c', autolag='AIC', result_object=False, **adf_options)
    with warnings.catch_warnings(record=True) as ws:
        warnings.simplefilter('always')
        k = kpss(x, regression='c', nlags='auto', result_object=False)
    bound = '<=' if k[1] == .01 else '>=' if k[1] == .1 else '='
    return dict(n=len(x), start=str(x.index.min())[:10], end=str(x.index.max())[:10],
                ADF_stat=a[0], ADF_p=a[1], ADF_lags=a[2], ADF_nobs=a[3],
                KPSS_stat=k[0], KPSS_p=k[1], KPSS_bound=bound, KPSS_lags=k[2],
                OR_pass=bool(a[1] < .05 or k[1] > .05),
                AND_pass=bool(a[1] < .05 and k[1] > .05))


# %% 1. Input data, calendar alignment, and macro dictionary
MACRO_DESCRIPTIONS = [
    dict(group="Core", variable="Unrate", source_column=MAPPING["Unrate"],
         description="Household Survey: Unemployment Rate, (%, SA)",
         short_run_transform="diff(12), percentage points"),
    dict(group="Core", variable="CPI", source_column=MAPPING["CPI"],
         description="CPI: Urban Consumer - All Items, (Index 1982-84=100, SA)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Core", variable="DPI", source_column=MAPPING["DPI"],
         description="Income: Disposable Personal, (Bil. USD, SAAR)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Small Business", variable="SBOpt", source_column=MAPPING["SBOpt"],
         description="Small business: Normalized optimism index, (Index 1986=100, SA)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Small Business", variable="GDP_Wholesale", source_column=MAPPING["GDP_Wholesale"],
         description="Gross Product Originating: Wholesale Trade, (Bil. USD, SAAR)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="IP", source_column=MAPPING["IP"],
         description="Industrial Production: Total, (Index 2017=100, SA)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="CorpProfits", source_column=MAPPING["CorpProfits"],
         description="Corporate Profits with IVA and CCAdj: Profits After Tax with IVA and CCAdj, (Bil. USD, SAAR)",
         short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="DJ_TotalMkt", source_column=MAPPING["DJ_TotalMkt"],
         description="Dow Jones U.S. total stock market index - End of period, (Index, NSA)",
         short_run_transform="pct_change(12) * 100"),
]


def normalize_month(value):
    """Normalize date objects, numeric/string YYYYMM, and ISO dates to month start."""
    if pd.isna(value):
        raise ValueError("YYYYMM contains a missing date.")
    if isinstance(value, (pd.Timestamp, datetime, date, np.datetime64)):
        ts = pd.Timestamp(value)
    else:
        token = str(value).strip()
        if re.fullmatch(r"\d{6}(\.0+)?", token):
            ts = pd.to_datetime(token[:6], format="%Y%m")
        elif re.fullmatch(r"\d{4}[-/]\d{1,2}([-/]\d{1,2})?([ T].*)?", token):
            ts = pd.Timestamp(token)
        else:
            raise ValueError(f"Unsupported YYYYMM value: {value!r}. Expected YYYYMM or an ISO date.")
    return ts.to_period("M").to_timestamp()


def read_monthly(path, header, columns, first_month, last_month):
    """Read monthly CSV data without imputation or duplicate aggregation."""
    path = Path(path)
    if not path.is_file():
        raise FileNotFoundError(f"Input file not found: {path}")
    if path.suffix.lower() != ".csv":
        raise ValueError(f"Expected a CSV file: {path}")
    raw = pd.read_csv(
        path,
        header=header,
        encoding=CSV_ENCODING,
        sep=CSV_SEPARATOR,
        engine="c",
        float_precision="round_trip",
        low_memory=False,
    ).dropna(how="all")
    raw.columns = raw.columns.str.strip()
    if raw.columns.duplicated().any():
        raise ValueError(f"{path.name}: duplicate column names after trimming whitespace.")
    required = [DATE_COLUMN] + list(columns)
    missing = [name for name in required if name not in raw.columns]
    if missing:
        raise ValueError(f"{path.name}: missing columns {missing}; available columns: {list(raw.columns)}")
    chosen = raw[required].copy()
    chosen.index = pd.DatetimeIndex([normalize_month(v) for v in chosen.pop(DATE_COLUMN)], name="date")
    if chosen.index.duplicated().any():
        duplicates = chosen.index[chosen.index.duplicated()].strftime("%Y-%m").tolist()
        raise ValueError(f"{path.name}: duplicate months are not aggregated: {duplicates}")
    was_sorted = chosen.index.is_monotonic_increasing
    chosen = chosen.sort_index()
    months = pd.date_range(first_month, last_month, freq="MS")
    absent = months.difference(chosen.index)
    if len(absent):
        raise ValueError(f"{path.name}: missing required months: {absent.strftime('%Y-%m').tolist()}")
    selected = chosen.loc[months].apply(pd.to_numeric, errors="raise").astype(float)
    finite = np.isfinite(selected.to_numpy())
    if not finite.all():
        r, c = np.where(~finite)
        examples = [(str(selected.index[i].date()), selected.columns[j]) for i, j in zip(r[:10], c[:10])]
        raise ValueError(f"{path.name}: missing or infinite values at {examples}; no imputation applied.")
    record = dict(file=path.name, format="CSV", encoding=CSV_ENCODING, separator=CSV_SEPARATOR,
                  header_zero_based=header, rows_read=len(raw),
                  rows_used=len(selected), rows_outside_scope=len(raw)-len(selected),
                  first_month=first_month, last_month=last_month, input_sorted=was_sorted,
                  duplicate_months=0, missing_months=0, invalid_required_values=0)
    return selected, record


def load_data():
    c, cq = read_monthly(CARD, CARD_HEADER,
                         ["Commercial Card", "Small Business"], START, END)
    m, mq = read_monthly(MACRO, MACRO_HEADER,
                         list(MAPPING.values()), "2010-01-01", END)
    d = m.rename(columns={v: k for k, v in MAPPING.items()}).copy()
    d["Commercial Card"] = c["Commercial Card"]
    d["Small Business"] = c["Small Business"]
    d["Combined"] = d["Commercial Card"] + d["Small Business"]
    if not (d.loc[START:, "Combined"] > 0).all():
        raise ValueError("Combined must be positive for the document's YoY and MAPE calculations.")
    if (d[[name for name in MAPPING if name != "Unrate"]] == 0).any().any():
        raise ValueError("A macro level used as a percentage-change denominator is zero.")
    save("input_validation", pd.DataFrame([cq, mq]))
    # Keep 2010 history for an explicit sensitivity only. Document correlation
    # values identify transformation after aligning to target history (2011+).
    for name in MAPPING:
        x = d.loc[START:, name]
        d[name + '_yoy'] = x.diff(12) if name == 'Unrate' else x.pct_change(12, fill_method=None) * 100
    d['y'] = d.Combined.pct_change(12, fill_method=None) * 100
    lr = fit(d.loc[START:CUT], 'Combined', LR_TERMS)
    d['baseline'] = lr.predict(sm.add_constant(d[LR_TERMS], has_constant='add'))
    d['EC'] = d.Combined - d.baseline
    d['EC_lag12'] = d.EC.shift(12)
    features = {}
    for name in MAPPING:
        for lag in range(13):
            features[f'{name}_lag{lag}'] = d[name + '_yoy'].shift(lag)
    for lag in range(1, 13):
        features[f'y_lag{lag}'] = d.y.shift(lag)
    d = pd.concat([d, pd.DataFrame(features)], axis=1)
    return d, lr


def partial(d, candidate, df_adjust=False):
    z = d[['y', 'EC_lag12', candidate]].dropna()
    control = sm.add_constant(z[['EC_lag12']], has_constant='add')
    ry = sm.OLS(z.y, control).fit().resid
    rx = sm.OLS(z[candidate], control).fit().resid
    rho = stats.pearsonr(ry, rx).statistic
    # Document appears to use Pearson's p on residuals; compare proper n-3 df.
    dof = len(z) - (3 if df_adjust else 2)
    t = rho * np.sqrt(dof / (1 - rho * rho))
    return dict(rho=rho, p=2 * stats.t.sf(abs(t), dof), n=len(z), df=dof,
                start=str(z.index.min())[:10], end=str(z.index.max())[:10])


def screen(d, common=False):
    names = [f'{m}_lag{l}' for m in MAPPING for l in range(13)]
    z = d.dropna(subset=['y', 'EC_lag12'] + names) if common else d
    rows = []
    for name in names:
        a = partial(z.loc[START:CUT], name)
        b = partial(z.loc['2023-10-01':END], name)
        ac = partial(z.loc[START:CUT], name, True)
        bc = partial(z.loc['2023-10-01':END], name, True)
        same = np.sign(a['rho']) == np.sign(b['rho'])
        rows.append(dict(candidate=name, family=name.rsplit('_lag', 1)[0], lag=int(name.rsplit('lag', 1)[1]),
                         **{f'{k}_IS': v for k, v in a.items()},
                         **{f'{k}_OOT': v for k, v in b.items()},
                         p_IS_corrected=ac['p'], p_OOT_corrected=bc['p'], sign_match=same,
                         passed=bool(a['p'] < .2 and b['p'] < .2 and same),
                         passed_corrected=bool(ac['p'] < .2 and bc['p'] < .2 and same)))
    return pd.DataFrame(rows)


def forward(d, target, candidates, base=(), vif_limit=10., common=False, all_p=False, residual_gate=None):
    selected = list(base)
    pending = list(candidates)
    records, rounds = [], []
    frame = d.dropna(subset=[target] + selected + pending) if common else d
    for round_no in range(1, len(candidates) + 1):
        trials = []
        for candidate in pending:
            r = fit(frame, target, selected + [candidate])
            vf = vifs(r)
            max_p = r.pvalues.drop('const').max()
            eligible = r.pvalues[candidate] < .05 and max(vf.values()) < vif_limit
            if all_p:
                eligible = eligible and max_p < .05
            previous = fit(frame.loc[r.resid.index], target, selected)
            rec = dict(round=round_no, candidate=candidate, selected_before=' + '.join(selected),
                       beta_added=r.params[candidate], p_added=r.pvalues[candidate], max_p=max_p,
                       max_VIF=max(vf.values()), all_VIF=json.dumps(vf),
                       eligible=bool(eligible), p_pass=bool(r.pvalues[candidate] < .05),
                       VIF_pass=bool(max(vf.values()) < vif_limit),
                       adj_R2_gain_same_sample=r.rsquared_adj - previous.rsquared_adj,
                       **diagnostics(r))
            if target == 'Combined':
                rec['residual_ADF_p'] = adfuller(r.resid, regression='c', autolag='AIC', result_object=False)[1]
                rec['EG_p'] = float(coint(frame.loc[r.resid.index, target], frame.loc[r.resid.index, selected + [candidate]], trend='c', autolag='aic')[1])
                if residual_gate is not None:
                    rec['residual_gate_pass'] = bool(rec[residual_gate] < .05)
                    rec['eligible'] = rec['eligible'] and rec['residual_gate_pass']
            trials.append(rec)
        good = [r for r in trials if r['eligible']]
        winner = sorted(good, key=lambda r: (-r['adj_R2'], r['candidate']))[0] if good else None
        for rec in trials:
            rec['chosen'] = bool(winner and rec['candidate'] == winner['candidate'])
            rec['reject_reason'] = ('selected' if rec['chosen'] else
                                    'p>=0.05; VIF fails' if not rec['p_pass'] and not rec['VIF_pass'] else
                                    'p>=0.05' if not rec['p_pass'] else
                                    'VIF fails' if not rec['VIF_pass'] else
                                    'residual stationarity gate fails' if not rec.get('residual_gate_pass', True) else
                                    'existing term p>=0.05' if all_p and rec['max_p'] >= .05 else
                                    'eligible but lower adjusted R2')
        records.extend(trials)
        rounds.append(dict(round=round_no, added=winner['candidate'] if winner else '(stop)',
                           adj_R2=winner['adj_R2'] if winner else np.nan,
                           p_added=winner['p_added'] if winner else np.nan,
                           n=winner['n'] if winner else np.nan))
        if not winner:
            break
        selected.append(winner['candidate'])
        pending.remove(winner['candidate'])
        if not pending:
            break
    return selected, pd.DataFrame(records), pd.DataFrame(rounds)


def cap_pool(scr, rule='OOT', eligible_families=None):
    z = scr[scr.passed].copy()
    if eligible_families is not None:
        z = z[z.family.isin(eligible_families)]
    if rule == 'IS':
        z['strength'] = z.rho_IS.abs()
    elif rule == 'OOT':
        z['strength'] = z.rho_OOT.abs()
    elif rule == 'minrho':
        z['strength'] = z[['rho_IS','rho_OOT']].abs().min(axis=1)
    elif rule == 'maxp':
        z['strength'] = -z[['p_IS','p_OOT']].max(axis=1)
    else:
        z['strength'] = -z.lag
    z = z.sort_values(['strength','candidate'],ascending=[False,True]).groupby('family',sort=False).head(2)
    return z, z.candidate.tolist() + [f'y_lag{l}' for l in range(1,13)]


def metrics(actual, pred):
    err = np.asarray(pred) - np.asarray(actual)
    return dict(n=len(err), RMSE=np.sqrt(np.mean(err ** 2)), MAPE=np.mean(np.abs(err / np.asarray(actual))) * 100)


def forecast(d, sr, mode):
    levels = d.Combined.copy()
    growth = d.y.copy()
    ec = d.EC.copy()
    dates = d.loc['2023-10-01':END].index
    if mode != 'static':
        levels.loc[dates] = np.nan
        growth.loc[dates] = np.nan
        if mode == 'dynamic_full':
            ec.loc[dates] = np.nan
    rows = []
    for dt in dates:
        old = dt - pd.DateOffset(months=12)
        x = {'const': 1., 'EC_lag12': ec.at[old],
             'y_lag1': growth.at[dt - pd.DateOffset(months=1)],
             'y_lag6': growth.at[dt - pd.DateOffset(months=6)],
             'CorpProfits_lag3': d.at[dt, 'CorpProfits_lag3']}
        yh = sum(sr.params[name] * x[name] for name in sr.params.index)
        pred = levels.at[old] * (1 + yh / 100)
        rows.append(dict(date=dt, mode=mode, actual=d.at[dt, 'Combined'],
                         y_actual=d.at[dt, 'y'], y_pred=yh, prediction=pred,
                         prior12_level_used=levels.at[old], EC_lag12_used=ec.at[old],
                         y_lag1_used=x['y_lag1'], y_lag6_used=x['y_lag6'],
                         CorpProfits_lag3_used=x['CorpProfits_lag3']))
        if mode != 'static':
            levels.at[dt] = pred
            growth.at[dt] = yh
            if mode == 'dynamic_full':
                ec.at[dt] = pred - d.at[dt, 'baseline']
    return pd.DataFrame(rows)


# %% 2-6. Document experiments and rule sensitivities
def run_experiments():
    inputs = [dict(role='fee_source', path=str(CARD), sha256=sha(CARD)),
              dict(role='macro_source', path=str(MACRO), sha256=sha(MACRO))]
    d, lr = load_data()
    ins, oot = d.loc[START:CUT], d.loc['2023-10-01':END]
    print('Loaded and aligned', len(ins), len(oot), flush=True)
    save('macro_dictionary', pd.DataFrame(MACRO_DESCRIPTIONS))
    save('monthly_data_and_features', d.reset_index(names='date'))
    save('annual_fee_reconciliation', d.loc[START:, ['Commercial Card','Small Business','Combined']].groupby(d.loc[START:].index.year).sum().reset_index(names='year'))
    st = []
    for name in ['Combined'] + list(MAPPING):
        st.append(dict(transform='level', variable=name, **stationarity(ins[name])))
        st.append(dict(transform='diff1', variable=name, **stationarity(d[name].diff().loc[START:CUT])))
    for name in MAPPING:
        st.append(dict(transform='yoy12', variable=name, **stationarity(ins[name+'_yoy'])))
        st.append(dict(transform='yoy12_full_sample', variable=name, **stationarity(d.loc[START:END,name+'_yoy'])))
        x=d[name].diff(12) if name=='Unrate' else d[name].pct_change(12,fill_method=None)*100
        st.append(dict(transform='yoy12_IS_with_2010_history', variable=name, **stationarity(x.loc[START:CUT])))
    st.append(dict(transform='yoy12', variable='Combined', **stationarity(ins.y)))
    save('stationarity', pd.DataFrame(st))
    selected, lr_trials, lr_rounds = forward(ins, 'Combined', LR_CANDIDATES, residual_gate='residual_ADF_p')
    save('long_run_forward_all_trials', lr_trials)
    save('long_run_forward_rounds', lr_rounds)
    print('LR path', selected, flush=True)
    lr_sensitivity=[]
    for gate in [None,'residual_ADF_p','EG_p']:
        for vmax in [5.,10.]:
            ss,tt,rr=forward(ins,'Combined',LR_CANDIDATES,vif_limit=vmax,residual_gate=gate)
            tag=f'{gate or "no_residual_gate"}_VIF{int(vmax)}'
            save('long_run_trials_'+tag,tt)
            lr_sensitivity.append(dict(rule=tag,selected=' + '.join(ss)))
    save('long_run_rule_sensitivity',pd.DataFrame(lr_sensitivity))
    eg = coint(ins.Combined, ins[LR_TERMS], trend='c', autolag='aic')
    save('cointegration_audit', pd.DataFrame([
        dict(test='ordinary residual ADF with constant', stat=adfuller(lr.resid, result_object=False)[0], p=adfuller(lr.resid, result_object=False)[1]),
        dict(test='Engle Granger MacKinnon N=3', stat=eg[0], p=eg[1]),
    ]))
    sr = fit(ins, 'y', SR_TERMS)
    save('coefficients', pd.DataFrame(coef(lr, 'long_run') + coef(sr, 'short_run')))
    model_diag = [dict(model='long_run', **diagnostics(lr)),
                  dict(model='short_run_base', **diagnostics(fit(ins, 'y', ['EC_lag12']))),
                  dict(model='short_run_final', **diagnostics(sr))]
    save('model_diagnostics', pd.DataFrame(model_diag))
    screens = {}
    for common in [False, True]:
        key = 'common' if common else 'pairwise'
        scr = screen(d, common)
        screens[key] = scr
        save('partial_correlations_'+key, scr)
        print('Screen', key, 'pass', scr.passed.sum(), 'corrected', scr.passed_corrected.sum(), flush=True)
    scr = screens['pairwise']
    # The published full-sample stationarity table excludes CPI only.
    # IS-only testing also excludes SBOpt, which has no correlation survivors.
    eligible_families=[m for m in MAPPING if m!='CPI']
    scr['stationarity_pass_document'] = scr.family.isin(eligible_families)
    scr['passed_after_stationarity'] = scr.passed & scr.stationarity_pass_document
    scr['passed_corrected_after_stationarity'] = scr.passed_corrected & scr.stationarity_pass_document
    save('partial_correlations_document_aligned',scr)
    capped,pool = cap_pool(scr,'OOT',eligible_families)
    save('short_run_capped_pool', capped)
    sr_selected, sr_trials, sr_rounds = forward(ins, 'y', pool, base=['EC_lag12'])
    save('short_run_forward_all_trials', sr_trials)
    save('short_run_forward_rounds', sr_rounds)
    print('SR path', sr_selected, '\n', sr_rounds.to_string(index=False), flush=True)
    # Controlled sensitivity: fixed sample to make comparisons use identical dates.
    alt_selected, alt_trials, alt_rounds = forward(ins, 'y', pool, base=['EC_lag12'], common=True)
    save('short_run_forward_common_sample_trials', alt_trials)
    save('short_run_forward_common_sample_rounds', alt_rounds)
    print('SR common-sample path', alt_selected, flush=True)
    caps=[]
    for rule in ['IS','OOT','minrho','maxp','first']:
        cc,pp=cap_pool(scr,rule,eligible_families)
        ss,tt,rr=forward(ins,'y',pp,base=['EC_lag12'])
        save('short_run_forward_'+rule+'_cap_trials',tt)
        caps.append(dict(rule=rule,pool='; '.join(cc.candidate),selected=' + '.join(ss),
                         final_adj_R2=(float(rr.dropna(subset=['adj_R2']).iloc[-1].adj_R2) if rr.adj_R2.notna().any() else np.nan)))
    save('short_run_cap_sensitivity',pd.DataFrame(caps))
    # Verify the published stopping statement against every one of the 104
    # macros and 12 target lags, recording which were actually in the capped pool.
    after=[]
    for candidate in [f'{m}_lag{l}' for m in MAPPING for l in range(13)] + [f'y_lag{l}' for l in range(1,13)]:
        if candidate in SR_TERMS:
            continue
        r=fit(ins,'y',SR_TERMS+[candidate])
        after.append(dict(candidate=candidate,in_capped_pool=candidate in pool,p_added=r.pvalues[candidate],
                          max_VIF=max(vifs(r).values()),**diagnostics(r)))
    save('short_run_after_document_model_all_candidates',pd.DataFrame(after))
    # Reordering columns while holding the model fixed is distinct from
    # changing the sequence of conditional models in forward selection.
    permutation=[]
    for terms in [LR_TERMS, list(reversed(LR_TERMS))]:
        r=fit(ins,'Combined',terms)
        permutation.append(dict(model='LR',columns='; '.join(terms),
                                max_coefficient_difference=float((r.params-lr.params).abs().max()),
                                max_p_difference=float((r.pvalues-lr.pvalues).abs().max())))
    reverse_selected,_,_=forward(ins,'Combined',list(reversed(LR_CANDIDATES)),residual_gate='residual_ADF_p')
    if reverse_selected != selected:
        raise RuntimeError('LR candidate order changed the forward selection result.')
    reverse_sr,_,_=forward(ins,'y',list(reversed(pool)),base=['EC_lag12'])
    if reverse_sr != sr_selected:
        raise RuntimeError('SR candidate order changed the forward selection result.')
    save('column_order_verification',pd.DataFrame(permutation))
    predictions, backtests = [], []
    for mode in ['static', 'dynamic_full', 'dynamic_actual_EC']:
        p = forecast(d, sr, mode)
        predictions.append(p)
        backtests.append(dict(mode=mode, **metrics(p.actual, p.prediction)))
    fitted = pd.DataFrame({'date': sr.resid.index, 'y_actual': ins.loc[sr.resid.index,'y'], 'y_pred': sr.fittedvalues})
    fitted['actual'] = d.loc[sr.resid.index, 'Combined'].values
    fitted['prior12_actual'] = d.Combined.shift(12).loc[sr.resid.index].values
    fitted['prediction'] = fitted.prior12_actual * (1 + fitted.y_pred/100)
    save('in_sample_predictions', fitted)
    backtests.append(dict(mode='in_sample', **metrics(fitted.actual, fitted.prediction)))
    save('oot_predictions', pd.concat(predictions))
    save('backtest_metrics', pd.DataFrame(backtests))
    print('Backtests', backtests, flush=True)
    ecstats = []
    for period, x in [('IS', ins.EC), ('OOT', oot.EC)] + [(str(year), z.EC) for year,z in oot.groupby(oot.index.year)] + [('2023_full_year',d.loc['2023','EC'])]:
        ecstats.append(dict(period=period, n=len(x), mean=x.mean(), std_sample=x.std(ddof=1), std_population=x.std(ddof=0),
                            increases=int((x.diff()>0).sum()), decreases=int((x.diff()<0).sum())))
    save('ec_summary', pd.DataFrame(ecstats))
    cusum = []
    for label, frame in [('IS', ins), ('full', d.loc[START:END])]:
        r = fit(frame, 'Combined', LR_TERMS)
        for ddof in [0, 2, 3]:
            z = breaks_cusumolsresid(r.resid, ddof=ddof)
            cusum.append(dict(sample=label, ddof=ddof, stat=z[0], p=z[1], n=len(frame)))
    save('cusum', pd.DataFrame(cusum))
    full = d.loc[START:END]
    pooled = fit(full, 'Combined', LR_TERMS)
    chow = []
    for cutoff in ['2023-01-01', '2023-10-01', '2024-01-01', '2024-07-01']:
        pre = full.loc[full.index < cutoff]
        post = full.loc[full.index >= cutoff]
        rp, rq = fit(pre, 'Combined', LR_TERMS), fit(post, 'Combined', LR_TERMS)
        k, n = 3, len(full)
        f = ((pooled.ssr-rp.ssr-rq.ssr)/k) / ((rp.ssr+rq.ssr)/(n-2*k))
        chow.append(dict(break_date=cutoff, F=f, p=stats.f.sf(f,k,n-2*k), n_pre=len(pre), n_post=len(post),
                         SSR_pooled=pooled.ssr, SSR_pre=rp.ssr, SSR_post=rq.ssr, df_num=k, df_den=n-2*k))
    save('chow', pd.DataFrame(chow))
    rolling = []
    for i in range(29,len(full)):
        frame = full.iloc[i-29:i+1]
        r = fit(frame, 'Combined', LR_TERMS)
        rolling.append(dict(window_start=frame.index[0], window_end=frame.index[-1], n=30,
                            **r.params.to_dict(), CPI_p=r.pvalues.CPI, SBOpt_p=r.pvalues.SBOpt,
                            max_VIF=max(vifs(r).values()), adj_R2=r.rsquared_adj))
    save('rolling_30m_coefficients', pd.DataFrame(rolling))
    roll_sensitivity=[]
    for width in [24,30,36]:
        for dt in pd.date_range('2022-01-01','2022-12-01',freq='MS'):
            frame=full.loc[:dt].tail(width)
            r=fit(frame,'Combined',LR_TERMS)
            roll_sensitivity.append(dict(window=width,end=dt,CPI=r.params.CPI,SBOpt=r.params.SBOpt))
    save('rolling_2022_window_sensitivity',pd.DataFrame(roll_sensitivity))
    trend_rows, trend_predictions = [], []
    t = pd.DataFrame({'const':1.,'month':np.arange(len(oot),dtype=float)},index=oot.index)
    for label,nfit in [('full_oot',27), ('first18',18)]:
        tr = sm.OLS(oot.EC.iloc[:nfit],t.iloc[:nfit]).fit()
        pred = oot.baseline + tr.predict(t)
        ids = oot.index if nfit==27 else oot.index[18:]
        trend_rows.append(dict(model=label,n_fit=nfit,evaluation_role='retrospective_fit' if nfit==27 else 'heldout',
                               const=tr.params['const'],slope=tr.params['month'],
                               slope_p=tr.pvalues['month'],R2=tr.rsquared,
                               **{'evaluation_'+k:v for k,v in metrics(oot.loc[ids,'Combined'],pred.loc[ids]).items()}))
        for dt in oot.index:
            trend_predictions.append(dict(model=label,date=dt,role='fit' if dt in oot.index[:nfit] else 'heldout',
                                          actual=oot.at[dt,'Combined'], baseline=oot.at[dt,'baseline'],
                                          EC_actual=oot.at[dt,'EC'],EC_pred=tr.predict(t.loc[[dt]]).iloc[0],prediction=pred.at[dt]))
    save('overlay_trend_metrics', pd.DataFrame(trend_rows))
    save('overlay_monthly_predictions',pd.DataFrame(trend_predictions))
    print('Completed EC, CUSUM, Chow, rolling coefficients and trend overlay.', flush=True)
    robust = sr.get_robustcov_results(cov_type='HAC', maxlags=12, use_correction=True, use_t=True)
    save('short_run_HAC12_diagnostic',pd.DataFrame({'term':sr.params.index,'OLS_p':sr.pvalues.values,'HAC12_p':robust.pvalues,'HAC12_SE':robust.bse}))
    bgs = []
    for lag in [1, 6, 12]:
        bg = acorr_breusch_godfrey(sr,nlags=lag,result_object=False)
        bgs.append(dict(lags=lag,LM=bg[0],LM_p=bg[1],F=bg[2],F_p=bg[3]))
    save('short_run_BG_diagnostic',pd.DataFrame(bgs))
    manifest=dict(inputs=inputs,python=sys.version,numpy=np.__version__,pandas=pd.__version__,scipy=scipy.__version__,
                  statsmodels=statsmodels.__version__,macro_mapping=MAPPING,
                  settings=dict(start=START,IS_end=CUT,OOT_end=END,target='100*(Combined_t/Combined_t-12 - 1)',
                                EC='Combined - frozen IS long-run fitted level; lag12; no transform',
                                unrate='percentage-point difference over 12 months',
                                macro_history='primary: align levels to 2011+ then transform; 2010-history sensitivity retained',
                                ADF='constant, maxlag=None, autolag=AIC',KPSS='constant, nlags=auto',
                                forward='all-candidate trial each round; max adj R2 among added p<.05 and max slope VIF<10; per-model complete cases',
                                forward_VIF10='inferred from reported DJ_TotalMkt treatment; sensitivities at 5 and 10',
                                LR_residual_gate='ordinary residual ADF<.05; necessary inferred gate for CPI first; formal EG and no-gate sensitivities retained',
                                partial='8 families x lag0..12; compare pairwise and common samples; Pearson residual p and corrected df',
                                family_cap='inferred reproducible configuration: top 2 by absolute OOT partial rho; not uniquely identified; five cap alternatives retained'),
                  long_run_selected=selected,short_run_selected=sr_selected,short_run_common_selected=alt_selected)
    manifest.update(runtime=runtime_info(), reference_document=REFERENCE_DOCUMENT,
                    reference_input_sha256=REFERENCE_INPUT_SHA256,
                    input_format="CSV",
                    reference_input_bytes_match={r['role']:None for r in inputs},
                    reference_input_hash_comparison="Not applicable across CSV and the original Excel format.",
                    fixed_document_long_run=LR_TERMS, fixed_document_short_run=SR_TERMS)
    write_json('run_manifest.json', manifest)
    if not all(sha(r['path'])==r['sha256'] for r in inputs):
        raise RuntimeError('An input changed during execution; inspect source files and rerun.')
    return d, lr, sr, manifest



# %% 7. Reference values and claim-level audit, separate from estimation
REFERENCE_DOCUMENT = {
    "name": "Commercial_Card_and_Small_Business_Fee_Consolidated.docx",
    "sha256": "7e45500f8e39b562dddaa58354d2bbfc20251cf7ba2e0fa576d8edd0e9b4988f",
    "note": "Fixed document version. source_block is the zero-based Word body XML block index; source_page refers to the five-page source rendering."
}
REFERENCE_INPUT_SHA256 = {
    "fee_source": "3efddeb29dbd481d7bdd26e4e322f17711e9b3f5aa00a9576e9f56f9c1f58f30",
    "macro_source": "3af42d8eb700037ed09080ecdc76dd07b51325cf51a0896998d9b28fa67d07fa"
}
# The 13 source tables are embedded below. Prose and equation claims are in build_numeric_audit.
REFERENCE_TABLES = [
    {"block": 10, "type": "table", "rows": [
        ["Variable","ADF p","KPSS p","Classification"],
        ["Combined (target)","0.932","0.01","I(1)"],
        ["Unrate","0.046","0.01","ambiguous — excluded"],
        ["CPI","0.999","0.01","I(1)"],
        ["DPI","0.999","0.01","I(1)"],
        ["SBOpt","0.337","0.043","I(1)"],
        ["GDP_Wholesale","0.995","0.01","I(1)"],
        ["IP","0.029","0.1","ambiguous — excluded"],
        ["CorpProfits","0.999","0.01","I(1)"],
        ["DJ_TotalMkt","0.958","0.01","I(1)"]
    ]},
    {"block": 17, "type": "table", "rows": [
        ["Round","Added","New varp-value","VIF ok","Residual ADFp","Stationary?"],
        ["1","CPI","<0.001","Yes","0.009","Yes"],
        ["2","SBOpt","0.029","Yes","0.008","Yes"]
    ]},
    {"block": 23, "type": "table", "rows": [
        ["Term","Coefficient","p-value","VIF"],
        ["const","-3766.52","<0.001","-"],
        ["CPI","19.436","<0.001","1.0"],
        ["SBOpt","3.872","0.029","1.0"]
    ]},
    {"block": 35, "type": "table", "rows": [
        ["Variable","ADF p","KPSS p","Result"],
        ["Unrate","0.046","0.1","OK"],
        ["CPI","0.265","0.01","FAIL"],
        ["DPI","0.002","0.058","OK"],
        ["SBOpt","0.081","0.1","OK"],
        ["GDP_Wholesale","0.319","0.1","OK"],
        ["IP","0.017","0.1","OK"],
        ["CorpProfits","0.13","0.1","OK"],
        ["DJ_TotalMkt","0.005","0.1","OK"]
    ]},
    {"block": 42, "type": "table", "rows": [
        ["Candidate","rho (IS)","p (IS)","rho (OOT)","p (OOT)"],
        ["Unrate_lag3","-0.389","<0.001","-0.267","0.178"],
        ["Unrate_lag4","-0.315","<0.001","-0.296","0.133"],
        ["Unrate_lag5","-0.286","0.001","-0.366","0.06"],
        ["Unrate_lag6","-0.3","<0.001","-0.452","0.018"],
        ["Unrate_lag7","-0.295","<0.001","-0.452","0.018"],
        ["Unrate_lag8","-0.323","<0.001","-0.649","<0.001"],
        ["Unrate_lag9","-0.221","0.011","-0.296","0.133"],
        ["Unrate_lag10","-0.15","0.088","-0.625","<0.001"],
        ["DPI_lag10","0.123","0.163","0.439","0.022"],
        ["GDP_Wholesale_lag10","0.21","0.016","0.348","0.075"],
        ["GDP_Wholesale_lag11","0.124","0.161","0.437","0.023"],
        ["IP_lag3","0.412","<0.001","0.263","0.184"],
        ["IP_lag11","0.161","0.068","0.317","0.107"],
        ["CorpProfits_lag2","0.528","<0.001","0.361","0.065"],
        ["CorpProfits_lag3","0.473","<0.001","0.381","0.05"],
        ["CorpProfits_lag4","0.378","<0.001","0.256","0.198"],
        ["CorpProfits_lag9","0.424","<0.001","0.294","0.137"],
        ["CorpProfits_lag10","0.401","<0.001","0.377","0.052"],
        ["CorpProfits_lag11","0.357","<0.001","0.312","0.113"]
    ]},
    {"block": 47, "type": "table", "rows": [
        ["Round","Added","Adj. R² after"],
        ["1","y_lag1","0.665"],
        ["2","y_lag6","0.704"],
        ["3","CorpProfits_lag3","0.713"],
        ["4","(none further pass at 5%)","-"]
    ]},
    {"block": 52, "type": "table", "rows": [
        ["Term","Coefficient","p-value","VIF"],
        ["const","2.514","0.005","-"],
        ["EC_lag12","-0.0546","<0.001","1.22"],
        ["y_lag1","0.366","<0.001","2.29"],
        ["y_lag6","0.22","<0.001","1.26"],
        ["CorpProfits_lag3","0.214","0.024","2.09"]
    ]},
    {"block": 56, "type": "table", "rows": [
        ["Method","RMSE","MAPE"],
        ["Static, one-step-ahead (true history)","485.8","19.15%"],
        ["Dynamic, fully recursive 27-month","628.2","25.62%"]
    ]},
    {"block": 60, "type": "table", "rows": [
        ["Period","Mean EC"],
        ["In-sample (2011-2023)","~0 (std 120)"],
        ["OOT (2023-10 to 2025-12)","-510 (std 265)"],
        ["2023 (partial)","-62"],
        ["2024","-399"],
        ["2025","-723"]
    ]},
    {"block": 65, "type": "table", "rows": [
        ["Sample used","CUSUM statistic","p-value","Verdict"],
        ["In-sample only (2011-2023)","0.951","0.327","Stable"],
        ["Full sample (2011-2025)","2.173","0.0002","Highly unstable"]
    ]},
    {"block": 68, "type": "table", "rows": [
        ["Candidate breakdate","F-statistic","p-value","n (pre / post)"],
        ["2023-01-01","88.1","<0.0001","144 / 36"],
        ["2023-10-01","85.39","<0.0001","153 / 27"],
        ["2024-01-01","85.44","<0.0001","156 / 24"],
        ["2024-07-01","66.24","<0.0001","162 / 18"]
    ]},
    {"block": 71, "type": "table", "rows": [
        ["Rolling 30-month windowending","CPI coefficient","SBOpt coefficient"],
        ["2022-06","26.9","27.7"],
        ["2023-06","24.6","17.4"],
        ["2023-09","8.9","-24.6"],
        ["2024-06","-1.3","-34.8"],
        ["2025-06","-6.6","-15.4"],
        ["2025-12","-11.9","-11.4"]
    ]},
    {"block": 81, "type": "table", "rows": [
        ["Test","Result"],
        ["Trend slope (fit on first 18 months only)","-34.54/month, p<0.0001"],
        ["MAPE on held-out last 9 months (genuine forecast)","5.91%"]
    ]}
]

def read(name):
    return pd.read_csv(OUT / f'{name}.csv')


def md_table(frame, digits=6):
    def show(x):
        if pd.isna(x): return ''
        if isinstance(x, (float, np.floating)):
            if x != 0 and abs(x) < 10 ** (-digits): return f'{x:.3e}'
            return f'{x:.{digits}f}'.rstrip('0').rstrip('.')
        return str(x).replace('|', '\\|').replace('\n', ' ')
    return '\n'.join(['| ' + ' | '.join(map(str,frame.columns)) + ' |',
                      '| ' + ' | '.join(['---']*len(frame.columns)) + ' |'] +
                     ['| ' + ' | '.join(show(v) for v in row) + ' |' for row in frame.itertuples(index=False,name=None)])


def build_numeric_audit(d):
    blocks={b['block']:b for b in REFERENCE_TABLES}
    st=read('stationarity').set_index(['transform','variable'])
    co=read('coefficients').set_index(['model','term'])
    diag=read('model_diagnostics').set_index('model')
    lrtr=read('long_run_forward_all_trials')
    srtr=read('short_run_forward_all_trials')
    srro=read('short_run_forward_rounds').set_index('round')
    pc=read('partial_correlations_document_aligned').set_index('candidate')
    bt=read('backtest_metrics').set_index('mode')
    ec=read('ec_summary').set_index('period')
    cu=read('cusum').query('ddof==3').set_index('sample')
    ch=read('chow').set_index('break_date')
    roll=read('rolling_30m_coefficients').set_index('window_end')
    ov=read('overlay_trend_metrics').set_index('model')
    rows=[]

    def check(claim,actual,kind):
        claim=str(claim).strip().replace(',','').replace('%','').replace('~','')
        op='<' if claim.startswith('<') else '='
        value=claim.lstrip('<')
        expected=float(value)
        decimals=max(0,-Decimal(value).as_tuple().exponent)
        tolerance=0 if kind=='count' else .5*10**(-decimals)
        ok=float(actual)<expected if op=='<' else abs(float(actual)-expected)<=tolerance+1e-12
        return expected,tolerance,bool(ok)

    def add(section,page,block,item,metric,claim,actual,kind='number',note='',alt=None,issue=''):
        expected,tol,ok=check(claim,actual,kind)
        aok=check(claim,alt,kind)[2] if alt is not None else None
        rows.append(dict(id=f'N{len(rows)+1:03d}',section=section,source_page=page,source_block=block,
                         item=item,metric=metric,document_value=str(claim),recomputed_value=float(actual),
                         difference=float(actual)-expected,tolerance=tol,
                         numeric_status=('unavailable' if not np.isfinite(float(actual)) else 'match' if ok else 'discrepancy'),
                         alternative_value=alt,alternative_matches=aok,issue=issue,note=note))

    # All numerical result cells in the 13 source tables, plus numerical claims
    # in prose and equations. Section numbers / dates used as identifiers are
    # represented by locators and window metadata rather than counted as results.
    for row in blocks[10]['rows'][1:]:
        name=row[0].replace(' (target)','')
        for col,key in [(1,'ADF_p'),(2,'KPSS_p')]:
            note='IS: 2011-01 through 2023-09, n=153. '
            if key=='KPSS_p' and st.loc[('level',name),key] in [.01,.1]:note+='KPSS boundary value; the true p-value may be outside the tabulated range.'
            add('1.1',1,10,name,key,row[col],st.loc[('level',name),key],note=note,
                issue='Both IP tests support stationarity; the reference label ambiguous is inaccurate' if name=='IP' else '')
    for row in blocks[17]['rows'][1:]:
        r=lrtr[(lrtr['round']==int(row[0])) & (lrtr.candidate==row[1])].reindex(columns=lrtr.columns)
        r = r.iloc[0] if len(r) else pd.Series(dtype=float)
        for col,key in [(2,'p_added'),(4,'residual_ADF_p')]:
            add('1.2',1,17,row[1],key,row[col],r.get(key, np.nan),note='Includes the inferred residual ADF < 0.05 and VIF < 10 gates.',issue='Selection rules are incompletely specified')
    for block,model,page,section in [(23,'long_run',2,'1.2'),(52,'short_run',4,'2.4')]:
        for row in blocks[block]['rows'][1:]:
            for col,key in [(1,'coefficient'),(2,'p'),(3,'VIF')]:
                if row[col]=='-':continue
                issue='Coefficient rounding discrepancy' if model=='short_run' and row[0]=='CorpProfits_lag3' and key=='coefficient' else ''
                add(section,page,block,row[0],key,row[col],co.loc[(model,row[0]),key],issue=issue,
                    note='The reference reports 0.214; the unrounded re-estimate and difference are recorded separately.' if issue else 'Model re-estimated with conventional OLS standard errors.')
    for row in blocks[35]['rows'][1:]:
        for col,key in [(1,'ADF_p'),(2,'KPSS_p')]:
            add('2.2',2,35,row[0],key,row[col],st.loc[('yoy12',row[0]),key],
                alt=st.loc[('yoy12_full_sample',row[0]),key],issue='Full-sample testing includes OOT observations',
                note='Primary: IS 2012-01 through 2023-09, n=141. Alternative: full sample through 2025-12, n=168. alternative_matches records the separate comparison; the IS result is not replaced.')
    for row in blocks[42]['rows'][1:]:
        for col,key in [(1,'rho_IS'),(2,'p_IS'),(3,'rho_OOT'),(4,'p_OOT')]:
            add('2.3',3,42,row[0],key,row[col],pc.loc[row[0],key],
                note=f"Candidate-specific complete cases; IS n={int(pc.loc[row[0],'n_IS'])}, OOT n=27. The residual Pearson test uses n-2 degrees of freedom.",
                issue='Unrate correlations require particular scrutiny' if row[0].startswith('Unrate') else 'Partial-correlation degrees of freedom and OOT-informed selection')
    for row in blocks[47]['rows'][1:4]:
        add('2.4',3,47,row[1],'adj_R2',row[2],srro.adj_R2.get(int(row[0]), np.nan),
            note='Retain the top two lags per family by |OOT rho|. This inferred rule is not specified in the reference and is not uniquely identified.',issue='Family-cap rule is incompletely specified')
    for row,mode in zip(blocks[56]['rows'][1:],['static','dynamic_full']):
        for col,key in [(1,'RMSE'),(2,'MAPE')]:
            add('3',4,56,mode,key,row[col],bt.loc[mode,key],note='27 months; coefficients are frozen. The dynamic path recursively updates y, revenue levels, and EC.')
    for item,metric,claim in [('IS','mean','~0'),('IS','std_sample','120'),('OOT','mean','-510'),('OOT','std_sample','265'),('2023','mean','-62'),('2024','mean','-399'),('2025','mean','-723')]:
        add('4',4,60,item,metric,claim,ec.loc[item,metric],
            alt=ec.loc['2023_full_year','mean'] if item=='2023' else None,
            issue='The 2023 partial-year label does not match the reported data' if item=='2023' else '',
            note='Primary: October-December 2023. Alternative: all 12 months of 2023.' if item=='2023' else 'Standard deviation uses ddof=1; EC is calculated with frozen long-run coefficients.')
    for row,sample in zip(blocks[65]['rows'][1:],['IS','full']):
        for col,key in [(1,'stat'),(2,'p')]:
            add('5.1',4,65,sample,key,row[col],cu.loc[sample,key],note='Refit CPI + SBOpt on the specified sample; CUSUM ddof=3.')
    for row in blocks[68]['rows'][1:]:
        for col,key in [(1,'F'),(2,'p')]:
            add('5.2',5,68,row[0],key,row[col],ch.loc[row[0],key],note='Chow test uses k=3 including the intercept; numerator and denominator degrees of freedom are recorded in chow.csv.')
        for claim,key in zip(row[3].split(' / '),['n_pre','n_post']):
            add('5.2',5,68,row[0],key,claim,ch.loc[row[0],key],kind='count')
    for row in blocks[71]['rows'][1:]:
        for col,key in [(1,'CPI'),(2,'SBOpt')]:
            add('5.3',5,71,row[0],key,row[col],roll.loc[row[0]+'-01',key],
                note='30 months including the end month. The 2022-06 window spans 2020-01 through 2022-06.',
                issue='Rolling-coefficient discrepancy at 2022-06' if row[0]=='2022-06' else '')
    add('6.2',5,81,'first18','slope','-34.54',ov.loc['first18','slope'])
    add('6.2',5,81,'first18','slope_p','<0.0001',ov.loc['first18','slope_p'])
    add('6.2',5,81,'heldout9','MAPE','5.91%',ov.loc['first18','evaluation_MAPE'],note='Trend fitted on 2023-10 through 2025-03 and evaluated on 2025-04 through 2025-12. This is a conditional forecast using the supplied macro values.')
    # Prose/equations: preserve duplicate appearances as separately located claims.
    add('Scope',1,3,'IS','n','153',len(d.loc[START:CUT]),kind='count',note='Derived from the reference dates 2011-01 through 2023-09; this paragraph does not explicitly report the count.')
    add('Scope',1,3,'OOT','n','27',len(d.loc['2023-10-01':END]),kind='count',note='Derived from the reference dates 2023-10 through 2025-12.')
    add('1.1',1,12,'Combined','ADF_p','0.932',st.loc[('level','Combined'),'ADF_p'])
    add('1.1',1,12,'LR candidates','count','6',len(LR_CANDIDATES),kind='count')
    add('1.2',1,15,'LR candidates','count','6',len(LR_CANDIDATES),kind='count')
    for term,claim in [('const','-3766.52'),('CPI','19.44'),('SBOpt','3.87')]:
        add('1.2',2,22,term,'equation coefficient',claim,co.loc[('long_run',term),'coefficient'])
    for metric,claim in [('R2','.929'),('adj_R2','.929'),('n','153')]:
        add('1.2',2,25,'long_run',metric,claim,diag.loc['long_run',metric],kind='count' if metric=='n' else 'number')
    add('1.2',2,25,'EC','residual_ADF_p','.0081',read('cointegration_audit').iloc[0].p,
        alt=read('cointegration_audit').iloc[1].p,issue='Ordinary residual ADF is presented as a formal cointegration test',
        note='Primary: ordinary residual ADF. Alternative: formal Engle-Granger. These are different tests and their conclusions are not interchangeable.')
    add('2.1',2,29,'short_run_base','DW','1.02',diag.loc['short_run_base','DW'])
    add('2.3',3,41,'screen','survivors','19',int(pc.passed_after_stationarity.sum()),kind='count',issue='Separate screening results use the corrected n-3 degrees of freedom',note='Exclude CPI as in the reference, then apply the correlation gates. Raw and degrees-of-freedom-corrected results are retained separately.')
    add('2.3',3,41,'screen','total candidates','104',len(pc),kind='count',note='Eight macro families, each with lags 0 through 12.')
    cap=read('short_run_capped_pool')
    for metric,claim,value in [('max lags per family','2',cap.groupby('family').size().max()),('macro pool','9',len(cap)),('target lag pool','12',12)]:
        add('2.3',3,44,'pool',metric,claim,value,kind='count',issue='Family-cap rule is incompletely specified')
    for block,term,claim in [(50,'const','2.514'),(50,'EC_lag12','-0.0546'),(50,'y_lag1','0.366'),(51,'y_lag6','0.220'),(51,'CorpProfits_lag3','0.214')]:
        add('2.4',4,block,term,'equation coefficient',claim,co.loc[('short_run',term),'coefficient'],
            issue='Coefficient rounding discrepancy' if term=='CorpProfits_lag3' else '')
    for metric,claim in [('R2','.722'),('adj_R2','.713'),('n','135'),('AIC','940.5'),('DW','2.075'),('JB_p','.935')]:
        add('2.4',4,54,'short_run_final',metric,claim,diag.loc['short_run_final',metric],kind='count' if metric=='n' else 'number')
    for metric,claim in [('MAPE','5.56%'),('RMSE','110.4')]:
        add('2.4',4,54,'in_sample',metric,claim,bt.loc['in_sample',metric])
    add('4',4,59,'EC IS','std_sample','120',ec.loc['IS','std_sample'])
    add('5.3',5,71,'rolling','window months','30',30,kind='count')
    for metric,claim in [('slope','-29.44'),('slope_p','<0.001'),('R2','.778')]:
        add('6.1',5,78,'full_oot',metric,claim,ov.loc['full_oot',metric])
    add('6.2',5,80,'overlay','fit months','18',ov.loc['first18','n_fit'],kind='count')
    add('6.2',5,80,'overlay','heldout months','9',ov.loc['first18','evaluation_n'],kind='count')

    ledger=pd.DataFrame(rows)
    save('numeric_claim_audit',ledger)
    save('numeric_discrepancies',ledger[ledger.numeric_status.ne('match')])
    return ledger




# %% 8. Independent checks, audit summary, and entry point
def write_json(name, value):
    (OUT / name).write_text(json.dumps(value, ensure_ascii=False, indent=2), encoding="utf-8")


def independent_validation(d, lr, sr, manifest):
    checks = []

    def record(name, value, limit):
        checks.append(dict(check=name, actual=float(value), limit=limit,
                           passed=bool(np.isfinite(value) and value <= limit)))

    for label, model in [("long_run", lr), ("short_run", sr)]:
        beta = np.linalg.lstsq(model.model.exog, model.model.endog, rcond=None)[0]
        record(label + "_numpy_lstsq", np.max(np.abs(beta - model.params.values)), 1e-8)
    record("combined_components", np.max(np.abs(d.loc[START:, "Combined"] -
           d.loc[START:, "Commercial Card"] - d.loc[START:, "Small Business"])), 1e-10)
    yoy_errors, lag_errors = [], []
    for dt in d.loc["2012-01-01":END].index:
        old = dt - pd.DateOffset(months=12)
        yoy_errors.append(100 * (d.at[dt, "Combined"] / d.at[old, "Combined"] - 1) - d.at[dt, "y"])
        lag_errors.append(d.at[dt, "EC_lag12"] - d.at[old, "EC"])
    record("calendar_yoy_alignment", np.max(np.abs(yoy_errors)), 1e-10)
    record("calendar_EC_lag12_alignment", np.max(np.abs(lag_errors)), 1e-10)
    record("long_run_residual_orthogonality", np.max(np.abs(lr.model.exog.T @ lr.resid)), 1e-5)

    # Independent recursion: initialize dictionaries using IS only; future actual revenue is never added.
    levels = d.loc[:CUT, "Combined"].dropna().to_dict()
    growth = d.loc[:CUT, "y"].dropna().to_dict()
    ec = d.loc[:CUT, "EC"].dropna().to_dict()
    predictions = []
    for dt in d.loc["2023-10-01":END].index:
        old = dt - pd.DateOffset(months=12)
        yh = (sr.params["const"] + sr.params["EC_lag12"] * ec[old]
              + sr.params["y_lag1"] * growth[dt - pd.DateOffset(months=1)]
              + sr.params["y_lag6"] * growth[dt - pd.DateOffset(months=6)]
              + sr.params["CorpProfits_lag3"] * d.at[dt, "CorpProfits_lag3"])
        levels[dt] = levels[old] * (1 + yh / 100)
        growth[dt], ec[dt] = yh, levels[dt] - d.at[dt, "baseline"]
        predictions.append(levels[dt])
    saved = read("oot_predictions").query('mode == "dynamic_full"').prediction.to_numpy()
    record("independent_recursive_forecast", np.max(np.abs(np.asarray(predictions) - saved)), 1e-7)

    # Remove future targets and residuals from the primary forecast input; predictions must not change.
    poisoned = d.copy()
    poisoned.loc["2023-10-01":END, ["Combined", "y", "EC"]] = np.nan
    recalculated = forecast(poisoned, sr, "dynamic_full").prediction.to_numpy()
    record("dynamic_future_actuals_removed", np.max(np.abs(recalculated - saved)), 1e-7)
    for inp in manifest["inputs"]:
        record("input_unchanged_" + inp["role"], int(sha(inp["path"]) != inp["sha256"]), 0)
    result = pd.DataFrame(checks)
    save("independent_validation", result)
    return result


def build_summary(ledger, validations, manifest):
    counts = ledger.numeric_status.value_counts().to_dict()
    backtests = read("backtest_metrics")
    summary = dict(
        status="complete" if validations.passed.all() else "validation_failed",
        claims=len(ledger),
        matches=int(counts.get("match", 0)),
        discrepancies=int(counts.get("discrepancy", 0)),
        unavailable=int(counts.get("unavailable", 0)),
        independent_checks=len(validations),
        independent_checks_passed=int(validations.passed.sum()),
        python=platform.python_version(),
        output=str(OUT),
        long_run_selected=manifest["long_run_selected"],
        short_run_selected=manifest["short_run_selected"],
        backtests=backtests.to_dict(orient="records"),
    )
    write_json("audit_summary.json", summary)
    differences = ledger[ledger.numeric_status.ne("match")]
    text = [
        "# Combined YoY12 Reproduction Audit", "",
        f"{len(ledger)} claims: {summary['matches']} matched, "
        f"{summary['discrepancies']} discrepancies, {summary['unavailable']} unavailable.",
        "The count includes repeated claims across tables, equations, and prose, "
        "plus two sample counts derived from the document dates.",
        f"Independent checks passed: {summary['independent_checks_passed']}/{len(validations)}.", "",
        "## Scope", "",
        "Combined = Small Business + Commercial Card. "
        "The short-run target is the 12-month percentage change; "
        "EC_lag12 is the lagged residual from the frozen long-run model.",
        "IS: 2011-01 through 2023-09. OOT: 2023-10 through 2025-12. "
        "Revenue retains the source units; MAPE is expressed in percent.",
        "Short-run stationarity claims are compared with IS results. "
        "Full-sample alternatives are recorded separately without replacing the primary results.", "",
        "## Models and backtests", "",
        "Fixed reference long-run terms: CPI + SBOpt.",
        "Fixed reference short-run terms: EC_lag12 + y_lag1 + y_lag6 + CorpProfits_lag3.",
        "Coefficients are re-estimated from the input data. Independent selection paths:", "",
        "- Long run: " + " + ".join(manifest["long_run_selected"]),
        "- Short run: " + " + ".join(manifest["short_run_selected"]), "",
        md_table(backtests), "",
        "dynamic_actual_EC is a deliberately contaminated diagnostic, not a valid dynamic backtest.",
        "dynamic_full excludes future actual revenue but uses the supplied macro values. "
        "Static forecasts use actual history at each date.", "",
        "## Numerical discrepancies", "",
        "Tolerance is half a unit of the last reported decimal place. "
        "Counts require exact equality; less-than claims are checked as inequalities.",
        "Unavailable means the required candidate or round is absent from the current selection path, "
        "or the recomputed value is nonfinite. It is never treated as a match.", "",
        md_table(differences[[
            "id", "section", "source_page", "item", "metric", "document_value",
            "recomputed_value", "difference", "numeric_status",
        ]]), "",
        "The complete claim ledger contains source locations, alternative comparisons, "
        "and audit notes in numeric_claim_audit.csv.", "",
        "## Methodological limitations", "",
        "- Residual ADF < 0.05, VIF < 10, and the top two lags by absolute OOT partial correlation "
        "are inferred reproducible rules, not a uniquely identified original algorithm. "
        "Alternative-rule results are retained.",
        "- Ordinary residual ADF and formal Engle-Granger tests are reported separately.",
        "- The document-aligned path excludes CPI and uses OOT correlations in selection; "
        "OOT is therefore not a fully independent validation set.",
        "- Primary partial-correlation p-values use residual Pearson degrees of freedom n-2. "
        "The corrected n-3 results are retained separately.",
        "- Candidate-specific complete cases change the sample across models. "
        "A common-sample sensitivity is included.",
        "- The full_oot trend is retrospective. The first18 trend reserves the final nine months "
        "for evaluating its trend parameters, conditional on the supplied macro values.",
        "- Partial-year and full-year 2023 EC means are separate. "
        "Rolling 30-month windows include the end month.", "",
        "## Provenance", "",
        "macro_dictionary.csv records variable descriptions. "
        "input_validation.csv records calendar and value checks.",
        "run_manifest.json records input paths, SHA-256 hashes, model rules, "
        "and the observed Python/package versions.",
        "Input file hashes identify the current CSV files. Original reference hashes identify Excel files, "
        "so byte-for-byte agreement is not evaluated across these formats.", "",
    ]
    (OUT / "RUN_SUMMARY.md").write_text("\n".join(text), encoding="utf-8")
    return summary


def main(argv=None):
    """Execute the reference experiments and numerical audit."""
    global CARD, MACRO, OUT
    parser = argparse.ArgumentParser(description="Combined YoY12 / EC_lag12 reproduction audit")
    parser.add_argument("--card", type=Path, default=DEFAULT_CARD, help="Fee CSV file")
    parser.add_argument("--macro", type=Path, default=DEFAULT_MACRO, help="Macro CSV file")
    parser.add_argument("--output", type=Path, help="New or empty output directory")
    args = parser.parse_args(argv)
    CARD, MACRO = args.card.expanduser().resolve(), args.macro.expanduser().resolve()
    for path in (CARD, MACRO):
        if not path.is_file():
            raise FileNotFoundError(f"Input file not found: {path}")
    OUT = (
        args.output
        or ROOT / "outputs" / ("reproduction_yoy12_" + datetime.now().strftime("%Y%m%d_%H%M%S_%f"))
    ).expanduser().resolve()
    if OUT.exists() and (not OUT.is_dir() or any(OUT.iterdir())):
        raise FileExistsError(f"Output path is not an empty directory; refusing to overwrite: {OUT}")
    OUT.mkdir(parents=True, exist_ok=True)
    write_json("run_status.json", dict(status="running", started=datetime.now().astimezone().isoformat()))
    try:
        write_json("reference_document_tables.json", dict(document=REFERENCE_DOCUMENT, tables=REFERENCE_TABLES))
        print(f"Running all experiments. Output: {OUT}", flush=True)
        d, lr, sr, manifest = run_experiments()
        ledger = build_numeric_audit(d)
        validations = independent_validation(d, lr, sr, manifest)
        summary = build_summary(ledger, validations, manifest)
        if not validations.passed.all():
            raise RuntimeError("Independent numerical checks failed; results are not validated.")
        write_json("run_status.json", dict(status="complete", finished=datetime.now().astimezone().isoformat()))
        print(
            f"\nCompleted: {summary['claims']} claims, {summary['matches']} matched, "
            f"{summary['discrepancies']} discrepancies, {summary['unavailable']} unavailable."
        )
        print(f"Audit summary: {OUT / 'RUN_SUMMARY.md'}", flush=True)
        return summary
    except Exception as exc:
        (OUT / "error.log").write_text(traceback.format_exc(), encoding="utf-8")
        write_json("run_status.json", dict(
            status="failed", error=str(exc), finished=datetime.now().astimezone().isoformat(),
        ))
        raise


if __name__ == "__main__":
    # Interactive kernels supply their own process arguments.
    interactive = "ipykernel" in sys.modules or hasattr(sys, "ps1")
    try:
        main([] if interactive else None)
    except (ValueError, RuntimeError, OSError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        if not interactive:
            sys.exit(1)
