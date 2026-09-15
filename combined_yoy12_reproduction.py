#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""Combined fee 文档逐项复现 | Python 3.14.5 | Positron | v1.0

范围
----
Combined = Small Business + Commercial Card（保留 Excel 原始收入单位）。
y[t] = 100 * (Combined[t] / Combined[t-12] - 1)，不是 log difference。
EC[t] = Combined[t] - IS 长期回归预测水平，短期只使用 EC_lag12。
复算文档第 1–6 节：长期/短期模型、筛选、回测、结构变化与 trend overlay。
不包括其他 y 变换、EC_lag1 或生产模型改进版。

一、另一台电脑的首次安装（只需本文件和两份原始 Excel）
------------------------------------------------------
1. 安装标准 64 位 CPython 3.14.5（不是 3.14.5t / free-threaded 版本）：
   https://www.python.org/downloads/release/python-3145/
   创建一个新的项目文件夹，放入本 .py 文件，避免混用其他项目的 .venv。
2. 在项目文件夹内建立 data 子文件夹，放入：
   data/Cardfee.xlsx
   data/Macro_Data_201001-202612.xls
   不需要 Word 或此前运行结果。原文参考数值已嵌入，仅用于比较，不用于估计。
3. 在系统终端或 Positron Terminal 中配置（以下是终端命令，不是 Python 语句）：

   Windows PowerShell（先确认 py -3.14 --version 确实输出 3.14.5）：
       py -3.14 --version
       py -3.14 combined_yoy12_reproduction.py --setup
       .\.venv\Scripts\python.exe combined_yoy12_reproduction.py --check-env
       .\.venv\Scripts\python.exe combined_yoy12_reproduction.py

   macOS / Linux（先确认 python3.14 --version 确实输出 3.14.5）：
       python3.14 --version
       python3.14 combined_yoy12_reproduction.py --setup
       ./.venv/bin/python combined_yoy12_reproduction.py --check-env
       ./.venv/bin/python combined_yoy12_reproduction.py

   --setup 会在本文件所在目录创建隔离的 .venv 并通过 pip 安装下方锁定版本。
   不下载 Python 本体，不修改系统 Python；需要联网访问 pip 配置的包源。
   如企业网络限制安装，请联系 IT 配置包源/代理，不要关闭 TLS 校验。
   重跑 --setup 可补全安装；若已有环境是其他 Python 版本，会报错而不覆盖。
   可用 --venv-dir 指定另一个环境目录。不要跨电脑复制 .venv，应重新创建。

二、在 Positron 中运行
--------------------
用 Open Folder 打开项目目录，在解释器选择器选择项目 .venv 的 Python 3.14.5，
启动该解释器的 Console Session，然后运行整个文件，或在该 Console 执行：
    import combined_yoy12_reproduction as audit
    audit.main([])
如果解释器未出现，可运行命令 “Interpreter: Discover All Interpreters”。
Positron 默认自带 Python kernel，本脚本不依赖 notebook / R / Excel 桌面应用。
https://positron.posit.co/python-installations.html
https://positron.posit.co/managing-interpreters.html

三、自定义路径与输出
------------------
终端：
    python combined_yoy12_reproduction.py --card "D:/project/Cardfee.xlsx" \
        --macro "D:/project/Macro_Data_201001-202612.xls" --output "D:/project/run_01"
上例反斜线续行适合 macOS/Linux；Windows 请把命令写在同一行。
Positron Console 可调用：
    audit.main(["--card", "D:/project/Cardfee.xlsx", "--macro", "D:/project/macros.xls"])
相对的 CLI 路径以当前工作目录为基准；默认 data 路径以 .py 所在目录为基准。
默认输出为 outputs/reproduction_yoy12_时间戳，每次新建，不覆盖原数据或旧结果。
也可修改下方 DEFAULT_CARD / DEFAULT_MACRO 与 sheet/header 设置。

首先看 RUN_SUMMARY.md、numeric_discrepancies.csv 和 numeric_claim_audit.csv。
CSV 使用 UTF-8 BOM。输出还包含全部候选试验、系数、月度预测、验证结果、
SHA-256、环境版本、requirements_resolved.txt 和带说明的 macro_dictionary.csv。
偏差不会导致失败退出；数据无效、环境不符或计算校验失败会明确报错。

四、复现口径和限制（重要）
------------------------
IS = 2011-01..2023-09；OOT = 2023-10..2025-12。
宏观水平先与 2011 起收入对齐，再做 12 个月变换；2010 历史仅用于敏感性。
Unrate 使用 12 个月百分点差，其余 7 个宏观使用 12 个月百分比变化。
原文固定变量模型重新拟合，与独立 forward selection 的结果分开保存。
VIF<10、长期普通残差 ADF<0.05 门槛、每类按 |OOT rho| 取前 2 个 lag，
是能匹配原文的一套推定配置，不声称这是唯一原始算法。保留替代规则实验。
文档短期平稳性以 IS 为主核查，同时列出 full-sample 替代值；文档对齐候选池
固定排除 CPI。全样本平稳性和 OOT 相关性参与筛选意味着 OOT 不是独立验证。
普通残差 ADF 与正式 Engle–Granger 分开输出，不混同统计结论。
dynamic_full 递归更新收入、y、EC，不使用 OOT 实际收入作预测输入；仍使用
表内相应宏观，因此是条件预测。dynamic_actual_EC 是故意保留真实 EC 的诊断
对照，不是有效动态回测。静态回测每期可用真实历史。全部使用未舍入系数。
原文数值比较容差按显示精度的半个末位单位；计数精确匹配，<p 按不等式。
源文件版本改变时，结果应改变；不得为了追平原文数值调整原始数据。
"""

# %% 0. 环境、可修改路径（此段仅使用标准库）
from pathlib import Path
from datetime import datetime, date
from decimal import Decimal
from importlib import metadata
import argparse
import hashlib
import json
import os
import platform
import re
import subprocess
import sys
import sysconfig
import traceback
import venv
import warnings

ROOT = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
EXPECTED_PYTHON = (3, 14, 5)
PINNED = {
    "numpy": "2.3.5", "pandas": "2.3.3", "scipy": "1.18.1",
    "statsmodels": "0.15.0", "patsy": "1.0.2", "openpyxl": "3.1.5",
    "xlrd": "2.0.2", "packaging": "25.0", "python-dateutil": "2.9.0.post0",
    "pytz": "2025.2", "tzdata": "2025.2", "et-xmlfile": "2.0.0", "six": "1.17.0",
    "formulaic": "1.2.2", "interface-meta": "2.0.1", "narwhals": "2.26.0",
    "typing-extensions": "4.16.0", "wrapt": "2.4.1",
}
REQUIREMENTS = [f"{name}=={version}" for name, version in PINNED.items()]
DEFAULT_CARD = ROOT / "data" / "Cardfee.xlsx"
DEFAULT_MACRO = ROOT / "data" / "Macro_Data_201001-202612.xls"
CARD_SHEET, CARD_HEADER = "汇总数据", 0  # header 为 0-based：Excel 第 1 行
MACRO_SHEET, MACRO_HEADER = "Macro Data", 1  # Excel 第 2 行
DATE_COLUMN = "YYYYMM"
CARD, MACRO, OUT = DEFAULT_CARD, DEFAULT_MACRO, None


def require_python():
    if (platform.python_implementation() != "CPython" or
            sys.version_info[:3] != EXPECTED_PYTHON or
            sysconfig.get_config_var("Py_GIL_DISABLED") or
            sys.maxsize <= 2**32):
        raise RuntimeError(
            "需要标准 64 位 CPython 3.14.5（非 free-threaded）。当前："
            f"{platform.python_implementation()} {platform.python_version()}；{sys.executable}。"
            "请安装正确版本，并在 Positron 选择对应解释器。"
        )


def environment_info(strict=True):
    versions, problems = {}, []
    for name, expected in PINNED.items():
        try:
            versions[name] = metadata.version(name)
        except metadata.PackageNotFoundError:
            versions[name] = None
        if versions[name] != expected:
            problems.append(f"{name}: 当前 {versions[name] or '未安装'}，要求 {expected}")
    info = dict(python=sys.version, executable=sys.executable, platform=platform.platform(),
                machine=platform.machine(), packages=versions, pinned=PINNED,
                version_mismatches=problems)
    if strict and problems:
        raise RuntimeError("依赖环境不符，请先在终端运行 --setup，然后选择生成的环境：\n" + "\n".join(problems))
    return info


def setup_environment(destination):
    require_python()
    if Path(destination).expanduser().is_symlink():
        raise ValueError("环境目录不能是符号链接。")
    destination = Path(destination).expanduser().resolve()
    if destination in (ROOT, Path.home(), Path(destination.anchor)):
        raise ValueError("环境目录必须是独立子目录，不能是项目根目录、主目录或磁盘根目录。")
    python = destination / ("Scripts/python.exe" if os.name == "nt" else "bin/python")
    if destination.exists():
        if not (destination / "pyvenv.cfg").is_file() or not python.is_file():
            raise ValueError(f"目录已经存在但不是可用 venv，不会覆盖：{destination}")
        probe = subprocess.check_output([str(python), "-c",
            "import sys,sysconfig; print('.'.join(map(str,sys.version_info[:3]))); "
            "print(bool(sysconfig.get_config_var('Py_GIL_DISABLED')))"] , text=True).splitlines()
        if probe != ["3.14.5", "False"]:
            raise ValueError(f"现有环境不是标准 Python 3.14.5：{destination}。请用 --venv-dir 选择新目录。")
    else:
        print(f"Creating isolated Python 3.14.5 environment: {destination}", flush=True)
        venv.EnvBuilder(with_pip=True).create(destination)
    print("Installing pinned packages (no source spreadsheets are accessed)...", flush=True)
    subprocess.run([str(python), "-m", "pip", "install", "--only-binary=:all:", *REQUIREMENTS], check=True)
    subprocess.run([str(python), "-m", "pip", "check"], check=True)
    print(f"\nEnvironment ready. Select this interpreter in Positron:\n{python}")
    return destination


def load_libraries():
    # 延迟导入：干净的 Python 也能执行 --help / --setup / --print-requirements。
    global np, pd, scipy, stats, statsmodels, sm
    global adfuller, kpss, coint, durbin_watson, jarque_bera
    global variance_inflation_factor, breaks_cusumolsresid, acorr_breusch_godfrey
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


# %% 1. 输入数据、月份与变量字典
MACRO_DESCRIPTIONS = [
    dict(group="Core", variable="Unrate", source_column=MAPPING["Unrate"],
         description="Household Survey: Unemployment Rate, (%, SA)",
         description_zh="住户调查失业率（%，季节调整）", short_run_transform="diff(12), percentage points"),
    dict(group="Core", variable="CPI", source_column=MAPPING["CPI"],
         description="CPI: Urban Consumer - All Items, (Index 1982-84=100, SA)",
         description_zh="城市消费者全项目 CPI（1982–84=100，季节调整）", short_run_transform="pct_change(12) * 100"),
    dict(group="Core", variable="DPI", source_column=MAPPING["DPI"],
         description="Income: Disposable Personal, (Bil. USD, SAAR)",
         description_zh="个人可支配收入（十亿美元，季节调整年率）", short_run_transform="pct_change(12) * 100"),
    dict(group="Small Business", variable="SBOpt", source_column=MAPPING["SBOpt"],
         description="Small business: Normalized optimism index, (Index 1986=100, SA)",
         description_zh="小企业标准化乐观指数（1986=100，季节调整）", short_run_transform="pct_change(12) * 100"),
    dict(group="Small Business", variable="GDP_Wholesale", source_column=MAPPING["GDP_Wholesale"],
         description="Gross Product Originating: Wholesale Trade, (Bil. USD, SAAR)",
         description_zh="批发贸易行业增加值（十亿美元，季节调整年率）", short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="IP", source_column=MAPPING["IP"],
         description="Industrial Production: Total, (Index 2017=100, SA)",
         description_zh="工业生产总指数（2017=100，季节调整）", short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="CorpProfits", source_column=MAPPING["CorpProfits"],
         description="Corporate Profits with IVA and CCAdj: Profits After Tax with IVA and CCAdj, (Bil. USD, SAAR)",
         description_zh="经存货估值及资本消耗调整的税后企业利润（十亿美元，季节调整年率）", short_run_transform="pct_change(12) * 100"),
    dict(group="Commercial", variable="DJ_TotalMkt", source_column=MAPPING["DJ_TotalMkt"],
         description="Dow Jones U.S. total stock market index - End of period, (Index, NSA)",
         description_zh="道琼斯美国全市场股票指数（期末指数，未季节调整）", short_run_transform="pct_change(12) * 100"),
]


def normalize_month(value):
    """支持 Excel datetime、YYYYMM 数字/文本和 ISO 日期；统一到月初。"""
    if pd.isna(value):
        raise ValueError("YYYYMM 存在空日期。")
    if isinstance(value, (pd.Timestamp, datetime, date, np.datetime64)):
        ts = pd.Timestamp(value)
    else:
        token = str(value).strip()
        if re.fullmatch(r"\d{6}(\.0+)?", token):
            ts = pd.to_datetime(token[:6], format="%Y%m")
        elif re.fullmatch(r"\d{4}[-/]\d{1,2}([-/]\d{1,2})?([ T].*)?", token):
            ts = pd.Timestamp(token)
        else:
            raise ValueError(f"无法识别 YYYYMM={value!r}，请提供 YYYYMM 或 ISO/Excel 日期。")
    return ts.to_period("M").to_timestamp()


def read_monthly(path, sheet, header, columns, first_month, last_month):
    """只读输入。不插值、不前向填充、不自动合并重复月份。"""
    path = Path(path)
    if not path.is_file():
        raise FileNotFoundError(f"找不到输入文件：{path}")
    suffix = path.suffix.lower()
    if suffix not in (".xls", ".xlsx", ".xlsm"):
        raise ValueError(f"需要 .xls / .xlsx / .xlsm 文件：{path}")
    engine = "xlrd" if suffix == ".xls" else "openpyxl"
    raw = pd.read_excel(path, sheet_name=sheet, header=header, engine=engine).dropna(how="all")
    required = [DATE_COLUMN] + list(columns)
    missing = [name for name in required if name not in raw.columns]
    if missing:
        raise ValueError(f"{path.name} / {sheet} 缺少列：{missing}；现有列：{list(raw.columns)}")
    chosen = raw[required].copy()
    chosen.index = pd.DatetimeIndex([normalize_month(v) for v in chosen.pop(DATE_COLUMN)], name="date")
    if chosen.index.duplicated().any():
        duplicates = chosen.index[chosen.index.duplicated()].strftime("%Y-%m").tolist()
        raise ValueError(f"{path.name} 出现重复月份（不允许自动汇总）：{duplicates}")
    was_sorted = chosen.index.is_monotonic_increasing
    chosen = chosen.sort_index()
    months = pd.date_range(first_month, last_month, freq="MS")
    absent = months.difference(chosen.index)
    if len(absent):
        raise ValueError(f"{path.name} 缺少需要的月份：{absent.strftime('%Y-%m').tolist()}")
    selected = chosen.loc[months].apply(pd.to_numeric, errors="raise").astype(float)
    finite = np.isfinite(selected.to_numpy())
    if not finite.all():
        r, c = np.where(~finite)
        examples = [(str(selected.index[i].date()), selected.columns[j]) for i, j in zip(r[:10], c[:10])]
        raise ValueError(f"{path.name} 含缺失/无穷数值：{examples}；不自动填充。")
    record = dict(file=path.name, sheet=sheet, header_zero_based=header, rows_read=len(raw),
                  rows_used=len(selected), rows_outside_scope=len(raw)-len(selected),
                  first_month=first_month, last_month=last_month, input_sorted=was_sorted,
                  duplicate_months=0, missing_months=0, invalid_required_values=0)
    return selected, record


def load_data():
    c, cq = read_monthly(CARD, CARD_SHEET, CARD_HEADER,
                         ["Commercial Card", "Small Business"], START, END)
    m, mq = read_monthly(MACRO, MACRO_SHEET, MACRO_HEADER,
                         list(MAPPING.values()), "2010-01-01", END)
    d = m.rename(columns={v: k for k, v in MAPPING.items()}).copy()
    d["Commercial Card"] = c["Commercial Card"]
    d["Small Business"] = c["Small Business"]
    d["Combined"] = d["Commercial Card"] + d["Small Business"]
    if not (d.loc[START:, "Combined"] > 0).all():
        raise ValueError("Combined 必须为正，才能按本文口径计算 YoY 和 MAPE。")
    if (d[[name for name in MAPPING if name != "Unrate"]] == 0).any().any():
        raise ValueError("某个宏观百分比变化的分母水平为零，需检查源数据。")
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


# %% 2–6. 所有文档实验与规则敏感性
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
    manifest.update(environment=environment_info(), reference_document=REFERENCE_DOCUMENT,
                    reference_input_sha256=REFERENCE_INPUT_SHA256,
                    reference_input_bytes_match={r['role']:r['sha256']==REFERENCE_INPUT_SHA256[r['role']] for r in inputs},
                    fixed_document_long_run=LR_TERMS, fixed_document_short_run=SR_TERMS)
    write_json('run_manifest.json', manifest)
    if not all(sha(r['path'])==r['sha256'] for r in inputs):
        raise RuntimeError('An input changed during execution; inspect source files and rerun.')
    return d, lr, sr, manifest



# %% 7. 原文参考值与逐项对照（绝不用于拟合）
REFERENCE_DOCUMENT = {
    "name": "Commercial_Card_and_Small_Business_Fee_Consolidated.docx",
    "sha256": "7e45500f8e39b562dddaa58354d2bbfc20251cf7ba2e0fa576d8edd0e9b4988f",
    "note": "固定原文版本。source_block为Word正文XML的0-based块号；source_page对应原文件渲染的5页。"
}
REFERENCE_INPUT_SHA256 = {
    "fee_source": "3efddeb29dbd481d7bdd26e4e322f17711e9b3f5aa00a9576e9f56f9c1f58f30",
    "macro_source": "3af42d8eb700037ed09080ecdc76dd07b51325cf51a0896998d9b28fa67d07fa"
}
# 仅嵌入用户文档的13张表；段落/公式中的对照值位于build_numeric_audit。
REFERENCE_TABLES = json.loads(r'''[
  {
    "block": 10,
    "type": "table",
    "rows": [
      [
        "Variable",
        "ADF p",
        "KPSS p",
        "Classification"
      ],
      [
        "Combined (target)",
        "0.932",
        "0.01",
        "I(1)"
      ],
      [
        "Unrate",
        "0.046",
        "0.01",
        "ambiguous — excluded"
      ],
      [
        "CPI",
        "0.999",
        "0.01",
        "I(1)"
      ],
      [
        "DPI",
        "0.999",
        "0.01",
        "I(1)"
      ],
      [
        "SBOpt",
        "0.337",
        "0.043",
        "I(1)"
      ],
      [
        "GDP_Wholesale",
        "0.995",
        "0.01",
        "I(1)"
      ],
      [
        "IP",
        "0.029",
        "0.1",
        "ambiguous — excluded"
      ],
      [
        "CorpProfits",
        "0.999",
        "0.01",
        "I(1)"
      ],
      [
        "DJ_TotalMkt",
        "0.958",
        "0.01",
        "I(1)"
      ]
    ]
  },
  {
    "block": 17,
    "type": "table",
    "rows": [
      [
        "Round",
        "Added",
        "New varp-value",
        "VIF ok",
        "Residual ADFp",
        "Stationary?"
      ],
      [
        "1",
        "CPI",
        "<0.001",
        "Yes",
        "0.009",
        "Yes"
      ],
      [
        "2",
        "SBOpt",
        "0.029",
        "Yes",
        "0.008",
        "Yes"
      ]
    ]
  },
  {
    "block": 23,
    "type": "table",
    "rows": [
      [
        "Term",
        "Coefficient",
        "p-value",
        "VIF"
      ],
      [
        "const",
        "-3766.52",
        "<0.001",
        "-"
      ],
      [
        "CPI",
        "19.436",
        "<0.001",
        "1.0"
      ],
      [
        "SBOpt",
        "3.872",
        "0.029",
        "1.0"
      ]
    ]
  },
  {
    "block": 35,
    "type": "table",
    "rows": [
      [
        "Variable",
        "ADF p",
        "KPSS p",
        "Result"
      ],
      [
        "Unrate",
        "0.046",
        "0.1",
        "OK"
      ],
      [
        "CPI",
        "0.265",
        "0.01",
        "FAIL"
      ],
      [
        "DPI",
        "0.002",
        "0.058",
        "OK"
      ],
      [
        "SBOpt",
        "0.081",
        "0.1",
        "OK"
      ],
      [
        "GDP_Wholesale",
        "0.319",
        "0.1",
        "OK"
      ],
      [
        "IP",
        "0.017",
        "0.1",
        "OK"
      ],
      [
        "CorpProfits",
        "0.13",
        "0.1",
        "OK"
      ],
      [
        "DJ_TotalMkt",
        "0.005",
        "0.1",
        "OK"
      ]
    ]
  },
  {
    "block": 42,
    "type": "table",
    "rows": [
      [
        "Candidate",
        "rho (IS)",
        "p (IS)",
        "rho (OOT)",
        "p (OOT)"
      ],
      [
        "Unrate_lag3",
        "-0.389",
        "<0.001",
        "-0.267",
        "0.178"
      ],
      [
        "Unrate_lag4",
        "-0.315",
        "<0.001",
        "-0.296",
        "0.133"
      ],
      [
        "Unrate_lag5",
        "-0.286",
        "0.001",
        "-0.366",
        "0.06"
      ],
      [
        "Unrate_lag6",
        "-0.3",
        "<0.001",
        "-0.452",
        "0.018"
      ],
      [
        "Unrate_lag7",
        "-0.295",
        "<0.001",
        "-0.452",
        "0.018"
      ],
      [
        "Unrate_lag8",
        "-0.323",
        "<0.001",
        "-0.649",
        "<0.001"
      ],
      [
        "Unrate_lag9",
        "-0.221",
        "0.011",
        "-0.296",
        "0.133"
      ],
      [
        "Unrate_lag10",
        "-0.15",
        "0.088",
        "-0.625",
        "<0.001"
      ],
      [
        "DPI_lag10",
        "0.123",
        "0.163",
        "0.439",
        "0.022"
      ],
      [
        "GDP_Wholesale_lag10",
        "0.21",
        "0.016",
        "0.348",
        "0.075"
      ],
      [
        "GDP_Wholesale_lag11",
        "0.124",
        "0.161",
        "0.437",
        "0.023"
      ],
      [
        "IP_lag3",
        "0.412",
        "<0.001",
        "0.263",
        "0.184"
      ],
      [
        "IP_lag11",
        "0.161",
        "0.068",
        "0.317",
        "0.107"
      ],
      [
        "CorpProfits_lag2",
        "0.528",
        "<0.001",
        "0.361",
        "0.065"
      ],
      [
        "CorpProfits_lag3",
        "0.473",
        "<0.001",
        "0.381",
        "0.05"
      ],
      [
        "CorpProfits_lag4",
        "0.378",
        "<0.001",
        "0.256",
        "0.198"
      ],
      [
        "CorpProfits_lag9",
        "0.424",
        "<0.001",
        "0.294",
        "0.137"
      ],
      [
        "CorpProfits_lag10",
        "0.401",
        "<0.001",
        "0.377",
        "0.052"
      ],
      [
        "CorpProfits_lag11",
        "0.357",
        "<0.001",
        "0.312",
        "0.113"
      ]
    ]
  },
  {
    "block": 47,
    "type": "table",
    "rows": [
      [
        "Round",
        "Added",
        "Adj. R² after"
      ],
      [
        "1",
        "y_lag1",
        "0.665"
      ],
      [
        "2",
        "y_lag6",
        "0.704"
      ],
      [
        "3",
        "CorpProfits_lag3",
        "0.713"
      ],
      [
        "4",
        "(none further pass at 5%)",
        "-"
      ]
    ]
  },
  {
    "block": 52,
    "type": "table",
    "rows": [
      [
        "Term",
        "Coefficient",
        "p-value",
        "VIF"
      ],
      [
        "const",
        "2.514",
        "0.005",
        "-"
      ],
      [
        "EC_lag12",
        "-0.0546",
        "<0.001",
        "1.22"
      ],
      [
        "y_lag1",
        "0.366",
        "<0.001",
        "2.29"
      ],
      [
        "y_lag6",
        "0.22",
        "<0.001",
        "1.26"
      ],
      [
        "CorpProfits_lag3",
        "0.214",
        "0.024",
        "2.09"
      ]
    ]
  },
  {
    "block": 56,
    "type": "table",
    "rows": [
      [
        "Method",
        "RMSE",
        "MAPE"
      ],
      [
        "Static, one-step-ahead (true history)",
        "485.8",
        "19.15%"
      ],
      [
        "Dynamic, fully recursive 27-month",
        "628.2",
        "25.62%"
      ]
    ]
  },
  {
    "block": 60,
    "type": "table",
    "rows": [
      [
        "Period",
        "Mean EC"
      ],
      [
        "In-sample (2011-2023)",
        "~0 (std 120)"
      ],
      [
        "OOT (2023-10 to 2025-12)",
        "-510 (std 265)"
      ],
      [
        "2023 (partial)",
        "-62"
      ],
      [
        "2024",
        "-399"
      ],
      [
        "2025",
        "-723"
      ]
    ]
  },
  {
    "block": 65,
    "type": "table",
    "rows": [
      [
        "Sample used",
        "CUSUM statistic",
        "p-value",
        "Verdict"
      ],
      [
        "In-sample only (2011-2023)",
        "0.951",
        "0.327",
        "Stable"
      ],
      [
        "Full sample (2011-2025)",
        "2.173",
        "0.0002",
        "Highly unstable"
      ]
    ]
  },
  {
    "block": 68,
    "type": "table",
    "rows": [
      [
        "Candidate breakdate",
        "F-statistic",
        "p-value",
        "n (pre / post)"
      ],
      [
        "2023-01-01",
        "88.1",
        "<0.0001",
        "144 / 36"
      ],
      [
        "2023-10-01",
        "85.39",
        "<0.0001",
        "153 / 27"
      ],
      [
        "2024-01-01",
        "85.44",
        "<0.0001",
        "156 / 24"
      ],
      [
        "2024-07-01",
        "66.24",
        "<0.0001",
        "162 / 18"
      ]
    ]
  },
  {
    "block": 71,
    "type": "table",
    "rows": [
      [
        "Rolling 30-month windowending",
        "CPI coefficient",
        "SBOpt coefficient"
      ],
      [
        "2022-06",
        "26.9",
        "27.7"
      ],
      [
        "2023-06",
        "24.6",
        "17.4"
      ],
      [
        "2023-09",
        "8.9",
        "-24.6"
      ],
      [
        "2024-06",
        "-1.3",
        "-34.8"
      ],
      [
        "2025-06",
        "-6.6",
        "-15.4"
      ],
      [
        "2025-12",
        "-11.9",
        "-11.4"
      ]
    ]
  },
  {
    "block": 81,
    "type": "table",
    "rows": [
      [
        "Test",
        "Result"
      ],
      [
        "Trend slope (fit on first 18 months only)",
        "-34.54/month, p<0.0001"
      ],
      [
        "MAPE on held-out last 9 months (genuine forecast)",
        "5.91%"
      ]
    ]
  }
]''')

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
                         numeric_status=('不可复算' if not np.isfinite(float(actual)) else '一致' if ok else '偏差'),
                         alternative_value=alt,alternative_matches=aok,issue=issue,note=note))

    # All numerical result cells in the 13 source tables, plus numerical claims
    # in prose and equations. Section numbers / dates used as identifiers are
    # represented by locators and window metadata rather than counted as results.
    for row in blocks[10]['rows'][1:]:
        name=row[0].replace(' (target)','')
        for col,key in [(1,'ADF_p'),(2,'KPSS_p')]:
            note='IS 2011-01至2023-09，n=153。'
            if key=='KPSS_p' and st.loc[('level',name),key] in [.01,.1]:note+='KPSS边界值，实际p可能在查表范围之外。'
            add('1.1',1,10,name,key,row[col],st.loc[('level',name),key],note=note,
                issue='IP两项检验均支持平稳；原文ambiguous标签不准确' if name=='IP' else '')
    for row in blocks[17]['rows'][1:]:
        r=lrtr[(lrtr['round']==int(row[0])) & (lrtr.candidate==row[1])].reindex(columns=lrtr.columns)
        r = r.iloc[0] if len(r) else pd.Series(dtype=float)
        for col,key in [(2,'p_added'),(4,'residual_ADF_p')]:
            add('1.2',1,17,row[1],key,row[col],r.get(key, np.nan),note='补全残差ADF<0.05门槛及VIF<10后复现。',issue='筛选规则未写全')
    for block,model,page,section in [(23,'long_run',2,'1.2'),(52,'short_run',4,'2.4')]:
        for row in blocks[block]['rows'][1:]:
            for col,key in [(1,'coefficient'),(2,'p'),(3,'VIF')]:
                if row[col]=='-':continue
                issue='系数舍入偏差' if model=='short_run' and row[0]=='CorpProfits_lag3' and key=='coefficient' else ''
                add(section,page,block,row[0],key,row[col],co.loc[(model,row[0]),key],issue=issue,
                    note='原文为0.214；本次未舍入重估值和差额见对应列。' if issue else '模型重新估计，使用普通OLS标准误。')
    for row in blocks[35]['rows'][1:]:
        for col,key in [(1,'ADF_p'),(2,'KPSS_p')]:
            add('2.2',2,35,row[0],key,row[col],st.loc[('yoy12',row[0]),key],
                alt=st.loc[('yoy12_full_sample',row[0]),key],issue='使用了含OOT的全样本',
                note='主复算为2012-01至2023-09，n=141；替代值为2012-01至2025-12，n=168。alternative_matches 单独标明全样本替代值是否匹配；不替换IS主值。')
    for row in blocks[42]['rows'][1:]:
        for col,key in [(1,'rho_IS'),(2,'p_IS'),(3,'rho_OOT'),(4,'p_OOT')]:
            add('2.3',3,42,row[0],key,row[col],pc.loc[row[0],key],
                note=f"候选独立删缺失；IS n={int(pc.loc[row[0],'n_IS'])}，OOT n=27。p使用残差Pearson检验的n−2自由度。",
                issue='Unrate相关性需重点核查' if row[0].startswith('Unrate') else 'partial correlation自由度及OOT参与筛选')
    for row in blocks[47]['rows'][1:4]:
        add('2.4',3,47,row[1],'adj_R2',row[2],srro.adj_R2.get(int(row[0]), np.nan),
            note='每类按|OOT rho|保留两项可复现；该排序规则未明示且不是唯一匹配配置。',issue='family cap规则未写全')
    for row,mode in zip(blocks[56]['rows'][1:],['static','dynamic_full']):
        for col,key in [(1,'RMSE'),(2,'MAPE')]:
            add('3',4,56,mode,key,row[col],bt.loc[mode,key],note='27个月；系数冻结。动态路径递归更新y、收入水平及EC。')
    for item,metric,claim in [('IS','mean','~0'),('IS','std_sample','120'),('OOT','mean','-510'),('OOT','std_sample','265'),('2023','mean','-62'),('2024','mean','-399'),('2025','mean','-723')]:
        add('4',4,60,item,metric,claim,ec.loc[item,metric],
            alt=ec.loc['2023_full_year','mean'] if item=='2023' else None,
            issue='2023 partial标签与数据不符' if item=='2023' else '',
            note='主值为2023年10–12月；替代值为2023全年12个月。' if item=='2023' else 'std使用ddof=1；均值使用冻结长期系数计算的EC。')
    for row,sample in zip(blocks[65]['rows'][1:],['IS','full']):
        for col,key in [(1,'stat'),(2,'p')]:
            add('5.1',4,65,sample,key,row[col],cu.loc[sample,key],note='该样本重新拟合CPI+SBOpt回归；CUSUM ddof=3。')
    for row in blocks[68]['rows'][1:]:
        for col,key in [(1,'F'),(2,'p')]:
            add('5.2',5,68,row[0],key,row[col],ch.loc[row[0],key],note='Chow含截距k=3；分子、分母自由度见chow.csv。')
        for claim,key in zip(row[3].split(' / '),['n_pre','n_post']):
            add('5.2',5,68,row[0],key,claim,ch.loc[row[0],key],kind='count')
    for row in blocks[71]['rows'][1:]:
        for col,key in [(1,'CPI'),(2,'SBOpt')]:
            add('5.3',5,71,row[0],key,row[col],roll.loc[row[0]+'-01',key],
                note='30个月含结束月。2022-06对应2020-01至2022-06。',
                issue='2022-06滚动系数偏差' if row[0]=='2022-06' else '')
    add('6.2',5,81,'first18','slope','-34.54',ov.loc['first18','slope'])
    add('6.2',5,81,'first18','slope_p','<0.0001',ov.loc['first18','slope_p'])
    add('6.2',5,81,'heldout9','MAPE','5.91%',ov.loc['first18','evaluation_MAPE'],note='2023-10至2025-03拟合趋势，2025-04至2025-12验证。使用该期表内宏观值，属于条件预测。')
    # Prose/equations: preserve duplicate appearances as separately located claims.
    add('范围',1,3,'IS','n','153',len(d.loc[START:CUT]),kind='count',note='由原文日期2011-01至2023-09推导；原文此段未直接列样本量。')
    add('范围',1,3,'OOT','n','27',len(d.loc['2023-10-01':END]),kind='count',note='由原文日期2023-10至2025-12推导。')
    add('1.1',1,12,'Combined','ADF_p','0.932',st.loc[('level','Combined'),'ADF_p'])
    add('1.1',1,12,'LR candidates','count','6',len(LR_CANDIDATES),kind='count')
    add('1.2',1,15,'LR candidates','count','6',len(LR_CANDIDATES),kind='count')
    for term,claim in [('const','-3766.52'),('CPI','19.44'),('SBOpt','3.87')]:
        add('1.2',2,22,term,'equation coefficient',claim,co.loc[('long_run',term),'coefficient'])
    for metric,claim in [('R2','.929'),('adj_R2','.929'),('n','153')]:
        add('1.2',2,25,'long_run',metric,claim,diag.loc['long_run',metric],kind='count' if metric=='n' else 'number')
    add('1.2',2,25,'EC','residual_ADF_p','.0081',read('cointegration_audit').iloc[0].p,
        alt=read('cointegration_audit').iloc[1].p,issue='普通残差ADF误作正式协整检验',
        note='主值为普通残差ADF；替代值为正式EG，不是同一个检验，不能混用结论。')
    add('2.1',2,29,'short_run_base','DW','1.02',diag.loc['short_run_base','DW'])
    add('2.3',3,41,'screen','survivors','19',int(pc.passed_after_stationarity.sum()),kind='count',issue='另见修正n−3自由度的候选筛选结果',note='按原文排除CPI后应用相关性门槛；原始及自由度修正结果另存。')
    add('2.3',3,41,'screen','total candidates','104',len(pc),kind='count',note='8个macro × lag0至12。')
    cap=read('short_run_capped_pool')
    for metric,claim,value in [('max lags per family','2',cap.groupby('family').size().max()),('macro pool','9',len(cap)),('target lag pool','12',12)]:
        add('2.3',3,44,'pool',metric,claim,value,kind='count',issue='family cap规则未写全')
    for block,term,claim in [(50,'const','2.514'),(50,'EC_lag12','-0.0546'),(50,'y_lag1','0.366'),(51,'y_lag6','0.220'),(51,'CorpProfits_lag3','0.214')]:
        add('2.4',4,block,term,'equation coefficient',claim,co.loc[('short_run',term),'coefficient'],
            issue='系数舍入偏差' if term=='CorpProfits_lag3' else '')
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
    save('numeric_discrepancies',ledger[ledger.numeric_status.ne('一致')])
    return ledger




# %% 8. 独立校验、运行摘要和命令行入口
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

    # 完全独立的递归实现：字典只初始化IS，未来实际收入根本不进入状态。
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

    # 另把生产函数的未来目标/残差全部置空，验证预测值本身不受影响。
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
    summary = dict(status="complete" if validations.passed.all() else "validation_failed",
                   claims=len(ledger), matches=int(counts.get("一致", 0)),
                   discrepancies=int(counts.get("偏差", 0)), unavailable=int(counts.get("不可复算", 0)),
                   independent_checks=len(validations), independent_checks_passed=int(validations.passed.sum()),
                   python=platform.python_version(), output=str(OUT),
                   long_run_selected=manifest["long_run_selected"],
                   short_run_selected=manifest["short_run_selected"],
                   backtests=backtests.to_dict(orient="records"))
    write_json("audit_summary.json", summary)
    differences = ledger[ledger.numeric_status.ne("一致")]
    text = ["# Combined YoY12 文档复现结果", "",
            f"Python {platform.python_version()}。核查 {len(ledger)} 处："
            f"一致 {summary['matches']}，偏差 {summary['discrepancies']}，不可复算 {summary['unavailable']}。",
            "计数包含表格、公式、段落中的重复出现及2个由文档日期推导的样本量。",
            f"独立实现校验：{summary['independent_checks_passed']}/{len(validations)} 通过。", "",
            "## 口径", "",
            "Combined = Small Business + Commercial Card；y = 12-month % change；EC = 冻结长期模型残差，取lag12。",
            "IS：2011-01至2023-09；OOT：2023-10至2025-12。收入单位与源Excel一致，MAPE列的单位为%。",
            "短期平稳性原文对照使用IS主值，full-sample结果在alternative_value列，不自动替代。", "",
            "## 模型与回测", "",
            "文档指定长期变量：CPI + SBOpt。指定短期变量：EC_lag12 + y_lag1 + y_lag6 + CorpProfits_lag3。",
            "以上变量集均从输入重新估计，系数没有硬编码。独立forward路径另列：", "",
            "- LR：" + " + ".join(manifest["long_run_selected"]),
            "- SR：" + " + ".join(manifest["short_run_selected"]), "",
            md_table(backtests), "",
            "dynamic_actual_EC仅为有真实EC输入的诊断对照，不是有效动态回测。",
            "dynamic_full不使用未来真实收入，但使用表内宏观。静态预测逐期使用实际历史。", "",
            "## 数值偏差", "",
            "容差为原文显示精度的半个末位单位；计数零容差；小于号按不等式核查。",
            "不可复算表示该对照所需候选/轮次在当前独立筛选路径中不存在或计算值非有限数，不能当作一致。", "",
            md_table(differences[["id", "section", "source_page", "item", "metric", "document_value",
                                   "recomputed_value", "difference", "numeric_status"]]), "",
            "完整223处原文定位、替代口径与核查提示见numeric_claim_audit.csv。", "",
            "## 规则与解释限制", "",
            "- 残差ADF门槛、VIF<10、按|OOT rho|每类取前2项是可复现的推定配置；替代规则CSV同时输出。",
            "- 普通残差ADF不是正式Engle–Granger。见cointegration_audit.csv，两项检验须分别解释。",
            "- 原文对齐路径排除CPI、以OOT相关性参与筛选，因此OOT不是完全独立验证集。",
            "- partial correlation主值复现残差Pearson的n−2自由度，同时保存n−3修正结果。",
            "- 候选逐模型删缺失会改变样本，另有共同样本对照。",
            "- 第6节full_oot趋势为事后拟合；first18拟合趋势后最后9个月对趋势参数留出。",
            "- 2023 partial与全年EC均值分别保存；30个月滚动窗口包含结束月。", "",
            "## 输入与环境追溯", "",
            "宏观描述见macro_dictionary.csv；原始月份/有效值检查见input_validation.csv。",
            "run_manifest.json记录实际路径、SHA-256、固定模型与推定规则；requirements_resolved.txt记录本次完整环境。",
            "输入SHA-256与最初核查版本是否逐字节一致：" + str(manifest["reference_input_bytes_match"]),
            "若不一致，只能说明文件字节发生变化，不自动等同于所用数据值发生变化。", ""]
    (OUT / "RUN_SUMMARY.md").write_text("\n".join(text), encoding="utf-8")
    return summary


def main(argv=None):
    """CLI及Positron均可调用；Positron建议main([])，避免继承kernel参数。"""
    global CARD, MACRO, OUT
    parser = argparse.ArgumentParser(description="Combined YoY12 / EC_lag12 文档逐项复现；Python 3.14.5")
    actions = parser.add_mutually_exclusive_group()
    actions.add_argument("--setup", action="store_true", help="创建独立venv并安装锁定依赖；不运行实验")
    actions.add_argument("--check-env", action="store_true", help="显示并严格检查Python和依赖版本")
    actions.add_argument("--print-requirements", action="store_true", help="仅打印锁定依赖（不需第三方包）")
    parser.add_argument("--venv-dir", type=Path, default=ROOT / ".venv", help="仅--setup使用，默认脚本目录/.venv")
    parser.add_argument("--card", type=Path, default=DEFAULT_CARD, help="原始收入Excel")
    parser.add_argument("--macro", type=Path, default=DEFAULT_MACRO, help="原始宏观Excel")
    parser.add_argument("--output", type=Path, help="新/空输出目录；默认在脚本目录下按时间戳创建")
    args = parser.parse_args(argv)
    if args.print_requirements:
        print("\n".join(REQUIREMENTS))
        return None
    if args.setup:
        return setup_environment(args.venv_dir)
    require_python()
    if args.check_env:
        print(json.dumps(environment_info(strict=False), ensure_ascii=False, indent=2))
        environment_info()
        print("Environment check passed.")
        return None
    environment_info()
    load_libraries()
    CARD, MACRO = args.card.expanduser().resolve(), args.macro.expanduser().resolve()
    for path in (CARD, MACRO):
        if not path.is_file():
            raise FileNotFoundError(f"找不到 {path}。请按文件顶部说明放置数据，或传入 --card / --macro。")
    OUT = (args.output or ROOT / "outputs" / ("reproduction_yoy12_" + datetime.now().strftime("%Y%m%d_%H%M%S_%f"))).expanduser().resolve()
    if OUT.exists() and (not OUT.is_dir() or any(OUT.iterdir())):
        raise FileExistsError(f"输出路径已存在且不是空目录，不会覆盖：{OUT}")
    OUT.mkdir(parents=True, exist_ok=True)
    write_json("run_status.json", dict(status="running", started=datetime.now().astimezone().isoformat()))
    try:
        (OUT / "requirements_pinned.txt").write_text("\n".join(REQUIREMENTS) + "\n", encoding="utf-8")
        resolved = sorted(f"{p.metadata['Name']}=={p.version}" for p in metadata.distributions() if p.metadata.get("Name"))
        (OUT / "requirements_resolved.txt").write_text("\n".join(resolved) + "\n", encoding="utf-8")
        write_json("reference_document_tables.json", dict(document=REFERENCE_DOCUMENT, tables=REFERENCE_TABLES))
        print(f"Running all experiments. Output: {OUT}", flush=True)
        d, lr, sr, manifest = run_experiments()
        ledger = build_numeric_audit(d)
        validations = independent_validation(d, lr, sr, manifest)
        summary = build_summary(ledger, validations, manifest)
        if not validations.passed.all():
            raise RuntimeError("独立计算校验未通过，请查看independent_validation.csv，不应接受本次结果。")
        write_json("run_status.json", dict(status="complete", finished=datetime.now().astimezone().isoformat()))
        print(f"\nCompleted: {summary['claims']} claims, {summary['matches']} matched, "
              f"{summary['discrepancies']} discrepancies, {summary['unavailable']} unavailable.")
        print(f"Read: {OUT / 'RUN_SUMMARY.md'}", flush=True)
        return summary
    except Exception as exc:
        (OUT / "error.log").write_text(traceback.format_exc(), encoding="utf-8")
        write_json("run_status.json", dict(status="failed", error=str(exc), finished=datetime.now().astimezone().isoformat()))
        raise


if __name__ == "__main__":
    # 兼容Positron整文件执行；在Notebook/Console内不要读取kernel的-f等参数。
    interactive = "ipykernel" in sys.modules or hasattr(sys, "ps1")
    try:
        main([] if interactive else None)
    except (ValueError, RuntimeError, OSError, subprocess.CalledProcessError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        if not interactive:
            sys.exit(1)
