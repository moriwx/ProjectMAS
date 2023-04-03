
import numpy as np
import pandas as pd
from scipy import stats

raw = pd.DataFrame({'品种':'''A
B
C
D
C
B
A
D
D
A
C
B
C
D
A
B
A
C
B
D
A
D
B
C'''.split(), '环境':'''1
1
1
1
2
2
2
2
3
3
3
3
4
4
4
4
5
5
5
5
6
6
6
6'''.split(), '产量':list(map(float, '''90.3
92.5
85.5
82.5
90.8
89.5
89.2
89.5
85.6
98.2
89.6
90.6
86.2
87.4
93.9
94.7
87.4
88.0
87.0
78.9
97.9
90.7
95.8
93.4'''.split()))})
df = raw.pivot(index='环境', columns='品种', values='产量')

# 1. 4.
data = df.values
x = data.mean()
k = len(data[0]) # 处理数
b = len(data) # 区组数
n = k * b
SStotal = data.var() * n
SST = (np.square(data.mean(0)-x)).sum() * b
SSB = (np.square(data.mean(1)-x)).sum() * k
SSE = SStotal - SST - SSB
df_total, df_t, df_b = n-1, k-1, b-1
df_e = df_t * df_b
MST, MSB, MSE = SST/df_t, SSB/df_b, SSE/df_e
FT = MST/MSE
FB = MSB/MSE
pt = stats.f.sf(FT, df_t, df_e)
pb = stats.f.sf(FB, df_b, df_e)
pd.DataFrame(dict(zip(['平方和', '自由度', '均方和', 'F比', 'p值'],
                      [[SST,SSB,SSE,SStotal], [df_t,df_b,df_e,df_total], [MST,MSB,MSE,np.nan],
                       [FT,FB,np.nan,np.nan], [pt,pb,np.nan,np.nan]])),
             index = ['处理','区组','误差','总和'])
# OR
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
model = ols('产量 ~ C(品种) + C(环境)', data = raw).fit()
anova_lm(model, type = 2)

# 2.
4.08 * np.sqrt(MSE/b)
raw.groupby('品种')['产量'].mean()
np.array([i-j for i in df.mean() for j in df.mean()]).reshape((4,4))

# 3.
x3 = data - data.mean(0) - data.mean(1).reshape(6,1) + x
stats.kstest(x3.reshape(24),'norm')
stats.shapiro(x3)
stats.levene(x3[:,0], x3[:,1], x3[:,2], x3[:,3])

# 5.
stats.f_oneway(df.A, df.B, df.C, df.D)
