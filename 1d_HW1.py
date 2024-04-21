import math
import numpy as np
from scipy.stats import f, t
xbar = .125
ybar = 45.7886
lxy = 25.5218
lyy = 2432.4566
lxx = .3024
n = 16.
beta1 = lxy/lxx
beta1

beta0 = ybar - beta1*xbar
beta0

1/n+xbar**2/lxx

1/lxx

-xbar/lxx/math.sqrt((1/n+xbar**2/lxx)/lxx)

sst=lyy
sst

ssr=beta1**2*lxx
ssr

sse = sst-ssr
sse

sse/(n-2)

ssr/sse*(n-2)

f.isf(q=.05, dfn=1, dfd=14)

2.63/np.sqrt(10.72)

t.isf(.05/6, 19)/np.sqrt(f.isf(q=.05, dfn=3, dfd=17)*19*3/17)

9.965 - t.isf(.05/6, 19)*np.sqrt(3.628/20)

c1=np.array([1,-1,0])
c2=np.array([1,0,-1])
c3=np.array([0,1,-1])
S=np.array([[101.3, 63.0, 71.0],[63.0 ,80.2, 55.6],[71.0 ,55.6, 97.4]])
np.sqrt(np.dot(np.dot(c2,S),c2.T)/40)*np.sqrt(f.isf(q=.05, dfn=2, dfd=38)*39*2/38)

np.sqrt(np.dot(np.dot(c3,S),c3.T)/40)*np.sqrt(f.isf(q=.05, dfn=2, dfd=38)*39*2/38)

f.isf(q=.05, dfn=2, dfd=38)*39*2/38

1 - f.cdf(ssr/sse*14, 1, 14)

sigmahat = math.sqrt(sse/14)
sigmahat

cir = sigmahat * t.isf(.025, 14)
cir

(beta1-cir, beta1+cir)

yhat = beta0+beta1*.15
yhat

cir1 = t.isf(.025, 14) * sigmahat * math.sqrt(17/16+(.15-xbar)**2/lxx)
cir1

(yhat-cir1, yhat+cir1)
