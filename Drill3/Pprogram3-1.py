# -*- coding: utf-8 -*-
"""
Created on Sun Sep 26 01:55:22 2021

@author: Chen
"""
import numpy as np
from Re import RE

# Z ~ N(0,1) will be enough
# Note: location-scale family. Z = (X-mu)/sigma

ITER = 100000
n = 5

#--------------------------------------------------------------------
def HL(x, estimator, na_rm=False):
    #estimator = match.arg(estimator)
    if na_rm:
        x = x[~np.isnan(x)]
    #xx = np.outer(x, x, "+")
    xx = x[:,None]+x
    if estimator == 'HL1':
        HL_estimation = 0.5 * np.median(xx[np.tril_indices(len(xx), -1)])                                        
    elif estimator == 'HL2':
        HL_estimation = 0.5 * np.median(xx[np.tril_indices(len(xx))])
                                        
    else:
        HL_estimation = 0.5 * np.median(xx)
        
    return HL_estimation

#--------------------------------------------------------------------


MU1 = np.zeros(ITER)
MU2 = np.zeros(ITER)
MU3 = np.zeros(ITER)

for i in range(ITER):
    Z = np.random.normal(size=n)
    MU1[i] = np.median(Z)
    MU2[i] = np.mean(Z)
    MU3[i] = HL(Z,'HL1')

print(np.var(MU1))
print(np.var(MU2))
print(np.var(MU3))

# ====================================================================


print(np.var(MU2) / np.var(MU1))
print(RE(5, 'median'))

print(RE(5, 'mean'))
print(np.var(MU2) / np.var(MU2))

print(RE(5, 'HL1'))
print(np.var(MU2) / np.var(MU3))