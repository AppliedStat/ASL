# -*- coding: utf-8 -*-
"""
Created on Sun Sep 26 01:55:22 2021

@author: Chen
"""
import numpy as np

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

def RE(n,estimator,ITER=1000000):
    MU1 = np.zeros(ITER)
    MU2 = np.zeros(ITER)
    MU3 = np.zeros(ITER)
    MU4 = np.zeros(ITER)
    MU5 = np.zeros(ITER)
    
    for i in range(ITER):
        Z = np.random.normal(size=n)
        MU1[i] = np.median(Z)
        MU2[i] = np.mean(Z)
        MU3[i] = HL(Z,'HL1')
        MU4[i] = HL(Z,'HL2')
        MU5[i] = HL(Z,'HL3')
    if estimator == 'median':
        return np.var(MU2) / np.var(MU1)
    elif estimator == 'mean':
        return 1
    elif estimator == 'HL1':
        return np.var(MU2) / np.var(MU3)
    elif estimator == 'HL2':
        return np.var(MU2) / np.var(MU4)
    else:
        return np.var(MU2) / np.var(MU5)

print(RE(5,'median'))