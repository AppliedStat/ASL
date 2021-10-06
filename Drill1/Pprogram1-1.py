# -*- coding: utf-8 -*-
"""
Created on Fri Sep 24 20:59:51 2021

@author: Chen
"""

import numpy as np

"""
# 1. Variance
"""
# 2 (a) 
ITER = 100000
n = 5
mu = 10; sigma=10

# NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

SUM = np.zeros(ITER)
for i in range(ITER):
    Xi = np.random.normal(loc=mu, scale=sigma, size=n)
    SUM[i] = np.sum((Xi-mu)**2) 

print(np.mean(SUM)/sigma**2)


# 2 (b) 
ITER = 100000
n = 5
mu = 10; sigma=10

# NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

SUM = np.zeros(ITER)
for i in range(ITER):
    Xi = np.random.normal(loc=mu, scale=sigma, size=n)
    Xbar = np.mean(Xi)
    SUM[i] = np.sum((Xi-Xbar)**2) 

print(np.mean(SUM)/sigma**2)


# 3 (b) 
ITER = 100000
n = 5
mu = 1; sigma=mu     # It works even with different mu

# NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

SUM = np.zeros(ITER)
for i in range(ITER):
    Xi = np.random.exponential(scale=mu, size=n)
    Xbar = np.mean(Xi)
    SUM[i] = np.sum((Xi-Xbar)**2) 

print(np.mean(SUM)/sigma**2)
