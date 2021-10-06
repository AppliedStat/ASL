# -*- coding: utf-8 -*-
"""
Created on Sat Sep 25 02:07:59 2021

@author: Chen
"""

import numpy as np
import math

# ========================
# 2. Standard Deviation
# ========================

# 3 
ITER = 100000
n = 5
mu =0; sigma=1   ## It works even with different mu and sigma
  
# NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2
  
SUM = np.zeros(ITER)
for i in range(ITER):
    Xi = np.random.normal(loc=mu, scale=sigma, size=n)
    SUM[i] = np.std(Xi, ddof = 1) 

print(np.mean(SUM)/sigma)


def c4A(n):
    return np.sqrt(2/(n-1))*math.gamma(n/2)/math.gamma(n/2-1/2)

# ========================
# Using a log-gamma 
# ------------------------
def c4B(n):
    tmp = math.lgamma(n/2) - math.lgamma(n/2-1/2)
    return np.sqrt(2/(n-1)) * math.exp(tmp)

# ========================
# Using a simulation 
# ------------------------
def c4C(n, iter=1000):
    m1 = 0
    for i in range(1,iter+1):
        m1 = m1 + np.std(np.random.normal(size=n),ddof = 1) / iter
    return m1

# ------------------------

# double-check
print(c4A(n))
print(c4B(n))
print(c4C(n))

# 4 
ITER = 100000
n = 5
mu = 1; sigma=mu

# NOTE: K1 = average values of sum(X_i-mu)^2 / sigma^2

SUM = np.zeros(ITER)
for i in range(ITER):
    Xi = np.random.exponential(scale=mu, size=n)
    SUM[i] = np.std(Xi, ddof = 1) 

print(np.mean(SUM)/sigma)