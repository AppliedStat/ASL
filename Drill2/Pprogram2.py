# -*- coding: utf-8 -*-
"""
Created on Sat Sep 25 17:52:56 2021

@author: Chen
"""
import numpy as np
from SCD import weighted_median
import matplotlib.pyplot as plt

# ---------------------------------
# Equal Weights
# ---------------------------------

# 1 (a): Odd number of observations
x = np.arange(0, 6, 0.01)
fn1 = 0.2*abs(x-1) + 0.2*abs(x-2) + 0.2*abs(x-3) + 0.2*abs(x-4) + 0.2*abs(x-5) 
plt.plot(x,fn1)
plt.xlabel("x")
plt.ylabel("fn1(x)")
plt.show()

# 1 (b): Even number of observations 
fn2 = 0.25*abs(x-1) + 0.25*abs(x-2) + 0.25*abs(x-3) + 0.25*abs(x-4) 
plt.plot(x,fn2)
plt.xlabel("x")
plt.ylabel("fn2(x)")
plt.show() 


# ---------------------------------
# Unequal Weights
# ---------------------------------

# 2 (a): Odd number of observations 
fn3 = 0.3*abs(x-1) + 0.2*abs(x-2) + 0.1*abs(x-3) + 0.2*abs(x-4) + 0.2*abs(x-5) 
plt.plot(x,fn3)
plt.xlabel("x")
plt.ylabel("fn3(x)")
plt.show() 

# 2 (b) Even number of observations 
fn4 = 0.25*abs(x-1) + 0.30*abs(x-2) + 0.20*abs(x-3) + 0.25*abs(x-4) 
plt.plot(x,fn4)
plt.xlabel("x")
plt.ylabel("fn4(x)")
plt.show()



# =============================================
# Using median and weighted.median
# =============================================

# ---------------------------------
# Equal Weights
# ---------------------------------

# 1 (a): Odd number of observations 
print(np.median([1,2,3,4,5]))

# 1 (b): Even number of observations 
print(np.median([1,2,3,4]))

print(weighted_median([1,2,3,4,5], [0.3, 0.2, 0.1, 0.2, 0.2]))
print(weighted_median([1,2,3,4], [0.25, 0.3, 0.2, 0.24]))