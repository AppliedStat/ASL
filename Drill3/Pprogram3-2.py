# -*- coding: utf-8 -*-
"""
Created on Mon Sep 27 01:53:08 2021

@author: Chen
"""

import numpy as np
from time import time
from functools import wraps

def print_execute_time(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        
        start = time()
        func_return = func(*args, **kwargs)
        end = time()

        print(f'Total execution time of {func.__name__}: {end - start}')

        return func_return

    return wrapper


# How to program HL estimator
#===================================================================
@print_execute_time
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
#===================================================================
@print_execute_time
def HL1(x):
    n = len(x)
    yij = []
    for i in range(n-1):
        for j in range(i+1, n):     ### <----------
            """
            #The comments run faster
            med_value = (x[i]+x[j])/2
            yij.append(med_value)
            """
            yij=np.append(yij,(x[i]+x[j])/2)
    return np.median(yij)
#-----------------------------
@print_execute_time
def HL2(x):
    n = len(x)
    yij = []
    for i in range(n):
        for j in range(i, n):     ### <----------
            """
            #run faster
            med_value = (x[i]+x[j])/2
            yij.append(med_value)
            """
            yij=np.append(yij,(x[i]+x[j])/2)
    return np.median(yij)
#-----------------------------
@print_execute_time
def HL3(x):
    n = len(x)
    yij = []
    for i in range(n):
        for j in range(n):     ### <----------
            """
            #run faster
            med_value = (x[i]+x[j])/2
            yij.append(med_value)
            """
            yij=np.append(yij,(x[i]+x[j])/2)
    return np.median(yij)
#-----------------------------


#===================================================================
# Testing the above functions

np.random.seed(1)
x = np.random.uniform(size=99)
print(f'HL(x, "HL1") for x = runif(99): {HL.__wrapped__(x, "HL1")}')
print(f'HL1(x) for x = runif(99): {HL1.__wrapped__(x)}')
 
print(f'HL(x, "HL2") for x = runif(99): {HL.__wrapped__(x, "HL2")}')
print(f'HL2(x) for x = runif(99): {HL2.__wrapped__(x)}')

print(f'HL(x, "HL3") for x = runif(99): {HL.__wrapped__(x, "HL3")}')
print(f'HL3(x) for x = runif(99): {HL3.__wrapped__(x)}')

#===================================================================
# Their performance
if __name__ == '__main__':

    n = 10
    x = np.random.normal(size=n)
    print('when n=10, running time and values in different programming ways:')
    # HL1
    print('HL1:')
    print( HL(x, "HL1") )
    print( HL1(x) )
    
    # HL2
    print('HL2:')
    print( HL(x, "HL2") )
    print( HL2(x) )
    
    # HL3
    print('HL3:')
    print( HL(x, "HL3") )
    print( HL3(x) )
    
    print('when n=50, running time and values in different programming ways:')
    #===================================================================
    # Their performance
    n = 50
    x = np.random.normal(size=n)
    # HL1
    print('HL1:')
    print( HL(x, "HL1") )
    print( HL1(x) )
    
    # HL2
    print('HL2:')
    print( HL(x, "HL2") )
    print( HL2(x) )
    
    # HL3
    print('HL3:')
    print( HL(x, "HL3") )
    print( HL3(x) )
    
    print('when n=100, running time and values in different programming ways:')
    #===================================================================
    # Their performance
    n = 100
    x = np.random.normal(size=n)
    # HL1
    print('HL1:')
    print( HL(x, "HL1") )
    print( HL1(x) )
    
    # HL2
    print('HL2:')
    print( HL(x, "HL2") )
    print( HL2(x) )
    
    # HL3
    print('HL3:')
    print( HL(x, "HL3") )
    print( HL3(x) )
    
    print('when n=500, running time and values in different programming ways:')
    #===================================================================
    # Their performance
    n = 500
    x = np.random.normal(size=n)
    # HL1
    print('HL1:')
    print( HL(x, "HL1") )
    print( HL1(x) )
    
    # HL2
    print('HL2:')
    print( HL(x, "HL2") )
    print( HL2(x) )
    
    # HL3
    print('HL3:')
    print( HL(x, "HL3") )
    print( HL3(x) )
