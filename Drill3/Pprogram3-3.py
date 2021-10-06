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
def HL1a(x):
    n = len(x)
    yij = []
    for i in range(n-1):
        for j in range(i+1, n):     ### <----------
            """
            #run faster
            med_value = (x[i]+x[j])/2
            yij.append(med_value)
            """
            yij=np.append(yij,(x[i]+x[j])/2)
    return np.median(yij)
#
@print_execute_time
def HL1b(x):
    n = len(x)
    yij = []
    for i in range(n-1):
        yij = np.append(yij, x[i]+x[i+1:n])
    return np.median(yij/2)
#-----------------------------
@print_execute_time
def HL2a(x):
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
#
@print_execute_time
def HL2b(x):
    n = len(x)
    yij = []
    for i in range(n):
        yij = np.append(yij, x[i]+x[i:n])
    return np.median(yij/2)
#-----------------------------
@print_execute_time
def HL3a(x):
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

@print_execute_time
def HL3b(x):
    n = len(x)
    yij = []
    for i in range(n):
        yij = np.append(yij, x[i]+x[0:n])
    return np.median(yij/2)
#-----------------------------


#===================================================================
# Testing the above functions

np.random.seed(1)
x = np.random.uniform(size=99)
print(f'HL(x, "HL1") for x = runif(99): {HL.__wrapped__(x, "HL1")}')
print(f'HL1a(x) for x = runif(99): {HL1a.__wrapped__(x)}')
print(f'HL1b(x) for x = runif(99): {HL1b.__wrapped__(x)}')
 
print(f'HL(x, "HL2") for x = runif(99): {HL.__wrapped__(x, "HL2")}')
print(f'HL2a(x) for x = runif(99): {HL2a.__wrapped__(x)}')
print(f'HL2b(x) for x = runif(99): {HL2b.__wrapped__(x)}')

print(f'HL(x, "HL3") for x = runif(99): {HL.__wrapped__(x, "HL3")}')
print(f'HL3a(x) for x = runif(99): {HL3a.__wrapped__(x)}')
print(f'HL3b(x) for x = runif(99): {HL3b.__wrapped__(x)}')


#===================================================================
# Their performance
if __name__ == '__main__':

    n = 200
    x = np.random.normal(size=n)
    print('when n=200, running time and values in different programming ways:')
    # HL1
    print('HL1:')
    print( HL(x, "HL1") )
    print( HL1a(x) )
    print( HL1b(x) )
    
    # HL2
    print('HL2:')
    print( HL(x, "HL2") )
    print( HL2a(x) )
    print( HL2b(x) )
    
    # HL3
    print('HL3:')
    print( HL(x, "HL3") )
    print( HL3a(x) )
    print( HL3b(x) )
    

    print('when n=500, running time and values in different programming ways:')
    #===================================================================
    # Their performance
    n = 500
    x = np.random.normal(size=n)
    # HL1
    print('HL1:')
    print( HL(x, "HL1") )
    print( HL1a(x) )
    print( HL1b(x) )
    
    # HL2
    print('HL2:')
    print( HL(x, "HL2") )
    print( HL2a(x) )
    print( HL2b(x) )
    
    # HL3
    print('HL3:')
    print( HL(x, "HL3") )
    print( HL3a(x) )
    print( HL3b(x) )
