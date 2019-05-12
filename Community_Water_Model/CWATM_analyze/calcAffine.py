# -*- coding: utf-8 -*-
"""
Created on Thu Jun 14 20:08:57 2018

@author: wangy

Translation from the source file of
https://www.nmr.mgh.harvard.edu/PMI/toolbox/Documentation/calcaffine.xhtml

Given transformed coordinates, calculate the affine parameters that can be 
used to translate grid pixels to the coordinates.

Syntax:    [A,B] = calcAffine(x,y,u,v);
Inputs:    x,y    Initial coordinates
           u,v    Final coordinates
Outputs:   A      Transformation matrix
           B      Displacement vector
"""
import numpy as np

def calcAffine(x, y, u, v):
   
    A = np.zeros([6,6])
    B = np.zeros(6)
    
    # Compute the individual matrix terms
    sx  = sum(x)
    sy  = sum(y) 
    su  = sum(u)
    sv  = sum(v)

    sxx = sum(x * x)
    sxy = sum(x * y)
    syy = sum(y * y)

    sux = sum(u * x)
    suy = sum(u * y)    
    svx = sum(v * x)
    svy = sum(v * y)

    s = len(x) # length(x) or 1 ???

    # Construct the LLS matrix, form never changes

    A[0,:] = [sxx, sxy, 0, 0, sx, 0]
    A[1,:] = [sxy, syy, 0, 0, sy, 0]
    A[2,:] = [0, 0, sxx, sxy, 0, sx]
    A[3,:] = [0, 0, sxy, syy, 0, sy]
    A[4,:] = [sx, sy, 0, 0, s, 0]
    A[5,:] = [0, 0, sx, sy, 0, s]

    # Construct the vector, again form is constant
    B = np.array([sux, suy, svx, svy, su, sv])

    # Find the least-squares solution
    C = np.matmul(np.linalg.inv(A), B)
    
    #print(C)

    C = list(C[[0,2,4,1,3,5]])

    #print(C)

    return C