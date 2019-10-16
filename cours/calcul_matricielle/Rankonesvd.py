# -*- coding: utf-8 -*-
"""
Created on Wed Oct  9 07:22:37 2019

@author: pc
"""



import numpy as np
from numpy.linalg import norm
from random import normalvariate
from math import sqrt
from scipy.io import loadmat
import matplotlib.pylab as plt
import pandas as pd

def randomUnitVector(n):
    unnormalized = [normalvariate(0, 1) for _ in range(n)]
    theNorm = sqrt(sum(x * x for x in unnormalized))
    return [x / theNorm for x in unnormalized]


def svd_1d(X, epsilon=1e-10):
    A = np.array(X)
    
    n, m = A.shape
    
    # Transformation de A en en matrice stochastique X de dimension (n+m) * (n+m)
    Dr = np.diag(A.sum(axis=1))
    Dc = np.diag(A.sum(axis=0))
    
    Dc_1 = np.linalg.inv(Dc)
    Dr_1 = np.linalg.inv(Dr)
    
    col1 = np.concatenate([np.zeros((n,n)), np.dot(Dc_1 , A.T)])
    col2 = np.concatenate([np.dot(Dr_1 , A), np.zeros((m,m))])
    
    S = np.concatenate([col1, col2], axis=1)
    
    # initialisation du vecteur currentV
    x = randomUnitVector(n+m)
    lastV = None
    currentV = x
    
    lastE = np.linalg.norm(currentV)

    # Itérations 
    iterations = 0
    while True:
        iterations += 1
        lastV = np.array(currentV)
        currentV = np.dot(S, lastV)
        currentV = currentV / norm(currentV)
        
        last_u = lastV[list(range(0,n))]
        last_v = lastV[list(range(n,n+m))]
        
        current_u = currentV[list(range(0,n))]
        current_v = currentV[list(range(n,n+m))]
        
        e_u = np.linalg.norm(current_u - last_u)
        e_v = np.linalg.norm(current_v - last_v)
        
        currentE = e_u + e_v
        
        d = abs(currentE - lastE)
        lastE = currentE
        
        if d <= epsilon:
            print("converged in {} iterations!".format(iterations))

            #u = currentV[range(0,n)]
            #v = currentV[range(n,n+m)]
            
            return current_u, current_v
        
def reorder(X, u, v):
    """
        return reordered rows and columns of X with respect to u and v
    """
    sort_u = u.argsort()
    sort_v = v.argsort()

    reordered_X = X[:, sort_v]
    reordered_X = reordered_X[sort_u, :]
    
    return reordered_X

X = np.array([[0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0],
                 [0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0],
                 [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0],
                 [1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1],
                 [0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0],
                 [1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1],
                 [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0],
                 [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0],
                 [0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0]])

columns=["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P"]
index=["HighSchool", "AgricultCoop", "Railstation", "OneRoomSchool", "Veterinary", "NoDoctor", "NoWaterSupply",  "PoliceStation", "LandReallocation"]

pd.DataFrame(X, columns=columns, index = index)


# dos = randomUnitVector(3)
dos = normalvariate(0,1)
print(dos)

# u, v = svd_1d(X, 1e-5)

# sort_u = u.argsort()
# sort_v = v.argsort()

# columns=["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P"]
# columns = np.array(columns)
# columns = columns[sort_v]

# index=["HighSchool", "AgricultCoop", "Railstation", "OneRoomSchool", "Veterinary", "NoDoctor", "NoWaterSupply",  "PoliceStation", "LandReallocation"]
# index = np.array(index)
# index = index[sort_u]

# reordered_X = reorder(X, u, v)

# pd.DataFrame(reordered_X, index = index, columns = columns)




# plt.figure(figsize = (8,4.5))
# plt.imshow(X, cmap='binary', aspect='auto')
# plt.show()

# plt.figure(figsize = (8,4.5))
# plt.imshow(reordered_X, cmap='binary', aspect='auto')
# plt.show()



# ## CSTR Dataset

# mat = loadmat("data/cstr.mat")
# X = mat["fea"]

# X.shape

# u, v = svd_1d(X, 1e-5)
# reordered_X = reorder(X, u, v)


# x, y = X.nonzero()
# plt.figure(figsize = (15,15))
# plt.subplot(1,2,1)
# plt.scatter(x,y, s=0.3, c=X[x,y], cmap = "binary")
# plt.title("Before RankOneSVD")

# x, y = reordered_X.nonzero()
# plt.subplot(1,2,2)
# plt.scatter(x,y, s=0.3, c=reordered_X[x,y], cmap = "binary")
# plt.title("After RankOneSVD")
# plt.show()








