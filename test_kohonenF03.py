#
#
#
#
from ctypes import cdll,POINTER,c_int,c_double
import numpy as np 
import matplotlib.pyplot as plt 
import os
#
#
#
fort_mod = cdll.LoadLibrary(os.path.abspath("libkohonen.so"))
fort_mod.train_som_.argtypes = [POINTER(c_double),#
                               POINTER(c_int),# nvar
                               POINTER(c_int),# npat
                               POINTER(c_int),# nx
                               POINTER(c_int),# ny
                               POINTER(c_int),# nepoch
                               POINTER(c_double),#alpha
                               POINTER(c_int),#grid_type
                               POINTER(c_int),#distance_type
                               POINTER(c_int),#neigh_type
                               POINTER(c_int),#toroidal
                               POINTER(c_double),#prot(nx*ny,nvar),
                               POINTER(c_double),#distorsion(nepoch)
                               POINTER(c_double),#u_matrix(2*nx-1,2*ny-1)
                               POINTER(c_double),#coords()nx*ny,3
                               POINTER(c_int),##number_patterns(nx,ny)
                               POINTER(c_int)]#node_index(npat,3)] 
#
fort_mod.predict_som_.argtypes = [POINTER(c_double), #prot
                                  POINTER(c_int),#nx
                                  POINTER(c_int),#ny
                                  OINTER(c_double),#new_pat(npat,nvar)
                                  POINTER(c_int),#npat
                                  POINTER(c_int),#nvar
                                  POINTER(c_int)#,node_index(npat,3)
]
#
f = open("iris.dat", "r")

f.close();