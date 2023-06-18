#
#
#
#
import os
import sys
from ctypes import cdll,POINTER,c_int,c_double
import numpy as np 
import matplotlib.pyplot as plt 
#
#
#
#fort_mod = cdll.LoadLibrary(os.path.abspath("libkohonen.so"))
#os.add_dll_directory("C:\GITProjects\KohonenF03")
fort_mod = cdll.LoadLibrary(os.path.abspath('self_organized_map_utilities.dll'))
fort_mod.train_som_.argtypes = [POINTER(c_double),# x, input data
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
                                  POINTER(c_double),#new_pat(npat,nvar)
                                  POINTER(c_int),#npat
                                  POINTER(c_int),#nvar
                                  POINTER(c_int)#,node_index(npat,3)
]
# Read iris data
f=np.loadtxt("iris.dat", dtype=c_double)
# Print number of rows and columns
print(f.shape)
x=np.empty((150,4), dtype = (c_double))
x[:,0]=f[:,0]
x[:,1]=f[:,1]
x[:,2]=f[:,2]
x[:,3]=f[:,3]

print(x)
#sys.exit('Error')
# Define variable for input
nvar=4
npat=150
nx=10
ny=10
nepoch=200
alpha=0.05
grid_type=0
distance_type=1
neigh_type=0
toroidal=0
prot=np.empty((nx*ny,nvar),dtype=c_double,order='F')
distortion=np.empty((nepoch),dtype=c_double,order='F')
u_matrix=np.empty((2*nx-1,2*ny-1),dtype=c_double,order='F')
coords=np.empty((nx*ny,3),dtype=c_double,order='F')
number_patterns=np.empty((nx,ny),dtype=c_int,order='F')
node_index=np.empty((npat,3),dtype=c_int,order='F')
coords1=np.empty((nx*ny),dtype=c_double,order='F')
#
fort_mod.train_som_(x.ctypes.data_as(POINTER(c_double)),
                    c_int(nvar),c_int(npat),c_int(nx),c_int(ny),
                    c_int(nepoch),c_double(alpha),c_int(grid_type),c_int(distance_type),c_int(neigh_type),
                    c_int(toroidal),prot.ctypes.data_as(POINTER(c_double)),
                    distortion.ctypes.data_as(POINTER(c_double)),
                    u_matrix.ctypes.data_as(POINTER(c_double)),
                    coords.ctypes.data_as(POINTER(c_double)),
                    number_patterns.ctypes.data_as(POINTER(c_int)),
                    node_index.ctypes.data_as(POINTER(c_int)))
#
print(prot)
print(u_matrix)
print(coords)
plt.imshow(number_patterns)
plt.colorbar()
plt.show()
plt.plot(distortion)
plt.show()
#print(coords[:,])
#print(coords1)
