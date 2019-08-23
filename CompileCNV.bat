gfortran -c -Ofast -mcmodel=medium timing.f90
gfortran -c -Ofast -mcmodel=medium fileFunctions.f90
gfortran -c -Ofast -mcmodel=medium layerFunctions.f90
gfortran -c -Ofast -mcmodel=medium matrixFunctions.f90
gfortran -Ofast -mcmodel=medium CNV.f90 fileFunctions.f90 layerFunctions.f90 matrixFunctions.f90 timing.f90 -o CNV.exe