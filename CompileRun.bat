gfortran -c -Ofast -mcmodel=medium timing.f90
gfortran -c -Ofast -mcmodel=medium fileFunctions.f90
gfortran -c -Ofast -mcmodel=medium layerFunctions.f90
gfortran -c -Ofast -mcmodel=medium matrixFunctions.f90
gfortran -c -Ofast -mcmodel=medium CNV.f90
gfortran -Ofast -mcmodel=medium run.f90 CNV.o fileFunctions.o layerFunctions.o matrixFunctions.o timing.o -o run.exe