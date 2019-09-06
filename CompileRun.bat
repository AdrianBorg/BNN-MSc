:: gfortran -cpp -c -Ofast -mcmodel=medium timing.f90
:: gfortran -cpp -c -Ofast -mcmodel=medium fileFunctions.f90
:: gfortran -cpp -c -Ofast -mcmodel=medium layerFunctions.f90
:: gfortran -cpp -c -Ofast -mcmodel=medium matrixFunctions.f90
:: gfortran -cpp -c -Ofast -mcmodel=medium CNV.f90
:: gfortran -cpp -Ofast -mcmodel=medium run.f90 CNV.o fileFunctions.o layerFunctions.o matrixFunctions.o timing.o -o run.exe

:: use this for running with timing 
gfortran -DDO_TIMING -cpp -Ofast -mcmodel=medium run.f90 CNV.f90 fileFunctions.f90 layerFunctions.f90 matrixFunctions.f90 timing.f90 -o run.exe

:: use this for running without
:: gfortran -cpp -Ofast -mcmodel=medium run.f90 CNV.f90 fileFunctions.f90 layerFunctions.f90 matrixFunctions.f90 timing.f90 -o run.exe
