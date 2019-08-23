gfortran -c -Ofast timing.f90
gfortran -c -Ofast fileFunctions.f90
gfortran -c -Ofast layerFunctions.f90
gfortran -c -Ofast matrixFunctions.f90
gfortran -c -Ofast CNV.f90
gfortran -c -Ofast testfunctions.f90
gfortran -Ofast run.f90 CNV.f90 fileFunctions.f90 layerFunctions.f90 matrixFunctions.f90 timing.f90 -o run.exe