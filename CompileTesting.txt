gfortran -c fileFunctions.f90
gfortran -c layerFunctions.f90
gfortran -c matrixFunctions.f90
gfortran testfunctions.f90 fileFunctions.f90 layerFunctions.f90 matrixFunctions.f90 -o tests.exe