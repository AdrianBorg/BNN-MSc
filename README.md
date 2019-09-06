# BNN-MSc

Implementation of BNN in Fortran based off the CNV network in https://github.com/Xilinx/BNN-PYNQ (which in turn is based off of https://arxiv.org/pdf/1612.07119.pdf

## Running
- run.exe: runs the test script
- test.exe: runs the unit tests

## Compiling
In order to make compiling easy, 2 batch files have been created. 
- CompileRun: this compiles to run the testing script
- CompileTest: this compiles in order to run the unit tests

## Timing
In order to run the system while timing the timed subroutines, the flag -DDO_TIMING must be included when compiling

## File System
The files have been split up into several files and folders, the most important ones are mentioned below:

### Fortran files
 - run: the main file which runs the test script
 - matrixFunctionc: contains subroutines related to matrix manipulation and matrix data
 - layerFunctions: contains subroutines related to the neural network layers required for the CNV network
 - fileFunctions: contains subroutines concerned with importing of data from files
 - CNV: contains the subroutine which consturcs the CNV network and performes an inference using the network
 - testingfunctions: contains the subroutines created in order to provide unit testing funcitonality
 - unittests: contains the unit tests
 - timing: contains the timing functions
 - miscfiles/misclayerfunctions: contains all other layer functions which were developed but not used
 - miscfiles/miscmatrixfuncitons: contains all matrix funcitons which were developed but not used
 
 ### Folders
 - params: contains the files from which the parameters of the CNV network are read
 - imgBins: contains the binary file of the test images used during testing
 - miscfiles: contains files which were developed but not used, could be useful for creating new networks
 
 ### Eclipse
 The .cproject, .project files and the Debug folder are a result of using eclipse and photran, they have been included in case it is convenient
 
