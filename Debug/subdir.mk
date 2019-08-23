################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../CNV.f90 \
../fileFunctions.f90 \
../layerFunctions.f90 \
../matrixFunctions.f90 \
../run.f90 \
../test.f90 \
../testfunctions.f90 \
../timing.f90 

O_SRCS += \
../CNV.o \
../MiscLayerFunctions.o \
../MiscMatrixFunctions.o \
../fileFunctions.o \
../layerFunctions.o \
../matrixFunctions.o \
../testfunctions.o \
../timing.o 

OBJS += \
./CNV.o \
./fileFunctions.o \
./layerFunctions.o \
./matrixFunctions.o \
./run.o \
./test.o \
./testfunctions.o \
./timing.o 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

CNV.o: ../CNV.f90

fileFunctions.o: ../fileFunctions.f90

layerFunctions.o: ../layerFunctions.f90

matrixFunctions.o: ../matrixFunctions.f90

run.o: ../run.f90

test.o: ../test.f90

testfunctions.o: ../testfunctions.f90

timing.o: ../timing.f90


