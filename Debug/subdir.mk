################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../fileFunctions.f90 \
../layerFunctions.f90 \
../matrixFunctions.f90 \
../testfunctions.f90 

OBJS += \
./fileFunctions.o \
./layerFunctions.o \
./matrixFunctions.o \
./testfunctions.o 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

fileFunctions.o: ../fileFunctions.f90

layerFunctions.o: ../layerFunctions.f90

matrixFunctions.o: ../matrixFunctions.f90

testfunctions.o: ../testfunctions.f90


