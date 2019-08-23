################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../MiscFiles/MiscLayerFunctions.f90 \
../MiscFiles/MiscMatrixFunctions.f90 

OBJS += \
./MiscFiles/MiscLayerFunctions.o \
./MiscFiles/MiscMatrixFunctions.o 


# Each subdirectory must supply rules for building sources it contributes
MiscFiles/%.o: ../MiscFiles/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

MiscFiles/MiscLayerFunctions.o: ../MiscFiles/MiscLayerFunctions.f90

MiscFiles/MiscMatrixFunctions.o: ../MiscFiles/MiscMatrixFunctions.f90


