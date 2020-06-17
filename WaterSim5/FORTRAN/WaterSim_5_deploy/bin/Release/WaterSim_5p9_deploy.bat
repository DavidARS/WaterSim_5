REM this file copies the testing dll to a Csharp folder
REM created on 07.30.13
REM last write was: 01.24.14,07.23.14,10.27.14,12.21.15,05.27.16,07.27.16
REM das
echo off
cd\
cd C:\WaterSim\Fortran\WaterSim_5_deploy\bin\Release
xcopy /y WaterSimDCDC_model_5.dll "C:\WaterSim\CSharp\API_11_1\API_Projects\Scenarios\API_Scenarios\bin\Debug"
xcopy /y WaterSimDCDC_model_5.lib "C:\WaterSim\CSharp\API_11_1\API_Projects\Scenarios\API_Scenarios\bin\Debug\"

pause

REM
REM
