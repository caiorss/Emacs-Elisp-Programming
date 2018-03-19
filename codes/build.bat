
@echo off

rem  Compile for x86 or x64 bits 
rem ------------------------------
set MODE=x86          
rem set MODE=x64   

@REM Visual studio building tools path - Install it with chocolately 
set VS2017="C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsamd64_x86.bat"
set VS2015="C:\Program Files (x86)\Microsoft Visual C++ Build Tools\vcbuildtools.bat" 

rem Save current directory     
pushd %CD%

@REM Set visual Studio 2017 
call %VS2017% %MODE% 

@REM Restore saved directory     
popd 

@REM ------------------ User Command Goes Here ----------------- @REM     
       
@REM Build solution in Debug mode
cl.exe  test1.cpp && test1.exe 

@REM Set /p Wait=Build Process Completed...
