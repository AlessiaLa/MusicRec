@echo off
rem ***************************************************************************
rem Batch script for starting SWI-Prolog with connection to the WN_CONNECT tool
rem 
rem If you want to use this tools from any directory:
rem   Add to the PATH environment variable where is located this tool
rem   For this, use either the system dialog box or in Control Panel 
rem   (Environment Variables), or in a terminal:
rem   set PATH=c:\wn;%PATH%
rem   Replace c:\wn with the actual folder you have installed WN_CONNECT
rem ***************************************************************************

rem Locate the path of wn_connect.pl by examining the PATH environment variable
set folder=
for %%A in ("%path:;=";"%") do (
    if exist %%~A\wn_connect.pl set folder=%%~A
)
rem Check if the file has been found
if defined folder goto continue
echo ERROR: File wn_connect.pl not found. Check that its location has been added to the environment variable PATH
goto end
:continue
rem Change to directory where w_connect.pl is located, saving the current directory
pushd %folder%
swipl wn_connect.pl
rem Return to the initial directory
popd
set folder=
:end