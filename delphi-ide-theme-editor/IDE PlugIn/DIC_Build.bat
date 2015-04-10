

BRCC32 VersionInfo.rc
echo echo XE
echo call "C:\Program Files (x86)\Embarcadero\RAD Studio\8.0\bin\rsvars.bat"
echo msbuild.exe "DelphiIDEColorizer_XE.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
echo set BUILD_STATUS=%ERRORLEVEL%
echo if %BUILD_STATUS%==0 GOTO XE2
echo pause
echo EXIT

:XE2
echo XE2
call "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\rsvars.bat"
msbuild.exe "DelphiIDEColorizer_XE2.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO XE3
pause
EXIT

:XE3
echo XE3
call "C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\rsvars.bat"
msbuild.exe "DelphiIDEColorizer_XE3.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO XE4
pause
EXIT

:XE4
echo XE4
call "C:\Program Files (x86)\Embarcadero\RAD Studio\11.0\bin\rsvars.bat"
msbuild.exe "DelphiIDEColorizer_XE4.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO XE5
pause
EXIT

:XE5
echo XE5
call "C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\bin\rsvars.bat"
msbuild.exe "DelphiIDEColorizer_XE5.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO XE6
pause
EXIT

:XE6
echo XE6
call "C:\Program Files (x86)\Embarcadero\Studio\14.0\bin\rsvars.bat"
msbuild.exe "DelphiIDEColorizer_XE6.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO XE7


:XE7
echo XE7
call "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\rsvars.bat"
msbuild.exe "DelphiIDEColorizer_XE7.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO XE8
:DONE


:XE8
echo XE8
call "C:\Program Files (x86)\Embarcadero\Studio\16.0\bin\rsvars.bat"
msbuild.exe "DelphiIDEColorizer_XE8.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO DONE
:DONE
pause
EXIT