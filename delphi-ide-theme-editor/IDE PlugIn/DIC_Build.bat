BRCC32 VersionInfo.rc
echo XE2
call "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\rsvars.bat"
msbuild.exe "DelphiIDEColorizer_XE2.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
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
if %BUILD_STATUS%==0 GOTO DONE

:DONE

pause
EXIT