call "C:\Program Files (x86)\Embarcadero\RAD Studio\11.0\bin\rsvars.bat"
msbuild.exe "DITE.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO INNO
pause
EXIT

:INNO
"C:\Program Files (x86)\Inno Setup 5\iscc.exe" "C:\Dephi\github\delphi-ide-theme-editor\delphi-ide-theme-editor\DelphiIDEThemeEditor.iss"
set INNO_STATUS=%ERRORLEVEL%
if %INNO_STATUS%==0 GOTO END
pause
EXIT


:END 
pause
