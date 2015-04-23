

!include "Sections.nsh"
!include "MUI.nsh"
!include "LogicLib.nsh"
!include "WordFunc.nsh"


SetOverwrite on
SetCompress auto
SetCompressor /SOLID lzma
SetCompressorDictSize 32
SetDatablockOptimize on
SetDateSave on
RequestExecutionLevel admin

;!define DEBUG "1"
!define SUPPORTS_BDS "1"
!ifndef VER_MAJOR
  !define VER_MAJOR "0"
!endif

!ifndef VER_MINOR
  !define VER_MINOR "7.7.9"
!endif

!ifndef IDE_VERSION_DXE
!ifndef IDE_VERSION_DXE2
!ifndef IDE_VERSION_DXE3
!ifndef IDE_VERSION_DXE4
!ifndef IDE_VERSION_DXE5
!ifndef IDE_VERSION_DXE6
!ifndef IDE_VERSION_DXE7
!ifndef IDE_VERSION_DXE8

  !define FULL_VERSION    "1"  
  ;!define IDE_VERSION_DXE  "1"  
  !define IDE_VERSION_DXE2 "1"
  !define IDE_VERSION_DXE3 "1"
  !define IDE_VERSION_DXE4 "1"
  !define IDE_VERSION_DXE5 "1"
  !define IDE_VERSION_DXE6 "1"
  !define IDE_VERSION_DXE7 "1"
  !define IDE_VERSION_DXE8 "1"
  
!endif
!endif
!endif
!endif
!endif
!endif
!endif
!endif

!ifndef FULL_VERSION
  !define IDE_VERSION

  !ifdef IDE_VERSION_DXE
    !define IDE_SHORT_NAME "DXE"
    !define IDE_LONG_NAME "RAD Studio XE"
  !endif  
  !ifdef IDE_VERSION_DXE2
    !define IDE_SHORT_NAME "DXE2"
    !define IDE_LONG_NAME "RAD Studio XE2"
  !endif
  !ifdef IDE_VERSION_DXE3
    !define IDE_SHORT_NAME "DXE3"
    !define IDE_LONG_NAME "RAD Studio XE3"
  !endif
  !ifdef IDE_VERSION_DXE4
    !define IDE_SHORT_NAME "DXE4"
    !define IDE_LONG_NAME "RAD Studio XE4"
  !endif
  !ifdef IDE_VERSION_DXE5
    !define IDE_SHORT_NAME "DXE5"
    !define IDE_LONG_NAME "RAD Studio XE5"
  !endif
  !ifdef IDE_VERSION_DXE6
    !define IDE_SHORT_NAME "DXE6"
    !define IDE_LONG_NAME "RAD Studio XE6 / Appmethod 1.14"
  !endif
  !ifdef IDE_VERSION_DXE7
    !define IDE_SHORT_NAME "DXE7"
    !define IDE_LONG_NAME "RAD Studio XE7 / Appmethod 1.15"
  !endif
  !ifdef IDE_VERSION_DXE8
    !define IDE_SHORT_NAME "DXE8"
    !define IDE_LONG_NAME "RAD Studio XE8"
  !endif  
!endif

!ifdef IDE_VERSION
  !define VERSION_STRING "${VER_MAJOR}.${VER_MINOR}_${IDE_SHORT_NAME}"
!else
  !define VERSION_STRING "${VER_MAJOR}.${VER_MINOR}"
!endif

!ifndef INSTALLER_NAME
  !define INSTALLER_NAME "Setup_DIC.exe"
!endif

!ifdef IDE_VERSION
  Name "$(APPNAME) ${VER_MAJOR}.${VER_MINOR} For ${IDE_LONG_NAME}"
!else
  Name "$(APPNAME) ${VER_MAJOR}.${VER_MINOR}"
!endif

!ifdef IDE_VERSION
Caption "$(APPNAME) ${VER_MAJOR}.${VER_MINOR} For ${IDE_LONG_NAME}"
!else
Caption "$(APPNAME) ${VER_MAJOR}.${VER_MINOR}"
!endif

BrandingText "$(APPNAME) Build ${__DATE__}"
OutFile "Output\${INSTALLER_NAME}"

Function InitVersion
  !packhdr tmp.dat '"C:\Program Files (x86)\Resource Hacker\ResHacker.exe" -addoverwrite tmp.dat, tmp.dat, VersionInfo.RES, versioninfo,1,'
FunctionEnd

!verbose 3

!define MUI_ICON "Images\DIC.ico"
!define MUI_UNICON "Images\DIC.ico"
!define APPNAMEDIR "Delphi IDE Colorizer"
!define MUI_ABORTWARNING

!define MUI_WELCOMEPAGE_TITLE_3LINES
!define MUI_FINISHPAGE_TITLE_3LINES

;!define MUI_HEADERIMAGE
;!define MUI_HEADERIMAGE_BITMAP "logoinstall.bmp"
!define MUI_WELCOMEFINISHPAGE_BITMAP "wizard_intaller.bmp"

!insertmacro MUI_PAGE_WELCOME
;!insertmacro MUI_PAGE_LICENSE $(SLICENSEFILE)
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\Help\$(SHELPCHM)"
;!define MUI_FINISHPAGE_SHOWREADME_FUNCTION ShowReleaseNotes

!define MUI_FINISHPAGE_NOREBOOTSUPPORT

!insertmacro MUI_PAGE_FINISH

!define MUI_LANGDLL_REGISTRY_ROOT "HKCU"
!define MUI_LANGDLL_REGISTRY_KEY "Software\The Road To Delphi\DIC"
!define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"


!insertmacro MUI_LANGUAGE "English"

; CnWizards Name
LangString APPNAME        1033 "Delphi IDE Colorizer"

; Install Type
LangString TYPICALINST    1033 "Typical"
LangString MINIINST       1033 "Minimized"
LangString CUSTINST       1033 "Custom"

; Section Name
LangString PROGRAMDATA    1033 "Data files"
LangString HELPFILE       1033 "Help Files"
LangString OTHERTOOLS     1033 "Tools"

; Shortcut Name
LangString SUNINSTALL     1033 "Uninstall"


; Dialog Message
LangString SQUERYIDE      1033 "Setup has detected some wizard dlls are in using.$\n\
                                Please close Delphi (RAD Studio) first.$\n$\n\
                                Click [OK] to retry and continue.$\n\
                                Click [Cancel] to exit Setup."
LangString SQUERYDELETE   1033 "Delete user data files and wizards settings?$\n(If you want to keep them, please click [No].)"

; Section Description
LangString DESCDATA       1033 "The core programs and data files required to use wizards."
LangString DESCHELP       1033 "Help file for wizards."
LangString DESDLL         1033 "Install wizard dll file for #DLL#."

;!include "Lang\DIC_en.nsh"
!verbose 4


XPstyle on
WindowIcon on
BGGradient off
CRCCheck on
AutoCloseWindow true
ShowInstDetails show
ShowUninstDetails show
AllowRootDirInstall false


;InstallDir "$PROGRAMFILES\The Road To Delphi\DIC"
InstallDir "$LOCALAPPDATA\The Road To Delphi\DIC"
InstallDirRegKey HKLM \
                "Software\Microsoft\Windows\CurrentVersion\Uninstall\The Road To Delphi\DIC" \
                "UninstallString"


InstType "$(TYPICALINST)"
InstType "$(MINIINST)"
InstType /CUSTOMSTRING=$(CUSTINST)

Section "$(PROGRAMDATA)" SecData
  SectionIn 1 2 RO
  ClearErrors

FileLoop:

!ifdef SUPPORTS_BDS

!ifdef IDE_VERSION_DXE
  IfFileExists "$INSTDIR\XE\DelphiIDEColorizer_XE.dll" 0 +4
  FileOpen $0 "$INSTDIR\XE\DelphiIDEColorizer_XE.dll" a
  IfErrors FileInUse
  FileClose $0
!endif

!ifdef IDE_VERSION_DXE2
  IfFileExists "$INSTDIR\XE2\DelphiIDEColorizer_XE2.dll" 0 +4
  FileOpen $0 "$INSTDIR\XE2\DelphiIDEColorizer_XE2.dll" a
  IfErrors FileInUse
  FileClose $0
!endif

!ifdef IDE_VERSION_DXE3
  IfFileExists "$INSTDIR\XE3\DelphiIDEColorizer_XE3.dll" 0 +4
  FileOpen $0 "$INSTDIR\XE3\DelphiIDEColorizer_XE3.dll" a
  IfErrors FileInUse
  FileClose $0
!endif

!ifdef IDE_VERSION_DXE4
  IfFileExists "$INSTDIR\XE4\DelphiIDEColorizer_XE4.dll" 0 +4
  FileOpen $0 "$INSTDIR\XE4\DelphiIDEColorizer_XE4.dll" a
  IfErrors FileInUse
  FileClose $0
!endif

!ifdef IDE_VERSION_DXE5
  IfFileExists "$INSTDIR\XE5\DelphiIDEColorizer_XE5.dll" 0 +4
  FileOpen $0 "$INSTDIR\XE5\DelphiIDEColorizer_XE5.dll" a
  IfErrors FileInUse
  FileClose $0
!endif

!ifdef IDE_VERSION_DXE6
  IfFileExists "$INSTDIR\XE6\DelphiIDEColorizer_XE6.dll" 0 +4
  FileOpen $0 "$INSTDIR\XE6\DelphiIDEColorizer_XE6.dll" a
  IfErrors FileInUse
  FileClose $0
!endif

!ifdef IDE_VERSION_DXE7
  IfFileExists "$INSTDIR\XE7\DelphiIDEColorizer_XE7.dll" 0 +4
  FileOpen $0 "$INSTDIR\XE7\DelphiIDEColorizer_XE7.dll" a
  IfErrors FileInUse
  FileClose $0
!endif

!ifdef IDE_VERSION_DXE8
  IfFileExists "$INSTDIR\XE8\DelphiIDEColorizer_XE8.dll" 0 +4
  FileOpen $0 "$INSTDIR\XE8\DelphiIDEColorizer_XE8.dll" a
  IfErrors FileInUse
  FileClose $0
!endif

!endif

  Goto InitOk

FileInUse:
  FileClose $0
  MessageBox MB_OKCANCEL|MB_ICONQUESTION "$(SQUERYIDE)" IDOK FileLoop
  Quit

InitOk:
  SetOutPath $INSTDIR
  ;File "DIC.xml"
  ;File "HookedWindows.dat"
  ;File "Settings.ini"
  ;File "..\..\License.*.txt"
  ;SetOutPath $INSTDIR\Themes
  ;File "Themes\*.idetheme"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "DisplayIcon" "$\"$INSTDIR\uninst.exe$\""
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "DisplayName" "${APPNAMEDIR}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "DisplayVersion" "${VERSION_STRING}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "HelpLink" "http://code.google.com/p/delphi-ide-theme-editor/"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "Publisher" "The Road To Delphi"  
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "Contact" "theroadtodelphi@gmail.com"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "URLInfoAbout" "http://code.google.com/p/delphi-ide-theme-editor/"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "URLUpdateInfo" "http://code.google.com/p/delphi-ide-theme-editor/"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC" "UninstallString" "$\"$INSTDIR\uninst.exe$\""
  WriteRegDWORD HKCU "Software\The Road To Delphi\DIC\Option" "CurrentLangID" $LANGUAGE
  WriteUninstaller "$INSTDIR\uninst.exe"
SectionEnd


!ifdef IDE_VERSION_DXE
Section "RAD Studio XE" SecDXE
  SectionIn 1 2
  SetOutPath $INSTDIR\XE
  File "Updater.exe"
  File "libeay32.dll"
  File "ssleay32.dll"
  File "DownloadInfo.xml"
  File "HookedWindows.dat"
  File "HookedScrollBars.dat"
  File "WinAPIClasses.dat"
  SetOverwrite off
  File "Init\Settings.ini"
  SetOverwrite on 
  File "DelphiIDEColorizer_XE.dll"
  SetOutPath $INSTDIR\XE\Themes
  File "Themes\*.idetheme"  
  SetOutPath $INSTDIR\XE\Images\dock_images
  File "Images\dock_images\*.png"  
  WriteRegStr HKCU "Software\Embarcadero\BDS\8.0\Experts" "DelphiIDEColorizer_XE" "$INSTDIR\XE\DelphiIDEColorizer_XE.dll"
SectionEnd
!endif
  

!ifdef IDE_VERSION_DXE2
Section "RAD Studio XE2" SecDXE2
  SectionIn 1 2
  SetOutPath $INSTDIR\XE2
  File "Updater.exe"
  File "libeay32.dll"
  File "ssleay32.dll"
  File "DownloadInfo.xml"  
  File "HookedWindows.dat"
  File "HookedScrollBars.dat"  
  File "WinAPIClasses.dat"  
  SetOverwrite off
  File "Init\Settings.ini"  
  SetOverwrite on 
  File "DelphiIDEColorizer_XE2.dll"
  ;SetOutPath $INSTDIR\XE2\Styles
  ;File "Styles\*.vsf"    
  SetOutPath $INSTDIR\XE2\Themes
  File "Themes\*.idetheme"  
  SetOutPath $INSTDIR\XE2\Images\dock_images
  File "Images\dock_images\*.png"  
  WriteRegStr HKCU "Software\Embarcadero\BDS\9.0\Experts" "DelphiIDEColorizer_XE2" "$INSTDIR\XE2\DelphiIDEColorizer_XE2.dll"
SectionEnd
!endif

!ifdef IDE_VERSION_DXE3
Section "RAD Studio XE3" SecDXE3
  SectionIn 1 2
  SetOutPath $INSTDIR\XE3
  File "Updater.exe"
  File "libeay32.dll"
  File "ssleay32.dll"
  File "DownloadInfo.xml"
  File "HookedWindows.dat"
  File "HookedScrollBars.dat"  
  File "WinAPIClasses.dat"  
  SetOverwrite off
  File "Init\Settings.ini"
  SetOverwrite on
  File "DelphiIDEColorizer_XE3.dll"
  ;SetOutPath $INSTDIR\XE3\Styles
  ;File "Styles\*.vsf"      
  SetOutPath $INSTDIR\XE3\Themes
  File "Themes\*.idetheme" 
  SetOutPath $INSTDIR\XE3\Images\dock_images
  File "Images\dock_images\*.png"    
  WriteRegStr HKCU "Software\Embarcadero\BDS\10.0\Experts" "DelphiIDEColorizer_XE3" "$INSTDIR\XE3\DelphiIDEColorizer_XE3.dll"
SectionEnd
!endif

!ifdef IDE_VERSION_DXE4
Section "RAD Studio XE4" SecDXE4
  SectionIn 1 2
  SetOutPath $INSTDIR\XE4
  File "Updater.exe"
  File "libeay32.dll"
  File "ssleay32.dll"
  File "DownloadInfo.xml"  
  File "HookedWindows.dat"
  File "HookedScrollBars.dat"  
  File "WinAPIClasses.dat"  
  SetOverwrite off
  File "Init\Settings.ini"
  SetOverwrite on
  File "DelphiIDEColorizer_XE4.dll"
  ;SetOutPath $INSTDIR\XE4\Styles
  ;File "Styles\*.vsf"      
  SetOutPath $INSTDIR\XE4\Themes
  File "Themes\*.idetheme" 
  SetOutPath $INSTDIR\XE4\Images\dock_images
  File "Images\dock_images\*.png"    
  WriteRegStr HKCU "Software\Embarcadero\BDS\11.0\Experts" "DelphiIDEColorizer_XE4" "$INSTDIR\XE4\DelphiIDEColorizer_XE4.dll"
SectionEnd
!endif

!ifdef IDE_VERSION_DXE5
Section "RAD Studio XE5" SecDXE5
  SectionIn 1 2
  SetOutPath $INSTDIR\XE5
  File "Updater.exe"
  File "libeay32.dll"
  File "ssleay32.dll"
  File "DownloadInfo.xml"  
  File "HookedWindows.dat"
  File "HookedScrollBars.dat"  
  File "WinAPIClasses.dat"  
  SetOverwrite off
  File "Init\Settings.ini"
  SetOverwrite on
  File "DelphiIDEColorizer_XE5.dll"
  ;SetOutPath $INSTDIR\XE5\Styles
  ;File "Styles\*.vsf"      
  SetOutPath $INSTDIR\XE5\Themes
  File "Themes\*.idetheme" 
  SetOutPath $INSTDIR\XE5\Images\dock_images
  File "Images\dock_images\*.png"    
  WriteRegStr HKCU "Software\Embarcadero\BDS\12.0\Experts" "DelphiIDEColorizer_XE5" "$INSTDIR\XE5\DelphiIDEColorizer_XE5.dll"
SectionEnd
!endif

!ifdef IDE_VERSION_DXE6
Section "RAD Studio XE6" SecDXE6
  SectionIn 1 2
  SetOutPath $INSTDIR\XE6
  File "Updater.exe"
  File "libeay32.dll"
  File "ssleay32.dll"
  File "DownloadInfo.xml"  
  File "HookedWindows.dat"
  File "HookedScrollBars.dat"  
  File "WinAPIClasses.dat"  
  SetOverwrite off
  File "Init\Settings.ini"
  SetOverwrite on  
  File "DelphiIDEColorizer_XE6.dll"
  ;just for AppMethod
  SetOutPath $INSTDIR\XE6\Styles
  File "Styles\*.vsf"      
  SetOutPath $INSTDIR\XE6\Themes
  File "Themes\*.idetheme" 
  SetOutPath $INSTDIR\XE6\Images\dock_images
  File "Images\dock_images\*.png"    
  WriteRegStr HKCU "Software\Embarcadero\BDS\14.0\Experts" "DelphiIDEColorizer_XE6" "$INSTDIR\XE6\DelphiIDEColorizer_XE6.dll"
SectionEnd
!endif


!ifdef IDE_VERSION_DXE7
Section "RAD Studio XE7" SecDXE7
  SectionIn 1 2
  SetOutPath $INSTDIR\XE7
  File "Updater.exe"
  File "libeay32.dll"
  File "ssleay32.dll"
  File "DownloadInfo.xml"  
  File "HookedWindows.dat"
  File "HookedScrollBars.dat"  
  File "WinAPIClasses.dat"  
  SetOverwrite off
  File "Init\Settings.ini"
  SetOverwrite on  
  File "DelphiIDEColorizer_XE7.dll"
  ;just for AppMethod
  SetOutPath $INSTDIR\XE7\Styles
  File "Styles\*.vsf"      
  SetOutPath $INSTDIR\XE7\Themes
  File "Themes\*.idetheme" 
  SetOutPath $INSTDIR\XE7\Images\dock_images
  File "Images\dock_images\*.png"    
  WriteRegStr HKCU "Software\Embarcadero\BDS\15.0\Experts" "DelphiIDEColorizer_XE7" "$INSTDIR\XE7\DelphiIDEColorizer_XE7.dll"
SectionEnd
!endif

!ifdef IDE_VERSION_DXE8
Section "RAD Studio XE8" SecDXE8
  SectionIn 1 2
  SetOutPath $INSTDIR\XE8
  File "Updater.exe"
  File "libeay32.dll"
  File "ssleay32.dll"
  File "DownloadInfo.xml"  
  File "HookedWindows.dat"
  File "HookedScrollBars.dat"  
  File "WinAPIClasses.dat"  
  SetOverwrite off
  File "Init\Settings.ini"
  SetOverwrite on  
  File "DelphiIDEColorizer_XE8.dll"
  ;just for AppMethod
  SetOutPath $INSTDIR\XE8\Styles
  File "Styles\*.vsf"      
  SetOutPath $INSTDIR\XE8\Themes
  File "Themes\*.idetheme" 
  SetOutPath $INSTDIR\XE8\Images\dock_images
  File "Images\dock_images\*.png"    
  WriteRegStr HKCU "Software\Embarcadero\BDS\16.0\Experts" "DelphiIDEColorizer_XE8" "$INSTDIR\XE8\DelphiIDEColorizer_XE8.dll"
SectionEnd
!endif

!define SF_SELBOLD    9

Function .onInit

  InitPluginsDir
  File /oname=$PLUGINSDIR\Auric.vsf "..\Styles\Auric.vsf"
  NSISVCLStyles::LoadVCLStyle $PLUGINSDIR\Auric.vsf
  
  ;!insertmacro MUI_LANGDLL_DISPLAY
  Call InitVersion
  Call SetCheckBoxes
FunctionEnd


!macro SET_COMPILER_CHECKBOX REGROOT REGKEY REGVALUE SECNAME

  Push $0
  Push $R0

  SectionGetFlags "${SECNAME}" $0
  ReadRegStr $R0 "${REGROOT}" "${REGKEY}" "${REGVALUE}"
  StrCmp $R0 "" +3
  IntOp $0 $0 | ${SF_SELBOLD}

  goto +2
  IntOp $0 $0 & ${SECTION_OFF}

  SectionSetFlags "${SECNAME}" $0

  Pop $R0
  Pop $0

!macroend


!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
!ifdef IDE_VERSION_DXE
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDXE} "Delphi IDE Colorizer for Delphi XE"
!endif
!ifdef IDE_VERSION_DXE2
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDXE2} "Delphi IDE Colorizer for Delphi XE2"
!endif
!ifdef IDE_VERSION_DXE3
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDXE3} "Delphi IDE Colorizer for Delphi XE3"
!endif
!ifdef IDE_VERSION_DXE4
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDXE4} "Delphi IDE Colorizer for Delphi XE4"
!endif
!ifdef IDE_VERSION_DXE5
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDXE5} "Delphi IDE Colorizer for Delphi XE5"
!endif
!ifdef IDE_VERSION_DXE6
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDXE6} "Delphi IDE Colorizer for Delphi XE6"
!endif
!ifdef IDE_VERSION_DXE7
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDXE7} "Delphi IDE Colorizer for Delphi XE7"
!endif	
!ifdef IDE_VERSION_DXE8
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDXE8} "Delphi IDE Colorizer for Delphi XE8"
!endif	
!insertmacro MUI_FUNCTION_DESCRIPTION_END

Function SetCheckBoxes

  StrCpy $1 ${SecData}

!ifdef IDE_VERSION_DXE
  !insertmacro SET_COMPILER_CHECKBOX HKCU "Software\Embarcadero\BDS\8.0" "App" ${SecDXE}
!endif  
!ifdef IDE_VERSION_DXE2
  !insertmacro SET_COMPILER_CHECKBOX HKCU "Software\Embarcadero\BDS\9.0" "App" ${SecDXE2}
!endif
!ifdef IDE_VERSION_DXE3
  !insertmacro SET_COMPILER_CHECKBOX HKCU "Software\Embarcadero\BDS\10.0" "App" ${SecDXE3}
!endif
!ifdef IDE_VERSION_DXE4
  !insertmacro SET_COMPILER_CHECKBOX HKCU "Software\Embarcadero\BDS\11.0" "App" ${SecDXE4}
!endif
!ifdef IDE_VERSION_DXE5
  !insertmacro SET_COMPILER_CHECKBOX HKCU "Software\Embarcadero\BDS\12.0" "App" ${SecDXE5}
!endif
!ifdef IDE_VERSION_DXE6
  !insertmacro SET_COMPILER_CHECKBOX HKCU "Software\Embarcadero\BDS\14.0" "App" ${SecDXE6}
!endif
!ifdef IDE_VERSION_DXE7
  !insertmacro SET_COMPILER_CHECKBOX HKCU "Software\Embarcadero\BDS\15.0" "App" ${SecDXE7}
!endif
!ifdef IDE_VERSION_DXE8
  !insertmacro SET_COMPILER_CHECKBOX HKCU "Software\Embarcadero\BDS\16.0" "App" ${SecDXE8}
!endif
FunctionEnd


Section "Uninstall"
  Delete "$INSTDIR\*.*"
  Delete "$INSTDIR\XE\*.exe" 
  Delete "$INSTDIR\XE\*.xml"   
  Delete "$INSTDIR\XE\*.dll"
  Delete "$INSTDIR\XE\*.dat"
  Delete "$INSTDIR\XE2\*.exe" 
  Delete "$INSTDIR\XE2\*.xml"     
  Delete "$INSTDIR\XE2\*.dll"
  Delete "$INSTDIR\XE2\*.dat"  
  Delete "$INSTDIR\XE3\*.exe" 
  Delete "$INSTDIR\XE3\*.xml"     
  Delete "$INSTDIR\XE3\*.dll"
  Delete "$INSTDIR\XE3\*.dat"
  Delete "$INSTDIR\XE4\*.exe" 
  Delete "$INSTDIR\XE4\*.xml"     
  Delete "$INSTDIR\XE4\*.dll"
  Delete "$INSTDIR\XE4\*.dat"
  Delete "$INSTDIR\XE5\*.exe" 
  Delete "$INSTDIR\XE5\*.xml"     
  Delete "$INSTDIR\XE5\*.dll"
  Delete "$INSTDIR\XE5\*.dat"
  Delete "$INSTDIR\XE6\*.exe" 
  Delete "$INSTDIR\XE6\*.xml"     
  Delete "$INSTDIR\XE6\*.dll"
  Delete "$INSTDIR\XE6\*.dat"
  Delete "$INSTDIR\XE7\*.dll"
  Delete "$INSTDIR\XE7\*.dat"
  Delete "$INSTDIR\XE8\*.dll"
  Delete "$INSTDIR\XE8\*.dat"  
  Delete "$INSTDIR\XE\Images\dock_images\*.*"  
  Delete "$INSTDIR\XE\Themes\*.*"  
  Delete "$INSTDIR\XE2\Images\dock_images\*.*"  
  Delete "$INSTDIR\XE2\Themes\*.*"
  Delete "$INSTDIR\XE3\Images\dock_images\*.*"  
  Delete "$INSTDIR\XE3\Themes\*.*"
  Delete "$INSTDIR\XE4\Images\dock_images\*.*"  
  Delete "$INSTDIR\XE4\Themes\*.*"
  Delete "$INSTDIR\XE5\Images\dock_images\*.*"    
  Delete "$INSTDIR\XE5\Themes\*.*"
  Delete "$INSTDIR\XE6\Images\dock_images\*.*"    
  Delete "$INSTDIR\XE6\Themes\*.*"
  Delete "$INSTDIR\XE7\Images\dock_images\*.*"    
  Delete "$INSTDIR\XE7\Themes\*.*"
  Delete "$INSTDIR\XE8\Images\dock_images\*.*"    
  Delete "$INSTDIR\XE8\Themes\*.*"  
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\DIC"

!ifdef IDE_VERSION_DXE
  DeleteRegValue HKCU "Software\Embarcadero\BDS\8.0\Experts" "DelphiIDEColorizer_XE"
!endif
!ifdef IDE_VERSION_DXE2
  DeleteRegValue HKCU "Software\Embarcadero\BDS\9.0\Experts" "DelphiIDEColorizer_XE2"
!endif
!ifdef IDE_VERSION_DXE3
  DeleteRegValue HKCU "Software\Embarcadero\BDS\10.0\Experts" "DelphiIDEColorizer_XE3"
!endif
!ifdef IDE_VERSION_DXE4
  DeleteRegValue HKCU "Software\Embarcadero\BDS\11.0\Experts" "DelphiIDEColorizer_XE4"
!endif
!ifdef IDE_VERSION_DXE5
  DeleteRegValue HKCU "Software\Embarcadero\BDS\12.0\Experts" "DelphiIDEColorizer_XE5"
!endif
!ifdef IDE_VERSION_DXE6
  DeleteRegValue HKCU "Software\Embarcadero\BDS\14.0\Experts" "DelphiIDEColorizer_XE6"
!endif
!ifdef IDE_VERSION_DXE7
  DeleteRegValue HKCU "Software\Embarcadero\BDS\15.0\Experts" "DelphiIDEColorizer_XE7"
!endif
!ifdef IDE_VERSION_DXE8
  DeleteRegValue HKCU "Software\Embarcadero\BDS\16.0\Experts" "DelphiIDEColorizer_XE8"
!endif
  ;MessageBox MB_YESNO|MB_ICONQUESTION "$(SQUERYDELETE)" IDNO NoDelete
  DeleteRegKey HKCU "Software\The Road To Delphi\DIC"
  ;RMDir /r $INSTDIR

;NODelete:
SectionEnd

Function un.onInit
  InitPluginsDir
  File /oname=$PLUGINSDIR\Auric.vsf "..\Styles\Auric.vsf"
  NSISVCLStyles::LoadVCLStyle $PLUGINSDIR\Auric.vsf
FunctionEnd


;Function ShowReleaseNotes
;!ifndef NO_HELP
;  IfFileExists "$INSTDIR\Help\$(SHELPCHM)" 0 OpenWeb
    ;ExecShell "open" "$INSTDIR\Help\$(SHELPCHM)"
;   Goto FuncEnd

; OpenWeb:
;endif
;   ExecShell "open" "http://code.google.com/p/delphi-ide-theme-editor/"
;ifndef NO_HELP
; FuncEnd:
;endif
;FunctionEnd