Const HKEY_CURRENT_USER = &H80000001
strComputer = "."
Set objRegistry=GetObject("winmgmts:\\" & strComputer & "\root\default:StdRegProv")
strKeyPath   = "Software\Embarcadero\BDS\11.0\Experts"
strValueName = "DelphiIDEColorizer_XE4"
strValue     = "C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\DelphiIDEColorizer_XE4.dll"
objRegistry.SetStringValue HKEY_CURRENT_USER, strKeyPath, strValueName, strValue