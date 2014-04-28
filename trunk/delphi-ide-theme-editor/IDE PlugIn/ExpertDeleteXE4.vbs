Const HKEY_CURRENT_USER = &H80000001
strComputer = "."
Set objRegistry=GetObject("winmgmts:\\" & strComputer & "\root\default:StdRegProv")
strKeyPath = "Software\Embarcadero\BDS\11.0\Experts"
strValueName = "DelphiIDEColorizer_XE4"
objRegistry.DeleteValue HKEY_CURRENT_USER, strKeyPath, strValueName