// JCL_DEBUG_EXPERT_INSERTJDBG OFF
library DelphiIDEColorizer_XE;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Main in 'Main.pas',
  Colorizer.Utils in 'Colorizer.Utils.pas',
  Colorizer.Settings in 'Colorizer.Settings.pas',
  Colorizer.HookForms in 'Colorizer.HookForms.pas',
  Colorizer.Hooks in 'Colorizer.Hooks.pas',
  Colorizer.StoreColorMap in 'Colorizer.StoreColorMap.pas',
  ColorXPStyleActnCtrls in 'ColorXPStyleActnCtrls.pas',
  Colorizer.OptionsDlg in 'Colorizer.OptionsDlg.pas' {FrameColorizer: TFrame},
  Colorizer.SettingsForm in 'Colorizer.SettingsForm.pas' {FormIDEColorizerSettings},
  uDelphiVersions in '..\Units\uDelphiVersions.pas',
  uSupportedIDEs in '..\Units\uSupportedIDEs.pas',
  uMisc in '..\Units\uMisc.pas',
  uRegistry in '..\Units\uRegistry.pas',
  uRttiHelper in 'uRttiHelper.pas',
  DDetours in '..\Common\delphi-detours-library\DDetours.pas',
  InstDecode in '..\Common\delphi-detours-library\InstDecode.pas',
  uDelphiIDEHighlight in '..\Units\uDelphiIDEHighlight.pas',
  uStackTrace in '..\Units\uStackTrace.pas',
  Colorizer.Wrappers in 'Colorizer.Wrappers.pas',
  Colorizer.HookScrollBars in 'Colorizer.HookScrollBars.pas',
  Colorizer.uxThemeHelper in 'Colorizer.uxThemeHelper.pas',
  uColorSelector in '..\Units\uColorSelector.pas',	
  Vcl.Styles.Utils.FlatControls in '..\Common\Vcl Styles Utils\Vcl.Styles.Utils.FlatControls.pas',
  Vcl.Styles.Utils.FlatStyleHook in '..\Common\Vcl Styles Utils\Vcl.Styles.Utils.FlatStyleHook.pas',
  Vcl.Styles.Utils.FlatMenus in '..\Common\Vcl Styles Utils\Vcl.Styles.Utils.FlatMenus.pas';

{$R *.res}
{$R VersionInfo.res}
begin
end.
