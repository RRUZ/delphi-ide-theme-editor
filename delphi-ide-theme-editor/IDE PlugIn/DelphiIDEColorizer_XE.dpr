// JCL_DEBUG_EXPERT_INSERTJDBG OFF
library DelphiIDEColorizer_XE;

uses
  {$IFDEF DEBUG}
  {$ENDIF}
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
  uIDEExpertUtils in 'uIDEExpertUtils.pas',
  uDelphiIDEHighlight in '..\Units\uDelphiIDEHighlight.pas',
  DDetours in '..\Common\delphi-detours-library\DDetours.pas',
  InstDecode in '..\Common\delphi-detours-library\InstDecode.pas';

{$R *.res}
{$R VersionInfo.res}
begin
end.
