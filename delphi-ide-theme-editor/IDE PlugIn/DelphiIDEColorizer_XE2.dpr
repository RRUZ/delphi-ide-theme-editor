// JCL_DEBUG_EXPERT_INSERTJDBG OFF
library DelphiIDEColorizer_XE2;

uses
  Main in 'Main.pas',
  Colorizer.Utils in 'Colorizer.Utils.pas',
  Colorizer.Settings in 'Colorizer.Settings.pas',
  Colorizer.Hook.Forms in 'Colorizer.Hook.Forms.pas',
  Colorizer.Hooks in 'Colorizer.Hooks.pas',
  Colorizer.StoreColorMap in 'Colorizer.StoreColorMap.pas',
  Colorizer.OptionsDlg in 'Colorizer.OptionsDlg.pas' {FrameColorizer: TFrame},
  Colorizer.SettingsForm in 'Colorizer.SettingsForm.pas' {FormIDEColorizerSettings},
  uDelphiVersions in '..\Units\uDelphiVersions.pas',
  uSupportedIDEs in '..\Units\uSupportedIDEs.pas',
  uMisc in '..\Units\uMisc.pas',
  uRegistry in '..\Units\uRegistry.pas',
  uRttiHelper in 'uRttiHelper.pas',
  Vcl.Styles.Ext in '..\Common\Vcl Styles Utils\Vcl.Styles.Ext.pas',
  DDetours in '..\Common\delphi-detours-library\DDetours.pas',
  InstDecode in '..\Common\delphi-detours-library\InstDecode.pas',
  uDelphiIDEHighlight in '..\Units\uDelphiIDEHighlight.pas',
  uStackTrace in '..\Units\uStackTrace.pas',
  Colorizer.Wrappers in 'Colorizer.Wrappers.pas',
  uColorSelector in '..\Units\uColorSelector.pas',
  Colorizer.Hooks.GDIPOBJ in 'Colorizer.Hooks.GDIPOBJ.pas',
  Vcl.Styles.Utils.FlatControls in '..\Common\Vcl Styles Utils\Vcl.Styles.Utils.FlatControls.pas',
  Vcl.Styles.Utils.FlatStyleHook in '..\Common\Vcl Styles Utils\Vcl.Styles.Utils.FlatStyleHook.pas',
  Vcl.Styles.Utils.FlatMenus in '..\Common\Vcl Styles Utils\Vcl.Styles.Utils.FlatMenus.pas',
  Vcl.Styles.Utils.Graphics in '..\Common\Vcl Styles Utils\Vcl.Styles.Utils.Graphics.pas';

{$R *.res}
{$R VersionInfo.res}
begin
end.
