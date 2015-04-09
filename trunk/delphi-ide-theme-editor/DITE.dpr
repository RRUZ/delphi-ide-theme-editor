// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program DITE;
{$WARN SYMBOL_PLATFORM OFF}

uses
  {$IFDEF DEBUG}
  {$ENDIF }
  uStackTrace in 'Units\uStackTrace.pas',
  Generics.Defaults,
  Generics.Collections,
  Forms,
  Main in 'Main.pas' {FrmMain},
  uDelphiIDEHighlight in 'Units\uDelphiIDEHighlight.pas',
  uDelphiVersions in 'Units\uDelphiVersions.pas',
  uHSLUtils in 'Units\uHSLUtils.pas',
  uHueSat in 'Units\uHueSat.pas' {FrmHueSat},
  uRegistry in 'Units\uRegistry.pas',
  uSettings in 'Units\uSettings.pas' {FrmSettings},
  uColorSelector in 'Units\uColorSelector.pas' {DialogColorSelector},
  VSThemes in 'Units\VSThemes.pas',
  EclipseThemes in 'Units\EclipseThemes.pas',
  uLazarusVersions in 'Units\uLazarusVersions.pas',
  uSupportedIDEs in 'Units\uSupportedIDEs.pas',
  uMisc in 'Units\uMisc.pas',
  uLazarusIDEHighlight in 'Units\uLazarusIDEHighlight.pas',
  Vcl.Themes,
  Vcl.Styles,
  uVclStylesFix in 'Units\uVclStylesFix.pas',
  uLoadThemesImages in 'Units\uLoadThemesImages.pas',
  uStdActionsPopMenu in 'Units\uStdActionsPopMenu.pas',
  uHelpInsight in 'Units\uHelpInsight.pas',
  uColorPanel in 'Units\uColorPanel.pas' {ColorPanel},
  uSMSIDEHighlight in 'Units\uSMSIDEHighlight.pas',
  uSMSVersions in 'Units\uSMSVersions.pas',
  uAppMethodVersions in 'Units\uAppMethodVersions.pas',
  Vcl.Styles.Ext in 'Common\Vcl Styles Utils\Vcl.Styles.Ext.pas',
  Vcl.Styles.Fixes in 'Common\Vcl Styles Utils\Vcl.Styles.Fixes.pas',
  Vcl.Styles.Hooks in 'Common\Vcl Styles Utils\Vcl.Styles.Hooks.pas',
  Vcl.Styles.Utils.ComCtrls in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.ComCtrls.pas',
  Vcl.Styles.Utils.Forms in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.Forms.pas',
  Vcl.Styles.Utils.Menus in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.ScreenTips in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.ScreenTips.pas',
  Vcl.Styles.Utils.StdCtrls in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.StdCtrls.pas',
  Vcl.Styles.Utils.SysControls in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.SysControls.pas',
  Vcl.Styles.Utils.SysStyleHook in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Styles.Utils.SystemMenu in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.SystemMenu.pas',
  DDetours in 'Common\delphi-detours-library\DDetours.pas',
  InstDecode in 'Common\delphi-detours-library\InstDecode.pas',
  Vcl.Styles.FormStyleHooks in 'Common\Vcl Styles Utils\Vcl.Styles.FormStyleHooks.pas',
  Vcl.Styles.NC in 'Common\Vcl Styles Utils\Vcl.Styles.NC.pas',
  Vcl.Styles.OwnerDrawFix in 'Common\Vcl Styles Utils\Vcl.Styles.OwnerDrawFix.pas',
  uAdditionalSettings in 'Units\uAdditionalSettings.pas' {FrmAdditionalSettings},
  Vcl.Styles.UxTheme in 'Common\Vcl Styles Utils\Vcl.Styles.UxTheme.pas',
  Vcl.Styles.Utils.Graphics in 'Common\Vcl Styles Utils\Vcl.Styles.Utils.Graphics.pas';

{$R *.res}

Var
  IDEsList:TList<TDelphiVersionData>;
begin
  IDEsList:=TObjectList<TDelphiVersionData>.Create;
  FillListDelphiVersions(IDEsList);

  if (not IsSMSInstalled) and (not IsLazarusInstalled) and (IDEsList.Count = 0) then
  begin
    IDEsList.Free;
    MsgBox('You don''t have a Object Pascal IDE installed (1)');
    Halt(0);
  end;
  IDEsList.Free;

  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  if FrmMain.Settings.CheckForUpdates then
   CheckForUpdates(True);


  Application.Run;
end.
