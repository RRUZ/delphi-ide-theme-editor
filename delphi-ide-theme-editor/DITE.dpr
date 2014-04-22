program DITE;
{$WARN SYMBOL_PLATFORM OFF}

uses
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
  uCheckUpdate in 'Units\uCheckUpdate.pas' {FrmCheckUpdate},
  uWinInet in 'Units\uWinInet.pas',
  Vcl.Themes,
  Vcl.Styles,
  uColorizerSettings in 'IDE PlugIn\uColorizerSettings.pas' {FrmIDEColorizerSettings},
  uStoreColorMap in 'IDE PlugIn\uStoreColorMap.pas',
  uIDEExpertUtils in 'IDE PlugIn\uIDEExpertUtils.pas',
  uClrSettings in 'IDE PlugIn\uClrSettings.pas',
  uVclStylesFix in 'Units\uVclStylesFix.pas',
  uLoadThemesImages in 'Units\uLoadThemesImages.pas',
  Vcl.Styles.Ext in 'Units\Vcl.Styles.Ext.pas',
  uStdActionsPopMenu in 'Units\uStdActionsPopMenu.pas',
  uHelpInsight in 'Units\uHelpInsight.pas',
  Vcl.Styles.OwnerDrawFix in 'Units\Vcl.Styles.OwnerDrawFix.pas',
  uColorPanel in 'Units\uColorPanel.pas' {ColorPanel},
  uSMSIDEHighlight in 'Units\uSMSIDEHighlight.pas',
  uSMSVersions in 'Units\uSMSVersions.pas',
  uAppMethodVersions in 'Units\uAppMethodVersions.pas',
  KOLDetours in 'Units\KOLDetours.pas',
  Vcl.Styles.Utils.ComCtrls in 'Units\Vcl.Styles.Utils.ComCtrls.pas',
  Vcl.Styles.Utils.Forms in 'Units\Vcl.Styles.Utils.Forms.pas',
  Vcl.Styles.Utils.Menus in 'Units\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.ScreenTips in 'Units\Vcl.Styles.Utils.ScreenTips.pas',
  Vcl.Styles.Utils.StdCtrls in 'Units\Vcl.Styles.Utils.StdCtrls.pas',
  Vcl.Styles.Utils.SysControls in 'Units\Vcl.Styles.Utils.SysControls.pas',
  Vcl.Styles.Utils.SysStyleHook in 'Units\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Styles.Utils.SystemMenu in 'Units\Vcl.Styles.Utils.SystemMenu.pas',
  Vcl.Styles.Fixes in 'Units\Vcl.Styles.Fixes.pas',
  Vcl.Styles.Hooks in 'Units\Vcl.Styles.Hooks.pas';

{$R *.res}

procedure UpdateApp;
var
  Frm: TFrmCheckUpdate;
begin
  Frm := TFrmCheckUpdate.Create(nil);
  try
    Frm.CheckExternal:=True;
    if Frm.UpdateAvailable then
      Frm.ExecuteUpdater;
  finally
    Frm.Free;
  end;
end;

Var
  IDEsList:TList<TDelphiVersionData>;
begin
  IDEsList:=TList<TDelphiVersionData>.Create;
  FillListDelphiVersions(IDEsList);

  if (not IsSMSInstalled) and (not IsLazarusInstalled) and (IDEsList.Count = 0) then
  begin
    IDEsList.Free;
    MsgBox('You don''t have a Object Pascal IDE installed (1)');
    Halt(0);
  end;
  IDEsList.Free;

  //ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  if FrmMain.Settings.CheckForUpdates then
   UpdateApp;


  Application.Run;
end.
