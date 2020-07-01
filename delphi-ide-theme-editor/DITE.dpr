// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program DITE;
{$WARN SYMBOL_PLATFORM OFF}

uses
  {$IFDEF DEBUG}
  {$ENDIF }
  DITE.StackTrace in 'Units\DITE.StackTrace.pas',
  Generics.Defaults,
  Generics.Collections,
  Forms,
  DITE.Main in 'DITE.Main.pas' {FrmMain},
  DITE.DelphiIDEHighlight in 'Units\DITE.DelphiIDEHighlight.pas',
  DITE.DelphiVersions in 'Units\DITE.DelphiVersions.pas',
  DITE.HSLUtils in 'Units\DITE.HSLUtils.pas',
  DITE.HueSat in 'Units\DITE.HueSat.pas' {FrmHueSat},
  DITE.Registry in 'Units\DITE.Registry.pas',
  DITE.Settings in 'Units\DITE.Settings.pas' {FrmSettings},
  DITE.ColorSelector in 'Units\DITE.ColorSelector.pas' {DialogColorSelector},
  DITE.VSThemes in 'Units\DITE.VSThemes.pas',
  DITE.EclipseThemes in 'Units\DITE.EclipseThemes.pas',
  DITE.LazarusVersions in 'Units\DITE.LazarusVersions.pas',
  DITE.SupportedIDEs in 'Units\DITE.SupportedIDEs.pas',
  DITE.Misc in 'Units\DITE.Misc.pas',
  DITE.LazarusIDEHighlight in 'Units\DITE.LazarusIDEHighlight.pas',
  Vcl.Themes,
  Vcl.Styles,
  DITE.VclStylesFix in 'Units\DITE.VclStylesFix.pas',
  DITE.LoadThemesImages in 'Units\DITE.LoadThemesImages.pas',
  DITE.StdActionsPopMenu in 'Units\DITE.StdActionsPopMenu.pas',
  DITE.HelpInsight in 'Units\DITE.HelpInsight.pas',
  DITE.ColorPanel in 'Units\DITE.ColorPanel.pas' {ColorPanel},
  DITE.SMSIDEHighlight in 'Units\DITE.SMSIDEHighlight.pas',
  DITE.SMSVersions in 'Units\DITE.SMSVersions.pas',
  DITE.AppMethodVersions in 'Units\DITE.AppMethodVersions.pas',
  DITE.AdditionalSettings in 'Units\DITE.AdditionalSettings.pas' {FrmAdditionalSettings};

{$R *.res}

Var
  IDEsList: TList<TDelphiVersionData>;
begin
  IDEsList := TObjectList<TDelphiVersionData>.Create;
  FillListDelphiVersions(IDEsList);

  if not IsSMSInstalled and not IsLazarusInstalled and (IDEsList.Count = 0) then
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
