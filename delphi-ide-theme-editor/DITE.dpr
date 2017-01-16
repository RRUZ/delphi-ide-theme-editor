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
  DITE.Main in 'DITE.Main.pas' {FrmMain},
  uDelphiIDEHighlight in 'Units\uDelphiIDEHighlight.pas',
  uDelphiVersions in 'Units\uDelphiVersions.pas',
  uHSLUtils in 'Units\uHSLUtils.pas',
  uHueSat in 'Units\uHueSat.pas' {FrmHueSat},
  uRegistry in 'Units\uRegistry.pas',
  DITE.Settings in 'Units\DITE.Settings.pas' {FrmSettings},
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
  uAdditionalSettings in 'Units\uAdditionalSettings.pas' {FrmAdditionalSettings};

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
