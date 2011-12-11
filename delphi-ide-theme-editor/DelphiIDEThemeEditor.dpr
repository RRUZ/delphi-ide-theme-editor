program DelphiIDEThemeEditor;
{$WARN SYMBOL_PLATFORM OFF}

uses
  uStackTrace in 'Units\uStackTrace.pas',
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
  uLoadThemesImages in 'Units\uLoadThemesImages.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
