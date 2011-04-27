 // JCL_DEBUG_EXPERT_GENERATEJDBG OFF
 // JCL_DEBUG_EXPERT_INSERTJDBG OFF
 // JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program DelphiIDEThemeEditor;

uses
  uStackTrace in 'Units\uStackTrace.pas',
  Forms,
  Main in 'Main.pas' {FrmMain},
  uDelphiIDEHighlight in 'Units\uDelphiIDEHighlight.pas',
  uDelphiVersions in 'Units\uDelphiVersions.pas',
  uHSLUtils in 'Units\uHSLUtils.pas',
  uHueSat in 'Units\uHueSat.pas' {FrmHueSat},
  uRegistry in 'Units\uRegistry.pas',
  uSettings in 'Units\uSettings.pas' {FrmSettings};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
