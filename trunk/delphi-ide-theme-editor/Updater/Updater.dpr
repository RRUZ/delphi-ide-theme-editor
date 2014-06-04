// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE ON
program Updater;

uses
  Vcl.Forms,
  uCheckUpdate in 'uCheckUpdate.pas',
  uMisc in '..\Units\uMisc.pas',
  uWinInet in '..\Units\uWinInet.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmCheckUpdate, FrmCheckUpdate);
  Application.Run;
end.
