// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program Updater;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  System.StrUtils,
  System.SysUtils,
  System.IOUtils,
  uMain in 'uMain.pas' {FrmMain},
  uHttpDownload in 'uHttpDownload.pas',
  uMisc in '..\Units\uMisc.pas';

{$R *.res}

Procedure SetStyle;
var
 i : integer;
 StylefileName : string;
begin
   if ParamCount>=1 then
   begin
     for i:= 1 to ParamCount do
     begin
      if StartsText('-style:', ParamStr(i)) then
      begin
       StylefileName := StringReplace(ParamStr(i), '-style:', '', [rfReplaceAll]);
       if TFile.Exists(StylefileName) and TStyleManager.IsValidStyle(StylefileName) then
       begin
         TStyleManager.SetStyle(TStyleManager.LoadFromFile(StylefileName));
       end;

       exit;
      end;
     end;
   end;
end;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  SetStyle;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
