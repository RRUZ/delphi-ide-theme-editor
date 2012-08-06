// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program Demo;

uses
  Forms,
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Color Lib Pack v2.0 Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
