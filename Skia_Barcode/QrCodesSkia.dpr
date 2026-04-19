program QrCodesSkia;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  QRBatchEngine in 'QRBatchEngine.pas',
  QRRenderer in 'QRRenderer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
