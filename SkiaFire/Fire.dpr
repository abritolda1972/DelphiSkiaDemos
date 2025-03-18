program Fire;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  UnMain in 'UnMain.pas' {Form1};

{$R *.res}

begin
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable:= False;
  GlobalUseVulkan:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
