program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  globaluseskia:=true;
  GlobalUseSkiaRasterWhenAvailable:=False;
  GlobalUseVulkan := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
