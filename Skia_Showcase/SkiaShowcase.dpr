program SkiaShowcase;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.skia,
  uMainForm in 'uMainForm.pas' {MainForm},
  uParticleSystem in 'uParticleSystem.pas',
  uAnimationEngine in 'uAnimationEngine.pas',
  uShaderPainter in 'uShaderPainter.pas',
  uDemoScenes in 'uDemoScenes.pas',
  uThemeEngine in 'uThemeEngine.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable:= False;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
