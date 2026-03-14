program SkiaNodeEditorDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  uNodeEditor in 'uNodeEditor.pas' {FormNodeEditor},
  uNodePlugin in 'uNodePlugin.pas',
  uBuiltinNodes in 'uBuiltinNodes.pas',
  uExamplePlugins in 'uExamplePlugins.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable:= False;
  Application.Initialize;
  Application.CreateForm(TFormNodeEditor, FormNodeEditor);
  Application.Run;
end.
