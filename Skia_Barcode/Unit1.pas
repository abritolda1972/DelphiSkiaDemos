unit Unit1;

{ ===========================================================================
  QR Code Generator — Skia4Delphi Edition

  Descrição: Gerador de QR Codes de alta performance utilizando a Skia.
  Funcionalidades:
    + Imagem central no QR Code (logo/ícone)
    + Geração em lote (batch) em background thread (TThread / Task)

  Copyright (c) 2026 Alberto Brito. Todos os direitos reservados.
  Autor: Alberto Brito
  Data: 15 de Abril de 2026
  Versão: 1.0.0
  =========================================================================== }
interface

uses
  QRBatchEngine,
  QRRenderer,
  QRVclUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.NetEncoding,
  System.Skia,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.ZLib,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Skia,
  Vcl.StdCtrls,
  Winapi.Windows;

type
  TForm1 = class(TForm)
    // ---- Painel esquerdo: controlos existentes ----
    PaintBox:          TSkPaintBox;
    BtnGerar:          TButton;
    BtnSalvar:         TButton;
    BtnLogo:           TButton;
    BtnRemoverLogo:    TButton;
    LblTexto:          TLabel;
    LblDados:          TLabel;
    CboDados:          TComboBox;
    LblFinderExt:      TLabel;
    CboFinderExt:      TComboBox;
    LblFinderInt:      TLabel;
    CboFinderInt:      TComboBox;
    LblModoDados:      TLabel;
    CboModoDados:      TComboBox;
    LblModoFinder:     TLabel;
    CboModoFinder:     TComboBox;
    LblBg:             TLabel;
    PanelBgColor:      TPanel;
    LblDadosCor1:      TLabel;
    PanelDataColor1:   TPanel;
    LblDadosCor2:      TLabel;
    PanelDataColor2:   TPanel;
    LblFinderCor1:     TLabel;
    PanelFinderColor1: TPanel;
    LblFinderCor2:     TLabel;
    PanelFinderColor2: TPanel;
    EdtTexto:          TMemo;
    LblLogoInfo:       TLabel;
    LblLogoSize:       TLabel;
    TrkLogoSize:       TTrackBar;

    // ---- Painel de batch (novo) ----
    PnlBatch:          TPanel;       // container do grupo batch
    LblBatchTitle:     TLabel;
    LblBatchList:      TLabel;
    MemoLote:          TMemo;        // lista de textos, 1 por linha
    LblOutputDir:      TLabel;
    EdtOutputDir:      TEdit;
    BtnEscolherDir:    TButton;
    LblFilePrefix:     TLabel;
    EdtFilePrefix:     TEdit;
    LblExportSize:     TLabel;
    EdtExportSize:     TEdit;
    BtnGerarLote:      TButton;
    BtnCancelarLote:   TButton;
    ProgressLote:      TProgressBar;
    LblProgressInfo:   TLabel;       // "3 / 10 — Sucesso: 2  Erro: 1"
    LblBatchStatus:    TLabel;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;       // "Em curso..." / "Concluído"

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnGerarClick(Sender: TObject);
    procedure BtnSalvarClick(Sender: TObject);
    procedure BtnLogoClick(Sender: TObject);
    procedure BtnRemoverLogoClick(Sender: TObject);
    procedure PanelColorClick(Sender: TObject);
    procedure PaintBoxDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure TrkLogoSizeChange(Sender: TObject);

    procedure BtnEscolherDirClick(Sender: TObject);
    procedure BtnGerarLoteClick(Sender: TObject);
    procedure BtnCancelarLoteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  private
    FCurrentOpts: TQROptions;
    FHasQR:       Boolean;
    FLogoImage:   ISkImage;    // usado pelo PaintBox (main thread)
    FLogoPath:    string;      // path em disco do logo

    FBatchEngine: TQRBatchEngine;  // referência à worker thread activa
    FBatchRunning: Boolean;

    // Callbacks do batch engine (chamados na main thread via Queue)
    procedure OnBatchProgress(
      const AResult:  TQRBatchResult;
      TotalItems:     Integer;
      ProcessedItems: Integer;
      SuccessCount:   Integer;
      ErrorCount:     Integer);
    procedure OnBatchFinished(
      TotalItems:   Integer;
      SuccessCount: Integer;
      ErrorCount:   Integer;
      OutputDir:    string);

    // Recolhe as opções visuais actuais da UI
    function CollectOptions: TQROptions;

    // Helper: actualiza estado dos botões do batch
    procedure SetBatchUIRunning(ARunning: Boolean);

    function ComprimirTextoBase64(const Texto: string): string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Vcl.FileCtrl;  // para SelectDirectory

// ---------------------------------------------------------------------------
// Compressão / Base64
// ---------------------------------------------------------------------------

function TForm1.ComprimirTextoBase64(const Texto: string): string;
var
  LInput, LOutput: TBytesStream;
  LZip: TZCompressionStream;
begin
  LInput  := TBytesStream.Create(TEncoding.UTF8.GetBytes(Texto));
  LOutput := TBytesStream.Create;
  try
    LZip := TZCompressionStream.Create(clMax, LOutput);
    try
      LZip.CopyFrom(LInput, LInput.Size);
    finally
      LZip.Free;
    end;
    Result := TNetEncoding.Base64.EncodeBytesToString(LOutput.Bytes, LOutput.Size);
  finally
    LInput.Free;
    LOutput.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Recolher opções visuais da UI
// ---------------------------------------------------------------------------

function TForm1.CollectOptions: TQROptions;
begin
  Result.DataShape := TQRShape(CboDados.ItemIndex);

  Result.DataColor.Mode   := TQRColorMode(CboModoDados.ItemIndex);
  Result.DataColor.Color1 := VclColorToAlpha(PanelDataColor1.Color);
  Result.DataColor.Color2 := VclColorToAlpha(PanelDataColor2.Color);

  Result.FinderStyle.OuterShape   := TQRShape(CboFinderExt.ItemIndex);
  Result.FinderStyle.InnerShape   := TQRShape(CboFinderInt.ItemIndex);
  Result.FinderStyle.Color.Mode   := TQRColorMode(CboModoFinder.ItemIndex);
  Result.FinderStyle.Color.Color1 := VclColorToAlpha(PanelFinderColor1.Color);
  Result.FinderStyle.Color.Color2 := VclColorToAlpha(PanelFinderColor2.Color);

  Result.BgColor := VclColorToAlpha(PanelBgColor.Color);
end;

// ---------------------------------------------------------------------------
// PaintBox.OnDraw — delega no TQRRenderer
// ---------------------------------------------------------------------------

procedure TForm1.PaintBoxDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  Paint:          ISkPaint;
  Font:           ISkFont;
  Margin, QRSize: Single;
  MsgX, MsgY:    Single;
begin
  if not FHasQR then
  begin
    Paint := TSkPaint.Create;
    Paint.Color := $FFE8EAF6;
    ACanvas.DrawRect(ADest, Paint);
    Font   := TSkFont.Create(TSkTypeface.MakeDefault, 18);
    MsgX   := ADest.Left + (ADest.Width  / 2) - 140;
    MsgY   := ADest.Top  + (ADest.Height / 2);
    Paint.Color := $FF9E9E9E;
    ACanvas.DrawSimpleText('Clique em "Gerar QR Code"', MsgX, MsgY, Font, Paint);
    Exit;
  end;

  Margin := 8;
  QRSize := Min(ADest.Width, ADest.Height) - Margin * 2;

  ACanvas.Save;
  ACanvas.Translate(
    ADest.Left + (ADest.Width  - QRSize) / 2,
    ADest.Top  + (ADest.Height - QRSize) / 2);

  TQRRenderer.RenderQR(ACanvas, QRSize, FCurrentOpts, EdtTexto.Lines.Text,
    FLogoImage, TrkLogoSize.Position / 100.0);

  ACanvas.Restore;
end;

// ---------------------------------------------------------------------------
// Eventos do formulário
// ---------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
const
  Shapes: array[0..4] of string = (
    'Círculo', 'Quadrado', 'Diamante', 'Canto Redondo', 'Coração');
begin
  FHasQR       := False;
  FLogoImage   := nil;
  FLogoPath    := '';
  FBatchEngine := nil;
  FBatchRunning := False;

  PaintBox.OnDraw := PaintBoxDraw;

  CboDados.Items.AddStrings(Shapes);     CboDados.ItemIndex     := 0;
  CboFinderExt.Items.AddStrings(Shapes); CboFinderExt.ItemIndex := 0;
  CboFinderInt.Items.AddStrings(Shapes); CboFinderInt.ItemIndex := 1;

  CboModoDados.Items.Add('Cor sólida');
  CboModoDados.Items.Add('Gradiente');
  CboModoDados.ItemIndex := 0;

  CboModoFinder.Items.Add('Cor sólida');
  CboModoFinder.Items.Add('Gradiente');
  CboModoFinder.ItemIndex := 0;

  for var P in [TPanel(PanelBgColor), PanelDataColor1, PanelDataColor2,
                PanelFinderColor1, PanelFinderColor2] do
  begin
    P.ParentBackground := False;
    P.ParentColor      := False;
  end;

  PanelBgColor.Color      := clWhite;
  PanelDataColor1.Color   := clBlack;
  PanelDataColor2.Color   := clRed;
  PanelFinderColor1.Color := clBlack;
  PanelFinderColor2.Color := clBlue;

  EdtTexto.Lines.Text := 'https://meusite.com';

  // Max=30 corresponde ao cap de segurança interno do renderer
  // (ComputeReservedArea.MAX_SAFE_FRACTION). Acima disto o ECC-H
  // pode não conseguir reconstruir os dados sob o logo.
  TrkLogoSize.Min      := 10;
  TrkLogoSize.Max      := 30;
  TrkLogoSize.Position := 20;
  TrkLogoSize.TickStyle := tsNone;

  LblLogoInfo.Caption := 'Sem logo';
  LblLogoSize.Caption := 'Tamanho logo: 20%';

  // Estado inicial do batch
  EdtOutputDir.Text  := TPath.Combine(
    TPath.GetDocumentsPath, 'QRCodes_Lote');
  EdtFilePrefix.Text := 'qr_';
  EdtExportSize.Text := '1200';

  MemoLote.Lines.Text :=
    'https://exemplo.com' + sLineBreak +
    'https://outro.com'   + sLineBreak +
    'Texto de exemplo';

  ProgressLote.Min     := 0;
  ProgressLote.Max     := 100;
  ProgressLote.Position := 0;

  LblProgressInfo.Caption := '';
  LblBatchStatus.Caption  := 'Pronto';

  SetBatchUIRunning(False);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Se existe um batch a correr, sinaliza terminação mas não espera
  // (FreeOnTerminate=True — a thread liberta-se sozinha)
  if Assigned(FBatchEngine) and FBatchRunning then
    FBatchEngine.Terminate;
end;

// ---------------------------------------------------------------------------
// Eventos existentes (inalterados na lógica)
// ---------------------------------------------------------------------------

procedure TForm1.PanelColorClick(Sender: TObject);
var
  Dlg:   TColorDialog;
  Panel: TPanel;
begin
  Panel := Sender as TPanel;
  Dlg   := TColorDialog.Create(Self);
  try
    Dlg.Color := Panel.Color;
    if Dlg.Execute then
    begin
      Panel.Color            := Dlg.Color;
      Panel.ParentBackground := False;
      Panel.ParentColor      := False;
      Panel.Invalidate;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TForm1.BtnLogoClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Title  := 'Escolher imagem para o centro do QR Code';
    Dlg.Filter := 'Imagens|*.png;*.jpg;*.jpeg;*.webp;*.bmp|PNG|*.png|JPEG|*.jpg;*.jpeg|Todos|*.*';
    Dlg.Options := Dlg.Options + [ofFileMustExist];
    if not Dlg.Execute then Exit;

    FLogoImage := TSkImage.MakeFromEncodedFile(Dlg.FileName);
    var
    LSingle := FLogoImage.Height;
    LSingle := FLogoImage.Width;

    if not Assigned(FLogoImage) then
    begin
      ShowMessage('Não foi possível carregar a imagem.' + sLineBreak +
                  'Use PNG, JPEG ou WebP.');
      Exit;
    end;

    FLogoPath           := Dlg.FileName;
    LblLogoInfo.Caption := 'Logo: ' + ExtractFileName(Dlg.FileName);

    if FHasQR then
      PaintBox.Redraw;
  finally
    Dlg.Free;
  end;
end;

procedure TForm1.BtnRemoverLogoClick(Sender: TObject);
begin
  FLogoImage := nil;
  FLogoPath  := '';
  LblLogoInfo.Caption := 'Sem logo';
  if FHasQR then
    PaintBox.Redraw;
end;

procedure TForm1.TrkLogoSizeChange(Sender: TObject);
begin
  LblLogoSize.Caption := Format('Tamanho logo: %d%%', [TrkLogoSize.Position]);
  if FHasQR and Assigned(FLogoImage) then
    PaintBox.Redraw;
end;

procedure TForm1.BtnGerarClick(Sender: TObject);
begin
  FCurrentOpts := CollectOptions;
  FHasQR       := True;
  PaintBox.Redraw;
end;

procedure TForm1.BtnSalvarClick(Sender: TObject);
const
  ExportSize = 1200;
var
  PngBytes: TBytes;
  Logo:     TQRLogoData;
  Stream:   TFileStream;
  Dlg:      TSaveDialog;
begin
  if not FHasQR then
  begin
    ShowMessage('Gere primeiro um QR Code!');
    Exit;
  end;

  Dlg := TSaveDialog.Create(Self);
  try
    Dlg.Filter     := 'PNG Image|*.png';
    Dlg.DefaultExt := 'png';
    Dlg.FileName   := 'qrcode.png';
    if not Dlg.Execute then Exit;

    // Prepara logo em bytes (thread-safe, mas aqui ainda é main thread)
    Logo := MakeLogoData(FLogoPath, TrkLogoSize.Position / 100.0);

    PngBytes := TQRRenderer.GeneratePNG(
      EdtTexto.Lines.Text, CollectOptions, Logo, ExportSize);

    Stream := TFileStream.Create(Dlg.FileName, fmCreate);
    try
      if Length(PngBytes) > 0 then
        Stream.WriteBuffer(PngBytes[0], Length(PngBytes));
    finally
      Stream.Free;
    end;

    ShowMessage('QR Code guardado:' + sLineBreak + Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var i: integer;
begin
 memolote.Lines.Clear;
 for i := 0 to 400 do
   Begin
     memolote.Lines.Add('Texto para qrcode '+inttostr(i));
   End;

end;

// ---------------------------------------------------------------------------
// Batch — escolher directório
// ---------------------------------------------------------------------------

procedure TForm1.BtnEscolherDirClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := EdtOutputDir.Text;
  if SelectDirectory('Seleccionar directório de saída', '', Dir) then
    EdtOutputDir.Text := Dir;
end;

// ---------------------------------------------------------------------------
// Batch — iniciar geração em lote
// ---------------------------------------------------------------------------

procedure TForm1.BtnGerarLoteClick(Sender: TObject);
var
  Job:      TQRBatchJob;
  Items:    TArray<string>;
  ExportSz: Integer;
begin
  if FBatchRunning then
  begin
    ShowMessage('Já existe um lote em processamento. Aguarde ou cancele.');
    Exit;
  end;

  Items := CollectNonEmptyLines(MemoLote.Lines);
  if Length(Items) = 0 then
  begin
    ShowMessage('A lista de entradas está vazia.' + sLineBreak +
                'Adicione pelo menos uma linha no campo "Lista de entradas".');
    Exit;
  end;

  if Trim(EdtOutputDir.Text) = '' then
  begin
    ShowMessage('Por favor, indique o directório de saída.');
    Exit;
  end;

  ExportSz := StrToIntDef(Trim(EdtExportSize.Text), 1200);
  if (ExportSz < 100) or (ExportSz > 4000) then
  begin
    ShowMessage('Tamanho de exportação inválido. Use um valor entre 100 e 4000 px.');
    Exit;
  end;

  // Snapshot imutável de todas as opções — seguro para passar à thread
  Job.Items      := Items;
  Job.Opts       := CollectOptions;
  Job.Logo       := MakeLogoData(FLogoPath, TrkLogoSize.Position / 100.0);
  Job.OutputDir  := Trim(EdtOutputDir.Text);
  Job.ExportSize := ExportSz;
  Job.FilePrefix := Trim(EdtFilePrefix.Text);
  if Job.FilePrefix = '' then
    Job.FilePrefix := 'qr_';

  // Prepara UI
  ProgressLote.Max      := Length(Items);
  ProgressLote.Position := 0;
  LblProgressInfo.Caption := Format('0 / %d', [Length(Items)]);
  LblBatchStatus.Caption  := 'A processar...';
  SetBatchUIRunning(True);

  // Cria e arranca a worker thread
  FBatchEngine  := TQRBatchEngine.Create(Job, OnBatchProgress, OnBatchFinished);
  FBatchRunning := True;
  FBatchEngine.Start;
end;

// ---------------------------------------------------------------------------
// Batch — cancelar
// ---------------------------------------------------------------------------

procedure TForm1.BtnCancelarLoteClick(Sender: TObject);
begin
  if Assigned(FBatchEngine) and FBatchRunning then
  begin
    FBatchEngine.Terminate;
    LblBatchStatus.Caption := 'A cancelar...';
    BtnCancelarLote.Enabled := False;
  end;
end;

// ---------------------------------------------------------------------------
// Callbacks do batch engine — executados na main thread via TThread.Queue
// ---------------------------------------------------------------------------

procedure TForm1.OnBatchProgress(
  const AResult:  TQRBatchResult;
  TotalItems:     Integer;
  ProcessedItems: Integer;
  SuccessCount:   Integer;
  ErrorCount:     Integer);
begin
  ProgressLote.Position := ProcessedItems;

  LblProgressInfo.Caption := Format(
    '%d / %d  —  Sucesso: %d  Erro: %d',
    [ProcessedItems, TotalItems, SuccessCount, ErrorCount]);

  if not AResult.Success then
  begin
    // Opcional: mostrar no status a última entrada com erro
    LblBatchStatus.Caption := Format(
      'Erro em item %d: %s', [AResult.Index + 1,
      Copy(AResult.ErrorMessage, 1, 60)]);
  end
  else
    LblBatchStatus.Caption := Format('Processado: %s',
      [Copy(AResult.Content, 1, 50)]);
end;

procedure TForm1.OnBatchFinished(
  TotalItems:   Integer;
  SuccessCount: Integer;
  ErrorCount:   Integer;
  OutputDir:    string);
var
  Msg: string;
begin
  FBatchRunning := False;
  FBatchEngine  := nil;   // FreeOnTerminate=True — já foi libertado pela thread

  SetBatchUIRunning(False);

  ProgressLote.Position := TotalItems;

  Msg := Format(
    'Lote concluído!'            + sLineBreak +
    'Total processados : %d'     + sLineBreak +
    'Com sucesso       : %d'     + sLineBreak +
    'Com erro          : %d'     + sLineBreak + sLineBreak +
    'Ficheiros em:'              + sLineBreak +
    '%s',
    [TotalItems, SuccessCount, ErrorCount, OutputDir]);

  LblBatchStatus.Caption  := Format('Concluído — %d OK, %d erro(s)',
    [SuccessCount, ErrorCount]);
  LblProgressInfo.Caption := Format('%d / %d  —  Sucesso: %d  Erro: %d',
    [TotalItems, TotalItems, SuccessCount, ErrorCount]);

  ShowMessage(Msg);
end;

// ---------------------------------------------------------------------------
// Helper UI
// ---------------------------------------------------------------------------

procedure TForm1.SetBatchUIRunning(ARunning: Boolean);
begin
  BtnGerarLote.Enabled    := not ARunning;
  BtnCancelarLote.Enabled := ARunning;
  MemoLote.ReadOnly       := ARunning;
  EdtOutputDir.ReadOnly   := ARunning;
  EdtFilePrefix.ReadOnly  := ARunning;
  EdtExportSize.ReadOnly  := ARunning;
  BtnEscolherDir.Enabled  := not ARunning;
end;

end.
