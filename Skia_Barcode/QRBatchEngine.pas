unit QRBatchEngine;

{ ===========================================================================
  QRBatchEngine — Geração em lote de QR Codes em background thread.

  Design:
  · TQRBatchJob    — parâmetros imutáveis de um lote (snapshot da UI)
  · TQRBatchResult — resultado de um item individual
  · TQRBatchEngine — TThread que processa o lote e notifica a UI via callbacks
  =========================================================================== }

interface

uses
  QRRenderer,
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Types;

type
  { Resultado de um único item do lote }
  TQRBatchResult = record
    Index:        Integer;   // posição na lista original (0-based)
    Content:      string;    // texto que foi processado
    OutputFile:   string;    // caminho do PNG gerado (vazio se erro)
    Success:      Boolean;
    ErrorMessage: string;
  end;

  { Callback chamado na main thread com o progresso após cada item }
  TBatchProgressEvent = procedure(
    const AResult:    TQRBatchResult;
    TotalItems:       Integer;
    ProcessedItems:   Integer;
    SuccessCount:     Integer;
    ErrorCount:       Integer) of object;

  { Callback chamado na main thread quando o lote termina }
  TBatchFinishedEvent = procedure(
    TotalItems:     Integer;
    SuccessCount:   Integer;
    ErrorCount:     Integer;
    OutputDir:      string) of object;

  { Snapshot imutável dos parâmetros — criado na main thread, lido pela worker }
  TQRBatchJob = record
    Items:      TArray<string>;   // textos a processar (sem linhas vazias)
    Opts:       TQROptions;       // opções visuais
    Logo:       TQRLogoData;      // bytes do logo (vazio = sem logo)
    OutputDir:  string;           // diretório de destino
    ExportSize: Integer;          // tamanho do PNG em px (default 1200)
    FilePrefix: string;           // prefixo do ficheiro, ex: 'qr_'
  end;

  TQRBatchEngine = class(TThread)
  private
    FJob:            TQRBatchJob;
    FOnProgress:     TBatchProgressEvent;
    FOnFinished:     TBatchFinishedEvent;

    // Variáveis partilhadas apenas para Queue (acesso exclusivo por Queue/Sync)
    FLastResult:     TQRBatchResult;
    FTotalItems:     Integer;
    FProcessed:      Integer;
    FSuccessCount:   Integer;
    FErrorCount:     Integer;

    procedure NotifyProgress;
    procedure NotifyFinished;
  protected
    procedure Execute; override;
  public
    constructor Create(const AJob: TQRBatchJob;
      AOnProgress: TBatchProgressEvent;
      AOnFinished: TBatchFinishedEvent);
  end;

{ Constrói um TQRLogoData a partir de bytes brutos de um ficheiro de imagem }
function MakeLogoData(const AImagePath: string;
  SizeFraction: Single): TQRLogoData;

{ Lê as linhas de um TMemo/TStrings e devolve apenas as não-vazias }
function CollectNonEmptyLines(ALines: TStrings): TArray<string>;

implementation

uses
  System.Math;

// ---------------------------------------------------------------------------
// Utilitários públicos
// ---------------------------------------------------------------------------

function MakeLogoData(const AImagePath: string;
  SizeFraction: Single): TQRLogoData;
begin
  Result.SizeFraction := SizeFraction;
  Result.Bytes        := [];
  if (AImagePath <> '') and TFile.Exists(AImagePath) then
    Result.Bytes := TFile.ReadAllBytes(AImagePath);
end;

function CollectNonEmptyLines(ALines: TStrings): TArray<string>;
var
  I, Count: Integer;
begin
  SetLength(Result, ALines.Count);
  Count := 0;
  for I := 0 to ALines.Count - 1 do
  begin
    if Trim(ALines[I]) <> '' then
    begin
      Result[Count] := Trim(ALines[I]);
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;

// ---------------------------------------------------------------------------
// TQRBatchEngine
// ---------------------------------------------------------------------------

constructor TQRBatchEngine.Create(const AJob: TQRBatchJob;
  AOnProgress: TBatchProgressEvent;
  AOnFinished: TBatchFinishedEvent);
begin
  inherited Create(True);   // criado suspenso
  FJob         := AJob;
  FOnProgress  := AOnProgress;
  FOnFinished  := AOnFinished;
  FreeOnTerminate := True;
end;

procedure TQRBatchEngine.NotifyProgress;
begin
  // Já estamos dentro de Queue — executado na main thread
  if Assigned(FOnProgress) then
    FOnProgress(FLastResult, FTotalItems, FProcessed,
                FSuccessCount, FErrorCount);
end;

procedure TQRBatchEngine.NotifyFinished;
begin
  if Assigned(FOnFinished) then
    FOnFinished(FTotalItems, FSuccessCount, FErrorCount, FJob.OutputDir);
end;

procedure TQRBatchEngine.Execute;
var
  I:          Integer;
  Content:    string;
  PngBytes:   TBytes;
  OutFile:    string;
  SafeName:   string;
  Res:        TQRBatchResult;

  { Sanitiza o texto para usar como parte do nome do ficheiro }
  function SafeFileName(const S: string; MaxLen: Integer = 40): string;
  var
    C: Char;
  begin
    Result := '';
    for C in S do
      if CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.']) then
        Result := Result + C
      else
        Result := Result + '_';
    if Length(Result) > MaxLen then
      Result := Copy(Result, 1, MaxLen);
    if Result = '' then
      Result := 'item';
  end;

begin
  FTotalItems   := Length(FJob.Items);
  FProcessed    := 0;
  FSuccessCount := 0;
  FErrorCount   := 0;

  // Garante que o directório existe
  try
    if not TDirectory.Exists(FJob.OutputDir) then
      TDirectory.CreateDirectory(FJob.OutputDir);
  except
    on E: Exception do
    begin
      // Se não conseguimos criar o directório, termina com erro em tudo
      for I := 0 to FTotalItems - 1 do
      begin
        Res.Index        := I;
        Res.Content      := FJob.Items[I];
        Res.OutputFile   := '';
        Res.Success      := False;
        Res.ErrorMessage := 'Não foi possível criar o directório: ' + E.Message;
        Inc(FProcessed);
        Inc(FErrorCount);
        FLastResult := Res;
        TThread.Queue(nil, NotifyProgress);
      end;
      TThread.Queue(nil, NotifyFinished);
      Exit;
    end;
  end;

  for I := 0 to FTotalItems - 1 do
  begin
    if Terminated then Break;

    Content := FJob.Items[I];
    Res.Index   := I;
    Res.Content := Content;

    try
      // Nome do ficheiro: prefixo + índice zero-padded + trecho do conteúdo
      SafeName := FJob.FilePrefix
                + Format('%.*d', [Length(IntToStr(FTotalItems)), I + 1]);


         //comentado porque nao nao quero o conteudo no nome do ficheiro
//      SafeName := FJob.FilePrefix
//                + Format('%.*d', [Length(IntToStr(FTotalItems)), I + 1])
//                + '_' + SafeFileName(Content);

      OutFile := TPath.Combine(FJob.OutputDir, SafeName + '.png');

      // Renderização 100% off-screen, sem tocar em componentes VCL
      PngBytes := TQRRenderer.GeneratePNG(
        Content, FJob.Opts, FJob.Logo, FJob.ExportSize);

      if Length(PngBytes) = 0 then
        raise Exception.Create('GeneratePNG devolveu dados vazios.');

      // Escrita do ficheiro
      TFile.WriteAllBytes(OutFile, PngBytes);

      Res.OutputFile   := OutFile;
      Res.Success      := True;
      Res.ErrorMessage := '';
      Inc(FSuccessCount);

    except
      on E: Exception do
      begin
        Res.OutputFile   := '';
        Res.Success      := False;
        Res.ErrorMessage := E.Message;
        Inc(FErrorCount);
      end;
    end;

    Inc(FProcessed);
    FLastResult := Res;

    // Notifica a UI de forma thread-safe (Queue é assíncrono e não bloqueia)
    TThread.Queue(nil, NotifyProgress);
  end;

  TThread.Queue(nil, NotifyFinished);
end;

end.
