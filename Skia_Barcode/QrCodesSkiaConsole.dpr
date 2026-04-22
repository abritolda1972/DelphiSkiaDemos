program QrCodesSkiaConsole;

{ ===========================================================================
  QrCodesSkiaConsole — demonstração headless (sem UI) do QRCodeService.

  Mostra:
    [1] GenerateQrCode devolvendo TStream (o chamador decide o destino)
    [2] GenerateQrCode a gravar directamente num path PNG
    [3] Geração em paralelo (TParallel.For) — thread-safe, ideal para
        webservices, workers ou processamento em lote sem UI

  Output: C:\Fontes Sistemas\DelphiSkiaDemos\Skia_Barcode\LogoSamples

  Requisitos em runtime:
    · sk4d.dll deve estar no mesmo directório do executável
      (copia do projecto VCL ao lado).
  =========================================================================== }

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.UITypes,
  System.Diagnostics,
  System.Threading,
  System.Math,
  System.Skia,
  QRRenderer in 'QRRenderer.pas',
  QRCodeService in 'QRCodeService.pas',
  DelphiZXIngQRCode in 'DelphiZXIngQRCode.pas';

var
  OUT_DIR: string;
  PATH_LOGOS: string;

// ---------------------------------------------------------------------------
// Helpers locais (apenas para a demo)
// ---------------------------------------------------------------------------

procedure EnsureOutputDir;
begin
  if not TDirectory.Exists(OUT_DIR) then
    TDirectory.CreateDirectory(OUT_DIR);
end;

{ Lista todos os logos de teste disponíveis em LogoSamples.
  Procura nomes típicos (square/16x9/9x16/10x1) para demonstrar a
  reserva rectangular alinhada ao aspecto do logo. }
function FindLogoSamples: TArray<string>;
const
  CANDIDATES: array[0..5] of string = (
    'square.png', '16x9.png', '9x16.png', '10x1.png',
    'logo.png', 'Delphi_Logo_12.svg.png');
var
  Cand, Path: string;
  Count: Integer;
begin
  SetLength(Result, Length(CANDIDATES));
  Count := 0;
  for Cand in CANDIDATES do
  begin
    Path := TPath.Combine(PATH_LOGOS, Cand);
    if TFile.Exists(Path) then
    begin
      Result[Count] := Path;
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;

procedure Banner(const ATitle: string);
begin
  Writeln('');
  Writeln('--- ', ATitle, ' ',
    StringOfChar('-', Max(0, 70 - Length(ATitle) - 6)));
end;

// ---------------------------------------------------------------------------
// Demo 1 — GenerateQrCode(TStream)
// ---------------------------------------------------------------------------

procedure Demo_GenerateStream;
var
  Req:     TQRCodeRequest;
  Stream:  TStream;
  OutFile: TFileStream;
  Path:    string;
begin
  Banner('[1] GenerateQrCode(Request): TStream');

  Req := TQRCodeRequest.New('https://github.com/skia4delphi/skia4delphi')
    .WithExportSize(900)
    .WithDataColor(TAlphaColors.Navy)
    .WithFinderColor(TAlphaColors.Crimson)
    .WithShapes(qrsRoundRect, qrsRoundRect, qrsCircle);

  // O serviço devolve um TStream — o chamador escolhe o destino
  // (ficheiro, HTTP response, base64, etc). Aqui gravamos num PNG.
  Stream := TQRCodeService.Generate(Req);
  try
    Path := TPath.Combine(OUT_DIR, 'sample_01_from_stream.png');
    OutFile := TFileStream.Create(Path, fmCreate);
    try
      OutFile.CopyFrom(Stream, 0);
    finally
      OutFile.Free;
    end;
    Writeln('    ', Path);
    Writeln('    bytes: ', Stream.Size);
  finally
    Stream.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Demo 2 — GenerateQrCode(Path)
// ---------------------------------------------------------------------------

procedure Demo_GenerateFile;
var
  Req:  TQRCodeRequest;
  Path: string;
begin
  Banner('[2] GenerateQrCode(Request, APath)');

  Req := TQRCodeRequest.New('SKIA4DELPHI + ZXING — consola headless')
    .WithExportSize(800)
    .WithGradientDataColor(TAlphaColors.Purple, TAlphaColors.Teal)
    .WithShapes(qrsDiamond, qrsSquare, qrsDiamond);

  Path := TPath.Combine(OUT_DIR, 'sample_02_direct_file.png');
  TQRCodeService.Generate(Req, Path);

  Writeln('    ', Path);
end;

// ---------------------------------------------------------------------------
// Demo 3 — QR com logo (se houver logo.png em LogoSamples)
// ---------------------------------------------------------------------------

procedure Demo_GenerateWithLogo;
var
  Logos:    TArray<string>;
  LogoPath: string;
  Req:      TQRCodeRequest;
  OutName:  string;
  OutPath:  string;
begin
  Banner('[3] QR com logo — reserva rectangular casada ao aspecto');

  Logos := FindLogoSamples;
  if Length(Logos) = 0 then
  begin
    Writeln('    (sem logos em LogoSamples — coloque square.png / 16x9.png / etc.)');
    Exit;
  end;

  for LogoPath in Logos do
  begin
    OutName := 'sample_03_logo_' +
      TPath.GetFileNameWithoutExtension(LogoPath) + '.png';
    OutPath := TPath.Combine(OUT_DIR, OutName);

    Req := TQRCodeRequest.New('https://example.com/item-with-logo/' +
        TPath.GetFileNameWithoutExtension(LogoPath))
      .WithExportSize(1200)
      .WithDataColor(TAlphaColors.Black)
      .WithFinderColor(TAlphaColors.Darkslateblue)
      .WithShapes(qrsCircle, qrsRoundRect, qrsDiamond)
      .WithLogoFromFile(LogoPath, 0.25);

    TQRCodeService.Generate(Req, OutPath);
    Writeln('    ', TPath.GetFileName(LogoPath), '  ->  ',
      TPath.GetFileName(OutPath));
  end;
end;

// ---------------------------------------------------------------------------
// Demo 4 — Geração em paralelo (TParallel.For)
//
// Simula um cenário de webservice / worker: N pedidos processados
// concorrentemente, cada um numa thread do pool. O serviço é stateless,
// não há locks partilhados.
// ---------------------------------------------------------------------------

procedure Demo_Multithreaded;
const
  COUNT = 50;
var
  SW:      TStopwatch;
  ErrCnt:  Integer;
begin
  Banner(Format('[4] %d QR codes em paralelo (TParallel.For)', [COUNT]));

  ErrCnt := 0;
  SW := TStopwatch.StartNew;

  TParallel.&For(0, COUNT - 1,
    procedure(I: Integer)
    var
      Req:  TQRCodeRequest;
      Path: string;
    begin
      try
        Req := TQRCodeRequest.New(Format('https://example.com/items/%d', [I]))
          .WithExportSize(600)
          .WithDataColor(TAlphaColors.Black);

        Path := TPath.Combine(OUT_DIR, Format('batch_%.3d.png', [I]));
        TQRCodeService.Generate(Req, Path);
      except
        on E: Exception do
          AtomicIncrement(ErrCnt);
      end;
    end);

  SW.Stop;

  Writeln(Format('    %d ficheiros, %d erro(s), em %d ms  (%.1f QR/s)',
    [COUNT, ErrCnt, SW.ElapsedMilliseconds,
     COUNT / Max(1, SW.ElapsedMilliseconds / 1000)]));
end;

// ---------------------------------------------------------------------------
// Demo 5 — Uso via interface (IQRCodeService) para DI / mocks
// ---------------------------------------------------------------------------

procedure Demo_InterfaceUsage;
var
  Svc:  IQRCodeService;
  Req:  TQRCodeRequest;
  Path: string;
begin
  Banner('[5] Uso via interface IQRCodeService (DI friendly)');

  Svc := TQRCodeService.CreateService;

  Req := TQRCodeRequest.New('Injection-friendly QR')
    .WithExportSize(700)
    .WithDataColor(TAlphaColors.Seagreen);

  Path := TPath.Combine(OUT_DIR, 'sample_05_via_interface.png');
  Svc.GenerateQrCode(Req, Path);

  Writeln('    ', Path);
end;

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

begin
  OUT_DIR := TPath.Combine(TPath.GetAppPath, 'ConsoleResult');
  PATH_LOGOS := TPath.Combine(TPath.GetAppPath, 'LogoSamples');

  try
    Writeln('====================================================================');
    Writeln(' QrCodesSkiaConsole — geração headless e multi-threaded de QR Codes');
    Writeln('====================================================================');
    Writeln('Output: ', OUT_DIR);

    EnsureOutputDir;

    Demo_GenerateStream;
    Demo_GenerateFile;
    Demo_GenerateWithLogo;
    Demo_Multithreaded;
    Demo_InterfaceUsage;

    Writeln('');
    Writeln('Concluído com sucesso. Prima ENTER para sair.');
    Readln;
  except
    on E: Exception do
    begin
      Writeln('');
      Writeln('ERRO: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
      Writeln('Prima ENTER para sair.');
      Readln;
    end;
  end;
end.
