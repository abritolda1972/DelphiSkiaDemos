unit QRCodeService;

{ ===========================================================================
  QRCodeService — API headless para geração de QR Codes.

  Desenho orientado por SOLID:

    · SRP  — TQRCodeService tem uma única responsabilidade: orquestrar a
             geração do PNG a partir de um pedido (TQRCodeRequest). Toda a
             parametrização visual vive no record do pedido.

    · OCP  — Novos tipos de "pedido pré-configurado" adicionam-se por
             factories / helpers sem alterar o serviço.

    · LSP  — IQRCodeService expõe o contrato mínimo; a implementação
             concreta pode ser substituída em testes (fakes/mocks).

    · ISP  — A interface tem apenas os 2 métodos que o consumidor precisa.

    · DIP  — Zero dependências de Vcl/UI ou Winapi. Só System.*,
             System.Skia e QRRenderer. Consumers dependem da interface.

  Thread-safety:
    · Todos os métodos são stateless. Cada chamada cria a sua própria
      superfície Skia e a sua própria instância de TDelphiZXingQRCode.
    · Seguro para uso concorrente (webservices, workers, TParallel.For).

  Portabilidade:
    · 100% cross-platform (Windows, Linux, macOS, iOS, Android).
    · Zero dependências de Vcl / Fmx / Winapi / Posix — só System.*,
      System.Skia e QRRenderer (também neutro).
    · Usável tanto em aplicações VCL como FMX ou consolas headless.
  =========================================================================== }

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  QRRenderer;

type
  { Pedido imutável — value object seguro para passar entre threads.
    Usa um padrão "with" fluído para configuração: cada With* devolve
    uma cópia modificada, sem alterar o original. }
  TQRCodeRequest = record
    Text:             string;
    ExportSize:       Integer;     // lado do PNG em píxeis (64..4096)
    Options:          TQROptions;
    LogoBytes:        TBytes;      // vazio = sem logo
    LogoSizeFraction: Single;      // 0..0.30 (cap aplicado pelo renderer)

    { Factory: pedido com defaults sensatos — ponto de partida habitual. }
    class function New(const AText: string): TQRCodeRequest; static;

    { Builder fluído — cada chamada devolve uma cópia modificada. }
    function WithExportSize(ASize: Integer): TQRCodeRequest;
    function WithLogoFromBytes(const ABytes: TBytes;
      ASizeFraction: Single = 0.20): TQRCodeRequest;
    function WithLogoFromFile(const APath: string;
      ASizeFraction: Single = 0.20): TQRCodeRequest;
    function WithDataColor(AColor: TAlphaColor): TQRCodeRequest;
    function WithGradientDataColor(AColor1, AColor2: TAlphaColor): TQRCodeRequest;
    function WithFinderColor(AColor: TAlphaColor): TQRCodeRequest;
    function WithBgColor(AColor: TAlphaColor): TQRCodeRequest;
    function WithShapes(AData, AFinderOuter, AFinderInner: TQRShape): TQRCodeRequest;
  end;

  { Contrato mínimo de geração — adequado para DI e para mocks em testes. }
  IQRCodeService = interface
    ['{8F1B2C34-4E5D-6F78-9A0B-1C2D3E4F5061}']
    function  GenerateQrCode(const ARequest: TQRCodeRequest): TStream; overload;
    procedure GenerateQrCode(const ARequest: TQRCodeRequest;
      const APath: string); overload;
  end;

  { Implementação concreta. Stateless — pode ser criada e libertada à
    vontade, ou usada como classe (métodos de classe para callers que
    não querem lidar com instância). }
  TQRCodeService = class(TInterfacedObject, IQRCodeService)
  public
    { Via instância (para dependency injection / testes) }
    function  GenerateQrCode(const ARequest: TQRCodeRequest): TStream; overload;
    procedure GenerateQrCode(const ARequest: TQRCodeRequest;
      const APath: string); overload;

    { Atalhos estáticos — sem necessidade de instanciar }
    class function  Generate(const ARequest: TQRCodeRequest): TStream; overload; static;
    class procedure Generate(const ARequest: TQRCodeRequest;
      const APath: string); overload; static;

    { Factory do contrato — facilita DI: MyVar := TQRCodeService.CreateService; }
    class function CreateService: IQRCodeService; static;
  end;

implementation

uses
  System.IOUtils;

// ---------------------------------------------------------------------------
// Helpers internos — construção de TQRColor sem ter de expor o record
// ---------------------------------------------------------------------------

function SolidColor(AColor: TAlphaColor): TQRColor; inline;
begin
  Result.Mode   := qrcSolid;
  Result.Color1 := AColor;
  Result.Color2 := AColor;
end;

function GradientColor(AColor1, AColor2: TAlphaColor): TQRColor; inline;
begin
  Result.Mode   := qrcGradient;
  Result.Color1 := AColor1;
  Result.Color2 := AColor2;
end;

// ---------------------------------------------------------------------------
// TQRCodeRequest
// ---------------------------------------------------------------------------

class function TQRCodeRequest.New(const AText: string): TQRCodeRequest;
begin
  Result.Text       := AText;
  Result.ExportSize := 1200;

  Result.Options.DataShape := qrsCircle;
  Result.Options.DataColor := SolidColor(TAlphaColors.Black);
  Result.Options.BgColor   := TAlphaColors.White;

  Result.Options.FinderStyle.OuterShape := qrsCircle;
  Result.Options.FinderStyle.InnerShape := qrsSquare;
  Result.Options.FinderStyle.Color      := SolidColor(TAlphaColors.Black);

  Result.LogoBytes        := nil;
  Result.LogoSizeFraction := 0.0;
end;

function TQRCodeRequest.WithExportSize(ASize: Integer): TQRCodeRequest;
begin
  Result := Self;
  Result.ExportSize := ASize;
end;

function TQRCodeRequest.WithLogoFromBytes(const ABytes: TBytes;
  ASizeFraction: Single): TQRCodeRequest;
begin
  Result := Self;
  Result.LogoBytes        := ABytes;
  Result.LogoSizeFraction := ASizeFraction;
end;

function TQRCodeRequest.WithLogoFromFile(const APath: string;
  ASizeFraction: Single): TQRCodeRequest;
begin
  Result := Self;
  if (APath <> '') and TFile.Exists(APath) then
  begin
    Result.LogoBytes        := TFile.ReadAllBytes(APath);
    Result.LogoSizeFraction := ASizeFraction;
  end
  else
  begin
    Result.LogoBytes        := nil;
    Result.LogoSizeFraction := 0.0;
  end;
end;

function TQRCodeRequest.WithDataColor(AColor: TAlphaColor): TQRCodeRequest;
begin
  Result := Self;
  Result.Options.DataColor := SolidColor(AColor);
end;

function TQRCodeRequest.WithGradientDataColor(AColor1,
  AColor2: TAlphaColor): TQRCodeRequest;
begin
  Result := Self;
  Result.Options.DataColor := GradientColor(AColor1, AColor2);
end;

function TQRCodeRequest.WithFinderColor(AColor: TAlphaColor): TQRCodeRequest;
begin
  Result := Self;
  Result.Options.FinderStyle.Color := SolidColor(AColor);
end;

function TQRCodeRequest.WithBgColor(AColor: TAlphaColor): TQRCodeRequest;
begin
  Result := Self;
  Result.Options.BgColor := AColor;
end;

function TQRCodeRequest.WithShapes(AData, AFinderOuter,
  AFinderInner: TQRShape): TQRCodeRequest;
begin
  Result := Self;
  Result.Options.DataShape              := AData;
  Result.Options.FinderStyle.OuterShape := AFinderOuter;
  Result.Options.FinderStyle.InnerShape := AFinderInner;
end;

// ---------------------------------------------------------------------------
// TQRCodeService
// ---------------------------------------------------------------------------

function TQRCodeService.GenerateQrCode(
  const ARequest: TQRCodeRequest): TStream;
var
  Logo:     TQRLogoData;
  PngBytes: TBytes;
  Mem:      TMemoryStream;
begin
  // Validação defensiva — falha cedo e claramente
  if Trim(ARequest.Text) = '' then
    raise EArgumentException.Create(
      'QRCodeService: o texto do QR não pode estar vazio.');

  if (ARequest.ExportSize < 64) or (ARequest.ExportSize > 4096) then
    raise EArgumentOutOfRangeException.CreateFmt(
      'QRCodeService: ExportSize (%d) fora do intervalo 64..4096 px.',
      [ARequest.ExportSize]);

  Logo.Bytes        := ARequest.LogoBytes;
  Logo.SizeFraction := ARequest.LogoSizeFraction;

  // Renderização off-screen, totalmente thread-safe
  PngBytes := TQRRenderer.GeneratePNG(
    ARequest.Text, ARequest.Options, Logo, ARequest.ExportSize);

  if Length(PngBytes) = 0 then
    raise Exception.Create('QRCodeService: geração devolveu 0 bytes.');

  Mem := TMemoryStream.Create;
  try
    Mem.WriteBuffer(PngBytes[0], Length(PngBytes));
    Mem.Position := 0;
    Result := Mem;
  except
    Mem.Free;
    raise;
  end;
end;

procedure TQRCodeService.GenerateQrCode(const ARequest: TQRCodeRequest;
  const APath: string);
var
  Stream:   TStream;
  Dir:      string;
  OutFile:  TFileStream;
begin
  if Trim(APath) = '' then
    raise EArgumentException.Create('QRCodeService: APath não pode ser vazio.');

  // Garante que o directório de destino existe
  Dir := TPath.GetDirectoryName(APath);
  if (Dir <> '') and (not TDirectory.Exists(Dir)) then
    TDirectory.CreateDirectory(Dir);

  Stream := GenerateQrCode(ARequest);
  try
    OutFile := TFileStream.Create(APath, fmCreate);
    try
      OutFile.CopyFrom(Stream, 0);
    finally
      OutFile.Free;
    end;
  finally
    Stream.Free;
  end;
end;

class function TQRCodeService.Generate(
  const ARequest: TQRCodeRequest): TStream;
var
  Svc: TQRCodeService;
begin
  Svc := TQRCodeService.Create;
  try
    Result := Svc.GenerateQrCode(ARequest);
  finally
    Svc.Free;
  end;
end;

class procedure TQRCodeService.Generate(const ARequest: TQRCodeRequest;
  const APath: string);
var
  Svc: TQRCodeService;
begin
  Svc := TQRCodeService.Create;
  try
    Svc.GenerateQrCode(ARequest, APath);
  finally
    Svc.Free;
  end;
end;

class function TQRCodeService.CreateService: IQRCodeService;
begin
  Result := TQRCodeService.Create;
end;

end.
