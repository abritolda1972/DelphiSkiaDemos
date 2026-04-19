unit QRRenderer;

{ ===========================================================================
  QRRenderer — Renderização Skia isolada de qualquer controlo visual VCL.
  Pode ser chamada tanto da main thread (PaintBox) como de worker threads.
  =========================================================================== }

interface

uses
  DelphiZXingQRCode,
  System.Classes,
  System.Math,
  System.Skia,
  System.SysUtils,
  System.Types,
  System.UITypes,
  Vcl.Graphics,
  Winapi.Windows;

type
  TQRShape     = (qrsCircle, qrsSquare, qrsDiamond, qrsRoundRect, qrsHeart);
  TQRColorMode = (qrcSolid, qrcGradient);

  TQRColor = record
    Mode:   TQRColorMode;
    Color1: TAlphaColor;
    Color2: TAlphaColor;
  end;

  TQRFinderStyle = record
    OuterShape: TQRShape;
    InnerShape: TQRShape;
    Color:      TQRColor;
  end;

  TQROptions = record
    DataShape:   TQRShape;
    DataColor:   TQRColor;
    FinderStyle: TQRFinderStyle;
    BgColor:     TAlphaColor;
  end;

  { Tudo o que o renderer precisa do logo — bytes PNG/JPEG brutos.
    Thread-safe: passamos bytes imutáveis, cada thread cria o seu ISkImage. }
  TQRLogoData = record
    Bytes:        TBytes;   // vazio = sem logo
    SizeFraction: Single;   // 0.10 .. 0.35
  end;

  { Área reservada ao logo, alinhada à grelha de módulos do QR.
    Tudo o que está dentro desta área não é desenhado como módulo —
    o scanner vê uma zona limpa e o ECC (nível H) recupera os dados. }
  TQRReservedArea = record
    HasReserve:         Boolean;
    StartCol, EndCol:   Integer;   // inclusivo, em coordenadas de módulo
    StartRow, EndRow:   Integer;   // inclusivo
    RectPx:             TRectF;    // rectângulo em pixels, alinhado à grelha
  end;

  TQRRenderer = class
  private
    class function MakeLinearShader(const QRColor: TQRColor;
      const Bounds: TRectF): ISkShader; static;
    class function BuildModulePath(CX, CY, R: Single;
      Shape: TQRShape): ISkPath; static;
    class procedure DrawModuleSk(const ACanvas: ISkCanvas;
      const APaint: ISkPaint; CX, CY, R: Single;
      Shape: TQRShape); static;
    class procedure DrawFinderSk(const ACanvas: ISkCanvas;
      Scale: Single; StartCol, StartRow: Integer;
      const Style: TQRFinderStyle; BgColor: TAlphaColor;
      T: Single); static;
    class function ComputeReservedArea(QRRows: Integer; Scale,
      LogoSizeFraction: Single): TQRReservedArea; static;
    class procedure DrawLogoOnReserved(const ACanvas: ISkCanvas;
      const ALogoImage: ISkImage; const Reserved: TQRReservedArea;
      BgColor: TAlphaColor); static;
  public
    { Renderiza o QR no canvas fornecido. Se ALogoImage <> nil e
      LogoSizeFraction > 0, reserva um quadrado central alinhado à grelha
      (os módulos nessa zona não são desenhados) e desenha o logo por cima.
      Pode ser chamado de qualquer thread — não toca em componentes VCL. }
    class procedure RenderQR(const ACanvas: ISkCanvas; ASize: Single;
      const Opts: TQROptions; const ATexto: string;
      const ALogoImage: ISkImage = nil;
      LogoSizeFraction: Single = 0); static;

    { Gera a imagem completa (QR + logo) off-screen e devolve PNG em bytes.
      Thread-safe. Lança excepção em caso de erro. }
    class function GeneratePNG(const ATexto: string; const Opts: TQROptions;
      const Logo: TQRLogoData; ExportSize: Integer = 1200): TBytes; static;
  end;

{ Utilitários de cor — expostos para Unit1 não ter de redefini-los }
function VclColorToAlpha(C: TColor): TAlphaColor;

implementation

// ---------------------------------------------------------------------------
// Utilitários de cor
// ---------------------------------------------------------------------------

function VclColorToAlpha(C: TColor): TAlphaColor;
var
  CRGB: COLORREF;
begin
  CRGB   := ColorToRGB(C);
  Result := $FF000000
           or (TAlphaColor(GetRValue(CRGB)) shl 16)
           or (TAlphaColor(GetGValue(CRGB)) shl  8)
           or  TAlphaColor(GetBValue(CRGB));
end;

function LerpColor(C1, C2: TAlphaColor; T: Single): TAlphaColor;

  function Ch(Shift: Byte): TAlphaColor;
  begin
    Result := Round(
      ((C1 shr Shift) and $FF) * (1.0 - T) +
      ((C2 shr Shift) and $FF) * T);
  end;

begin
  Result := (Ch(24) shl 24)
          or (Ch(16) shl 16)
          or (Ch( 8) shl  8)
          or  Ch( 0);
end;

// ---------------------------------------------------------------------------
// TQRRenderer — métodos privados
// ---------------------------------------------------------------------------

class function TQRRenderer.MakeLinearShader(const QRColor: TQRColor;
  const Bounds: TRectF): ISkShader;
var
  Colors: TArray<TAlphaColor>;
begin
  if QRColor.Mode = qrcSolid then
  begin
    Result := nil;
    Exit;
  end;
  Colors := [QRColor.Color1, QRColor.Color2];
  Result := TSkShader.MakeGradientLinear(
    TPointF.Create(Bounds.Left,  Bounds.Top),
    TPointF.Create(Bounds.Right, Bounds.Bottom),
    Colors,
    nil,
    TSkTileMode.Clamp);
end;

class function TQRRenderer.BuildModulePath(CX, CY, R: Single;
  Shape: TQRShape): ISkPath;
var
  B:        ISkPathBuilder;
  RR:       ISkRoundRect;
  Radii:    TSkRoundRectRadii;
  RadiusXY: TPointF;
begin
  B := TSkPathBuilder.Create;
  case Shape of

    qrsCircle:
      B.AddCircle(CX, CY, R);

    qrsSquare:
      B.AddRect(TRectF.Create(CX - R, CY - R, CX + R, CY + R));

    qrsDiamond:
    begin
      B.MoveTo(CX,     CY - R);
      B.LineTo(CX + R, CY);
      B.LineTo(CX,     CY + R);
      B.LineTo(CX - R, CY);
      B.Close;
    end;

    qrsRoundRect:
    begin
      RR       := TSkRoundRect.Create;
      RadiusXY := TPointF.Create(R * 0.35, R * 0.35);
      Radii[TSkRoundRectCorner.UpperLeft]  := RadiusXY;
      Radii[TSkRoundRectCorner.UpperRight] := RadiusXY;
      Radii[TSkRoundRectCorner.LowerRight] := RadiusXY;
      Radii[TSkRoundRectCorner.LowerLeft]  := RadiusXY;
      RR.SetRect(TRectF.Create(CX - R, CY - R, CX + R, CY + R), Radii);
      B.AddRoundRect(RR);
    end;

    qrsHeart:
    begin
      B.MoveTo(CX, CY + R);
      B.CubicTo(
        CX - R * 1.2, CY + R * 0.4,
        CX - R * 1.5, CY - R * 0.3,
        CX,           CY - R * 0.2);
      B.CubicTo(
        CX + R * 1.5, CY - R * 0.3,
        CX + R * 1.2, CY + R * 0.4,
        CX,           CY + R);
      B.Close;
    end;

  end;
  Result := B.Detach;
end;

class procedure TQRRenderer.DrawModuleSk(const ACanvas: ISkCanvas;
  const APaint: ISkPaint; CX, CY, R: Single; Shape: TQRShape);
begin
  ACanvas.DrawPath(BuildModulePath(CX, CY, R, Shape), APaint);
end;

class procedure TQRRenderer.DrawFinderSk(const ACanvas: ISkCanvas;
  Scale: Single; StartCol, StartRow: Integer;
  const Style: TQRFinderStyle; BgColor: TAlphaColor; T: Single);
var
  X, Y, S7, S5, S3, CX, CY: Single;
  FColor: TAlphaColor;
  Paint:  ISkPaint;
begin
  X  := StartCol * Scale;
  Y  := StartRow * Scale;
  S7 := Scale * 7;
  S5 := Scale * 5;
  S3 := Scale * 3;
  CX := X + S7 / 2;
  CY := Y + S7 / 2;

  if Style.Color.Mode = qrcGradient then
    FColor := LerpColor(Style.Color.Color1, Style.Color.Color2, T)
  else
    FColor := Style.Color.Color1;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style     := TSkPaintStyle.Fill;

  Paint.Color := BgColor;
  ACanvas.DrawRect(TRectF.Create(X, Y, X + S7, Y + S7), Paint);

  Paint.Color := FColor;
  DrawModuleSk(ACanvas, Paint, CX, CY, S7 / 2, Style.OuterShape);

  Paint.Color := BgColor;
  DrawModuleSk(ACanvas, Paint, CX, CY, S5 / 2, Style.OuterShape);

  Paint.Color := FColor;
  DrawModuleSk(ACanvas, Paint, CX, CY, S3 / 2, Style.InnerShape);
end;

// ---------------------------------------------------------------------------
// TQRRenderer — métodos públicos
// ---------------------------------------------------------------------------

class function TQRRenderer.ComputeReservedArea(QRRows: Integer; Scale,
  LogoSizeFraction: Single): TQRReservedArea;
const
  { Cap de segurança: mesmo com ECC nível H (~30%) o logo nunca deve
    ocupar mais de ~25% da área total de módulos, senão o ECC não
    consegue sempre reconstruir. Fracção 0.25 = 6.25% de área ≈ seguro. }
  MAX_SAFE_FRACTION = 0.30;
var
  SafeFraction:      Single;
  DesiredPx:         Single;
  DesiredModules:    Integer;
  Span:              Integer;
  CenterMod:         Integer;
  Half:              Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (LogoSizeFraction <= 0) or (QRRows <= 0) then
    Exit;

  // Clamp ao máximo seguro — logo maior do que isto parte o QR mesmo com ECC-H
  SafeFraction := LogoSizeFraction;
  if SafeFraction > MAX_SAFE_FRACTION then
    SafeFraction := MAX_SAFE_FRACTION;

  // Tamanho desejado do logo em pixels (inclui margem de ~1 módulo de cada lado)
  DesiredPx      := (QRRows * Scale) * SafeFraction;
  DesiredModules := Ceil(DesiredPx / Scale) + 2;   // +1 módulo de padding de cada lado

  // Força número ímpar para centrar perfeitamente num módulo central
  if (DesiredModules mod 2) = 0 then
    Inc(DesiredModules);

  // QR matrices (com QZ=2) têm sempre número ímpar de linhas/colunas — o centro é um módulo
  CenterMod := QRRows div 2;
  Half      := DesiredModules div 2;

  Span := DesiredModules;

  Result.HasReserve := True;
  Result.StartCol   := CenterMod - Half;
  Result.EndCol     := CenterMod + Half;
  Result.StartRow   := CenterMod - Half;
  Result.EndRow     := CenterMod + Half;

  // Rectângulo em pixels — alinhado exactamente à grelha
  Result.RectPx := TRectF.Create(
    Result.StartCol * Scale,
    Result.StartRow * Scale,
    (Result.StartCol + Span) * Scale,
    (Result.StartRow + Span) * Scale);
end;

class procedure TQRRenderer.DrawLogoOnReserved(const ACanvas: ISkCanvas;
  const ALogoImage: ISkImage; const Reserved: TQRReservedArea;
  BgColor: TAlphaColor);
var
  BgPaint, ImgPaint:  ISkPaint;
  BgRR:               ISkRoundRect;
  BgRadii:            TSkRoundRectRadii;
  BgRadius:           TPointF;
  Radius:             Single;
  LogoRect, SrcRect:  TRectF;
  Inset:              Single;
  ImgAspect:          Single;
  BoxW, BoxH:         Single;
  DrawW, DrawH:       Single;
  CX, CY:             Single;
begin
  if (not Reserved.HasReserve) or (not Assigned(ALogoImage)) then
    Exit;

  // Fundo alinhado à grelha — o scanner vê uma zona limpa, perfeitamente quadrada.
  // Cantos ligeiramente arredondados (estético, sem comprometer a grelha).
  BgPaint := TSkPaint.Create;
  BgPaint.AntiAlias := True;
  BgPaint.Style     := TSkPaintStyle.Fill;
  BgPaint.Color     := BgColor;

  Radius := Reserved.RectPx.Width * 0.08;
  BgRadius := TPointF.Create(Radius, Radius);
  BgRR     := TSkRoundRect.Create;
  BgRadii[TSkRoundRectCorner.UpperLeft]  := BgRadius;
  BgRadii[TSkRoundRectCorner.UpperRight] := BgRadius;
  BgRadii[TSkRoundRectCorner.LowerRight] := BgRadius;
  BgRadii[TSkRoundRectCorner.LowerLeft]  := BgRadius;
  BgRR.SetRect(Reserved.RectPx, BgRadii);
  ACanvas.DrawRoundRect(BgRR, BgPaint);

  // Caixa quadrada interior (com pequena margem visual relativa ao fundo)
  Inset := Reserved.RectPx.Width * 0.10;
  LogoRect := TRectF.Create(
    Reserved.RectPx.Left   + Inset,
    Reserved.RectPx.Top    + Inset,
    Reserved.RectPx.Right  - Inset,
    Reserved.RectPx.Bottom - Inset);

  // Preserva o rácio de aspecto do logo original (modo "fit" / letterbox).
  // Assim imagens não-quadradas não ficam distorcidas — as faixas
  // laterais são preenchidas pelo fundo da área reservada (BgColor).
  if (ALogoImage.Width > 0) and (ALogoImage.Height > 0) then
  begin
    ImgAspect := ALogoImage.Width / ALogoImage.Height;
    BoxW      := LogoRect.Width;
    BoxH      := LogoRect.Height;

    if ImgAspect >= 1.0 then
    begin
      // Imagem mais larga do que alta — limitada pela largura
      DrawW := BoxW;
      DrawH := BoxW / ImgAspect;
    end
    else
    begin
      // Imagem mais alta do que larga — limitada pela altura
      DrawH := BoxH;
      DrawW := BoxH * ImgAspect;
    end;

    CX := (LogoRect.Left + LogoRect.Right)  * 0.5;
    CY := (LogoRect.Top  + LogoRect.Bottom) * 0.5;

    LogoRect := TRectF.Create(
      CX - DrawW * 0.5,
      CY - DrawH * 0.5,
      CX + DrawW * 0.5,
      CY + DrawH * 0.5);
  end;

  SrcRect  := TRectF.Create(0, 0, ALogoImage.Width, ALogoImage.Height);
  ImgPaint := TSkPaint.Create;
  ImgPaint.AntiAlias := True;

  ACanvas.DrawImageRect(
    ALogoImage,
    SrcRect,
    LogoRect,
    TSkSamplingOptions.Create(TSkFilterMode.Linear, TSkMipmapMode.Linear),
    ImgPaint);
end;

class procedure TQRRenderer.RenderQR(const ACanvas: ISkCanvas; ASize: Single;
  const Opts: TQROptions; const ATexto: string;
  const ALogoImage: ISkImage; LogoSizeFraction: Single);
var
  QRCode:   TDelphiZXingQRCode;
  Row, Col, QZ: Integer;
  Scale:    Single;
  CX, CY, R, T: Single;
  Paint:    ISkPaint;
  Shader:   ISkShader;
  Bounds:   TRectF;
  Reserved: TQRReservedArea;
begin
  QRCode := TDelphiZXingQRCode.Create;
  try
    QRCode.Data                 := ATexto;
    QRCode.Encoding             := qrAuto;
    QRCode.QuietZone            := 2;
    QRCode.ErrorCorrectionLevel := 3; // H — permite recuperar ~30% de módulos

    QZ     := QRCode.QuietZone;
    Scale  := ASize / QRCode.Rows;
    Bounds := TRectF.Create(0, 0, ASize, ASize);

    // Área reservada ao logo, alinhada à grelha de módulos. Módulos dentro
    // desta área NÃO são desenhados — o scanner vê uma zona limpa e o
    // ECC reconstrói os dados em falta.
    if Assigned(ALogoImage) and (LogoSizeFraction > 0) then
      Reserved := ComputeReservedArea(QRCode.Rows, Scale, LogoSizeFraction)
    else
      FillChar(Reserved, SizeOf(Reserved), 0);

    Paint := TSkPaint.Create;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := Opts.BgColor;
    ACanvas.DrawRect(Bounds, Paint);

    Shader := MakeLinearShader(Opts.DataColor, Bounds);

    for Row := 0 to QRCode.Rows - 1 do
      for Col := 0 to QRCode.Columns - 1 do
      begin
        // Salta finder patterns (redesenhados em seguida com o estilo escolhido)
        if ((Row >= QZ) and (Row <= QZ + 7) and
            (Col >= QZ) and (Col <= QZ + 7)) or
           ((Row >= QZ) and (Row <= QZ + 7) and
            (Col >= QRCode.Columns - QZ - 8) and
            (Col <= QRCode.Columns - QZ - 1)) or
           ((Row >= QRCode.Rows - QZ - 8) and
            (Row <= QRCode.Rows - QZ - 1) and
            (Col >= QZ) and (Col <= QZ + 7)) then
          Continue;

        // Salta módulos dentro da zona reservada ao logo — ficam a BgColor.
        // O ECC (nível H) reconstrói os dados perdidos na leitura.
        if Reserved.HasReserve and
           (Row >= Reserved.StartRow) and (Row <= Reserved.EndRow) and
           (Col >= Reserved.StartCol) and (Col <= Reserved.EndCol) then
          Continue;

        if not QRCode.IsBlack[Row, Col] then
          Continue;

        T  := (Row + Col) / (QRCode.Rows + QRCode.Columns - 2);
        CX := Col * Scale + Scale * 0.5;
        CY := Row * Scale + Scale * 0.5;
        R  := Scale * 0.45;

        Paint := TSkPaint.Create;
        Paint.AntiAlias := True;
        Paint.Style     := TSkPaintStyle.Fill;

        if Assigned(Shader) then
          Paint.Shader := Shader
        else
          Paint.Color := Opts.DataColor.Color1;

        DrawModuleSk(ACanvas, Paint, CX, CY, R, Opts.DataShape);
      end;

    DrawFinderSk(ACanvas, Scale,
      QZ, QZ, Opts.FinderStyle, Opts.BgColor, 0.0);
    DrawFinderSk(ACanvas, Scale,
      QRCode.Columns - QZ - 7, QZ, Opts.FinderStyle, Opts.BgColor, 0.5);
    DrawFinderSk(ACanvas, Scale,
      QZ, QRCode.Rows - QZ - 7, Opts.FinderStyle, Opts.BgColor, 1.0);

    // Desenha o logo por cima da zona já reservada (sem módulos por baixo).
    if Reserved.HasReserve then
      DrawLogoOnReserved(ACanvas, ALogoImage, Reserved, Opts.BgColor);

  finally
    QRCode.Free;
  end;
end;

class function TQRRenderer.GeneratePNG(const ATexto: string;
  const Opts: TQROptions; const Logo: TQRLogoData;
  ExportSize: Integer): TBytes;
var
  Surface:   ISkSurface;
  Image:     ISkImage;
  LogoImage: ISkImage;
  Data:      TArray<Byte>;
begin
  Surface := TSkSurface.MakeRaster(ExportSize, ExportSize);
  if not Assigned(Surface) then
    raise Exception.Create('Não foi possível criar a superfície Skia.');

  Surface.Canvas.Clear(TAlphaColors.Null);

  // Constrói ISkImage do logo a partir dos bytes (thread-safe)
  LogoImage := nil;
  if Length(Logo.Bytes) > 0 then
    LogoImage := TSkImage.MakeFromEncoded(Logo.Bytes);

  RenderQR(Surface.Canvas, ExportSize, Opts, ATexto,
    LogoImage, Logo.SizeFraction);

  Image := Surface.MakeImageSnapshot;
  if not Assigned(Image) then
    raise Exception.Create('Falha ao obter snapshot da imagem.');

  Data   := Image.Encode(TSkEncodedImageFormat.Png, 100);
  Result := Data;
end;

end.
