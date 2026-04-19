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
  public
    { Renderiza o QR (sem logo) no canvas fornecido.
      Pode ser chamado de qualquer thread — não toca em componentes VCL. }
    class procedure RenderQR(const ACanvas: ISkCanvas; ASize: Single;
      const Opts: TQROptions; const ATexto: string); static;

    { Desenha o logo centrado. LogoImage pode ser nil. }
    class procedure DrawLogo(const ACanvas: ISkCanvas; AQRSize: Single;
      const ALogoImage: ISkImage; SizeFraction: Single;
      BgColor: TAlphaColor); static;

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

class procedure TQRRenderer.RenderQR(const ACanvas: ISkCanvas; ASize: Single;
  const Opts: TQROptions; const ATexto: string);
var
  QRCode:  TDelphiZXingQRCode;
  Row, Col, QZ: Integer;
  Scale:   Single;
  CX, CY, R, T: Single;
  Paint:   ISkPaint;
  Shader:  ISkShader;
  Bounds:  TRectF;
begin
  QRCode := TDelphiZXingQRCode.Create;
  try
    QRCode.Data                 := ATexto;
    QRCode.Encoding             := qrAuto;
    QRCode.QuietZone            := 2;
    QRCode.ErrorCorrectionLevel := 3; // H

    QZ     := QRCode.QuietZone;
    Scale  := ASize / QRCode.Rows;
    Bounds := TRectF.Create(0, 0, ASize, ASize);

    Paint := TSkPaint.Create;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := Opts.BgColor;
    ACanvas.DrawRect(Bounds, Paint);

    Shader := MakeLinearShader(Opts.DataColor, Bounds);

    for Row := 0 to QRCode.Rows - 1 do
      for Col := 0 to QRCode.Columns - 1 do
      begin
        if ((Row >= QZ) and (Row <= QZ + 7) and
            (Col >= QZ) and (Col <= QZ + 7)) or
           ((Row >= QZ) and (Row <= QZ + 7) and
            (Col >= QRCode.Columns - QZ - 8) and
            (Col <= QRCode.Columns - QZ - 1)) or
           ((Row >= QRCode.Rows - QZ - 8) and
            (Row <= QRCode.Rows - QZ - 1) and
            (Col >= QZ) and (Col <= QZ + 7)) then
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

  finally
    QRCode.Free;
  end;
end;

class procedure TQRRenderer.DrawLogo(const ACanvas: ISkCanvas;
  AQRSize: Single; const ALogoImage: ISkImage; SizeFraction: Single;
  BgColor: TAlphaColor);
var
  LogoSize:         Single;
  LogoRect:         TRectF;
  CenterX, CenterY: Single;
  BgPaint:          ISkPaint;
  BgPad:            Single;
  BgRect:           TRectF;
  BgRR:             ISkRoundRect;
  BgRadii:          TSkRoundRectRadii;
  BgRadius:         TPointF;
  ImgPaint:         ISkPaint;
  SrcRect:          TRectF;
begin
  if not Assigned(ALogoImage) then Exit;

  LogoSize := AQRSize * SizeFraction;
  CenterX  := AQRSize / 2;
  CenterY  := AQRSize / 2;

  LogoRect := TRectF.Create(
    CenterX - LogoSize / 2,
    CenterY - LogoSize / 2,
    CenterX + LogoSize / 2,
    CenterY + LogoSize / 2);

  BgPad  := LogoSize * 0.06;
  BgRect := TRectF.Create(
    LogoRect.Left   - BgPad,
    LogoRect.Top    - BgPad,
    LogoRect.Right  + BgPad,
    LogoRect.Bottom + BgPad);

  BgPaint := TSkPaint.Create;
  BgPaint.AntiAlias := True;
  BgPaint.Style     := TSkPaintStyle.Fill;
  BgPaint.Color     := BgColor;

  BgRadius := TPointF.Create(BgPad * 2, BgPad * 2);
  BgRR     := TSkRoundRect.Create;
  BgRadii[TSkRoundRectCorner.UpperLeft]  := BgRadius;
  BgRadii[TSkRoundRectCorner.UpperRight] := BgRadius;
  BgRadii[TSkRoundRectCorner.LowerRight] := BgRadius;
  BgRadii[TSkRoundRectCorner.LowerLeft]  := BgRadius;
  BgRR.SetRect(BgRect, BgRadii);
  ACanvas.DrawRoundRect(BgRR, BgPaint);

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

  RenderQR(Surface.Canvas, ExportSize, Opts, ATexto);

  // Constrói ISkImage do logo a partir dos bytes (thread-safe)
  LogoImage := nil;
  if Length(Logo.Bytes) > 0 then
  begin
    LogoImage := TSkImage.MakeFromEncoded(Logo.Bytes);
    // Se falhar o decode, simplesmente não desenha logo
  end;

  if Assigned(LogoImage) then
    DrawLogo(Surface.Canvas, ExportSize, LogoImage,
      Logo.SizeFraction, Opts.BgColor);

  Image := Surface.MakeImageSnapshot;
  if not Assigned(Image) then
    raise Exception.Create('Falha ao obter snapshot da imagem.');

  Data   := Image.Encode(TSkEncodedImageFormat.Png, 100);
  Result := Data;
end;

end.
