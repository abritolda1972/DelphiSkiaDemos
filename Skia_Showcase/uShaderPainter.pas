unit uShaderPainter;

{
  uShaderPainter.pas
  ==================
  Skia4Delphi Showcase - Procedural Shader-Style Painter
  Implements GPU-like procedural effects using Skia's
  paint, shader, and filter pipeline.

}

interface

uses
  System.SysUtils,
  System.Math,
  System.UITypes,
  Skia,
  uThemeEngine,
  uAnimationEngine, System.Types;

type
  TShaderPainter = class
  public
    // Background effects
    class procedure DrawGridBackground(const ACanvas: ISkCanvas;
      const ABounds: TRectF; const ATime: Single);

    class procedure DrawHexagonalGrid(const ACanvas: ISkCanvas;
      const ABounds: TRectF; const ATime: Single);

    class procedure DrawWaveform(const ACanvas: ISkCanvas;
      const ABounds: TRectF; const ATime: Single;
      const AColor: TAlphaColor; const AAmplitude: Single = 30);

    class procedure DrawSpiral(const ACanvas: ISkCanvas;
      const ACenterX, ACenterY: Single; const ATime: Single;
      const ARadius: Single; const AColor: TAlphaColor);

    class procedure DrawLissajous(const ACanvas: ISkCanvas;
      const ACenterX, ACenterY: Single; const ATime: Single;
      const AWidth, AHeight: Single; const AColor: TAlphaColor);

    class procedure DrawMandalaRing(const ACanvas: ISkCanvas;
      const ACenterX, ACenterY: Single; const ARadius: Single;
      const APetals: Integer; const ATime: Single; const AColor: TAlphaColor);

    class procedure DrawNeonText(const ACanvas: ISkCanvas;
      const AText: string; const AX, AY: Single; const ASize: Single;
      const AColor: TAlphaColor; const AGlowRadius: Single = 15);

    class procedure DrawGlassPanel(const ACanvas: ISkCanvas;
      const ABounds: TRectF; const ACornerRadius: Single = 16);

    class procedure DrawNeonBorder(const ACanvas: ISkCanvas;
      const ABounds: TRectF; const ACornerRadius: Single;
      const AColor: TAlphaColor; const ATime: Single = 0);

    class procedure DrawProgressRing(const ACanvas: ISkCanvas;
      const ACenterX, ACenterY: Single; const ARadius: Single;
      const AProgress: Single; const AColor: TAlphaColor;
      const AWidth: Single = 4; const ALabel: string = '');

    class procedure DrawAudioSpectrum(const ACanvas: ISkCanvas;
      const ABounds: TRectF; const ATime: Single; const AColor: TAlphaColor);

    class procedure DrawPlasmaBall(const ACanvas: ISkCanvas;
      const ACenterX, ACenterY: Single; const ARadius: Single;
      const ATime: Single);

    class procedure DrawCodeRain(const ACanvas: ISkCanvas;
      const ABounds: TRectF; const ATime: Single);

    class procedure DrawStar(const ACanvas: ISkCanvas;
      const ACenterX, ACenterY: Single; const ARadius: Single;
      const APoints: Integer; const AInnerRadius: Single;
      const AAngle: Single; const AColor: TAlphaColor);

    class procedure DrawGeometricPattern(const ACanvas: ISkCanvas;
      const ABounds: TRectF; const ATime: Single);
  end;

implementation

{ TShaderPainter }

function ToAlphaColor(R, G, B, A: Byte): TAlphaColor;
  var
    LRec: TAlphaColorRec;
  begin
    LRec.R := R;
    LRec.G := G;
    LRec.B := B;
    LRec.A := A;
    Result := TAlphaColor(LRec);
  end;

class procedure TShaderPainter.DrawGridBackground(const ACanvas: ISkCanvas;
  const ABounds: TRectF; const ATime: Single);
var
  LPaint: ISkPaint;
  X, Y, Alpha: Single;
  GridSize: Single;

  // Helper para criar a cor sem erro de compilação


begin
  GridSize := 40;

  // Criamos o Paint uma única vez
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 0.5;

  // Linhas Verticais
  X := ABounds.Left;
  while X <= ABounds.Right do
  begin
    Alpha := 0.3 + 0.15 * Sin(X * 0.02 + ATime * 0.5);
    // Correção da cor:
    LPaint.Color := ToAlphaColor(0, 180, 255, Round(Alpha * 60));

    // Otimização: DrawLine é muito mais rápido que PathBuilder para grades
    ACanvas.DrawLine(X, ABounds.Top, X, ABounds.Bottom, LPaint);

    X := X + GridSize;
  end;

  // Linhas Horizontais
  Y := ABounds.Top;
  while Y <= ABounds.Bottom do
  begin
    Alpha := 0.3 + 0.15 * Sin(Y * 0.02 + ATime * 0.3);
    // Correção da cor:
    LPaint.Color := ToAlphaColor(0, 180, 255, Round(Alpha * 60));

    ACanvas.DrawLine(ABounds.Left, Y, ABounds.Right, Y, LPaint);

    Y := Y + GridSize;
  end;
end;

class procedure TShaderPainter.DrawHexagonalGrid(const ACanvas: ISkCanvas;
  const ABounds: TRectF; const ATime: Single);
var
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  I, J, K: Integer; // K será nosso contador ordinal para o hexágono
  CX, CY, R: Single;
  PX, PY, Alpha: Single;
  ColCount, RowCount: Integer;
  LRotation: Single;
begin
  R := 28;
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 0.8;

  // Criamos o PathBuilder UMA VEZ fora dos loops
  LPathBuilder := TSkPathBuilder.Create;

  ColCount := Round(ABounds.Width / (R * 1.73)) + 2;
  RowCount := Round(ABounds.Height / (R * 1.5)) + 2;

  // Cache da rotação para evitar recalcular o tempo no loop interno
  LRotation := ATime * 0.05;

  for J := 0 to RowCount do
  begin
    for I := 0 to ColCount do
    begin
      CX := ABounds.Left + I * R * 1.732;
      CY := ABounds.Top  + J * R * 1.5;
      if J mod 2 = 1 then
        CX := CX + R * 0.866;

      Alpha := 0.2 + 0.15 * Sin(CX * 0.01 + CY * 0.008 + ATime * 0.4);
      LPaint.Color := ToAlphaColor(0, 180, 255, Round(Alpha * 50));

      // Construção do Hexágono
      // Nota: Em algumas versões do Skia4Delphi, o Detach limpa o builder automaticamente.
      for K := 0 to 5 do
      begin
        // Convertendo o índice K para radianos manualmente
        PX := CX + R * Cos((K * 60) * Pi / 180 + LRotation);
        PY := CY + R * Sin((K * 60) * Pi / 180 + LRotation);

        if K = 0 then
          LPathBuilder.MoveTo(PX, PY)
        else
          LPathBuilder.LineTo(PX, PY);
      end;

      LPathBuilder.Close;

      // DrawPath consome o path gerado pelo Detach
      ACanvas.DrawPath(LPathBuilder.Detach, LPaint);
    end;
  end;
end;

class procedure TShaderPainter.DrawWaveform(const ACanvas: ISkCanvas;
  const ABounds: TRectF; const ATime: Single;
  const AColor: TAlphaColor; const AAmplitude: Single);
var
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  X, Y, CY, W: Single;
  Step, WaveCount: Integer;
  First: Boolean;
begin
  CY := ABounds.CenterPoint.Y;
  W  := ABounds.Width;
  WaveCount := 5;

  for Step := WaveCount downto 1 do
  begin
    LPaint := TSkPaint.Create;
    LPaint.AntiAlias := True;
    LPaint.Style := TSkPaintStyle.Stroke;
    LPaint.StrokeWidth := 2.5 - Step * 0.3;
    LPaint.StrokeCap := TSkStrokeCap.Round;
    LPaint.Color :=     ToAlphaColor(TAlphaColorRec(AColor).R,
      TAlphaColorRec(AColor).G,
      TAlphaColorRec(AColor).B,
      Round((1 - Step / (WaveCount + 1)) * 200)
    );



    LPathBuilder := TSkPathBuilder.Create;
    First := True;
    X := ABounds.Left;
    while X <= ABounds.Right do
    begin
      Y := CY
        + AAmplitude       * Sin((X / W) * 4  * Pi + ATime * 2 + Step * 0.5)
                           * (1 - Step * 0.1)
        + AAmplitude * 0.4 * Sin((X / W) * 7  * Pi - ATime * 3 + Step * 0.3)
                           * (1 - Step * 0.15)
        + AAmplitude * 0.2 * Sin((X / W) * 13 * Pi + ATime * 1.5)
                           * (1 - Step * 0.1);

      if First then LPathBuilder.MoveTo(X, Y)
      else          LPathBuilder.LineTo(X, Y);
      First := False;
      X := X + 2;
    end;
    ACanvas.DrawPath(LPathBuilder.Detach, LPaint);
  end;
end;

class procedure TShaderPainter.DrawSpiral(const ACanvas: ISkCanvas;
  const ACenterX, ACenterY: Single; const ATime: Single;
  const ARadius: Single; const AColor: TAlphaColor);
var
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  T, Angle, R, PX, PY, Alpha: Single;
  Steps, I: Integer;
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 1.5;
  LPaint.StrokeCap := TSkStrokeCap.Round;

  Steps := 400;
  LPathBuilder := TSkPathBuilder.Create;

  for I := 0 to Steps do
  begin
    T     := I / Steps;
    Angle := T * 8 * Pi + ATime;
    R     := T * ARadius;
    PX    := ACenterX + Cos(Angle) * R;
    PY    := ACenterY + Sin(Angle) * R;
    Alpha := T;

    LPaint.Color := ToAlphaColor(
      TAlphaColorRec(AColor).R,
      TAlphaColorRec(AColor).G,
      TAlphaColorRec(AColor).B,
      Round(Alpha * 200)
    );

    if I = 0 then LPathBuilder.MoveTo(PX, PY)
    else          LPathBuilder.LineTo(PX, PY);
  end;

  ACanvas.DrawPath(LPathBuilder.Detach, LPaint);
end;

class procedure TShaderPainter.DrawLissajous(const ACanvas: ISkCanvas;
  const ACenterX, ACenterY: Single; const ATime: Single;
  const AWidth, AHeight: Single; const AColor: TAlphaColor);
var
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  T, PX, PY, Alpha, Phase: Single;
  FreqX, FreqY: Single;
  Steps, I: Integer;
begin
  FreqX := 3;
  FreqY := 2;
  Phase := ATime * 0.3;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 1.8;
  LPaint.StrokeCap := TSkStrokeCap.Round;

  Steps := 500;
  LPathBuilder := TSkPathBuilder.Create;

  for I := 0 to Steps do
  begin
    T  := (I / Steps) * 2 * Pi;
    PX := ACenterX + Cos(FreqX * T + Phase) * AWidth  * 0.45;
    PY := ACenterY + Sin(FreqY * T)          * AHeight * 0.45;
    Alpha := 0.3 + 0.7 * ((Sin(T) + 1) * 0.5);

    LPaint.Color := ToAlphaColor(
      TAlphaColorRec(AColor).R,
      TAlphaColorRec(AColor).G,
      TAlphaColorRec(AColor).B,
      Round(Alpha * 220)
    );

    if I = 0 then LPathBuilder.MoveTo(PX, PY)
    else          LPathBuilder.LineTo(PX, PY);
  end;

  ACanvas.DrawPath(LPathBuilder.Detach, LPaint);
end;

class procedure TShaderPainter.DrawMandalaRing(const ACanvas: ISkCanvas;
  const ACenterX, ACenterY: Single; const ARadius: Single;
  const APetals: Integer; const ATime: Single; const AColor: TAlphaColor);
var
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  I: Integer;
  BaseAngle: Single;
  InnerR: Single;
  CX, CY, CP1X, CP1Y, CP2X, CP2Y: Single;
begin
  InnerR := ARadius * 0.5;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 1.2;
  LPaint.Color := AColor;

  for I := 0 to APetals - 1 do
  begin
    BaseAngle := (I / APetals) * 2 * Pi + ATime * 0.3;

    LPathBuilder := TSkPathBuilder.Create;
    CX := ACenterX + Cos(BaseAngle) * InnerR;
    CY := ACenterY + Sin(BaseAngle) * InnerR;

    LPathBuilder.MoveTo(ACenterX, ACenterY);

    CP1X := ACenterX + Cos(BaseAngle - Pi / APetals) * ARadius;
    CP1Y := ACenterY + Sin(BaseAngle - Pi / APetals) * ARadius;
    CP2X := ACenterX + Cos(BaseAngle + Pi / APetals) * ARadius;
    CP2Y := ACenterY + Sin(BaseAngle + Pi / APetals) * ARadius;
    LPathBuilder.CubicTo(CP1X, CP1Y, CP2X, CP2Y, ACenterX, ACenterY);

    ACanvas.DrawPath(LPathBuilder.Detach, LPaint);
  end;

  // Inner + outer circles
  LPaint.Color :=ToAlphaColor(
    TAlphaColorRec(AColor).R,
    TAlphaColorRec(AColor).G,
    TAlphaColorRec(AColor).B,
    100
  );
  ACanvas.DrawCircle(ACenterX, ACenterY, InnerR * 0.8, LPaint);
  ACanvas.DrawCircle(ACenterX, ACenterY, ARadius,      LPaint);
end;

class procedure TShaderPainter.DrawNeonText(const ACanvas: ISkCanvas;
  const AText: string; const AX, AY: Single; const ASize: Single;
  const AColor: TAlphaColor; const AGlowRadius: Single);
var
  LFont: ISkFont;
  LPaint: ISkPaint;
  LBlob: ISkTextBlob;

  // Função auxiliar que já criamos anteriormente
  function ChangeAlpha(const AColor: TAlphaColor; const AAlpha: Byte): TAlphaColor;
  var
    LRec: TAlphaColorRec;
  begin
    LRec := TAlphaColorRec(AColor);
    LRec.A := AAlpha;
    Result := TAlphaColor(LRec);
  end;

begin
  LFont := TSkFont.Create(TSkTypeface.MakeDefault, ASize);
  LBlob := TSkTextBlob.MakeFromText(AText, LFont);

  if not Assigned(LBlob) then Exit;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  // 1. Outer glow (Brilho externo)
  LPaint.Color := ChangeAlpha(AColor, 60);
  // CORREÇÃO: Adicionado o 'nil' como 3º parâmetro
  LPaint.ImageFilter := TSkImageFilter.MakeBlur(AGlowRadius, AGlowRadius, nil, TSkTileMode.Decal);
  ACanvas.DrawTextBlob(LBlob, AX, AY, LPaint);

  // 2. Mid glow (Brilho médio)
  LPaint.Color := ChangeAlpha(AColor, 120);
  // CORREÇÃO: Adicionado o 'nil' como 3º parâmetro
  LPaint.ImageFilter := TSkImageFilter.MakeBlur(AGlowRadius * 0.4, AGlowRadius * 0.4, nil, TSkTileMode.Decal);
  ACanvas.DrawTextBlob(LBlob, AX, AY, LPaint);

  // 3. Crisp core (Centro nítido)
  LPaint.Color := AColor;
  LPaint.ImageFilter := nil; // Importante: remover o filtro para o texto central ficar nítido
  ACanvas.DrawTextBlob(LBlob, AX, AY, LPaint);
end;

class procedure TShaderPainter.DrawGlassPanel(const ACanvas: ISkCanvas;
  const ABounds: TRectF; const ACornerRadius: Single);
var
  LPaint: ISkPaint;
  LBorderPaint: ISkPaint;
  LColors: TArray<TAlphaColor>;
  LPos: TArray<Single>;
begin
  // Glass fill — diagonal gradient
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LColors := [$18162840, $0A0A1020];
  LPos    := [0.0, 1.0];
  LPaint.Shader := TSkShader.MakeGradientLinear(
    ABounds.TopLeft, ABounds.BottomRight,
    LColors, LPos, TSkTileMode.Clamp
  );
  ACanvas.DrawRoundRect(ABounds, ACornerRadius, ACornerRadius, LPaint);

  // Inner top-edge shimmer
  LBorderPaint := TSkPaint.Create;
  LBorderPaint.AntiAlias := True;
  LBorderPaint.Style := TSkPaintStyle.Stroke;
  LBorderPaint.StrokeWidth := 1.0;
  LColors := [$4400D4FF, $0000D4FF, $0000D4FF, $2200D4FF];
  LPos    := [0.0, 0.3, 0.7, 1.0];
  LBorderPaint.Shader := TSkShader.MakeGradientLinear(
    ABounds.TopLeft,
    TPointF.Create(ABounds.Right, ABounds.Top),
    LColors, LPos, TSkTileMode.Clamp
  );
  ACanvas.DrawRoundRect(ABounds, ACornerRadius, ACornerRadius, LBorderPaint);
end;

class procedure TShaderPainter.DrawNeonBorder(const ACanvas: ISkCanvas;
  const ABounds: TRectF; const ACornerRadius: Single;
  const AColor: TAlphaColor; const ATime: Single);
var
  LGlowPaint, LCorePaint: ISkPaint;
  LPulse: Single;
begin
  LPulse := 0.7 + 0.3 * Sin(ATime * 3);

  // Outer glow
  LGlowPaint := TSkPaint.Create;
  LGlowPaint.AntiAlias := True;
  LGlowPaint.Style := TSkPaintStyle.Stroke;
  LGlowPaint.StrokeWidth := 8;
  LGlowPaint.Color := ToAlphaColor(
    TAlphaColorRec(AColor).R,
    TAlphaColorRec(AColor).G,
    TAlphaColorRec(AColor).B,
    Round(40 * LPulse)
  );
  LGlowPaint.ImageFilter := TSkImageFilter.MakeBlur(6, 6, nil,TSkTileMode.Decal);
  ACanvas.DrawRoundRect(ABounds, ACornerRadius, ACornerRadius, LGlowPaint);

  // Core border
  LCorePaint := TSkPaint.Create;
  LCorePaint.AntiAlias := True;
  LCorePaint.Style := TSkPaintStyle.Stroke;
  LCorePaint.StrokeWidth := 1.5;
  LCorePaint.Color := ToAlphaColor(
    TAlphaColorRec(AColor).R,
    TAlphaColorRec(AColor).G,
    TAlphaColorRec(AColor).B,
    Round(180 * LPulse)
  );
  ACanvas.DrawRoundRect(ABounds, ACornerRadius, ACornerRadius, LCorePaint);
end;

class procedure TShaderPainter.DrawProgressRing(const ACanvas: ISkCanvas;
  const ACenterX, ACenterY: Single; const ARadius: Single;
  const AProgress: Single; const AColor: TAlphaColor;
  const AWidth: Single; const ALabel: string);
var
  LPaint: ISkPaint;
  LRect: TRectF;
  SweepAngle: Single;
  LFont: ISkFont;
  LTextPaint: ISkPaint;
  TextW: Single;
begin
  LRect := TRectF.Create(
    ACenterX - ARadius, ACenterY - ARadius,
    ACenterX + ARadius, ACenterY + ARadius
  );

  // Track ring
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := AWidth;
  LPaint.StrokeCap := TSkStrokeCap.Round;
  LPaint.Color := ToAlphaColor(
    TAlphaColorRec(AColor).R,
    TAlphaColorRec(AColor).G,
    TAlphaColorRec(AColor).B,
    30
  );
  ACanvas.DrawArc(LRect, 0, 360, False, LPaint);

  // Progress arc with glow
  SweepAngle := AProgress * 360;
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := AWidth;
  LPaint.StrokeCap := TSkStrokeCap.Round;
  LPaint.Color := AColor;
  LPaint.ImageFilter := TSkImageFilter.MakeBlur(3, 3,nil, TSkTileMode.Decal);
  ACanvas.DrawArc(LRect, -90, SweepAngle, False, LPaint);

  // Crisp core arc
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := AWidth;
  LPaint.StrokeCap := TSkStrokeCap.Round;
  LPaint.Color := AColor;
  ACanvas.DrawArc(LRect, -90, SweepAngle, False, LPaint);

  // Label text
  if ALabel <> '' then
  begin
    LFont := TSkFont.Create(TSkTypeface.MakeDefault, ARadius * 0.35);
    LTextPaint := TSkPaint.Create;
    LTextPaint.AntiAlias := True;
    LTextPaint.Color := AColor;
    TextW := LFont. MeasureText(ALabel, LTextPaint);
    ACanvas.DrawSimpleText(ALabel,
      ACenterX - TextW * 0.5, ACenterY + ARadius * 0.15,
      LFont, LTextPaint);
  end;
end;

class procedure TShaderPainter.DrawAudioSpectrum(const ACanvas: ISkCanvas;
  const ABounds: TRectF; const ATime: Single; const AColor: TAlphaColor);
var
  LPaint: ISkPaint;
  I, Bars: Integer;
  BarW, X, H, FreqSim: Single;
begin
  Bars := 32;
  BarW := ABounds.Width / Bars;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Fill;

  for I := 0 to Bars - 1 do
  begin
    X := ABounds.Left + I * BarW;
    FreqSim :=
      0.5 + 0.5 * Sin(I * 0.4 + ATime * 4) +
      0.3 * Sin(I * 0.7 - ATime * 2.5) +
      0.2 * Sin(I * 0.2 + ATime * 6);
    FreqSim := Max(0.05, FreqSim);
    H := FreqSim * ABounds.Height * 0.85;

    LPaint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(X, ABounds.Bottom),
      TPointF.Create(X, ABounds.Bottom - H),
      [ToAlphaColor(
         TAlphaColorRec(AColor).R, TAlphaColorRec(AColor).G,
         TAlphaColorRec(AColor).B, 255),
       ToAlphaColor(
         TAlphaColorRec(AColor).R, TAlphaColorRec(AColor).G,
         TAlphaColorRec(AColor).B, 80)],
      [0.0, 1.0], TSkTileMode.Clamp
    );
    ACanvas.DrawRect(
      TRectF.Create(X + 1, ABounds.Bottom - H, X + BarW - 2, ABounds.Bottom),
      LPaint);
  end;
end;

class procedure TShaderPainter.DrawPlasmaBall(const ACanvas: ISkCanvas;
  const ACenterX, ACenterY: Single; const ARadius: Single;
  const ATime: Single);
var
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  I, Tendrils, Steps, Step: Integer;
  BaseAngle, WobbleAngle, R, T, PX, PY: Single;
  Alpha: Integer;
begin
  // Core radial gradient
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Shader := TSkShader.MakeGradientRadial(
    TPointF.Create(ACenterX, ACenterY), ARadius,
    [$FF00D4FF, $8800AAFF, $440044BB, $00000033],
    [0.0, 0.3, 0.7, 1.0], TSkTileMode.Clamp
  );
  ACanvas.DrawCircle(ACenterX, ACenterY, ARadius, LPaint);

  // Tendrils
  Tendrils := 8;
  Steps    := 60;
  for I := 0 to Tendrils - 1 do
  begin
    LPaint := TSkPaint.Create;
    LPaint.AntiAlias := True;
    LPaint.Style := TSkPaintStyle.Stroke;
    LPaint.StrokeWidth := 1.5;
    LPaint.StrokeCap := TSkStrokeCap.Round;

    Alpha := Round(150 + 80 * Sin(I + ATime * 2));
    LPaint.Color := ToAlphaColor(0, 200, 255, Alpha);
    LPaint.ImageFilter := TSkImageFilter.MakeBlur(3, 3, nil,TSkTileMode.Decal);

    LPathBuilder := TSkPathBuilder.Create;
    LPathBuilder.MoveTo(ACenterX, ACenterY);

    BaseAngle := (I / Tendrils) * 2 * Pi + ATime * 1.2;
    for Step := 1 to Steps do
    begin
      T := Step / Steps;
      WobbleAngle := BaseAngle
        + Sin(T * Pi * 3 + ATime * 2 + I) * 0.4
        + Sin(T * Pi * 7 - ATime * 1.5) * 0.15;
      R  := T * ARadius * 0.92;
      PX := ACenterX + Cos(WobbleAngle) * R;
      PY := ACenterY + Sin(WobbleAngle) * R;
      LPathBuilder.LineTo(PX, PY);
    end;
    ACanvas.DrawPath(LPathBuilder.Detach, LPaint);
  end;

  // Outer rim glow
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 2;
  LPaint.Color := $CC00D4FF;
  LPaint.ImageFilter := TSkImageFilter.MakeBlur(4, 4,nil, TSkTileMode.Decal);
  ACanvas.DrawCircle(ACenterX, ACenterY, ARadius, LPaint);
end;

class procedure TShaderPainter.DrawCodeRain(const ACanvas: ISkCanvas;
  const ABounds: TRectF; const ATime: Single);
{
  Static procedural code-rain overlay.
  Draws multiple vertical columns of animated glyphs that scroll at
  different speeds, simulating a Matrix-style data stream backdrop.
  Complements the particle-based TMatrixScene for extra density.
}
const
  GLYPHS: array[0..35] of Char = (
    '0','1','2','3','4','5','6','7','8','9',
    'A','B','C','D','E','F','G','H','I','J',
    '?','?','?','?','?','?','?','?','?','?',
    '|','=','+','-','<','>'
  );
  COL_W   = 16;
  CHAR_H  = 18;
  TAIL_LEN = 12;   // Number of trailing fading characters
var
  LFont:        ISkFont;
  LBrightPaint: ISkPaint;
  LGlowPaint:   ISkPaint;
  LDimPaint:    ISkPaint;
  ColCount, RowCount: Integer;
  Col, Row: Integer;
  CX, CY: Single;
  GlyphIdx: Integer;
  HeadRow: Integer;
  TailAlpha: Single;
  ColSpeed, ColOffset: Single;
  TotalRows: Integer;
begin
  ColCount  := Round(ABounds.Width  / COL_W)  + 1;
  TotalRows := Round(ABounds.Height / CHAR_H) + TAIL_LEN + 1;

  LFont       := TSkFont.Create(TSkTypeface.MakeDefault, 12);

  for Col := 0 to ColCount - 1 do
  begin
    CX := ABounds.Left + Col * COL_W;

    // Each column has a unique speed and phase offset
    ColSpeed  := 0.4 + (Col mod 7) * 0.18;      // 0.4 .. 1.66
    ColOffset := (Col * 137) mod TotalRows;       // pseudo-random start

    HeadRow := Round(ATime * ColSpeed * (60 / CHAR_H) + ColOffset) mod TotalRows;

    // Draw tail (fading chars above head)
    for Row := 0 to TAIL_LEN - 1 do
    begin
      var TailRowIdx := HeadRow - Row;
      if TailRowIdx < 0 then TailRowIdx := TailRowIdx + TotalRows;

      CY := ABounds.Top + TailRowIdx * CHAR_H;
      if (CY < ABounds.Top - CHAR_H) or (CY > ABounds.Bottom + CHAR_H) then
        Continue;

      TailAlpha := 1.0 - (Row / TAIL_LEN);
      TailAlpha := TailAlpha * TailAlpha;  // quadratic fade

      GlyphIdx := (Col * 13 + TailRowIdx * 7 +
                   Round(ATime * 8 * (1 - Row / TAIL_LEN))) mod Length(GLYPHS);

      if Row = 0 then
      begin
        // Brightest head glyph with glow
        LGlowPaint := TSkPaint.Create;
        LGlowPaint.AntiAlias := True;
        LGlowPaint.Color := ToAlphaColor(160, 255, 200, 180);
        LGlowPaint.ImageFilter := TSkImageFilter.MakeBlur(4, 4, nil,TSkTileMode.Decal);
        ACanvas.DrawSimpleText(GLYPHS[GlyphIdx], CX, CY, LFont, LGlowPaint);

        LBrightPaint := TSkPaint.Create;
        LBrightPaint.AntiAlias := True;
        LBrightPaint.Color := ToAlphaColor(220, 255, 220, 255);
        ACanvas.DrawSimpleText(GLYPHS[GlyphIdx], CX, CY, LFont, LBrightPaint);
      end
      else
      begin
        // Dimming tail
        LDimPaint := TSkPaint.Create;
        LDimPaint.AntiAlias := True;

        if Row <= 2 then
          // First few chars still greenish-cyan
          LDimPaint.Color := ToAlphaColor(
            0, Round(200 * TailAlpha), Round(255 * TailAlpha),
            Round(TailAlpha * 200))
        else
          // Deeper tail fades to the theme green
          LDimPaint.Color := ToAlphaColor(
            0, Round(255 * TailAlpha), Round(65 * TailAlpha),
            Round(TailAlpha * 160));

        ACanvas.DrawSimpleText(GLYPHS[GlyphIdx], CX, CY, LFont, LDimPaint);
      end;
    end;
  end;
end;

class procedure TShaderPainter.DrawStar(const ACanvas: ISkCanvas;
  const ACenterX, ACenterY: Single; const ARadius: Single;
  const APoints: Integer; const AInnerRadius: Single;
  const AAngle: Single; const AColor: TAlphaColor);
var
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  I: Integer;
  OuterAngle, InnerAngle: Single;
begin
  // Glow pass
  LPathBuilder := TSkPathBuilder.Create;
  for I := 0 to APoints - 1 do
  begin
    OuterAngle := AAngle + (I * 2 * Pi / APoints) - Pi / 2;
    InnerAngle := AAngle + ((I + 0.5) * 2 * Pi / APoints) - Pi / 2;
    if I = 0 then
      LPathBuilder.MoveTo(
        ACenterX + Cos(OuterAngle) * ARadius,
        ACenterY + Sin(OuterAngle) * ARadius)
    else
      LPathBuilder.LineTo(
        ACenterX + Cos(OuterAngle) * ARadius,
        ACenterY + Sin(OuterAngle) * ARadius);
    LPathBuilder.LineTo(
      ACenterX + Cos(InnerAngle) * AInnerRadius,
      ACenterY + Sin(InnerAngle) * AInnerRadius);
  end;
  LPathBuilder.Close;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Color := ToAlphaColor(
    TAlphaColorRec(AColor).R,
    TAlphaColorRec(AColor).G,
    TAlphaColorRec(AColor).B,
    60
  );
  LPaint.ImageFilter := TSkImageFilter.MakeBlur(10, 10,nil, TSkTileMode.Decal);
  ACanvas.DrawPath(LPathBuilder.Detach, LPaint);

  // Core pass
  LPathBuilder := TSkPathBuilder.Create;
  for I := 0 to APoints - 1 do
  begin
    OuterAngle := AAngle + (I * 2 * Pi / APoints) - Pi / 2;
    InnerAngle := AAngle + ((I + 0.5) * 2 * Pi / APoints) - Pi / 2;
    if I = 0 then
      LPathBuilder.MoveTo(
        ACenterX + Cos(OuterAngle) * ARadius,
        ACenterY + Sin(OuterAngle) * ARadius)
    else
      LPathBuilder.LineTo(
        ACenterX + Cos(OuterAngle) * ARadius,
        ACenterY + Sin(OuterAngle) * ARadius);
    LPathBuilder.LineTo(
      ACenterX + Cos(InnerAngle) * AInnerRadius,
      ACenterY + Sin(InnerAngle) * AInnerRadius);
  end;
  LPathBuilder.Close;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Color := AColor;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 1.5;
  ACanvas.DrawPath(LPathBuilder.Detach, LPaint);
end;

class procedure TShaderPainter.DrawGeometricPattern(const ACanvas: ISkCanvas;
  const ABounds: TRectF; const ATime: Single);
var
  LPaint: ISkPaint;
  CX, CY, R, Angle: Single;
  I, Sides, J: Integer;
  LPath: ISkPathBuilder;
  A, PX, PY: Single;
begin
  CX := ABounds.CenterPoint.X;
  CY := ABounds.CenterPoint.Y;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 0.8;

  for I := 1 to 8 do
  begin
    R     := (I / 8) * Min(ABounds.Width, ABounds.Height) * 0.45;
    Angle := ATime * 0.3 * IfThen(I mod 2 = 0, 1, -1);
    Sides := 4 + I;

    LPaint.Color := ToAlphaColor(
      0, 180 + I * 8, 255,
      Round(40 + (I / 8) * 80)
    );

    LPath := TSkPathBuilder.Create;
    for J := 0 to Sides do
    begin
      A  := Angle + (J / Sides) * 2 * Pi;
      PX := CX + Cos(A) * R;
      PY := CY + Sin(A) * R;
      if J = 0 then LPath.MoveTo(PX, PY)
      else          LPath.LineTo(PX, PY);
    end;
    LPath.Close;
    ACanvas.DrawPath(LPath.Detach, LPaint);
  end;
end;

end.
