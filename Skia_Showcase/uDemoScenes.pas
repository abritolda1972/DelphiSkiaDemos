unit uDemoScenes;

{
  uDemoScenes.pas
  ===============
  Skia4Delphi Showcase - Interactive Demo Scene Manager

}

interface

uses
  System.SysUtils,
  System.Math,
  System.UITypes,
  System.Generics.Collections,
  System.Types,
  FMX.Types,
  Skia,
  uThemeEngine,
  uShaderPainter,
  uParticleSystem,
  uAnimationEngine;

type
  TSceneID = (
    scParticles,
    scWaveforms,
    scGeometry,
    scGlass,
    scPlasma,
    scMatrix,
    scMandala,
    scSpectrum
  );

  TDemoScene = class abstract
  protected
    FBounds:   TRectF;
    FTime:     Single;
    FTitle:    string;
    FSubtitle: string;
    FTags:     string;
  public
    constructor Create; virtual;
    procedure SetBounds(const ABounds: TRectF); virtual;
    procedure Update(const ADeltaTime: Single); virtual; abstract;
    procedure Draw(const ACanvas: ISkCanvas); virtual; abstract;
    procedure OnClick(const AX, AY: Single); virtual;
    property Title:    string read FTitle;
    property Subtitle: string read FSubtitle;
    property Tags:     string read FTags;
  end;

  TParticleScene = class(TDemoScene)
  private
    FPS: TParticleSystem;
    FMode: Integer;
    FModeTime: Single;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetBounds(const ABounds: TRectF); override;
    procedure Update(const ADeltaTime: Single); override;
    procedure Draw(const ACanvas: ISkCanvas); override;
    procedure OnClick(const AX, AY: Single); override;
  end;

  TWaveformScene = class(TDemoScene)
  private
    FPhase:     array[0..3] of Single;
    FAmplitude: array[0..3] of Single;
    FColors:    array[0..3] of TAlphaColor;
  public
    constructor Create; override;
    procedure Update(const ADeltaTime: Single); override;
    procedure Draw(const ACanvas: ISkCanvas); override;
  end;

  TGeometryScene = class(TDemoScene)
  private
    FAngle:      Single;
    FLissPhase: Single;
  public
    constructor Create; override;
    procedure Update(const ADeltaTime: Single); override;
    procedure Draw(const ACanvas: ISkCanvas); override;
    procedure OnClick(const AX, AY: Single); override;
  end;

  TGlassScene = class(TDemoScene)
  private
    FCards: array[0..5] of record
      Bounds:          TRectF;
      Color:           TAlphaColor;
      Icon:            string;
      Title:           string;
      Value:           string;
      Progress:        Single;
      TargetProgress: Single;
    end;
    FHoverCard: Integer;
    FRingAngle: Single;
    procedure LayoutCards;
  public
    constructor Create; override;
    procedure SetBounds(const ABounds: TRectF); override;
    procedure Update(const ADeltaTime: Single); override;
    procedure Draw(const ACanvas: ISkCanvas); override;
    procedure OnClick(const AX, AY: Single); override;
  end;

  TPlasmaScene = class(TDemoScene)
  private
    FPS: TParticleSystem;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetBounds(const ABounds: TRectF); override;
    procedure Update(const ADeltaTime: Single); override;
    procedure Draw(const ACanvas: ISkCanvas); override;
    procedure OnClick(const AX, AY: Single); override;
  end;

  TMatrixScene = class(TDemoScene)
  private
    FPS: TParticleSystem;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetBounds(const ABounds: TRectF); override;
    procedure Update(const ADeltaTime: Single); override;
    procedure Draw(const ACanvas: ISkCanvas); override;
  end;

  TMandalaScene = class(TDemoScene)
  private
    FRings: array[0..4] of record
      Radius: Single;
      Petals: Integer;
      Color:  TAlphaColor;
      Speed:  Single;
      Phase:  Single;
    end;
    FStarAngle: Single;
  public
    constructor Create; override;
    procedure Update(const ADeltaTime: Single); override;
    procedure Draw(const ACanvas: ISkCanvas); override;
  end;

  TSpectrumScene = class(TDemoScene)
  private
    FBands:       array[0..63] of Single;
    FTargetBands: array[0..63] of Single;
    FUpdateTimer: Single;
  public
    constructor Create; override;
    procedure Update(const ADeltaTime: Single); override;
    procedure Draw(const ACanvas: ISkCanvas); override;
    procedure OnClick(const AX, AY: Single); override;
  end;

  TSceneManager = class
  private
    FScenes:          TObjectDictionary<TSceneID, TDemoScene>;
    FCurrentScene:    TSceneID;
    FTransitionAlpha: Single;
    FInTransition:    Boolean;
    FNextScene:       TSceneID;
    FBounds:          TRectF; // Adicionado para persistir limites locais
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetBounds(const ABounds: TRectF);
    procedure SwitchTo(const AScene: TSceneID);
    procedure Update(const ADeltaTime: Single);
    procedure Draw(const ACanvas: ISkCanvas);
    procedure OnClick(const AX, AY: Single);
    function  GetCurrentScene: TDemoScene;
    property  CurrentScene: TSceneID read FCurrentScene;
  end;

implementation

{ TDemoScene }

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

constructor TDemoScene.Create;
begin
  inherited;
  FTitle    := 'Scene';
  FSubtitle := '';
  FTags      := '';
end;

procedure TDemoScene.SetBounds(const ABounds: TRectF);
begin
  FBounds := ABounds;
end;

procedure TDemoScene.OnClick(const AX, AY: Single);
begin
end;

{ TParticleScene }

constructor TParticleScene.Create;
begin
  inherited;
  FTitle    := 'Particle Systems';
  FSubtitle := 'Real-time GPU-style particle physics';
  FTags     := 'Particles • Physics • Glow • Trails';
  FPS := TParticleSystem.Create(2000);
  FPS.SetType(ptSpark);
  FPS.SetEmitRate(80);
  FMode := 0;
end;

destructor TParticleScene.Destroy;
begin
  FPS.Free;
  inherited;
end;

procedure TParticleScene.SetBounds(const ABounds: TRectF);
begin
  inherited;
  FPS.SetBounds(ABounds.Width, ABounds.Height);
  FPS.SetCenter(ABounds.CenterPoint.X, ABounds.CenterPoint.Y);
end;

procedure TParticleScene.Update(const ADeltaTime: Single);
begin
  FTime     := FTime     + ADeltaTime;
  FModeTime := FModeTime + ADeltaTime;

  if FModeTime > 5 then
  begin
    FModeTime := 0;
    FMode := (FMode + 1) mod 3;
    case FMode of
      0: begin
           FPS.SetType(ptSpark);
           FPS.SetColors([Theme.Palette.Primary, Theme.Palette.Accent1, Theme.Palette.Accent3]);
           FPS.SetEmitRate(80);
         end;
      1: begin
           FPS.SetType(ptNebula);
           FPS.SetColors([Theme.Palette.Secondary, Theme.Palette.Accent2, Theme.Palette.Primary]);
           FPS.SetEmitRate(3);
         end;
      2: begin
           FPS.SetType(ptOrbit);
           FPS.SetColors([Theme.Palette.Accent1, Theme.Palette.Primary, Theme.Palette.Secondary]);
           FPS.SetEmitRate(5);
         end;
    end;
  end;

  FPS.SetCenter(
    FBounds.CenterPoint.X + Sin(FTime * 0.4) * FBounds.Width  * 0.2,
    FBounds.CenterPoint.Y + Sin(FTime * 0.8) * FBounds.Height * 0.15
  );
  FPS.Update(ADeltaTime);
end;

procedure TParticleScene.Draw(const ACanvas: ISkCanvas);
const
  ModeNames: array[0..2] of string = ('SPARK MODE', 'NEBULA MODE', 'ORBIT MODE');
begin
  TShaderPainter.DrawGridBackground(ACanvas, FBounds, FTime);
  FPS.Draw(ACanvas);

  TShaderPainter.DrawNeonText(ACanvas, ModeNames[FMode],
    FBounds.Left + 20, FBounds.Bottom - 30, 11,
    Theme.Palette.Accent1, 8);

  var CountStr := FPS.Count.ToString + ' PARTICLES';
  TShaderPainter.DrawNeonText(ACanvas, CountStr,
    FBounds.Right - 160, FBounds.Bottom - 30, 11,
    Theme.Palette.TextMuted, 5);
end;

procedure TParticleScene.OnClick(const AX, AY: Single);
begin
  FPS.Burst(AX - FBounds.Left, AY - FBounds.Top, 80);
end;

{ TWaveformScene }

constructor TWaveformScene.Create;
begin
  inherited;
  FTitle    := 'Signal Waveforms';
  FSubtitle := 'Multi-layer audio visualization';
  FTags     := 'Waveform • Paths • Gradients • Layering';
  FPhase[0] := 0;      FPhase[1] := Pi / 4;
  FPhase[2] := Pi / 2; FPhase[3] := Pi;
  FAmplitude[0] := 40; FAmplitude[1] := 30;
  FAmplitude[2] := 20; FAmplitude[3] := 25;
  FColors[0] := Theme.Palette.Primary;
  FColors[1] := Theme.Palette.Secondary;
  FColors[2] := Theme.Palette.Accent1;
  FColors[3] := Theme.Palette.Accent3;
end;

procedure TWaveformScene.Update(const ADeltaTime: Single);
begin
  FTime := FTime + ADeltaTime;
  FPhase[0] := FPhase[0] + ADeltaTime * 1.8;
  FPhase[1] := FPhase[1] + ADeltaTime * 2.3;
  FPhase[2] := FPhase[2] + ADeltaTime * 1.2;
  FPhase[3] := FPhase[3] + ADeltaTime * 3.1;
  FAmplitude[0] := 35 + 15 * Sin(FTime * 0.7);
  FAmplitude[1] := 25 + 12 * Sin(FTime * 0.9);
  FAmplitude[2] := 18 +  8 * Sin(FTime * 1.3);
  FAmplitude[3] := 20 + 10 * Sin(FTime * 0.5);
end;

procedure TWaveformScene.Draw(const ACanvas: ISkCanvas);
const
  Labels: array[0..3] of string = ('CH1  SYNTH', 'CH2  BASS', 'CH3  LEAD', 'CH4  PAD');
var
  I: Integer;
  LaneH: Single;
  LaneBounds: TRectF;
  LFont: ISkFont;
  LPaint: ISkPaint;
  SepPaint: ISkPaint;
begin
  LaneH  := FBounds.Height / 4;
  LFont  := TSkFont.Create(TSkTypeface.MakeDefault, 9);
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  for I := 0 to 3 do
  begin
    LaneBounds := TRectF.Create(
      FBounds.Left,  FBounds.Top + I * LaneH,
      FBounds.Right, FBounds.Top + (I + 1) * LaneH
    );

    SepPaint := TSkPaint.Create;
    SepPaint.Color := $15FFFFFF;
    SepPaint.Style := TSkPaintStyle.Stroke;
    SepPaint.StrokeWidth := 0.5;
    ACanvas.DrawLine(
      TPointF.Create(FBounds.Left,  LaneBounds.Bottom),
      TPointF.Create(FBounds.Right, LaneBounds.Bottom),
      SepPaint
    );

    TShaderPainter.DrawWaveform(ACanvas, LaneBounds, FPhase[I],
      FColors[I], FAmplitude[I]);

//    LPaint.Color := (FColors[I] and $00FFFFFF) or ($96 shl 24);
LPaint.Color := Cardinal((Cardinal(FColors[I]) and $00FFFFFF) or (Cardinal($96) shl 24));
    ACanvas.DrawSimpleText(Labels[I],
      FBounds.Left + 8, LaneBounds.Top + 14, LFont, LPaint);
  end;
end;

{ TGeometryScene }

constructor TGeometryScene.Create;
begin
  inherited;
  FTitle    := 'Mathematical Curves';
  FSubtitle := 'Lissajous, spirals & geometric forms';
  FTags     := 'Lissajous • Spirals • Paths • Math';
end;

procedure TGeometryScene.Update(const ADeltaTime: Single);
begin
  FTime       := FTime      + ADeltaTime;
  FAngle      := FAngle      + ADeltaTime * 0.8;
  FLissPhase := FLissPhase + ADeltaTime * 0.25;
end;

procedure TGeometryScene.Draw(const ACanvas: ISkCanvas);
var
  CX, CY, HW, HH: Single;
begin
  CX := FBounds.CenterPoint.X;
  CY := FBounds.CenterPoint.Y;
  HW := FBounds.Width  * 0.5;
  HH := FBounds.Height * 0.5;

  TShaderPainter.DrawGeometricPattern(ACanvas, FBounds, FTime);
  TShaderPainter.DrawSpiral(ACanvas, CX, CY, FTime,
    Min(HW, HH) * 0.5, Theme.Palette.Accent1);
  TShaderPainter.DrawLissajous(ACanvas, CX, CY, FLissPhase,
    HW * 0.8, HH * 0.8, Theme.Palette.Secondary);

  TShaderPainter.DrawStar(ACanvas, CX, CY - HH * 0.7, 18, 6, 8,
    FAngle * 1.5, Theme.Palette.Primary);
  TShaderPainter.DrawStar(ACanvas, CX + HW * 0.7, CY, 14, 5, 6,
    -FAngle, Theme.Palette.Accent1);
  TShaderPainter.DrawStar(ACanvas, CX - HW * 0.7, CY, 14, 4, 7,
    FAngle * 0.7, Theme.Palette.Accent2);
  TShaderPainter.DrawStar(ACanvas, CX, CY + HH * 0.7, 18, 8, 7,
    -FAngle * 1.2, Theme.Palette.Accent3);
end;

procedure TGeometryScene.OnClick(const AX, AY: Single);
begin
  FLissPhase := FLissPhase + 0.3;
end;

{ TGlassScene }

constructor TGlassScene.Create;
begin
  inherited;
  FTitle     := 'Glassmorphism UI';
  FSubtitle  := 'Layered translucent panels with neon accents';
  FTags      := 'Glass • Blur • Gradients • Panels';
  FHoverCard := -1;

  FCards[0].Color := Theme.Palette.Primary;
  FCards[0].Icon  := 'CPU'; FCards[0].Title := 'Processing';
  FCards[0].Value := '94.2%'; FCards[0].TargetProgress := 0.942;

  FCards[1].Color := Theme.Palette.Secondary;
  FCards[1].Icon  := 'MEM'; FCards[1].Title := 'Memory';
  FCards[1].Value := '67.8%'; FCards[1].TargetProgress := 0.678;

  FCards[2].Color := Theme.Palette.Accent1;
  FCards[2].Icon  := 'NET'; FCards[2].Title := 'Network';
  FCards[2].Value := '1.2 GB/s'; FCards[2].TargetProgress := 0.82;

  FCards[3].Color := Theme.Palette.Accent3;
  FCards[3].Icon  := 'GPU'; FCards[3].Title := 'Graphics';
  FCards[3].Value := '88.5%'; FCards[3].TargetProgress := 0.885;

  FCards[4].Color := Theme.Palette.Accent2;
  FCards[4].Icon  := 'TMP'; FCards[4].Title := 'Temperature';
  FCards[4].Value := '72°C'; FCards[4].TargetProgress := 0.72;

  FCards[5].Color := $FF7C3AED;
  FCards[5].Icon  := 'FPS'; FCards[5].Title := 'Frame Rate';
  FCards[5].Value := '144 fps'; FCards[5].TargetProgress := 1.0;
end;

procedure TGlassScene.LayoutCards;
var
  I, Col, Row: Integer;
  CardW, CardH, PadX, PadY: Single;
begin
  CardW := (FBounds.Width  - 60) / 3;
  CardH := (FBounds.Height - 60) / 2;
  PadX  := 20;
  PadY  := 20;

  for I := 0 to 5 do
  begin
    Col := I mod 3;
    Row := I div 3;
    FCards[I].Bounds := TRectF.Create(
      FBounds.Left + PadX + Col * (CardW + PadX),
      FBounds.Top  + PadY + Row * (CardH + PadY),
      FBounds.Left + PadX + Col * (CardW + PadX) + CardW,
      FBounds.Top  + PadY + Row * (CardH + PadY) + CardH
    );
  end;
end;

procedure TGlassScene.SetBounds(const ABounds: TRectF);
begin
  inherited;
  LayoutCards;
end;

procedure TGlassScene.Update(const ADeltaTime: Single);
var
  I: Integer;
begin
  FTime      := FTime      + ADeltaTime;
  FRingAngle := FRingAngle + ADeltaTime * 0.5;

  for I := 0 to 5 do
  begin
    FCards[I].Progress := FCards[I].Progress +
      (FCards[I].TargetProgress - FCards[I].Progress) * ADeltaTime * 3;

    FCards[I].TargetProgress := FCards[I].TargetProgress +
      (Random - 0.5) * 0.005;
    FCards[I].TargetProgress :=
      EnsureRange(FCards[I].TargetProgress, 0.1, 1.0);
  end;
end;

procedure TGlassScene.Draw(const ACanvas: ISkCanvas);
var
  I: Integer;
  LFont, LLargeFont: ISkFont;
  LPaint: ISkPaint;
  CB: TRectF;
  TW: Single;
begin
  TShaderPainter.DrawHexagonalGrid(ACanvas, FBounds, FTime);

  LFont      := TSkFont.Create(TSkTypeface.MakeDefault, 11);
  LLargeFont := TSkFont.Create(TSkTypeface.MakeDefault, 20);
  LPaint      := TSkPaint.Create;
  LPaint.AntiAlias := True;

  for I := 0 to 5 do
  begin
    CB := FCards[I].Bounds;

    TShaderPainter.DrawGlassPanel(ACanvas, CB, 16);
    TShaderPainter.DrawNeonBorder(ACanvas, CB, 16, FCards[I].Color, FTime + I * 0.5);

    var RingCX := CB.Left + CB.Width  * 0.5;
    var RingCY := CB.Top  + CB.Height * 0.5 - 8;
    var RingR  := Min(CB.Width, CB.Height) * 0.28;
    TShaderPainter.DrawProgressRing(ACanvas, RingCX, RingCY, RingR,
      FCards[I].Progress, FCards[I].Color, 4, FCards[I].Icon);

    LPaint.Color := Theme.Palette.TextSecondary;
    TW := LFont.MeasureText(FCards[I].Title); // FIX: Removido nil, LPaint
    ACanvas.DrawSimpleText(FCards[I].Title,
      CB.Left + (CB.Width - TW) * 0.5, CB.Bottom - 28,
      LFont, LPaint);

    TW := LLargeFont.MeasureText(FCards[I].Value); // FIX: Removido nil, LPaint
    TShaderPainter.DrawNeonText(ACanvas, FCards[I].Value,
      CB.Left + (CB.Width - TW) * 0.5,
      CB.Bottom - 8, 13, FCards[I].Color, 8);
  end;
end;

procedure TGlassScene.OnClick(const AX, AY: Single);
var
  I: Integer;
begin
  for I := 0 to 5 do
    if FCards[I].Bounds.Contains(TPointF.Create(AX, AY)) then
    begin
      FCards[I].TargetProgress := Random;
      Break;
    end;
end;

{ TPlasmaScene }

constructor TPlasmaScene.Create;
begin
  inherited;
  FTitle    := 'Plasma Ball';
  FSubtitle := 'Electric plasma simulation';
  FTags     := 'Plasma • Shaders • Glow • Effects';
  FPS := TParticleSystem.Create(500);
  FPS.SetType(ptOrbit);
  FPS.SetColors([$FF00D4FF, $FF00FFCC, $FF0088FF]);
  FPS.SetEmitRate(8);
end;

destructor TPlasmaScene.Destroy;
begin
  FPS.Free;
  inherited;
end;

procedure TPlasmaScene.SetBounds(const ABounds: TRectF);
begin
  inherited;
  FPS.SetBounds(ABounds.Width, ABounds.Height);
  FPS.SetCenter(ABounds.CenterPoint.X, ABounds.CenterPoint.Y);
end;

procedure TPlasmaScene.Update(const ADeltaTime: Single);
begin
  FTime := FTime + ADeltaTime;
  FPS.Update(ADeltaTime);
end;

procedure TPlasmaScene.Draw(const ACanvas: ISkCanvas);
var
  CX, CY, R: Single;
begin
  CX := FBounds.CenterPoint.X;
  CY := FBounds.CenterPoint.Y;
  R  := Min(FBounds.Width, FBounds.Height) * 0.35;

  FPS.Draw(ACanvas);
  TShaderPainter.DrawPlasmaBall(ACanvas, CX, CY, R, FTime);

  TShaderPainter.DrawNeonText(ACanvas, 'PLASMA FIELD ACTIVE',
    FBounds.Left + 20, FBounds.Top + 30, 10,
    Theme.Palette.Primary, 6);
  TShaderPainter.DrawNeonText(ACanvas,
    Format('FREQUENCY: %.1f THz', [2.4 + Sin(FTime * 0.3) * 0.8]),
    FBounds.Left + 20, FBounds.Top + 48, 9,
    Theme.Palette.TextMuted, 4);
end;

procedure TPlasmaScene.OnClick(const AX, AY: Single);
begin
  FPS.Burst(AX - FBounds.Left, AY - FBounds.Top, 40);
end;

{ TMatrixScene }

constructor TMatrixScene.Create;
begin
  inherited;
  FTitle    := 'Digital Rain';
  FSubtitle := 'Matrix-style cascading data stream';
  FTags     := 'Matrix • Rain • Text • Cascade';
  FPS := TParticleSystem.Create(300);
  FPS.SetType(ptData);
  FPS.SetColors([$FF00FF41, $FF00D4FF, $FF00FFCC]);
  FPS.SetEmitRate(8);
end;

destructor TMatrixScene.Destroy;
begin
  FPS.Free;
  inherited;
end;

procedure TMatrixScene.SetBounds(const ABounds: TRectF);
begin
  inherited;
  FPS.SetBounds(ABounds.Width, ABounds.Height);
  FPS.SetCenter(ABounds.CenterPoint.X, ABounds.CenterPoint.Y);
end;

procedure TMatrixScene.Update(const ADeltaTime: Single);
begin
  FTime := FTime + ADeltaTime;
  FPS.Update(ADeltaTime);
end;

procedure TMatrixScene.Draw(const ACanvas: ISkCanvas);
begin
  TShaderPainter.DrawCodeRain(ACanvas, FBounds, FTime);
  FPS.Draw(ACanvas);

  TShaderPainter.DrawNeonText(ACanvas, 'SIGNAL INTERCEPTED',
    FBounds.CenterPoint.X - 90, FBounds.Top + 35, 14,
    Theme.Palette.Accent1, 10);
  TShaderPainter.DrawNeonText(ACanvas, 'DECRYPTING DATA STREAM...',
    FBounds.CenterPoint.X - 110, FBounds.Top + 58, 10,
    $FF00FF41, 6);
end;

{ TMandalaScene }

constructor TMandalaScene.Create;
const
  Colors: array[0..4] of TAlphaColor = (
    $FFFF0090, $FF00D4FF, $FF00FFCC, $FFFFC400, $FFFF3366
  );
  Petals: array[0..4] of Integer  = (6, 8, 12, 10, 16);
  Radii:  array[0..4] of Single   = (30, 70, 110, 150, 190);
  Speeds: array[0..4] of Single   = (1.2, -0.8, 0.6, -0.4, 0.3);
var
  I: Integer;
begin
  inherited;
  FTitle    := 'Mandala Generator';
  FSubtitle := 'Procedural geometric mandalas';
  FTags     := 'Mandala • Geometry • Bezier • Symmetry';

  FillChar(FRings, SizeOf(FRings), 0);

  for I := 0 to 4 do
  begin
    FRings[I].Color  := Colors[I];
    FRings[I].Petals := Petals[I];
    FRings[I].Radius := Radii[I];
    FRings[I].Speed  := Speeds[I];
    FRings[I].Phase  := (I / 5) * 2 * Pi;
  end;
end;

procedure TMandalaScene.Update(const ADeltaTime: Single);
var
  I: Integer;
begin
  FTime       := FTime      + ADeltaTime;
  FStarAngle := FStarAngle + ADeltaTime * 0.5;
  for I := 0 to 4 do
    FRings[I].Phase := FRings[I].Phase + FRings[I].Speed * ADeltaTime;
end;

procedure TMandalaScene.Draw(const ACanvas: ISkCanvas);
var
  I: Integer;
  CX, CY: Single;
  GP: ISkPaint;
begin
  CX := FBounds.CenterPoint.X;
  CY := FBounds.CenterPoint.Y;

  TShaderPainter.DrawGeometricPattern(ACanvas, FBounds, FTime * 0.3);

  for I := 4 downto 0 do
    TShaderPainter.DrawMandalaRing(ACanvas, CX, CY,
      FRings[I].Radius, FRings[I].Petals,
      FRings[I].Phase, FRings[I].Color);

  TShaderPainter.DrawStar(ACanvas, CX, CY, 22, 8, 10,
    FStarAngle, Theme.Palette.Accent3);

  GP := TSkPaint.Create;
  GP.AntiAlias := True;
  GP.Shader := TSkShader.MakeGradientRadial(
    TPointF.Create(CX, CY), 15,
    [$FFFFC400, $00FFC400], nil, TSkTileMode.Clamp
  );
  ACanvas.DrawCircle(CX, CY, 15, GP);
end;

{ TSpectrumScene }

constructor TSpectrumScene.Create;
var
  I: Integer;
begin
  inherited;
  FTitle    := 'Audio Spectrum';
  FSubtitle := 'Real-time frequency analyzer';
  FTags     := 'Spectrum • Audio • Bars • Gradient';
  Randomize;
  for I := 0 to 63 do
  begin
    FBands[I]       := Random;
    FTargetBands[I] := Random;
  end;
end;

procedure TSpectrumScene.Update(const ADeltaTime: Single);
var
  I: Integer;
begin
  FTime        := FTime        + ADeltaTime;
  FUpdateTimer := FUpdateTimer + ADeltaTime;

  if FUpdateTimer > 0.08 then
  begin
    FUpdateTimer := 0;
    for I := 0 to 63 do
    begin
      FTargetBands[I] :=
        0.1 +
        0.4 * Max(0, Sin(I * 0.15 + FTime * 3)) +
        0.3 * Max(0, Sin(I * 0.08 - FTime * 2)) +
        0.2 * Random;

      if I < 8 then
        FTargetBands[I] := FTargetBands[I] +
          0.3 * Max(0, Sin(FTime * 4));
    end;
  end;

  for I := 0 to 63 do
    FBands[I] := FBands[I] +
      (FTargetBands[I] - FBands[I]) * Min(1, ADeltaTime * 10);
end;

procedure TSpectrumScene.Draw(const ACanvas: ISkCanvas);
const
  Colors: array[0..3] of TAlphaColor = (
    $FFFF0090, $FF00D4FF, $FF00FFCC, $FFFFC400
  );
  Labels: array[0..3] of string = ('SUB', 'BASS', 'MID', 'HIGH');
var
  I, ColorIdx: Integer;
  LPaint: ISkPaint;
  LFont: ISkFont;
  TextP: ISkPaint;
  BarW, BarH, X, Y: Single;
  BarColor: TAlphaColor;
  PkPaint: ISkPaint;
begin
  BarW  := (FBounds.Width - 20) / 64;
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  for I := 0 to 63 do
  begin
    X        := FBounds.Left + 10 + I * BarW;
    BarH     := FBands[I] * (FBounds.Height * 0.75);
    Y        := FBounds.Bottom - 40 - BarH;
    ColorIdx := I div 16;
    BarColor := Colors[ColorIdx];

    // FIX: Gradiente corrigido para arrays simples de TAlphaColor
    LPaint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(X, FBounds.Bottom - 40),
      TPointF.Create(X, Y),
      [BarColor, (BarColor and $00FFFFFF) or ($3C shl 24)],
      [0.0, 1.0], TSkTileMode.Clamp
    );

    ACanvas.DrawRect(
      TRectF.Create(X + 1, Y, X + BarW - 2, FBounds.Bottom - 40),
      LPaint);

    PkPaint := TSkPaint.Create;
    PkPaint.Color     := BarColor;
    PkPaint.AntiAlias := True;
    ACanvas.DrawCircle(X + BarW * 0.5, Y, 2, PkPaint);
  end;

  LFont := TSkFont.Create(TSkTypeface.MakeDefault, 9);
  TextP := TSkPaint.Create;
  TextP.AntiAlias := True;
  TextP.Color := Theme.Palette.TextMuted;

  for I := 0 to 3 do
    ACanvas.DrawSimpleText(Labels[I],
      FBounds.Left + 10 + I * 16 * BarW, FBounds.Bottom - 20,
      LFont, TextP);

  TShaderPainter.DrawNeonText(ACanvas,
    Format('%.1f dB', [-6 + FBands[0] * 12]),
    FBounds.Right - 80, FBounds.Top + 25, 11,
    Theme.Palette.Accent1, 5);
end;

procedure TSpectrumScene.OnClick(const AX, AY: Single);
var
  I: Integer;
begin
  for I := 0 to 63 do
    FTargetBands[I] := 0.5 + Random * 0.5;
end;

{ TSceneManager }

constructor TSceneManager.Create;
begin
  inherited;
  FScenes := TObjectDictionary<TSceneID, TDemoScene>.Create([doOwnsValues]);
  FScenes.Add(scParticles, TParticleScene.Create);
  FScenes.Add(scWaveforms, TWaveformScene.Create);
  FScenes.Add(scGeometry,  TGeometryScene.Create);
  FScenes.Add(scGlass,      TGlassScene.Create);
  FScenes.Add(scPlasma,    TPlasmaScene.Create);
  FScenes.Add(scMatrix,    TMatrixScene.Create);
  FScenes.Add(scMandala,    TMandalaScene.Create);
  FScenes.Add(scSpectrum,  TSpectrumScene.Create);
  FCurrentScene    := scParticles;
  FTransitionAlpha := 1.0;
end;

destructor TSceneManager.Destroy;
begin
  FScenes.Free;
  inherited;
end;

procedure TSceneManager.SetBounds(const ABounds: TRectF);
var
  Scene: TDemoScene;
begin
  FBounds := ABounds; // FIX: Armazena localmente
  for Scene in FScenes.Values do
    Scene.SetBounds(ABounds);
end;

procedure TSceneManager.SwitchTo(const AScene: TSceneID);
begin
  if AScene = FCurrentScene then Exit;
  FCurrentScene    := AScene;
  FTransitionAlpha := 0;
  FInTransition    := True;
  FNextScene       := AScene;
end;

procedure TSceneManager.Update(const ADeltaTime: Single);
begin
  FScenes[FCurrentScene].Update(ADeltaTime);

  if FInTransition then
  begin
    FTransitionAlpha := FTransitionAlpha + ADeltaTime * 4;
    if FTransitionAlpha >= 1.0 then
    begin
      FTransitionAlpha := 1.0;
      FInTransition    := False;
    end;
  end;
end;

procedure TSceneManager.Draw(const ACanvas: ISkCanvas);
var
  Scene: TDemoScene;
  FadePaint: ISkPaint;
begin
  ACanvas.Save;
  Scene := FScenes[FCurrentScene];
  Scene.Draw(ACanvas);

  if FInTransition then
  begin
    FadePaint := TSkPaint.Create;
    FadePaint.Color := ToAlphaColor(0, 0, 0,
      Round((1 - FTransitionAlpha) * 200));
    ACanvas.DrawRect(FBounds, FadePaint); // FIX: Usa o FBounds da própria classe
  end;
  ACanvas.Restore;
end;

procedure TSceneManager.OnClick(const AX, AY: Single);
begin
  FScenes[FCurrentScene].OnClick(AX, AY);
end;

function TSceneManager.GetCurrentScene: TDemoScene;
begin
  Result := FScenes[FCurrentScene];
end;

end.
