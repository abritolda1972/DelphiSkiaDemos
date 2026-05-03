unit uThemeEngine;

{
  uThemeEngine.pas
  ================
  Skia4Delphi Showcase - Theme & Design System Engine
  Implements a cyberpunk/futuristic dark aesthetic with neon accents
  and glassmorphism effects.
}

interface

uses
  System.UITypes,
  Skia, System.Types;

type
  TColorPalette = record
    Background:    TAlphaColor;
    Surface:       TAlphaColor;
    SurfaceGlass:  TAlphaColor;
    Primary:       TAlphaColor;
    Secondary:     TAlphaColor;
    Accent1:       TAlphaColor;  // Cyan neon
    Accent2:       TAlphaColor;  // Magenta neon
    Accent3:       TAlphaColor;  // Gold
    TextPrimary:   TAlphaColor;
    TextSecondary: TAlphaColor;
    TextMuted:     TAlphaColor;
    Border:        TAlphaColor;
    Glow:          TAlphaColor;
  end;

  TThemeEngine = class
  private
    class var FInstance: TThemeEngine;
  public
    Palette: TColorPalette;
    constructor Create;
    class function Instance: TThemeEngine;
    class destructor Destroy;

    // Paint factory methods
    function MakeGlowPaint(const AColor: TAlphaColor; const ARadius: Single = 20): ISkPaint;
    function MakeNeonBorderPaint(const AColor: TAlphaColor; const AWidth: Single = 1.5): ISkPaint;
    function MakeGlassPaint(const AAlpha: Byte = 40): ISkPaint;
    function MakeTextPaint(const AColor: TAlphaColor; const ASize: Single = 14): ISkPaint;
    function MakeGradientPaint(const AColor1, AColor2: TAlphaColor; const x1, y1, x2, y2: Single): ISkPaint;

    // Shader helpers
    function MakeScanlineShader(const ABounds: TRectF; const ASpacing: Single = 4): ISkShader;
    function MakeNoiseShader(const AScale: Single = 0.8): ISkShader;

    // Color utilities
    function AlphaBlend(const AColor: TAlphaColor; const AAlpha: Byte): TAlphaColor;
    function GlowColor(const AColor: TAlphaColor): TAlphaColor;
  end;

function Theme: TThemeEngine; inline;

implementation

uses
  System.SysUtils;

{ TThemeEngine }

constructor TThemeEngine.Create;
begin
  inherited;
  Palette.Background    := $FF030812;  // Deep space black-blue
  Palette.Surface       := $FF0A1628;  // Dark navy surface
  Palette.SurfaceGlass  := $1A1E3A60;  // Translucent glass
  Palette.Primary       := $FF00D4FF;  // Electric cyan
  Palette.Secondary     := $FFFF0090;  // Hot magenta
  Palette.Accent1       := $FF00FFCC;  // Neon mint
  Palette.Accent2       := $FFFF3366;  // Neon pink-red
  Palette.Accent3       := $FFFFC400;  // Gold
  Palette.TextPrimary   := $FFF0F8FF;  // Almost white
  Palette.TextSecondary := $FFB0C8E8;  // Soft blue-white
  Palette.TextMuted     := $FF5A7090;  // Muted blue
  Palette.Border        := $3300D4FF;  // Translucent cyan
  Palette.Glow          := $2200D4FF;  // Very translucent cyan
end;

class function TThemeEngine.Instance: TThemeEngine;
begin
  if not Assigned(FInstance) then
    FInstance := TThemeEngine.Create;
  Result := FInstance;
end;

class destructor TThemeEngine.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TThemeEngine.AlphaBlend(const AColor: TAlphaColor; const AAlpha: Byte): TAlphaColor;
begin
Result := (AColor and $00FFFFFF) or (TAlphaColor(AAlpha) shl 24);
end;

function TThemeEngine.GlowColor(const AColor: TAlphaColor): TAlphaColor;
begin
  Result := AlphaBlend(AColor, 80);
end;

function TThemeEngine.MakeGlowPaint(const AColor: TAlphaColor; const ARadius: Single): ISkPaint;
var
  LFilter: ISkImageFilter;
begin
  Result := TSkPaint.Create;
  Result.Color := AColor;
  LFilter := TSkImageFilter. MakeBlur(ARadius, ARadius,nil, TSkTileMode.Decal);
  Result.ImageFilter := LFilter;
  Result.Style := TSkPaintStyle.Fill;
  Result.AntiAlias := True;
end;

function TThemeEngine.MakeNeonBorderPaint(const AColor: TAlphaColor; const AWidth: Single): ISkPaint;
begin
  Result := TSkPaint.Create;
  Result.Color := AColor;
  Result.Style := TSkPaintStyle.Stroke;
  Result.StrokeWidth := AWidth;
  Result.AntiAlias := True;
  Result.StrokeCap := TSkStrokeCap.Round;
  Result.StrokeJoin := TSkStrokeJoin.Round;
end;

function TThemeEngine.MakeGlassPaint(const AAlpha: Byte): ISkPaint;
begin
  Result := TSkPaint.Create;
  Result.Color := AlphaBlend(Palette.Surface, AAlpha);
  Result.Style := TSkPaintStyle.Fill;
  Result.AntiAlias := True;
end;

function TThemeEngine.MakeTextPaint(const AColor: TAlphaColor; const ASize: Single): ISkPaint;
begin
  Result := TSkPaint.Create;
  Result.Color := AColor;
  Result.AntiAlias := True;
end;

function TThemeEngine.MakeGradientPaint(const AColor1, AColor2: TAlphaColor;
  const x1, y1, x2, y2: Single): ISkPaint;
var
  LColors: TArray<TAlphaColor>;
  LPos: TArray<Single>;
begin
  Result := TSkPaint.Create;
  Result.AntiAlias := True;
  LColors := [AColor1, AColor2];
  LPos := [0.0, 1.0];
  Result.Shader := TSkShader.MakeGradientLinear(
    TPointF.Create(x1, y1),
    TPointF.Create(x2, y2),
    LColors, LPos, TSkTileMode.Clamp
  );
end;

function TThemeEngine.MakeScanlineShader(const ABounds: TRectF; const ASpacing: Single): ISkShader;
var
  LColors: TArray<TAlphaColor>;
  LPos: TArray<Single>;
begin
  LColors := [$00000000, $00000000, $0A000000, $00000000];
  LPos := [0.0, 0.5, 0.51, 1.0];
  Result := TSkShader.MakeGradientLinear(
    TPointF.Create(0, 0),
    TPointF.Create(0, ASpacing),
    LColors, LPos, TSkTileMode.Repeat
  );
end;

function TThemeEngine.MakeNoiseShader(const AScale: Single): ISkShader;
begin
  Result := TSkShader.MakePerlinNoiseFractalNoise(AScale, AScale, 4, 0.5);
end;

function Theme: TThemeEngine;
begin
  Result := TThemeEngine.Instance;
end;

end.
