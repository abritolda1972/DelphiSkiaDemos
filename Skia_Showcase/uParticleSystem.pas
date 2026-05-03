unit uParticleSystem;

{
  uParticleSystem.pas
  ===================
  Skia4Delphi Showcase - High-Performance Particle System
  Simulates thousands of GPU-like particles with physics,
  color gradients, and trail effects.
}

interface

uses
  System.SysUtils,
  System.Math,
  System.UITypes,
  System.Generics.Collections,
  FMX.Types,
  Skia,
  uThemeEngine,
  uAnimationEngine;

type
  TParticleType = (
    ptSpark,       // Fast, bright sparks
    ptNebula,      // Slow, large glowing orbs
    ptData,        // Matrix-style falling data
    ptOrbit,       // Orbiting particles
    ptExplosion    // Burst from center
  );

  TParticle = record
    X, Y:        Single;
    VX, VY:      Single;
    Life:        Single;  // 0..1
    MaxLife:     Single;
    Size:        Single;
    Color:       TAlphaColor;
    Color2:      TAlphaColor;
    Rotation:    Single;
    RotSpeed:    Single;
    ParticleType: TParticleType;
    Orbit:       Single; // orbit radius for ptOrbit
    OrbitAngle:  Single;
    OrbitSpeed:  Single;
    Char:        Char;   // for ptData
  end;

  TParticleSystem = class
  private
    FParticles:   array of TParticle;
    FCount:       Integer;
    FMaxParticles: Integer;
    FCenterX, FCenterY: Single;
    FWidth, FHeight: Single;
    FTime:        Single;
    FParticleType: TParticleType;
    FEmitRate:    Single;
    FEmitAccum:   Single;
    FColors:      TArray<TAlphaColor>;

    procedure EmitParticle;
    procedure InitSpark(var P: TParticle);
    procedure InitNebula(var P: TParticle);
    procedure InitData(var P: TParticle);
    procedure InitOrbit(var P: TParticle);
    procedure UpdateSpark(var P: TParticle; const DT: Single);
    procedure UpdateNebula(var P: TParticle; const DT: Single);
    procedure UpdateData(var P: TParticle; const DT: Single);
    procedure UpdateOrbit(var P: TParticle; const DT: Single);
    function RandomColor: TAlphaColor;
  public
    constructor Create(const AMaxParticles: Integer = 1000);

    procedure SetBounds(const AWidth, AHeight: Single);
    procedure SetCenter(const AX, AY: Single);
    procedure SetType(const AType: TParticleType);
    procedure SetColors(const AColors: TArray<TAlphaColor>);
    procedure SetEmitRate(const ARate: Single);

    procedure Burst(const AX, AY: Single; const ACount: Integer = 50);

    procedure Update(const ADeltaTime: Single);
    procedure Draw(const ACanvas: ISkCanvas);

    property Count: Integer read FCount;
    property Time: Single read FTime;
  end;

implementation

uses
  System.Character;

{ TParticleSystem }

constructor TParticleSystem.Create(const AMaxParticles: Integer);
begin
//  inherited Create;
//  FMaxParticles := AMaxParticles;
//  SetLength(FParticles, FMaxParticles);
//  FCount := 0;
//  FEmitRate := 30;
//  FParticleType := ptSpark;
//  FColors := [
//    Theme.Palette.Primary,
//    Theme.Palette.Accent1,
//    Theme.Palette.Secondary
//  ];

inherited Create;
  FMaxParticles := AMaxParticles;
  SetLength(FParticles, FMaxParticles);
  FCount := 0;
  FEmitRate := 30;
  FEmitAccum := 0; // Inicializa sempre os acumuladores
  FParticleType := ptSpark;

  // Verifica se o tema existe, senão usa cores padrão
 // if Assigned(Theme) and Assigned(Theme.Palette) then
    FColors := [Theme.Palette.Primary, Theme.Palette.Accent1, Theme.Palette.Secondary]
  //else
 //   FColors := [$FFFFFFFF, $FF888888, $FF444444];
end;

procedure TParticleSystem.SetBounds(const AWidth, AHeight: Single);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FCenterX := AWidth * 0.5;
  FCenterY := AHeight * 0.5;
end;

procedure TParticleSystem.SetCenter(const AX, AY: Single);
begin
  FCenterX := AX;
  FCenterY := AY;
end;

procedure TParticleSystem.SetType(const AType: TParticleType);
begin
  FParticleType := AType;
  FCount := 0;
end;

procedure TParticleSystem.SetColors(const AColors: TArray<TAlphaColor>);
begin
  FColors := AColors;
end;

procedure TParticleSystem.SetEmitRate(const ARate: Single);
begin
  FEmitRate := ARate;
end;

function TParticleSystem.RandomColor: TAlphaColor;
begin
  if Length(FColors) = 0 then
    Result := Theme.Palette.Primary
  else
    Result := FColors[Random(Length(FColors))];
end;

procedure TParticleSystem.InitSpark(var P: TParticle);
var
  Angle, Speed: Single;
begin
  Angle := Random * 2 * Pi;
  Speed := 20 + Random * 200;
  P.X := FCenterX + (Random - 0.5) * 20;
  P.Y := FCenterY + (Random - 0.5) * 20;
  P.VX := Cos(Angle) * Speed;
  P.VY := Sin(Angle) * Speed;
  P.MaxLife := 0.5 + Random * 1.5;
  P.Life := P.MaxLife;
  P.Size := 1 + Random * 3;
  P.Color := RandomColor;
  P.Color2 := Theme.Palette.Accent3;
  P.Rotation := Random * 2 * Pi;
  P.RotSpeed := (Random - 0.5) * 10;
  P.ParticleType := ptSpark;
end;

procedure TParticleSystem.InitNebula(var P: TParticle);
begin
  P.X := Random * FWidth;
  P.Y := Random * FHeight;
  P.VX := (Random - 0.5) * 10;
  P.VY := (Random - 0.5) * 10;
  P.MaxLife := 3 + Random * 5;
  P.Life := P.MaxLife;
  P.Size := 20 + Random * 60;
  P.Color := RandomColor;
  P.ParticleType := ptNebula;
end;

procedure TParticleSystem.InitData(var P: TParticle);
const
  CHARS = '01アイウエオカキクケコサシスセソABCDEFGHIJKLMNOP';
begin
  P.X := Random * FWidth;
  P.Y := -10;
  P.VX := 0;
  P.VY := 40 + Random * 120;
  P.MaxLife := FHeight / Max(1, P.VY) + Random;
  P.Life := P.MaxLife;
  P.Size := 9 + Random * 4;
  P.Color := RandomColor;
  P.Char := CHARS[1 + Random(Length(CHARS))];
  P.ParticleType := ptData;
end;

procedure TParticleSystem.InitOrbit(var P: TParticle);
begin
  P.Orbit := 30 + Random * Min(FWidth, FHeight) * 0.4;
  P.OrbitAngle := Random * 2 * Pi;
  P.OrbitSpeed := (0.3 + Random * 1.5) * (IfThen(Random > 0.5, 1, -1));
  P.X := FCenterX + Cos(P.OrbitAngle) * P.Orbit;
  P.Y := FCenterY + Sin(P.OrbitAngle) * P.Orbit;
  P.MaxLife := 5 + Random * 10;
  P.Life := P.MaxLife;
  P.Size := 2 + Random * 5;
  P.Color := RandomColor;
  P.ParticleType := ptOrbit;
end;

procedure TParticleSystem.EmitParticle;
var
  P: TParticle;
begin
  if FCount >= FMaxParticles then Exit;
  FillChar(P, SizeOf(P), 0);
  case FParticleType of
    ptSpark:  InitSpark(P);
    ptNebula: InitNebula(P);
    ptData:   InitData(P);
    ptOrbit:  InitOrbit(P);
  end;
  FParticles[FCount] := P;
  Inc(FCount);
end;

procedure TParticleSystem.Burst(const AX, AY: Single; const ACount: Integer);
var
  I: Integer;
  P: TParticle;
  Angle, Speed: Single;
  OldCX, OldCY: Single;
begin
  OldCX := FCenterX;
  OldCY := FCenterY;
  FCenterX := AX;
  FCenterY := AY;
  for I := 0 to ACount - 1 do
  begin
    if FCount >= FMaxParticles then Break;
    FillChar(P, SizeOf(P), 0);
    Angle := (I / ACount) * 2 * Pi + Random * 0.5;
    Speed := 100 + Random * 300;
    P.X := AX + (Random - 0.5) * 10;
    P.Y := AY + (Random - 0.5) * 10;
    P.VX := Cos(Angle) * Speed;
    P.VY := Sin(Angle) * Speed;
    P.MaxLife := 0.4 + Random * 0.8;
    P.Life := P.MaxLife;
    P.Size := 2 + Random * 6;
    P.Color := RandomColor;
    P.Color2 := Theme.Palette.Accent3;
    P.RotSpeed := (Random - 0.5) * 15;
    P.ParticleType := ptExplosion;
    FParticles[FCount] := P;
    Inc(FCount);
  end;
  FCenterX := OldCX;
  FCenterY := OldCY;
end;

procedure TParticleSystem.UpdateSpark(var P: TParticle; const DT: Single);
begin
  P.VY := P.VY + 60 * DT;
  P.VX := P.VX * (1 - 0.5 * DT);
  P.X := P.X + P.VX * DT;
  P.Y := P.Y + P.VY * DT;
  P.Rotation := P.Rotation + P.RotSpeed * DT;
  P.Life := P.Life - DT;
end;

procedure TParticleSystem.UpdateNebula(var P: TParticle; const DT: Single);
begin
  P.X := P.X + P.VX * DT;
  P.Y := P.Y + P.VY * DT;
  P.VX := P.VX + (FCenterX - P.X) * 0.001;
  P.VY := P.VY + (FCenterY - P.Y) * 0.001;
  P.Life := P.Life - DT * 0.1;
end;

procedure TParticleSystem.UpdateData(var P: TParticle; const DT: Single);
begin
  P.Y := P.Y + P.VY * DT;
  P.Life := P.Life - DT;
  if Random < 0.02 then
    P.Char := Chr(Ord('A') + Random(26));
end;

procedure TParticleSystem.UpdateOrbit(var P: TParticle; const DT: Single);
begin
  P.OrbitAngle := P.OrbitAngle + P.OrbitSpeed * DT;
  P.Orbit := P.Orbit + Sin(FTime * 2 + P.OrbitAngle) * 0.2;
  P.X := FCenterX + Cos(P.OrbitAngle) * P.Orbit;
  P.Y := FCenterY + Sin(P.OrbitAngle) * P.Orbit;
  P.Life := P.Life - DT * 0.05;
end;

procedure TParticleSystem.Update(const ADeltaTime: Single);
var
  I: Integer;
  DT: Single;
begin
  DT := Min(ADeltaTime, 0.033);
  FTime := FTime + DT;

  FEmitAccum := FEmitAccum + FEmitRate * DT;
  while FEmitAccum >= 1.0 do
  begin
    EmitParticle;
    FEmitAccum := FEmitAccum - 1.0;
  end;

  I := 0;
  while I < FCount do
  begin
    case FParticles[I].ParticleType of
      ptSpark, ptExplosion: UpdateSpark(FParticles[I], DT);
      ptNebula:  UpdateNebula(FParticles[I], DT);
      ptData:    UpdateData(FParticles[I], DT);
      ptOrbit:   UpdateOrbit(FParticles[I], DT);
    end;

    if FParticles[I].Life <= 0 then
    begin
      FParticles[I] := FParticles[FCount - 1];
      Dec(FCount);
    end
    else
      Inc(I);
  end;
end;

procedure TParticleSystem.Draw(const ACanvas: ISkCanvas);
var
  I: Integer;
  P: TParticle;
  LAlpha: Single;
  LSize: Single;
  LPaint: ISkPaint;
  LGlowPaint: ISkPaint;
  LFontPaint: ISkPaint;
  LFont: ISkFont;

  // Função helper definitiva para qualquer versão de Delphi
  function ToAlphaColor(const R, G, B, A: Byte): TAlphaColor;
  var
    LRec: TAlphaColorRec;
  begin
    LRec.R := R;
    LRec.G := G;
    LRec.B := B;
    LRec.A := A;
    Result := TAlphaColor(LRec);
  end;

  function ChangeAlpha(const AColor: TAlphaColor; const AAlpha: Byte): TAlphaColor;
  var
    LRec: TAlphaColorRec;
  begin
    LRec := TAlphaColorRec(AColor);
    LRec.A := AAlpha;
    Result := TAlphaColor(LRec);
  end;

begin
  if FCount <= 0 then Exit;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LGlowPaint := TSkPaint.Create;
  LGlowPaint.AntiAlias := True;
  LFontPaint := TSkPaint.Create;
  LFontPaint.AntiAlias := True;

  for I := 0 to FCount - 1 do
  begin
    P := FParticles[I];
    LAlpha := EnsureRange(P.Life / Max(0.001, P.MaxLife), 0, 1);

    case P.ParticleType of
      ptSpark, ptExplosion:
      begin
        LSize := P.Size * LAlpha;
        LGlowPaint.Color := ChangeAlpha(P.Color, Round(LAlpha * 80));
        LGlowPaint.ImageFilter := TSkImageFilter.MakeBlur(LSize * 2, LSize * 2, nil, TSkTileMode.Decal);
        ACanvas.DrawCircle(P.X, P.Y, LSize * 2.5, LGlowPaint);

        LPaint.Color := ChangeAlpha(P.Color, Round(LAlpha * 255));
        ACanvas.DrawCircle(P.X, P.Y, Max(0.5, LSize), LPaint);
      end;

      ptNebula:
      begin
        LGlowPaint.Color := ChangeAlpha(P.Color, Round(LAlpha * LAlpha * 30));
        LGlowPaint.ImageFilter := TSkImageFilter.MakeBlur(P.Size * 0.5, P.Size * 0.5, nil, TSkTileMode.Decal);
        ACanvas.DrawCircle(P.X, P.Y, P.Size, LGlowPaint);
      end;

      ptData:
      begin
        LFont := TSkFont.Create(TSkTypeface.MakeDefault, P.Size);

        // CORRIGIDO: Usando a nossa função helper para criar o verde
        LFontPaint.Color := ToAlphaColor(220, 255, 220, Round(LAlpha * 255));
        ACanvas.DrawSimpleText(P.Char, P.X, P.Y, LFont, LFontPaint);

        LFontPaint.Color := ChangeAlpha(P.Color, Round(LAlpha * 150));
        ACanvas.DrawSimpleText(P.Char, P.X, P.Y + P.Size * 1.2, LFont, LFontPaint);
      end;

      ptOrbit:
      begin
        LSize := P.Size;
        LGlowPaint.Color := ChangeAlpha(P.Color, Round(LAlpha * 60));
        LGlowPaint.ImageFilter := TSkImageFilter.MakeBlur(LSize * 3, LSize * 3, nil, TSkTileMode.Decal);
        ACanvas.DrawCircle(P.X, P.Y, LSize * 3, LGlowPaint);

        LPaint.Color := ChangeAlpha(P.Color, Round(LAlpha * 200));
        ACanvas.DrawCircle(P.X, P.Y, LSize, LPaint);
      end;
    end;
  end;
end;

end.
