unit uAnimationEngine;

{
  uAnimationEngine.pas
  ====================
  Skia4Delphi Showcase - High-Performance Animation Engine
  Provides easing functions, tweens, and timeline management
  for smooth 60fps animations.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  FMX.Types;

type
  TEasingType = (
    etLinear,
    etQuadIn, etQuadOut, etQuadInOut,
    etCubicIn, etCubicOut, etCubicInOut,
    etElasticOut, etBounceOut,
    etBackOut, etExpoOut,
    etSineInOut, etCircOut
  );

  TTweenUpdateProc = reference to procedure(const AValue: Single);
  TTweenCompleteProc = reference to procedure;

  TTween = class
  private
    FStartTime: Int64;
    FDuration:  Single;  // seconds
    FFrom:      Single;
    FTo:        Single;
    FEasing:    TEasingType;
    FOnUpdate:  TTweenUpdateProc;
    FOnComplete: TTweenCompleteProc;
    FLoop:      Boolean;
    FReverse:    Boolean;
    FActive:    Boolean;
    FPingPong:  Boolean;
    FDelay:      Single;
    function ApplyEasing(const T: Single): Single;
  public
    constructor Create(const AFrom, ATo, ADuration: Single;
      const AEasing: TEasingType = etCubicOut);
    procedure Start;
    procedure Stop;
    procedure Reset;
    function Update(const ACurrentTimeMs: Int64): Boolean; // returns True if still active
    property Loop: Boolean read FLoop write FLoop;
    property PingPong: Boolean read FPingPong write FPingPong;
    property Delay: Single read FDelay write FDelay;
    property Active: Boolean read FActive;
    property OnUpdate: TTweenUpdateProc read FOnUpdate write FOnUpdate;
    property OnComplete: TTweenCompleteProc read FOnComplete write FOnComplete;
  end;

  TAnimationEngine = class
  private
    class var FInstance: TAnimationEngine;
    FTweens: TObjectList<TTween>;
    FTimer: TTimer;
    FStartTime: Int64;
    procedure OnTimer(Sender: TObject);
    function GetOnUpdate: TNotifyEvent;
    procedure SetOnUpdate(const Value: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TAnimationEngine;
    class destructor Destroy;

    function AddTween(const AFrom, ATo, ADuration: Single;
      const AEasing: TEasingType;
      const AOnUpdate: TTweenUpdateProc;
      const AOnComplete: TTweenCompleteProc = nil): TTween;

    function CreateLoopTween(const ADuration: Single;
      const AEasing: TEasingType;
      const AOnUpdate: TTweenUpdateProc): TTween;

    procedure RemoveTween(ATween: TTween);
    procedure Clear;

    property OnUpdate: TNotifyEvent read GetOnUpdate write SetOnUpdate; // Hook for scene updates
    function ElapsedMs: Int64;
  end;

// Standalone easing functions
function EaseLinear(t: Single): Single;
function EaseQuadOut(t: Single): Single;
function EaseCubicOut(t: Single): Single;
function EaseElasticOut(t: Single): Single;
function EaseBounceOut(t: Single): Single;
function EaseBackOut(t: Single): Single;
function EaseExpoOut(t: Single): Single;
function EaseSineInOut(t: Single): Single;
function EaseCircOut(t: Single): Single;
function Lerp(const A, B, T: Single): Single; inline;
function SmoothStep(const Edge0, Edge1, X: Single): Single;
function Pulse(const T, Frequency: Single): Single; inline;
function NoiseLike(const X, Y, T: Single): Single;

implementation

uses
  System.DateUtils,
  System.TimeSpan;

{ Easing Functions }

function EaseLinear(t: Single): Single;
begin
  Result := t;
end;

function EaseQuadOut(t: Single): Single;
begin
  Result := 1 - (1 - t) * (1 - t);
end;

function EaseCubicOut(t: Single): Single;
var
  t1: Single;
begin
  t1 := 1 - t;
  Result := 1 - t1 * t1 * t1;
end;

function EaseElasticOut(t: Single): Single;
const
  C4 = (2 * Pi) / 3;
begin
  if t = 0 then Result := 0
  else if t = 1 then Result := 1
  else
    Result := Power(2, -10 * t) * Sin((t * 10 - 0.75) * C4) + 1;
end;

function EaseBounceOut(t: Single): Single;
const
  N1 = 7.5625;
  D1 = 2.75;
begin
  if t < 1 / D1 then
    Result := N1 * t * t
  else if t < 2 / D1 then
  begin
    t := t - (1.5 / D1);
    Result := N1 * t * t + 0.75;
  end
  else if t < 2.5 / D1 then
  begin
    t := t - (2.25 / D1);
    Result := N1 * t * t + 0.9375;
  end
  else
  begin
    t := t - (2.625 / D1);
    Result := N1 * t * t + 0.984375;
  end;
end;

function EaseBackOut(t: Single): Single;
const
  C1 = 1.70158;
  C3 = C1 + 1;
begin
  Result := 1 + C3 * Power(t - 1, 3) + C1 * Power(t - 1, 2);
end;

function EaseExpoOut(t: Single): Single;
begin
  if t = 1 then Result := 1
  else Result := 1 - Power(2, -10 * t);
end;

function EaseSineInOut(t: Single): Single;
begin
  Result := -(Cos(Pi * t) - 1) / 2;
end;

function EaseCircOut(t: Single): Single;
begin
  Result := Sqrt(1 - Power(t - 1, 2));
end;

function Lerp(const A, B, T: Single): Single;
begin
  Result := A + (B - A) * T;
end;

function SmoothStep(const Edge0, Edge1, X: Single): Single;
var
  t: Single;
begin
  t := EnsureRange((X - Edge0) / (Edge1 - Edge0), 0, 1);
  Result := t * t * (3 - 2 * t);
end;

function Pulse(const T, Frequency: Single): Single;
begin
  Result := (Sin(T * Frequency * 2 * Pi) + 1) * 0.5;
end;

function NoiseLike(const X, Y, T: Single): Single;
begin
  Result := (
    Sin(X * 1.234 + T * 0.7) * 0.5 +
    Sin(Y * 0.987 + T * 1.3) * 0.3 +
    Sin((X + Y) * 0.567 + T * 0.5) * 0.2
  ) * 0.5 + 0.5;
end;

{ TTween }

constructor TTween.Create(const AFrom, ATo, ADuration: Single; const AEasing: TEasingType);
begin
  inherited Create;
  FFrom := AFrom;
  FTo := ATo;
  FDuration := ADuration;
  FEasing := AEasing;
  FActive := False;
  FLoop := False;
  FPingPong := False;
  FReverse := False;
  FDelay := 0;
end;

function TTween.ApplyEasing(const T: Single): Single;
begin
  case FEasing of
    etLinear:    Result := EaseLinear(T);
    etQuadIn:    Result := T * T;
    etQuadOut:   Result := EaseQuadOut(T);
    etQuadInOut: Result := EaseSineInOut(T);
    etCubicIn:   Result := T * T * T;
    etCubicOut:  Result := EaseCubicOut(T);
    etCubicInOut:
    begin
      if T < 0.5 then Result := 4 * T * T * T
      else Result := 1 - Power(-2 * T + 2, 3) / 2;
    end;
    etElasticOut: Result := EaseElasticOut(T);
    etBounceOut:  Result := EaseBounceOut(T);
    etBackOut:    Result := EaseBackOut(T);
    etExpoOut:    Result := EaseExpoOut(T);
    etSineInOut:  Result := EaseSineInOut(T);
    etCircOut:    Result := EaseCircOut(T);
  else
    Result := T;
  end;
end;

procedure TTween.Start;
begin
  FStartTime := TThread.GetTickCount64;
  FActive := True;
  FReverse := False;
end;

procedure TTween.Stop;
begin
  FActive := False;
end;

procedure TTween.Reset;
begin
  FStartTime := TThread.GetTickCount64;
  FReverse := False;
end;

function TTween.Update(const ACurrentTimeMs: Int64): Boolean;
var
  LElapsed, LNorm, LEased, LValue: Single;
begin
  if not FActive then Exit(False);

  LElapsed := (ACurrentTimeMs - FStartTime) / 1000.0 - FDelay;
  if LElapsed < 0 then
  begin
    if Assigned(FOnUpdate) then FOnUpdate(FFrom);
    Exit(True);
  end;

  LNorm := LElapsed / FDuration;

  if LNorm >= 1.0 then
  begin
    if FPingPong and not FReverse then
    begin
      FReverse := True;
      FStartTime := ACurrentTimeMs;
      if Assigned(FOnUpdate) then FOnUpdate(FTo);
      Exit(True);
    end
    else if FLoop or (FPingPong and FReverse) then
    begin
      FStartTime := ACurrentTimeMs;
      FReverse := False;
      if Assigned(FOnUpdate) then FOnUpdate(FFrom);
      Exit(True);
    end
    else
    begin
      if Assigned(FOnUpdate) then FOnUpdate(FTo);
      if Assigned(FOnComplete) then FOnComplete;
      FActive := False;
      Exit(False);
    end;
  end;

  if FReverse then LNorm := 1.0 - LNorm;

  LEased := ApplyEasing(EnsureRange(LNorm, 0, 1));
  LValue := Lerp(FFrom, FTo, LEased);
  if Assigned(FOnUpdate) then FOnUpdate(LValue);
  Result := True;
end;

{ TAnimationEngine }

constructor TAnimationEngine.Create;
begin
  inherited;
  FTweens := TObjectList<TTween>.Create(True);
  FStartTime := TThread.GetTickCount64;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 16;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := True;
end;

destructor TAnimationEngine.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  FTweens.Free;
  inherited;
end;

function TAnimationEngine.GetOnUpdate: TNotifyEvent;
begin
  Result := FTimer.OnTimer;
end;

procedure TAnimationEngine.SetOnUpdate(const Value: TNotifyEvent);
begin
  FTimer.OnTimer := Value;
end;

class function TAnimationEngine.Instance: TAnimationEngine;
begin
  if not Assigned(FInstance) then
    FInstance := TAnimationEngine.Create;
  Result := FInstance;
end;

class destructor TAnimationEngine.Destroy;
begin
  FreeAndNil(FInstance);
end;

procedure TAnimationEngine.OnTimer(Sender: TObject);
var
  LNow: Int64;
  I: Integer;
begin
  LNow := TThread.GetTickCount64;
  I := 0;
  while I < FTweens.Count do
  begin
    if not FTweens[I].Update(LNow) then
      FTweens.Delete(I)
    else
      Inc(I);
  end;
end;

function TAnimationEngine.AddTween(const AFrom, ATo, ADuration: Single;
  const AEasing: TEasingType; const AOnUpdate: TTweenUpdateProc;
  const AOnComplete: TTweenCompleteProc): TTween;
begin
  Result := TTween.Create(AFrom, ATo, ADuration, AEasing);
  Result.OnUpdate := AOnUpdate;
  Result.OnComplete := AOnComplete;
  FTweens.Add(Result);
  Result.Start;
end;

function TAnimationEngine.CreateLoopTween(const ADuration: Single;
  const AEasing: TEasingType; const AOnUpdate: TTweenUpdateProc): TTween;
begin
  Result := TTween.Create(0, 1, ADuration, AEasing);
  Result.OnUpdate := AOnUpdate;
  Result.Loop := True;
  FTweens.Add(Result);
  Result.Start;
end;

procedure TAnimationEngine.RemoveTween(ATween: TTween);
begin
  FTweens.Remove(ATween);
end;

procedure TAnimationEngine.Clear;
begin
  FTweens.Clear;
end;

function TAnimationEngine.ElapsedMs: Int64;
begin
  Result := TThread.GetTickCount64 - FStartTime;
end;

end.
