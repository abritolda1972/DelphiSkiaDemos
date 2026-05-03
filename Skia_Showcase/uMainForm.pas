unit uMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math,
  System.DateUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  System.Skia,
  FMX.Skia,
  uThemeEngine,
  uDemoScenes,
  uShaderPainter,
  uAnimationEngine, FMX.Controls.Presentation;

type
  TNavItem = record
    ID: TSceneID;
    Label_: string;
    Icon: string;
    Color: TAlphaColor;
    IsActive: Boolean;
    HoverAnim: Single;
  end;

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FMainBox: Tskpaintbox;
    FTimer: TTimer;
    FSceneManager:  TSceneManager;
    FNavItems:      array[0..7] of TNavItem;
    FLastTick:      Int64;
    FDeltaTime:     Single;
    FFPS:           Single;
    FFPSTimer:      Single;
    FFrameCount:    Integer;
    FMouseX, FMouseY: Single;
    FHoveredNav:    Integer;
    FNavVisible:    Boolean;
    FNavWidth:      Single;
    FNavTargetW:    Single;
    FHeaderH:       Single;
    FFooterH:       Single;

    FSceneRect:     TRectF;
    FNavRect:       TRectF;
    FHeaderRect:    TRectF;
    FFooterRect:    TRectF;

    procedure SetupUI;
    procedure SetupNavItems;
    procedure ComputeLayout;
    procedure OnPaint(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure DrawHeader(const ACanvas: ISkCanvas);
    procedure DrawNavSidebar(const ACanvas: ISkCanvas);
    procedure DrawNavItem(const ACanvas: ISkCanvas; const AItem: TNavItem; const ABounds: TRectF; const AIsHot: Boolean);
    procedure DrawSceneArea(const ACanvas: ISkCanvas);
    procedure DrawFooter(const ACanvas: ISkCanvas);
    procedure DrawFPSOverlay(const ACanvas: ISkCanvas);
    procedure DrawBackgroundLayer(const ACanvas: ISkCanvas);
    procedure DrawCornerDecals(const ACanvas: ISkCanvas);
    procedure OnTimer(Sender: TObject);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    function GetNavItemBounds(const AIndex: Integer): TRectF;
    function HitTestNav(const AX, AY: Single): Integer;
    procedure SelectScene(const AIndex: Integer);
    procedure UpdateDeltaTime;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'Skia4Delphi Showcase — Cyberpunk Edition';
  Width := 1280;
  Height := 800;
  FHeaderH := 64;
  FFooterH := 44;
  FNavWidth := 200;
  FHoveredNav := -1;
  SetupUI;
  SetupNavItems;
  ComputeLayout;
  FSceneManager := TSceneManager.Create;
  FSceneManager.SetBounds(FSceneRect);
  SelectScene(0);
  Randomize;
  FLastTick := TThread.GetTickCount64;
  FTimer.Enabled := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FTimer.Enabled := False;
  FSceneManager.Free;
end;

procedure TMainForm.SetupUI;
begin
FMainBox := TSkPaintBox.Create(Self);
  FMainBox.Parent := Self;
  FMainBox.Align := TAlignLayout.Client;

  FMainBox.HitTest := True;
  FMainBox.CanFocus := True; // Essencial para capturar teclas e cliques
  FMainBox.AutoCapture := True;

  FMainBox.OnDraw := OnPaint;
  FMainBox.OnMouseMove := OnMouseMove;
  FMainBox.OnMouseDown := OnMouseDown;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 16;
  FTimer.OnTimer := OnTimer;

  // Adicione esta linha para garantir que o teclado funcione
  Self.ActiveControl := FMainBox;
end;

procedure TMainForm.SetupNavItems;
const
  SIDs: array[0..7] of TSceneID = (scParticles, scWaveforms, scGeometry, scGlass, scPlasma, scMatrix, scMandala, scSpectrum);
  SLbls: array[0..7] of string = ('Particles', 'Waveforms', 'Geometry', 'Glass UI', 'Plasma', 'Matrix', 'Mandala', 'Spectrum');
  SIcons: array[0..7] of string = ('✦', '∿', '⬡', '◈', '◉', '⊞', '✿', '▋');
  SCols: array[0..7] of TAlphaColor = ($FF00D4FF, $FFFF0090, $FF00FFCC, $FFFFC400, $FF00D4FF, $FF00FF41, $FFFF3366, $FF7C3AED);
begin
  for var I := 0 to 7 do
  begin
    FNavItems[I].ID := SIDs[I];
    FNavItems[I].Label_ := SLbls[I];
    FNavItems[I].Icon := SIcons[I];
    FNavItems[I].Color := SCols[I];
    FNavItems[I].IsActive := (I = 0);
    FNavItems[I].HoverAnim := 0;
  end;
end;

procedure TMainForm.ComputeLayout;
begin
  FHeaderRect := TRectF.Create(0, 0, Width, FHeaderH);
  FNavRect    := TRectF.Create(0, FHeaderH, FNavWidth, Height - FFooterH);
  FSceneRect  := TRectF.Create(FNavWidth, FHeaderH, Width, Height - FFooterH);
  FFooterRect := TRectF.Create(0, Height - FFooterH, Width, Height);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  ComputeLayout;
  if Assigned(FSceneManager) then FSceneManager.SetBounds(FSceneRect);
end;

procedure TMainForm.OnTimer(Sender: TObject);
begin
  UpdateDeltaTime;
 for var I := 0 to 7 do
  begin
    var Target := IfThen(I = FHoveredNav, 1.0, 0.0);
    FNavItems[I].HoverAnim := FNavItems[I].HoverAnim + (Target - FNavItems[I].HoverAnim) * Min(1, FDeltaTime * 8);
  end;

  if Assigned(FSceneManager) then
    FSceneManager.Update(FDeltaTime);

  Inc(FFrameCount);
  FFPSTimer := FFPSTimer + FDeltaTime;
  if FFPSTimer >= 1.0 then begin
    FFPS := FFrameCount / FFPSTimer;
    FFrameCount := 0;
    FFPSTimer := 0;
  end;
  FMainBox.Redraw;
end;

procedure TMainForm.UpdateDeltaTime;
begin
  var LNow := TThread.GetTickCount64;
  FDeltaTime := Min((LNow - FLastTick) / 1000.0, 0.05);
  FLastTick := LNow;
end;

procedure TMainForm.OnPaint(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  ACanvas.Clear(Theme.Palette.Background);
  DrawBackgroundLayer(ACanvas);
  DrawHeader(ACanvas);
  DrawNavSidebar(ACanvas);
  DrawSceneArea(ACanvas);
  DrawFooter(ACanvas);
  DrawCornerDecals(ACanvas);
end;

procedure TMainForm.DrawBackgroundLayer(const ACanvas: ISkCanvas);
var
  LPaint: ISkPaint;
  T: Single;
begin
  T := TThread.GetTickCount64 / 1000.0;
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Shader := TSkShader.MakeGradientRadial(
    TPointF.Create(Width * 0.5 + Sin(T * 0.2) * 200, Height * 0.3 + Cos(T * 0.15) * 100),
    Width * 0.7, [$08002244, $00000000], nil, TSkTileMode.Clamp);
  ACanvas.DrawRect(TRectF.Create(0, 0, Width, Height), LPaint);

  LPaint.Shader := TSkShader.MakeGradientRadial(
    TPointF.Create(Width * 0.8 + Sin(T * 0.1) * 100, Height * 0.8),
    Width * 0.5, [$06440022, $00000000], nil, TSkTileMode.Clamp);
  ACanvas.DrawRect(TRectF.Create(0, 0, Width, Height), LPaint);
end;

procedure TMainForm.DrawHeader(const ACanvas: ISkCanvas);
var
  LPaint, GP: ISkPaint;
  T: Single;
  SubFont: ISkFont;
begin
  T := TThread.GetTickCount64 / 1000.0;
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Shader := TSkShader.MakeGradientLinear(FHeaderRect.TopLeft,
    TPointF.Create(FHeaderRect.Right, FHeaderRect.Bottom),
    [$FF060D1F, $FF030810], nil, TSkTileMode.Clamp);
  ACanvas.DrawRect(FHeaderRect, LPaint);

  LPaint.Shader := nil;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 1;
  LPaint.Shader := TSkShader.MakeGradientLinear(
    TPointF.Create(FHeaderRect.Left, FHeaderRect.Bottom),
    TPointF.Create(FHeaderRect.Right * 0.7, FHeaderRect.Bottom),
    [$8800D4FF, $3300FFCC, $00000000], nil, TSkTileMode.Clamp);
  ACanvas.DrawLine(FHeaderRect.Left, FHeaderRect.Bottom, FHeaderRect.Right, FHeaderRect.Bottom, LPaint);

  var LogoX := 18.0;
  var LogoY := FHeaderH * 0.5;
  GP := TSkPaint.Create;
  GP.AntiAlias := True;
  GP.Shader := TSkShader.MakeGradientRadial(TPointF.Create(LogoX, LogoY), 18, [$FF00D4FF, $6600AAFF, $00000000], nil, TSkTileMode.Clamp);
  ACanvas.DrawCircle(LogoX, LogoY, 18, GP);
  TShaderPainter.DrawStar(ACanvas, LogoX, LogoY, 12, 6, 5, T * 1.5, $CCFFFFFF);
  TShaderPainter.DrawNeonText(ACanvas, 'SKIA4DELPHI', LogoX * 2 + 10, FHeaderH * 0.52 + 4, 20, Theme.Palette.Primary, 12);

  if Assigned(FSceneManager) then begin
    var Scene := FSceneManager.GetCurrentScene;
    var CX := FNavWidth + (Width - FNavWidth) * 0.5;
    TShaderPainter.DrawNeonText(ACanvas, Scene.Title, CX - 100, FHeaderH * 0.52 + 4, 16, Theme.Palette.Accent1, 8);
     SubFont := TSkFont.Create(TSkTypeface.MakeDefault, 9);
    var TW := SubFont.MeasureText(Scene.Subtitle);
    ACanvas.DrawSimpleText(Scene.Subtitle, CX - TW * 0.5, FHeaderH * 0.52 + 18, SubFont, TSkPaint.Create);
  end;
end;

procedure TMainForm.DrawNavSidebar(const ACanvas: ISkCanvas);
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Shader := TSkShader.MakeGradientLinear(FNavRect.TopLeft,
    TPointF.Create(FNavRect.Right, FNavRect.Top), [$FF050C1A, $FF030810], nil, TSkTileMode.Clamp);
  ACanvas.DrawRect(FNavRect, LPaint);

  LPaint.Shader := nil;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.Color := $2200D4FF;
  ACanvas.DrawLine(FNavRect.Right, FNavRect.Top, FNavRect.Right, FNavRect.Bottom, LPaint);

  for var I := 0 to 7 do DrawNavItem(ACanvas, FNavItems[I], GetNavItemBounds(I), I = FHoveredNav);
end;

function TMainForm.GetNavItemBounds(const AIndex: Integer): TRectF;
begin
  var StartY := FNavRect.Top + 38;
  Result := TRectF.Create(FNavRect.Left + 8, StartY + AIndex * 52, FNavRect.Right - 8, StartY + AIndex * 52 + 48);
end;

procedure TMainForm.DrawNavItem(const ACanvas: ISkCanvas; const AItem: TNavItem; const ABounds: TRectF; const AIsHot: Boolean);
var
  LPaint,PPaint: ISkPaint;
begin
  if AItem.IsActive then begin
    LPaint := TSkPaint.Create;
    LPaint.AntiAlias := True;
    var C := AItem.Color;
    LPaint.Shader := TSkShader.MakeGradientLinear(ABounds.TopLeft,
      TPointF.Create(ABounds.Right, ABounds.Top),
      [(C and $00FFFFFF) or ($1E shl 24), $00000000], nil, TSkTileMode.Clamp);
    ACanvas.DrawRoundRect(ABounds, 10, 10, LPaint);
  end;
  var LFont := TSkFont.Create(TSkTypeface.MakeDefault, 12);
  PPaint := TSkPaint.Create;
  PPaint.Color := IfThen(AItem.IsActive, Theme.Palette.TextPrimary, Theme.Palette.TextSecondary);
  ACanvas.DrawSimpleText(AItem.Label_, ABounds.Left + 38, ABounds.Top + 28, LFont, PPaint);
end;

procedure TMainForm.DrawSceneArea(const ACanvas: ISkCanvas);
begin
  ACanvas.Save;
  ACanvas.ClipRect(FSceneRect);
  if Assigned(FSceneManager) then FSceneManager.Draw(ACanvas);
  ACanvas.Restore;
end;

procedure TMainForm.DrawFooter(const ACanvas: ISkCanvas);
var LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Color := $FF040B19;
  ACanvas.DrawRect(FFooterRect, LPaint);
  TShaderPainter.DrawNeonText(ACanvas, Format('%.0f FPS', [FFPS]), Width - 90, FFooterRect.Bottom - 10, 12, Theme.Palette.Accent1, 5);
end;

procedure TMainForm.DrawFPSOverlay(const ACanvas: ISkCanvas); begin end;

procedure TMainForm.DrawCornerDecals(const ACanvas: ISkCanvas);
var LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.Color := $3300D4FF;
  ACanvas.DrawRect(FSceneRect, LPaint);
end;

procedure TMainForm.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  FMouseX := X; FMouseY := Y;
  FHoveredNav := HitTestNav(X, Y);
end;

procedure TMainForm.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin

  var NavIdx := HitTestNav(X, Y);
  if NavIdx >= 0 then SelectScene(NavIdx)
  else if FSceneRect.Contains(TPointF.Create(X, Y)) and Assigned(FSceneManager) then FSceneManager.OnClick(X, Y);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (KeyChar >= '1') and (KeyChar <= '8') then SelectScene(Ord(KeyChar) - Ord('1'));
end;

function TMainForm.HitTestNav(const AX, AY: Single): Integer;
begin
  Result := -1;
  for var I := 0 to 7 do if GetNavItemBounds(I).Contains(TPointF.Create(AX, AY)) then Exit(I);
end;

procedure TMainForm.SelectScene(const AIndex: Integer);
const SIDs: array[0..7] of TSceneID = (scParticles, scWaveforms, scGeometry, scGlass, scPlasma, scMatrix, scMandala, scSpectrum);
begin
  if (AIndex < 0) or (AIndex > 7) then Exit;
  for var I := 0 to 7 do FNavItems[I].IsActive := (I = AIndex);
  if Assigned(FSceneManager) then FSceneManager.SwitchTo(SIDs[AIndex]);
end;

end.
