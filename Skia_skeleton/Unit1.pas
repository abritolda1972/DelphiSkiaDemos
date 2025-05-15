unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, System.Math.Vectors, FMX.Objects, FMX.Effects, System.Math;
type
  TSkeletonElementKind = (Rectangle, Circle);

  TSkeletonElement = record
    Kind: TSkeletonElementKind;
    Bounds: TRectF;
  end;

type
  TForm1 = class(TForm)
    SkAnimatedPaintBox1: TSkAnimatedPaintBox;
    Rectangle1: TRectangle;
    ShadowEffect1: TShadowEffect;
    Rectangle2: TRectangle;
    SkAnimatedPaintBox2: TSkAnimatedPaintBox;
    ShadowEffect2: TShadowEffect;
    Rectangle3: TRectangle;
    SkAnimatedPaintBox3: TSkAnimatedPaintBox;
    ShadowEffect3: TShadowEffect;
    procedure FormCreate(Sender: TObject);
    procedure SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
      const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
      const AOpacity: Single);
    procedure SkAnimatedPaintBox2AnimationDraw(ASender: TObject;
      const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
      const AOpacity: Single);
    procedure SkAnimatedPaintBox3AnimationDraw(ASender: TObject;
      const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
      const AOpacity: Single);
  private
    { Private declarations }
    FSkeletonElements: TArray<TSkeletonElement>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
   // Define as posições dos elementos do esqueleto
   // Configura elementos do esqueleto
  SetLength(FSkeletonElements, 5);

  // Retângulos
  FSkeletonElements[0] := Default(TSkeletonElement);
  FSkeletonElements[0].Kind := TSkeletonElementKind.Rectangle;
  FSkeletonElements[0].Bounds := TRectF.Create(20, 20, 280, 60);   // Cabeçalho

  FSkeletonElements[1] := Default(TSkeletonElement);
  FSkeletonElements[1].Kind := TSkeletonElementKind.Rectangle;
  FSkeletonElements[1].Bounds := TRectF.Create(20, 80, 280, 100);  // Linha 1

  // Círculos
  FSkeletonElements[2] := Default(TSkeletonElement);
  FSkeletonElements[2].Kind := TSkeletonElementKind.Circle;
  FSkeletonElements[2].Bounds := TRectF.Create(20, 120, 60, 160);  // Círculo 1

  FSkeletonElements[3] := Default(TSkeletonElement);
  FSkeletonElements[3].Kind := TSkeletonElementKind.Circle;
  FSkeletonElements[3].Bounds := TRectF.Create(80, 120, 120, 160); // Círculo 2

  FSkeletonElements[4] := Default(TSkeletonElement);
  FSkeletonElements[4].Kind := TSkeletonElementKind.Rectangle;
  FSkeletonElements[4].Bounds := TRectF.Create(125, 120, 280, 160);  // Linha 2
end;

procedure TForm1.SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
var
  LPaint: ISkPaint;
  LShader: ISkShader;
  LMatrix: TMatrix;
  LProgress: Single;
begin
  // Desenha os elementos do esqueleto
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColor($FFE0E0E0); // Cinza claro

  for var Element in FSkeletonElements do
  begin
    case Element.Kind of
      TSkeletonElementKind.Rectangle:
        ACanvas.DrawRoundRect(Element.Bounds, 4, 4, LPaint);

      TSkeletonElementKind.Circle:
        ACanvas.DrawOval(Element.Bounds, LPaint);
    end;
  end;


  // Ajuste principal: Progresso linear contínuo (0..1..0)
  LProgress := Frac(AProgress * 3); // 2 ciclos completos por segundo
  // Ou para velocidade fixa:
  // LProgress := Frac(AProgress * 1.5); // Ajuste o multiplicador para velocidade

  // Cria o gradiente para o efeito de brilho
  const Colors: TArray<TAlphaColor> = [$10E0E0E0, $65FFFFFF, $10E0E0E0];
  const Pos: TArray<Single> = [0, 0.5, 1]; // Posições fixas

  LShader := TSkShader.MakeGradientLinear(
    PointF(0, 0),
    PointF(100, 0),
    Colors,
    Pos,
    TSKTileMode.Clamp
  );

  // Calcula a posição do gradiente
  LMatrix := TMatrix.CreateTranslation(
    -100 + (LProgress * (SkAnimatedPaintBox1.Width + 200)),
    0
  );

  // Aplica a matriz e desenha
  LPaint := TSkPaint.Create;
  LPaint.Shader := LShader.MakeWithLocalMatrix(LMatrix);
  LPaint.Blender:= Tskblender.MakeMode(TSkBlendMode.SrcATop); // Corrigido o nome da propriedade

  // Desenha o brilho sobre os elementos
  for var Element in FSkeletonElements do
  begin
    case Element.Kind of
      TSkeletonElementKind.Rectangle:
        ACanvas.DrawRoundRect(Element.Bounds, 4, 4, LPaint);

      TSkeletonElementKind.Circle:
        ACanvas.DrawOval(Element.Bounds, LPaint);
    end;
  end;
end;

procedure TForm1.SkAnimatedPaintBox2AnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
const
  LINE_SPEED = 1;
  PEAK_HEIGHT = 12.0;
  PEAK_WIDTH = 0.1;  // 10% da largura do elemento para cada pico
var
  LPaint: ISkPaint;
  LPathBuilder: Iskpathbuilder;
begin
  // 1. Desenha os elementos estáticos primeiro
  LPaint := TSkPaint.Create;
  LPaint.Color := $FFE0E0E0;

  for var Element in FSkeletonElements do
  begin
    case Element.Kind of
      TSkeletonElementKind.Rectangle:
        ACanvas.DrawRoundRect(Element.Bounds, 4, 4, LPaint);
      TSkeletonElementKind.Circle:
        ACanvas.DrawOval(Element.Bounds, LPaint);
    end;
  end;

  // 2. Configuração do estilo da linha
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Red;
  LPaint.StrokeWidth := 2;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeCap := TSkStrokeCap.Round;
  LPaint.AntiAlias := True;

  // 3. Para cada elemento, desenha a linha animada
  for var Element in FSkeletonElements do
  begin
   LPathBuilder := TSkPathBuilder.Create;
    try
      var LProgress := AProgress * LINE_SPEED;
      var LElementRect := Element.Bounds;

      // Pontos principais com picos mais estreitos
      var LPoints: TArray<TPointF> := [
        // Primeiro pico (centro do elemento)
        PointF(LElementRect.Left + (LElementRect.Width * 0.45), LElementRect.CenterPoint.Y),  // Início pico
        PointF(LElementRect.Left + (LElementRect.Width * 0.50), LElementRect.CenterPoint.Y - PEAK_HEIGHT),  // Topo pico
        PointF(LElementRect.Left + (LElementRect.Width * 0.55), LElementRect.CenterPoint.Y),  // Fim pico

        // Segundo pico (últimos 10% do elemento)
        PointF(LElementRect.Left + (LElementRect.Width * 0.85), LElementRect.CenterPoint.Y),  // Início pico
        PointF(LElementRect.Left + (LElementRect.Width * 0.90), LElementRect.CenterPoint.Y - PEAK_HEIGHT),  // Topo pico
        PointF(LElementRect.Left + (LElementRect.Width * 0.95), LElementRect.CenterPoint.Y)   // Fim pico
      ];

      var LX := LElementRect.Left + (LElementRect.Width * LProgress);

      // Começa na esquerda do elemento
      LPathBuilder.MoveTo(LElementRect.Left, LElementRect.CenterPoint.Y);

      // Desenha até a posição atual do progresso
      var I := 0;
      while (I < Length(LPoints)) and (LX > LPoints[I].X) do
      begin
        LPathBuilder.LineTo(LPoints[I]);
        Inc(I);
      end;

      // Desenha o segmento parcial se necessário
      if (I < Length(LPoints)) and (LX <= LPoints[I].X) then
      begin
        if I > 0 then
        begin
          var T := (LX - LPoints[I-1].X) / (LPoints[I].X - LPoints[I-1].X);
          T := EnsureRange(T, 0, 1);
          var CurrentY := LPoints[I-1].Y + T * (LPoints[I].Y - LPoints[I-1].Y);
          LPathBuilder.LineTo(LX, CurrentY);
        end
        else
        begin
          LPathBuilder.LineTo(LX, LElementRect.CenterPoint.Y);
        end;
      end
      else
      begin
        LPathBuilder.LineTo(LX, LElementRect.CenterPoint.Y);
      end;

      Lpaint.ImageFilter:= Tskimagefilter.MakeBlur(1,1);

      var LPath := LPathBuilder.Detach;
      ACanvas.DrawPath(LPath, LPaint);

    finally
      LPathBuilder := nil;
    end;
  end;
end;



procedure TForm1.SkAnimatedPaintBox3AnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColorF.Create(224,224,224,
                   (Sin(AProgress * Pi *1) + 1) / 9 + 0.005).ToAlphaColor;

  for var Element in FSkeletonElements do
    case Element.Kind of
      TSkeletonElementKind.Rectangle: ACanvas.DrawRoundRect(Element.Bounds, 4, 4, LPaint);
      TSkeletonElementKind.Circle:    ACanvas.DrawOval(Element.Bounds, LPaint);
    end;

end;

end.
