unit UnMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia;

type
  TForm1 = class(TForm)
    SkAnimatedPaintBox1: TSkAnimatedPaintBox;
    procedure SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
      const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
      const AOpacity: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);

const flame1 =
'''
  uniform float3 iResolution;      // Viewport resolution (pixels)
  uniform float  iTime;            // Shader playback time (s)
  const vec3 c = vec3(1, 0, -1);
  const mat2 m = .4 * mat2(4, 3, -3, 4);

  float hash12(vec2 p)
  {
      vec3 p3  = fract(vec3(p.xyx) * .1031);
      p3 += dot(p3, p3.yzx + 33.33);
      return fract(dot(p3.xy, p3.zz));
  }

  float lfnoise(vec2 t)
  {
      vec2 i = floor(t);
      t = c.xx * smoothstep(0., 1., fract(t));
      vec2 v1 = 2. * mix(vec2(hash12(i), hash12(i + c.xy)),
                       vec2(hash12(i + c.yx), hash12(i + c.xx)), t.y) - 1.;
      return mix(v1.x, v1.y, t.x);
  }

  float fbm(vec2 uv)
  {
      vec2 uv0 = uv;
      // Aumento da escala Y para comprimir verticalmente
      uv = uv * vec2(5., 3.5) - vec2(-2., -.25) - 3.1 * iTime * c.yx;
      float f = 1.,
            a = .5,
            c = 2.5;

      for(int i = 0; i < 5; ++i) {
          // Redução do deslocamento horizontal
          uv.x -= .12 * clamp(1. - pow(uv0.y, 4.), 0., 1.0) * lfnoise(c * (uv + float(i) * .612 + iTime));
          c *= 2.;
          f += a * lfnoise(uv + float(i) * .415);
          a /= 2.;
          uv *= m;
      }
      return f / 2.;
  }

  vec4 main(vec2 fragCoord)
  {
      // Coordenadas com origem no bottom-left
      vec2 uv = vec2(fragCoord.x, iResolution.y - fragCoord.y) / iResolution.xy;

      // Controle de altura mais agressivo
      float verticalFade = 1.0 - smoothstep(0.3, 0.5, uv.y); // Começa a desaparecer aos 30%
      verticalFade = pow(verticalFade, 2.0); // Fade mais abrupto

      vec3 color = clamp(1.5 * pow(
          clamp(pow(fbm(uv), 1.5 + 65.0 * clamp(uv.y * uv.y, 0., 1.)) * 1.2,
          0., 1.) * c.xxx,
          vec3(1, 3, 6)),
          0., 1.);

      // Alpha com dois controles
      float alpha = smoothstep(0.2, 0.6, length(color)) * verticalFade;
      alpha *= step(uv.y, 0.5); // Corte total na metade

      return vec4(color, alpha);
  }
''';

var
  Ceffect :Iskruntimeeffect;
  CShaderBuilder: Iskruntimeshaderbuilder;
  CShader: Iskshader;
  ErrorMsg: String;
  APaint: Iskpaint;
begin
Ceffect:= Tskruntimeeffect.makeforshader(flame1,ErrorMsg);
   CShaderBuilder:=  TSkRuntimeShaderBuilder.Create(Ceffect);
   CShader:=CshaderBuilder.MakeShader;
if assigned(CShaderBuilder) then
Begin
  if(Ceffect.UniformExists('iResolution')) then
    CShaderBuilder.SetUniform('iResolution',[SkanimatedPaintBox1.Width, SkanimatedPaintBox1.Height,0]);
  if(Ceffect.UniformExists('iTime')) then
    CShaderBuilder.SetUniform('iTime', SkanimatedPaintBox1.Animation.CurrentTime);


 CShader:=CshaderBuilder.MakeShader;
    Apaint:= Tskpaint.Create;
    Apaint.Shader:= Cshader;
    Acanvas.Save;
      try
      Acanvas.ClipRect(adest);
      acanvas.DrawPaint(Apaint);

    finally
      Acanvas.Restore;
    end;
End;

end;

end.
