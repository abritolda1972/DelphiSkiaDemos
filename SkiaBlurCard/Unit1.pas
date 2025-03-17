unit Unit1;

interface

uses
  System.SysUtils, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia,System.Types;

type
  TForm1 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.fmx}

procedure TForm1.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
  var apaint: iskpaint;
      compositeRuntimeEffect : IskruntimeEffect;
      compositeShaderBuilder : IskRuntimeshaderbuilder;
  const
  compositeSksl =
  '''
    uniform shader content;
    uniform shader blur;
    uniform shader noise;

    uniform vec4 rectangle;
    uniform float radius;

    uniform float dropShadowSize;

    // Simplified version of SDF (signed distance function) for a rounded box
    // from https://www.iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm
    float roundedRectangleSDF(vec2 position, vec2 box, float radius) {
        vec2 q = abs(position) - box + vec2(radius);
        return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - radius;
    }

    vec4 main(vec2 coord) {
        vec2 shiftRect = (rectangle.zw - rectangle.xy) / 2.0;
        vec2 shiftCoord = coord - rectangle.xy;
        float distanceToClosestEdge = roundedRectangleSDF(
            shiftCoord - shiftRect, shiftRect, radius);

        vec4 c = content.eval(coord);
        if (distanceToClosestEdge > 0.0) {
            // We're outside of the filtered area
            if (distanceToClosestEdge < dropShadowSize) {
                // Emulate drop shadow around the filtered area
                float darkenFactor = (dropShadowSize - distanceToClosestEdge) / dropShadowSize;
                // Use exponential drop shadow decay for more pleasant visuals
                darkenFactor = pow(darkenFactor, 1.6);
                // Shift towards black, by 10% around the edge, dissipating to 0% further away
                return c * (0.9 + (1.0 - darkenFactor) / 10.0);
            }
            return c;
        }

        vec4 b = blur.eval(coord);
        vec4 n = noise.eval(coord);
        // How far are we from the top-left corner?
        float lightenFactor = min(1, length(coord - rectangle.xy) / (0.8 * length(rectangle.zw - rectangle.xy)));
        // Add some noise for extra texture
        float noiseLuminance = dot(n.rgb, vec3(0.2126, 0.7152, 0.0722));
        lightenFactor = min(1, lightenFactor + noiseLuminance);
        // Shift towards white, by 35% in top left corner, down to 10% in bottom right corner
        return b + (vec4(0.3) - b) * (0.35 - 0.25 * lightenFactor);
    }
  ''';
begin
apaint:= Tskpaint.Create;
apaint.AntiAlias:=true;
compositeRuntimeEffect :=Tskruntimeeffect.MakeForShader(compositeSksl);
compositeShaderBuilder:= TskRuntimeshaderbuilder.Create(compositeRuntimeEffect);

compositeShaderBuilder.SetUniform('rectangle',[85,110,405,290]);
compositeShaderBuilder.SetUniform('radius',20.0);
compositeShaderBuilder.SetUniform('dropShadowSize',20.0);
compositeShaderBuilder.SetChild('noise', Tskshader.MakePerlinNoiseFractalNoise(0.95,0.95,9,0.1));


apaint.ImageFilter:= Tskimagefilter.MakeRuntimeShader(compositeShaderBuilder,['content','blur'],[nil,tskimagefilter.MakeBlur(20,20,nil,Tsktilemode.Clamp)]);

apaint.Shader:= Tskshader.MakeGradientLinear(Tpointf.Create(450,60), Tpointf.Create(290,190),[$FF7A26D9,$FFE444E1],nil,Tsktilemode.clamp);
acanvas.DrawCircle(375,125,100,apaint);

apaint.Shader:=nil;
apaint.Color:= $FFEA357C;
acanvas.DrawCircle(100,265,55,apaint);

apaint.Shader:= Tskshader.MakeGradientLinear(Tpointf.Create(180,125), Tpointf.Create(230,125),[$FFEA334C,$FFEC6051],nil,Tsktilemode.clamp);
acanvas.DrawCircle(205,125,25,apaint);

apaint.StrokeWidth:=2;
apaint.Shader:= Tskshader.MakeGradientLinear(Tpointf.Create(120,110), Tpointf.Create(405,290),[$80FFFFFF, $00FFFFFF, $00FF48DB, $80FF48DB]);
acanvas.DrawRoundRect(Trectf.Create(83,108,407,293),20,20,apaint);

end;

end.
