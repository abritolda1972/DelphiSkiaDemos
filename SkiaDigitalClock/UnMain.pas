unit UnMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, system.DateUtils;

type
  TForm1 = class(TForm)
    SkAnimatedPaintBox1: TSkAnimatedPaintBox;
    procedure SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
      const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
      const AOpacity: Single);
  private
    procedure DrawClock(Acanvas: ISKCanvas; Adest: TRectF; Apaint: Iskpaint);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

Procedure TForm1.DrawClock(Acanvas: ISKCanvas; Adest: TRectF; Apaint: Iskpaint);
var
  Ceffect :Iskruntimeeffect;
  CShaderBuilder: Iskruntimeshaderbuilder;
  CShader: Iskshader;
  ErrorMsg: String;
const
Clock=
 '''
    uniform float3 iResolution;      // Viewport resolution (pixels)
    uniform float  iTime;            // Shader playback time (s)
    uniform vec4 iDate;// =(2025,01,26,3600); // (year, month, day, time in seconds)


    int TWELVE_HOUR_CLOCK =1;
    int GLOWPULSE =1;
    int SHOW_GRID = 1;

    float pi = atan(1.0)*4.0;
    float tau = atan(1.0)*8.0;

    const float scale = 1.0 / 10.0; // Reduced from 6.0 to make everything smaller

    vec2 digitSize = vec2(1.0,1.5) * scale;
    vec2 digitSpacing = vec2(1.1,1.6) * scale;



    // hash function copy from https://www.shadertoy.com/view/4djSRW
    float hash12(vec2 p)
    {
        vec3 p3  = fract(vec3(p.xyx) * .1031);
        p3 += dot(p3, p3.yzx + 33.33);
        return fract((p3.x + p3.y) * p3.z);
    }


    float noise(vec2 pos) {
        vec2 i = floor(pos);
        vec2 f = fract(pos);

        float a = hash12(i);
        float b = hash12(i + vec2(1, 0));
        float c = hash12(i + vec2(0, 1));
        float d = hash12(i + vec2(1, 1));

        vec2 u = f * f * (3.0 - 2.0 * f);

        return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
    }

    //Distance to a line segment,
    float dfLine(vec2 start, vec2 end, vec2 uv)
    {
      start *= scale;
      end *= scale;

      vec2 line = end - start;
      float frac = dot(uv - start,line) / dot(line,line);
      return distance(start + line * clamp(frac, 0.0, 1.0), uv);
    }

    //Distance to the edge of a circle.
    float dfCircle(vec2 origin, float radius, vec2 uv)
    {
      origin *= scale;
      radius *= scale;

      return abs(length(uv - origin) - radius);
    }

    //Distance to an arc.
    float dfArc(vec2 origin, float start, float sweep, float radius, vec2 uv)
    {
      origin *= scale;
      radius *= scale;

      uv -= origin;
      uv *= mat2(cos(start), sin(start),-sin(start), cos(start));

      float offs = (sweep / 2.0 - pi);
      float ang = mod(atan(uv.y, uv.x) - offs, tau) + offs;
      ang = clamp(ang, min(0.0, sweep), max(0.0, sweep));

      return distance(radius * vec2(cos(ang), sin(ang)), uv);
    }

    //Distance to the digit "d" (0-9).
    float dfDigit(vec2 origin, float d, vec2 uv)
    {
      uv -= origin;
      d = floor(d);
      float dist = 1e6;

      if(d == 0.0)
      {
        dist = min(dist, dfLine(vec2(1.000,1.000), vec2(1.000,0.500), uv));
        dist = min(dist, dfLine(vec2(0.000,1.000), vec2(0.000,0.500), uv));
        dist = min(dist, dfArc(vec2(0.500,1.000),0.000, 3.142, 0.500, uv));
        dist = min(dist, dfArc(vec2(0.500,0.500),3.142, 3.142, 0.500, uv));
        return dist;
      }
      if(d == 1.0)
      {
        dist = min(dist, dfLine(vec2(0.500,1.500), vec2(0.500,0.000), uv));
        return dist;
      }
      if(d == 2.0)
      {
        dist = min(dist, dfLine(vec2(1.000,0.000), vec2(0.000,0.000), uv));
        dist = min(dist, dfLine(vec2(0.388,0.561), vec2(0.806,0.719), uv));
        dist = min(dist, dfArc(vec2(0.500,1.000),0.000, 3.142, 0.500, uv));
        dist = min(dist, dfArc(vec2(0.700,1.000),5.074, 1.209, 0.300, uv));
        dist = min(dist, dfArc(vec2(0.600,0.000),1.932, 1.209, 0.600, uv));
        return dist;
      }
      if(d == 3.0)
      {
        dist = min(dist, dfLine(vec2(0.000,1.500), vec2(1.000,1.500), uv));
        dist = min(dist, dfLine(vec2(1.000,1.500), vec2(0.500,1.000), uv));
        dist = min(dist, dfArc(vec2(0.500,0.500),3.142, 4.712, 0.500, uv));
        return dist;
      }
      if(d == 4.0)
      {
        dist = min(dist, dfLine(vec2(0.700,1.500), vec2(0.000,0.500), uv));
        dist = min(dist, dfLine(vec2(0.000,0.500), vec2(1.000,0.500), uv));
        dist = min(dist, dfLine(vec2(0.700,1.200), vec2(0.700,0.000), uv));
        return dist;
      }
      if(d == 5.0)
      {
        dist = min(dist, dfLine(vec2(1.000,1.500), vec2(0.300,1.500), uv));
        dist = min(dist, dfLine(vec2(0.300,1.500), vec2(0.200,0.900), uv));
        dist = min(dist, dfArc(vec2(0.500,0.500),3.142, 5.356, 0.500, uv));
        return dist;
      }
      if(d == 6.0)
      {
        dist = min(dist, dfLine(vec2(0.067,0.750), vec2(0.500,1.500), uv));
        dist = min(dist, dfCircle(vec2(0.500,0.500), 0.500, uv));
        return dist;
      }
      if(d == 7.0)
      {
        dist = min(dist, dfLine(vec2(0.000,1.500), vec2(1.000,1.500), uv));
        dist = min(dist, dfLine(vec2(1.000,1.500), vec2(0.500,0.000), uv));
        return dist;
      }
      if(d == 8.0)
      {
        dist = min(dist, dfCircle(vec2(0.500,0.400), 0.400, uv));
        dist = min(dist, dfCircle(vec2(0.500,1.150), 0.350, uv));
        return dist;
      }
      if(d == 9.0)
      {
        dist = min(dist, dfLine(vec2(0.933,0.750), vec2(0.500,0.000), uv));
        dist = min(dist, dfCircle(vec2(0.500,1.000), 0.500, uv));
        return dist;
      }

      return dist;
    }

    //Distance to a number
    float dfNumber(vec2 origin, float num, vec2 uv)
    {
      uv -= origin;
      float dist = 1e6;
      float offs = 0.0;

      for(float i = 5.0;i > -3.0;i--)
      {
        float d = mod(num / pow(10.0,i),10.0);

        vec2 pos = digitSpacing * vec2(offs,0.0);

        if(i == 0.0)
        {
          dist = min(dist, dfCircle(vec2(offs+0.9,0.1)*1.1, 0.04,uv));
        }

        if(num > pow(10.0,i) || i == 0.0)
        {
          dist = min(dist, dfDigit(pos, d, uv));
          offs++;
        }
      }
      return dist;
    }

    //Distance to a number This handles 2 digit integers, leading 0's will be drawn
    float dfNumberInt(vec2 origin, int inum, vec2 uv)
    {
        float num = float(inum);
      uv -= origin;
      float dist = 1e6;
      float offs = 0.0;

      for(float i = 1.0;i >= 0.0;i--)
      {
        float d = mod(num / pow(10.0,i),10.0);

        vec2 pos = digitSpacing * vec2(offs,0.0);

            dist = min(dist, dfDigit(pos, d, uv));
            offs++;
      }
      return dist;
    }

    float dfColon(vec2 origin, vec2 uv) {
      uv -= origin;
      float dist = 1e6;
      float offs = 0.0;

        dist = min(dist, dfCircle(vec2(offs+0.9,0.9)*1.1, 0.04,uv));
        dist = min(dist, dfCircle(vec2(offs+0.9,0.4)*1.1, 0.04,uv));
        return dist;
    }

    //Length of a number in digits
    float numberLength(float n)
    {
      return floor(max(log(n) / log(10.0), 0.0) + 1.0) + 2.0;
    }

    vec4 main( vec2 fragCoord )
    {
        vec2 aspect = iResolution.xy / iResolution.y;
        vec2 uv = (fragCoord.xy / iResolution.y - aspect/2.0) *0.86;


       // vec2 uv = fragCoord/iResolution.xy;
       // float aspect = iResolution.x/iResolution.y;
       // uv = (uv - vec2(0.5)) * vec2(aspect,1.);

         uv.y = 1 - uv.y;
         uv.y -= .9;
         uv.x += 1.5;

        int hour = int(iDate.w/3600.);
    //#if TWELVE_HOUR_CLOCK
      //  if( hour > 12 ) hour -= 12;
      //  if( hour == 0 ) hour = 12;
    //#endif
        int minute = int(mod(iDate.w/60.,60.));
        int second = int(mod(iDate.w,60.));
        int day = int(iDate.z);

        float nsize = numberLength(9999.);
        vec2 pos = digitSpacing * vec2(nsize,1.0)/2.0;

        vec2 basepos = pos;
        basepos.x -= 0.26;

        float dist = 1e6;
        pos.x = basepos.x + 0.35;
        dist = min(dist, dfNumberInt(pos, hour, uv));

        pos.x = basepos.x + 0.5;
        dist = min(dist, dfColon(pos, uv));

        pos.x = basepos.x + 0.65;
        dist = min(dist, dfNumberInt(pos, minute, uv));

        pos.x = basepos.x + 0.8;
        dist = min(dist, dfColon(pos, uv));

        pos.x = basepos.x + 0.95;
        dist = min(dist, dfNumberInt(pos, second, uv));

        vec3 color = vec3(0.0);

        float shade = 1.0;

        shade = 0.004 / (dist);

        color += vec3(1.0, 0.0, 0.0) * shade;
    //#if GLOWPULSE
        // Updated glow color to match
        color += vec3(1.0, 0.5, 0.0) * shade * noise((uv + vec2(iTime*.5)) * 2.5 + .5);
    //#endif

    //    #ifdef SHOW_GRID
        float grid = 0.5-max(abs(mod(uv.x*64.0,1.0)-0.5), abs(mod(uv.y*64.0,1.0)-0.5));

        color *= 0.25+vec3(smoothstep(0.0,64.0 / iResolution.y,grid))*0.75;
    //    #endif

        return vec4( color , 0.0 );
    }
 ''';
Begin

Ceffect:= Tskruntimeeffect.makeforshader(clock,ErrorMsg);
   CShaderBuilder:=  TSkRuntimeShaderBuilder.Create(Ceffect);
   CShader:=CshaderBuilder.MakeShader;
if assigned(CShaderBuilder) then
Begin
  if(Ceffect.UniformExists('iResolution')) then
    CShaderBuilder.SetUniform('iResolution',[SkanimatedPaintBox1.Width, SkanimatedPaintBox1.Height,0]);
  if(Ceffect.UniformExists('iTime')) then
    CShaderBuilder.SetUniform('iTime', SkanimatedPaintBox1.Animation.CurrentTime);
  if(Ceffect.UniformExists('iDate')) then
    CShaderBuilder.SetUniform('iDate',[yearof(now),monthof(now),DayOf(now),SecondOfTheDay(now)]);

 CShader:=CshaderBuilder.MakeShader;

    Apaint.Shader:= Cshader;
    Acanvas.Save;
      try
      //apaint.Blender:= Tskblender.MakeMode(tskblendmode.Plus);

      Acanvas.ClipRect(adest);
      acanvas.DrawPaint(Apaint);

    finally
      Acanvas.Restore;
    end;
End;
End;

procedure TForm1.SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
Var
 Lpaint: ISKpaint;
begin
Lpaint:= Tskpaint.Create;
Drawclock(Acanvas, Adest,Lpaint);

end;

end.
