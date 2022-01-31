{$DEFINE AUDIO}
unit UMain;

interface

//
// Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
// Dialogs, ExtCtrls, UGame
// {$IFDEF AUDIO}, Bass{$ENDIF};
uses
  //OpenGL_FMX,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ExtCtrls,
  FMX.Filter.Effects,
  UGame
{$IFDEF AUDIO}, Bass, Vcl.ExtCtrls{$ENDIF};
// Vcl.ExtCtrls;

const
  levelData: array [0 .. 11, 0 .. 2] of single = (
    // nodes, teams, ai delay
    (5, 2, 4), (6, 2, 3.5), (8, 2, 3), (10, 2, 3), (9, 3, 4), (12, 3, 3.5),
    (15, 3, 3), (6, 3, 2.5), (16, 4, 3), (18, 4, 2.5), (12, 4, 2),
    (20, 4, 1.5));

type
  TTween = class
  private
    FDelay: Cardinal;
    FAnimate: Boolean;
    FAlpha: single;
    FColor: Cardinal;
    FFadeOut: Boolean;
    FStart: Cardinal;
    FVisible: Boolean;
    FOnAnimationEnd: TNotifyEvent;
  public
    procedure Show(const color: Cardinal);
    procedure Hide(Delay: Cardinal = 0);

    procedure Animate;

    property Alpha: single read FAlpha;
    property color: Cardinal read FColor;
    property Visible: Boolean read FVisible;
    property OnAnimationEnd: TNotifyEvent read FOnAnimationEnd
      write FOnAnimationEnd;
  end;

  TfrmMain = class(TForm, IDisplay)
    Timer1: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
    FLevel: Integer;
    FMouse: TPoint;
    FMouseDown: Boolean;
    FGame: TGameScene;
    FTween: TTween;
    FLastTick: Cardinal;
    FProgram: Cardinal;
    FStart: Cardinal;
{$IFDEF AUDIO}
    FMusicChannel: Cardinal;
    FHit: array [0 .. 5] of Cardinal;
    FLaunch: Cardinal;
    FPing: array [0 .. 2] of Cardinal;

    procedure LoadEffect(Filename: string; var Handle: Cardinal);

{$ENDIF}
    procedure DrawLine(x1, y1, x2, y2: single; color: Cardinal;
      thickness: single = 2; Alpha: single = 1);
    procedure DrawQuad(x1, y1, x2, y2: single; color: Cardinal;
      Alpha: single = 1);
    procedure drawCircle(X, Y, outer_radius, inner_radius: single;
      color: Cardinal; glow: Boolean = false; Alpha: single = 1.0;
      segments: Integer = 32);

    procedure GetMouse(out X, Y: single);
    procedure GetDimension(out Width, Height: single);
    procedure PlaySound(const sample: string);
    procedure DoAnimationEnd(Sender: TObject);

    function LoadShader(const fragmentShaderString: string): Boolean;

  public
    { Public-Deklarationen }
    procedure StartGame;
  end;

var
  frmMain: TfrmMain;

implementation

uses dglOpengl;

{$R *.dfm}

const
  vertexShaderString = 'void main(void) { ' + sLineBreak +
    'gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;}';

  pixelShaderString = '    uniform vec3 iResolution;' + sLineBreak +
    'uniform float iTime;' + sLineBreak + '#define ANIMATE' + sLineBreak +
    '#define OCTAVES 5' + sLineBreak + 'vec3 mod289(vec3 x)' + sLineBreak + '{'
    + sLineBreak + '  return x - floor(x * (1.0 / 289.0)) * 289.0;' + sLineBreak
    + '}' + sLineBreak + 'vec2 mod289(vec2 x)' + sLineBreak + '{' + sLineBreak +
    '  return x - floor(x * (1.0 / 289.0)) * 289.0;' + sLineBreak + '}' +
    sLineBreak + 'vec3 permute(vec3 x)' + sLineBreak + '{' + sLineBreak +
    '  return mod289(((x*34.0)+1.0)*x);' + sLineBreak + '}' + sLineBreak +
    'float snoise(vec2 v)' + sLineBreak + '  {' + sLineBreak +
    '  const vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0' +
    sLineBreak +
    '                      0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)' +
    sLineBreak + '                     -0.577350269189626,  // -1.0 + 2.0 * C.x'
    + sLineBreak + '                      0.024390243902439); // 1.0 / 41.0' +
    sLineBreak + '// First corner' + sLineBreak +
    '  vec2 i  = floor(v + dot(v, C.yy) );' + sLineBreak +
    '  vec2 x0 = v -   i + dot(i, C.xx);' + sLineBreak + '// Other corners' +
    sLineBreak + '  vec2 i1;' + sLineBreak +
    '  //i1.x = step( x0.y, x0.x ); // x0.x > x0.y ? 1.0 : 0.0' + sLineBreak +
    '  //i1.y = 1.0 - i1.x;' + sLineBreak +
    '  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);' + sLineBreak +
    '  // x0 = x0 - 0.0 + 0.0 * C.xx ;' + sLineBreak +
    '  // x1 = x0 - i1 + 1.0 * C.xx ;' + sLineBreak +
    '  // x2 = x0 - 1.0 + 2.0 * C.xx ;' + sLineBreak +
    '  vec4 x12 = x0.xyxy + C.xxzz;' + sLineBreak + '  x12.xy -= i1;' +
    sLineBreak + '// Permutations' + sLineBreak +
    '  i = mod289(i); // Avoid truncation effects in permutation' + sLineBreak +
    '  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))' + sLineBreak +
    '		+ i.x + vec3(0.0, i1.x, 1.0 ));' + sLineBreak +
    '  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);'
    + sLineBreak + '  m = m*m ;' + sLineBreak + '  m = m*m ;' + sLineBreak +
    '// Gradients: 41 points uniformly over a line, mapped onto a diamond.' +
    sLineBreak +
    '// The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)' +
    sLineBreak + '  vec3 x = 2.0 * fract(p * C.www) - 1.0;' + sLineBreak +
    '  vec3 h = abs(x) - 0.5;' + sLineBreak + '  vec3 ox = floor(x + 0.5);' +
    sLineBreak + '  vec3 a0 = x - ox;' + sLineBreak +
    '// Normalise gradients implicitly by scaling m' + sLineBreak +
    '// Approximation of: m *= inversesqrt( a0*a0 + h*h );' + sLineBreak +
    '  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );' + sLineBreak
    + '// Compute final noise value at P' + sLineBreak + '  vec3 g;' +
    sLineBreak + '  g.x  = a0.x  * x0.x  + h.x  * x0.y;' + sLineBreak +
    '  g.yz = a0.yz * x12.xz + h.yz * x12.yw;' + sLineBreak +
    '  return 130.0 * dot(m, g);' + sLineBreak + '}' + sLineBreak +
    'vec2 rand2(vec2 p)' + sLineBreak + '{' + sLineBreak +
    '    p = vec2(dot(p, vec2(12.9898,78.233)), dot(p, vec2(26.65125, 83.054543)));'
    + sLineBreak + '    return fract(sin(p) * 43758.5453);' + sLineBreak + '}' +
    sLineBreak + 'float rand(vec2 p)' + sLineBreak + '{' + sLineBreak +
    '    return fract(sin(dot(p.xy ,vec2(54.90898,18.233))) * 4337.5453);' +
    sLineBreak + '}' + sLineBreak + 'vec3 hsv2rgb(vec3 c)' + sLineBreak + '{' +
    sLineBreak + '    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);' +
    sLineBreak + '    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);' +
    sLineBreak + '    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);'
    + sLineBreak + '}' + sLineBreak +
    '// Thanks to David Hoskins https://www.shadertoy.com/view/4djGRh' +
    sLineBreak + 'float stars(in vec2 x, float numCells, float size, float br)'
    + sLineBreak + '{' + sLineBreak + '    vec2 n = x * numCells;' + sLineBreak
    + '    vec2 f = floor(n);' + sLineBreak + '	float d = 1.0e10;' + sLineBreak
    + '    for (int i = -1; i <= 1; ++i)' + sLineBreak + '    {' + sLineBreak +
    '        for (int j = -1; j <= 1; ++j)' + sLineBreak + '        {' +
    sLineBreak + '            vec2 g = f + vec2(float(i), float(j));' +
    sLineBreak + '			g = n - g - rand2(mod(g, numCells)) + rand(g);' +
    sLineBreak + '            // Control size' + sLineBreak +
    '            g *= 1. / (numCells * size);' + sLineBreak +
    '			d = min(d, dot(g, g));' + sLineBreak + '        }' + sLineBreak +
    '    }' + sLineBreak +
    '    return br * (smoothstep(.95, 1., (1. - sqrt(d))));' + sLineBreak + '}'
    + sLineBreak + '// Simple fractal noise' + sLineBreak +
    '// persistence - A multiplier that determines how quickly the amplitudes diminish for'
    + sLineBreak + '// each successive octave.' + sLineBreak +
    '// lacunarity - A multiplier that determines how quickly the frequency increases for'
    + sLineBreak + '// each successive octave.' + sLineBreak +
    'float fractalNoise(in vec2 coord, in float persistence, in float lacunarity)'
    + sLineBreak + '{' + sLineBreak + '    float n = 0.;' + sLineBreak +
    '    float frequency = 1.;' + sLineBreak + '    float amplitude = 1.;' +
    sLineBreak + '    for (int o = 0; o < OCTAVES; ++o)' + sLineBreak + '    {'
    + sLineBreak + '        n += amplitude * snoise(coord * frequency);' +
    sLineBreak + '        amplitude *= persistence;' + sLineBreak +
    '        frequency *= lacunarity;' + sLineBreak + '    }' + sLineBreak +
    '    return n;' + sLineBreak + '}' + sLineBreak +
    'vec3 fractalNebula(in vec2 coord, vec3 color, float transparency)' +
    sLineBreak + '{' + sLineBreak + '    float n = fractalNoise(coord, .5, 2.);'
    + sLineBreak + '    return n * color * transparency;' + sLineBreak + '}' +
    sLineBreak + 'void mainImage(out vec4 fragColor, in vec2 fragCoord)' +
    sLineBreak + '{' + sLineBreak +
    '    float resolution = max(iResolution.y, iResolution.y);' + sLineBreak +
    '    vec2 coord = fragCoord.xy / resolution;' + sLineBreak +
    '    vec3 result = vec3(0.);' + sLineBreak + '#ifdef ANIMATE' + sLineBreak +
    '    vec3 nebulaColor1 = hsv2rgb(vec3(.5+.5*sin(iTime*.1), 0.5, .25));' +
    sLineBreak +
    '	vec3 nebulaColor2 = hsv2rgb(vec3(.5+.5*sin(iTime*.21), 1., .25));' +
    sLineBreak + '#else' + sLineBreak +
    '    vec3 nebulaColor1 = hsv2rgb(vec3(.5, 0.5, .25));' + sLineBreak +
    '    vec3 nebulaColor2 = hsv2rgb(vec3(.7, 1., .25));' + sLineBreak +
    '#endif' + sLineBreak +
    '    result += fractalNebula(coord + vec2(.1, .1), nebulaColor1, 1.);' +
    sLineBreak +
    '    result += fractalNebula(coord + vec2(0., .2), nebulaColor2, .5);' +
    sLineBreak +
    '    result += stars(coord, 4., 0.1, 2.) * vec3(.74, .74, .74);' +
    sLineBreak +
    '    result += stars(coord, 8., 0.05, 1.) * vec3(.97, .74, .74);' +
    sLineBreak +
    '    result += stars(coord, 16., 0.025, 0.5) * vec3(.9, .9, .95);' +
    sLineBreak + '    fragColor = vec4(result, 1.);' + sLineBreak + '}' +
    sLineBreak +
    'void main(void) { mainImage(gl_FragColor, gl_FragCoord.xy); }';

function glSlang_GetInfoLog(glObject: Cardinal): Ansistring;
var
  blen, slen: GLInt;
  InfoLog: PGLCharARB;
begin
  glGetObjectParameterivARB(glObject, GL_OBJECT_INFO_LOG_LENGTH_ARB, @blen);
  if blen > 1 then
  begin
    GetMem(InfoLog, blen * SizeOf(GLCharARB));
    glGetInfoLogARB(glObject, blen, slen, InfoLog);
    Result := PAnsiChar(InfoLog);
    Dispose(InfoLog);
  end;
end;

function validateProgram(prog: GLUInt): Boolean;
var
  status: GLInt;
begin
  glValidateProgram(prog);
  glGetProgramiv(prog, GL_VALIDATE_STATUS, @status);
  Result := status = 1;
end;

function compileShader(typ: GLenum; shaderString: string): GLUInt;
var
  status: GLInt;
  sources: PGLchar;
  shader: GLUInt;
begin
  Result := 0;

  sources := PGLchar(Ansistring(shaderString));
  shader := glCreateShader(typ);
  if (shader = 0) or (shader = GL_INVALID_ENUM) then
    exit;

  glShaderSource(shader, 1, @sources, nil);
  glCompileShader(shader);

  glGetShaderiv(shader, GL_COMPILE_STATUS, @status);
  if status = 0 then
    showmessage(string(glSlang_GetInfoLog(shader)));

  if status = 0 then
    exit;
  Result := shader;
end;

{ TTween }

procedure TTween.Hide(Delay: Cardinal = 0);
begin
  FFadeOut := true;
  FStart := 0;
  FAnimate := true;
  FDelay := Delay;
end;

procedure TTween.Show(const color: Cardinal);
begin
  FColor := color;
  FFadeOut := false;
  FStart := 0;
  FAnimate := true;
  FVisible := true;
  FDelay := 0;
end;

procedure TTween.Animate;
begin
  if not FVisible then
    exit;
  if not FAnimate then
    exit;

  if FDelay > 0 then
  begin
    FDelay := FDelay - 1;
    exit;
  end;

  if FFadeOut then
    FAlpha := FAlpha - 0.1
  else
    FAlpha := FAlpha + 0.1;

  if FAlpha >= 1 then
  begin
    FAnimate := false;
    FAlpha := 1;
    if assigned(FOnAnimationEnd) then
      FOnAnimationEnd(self);
  end;

  if FAlpha < 0 then
  begin
    FAnimate := false;
    FVisible := false;
    if assigned(FOnAnimationEnd) then
      FOnAnimationEnd(self);
  end;
end;

{ TfrmMain }

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // VV wglMakeCurrent(Canvas.Handle, 0);
  FTween.free;
  FGame.free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  glViewPort(0, 0, ClientWidth, ClientHeight);
end;

{$IFDEF AUDIO}

procedure TfrmMain.LoadEffect(Filename: string; var Handle: Cardinal);
begin
  Filename := 'Effects\' + Filename;
  Handle := BASS_SampleLoad(false, PWideChar(WideString(Filename)), 0, 0, 3,
    BASS_SAMPLE_OVER_POS or BASS_UNICODE);
end;
{$ENDIF}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  PixelFormat: Cardinal;
  PFD: pixelformatdescriptor;
  RC: HGLRC;
  shader: TStringlist;
{$IFDEF AUDIO}
  i: Integer;
{$ENDIF}
begin
{$IFDEF AUDIO}
  if BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    FMusicChannel := BASS_MusicLoad(false,
      PWideChar(WideString('Effects\galaxyii.mod')), 0, 0,
      BASS_STREAM_AUTOFREE or BASS_UNICODE, 44100);

    // for i := 0 to 6 do loadEffect(format('hit0%d.mp3', [i + 1]), FHit[i]);
    // for i := 0 to 2 do loadEffect(format('ping0%d.mp3', [i + 1]), FPing[i]);
    LoadEffect('launch01.mp3', FLaunch);

    Bass_ChannelPlay(FMusicChannel, true);
  end;
{$ENDIF}
  RandSeed := GetTickCount;
  RandSeed := 1123;
  with PFD do
  begin
    nSize := SizeOf(pixelformatdescriptor);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 16; // Farbtiefe
    cRedBits := 0;
    cRedShift := 0;
    cGreenBits := 0;
    cBlueBits := 0;
    cBlueShift := 0;
    cAlphaBits := 0;
    cAlphaShift := 0;
    cAccumBits := 0; // Accumulation Buffer
    cAccumRedBits := 0;
    cAccumGreenBits := 0;
    cAccumBlueBits := 0;
    cAccumAlphaBits := 0;
    cDepthBits := 16; // Z-Buffer Tiefe
    cStencilBits := 0; // Stencil Buffer
    cAuxBuffers := 0;
    iLayerType := PFD_MAIN_PLANE;
    bReserved := 0;
    dwLayerMask := 0;
    dwVisibleMask := 0;
    dwDamageMask := 0
  end;
  // Pixel Format setzten
  PixelFormat := ChoosePixelFormat(Canvas.Handle, @PFD);
  SetPixelFormat(Canvas.Handle, PixelFormat, @PFD);

  RC := wglCreateContext(Canvas.Handle);
  ActivateRenderingContext(Canvas.Handle, RC);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  glEnable(GL_LINE_SMOOTH);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);

  FTween := TTween.Create;
  FTween.OnAnimationEnd := DoAnimationEnd;

  ClientWidth := 800;
  ClientHeight := 800;

  FGame := TGameScene.Create(self);
  FLevel := 0;
  StartGame;

  shader := TStringlist.Create;
  LoadShader(pixelShaderString);
end;

procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  node: TNode;
begin
  FMouse := Point(X, Y);
  if FMouseDown then
  begin
    node := FGame.getClosestNode(X, Y);
    if assigned(node) and ((node.team = 1) or (node.captureTeam = 1)) then
      node.selected := true;
  end;
end;

procedure TfrmMain.DrawLine(x1, y1, x2, y2: single; color: Cardinal;
  thickness: single = 2; Alpha: single = 1);
var
  a, r, g, b: single;
  w, h: single;
begin
  a := ((color shr 24) and $FF) / 255;
  if (a <= 0) then
    exit;

  if Alpha < 1 then
    a := a * Alpha;

  r := ((color shr 16) and $FF) / 255;
  g := ((color shr 8) and $FF) / 255;
  b := ((color and $FF)) / 255;

  glLineWidth(thickness);
  glBegin(GL_LINES);

  w := 2 / ClientWidth;
  h := 2 / ClientHeight;

  x1 := w * x1 - 1;
  x2 := w * x2 - 1;

  y1 := 1 - h * y1;
  y2 := 1 - h * y2;

  glColor4f(r, g, b, a);
  glVertex2f(x1, y1);

  glColor4f(r, g, b, a);
  glVertex2f(x2, y2);

  glEnd();
end;

procedure TfrmMain.DrawQuad(x1, y1, x2, y2: single; color: Cardinal;
  Alpha: single = 1);
var
  a, r, g, b: single;
  w, h: single;
begin
  a := ((color shr 24) and $FF) / 255;
  if (a <= 0) or (Opaque < 0) then
    exit;

  if Alpha < 1 then
    a := a * Alpha;

  r := ((color shr 16) and $FF) / 255;
  g := ((color shr 8) and $FF) / 255;
  b := ((color and $FF)) / 255;

  w := 2 / ClientWidth;
  h := 2 / ClientHeight;

  x1 := w * x1 - 1;
  x2 := w * x2 - 1;
  y1 := 1 - h * y1;
  y2 := 1 - h * y2;

  glColor4f(r, g, b, a);

  glBegin(GL_TRIANGLE_STRIP);
  glVertex2f(x1, y2);
  glVertex2f(x1, y1);
  glVertex2f(x2, y1);
  glVertex2f(x1, y2);
  glVertex2f(x2, y2);
  glVertex2f(x2, y1);
  glEnd();
end;

procedure TfrmMain.drawCircle(X, Y, outer_radius, inner_radius: single;
  color: Cardinal; glow: Boolean = false; Alpha: single = 1.0;
  segments: Integer = 32);
var
  a, r, g, b: single;
  w, h: single;
  x1, y1, x2, y2: single;
  step, angle: single;
  i: Integer;
begin
  a := ((color shr 24) and $FF) / 255;
  if (a <= 0) or (Opaque < 0) then
    exit;

  if Alpha < 1 then
    a := a * Alpha;

  r := ((color shr 16) and $FF) / 255;
  g := ((color shr 8) and $FF) / 255;
  b := ((color and $FF)) / 255;

  w := 2 / ClientWidth;
  h := 2 / ClientHeight;

  angle := 0;
  step := PI * 2 / segments;

  glBegin(GL_TRIANGLE_STRIP);
  for i := 0 to segments do
  begin
    x1 := (X + cos(angle) * outer_radius) * w - 1;
    y1 := 1 - (Y + sin(angle) * outer_radius) * h;
    x2 := (X + cos(angle) * inner_radius) * w - 1;
    y2 := 1 - (Y + sin(angle) * inner_radius) * h;

    if glow then
      glColor4f(r, g, b, 0)
    else
      glColor4f(r, g, b, a);

    glVertex2d(x1, y1);
    glColor4f(r, g, b, a);
    glVertex2d(x2, y2);

    angle := angle + step;
  end;
  glEnd();

end;

procedure TfrmMain.GetMouse(out X, Y: single);
begin
  X := FMouse.X;
  Y := FMouse.Y;
end;

procedure TfrmMain.GetDimension(out Width, Height: single);
begin
  Width := ClientWidth;
  Height := ClientHeight;
end;

procedure TfrmMain.PlaySound(const sample: string);
{$IFDEF AUDIO}
var
  Handle: Cardinal;
  ch: HCHANNEL;
begin
  if sample = 'hit01' then
    Handle := FHit[0]
  else if sample = 'hit02' then
    Handle := FHit[1]
  else if sample = 'hit03' then
    Handle := FHit[2]
  else if sample = 'hit04' then
    Handle := FHit[3]
  else if sample = 'hit05' then
    Handle := FHit[4]
  else if sample = 'hit06' then
    Handle := FHit[5]
  else if sample = 'launch01' then
    Handle := FLaunch
  else if sample = 'ping01' then
    Handle := FPing[0]
  else if sample = 'ping02' then
    Handle := FPing[0]
  else if sample = 'ping03' then
    Handle := FPing[0]
  else
    exit;
  ch := BASS_SampleGetChannel(Handle, false);
  BASS_ChannelSetAttribute(ch, BASS_ATTRIB_VOL, 0.2);
  Bass_ChannelPlay(ch, false);
{$ELSE}
begin
{$ENDIF}
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  node: TNode;
begin
  FMouseDown := true;
  node := FGame.getClosestNode(X, Y);
  if assigned(node) and ((node.team = 1) or (node.captureTeam = 1)) then
    node.selected := true;
end;

procedure TfrmMain.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := false;
  FGame.sendShips();
end;

procedure TfrmMain.StartGame;
begin
  FGame.Init(trunc(levelData[FLevel][0]), trunc(levelData[FLevel][1]),
    trunc(levelData[FLevel][2]));
end;

procedure TfrmMain.DoAnimationEnd(Sender: TObject);
begin
  if FTween.Visible then
  begin
    FTween.Hide(20);
    if FGame.TeamWon = 1 then
      inc(FLevel);
    StartGame;
  end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var
  tick: Cardinal;
begin
  tick := GetTickCount;
  FGame.Update((tick - FLastTick) / 1000);
  FLastTick := tick;

  if (FGame.GameOver) and (not FTween.Visible) then
    FTween.Show(gamecolors[FGame.TeamWon]);

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  if FProgram <> 0 then
  begin
    glUseProgram(FProgram);
    glUniform1f(glGetUniformLocation(FProgram, 'iTime'),
      (GetTickCount - FStart) / 1000);
    glUniform3f(glGetUniformLocation(FProgram, 'iResolution'), ClientWidth,
      ClientHeight, 0);

    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex3f(-1.0, -1.0, 0.0);
    glTexCoord2f(ClientWidth, 0);
    glVertex3f(1.0, -1.0, 0.0);
    glTexCoord2f(ClientWidth, ClientHeight);
    glVertex3f(1.0, 1.0, 0.0);
    glTexCoord2f(0, ClientHeight);
    glVertex3f(-1.0, 1.0, 0.0);
    glEnd();
    glUseProgram(0);
  end;

  FGame.Render;

  if FTween.Visible then
  begin
    FTween.Animate;
    DrawQuad(0, 0, ClientWidth, ClientHeight, FTween.color, FTween.Alpha);
  end;

  SwapBuffers(Canvas.Handle);
end;

function TfrmMain.LoadShader(const fragmentShaderString: string): Boolean;
var
  vertShader, fragShader: GLUInt;
  status: GLInt;
begin
  Result := false;

  if FProgram <> 0 then // Already Loaded
  begin
    glDeleteProgram(FProgram);
    FProgram := 0;
  end;

  if fragmentShaderString = '' then
  begin
    Result := true;
    exit;
  end;

  FStart := GetTickCount;

  vertShader := 0;
  fragShader := 0;

  FProgram := glCreateProgram();
  try
    vertShader := compileShader(GL_VERTEX_SHADER, vertexShaderString);
    if vertShader = 0 then
      exit;

    fragShader := compileShader(GL_FRAGMENT_SHADER, fragmentShaderString);
    if fragShader = 0 then
      exit;

    glAttachShader(FProgram, vertShader);
    glAttachShader(FProgram, fragShader);
    glLinkProgram(FProgram);

    glGetProgramiv(FProgram, GL_LINK_STATUS, @status);
    if status = 0 then
      exit;

    Result := validateProgram(FProgram);
  finally
    glDeleteShader(vertShader);
    glDeleteShader(fragShader);

    if not Result then
    begin
      glDeleteProgram(FProgram);
      FProgram := 0;
    end;
  end;
end;

initialization

InitOpenGl;

end.
