{ Android NativeActivity drawing a rotating triangle with EGL and OpenGL ES 2 (unit gles20).
  A tap reverses the rotation. Messages are logged under the logcat tag FPCGLES. }
library glesdemo;

{$mode objfpc}{$H+}

uses
  cthreads, ctypes, SysUtils, Classes, gles20;

type
  PANativeActivity = ^TANativeActivity;
  PANativeActivityCallbacks = ^TANativeActivityCallbacks;

  TActivityProc = procedure(aActivity: PANativeActivity); cdecl;
  TActivityWindowProc = procedure(aActivity: PANativeActivity; aWindow: Pointer); cdecl;
  TActivityQueueProc = procedure(aActivity: PANativeActivity; aQueue: Pointer); cdecl;

  { ANativeActivityCallbacks from android/native_activity.h }
  TANativeActivityCallbacks = record
    onStart: TActivityProc;
    onResume: TActivityProc;
    onSaveInstanceState: function(aActivity: PANativeActivity; aOutSize: Pcsize_t): Pointer; cdecl;
    onPause: TActivityProc;
    onStop: TActivityProc;
    onDestroy: TActivityProc;
    onWindowFocusChanged: procedure(aActivity: PANativeActivity; aHasFocus: cint); cdecl;
    onNativeWindowCreated: TActivityWindowProc;
    onNativeWindowResized: TActivityWindowProc;
    onNativeWindowRedrawNeeded: TActivityWindowProc;
    onNativeWindowDestroyed: TActivityWindowProc;
    onInputQueueCreated: TActivityQueueProc;
    onInputQueueDestroyed: TActivityQueueProc;
    onContentRectChanged: procedure(aActivity: PANativeActivity; aRect: Pointer); cdecl;
    onConfigurationChanged: TActivityProc;
    onLowMemory: TActivityProc;
  end;

  { ANativeActivity from android/native_activity.h }
  TANativeActivity = record
    callbacks: PANativeActivityCallbacks;
    vm: Pointer;
    env: Pointer;
    clazz: Pointer;
    internalDataPath: PAnsiChar;
    externalDataPath: PAnsiChar;
    sdkVersion: cint32;
    instance: Pointer;
    assetManager: Pointer;
    obbPath: PAnsiChar;
  end;

  TLooperCallback = function(aFd, aEvents: cint; aData: Pointer): cint; cdecl;

  { TRenderThread }

  TRenderThread = class(TThread)
  private
    FWindow: Pointer;
    FDisplay: EGLDisplay;
    FSurface: EGLSurface;
    FContext: EGLContext;
    FProgram: GLuint;
    FAngleLoc: GLint;
    function InitEGL: Boolean;
    procedure DoneEGL;
    function InitScene: Boolean;
    procedure DrawFrame(aAngle, aTime: Single);
    procedure CheckFirstFrame;
  protected
    procedure Execute; override;
  public
    // Create a running thread that renders to the native window aWindow
    constructor Create(aWindow: Pointer);
  end;

const
  cTag = 'FPCGLES';
  ANDROID_LOG_INFO = 4;
  AINPUT_EVENT_TYPE_MOTION = 2;
  AMOTION_EVENT_ACTION_MASK = $FF;
  AMOTION_EVENT_ACTION_DOWN = 0;
  ALOOPER_EVENT_INPUT = 1;

  cVertexShader =
    'attribute vec2 aPos;'#10+
    'attribute vec3 aColor;'#10+
    'uniform float uAngle;'#10+
    'varying vec3 vColor;'#10+
    'void main() {'#10+
    '  float c = cos(uAngle); float s = sin(uAngle);'#10+
    '  gl_Position = vec4(c*aPos.x - s*aPos.y, s*aPos.x + c*aPos.y, 0.0, 1.0);'#10+
    '  vColor = aColor;'#10+
    '}';
  cFragmentShader =
    'precision mediump float;'#10+
    'varying vec3 vColor;'#10+
    'void main() { gl_FragColor = vec4(vColor, 1.0); }';

  cVertices: array[0..5] of GLfloat = (0.0, 0.7, -0.6, -0.5, 0.6, -0.5);
  cColors: array[0..8] of GLfloat = (1, 0, 0, 0, 1, 0, 0, 0, 1);

function __android_log_write(aPrio: cint; aTag, aText: PAnsiChar): cint; cdecl; external 'log';
function ANativeWindow_setBuffersGeometry(aWindow: Pointer; aWidth, aHeight, aFormat: cint32): cint32; cdecl; external 'android';
function ALooper_forThread: Pointer; cdecl; external 'android';
procedure AInputQueue_attachLooper(aQueue, aLooper: Pointer; aIdent: cint; aCallback: TLooperCallback; aData: Pointer); cdecl; external 'android';
procedure AInputQueue_detachLooper(aQueue: Pointer); cdecl; external 'android';
function AInputQueue_getEvent(aQueue: Pointer; aOutEvent: PPointer): cint32; cdecl; external 'android';
function AInputQueue_preDispatchEvent(aQueue, aEvent: Pointer): cint32; cdecl; external 'android';
procedure AInputQueue_finishEvent(aQueue, aEvent: Pointer; aHandled: cint); cdecl; external 'android';
function AInputEvent_getType(aEvent: Pointer): cint32; cdecl; external 'android';
function AMotionEvent_getAction(aEvent: Pointer): cint32; cdecl; external 'android';

var
  RenderThread: TRenderThread;
  Direction: Integer = 1;
  TapCount: Integer;


// Write aMsg to logcat under cTag
procedure Log(const aMsg: String);

begin
  __android_log_write(ANDROID_LOG_INFO, cTag, PAnsiChar(AnsiString(aMsg)));
end;


// Compile a shader of kind aKind from aSource; 0 on failure
function CompileShader(aKind: GLenum; const aSource: AnsiString): GLuint;

var
  lSrc: PAnsiChar;
  lOk: GLint;
  lLog: array[0..511] of AnsiChar;

begin
  Result:=glCreateShader(aKind);
  lSrc:=PAnsiChar(aSource);
  glShaderSource(Result, 1, @lSrc, nil);
  glCompileShader(Result);
  glGetShaderiv(Result, GL_COMPILE_STATUS, @lOk);
  if lOk=0 then
    begin
    glGetShaderInfoLog(Result, SizeOf(lLog), nil, @lLog[0]);
    Log('shader compile failed: '+lLog);
    glDeleteShader(Result);
    Result:=0;
    end;
end;


{ TRenderThread }

constructor TRenderThread.Create(aWindow: Pointer);

begin
  FWindow:=aWindow;
  inherited Create(False);
end;


function TRenderThread.InitEGL: Boolean;

const
  cConfigAttribs: array[0..10] of EGLint = (
    EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT, EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
    EGL_RED_SIZE, 8, EGL_GREEN_SIZE, 8, EGL_BLUE_SIZE, 8, EGL_NONE);
  cContextAttribs: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);

var
  lMajor, lMinor, lCount, lFormat, lWidth, lHeight: EGLint;
  lConfig: EGLConfig;

begin
  Result:=False;
  if not Assigned(eglGetDisplay) then
    begin
    Log('libEGL.so not loaded');
    Exit;
    end;
  FDisplay:=eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if (FDisplay=EGL_NO_DISPLAY) or (eglInitialize(FDisplay, @lMajor, @lMinor)=EGL_FALSE) then
    begin
    Log(Format('eglInitialize failed: $%x', [eglGetError()]));
    Exit;
    end;
  Log(Format('EGL %d.%d, vendor %s', [lMajor, lMinor, eglQueryString(FDisplay, EGL_VENDOR)]));
  if (eglChooseConfig(FDisplay, @cConfigAttribs[0], @lConfig, 1, @lCount)=EGL_FALSE) or (lCount<1) then
    begin
    Log('eglChooseConfig found no RGB888 ES2 window config');
    Exit;
    end;
  eglGetConfigAttrib(FDisplay, lConfig, EGL_NATIVE_VISUAL_ID, @lFormat);
  ANativeWindow_setBuffersGeometry(FWindow, 0, 0, lFormat);
  FSurface:=eglCreateWindowSurface(FDisplay, lConfig, EGLNativeWindowType(FWindow), nil);
  if FSurface=EGL_NO_SURFACE then
    begin
    Log(Format('eglCreateWindowSurface failed: $%x', [eglGetError()]));
    Exit;
    end;
  FContext:=eglCreateContext(FDisplay, lConfig, EGL_NO_CONTEXT, @cContextAttribs[0]);
  if FContext=EGL_NO_CONTEXT then
    begin
    Log(Format('eglCreateContext failed: $%x', [eglGetError()]));
    Exit;
    end;
  if eglMakeCurrent(FDisplay, FSurface, FSurface, FContext)=EGL_FALSE then
    begin
    Log(Format('eglMakeCurrent failed: $%x', [eglGetError()]));
    Exit;
    end;
  eglQuerySurface(FDisplay, FSurface, EGL_WIDTH, @lWidth);
  eglQuerySurface(FDisplay, FSurface, EGL_HEIGHT, @lHeight);
  glViewport(0, 0, lWidth, lHeight);
  Log(Format('surface %dx%d, GL_RENDERER %s, GL_VERSION %s',
    [lWidth, lHeight, PAnsiChar(glGetString(GL_RENDERER)), PAnsiChar(glGetString(GL_VERSION))]));
  Result:=True;
end;


procedure TRenderThread.DoneEGL;

begin
  if FDisplay=EGL_NO_DISPLAY then
    Exit;
  eglMakeCurrent(FDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
  if FContext<>EGL_NO_CONTEXT then
    eglDestroyContext(FDisplay, FContext);
  if FSurface<>EGL_NO_SURFACE then
    eglDestroySurface(FDisplay, FSurface);
  eglTerminate(FDisplay);
  FDisplay:=EGL_NO_DISPLAY;
end;


function TRenderThread.InitScene: Boolean;

var
  lVertex, lFragment: GLuint;
  lOk: GLint;

begin
  Result:=False;
  lVertex:=CompileShader(GL_VERTEX_SHADER, cVertexShader);
  lFragment:=CompileShader(GL_FRAGMENT_SHADER, cFragmentShader);
  if (lVertex=0) or (lFragment=0) then
    Exit;
  FProgram:=glCreateProgram();
  glAttachShader(FProgram, lVertex);
  glAttachShader(FProgram, lFragment);
  glBindAttribLocation(FProgram, 0, 'aPos');
  glBindAttribLocation(FProgram, 1, 'aColor');
  glLinkProgram(FProgram);
  glGetProgramiv(FProgram, GL_LINK_STATUS, @lOk);
  if lOk=0 then
    begin
    Log('program link failed');
    Exit;
    end;
  glUseProgram(FProgram);
  FAngleLoc:=glGetUniformLocation(FProgram, 'uAngle');
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, @cVertices[0]);
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, @cColors[0]);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  Result:=True;
end;


procedure TRenderThread.DrawFrame(aAngle, aTime: Single);

begin
  glClearColor(0.1, 0.2+0.1*Sin(aTime), 0.4, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glUniform1f(FAngleLoc, aAngle);
  glDrawArrays(GL_TRIANGLES, 0, 3);
end;


procedure TRenderThread.CheckFirstFrame;

var
  lCorner: array[0..3] of Byte;
  lWidth, lHeight: EGLint;

begin
  { Pixel (2,2) shows the clear colour of time 0: (0.1, 0.2, 0.4). }
  glClearColor(0.1, 0.2, 0.4, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glUniform1f(FAngleLoc, 0);
  glDrawArrays(GL_TRIANGLES, 0, 3);
  eglQuerySurface(FDisplay, FSurface, EGL_WIDTH, @lWidth);
  eglQuerySurface(FDisplay, FSurface, EGL_HEIGHT, @lHeight);
  glReadPixels(2, 2, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @lCorner[0]);
  Log(Format('first frame: corner rgb=%d,%d,%d glGetError=$%x',
    [lCorner[0], lCorner[1], lCorner[2], glGetError()]));
end;


procedure TRenderThread.Execute;

var
  lAngle: Single;
  lStart, lLast, lNow: QWord;
  lFrames: Integer;

begin
  FDisplay:=EGL_NO_DISPLAY;
  FSurface:=EGL_NO_SURFACE;
  FContext:=EGL_NO_CONTEXT;
  try
    if InitEGL and InitScene then
      begin
      CheckFirstFrame;
      lAngle:=0;
      lFrames:=0;
      lStart:=GetTickCount64;
      lLast:=lStart;
      while not Terminated do
        begin
        lNow:=GetTickCount64;
        lAngle:=lAngle+Direction*(lNow-lLast)/1000*1.5;
        lLast:=lNow;
        DrawFrame(lAngle, (lNow-lStart)/1000);
        if eglSwapBuffers(FDisplay, FSurface)=EGL_FALSE then
          begin
          Log(Format('eglSwapBuffers failed: $%x', [eglGetError()]));
          Break;
          end;
        Inc(lFrames);
        if lFrames=120 then
          Log(Format('120 frames in %d ms', [GetTickCount64-lStart]));
        end;
      end;
  except
    on E: Exception do
      Log('render thread: '+E.ClassName+': '+E.Message);
  end;
  DoneEGL;
end;


// Drain the input queue aData; a touch-down reverses the rotation
function InputCallback(aFd, aEvents: cint; aData: Pointer): cint; cdecl;

var
  lEvent: Pointer;
  lHandled: cint;

begin
  lEvent:=nil;
  while AInputQueue_getEvent(aData, @lEvent)>=0 do
    begin
    if AInputQueue_preDispatchEvent(aData, lEvent)<>0 then
      Continue;
    lHandled:=0;
    if (AInputEvent_getType(lEvent)=AINPUT_EVENT_TYPE_MOTION)
      and ((AMotionEvent_getAction(lEvent) and AMOTION_EVENT_ACTION_MASK)=AMOTION_EVENT_ACTION_DOWN) then
      begin
      Direction:=-Direction;
      Inc(TapCount);
      Log(Format('tap %d: direction %d', [TapCount, Direction]));
      lHandled:=1;
      end;
    AInputQueue_finishEvent(aData, lEvent, lHandled);
    end;
  Result:=1;
end;


// Start rendering to aWindow
procedure OnWindowCreated(aActivity: PANativeActivity; aWindow: Pointer); cdecl;

begin
  Log('window created');
  RenderThread:=TRenderThread.Create(aWindow);
end;


// Stop rendering before aWindow goes away
procedure OnWindowDestroyed(aActivity: PANativeActivity; aWindow: Pointer); cdecl;

begin
  Log('window destroyed');
  if Assigned(RenderThread) then
    begin
    RenderThread.Terminate;
    RenderThread.WaitFor;
    FreeAndNil(RenderThread);
    end;
end;


// Attach aQueue to the main thread looper
procedure OnInputQueueCreated(aActivity: PANativeActivity; aQueue: Pointer); cdecl;

begin
  AInputQueue_attachLooper(aQueue, ALooper_forThread(), 1, @InputCallback, aQueue);
end;


// Detach aQueue from the looper
procedure OnInputQueueDestroyed(aActivity: PANativeActivity; aQueue: Pointer); cdecl;

begin
  AInputQueue_detachLooper(aQueue);
end;


// Entry point called by android.app.NativeActivity
procedure ANativeActivity_onCreate(aActivity: PANativeActivity; aSavedState: Pointer; aSavedStateSize: csize_t); cdecl;

begin
  Log(Format('onCreate, SDK %d, libEGL loaded: %s, libGLESv2 loaded: %s',
    [aActivity^.sdkVersion, BoolToStr(Assigned(eglGetDisplay), True), BoolToStr(Assigned(glClear), True)]));
  aActivity^.callbacks^.onNativeWindowCreated:=@OnWindowCreated;
  aActivity^.callbacks^.onNativeWindowDestroyed:=@OnWindowDestroyed;
  aActivity^.callbacks^.onInputQueueCreated:=@OnInputQueueCreated;
  aActivity^.callbacks^.onInputQueueDestroyed:=@OnInputQueueDestroyed;
end;


exports
  ANativeActivity_onCreate;

end.
