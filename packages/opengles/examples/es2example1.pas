program es2example1;
// By Benjamin 'BeRo' Rosseaux - benjamin@rosseaux.com - http://www.rosseaux.com
// Tested on desktop linux and the Nokia N900 with Maemo 5
{$ifdef fpc}
 {$mode delphi}
 {$ifdef CPUARM}
  {$define ogles}
 {$endif}
 {$ifdef CPUI386}
  {$define CPU386}
 {$endif}
 {$ifdef CPU386}
  {$asmmode intel}
 {$endif}
{$endif}

uses SysUtils,Classes,BaseUnix,Unix,UnixType,X,XLib,XUtil,XAtom{$ifdef ogles},gles20{$else},xf86vmode,GL,GLX,GLExt{$endif};

const VertexArray:array[0..11] of single=(0,-1,0,1,
                                          1,1,0,1,
                                          -1,1,0,1);

      ShaderPrecode={$ifdef ogles}''{$else}'#version 120'#10{$endif};

      VertexShaderSource:string=ShaderPrecode+'attribute vec4 position;'#10+
                                              'varying mediump vec2 pos;'#10+ 
                                              'void main(){'#10+
                                              ' gl_Position=position;'#10+
                                              ' pos=position.xy;'#10+
                                              '}';
                                              
      FragmentShaderSource:string=ShaderPrecode+'varying mediump vec2 pos;'#10+
                                                'uniform mediump float phase;'#10+
                                                'void main(){'#10+
                                                ' gl_FragColor=vec4(1.0,1.0,1.0,1.0)*sin(((pos.x*pos.x)+(pos.y*pos.y))*40.0+phase);'#10+
                                                '}';

var ReturnCode,CanvasWidth,CanvasHeight:integer;
    Running:boolean;
    VertexShader,FragmentShader,ShaderProgram:TGLuint;
    PhaseLocation:TGLint;
    CurrentTime:int64;

function GetTime:int64;
var NowTime:TDateTime;
    Year,Month,Day,hour,min,sec,msec:word;
begin
 NowTime:=Now;
 DecodeDate(NowTime,Year,Month,Day);
 DecodeTime(NowTime,hour,min,sec,msec);
 result:=(((((((((((Year*365)+Month)*31)+Day)*24)+hour)*60)+min)*60)+sec)*1000)+msec;
end;
        
procedure PrintShaderInfoLog(Shader:TGLUint;ShaderType:string);
var len,Success:TGLint;
    Buffer:pchar;
begin
 glGetShaderiv(Shader,GL_COMPILE_STATUS,@Success);
 if Success<>GL_TRUE then begin
  glGetShaderiv(Shader,GL_INFO_LOG_LENGTH,@len);
  if len>0 then begin
   getmem(Buffer,len+1);
   glGetShaderInfoLog(Shader,len,nil,Buffer);
   writeln(ShaderType,': ',Buffer);
   freemem(Buffer);
   Running:=false;
   ReturnCode:=1;
  end;
 end;
end;

function CreateShader(ShaderType:TGLenum;Source:pchar):TGLuint;
begin
 result:=glCreateShader(ShaderType);
 glShaderSource(result,1,@Source,nil);
 glCompileShader(result);
 if ShaderType=GL_VERTEX_SHADER then begin
  PrintShaderInfoLog(result,'Vertex shader');
 end else begin
  PrintShaderInfoLog(result,'Fragment shader');
 end;
end;

procedure Init;
begin
 ShaderProgram:=glCreateProgram();

 VertexShader:=CreateShader(GL_VERTEX_SHADER,pchar(VertexShaderSource));
 FragmentShader:=CreateShader(GL_FRAGMENT_SHADER,pchar(FragmentShaderSource));

 glAttachShader(ShaderProgram,VertexShader);
 glAttachShader(ShaderProgram,FragmentShader);
 
 glLinkProgram(ShaderProgram);
 
 glUseProgram(ShaderProgram);
 
 PhaseLocation:=glGetUniformLocation(ShaderProgram,'phase');
 if PhaseLocation<0 then begin
  writeln('Error: Cannot get phase shader uniform location');
  Running:=false;
  ReturnCode:=1;
 end;
end;

procedure Done;
begin
end;

procedure Render;
begin
 glViewPort(0,0,CanvasWidth,CanvasHeight);
 glClearColor(0,1,0,1);
 glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
 
 glUniform1f(PhaseLocation,(GetTime mod 2000)*0.001*pi);
 
 glVertexAttribPointer(0,4,GL_FLOAT,0,0,@VertexArray);
 glEnableVertexAttribArray(0);
 glDrawArrays(GL_TRIANGLE_STRIP,0,3);
 
end;

const MOUSE_MASK=ButtonPressMask or ButtonReleaseMask or PointerMotionMask or ButtonMotionMask;
      KEY_MASK=KeyPressMask or KeyReleaseMask or KeymapStateMask;
      X_MASK=KEY_MASK or MOUSE_MASK or VisibilityChangeMask or StructureNotifymask or ExposureMask;

{$ifdef ogles}
     Attr:array[0..6] of EGLint=(EGL_BUFFER_SIZE,16,
                                  EGL_DEPTH_SIZE,8,
                                  EGL_RENDERABLE_TYPE,EGL_OPENGL_ES2_BIT,
                                  EGL_NONE);
{    Attr:array[0..12] of EGLint=(EGL_BUFFER_SIZE,16,
                                  EGL_RED_SIZE,6,
                                  EGL_GREEN_SIZE,5,
                                  EGL_BLUE_SIZE,6,
                                  EGL_DEPTH_SIZE,8,
                                  EGL_RENDERABLE_TYPE,EGL_OPENGL_ES2_BIT,
                                  EGL_NONE);}
     CtxAttr:array[0..2] of EGLint=(EGL_CONTEXT_CLIENT_VERSION,2,EGL_NONE);
{$else}
     Attr:array[0..8] of TGLint=(GLX_RGBA,
                                 GLX_RED_SIZE,1,
                                 GLX_GREEN_SIZE,1,
                                 GLX_BLUE_SIZE,1,
                                 GLX_DOUBLEBUFFER,
                                 none);
{$endif}

     Title:string='GL test';
    
var dpy:PXDisplay;
    win,root:TWindow;
    screennum,ScreenWidth,ScreenHeight:integer;
    screen:PScreen;
    visual:PVisual;
    Event:TXEvent;
{$ifdef ogles}
    swa:TXSetWindowAttributes;
    ecfg:EGLConfig;
    num_config:EGLint;
    esfc:EGLSurface;
    ectxt:EGLContext;
    edpy:EGLDisplay;
    mouse_accel_numerator,mouse_accel_denominator,mouse_threshold:integer;
{$else}
    WinAttr:TXSetWindowAttributes;
    GLXCont:GLXContext;
    WindowTitleProperty:TXTextProperty;
    visualinfo:PXVisualInfo;
    ErrorBase,EventBase:integer;
{$endif}
    WM_DELETE_WINDOW:TAtom;
    
procedure DisableComposition;
{$ifdef ogles}
const one:integer=1;
{$endif}
var xclient:TXClientMessageEvent;
    wm_state,fullscreen{$ifdef ogles},non_composited{$endif}:TAtom;
begin
 wm_state:=XInternAtom(dpy,'_NET_WM_STATE',false);
 fullscreen:=XInternAtom(dpy,'_NET_WN_STATE_FULLSCREEN',false); 
 XChangeProperty(dpy,win,wm_state,XA_ATOM,32,PropModeReplace,@fullscreen,1);
 XFlush(dpy);
 
{$ifdef ogles}
 non_composited:=XInternAtom(dpy,'_HILDON_NON_COMPOSITED_WINDOW',false);
 XChangeProperty(dpy,win,non_composited,XA_INTEGER,32,PropModeReplace,@one,1);
 XFlush(dpy);
{$endif}
 
 xclient._type:=ClientMessage;
 xclient.window:=win;
 xclient.message_type:=XInternAtom(dpy,'_NET_WM_STATE',false);
 xclient.format:=32;
 xclient.data.l[0]:=1;
 xclient.data.l[1]:=XInternAtom(dpy,'_NET_WM_STATE_FULLSCREEN',false);
 xclient.data.l[2]:=0;
 xclient.data.l[3]:=0;
 xclient.data.l[4]:=0;
 XSendEvent(dpy,root,false,SubstructureRedirectMask or SubstructureNotifyMask,PXEvent(pointer(@xclient)));
 XFlush(dpy);
end;

procedure SetEmptyMouseCursor;
const bm_no_data:array[0..7] of byte=(0,0,0,0,0,0,0,0);
var bm_no:TPixmap;
    cmap:TColormap;
    no_ptr:TCursor;
    black,dummy:TXColor;
begin
 cmap:=DefaultColormap(Dpy,screennum);
 XAllocNamedColor(Dpy,cmap,'black',@black,@dummy);
 bm_no:=XCreateBitmapFromData(Dpy,Win,POINTER(@bm_no_data),8,8);
 no_ptr:=XCreatePixmapCursor(Dpy,bm_no,bm_no,@black,@black,0,0);
 XDefineCursor(Dpy,Win,no_ptr);
 XFreeCursor(Dpy,no_ptr);
 if bm_no<>None then begin
  XFreePixmap(Dpy,bm_no);
 end;
 XFreeColors(Dpy,cmap,@black.pixel,1,0);
end;

begin
 CurrentTime:=0;
 
 dpy:=XOpenDisplay(nil);
 if not assigned(dpy) then begin
  writeln('Error: Cannot connect to X server');
  halt(1);
 end;
 
 root:=DefaultRootWindow(dpy);

 screen:=XDefaultScreenOfDisplay(dpy);
 if not assigned(screen) then begin
  writeln('Error: Cannot get default screen');
  halt(1);
 end;
 
 ScreenWidth:=screen^.Width;
 ScreenHeight:=screen^.Height;

 screennum:=XDefaultScreen(dpy);
 
 visual:=XDefaultVisualOfScreen(screen);
 
 CanvasWidth:=ScreenWidth;
 CanvasHeight:=ScreenHeight;
{$ifdef ogles}
 
 swa.event_mask:=X_MASK;

 win:=XCreateWindow(dpy,root,0,0,ScreenWidth,ScreenHeight,0,CopyFromParent,InputOutput,Visual,CWEventMask,@swa);
 
 WM_DELETE_WINDOW:=XInternAtom(dpy,'WM_DELETE_WINDOW',FALSE);
 XSetWMProtocols(dpy,win,@WM_DELETE_WINDOW,1);
 XFlush(dpy);
 
 DisableComposition;
 
 XMapWindow(dpy,win);
 XFlush(dpy);
 
 DisableComposition;

 XSelectInput(Dpy,Win,FocusChangeMask or KeyPressMask or KeyReleaseMask or PropertyChangeMask or StructureNotifyMask or KeymapStateMask or PointerMotionMask or EnterWindowMask or LeaveWindowMask or ButtonPressMask or ButtonReleaseMask or ExposureMask);
 
 XStoreName(dpy,win,pchar(Title));
 XFlush(dpy);

 XMoveWindow(dpy,win,0,0);
 XRaiseWindow(dpy,win);
 //XWarpPointer(dpy,None,win,0,0,0,0,0,0);
 XFlush(dpy);

 SetEmptyMouseCursor;
 
 XGrabPointer(dpy,win,true,MOUSE_MASK,GrabModeAsync,GrabModeAsync,win,None,CurrentTime);
 XFlush(dpy);
 
 XGetPointerControl(dpy,@mouse_accel_numerator,@mouse_accel_denominator,@mouse_threshold);
 XFlush(dpy);
 
 XChangePointerControl(dpy,1,1,1,1,0);
 XFlush(dpy);
 
 XGrabKeyboard(dpy,win,false,GrabModeAsync,GrabModeAsync,CurrentTime);
 XFlush(dpy);
 
 edpy:=eglGetDisplay(dpy);
 if edpy=EGL_NO_DISPLAY then begin
  writeln('Error: Got no EGL display');
  halt(1);
 end;

 if eglInitialize(edpy,nil,nil)=0 then begin
  writeln('Error: Unable to initialize EGL');
  halt(1);
 end;

 num_config:=0;
 if eglChooseConfig(edpy,@Attr,@ecfg,1,@num_config)=0 then begin
  writeln('Error: Failed to choose config (',eglGetError,')');
  halt(1);
 end;
 if num_config<>1 then begin
  writeln('Error: Didn''t get exactly config but ',num_config);
  halt(1);
 end;
 
 esfc:=eglCreateWindowSurface(edpy,ecfg,win,nil);
 if esfc=EGL_NO_SURFACE then begin
  writeln('Error: Unable to create EGL surface (',eglGetError,')');
  halt(1);
 end;
 
 ectxt:=eglCreateContext(edpy,ecfg,EGL_NO_CONTEXT,@CtxAttr);
 if ectxt=EGL_NO_CONTEXT then begin
  writeln('Error: Unable to create EGL context (',eglGetError,')');
  halt(1);
 end;
 
 eglMakeCurrent(edpy,esfc,esfc,ectxt);
 
{$else}
 InitGLX;
 
 if not glXQueryExtension(dpy,ErrorBase,EventBase) then begin
  writeln('Error: GLX extension not supported');
  halt(1);
 end;
 
 visualinfo:=glXChooseVisual(dpy,screennum,Attr);
 if not assigned(Visualinfo) THEN BEGIN
  writeln('Error: Could not find visual info');
  exit;
 end;

 WinAttr.colormap:=XCreateColormap(dpy,root,VisualInfo.Visual,AllocNone);
 WinAttr.border_pixel:=0; 
 WinAttr.background_pixel:=0;
 WinAttr.event_mask:=X_MASK;
 WinAttr.override_redirect:=1;
 WinAttr.backing_store:=NotUseful;
 WinAttr.save_under:=0;
 
 Win:=XCreateWindow(dpy,root,0,0,ScreenWidth,ScreenHeight,0,VisualInfo.Depth,InputOutput,VisualInfo.Visual,CWOverrideRedirect or CWBackPixel or CWColormap or CWBackingStore or CWSaveUnder or CWEventMask,@WinAttr);

 XSelectInput(Dpy,Win,FocusChangeMask or KeyPressMask or KeyReleaseMask or PropertyChangeMask or StructureNotifyMask or KeymapStateMask or PointerMotionMask or EnterWindowMask or LeaveWindowMask or ButtonPressMask or ButtonReleaseMask or ExposureMask);

 XStringListToTextProperty(@Title,1,@WindowTitleProperty);
 XSetWMName(Dpy,Win,@WindowTitleProperty);

 glXCont:=glXCreateContext(Dpy,VisualInfo,none,true);
 if not assigned(glXCont) then begin
  writeln('Error: Could not create an OpenGL rendering context');
  halt(1);
 end;

 DisableComposition;

 XMapWindow(Dpy,Win);
 XFlush(dpy);
 
 DisableComposition;

 glXMakeCurrent(Dpy,Win,glXCont);
 
 SetEmptyMouseCursor;
 
 XMoveWindow(dpy,win,0,0);
 XRaiseWindow(dpy,win);
 XWarpPointer(dpy,None,win,0,0,0,0,0,0);
 XFlush(dpy);

 XF86VidmodeSetViewPort(dpy,screennum,0,0);
 XFlush(dpy);
 
 XGrabPointer(dpy,win,true,MOUSE_MASK,GrabModeAsync,GrabModeAsync,win,None,CurrentTime);
 XGrabKeyboard(dpy,win,false,GrabModeAsync,GrabModeAsync,CurrentTime);
 XFlush(dpy);

 XAutoRepeatOn(Dpy);
 XFlush(dpy);
 
 WM_DELETE_WINDOW:=XInternAtom(dpy,'WM_DELETE_WINDOW',FALSE);
 XSetWMProtocols(dpy,win,@WM_DELETE_WINDOW,1);
 XFlush(Dpy);
 
 CanvasWidth:=ScreenWidth;
 CanvasHeight:=ScreenHeight;
 
 if assigned(visual) then begin
  // Den Compiler befriedigen, so dass der kein Warning deswegen ausspuckt  :)
 end;
{$endif}
 
 Running:=true;
 ReturnCode:=0;
{$ifndef ogles}
 if not (Load_GL_version_1_2 and Load_GL_version_1_3 and Load_GL_version_1_4 and Load_GL_version_2_0) then begin
  Running:=false;
  writeln('Error: Cannot load OpenGL 2.0 API');
  ReturnCode:=1;
 end;
{$endif}
 if Running then begin
  Init;
  CurrentTime:=GetTime;
 end;
 while Running do begin
 
  while XPending(Dpy)>0 do begin
   XNextEvent(Dpy,@Event);
   
   case Event._type of

    ClientMessage:BEGIN
     if (Event.xclient.format=32) and (longword(Event.xclient.Data.l[0])=longword(WM_DELETE_WINDOW)) then begin
      Running:=false;
     end;
    end;
    
    Expose:begin
    end;
    
    ConfigureNotify:begin
    end;
    
    MotionNotify:begin
//   Running:=false;
    end;
    
    ButtonPress:begin
     Running:=false;
    end;
    
    ButtonRelease:begin
     Running:=false;
    end;
    
    KeyMapNotify:begin
    end;
    
    KeyPress:begin
     Running:=false;
    end;
    
    KeyRelease:begin
     Running:=false;
    end;
    
   end;
   
  end;
  
  CurrentTime:=GetTime;
  Render;
  
{$ifdef ogles}
  eglSwapBuffers(edpy,esfc);
{$else}
  glXSwapBuffers(dpy,win);
{$endif}

  XFlush(dpy);
  
 end;
 if ReturnCode=0 then begin
  Done;
 end;
 
{$ifdef ogles}
 XChangePointerControl(dpy,1,1,mouse_accel_numerator,mouse_accel_denominator,mouse_threshold);
 XUngrabPointer(dpy,CurrentTime);
 XUngrabKeyboard(dpy,CurrentTime);
 eglDestroyContext(edpy,ectxt);
 eglDestroySurface(edpy,esfc);
 eglTerminate(edpy);
{$else}
 XUngrabPointer(dpy,CurrentTime);
 XUngrabKeyboard(dpy,CurrentTime);
 glXDestroyContext(dpy,glxCont);
{$endif}

 XDestroyWindow(dpy,win);
 XCloseDisplay(dpy);

 halt(ReturnCode);
end.
