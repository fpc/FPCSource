{
  GLX demo for FreePascal
  2005 Bart Tierens, BTierens@netscape.net

  This program is in the public domain
  
  Warning: This demo works only with FreePascal 2.1 and better, due to changes to the glx header
}
program glxTest;

{$MODE delphi}

uses glx,unix,x,xlib,xutil,gl,glu;

var
  { Attributes to choose context with glXChooseVisual }
  Attr: Array[0..8] of integer = (
    GLX_RGBA,
    GLX_RED_SIZE, 1,
    GLX_GREEN_SIZE, 1,
    GLX_BLUE_SIZE, 1,
    GLX_DOUBLEBUFFER,
    none);

  { Attributes to choose context with glXChooseFBConfig.
    Similar to Attr, but not exactly compatible. }
  AttrFB: Array[0..10] of integer = (
    GLX_X_RENDERABLE, 1 { true },
    GLX_RED_SIZE, 1,
    GLX_GREEN_SIZE, 1,
    GLX_BLUE_SIZE, 1,
    GLX_DOUBLEBUFFER, 1 { true },
    none);

  visinfo: PXVisualInfo;
  cm: TColormap;
  winAttr: TXSetWindowAttributes;
  glXCont: GLXContext;
  dpy: PDisplay;
  win: TWindow;

procedure redraw();
begin
  glClear(GL_COLOR_BUFFER_BIT);

  glTranslatef(-0.5,-0.5,-2);
  glBegin(GL_QUADS);
    glColor3f(1,0,0);
    glVertex3f(0,0,0);
    glColor3f(0,1,0);
    glVertex3f(1,0,0);
    glColor3f(0,0,1);
    glVertex3f(1,1,0);
    glColor3f(1,1,1);
    glVertex3f(0,1,0);
  glEnd();

  glXSwapBuffers(dpy, win); //Swap the buffers
end;

procedure resize(width,height: integer);
begin
  glViewport(0,0,width,height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(45,width/height,0.1,200);
  glMatrixMode(GL_MODELVIEW);
end;

procedure loop();
var
  event: TXEvent;
begin
  while true do
  begin
    XNextEvent(dpy,@event);
    case event._type of
    Expose: redraw();
    ConfigureNotify: resize(event.xconfigure.width,event.xconfigure.height);
    KeyPress: halt(1);
    end;
  end;
end;

procedure Error(const S: string);
begin
  Writeln(ErrOutput, 'Error: ', S);
  Halt(1);
end;

var
  window_title_property: TXTextProperty;
  title: String;
  FBConfig: TGLXFBConfig;
  FBConfigs: PGLXFBConfig;
  FBConfigsCount: Integer;

  { Used with glXCreateContextAttribsARB to select 3.0 forward-compatible context }
  Context30Forward: array [0..6] of Integer =
  ( GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
    GLX_CONTEXT_MINOR_VERSION_ARB, 0,
    GLX_CONTEXT_FLAGS_ARB        , GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
    None
  );
begin
  dpy := XOpenDisplay(nil);
  if(dpy = nil) then
    Error('Could not connect to X server');

  if not GLX_version_1_0(dpy) then
    Error('GLX extension not supported');

  if GLX_version_1_3(dpy) then
  begin
    { use approach recommended since glX 1.3 }
    FBConfigs := glXChooseFBConfig(dpy, DefaultScreen(dpy), AttrFB, FBConfigsCount);
    if FBConfigsCount = 0 then
      Error('Could not find FB config');

    { just choose the first FB config from the FBConfigs list.
      More involved selection possible. }
    FBConfig := FBConfigs^;
    visinfo := glXGetVisualFromFBConfig(dpy, FBConfig);
  end else
  begin
    visinfo := glXChooseVisual(dpy, DefaultScreen(dpy), Attr);
  end;

  if(visinfo = nil) then
    Error('Could not find visual');

  //Create a new colormap
  cm := XCreateColormap(dpy,RootWindow(dpy,visinfo.screen),visinfo.visual,AllocNone);
  winAttr.colormap := cm;
  winAttr.border_pixel := 0;
  winAttr.background_pixel := 0;
  winAttr.event_mask := ExposureMask or ButtonPressMask or StructureNotifyMask or KeyPressMask;

  //Create a window
  win := XCreateWindow(dpy,RootWindow(dpy,visinfo.screen),0,0,640,480,0,visinfo.depth,InputOutput,visinfo.visual,CWBorderPixel or CWColormap or CWEventMask,@winAttr);

  title := 'FreePascal GLX demo --------- Press any key to exit';
  XStringListToTextProperty(@title,1,@window_title_property);
  XSetWMName(dpy,win,@window_title_property);

  //Create an OpenGL rendering context
  if GLX_version_1_3(dpy) then
  begin
    writeln('Using GLX 1.3 code path');
    { Uncomment two lines below to use GLX_ARB_create_context extension
      to request OpenGL 3.0 forward-compatible context. This is just
      a simple example, be aware of some shortcomings:

      - In case of failure, glXCreateContextAttribsARB not only returns nil,
        it also raises X error that by default simply breaks your program.
        In a real program, you probably want to catch it (use XSetErrorHandler
        to assign custom error handler) and retry glXCreateContextAttribsARB
        with less restrictive attributes.

      - In case of success, you will just see a black screen.
        That's because the Redraw and Resize procedures defined in this program
        actually use deprecated OpenGL calls, that are *not* available in
        a forward-compatible context (glGetError would show actual errors). }
//  if GLX_ARB_create_context(dpy, DefaultScreen(dpy)) then
//    glXCont := glXCreateContextAttribsARB(dpy, FBConfig, 0, true, Context30Forward) else
      { use approach recommended since glX 1.3 }
      glXCont := glXCreateNewContext(dpy, FBConfig, GLX_RGBA_TYPE, 0, true);
  end else
    glXCont := glXCreateContext(dpy, visinfo, none, true);

  if(glXCont = nil) then
    Error('Could not create an OpenGL rendering context');

  //Make it current
  glXMakeCurrent(dpy,win,glXCont);

  //Map the window on the display
  XMapWindow(dpy,win);
  
  loop();
end.