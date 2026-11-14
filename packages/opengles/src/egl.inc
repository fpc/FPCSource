{ EGL helper functions and library loader. }
  { was #define dname def_expr }
  function EGL_DEFAULT_DISPLAY : EGLNativeDisplayType;
      begin
         EGL_DEFAULT_DISPLAY:=EGLNativeDisplayType(0);
      end;

  { was #define dname def_expr }
  function EGL_NO_CONTEXT : EGLContext;
      begin
         EGL_NO_CONTEXT:=EGLContext(0);
      end;

  { was #define dname def_expr }
  function EGL_NO_DISPLAY : EGLDisplay;
      begin
         EGL_NO_DISPLAY:=EGLDisplay(0);
      end;

  { was #define dname def_expr }
  function EGL_NO_SURFACE : EGLSurface;
      begin
         EGL_NO_SURFACE:=EGLSurface(0);
      end;

  { was #define dname def_expr }
  function EGL_DONT_CARE : EGLint;
      begin
         EGL_DONT_CARE:=EGLint(-(1));
      end;

  { was #define dname def_expr }
  function EGL_UNKNOWN : EGLint;
      begin
         EGL_UNKNOWN:=EGLint(-(1));
      end;

  { was #define dname def_expr }
  function EGL_NO_IMAGE_KHR : EGLImageKHR;
      begin
         EGL_NO_IMAGE_KHR:=EGLImageKHR(0);
      end;


  var
    EGLLib : tlibhandle;


  { Address of an EGL entry point: the library export, else eglGetProcAddress }
  function EGLProcAddress(ProcName:PAnsiChar):pointer;
    begin
      result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}dynlibs.GetProcAddress(EGLLib,ProcName);
      if assigned(eglGetProcAddress) and not assigned(result) then
        result:=eglGetProcAddress(ProcName);
    end;


  procedure FreeEGL;
    begin
      if EGLLib<>0 then
        FreeLibrary(EGLLib);

      eglGetError:=nil;
      eglGetDisplay:=nil;
      eglInitialize:=nil;
      eglTerminate:=nil;
      eglQueryString:=nil;
      eglGetConfigs:=nil;
      eglChooseConfig:=nil;
      eglGetConfigAttrib:=nil;
      eglCreateWindowSurface:=nil;
      eglCreatePbufferSurface:=nil;
      eglCreatePixmapSurface:=nil;
      eglDestroySurface:=nil;
      eglQuerySurface:=nil;
      eglBindAPI:=nil;
      eglQueryAPI:=nil;
      eglWaitClient:=nil;
      eglReleaseThread:=nil;
      eglCreatePbufferFromClientBuffer:=nil;
      eglSurfaceAttrib:=nil;
      eglBindTexImage:=nil;
      eglReleaseTexImage:=nil;
      eglSwapInterval:=nil;
      eglCreateContext:=nil;
      eglDestroyContext:=nil;
      eglMakeCurrent:=nil;
      eglGetCurrentContext:=nil;
      eglGetCurrentSurface:=nil;
      eglGetCurrentDisplay:=nil;
      eglQueryContext:=nil;
      eglWaitGL:=nil;
      eglWaitNative:=nil;
      eglSwapBuffers:=nil;
      eglCopyBuffers:=nil;
      eglGetProcAddress:=nil;
    end;


  procedure LoadEGL(lib : PAnsiChar);
    begin
      FreeEGL;
      EGLLib:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}dynlibs.LoadLibrary(lib);
      if EGLLib=0 then
        raise Exception.Create(format('Could not load library: %s',[lib]));

      pointer(eglGetProcAddress):={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}dynlibs.GetProcAddress(EGLLib,'eglGetProcAddress');

      pointer(eglGetError):=EGLProcAddress('eglGetError');
      pointer(eglGetDisplay):=EGLProcAddress('eglGetDisplay');
      pointer(eglInitialize):=EGLProcAddress('eglInitialize');
      pointer(eglTerminate):=EGLProcAddress('eglTerminate');
      pointer(eglQueryString):=EGLProcAddress('eglQueryString');
      pointer(eglGetConfigs):=EGLProcAddress('eglGetConfigs');
      pointer(eglChooseConfig):=EGLProcAddress('eglChooseConfig');
      pointer(eglGetConfigAttrib):=EGLProcAddress('eglGetConfigAttrib');
      pointer(eglCreateWindowSurface):=EGLProcAddress('eglCreateWindowSurface');
      pointer(eglCreatePbufferSurface):=EGLProcAddress('eglCreatePbufferSurface');
      pointer(eglCreatePixmapSurface):=EGLProcAddress('eglCreatePixmapSurface');
      pointer(eglDestroySurface):=EGLProcAddress('eglDestroySurface');
      pointer(eglQuerySurface):=EGLProcAddress('eglQuerySurface');
      pointer(eglBindAPI):=EGLProcAddress('eglBindAPI');
      pointer(eglQueryAPI):=EGLProcAddress('eglQueryAPI');
      pointer(eglWaitClient):=EGLProcAddress('eglWaitClient');
      pointer(eglReleaseThread):=EGLProcAddress('eglReleaseThread');
      pointer(eglCreatePbufferFromClientBuffer):=EGLProcAddress('eglCreatePbufferFromClientBuffer');
      pointer(eglSurfaceAttrib):=EGLProcAddress('eglSurfaceAttrib');
      pointer(eglBindTexImage):=EGLProcAddress('eglBindTexImage');
      pointer(eglReleaseTexImage):=EGLProcAddress('eglReleaseTexImage');
      pointer(eglSwapInterval):=EGLProcAddress('eglSwapInterval');
      pointer(eglCreateContext):=EGLProcAddress('eglCreateContext');
      pointer(eglDestroyContext):=EGLProcAddress('eglDestroyContext');
      pointer(eglMakeCurrent):=EGLProcAddress('eglMakeCurrent');
      pointer(eglGetCurrentContext):=EGLProcAddress('eglGetCurrentContext');
      pointer(eglGetCurrentSurface):=EGLProcAddress('eglGetCurrentSurface');
      pointer(eglGetCurrentDisplay):=EGLProcAddress('eglGetCurrentDisplay');
      pointer(eglQueryContext):=EGLProcAddress('eglQueryContext');
      pointer(eglWaitGL):=EGLProcAddress('eglWaitGL');
      pointer(eglWaitNative):=EGLProcAddress('eglWaitNative');
      pointer(eglSwapBuffers):=EGLProcAddress('eglSwapBuffers');
      pointer(eglCopyBuffers):=EGLProcAddress('eglCopyBuffers');
    end;
