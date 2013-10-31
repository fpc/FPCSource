{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2012 Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$IFDEF ENABLE_X11_EXTENSION_GLX}

constructor TX11GLXFBConfig.Create(ADisplay: PDisplay; AScreen: cint; const AAttr: IPTCOpenGLAttributes);
begin
  FDisplay := ADisplay;
  FScreen := AScreen;

  { default swap interval for the GLX_SGI_swap_control extension }
  FSGISwapInterval := 1;

  if not GLX_version_1_0(FDisplay) then
    raise TPTCError.Create('GLX extension not supported');

  ChooseFBConfig(AAttr);
  GetVisualFromFBConfig;
end;

destructor TX11GLXFBConfig.Destroy;
begin
  if FRenderingContextHasBeenMadeCurrentAtLeastOnce then
    glXMakeContextCurrent(FDisplay, None, None, nil);

  if FGLXContext <> nil then
    glXDestroyContext(FDisplay, FGLXContext);

  if FGLXWindowCreated then
    glXDestroyWindow(FDisplay, FGLXWindow);

  inherited;
end;

procedure TX11GLXFBConfig.ChooseFBConfig(const AAttr: IPTCOpenGLAttributes);
const
  FBAttribList: array [0..18] of cint =
   (GLX_X_RENDERABLE, 1,
    GLX_RED_SIZE, 1,
    GLX_GREEN_SIZE, 1,
    GLX_BLUE_SIZE, 1,
    GLX_DOUBLEBUFFER, 1,
    GLX_STEREO, 0,
    GLX_BUFFER_SIZE, 0,
    GLX_DEPTH_SIZE, 0,
    GLX_STENCIL_SIZE, 0,
    None
   );
var
  FBConfigs: PGLXFBConfig = nil;
  FBConfigsCount: cint = 0;
begin
  if AAttr.DoubleBufferDontCare then
    FBAttribList[9] := cint(GLX_DONT_CARE)
  else
    if AAttr.DoubleBuffer then
      FBAttribList[9] := 1
    else
      FBAttribList[9] := 0;

  if AAttr.StereoDontCare then
    FBAttribList[11] := cint(GLX_DONT_CARE)
  else
    if AAttr.Stereo then
      FBAttribList[11] := 1
    else
      FBAttribList[11] := 0;

  FBAttribList[13] := AAttr.BufferSize;
  FBAttribList[15] := AAttr.DepthSize;
  FBAttribList[17] := AAttr.StencilSize;

  FBConfigs := glXChooseFBConfig(FDisplay, FScreen, @FBAttribList[0], FBConfigsCount);
  try
    if (FBConfigs = nil) or (FBConfigsCount = 0) then
      raise TPTCError.Create('Cannot find suitable OpenGL framebuffer configuration (GLXFBConfig)');
    FGLXFBConfig := FBConfigs[0];
  finally
    if FBConfigs <> nil then
      XFree(FBConfigs);
  end;
end;

procedure TX11GLXFBConfig.GetVisualFromFBConfig;
var
  vi: PXVisualInfo = nil;
begin
  vi := glXGetVisualFromFBConfig(FDisplay, FGLXFBConfig);
  try
    if vi = nil then
      raise TPTCError.Create('Cannot obtain XVisualInfo from GLXFBConfig');
    FVisInfo := vi^;
  finally
    if vi <> nil then
      XFree(vi);
  end;
end;

procedure TX11GLXFBConfig.CreateRenderingContext;
begin
  FGLXContext := glXCreateNewContext(FDisplay, FGLXFBConfig, GLX_RGBA_TYPE, nil, {False}True);
  if FGLXContext = nil then
    raise TPTCError.Create('Failed to create a GLX rendering context');
end;

procedure TX11GLXFBConfig.AttachToWindow(AWindow: TWindow);
begin
  CreateRenderingContext;

  FGLXWindow := glXCreateWindow(FDisplay, FGLXFBConfig, AWindow, nil);
{  if FGLXWindow = nil then
    raise TPTCError.Create('Failed to create onscreen rendering area in window');}
  FGLXWindowCreated := True;
end;

procedure TX11GLXFBConfig.MakeRenderingContextCurrent;
begin
  if not glXMakeContextCurrent(FDisplay, FGLXWindow, FGLXWindow, FGLXContext) then
    raise TPTCError.Create('Error making the rendering context current');
  FRenderingContextHasBeenMadeCurrentAtLeastOnce := True;

  { although GLX functions aren't supposed to be context-specific, it doesn't hurt
    to reload them after the context is ready }
  InitGLX;
end;

procedure TX11GLXFBConfig.SetSwapInterval(AInterval: Integer);

  function GlxResult2String(AGlxResult: cint): string;
  begin
    case AGlxResult of
      GLX_BAD_VALUE:   Result := 'GLX_BAD_VALUE';
      GLX_BAD_CONTEXT: Result := 'GLX_BAD_CONTEXT';
      else
        Result := 'Unknown (' + IntToStr(AGlxResult) + ')';
    end;
  end;

var
  GlxResult: cint;
begin
  LOG('SetSwapInterval(' + IntToStr(AInterval) + ')');
  if AInterval < 0 then
    raise TPTCError.Create('Invalid swap interval');
  LOG('checking if GLX_MESA_swap_control is supported');
  if GLX_MESA_swap_control(FDisplay, FScreen) then
  begin
    LOG('using GLX_MESA_swap_control');
    LOG('glXSwapIntervalMESA(' + IntToStr(AInterval) + ')');
    GlxResult := glXSwapIntervalMESA(AInterval);
    if GlxResult <> 0 then
      LOG('glXSwapIntervalMESA failed: ' + GlxResult2String(GlxResult));
  end
  else
  begin
    LOG('checking if GLX_SGI_swap_control is supported');
    if GLX_SGI_swap_control(FDisplay, FScreen) then
    begin
      LOG('using GLX_SGI_swap_control');
      if AInterval = 0 then
      begin
        LOG('Warning: According to spec, the GLX_SGI_swap_control extension doesn''t');
        LOG('support setting the swap interval to 0, so this call is likely to fail,');
        LOG('but we''ll try it anyway, just in case the drivers support it.');
      end;
      LOG('glXSwapIntervalSGI(' + IntToStr(AInterval) + ')');
      GlxResult := glXSwapIntervalSGI(AInterval);
      if GlxResult <> 0 then
        LOG('glXSwapIntervalSGI failed: ' + GlxResult2String(GlxResult));
      if GlxResult = 0 then
        FSGISwapInterval := AInterval;
    end
    else
      LOG('no supported extensions found for setting the swap interval');
  end;
end;

function TX11GLXFBConfig.GetSwapInterval: Integer;
begin
  LOG('GetSwapInterval');
  LOG('checking if GLX_MESA_swap_control is supported');
  if GLX_MESA_swap_control(FDisplay, FScreen) then
  begin
    LOG('using GLX_MESA_swap_control');
    LOG('glXGetSwapIntervalMESA()');
    Result := glXGetSwapIntervalMESA();
    LOG('glxGetSwapIntervalMESA() result', Result);
  end
  else
  begin
    LOG('checking if GLX_SGI_swap_control is supported');
    if GLX_SGI_swap_control(FDisplay, FScreen) then
    begin
      LOG('GLX_SGI_swap_control is supported');
      LOG('Returning the last set swap interval via this extension: ' + IntToStr(FSGISwapInterval));
      Result := FSGISwapInterval;
    end
    else
    begin
      LOG('no supported extensions found for setting the swap interval, assuming the swap interval is 0');
      Result := 0;
    end;
  end;
end;

procedure TX11GLXFBConfig.SwapBuffers;
begin
  glXSwapBuffers(FDisplay, FGLXWindow);
end;

{$ENDIF ENABLE_X11_EXTENSION_GLX}
