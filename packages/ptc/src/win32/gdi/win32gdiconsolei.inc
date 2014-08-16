{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2007, 2009-2013  Nikolay Nikolov (nickysn@users.sourceforge.net)

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

constructor TGDIConsole.Create;
begin
  inherited Create;

  FCursorMode := CURSOR_DEFAULT;

  FDisplayWidth := GetSystemMetrics(SM_CXSCREEN);
  FDisplayHeight := GetSystemMetrics(SM_CYSCREEN);

  FDefaultWidth := 640;
  FDefaultHeight := 480;
  FDefaultFormat := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FArea := TPTCArea.Create;
  FClip := TPTCArea.Create;
  FPalette := TPTCPalette.Create;
  FWin32Cursor := TWin32Cursor.Create;

  FFullscreen := True;
  FResizable := False;

  FModeSetter := TWin32ModeSetter.Create;

  FOpen := False;

  SetLength(FModes, 1);
  FModes[0] := TPTCMode.Create(FDisplayWidth, FDisplayHeight, FDefaultFormat);

  { configure console }
  Configure('ptcpas.cfg');
end;

destructor TGDIConsole.Destroy;
var
  I: Integer;
begin
  Close;

  {...}

  FModeSetter.Free;

  FWin32DIB.Free;
  FWindow.Free;
  FEventQueue.Free;
  FCopy.Free;
  FClear.Free;
  FWin32Cursor.Free;

  inherited Destroy;
end;

procedure TGDIConsole.Open(const ATitle: string; APages: Integer = 0);
begin
  Open(ATitle, FDefaultFormat, APages);
end;

procedure TGDIConsole.Open(const ATitle: string; AFormat: IPTCFormat;
               APages: Integer = 0);
begin
  Open(ATitle, FDefaultWidth, FDefaultHeight, AFormat, APages);
end;

procedure TGDIConsole.Open(const ATitle: string; AMode: IPTCMode;
                           APages: Integer = 0);
begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
end;

procedure TGDIConsole.Open(const ATitle: string; AWidth, AHeight: Integer;
                           AFormat: IPTCFormat; APages: Integer = 0);
var
  tmpArea: IPTCArea;
begin
  if FOpen then
    Close;

  case FCursorMode of
    CURSOR_DEFAULT:
      FCursor := not FFullScreen;
    CURSOR_SHOW:
      FCursor := True;
    CURSOR_HIDE:
      FCursor := False;
  end;

  if FFullscreen then
  begin
    FModeSetter.Save;
    FModeSetter.Open(AWidth, AHeight, AFormat);
  end;

  if FFullscreen then
  begin
    if FUseOpenGL then
      FWindow := TWin32OpenGLWindow.Create('PTC_OPENGL_FULLSCREEN',
                                           ATitle,
                                           WS_EX_TOPMOST or WS_EX_APPWINDOW,
                                           WS_VISIBLE or WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN,
                                           CS_HREDRAW or CS_VREDRAW or CS_OWNDC,
                                           SW_NORMAL,
                                           CW_USEDEFAULT, CW_USEDEFAULT,
                                           AWidth, AHeight,
                                           {m_center_window}False,
                                           False,
                                           FCursor,
                                           InterceptClose,
                                           FOpenGLAttributes)
    else
      FWindow := TWin32Window.Create('PTC_GDI_FULLSCREEN',
                                     ATitle,
                                     WS_EX_TOPMOST or WS_EX_APPWINDOW,
                                     WS_VISIBLE or WS_POPUP,
                                     CS_HREDRAW or CS_VREDRAW,
                                     SW_NORMAL,
                                     CW_USEDEFAULT, CW_USEDEFAULT,
                                     AWidth, AHeight,
                                     {m_center_window}False,
                                     False,
                                     FCursor,
                                     InterceptClose);
  end
  else
  begin
    if FUseOpenGL then
      if FResizable then
        FWindow := TWin32OpenGLWindow.Create('PTC_OPENGL_WINDOWED_RESIZABLE',
                                             ATitle,
                                             0,
                                             WS_OVERLAPPEDWINDOW or WS_VISIBLE or WS_CLIPSIBLINGS or WS_CLIPCHILDREN,
                                             CS_HREDRAW or CS_VREDRAW or CS_OWNDC,
                                             SW_NORMAL,
                                             CW_USEDEFAULT, CW_USEDEFAULT,
                                             AWidth, AHeight,
                                             {m_center_window}False,
                                             False,
                                             FCursor,
                                             InterceptClose,
                                             FOpenGLAttributes)
      else
        FWindow := TWin32OpenGLWindow.Create('PTC_OPENGL_WINDOWED_FIXED',
                                             ATitle,
                                             0,
                                             WS_VISIBLE or WS_SYSMENU or WS_CAPTION or WS_MINIMIZEBOX or WS_CLIPSIBLINGS or WS_CLIPCHILDREN,
                                             CS_HREDRAW or CS_VREDRAW or CS_OWNDC,
                                             SW_NORMAL,
                                             CW_USEDEFAULT, CW_USEDEFAULT,
                                             AWidth, AHeight,
                                             {m_center_window}False,
                                             False,
                                             FCursor,
                                             InterceptClose,
                                             FOpenGLAttributes)
    else
      if FResizable then
        FWindow := TWin32Window.Create('PTC_GDI_WINDOWED_RESIZABLE',
                                       ATitle,
                                       0,
                                       WS_OVERLAPPEDWINDOW or WS_VISIBLE,
                                       CS_HREDRAW or CS_VREDRAW,
                                       SW_NORMAL,
                                       CW_USEDEFAULT, CW_USEDEFAULT,
                                       AWidth, AHeight,
                                       {m_center_window}False,
                                       False,
                                       FCursor,
                                       InterceptClose)
      else
        FWindow := TWin32Window.Create('PTC_GDI_WINDOWED_FIXED',
                                       ATitle,
                                       0,
                                       WS_VISIBLE or WS_SYSMENU or WS_CAPTION or WS_MINIMIZEBOX,
                                       CS_HREDRAW or CS_VREDRAW,
                                       SW_NORMAL,
                                       CW_USEDEFAULT, CW_USEDEFAULT,
                                       AWidth, AHeight,
                                       {m_center_window}False,
                                       False,
                                       FCursor,
                                       InterceptClose);
  end;

  FWin32DIB := TWin32DIB.Create(AWidth, AHeight);

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FResize);
  FreeAndNil(FHook);
  FreeAndNil(FEventQueue);
  FEventQueue := TEventQueue.Create;
  FHook := TGDIHook.Create(Self, FWindow.Handle, FWindow.Thread, FCursor, {AManaged}True, FFullScreen);
  FKeyboard := TWin32Keyboard.Create(FWindow.Handle, FWindow.Thread, False, FEventQueue);
  FMouse := TWin32Mouse.Create(FWindow.Handle, FWindow.Thread, False, FEventQueue, FFullScreen, AWidth, AHeight);
  if FFullscreen then
    FMouse.SetWindowArea(0, 0, AWidth, AHeight);
  if not FFullscreen and FResizable then
    FResize := TWin32Resize.Create(FWindow.Handle, FWindow.Thread, FEventQueue);

  tmpArea := TPTCArea.Create(0, 0, AWidth, AHeight);
  FArea := tmpArea;
  FClip := tmpArea;

  FWindow.Update;

  { hide/show cursor globally if running fullscreen }
  if FFullscreen then
    if FCursor then
      FWin32Cursor.Show
    else
      FWin32Cursor.Hide;

  FTitle := ATitle;

  FOpen := True;
end;

procedure TGDIConsole.Close;
begin
  if not FOpen then
    exit;

  {...}

  if FFullscreen then
  begin
    FModeSetter.Close;
    FModeSetter.Restore;
    FWin32Cursor.Show;
  end;

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FResize);
  FreeAndNil(FHook);

  FreeAndNil(FWin32DIB);
  FreeAndNil(FWindow);

  FreeAndNil(FEventQueue);

  FTitle := '';

  FOpen := False;
end;

procedure TGDIConsole.InternalResize(AWidth, AHeight: Integer);
begin
  CheckOpen(    'TGDIConsole.InternalResize');
  CheckUnlocked('TGDIConsole.InternalResize');

  if FFullscreen then
    raise TPTCError.Create('TGDIConsole.InternalResize only works in windowed mode');
  if not FResizable then
    raise TPTCError.Create('TGDIConsole.InternalResize only works in resizable window mode');

  FreeAndNil(FWin32DIB);
  FWin32DIB := TWin32DIB.Create(AWidth, AHeight);
  FArea := TPTCAreaFactory.CreateNew(0, 0, AWidth, AHeight);
  FClip := FArea;

  FMouse.SetConsoleSize(AWidth, AHeight);
end;

procedure TGDIConsole.Copy(ASurface: IPTCSurface);
begin
  // todo...
end;

procedure TGDIConsole.Copy(ASurface: IPTCSurface;
                           ASource, ADestination: IPTCArea);
begin
  // todo...
end;

procedure TGDIConsole.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette);
var
  console_pixels: Pointer;
begin
  CheckOpen(    'TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
  CheckUnlocked('TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
  if Clip.Equals(Area) then
  begin
    try
      console_pixels := Lock;
      try
        FCopy.Request(AFormat, Format);
        FCopy.Palette(APalette, Palette);
        FCopy.Copy(APixels, 0, 0, AWidth, AHeight, APitch, console_pixels, 0, 0,
                    Width, Height, Pitch);
      finally
        Unlock;
      end;
    except
      on error: TPTCError do
        raise TPTCError.Create('failed to load pixels to console', error);
    end;
  end
  else
    Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, TPTCArea.Create(0, 0, width, height), Area);
end;

procedure TGDIConsole.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette;
                           ASource, ADestination: IPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: IPTCArea;
begin
  CheckOpen(    'TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  CheckUnlocked('TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  try
    console_pixels := Lock;
    try
      TPTCClipper.Clip(ASource, TPTCArea.Create(0, 0, AWidth, AHeight), clipped_source, ADestination, Clip, clipped_destination);
      FCopy.request(AFormat, Format);
      FCopy.palette(APalette, Palette);
      FCopy.copy(APixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, APitch,
                 console_pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, Pitch);
    finally
      Unlock;
    end;
  except
    on error: TPTCError do
      raise TPTCError.Create('failed to load pixels to console area', error);
  end;
end;

procedure TGDIConsole.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette);
begin
  // todo...
end;

procedure TGDIConsole.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette;
                           ASource, ADestination: IPTCArea);
begin
  // todo...
end;

function TGDIConsole.Lock: Pointer;
begin
  Result := FWin32DIB.Pixels; // todo...
  FLocked := True;
end;

procedure TGDIConsole.Unlock;
begin
  FLocked := False;
end;

procedure TGDIConsole.UpdateCursor;
begin
  if not FOpen then
    exit;

  case FCursorMode of
    CURSOR_DEFAULT:
      FCursor := not FFullScreen;
    CURSOR_SHOW:
      FCursor := True;
    CURSOR_HIDE:
      FCursor := False;
  end;

  { update hook cursor }
  FHook.Cursor(FCursor);

  { update window cursor }
  FWindow.Cursor(FCursor);

  { hide/show cursor globally if running fullscreen }
  if FFullscreen then
    if FCursor then
      FWin32Cursor.Show
    else
      FWin32Cursor.Hide;
end;

procedure TGDIConsole.Clear;
begin
  // todo...
end;

procedure TGDIConsole.Clear(AColor: IPTCColor);
begin
  // todo...
end;

procedure TGDIConsole.Clear(AColor: IPTCColor;
                            AArea: IPTCArea);
begin
  // todo...
end;

procedure TGDIConsole.Configure(const AFileName: String);
var
  F: TextFile;
  S: string;
begin
  AssignFile(F, AFileName);
  {$push}{$I-}
  Reset(F);
  {$pop}
  if IOResult <> 0 then
    exit;
  while not EoF(F) do
  begin
    {$push}{$I-}
    Readln(F, S);
    {$pop}
    if IOResult <> 0 then
      Break;
    Option(S);
  end;
  CloseFile(F);
end;

function TGDIConsole.Option(const AOption: String): Boolean;
begin
  LOG('console option', AOption);
  Result := True;
  if AOption = 'default output' then
  begin
    FFullscreen := False;
    exit;
  end;
  if AOption = 'windowed output' then
  begin
    FFullscreen := False;
    exit;
  end;
  if AOption = 'fullscreen output' then
  begin
    FFullscreen := True;
    exit;
  end;
  if AOption = 'resizable window' then
  begin
    FResizable := True;
    exit;
  end;
  if AOption = 'fixed window' then
  begin
    FResizable := False;
    exit;
  end;
  if AOption = 'default cursor' then
  begin
    FCursorMode := CURSOR_DEFAULT;
    UpdateCursor;
    exit;
  end;
  if AOption = 'show cursor' then
  begin
    FCursorMode := CURSOR_SHOW;
    UpdateCursor;
    exit;
  end;
  if AOption = 'hide cursor' then
  begin
    FCursorMode := CURSOR_HIDE;
    UpdateCursor;
    exit;
  end;
  if AOption = 'grab mouse' then
  begin
    if FOpen and (not FFullscreen) then
      FWindow.ConfineCursor(True);
    FGrabMouse := True;
    exit;
  end;
  if AOption = 'ungrab mouse' then
  begin
    if FOpen and (not FFullscreen) then
      FWindow.ConfineCursor(False);
    FGrabMouse := False;
    exit;
  end;
  if AOption = 'intercept window close' then
  begin
    InterceptClose := True;
    Result := True;
    exit;
  end;

  Result := FCopy.Option(AOption);
end;

procedure TGDIConsole.Palette(APalette: IPTCPalette);
begin
  // todo...
end;

procedure TGDIConsole.Clip(AArea: IPTCArea);
var
  tmp: TPTCArea;
begin
  CheckOpen('TGDIConsole.Clip(AArea)');

  FClip := TPTCClipper.Clip(AArea, FArea);
end;

function TGDIConsole.Clip: IPTCArea;
begin
  CheckOpen('TGDIConsole.Clip');
  Result := FClip;
end;

function TGDIConsole.Palette: IPTCPalette;
begin
  CheckOpen('TGDIConsole.Palette');
  Result := FPalette;
end;

function TGDIConsole.Modes: TPTCModeList;
begin
  Result := FModes;
end;

procedure TGDIConsole.Flush;
begin
  CheckOpen(    'TGDIConsole.Flush');
  CheckUnlocked('TGDIConsole.Flush');

  // todo...
end;

procedure TGDIConsole.Finish;
begin
  CheckOpen(    'TGDIConsole.Finish');
  CheckUnlocked('TGDIConsole.Finish');

  // todo...
end;

procedure TGDIConsole.Update;
var
  ClientRect: RECT;
  DeviceContext: HDC;
begin
  CheckOpen(    'TGDIConsole.Update');
  CheckUnlocked('TGDIConsole.Update');

  FWindow.Update;

  DeviceContext := GetDC(FWindow.Handle);

  if DeviceContext <> 0 then
  begin
    if GetClientRect(FWindow.Handle, @ClientRect) then
    begin
      StretchDIBits(DeviceContext,
                    0, 0, ClientRect.right, ClientRect.bottom,
                    0, 0, FWin32DIB.Width, FWin32DIB.Height,
                    FWin32DIB.Pixels,
                    FWin32DIB.BMI^,
                    DIB_RGB_COLORS,
                    SRCCOPY);
    end;

    ReleaseDC(FWindow.Handle, DeviceContext);
  end;
end;

procedure TGDIConsole.Update(AArea: IPTCArea);
begin
  Update;
end;

function TGDIConsole.NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
var
  UseGetMessage: Boolean;
begin
  CheckOpen('TGDIConsole.NextEvent');
//  CheckUnlocked('TGDIConsole.NextEvent');

  UseGetMessage := False;
  repeat
    { update window }
    FWindow.Update(False, UseGetMessage);

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);
    if AWait then
      UseGetMessage := True;
  until (not AWait) or (AEvent <> Nil);
  Result := AEvent <> nil;
end;

function TGDIConsole.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
var
  UseGetMessage: Boolean;
begin
  CheckOpen('TGDIConsole.PeekEvent');
//  CheckUnlocked('TGDIConsole.PeekEvent');

  UseGetMessage := False;
  repeat
    { update window }
    FWindow.Update(False, UseGetMessage);

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);
    if AWait then
      UseGetMessage := True;
  until (not AWait) or (Result <> Nil);
end;

function TGDIConsole.GetWidth: Integer;
begin
  CheckOpen('TGDIConsole.GetWidth');
  Result := FWin32DIB.Width;
end;

function TGDIConsole.GetHeight: Integer;
begin
  CheckOpen('TGDIConsole.GetHeight');
  Result := FWin32DIB.Height;
end;

function TGDIConsole.GetPitch: Integer;
begin
  CheckOpen('TGDIConsole.GetPitch');
  Result := FWin32DIB.Pitch;
end;

function TGDIConsole.GetArea: IPTCArea;
begin
  CheckOpen('TGDIConsole.GetArea');
  Result := FArea;
end;

function TGDIConsole.GetFormat: IPTCFormat;
begin
  CheckOpen('TGDIConsole.GetFormat');
  Result := FWin32DIB.Format;
end;

function TGDIConsole.GetPages: Integer;
begin
  CheckOpen('TGDIConsole.GetPages');
  Result := 2;
end;

function TGDIConsole.GetName: string;
begin
  Result := 'GDI';
end;

function TGDIConsole.GetTitle: string;
begin
  CheckOpen('TGDIConsole.GetTitle');
  Result := FTitle;
end;

function TGDIConsole.GetInformation: string;
begin
  CheckOpen('TGDIConsole.GetInformation');
  Result := ''; // todo...
end;

function TGDIConsole.GetOpenGL_Enabled: Boolean;
begin
  Result := FUseOpenGL;
end;

procedure TGDIConsole.SetOpenGL_Enabled(AValue: Boolean);
begin
  if FOpen then
    raise TPTCError.Create('Value of UseOpenGL cannot be modified while the console is open');
  FUseOpenGL := AValue;
end;

procedure TGDIConsole.OpenGL_SwapBuffers;
begin
  SwapBuffers(wglGetCurrentDC);
end;

procedure TGDIConsole.OpenGL_SetSwapInterval(AInterval: Integer);
begin
  LOG('SetSwapInterval(' + IntToStr(AInterval) + ')');
  if AInterval < 0 then
    raise TPTCError.Create('Invalid swap interval');
  LOG('checking if WGL_EXT_swap_control is supported');
  if Load_WGL_EXT_swap_control then
  begin
    LOG('using WGL_EXT_swap_control');
    LOG('wglSwapIntervalEXT(' + IntToStr(AInterval) + ')');
    if not wglSwapIntervalEXT(AInterval) then
	  LOG('wglSwapIntervalEXT failed');
  end
  else
    LOG('no supported extensions found for setting the swap interval');
end;

function TGDIConsole.OpenGL_GetSwapInterval: Integer;
begin
  LOG('GetSwapInterval');
  LOG('checking if WGL_EXT_swap_control is supported');
  if Load_WGL_EXT_swap_control then
  begin
    LOG('using WGL_EXT_swap_control');
    LOG('wglGetSwapIntervalEXT()');
    Result := wglGetSwapIntervalEXT();
    LOG('wglGetSwapIntervalEXT() result', Result);
  end
  else
  begin
    LOG('no supported extensions found for setting the swap interval, assuming the swap interval is 0');
    Result := 0;
  end;
end;

procedure TGDIConsole.SetInterceptClose(AInterceptClose: Boolean);
begin
  FInterceptClose := AInterceptClose;
  if Assigned(FWindow) then
    FWindow.InterceptClose := AInterceptClose;
end;

procedure TGDIConsole.CheckOpen(const AMessage: String);
begin
  if not FOpen then
  try
    raise TPTCError.Create('console is not open');
  except
    on error: TPTCError do
      raise TPTCError.Create(AMessage, error);
  end;
end;

procedure TGDIConsole.CheckUnlocked(const AMessage: String);
begin
  if FLocked then
  try
    raise TPTCError.Create('console is locked');
  except
    on error: TPTCError do
      raise TPTCError.Create(AMessage, error);
  end;
end;
