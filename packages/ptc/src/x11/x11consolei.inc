{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2013 Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

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

constructor TX11Console.Create;
var
  s: AnsiString;
begin
  inherited Create;

  { default flags }
  FFlags := [PTC_X11_TRY_XSHM, PTC_X11_TRY_XF86VIDMODE];

  FTitle := '';

  Configure('/usr/share/ptcpas/ptcpas.conf');
  s := fpgetenv('HOME');
  if s = '' then
    s := '/';
  if s[Length(s)] <> '/' then
    s := s + '/';
  s := s + '.ptcpas.conf';
  Configure(s);
end;

destructor TX11Console.Destroy;
begin
  Close;
  FreeAndNil(FX11Display);
  inherited Destroy;
end;

procedure TX11Console.Configure(const AFileName: String);
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

function TX11Console.Option(const AOption: string): Boolean;
begin
  Result := True;
  if AOption = 'default output' then
  begin
    { default is windowed for now }
    FFlags := FFlags - [PTC_X11_FULLSCREEN];
    exit;
  end;
  if AOption = 'windowed output' then
  begin
    FFlags := FFlags - [PTC_X11_FULLSCREEN];
    exit;
  end;
  if AOption = 'fullscreen output' then
  begin
    FFlags := FFlags + [PTC_X11_FULLSCREEN];
    exit;
  end;
  if AOption = 'leave window open' then
  begin
    FFlags := FFlags + [PTC_X11_LEAVE_WINDOW];
    exit;
  end;
  if AOption = 'leave display open' then
  begin
    FFlags := FFlags + [PTC_X11_LEAVE_DISPLAY];
    exit;
  end;
  if (AOption = 'dga') or (AOption = 'dga on') then
  begin
    FFlags := FFlags + [PTC_X11_TRY_DGA1, PTC_X11_TRY_DGA2];
    exit;
  end;
  if AOption = 'dga off' then
  begin
    FFlags := FFlags - [PTC_X11_TRY_DGA1, PTC_X11_TRY_DGA2];
    exit;
  end;
  if (AOption = 'dga1') or (AOption = 'dga1 on') then
  begin
    FFlags := FFlags + [PTC_X11_TRY_DGA1];
    exit;
  end;
  if AOption = 'dga1 off' then
  begin
    FFlags := FFlags - [PTC_X11_TRY_DGA1];
    exit;
  end;
  if (AOption = 'dga2') or (AOption = 'dga2 on') then
  begin
    FFlags := FFlags + [PTC_X11_TRY_DGA2];
    exit;
  end;
  if AOption = 'dga2 off' then
  begin
    FFlags := FFlags - [PTC_X11_TRY_DGA2];
    exit;
  end;
  if (AOption = 'xf86vidmode') or (AOption = 'xf86vidmode on') then
  begin
    FFlags := FFlags + [PTC_X11_TRY_XF86VIDMODE];
    exit;
  end;
  if AOption = 'xf86vidmode off' then
  begin
    FFlags := FFlags - [PTC_X11_TRY_XF86VIDMODE];
    exit;
  end;
  if (AOption = 'xrandr') or (AOption = 'xrandr on') then
  begin
    FFlags := FFlags + [PTC_X11_TRY_XRANDR];
    exit;
  end;
  if AOption = 'xrandr off' then
  begin
    FFlags := FFlags - [PTC_X11_TRY_XRANDR];
    exit;
  end;
  if (AOption = 'xshm') or (AOption = 'xshm on') then
  begin
    FFlags := FFlags + [PTC_X11_TRY_XSHM];
    exit;
  end;
  if AOption = 'xshm off' then
  begin
    FFlags := FFlags - [PTC_X11_TRY_XSHM];
    exit;
  end;
  if AOption = 'default cursor' then
  begin
    FFlags := FFlags - [PTC_X11_FULLSCREEN_CURSOR_VISIBLE, PTC_X11_WINDOWED_CURSOR_INVISIBLE];
    UpdateCursor;
    exit;
  end;
  if AOption = 'show cursor' then
  begin
    FFlags := (FFlags - [PTC_X11_WINDOWED_CURSOR_INVISIBLE]) + [PTC_X11_FULLSCREEN_CURSOR_VISIBLE];
    UpdateCursor;
    exit;
  end;
  if AOption = 'hide cursor' then
  begin
    FFlags := (FFlags - [PTC_X11_FULLSCREEN_CURSOR_VISIBLE]) + [PTC_X11_WINDOWED_CURSOR_INVISIBLE];
    UpdateCursor;
    exit;
  end;
  if AOption = 'grab mouse' then
  begin
    FFlags := FFlags + [PTC_X11_GRAB_MOUSE];
    UpdateMouseGrab;
    exit;
  end;
  if AOption = 'ungrab mouse' then
  begin
    FFlags := FFlags - [PTC_X11_GRAB_MOUSE];
    UpdateMouseGrab;
    exit;
  end;
  if AOption = 'intercept window close' then
  begin
    FFlags := FFlags + [PTC_X11_INTERCEPT_WINDOW_CLOSE];
    if Assigned(FX11Display) then
      FX11Display.InterceptClose := True;
    exit;
  end;
  if AOption = 'resizable window' then
  begin
    FFlags := FFlags + [PTC_X11_RESIZABLE_WINDOW];
    exit;
  end;
  if AOption = 'fixed window' then
  begin
    FFlags := FFlags - [PTC_X11_RESIZABLE_WINDOW];
    exit;
  end;
  if AOption = 'enable logging' then
  begin
    LOG_enabled := True;
    Result := True;
    exit;
  end;
  if AOption = 'disable logging' then
  begin
    LOG_enabled := False;
    Result := True;
    exit;
  end;

  if Assigned(FX11Display) then
    Result := FX11Display.FCopy.Option(AOption)
  else
    Result := False;
end;

function TX11Console.GetOpenGL_Enabled: Boolean;
begin
  Result := PTC_X11_USE_OPENGL in FFlags;
end;

procedure TX11Console.SetOpenGL_Enabled(AValue: Boolean);
begin
  if (FX11Display <> nil) and FX11Display.IsOpen then
    raise TPTCError.Create('Value of UseOpenGL cannot be modified while the console is open');
  if AValue then
    FFlags := FFlags + [PTC_X11_USE_OPENGL]
  else
    FFlags := FFlags - [PTC_X11_USE_OPENGL];
end;

function TX11Console.Modes: TPTCModeList;
begin
  if FX11Display = nil then
    FX11Display := CreateDisplay;

  FX11Display.GetModes(FModes);

  Result := FModes;
end;

{TODO: Find current pixel depth}
procedure TX11Console.Open(const ATitle: string; APages: Integer = 0);
begin
  Open(ATitle, TPTCFormat.Create(32, $FF0000, $FF00, $FF), APages);
end;

procedure TX11Console.Open(const ATitle: string; AFormat: IPTCFormat;
                           APages: Integer = 0);
begin
  Open(ATitle, 640, 480, AFormat, APages);
end;

procedure TX11Console.Open(const ATitle: string; AMode: IPTCMode;
                           APages: Integer = 0);
begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
end;

function TX11Console.CreateDisplay: TX11Display;
var
  display: PDisplay;
  screen: Integer;
  DetectableAutoRepeatSupported: Boolean;
begin
  { Check if we can open an X display }
  display := XOpenDisplay(nil);
  if display = nil then
    raise TPTCError.Create('Cannot open X display');

  { DefaultScreen should be fine }
  screen := DefaultScreen(display);

  if not XkbSetDetectableAutoRepeat(display, True, @DetectableAutoRepeatSupported) then
    LOG('DetectableAutoRepeat not supported')
  else
    if not DetectableAutoRepeatSupported then
      LOG('DetectableAutoRepeat not supported');

  {$IFDEF ENABLE_X11_EXTENSION_XF86DGA2}
  if (PTC_X11_TRY_DGA2 in FFlags) and not (PTC_X11_USE_OPENGL in FFlags) then
  begin
    try
      Result := TX11DGA2Display.Create(display, screen, FFlags + [PTC_X11_LEAVE_DISPLAY]);
      Result.SetFlags(FFlags);
      exit;
    except
      LOG('DGA 2.0 failed');
    end;
  end;
  {$ENDIF ENABLE_X11_EXTENSION_XF86DGA2}

  {$IFDEF ENABLE_X11_EXTENSION_XF86DGA1}
  if (PTC_X11_TRY_DGA1 in FFlags) and not (PTC_X11_USE_OPENGL in FFlags) then
  begin
    try
      Result := TX11DGA1Display.Create(display, screen, FFlags + [PTC_X11_LEAVE_DISPLAY]);
      Result.SetFlags(FFlags);
    except
      LOG('DGA 1.0 failed');
    end;
  end;
  {$ENDIF ENABLE_X11_EXTENSION_XF86DGA1}

  Result := TX11WindowDisplay.Create(display, screen, FFlags);
end;

procedure TX11Console.Open(const ATitle: string; AWidth, AHeight: Integer;
                           AFormat: IPTCFormat; APages: Integer = 0);
begin
  Close;
  FTitle := ATitle;

  if FX11Display = nil then
    FX11Display := CreateDisplay;
  FX11Display.Open(ATitle, AWidth, AHeight, AFormat, FOpenGLAttributes);

  UpdateCursor;
end;

procedure TX11Console.Close;
begin
  FreeAndNil(FX11Display);
end;

procedure TX11Console.InternalResize(AWidth, AHeight: Integer);
begin
  FX11Display.InternalResize(AWidth, AHeight);
end;

procedure TX11Console.Flush;
begin
  Update;
end;

procedure TX11Console.Finish;
begin
  Update;
end;

procedure TX11Console.Update;
begin
  FX11Display.Update;
end;

procedure TX11Console.Update(AArea: IPTCArea);
begin
  FX11Display.Update(AArea);
end;

function TX11Console.NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
begin
  Result := FX11Display.NextEvent(AEvent, AWait, AEventMask);
end;

function TX11Console.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
begin
  Result := FX11Display.PeekEvent(AWait, AEventMask);
end;

procedure TX11Console.Copy(ASurface: IPTCSurface);
begin
  {todo!...}
end;

procedure TX11Console.Copy(ASurface: IPTCSurface;
                           ASource, ADestination: IPTCArea);
begin
  {todo!...}
end;

function TX11Console.Lock: Pointer;
begin
  Result := FX11Display.Lock;
end;

procedure TX11Console.Unlock;
begin
  FX11Display.Unlock;
end;

procedure TX11Console.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette);
begin
  FX11Display.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette);
end;

procedure TX11Console.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette;
                           ASource, ADestination: IPTCArea);
begin
  FX11Display.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination);
end;

procedure TX11Console.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette);
begin
  {todo!...}
end;

procedure TX11Console.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           AFormat: IPTCFormat;
                           APalette: IPTCPalette;
                           ASource, ADestination: IPTCArea);
begin
  {todo!...}
end;

procedure TX11Console.Clear;
var
  tmp: IPTCColor;
begin
  if Format.Direct then
    tmp := TPTCColor.Create(0, 0, 0, 0)
  else
    tmp := TPTCColor.Create(0);

  Clear(tmp);
end;

procedure TX11Console.Clear(AColor: IPTCColor);
begin
  FX11Display.Clear(AColor);
end;

procedure TX11Console.Clear(AColor: IPTCColor;
                            AArea: IPTCArea);
begin
  FX11Display.Clear(AColor, AArea);
end;

procedure TX11Console.Palette(APalette: IPTCPalette);
begin
  FX11Display.Palette(APalette);
end;

function TX11Console.Palette: IPTCPalette;
begin
  Result := FX11Display.Palette;
end;

procedure TX11Console.Clip(AArea: IPTCArea);
begin
  FX11Display.Clip(AArea);
end;

function TX11Console.GetWidth: Integer;
begin
  Result := FX11Display.Width;
end;

function TX11Console.GetHeight: Integer;
begin
  Result := FX11Display.Height;
end;

function TX11Console.GetPitch: Integer;
begin
  Result := FX11Display.Pitch;
end;

function TX11Console.GetPages: Integer;
begin
  Result := 2;
end;

function TX11Console.GetArea: IPTCArea;
begin
  Result := FX11Display.Area;
end;

function TX11Console.Clip: IPTCArea;
begin
  Result := FX11Display.Clip;
end;

function TX11Console.GetFormat: IPTCFormat;
begin
  Result := FX11Display.Format;
end;

function TX11Console.GetName: string;
begin
  Result := 'X11';
end;

function TX11Console.GetTitle: string;
begin
  Result := FTitle;
end;

function TX11Console.GetInformation: string;
begin
  if FX11Display = nil then
    exit('PTC X11');
  Result := 'PTC X11, ';
  if FX11Display.IsFullScreen then
    Result := Result + 'fullscreen '
  else
    Result := Result + 'windowed ';

  { TODO: use virtual methods, instead of "is" }
  if FX11Display Is TX11WindowDisplay then
  begin
    if TX11WindowDisplay(FX11Display).FPrimary <> nil then
      Result := Result + '(' + TX11WindowDisplay(FX11Display).FPrimary.Name + ') '
    else
      Result := Result + '';
  end
  else
  begin
    {$IFDEF ENABLE_X11_EXTENSION_XF86DGA2}
    if FX11Display Is TX11DGA2Display then
      Result := Result + '(DGA) '
    else
    {$ENDIF ENABLE_X11_EXTENSION_XF86DGA2}
    {$IFDEF ENABLE_X11_EXTENSION_XF86DGA1}
    if FX11Display Is TX11DGA1Display then
      Result := Result + '(DGA) '
    else
    {$ENDIF ENABLE_X11_EXTENSION_XF86DGA1}
    begin
      {...}
    end;
  end;
  Result := Result + 'mode, ' +
            IntToStr(FX11Display.Width) + 'x' +
            IntToStr(FX11Display.Height) + ', ' +
            IntToStr(FX11Display.Format.Bits) + ' bit';
end;

procedure TX11Console.UpdateCursor;
begin
  if Assigned(FX11Display) then
  begin
    if FX11Display.IsFullScreen then
      FX11Display.SetCursor(PTC_X11_FULLSCREEN_CURSOR_VISIBLE in FFlags)
    else
      FX11Display.SetCursor(not (PTC_X11_WINDOWED_CURSOR_INVISIBLE in FFlags));
  end;
end;

procedure TX11Console.UpdateMouseGrab;
begin
  if Assigned(FX11Display) then
    FX11Display.SetMouseGrab(PTC_X11_GRAB_MOUSE in FFlags);
end;

procedure TX11Console.OpenGL_SwapBuffers;
begin
  FX11Display.OpenGL_SwapBuffers;
end;

procedure TX11Console.OpenGL_SetSwapInterval(AInterval: Integer);
begin
  FX11Display.OpenGL_SetSwapInterval(AInterval);
end;

function TX11Console.OpenGL_GetSwapInterval: Integer;
begin
  Result := FX11Display.OpenGL_GetSwapInterval;
end;
