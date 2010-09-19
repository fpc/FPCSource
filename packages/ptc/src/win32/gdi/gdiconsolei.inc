{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

  FDefaultWidth := 320;
  FDefaultHeight := 200;
  FDefaultFormat := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FArea := TPTCArea.Create;
  FClip := TPTCArea.Create;
  FPalette := TPTCPalette.Create;

  FOpen := False;

  FModes[0] := TPTCMode.Create(FDisplayWidth, FDisplayHeight, FDefaultFormat);
  FModes[1] := TPTCMode.Create;

  { configure console }
  Configure('ptcpas.cfg');
end;

destructor TGDIConsole.Destroy;
var
  I: Integer;
begin
  Close;

  {...}

  FWin32DIB.Free;
  FWindow.Free;
  FPalette.Free;
  FEventQueue.Free;
  FCopy.Free;
  FClear.Free;
  FArea.Free;
  FClip.Free;
  FDefaultFormat.Free;

  for I := Low(FModes) to High(FModes) do
    FModes[I].Free;

  inherited Destroy;
end;

procedure TGDIConsole.Open(const ATitle: string; APages: Integer = 0);
begin
  Open(ATitle, FDefaultFormat, APages);
end;

procedure TGDIConsole.Open(const ATitle: string; const AFormat: TPTCFormat;
               APages: Integer = 0);
begin
  Open(ATitle, FDefaultWidth, FDefaultHeight, AFormat, APages);
end;

procedure TGDIConsole.Open(const ATitle: string; const AMode: TPTCMode;
                           APages: Integer = 0);
begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
end;

procedure TGDIConsole.Open(const ATitle: string; AWidth, AHeight: Integer;
               const AFormat: TPTCFormat; APages: Integer = 0);
var
  tmp: TPTCArea;
begin
  if FOpen then
    Close;

(*  FWindow := TWin32Window.Create('PTC_GDI_FULLSCREEN',
                                 ATitle,
                                 WS_EX_TOPMOST,
                                 DWord(WS_POPUP or WS_SYSMENU or WS_VISIBLE), // fpc windows RTL bug - WS_POPUP should be a DWord!!!
                                 SW_NORMAL,
                                 0, 0,
                                 GetSystemMetrics(SM_CXSCREEN),
                                 GetSystemMetrics(SM_CYSCREEN),
                                 False, False);*)

  FWindow := TWin32Window.Create('PTC_GDI_WINDOWED_FIXED',
                                 ATitle,
                                 0,
                                 WS_VISIBLE or WS_SYSMENU or WS_CAPTION or WS_MINIMIZEBOX,
                                 SW_NORMAL,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 AWidth, AHeight,
                                 {m_center_window}False,
                                 False);

(*  FWindow := TWin32Window.Create('PTC_GDI_WINDOWED_RESIZABLE',
                                 ATitle,
                                 0,
                                 WS_OVERLAPPEDWINDOW or WS_VISIBLE,
                                 SW_NORMAL,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 AWidth, AHeight,
                                 {m_center_window}False,
                                 False);*)

  FWin32DIB := TWin32DIB.Create(AWidth, AHeight);

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
  FEventQueue := TEventQueue.Create;
  FKeyboard := TWin32Keyboard.Create(FWindow.Handle, FWindow.Thread, False, FEventQueue);
  FMouse := TWin32Mouse.Create(FWindow.Handle, FWindow.Thread, False, FEventQueue, {FFullScreen}False, AWidth, AHeight);

  tmp := TPTCArea.Create(0, 0, AWidth, AHeight);
  try
    FArea.Assign(tmp);
    FClip.Assign(tmp);
  finally
    tmp.Free;
  end;

  FWindow.Update;

  FTitle := ATitle;

  FOpen := True;
end;

procedure TGDIConsole.Close;
begin
  if not FOpen then
    exit;

  {...}

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);

  FreeAndNil(FWin32DIB);
  FreeAndNil(FWindow);

  FreeAndNil(FEventQueue);

  FTitle := '';

  FOpen := False;
end;

procedure TGDIConsole.Copy(ASurface: TPTCBaseSurface);
begin
  // todo...
end;

procedure TGDIConsole.Copy(ASurface: TPTCBaseSurface;
                           const ASource, ADestination: TPTCArea);
begin
  // todo...
end;

procedure TGDIConsole.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette);
var
  Area_: TPTCArea;
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
  begin
    Area_ := TPTCArea.Create(0, 0, width, height);
    try
      Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, Area_, Area);
    finally
      Area_.Free;
    end;
  end;
end;

procedure TGDIConsole.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette;
                           const ASource, ADestination: TPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: TPTCArea;
  tmp: TPTCArea;
begin
  CheckOpen(    'TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  CheckUnlocked('TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  clipped_source := nil;
  clipped_destination := nil;
  try
    console_pixels := Lock;
    try
      clipped_source := TPTCArea.Create;
      clipped_destination := TPTCArea.Create;
      tmp := TPTCArea.Create(0, 0, AWidth, AHeight);
      try
        TPTCClipper.Clip(ASource, tmp, clipped_source, ADestination, Clip, clipped_destination);
      finally
        tmp.Free;
      end;
      FCopy.request(AFormat, Format);
      FCopy.palette(APalette, Palette);
      FCopy.copy(APixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, APitch,
                 console_pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, Pitch);
    finally
      Unlock;
      clipped_source.Free;
      clipped_destination.Free;
    end;
  except
    on error: TPTCError do
      raise TPTCError.Create('failed to load pixels to console area', error);
  end;
end;

procedure TGDIConsole.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette);
begin
  // todo...
end;

procedure TGDIConsole.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette;
                           const ASource, ADestination: TPTCArea);
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
      FCursor := {Not FFullScreen}True;
    CURSOR_SHOW:
      FCursor := True;
    CURSOR_HIDE:
      FCursor := False;
  end;

  { update window cursor }
  FWindow.Cursor(FCursor);

  { hide/show cursor globally if running fullscreen }
{  if FFullscreen then
    if FCursor then
      Win32Cursor_resurrect
    else
      Win32Cursor_kill;}
end;

procedure TGDIConsole.Clear;
begin
  // todo...
end;

procedure TGDIConsole.Clear(const AColor: TPTCColor);
begin
  // todo...
end;

procedure TGDIConsole.Clear(const AColor: TPTCColor;
                            const AArea: TPTCArea);
begin
  // todo...
end;

procedure TGDIConsole.Configure(const AFileName: String);
var
  F: TextFile;
  S: string;
begin
  AssignFile(F, AFileName);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult <> 0 then
    exit;
  while not EoF(F) do
  begin
    {$I-}
    Readln(F, S);
    {$I+}
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
    FWindow.ConfineCursor(true);
    exit;
  end;
  if AOption = 'ungrab mouse' then
  begin
    FWindow.ConfineCursor(false);
    exit;
  end;

  Result := FCopy.Option(AOption);
end;

procedure TGDIConsole.Palette(const APalette: TPTCPalette);
begin
  // todo...
end;

procedure TGDIConsole.Clip(const AArea: TPTCArea);
var
  tmp: TPTCArea;
begin
  CheckOpen('TGDIConsole.Clip(AArea)');

  tmp := TPTCClipper.Clip(AArea, FArea);
  try
    FClip.Assign(tmp);
  finally
    tmp.Free;
  end;
end;

function TGDIConsole.Clip: TPTCArea;
begin
  CheckOpen('TGDIConsole.Clip');
  Result := FClip;
end;

function TGDIConsole.Palette: TPTCPalette;
begin
  CheckOpen('TGDIConsole.Palette');
  Result := FPalette;
end;

function TGDIConsole.Modes: PPTCMode;
begin
  Result := @FModes[0];
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

procedure TGDIConsole.Update(const AArea: TPTCArea);
begin
  Update;
end;

function TGDIConsole.NextEvent(var AEvent: TPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
begin
  CheckOpen('TGDIConsole.NextEvent');
//  CheckUnlocked('TGDIConsole.NextEvent');

  FreeAndNil(AEvent);
  repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);
  until (not AWait) or (AEvent <> Nil);
  Result := AEvent <> nil;
end;

function TGDIConsole.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): TPTCEvent;
begin
  CheckOpen('TGDIConsole.PeekEvent');
//  CheckUnlocked('TGDIConsole.PeekEvent');

  repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);
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

function TGDIConsole.GetArea: TPTCArea;
begin
  CheckOpen('TGDIConsole.GetArea');
  Result := FArea;
end;

function TGDIConsole.GetFormat: TPTCFormat;
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
