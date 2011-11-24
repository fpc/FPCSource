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

  FWin32DIB.Free;
  FWindow.Free;
  FEventQueue.Free;
  FCopy.Free;
  FClear.Free;

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

  tmpArea := TPTCArea.Create(0, 0, AWidth, AHeight);
  FArea := tmpArea;
  FClip := tmpArea;

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
