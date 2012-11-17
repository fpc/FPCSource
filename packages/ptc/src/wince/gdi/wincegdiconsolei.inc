{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2007, 2009-2011  Nikolay Nikolov (nickysn@users.sourceforge.net)

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

constructor TWinCEGDIConsole.Create;
begin
  inherited Create;

  FDisplayWidth := GetSystemMetrics(SM_CXSCREEN);
  FDisplayHeight := GetSystemMetrics(SM_CYSCREEN);
  FDefaultFormat := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FArea := TPTCArea.Create;
  FClip := TPTCArea.Create;

  SetLength(FModes, 1);
  FModes[0] := TPTCMode.Create(FDisplayWidth, FDisplayHeight, FDefaultFormat);
end;

destructor TWinCEGDIConsole.Destroy;
begin
  Close;

  FWindow.Free;

  FEventQueue.Free;
  FCopy.Free;
  FClear.Free;

  inherited Destroy;
end;

procedure TWinCEGDIConsole.Open(const ATitle: string; APages: Integer = 0);
begin
  Open(ATitle, FDefaultFormat, APages);
end;

procedure TWinCEGDIConsole.Open(const ATitle: string; AFormat: IPTCFormat;
                                APages: Integer = 0);
begin
  Open(ATitle, FDisplayWidth, FDisplayHeight, AFormat, APages);
end;

procedure TWinCEGDIConsole.Open(const ATitle: string; AMode: IPTCMode;
                                APages: Integer = 0);
begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
end;

procedure TWinCEGDIConsole.Open(const ATitle: string; AWidth, AHeight: Integer;
                                AFormat: IPTCFormat; APages: Integer = 0);
var
  DeviceContext: HDC;
begin
  LOG('TWinCEGDIConsole.Open');

  if FBitmap <> 0 then
  begin
    DeleteObject(FBitmap);
    FBitmap := 0;
  end;
  FreeAndNil(FWindow);
  FreeAndNil(FBitmapInfo);
  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);

  LOG('creating window');
  FWindow := TWinCEWindow.Create('PTC_GDI_WINDOWED_FIXED',
                                 ATitle,
                                 0,
                                 WS_VISIBLE {Or WS_SYSMENU or WS_CAPTION},
                                 SW_SHOWNORMAL,
                                 0, 0,
                                 FDisplayWidth, FDisplayHeight,
				 @WndProc);
  LOG('window created successfully');

  FBitmapInfo := TWinCEBitmapInfo.Create(FDisplayWidth, FDisplayHeight);

  LOG('trying to create a dib section');
  DeviceContext := GetDC(FWindow.WindowHandle);
  if DeviceContext = 0 then
    raise TPTCError.Create('could not get device context of window');
  FBitmap := CreateDIBSection(DeviceContext,
                              FBitmapInfo.BMI^,
                              DIB_RGB_COLORS,
                              FBitmapPixels,
                              0, 0);
  ReleaseDC(FWindow.WindowHandle, DeviceContext);
  if FBitmap = 0 then
    raise TPTCError.Create('could not create dib section');

  FArea := TPTCArea.Create(0, 0, FDisplayWidth, FDisplayHeight);
  FClip := FArea;

  FEventQueue := TEventQueue.Create;
  FKeyboard := TWinCEKeyboard.Create(FEventQueue);
  FMouse := TWinCEMouse.Create(FEventQueue, False, FDisplayWidth, FDisplayHeight);

  FWindow.Update;

  {todo...}
  FOpen := True;
  LOG('console open succeeded');
end;

procedure TWinCEGDIConsole.Close;
begin
  LOG('TWinCEGDIConsole.Close');

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);

  FBitmapPixels := nil; { just in case... }
  FreeAndNil(FBitmapInfo);
  if FBitmap <> 0 then
  begin
    DeleteObject(FBitmap);
    FBitmap := 0;
  end;
  FreeAndNil(FWindow);

  {todo...}

  FOpen := False;
end;

procedure TWinCEGDIConsole.Copy(ASurface: IPTCSurface);
begin
  {todo...}
end;

procedure TWinCEGDIConsole.Copy(ASurface: IPTCSurface;
                                ASource, ADestination: IPTCArea);
begin
  {todo...}
end;

procedure TWinCEGDIConsole.Load(const APixels: Pointer;
                                AWidth, AHeight, APitch: Integer;
                                AFormat: IPTCFormat;
                                APalette: IPTCPalette);
var
  console_pixels: Pointer;
begin
  CheckOpen(    'TWinCEGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
  CheckUnlocked('TWinCEGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
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

procedure TWinCEGDIConsole.Load(const APixels: Pointer;
                                AWidth, AHeight, APitch: Integer;
                                AFormat: IPTCFormat;
                                APalette: IPTCPalette;
                                ASource, ADestination: IPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: IPTCArea;
begin
  CheckOpen(    'TWinCEGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  CheckUnlocked('TWinCEGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
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

procedure TWinCEGDIConsole.Save(APixels: Pointer;
                                AWidth, AHeight, APitch: Integer;
                                AFormat: IPTCFormat;
                                APalette: IPTCPalette);
begin
  {todo...}
end;

procedure TWinCEGDIConsole.Save(APixels: Pointer;
                                AWidth, AHeight, APitch: Integer;
                                AFormat: IPTCFormat;
                                APalette: IPTCPalette;
                                ASource, ADestination: IPTCArea);
begin
  {todo...}
end;

function TWinCEGDIConsole.Lock: Pointer;
begin
  Result := FBitmapPixels; // todo...
  FLocked := True;
end;

procedure TWinCEGDIConsole.Unlock;
begin
  FLocked := False;
end;

procedure TWinCEGDIConsole.Clear;
begin
  {todo...}
end;

procedure TWinCEGDIConsole.Clear(AColor: IPTCColor);
begin
  {todo...}
end;

procedure TWinCEGDIConsole.Clear(AColor: IPTCColor;
                                 AArea: IPTCArea);
begin
  {todo...}
end;

procedure TWinCEGDIConsole.Configure(const AFileName: string);
var
  F: Text;
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

function TWinCEGDIConsole.Option(const AOption: string): Boolean;
begin
  LOG('console option', AOption);

  // todo...

  Result := FCopy.Option(AOption);
end;

procedure TWinCEGDIConsole.Palette(APalette: IPTCPalette);
begin
  {todo...}
end;

procedure TWinCEGDIConsole.Clip(AArea: IPTCArea);
begin
  CheckOpen('TWinCEGDIConsole.Clip(AArea)');

  FClip := TPTCClipper.Clip(AArea, FArea);
end;

function TWinCEGDIConsole.Clip: IPTCArea;
begin
  CheckOpen('TWinCEGDIConsole.Clip');
  Result := FClip;
end;

function TWinCEGDIConsole.Palette: IPTCPalette;
begin
  {todo...}
end;

function TWinCEGDIConsole.Modes: TPTCModeList;
begin
  Result := FModes;
end;

procedure TWinCEGDIConsole.Flush;
begin
  {todo...}
end;

procedure TWinCEGDIConsole.Finish;
begin
  {todo...}
end;

function TWinCEGDIConsole.WndProc(Ahwnd: HWND; AuMsg: UINT; AwParam: WPARAM; AlParam: LPARAM): LRESULT;
begin
  case AuMsg of
  WM_CLOSE: begin
    LOG('TWinCEGDIConsole.WndProc: WM_CLOSE');
    Halt(0);
  end;
  WM_KEYDOWN, WM_KEYUP: begin
    if FKeyboard <> nil then
      Result := FKeyboard.WndProc(Ahwnd, AuMsg, AwParam, AlParam)
    else
      Result := 0;
    exit;
  end;
  WM_MOUSEMOVE,
  WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK,
  WM_MBUTTONDOWN, WM_MBUTTONUP, WM_MBUTTONDBLCLK,
  WM_RBUTTONDOWN, WM_RBUTTONUP, WM_RBUTTONDBLCLK: begin
    if FMouse <> nil then
      Result := FMouse.WndProc(Ahwnd, AuMsg, AwParam, AlParam)
    else
      Result := 0;
    exit;
  end;
  else
    Result := DefWindowProcW(Ahwnd, AuMsg, AwParam, AlParam);
  end;
end;

procedure TWinCEGDIConsole.Update;
var
  ClientRect: RECT;
  DeviceContext, DeviceContext2: HDC;
begin
  CheckOpen(    'TWinCEGDIConsole.Update');
  CheckUnlocked('TWinCEGDIConsole.Update');

  FWindow.Update;

  DeviceContext := GetDC(FWindow.WindowHandle);

  if DeviceContext <> 0 then
  begin
    if GetClientRect(FWindow.WindowHandle, @ClientRect) then
    begin
      DeviceContext2 := CreateCompatibleDC(DeviceContext);
      if DeviceContext2 <> 0 then
      begin
        SelectObject(DeviceContext2, FBitmap);

        StretchBlt(DeviceContext,
                   0, 0, ClientRect.right, ClientRect.bottom,
                   DeviceContext2,
                   0, 0, FBitmapInfo.Width, FBitmapInfo.Height,
                   SRCCOPY);

        DeleteDC(DeviceContext2);
      end;
    end;

    ReleaseDC(FWindow.WindowHandle, DeviceContext);
  end;
end;

procedure TWinCEGDIConsole.Update(AArea: IPTCArea);
begin
  {todo...}
  Update;
end;

function TWinCEGDIConsole.NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
begin
  CheckOpen('TWinCEGDIConsole.NextEvent');
//  CheckUnlocked('TWinCEGDIConsole.NextEvent');

  repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);
  until (not AWait) or (AEvent <> Nil);
  Result := AEvent <> nil;
end;

function TWinCEGDIConsole.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
begin
  CheckOpen('TWinCEGDIConsole.PeekEvent');
//  CheckUnlocked('TWinCEGDIConsole.PeekEvent');

  repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);
  until (not AWait) or (Result <> Nil);
end;

function TWinCEGDIConsole.GetWidth: Integer;
begin
  CheckOpen('TWinCEGDIConsole.GetWidth');
  Result := FBitmapInfo.Width;
end;

function TWinCEGDIConsole.GetHeight: Integer;
begin
  CheckOpen('TWinCEGDIConsole.GetHeight');
  Result := FBitmapInfo.Height;
end;

function TWinCEGDIConsole.GetPitch: Integer;
begin
  CheckOpen('TWinCEGDIConsole.GetPitch');
  Result := FBitmapInfo.Pitch;
end;

function TWinCEGDIConsole.GetFormat: IPTCFormat;
begin
  CheckOpen('TWinCEGDIConsole.GetFormat');
  Result := FBitmapInfo.Format;
end;

function TWinCEGDIConsole.GetArea: IPTCArea;
begin
  CheckOpen('TWinCEGDIConsole.GetArea');
  Result := FArea;
end;

function TWinCEGDIConsole.GetPages: Integer;
begin
  CheckOpen('TWinCEGDIConsole.GetPages');
  Result := 2;
end;

function TWinCEGDIConsole.GetName: string;
begin
  Result := 'WinCE';
end;

function TWinCEGDIConsole.GetTitle: string;
begin
  CheckOpen('TWinCEGDIConsole.GetTitle');
  Result := FTitle;
end;

function TWinCEGDIConsole.GetInformation: string;
begin
  CheckOpen('TWinCEGDIConsole.GetInformation');
  Result := ''; // todo...
end;

procedure TWinCEGDIConsole.CheckOpen(AMessage: string);
begin
  if not FOpen then
  try
    raise TPTCError.Create('console is not open');
  except
    on error: TPTCError do
      raise TPTCError.Create(AMessage, error);
  end;
end;

procedure TWinCEGDIConsole.CheckUnlocked(AMessage: string);
begin
  if FLocked then
  try
    raise TPTCError.Create('console is locked');
  except
    on error: TPTCError do
      raise TPTCError.Create(AMessage, error);
  end;
end;
