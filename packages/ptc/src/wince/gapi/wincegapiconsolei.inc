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

constructor TWinCEGAPIConsole.Create;

begin
  inherited Create;

  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FArea := TPTCArea.Create;
  FClip := TPTCArea.Create;

  LOG('getting display properties');
  FGXDisplayProperties := GXGetDisplayProperties;
  LOG('width='  + IntToStr(FGXDisplayProperties.cxWidth ));
  LOG('height=' + IntToStr(FGXDisplayProperties.cyHeight));
  LOG('xpitch=' + IntToStr(FGXDisplayProperties.cbxPitch));
  LOG('ypitch=' + IntToStr(FGXDisplayProperties.cbyPitch));
  LOG('BPP='    + IntToStr(FGXDisplayProperties.cBPP    ));
  LOG('format=' + IntToStr(FGXDisplayProperties.ffFormat));

  FDisplayWidth := FGXDisplayProperties.cxWidth;
  FDisplayHeight := FGXDisplayProperties.cyHeight;
  FDisplayPitch := FGXDisplayProperties.cbyPitch;
  FDisplayFormat := nil;

  if (FGXDisplayProperties.ffFormat and kfDirect565) <> 0 then
    FDisplayFormat := TPTCFormat.Create(FGXDisplayProperties.cBPP,
                                        $F800, $07E0, $001F);

  if (FGXDisplayProperties.ffFormat and kfDirect888) <> 0 then
    FDisplayFormat := TPTCFormat.Create(FGXDisplayProperties.cBPP,
                                        $FF0000, $FF00, $FF);

  if (FGXDisplayProperties.ffFormat and kfDirect555) <> 0 then
    FDisplayFormat := TPTCFormat.Create(FGXDisplayProperties.cBPP,
                                        $7C00, $03E0, $001F);

  if (FGXDisplayProperties.ffFormat and kfDirect444) <> 0 then
    FDisplayFormat := TPTCFormat.Create(FGXDisplayProperties.cBPP,
                                        $0F00, $00F0, $000F);

  if FDisplayFormat = nil then
    raise TPTCError.Create('GAPI: Unknown/unsupported pixel format');

  FModes[0] := TPTCMode.Create(FDisplayWidth, FDisplayHeight, FDisplayFormat);
  FModes[1] := TPTCMode.Create;
end;

destructor TWinCEGAPIConsole.Destroy;

var
  I: Integer;

begin
  Close;

  FCopy.Free;
  FClear.Free;
  FArea.Free;
  FClip.Free;
  FDisplayFormat.Free;

  for I := Low(FModes) to High(FModes) do
    FModes[I].Free;

  inherited Destroy;
end;

procedure TWinCEGAPIConsole.Open(const ATitle: string; APages: Integer = 0);

begin
  Open(ATitle, FDisplayFormat, APages);
end;

procedure TWinCEGAPIConsole.Open(const ATitle: string; const AFormat: TPTCFormat;
                                 APages: Integer = 0);

begin
  Open(ATitle, FDisplayWidth, FDisplayHeight, AFormat, APages);
end;

procedure TWinCEGAPIConsole.Open(const ATitle: string; const AMode: TPTCMode;
                                 APages: Integer = 0);

begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
end;

procedure TWinCEGAPIConsole.Open(const ATitle: string; AWidth, AHeight: Integer;
                                 const AFormat: TPTCFormat; APages: Integer = 0);

var
  tmp: TPTCArea;

begin
  LOG('TWinCEGAPIConsole.Open');

  if FOpen then
    Close;

  try
    LOG('creating window');
    FWindow := TWinCEWindow.Create('PTC_GAPI_FULLSCREEN',
                                   ATitle,
                                   0,
                                   WS_VISIBLE {Or WS_SYSMENU or WS_CAPTION},
                                   SW_SHOWNORMAL,
                                   0, 0,
                                   FDisplayWidth, FDisplayHeight,
				   @WndProc);
    LOG('window created successfully');

    LOG('opening display');
    if GXOpenDisplay(FWindow.WindowHandle, GX_FULLSCREEN) <> 0 then
      FGXDisplayIsOpen := True {success!!!}
    else
      raise TPTCError.Create('could not open display');

    tmp := TPTCArea.Create(0, 0, FDisplayWidth, FDisplayHeight);
    try
      FArea.Assign(tmp);
      FClip.Assign(tmp);
    finally
      tmp.Free;
    end;

    FEventQueue := TEventQueue.Create;
    FKeyboard := TWinCEKeyboard.Create(FEventQueue);
    FMouse := TWinCEMouse.Create(FEventQueue, True, FDisplayWidth, FDisplayHeight);

    if {m_primary.m_fullscreen}True then
      FMouse.SetWindowArea(0, 0, FDisplayWidth, FDisplayHeight);

    FWindow.Update;

    FOpen := True;
  except
    on error: TObject do
    begin
      Close;
      Raise;
    end;
  end;
end;

procedure TWinCEGAPIConsole.Close;

begin
  LOG('TWinCEGAPIConsole.Close');

  if FGXDisplayIsOpen Then;
    GXCloseDisplay;
  FGXDisplayIsOpen := False;

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FWindow);
  FreeAndNil(FEventQueue);

  FOpen := False;
end;

procedure TWinCEGAPIConsole.Copy(ASurface: TPTCBaseSurface);

begin
end;

procedure TWinCEGAPIConsole.Copy(ASurface: TPTCBaseSurface;
                                 const ASource, ADestination: TPTCArea);

begin
end;

procedure TWinCEGAPIConsole.Load(const APixels: Pointer;
                                 AWidth, AHeight, APitch: Integer;
                                 const AFormat: TPTCFormat;
                                 const APalette: TPTCPalette);
var
  Area_: TPTCArea;
  console_pixels: Pointer;

begin
  CheckOpen(    'TWinCEGAPIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
  CheckUnlocked('TWinCEGAPIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
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

procedure TWinCEGAPIConsole.Load(const APixels: Pointer;
                                 AWidth, AHeight, APitch: Integer;
                                 const AFormat: TPTCFormat;
                                 const APalette: TPTCPalette;
                                 const ASource, ADestination: TPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: TPTCArea;
  tmp: TPTCArea;

begin
  CheckOpen(    'TWinCEGAPIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  CheckUnlocked('TWinCEGAPIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
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

procedure TWinCEGAPIConsole.Save(APixels: Pointer;
                                 AWidth, AHeight, APitch: Integer;
                                 const AFormat: TPTCFormat;
                                 const APalette: TPTCPalette);

begin
end;

procedure TWinCEGAPIConsole.Save(APixels: Pointer;
                                 AWidth, AHeight, APitch: Integer;
                                 const AFormat: TPTCFormat;
                                 const APalette: TPTCPalette;
                                 const ASource, ADestination: TPTCArea);

begin
end;

function TWinCEGAPIConsole.Lock: Pointer;

begin
  CheckUnlocked('display already locked');
  Result := GXBeginDraw;

  if Result = nil then
    raise TPTCError.Create('the display cannot be locked');

  FLocked := True;
end;

procedure TWinCEGAPIConsole.Unlock;

begin
  if not FLocked then
    raise TPTCError.Create('display is not locked');

  if GXEndDraw = 0 then
    raise TPTCError.Create('could not unlock display');

  FLocked := False;
end;

procedure TWinCEGAPIConsole.Clear;

begin
end;

procedure TWinCEGAPIConsole.Clear(const AColor: TPTCColor);

begin
end;

procedure TWinCEGAPIConsole.Clear(const AColor: TPTCColor;
                    const AArea: TPTCArea);

begin
end;

procedure TWinCEGAPIConsole.Configure(const AFileName: String);

var
  F: Text;
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

function TWinCEGAPIConsole.Option(const AOption: String): Boolean;

begin
  LOG('console option', AOption);

  // todo...

  Result := FCopy.Option(AOption);
end;

procedure TWinCEGAPIConsole.Palette(const APalette: TPTCPalette);

begin
end;

procedure TWinCEGAPIConsole.Clip(const AArea: TPTCArea);

var
  tmp: TPTCArea;

begin
  CheckOpen('TWinCEGAPIConsole.Clip(AArea)');

  tmp := TPTCClipper.Clip(AArea, FArea);
  try
    FClip.Assign(tmp);
  finally
    tmp.Free;
  end;
end;

function TWinCEGAPIConsole.Clip: TPTCArea;

begin
  CheckOpen('TWinCEGAPIConsole.Clip');
  Result := FClip;
end;

function TWinCEGAPIConsole.Palette: TPTCPalette;

begin
end;

function TWinCEGAPIConsole.Modes: PPTCMode;

begin
  Result := @FModes[0];
end;

function TWinCEGAPIConsole.WndProc(Ahwnd: HWND; AuMsg: UINT; AwParam: WPARAM; AlParam: LPARAM): LRESULT;

begin
  case AuMsg of
  WM_CLOSE: begin
    LOG('TWinCEGAPIConsole.WndProc: WM_CLOSE');
    Halt(0);
  end;
  WM_KILLFOCUS: begin
    LOG('TWinCEGAPIConsole.WndProc: WM_KILLFOCUS');
    if FGXDisplayIsOpen then
      GXSuspend;
    Result := 0;
    exit;
  end;
  WM_SETFOCUS: begin
    LOG('TWinCEGAPIConsole.WndProc: WM_SETFOCUS');
    if FGXDisplayIsOpen then
      GXResume;
    Result := 0;
    exit;
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

procedure TWinCEGAPIConsole.Flush;

begin
  CheckOpen    ('TWinCEGAPIConsole.Flush');
  CheckUnlocked('TWinCEGAPIConsole.Flush');

  Update;
end;

procedure TWinCEGAPIConsole.Finish;

begin
  CheckOpen    ('TWinCEGAPIConsole.Finish');
  CheckUnlocked('TWinCEGAPIConsole.Finish');

  Update;
end;

procedure TWinCEGAPIConsole.Update;

begin
  CheckOpen    ('TWinCEGAPIConsole.Update');
  CheckUnlocked('TWinCEGAPIConsole.Update');

  FWindow.Update;
end;

procedure TWinCEGAPIConsole.Update(const AArea: TPTCArea);

begin
  Update;
end;

function TWinCEGAPIConsole.NextEvent(var AEvent: TPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;

begin
  CheckOpen('TWinCEGAPIConsole.NextEvent');
//  CheckUnlocked('TWinCEGAPIConsole.NextEvent');

  FreeAndNil(AEvent);
  repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);
  until (not AWait) or (AEvent <> Nil);
  Result := AEvent <> nil;
end;

function TWinCEGAPIConsole.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): TPTCEvent;

begin
  CheckOpen('TWinCEGAPIConsole.PeekEvent');
//  CheckUnlocked('TWinCEGAPIConsole.PeekEvent');

  repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);
  until (not AWait) or (Result <> nil);
end;

function TWinCEGAPIConsole.GetWidth: Integer;

begin
  CheckOpen('TWinCEGAPIConsole.GetWidth');
  Result := FDisplayWidth;
end;

function TWinCEGAPIConsole.GetHeight: Integer;

begin
  CheckOpen('TWinCEGAPIConsole.GetHeight');
  Result := FDisplayHeight;
end;

function TWinCEGAPIConsole.GetPitch: Integer;

begin
  CheckOpen('TWinCEGAPIConsole.GetPitch');
  Result := FDisplayPitch;
end;

function TWinCEGAPIConsole.GetArea: TPTCArea;

begin
  CheckOpen('TWinCEGAPIConsole.GetArea');
  Result := FArea;
end;

function TWinCEGAPIConsole.GetFormat: TPTCFormat;

begin
  CheckOpen('TWinCEGAPIConsole.GetFormat');
  Result := FDisplayFormat;
end;

function TWinCEGAPIConsole.GetPages: Integer;

begin
  CheckOpen('TWinCEGAPIConsole.GetPages');
  Result := 1; {???}
end;

function TWinCEGAPIConsole.GetName: string;

begin
  Result := 'GAPI';
end;

function TWinCEGAPIConsole.GetTitle: string;

begin
  CheckOpen('TWinCEGAPIConsole.GetTitle');
  Result := FTitle;
end;

function TWinCEGAPIConsole.GetInformation: string;

begin
  Result := ''; // todo...
end;

procedure TWinCEGAPIConsole.CheckOpen(    AMessage: String);

begin
  if not FOpen then
  try
    raise TPTCError.Create('console is not open');
  except
    on error: TPTCError do
      raise TPTCError.Create(AMessage, error);
  end;
end;

procedure TWinCEGAPIConsole.CheckUnlocked(AMessage: String);

begin
  if FLocked then
  try
    raise TPTCError.Create('console is locked');
  except
    on error: TPTCError do
      raise TPTCError.Create(AMessage, error);
  end;
end;
