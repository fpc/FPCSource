{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2012 Nikolay Nikolov (nickysn@users.sourceforge.net)
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

{$IFDEF ENABLE_X11_EXTENSION_XF86DGA1}

constructor TX11DGA1Display.Create(ADisplay: PDisplay; AScreen: Integer; const AFlags: TX11Flags);
var
  dummy1, dummy2: Integer;
begin
  inherited;

  LOG('trying to create a DGA 1.0 display');

  FInDirect := False;
  FInMode := False;
  FModeInfo := nil;

  { Check if we are root }
  if fpgeteuid <> 0 then
    raise TPTCError.Create('Have to be root to switch to DGA mode');

  { Check if the DGA extension and VidMode extension can be used }
  if not XF86DGAQueryExtension(FDisplay, @dummy1, @dummy2) then
    raise TPTCError.Create('DGA extension not available');
  if not XF86VidModeQueryExtension(FDisplay, @dummy1, @dummy2) then
    raise TPTCError.Create('VidMode extension not available');
end;

destructor TX11DGA1Display.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TX11DGA1Display.Open(ATitle: string; AWidth, AHeight: Integer; AFormat: IPTCFormat; const AOpenGLAttributes: IPTCOpenGLAttributes);
var
  vml: PXF86VidModeModeLine;
  dotclock: Integer;
  i: Integer;
  root: TWindow;
  e: TXEvent;
  found: Boolean;
  r, g, b: Single;
  found_mode: Integer;
  min_diff: Integer;
  d_x, d_y: Integer;
begin
  FWidth := AWidth;
  FHeight := AHeight;

  { Get all availabe video modes }
  XF86VidModeGetAllModeLines(FDisplay, FScreen, @FModeInfoNum, @FModeInfo);

  FPreviousMode := -1;
  { Save previous mode }
  New(vml);
  try
    XF86VidModeGetModeLine(FDisplay, FScreen, @dotclock, vml);
    try
      for i := 0 to FModeInfoNum - 1 do
      begin
        if (vml^.hdisplay = FModeInfo[i]^.hdisplay) and
           (vml^.vdisplay = FModeInfo[i]^.vdisplay) then
        begin
          FPreviousMode := i;
          Break;
        end;
      end;
    finally
      if vml^.privsize <> 0 then
        XFree(vml^.c_private);
    end;
  finally
    Dispose(vml);
  end;
  if FPreviousMode = -1 then
    raise TPTCError.Create('Current mode not found in modelist?! Err, this shouldn''t happen :)');

  { Find a video mode to set }

  { Normal modesetting first, find exactly matching mode }
  found_mode := -1;
  for i := 0 to FModeInfoNum - 1 do
    if (FModeInfo[i]^.hdisplay = AWidth) and (FModeInfo[i]^.vdisplay = AHeight) then
    begin
      found_mode := i;
      Break;
    end;

  { try to find a mode that matches the width first }
  if found_mode = -1 then
    for i := 0 to FModeInfoNum - 1 do
      if (FModeInfo[i]^.hdisplay = AWidth) and
         (FModeInfo[i]^.vdisplay >= AHeight) then
      begin
        found_mode := i;
        Break;
      end;

  { Next try to match the height }
  if found_mode = -1 then
    for i := 0 to FModeInfoNum - 1 do
      if (FModeInfo[i]^.hdisplay >= AWidth) and
         (FModeInfo[i]^.vdisplay = AHeight) then
      begin
        found_mode := i;
        Break;
      end;

  if found_mode = -1 then
  begin
    { Finally, find the mode that is bigger than the requested one and makes }
    { the least difference }
    min_diff := 987654321;
    for i := 0 to FModeInfoNum - 1 do
      if (FModeInfo[i]^.hdisplay >= AWidth) and (FModeInfo[i]^.vdisplay >= AHeight) then
      begin
        d_x := Sqr(FModeInfo[i]^.hdisplay - AWidth);
        d_y := Sqr(FModeInfo[i]^.vdisplay - AHeight);
        if (d_x + d_y) < min_diff then
        begin
          min_diff := d_x + d_y;
          found_mode := i;
        end;
      end;
  end;

  if found_mode = -1 then
    raise TPTCError.Create('Cannot find a video mode to use');

  if not XF86VidModeSwitchToMode(FDisplay, FScreen, FModeInfo[found_mode]) then
    raise TPTCError.Create('Error switching to requested video mode');
  FDestX := (FModeInfo[found_mode]^.hdisplay div 2) - (AWidth div 2);
  FDestY := (FModeInfo[found_mode]^.vdisplay div 2) - (AHeight div 2);

  XFlush(FDisplay);
  FInMode := True;

  { Check if the requested colour mode is available }
  FFormat := GetX11Format(AFormat);

  { Grab exclusive control over the keyboard and mouse }
  root := XRootWindow(FDisplay, FScreen);
  XGrabKeyboard(FDisplay, root, True, GrabModeAsync, GrabModeAsync, CurrentTime);
  XGrabPointer(FDisplay, root, True, PointerMotionMask or ButtonPressMask or
               ButtonReleaseMask, GrabModeAsync, GrabModeAsync, None, None,
               CurrentTime);
  XFlush(FDisplay);

  { Get Display information }
  XF86DGAGetVideo(FDisplay, FScreen, @FDGAAddr, @FDGALineWidth,
                  @FDGABankSize, @FDGAMemSize);

  { Don't have to be root anymore }
{  fpsetuid(fpgetuid);...}

  XF86DGAGetViewPortSize(FDisplay, FScreen, @FDGAWidth, @FDGAHeight);

  if XF86DGAForkApp(FScreen) <> 0 then
    raise TPTCError.Create('cannot do safety fork');

  if XF86DGADirectVideo(FDisplay, FScreen, XF86DGADirectGraphics or
      XF86DGADirectKeyb or XF86DGADirectMouse) = 0 then
    raise TPTCError.Create('cannot switch to DGA mode');

  FInDirect := True;
  FillChar(FDGAAddr^, FDGALineWidth * FDGAHeight * (FFormat.Bits div 8), 0);

  XSelectInput(FDisplay, DefaultRootWindow(FDisplay),
               KeyPressMask or KeyReleaseMask);

  XF86DGASetViewPort(FDisplay, FScreen, 0, 0); { Important.. sort of =) }

  found := False;
  repeat
    { Stupid loop. The key }
    { events were causing }
    { problems.. }
    found := XCheckMaskEvent(FDisplay, KeyPressMask or KeyReleaseMask, @e);
  until not found;

  { Create colour map in 8 bit mode }
  if FFormat.Bits = 8 then
  begin
    FColours := GetMem(256 * SizeOf(TXColor));
    if FColours = nil then
      raise TPTCError.Create('Cannot allocate colour map cells');
    FCMap := XCreateColormap(FDisplay, RootWindow(FDisplay, FScreen),
                             DefaultVisual(FDisplay, FScreen), AllocAll);
    if FCMap = 0 then
      raise TPTCError.Create('Cannot create colour map');
  end
  else
    FCMap := 0;

  { Set 332 palette, for now }
  if (FFormat.Bits = 8) and FFormat.Direct then
  begin
    {Taken from PTC 0.72, i hope it's fine}
    for i := 0 to 255 do
    begin
      r := ((i and $E0) shr 5) * 255 / 7;
      g := ((i and $1C) shr 2) * 255 / 7;
      b := (i and $03) * 255 / 3;

      FColours[i].pixel := i;

      FColours[i].red := Round(r) shl 8;
      FColours[i].green := Round(g) shl 8;
      FColours[i].blue := Round(b) shl 8;

      Byte(FColours[i].flags) := DoRed or DoGreen or DoBlue;
    end;
    XStoreColors(FDisplay, FCMap, FColours, 256);
    XF86DGAInstallColormap(FDisplay, FScreen, FCMap);
  end;

  { Set clipping area }
  FClip := TPTCArea.Create(0, 0, FWidth, FHeight);

  FOpen := True;
end;

{ not in DGA mode }
procedure TX11DGA1Display.Open(AWindow: TWindow; AFormat: IPTCFormat);
begin
  if AWindow = 0 Then; { Prevent warnings }
  if AFormat = nil Then;
end;

procedure TX11DGA1Display.Open(AWindow: TWindow; AFormat: IPTCFormat; AX, AY, AWidth, AHeight: Integer);
begin
  if (AWindow = 0) or
     (AFormat = Nil) or
     (AX = 0) or
     (AY = 0) or
     (AWidth = 0) or
     (AHeight = 0) Then;
end;

procedure TX11DGA1Display.Close;
begin
  FOpen := False;

  if FInDirect then
  begin
    FInDirect := False;
    XF86DGADirectVideo(FDisplay, FScreen, 0);
  end;

  if FInMode then
  begin
    FInMode := False;
    XF86VidModeSwitchToMode(FDisplay, FScreen, FModeInfo[FPreviousMode]);
    XUngrabKeyboard(FDisplay, CurrentTime);
    XUngrabPointer(FDisplay, CurrentTime);
  end;

  if FDisplay <> nil then
    XFlush(FDisplay);

  if FCMap <> 0 then
  begin
    XFreeColormap(FDisplay, FCMap);
    FCMap := 0;
  end;

  FreeMemAndNil(FColours);

  if FModeInfo <> nil then
  begin
    XFree(FModeInfo);
    FModeInfo := nil;
  end;
end;

procedure TX11DGA1Display.GetModes(var AModes: TPTCModeList);
begin
  SetLength(AModes, 0);
  {todo...}
end;

procedure TX11DGA1Display.Update;
begin
end;

procedure TX11DGA1Display.Update(AArea: IPTCArea);
begin
end;

procedure TX11DGA1Display.HandleEvents;
var
  e: TXEvent;
  NewFocus: Boolean;
  NewFocusSpecified: Boolean;

  function UsefulEventsPending: Boolean;
  var
    tmpEvent: TXEvent;
  begin
    if XCheckTypedEvent(FDisplay, ClientMessage, @tmpEvent) then
    begin
      Result := True;
      XPutBackEvent(FDisplay, @tmpEvent);
      exit;
    end;

    if XCheckMaskEvent(FDisplay, FocusChangeMask or
                       KeyPressMask or KeyReleaseMask or
                       ButtonPressMask or ButtonReleaseMask or
                       PointerMotionMask or ExposureMask, @tmpEvent) then
    begin
      Result := True;
      XPutBackEvent(FDisplay, @tmpEvent);
      exit;
    end;

    Result := False;
  end;

begin
  NewFocusSpecified := False;
  while UsefulEventsPending do
  begin
    XNextEvent(FDisplay, @e);
    case e._type of
      FocusIn: begin
        NewFocus := True;
        NewFocusSpecified := True;
      end;
      FocusOut: begin
        NewFocus := False;
        NewFocusSpecified := True;
      end;
      ClientMessage: begin
{        if (e.xclient.format = 32) and (TAtom(e.xclient.data.l[0]) = m_atom_close) then
          Halt(0);}
      end;
      Expose: begin
        {...}
      end;
      KeyPress, KeyRelease: HandleKeyEvent(e.xkey);
      ButtonPress, ButtonRelease: begin
        {...}
      end;
      MotionNotify: begin
        {...}
      end;
    end;
  end;
//  HandleChangeFocus(NewFocus);
end;

function TX11DGA1Display.NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
var
  tmpEvent: TXEvent;
begin
  repeat
    { process all events from the X queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);

    if AWait and (AEvent = Nil) then
    begin
      { if the X event queue is empty, block until an event is received }
      XPeekEvent(FDisplay, @tmpEvent);
    end;
  until (not AWait) or (AEvent <> Nil);
  Result := AEvent <> nil;
end;

function TX11DGA1Display.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
var
  tmpEvent: TXEvent;
begin
  repeat
    { process all events from the X queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);

    if AWait and (Result = Nil) then
    begin
      { if the X event queue is empty, block until an event is received }
      XPeekEvent(FDisplay, @tmpEvent);
    end;
  until (not AWait) or (Result <> Nil);
end;


function TX11DGA1Display.Lock: Pointer;
begin
  Result := FDGAAddr + FDGALineWidth * FDestY * (FFormat.Bits div 8) +
                       FDestX * (FFormat.Bits div 8);
end;

procedure TX11DGA1Display.Unlock;
begin
end;

procedure TX11DGA1Display.Palette(APalette: IPTCPalette);
var
  pal: PUint32;
  i: Integer;
begin
  pal := APalette.data;
  if not FFormat.Indexed then
    exit;
  for i := 0 to 255 do
  begin
    FColours[i].pixel := i;

    FColours[i].red := ((pal[i] shr 16) and $FF) shl 8;
    FColours[i].green := ((pal[i] shr 8) and $FF) shl 8;
    FColours[i].blue := (pal[i] and $FF) shl 8;

    Byte(FColours[i].flags) := DoRed or DoGreen or DoBlue;
  end;
  XStoreColors(FDisplay, FCMap, FColours, 256);
  XF86DGAInstallColormap(FDisplay, FScreen, FCMap);
end;

function TX11DGA1Display.GetPitch: Integer;
begin
  Result := FDGALineWidth * (FFormat.Bits div 8);
end;

function TX11DGA1Display.GetX11Window: TWindow;
begin
  Result := DefaultRootWindow(FDisplay);
end;

function TX11DGA1Display.IsFullScreen: Boolean;
begin
  { DGA is always fullscreen }
  Result := True;
end;

function TX11DGA1Display.IsOpen: Boolean;
begin
  Result := FOpen;
end;

procedure TX11DGA1Display.SetCursor(AVisible: Boolean);
begin
  {nothing... raise exception if visible=true?}
end;

procedure TX11DGA1Display.SetMouseGrab(AGrabMouse: Boolean);
begin
  {...}
end;

{$ENDIF ENABLE_X11_EXTENSION_XF86DGA1}
