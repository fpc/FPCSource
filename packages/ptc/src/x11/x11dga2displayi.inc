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

{$IFDEF ENABLE_X11_EXTENSION_XF86DGA2}

constructor TX11DGA2Display.Create(ADisplay: PDisplay; AScreen: Integer; const AFlags: TX11Flags);
var
  MajorVersion, MinorVersion: cint;
begin
  inherited;

  LOG('trying to open a DGA 2.0 display');

  { Check if the DGA extension can be used }
  LOG('checking if the DGA extension can be used (XDGAQueryExtension)');
  if not XDGAQueryExtension(FDisplay, @FXDGAEventBase, @FXDGAErrorBase) then
    raise TPTCError.Create('DGA extension not available');

  LOG('checking DGA version (XDGAQueryVersion)');
  if not XDGAQueryVersion(FDisplay, @MajorVersion, @MinorVersion) then
    raise TPTCError.Create('Unable to query DGA version');

  LOG('DGA version ' + IntToStr(MajorVersion) + '.' + IntToStr(MinorVersion));

  if MajorVersion < 2 then
    raise TPTCError.Create('DGA version older than 2.0');
end;

destructor TX11DGA2Display.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TX11DGA2Display.Open(ATitle: string; AWidth, AHeight: Integer; AFormat: IPTCFormat; const AOpenGLAttributes: IPTCOpenGLAttributes);

  function ModeFlagsToString(AFlags: cint): string;
  begin
    Result := '';
    if (AFlags and XDGAConcurrentAccess) <> 0 then
      Result := Result + ' XDGAConcurrentAccess';
    if (AFlags and XDGASolidFillRect) <> 0 then
      Result := Result + ' XDGASolidFillRect';
    if (AFlags and XDGABlitRect) <> 0 then
      Result := Result + ' XDGABlitRect';
    if (AFlags and XDGABlitTransRect) <> 0 then
      Result := Result + ' XDGABlitTransRect';
    if (AFlags and XDGAPixmap) <> 0 then
      Result := Result + ' XDGAPixmap';
    if (AFlags and XDGAInterlaced) <> 0 then
      Result := Result + ' XDGAInterlaced';
    if (AFlags and XDGADoublescan) <> 0 then
      Result := Result + ' XDGADoublescan';
    if Result <> '' then
      Delete(Result, 1, 1);
  end;
  
  function ViewportFlagsToString(AViewportFlags: cint): string;
  begin
    Result := '';
    if (AViewportFlags and XDGAFlipRetrace) <> 0 then
      Result := Result + ' XDGAFlipRetrace';
    if (AViewportFlags and XDGAFlipImmediate) <> 0 then
      Result := Result + ' XDGAFlipImmediate';
    if Result <> '' then
      Delete(Result, 1, 1);
  end;
  
  function ByteOrderToString(AByteOrder: cint): string;
  begin
    case AByteOrder of
      LSBFirst: Result := ' (LSBFirst)';
      MSBFirst: Result := ' (MSBFirst)';
    else
      Result := '';
    end;
  end;

  function VisualClassToString(AVisualClass: cshort): string;
  begin
    case AVisualClass of
      StaticGray: Result := ' (StaticGray)';
      GrayScale: Result := ' (GrayScale)';
      StaticColor: Result := ' (StaticColor)';
      PseudoColor: Result := ' (PseudoColor)';
      TrueColor: Result := ' (TrueColor)';
      DirectColor: Result := ' (DirectColor)';
    else
      Result := '';
    end;
  end;

  function FindMode: Integer;
  var
    I: Integer;
  begin
    { this function is currently a hack }
    { todo: implement generic algorithm for mode selection }

    for I := 0 to FXDGAModesNum - 1 do
    begin
      with FXDGAModes[I] do
      begin
        if (viewportWidth = AWidth) and (viewportHeight = AHeight) and
          (bitsPerPixel = AFormat.Bits) then
          exit(num);
      end;
    end;

    for I := 0 to FXDGAModesNum - 1 do
    begin
      with FXDGAModes[I] do
      begin
        if (viewportWidth = AWidth) and (viewportHeight = AHeight) then
          exit(num);
      end;
    end;

    Result := 1;  // todo: fix...
  end;

var
  i: Integer;
//  e: TXEvent;
  r, g, b: Single;
  found_mode: Integer;
begin
  FWidth := AWidth;
  FHeight := AHeight;

  LOG('trying to open framebuffer (XDGAOpenFramebuffer)');
  if not XDGAOpenFramebuffer(FDisplay, FScreen) then
    raise TPTCError.Create('Cannot open framebuffer - insufficient privileges?');
  FFramebufferIsOpen := True;

  { Get all available video modes }
  LOG('querying available display modes (XDGAQueryModes)');
  FXDGAModes := XDGAQueryModes(FDisplay, FScreen, @FXDGAModesNum);

  LOG('number of display modes', FXDGAModesNum);

  for I := 0 to FXDGAModesNum - 1 do
  begin
    LOG('mode#', I);
    with FXDGAModes[I] do
    begin
      LOG('num', num);
      LOG('name', name);
      LOG('verticalRefresh', verticalRefresh);
      LOG('flags', IntToStr(flags) + ' (' + ModeFlagsToString(flags) + ')');
      LOG('imageWidth', imageWidth);
      LOG('imageHeight', imageHeight);
      LOG('pixmapWidth', pixmapWidth);
      LOG('pixmapHeight', pixmapHeight);
      LOG('bytesPerScanline', bytesPerScanline);
      LOG('byteOrder', IntToStr(byteOrder) + ByteOrderToString(byteOrder));
      LOG('depth', depth);
      LOG('bitsPerPixel', bitsPerPixel);
      LOG('redMask', '$' + HexStr(redMask, 8));
      LOG('greenMask', '$' + HexStr(greenMask, 8));
      LOG('blueMask', '$' + HexStr(blueMask, 8));
      LOG('visualClass', IntToStr(visualClass) + VisualClassToString(visualClass));
      LOG('viewportWidth', viewportWidth);
      LOG('viewportHeight', viewportHeight);
      LOG('xViewportStep', xViewportStep);
      LOG('yViewportStep', yViewportStep);
      LOG('maxViewportX', maxViewportX);
      LOG('maxViewportY', maxViewportY);
      LOG('viewportFlags', IntToStr(viewportFlags) + ' (' + ViewportFlagsToString(viewportFlags) + ')');
      LOG('reserved1', reserved1);
      LOG('reserved2', reserved2);
      LOG('----------');
    end;
  end;

  found_mode := FindMode;

  FXDGADevice := XDGASetMode(FDisplay, FScreen, found_mode);
  if FXDGADevice = nil then
    raise TPTCError.Create('XDGASetMode failed (returned nil)');
  if FXDGADevice^.data = nil then
    raise TPTCError.Create('The pointer to the framebuffer, returned by XDGA is nil?!');
  FModeIsSet := True;

  if FWidth > FXDGADevice^.mode.viewportWidth then
    FWidth := FXDGADevice^.mode.viewportWidth;
  if FHeight > FXDGADevice^.mode.viewportHeight then
    FHeight := FXDGADevice^.mode.viewportHeight;

  FDestX := (FXDGADevice^.mode.viewportWidth div 2) - (FWidth div 2);
  FDestY := (FXDGADevice^.mode.viewportHeight div 2) - (FHeight div 2);

  { Check if the requested colour mode is available }
  FFormat := GetX11Format(AFormat);

(*  { Grab exclusive control over the keyboard and mouse }
  root := XRootWindow(FDisplay, FScreen);
  XGrabKeyboard(FDisplay, root, True, GrabModeAsync, GrabModeAsync, CurrentTime);
  XGrabPointer(FDisplay, root, True, PointerMotionMask or ButtonPressMask or
               ButtonReleaseMask, GrabModeAsync, GrabModeAsync, None, None,
               CurrentTime);*)
  XFlush(FDisplay);

  { Get Display information }
{  XF86DGAGetVideo(FDisplay, FScreen, @dga_addr, @dga_linewidth,
                  @dga_banksize, @dga_memsize);}

  { Don't have to be root anymore }
{  setuid(getuid);...}

//  XF86DGAGetViewPortSize(FDisplay, FScreen, @dga_width, @dga_height);

{  if XF86DGAForkApp(FScreen) <> 0 then
    raise TPTCError.Create('cannot do safety fork')
  else
  begin
    if XF86DGADirectVideo(FDisplay, FScreen, XF86DGADirectGraphics or
      XF86DGADirectKeyb or XF86DGADirectMouse) = 0 then
      raise TPTCError.Create('cannot switch to DGA mode');
  end;}

//  m_indirect := True;
//  FillChar(dga_addr^, dga_linewidth * dga_height * (FFormat.bits div 8), 0);

  XDGASelectInput(FDisplay, FScreen, KeyPressMask or KeyReleaseMask or
                                     ButtonPressMask or ButtonReleaseMask or
                                     PointerMotionMask);

  { Important.. sort of =) }
//  XDGASetViewport(FDisplay, FScreen, 0, 0, {XDGAFlipImmediate}XDGAFlipRetrace);

(*  found := False;
  repeat
    { Stupid loop. The key }
    { events were causing }
    { problems.. }
    found := XCheckMaskEvent(FDisplay, KeyPressMask or KeyReleaseMask, @e);
  until not found;*)

  { Create colour map in 8 bit mode }
  if FFormat.bits = 8 {todo:fix!} then
  begin
    FColours := GetMem(256 * SizeOf(TXColor));
    if FColours = nil then
      raise TPTCError.Create('Cannot allocate colour map cells');
//    FCMap := XCreateColormap(FDisplay, RootWindow(FDisplay, FScreen),
//                              DefaultVisual(FDisplay, FScreen), AllocAll);
    FCMap := XDGACreateColormap(FDisplay, FScreen, FXDGADevice, AllocAll);
    if FCMap = 0 then
      raise TPTCError.Create('Cannot create colour map');
  end
  else
    FCMap := 0;

  { Set 332 palette, for now }
  if (FFormat.bits = 8) and FFormat.direct then
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
    XDGAInstallColormap(FDisplay, FScreen, FCMap);
  end;

  { Set clipping area }
  FClip := TPTCArea.Create(0, 0, FWidth, FHeight);

  XDGASync(FDisplay, FScreen);

  FillChar(FXDGADevice^.data^,
    FXDGADevice^.mode.bytesPerScanline * FXDGADevice^.mode.imageHeight, 0);

  FOpen := True;
end;

{ not in DGA mode }
procedure TX11DGA2Display.open(w: TWindow; _format: IPTCFormat);
begin
  if w = 0 then; { Prevent warnings }
  if _format = nil then;
end;

procedure TX11DGA2Display.open(_window: TWindow; _format: IPTCFormat; x, y, w, h: Integer);
begin
  if (_window = 0) or (_format = nil) or (x = 0) or
     (y = 0) or (w = 0) or (h = 0) then;
end;

procedure TX11DGA2Display.close;
var
  tmp: Pointer;
begin
  FOpen := False;
  if FModeIsSet then
  begin
    FModeIsSet := False;

    { restore the original mode }
    XDGASetMode(FDisplay, FScreen, 0); { returns PXDGADevice }
{    XUngrabKeyboard(FDisplay, CurrentTime);
    XUngrabPointer(FDisplay, CurrentTime);}
  end;

  if FFramebufferIsOpen then
  begin
    FFramebufferIsOpen := False;
    XDGACloseFramebuffer(FDisplay, FScreen);
  end;

  if FDisplay <> nil then
    XFlush(FDisplay);

  if FCMap <> 0 then
  begin
    XFreeColormap(FDisplay, FCMap);
    FCMap := 0;
  end;

  FreeMemAndNil(FColours);

  if FXDGADevice <> nil then
  begin
    tmp := FXDGADevice;
    FXDGADevice := nil;
    XFree(tmp);
  end;

  if FXDGAModes <> nil then
  begin
    tmp := FXDGAModes;
    FXDGAModes := nil;
    XFree(tmp);
  end;
end;

procedure TX11DGA2Display.GetModes(var AModes: TPTCModeList);
begin
  SetLength(AModes, 0);
  {todo...}
end;

procedure TX11DGA2Display.Update;
begin
end;

procedure TX11DGA2Display.Update(AArea: IPTCArea);
begin
end;

function TX11DGA2Display_MatchAnyEvent(display: PDisplay; event: PXEvent; arg: TXPointer): LongBool; cdecl;
begin
  Result := LongBool(1);
end;

procedure TX11DGA2Display.HandleEvents;
var
  e: TXEvent;
  NewFocus: Boolean;
  NewFocusSpecified: Boolean;

  function UsefulEventsPending: Boolean;
  var
    tmpEvent: TXEvent;
  begin
    if XCheckIfEvent(FDisplay, @tmpEvent, @TX11DGA2Display_MatchAnyEvent, nil) then
    begin
      Result := True;
      XPutBackEvent(FDisplay, @tmpEvent);
      exit;
    end;

    Result := False;
  end;

  procedure HandleXDGAKeyEvent;
  var
    XDGAKeyEvent: TXDGAKeyEvent absolute e;
    XKeyEvent: TXKeyEvent;
  begin
    XDGAKeyEventToXKeyEvent(@XDGAKeyEvent, @XKeyEvent);
    HandleKeyEvent(XKeyEvent);
  end;

begin
  NewFocusSpecified := False;
  while UsefulEventsPending do
  begin
    XNextEvent(FDisplay, @e);
(*    case e._type of
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
    end;*)
    case e._type - FXDGAEventBase of
      KeyPress, KeyRelease: HandleXDGAKeyEvent;
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

function TX11DGA2Display.NextEvent(out event: IPTCEvent; wait: Boolean; const EventMask: TPTCEventMask): Boolean;
var
  tmpEvent: TXEvent;
begin
  repeat
    { process all events from the X queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    event := FEventQueue.NextEvent(EventMask);

    if wait and (event = nil) then
    begin
      { if the X event queue is empty, block until an event is received }
      XPeekEvent(FDisplay, @tmpEvent);
    end;
  until (not Wait) or (event <> nil);
  Result := event <> nil;
end;

function TX11DGA2Display.PeekEvent(wait: Boolean; const EventMask: TPTCEventMask): IPTCEvent;
var
  tmpEvent: TXEvent;
begin
  repeat
    { process all events from the X queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(EventMask);

    if wait and (Result = nil) then
    begin
      { if the X event queue is empty, block until an event is received }
      XPeekEvent(FDisplay, @tmpEvent);
    end;
  until (not Wait) or (Result <> nil);
end;

function TX11DGA2Display.lock: Pointer;
begin
  lock := PByte(FXDGADevice^.data) +
          FXDGADevice^.mode.bytesPerScanline * FDestY +
          FDestX * (FXDGADevice^.mode.bitsPerPixel div 8);
end;

procedure TX11DGA2Display.unlock;
begin
end;

procedure TX11DGA2Display.Palette(_palette: IPTCPalette);
var
  pal: PUint32;
  i: Integer;
begin
  pal := _palette.data;
  if not FFormat.indexed then
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
  XDGAInstallColormap(FDisplay, FScreen, FCMap);
end;

function TX11DGA2Display.GetPitch: Integer;
begin
  Result := FXDGADevice^.mode.bytesPerScanline;
end;

function TX11DGA2Display.getX11Window: TWindow;
begin
  Result := DefaultRootWindow(FDisplay);
end;

function TX11DGA2Display.isFullScreen: Boolean;
begin
  { DGA is always fullscreen }
  Result := True;
end;

function TX11DGA2Display.IsOpen: Boolean;
begin
  Result := FOpen;
end;

procedure TX11DGA2Display.SetCursor(visible: Boolean);
begin
  {nothing... raise exception if visible=true?}
end;

procedure TX11DGA2Display.SetMouseGrab(AGrabMouse: Boolean);
begin
  {...}
end;

{$ENDIF ENABLE_X11_EXTENSION_XF86DGA2}
