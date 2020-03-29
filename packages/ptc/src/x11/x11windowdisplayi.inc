{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2013, 2016, 2017 Nikolay Nikolov (nickysn@users.sourceforge.net)
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

constructor TX11WindowDisplay.Create(ADisplay: PDisplay; AScreen: Integer; const AFlags: TX11Flags);
{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
var
  MajorVer, MinorVer: cint;
  QueryVersionResult: TStatus;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}
begin
  inherited;
  FFocus := True;
  FX11InvisibleCursor := None;
  FCursorVisible := True;
  FGrabMouse := False;
  FMouseGrabbed := False;
{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
  if PTC_X11_TRY_XINPUT2 in FFlags then
  begin
    LOG('Querying the XInput2 extension...');
    if XQueryExtension(FDisplay, 'XInputExtension', @FXInput2MajorOpCode, @FXInput2FirstEvent, @FXInput2FirstError) then
    begin
      LOG('There is an XInput extension, now let''s query its version...');
      MajorVer := 2;
      MinorVer := 0;
      LOG('We want version ' + IntToStr(MajorVer) + '.' + IntToStr(MinorVer));
      QueryVersionResult := XIQueryVersion(FDisplay, @MajorVer, @MinorVer);
      if QueryVersionResult = Success then
      begin
        LOG('XInput2 extension is available (version ' + IntToStr(MajorVer) + '.' + IntToStr(MinorVer) + ')');
        FXInput2Enabled := True;
      end
      else
      begin
        LOG('The required XInput version is not available (version ' + IntToStr(MajorVer) + '.' + IntToStr(MinorVer) + ' only)');
      end;
    end
    else
    begin
      LOG('XInput2 extension not available');
      FXInput2Enabled := False;
    end;
  end;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}
end;

destructor TX11WindowDisplay.Destroy;
begin
  Close;
  inherited Destroy;
end;

{$warning remove, when fix added to xlib}
function XGetIMValues(para1:PXIM; dotdotdot:array of const):Pchar;cdecl;external libX11;
function XGetICValues(para1:PXIC; dotdotdot:array of const):Pchar;cdecl;external libX11;

{function fpsetlocale(category: cint; locale: Pchar): PChar;cdecl;external 'c' name 'setlocale';
const
  LC_ALL = 6;}

procedure TX11WindowDisplay.Open(ATitle: AnsiString; AWidth, AHeight: Integer; AFormat: IPTCFormat; const AOpenGLAttributes: IPTCOpenGLAttributes);
var
  xgcv: TXGCValues;
  textprop: TXTextProperty;
  e: TXEvent;
  found: Boolean;
  attr: TXSetWindowAttributes;
  AttrMask: culong;
  size_hints: PXSizeHints;
  tmppchar: PChar;
  tmpArrayOfCLong: array [1..1] of clong;
  tmpPixmap: TPixmap;
  BlackColor: TXColor;
  BlankCursorData: array [1..8] of Byte = (0, 0, 0, 0, 0, 0, 0, 0);
  CreateWindow_Depth: cint;
  CreateWindow_Visual: PVisual;
{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
  XInput2EventMask: TXIEventMask;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}
  selected_im_style: TXIMStyle = 0;
  im_event_mask: clong = 0;

  function IMStyleToStr(style: TXIMStyle): string;
  begin
    Result := '';
    if (style and XIMPreeditArea) <> 0 then
      Result := Result + ' XIMPreeditArea';
    if (style and XIMPreeditCallbacks) <> 0 then
      Result := Result + ' XIMPreeditCallbacks';
    if (style and XIMPreeditPosition) <> 0 then
      Result := Result + ' XIMPreeditPosition';
    if (style and XIMPreeditNothing) <> 0 then
      Result := Result + ' XIMPreeditNothing';
    if (style and XIMPreeditNone) <> 0 then
      Result := Result + ' XIMPreeditNone';
    if (style and XIMStatusArea) <> 0 then
      Result := Result + ' XIMStatusArea';
    if (style and XIMStatusCallbacks) <> 0 then
      Result := Result + ' XIMStatusCallbacks';
    if (style and XIMStatusNothing) <> 0 then
      Result := Result + ' XIMStatusNothing';
    if (style and XIMStatusNone) <> 0 then
      Result := Result + ' XIMStatusNone';
    if Length(Result) > 0 then
      Delete(Result, 1, 1);
    Result := IntToStr(style) + ' (' + Result + ')';
  end;

  procedure TryCreateIM;
  const
    PreferredStyle = XIMPreeditNothing or XIMStatusNothing;
{    PreferredStyle = XIMPreeditNone or XIMStatusNone;}
  var
    im_supported_styles: PXIMStyles;
    locale_modifiers: PChar;
    I: Integer;
  begin
{    LOG('setting locale');
    if fpsetlocale(LC_ALL, '') = nil then
      LOG('set locale failed');}

    { Check if X11 supports the current locale }
    LOG('checking X11 support for the current locale');
    if XSupportsLocale = 0 then
    begin
      LOG('locale not supported');
      exit;
    end;
    LOG('locale supported');

    LOG('setting locale modifiers');
    locale_modifiers := XSetLocaleModifiers('@im=none');
    if locale_modifiers = nil then
    begin
      LOG('XSetLocaleModifiers failed');
    end
    else
    begin
      LOG('XSetLocaleModifiers success; returned', locale_modifiers);
    end;

    { Open IM }
    LOG('opening input method');
    FXIM := XOpenIM(FDisplay, nil, '', '');
    LOG('input method (ptr)', HexStr(FXIM));

    { Select IM input style }
    if Assigned(FXIM) then
    begin
      LOG('querying the supported input styles');
      XGetIMValues(FXIM, [XNQueryInputStyle, @im_supported_styles, nil]);

      if Assigned(im_supported_styles) then
      begin
        LOG('number of styles', im_supported_styles^.count_styles);
        for I := 0 to im_supported_styles^.count_styles - 1 do
        begin
          LOG('style', IMStyleToStr(im_supported_styles^.supported_styles[I]));
          if (im_supported_styles^.supported_styles[I] and PreferredStyle) = PreferredStyle then
            selected_im_style := im_supported_styles^.supported_styles[I];
        end;
        XFree(im_supported_styles);
        LOG('selected style', IMStyleToStr(selected_im_style));
      end;
    end;
  end;

  procedure TryCreateIC;
  begin
    if Assigned(FXIM) and (selected_im_style <> 0) then
    begin
      LOG('creating input context');
      FXIC := XCreateIC(FXIM, [XNInputStyle, selected_im_style,
                               XNClientWindow, FWindow,
                               XNFocusWindow, FWindow, nil]);
      LOG('input context (ptr)', HexStr(FXIC));

      if Assigned(FXIC) then
      begin
        LOG('setting input context focus');
        XSetICFocus(FXIC);

        LOG('getting the IM event mask');
        XGetICValues(FXIC, [XNFilterEvents, @im_event_mask, nil]);
        LOG('IM event mask', im_event_mask);
      end;
    end;
  end;

begin
  FHeight := AHeight;
  FWidth := AWidth;
  FPreviousWidth := FWidth;
  FPreviousHeight := FHeight;
  FDestX := 0;
  FDestY := 0;

  FFullScreen := PTC_X11_FULLSCREEN in FFlags;
  FResizable := (PTC_X11_RESIZABLE_WINDOW in FFlags) and not FFullScreen;

  FFocus := True;

  FPreviousMousePositionSaved := False;

  FillChar(BlackColor, SizeOf(BlackColor), 0);
  BlackColor.red := 0;
  BlackColor.green := 0;
  BlackColor.blue := 0;

  { Try to open an input method }
  if PTC_X11_TRY_XIM in FFlags then
    TryCreateIM;

  { Create the mode switcher object }
  if (FModeSwitcher = Nil) and FFullScreen then
    FModeSwitcher := CreateModeSwitcher;

  { Create the invisible cursor }
  tmpPixmap := XCreateBitmapFromData(FDisplay, RootWindow(FDisplay, FScreen), @BlankCursorData, 8, 8);
  try
    FX11InvisibleCursor := XCreatePixmapCursor(FDisplay, tmpPixmap, tmpPixmap, @BlackColor, @BlackColor, 0, 0);
  finally
    if tmpPixmap <> None then
      XFreePixmap(FDisplay, tmpPixmap);
  end;

  { Check if we have that colour depth available.. Easy as there is no
    format conversion yet }
  FFormat := GetX11Format(AFormat);

  attr.border_pixel := BlackPixel(FDisplay, FScreen);
  attr.background_pixel := BlackPixel(FDisplay, FScreen);
  AttrMask := CWBackPixel or CWBorderPixel;

  CreateWindow_Depth := CopyFromParent;
  CreateWindow_Visual := PVisual(CopyFromParent);
  FVisual := DefaultVisual(FDisplay, FScreen);

  if PTC_X11_USE_OPENGL in FFlags then
  begin
{$IFDEF ENABLE_X11_EXTENSION_GLX}
    FGLXFBConfig := TX11GLXFBConfig.Create(FDisplay, FScreen, AOpenGLAttributes);
    CreateWindow_Depth := FGLXFBConfig.FVisInfo.depth;
    CreateWindow_Visual := FGLXFBConfig.FVisInfo.visual;
    FVisual := CreateWindow_Visual;
    FScreen := FGLXFBConfig.FVisInfo.screen; //?
    attr.colormap := XCreateColormap(FDisplay, RootWindow(FDisplay, FScreen), CreateWindow_Visual, AllocNone);
    AttrMask := AttrMask or CWColormap;
{$ELSE ENABLE_X11_EXTENSION_GLX}
    raise TPTCError.Create('Console does not support OpenGL');
{$ENDIF ENABLE_X11_EXTENSION_GLX}
  end;

  { Create a window }
  FWindow := XCreateWindow(FDisplay, RootWindow(FDisplay, FScreen), 0, 0,
                AWidth, AHeight, 0, CreateWindow_Depth, InputOutput, CreateWindow_Visual,
                AttrMask, @attr);
  { Register the delete atom }
  FAtomClose := XInternAtom(FDisplay, 'WM_DELETE_WINDOW', False);
  X11Check(XSetWMProtocols(FDisplay, FWindow, @FAtomClose, 1), 'XSetWMProtocols');

  if not (PTC_X11_USE_OPENGL in FFlags) then
  begin
    { Get graphics context }
    xgcv.graphics_exposures := Ord(False);
    FGC := XCreateGC(FDisplay, FWindow, GCGraphicsExposures, @xgcv);
    if FGC = nil then
      raise TPTCError.Create('can''t create graphics context');
  end;

  { Set window title }
  tmppchar := PChar(ATitle);
  X11Check(XStringListToTextProperty(@tmppchar, 1, @textprop), 'XStringListToTextProperty');
  try
    XSetWMName(FDisplay, FWindow, @textprop);
    XFlush(FDisplay);
  finally
    XFree(textprop.value);
  end;

  { Set normal hints }
  size_hints := XAllocSizeHints;
  try
    size_hints^.flags := PBaseSize;
    size_hints^.base_width := AWidth;
    size_hints^.base_height := AHeight;
    if FFullScreen then
    begin
      size_hints^.flags := size_hints^.flags or PMinSize or PWinGravity;
      size_hints^.min_width := AWidth;
      size_hints^.min_height := AHeight;
      size_hints^.win_gravity := StaticGravity;
    end
    else
    begin
      if FResizable then
      begin
        size_hints^.flags := size_hints^.flags or PMinSize;
        size_hints^.min_width := 0;
        size_hints^.min_height := 0;
      end
      else
      begin
        { not fullscreen and not resizable: maxsize=minsize=basesize }
        size_hints^.flags := size_hints^.flags or PMinSize or PMaxSize;
        size_hints^.min_width := AWidth;
        size_hints^.min_height := AHeight;
        size_hints^.max_width := AWidth;
        size_hints^.max_height := AHeight;
      end;
    end;
    XSetWMNormalHints(FDisplay, FWindow, size_hints);
    XFlush(FDisplay);
  finally
    XFree(size_hints);
  end;

  { Set the _NET_WM_STATE property }
  if FFullScreen then
  begin
    tmpArrayOfCLong[1] := XInternAtom(FDisplay, '_NET_WM_STATE_FULLSCREEN', False);

    XChangeProperty(FDisplay, FWindow,
                    XInternAtom(FDisplay, '_NET_WM_STATE', False),
                    XA_ATOM,
                    32, PropModeReplace, @tmpArrayOfCLong, 1);
  end;

  { Map the window and wait for success }
  XSelectInput(FDisplay, FWindow, StructureNotifyMask);
  XMapRaised(FDisplay, FWindow);
  repeat
    XNextEvent(FDisplay, @e);
    if e._type = MapNotify then
      Break;
  until False;

  { Try to create an input context for the input method }
  if PTC_X11_TRY_XIM in FFlags then
    TryCreateIC;

  { Get keyboard input and sync }
  XSelectInput(FDisplay, FWindow, KeyPressMask or KeyReleaseMask or
                                  StructureNotifyMask or FocusChangeMask or
                                  ButtonPressMask or ButtonReleaseMask or
                                  PointerMotionMask or ExposureMask or
                                  EnterWindowMask or LeaveWindowMask or im_event_mask);
  XSync(FDisplay, False);

{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
  { enable XInput2 raw mouse input for the window as well }
  if FXInput2Enabled then
  begin
    FillChar(XInput2EventMask, SizeOf(XInput2EventMask), 0);
    XInput2EventMask.deviceid := XIAllMasterDevices;

    XInput2EventMask.mask_len := XIMaskLen(XI_LASTEVENT);
    XInput2EventMask.mask := AllocMem(XInput2EventMask.mask_len);
    try
      XISetMask(XInput2EventMask.mask, XI_RawMotion);
      XISetMask(XInput2EventMask.mask, XI_RawButtonPress);
      XISetMask(XInput2EventMask.mask, XI_RawButtonRelease);

      XISelectEvents(FDisplay, RootWindow(FDisplay, FScreen){FWindow}, @XInput2EventMask, 1);
    finally
      FreeMem(XInput2EventMask.mask);
    end;
  end;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}

  if not (PTC_X11_USE_OPENGL in FFlags) then
  begin
    { Create XImage using factory method }
    FPrimary := CreateImage(FDisplay, FScreen, FWidth, FHeight, FFormat);
  end;

  found := False;
  repeat
    { Stupid loop. The key }
    { events were causing }
    { problems.. }
    found := XCheckMaskEvent(FDisplay, KeyPressMask or KeyReleaseMask, @e);
  until not found;

  attr.backing_store := Always;
  XChangeWindowAttributes(FDisplay, FWindow, CWBackingStore, @attr);

  { Set clipping area }
  FClip := TPTCArea.Create(0, 0, FWidth, FHeight);

  { Installs the right colour map for 8 bit modes }
  CreateColormap;

{$IFDEF ENABLE_X11_EXTENSION_GLX}
  if PTC_X11_USE_OPENGL in FFlags then
  begin
    FGLXFBConfig.AttachToWindow(FWindow);
    FGLXFBConfig.MakeRenderingContextCurrent;
  end;
{$ENDIF ENABLE_X11_EXTENSION_GLX}

  if FFullScreen then
    EnterFullScreen;

  FOpen := True;
end;

procedure TX11WindowDisplay.Open(AWindow: TWindow; AFormat: IPTCFormat);
begin
end;

procedure TX11WindowDisplay.Open(AWindow: TWindow; AFormat: IPTCFormat; AX, AY, AWidth, AHeight: Integer);
begin
end;

procedure TX11WindowDisplay.Close;
begin
  FOpen := False;

  FreeAndNil(FModeSwitcher);

{$IFDEF ENABLE_X11_EXTENSION_GLX}
  FreeAndNil(FGLXFBConfig);
{$ENDIF ENABLE_X11_EXTENSION_GLX}

  if Assigned(FXIC) then
  begin
    LOG('destroying input context');
    XDestroyIC(FXIC);
    FXIC := nil;
  end;

  if Assigned(FXIM) then
  begin
    LOG('closing input method');
    XCloseIM(FXIM);
    FXIM := nil;
  end;

  {pthreads?!}
  if FCMap <> 0 then
  begin
    XFreeColormap(FDisplay, FCMap);
    FCMap := 0;
  end;

  { Destroy XImage and buffer }
  FreeAndNil(FPrimary);
  FreeMemAndNil(FColours);

  if FGC <> nil then
  begin
    XFreeGC(FDisplay, FGC);
    FGC := nil;
  end;

  { Hide and destroy window }
  if (FWindow <> 0) and (not (PTC_X11_LEAVE_WINDOW In FFlags)) then
  begin
    XUnmapWindow(FDisplay, FWindow);
    XSync(FDisplay, False);

    XDestroyWindow(FDisplay, FWindow);
  end;

  { Free the invisible cursor }
  if FX11InvisibleCursor <> None then
  begin
    XFreeCursor(FDisplay, FX11InvisibleCursor);
    FX11InvisibleCursor := None;
  end;
end;

procedure TX11WindowDisplay.InternalResize(AWidth, AHeight: Integer);
begin
  if FFullScreen then
    raise TPTCError.Create('Internal resize not supported in fullscreen mode');
  if not FResizable then
    raise TPTCError.Create('Internal resize cannot be used on a non-resizable window');

  if not (PTC_X11_USE_OPENGL in FFlags) then
  begin
    { destroy previous XImage }
    FreeAndNil(FPrimary);
  end;

  FWidth := AWidth;
  FHeight := AHeight;

  if not (PTC_X11_USE_OPENGL in FFlags) then
  begin
    { Create XImage using factory method }
    FPrimary := CreateImage(FDisplay, FScreen, FWidth, FHeight, FFormat);
  end;

  { Set clipping area }
  FClip := TPTCArea.Create(0, 0, FWidth, FHeight);
end;

procedure TX11WindowDisplay.internal_ShowCursor(AVisible: Boolean);
var
  attr: TXSetWindowAttributes;
begin
  if AVisible then
    attr.cursor := None { Use the normal cursor }
  else
    attr.cursor := FX11InvisibleCursor; { Set the invisible cursor }

  XChangeWindowAttributes(FDisplay, FWindow, CWCursor, @attr);
end;

procedure TX11WindowDisplay.internal_GrabMouse(AGrabMouse: Boolean);
var
  GrabResult: cint;
begin
  if AGrabMouse then
  begin
    if not FMouseGrabbed then
    begin
      //Writeln('XGrabPointer...');
      GrabResult :=XGrabPointer(FDisplay, FWindow, True,
                                PointerMotionMask or ButtonPressMask or ButtonReleaseMask,
                                GrabModeAsync, GrabModeAsync,
                                FWindow, None, CurrentTime);
      if GrabResult <> GrabSuccess then
      begin
        LOG('XGrabPointer failed, result=' + IntToStr(GrabResult));
        //Writeln(GrabResult);
      end;

      { XGrabPointer may fail if someone else is already holding a mouse grab.
        It happens e.g. sometimes when alt+tabbing away and back to the application
        in GNOME. That's why we check the result and try again on the next call.
        And that's why this method is called everytime HandleEvents is called. }
      FMouseGrabbed := GrabResult = GrabSuccess;
    end;
  end
  else
  begin
    if FMouseGrabbed then
    begin
      //Writeln('XUngrabPointer');
      XUngrabPointer(FDisplay, CurrentTime);
      FMouseGrabbed := False;
    end;
  end;
end;

procedure TX11WindowDisplay.SetCursor(AVisible: Boolean);
begin
  FCursorVisible := AVisible;

  if FFocus then
    internal_ShowCursor(FCursorVisible);
end;

procedure TX11WindowDisplay.SetMouseGrab(AGrabMouse: Boolean);
begin
  FGrabMouse := AGrabMouse;

  if FFocus then
    internal_GrabMouse(FGrabMouse);
end;

function TX11WindowDisplay.SetRelativeMouseMode(ARelativeMouseMode: Boolean): Boolean;
begin
{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
  if FXInput2Enabled then
    FRelativeMouseMode := ARelativeMouseMode;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}
  Result := ARelativeMouseMode = FRelativeMouseMode;
end;

procedure TX11WindowDisplay.EnterFullScreen;
begin
  { try to switch mode }
  if Assigned(FModeSwitcher) then
    FModeSwitcher.SetBestMode(FWidth, FHeight);

  XSync(FDisplay, False);

  { Center the image }
  FDestX := FModeSwitcher.Width div 2 - FWidth div 2;
  FDestY := FModeSwitcher.Height div 2 - FHeight div 2;
end;

procedure TX11WindowDisplay.LeaveFullScreen;
begin
  { Restore previous mode }
  if Assigned(FModeSwitcher) then
    FModeSwitcher.RestorePreviousMode;

  XSync(FDisplay, False);
end;

procedure TX11WindowDisplay.HandleChangeFocus(ANewFocus: Boolean);
begin
  { No change? }
  if ANewFocus = FFocus then
    exit;

  FFocus := ANewFocus;
  if FFocus then
  begin
    { focus in }
    if FFullScreen then
      EnterFullScreen;

    internal_ShowCursor(FCursorVisible);
//    internal_GrabMouse(FGrabMouse);
  end
  else
  begin
    { focus out }

    internal_GrabMouse(False);

    if FFullScreen then
      LeaveFullScreen;

    internal_ShowCursor(True);
  end;

  XSync(FDisplay, False);
end;

function TX11WindowDisplay_MatchAnyEvent(display: PDisplay; event: PXEvent; arg: TXPointer): LongBool; cdecl;
begin
  Result := LongBool(1);
end;

procedure TX11WindowDisplay.HandleEvents;
var
  e: TXEvent;
  NewFocus: Boolean;
  NewFocusSpecified: Boolean;

  function UsefulEventsPending: Boolean;
  var
    tmpEvent: TXEvent;
  begin
    if XCheckIfEvent(FDisplay, @tmpEvent, @TX11WindowDisplay_MatchAnyEvent, nil) then
    begin
      Result := True;
      XPutBackEvent(FDisplay, @tmpEvent);
      exit;
    end;

    Result := False;
  end;

  procedure HandleMouseEvent;
  var
    x, y: cint;
    state: cuint;
    PTCMouseButtonState: TPTCMouseButtonState;

    button: TPTCMouseButton;
    before, after: Boolean;
    cstate: TPTCMouseButtonState;
  begin
{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
    { if XInput2 is enabled, and we're in relative mouse mode, then don't handle
    the regular mouse events -> use the XInput2 raw mouse events in this case }
    if FXInput2Enabled and FRelativeMouseMode then
      exit;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}
    if not FPreviousMousePositionSaved then
    begin
      FPreviousMouseX := -1; { -1 indicates relative mouse mode }
      FPreviousMouseY := -1; { -1 indicates relative mouse mode }
      FPreviousMouseButtonState := [];
      PTCMouseButtonState := [];
    end
    else
      PTCMouseButtonState := FPreviousMouseButtonState
        - [PTCMouseButton1, PTCMouseButton2, PTCMouseButton3, PTCMouseButton4, PTCMouseButton5];
    case e._type of
      MotionNotify: begin
        x := e.xmotion.x;
        y := e.xmotion.y;
        state := e.xmotion.state;
      end;
      ButtonPress, ButtonRelease: begin
        x := e.xbutton.x;
        y := e.xbutton.y;
        state := e.xbutton.state;
        if e._type = ButtonPress then
        begin
          case e.xbutton.button of
            Button1: state := state or Button1Mask;
            Button2: state := state or Button2Mask;
            Button3: state := state or Button3Mask;
            Button4: state := state or Button4Mask;
            Button5: state := state or Button5Mask;
            6..Ord(High(TPTCMouseButton))+1:
              Include(PTCMouseButtonState, TPTCMouseButton((Ord(PTCMouseButton6)-6)+e.xbutton.button));
          end;
        end
        else
        begin
          case e.xbutton.button of
            Button1: state := state and (not Button1Mask);
            Button2: state := state and (not Button2Mask);
            Button3: state := state and (not Button3Mask);
            Button4: state := state and (not Button4Mask);
            Button5: state := state and (not Button5Mask);
            6..Ord(High(TPTCMouseButton))+1:
              Exclude(PTCMouseButtonState, TPTCMouseButton((Ord(PTCMouseButton6)-6)+e.xbutton.button));
          end;
        end;
      end;
      else
        raise TPTCError.Create('Internal Error');
    end;

    if (state and Button1Mask) <> 0 then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton1];
    if (state and Button2Mask) <> 0 then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton3];
    if (state and Button3Mask) <> 0 then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton2];
    if (state and Button4Mask) <> 0 then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton4];
    if (state and Button5Mask) <> 0 then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton5];

    if (x >= 0) and (x < FWidth) and (y >= 0) and (y < FHeight) then
    begin
      if not FPreviousMousePositionSaved then
      begin
        FPreviousMouseX := x; { first DeltaX will be 0 }
        FPreviousMouseY := y; { first DeltaY will be 0 }
        FPreviousMouseButtonState := [];
      end
      { there's a previous mouse state saved, but it was in relative mouse mode? }
      else if (FPreviousMouseX = -1) or (FPreviousMouseY = -1) then
      begin
        FPreviousMouseX := x; { first DeltaX after relative mouse mode will be 0 }
        FPreviousMouseY := y; { first DeltaY after relative mouse mode will be 0 }
      end;

      { movement? }
      if (x <> FPreviousMouseX) or (y <> FPreviousMouseY) then
        FEventQueue.AddEvent(TPTCMouseEvent.Create(x, y, x - FPreviousMouseX, y - FPreviousMouseY, FPreviousMouseButtonState));

      { button presses/releases? }
      cstate := FPreviousMouseButtonState;
      for button := Low(button) to High(button) do
      begin
        before := button In FPreviousMouseButtonState;
        after := button In PTCMouseButtonState;
        if after and (not before) then
        begin
          { button was pressed }
          cstate := cstate + [button];
          FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(x, y, 0, 0, cstate, True, button));
        end
        else
          if before and (not after) then
          begin
            { button was released }
            cstate := cstate - [button];
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(x, y, 0, 0, cstate, False, button));
          end;
      end;

      FPreviousMouseX := x;
      FPreviousMouseY := y;
      FPreviousMouseButtonState := PTCMouseButtonState;
      FPreviousMousePositionSaved := True;
    end;
  end;

  procedure HandleConfigureNotifyEvent;
  begin
    if FFullScreen or not FResizable then
      exit;

    if (FPreviousWidth <> e.xconfigure.width) or (FPreviousHeight <> e.xconfigure.height) then
      FEventQueue.AddEvent(TPTCResizeEvent.Create(e.xconfigure.width, e.xconfigure.height));

    FPreviousWidth := e.xconfigure.width;
    FPreviousHeight := e.xconfigure.height;
  end;

{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
  procedure HandleXInput2RawEvent(const xi2e: TXIRawEvent);
  var
    I: Integer;
    MovementXAccelerated: cdouble = 0;
    MovementXRaw: cdouble = 0;
    MovementXRawInt: Integer = 0;
    MovementXAvailable: Boolean = False;
    MovementYAccelerated: cdouble = 0;
    MovementYRaw: cdouble = 0;
    MovementYRawInt: Integer = 0;
    MovementYAvailable: Boolean = False;
    valptr, rawptr: Pcdouble;
    PTCMouseButtonState: TPTCMouseButtonState;

    button: TPTCMouseButton;
    before, after: Boolean;
    cstate: TPTCMouseButtonState;
  begin
{    Writeln(xi2e.evtype, ' ', xi2e.detail);
    valptr := xi2e.valuators.values;
    rawptr := xi2e.raw_values;
    for I := 0 to xi2e.valuators.mask_len*8 - 1 do
      if XIMaskIsSet(xi2e.valuators.mask, I) then
      begin
        Writeln('Valuator ', I, ' value=', valptr^:0:50, ' raw=', rawptr^:0:50);
        Inc(valptr);
        Inc(rawptr);
      end;}

    { when not in relative mouse mode, we use the regular X11 mouse events }
    if not FRelativeMouseMode then
      exit;

    if not FPreviousMousePositionSaved then
    begin
      FPreviousMouseX := -1; { -1 indicates relative mouse mode }
      FPreviousMouseY := -1; { -1 indicates relative mouse mode }
      FPreviousMouseButtonState := [];
    end;

    if (xi2e.evtype = XI_RawButtonPress) or (xi2e.evtype = XI_RawButtonRelease) then
    begin
      PTCMouseButtonState := FPreviousMouseButtonState;
      if xi2e.evtype = XI_RawButtonPress then
        case xi2e.detail of
          Button1: PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton1];
          Button2: PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton3];
          Button3: PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton2];
          Button4: PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton4];
          Button5: PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton5];
          6..Ord(High(TPTCMouseButton))+1:
            Include(PTCMouseButtonState, TPTCMouseButton((Ord(PTCMouseButton6)-6)+xi2e.detail));
        end
      else
        case xi2e.detail of
          Button1: PTCMouseButtonState := PTCMouseButtonState - [PTCMouseButton1];
          Button2: PTCMouseButtonState := PTCMouseButtonState - [PTCMouseButton3];
          Button3: PTCMouseButtonState := PTCMouseButtonState - [PTCMouseButton2];
          Button4: PTCMouseButtonState := PTCMouseButtonState - [PTCMouseButton4];
          Button5: PTCMouseButtonState := PTCMouseButtonState - [PTCMouseButton5];
          6..Ord(High(TPTCMouseButton))+1:
            Exclude(PTCMouseButtonState, TPTCMouseButton((Ord(PTCMouseButton6)-6)+xi2e.detail));
        end;
    end
    else
      PTCMouseButtonState := FPreviousMouseButtonState;

    if xi2e.valuators.mask_len > 0 then
    begin
      valptr := xi2e.valuators.values;
      rawptr := xi2e.raw_values;
      if XIMaskIsSet(xi2e.valuators.mask, 0) then
      begin
        MovementXAccelerated := valptr^;
        MovementXRaw := rawptr^;
        MovementXRawInt := Round(MovementXRaw);
        MovementXAvailable := True;
        Inc(valptr);
        Inc(rawptr);
      end;
      if XIMaskIsSet(xi2e.valuators.mask, 1) then
      begin
        MovementYAccelerated := valptr^;
        MovementYRaw := rawptr^;
        MovementYRawInt := Round(MovementYRaw);
        MovementYAvailable := True;
        Inc(valptr);
        Inc(rawptr);
      end;
    end;

    { movement? }
    if (MovementXRawInt <> 0) or (MovementYRawInt <> 0) then
      FEventQueue.AddEvent(TPTCMouseEvent.Create(-1, -1, MovementXRawInt, MovementYRawInt, FPreviousMouseButtonState));

    { button presses/releases? }
    cstate := FPreviousMouseButtonState;
    for button := Low(button) to High(button) do
    begin
      before := button In FPreviousMouseButtonState;
      after := button In PTCMouseButtonState;
      if after and (not before) then
      begin
        { button was pressed }
        cstate := cstate + [button];
        FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(-1, -1, 0, 0, cstate, True, button));
      end
      else
        if before and (not after) then
        begin
          { button was released }
          cstate := cstate - [button];
          FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(-1, -1, 0, 0, cstate, False, button));
        end;
    end;

    FPreviousMouseX := -1;
    FPreviousMouseY := -1;
    FPreviousMouseButtonState := PTCMouseButtonState;
    FPreviousMousePositionSaved := True;
  end;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}

var
{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
  XInput2Event: PXIDeviceEvent;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}
  eaten_by_im: Boolean = False;
begin
  NewFocusSpecified := False;
  while UsefulEventsPending do
  begin
    XNextEvent(FDisplay, @e);
    { maybe send the event to the Input Method for processing. Only non-key
      events are sent here (key events are sent in HandleKeyEvent instead) }
    if Assigned(FXIM) and Assigned(FXIC) and
      (e._type <> KeyPress) and (e._type <> KeyRelease) then
    begin
      eaten_by_im := XFilterEvent(@e, FWindow);
      if eaten_by_im then
        exit;
    end;
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
        if (e.xclient.format = 32) and (TAtom(e.xclient.data.l[0]) = FAtomClose) then
          if InterceptClose then
            FEventQueue.AddEvent(TPTCCloseEvent.Create)
          else
            Halt(0);
      end;
      Expose: begin
        if e.xexpose.count = 0 then
          Draw;
      end;
      ConfigureNotify: HandleConfigureNotifyEvent;
      KeyPress, KeyRelease: HandleKeyEvent(e.xkey);
      ButtonPress, ButtonRelease, MotionNotify: HandleMouseEvent;
      EnterNotify: begin
        { clear the high (>=6) numbered mouse buttons, because the mouse events contain no state information for them }
        FPreviousMouseButtonState := FPreviousMouseButtonState
          * [PTCMouseButton1, PTCMouseButton2, PTCMouseButton3, PTCMouseButton4, PTCMouseButton5];
      end;
{$IFDEF ENABLE_X11_EXTENSION_XINPUT2}
      GenericEvent: begin
        if e.xcookie.extension = FXInput2MajorOpCode then
        begin
          if XGetEventData(FDisplay, @e.xcookie) then
            try
              XInput2Event := e.xcookie.data;
              case XInput2Event^.evtype of
                XI_RawButtonPress,
                XI_RawButtonRelease,
                XI_RawMotion:
                  HandleXInput2RawEvent(PXIRawEvent(XInput2Event)^);
              end;
            finally
              XFreeEventData(FDisplay, @e.xcookie);
            end;
        end;
      end;
{$ENDIF ENABLE_X11_EXTENSION_XINPUT2}
    end;
  end;
  if NewFocusSpecified then
    HandleChangeFocus(NewFocus);

  if FFocus then
    internal_GrabMouse(FGrabMouse);
end;

function TX11WindowDisplay.MoveMouseTo(X, Y: Integer): Boolean;
begin
  if not FOpen or (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    exit(False);
  XWarpPointer(FDisplay, None, FWindow, 0, 0, 0, 0, X, Y);
  Result := True;
end;

procedure TX11WindowDisplay.Draw;
begin
  if not (PTC_X11_USE_OPENGL in FFlags) then
  begin
    FPrimary.Put(FWindow, FGC, FDestX, FDestY);
  end;
end;

procedure TX11WindowDisplay.Update;
begin
  Draw;

  HandleEvents;
end;

procedure TX11WindowDisplay.Update(AArea: IPTCArea);
var
  UpdateArea: IPTCArea;
begin
  UpdateArea := TPTCClipper.Clip(TPTCArea.Create(0, 0, FWidth, FHeight), AArea);
  FPrimary.Put(FWindow, FGC, UpdateArea.Left, UpdateArea.Top,
               FDestX + UpdateArea.Left, FDestY + UpdateArea.Top,
               UpdateArea.Width, UpdateArea.Height);

  HandleEvents;
end;

function TX11WindowDisplay.NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
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

function TX11WindowDisplay.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
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
  until (not AWait) or (Result <> nil);
end;

function TX11WindowDisplay.Lock: Pointer;
begin
  Result := FPrimary.Lock;
end;

procedure TX11WindowDisplay.Unlock;
begin
end;

procedure TX11WindowDisplay.GetModes(var AModes: TPTCModeList);
var
  current_desktop_format: IPTCFormat;
begin
  if FModeSwitcher = nil then
    FModeSwitcher := CreateModeSwitcher;

  current_desktop_format := GetX11Format(TPTCFormat.Create(8));

  FModeSwitcher.GetModes(AModes, current_desktop_format);
end;

procedure TX11WindowDisplay.Palette(APalette: IPTCPalette);
var
  pal: PUint32;
  i: Integer;
begin
  pal := APalette.Data;
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
end;

function TX11WindowDisplay.GetPitch: Integer;
begin
  Result := FPrimary.Pitch;
end;

function TX11WindowDisplay.CreateImage(ADisplay: PDisplay; AScreen, AWidth, AHeight: Integer;
                                       AFormat: IPTCFormat): TX11Image;
begin
  {$IFDEF ENABLE_X11_EXTENSION_XSHM}
  if (PTC_X11_TRY_XSHM In FFlags) and XShmQueryExtension(ADisplay) then
  begin
    try
      LOG('trying to create a XShm image');
      Result := TX11ShmImage.Create(ADisplay, AScreen, AWidth, AHeight, AFormat);
      exit;
    except
      LOG('XShm failed');
    end;
  end;
  {$ENDIF ENABLE_X11_EXTENSION_XSHM}

  LOG('trying to create a normal image');
  Result := TX11NormalImage.Create(ADisplay, AScreen, AWidth, AHeight, AFormat);
end;

function TX11WindowDisplay.CreateModeSwitcher: TX11Modes;
begin
{$IFDEF ENABLE_X11_EXTENSION_XRANDR}
  if PTC_X11_TRY_XRANDR in FFlags then
    try
      LOG('trying to initialize the Xrandr mode switcher');
      Result := TX11ModesXrandr.Create(FDisplay, FScreen);
      exit;
    except
      LOG('Xrandr failed');
    end;
{$ENDIF ENABLE_X11_EXTENSION_XRANDR}

{$IFDEF ENABLE_X11_EXTENSION_XF86VIDMODE}
  if PTC_X11_TRY_XF86VIDMODE in FFlags then
    try
      LOG('trying to initialize the XF86VidMode mode switcher');
      Result := TX11ModesXF86VidMode.Create(FDisplay, FScreen);
      exit;
    except
      LOG('XF86VidMode failed');
    end;
{$ENDIF ENABLE_X11_EXTENSION_XF86VIDMODE}

  LOG('creating the standard NoModeSwitching mode switcher');
  Result := TX11ModesNoModeSwitching.Create(FDisplay, FScreen);
end;

function TX11WindowDisplay.GetX11Window: TWindow;
begin
  Result := FWindow;
end;

function TX11WindowDisplay.GetX11GC: TGC;
begin
  Result := FGC;
end;

function TX11WindowDisplay.IsFullScreen: Boolean;
begin
  Result := FFullScreen;
end;

function TX11WindowDisplay.IsOpen: Boolean;
begin
  Result := FOpen;
end;

procedure TX11WindowDisplay.CreateColormap; { Register colour maps }
var
  i: Integer;
  r, g, b: Single;
begin
  if FFormat.Bits = 8 then
  begin
    FColours := GetMem(256 * SizeOf(TXColor));
    if FColours = nil then
      raise TPTCError.Create('Cannot allocate colour map cells');
    FCMap := XCreateColormap(FDisplay, RootWindow(FDisplay, FScreen),
                             DefaultVisual(FDisplay, FScreen), AllocAll);
    if FCMap = 0 then
      raise TPTCError.Create('Cannot create colour map');
    XInstallColormap(FDisplay, FCMap);
    XSetWindowColormap(FDisplay, FWindow, FCMap);
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
  end;
end;

{$IFDEF ENABLE_X11_EXTENSION_GLX}
procedure TX11WindowDisplay.OpenGL_SwapBuffers;
begin
  FGLXFBConfig.SwapBuffers;
end;

procedure TX11WindowDisplay.OpenGL_SetSwapInterval(AInterval: Integer);
begin
  FGLXFBConfig.SetSwapInterval(AInterval);
end;

function TX11WindowDisplay.OpenGL_GetSwapInterval: Integer;
begin
  Result := FGLXFBConfig.GetSwapInterval;
end;
{$ENDIF ENABLE_X11_EXTENSION_GLX}
