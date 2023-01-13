{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2015, 2021 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

threadvar
  AutoreleasePool: NSAutoreleasePool;

function NSWStr(const ws: WideString): NSString;
begin
  if Length(ws) = 0 then
    Result := NSString.alloc.init
  else
    Result := NSString.alloc.initWithCharacters_length(@ws[1], Length(ws));
end;

{ NSPTCWindowDelegate }

function NSPTCWindowDelegate.windowShouldClose(sender: id): ObjCBOOL;
begin
  if Assigned(FConsole) then
    Result := FConsole.HandleWindowShouldClose(sender)
  else
    Result := True;
end;

{ NSPTCView }

procedure NSPTCView.drawRect(dirtyRect: NSRect);
begin
  if Assigned(FConsole) then
    FConsole.FImage.drawInRect_fromRect_operation_fraction(NSMakeRect(0, 0, FConsole.FWidth, FConsole.FHeight), NSZeroRect, NSCompositeCopy, 1.0);
end;

{ NSPTCWindow }

procedure NSPTCWindow.keyDown(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaKeyEvent(theEvent, 'keyDown')) then
    {inherited};
end;

procedure NSPTCWindow.keyUp(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaKeyEvent(theEvent, 'keyUp')) then
    {inherited};
end;

procedure NSPTCWindow.flagsChanged(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaKeyEvent(theEvent, 'flagsChanged')) then
    {inherited};
end;

procedure NSPTCWindow.mouseDown(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'mouseDown')) then
    inherited;
end;

procedure NSPTCWindow.mouseDragged(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'mouseDragged')) then
    inherited;
end;

procedure NSPTCWindow.mouseEntered(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'mouseEntered')) then
    inherited;
end;

procedure NSPTCWindow.mouseExited(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'mouseExited')) then
    inherited;
end;

procedure NSPTCWindow.mouseMoved(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'mouseMoved')) then
    inherited;
end;

procedure NSPTCWindow.mouseUp(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'mouseUp')) then
    inherited;
end;

procedure NSPTCWindow.rightMouseDown(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'rightMouseDown')) then
    inherited;
end;

procedure NSPTCWindow.rightMouseDragged(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'rightMouseDragged')) then
    inherited;
end;

procedure NSPTCWindow.rightMouseUp(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'rightMouseUp')) then
    inherited;
end;

procedure NSPTCWindow.otherMouseDown(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'otherMouseDown')) then
    inherited;
end;

procedure NSPTCWindow.otherMouseDragged(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'otherMouseDragged')) then
    inherited;
end;

procedure NSPTCWindow.otherMouseUp(theEvent: NSEvent);
begin
  if not (Assigned(FConsole) and FConsole.HandleCocoaMouseEvent(theEvent, 'otherMouseUp')) then
    inherited;
end;


{ TCocoaConsole }

class procedure TCocoaConsole.MaybeCreateAutoreleasePool;
begin
  if AutoreleasePool = nil then
    AutoreleasePool := NSAutoreleasePool.new;
end;

function TCocoaConsole.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TCocoaConsole.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TCocoaConsole.GetPitch: Integer;
begin
  Result := FPitch;
end;

function TCocoaConsole.GetArea: IPTCArea;
begin
  FArea := TPTCArea.Create(0, 0, FWidth, FHeight);
  Result := FArea;
end;

function TCocoaConsole.GetFormat: IPTCFormat;
begin
  Result := FFormat;
end;

function TCocoaConsole.GetPages: Integer;
begin
  Result := 1;
end;

function TCocoaConsole.GetName: AnsiString;
begin
  Result := 'Cocoa';
end;

function TCocoaConsole.GetTitle: AnsiString;
begin
  Result := FTitle;
end;

function TCocoaConsole.GetInformation: AnsiString;
begin
  Result := '';
end;

function TCocoaConsole.TranslateKeyCode(kcode: cushort): Integer;
begin
  case kcode of
    10:  exit(0);  // Section sign (U+00A7)
    18:  exit(PTCKEY_ONE);
    19:  exit(PTCKEY_TWO);
    20:  exit(PTCKEY_THREE);
    21:  exit(PTCKEY_FOUR);
    23:  exit(PTCKEY_FIVE);
    22:  exit(PTCKEY_SIX);
    26:  exit(PTCKEY_SEVEN);
    28:  exit(PTCKEY_EIGHT);
    25:  exit(PTCKEY_NINE);
    29:  exit(PTCKEY_ZERO);
    27:  exit(PTCKEY_MINUS);
    24:  exit(PTCKEY_EQUALS);
    51:  exit(PTCKEY_BACKSPACE);
    48:  exit(PTCKEY_TAB);
    12:  exit(PTCKEY_Q);
    13:  exit(PTCKEY_W);
    14:  exit(PTCKEY_E);
    15:  exit(PTCKEY_R);
    17:  exit(PTCKEY_T);
    16:  exit(PTCKEY_Y);
    32:  exit(PTCKEY_U);
    34:  exit(PTCKEY_I);
    31:  exit(PTCKEY_O);
    35:  exit(PTCKEY_P);
    33:  exit(PTCKEY_OPENBRACKET);
    30:  exit(PTCKEY_CLOSEBRACKET);
    36:  exit(PTCKEY_ENTER);
    0:   exit(PTCKEY_A);
    1:   exit(PTCKEY_S);
    2:   exit(PTCKEY_D);
    3:   exit(PTCKEY_F);
    5:   exit(PTCKEY_G);
    4:   exit(PTCKEY_H);
    38:  exit(PTCKEY_J);
    40:  exit(PTCKEY_K);
    37:  exit(PTCKEY_L);
    41:  exit(PTCKEY_SEMICOLON);
    39:  exit(PTCKEY_QUOTE);
    42:  exit(PTCKEY_BACKSLASH);
    50:  exit(PTCKEY_BACKQUOTE);
    6:   exit(PTCKEY_Z);
    7:   exit(PTCKEY_X);
    8:   exit(PTCKEY_C);
    9:   exit(PTCKEY_V);
    11:  exit(PTCKEY_B);
    45:  exit(PTCKEY_N);
    46:  exit(PTCKEY_M);
    43:  exit(PTCKEY_COMMA);
    47:  exit(PTCKEY_PERIOD);
    44:  exit(PTCKEY_SLASH);
    49:  exit(PTCKEY_SPACE);
    53:  exit(PTCKEY_ESCAPE);
    126: exit(PTCKEY_UP);
    123: exit(PTCKEY_LEFT);
    125: exit(PTCKEY_DOWN);
    124: exit(PTCKEY_RIGHT);
    122: exit(PTCKEY_F1);
    120: exit(PTCKEY_F2);
    99:  exit(PTCKEY_F3);
    118: exit(PTCKEY_F4);
    96:  exit(PTCKEY_F5);
    97:  exit(PTCKEY_F6);
    98:  exit(PTCKEY_F7);
    100: exit(PTCKEY_F8);
    101: exit(PTCKEY_F9);
    109: exit(PTCKEY_F10);
    103: exit(PTCKEY_F11);
    111: exit(PTCKEY_F12);
    105: exit(PTCKEY_F13);
    107: exit(PTCKEY_F14);
    113: exit(PTCKEY_F15);
    106: exit(PTCKEY_F16);
    64:  exit(PTCKEY_F17);
    79:  exit(PTCKEY_F18);
    80:  exit(PTCKEY_F19);
    115: exit(PTCKEY_HOME);
    119: exit(PTCKEY_END);
    116: exit(PTCKEY_PAGEUP);
    121: exit(PTCKEY_PAGEDOWN);
    63:  exit(PTCKEY_FUNCTION);
    117: exit(PTCKEY_DELETE);
    56,                       // Left Shift
    60:  exit(PTCKEY_SHIFT);  // Right Shift
    59,                         // Left Ctrl
    62:  exit(PTCKEY_CONTROL);  // Right Ctrl
    58,                     // Left Option (Alt) key
    61:  exit(PTCKEY_ALT);  // Right Option (Alt) key
    55:  exit(PTCKEY_COMMAND);  // Left Command key
    54:  exit(PTCKEY_COMMAND);  // Right Command key
    57:  exit(PTCKEY_CAPSLOCK);
    82:  exit(PTCKEY_NUMPAD0);
    83:  exit(PTCKEY_NUMPAD1);
    84:  exit(PTCKEY_NUMPAD2);
    85:  exit(PTCKEY_NUMPAD3);
    86:  exit(PTCKEY_NUMPAD4);
    87:  exit(PTCKEY_NUMPAD5);
    88:  exit(PTCKEY_NUMPAD6);
    89:  exit(PTCKEY_NUMPAD7);
    91:  exit(PTCKEY_NUMPAD8);
    92:  exit(PTCKEY_NUMPAD9);
    71:  exit(PTCKEY_CLEAR);  // Clear (in place of Num Lock)
    81:  exit(PTCKEY_EQUALS);  // numpad '='
    75:  exit(PTCKEY_DIVIDE);
    67:  exit(PTCKEY_MULTIPLY);
    78:  exit(PTCKEY_SUBTRACT);
    69:  exit(PTCKEY_ADD);
    76:  exit(PTCKEY_ENTER);  // numpad 'Enter'
    65:  exit(PTCKEY_DECIMAL);
  else
    exit(0);
  end;
end;

function TCocoaConsole.HandleCocoaKeyEvent(theEvent: NSEvent;
  const Method: AnsiString): Boolean;
var
  evtype: NSEventType;
  kcode: cushort;
  modflags: NSUInteger;
  Code, UniCode: Integer;
  Press: Boolean;
  PressAndRelease: Boolean = False;
  Characters: NSString;
  ModifierKeys: TPTCModifierKeys;
begin
  evtype := theEvent.type_;
  kcode := theEvent.keyCode;
  modflags := theEvent.modifierFlags;
  if evtype in [NSKeyDown, NSKeyUp] then
    Characters := theEvent.characters
  else
    Characters := nil;
  LOG('cocoa key event ' + Method + ' type=' + IntToStr(evtype) + ' keyCode=' + IntToStr(kcode) + ' modifierFlags=' + IntToStr(modflags));
  Result := False;
  Code := TranslateKeyCode(kcode);
  Unicode := -1;
  if Assigned(Characters) and (Characters.length = 1) then
    Unicode := Characters.characterAtIndex(0);
  ModifierKeys := [];
  if (modflags and NSAlternateKeyMask) <> 0 then
    Include(ModifierKeys, pmkAlt);
  if (modflags and NSShiftKeyMask) <> 0 then
    Include(ModifierKeys, pmkShift);
  if (modflags and NSControlKeyMask) <> 0 then
    Include(ModifierKeys, pmkControl);
  if (modflags and NSCommandKeyMask) <> 0 then
    Include(ModifierKeys, pmkCommand);
  if (modflags and NSNumericPadKeyMask) <> 0 then
    Include(ModifierKeys, pmkNumPadKey);
  if (modflags and NSFunctionKeyMask) <> 0 then
    Include(ModifierKeys, pmkFunction);
  if (kcode = 54) or    { Right Command key }
     (kcode = 60) or    { Right Shift }
     (kcode = 61) or    { Right Option (Alt) key }
     (kcode = 62) then  { Right Ctrl }
    Include(ModifierKeys, pmkRightKey);
  case evtype of
    NSKeyDown: Press := True;
    NSKeyUp: Press := False;
    NSFlagsChanged:
      begin
        case Code of
          PTCKEY_SHIFT: Press := pmkShift in ModifierKeys;
          PTCKEY_CONTROL: Press := pmkControl in ModifierKeys;
          PTCKEY_ALT: Press := pmkAlt in ModifierKeys;
          PTCKEY_COMMAND: Press := pmkCommand in ModifierKeys;
          PTCKEY_CAPSLOCK:
            begin
              { we only receive a modifierFlags message when caps lock is pressed down,
                but not when it goes up, so we enqueue both press and release on the
                ptc event queue }
              PressAndRelease := True;
            end;
          PTCKEY_FUNCTION:
            Press := pmkFunction in ModifierKeys;
        else
          begin
            LOG('Unknown NSFlagsChanged key code');
            exit;
          end;
        end;
      end;
  end;
  case Code of
    PTCKEY_UP,
    PTCKEY_DOWN,
    PTCKEY_LEFT,
    PTCKEY_RIGHT:
      begin
        Exclude(ModifierKeys, pmkNumPadKey);
        Unicode := -1;
      end;
    PTCKEY_ENTER:
      begin
        if pmkControl in ModifierKeys then
        begin
          if not (pmkAlt in ModifierKeys) and
             not (pmkShift in ModifierKeys) then
            Unicode := $0A
          else
            Unicode := -1;
        end
        else
          Unicode := $0D;
      end;
    PTCKEY_BACKSPACE:
      begin
        if pmkControl in ModifierKeys then
        begin
          if not (pmkAlt in ModifierKeys) and
             not (pmkShift in ModifierKeys) then
            Unicode := $7F
          else
            Unicode := -1;
        end
        else
          Unicode := $08;
      end;
    PTCKEY_TAB:
      begin
        if not (pmkControl in ModifierKeys) and
           not (pmkAlt in ModifierKeys) and
           not (pmkCommand in ModifierKeys) then
          Unicode := 9
        else
          Unicode := -1;
      end;
    PTCKEY_DELETE,
    PTCKEY_PAGEUP,
    PTCKEY_PAGEDOWN,
    PTCKEY_HOME,
    PTCKEY_END,
    PTCKEY_CLEAR,
    PTCKEY_F1,
    PTCKEY_F2,
    PTCKEY_F3,
    PTCKEY_F4,
    PTCKEY_F5,
    PTCKEY_F6,
    PTCKEY_F7,
    PTCKEY_F8,
    PTCKEY_F9,
    PTCKEY_F10,
    PTCKEY_F11,
    PTCKEY_F12,
    PTCKEY_F13,
    PTCKEY_F14,
    PTCKEY_F15,
    PTCKEY_F16,
    PTCKEY_F17,
    PTCKEY_F18,
    PTCKEY_F19:
      Unicode := -1;
  end;
  if PressAndRelease then
  begin
    FEventQueue.AddEvent(TPTCKeyEventFactory.CreateNew(Code, Unicode, ModifierKeys, True));
    FEventQueue.AddEvent(TPTCKeyEventFactory.CreateNew(Code, -1, ModifierKeys, False));
  end
  else if Press then
    FEventQueue.AddEvent(TPTCKeyEventFactory.CreateNew(Code, Unicode, ModifierKeys, True))
  else
    FEventQueue.AddEvent(TPTCKeyEventFactory.CreateNew(Code, -1, ModifierKeys, False));
end;

function TCocoaConsole.HandleCocoaMouseEvent(theEvent: NSEvent;
  const Method: AnsiString): Boolean;
begin
  //Writeln('HandleCocoaMouseEvent ', Method, ' ', theEvent.type_);
  Result := False;
end;

function TCocoaConsole.HandleWindowShouldClose(sender: id): Boolean;
begin
  Result := False;
  if InterceptClose then
    FEventQueue.AddEvent(TPTCCloseEventFactory.CreateNew)
  else
    Halt(0);
end;

procedure TCocoaConsole.HandleEvents;
var
  pool: NSAutoreleasePool;
  event: NSEvent;
begin
  repeat
    pool := NSAutoreleasePool.alloc.init;
    try
      event := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask,
        NSDate.distantPast,
        NSDefaultRunLoopMode,
        True);
      if event <> nil then
      begin
        NSApp.sendEvent(event);
        NSApp.updateWindows;
      end;
    finally
      pool.release;
    end;
  until event = nil;
end;

constructor TCocoaConsole.Create;
var
  s: AnsiString;
begin
  inherited Create;

  FTitle := '';
  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FPalette := TPTCPalette.Create;
  FClip := TPTCArea.Create;
  FArea := TPTCArea.Create;
  FFormat := TPTCFormatFactory.CreateNew;
  FEventQueue := TEventQueue.Create;

  Configure('/usr/share/ptcpas/ptcpas.conf');
  s := fpgetenv('HOME');
  if s = '' then
    s := '/';
  if s[Length(s)] <> '/' then
    s := s + '/';
  s := s + '.ptcpas.conf';
  Configure(s);
end;

destructor TCocoaConsole.Destroy;
begin
  Close;

  FCopy.Free;
  FClear.Free;
  FEventQueue.Free;

  inherited Destroy;
end;

procedure TCocoaConsole.Copy(ASurface: IPTCSurface);
begin

end;

procedure TCocoaConsole.Copy(ASurface: IPTCSurface; ASource,
  ADestination: IPTCArea);
begin

end;

function TCocoaConsole.Lock: Pointer;
begin
  Result := FImageRep.bitmapData;
end;

procedure TCocoaConsole.Unlock;
begin

end;

procedure TCocoaConsole.Load(const APixels: Pointer; AWidth, AHeight,
  APitch: Integer; AFormat: IPTCFormat; APalette: IPTCPalette);
var
  console_pixels: Pointer;
begin
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
    Load(APixels, AWidth, AHeight, APitch, AFormat, APalette,
         TPTCArea.Create(0, 0, width, height), Area);
end;

procedure TCocoaConsole.Load(const APixels: Pointer; AWidth, AHeight,
  APitch: Integer; AFormat: IPTCFormat; APalette: IPTCPalette; ASource,
  ADestination: IPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: IPTCArea;
begin
  try
    console_pixels := Lock;
    try
      TPTCClipper.Clip(ASource, TPTCArea.Create(0, 0, AWidth, AHeight),
                       clipped_source,
                       ADestination, Clip,
                       clipped_destination);
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

procedure TCocoaConsole.Save(APixels: Pointer; AWidth, AHeight,
  APitch: Integer; AFormat: IPTCFormat; APalette: IPTCPalette);
begin

end;

procedure TCocoaConsole.Save(APixels: Pointer; AWidth, AHeight,
  APitch: Integer; AFormat: IPTCFormat; APalette: IPTCPalette; ASource,
  ADestination: IPTCArea);
begin

end;

procedure TCocoaConsole.Clear;
begin

end;

procedure TCocoaConsole.Clear(AColor: IPTCColor);
begin

end;

procedure TCocoaConsole.Clear(AColor: IPTCColor; AArea: IPTCArea);
begin

end;

procedure TCocoaConsole.Palette(APalette: IPTCPalette);
begin

end;

procedure TCocoaConsole.Clip(AArea: IPTCArea);
begin
  FClip := AArea;
end;

function TCocoaConsole.Option(const AOption: AnsiString): Boolean;
begin
  LOG('console option', AOption);
  Result := True;
  case AOption of
    'intercept window close': FInterceptClose := True;
    'enable logging': LOG_enabled := True;
    'disable logging': LOG_enabled := False;
  else
    Result := FCopy.Option(AOption);
  end;
end;

function TCocoaConsole.Clip: IPTCArea;
begin
  Result := FClip;
end;

function TCocoaConsole.Palette: IPTCPalette;
begin
  Result := FPalette;
end;

procedure TCocoaConsole.Configure(const AFileName: AnsiString);
var
  F: TextFile;
  S: AnsiString;
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

function TCocoaConsole.Modes: TPTCModeList;
begin
  Result := nil;
end;

procedure TCocoaConsole.Open(const ATitle: AnsiString; APages: Integer);
begin
  Open(ATitle, TPTCFormatFactory.CreateNew(32, $FF0000, $FF00, $FF), APages);
end;

procedure TCocoaConsole.Open(const ATitle: AnsiString; AFormat: IPTCFormat;
  APages: Integer);
begin
  Open(ATitle, 640, 480, AFormat, APages);
end;

procedure TCocoaConsole.Open(const ATitle: AnsiString; AWidth, AHeight: Integer;
  AFormat: IPTCFormat; APages: Integer);
var
  rct: NSRect;
  pool: NSAutoreleasePool;
begin
  LOG('TCocoaConsole.Open');
  LOG('width', AWidth);
  LOG('height', AHeight);
  LOG('format', AFormat);
  LOG('pages', APages);
  Close;
  FTitle := ATitle;
  FWidth := AWidth;
  FHeight := AHeight;

  MaybeCreateAutoreleasePool;
  pool := NSAutoreleasePool.alloc.init;
  try
    NSApplication.sharedApplication;
    NSApp.setActivationPolicy(NSApplicationActivationPolicyRegular);
    NSApp.finishLaunching;

    rct := NSMakeRect(0, 0, AWidth, AHeight);

    FWindowDelegate := NSPTCWindowDelegate.alloc.init;
    FWindowDelegate.FConsole := Self;
    FWindow := NSPTCWindow.alloc.initWithContentRect_styleMask_backing_defer(rct,
      NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask {or NSResizableWindowMask},
      NSBackingStoreBuffered,
      //NSBackingStoreRetained,
      //NSBackingStoreNonretained,
      false);
    FWindow.FConsole := Self;
    FWindow.setDelegate(FWindowDelegate);

    FImageRep := NSBitmapImageRep.alloc;
    FImageRep := FImageRep.initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsP{erPixel}(
      nil,
      AWidth,
      AHeight,
      8,
      3,
      False,
      False,
      NSDeviceRGBColorSpace,
      0,
      32);
    {$ifdef FPC_BIG_ENDIAN}
      FFormat := TPTCFormatFactory.CreateNew(32, $FF000000, $FF0000, $FF00);
    {$else}
      FFormat := TPTCFormatFactory.CreateNew(32, $FF, $FF00, $FF0000);
    {$endif}
    FPitch := FImageRep.bytesPerRow;

    FImage := NSImage.alloc.initWithSize(NSMakeSize(AWidth, AHeight));
    FImage.addRepresentation(FImageRep);

    FView := NSPTCView.alloc.initWithFrame(NSMakeRect(0, 0, AWidth, AHeight));
    FView.FConsole := Self;

    FWindow.setContentView(FView);

    FWindow.setAcceptsMouseMovedEvents(True);
    FWindow.center;
    FWindow.setTitle(NSWStr(ATitle).autorelease);
    FWindow.makeKeyAndOrderFront(NSApp);
    FWindow.makeMainWindow;

    NSApp.activateIgnoringOtherApps(True);

    { Set clipping area }
    FClip := TPTCArea.Create(0, 0, FWidth, FHeight);
  finally
    pool.release;
  end;
end;

procedure TCocoaConsole.Open(const ATitle: AnsiString; AMode: IPTCMode;
  APages: Integer);
begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
end;

procedure TCocoaConsole.Close;
begin
  LOG('TCocoaConsole.Close');
  if Assigned(FWindow) then
  begin
    LOG('closing and releasing window');
    FWindow.setDelegate(nil);
    FWindow.FConsole := nil;
    FWindow.close;
    FWindow := nil;
  end;
  if Assigned(FWindowDelegate) then
  begin
    LOG('releasing window delegate');
    FWindowDelegate.FConsole := nil;
    FWindowDelegate.release;
    FWindowDelegate := nil;
  end;
  if Assigned(FView) then
  begin
    LOG('releasing view');
    FView.release;
    FView := nil;
  end;
  if Assigned(FImage) then
  begin
    LOG('releasing image');
    FImage.release;
    FImage := nil;
  end;
  if Assigned(FImageRep) then
  begin
    LOG('releasing image rep');
    FImageRep.release;
    FImageRep := nil;
  end;
  LOG('TCocoaConsole.Close done');
end;

procedure TCocoaConsole.Flush;
begin

end;

procedure TCocoaConsole.Finish;
begin

end;

procedure TCocoaConsole.Update;
var
  pool: NSAutoreleasePool;
begin
  pool := NSAutoreleasePool.alloc.init;
  try
    FView.setNeedsDisplay_(True);
  finally
    pool.release;
  end;

  HandleEvents;
end;

procedure TCocoaConsole.Update(AArea: IPTCArea);
begin
  Update;
end;

function TCocoaConsole.NextEvent(out AEvent: IPTCEvent; AWait: Boolean;
  const AEventMask: TPTCEventMask): Boolean;
var
  pool: NSAutoreleasePool;
begin
  repeat
    { process all events from the Cocoa event queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);

    if AWait and (AEvent = Nil) then
    begin
      pool := NSAutoreleasePool.alloc.init;
      try
        { if the Cocoa event queue is empty, block until an event is received }
        NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask,
          NSDate.distantFuture,
          NSDefaultRunLoopMode,
          False);
      finally
        pool.release;
      end;
    end;
  until (not AWait) or (AEvent <> Nil);
  Result := AEvent <> nil;
end;

function TCocoaConsole.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
var
  pool: NSAutoreleasePool;
begin
  repeat
    { process all events from the Cocoa event queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);

    if AWait and (Result = Nil) then
    begin
      pool := NSAutoreleasePool.alloc.init;
      try
        { if the Cocoa event queue is empty, block until an event is received }
        NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask,
          NSDate.distantFuture,
          NSDefaultRunLoopMode,
          False);
      finally
        pool.release;
      end;
    end;
  until (not AWait) or (Result <> nil);
end;

