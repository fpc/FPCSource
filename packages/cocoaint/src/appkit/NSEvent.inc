{ Parsed from Appkit.framework NSEvent.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSEVENT_PAS_H}
{$define NSEVENT_PAS_H}
type
  NSEventPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSEVENT_PAS_T}
{$define NSEVENT_PAS_T}

{ Constants }

const
  NSLeftMouseDown = 1;
  NSLeftMouseUp = 2;
  NSRightMouseDown = 3;
  NSRightMouseUp = 4;
  NSMouseMoved = 5;
  NSLeftMouseDragged = 6;
  NSRightMouseDragged = 7;
  NSMouseEntered = 8;
  NSMouseExited = 9;
  NSKeyDown = 10;
  NSKeyUp = 11;
  NSFlagsChanged = 12;
  NSAppKitDefined = 13;
  NSSystemDefined = 14;
  NSApplicationDefined = 15;
  NSPeriodic = 16;
  NSCursorUpdate = 17;
  NSScrollWheel = 22;
  NSTabletPoint = 23;
  NSTabletProximity = 24;
  NSOtherMouseDown = 25;
  NSOtherMouseUp = 26;
  NSOtherMouseDragged = 27;

const
  NSAnyEventMask = NSUIntegerMax;

const
  NSAlphaShiftKeyMask = 1  shl  16;
  NSShiftKeyMask = 1  shl  17;
  NSControlKeyMask = 1  shl  18;
  NSAlternateKeyMask = 1  shl  19;
  NSCommandKeyMask = 1  shl  20;
  NSNumericPadKeyMask = 1  shl  21;
  NSHelpKeyMask = 1  shl  22;
  NSFunctionKeyMask = 1  shl  23;
  NSDeviceIndependentModifierFlagsMask = $ffff0000;

const
  NSUnknownPointingDevice = NX_TABLET_POINTER_UNKNOWN;
  NSPenPointingDevice = NX_TABLET_POINTER_PEN;
  NSCursorPointingDevice = NX_TABLET_POINTER_CURSOR;
  NSEraserPointingDevice = NX_TABLET_POINTER_ERASER;

const
  NSPenTipMask = NX_TABLET_BUTTON_PENTIPMASK;
  NSPenLowerSideMask = NX_TABLET_BUTTON_PENLOWERSIDEMASK;
  NSPenUpperSideMask = NX_TABLET_BUTTON_PENUPPERSIDEMASK;

const
  NSUpArrowFunctionKey = $F700;
  NSDownArrowFunctionKey = $F701;
  NSLeftArrowFunctionKey = $F702;
  NSRightArrowFunctionKey = $F703;
  NSF1FunctionKey = $F704;
  NSF2FunctionKey = $F705;
  NSF3FunctionKey = $F706;
  NSF4FunctionKey = $F707;
  NSF5FunctionKey = $F708;
  NSF6FunctionKey = $F709;
  NSF7FunctionKey = $F70A;
  NSF8FunctionKey = $F70B;
  NSF9FunctionKey = $F70C;
  NSF10FunctionKey = $F70D;
  NSF11FunctionKey = $F70E;
  NSF12FunctionKey = $F70F;
  NSF13FunctionKey = $F710;
  NSF14FunctionKey = $F711;
  NSF15FunctionKey = $F712;
  NSF16FunctionKey = $F713;
  NSF17FunctionKey = $F714;
  NSF18FunctionKey = $F715;
  NSF19FunctionKey = $F716;
  NSF20FunctionKey = $F717;
  NSF21FunctionKey = $F718;
  NSF22FunctionKey = $F719;
  NSF23FunctionKey = $F71A;
  NSF24FunctionKey = $F71B;
  NSF25FunctionKey = $F71C;
  NSF26FunctionKey = $F71D;
  NSF27FunctionKey = $F71E;
  NSF28FunctionKey = $F71F;
  NSF29FunctionKey = $F720;
  NSF30FunctionKey = $F721;
  NSF31FunctionKey = $F722;
  NSF32FunctionKey = $F723;
  NSF33FunctionKey = $F724;
  NSF34FunctionKey = $F725;
  NSF35FunctionKey = $F726;
  NSInsertFunctionKey = $F727;
  NSDeleteFunctionKey = $F728;
  NSHomeFunctionKey = $F729;
  NSBeginFunctionKey = $F72A;
  NSEndFunctionKey = $F72B;
  NSPageUpFunctionKey = $F72C;
  NSPageDownFunctionKey = $F72D;
  NSPrintScreenFunctionKey = $F72E;
  NSScrollLockFunctionKey = $F72F;
  NSPauseFunctionKey = $F730;
  NSSysReqFunctionKey = $F731;
  NSBreakFunctionKey = $F732;
  NSResetFunctionKey = $F733;
  NSStopFunctionKey = $F734;
  NSMenuFunctionKey = $F735;
  NSUserFunctionKey = $F736;
  NSSystemFunctionKey = $F737;
  NSPrintFunctionKey = $F738;
  NSClearLineFunctionKey = $F739;
  NSClearDisplayFunctionKey = $F73A;
  NSInsertLineFunctionKey = $F73B;
  NSDeleteLineFunctionKey = $F73C;
  NSInsertCharFunctionKey = $F73D;
  NSDeleteCharFunctionKey = $F73E;
  NSPrevFunctionKey = $F73F;
  NSNextFunctionKey = $F740;
  NSSelectFunctionKey = $F741;
  NSExecuteFunctionKey = $F742;
  NSUndoFunctionKey = $F743;
  NSRedoFunctionKey = $F744;
  NSFindFunctionKey = $F745;
  NSHelpFunctionKey = $F746;
  NSModeSwitchFunctionKey = $F747;

const
  NSWindowExposedEventType = 0;
  NSApplicationActivatedEventType = 1;
  NSApplicationDeactivatedEventType = 2;
  NSWindowMovedEventType = 4;
  NSScreenChangedEventType = 8;
  NSAWTEventType = 16;

const
  NSPowerOffEventType = 1;

const
  NSMouseEventSubtype = NX_SUBTYPE_DEFAULT;
  NSTabletPointEventSubtype = NX_SUBTYPE_TABLET_POINT;
  NSTabletProximityEventSubtype = NX_SUBTYPE_TABLET_PROXIMITY;

{ Types }
type
  NSEventType = NSUInteger;
  NSPointingDeviceType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSEVENT_PAS_R}
{$define NSEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSEVENT_PAS_F}
{$define NSEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSEVENT_PAS_T}
{$define NSEVENT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSEvent = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSEVENT_PAS_C}
{$define NSEVENT_PAS_C}

{ NSEvent }
  NSEvent = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __type: NSEventType;
    __location: NSPoint;
    __modifierFlags: cuint;
    __WSTimestamp: NSInteger;
    __timestamp: NSTimeInterval;
    __windowNumber: NSInteger;
    __window: NSWindow;
    __context: NSGraphicsContext;
    _mouse: record
        eventNumber: cint;
        clickCount: cint;
        pressure: single;
        {$ifdef cpu64}
        deltaX: CGFloat;
        deltaY: CGFloat;
        subtype: cint;
        buttonNumber: cshort;
        reserved1: cshort;
            reserved2: array[0..2] of cint;
        {$endif}
      end;
    _key: record
        keys: NSString;
        unmodKeys: NSString;
        keyCode: cushort;
        isARepeat: Boolean;
        {$ifdef cpu64}
            reserved: array[0..5] of cint;
        {$endif}
      end;
    _tracking: record
        eventNumber: cint;
        trackingNumber: NSInteger;
        userData: Pointer;
        {$ifdef cpu64}
            reserved: array[0..5] of cint;
        {$endif}
      end;
    _scrollWheel: record
        deltaX: CGFloat;
        deltaY: CGFloat;
        deltaZ: CGFloat;
        {$ifdef cpu64}
        subtype: cshort;
        reserved1: cshort;
            reserved2: array[0..5] of cint;
        {$endif}
      end;
    _misc: record
        subtype: cint;
        data1: NSInteger;
        data2: NSInteger;
        {$ifdef cpu64}
            reserved: array[0..5] of cint;
        {$endif}
      end;
    {$ifdef cpu64}
        tabletPointData: array[0..13] of cint;
        tabletProximityData: array[0..13] of cint;
    {$endif}
    __data: record
      end;
    __eventRef: Pointer;
    {$ifdef cpu64}
    _reserved1: Pointer;
    _reserved2: Pointer;
    {$endif}
    
  public
    class function alloc: NSEvent; message 'alloc';

    function type_: NSEventType; message 'type';
    function modifierFlags: NSUInteger; message 'modifierFlags';
    function timestamp: NSTimeInterval; message 'timestamp';
    function window: NSWindow; message 'window';
    function windowNumber: NSInteger; message 'windowNumber';
    function context: NSGraphicsContext; message 'context';
    function clickCount: NSInteger; message 'clickCount';
    function buttonNumber: NSInteger; message 'buttonNumber';
    function eventNumber: NSInteger; message 'eventNumber';
    function pressure: single; message 'pressure';
    function locationInWindow: NSPoint; message 'locationInWindow';
    function deltaX: CGFloat; message 'deltaX';
    function deltaY: CGFloat; message 'deltaY';
    function deltaZ: CGFloat; message 'deltaZ';
    function characters: NSString; message 'characters';
    function charactersIgnoringModifiers: NSString; message 'charactersIgnoringModifiers';
    function isARepeat: Boolean; message 'isARepeat';
    function keyCode: cushort; message 'keyCode';
    function trackingNumber: NSInteger; message 'trackingNumber';
    function userData: Pointer; message 'userData';
    function trackingArea: NSTrackingArea; message 'trackingArea';
    function subtype: cshort; message 'subtype';
    function data1: NSInteger; message 'data1';
    function data2: NSInteger; message 'data2';
    function eventRef: Pointer; message 'eventRef';
    class function eventWithEventRef(eventRef_: Pointer): NSEvent; message 'eventWithEventRef:';
    function CGEvent: CGEventRef; message 'CGEvent';
    class function eventWithCGEvent(CGEvent_: CGEventRef): NSEvent; message 'eventWithCGEvent:';
    class procedure setMouseCoalescingEnabled(flag: Boolean); message 'setMouseCoalescingEnabled:';
    class function isMouseCoalescingEnabled: Boolean; message 'isMouseCoalescingEnabled';
    function deviceID: NSUInteger; message 'deviceID';
    function absoluteX: NSInteger; message 'absoluteX';
    function absoluteY: NSInteger; message 'absoluteY';
    function absoluteZ: NSInteger; message 'absoluteZ';
    function buttonMask: NSUInteger; message 'buttonMask';
    function tilt: NSPoint; message 'tilt';
    function rotation: single; message 'rotation';
    function tangentialPressure: single; message 'tangentialPressure';
    function vendorDefined: id; message 'vendorDefined';
    function vendorID: NSUInteger; message 'vendorID';
    function tabletID: NSUInteger; message 'tabletID';
    function pointingDeviceID: NSUInteger; message 'pointingDeviceID';
    function systemTabletID: NSUInteger; message 'systemTabletID';
    function vendorPointingDeviceType: NSUInteger; message 'vendorPointingDeviceType';
    function pointingDeviceSerialNumber: NSUInteger; message 'pointingDeviceSerialNumber';
    function uniqueID: culonglong; message 'uniqueID';
    function capabilityMask: NSUInteger; message 'capabilityMask';
    function pointingDeviceType: NSPointingDeviceType; message 'pointingDeviceType';
    function isEnteringProximity: Boolean; message 'isEnteringProximity';
    class procedure startPeriodicEventsAfterDelay_withPeriod(delay: NSTimeInterval; period: NSTimeInterval); message 'startPeriodicEventsAfterDelay:withPeriod:';
    class procedure stopPeriodicEvents; message 'stopPeriodicEvents';
    class function mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure(type__: NSEventType; location: NSPoint; flags: NSUInteger; time: NSTimeInterval; wNum: NSInteger; context_: NSGraphicsContext; eNum: NSInteger; cNum: NSInteger; pressure_: single): NSEvent; message 'mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:';
    class function keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCode(type__: NSEventType; location: NSPoint; flags: NSUInteger; time: NSTimeInterval; wNum: NSInteger; context_: NSGraphicsContext; keys: NSString; ukeys: NSString; flag: Boolean; code: cushort): NSEvent; message 'keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:';
    class function enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData(type__: NSEventType; location: NSPoint; flags: NSUInteger; time: NSTimeInterval; wNum: NSInteger; context_: NSGraphicsContext; eNum: NSInteger; tNum: NSInteger; data: Pointer): NSEvent; message 'enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:';
    class function otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2(type__: NSEventType; location: NSPoint; flags: NSUInteger; time: NSTimeInterval; wNum: NSInteger; context_: NSGraphicsContext; subtype_: cshort; d: NSInteger; d1: NSInteger): NSEvent; message 'otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:';
    class function mouseLocation: NSPoint; message 'mouseLocation';
  end; external;

{$endif}
{$endif}
