{ Parsed from AppKit.framework NSEvent.h }


{$ifdef TYPES}
type
  NSEventPtr = ^NSEvent;
{$endif}

{$ifdef TYPES}
type
  NSEventType = NSUInteger;
  NSEventTypePtr = ^NSEventType;

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
  NSEventTypeGesture = 29 { available in 10_5 };
  NSEventTypeMagnify = 30 { available in 10_5 };
  NSEventTypeSwipe = 31 { available in 10_5 };
  NSEventTypeRotate = 18 { available in 10_5 };
  NSEventTypeBeginGesture = 19 { available in 10_5 };
  NSEventTypeEndGesture = 20 { available in 10_5 };
  {$if defined(__LP64__)}
  NSEventTypeSmartMagnify = 32 { available in 10_8 };
  {$endif}
  NSEventTypeQuickLook = 33 { available in 10_8 };

type
  NSEventMask = culonglong;
  NSEventMaskPtr = ^NSEventMask;

const
  NSLeftMouseDownMask = 1 shl NSLeftMouseDown;
  NSLeftMouseUpMask = 1 shl NSLeftMouseUp;
  NSRightMouseDownMask = 1 shl NSRightMouseDown;
  NSRightMouseUpMask = 1 shl NSRightMouseUp;
  NSMouseMovedMask = 1 shl NSMouseMoved;
  NSLeftMouseDraggedMask = 1 shl NSLeftMouseDragged;
  NSRightMouseDraggedMask = 1 shl NSRightMouseDragged;
  NSMouseEnteredMask = 1 shl NSMouseEntered;
  NSMouseExitedMask = 1 shl NSMouseExited;
  NSKeyDownMask = 1 shl NSKeyDown;
  NSKeyUpMask = 1 shl NSKeyUp;
  NSFlagsChangedMask = 1 shl NSFlagsChanged;
  NSAppKitDefinedMask = 1 shl NSAppKitDefined;
  NSSystemDefinedMask = 1 shl NSSystemDefined;
  NSApplicationDefinedMask = 1 shl NSApplicationDefined;
  NSPeriodicMask = 1 shl NSPeriodic;
  NSCursorUpdateMask = 1 shl NSCursorUpdate;
  NSScrollWheelMask = 1 shl NSScrollWheel;
  NSTabletPointMask = 1 shl NSTabletPoint;
  NSTabletProximityMask = 1 shl NSTabletProximity;
  NSOtherMouseDownMask = 1 shl NSOtherMouseDown;
  NSOtherMouseUpMask = 1 shl NSOtherMouseUp;
  NSOtherMouseDraggedMask = 1 shl NSOtherMouseDragged;
  NSEventMaskGesture = 1 shl NSEventTypeGesture { available in 10_5 };
  NSEventMaskMagnify = 1 shl NSEventTypeMagnify { available in 10_5 };
  NSEventMaskSwipe = 1 shl NSEventTypeSwipe { available in 10_5 };
  NSEventMaskRotate = 1 shl NSEventTypeRotate { available in 10_5 };
  NSEventMaskBeginGesture = 1 shl NSEventTypeBeginGesture { available in 10_5 };
  NSEventMaskEndGesture = 1 shl NSEventTypeEndGesture { available in 10_5 };
  {$if defined(__LP64__)}
  NSEventMaskSmartMagnify = 1 shl NSEventTypeSmartMagnify { available in 10_8 };
  {$endif}
  NSAnyEventMask = NSUIntegerMax;
{$endif}

{$ifdef TYPES}
type
  NSEventModifierFlags = NSUInteger;
  NSEventModifierFlagsPtr = ^NSEventModifierFlags;

const
  NSAlphaShiftKeyMask = 1 shl 16;
  NSShiftKeyMask = 1 shl 17;
  NSControlKeyMask = 1 shl 18;
  NSAlternateKeyMask = 1 shl 19;
  NSCommandKeyMask = 1 shl 20;
  NSNumericPadKeyMask = 1 shl 21;
  NSHelpKeyMask = 1 shl 22;
  NSFunctionKeyMask = 1 shl 23;
  NSDeviceIndependentModifierFlagsMask = $ffff0000;

type
  NSPointingDeviceType = NSUInteger;
  NSPointingDeviceTypePtr = ^NSPointingDeviceType;

const
  NSUnknownPointingDevice = NX_TABLET_POINTER_UNKNOWN;
  NSPenPointingDevice = NX_TABLET_POINTER_PEN;
  NSCursorPointingDevice = NX_TABLET_POINTER_CURSOR;
  NSEraserPointingDevice = NX_TABLET_POINTER_ERASER;

type
  NSEventButtonMask = NSUInteger;
  NSEventButtonMaskPtr = ^NSEventButtonMask;

const
  NSPenTipMask = NX_TABLET_BUTTON_PENTIPMASK;
  NSPenLowerSideMask = NX_TABLET_BUTTON_PENLOWERSIDEMASK;
  NSPenUpperSideMask = NX_TABLET_BUTTON_PENUPPERSIDEMASK;

type
  NSEventPhase = NSUInteger;
  NSEventPhasePtr = ^NSEventPhase;

const
  NSEventPhaseNone = 0;
  NSEventPhaseBegan = $1 shl 0;
  NSEventPhaseStationary = $1 shl 1;
  NSEventPhaseChanged = $1 shl 2;
  NSEventPhaseEnded = $1 shl 3;
  NSEventPhaseCancelled = $1 shl 4;
  NSEventPhaseMayBegin = $1 shl 5;

type
  NSEventGestureAxis = NSInteger;
  NSEventGestureAxisPtr = ^NSEventGestureAxis;

const
  NSEventGestureAxisNone = 0;
  NSEventGestureAxisHorizontal = 1;
  NSEventGestureAxisVertical = 2;

type
  NSEventSwipeTrackingOptions = NSUInteger;
  NSEventSwipeTrackingOptionsPtr = ^NSEventSwipeTrackingOptions;

const
  NSEventSwipeTrackingLockDirection = $1 shl 0;
  NSEventSwipeTrackingClampGestureAmount = $1 shl 1;

type
  NSEventSubtype = cshort;
  NSEventSubtypePtr = ^NSEventSubtype;

const
  NSWindowExposedEventType = 0;
  NSApplicationActivatedEventType = 1;
  NSApplicationDeactivatedEventType = 2;
  NSWindowMovedEventType = 4;
  NSScreenChangedEventType = 8;
  NSAWTEventType = 16;
  NSPowerOffEventType = 1;
  NSMouseEventSubtype = NX_SUBTYPE_DEFAULT;
  NSTabletPointEventSubtype = NX_SUBTYPE_TABLET_POINT;
  NSTabletProximityEventSubtype = NX_SUBTYPE_TABLET_PROXIMITY;
  NSTouchEventSubtype = NX_SUBTYPE_MOUSE_TOUCH { available in 10_6 };
{$endif}

{$ifdef CLASSES}

type
  NSEvent = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _type: NSEventType;
    _location: NSPoint;
    _modifierFlags: cuint;
    _WSTimestamp: id;
    _timestamp: NSTimeInterval;
    _windowNumber: NSInteger;
    _window: NSWindow;
    _context: NSGraphicsContext;
    _data: record
      case longint of
        0: (
          mouse: record
            eventNumber: cint;
            clickCount: cint;
            pressure: single;
            {$if defined(__LP64__)}
            deltaX: CGFloat;
            deltaY: CGFloat;
            subtype: cint;
            buttonNumber: cshort;
            reserved1: cshort;
            reserved2:array[0..2] of cint;
            {$endif}
          end;
        );
        1: (
          key: record
            keys: NSString;
            unmodKeys: NSString;
            keyCode: cushort;
            isARepeat: ObjCBOOL;
            {$if defined(__LP64__)}
            eventFlags: cint;
            reserved:array[0..4] of cint;
            {$endif}
          end;
        );
        2: (
          tracking: record
            eventNumber_: cint;
            trackingNumber: NSInteger;
            userData: pointer;
            {$if defined(__LP64__)}
            reserved_:array[0..5] of cint;
            {$endif}
          end;
        );
        3: (
          scrollWheel: record
            deltaX_: CGFloat;
            deltaY_: CGFloat;
            deltaZ: CGFloat;
            {$if defined(__LP64__)}
            subtype_: cshort;
            reserved1_: cshort;
            reserved2_:array[0..5] of cint;
            {$endif}
          end;
        );
        4: (
          axisGesture: record
            deltaX_: CGFloat;
            deltaY_: CGFloat;
            deltaZ_: CGFloat;
            {$if defined(__LP64__)}
            reserved_:array[0..6] of cint;
            {$endif}
          end;
        );
        5: (
          miscGesture: record
            subtype_: cshort;
            gestureEnded: ObjCBOOL;
            reserved_: ObjCBOOL;
            value: cint;
            percentage: single;
            {$if defined(__LP64__)}
            reserved2_:array[0..6] of cint;
            {$endif}
          end;
        );
        6: (
          misc: record
            subtype_: cint;
            data1: NSInteger;
            data2: NSInteger;
            {$if defined(__LP64__)}
            reserved_:array[0..5] of cint;
            {$endif}
          end;
        );
        {$if defined(__LP64__)}
        7: (
          tabletPointData:array[0..13] of cint;
          tabletProximityData:array[0..13] of cint;
        );
        {$endif}
    end;
    _eventRef: pointer;
    {$if defined(__LP64__)}
    reserved1: pointer;
    reserved2: pointer;
    {$endif}
  public
    function type_: NSEventType; message 'type';
    function modifierFlags: NSEventModifierFlags; message 'modifierFlags';
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
    function hasPreciseScrollingDeltas: ObjCBOOL; message 'hasPreciseScrollingDeltas';
    function scrollingDeltaX: CGFloat; message 'scrollingDeltaX';
    function scrollingDeltaY: CGFloat; message 'scrollingDeltaY';
    function momentumPhase: NSEventPhase; message 'momentumPhase';
    function isDirectionInvertedFromDevice: ObjCBOOL; message 'isDirectionInvertedFromDevice';
    function characters: NSString; message 'characters';
    function charactersIgnoringModifiers: NSString; message 'charactersIgnoringModifiers';
    function isARepeat: ObjCBOOL; message 'isARepeat';
    function keyCode: cushort; message 'keyCode';
    function trackingNumber: NSInteger; message 'trackingNumber';
    function userData: pointer; message 'userData';
    function trackingArea: NSTrackingArea; message 'trackingArea';
    function subtype: NSEventSubtype; message 'subtype';
    function data1: NSInteger; message 'data1';
    function data2: NSInteger; message 'data2';
    function eventRef: pointer; message 'eventRef';
    class function eventWithEventRef (eventRef_: pointer): NSEvent; message 'eventWithEventRef:'; { available in 10_5 }
    function CGEvent: CGEventRef; message 'CGEvent';
    class function eventWithCGEvent (cgEvent_: CGEventRef): NSEvent; message 'eventWithCGEvent:'; { available in 10_5 }
    class procedure setMouseCoalescingEnabled (flag: ObjCBOOL); message 'setMouseCoalescingEnabled:'; { available in 10_5 }
    class function isMouseCoalescingEnabled: ObjCBOOL; message 'isMouseCoalescingEnabled'; { available in 10_5 }
    function magnification: CGFloat; message 'magnification';
    function deviceID: NSUInteger; message 'deviceID';
    function rotation: single; message 'rotation';
    function absoluteX: NSInteger; message 'absoluteX';
    function absoluteY: NSInteger; message 'absoluteY';
    function absoluteZ: NSInteger; message 'absoluteZ';
    function buttonMask: NSEventButtonMask; message 'buttonMask';
    function tilt: NSPoint; message 'tilt';
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
    function isEnteringProximity: ObjCBOOL; message 'isEnteringProximity';
    function touchesMatchingPhase_inView (phase: NSTouchPhase; view: NSView): NSSet; message 'touchesMatchingPhase:inView:'; { available in 10_6 }
    function phase: NSEventPhase; message 'phase';
    class function isSwipeTrackingFromScrollEventsEnabled: ObjCBOOL; message 'isSwipeTrackingFromScrollEventsEnabled'; { available in 10_7 }
    procedure trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandler (options: NSEventSwipeTrackingOptions; minDampenThreshold: CGFloat; maxDampenThreshold: CGFloat; trackingHandler: OpaqueCBlock); message 'trackSwipeEventWithOptions:dampenAmountThresholdMin:max:usingHandler:'; { available in 10_7 }
    class procedure startPeriodicEventsAfterDelay_withPeriod (delay: NSTimeInterval; period: NSTimeInterval); message 'startPeriodicEventsAfterDelay:withPeriod:';
    class procedure stopPeriodicEvents; message 'stopPeriodicEvents';
    class function mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure (type_: NSEventType; location: NSPoint; flags: NSEventModifierFlags; time: NSTimeInterval; wNum: NSInteger; context_: NSGraphicsContext; eNum: NSInteger; cNum: NSInteger; pressure_: single): NSEvent; message 'mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:';
    class function keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCode (type_: NSEventType; location: NSPoint; flags: NSEventModifierFlags; time: NSTimeInterval; wNum: NSInteger; context_: NSGraphicsContext; keys: NSString; ukeys: NSString; flag: ObjCBOOL; code: cushort): NSEvent; message 'keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:';
    class function enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData (type_: NSEventType; location: NSPoint; flags: NSEventModifierFlags; time: NSTimeInterval; wNum: NSInteger; context_: NSGraphicsContext; eNum: NSInteger; tNum: NSInteger; data: pointer): NSEvent; message 'enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:';
    class function otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2 (type_: NSEventType; location: NSPoint; flags: NSEventModifierFlags; time: NSTimeInterval; wNum: NSInteger; context_: NSGraphicsContext; subtype_: cshort; d1: NSInteger; d2: NSInteger): NSEvent; message 'otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:';
    class function mouseLocation: NSPoint; message 'mouseLocation';
    class function classModifierFlags: NSEventModifierFlags; message 'modifierFlags'; { available in 10_6 }
    class function pressedMouseButtons: NSUInteger; message 'pressedMouseButtons'; { available in 10_6 }
    class function doubleClickInterval: NSTimeInterval; message 'doubleClickInterval'; { available in 10_6 }
    class function keyRepeatDelay: NSTimeInterval; message 'keyRepeatDelay'; { available in 10_6 }
    class function keyRepeatInterval: NSTimeInterval; message 'keyRepeatInterval'; { available in 10_6 }
    class function addGlobalMonitorForEventsMatchingMask_handler (mask: NSEventMask; block: OpaqueCBlock): id; message 'addGlobalMonitorForEventsMatchingMask:handler:'; { available in 10_6 }
    class function addLocalMonitorForEventsMatchingMask_handler (mask: NSEventMask; block: OpaqueCBlock): id; message 'addLocalMonitorForEventsMatchingMask:handler:'; { available in 10_6 }
    class procedure removeMonitor (eventMonitor: id); message 'removeMonitor:'; { available in 10_6 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef TYPES}
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
{$endif}

