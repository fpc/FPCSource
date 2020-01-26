{ Parsed from AppKit.framework NSScroller.h }


{$ifdef TYPES}
type
  NSScrollerPtr = ^NSScroller;
{$endif}

{$ifdef TYPES}
type
  NSScrollArrowPosition = NSUInteger;
  NSScrollArrowPositionPtr = ^NSScrollArrowPosition;

const
  NSScrollerArrowsMaxEnd = 0;
  NSScrollerArrowsMinEnd = 1;
  NSScrollerArrowsDefaultSetting = 0;
  NSScrollerArrowsNone = 2;

type
  NSUsableScrollerParts = NSUInteger;
  NSUsableScrollerPartsPtr = ^NSUsableScrollerParts;

const
  NSNoScrollerParts = 0;
  NSOnlyScrollerArrows = 1;
  NSAllScrollerParts = 2;

type
  NSScrollerPart = NSUInteger;
  NSScrollerPartPtr = ^NSScrollerPart;

const
  NSScrollerNoPart = 0;
  NSScrollerDecrementPage = 1;
  NSScrollerKnob = 2;
  NSScrollerIncrementPage = 3;
  NSScrollerDecrementLine = 4;
  NSScrollerIncrementLine = 5;
  NSScrollerKnobSlot = 6;

type
  NSScrollerArrow = NSUInteger;
  NSScrollerArrowPtr = ^NSScrollerArrow;

const
  NSScrollerIncrementArrow = 0;
  NSScrollerDecrementArrow = 1;

type
  NSScrollerStyle = NSInteger;
  NSScrollerStylePtr = ^NSScrollerStyle;

const
  NSScrollerStyleLegacy = 0;
  NSScrollerStyleOverlay = 1;

type
  NSScrollerKnobStyle = NSInteger;
  NSScrollerKnobStylePtr = ^NSScrollerKnobStyle;

const
  NSScrollerKnobStyleDefault = 0;
  NSScrollerKnobStyleDark = 1;
  NSScrollerKnobStyleLight = 2;
{$endif}

{$ifdef CLASSES}

type
  NSScroller = objcclass external (NSControl)
  private
    _curValue: CGFloat;
    _percent: CGFloat;
    _knobSize: CGFloat;
    __sFlags2: bitpacked record
      case byte of
        0: (_anonBitField___sFlags20: cuint);
        1: (
          hitPart: 0..((1 shl 4)-1);
          controlSize: 0..((1 shl 2)-1);
          inMaxEnd: 0..1;
          setFloatValueOverridden: 0..1;
          setFloatValueKnobProportionOverridden: 0..1;
          style: 0..1;
          styleCompatibility: 0..1;
          overlayScrollerState: 0..((1 shl 2)-1);
          knobStyle: 0..((1 shl 2)-1);
          sbPaused: 0..1;
          isAnimatingKnob: 0..1;
          isTrackingMouse: 0..1;
          reserved: 0..((1 shl 14)-1);
        );
      end;
    _reserved: id;
    _reserved2: SEL;
    _sFlags: bitpacked record
      case byte of
        0: (_anonBitField__sFlags0: cuint);
        1: (
          isHoriz: 0..1;
          arrowsLoc: 0..((1 shl 2)-1);
          partsUsable: 0..((1 shl 2)-1);
          fine: 0..1;
          needsEnableFlush: 0..1;
          thumbing: 0..1;
          slotDrawn: 0..1;
          knobDrawn: 0..1;
          lit: 0..1;
          knobLit: 0..1;
          reserved: 0..1;
          controlTint: 0..((1 shl 3)-1);
          repeatCount: 0..((1 shl 16)-1);
        );
      end;
  public
    class function isCompatibleWithOverlayScrollers: ObjCBOOL; message 'isCompatibleWithOverlayScrollers'; { available in 10_7 }
    class function scrollerWidthForControlSize_scrollerStyle (controlSize_: NSControlSize; scrollerStyle: NSScrollerStyle): CGFloat; message 'scrollerWidthForControlSize:scrollerStyle:'; { available in 10_7 }
    class function scrollerWidthForControlSize (controlSize_: NSControlSize): CGFloat; message 'scrollerWidthForControlSize:'; deprecated 'in 10_0, 10_7, "Use +scrollerWidthForControlSize:scrollerStyle: instead"';
    class function scrollerWidth: CGFloat; message 'scrollerWidth'; deprecated 'in 10_0, 10_7, "Use +scrollerWidthForControlSize:scrollerStyle: instead"';
    class function preferredScrollerStyle: NSScrollerStyle; message 'preferredScrollerStyle'; { available in 10_7 }
    procedure setScrollerStyle(newValue: NSScrollerStyle); message 'setScrollerStyle:';
    function scrollerStyle: NSScrollerStyle; message 'scrollerStyle';
    procedure setKnobStyle(newValue: NSScrollerKnobStyle); message 'setKnobStyle:';
    function knobStyle: NSScrollerKnobStyle; message 'knobStyle';
    procedure drawParts; message 'drawParts'; deprecated 'in 10_0, 10_7';
    function rectForPart (partCode: NSScrollerPart): NSRect; message 'rectForPart:';
    procedure checkSpaceForParts; message 'checkSpaceForParts';
    function usableParts: NSUsableScrollerParts; message 'usableParts';
    procedure setArrowsPosition(newValue: NSScrollArrowPosition); message 'setArrowsPosition:';
    function arrowsPosition: NSScrollArrowPosition; message 'arrowsPosition';
    procedure setControlTint(newValue: NSControlTint); message 'setControlTint:';
    function controlTint: NSControlTint; message 'controlTint';
    procedure setControlSize(newValue: NSControlSize); message 'setControlSize:';
    function controlSize: NSControlSize; message 'controlSize';
    procedure drawArrow_highlight (whichArrow: NSScrollerArrow; flag: ObjCBOOL); message 'drawArrow:highlight:';
    procedure drawKnob; message 'drawKnob';
    procedure drawKnobSlotInRect_highlight (slotRect: NSRect; flag: ObjCBOOL); message 'drawKnobSlotInRect:highlight:';
    procedure highlight (flag: ObjCBOOL); message 'highlight:';
    function testPart (thePoint: NSPoint): NSScrollerPart; message 'testPart:';
    procedure trackKnob (theEvent: NSEvent); message 'trackKnob:';
    procedure trackScrollButtons (theEvent: NSEvent); message 'trackScrollButtons:';
    function hitPart: NSScrollerPart; message 'hitPart';
    procedure setKnobProportion(newValue: CGFloat); message 'setKnobProportion:';
    function knobProportion: CGFloat; message 'knobProportion';
  end;


type
  NSScroller_NSDeprecated = objccategory external name 'NSDeprecated' (NSScroller)
    procedure setFloatValue_knobProportion (aFloat: single; proportion: CGFloat); message 'setFloatValue:knobProportion:'; deprecated 'in 10_0, 10_5';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPreferredScrollerStyleDidChangeNotification: NSString { available in 10_7 }; cvar; external;
{$endif}

