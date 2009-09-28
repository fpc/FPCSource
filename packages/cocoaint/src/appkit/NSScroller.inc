{ Parsed from Appkit.framework NSScroller.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCROLLER_PAS_H}
{$define NSSCROLLER_PAS_H}
type
  NSScrollerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSCROLLER_PAS_T}
{$define NSSCROLLER_PAS_T}

{ Constants }

const
  NSScrollerArrowsMaxEnd = 0;
  NSScrollerArrowsMinEnd = 1;
  NSScrollerArrowsDefaultSetting = 0;
  NSScrollerArrowsNone = 2;

const
  NSNoScrollerParts = 0;
  NSOnlyScrollerArrows = 1;
  NSAllScrollerParts = 2;

const
  NSScrollerNoPart = 0;
  NSScrollerDecrementPage = 1;
  NSScrollerKnob = 2;
  NSScrollerIncrementPage = 3;
  NSScrollerDecrementLine = 4;
  NSScrollerIncrementLine = 5;
  NSScrollerKnobSlot = 6;

const
  NSScrollerIncrementArrow = 0;
  NSScrollerDecrementArrow = 1;

{ Types }
type
  NSScrollArrowPosition = NSUInteger;
  NSUsableScrollerParts = NSUInteger;
  NSScrollerPart = NSUInteger;
  NSScrollerArrow = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCROLLER_PAS_R}
{$define NSSCROLLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCROLLER_PAS_F}
{$define NSSCROLLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCROLLER_PAS_S}
{$define NSSCROLLER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScroller = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCROLLER_PAS_C}
{$define NSSCROLLER_PAS_C}

{ NSScroller }
  NSScroller = objcclass(NSControl)
  private
    __curValue: CGFloat;
    __percent: CGFloat;
    __knobSize: CGFloat;
    __sFlags2: bitpacked record
        hitPart: 0..((1 shl 4)-1);
        controlSize: 0..((1 shl 2)-1);
        inMaxEnd: 0..1;
        setFloatValueOverridden: 0..1;
        setFloatValueKnobProportionOverridden: 0..1;
        reserved: 0..((1 shl 23)-1);
      end;
    __target: id;
    __action: SEL;
    _sFlags: bitpacked record
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
      end;
    
  public
    class function alloc: NSScroller; message 'alloc';

    class function scrollerWidth: CGFloat; message 'scrollerWidth';
    class function scrollerWidthForControlSize(controlSize_: NSControlSize): CGFloat; message 'scrollerWidthForControlSize:';
    procedure drawParts; message 'drawParts';
    function rectForPart(partCode: NSScrollerPart): NSRect; message 'rectForPart:';
    procedure checkSpaceForParts; message 'checkSpaceForParts';
    function usableParts: NSUsableScrollerParts; message 'usableParts';
    procedure setArrowsPosition(where: NSScrollArrowPosition); message 'setArrowsPosition:';
    function arrowsPosition: NSScrollArrowPosition; message 'arrowsPosition';
    procedure setControlTint(controlTint_: NSControlTint); message 'setControlTint:';
    function controlTint: NSControlTint; message 'controlTint';
    procedure setControlSize(controlSize_: NSControlSize); message 'setControlSize:';
    function controlSize: NSControlSize; message 'controlSize';
    procedure drawArrow_highlight(whichArrow: NSScrollerArrow; flag: Boolean); message 'drawArrow:highlight:';
    procedure drawKnob; message 'drawKnob';
    procedure drawKnobSlotInRect_highlight(slotRect: NSRect; flag: Boolean); message 'drawKnobSlotInRect:highlight:';
    procedure highlight(flag: Boolean); message 'highlight:';
    function testPart(thePoint: NSPoint): NSScrollerPart; message 'testPart:';
    procedure trackKnob(theEvent: NSEvent); message 'trackKnob:';
    procedure trackScrollButtons(theEvent: NSEvent); message 'trackScrollButtons:';
    function hitPart: NSScrollerPart; message 'hitPart';
    function knobProportion: CGFloat; message 'knobProportion';
    procedure setKnobProportion(proportion: CGFloat); message 'setKnobProportion:';

    { Category: NSDeprecated }
    procedure setFloatValue_knobProportion(aFloat: single; proportion: CGFloat); message 'setFloatValue:knobProportion:';
  end; external;

{$endif}
{$endif}
