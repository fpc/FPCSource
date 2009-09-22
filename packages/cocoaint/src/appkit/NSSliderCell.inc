{ Parsed from Appkit.framework NSSliderCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSLIDERCELL_PAS_H}
{$define NSSLIDERCELL_PAS_H}
type
  NSSliderCellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSLIDERCELL_PAS_T}
{$define NSSLIDERCELL_PAS_T}

{ Constants }

const
  NSTickMarkBelow = 0;
  NSTickMarkAbove = 1;
  NSTickMarkLeft = NSTickMarkAbove;
  NSTickMarkRight = NSTickMarkBelow;

const
  NSLinearSlider = 0;
  NSCircularSlider = 1;

{ Types }
type
  NSTickMarkPosition = NSUInteger;
  NSSliderType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSLIDERCELL_PAS_R}
{$define NSSLIDERCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSLIDERCELL_PAS_F}
{$define NSSLIDERCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSLIDERCELL_PAS_T}
{$define NSSLIDERCELL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSliderCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSLIDERCELL_PAS_C}
{$define NSSLIDERCELL_PAS_C}

{ NSSliderCell }
  NSSliderCell = objcclass(NSActionCell)
  private
        _reserved: array[0..2] of cint;
    __numberOfTickMarks: cint;
    __altIncValue: double;
    __value: double;
    __maxValue: double;
    __minValue: double;
    __trackRect: NSRect;
    __scFlags: bitpacked record
        weAreVertical: 0..1;
        weAreVerticalSet: 0..1;
        reserved1: 0..1;
        isPressed: 0..1;
        allowsTickMarkValuesOnly: 0..1;
        tickMarkPosition: 0..1;
        sliderType: 0..((1 shl 2)-1);
        drawing: 0..1;
        reserved2: 0..((1 shl 23)-1);
      end;
    
  public
    class function alloc: NSSliderCell; message 'alloc';

    class function prefersTrackingUntilMouseUp: Boolean; message 'prefersTrackingUntilMouseUp';
    function minValue: double; message 'minValue';
    procedure setMinValue(aDouble: double); message 'setMinValue:';
    function maxValue: double; message 'maxValue';
    procedure setMaxValue(aDouble: double); message 'setMaxValue:';
    procedure setAltIncrementValue(incValue: double); message 'setAltIncrementValue:';
    function altIncrementValue: double; message 'altIncrementValue';
    function isVertical: NSInteger; message 'isVertical';
    procedure setTitleColor(newColor: NSColor); message 'setTitleColor:';
    function titleColor: NSColor; message 'titleColor';
    procedure setTitleFont(fontObj: NSFont); message 'setTitleFont:';
    function titleFont: NSFont; message 'titleFont';
    function title: NSString; message 'title';
    procedure setTitle(aString: NSString); message 'setTitle:';
    procedure setTitleCell(aCell: NSCell); message 'setTitleCell:';
    function titleCell: id; message 'titleCell';
    procedure setKnobThickness(aFloat: CGFloat); message 'setKnobThickness:';
    function knobThickness: CGFloat; message 'knobThickness';
    function knobRectFlipped(flipped: Boolean): NSRect; message 'knobRectFlipped:';
    procedure drawKnob(knobRect: NSRect); message 'drawKnob:';
    procedure drawBarInside_flipped(aRect: NSRect; flipped: Boolean); message 'drawBarInside:flipped:';
    function trackRect: NSRect; message 'trackRect';
    procedure setSliderType(sliderType_: NSSliderType); message 'setSliderType:';
    function sliderType: NSSliderType; message 'sliderType';

    { Category: NSTickMarkSupport }
    procedure setNumberOfTickMarks(count: NSInteger); message 'setNumberOfTickMarks:';
    function numberOfTickMarks: NSInteger; message 'numberOfTickMarks';
    procedure setTickMarkPosition(position: NSTickMarkPosition); message 'setTickMarkPosition:';
    function tickMarkPosition: NSTickMarkPosition; message 'tickMarkPosition';
    procedure setAllowsTickMarkValuesOnly(yorn: Boolean); message 'setAllowsTickMarkValuesOnly:';
    function allowsTickMarkValuesOnly: Boolean; message 'allowsTickMarkValuesOnly';
    function tickMarkValueAtIndex(index: NSInteger): double; message 'tickMarkValueAtIndex:';
    function rectOfTickMarkAtIndex(index: NSInteger): NSRect; message 'rectOfTickMarkAtIndex:';
    function indexOfTickMarkAtPoint(point: NSPoint): NSInteger; message 'indexOfTickMarkAtPoint:';
    function closestTickMarkValueToValue(value: double): double; message 'closestTickMarkValueToValue:';
  end; external;

{$endif}
{$endif}
