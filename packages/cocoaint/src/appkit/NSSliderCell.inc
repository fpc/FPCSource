{ Parsed from AppKit.framework NSSliderCell.h }


{$ifdef TYPES}
type
  NSSliderCellPtr = ^NSSliderCell;
{$endif}

{$ifdef TYPES}
type
  NSTickMarkPosition = NSUInteger;
  NSTickMarkPositionPtr = ^NSTickMarkPosition;

const
  NSTickMarkBelow = 0;
  NSTickMarkAbove = 1;
  NSTickMarkLeft = NSTickMarkAbove;
  NSTickMarkRight = NSTickMarkBelow;

type
  NSSliderType = NSUInteger;
  NSSliderTypePtr = ^NSSliderType;

const
  NSLinearSlider = 0;
  NSCircularSlider = 1;
{$endif}

{$ifdef CLASSES}

type
  NSSliderCell = objcclass external (NSActionCell)
  private
    _sliderCellPrivateData: id;
    _previousValue: CGFloat;
    _knobOffset: CGFloat;
    _numberOfTickMarks: cint;
    _altIncValue: double;
    _value: double;
    _maxValue: double;
    _minValue: double;
    _trackRect: NSRect;
    _scFlags: bitpacked record
      case byte of
        0: (_anonBitField___scFlags0: cuint);
        1: (
          weAreVertical: 0..1;
          weAreVerticalSet: 0..1;
          reserved1: 0..1;
          isPressed: 0..1;
          allowsTickMarkValuesOnly: 0..1;
          tickMarkPosition: 0..1;
          sliderType: 0..((1 shl 2)-1);
          drawing: 0..1;
          snappedToTickMark: 0..1;
          snappingAllowed: 0..1;
          reserved2: 0..((1 shl 21)-1);
        );
      end;
  public
    class function prefersTrackingUntilMouseUp: ObjCBOOL; message 'prefersTrackingUntilMouseUp';
    procedure setMinValue(newValue: double); message 'setMinValue:';
    function minValue: double; message 'minValue';
    procedure setMaxValue(newValue: double); message 'setMaxValue:';
    function maxValue: double; message 'maxValue';
    procedure setAltIncrementValue(newValue: double); message 'setAltIncrementValue:';
    function altIncrementValue: double; message 'altIncrementValue';
    procedure setSliderType(newValue: NSSliderType); message 'setSliderType:';
    function sliderType: NSSliderType; message 'sliderType';
    function isVertical: NSInteger; message 'isVertical';
    function trackRect: NSRect; message 'trackRect';
    function knobThickness: CGFloat; message 'knobThickness';
    function knobRectFlipped (flipped: ObjCBOOL): NSRect; message 'knobRectFlipped:';
    procedure drawKnob (knobRect: NSRect); message 'drawKnob:';
    procedure drawKnob_; message 'drawKnob';
    function barRectFlipped (flipped: ObjCBOOL): NSRect; message 'barRectFlipped:'; { available in 10_9 }
    procedure drawBarInside_flipped (aRect: NSRect; flipped: ObjCBOOL); message 'drawBarInside:flipped:';
    procedure setTitleColor (newColor: NSColor); message 'setTitleColor:'; deprecated 'in 10_0, 10_9';
    function titleColor: NSColor; message 'titleColor'; deprecated 'in 10_0, 10_9';
    procedure setTitleFont (fontObj: NSFont); message 'setTitleFont:'; deprecated 'in 10_0, 10_9';
    function titleFont: NSFont; message 'titleFont'; deprecated 'in 10_0, 10_9';
    function title_: NSString; message 'title'; deprecated 'in 10_0, 10_9';
    procedure setTitle (aString: NSString); message 'setTitle:'; deprecated 'in 10_0, 10_9';
    procedure setTitleCell (aCell: NSCell); message 'setTitleCell:'; deprecated 'in 10_0, 10_9';
    function titleCell: id; message 'titleCell'; deprecated 'in 10_0, 10_9';
    procedure setKnobThickness (aFloat: CGFloat); message 'setKnobThickness:'; deprecated 'in 10_0, 10_9';
  end;


type
  NSSliderCell_NSTickMarkSupport = objccategory external name 'NSTickMarkSupport' (NSSliderCell)
    procedure setNumberOfTickMarks(newValue: NSInteger); message 'setNumberOfTickMarks:';
    function numberOfTickMarks: NSInteger; message 'numberOfTickMarks';
    procedure setTickMarkPosition(newValue: NSTickMarkPosition); message 'setTickMarkPosition:';
    function tickMarkPosition: NSTickMarkPosition; message 'tickMarkPosition';
    procedure setAllowsTickMarkValuesOnly(newValue: ObjCBOOL); message 'setAllowsTickMarkValuesOnly:';
    function allowsTickMarkValuesOnly: ObjCBOOL; message 'allowsTickMarkValuesOnly';
    function tickMarkValueAtIndex (index: NSInteger): double; message 'tickMarkValueAtIndex:';
    function rectOfTickMarkAtIndex (index: NSInteger): NSRect; message 'rectOfTickMarkAtIndex:';
    function indexOfTickMarkAtPoint (point: NSPoint): NSInteger; message 'indexOfTickMarkAtPoint:';
    function closestTickMarkValueToValue (value: double): double; message 'closestTickMarkValueToValue:';
    procedure drawTickMarks; message 'drawTickMarks'; { available in 10_9 }
  end;
{$endif}

