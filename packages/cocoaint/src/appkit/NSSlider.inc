{ Parsed from AppKit.framework NSSlider.h }


{$ifdef TYPES}
type
  NSSliderPtr = ^NSSlider;
{$endif}

{$ifdef CLASSES}

type
  NSSlider = objcclass external (NSControl, NSAccessibilitySliderProtocol)
  public
    procedure setSliderType(newValue: NSSliderType); message 'setSliderType:';
    function sliderType: NSSliderType; message 'sliderType';
    procedure setMinValue(newValue: double); message 'setMinValue:';
    function minValue: double; message 'minValue';
    procedure setMaxValue(newValue: double); message 'setMaxValue:';
    function maxValue: double; message 'maxValue';
    procedure setAltIncrementValue(newValue: double); message 'setAltIncrementValue:';
    function altIncrementValue: double; message 'altIncrementValue';
    function knobThickness: CGFloat; message 'knobThickness';
    function isVertical: NSInteger; message 'isVertical';
    function acceptsFirstMouse (theEvent: NSEvent): ObjCBOOL; message 'acceptsFirstMouse:';
    procedure setTitleCell (aCell: NSCell); message 'setTitleCell:'; deprecated 'in 10_0, 10_9';
    function titleCell: id; message 'titleCell'; deprecated 'in 10_0, 10_9';
    procedure setTitleColor (newColor: NSColor); message 'setTitleColor:'; deprecated 'in 10_0, 10_9';
    function titleColor: NSColor; message 'titleColor'; deprecated 'in 10_0, 10_9';
    procedure setTitleFont (fontObj: NSFont); message 'setTitleFont:'; deprecated 'in 10_0, 10_9';
    function titleFont: NSFont; message 'titleFont'; deprecated 'in 10_0, 10_9';
    function title: NSString; message 'title'; deprecated 'in 10_0, 10_9';
    procedure setTitle (aString: NSString); message 'setTitle:'; deprecated 'in 10_0, 10_9';
    procedure setKnobThickness (aFloat: CGFloat); message 'setKnobThickness:'; deprecated 'in 10_0, 10_9';
    procedure setImage (backgroundImage: NSImage); message 'setImage:'; deprecated 'in 10_0, 10_9';
    function image: NSImage; message 'image'; deprecated 'in 10_0, 10_9';
  end;


type
  NSTickMarkSupport = objccategory external (NSSlider)
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
  end;
{$endif}

