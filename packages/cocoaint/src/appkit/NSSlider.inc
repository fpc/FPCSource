{ Parsed from Appkit.framework NSSlider.h }

{$ifdef TYPES}
{$ifndef NSSLIDER_PAS_T}
{$define NSSLIDER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSLIDER_PAS_R}
{$define NSSLIDER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSLIDER_PAS_F}
{$define NSSLIDER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSLIDER_PAS_S}
{$define NSSLIDER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSlider = objcclass;
  NSSliderPointer = ^NSSlider;
  NSSliderPtr = NSSliderPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSLIDER_PAS_C}
{$define NSSLIDER_PAS_C}

{ NSSlider }
  NSSlider = objcclass external (NSControl)
    
  public
    function minValue: double; message 'minValue';
    procedure setMinValue(aDouble: double); message 'setMinValue:';
    function maxValue: double; message 'maxValue';
    procedure setMaxValue(aDouble: double); message 'setMaxValue:';
    procedure setAltIncrementValue(incValue: double); message 'setAltIncrementValue:';
    function altIncrementValue: double; message 'altIncrementValue';
    procedure setTitleCell(aCell: NSCell); message 'setTitleCell:';
    function titleCell: id; message 'titleCell';
    procedure setTitleColor(newColor: NSColor); message 'setTitleColor:';
    function titleColor: NSColor; message 'titleColor';
    procedure setTitleFont(fontObj: NSFont); message 'setTitleFont:';
    function titleFont: NSFont; message 'titleFont';
    function title: NSString; message 'title';
    procedure setTitle(aString: NSString); message 'setTitle:';
    procedure setKnobThickness(aFloat: CGFloat); message 'setKnobThickness:';
    function knobThickness: CGFloat; message 'knobThickness';
    procedure setImage(backgroundImage: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    function isVertical: NSInteger; message 'isVertical';
    function acceptsFirstMouse(theEvent: NSEvent): Boolean; message 'acceptsFirstMouse:';
  end;

{ NSTickMarkSupport_NSSliderCategory }
  NSTickMarkSupport_NSSliderCategory = objccategory external name 'NSTickMarkSupport' (NSSlider)
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
  end;

{$endif}
{$endif}
