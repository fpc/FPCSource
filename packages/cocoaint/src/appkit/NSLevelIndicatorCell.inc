{ Parsed from AppKit.framework NSLevelIndicatorCell.h }


{$ifdef TYPES}
type
  NSLevelIndicatorCellPtr = ^NSLevelIndicatorCell;
{$endif}

{$ifdef TYPES}
type
  NSLevelIndicatorStyle = NSUInteger;
  NSLevelIndicatorStylePtr = ^NSLevelIndicatorStyle;

const
  NSRelevancyLevelIndicatorStyle = 0;
  NSContinuousCapacityLevelIndicatorStyle = 1;
  NSDiscreteCapacityLevelIndicatorStyle = 2;
  NSRatingLevelIndicatorStyle = 3;
{$endif}

{$ifdef CLASSES}

type
  NSLevelIndicatorCell = objcclass external (NSActionCell)
  private
    _value: double;
    _minValue: double;
    _maxValue: double;
    _warningValue: double;
    _criticalValue: double;
    _numberOfTickMarks: cint;
    _numberOfMajorTickMarks: cint;
    _liFlags: bitpacked record
      case byte of
        0: (_anonBitField__liFlags0: cuint);
        1: (
          indicatorStyle: 0..((1 shl 4)-1);
          tickMarkPosition: 0..1;
          selectable: 0..1;
          ratingPlaceholder: 0..1;
          reserved: 0..((1 shl 25)-1);
        );
      end;
    _cellFrame: NSRect;
    _reserved1: cint;
    _reserved2: cint;
    _reserved3: cint;
    _reserved4: cint;
  public
    function initWithLevelIndicatorStyle (levelIndicatorStyle: NSLevelIndicatorStyle): instancetype; message 'initWithLevelIndicatorStyle:';
    procedure setLevelIndicatorStyle(newValue: NSLevelIndicatorStyle); message 'setLevelIndicatorStyle:';
    function levelIndicatorStyle: NSLevelIndicatorStyle; message 'levelIndicatorStyle';
    procedure setMinValue(newValue: double); message 'setMinValue:';
    function minValue: double; message 'minValue';
    procedure setMaxValue(newValue: double); message 'setMaxValue:';
    function maxValue: double; message 'maxValue';
    procedure setWarningValue(newValue: double); message 'setWarningValue:';
    function warningValue: double; message 'warningValue';
    procedure setCriticalValue(newValue: double); message 'setCriticalValue:';
    function criticalValue: double; message 'criticalValue';
    procedure setTickMarkPosition(newValue: NSTickMarkPosition); message 'setTickMarkPosition:';
    function tickMarkPosition: NSTickMarkPosition; message 'tickMarkPosition';
    procedure setNumberOfTickMarks(newValue: NSInteger); message 'setNumberOfTickMarks:';
    function numberOfTickMarks: NSInteger; message 'numberOfTickMarks';
    procedure setNumberOfMajorTickMarks(newValue: NSInteger); message 'setNumberOfMajorTickMarks:';
    function numberOfMajorTickMarks: NSInteger; message 'numberOfMajorTickMarks';
    function rectOfTickMarkAtIndex (index: NSInteger): NSRect; message 'rectOfTickMarkAtIndex:';
    function tickMarkValueAtIndex (index: NSInteger): double; message 'tickMarkValueAtIndex:';
    procedure setImage (image_: NSImage); message 'setImage:';
  end;
{$endif}

