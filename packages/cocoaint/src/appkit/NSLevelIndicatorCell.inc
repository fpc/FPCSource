{ Parsed from Appkit.framework NSLevelIndicatorCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:08 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSLEVELINDICATORCELL_PAS_T}
{$define NSLEVELINDICATORCELL_PAS_T}

{ Constants }

const
  NSRelevancyLevelIndicatorStyle = 0;
  NSContinuousCapacityLevelIndicatorStyle = 1;
  NSDiscreteCapacityLevelIndicatorStyle = 2;
  NSRatingLevelIndicatorStyle = 3;

{ Types }
type
  NSLevelIndicatorStyle = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSLEVELINDICATORCELL_PAS_R}
{$define NSLEVELINDICATORCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSLEVELINDICATORCELL_PAS_F}
{$define NSLEVELINDICATORCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSLEVELINDICATORCELL_PAS_S}
{$define NSLEVELINDICATORCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSLevelIndicatorCell = objcclass;
  NSLevelIndicatorCellPointer = ^NSLevelIndicatorCell;

{$endif}

{$ifdef CLASSES}
{$ifndef NSLEVELINDICATORCELL_PAS_C}
{$define NSLEVELINDICATORCELL_PAS_C}

{ NSLevelIndicatorCell }
  NSLevelIndicatorCell = objcclass(NSActionCell)
  private
    __value: double;
    __minValue: double;
    __maxValue: double;
    __warningValue: double;
    __criticalValue: double;
    __numberOfTickMarks: cint;
    __numberOfMajorTickMarks: cint;
    __liFlags: bitpacked record
        indicatorStyle: 0..((1 shl 4)-1);
        tickMarkPosition: 0..1;
        selectable: 0..1;
        reserved: 0..((1 shl 26)-1);
      end;
    __cellFrame: NSRect;
    __reserved1: cint;
    __reserved2: cint;
    __reserved3: cint;
    __reserved4: cint;
    
  public
    class function alloc: NSLevelIndicatorCell; message 'alloc';

    function initWithLevelIndicatorStyle(levelIndicatorStyle_: NSLevelIndicatorStyle): id; message 'initWithLevelIndicatorStyle:';
    function levelIndicatorStyle: NSLevelIndicatorStyle; message 'levelIndicatorStyle';
    procedure setLevelIndicatorStyle(levelIndicatorStyle_: NSLevelIndicatorStyle); message 'setLevelIndicatorStyle:';
    function minValue: double; message 'minValue';
    procedure setMinValue(minValue_: double); message 'setMinValue:';
    function maxValue: double; message 'maxValue';
    procedure setMaxValue(maxValue_: double); message 'setMaxValue:';
    function warningValue: double; message 'warningValue';
    procedure setWarningValue(warningValue_: double); message 'setWarningValue:';
    function criticalValue: double; message 'criticalValue';
    procedure setCriticalValue(criticalValue_: double); message 'setCriticalValue:';
    procedure setTickMarkPosition(position: NSTickMarkPosition); message 'setTickMarkPosition:';
    function tickMarkPosition: NSTickMarkPosition; message 'tickMarkPosition';
    procedure setNumberOfTickMarks(count: NSInteger); message 'setNumberOfTickMarks:';
    function numberOfTickMarks: NSInteger; message 'numberOfTickMarks';
    procedure setNumberOfMajorTickMarks(count: NSInteger); message 'setNumberOfMajorTickMarks:';
    function numberOfMajorTickMarks: NSInteger; message 'numberOfMajorTickMarks';
    function rectOfTickMarkAtIndex(index: NSInteger): NSRect; message 'rectOfTickMarkAtIndex:';
    function tickMarkValueAtIndex(index: NSInteger): double; message 'tickMarkValueAtIndex:';
    procedure setImage(image_: NSImage); message 'setImage:';
  end; external;

{$endif}
{$endif}
