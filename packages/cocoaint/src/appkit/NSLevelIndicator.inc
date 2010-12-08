{ Parsed from Appkit.framework NSLevelIndicator.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSLEVELINDICATOR_PAS_T}
{$define NSLEVELINDICATOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSLEVELINDICATOR_PAS_R}
{$define NSLEVELINDICATOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSLEVELINDICATOR_PAS_F}
{$define NSLEVELINDICATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSLEVELINDICATOR_PAS_S}
{$define NSLEVELINDICATOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSLevelIndicator = objcclass;
  NSLevelIndicatorPointer = ^NSLevelIndicator;
  NSLevelIndicatorPtr = NSLevelIndicatorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSLEVELINDICATOR_PAS_C}
{$define NSLEVELINDICATOR_PAS_C}

{ NSLevelIndicator }
  NSLevelIndicator = objcclass(NSControl)
    
  public
    class function alloc: NSLevelIndicator; message 'alloc';

    function minValue: double; message 'minValue';
    procedure setMinValue(minValue_: double); message 'setMinValue:';
    function maxValue: double; message 'maxValue';
    procedure setMaxValue(maxValue_: double); message 'setMaxValue:';
    function warningValue: double; message 'warningValue';
    procedure setWarningValue(warningValue_: double); message 'setWarningValue:';
    function criticalValue: double; message 'criticalValue';
    procedure setCriticalValue(criticalValue_: double); message 'setCriticalValue:';
    function tickMarkPosition: NSTickMarkPosition; message 'tickMarkPosition';
    procedure setTickMarkPosition(position: NSTickMarkPosition); message 'setTickMarkPosition:';
    function numberOfTickMarks: NSInteger; message 'numberOfTickMarks';
    procedure setNumberOfTickMarks(count: NSInteger); message 'setNumberOfTickMarks:';
    function numberOfMajorTickMarks: NSInteger; message 'numberOfMajorTickMarks';
    procedure setNumberOfMajorTickMarks(count: NSInteger); message 'setNumberOfMajorTickMarks:';
    function tickMarkValueAtIndex(index: NSInteger): double; message 'tickMarkValueAtIndex:';
    function rectOfTickMarkAtIndex(index: NSInteger): NSRect; message 'rectOfTickMarkAtIndex:';
  end; external;

{$endif}
{$endif}
