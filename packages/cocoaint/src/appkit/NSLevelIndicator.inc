{ Parsed from Appkit.framework NSLevelIndicator.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:48 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSLEVELINDICATOR_PAS_H}
{$define NSLEVELINDICATOR_PAS_H}
type
  NSLevelIndicatorPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSLEVELINDICATOR_PAS_T}
{$define NSLEVELINDICATOR_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSLevelIndicator = objcclass;

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
