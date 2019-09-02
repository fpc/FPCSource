{ Parsed from AppKit.framework NSLevelIndicator.h }


{$ifdef TYPES}
type
  NSLevelIndicatorPtr = ^NSLevelIndicator;
{$endif}

{$ifdef CLASSES}

type
  NSLevelIndicator = objcclass external (NSControl)
  private
  public
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
    function tickMarkValueAtIndex (index: NSInteger): double; message 'tickMarkValueAtIndex:';
    function rectOfTickMarkAtIndex (index: NSInteger): NSRect; message 'rectOfTickMarkAtIndex:';
  end;
{$endif}

