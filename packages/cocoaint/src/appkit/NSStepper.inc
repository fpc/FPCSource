{ Parsed from AppKit.framework NSStepper.h }


{$ifdef TYPES}
type
  NSStepperPtr = ^NSStepper;
{$endif}

{$ifdef CLASSES}

type
  NSStepper = objcclass external (NSControl, NSAccessibilityStepperProtocol)
  private
    _reserved1: cuint;
    _reserved2: cuint;
    _reserved3: cuint;
    _reserved4: cuint;
  public
    procedure setMinValue(newValue: double); message 'setMinValue:';
    function minValue: double; message 'minValue';
    procedure setMaxValue(newValue: double); message 'setMaxValue:';
    function maxValue: double; message 'maxValue';
    procedure setIncrement(newValue: double); message 'setIncrement:';
    function increment: double; message 'increment';
    procedure setValueWraps(newValue: ObjCBOOL); message 'setValueWraps:';
    function valueWraps: ObjCBOOL; message 'valueWraps';
    procedure setAutorepeat(newValue: ObjCBOOL); message 'setAutorepeat:';
    function autorepeat: ObjCBOOL; message 'autorepeat';
  end;
{$endif}

