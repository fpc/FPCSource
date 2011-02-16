{ Parsed from Appkit.framework NSStepper.h }

{$ifdef TYPES}
{$ifndef NSSTEPPER_PAS_T}
{$define NSSTEPPER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSTEPPER_PAS_R}
{$define NSSTEPPER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSTEPPER_PAS_F}
{$define NSSTEPPER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSTEPPER_PAS_S}
{$define NSSTEPPER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStepper = objcclass;
  NSStepperPointer = ^NSStepper;
  NSStepperPtr = NSStepperPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTEPPER_PAS_C}
{$define NSSTEPPER_PAS_C}

{ NSStepper }
  NSStepper = objcclass external (NSControl)
  private
    _reserved1: cuint;
    _reserved2: cuint;
    _reserved3: cuint;
    _reserved4: cuint;
    
  public
    function minValue: double; message 'minValue';
    procedure setMinValue(minValue_: double); message 'setMinValue:';
    function maxValue: double; message 'maxValue';
    procedure setMaxValue(maxValue_: double); message 'setMaxValue:';
    function increment: double; message 'increment';
    procedure setIncrement(increment_: double); message 'setIncrement:';
    function valueWraps: Boolean; message 'valueWraps';
    procedure setValueWraps(valueWraps_: Boolean); message 'setValueWraps:';
    function autorepeat: Boolean; message 'autorepeat';
    procedure setAutorepeat(autorepeat_: Boolean); message 'setAutorepeat:';
  end;

{$endif}
{$endif}
