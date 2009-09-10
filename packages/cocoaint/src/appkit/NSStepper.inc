{ Parsed from Appkit.framework NSStepper.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSTEPPER_PAS_H}
{$define NSSTEPPER_PAS_H}
type
  NSStepperPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSSTEPPER_PAS_T}
{$define NSSTEPPER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStepper = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTEPPER_PAS_C}
{$define NSSTEPPER_PAS_C}

{ NSStepper }
  NSStepper = objcclass(NSControl)
  private
    __reserved1: cuint;
    __reserved2: cuint;
    __reserved3: cuint;
    __reserved4: cuint;
    
  public
    class function alloc: NSStepper; message 'alloc';

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
  end; external;

{$endif}
{$endif}
