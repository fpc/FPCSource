{ Parsed from Appkit.framework NSStepperCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSTEPPERCELL_PAS_H}
{$define NSSTEPPERCELL_PAS_H}
type
  NSStepperCellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSTEPPERCELL_PAS_T}
{$define NSSTEPPERCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSTEPPERCELL_PAS_R}
{$define NSSTEPPERCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSTEPPERCELL_PAS_F}
{$define NSSTEPPERCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSTEPPERCELL_PAS_T}
{$define NSSTEPPERCELL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStepperCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTEPPERCELL_PAS_C}
{$define NSSTEPPERCELL_PAS_C}

{ NSStepperCell }
  NSStepperCell = objcclass(NSActionCell)
  private
    __value: double;
    __minValue: double;
    __maxValue: double;
    __increment: double;
    __stFlags: bitpacked record
        valueWraps: 0..1;
        autorepeat: 0..1;
        drawing: 0..1;
        reserved: 0..((1 shl 29)-1);
      end;
    __reserved1: cuint;
    __reserved2: cuint;
    __reserved3: cuint;
    __reserved4: cuint;
    
  public
    class function alloc: NSStepperCell; message 'alloc';

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
