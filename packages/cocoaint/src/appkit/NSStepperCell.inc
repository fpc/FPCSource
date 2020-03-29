{ Parsed from AppKit.framework NSStepperCell.h }


{$ifdef TYPES}
type
  NSStepperCellPtr = ^NSStepperCell;
{$endif}

{$ifdef CLASSES}

type
  NSStepperCell = objcclass external (NSActionCell)
  private
    _value: double;
    _minValue: double;
    _maxValue: double;
    _increment: double;
    _stFlags: bitpacked record
      case byte of
        0: (_anonBitField__stFlags0: cuint);
        1: (
          valueWraps: 0..1;
          autorepeat: 0..1;
          drawing: 0..1;
          stats1: 0..1;
          reserved: 0..((1 shl 28)-1);
        );
      end;
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

