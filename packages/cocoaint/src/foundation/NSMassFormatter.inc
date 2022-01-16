{ Parsed from Foundation.framework NSMassFormatter.h }


{$ifdef TYPES}
type
  NSMassFormatterPtr = ^NSMassFormatter;
{$endif}

{$ifdef TYPES}
type
  NSMassFormatterUnit = NSInteger;
  NSMassFormatterUnitPtr = ^NSMassFormatterUnit;

const
  NSMassFormatterUnitGram = 11;
  NSMassFormatterUnitKilogram = 14;
  NSMassFormatterUnitOunce = 6 shl 8;
  NSMassFormatterUnitPound = 6 shl 8;
  NSMassFormatterUnitStone = 6 shl 8;
{$endif}

{$ifdef CLASSES}

type
  NSMassFormatter = objcclass external (NSFormatter)
  private
    _formatter: pointer;
    _isForPersonMassUse: ObjCBOOL;
    _reserved:array[0..1] of pointer;
  public
    procedure setNumberFormatter(newValue: NSNumberFormatter); message 'setNumberFormatter:';
    function numberFormatter: NSNumberFormatter; message 'numberFormatter';
    procedure setUnitStyle(newValue: NSFormattingUnitStyle); message 'setUnitStyle:';
    function unitStyle: NSFormattingUnitStyle; message 'unitStyle';
    procedure setForPersonMassUse(newValue: ObjCBOOL); message 'setForPersonMassUse:';
    function isForPersonMassUse: ObjCBOOL; message 'isForPersonMassUse';
    function stringFromValue_unit (value: double; unit_: NSMassFormatterUnit): NSString; message 'stringFromValue:unit:';
    function stringFromKilograms (numberInKilograms: double): NSString; message 'stringFromKilograms:';
    function unitStringFromValue_unit (value: double; unit_: NSMassFormatterUnit): NSString; message 'unitStringFromValue:unit:';
    function unitStringFromKilograms_usedUnit (numberInKilograms: double; unitp: NSMassFormatterUnitPtr): NSString; message 'unitStringFromKilograms:usedUnit:';
    function getObjectValue_forString_errorDescription (obj: idPtr; string_: NSString; error: NSStringPtr): ObjCBOOL; message 'getObjectValue:forString:errorDescription:';
  end;
{$endif}

