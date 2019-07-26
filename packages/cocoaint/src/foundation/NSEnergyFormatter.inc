{ Parsed from Foundation.framework NSEnergyFormatter.h }


{$ifdef TYPES}
type
  NSEnergyFormatterPtr = ^NSEnergyFormatter;
{$endif}

{$ifdef TYPES}
type
  NSEnergyFormatterUnit = NSInteger;
  NSEnergyFormatterUnitPtr = ^NSEnergyFormatterUnit;

const
  NSEnergyFormatterUnitJoule = 11;
  NSEnergyFormatterUnitKilojoule = 14;
  NSEnergyFormatterUnitCalorie = 7 shl 8;
  NSEnergyFormatterUnitKilocalorie = 7 shl 8;
{$endif}

{$ifdef CLASSES}

type
  NSEnergyFormatter = objcclass external (NSFormatter)
  private
    _formatter: pointer;
    _isForFoodEnergyUse: ObjCBOOL;
    _reserved:array[0..1] of pointer;
  public
    procedure setNumberFormatter(newValue: NSNumberFormatter); message 'setNumberFormatter:';
    function numberFormatter: NSNumberFormatter; message 'numberFormatter';
    procedure setUnitStyle(newValue: NSFormattingUnitStyle); message 'setUnitStyle:';
    function unitStyle: NSFormattingUnitStyle; message 'unitStyle';
    procedure setForFoodEnergyUse(newValue: ObjCBOOL); message 'setForFoodEnergyUse:';
    function isForFoodEnergyUse: ObjCBOOL; message 'isForFoodEnergyUse';
    function stringFromValue_unit (value: double; unit_: NSEnergyFormatterUnit): NSString; message 'stringFromValue:unit:';
    function stringFromJoules (numberInJoules: double): NSString; message 'stringFromJoules:';
    function unitStringFromValue_unit (value: double; unit_: NSEnergyFormatterUnit): NSString; message 'unitStringFromValue:unit:';
    function unitStringFromJoules_usedUnit (numberInJoules: double; unitp: NSEnergyFormatterUnitPtr): NSString; message 'unitStringFromJoules:usedUnit:';
    function getObjectValue_forString_errorDescription (obj: idPtr; string_: NSString; error: NSStringPtr): ObjCBOOL; message 'getObjectValue:forString:errorDescription:';
  end;
{$endif}

