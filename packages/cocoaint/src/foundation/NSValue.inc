{ Parsed from Foundation.framework NSValue.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSVALUE_PAS_H}
{$define NSVALUE_PAS_H}
type
  NSValuePointer = Pointer;
  NSNumberPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSVALUE_PAS_T}
{$define NSVALUE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSVALUE_PAS_R}
{$define NSVALUE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSVALUE_PAS_F}
{$define NSVALUE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSVALUE_PAS_S}
{$define NSVALUE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSValue = objcclass;
  NSNumber = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSVALUE_PAS_C}
{$define NSVALUE_PAS_C}

{ NSValue }
  NSValue = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSValue; message 'alloc';

    procedure getValue(value: Pointer); message 'getValue:';
    function objCType: PChar; message 'objCType';

    { Category: NSValueCreation }
    function initWithBytes_objCType(value: Pointer; type_: PChar): id; message 'initWithBytes:objCType:';
    class function valueWithBytes_objCType(value: Pointer; type_: PChar): NSValue; message 'valueWithBytes:objCType:';
    class function value_withObjCType(value: Pointer; type_: PChar): NSValue; message 'value:withObjCType:';

    { Category: NSValueExtensionMethods }
    class function valueWithNonretainedObject(anObject: id): NSValue; message 'valueWithNonretainedObject:';
    function nonretainedObjectValue: id; message 'nonretainedObjectValue';
    class function valueWithPointer(pointer_: Pointer): NSValue; message 'valueWithPointer:';
    function pointerValue: Pointer; message 'pointerValue';
    function isEqualToValue(value: NSValue): Boolean; message 'isEqualToValue:';
  end; external;

{ NSNumber }
  NSNumber = objcclass(NSValue)
    
  public
    class function alloc: NSNumber; message 'alloc';

    function charValue: char; message 'charValue';
    function unsignedCharValue: char; message 'unsignedCharValue';
    function shortValue: cshort; message 'shortValue';
    function unsignedShortValue: cushort; message 'unsignedShortValue';
    function intValue: cint; message 'intValue';
    function unsignedIntValue: cuint; message 'unsignedIntValue';
    function longValue: clong; message 'longValue';
    function unsignedLongValue: culong; message 'unsignedLongValue';
    function longLongValue: clonglong; message 'longLongValue';
    function unsignedLongLongValue: culonglong; message 'unsignedLongLongValue';
    function floatValue: single; message 'floatValue';
    function doubleValue: double; message 'doubleValue';
    function boolValue: Boolean; message 'boolValue';
    function integerValue: NSInteger; message 'integerValue';
    function unsignedIntegerValue: NSUInteger; message 'unsignedIntegerValue';
    function stringValue: NSString; message 'stringValue';
    function compare(otherNumber: NSNumber): NSComparisonResult; message 'compare:';
    function isEqualToNumber(number: NSNumber): Boolean; message 'isEqualToNumber:';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';

    { Category: NSNumberCreation }
    function initWithChar(value: char_): id; message 'initWithChar:';
    function initWithUnsignedChar(value: char_): id; message 'initWithUnsignedChar:';
    function initWithShort(value: cshort): id; message 'initWithShort:';
    function initWithUnsignedShort(value: cushort): id; message 'initWithUnsignedShort:';
    function initWithInt(value: cint): id; message 'initWithInt:';
    function initWithUnsignedInt(value: cuint): id; message 'initWithUnsignedInt:';
    function initWithLong(value: clong): id; message 'initWithLong:';
    function initWithUnsignedLong(value: culong): id; message 'initWithUnsignedLong:';
    function initWithLongLong(value: clonglong): id; message 'initWithLongLong:';
    function initWithUnsignedLongLong(value: culonglong): id; message 'initWithUnsignedLongLong:';
    function initWithFloat(value: single): id; message 'initWithFloat:';
    function initWithDouble(value: double): id; message 'initWithDouble:';
    function initWithBool(value: Boolean): id; message 'initWithBool:';
    function initWithInteger(value: NSInteger): id; message 'initWithInteger:';
    function initWithUnsignedInteger(value: NSUInteger): id; message 'initWithUnsignedInteger:';
    class function numberWithChar(value: char_): NSNumber; message 'numberWithChar:';
    class function numberWithUnsignedChar(value: char_): NSNumber; message 'numberWithUnsignedChar:';
    class function numberWithShort(value: cshort): NSNumber; message 'numberWithShort:';
    class function numberWithUnsignedShort(value: cushort): NSNumber; message 'numberWithUnsignedShort:';
    class function numberWithInt(value: cint): NSNumber; message 'numberWithInt:';
    class function numberWithUnsignedInt(value: cuint): NSNumber; message 'numberWithUnsignedInt:';
    class function numberWithLong(value: clong): NSNumber; message 'numberWithLong:';
    class function numberWithUnsignedLong(value: culong): NSNumber; message 'numberWithUnsignedLong:';
    class function numberWithLongLong(value: clonglong): NSNumber; message 'numberWithLongLong:';
    class function numberWithUnsignedLongLong(value: culonglong): NSNumber; message 'numberWithUnsignedLongLong:';
    class function numberWithFloat(value: single): NSNumber; message 'numberWithFloat:';
    class function numberWithDouble(value: double): NSNumber; message 'numberWithDouble:';
    class function numberWithBool(value: Boolean): NSNumber; message 'numberWithBool:';
    class function numberWithInteger(value: NSInteger): NSNumber; message 'numberWithInteger:';
    class function numberWithUnsignedInteger(value: NSUInteger): NSNumber; message 'numberWithUnsignedInteger:';

    { Category: NSDecimalNumberExtensions }
    function decimalValue: NSDecimal; message 'decimalValue';
  end; external;

{$endif}
{$endif}
