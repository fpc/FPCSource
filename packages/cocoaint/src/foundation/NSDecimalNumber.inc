{ Parsed from Foundation.framework NSDecimalNumber.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSDECIMALNUMBER_PAS_T}
{$define NSDECIMALNUMBER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDECIMALNUMBER_PAS_R}
{$define NSDECIMALNUMBER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDECIMALNUMBER_PAS_F}
{$define NSDECIMALNUMBER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDECIMALNUMBER_PAS_S}
{$define NSDECIMALNUMBER_PAS_S}

{ External string constants }
var
  NSDecimalNumberExactnessException: NSString; cvar; external;
  NSDecimalNumberOverflowException: NSString; cvar; external;
  NSDecimalNumberUnderflowException: NSString; cvar; external;
  NSDecimalNumberDivideByZeroException: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDecimalNumberBehaviorsProtocol = objcprotocol;
  NSDecimalNumber = objcclass;
  NSDecimalNumberPointer = ^NSDecimalNumber;
  NSDecimalNumberPtr = NSDecimalNumberPointer;
  NSDecimalNumberHandler = objcclass;
  NSDecimalNumberHandlerPointer = ^NSDecimalNumberHandler;
  NSDecimalNumberHandlerPtr = NSDecimalNumberHandlerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDECIMALNUMBER_PAS_C}
{$define NSDECIMALNUMBER_PAS_C}

{ NSDecimalNumber }
  NSDecimalNumber = objcclass(NSNumber)
  private
    _anoninternstruct_NSDecimalNumber0: bitpacked record
      case byte of
       0: (_anonbitfield_NSDecimalNumber0: cint);
       1: (
        _exponent: 0..((1 shl 8)-1);
        _length: 0..((1 shl 4)-1);
        _isNegative: 0..1;
        _isCompact: 0..1;
        _reserved: 0..1;
        _hasExternalRefCount: 0..1;
        _refs: 0..((1 shl 16)-1);
      );
      end;
    _mantissa: cushort;
    
  public
    class function alloc: NSDecimalNumber; message 'alloc';

    function initWithMantissa_exponent_isNegative(mantissa: culonglong; exponent: cshort; flag: Boolean): id; message 'initWithMantissa:exponent:isNegative:';
    function initWithDecimal(dcm: NSDecimal): id; message 'initWithDecimal:';
    function initWithString(numberValue: NSString): id; message 'initWithString:';
    function initWithString_locale(numberValue: NSString; locale_: id): id; message 'initWithString:locale:';
    function descriptionWithLocale(locale_: id): NSString; message 'descriptionWithLocale:';
    function decimalValue: NSDecimal; message 'decimalValue';
    class function decimalNumberWithMantissa_exponent_isNegative(mantissa: culonglong; exponent: cshort; flag: Boolean): NSDecimalNumber; message 'decimalNumberWithMantissa:exponent:isNegative:';
    class function decimalNumberWithDecimal(dcm: NSDecimal): NSDecimalNumber; message 'decimalNumberWithDecimal:';
    class function decimalNumberWithString(numberValue: NSString): NSDecimalNumber; message 'decimalNumberWithString:';
    class function decimalNumberWithString_locale(numberValue: NSString; locale_: id): NSDecimalNumber; message 'decimalNumberWithString:locale:';
    class function zero: NSDecimalNumber; message 'zero';
    class function one: NSDecimalNumber; message 'one';
    class function minimumDecimalNumber: NSDecimalNumber; message 'minimumDecimalNumber';
    class function maximumDecimalNumber: NSDecimalNumber; message 'maximumDecimalNumber';
    class function notANumber: NSDecimalNumber; message 'notANumber';
    function decimalNumberByAdding(decimalNumber: NSDecimalNumber): NSDecimalNumber; message 'decimalNumberByAdding:';
    function decimalNumberByAdding_withBehavior(decimalNumber: NSDecimalNumber; behavior: id): NSDecimalNumber; message 'decimalNumberByAdding:withBehavior:';
    function decimalNumberBySubtracting(decimalNumber: NSDecimalNumber): NSDecimalNumber; message 'decimalNumberBySubtracting:';
    function decimalNumberBySubtracting_withBehavior(decimalNumber: NSDecimalNumber; behavior: id): NSDecimalNumber; message 'decimalNumberBySubtracting:withBehavior:';
    function decimalNumberByMultiplyingBy(decimalNumber: NSDecimalNumber): NSDecimalNumber; message 'decimalNumberByMultiplyingBy:';
    function decimalNumberByMultiplyingBy_withBehavior(decimalNumber: NSDecimalNumber; behavior: id): NSDecimalNumber; message 'decimalNumberByMultiplyingBy:withBehavior:';
    function decimalNumberByDividingBy(decimalNumber: NSDecimalNumber): NSDecimalNumber; message 'decimalNumberByDividingBy:';
    function decimalNumberByDividingBy_withBehavior(decimalNumber: NSDecimalNumber; behavior: id): NSDecimalNumber; message 'decimalNumberByDividingBy:withBehavior:';
    function decimalNumberByRaisingToPower(power: NSUInteger): NSDecimalNumber; message 'decimalNumberByRaisingToPower:';
    function decimalNumberByRaisingToPower_withBehavior(power: NSUInteger; behavior: id): NSDecimalNumber; message 'decimalNumberByRaisingToPower:withBehavior:';
    function decimalNumberByMultiplyingByPowerOf10(power: cshort): NSDecimalNumber; message 'decimalNumberByMultiplyingByPowerOf10:';
    function decimalNumberByMultiplyingByPowerOf10_withBehavior(power: cshort; behavior: id): NSDecimalNumber; message 'decimalNumberByMultiplyingByPowerOf10:withBehavior:';
    function decimalNumberByRoundingAccordingToBehavior(behavior: id): NSDecimalNumber; message 'decimalNumberByRoundingAccordingToBehavior:';
    function compare(decimalNumber: NSNumber): NSComparisonResult; message 'compare:';
    class procedure setDefaultBehavior(behavior: id); message 'setDefaultBehavior:';
    class function defaultBehavior: id; message 'defaultBehavior';
    function objCType: PChar; message 'objCType';
    function doubleValue: double; message 'doubleValue';
  end; external;

{ NSDecimalNumberHandler }
  NSDecimalNumberHandler = objcclass(NSObject, NSDecimalNumberBehaviorsProtocol, NSCodingProtocol)
  private
    _anoninternstruct_NSDecimalNumber1: bitpacked record
      case byte of
       0: (_anonbitfield_NSDecimalNumber1: cint);
       1: (
        _scale: 0..((1 shl 16)-1);
        _roundingMode: 0..((1 shl 3)-1);
        _raiseOnExactness: 0..1;
        _raiseOnOverflow: 0..1;
        _raiseOnUnderflow: 0..1;
        _raiseOnDivideByZero: 0..1;
        _unused: 0..((1 shl 9)-1);
      );
      end;
    _reserved2: Pointer;
    _reserved: Pointer;
    
  public
    class function alloc: NSDecimalNumberHandler; message 'alloc';

    class function defaultDecimalNumberHandler: id; message 'defaultDecimalNumberHandler';
    function initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero(roundingMode: NSRoundingMode; scale: cshort; exact: Boolean; overflow: Boolean; underflow: Boolean; divideByZero: Boolean): id; message 'initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:';
    class function decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero(roundingMode: NSRoundingMode; scale: cshort; exact: Boolean; overflow: Boolean; underflow: Boolean; divideByZero: Boolean): id; message 'decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:';

    { Adopted Protocols }
    function roundingMode: NSRoundingMode;
    function scale: cshort;
    function exceptionDuringOperation_error_leftOperand_rightOperand(operation: SEL; error: NSCalculationError; leftOperand: NSDecimalNumber; rightOperand: NSDecimalNumber): NSDecimalNumber;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{ NSDecimalNumberExtensionsCategory }
  NSDecimalNumberExtensionsCategory = objccategory(NSNumber)
    function decimalValue: NSDecimal; message 'decimalValue';
  end; external;

{ NSDecimalNumberScanningCategory }
  NSDecimalNumberScanningCategory = objccategory(NSScanner)
    function scanDecimal(dcm: NSDecimalPtr): Boolean; message 'scanDecimal:';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSDECIMALNUMBER_PAS_P}
{$define NSDECIMALNUMBER_PAS_P}
  
{ NSDecimalNumberBehaviors Protocol }
  NSDecimalNumberBehaviorsProtocol = objcprotocol
    function roundingMode: NSRoundingMode; message 'roundingMode';
    function scale: cshort; message 'scale';
    function exceptionDuringOperation_error_leftOperand_rightOperand(operation: SEL; error: NSCalculationError; leftOperand: NSDecimalNumber; rightOperand: NSDecimalNumber): NSDecimalNumber; message 'exceptionDuringOperation:error:leftOperand:rightOperand:';
  end; external name 'NSDecimalNumberBehaviors';
{$endif}
{$endif}
