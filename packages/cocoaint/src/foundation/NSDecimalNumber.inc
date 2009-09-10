{ Parsed from Foundation.framework NSDecimalNumber.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDECIMALNUMBER_PAS_H}
{$define NSDECIMALNUMBER_PAS_H}
type
  NSDecimalNumberPointer = Pointer;
  NSDecimalNumberHandlerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSDECIMALNUMBER_PAS_T}
{$define NSDECIMALNUMBER_PAS_T}

{ CFString constants }
var
  NSDecimalNumberExactnessException: CFStringRef; external name '_NSDecimalNumberExactnessException';
  NSDecimalNumberOverflowException: CFStringRef; external name '_NSDecimalNumberOverflowException';
  NSDecimalNumberUnderflowException: CFStringRef; external name '_NSDecimalNumberUnderflowException';
  NSDecimalNumberDivideByZeroException: CFStringRef; external name '_NSDecimalNumberDivideByZeroException';

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
{$ifndef NSDECIMALNUMBER_PAS_T}
{$define NSDECIMALNUMBER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDecimalNumberBehaviorsProtocol = objcprotocol;
  NSDecimalNumber = objcclass;
  NSDecimalNumberHandler = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDECIMALNUMBER_PAS_C}
{$define NSDECIMALNUMBER_PAS_C}

{ NSDecimalNumber }
  NSDecimalNumber = objcclass(NSNumber)
  private
    __exponent: 0..((1 shl 8)-1);
    __length: 0..((1 shl 4)-1);
    __isNegative: 0..1;
    __isCompact: 0..1;
    __reserved: 0..1;
    __hasExternalRefCount: 0..1;
    __refs: 0..((1 shl 16)-1);
    __mantissa: cushort;
    
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
    function decimalNumberByRaisingToPower(power: culong): NSDecimalNumber; message 'decimalNumberByRaisingToPower:';
    function decimalNumberByRaisingToPower_withBehavior(power: culong; behavior: id): NSDecimalNumber; message 'decimalNumberByRaisingToPower:withBehavior:';
    function decimalNumberByMultiplyingByPowerOf10(power: cshort): NSDecimalNumber; message 'decimalNumberByMultiplyingByPowerOf10:';
    function decimalNumberByMultiplyingByPowerOf10_withBehavior(power: cshort; behavior: id): NSDecimalNumber; message 'decimalNumberByMultiplyingByPowerOf10:withBehavior:';
    function decimalNumberByRoundingAccordingToBehavior(behavior: id): NSDecimalNumber; message 'decimalNumberByRoundingAccordingToBehavior:';
    function compare(decimalNumber: NSNumber): NSComparisonResult; message 'compare:';
    class procedure setDefaultBehavior(behavior: id); message 'setDefaultBehavior:';
    class function defaultBehavior: id; message 'defaultBehavior';
    function objCType: char; message 'objCType';
    function doubleValue: double; message 'doubleValue';
  end; external;

{ NSDecimalNumberHandler }
  NSDecimalNumberHandler = objcclass(NSObject, NSDecimalNumberBehaviorsProtocol, NSCodingProtocol)
  private
    __scale: 0..((1 shl 16)-1);
    __roundingMode: 0..((1 shl 3)-1);
    __raiseOnExactness: 0..1;
    __raiseOnOverflow: 0..1;
    __raiseOnUnderflow: 0..1;
    __raiseOnDivideByZero: 0..1;
    __unused: 0..((1 shl 9)-1);
    __reserved2: Pointer;
    __reserved: Pointer;
    
  public
    class function alloc: NSDecimalNumberHandler; message 'alloc';

    class function defaultDecimalNumberHandler: id; message 'defaultDecimalNumberHandler';
    function initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero(roundingMode: NSRoundingMode; scale: cshort; exact: Boolean; overflow: Boolean; underflow: Boolean; divideByZero: Boolean): id; message 'initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:';
    class function decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero(roundingMode: NSRoundingMode; scale: cshort; exact: Boolean; overflow: Boolean; underflow: Boolean; divideByZero: Boolean): id; message 'decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:';
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
