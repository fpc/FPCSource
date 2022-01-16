{ Parsed from Foundation.framework NSDecimal.h }

{$ifdef TYPES}
type
  NSRoundingMode = NSUInteger;
  NSRoundingModePtr = ^NSRoundingMode;

const
  NSRoundPlain = 0;
  NSRoundDown = 1;
  NSRoundUp = 2;
  NSRoundBankers = 3;

type
  NSCalculationError = NSUInteger;
  NSCalculationErrorPtr = ^NSCalculationError;

const
  NSCalculationNoError = 0;
  NSCalculationLossOfPrecision = 1;
  NSCalculationUnderflow = 2;
  NSCalculationOverflow = 3;
  NSCalculationDivideByZero = 4;

const
  NSDecimalMaxSize = 8;
  NSDecimalNoScale = SHRT_MAX;

type
  NSDecimal = bitpacked record
    case byte of
      0: (_anonBitField_NSDecimal0: cuint);
      1: (
        _exponent: 0..((1 shl 8)-1);
        _length: 0..((1 shl 4)-1);
        _isNegative: 0..1;
        _isCompact: 0..1;
        _reserved: 0..((1 shl 18)-1);
        _mantissa:array[0..(NSDecimalMaxSize)-1] of cushort;
      );
    end;
type
  NSDecimalPtr = ^NSDecimal;
{$endif}

{$ifdef FUNCTIONS}
procedure NSDecimalCopy(destination: NSDecimalPtr; source: NSDecimalPtr); cdecl; external;
procedure NSDecimalCompact(number: NSDecimalPtr); cdecl; external;
function NSDecimalCompare(leftOperand: NSDecimalPtr; rightOperand: NSDecimalPtr): NSComparisonResult; cdecl; external;
procedure NSDecimalRound(result_: NSDecimalPtr; number: NSDecimalPtr; scale: NSInteger; roundingMode: NSRoundingMode); cdecl; external;
function NSDecimalNormalize(number1: NSDecimalPtr; number2: NSDecimalPtr; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external;
function NSDecimalAdd(result_: NSDecimalPtr; leftOperand: NSDecimalPtr; rightOperand: NSDecimalPtr; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external;
function NSDecimalSubtract(result_: NSDecimalPtr; leftOperand: NSDecimalPtr; rightOperand: NSDecimalPtr; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external;
function NSDecimalMultiply(result_: NSDecimalPtr; leftOperand: NSDecimalPtr; rightOperand: NSDecimalPtr; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external;
function NSDecimalDivide(result_: NSDecimalPtr; leftOperand: NSDecimalPtr; rightOperand: NSDecimalPtr; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external;
function NSDecimalPower(result_: NSDecimalPtr; number: NSDecimalPtr; power: NSUInteger; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external;
function NSDecimalMultiplyByPowerOf10(result_: NSDecimalPtr; number: NSDecimalPtr; power: cshort; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external;
function NSDecimalString(dcm: NSDecimalPtr; locale: id): NSString; cdecl; external;
{$endif}

