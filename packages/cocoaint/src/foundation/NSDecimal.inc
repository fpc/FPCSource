{ Parsed from Foundation.framework NSDecimal.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSDECIMAL_PAS_T}
{$define NSDECIMAL_PAS_T}

{ Constants }

const
  NSRoundPlain = 0;
  NSRoundDown = 1;
  NSRoundUp = 2;
  NSRoundBankers = 3;

const
  NSCalculationNoError = 0;
  NSCalculationLossOfPrecision = 0;
  NSCalculationUnderflow = 1;
  NSCalculationOverflow = 2;
  NSCalculationDivideByZero = 3;

{ Types }
type
  NSRoundingMode = NSUInteger;
  NSCalculationError = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDECIMAL_PAS_R}
{$define NSDECIMAL_PAS_R}

{ Records }
type
  NSDecimal = record
    _exponent: 0..((1 shl 8)-1);
    _length: 0..((1 shl 4)-1);
    _isNegative: 0..1;
    _isCompact: 0..1;
    _reserved: 0..((1 shl 18)-1);
    _mantissa: cushort;
  end;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDECIMAL_PAS_F}
{$define NSDECIMAL_PAS_F}

{ Functions }
procedure NSDecimalCopy(destinationPointer: NSDecimal; sourcePointer: NSDecimal); cdecl; external name 'NSDecimalCopy';
procedure NSDecimalCompact(numberPointer: NSDecimal); cdecl; external name 'NSDecimalCompact';
function NSDecimalCompare(leftOperandPointer: NSDecimal; rightOperandPointer: NSDecimal): NSComparisonResult; cdecl; external name 'NSDecimalCompare';
procedure NSDecimalRound(resultPointer: NSDecimal; numberPointer: NSDecimal; scale: NSInteger; roundingMode: NSRoundingMode); cdecl; external name 'NSDecimalRound';
function NSDecimalNormalize(number1Pointer: NSDecimal; number2Pointer: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalNormalize';
function NSDecimalAdd(resultPointer: NSDecimal; leftOperandPointer: NSDecimal; rightOperandPointer: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalAdd';
function NSDecimalSubtract(resultPointer: NSDecimal; leftOperandPointer: NSDecimal; rightOperandPointer: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalSubtract';
function NSDecimalMultiply(resultPointer: NSDecimal; leftOperandPointer: NSDecimal; rightOperandPointer: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalMultiply';
function NSDecimalDivide(resultPointer: NSDecimal; leftOperandPointer: NSDecimal; rightOperandPointer: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalDivide';
function NSDecimalPower(resultPointer: NSDecimal; numberPointer: NSDecimal; power: NSUInteger; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalPower';
function NSDecimalMultiplyByPowerOf10(resultPointer: NSDecimal; numberPointer: NSDecimal; power: cshort; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalMultiplyByPowerOf10';
function NSDecimalString(dcmPointer: NSDecimal; locale: id): NSString; cdecl; external name 'NSDecimalString';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDECIMAL_PAS_S}
{$define NSDECIMAL_PAS_S}

{$endif}
{$endif}
