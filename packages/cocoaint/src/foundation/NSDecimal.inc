{ Parsed from Foundation.framework NSDecimal.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSDECIMAL_PAS_T}
{$define NSDECIMAL_PAS_T}

{ Types }
type
  NSRoundingMode = culong;
  NSCalculationError = culong;

{ Constants }

const
  NSCalculationNoError = 0;
  NSCalculationDivideByZero = 0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDECIMAL_PAS_R}
{$define NSDECIMAL_PAS_R}

{ Records }
type
  NSDecimal = record
    _exponent: cint;
    _length: cuint;
    _isNegative: cuint;
    _isCompact: cuint;
    _reserved: cuint;
    _mantissa: cushort;
  end;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDECIMAL_PAS_F}
{$define NSDECIMAL_PAS_F}

{ Functions }
procedure NSDecimalCopy(var destination: NSDecimal; var source: NSDecimal); cdecl; external name 'NSDecimalCopy';
procedure NSDecimalCompact(var number: NSDecimal); cdecl; external name 'NSDecimalCompact';
function NSDecimalCompare(var leftOperand: NSDecimal; var rightOperand: NSDecimal): NSComparisonResult; cdecl; external name 'NSDecimalCompare';
procedure NSDecimalRound(var result: NSDecimal; var number: NSDecimal; scale: clong; roundingMode: NSRoundingMode); cdecl; external name 'NSDecimalRound';
function NSDecimalNormalize(var number1: NSDecimal; var number2: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalNormalize';
function NSDecimalAdd(var result: NSDecimal; var leftOperand: NSDecimal; var rightOperand: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalAdd';
function NSDecimalSubtract(var result: NSDecimal; var leftOperand: NSDecimal; var rightOperand: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalSubtract';
function NSDecimalMultiply(var result: NSDecimal; var leftOperand: NSDecimal; var rightOperand: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalMultiply';
function NSDecimalDivide(var result: NSDecimal; var leftOperand: NSDecimal; var rightOperand: NSDecimal; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalDivide';
function NSDecimalPower(var result: NSDecimal; var number: NSDecimal; power: culong; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalPower';
function NSDecimalMultiplyByPowerOf10(var result: NSDecimal; var number: NSDecimal; power: cshort; roundingMode: NSRoundingMode): NSCalculationError; cdecl; external name 'NSDecimalMultiplyByPowerOf10';
function NSDecimalString(var dcm: NSDecimal; locale: id): NSString; cdecl; external name 'NSDecimalString';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDECIMAL_PAS_T}
{$define NSDECIMAL_PAS_T}

{$endif}
{$endif}
