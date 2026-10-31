{ Delphi TArithmeticException names, GetExceptions, SetExceptions, RaiseExceptions and ClearExceptions with flags, issue #34144 }
program tarithexcept;

{$mode objfpc}{$h+}

uses
  SysUtils, Math;

var
  gZero: Double = 0;
  gOne: Double = 1;
  gHuge: Double = 1e300;
  gResult: Double;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  gExtZero: Extended = 0;
  gExtResult: Extended;
{$endif FPC_HAS_TYPE_EXTENDED}


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


// True if aFlags contains exactly the exceptions of aExpected, ignoring exPrecision and exDenormalized
function SameFlags(aFlags, aExpected: TArithmeticExceptions): Boolean;

begin
  Result:=(aFlags-[exPrecision,exDenormalized])=(aExpected-[exPrecision,exDenormalized]);
end;


var
  lOldMask: TArithmeticExceptionMask;
  lFlags: TArithmeticExceptions;
  lValue: TArithmeticException;
  lRaised: Boolean;

begin
  lValue:=TArithmeticException.exZeroDivide;
  Check(1,lValue=exZeroDivide,'qualified TArithmeticException value');
  Check(2,exAllArithmeticExceptions=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision],
    'exAllArithmeticExceptions');
  lFlags:=DefaultExceptionFlags;
  Check(3,lFlags=[],'DefaultExceptionFlags is empty');
  Check(4,LegacyExceptionFlags=[exInvalidOp,exZeroDivide,exOverflow],'LegacyExceptionFlags');

  lOldMask:=SetExceptionMask(exAllArithmeticExceptions);
  ClearExceptions(False);
  Check(10,GetExceptions=[],'no flags after ClearExceptions(False)');

  gResult:=gOne/gZero;
  Check(11,exZeroDivide in GetExceptions,'masked division by zero sets exZeroDivide');
  ClearExceptions(False);
  Check(12,GetExceptions=[],'ClearExceptions(False) clears exZeroDivide');

  gResult:=gOne/gZero;
  gResult:=gHuge*gHuge;
  Check(13,SameFlags(GetExceptions,[exZeroDivide,exOverflow]),'division by zero and overflow are pending');
  ClearExceptions(False,[exZeroDivide]);
  Check(14,SameFlags(GetExceptions,[exOverflow]),'ClearExceptions with flags clears only those flags');
  ClearExceptions(False);

  lFlags:=SetExceptions([exUnderflow]);
  Check(20,lFlags=[],'SetExceptions returns the previous flags');
  Check(21,GetExceptions=[exUnderflow],'SetExceptions sets the flags');
  lFlags:=SetExceptions([]);
  Check(22,lFlags=[exUnderflow],'SetExceptions returns the flags set before');
  Check(23,GetExceptions=[],'SetExceptions([]) clears the flags');

{$ifdef FPC_HAS_TYPE_EXTENDED}
  gExtResult:=1/gExtZero;
  Check(30,exZeroDivide in GetExceptions,'masked Extended division by zero sets exZeroDivide');
  ClearExceptions(False);
  Check(31,GetExceptions=[],'ClearExceptions(False) clears the Extended flags');
{$endif FPC_HAS_TYPE_EXTENDED}

  SetExceptions([exOverflow]);
  RaiseExceptions([exOverflow]);
  Check(40,GetExceptions=[],'RaiseExceptions clears a masked pending flag without raising');

  SetExceptionMask(exAllArithmeticExceptions-[exZeroDivide]);
  SetExceptions([exZeroDivide]);
  lRaised:=False;
  try
    RaiseExceptions([exZeroDivide]);
  except
    on E: EZeroDivide do
      lRaised:=True;
  end;
  SetExceptionMask(exAllArithmeticExceptions);
  Check(41,lRaised,'RaiseExceptions raises a pending unmasked exception');
  Check(42,GetExceptions=[],'RaiseExceptions clears the raised flag');

  ClearExceptions;
  RaiseExceptions;
  SetExceptionMask(lOldMask);
  ClearExceptions(False);
  writeln('ok');
end.
