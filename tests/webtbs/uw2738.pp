unit uw2738;

{$mode Delphi}
interface

Type
  T8087Exception = (emInvalidOp, emDenormalizedOperand, emZeroDivide, emOverflow,
    emUnderflow, emPrecision);
  T8087Exceptions = set of T8087Exception;


function SetMasked8087Exceptions(Exceptions: T8087Exceptions; ClearBefore: Boolean = True): T8087Exceptions;

implementation

function SetMasked8087Exceptions(Exceptions: T8087Exceptions; ClearBefore: Boolean): T8087Exceptions;

begin
 writeln('blaat called me');
end;

end.
