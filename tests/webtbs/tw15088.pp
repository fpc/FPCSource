{ %norun }
{$mode delphi}

type
  TExceptionMask = (emInvalid, emDenormalized, emZeroDivide,
    emOverflow, emUnderflow, emPrecision);
  TExceptionMasks = set of TExceptionMask;

var
  s: TExceptionMasks;
begin
  s:=[];
  writeln(byte(s));
end.
