{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ a variant / whose operands meet as Boolean (two Booleans, or an integer
  left of a Boolean) divides as floating point, as with the operands the
  other way round and as in Delphi; True counts as -1 }
program tvarbooldiv;

{$mode objfpc}

uses
  sysutils, {$ifndef FPUNONE}math,{$endif} variants;

procedure CheckDouble(const v: variant; expected: double; code: longint);
begin
  if VarType(v)<>varDouble then
    begin
      writeln('error ',code,': type ',VarType(v));
      halt(code);
    end;
  if double(v)<>expected then
    begin
      writeln('error ',code,': ',double(v));
      halt(code);
    end;
end;

var
  t, f, i, r: variant;
begin
{$ifndef FPUNONE}
  t:=true;
  f:=false;
  i:=longint(3);
  CheckDouble(t/t,1,1);
  CheckDouble(i/t,-3,2);
  CheckDouble(t/longint(-1),1,3);
  { a floating point division by zero answers an infinity while masked }
  SetExceptionMask(GetExceptionMask+[exZeroDivide]);
  r:=i/f;
  if not (IsInfinite(r) and (r>0)) then
    halt(5);
{$if defined(cpui386) or defined(cpux86_64)}
  { and raises while unmasked, where the FPU can raise it }
  SetExceptionMask(GetExceptionMask-[exZeroDivide]);
  try
    r:=i/f;
    halt(4);
  except
    on e: EZeroDivide do
      ;
  end;
{$endif}
{$endif FPUNONE}
  writeln('ok');
end.
