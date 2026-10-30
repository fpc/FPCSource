{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ An Empty operand of a variant /, div or mod counts as 0, as in Delphi: Empty
  divided by a number is 0, a number divided by Empty is a division by zero,
  Empty by Empty raises, and Null beside Empty gives Null. None of them ends
  the process }
program tvaremptydiv;

{$mode objfpc}

uses
  sysutils, {$ifndef FPUNONE}math,{$endif} variants;

var
  e, n, d, r, target, ref: variant;

procedure CheckType(const v: variant; vt: TVarType; code: longint);
begin
  if VarType(v)<>vt then
    begin
      writeln('error ',code,': type ',VarType(v),', expected ',vt);
      halt(code);
    end;
end;

procedure CheckZero(const v: variant; vt: TVarType; code: longint);
begin
  CheckType(v,vt,code);
  if v<>0 then
    begin
      writeln('error ',code,': not 0');
      halt(code);
    end;
end;

{ op 0: /, 1: div, 2: mod }
function Apply(const a, b: variant; op: longint): variant;
begin
  case op of
    0: result:=a/b;
    1: result:=a div b;
  else
    result:=a mod b;
  end;
end;

procedure CheckRaises(const a, b: variant; op: longint; cls: ExceptClass; code: longint);
begin
  try
    Apply(a,b,op);
  except
    on x: Exception do
      begin
        if x.ClassType<>cls then
          begin
            writeln('error ',code,': ',x.ClassName,' raised, expected ',cls.ClassName);
            halt(code);
          end;
        exit;
      end;
  end;
  writeln('error ',code,': nothing raised, expected ',cls.ClassName);
  halt(code);
end;

begin
  e:=Unassigned;
  n:=int64(3);
  { Empty divided by a number, and a number divided by Empty }
  CheckZero(e div n,varInt64,3);
  CheckZero(e mod n,varInt64,4);
  CheckRaises(n,e,1,EDivByZero,6);
  CheckRaises(n,e,2,EDivByZero,7);
{$ifndef FPUNONE}
  d:=1.5;
  CheckZero(e/n,varDouble,1);
  CheckZero(e/d,varDouble,2);
  CheckZero(e div d,varInteger,5);
  CheckRaises(d,e,1,EDivByZero,8);
  { a floating point division by zero answers an infinity while masked }
  SetExceptionMask(GetExceptionMask+[exZeroDivide]);
  r:=n/e;
  CheckType(r,varDouble,10);
  if not (IsInfinite(r) and (r>0)) then
    halt(11);
{$if defined(cpui386) or defined(cpux86_64)}
  { and raises while unmasked, where the FPU can raise it }
  SetExceptionMask(GetExceptionMask-[exZeroDivide]);
  CheckRaises(n,e,0,EZeroDivide,9);
{$endif}
{$endif FPUNONE}
  { Empty by Empty }
  CheckRaises(e,e,0,EZeroDivide,12);
  CheckRaises(e,e,1,EDivByZero,13);
  CheckRaises(e,e,2,EDivByZero,14);
  { Null beside Empty }
  CheckType(e/Null,varNull,15);
  CheckType(Null div e,varNull,16);
  { Empty held by reference }
  target:=Unassigned;
  TVarData(ref).vType:=varVariant or varByRef;
  TVarData(ref).vPointer:=@target;
  CheckZero(ref div n,varInt64,17);
  CheckRaises(n,ref,1,EDivByZero,18);
  TVarData(ref).vType:=varEmpty;
  writeln('ok');
end.
