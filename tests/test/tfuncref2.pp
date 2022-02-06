{ %OPT=-gh }

{ function reference with compatible signatures can be assigned to each other }
program tfuncref2;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

type
  TFunc1 = reference to function(aArg: LongInt): String;
  TFunc2 = reference to function(aArg: LongInt): String;

  TTest = class(TInterfacedObject, TFunc1)
    function Invoke(aArg: LongInt): String;
  end;

function TTest.Invoke(aArg: LongInt): String;
begin
  Str(aArg, Result);
end;

var
  f1: TFunc1;
  f2: TFunc2;
begin
  {$if declared(HaltOnNotReleased)}
  HaltOnNotReleased:=True;
  {$endif}
  f1 := TTest.Create;
  f2 := f1;
  f1 := Nil;
  if f2(42) <> '42' then
    Halt(1);
end.
