{ %FAIL }

{ function reference with different signatures can't be assigned to each other }
program tfuncref6;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

type
  TFunc1 = reference to function(aArg: LongInt): String;
  TFunc2 = reference to function(aArg: LongInt): LongInt;

var
  f1: TFunc1;
  f2: TFunc2;
begin
  f2 := Nil;
  f1 := f2;
end.
