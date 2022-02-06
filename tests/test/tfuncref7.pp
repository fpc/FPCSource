{ %FAIL }

{ function reference with different signatures can't be assigned to each other }
program tfuncref7;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

type
  TFunc1 = reference to function(aArg: LongInt): String; cdecl;
  TFunc2 = reference to function(aArg: LongInt): String; stdcall;

var
  f1: TFunc1;
  f2: TFunc2;
begin
  f2 := Nil;
  f1 := f2;
end.
