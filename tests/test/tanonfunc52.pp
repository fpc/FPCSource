{ %FAIL }

{ in modes with Result modeswitch the result identifier is required for
  for anonymous functions }
program tanonfunc52;

{$mode fpc}
{$modeswitch anonymousfunctions}

type
  TFunc = function(aArg1, aArg2: LongInt): LongInt;

var
  f: TFunc;
begin
  f := function(aArg1, aArg2: LongInt) : LongInt
       begin
       end;
end.
