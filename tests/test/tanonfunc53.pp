{ for consistency with the behavior of operators function result renaming can
  be used on anonymous functions in Result modes as well }

program tanonfunc53;

{$mode objfpc}
{$modeswitch anonymousfunctions}

type
  TFunc = function(aArg1, aArg2: LongInt): LongInt;

var
  f: TFunc;
begin
  f := function(aArg1, aArg2: LongInt) Res : LongInt
       begin
         Res := aArg1 + aArg2;
       end;
  if f(2, 3) <> 5 then
    Halt(1);

  f := function(aArg1, aArg2: LongInt) Res : LongInt
       begin
         Result := aArg1 - aArg2;
       end;
  if f(7, 4) <> 3 then
    Halt(2);
end.
