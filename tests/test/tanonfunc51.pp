{ in modes without modeswitch result the name of the result of an anonymous
  function can be specified just as for operators }

program tanonfunc51;

{$mode fpc}
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
end.
