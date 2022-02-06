program tanonfunc12;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test anonymous function in global scope }

type
  tproc = reference to procedure;

var
  i: longint;
  p: tproc;
begin
  p := procedure
  begin
    i := 123;
  end;
  p();
  if i <> 123 then
    halt(1);
end.

