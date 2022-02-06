program tanonfunc21;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test passing references across unit boundaries }

uses uanonfunc21;

procedure foo;
var
  i: Integer;
begin
  bar(procedure
    begin
      i := 123;
    end);
  if i <> 123 then
    halt(1);
end;

begin
  foo;
end.

