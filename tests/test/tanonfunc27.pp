program tanonfunc27;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test accessing a captured variable from both a nested procedure and an anonymous function }

type
  tproc = reference to procedure;

procedure foo;
var
  i: Integer;

  procedure inner;
  begin
    inc(i, 20);
  end;

begin
  i := 100;
  tproc(
    procedure begin
      inc(i, 3);
    end)();
  inner;
  if i <> 123 then
    halt(1);
end;

begin
  foo;
end.

