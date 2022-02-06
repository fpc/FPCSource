{ %NORUN }

program tanonfunc37;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test local reference declaration }

procedure Foo;
type
  TLocalProc = reference to procedure;
var
  P: TLocalProc;
begin
  P := procedure
    begin
    end;
  P();
end;

begin
  Foo;
end.

