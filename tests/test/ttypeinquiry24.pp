{ %FAIL }
{ "type of" a parameter inside its own declaration }
program ttypeinquiry24;

{$mode objfpc}

procedure DoIt(a: type of a);
begin
end;

begin
end.
