program tanonfunc39;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test generic local reference declaration }

procedure Foo;
type
  generic TLocalFunc<T> = reference to function(arg: T): T;
var
  F: specialize TLocalFunc<longint>;
begin
  F := function(arg: longint): longint
    begin
      Result := arg * arg;
    end;
  if F(5) <> 25 then
    halt(1);
end;

begin
  Foo;
end.

