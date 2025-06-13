{$Mode ObjFPC}
{$ModeSwitch StatementExpressions}
var
  counter: Integer;

function Foo: String;
begin
  Inc(Counter);
  Result := 'Foo';
end;

function Bar: String;
begin
  Inc(Counter);
  Result := 'Bar';
end;

var
  s: String;
begin
  s := if 0<1 then Foo else Bar;
  WriteLn(Counter, ': ', s);
  if Counter<>1 then
    Halt(1);
  if s <> 'Foo' then
    Halt(2);
end.
