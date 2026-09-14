{$mode objfpc}
{$ModeSwitch StatementExpressions}
uses classes;

var
  s: tstream;
begin
  s := if 0 < 1 then nil else tmemorystream.create;
  WriteLn(intptr(s));
  if assigned(s) then
    Halt(1);
end.
