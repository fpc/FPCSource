{$mode objfpc}
{$ModeSwitch StatementExpressions}
uses classes;

var
  s: tstream;
begin
  s := if 0 < 1 then tmemorystream.create else nil;
  WriteLn(intptr(s));
  if not assigned(s) then
    Halt(1);
  s.Free;
end.
