{$mode objfpc}
{$ModeSwitch StatementExpressions}
uses classes;

var
  s: tstream;
begin
  s := if 0 < 1 then tstringstream.create('') else tmemorystream.create;
  WriteLn(s.classname);
  if (not (s is tstringstream)) then
    Halt(1);
  s.Free;
end.
