{%FAIL}
{ try..finally cannot be an expression }
{$Mode ObjFPC}
{$ModeSwitch StatementExpressions}
var
  s: String;
begin
  s := try 'Foo' finally WriteLn('Bar'); end;
  Halt(1);
end.
