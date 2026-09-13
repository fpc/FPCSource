{%FAIL}
{$Mode ObjFPC}
{$ModeSwitch StatementExpressions}

var
  s: String;
begin
  // missing except else
  s := try 'Foo' except on e: TObject do 'Error' end;
  Halt(1);
end.
