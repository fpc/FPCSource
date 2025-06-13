{$Mode ObjFPC}{$H+}
{$ModeSwitch StatementExpressions}

const
  aUnicodeString = UnicodeString('Foo');

var
  s: UnicodeString;
  a: String;
begin
  s:= if false then aUnicodeString else '';
  if s<>'' then
    Halt(1);
end.
