{ %FAIL }
{ Old file: tbf0071.pp }
{  shows that an unterminated constant string in a writeln() statement crashes the compiler. }

program tbf0071;

begin
  writeln ('
end.
