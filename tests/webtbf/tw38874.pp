{ %fail }
program ConstString;

procedure Test;
const TestString='abc';'def';
begin
  writeln(TestString);
end;

begin
  Test;
end.
