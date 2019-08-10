{ %fail }

program project1;

{$mode objfpc}
type
  TestObject = object
  var
    TestNested: Integer;
  end;

begin
  writeln(TestObject.TestNested);
end. 
