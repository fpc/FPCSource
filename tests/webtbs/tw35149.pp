{ %norun }

program project1;

{$mode objfpc}
type
  TestObject = object
  var
    TestNested: Integer;
  end;

begin
  writeln(SizeOf(TestObject.TestNested));
end. 
