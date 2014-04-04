{ %FAIL }

{$MODE OBJFPC}
program tw25915;

type
   TTestClass = class
      FS: AnsiString;
      procedure TestMethod();
   end;

procedure TTestClass.TestMethod();
begin
   FS := Default(FS); // 'Error: Identifier not found "FS"' and 'Error: Type identifier expected'
end;

var
   S: AnsiString;
begin
   S := Default(S); // two times 'Error: Type identifier expected'
end.
