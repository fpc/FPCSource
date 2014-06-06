{ %NORUN }

{$MODE OBJFPC}
program tw26271;

type
   TRecord = record
      Member: Pointer;
   end;

function TestFunction(): TRecord;
begin // test.pas(10,1) Error: Can't assign values to const variable
   Result := Default(TRecord);
end;

begin
end.
