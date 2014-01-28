{ %fail }

{$mode delphi}

program test;

type
   TDynamicArray = array of record end; // or array of whatever
   PDynamicArray = ^TDynamicArray;

function TestA(): Pointer;
begin
   Result := nil;
end;

function TestB(): PDynamicArray;
begin
   // can't take address of function return value, but compiler instead says "Internal error 2006111510"
   Result := @TDynamicArray(TestA());
end;

begin
end.
