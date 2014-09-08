{ %fail }

{$mode delphi}

program test;

type
   PArray = ^TArray;
   TArray = array[1..sizeof(ptrint)] of byte;

function TestA(): Pointer;
begin
   Result := nil;
end;

function TestB(): PArray;
begin
   // can't take address of function return value, but compiler instead says "Internal error 2006111510"
   Result := @TArray(TestA());
end;

begin
end.
