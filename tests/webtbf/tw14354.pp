{ %fail }

{$mode objfpc}{$H+}

type
   PFoo = ^TFoo;
   TFoo = array[0..10] of Integer;
   TBar = array[0..SizeOf(PFoo(nil)^)] of Integer;
var
  Bar: TBar;

begin
  if High(Bar) <> SizeOf(TFoo) then
    WriteLn('Error: ', High(Bar), ' <> ', SizeOf(TFoo));
end.
