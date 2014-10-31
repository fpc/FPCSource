{ %norun }

{$MODE OBJFPC}
program test;

type
   TTest = class end;

procedure E(Arg1: array of UTF8String);
begin end;

procedure E(Arg1: array of TTest);
begin end;

begin
   E(['aa']); // Incompatible types: got "Constant String" expected "TTest"
end.
