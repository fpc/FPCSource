program tgeneric109;

{$mode objfpc}

uses
  ugeneric108b, ugeneric108a;

type
  TTestA = ugeneric108a.specialize TTest<LongInt>;
  TTestB = ugeneric108b.specialize TTest<LongInt>;

var
  a1: TTestA;
  b1: TTestB;
  a2: ugeneric108a.specialize TTest<LongInt>;
  b2: ugeneric108b.specialize TTest<LongInt>;
begin
  if a1.Test <> 1 then
    Halt(1);
  if b1.Test <> 2 then
    Halt(2);

  if a2.Test <> 1 then
    Halt(3);
  if b2.Test <> 2 then
    Halt(4);

  if ugeneric108a.specialize TTest<LongInt>.Test2 <> 1 then
    Halt(5);
  if ugeneric108b.specialize TTest<LongInt>.Test2 <> 2 then
    Halt(6);
end.
