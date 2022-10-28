program tgeneric111;

{$mode delphi}

uses
  ugeneric108b, ugeneric108a;

type
  TTestA = ugeneric108a.TTest<LongInt>;
  TTestB = ugeneric108b.TTest<LongInt>;

function Test1: ugeneric108a.TTest<LongInt>;
begin
  Result.f := 1;
end;

function Test2: ugeneric108b.TTest<LongInt>;
begin
  Result.f := 2;
end;

var
  a1: TTestA;
  b1: TTestB;
  a2: ugeneric108a.TTest<LongInt>;
  b2: ugeneric108b.TTest<LongInt>;
begin
  if a1.Test <> 1 then
    Halt(1);
  if b1.Test <> 2 then
    Halt(2);

  if a2.Test <> 1 then
    Halt(3);
  if b2.Test <> 2 then
    Halt(4);

  if ugeneric108a.TTest<LongInt>.Test2 <> 1 then
    Halt(5);
  if ugeneric108b.TTest<LongInt>.Test2 <> 2 then
    Halt(6);

  a1 := Test1;
  if a1.f <> 1 then
    Halt(7);
  b1 := Test2;
  if b1.f <> 2 then
    Halt(8);
end.
