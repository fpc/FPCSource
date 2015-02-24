{ %NORUN }

program tgeneric99;

{$mode objfpc}

uses
  ugeneric99;

type
  TTest1 = specialize TTest<LongInt>;
  TTest2 = ugeneric99.specialize TTest<LongInt>;

  TTest3 = TTestClass.specialize TTest<LongInt>;
  TTest4 = ugeneric99.TTestClass.specialize TTest<LongInt>;

  TTest5 = TTestRec.specialize TTest<LongInt>;
  TTest6 = ugeneric99.TTestRec.specialize TTest<LongInt>;

var
  test1: specialize TTestArray<LongInt>;
  test2: ugeneric99.specialize TTestArray<LongInt>;

  test3: ugeneric99.TTestClass.specialize TTestArray<LongInt>;
  test4: ugeneric99.TTestRec.specialize TTestArray<LongInt>;

  test5: ugeneric99.TTestClass.specialize TTest<LongInt>.TTestRec;
  test6: ugeneric99.TTestRec.specialize TTest<LongInt>.TTestClass;

procedure Proc1(aArg: specialize TTestArray<LongInt>);
begin
end;

procedure Proc2(aArg: ugeneric99.specialize TTestArray<LongInt>);
begin
end;

procedure Proc3(aArg: ugeneric99.TTestClass.specialize TTestArray<LongInt>);
begin
end;

procedure Proc4(aArg: ugeneric99.TTestRec.specialize TTestArray<LongInt>);
begin
end;

procedure Proc5(aArg: ugeneric99.TTestClass.specialize TTest<LongInt>.TTestRec);
begin
end;

procedure Proc6(aArg: ugeneric99.TTestRec.specialize TTest<LongInt>.TTestClass);
begin
end;

begin
end.
