{$mode objfpc}

{ we test the context sensitivity of generics here, by checking whether names
  are looked up at specialization or at definition time }

uses
  ugeneric14;

const
  Foo = 4;

type
  TIntTest = specialize TGTest<Integer>;

var
  A: TIntTest;
begin
  A := TIntTest.Create;
  A.DoSomething;
  writeln(A.data);
  if A.data = 4 then
    halt(1);
  A.Free;
end.
