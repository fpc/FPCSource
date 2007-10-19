{ %fail }

{$mode objfpc}

{ we test the context sensitivity of generics here, by checking whether names
  are looked up at specialization or at definition time. 
  For the moment this fails, because the assembler symbols are not global and
  therefor not accessible from other .o files }

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
