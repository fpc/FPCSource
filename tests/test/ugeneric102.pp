unit ugeneric102;

{$mode objfpc}{$H+}

interface

type
  generic TTest<T> = class
    class function Test(aTest: T): T; inline;
    class function Test2(aTest: T): T; inline;
  end;

  TTestLongInt = specialize TTest<LongInt>;

generic function TestFunc<T>(aTest: T): T; inline;

procedure Test;
procedure Test2;

implementation

class function TTest.Test(aTest: T): T;
begin
  Result := aTest;
end;

type
  TTestBoolean = specialize TTest<Boolean>;

{ here the functions won't be inlined, cause the bodies are missing }
procedure Test;
begin
  Writeln(TTestLongInt.Test(42));
  Writeln(TTestBoolean.Test(True));
  Writeln(specialize TTest<String>.Test('Hello World'));

  Writeln(TTestLongInt.Test2(42));
  Writeln(TTestBoolean.Test2(True));
  Writeln(specialize TTest<String>.Test2('Hello World'));

  Writeln(specialize TestFunc<LongInt>(42));
  Writeln(specialize TestFunc<Boolean>(True));
  Writeln(specialize TestFunc<String>('Hello World'));
end;

class function TTest.Test2(aTest: T): T;
begin
  Result := aTest;
end;

generic function TestFunc<T>(aTest: T): T;
begin
  Result := aTest;
end;

{ here the functions will be inlined as now the bodies are available }
procedure Test2;
begin
  Writeln(TTestLongInt.Test(42));
  Writeln(TTestBoolean.Test(True));
  Writeln(specialize TTest<String>.Test('Hello World'));

  Writeln(TTestLongInt.Test2(42));
  Writeln(TTestBoolean.Test2(True));
  Writeln(specialize TTest<String>.Test2('Hello World'));

  Writeln(specialize TestFunc<LongInt>(42));
  Writeln(specialize TestFunc<Boolean>(True));
  Writeln(specialize TestFunc<String>('Hello World'));
end;

end.
