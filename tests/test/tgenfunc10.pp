{ %NORUN }

{ ensure that specializations with local types are handled correctly }

program tgenfunc10;

{$mode objfpc}

operator := (aOther: LongInt): String;
begin
  Str(aOther, Result);
end;

generic function Test<T>(aArg: T): String;
begin
  Result := aArg.Test;
end;

procedure Test1;
type
  TTest = record
    Test: LongInt;
  end;

var
  s: String;
  t: TTest;
begin
  t.Test := 42;
  s := specialize Test<TTest>(t);
end;

procedure Test2;
type
  TTest = record
    Test: String;
  end;

var
  s: String;
  t: TTest;
begin
  t.Test := 'Hello World';
  s := specialize Test<TTest>(t);
end;

begin
  Test1;
  Test2;
end.
