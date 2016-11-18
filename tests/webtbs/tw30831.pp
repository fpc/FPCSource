{ %NORUN }

program tw30831;

{$mode objfpc}

type
  generic TTest<T> = class
  public
    procedure Test1(const aTest: T);
    procedure Test2(const aTest: T);
  end;

procedure TTest.Test1(const aTest: T);
begin
  WriteLn('Test1 ', aTest);
end;

procedure TTest.Test2(const aTest: T);
begin
  WriteLn('Test2 ', aTest);
end;

generic procedure Test<T>();
begin
  with specialize TTest<T>.Create do begin
    Test1(1);
    Test2(9);
    Free;
  end;
end;

begin
  specialize Test<Integer>();
  ReadLn;
end.

