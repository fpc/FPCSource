{ %NORUN }

program tb0673;

{$mode objfpc}

type
  TTest = class
    generic procedure Test<T>;
  end;

generic procedure TTest.Test<T>;

  procedure SubTest1; forward;

  procedure SubTest2;
  begin
    SubTest1;
  end;

  procedure SubTest1;
  begin

  end;

begin
  SubTest2;
end;

var
  t: TTest;
begin
  t.specialize Test<LongInt>;
end.
