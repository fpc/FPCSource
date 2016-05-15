{ %fail }
{ %opt=-vw -Sew }

program thelper;

{$mode objfpc}

type
  TTest = class;

  TTestHelper = class helper for TTest
    procedure Test;
  end;

  TTest = class
    Test: LongInt;
  end;

procedure TTestHelper.Test;
begin
end;

begin
end.
