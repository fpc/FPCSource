{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}
{$ModeSwitch TypeHelpers}

type
  TTest = record
  end;

  TTestHelper = type helper for TTest
  end;

  TTest1 = record
    contains TTestHelper;
  end;

begin
end.
