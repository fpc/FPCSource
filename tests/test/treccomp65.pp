{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}
{$ModeSwitch TypeHelpers}

type
  TTestHelper = type helper for Integer
  end;

  TTest = record
    contains TTestHelper;
  end;

begin
end.
