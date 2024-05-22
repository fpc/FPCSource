{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TTestHelper = class helper for TObject
  end;

  TTest = record
    contains TTestHelper;
  end;

begin
end.
