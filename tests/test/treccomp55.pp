{ %FAIL }
{ Record composition: composing a class helper is not allowed }
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
