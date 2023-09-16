{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildClass = class
  public
    B: Integer;
  end;

  TComposed = record
    A: Integer;
    contains TChildClass;
    C: Integer;
  end;

begin
end.
