{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildClass = class;

  TComposed = record
    A: Integer;
    contains child: TChildClass;
  end;

  TChildClass = class
  public
    B: Integer;
  end;

begin
end.
