{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}
{$ModeSwitch AdvancedRecords}

type
  TChildRec = record
  strict private
    B: Integer;
  end;

  TComposed = record
    contains child: TChildRec;
  end;

var
  rec: TComposed;
begin
  rec.B := 42;
end.
