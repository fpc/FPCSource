{ %FAIL }
{ Record composition: a property of the composing record cannot use
  a member of a composed record as read/write accessor }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = record
    A: Integer;
    contains child: TChildRec;
    C: Integer;
    property CB: Integer read B write B;
  end;

begin
end.
