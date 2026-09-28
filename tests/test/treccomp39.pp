{ %FAIL }
{ Record composition: an unnamed composition of a non-record type (Integer)
  is not allowed }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TComposed = record
    A: Integer;
    contains Integer;
    D: Integer;
  end;

begin
end.
