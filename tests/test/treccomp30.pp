{ %FAIL }
{ Record composition: composing a non-record type (Integer) is not allowed }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TComposed = record
    A: Integer;
    contains child: Integer;
    D: Integer;
  end;

begin
end.
