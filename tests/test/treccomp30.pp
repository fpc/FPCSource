{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

ype
  TComposed = record
    A: Integer;
    contains child: Integer;
    D: Integer;
  end;

begin
end.
