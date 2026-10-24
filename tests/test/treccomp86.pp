{ %FAIL }
{ Record composition: an unnamed composition of an anonymous record with
  a managed field is not allowed in a variant part }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TComposed = record
    A: Integer;
    case Boolean of
    True: (contains record
             S: AnsiString;
           end);
    False: (D: Integer);
  end;

begin
end.
