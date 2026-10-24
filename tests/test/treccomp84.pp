{ %FAIL }
{ Record composition: a named composition of a record with a managed field
  is not allowed in a variant part }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    S: AnsiString;
  end;

  TComposed = record
    A: Integer;
    case Boolean of
    True: (contains child: TChildRec);
    False: (D: Integer);
  end;

begin
end.
