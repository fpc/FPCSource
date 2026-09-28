{ %FAIL }
{ Record composition: members of a composition declared in a strict private
  section are not accessible from outside the composing record }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    C: Integer;
  end;

  TComposed = record
    A: Integer;
    B: Integer;
  strict private
    contains child: TChildRec;
  public
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:   ', IntPtr(@c));
  WriteLn('@c.C: ', IntPtr(@c.C));
end.
