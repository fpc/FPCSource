{ %FAIL }
{ Record composition: members of an unnamed composition declared in a strict
  private section are not accessible from outside the composing record }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
  private
    C: Integer;
  end;

  TComposed = record
    A: Integer;
    B: Integer;
  strict private
    contains TChildRec;
  public
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:   ', IntPtr(@c));
  WriteLn('@c.C: ', IntPtr(@c.C));
end.
