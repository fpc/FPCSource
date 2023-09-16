{ %FAIL }
{ %NORUN }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = record
  strict private
    child: TChildRec;
  public
    A: Integer;
    C: Integer;
    contains child;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.B:       ', IntPtr(@c.B));
  WriteLn('@c.child.B: ', IntPtr(@c.child.B));
end.
