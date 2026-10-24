program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    C: Integer;
  end;

  TComposed = record
    contains child: TChildRec;
    A: Integer;
    B: Integer;
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.C:       ', IntPtr(@c.C));
  WriteLn('@c.child.C: ', IntPtr(@c.child.C));
  if (@c.C=@c.child.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
