program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TFirstChild = record
    B: Integer;
  end;
  TSecondChild = record
    C: Integer;
  end;

  TComposed = record
    A: Integer;
    contains c1: TFirstChild;
    contains c2: TSecondChild;
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:      ', IntPtr(@c));
  WriteLn('@c.B:    ', IntPtr(@c.B));
  WriteLn('@c.c1.B: ', IntPtr(@c.c1.B));
  WriteLn('@c.C:    ', IntPtr(@c.C));
  WriteLn('@c.c2.C: ', IntPtr(@c.c2.C));
  if (@c.B=@c.c1.B) and
     (@c.C=@c.c2.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
