program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildChildRec = record
    C: Integer;
  end;

  TChildRec = record
    B: Integer;
    contains c2: TChildChildRec;
  end;

  TComposed = record
    A: Integer;
    contains c1: TChildRec;
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.C:       ', IntPtr(@c.C));
  WriteLn('@c.c1.c2.C: ', IntPtr(@c.c1.c2.C));
  if (@c.C=@c.c1.c2.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
