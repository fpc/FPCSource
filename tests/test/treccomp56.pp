program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec1 = record
    B: Integer;
  end;
  TChildRec2 = record
    C: Integer;
  end;

  TComposed = record
    A: Integer;
    contains union: record
      case Boolean of
      True: (contains c1: TChildRec1);
      False: (contains c2: TChildRec2);
    end;
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
