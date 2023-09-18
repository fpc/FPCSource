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
    contains record
      case Boolean of
      True: (contains TChildRec1);
      False: (contains TChildRec2);
    end;
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c.A: ', IntPtr(@c.A));
  WriteLn('@c.B: ', IntPtr(@c.B));
  WriteLn('@c.C: ', IntPtr(@c.C));
  WriteLn('@c.D: ', IntPtr(@c.D));
  if (SizeUInt(@c.A)<SizeUInt(@c.B)) and
     (SizeUInt(@c.B)=SizeUInt(@c.C)) and
     (SizeUInt(@c.C)<SizeUInt(@c.D)) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
