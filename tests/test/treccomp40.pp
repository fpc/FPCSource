program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildChildRec = record
    C: Integer;
  end;

  TChildRec = record
    B: Integer;
    contains TChildChildRec;
  end;

  TComposed = record
    A: Integer;
    contains TChildRec;
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
     (SizeUInt(@c.B)<SizeUInt(@c.C)) and
     (SizeUInt(@c.C)<SizeUInt(@c.D)) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
