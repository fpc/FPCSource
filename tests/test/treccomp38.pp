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
    contains TFirstChild;
    contains TSecondChild;
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:   ', IntPtr(@c));
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
