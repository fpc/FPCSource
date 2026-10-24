{ Record composition: an unnamed composition with an anonymous (inline declared)
  record type is placed in memory at its declaration position }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TComposed = record
    A: Integer;
    B: Integer;
    contains record
      C: Integer;
    end;
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c.B: ', IntPtr(@c.B));
  WriteLn('@c.C: ', IntPtr(@c.C));
  WriteLn('@c.D: ', IntPtr(@c.D));
  if (SizeUInt(@c.B)<SizeUInt(@c.C)) and
     (SizeUInt(@c.C)<SizeUInt(@c.D)) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
