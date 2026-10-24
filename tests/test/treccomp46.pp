{ Record composition: in a specialization of a generic record with an unnamed
  composition of its type parameter, the composed record is placed in memory
  at its declaration position }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    C: Integer;
  end;

  generic TComposed<T> = record
    A: Integer;
    B: Integer;
    contains T;
    D: Integer;
  end;

var
  c: specialize TComposed<TChildRec>;
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
