{ Record composition: writing a composed member via the composing record
  stores the value in the composed field, not overwritten by a neighbour field }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    A: Integer;
  end;

  TComposed = record
    contains child: TChildRec;
    B: Double;
  end;

var
  c: TComposed;
begin
  c.A := 42;
  c.B := 3.14;
  if (c.child.A=42) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
