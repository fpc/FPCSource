{ Record composition: in a specialization of a generic record with an unnamed
  composition of its type parameter declared first, the composed record is
  at offset 0 }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    A: Integer;
  end;

  generic TComposed<T> = record
    contains T;
    B: Double;
  end;

var
  c: specialize TComposed<TChildRec>;
begin
  c.A := 42;
  c.B := 3.14;
  if (@c=@c.A) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
