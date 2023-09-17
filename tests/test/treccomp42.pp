program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    A: Integer;
  end;

  generic TComposed<T> = record
    contains child: T;
    B: Double;
  end;

var
  c: specialize TComposed<TChildRec>;
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
