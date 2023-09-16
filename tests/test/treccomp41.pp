program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = record
    A: Integer;
    contains TChildRec;
    C: Integer;
  end;

var
  c1: TComposed;
  c2: TComposed;
begin
  c1.A := -1;
  c1.B := 42;
  c1.C := -1;
  c2 := c1;
  WriteLn('c1.A: ', c1.A);
  WriteLn('c2.A: ', c2.A);
  WriteLn('c1.B: ', c1.B);
  WriteLn('c2.B: ', c2.B);
  WriteLn('c1.C: ', c1.C);
  WriteLn('c2.C: ', c2.C);
  if (c1.A=c2.A) and
     (c1.B=c2.B) and
     (c1.C=c2.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
