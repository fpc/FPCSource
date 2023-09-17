{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  generic TComposed<T> = record
    A: Integer;
    B: Integer;
    contains child: T;
    D: Integer;
  end;

  TNothing = record end;

var
  c: specialize TComposed<TNothing>;
begin
  c.C := 42;
end.
