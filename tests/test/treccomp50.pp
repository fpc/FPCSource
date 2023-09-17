{ %FAIL }
{ %NORUN }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  generic TComposed<T> = record
    A: Integer;
    contains A: T;
    C: Integer;
  end;

var
  c: specialize TComposed<TChildRec>;
begin
end.
