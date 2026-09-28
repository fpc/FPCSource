{ %FAIL }
{ Record composition: in a specialization of a generic record composing its
  type parameter, an identifier that is not a member of the actual type is an error }
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
