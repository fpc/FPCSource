{ %FAIL }
{ %OPT=-Sew }
{ %NORUN }
{ Record composition: in a specialization of a generic record composing its type
  parameter, a field declared before the composition with the same name as
  a composed member gives a duplicate identifier warning }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  generic TComposed<T> = record
    A: Integer;
    B: Integer;
    contains child: T;
    C: Integer;
  end;

var
  c: specialize TComposed<TChildRec>;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.B:       ', IntPtr(@c.B));
  WriteLn('@c.child.B: ', IntPtr(@c.child.B));
  if (@c.B<>@c.child.B) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
