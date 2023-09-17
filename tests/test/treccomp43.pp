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

  TChildRec = record
    C: Integer;
  end;

var
  c: specialize TComposed<TChildRec>;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.C:       ', IntPtr(@c.C));
  WriteLn('@c.child.C: ', IntPtr(@c.child.C));
  if (@c.C=@c.child.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
