{ %FAIL }
{ %OPT=-Sew }
{ Record composition: a field of the composing record with the same name as
  a member of the composed record gives a duplicate identifier warning }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    C: Integer;
  end;

  TComposed = record
    A: Integer;
    B: Integer;
    contains child: TChildRec;
    C: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.C:       ', IntPtr(@c.C));
  WriteLn('@c.child.C: ', IntPtr(@c.child.C));
  if (@c.C<>@c.child.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
