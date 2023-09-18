program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec1 = record
    A: Integer;
  end;
  TChildRec2 = record
    B: Integer;
  end;

  TComposed = record
  contains TChildRec1;
  case Boolean of
    True: (contains TChildRec2);
  end;

var
  c: TComposed;
begin
  WriteLn('@c.A: ', IntPtr(@c.A));
  WriteLn('@c.B: ', IntPtr(@c.B));
  if @c.A<@c.B then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
