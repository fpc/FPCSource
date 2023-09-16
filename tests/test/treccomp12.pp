program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    A: Integer;
  end;

  TComposed = record
    contains TChildRec;
    B: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:   ', IntPtr(@c));
  WriteLn('@c.A: ', IntPtr(@c.A));
  WriteLn('@c.B: ', IntPtr(@c.B));
  if (@c=@c.A) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
