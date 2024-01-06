{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildClass = class
  public
    B: Integer;
  end;

  TComposed = record
    A: Integer;
    contains cls: TChildClass;
    C: Integer;
  end;

var
  c: TComposed;
begin
  c.cls:=TChildClass.Create;
  WriteLn('@c:       ', IntPtr(@c));
  WriteLn('@c.B:     ', IntPTr(@c.B));
  WriteLn('@c.cls.B: ', IntPtr(@c.cls.B));
  if (@c.cls.B=@c.B) then
  begin
    c.cls.Free;
    WriteLn('ok');
    halt(0);
  end;
  c.cls.Free;
  halt(1);
end.
