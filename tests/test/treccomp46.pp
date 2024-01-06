{ %FAIL }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  generic TComposed<T: TObject> = record
    A: Integer;
    contains cls: T;
    C: Integer;
  end;

  TChildClass = class
  public
    B: Integer;
  end;

var
  c: specialize TComposed<TChildClass>;
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
