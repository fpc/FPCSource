program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    function GetSelf: Pointer;
  end;

function TChildRec.GetSelf: Pointer;
begin
  Result := @Self;
end;

type
  TComposed = record
    A: Integer;
    contains child: TChildRec;
    C: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.GetSelf: ', IntPtr(c.GetSelf));
  WriteLn('@c.child:   ', IntPtr(@c.child));
  if c.GetSelf=@c.child then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
