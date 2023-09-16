program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
    function CheckAddr(ShouldBe: Pointer): Boolean;
  end;

function TChildRec.CheckAddr(ShouldBe: Pointer): Boolean;
begin
  Result := ShouldBe = @Self;
  WriteLn('@self: ', IntPtr(@self));
end;

type
  TComposed = record
    A: Integer;
    contains TChildRec;
    C: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:    ', IntPtr(@c));
  WriteLn('@c.B:  ', IntPtr(@c.B));
  if not c.CheckAddr(@c.A) and c.CheckAddr(@c.B) and
     not c.CheckAddr(@c.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
