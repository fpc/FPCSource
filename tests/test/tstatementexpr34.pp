{ managed result types must not leak }
{$Mode ObjFPC}{$H+}
{$ModeSwitch StatementExpressions}

function ConditionalThrow(i: Integer): String;
begin
  Result := Copy('abcdef', 1, i mod 5 + 1);
  if i mod 3 = 0 then
    raise TObject.Create;
end;

procedure Run;
var
  i: Integer;
  s: String;
begin
  for i := 1 to 1000 do
    begin
      s := if Odd(i) then Copy('abcdef', 1, i mod 5 + 1) else Copy('uvwxyz', 1, i mod 4 + 1);
      s := case i mod 3 of
        0: Copy('abcdef', 1, i mod 5 + 1);
        1: Copy('uvwxyz', 1, i mod 4 + 1);
        else s + 'x'
      end;
      s := try ConditionalThrow(i) except on o: TObject do Copy('abc', 1, i mod 3 + 1) else 'Error' end;
    end;
  WriteLn(s);
end;

var
  HeapBefore: PtrUInt;
begin
  HeapBefore := GetFPCHeapStatus.CurrHeapUsed;
  Run;
  WriteLn(HeapBefore, ' ', GetFPCHeapStatus.CurrHeapUsed);
  if GetFPCHeapStatus.CurrHeapUsed<>HeapBefore then
    Halt(1);
end.
