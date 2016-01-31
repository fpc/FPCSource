program tifthen1;

procedure Test(aValue: Boolean; aErrOffset: LongInt);
var
  i: LongInt;
  s: String;
  b: Boolean;
  c: Char;
begin
  i := IfThen(aValue, 42, 21);
  if (aValue and (i <> 42)) or (not aValue and (i <> 21)) then
    Halt(aErrOffset + 1);
  b := IfThen(aValue, False, True);
  if (aValue and b) or (not aValue and not b) then
    Halt(aErrOffset + 2);
  s := IfThen(aValue, 'Hello', 'World');
  if (aValue and (s <> 'Hello')) or (not aValue and (s <> 'World')) then
    Halt(aErrOffset + 3);
  c := IfThen(aValue, #13, #10);
  if (aValue and (c <> #13)) or (not aValue and (c <> #10)) then
    Halt(aErrOffset + 4);
end;

begin
  Test(False, 0);
  Test(True, 40);
end.
