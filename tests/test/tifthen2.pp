program tifthen2;

var
  execA: Boolean = False;
  execB: Boolean = False;

function A: LongInt;
begin
  A := 42;
  execA := True;
end;

function B: LongInt;
begin
  B := 21;
  execB := True;
end;

procedure Test(aValue: Boolean; aErrOffset: LongInt);
begin
  execA := False;
  execB := False;
  IfThen(aValue, A, B);
  if (aValue and not execA) or (not aValue and not execB) then
    Halt(aErrOffset + 1);
  if (aValue and execB) or (not aValue and execA) then
    Halt(aErrOffset + 2);
end;

begin
  Test(True, 0);
  Test(False, 10);
end.
