program tb0604;

{$mode objfpc}

{.$define writeresults}

procedure CheckResult(aActual, aExpected, aExitCode: LongInt);
begin
  if aActual <> aExpected then begin
{$ifdef writeresults}
    Writeln('Test ', aExitCode, ' failed. Result: ', aActual, ' Expected: ', aExpected);
{$endif}
    Halt(aExitCode);
  end;
end;

function TestSimple1: LongInt;
begin
  case 4 of
    2: Result := 2;
    4: Result := 4;
    6: Result := 6;
    else
      Result := 8;
  end;
end;

function TestSimple2: LongInt;
begin
  case 8 of
    2: Result := 2;
    4: Result := 4;
    6: Result := 6;
    else
      Result := 8;
  end;
end;

function TestSimple3: LongInt;
begin
  case 1 of
    2: Result := 2;
    4: Result := 4;
    6: Result := 6;
    else
      Result := 8;
  end;
end;

function TestSimple4: LongInt;
begin
  case 3 of
    2: Result := 2;
    4: Result := 4;
    6: Result := 6;
    else
      Result := 8;
  end;
end;

function TestSimple5: LongInt;
begin
  case 3 of
    2: Result := 2;
    4: Result := 4;
    6: Result := 6;
  end;
  Result := 8;
end;

function TestRange1: LongInt;
begin
  case 4 of
    2..4: Result := 3;
    6..8: Result := 7;
    else
      Result := 8;
  end;
end;

function TestRange2: LongInt;
begin
  case 3 of
    2..4: Result := 3;
    6..8: Result := 7;
    else
      Result := 8;
  end;
end;

function TestRange3: LongInt;
begin
  case 2 of
    2..4: Result := 3;
    6..8: Result := 7;
    else
      Result := 8;
  end;
end;

function TestRange4: LongInt;
begin
  case 5 of
    2..4: Result := 3;
    6..8: Result := 7;
    else
      Result := 8;
  end;
end;

function TestRange5: LongInt;
begin
  case 9 of
    2..4: Result := 3;
    6..8: Result := 7;
    else
      Result := 8;
  end;
end;

function TestRange6: LongInt;
begin
  case 1 of
    2..4: Result := 3;
    6..8: Result := 7;
    else
      Result := 8;
  end;
end;

function TestInlineFunc(a, b: LongInt): LongInt; inline;
begin
  case a of
    0..4:
      Result := a * b;
    6..9:
      Result := a + b;
  end;

  case b of
    0..4:
      Result := Result - (a - b);
    6..9:
      Result := Result * (a mod b);
  end;
end;

function TestInline: LongInt;
begin
  Result := TestInlineFunc(7, 3);
end;

begin
  CheckResult(TestSimple1, 4, 1);
  CheckResult(TestSimple2, 8, 2);
  CheckResult(TestSimple3, 8, 3);
  CheckResult(TestSimple4, 8, 4);
  CheckResult(TestSimple5, 8, 5);
  CheckResult(TestRange1, 3, 6);
  CheckResult(TestRange2, 3, 7);
  CheckResult(TestRange3, 3, 8);
  CheckResult(TestRange4, 8, 9);
  CheckResult(TestRange5, 8, 10);
  CheckResult(TestRange6, 8, 11);
  CheckResult(TestInline, 6, 121);
{$ifdef writeresults}
  Writeln('ok');
{$endif}
end.
