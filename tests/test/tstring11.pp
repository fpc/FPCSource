
{ As this is a Delphi compatibility test
  I suppose delphi mode is OK, PM 2013-06-05 }
{$mode delphi}

program tstrtest;

{$APPTYPE CONSOLE}

function Test1(const aValue: RawByteString): Integer; overload;
begin
  Result := 1;
end;

function Test1(const aValue: UnicodeString): Integer; overload;
begin
  Result := 2;
end;

function Test2(aValue: RawByteString): Integer; overload;
begin
  Result := 3;
end;

function Test2(aValue: UnicodeString): Integer; overload;
begin
  Result := 4;
end;

procedure CheckResult(aActual, aExpected: Integer);
begin
  if aActual <> aExpected then begin
    Writeln('Actual: ', aActual, ' Expected: ', aExpected);
    Readln;
    Halt(1);
  end;
end;

procedure TestOpenArray(oac: array of AnsiChar; owc: array of WideChar);
begin
  CheckResult(Test1(oac), 1);
  CheckResult(Test1(owc), 2);
  CheckResult(Test2(oac), 3);
  CheckResult(Test2(owc), 4);
end;

var
  pwc: PWideChar;
  pac: PAnsiChar;
  aac: array[0..20] of AnsiChar;
  awc: array[0..20] of WideChar;
  wc: WideChar;
  ac: AnsiChar;
  ss: ShortString;
begin
  CheckResult(Test1(pac), 1);
  CheckResult(Test1(pwc), 2);
  CheckResult(Test2(pac), 3);
  CheckResult(Test2(pwc), 4);

  CheckResult(Test1(ac), 1);
  CheckResult(Test1(wc), 2);
  CheckResult(Test2(ac), 3);
  CheckResult(Test2(wc), 4);

  CheckResult(Test1(aac), 1);
  CheckResult(Test1(awc), 2);
  CheckResult(Test2(aac), 3);
  CheckResult(Test2(awc), 4);

  CheckResult(Test1(ss), 1);
  CheckResult(Test2(ss), 3);

  TestOpenArray([], []);

  Writeln('ok');
  // Readln;
end.
