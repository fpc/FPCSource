program tthlp25;

{$mode delphi}
{$modeswitch typehelpers}

type
  TLongIntHelper = type helper for LongInt
    function Test: LongInt;
  end;

  TTest = record
  end;

  TTestHelper = record helper for TTest
    function Test: LongInt;
  end;

  TTestHelperSub = type helper(TTestHelper) for TTest
    function Test: LongInt;
  end;

function TLongIntHelper.Test: LongInt;
begin
  Result := Self * 2;
end;

function TTestHelper.Test: LongInt;
begin
  Result := 4;
end;

function TTestHelperSub.Test: LongInt;
begin
  Result := inherited Test * 2;
end;

var
  i: LongInt;
  t: TTest;
begin
  i := 2;
  if i.Test <> 4 then
    Halt(1);
  if t.Test <> 8 then
    Halt(2);
  Writeln('ok');
end.
