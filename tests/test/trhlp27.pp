{ record helpers hide methods of the extended record }
program trhlp27;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record
    function Test: Integer;
  end;

  TTestHelper = record helper for TTest
    function Test: Integer;
  end;

function TTest.Test: Integer;
begin
  Result := 1;
end;

function TTestHelper.Test: Integer;
begin
  Result := 2;
end;

var
  t: TTest;
  res: Integer;
begin
  res := t.Test;
  Writeln('t.Test: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
