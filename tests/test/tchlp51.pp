{ methods defined in a helper have higher priority than those defined in the
  extended type }
program tchlp51;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test: Integer;
  end;

  TTestHelper = class helper for TTest
  private
    function Test: Integer;
  public
    function AccessTest: Integer;
  end;

function TTest.Test: Integer;
begin
  Result := 1;
end;

function TTestHelper.Test: Integer;
begin
  Result := 2;
end;

function TTestHelper.AccessTest: Integer;
begin
  Result := Test;
end;

var
  t: TTest;
  res: Integer;
begin
  t := TTest.Create;
  res := t.AccessTest;
  Writeln('t.AccessTest: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
