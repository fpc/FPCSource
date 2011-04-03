{ class helpers hide methods of the extended class }
program tchlp28;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    function Test: Integer;
  end;

  TTestHelper = class helper for TTest
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
begin
  t := TTest.Create;
  if t.Test <> 2 then
    Halt(1);
  Writeln('ok');
end.

