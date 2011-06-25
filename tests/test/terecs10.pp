program terecs10;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record
    function GetTest(Index: Integer): Integer;
    property Test[Index: Integer]: Integer read GetTest; default;
  end;

function TTest.GetTest(Index: Integer): Integer;
begin
  Result := Index;
end;

var
  t: TTest;
begin
  if t[42] <> 42 then
    halt(1);
end.
