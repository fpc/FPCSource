{ %FAIL }
program tb0239;

{$MODE objfpc}

type
  TMyClass = class
    function GetMyProperty(Index: Integer): Integer;
    property MyProperty: Integer index 10000000000000000 read GetMyProperty;
  end;

function TMyClass.GetMyProperty(Index: Integer): Integer;
begin
  Result := 10;
end;

begin
end.
