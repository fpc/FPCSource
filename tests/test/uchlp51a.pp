unit uchlp51a;

{$mode objfpc}{$H+}

interface

type
  TFoo = class
    function Test: Integer;
  end;

implementation

function TFoo.Test: Integer;
begin
  Result := 1;
end;

end.

