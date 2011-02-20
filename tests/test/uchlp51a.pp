unit uchlp51a;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

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

