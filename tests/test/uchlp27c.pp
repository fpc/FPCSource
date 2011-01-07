unit uchlp27c; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  uchlp27a;

type
  TBar = class(TFoo)
    function Test: Integer;
  end;

implementation

{ TBar }

function TBar.Test: Integer;
begin
  Result := 3;
end;

end.

