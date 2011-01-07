unit uchlp33b; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  uchlp33a;

type
  TFooHelper = class helper for TFoo
    function Test: Integer;
  end;

implementation

function TFooHelper.Test: Integer;
begin
  Result := 1;
end;

end.

