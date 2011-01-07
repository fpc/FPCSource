unit uchlp33c;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  uchlp33a, uchlp33b;

type
  TFooHelper2 = class helper(TFooHelper) for TFoo
    function Test: Integer;
  end;

implementation

function TFooHelper2.Test: Integer;
begin
  Result := 2;
end;

end.

