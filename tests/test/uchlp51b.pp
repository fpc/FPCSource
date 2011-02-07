unit uchlp51b; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  uchlp51a;

type
  TFooHelper = class helper for TFoo
    function Test: Integer;
  end;

implementation

function TFooHelper.Test: Integer;
begin
  Result := 2;
end;

end.

