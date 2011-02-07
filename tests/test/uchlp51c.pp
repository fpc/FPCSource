unit uchlp51c; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  uchlp51a;

type
  TFooHelper2 = class helper for TFoo
    function AccessTest: Integer;
  end;

implementation

uses
  uchlp51b;

function TFooHelper2.AccessTest: Integer;
begin
  Result := Test;
end;

end.

