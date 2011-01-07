unit uchlp35; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

type
  TObjectHelperA = class helper for TObject
    function Test: Integer;
    function VirtualTest: Integer; virtual;
  end;

implementation

function TObjectHelperA.Test: Integer;
begin
  Result := VirtualTest;
end;

function TObjectHelperA.VirtualTest: Integer;
begin
  Result := 1;
end;

end.

