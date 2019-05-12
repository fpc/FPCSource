unit umshlp15a;

{$mode objfpc}

interface

type
  THelperA = class helper for TObject
    function Test: LongInt;
  end;

implementation

function THelperA.Test: LongInt;
begin
  Result := 1;
end;

end.
