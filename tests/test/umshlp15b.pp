unit umshlp15b;

{$mode objfpc}

interface

type
  THelperB = class helper for TObject
    function Test: LongInt;
  end;

implementation

function THelperB.Test: LongInt;
begin
  Result := 2;
end;

end.
