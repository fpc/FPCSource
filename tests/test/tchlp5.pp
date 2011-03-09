{ %NORUN }

{ class destructors are allowed }
program tchlp5;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
    class destructor Destroy;
  end;

class destructor TObjectHelper.Destroy;
begin

end;

begin

end.

