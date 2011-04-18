{ %FAIL }

{ class destructors are not allowed }
program thlp11;

{$ifdef fpc}
  {$mode delphi}
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

