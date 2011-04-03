{ %FAIL }

{ destructors are not allowed }
program thlp10;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
    destructor Destroy;
  end;

destructor TObjectHelper.Destroy;
begin
end;

begin
end.
