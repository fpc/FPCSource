{ %FAIL }

{ forward declarations are not allowed }
program thlp9;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper;

  TObjectHelper = class helper for TObject
  end;

begin
end.
