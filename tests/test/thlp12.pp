{ %FAIL }

{ class constructors are not allowed }
program thlp12;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
    class constructor Create;
  end;

class constructor TObjectHelper.Create;
begin
end;

begin
end.

