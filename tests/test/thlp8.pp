{ %FAIL }

{ Fields are not allowed in helpers }
program thlp8;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
    Field: Integer;
  end;

begin
end.
