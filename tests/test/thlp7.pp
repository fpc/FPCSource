{ %FAIL }

{ abstract methods are forbidden }
program thlp7;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{ Note: Delphi complains that forward declaration is not solved,
        but if you add a implementation it complains that
        "abstract" is not allowed }

type
  TObjectHelper = class helper for TObject
    procedure Test; virtual; abstract;
  end;

begin
end.
