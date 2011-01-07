{%FAIL}

{ abstract methods are not allowed }
program tchlp8;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
    procedure SomeMethod; virtual; abstract;
  end;

begin

end.

