{%FAIL}

{ forward declarations are not allowed }
program tchlp3;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject;

  TObjectHelper = class helper for TObject
  end;

begin

end.

