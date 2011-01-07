{%FAIL}

{ class destructors are not allowed }
program tchlp5;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
    class destructor Destroy; override;
  end;

begin

end.

