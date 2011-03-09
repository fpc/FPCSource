{%FAIL}

{ destructors are not allowed }
program tchlp4;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
    destructor Destroy; override;
  end;

destructor TObjectHelper.Destroy;
begin

end;

begin

end.

