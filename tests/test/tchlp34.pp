{ %FAIL }

{ a class helper can only inherit from another class helper }
program tchlp34;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TFoo = class

  end;

  TObjectHelper = class helper(TFoo) for TObject
  end;

begin
end.

