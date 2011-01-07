{ %FAIL }

{ class helpers must extend the same class if inheriting }
program tchlp26;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelperA = class helper for TObject
  end;

  TFoo = class
  end;

  TObjectHelperB = class helper(TObjectHelperA) for TFoo
  end;

begin

end.

