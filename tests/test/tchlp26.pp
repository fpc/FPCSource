{ %FAIL }

{ a class helper can only inherit from another class helper }
program tchlp26;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class

  end;

  TObjectHelper = class helper(TTest) for TObject
  end;

begin
end.

