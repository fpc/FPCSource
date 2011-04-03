{ %NORUN }

{ class helpers can extend a subclass of the parent's extended class }
program tchlp25;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

  TTest = class
  end;

  TTestHelper = class helper(TObjectHelper) for TTest
  end;

begin

end.

