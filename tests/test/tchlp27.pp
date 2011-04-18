{ %FAIL }

{ a class helper must extend a subclass of the parent class helper }
program tchlp27;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest1 = class

  end;

  TTest1Helper = class helper for TTest1
  end;

  TTest2 = class

  end;

  TTest2Helper = class helper(TTest1Helper) for TTest2
  end;

begin
end.
