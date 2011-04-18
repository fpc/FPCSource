{ %NORUN }

{ a helper can already be accessed when implementing a class' methods }
program tchlp34;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    procedure Test;
  end;

  TTestHelper = class helper for TTest
    procedure DoSomething;
  end;

procedure TTest.Test;
begin
  DoSomething;
end;

procedure TTestHelper.DoSomething;
begin

end;

begin
end.
