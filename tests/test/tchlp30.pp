{ %FAIL }

{ helpers must not override virtual methods of the extended class }
program tchlp30;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    function Test: Integer; virtual;
  end;

  TTestHelper = class helper for TTest
    function Test: Integer; override;
  end;

function TTest.Test: Integer;
begin

end;

function TTestHelper.Test: Integer;
begin

end;

begin

end.
