{ %NORUN }

{ although records can't use published members, record helpers can }
program trhlp22;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record

  end;

{$M+}
  TTestHelper = record helper for TTest
  private
    function GetTest: Integer;
  published
    property Test: Integer read GetTest;
  end;
{$M-}

function TTestHelper.GetTest: Integer;
begin

end;

begin
end.
