{ %NORUN }

{ although records can't have (strict) protected, record helpers can }
program trhlp20;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
  strict protected
    procedure Test;
  end;

procedure TTestHelper.Test;
begin

end;

begin
end.
