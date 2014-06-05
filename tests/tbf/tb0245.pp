{ %FAIL }

program tb245;

{$modeswitch advancedrecords}

type
  TTest = record
  type
    TTestSub = record
      f: array[0..1] of TTest;
    end;
  end;

begin

end.
