{ %FAIL }

program tb244;

{$modeswitch advancedrecords}

type
  TTest = record
  type
    TTestSub = record
      f: TTest;
    end;
  end;

begin

end.
