{ %FAIL }

program tb0225;

{$modeswitch advancedrecords}

type
  TTest = record
  public type
    TTestSub = record
      f: TTest;
    end;
  public
    f: array of TTestSub;
  end;

begin

end.
