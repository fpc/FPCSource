{ %NORUN }

program tb0590;

{$modeswitch advancedrecords}

type
  TTest = record
  public type
    TTestSub = record
      f: Integer;
    end;
  public
    f: array[0..0] of TTestSub;
  end;

begin

end.
