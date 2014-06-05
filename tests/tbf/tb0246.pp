{ %FAIL }

program tb246;

{$modeswitch advancedrecords}

type
  TTest = record
  type
    TTestSub = record
    type 
      TTestSub2 = record
        f: TTest;
      end;
    end;
  end;

begin

end.
