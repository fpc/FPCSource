{ %FAIL }

program tb243;

type
  TTest = record
    f: record
      f: TTest;
    end;
  end;

begin

end.
