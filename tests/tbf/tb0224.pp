{ %FAIL }

program tb0224;

type
  TTest = record
    f: array[0..0] of TTest;
  end;

begin

end.
