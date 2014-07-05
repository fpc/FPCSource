{ %FAIL }

program tb241;

type
  TTest = record
    f: array[0..1] of TTest;
  end;

begin

end.
