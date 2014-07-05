{ %FAIL }

program tb242;

type
  TTest = record
    f: array[0..1] of array[0..1] of TTest;
  end;

begin

end.
