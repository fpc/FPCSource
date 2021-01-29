{$mode iso}

type
  v = ^x;
  x = record
    n: Integer;
    case b: Boolean OF
      True:  (x0: Real);
      False: (x1, x2: Integer)
  end;

var
  a: v;

begin
  New(a, True);
  Dispose(a, True);
end.