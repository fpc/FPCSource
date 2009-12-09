{$r+,q+}

var A : LongWord;

begin
  A := $0FFFFFFF;
  Inc(A, LongWord($F0000000));
  // no runtime error if the above line is changed to:
  // A := A + LongWord($F0000000);
end.
