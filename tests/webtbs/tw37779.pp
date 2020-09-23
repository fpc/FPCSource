{ %NORUN }

program tw37779;

type
  Complex = record
    re : Double;
    im : Double;
  end;
  TComplexArray = array of Complex;
  TComplexArrayArray = array of TComplexArray;

var
  MC: array of array of array of array of TComplexArrayArray;

begin
  MC := nil;
  MC := Copy(MC);
end.
