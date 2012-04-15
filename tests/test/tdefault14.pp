{ %FAIL }

{ As C++ classes aren't fully implemented we disallow Default for them as well }
program tdefault14;

type
  TTest = cppclass
    f: LongInt;
  end;

var
  t: TTest;
begin
  t := Default(TTest);
end.
