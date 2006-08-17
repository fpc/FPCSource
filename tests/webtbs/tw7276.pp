uses Sysutils;

var
  s : AnsiString;

begin
  s := 'Hello';
  // tests whether the * in the format specifier allows all
  // kind of integers; note that the * in the format specifier here
  // is an incomplete format specification, i.e. it does not change
  // the length of the result string
  Format('%*s', [Integer(length(s)), s]);
  // this is only seemingly equivalent to above, but on 64 bit
  // machines the default integer type is Int64
  Format('%*s', [length(s), s]);
  // also test QWord
  Format('%*s', [QWord(length(s)), s]);  
end.