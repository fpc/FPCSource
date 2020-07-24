{ %RESULT=6 }
{$mode ISO}
program isoModeReadingNumbers(input, output);
var
    i: integer;
begin
 { we cannot call the executable with <&- >&- while running the test suite,
   so render the file handle manually illegal }   
 Textrec(input).handle:=$1234;
 readLn(i);
end.
