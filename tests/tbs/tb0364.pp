uses
   sysutils;

{ comment by submitter:
  The following statement (which works in Delphi)
    result:=Format('%10.n', [ival*1.0]);
  generated an unhandled exception (and said: Missing argument in format "").
  Checking the Delphi documentation, it agrees with the fpc documentation
  (units.pdf), that a dot should be followed by a <prec> (but Delphi does
  not appear to explicitly state that prec should be an integer).
  It appears that Delphi is treating this like %10.0n, although it is
  potentially undefined behaviour.  The fpc documentation indicates I
  should get an EConversionError exception if there are problems.
  (Actually the documentation may be inconsistent, since it also says
  I may get an EConvertError exception.)

  If I change the format string to %10.0n, the program runs OK using
  fpc, however, my thousand separators do not appear.
}

var
   s : string;
   ival : integer;

begin
   ThousandSeparator:='.';
   DecimalSeparator:=',';
   ival:=1234;
   s:=Format('%10.n', [ival*1.0]);
   writeln('s: "',s,'"');
   if s<>'     1.234' then
     begin
        writeln('Problem with Format');
        halt(1);
     end;
end.
