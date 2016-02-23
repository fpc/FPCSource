{$mode extendedpascal}

var
  ts : TimeStamp;

begin
  { just test if the functions exist }
  GetTimeStamp(ts);
  if ts.DateValid then
    begin
      writeln(ts.year);
      writeln(ts.month);
      writeln(ts.day);
    end;
  if ts.TimeValid then
    begin
      writeln(ts.hour);
      writeln(ts.minute);
      writeln(ts.second);
    end;
end.

