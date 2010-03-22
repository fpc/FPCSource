uses
  dateutils;
var
  date1,
  date2: tdatetime;
  jdate: double;
begin
  date1:=EncodeDateTime(2010,03,22,0,0,0,0);
  date2:=JulianDateToDateTime(2455277.50000);
  if date1<>date2 then
    begin
      writeln(date1:0:12);
      writeln(date2:0:12);
      halt(1);
    end;
  if DateTimeToJulianDate(date2)<>2455277.50000 then
    begin
      writeln(DateTimeToJulianDate(date2):0:5);
      writeln(2455277.50000:0:5);
      halt(2);
    end;
  jdate:=DateTimeToModifiedJulianDate(date1);
  if ModifiedJulianDateToDateTime(jdate)<>date1 then
    begin
      writeln(jdate:0:12);
      writeln(date1:0:12);
      halt(3);
    end;
end.
