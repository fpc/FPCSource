Program Example4;

{ Program to demonstrate the LocalToEpoch function. }

Uses oldlinux;

Var year,month,day,hour,minute,second : Word;

begin
  Write ('Year    : ');readln(Year);
  Write ('Month   : ');readln(Month);
  Write ('Day     : ');readln(Day);
  Write ('Hour    : ');readln(Hour);
  Write ('Minute  : ');readln(Minute);
  Write ('Seonds  : ');readln(Second);
  Write ('This is : ');
  Write (LocalToEpoch(year,month,day,hour,minute,second));
  Writeln (' seconds past 00:00 1/1/1980');
end.
