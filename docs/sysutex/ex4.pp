Program Example4;

{ This program demonstrates the DateTimeToString function }

Uses sysutils;


Procedure today (Fmt : string);

Var S : AnsiString;

begin
  DateTimeToString (S,Fmt,Date);
  Writeln (S);
end;

Procedure Now (Fmt : string);

Var S : AnsiString;

begin
  DateTimeToString (S,Fmt,Time);
  Writeln (S);
end;

Begin
  Today ('"Today is "dddd dd mmmm y');
  Today ('"Today is "d mmm yy');
  Today ('"Today is "d/mmm/yy');
  Now ('''The time is ''am/pmh:n:s');
  Now ('''The time is ''hh:nn:ssam/pm');
  Now ('''The time is ''tt');
End.