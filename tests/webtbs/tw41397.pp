program Project1;

{$mode objfpc}{$H+}

uses
  SysUtils;

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    // I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    //  = 2024 * 365 + 2024 div 4 - 2024 div 100 + 2024 div 400 + 244 - 693594
    //  = 738760     + 506        - 20           + 5            + 244 - 693594
    //  = 45901
    // 45901 is correct, but get 45900
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Writeln('   It should output here: 4.5901000000000000E+004');
    Writeln('But the actual output is:', Date);
    Result := abs(Date-45901)<0.1;
  end;
end;

var
  LDateTime: TDateTime;
  res : boolean;
begin
  res:=TryEncodeDate(2025, 09, 01, LDateTime);
  Writeln('TryEncodeDate:', DateTimeToStr(LDateTime));
  if not res then
    begin
      Writeln('Problem detected');
      Halt(1);
    end;
end.

