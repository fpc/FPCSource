Program Example2;
uses Dos;

{ Program to demonstrate the GetDate function. }

const
  DayStr:array[0..6] of string[3]=('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  MonthStr:array[1..12] of string[3]=('Jan','Feb','Mar','Apr','May','Jun',
                                      'Jul','Aug','Sep','Oct','Nov','Dec');
var
  Year,Month,Day,WDay : word;
begin
  GetDate(Year,Month,Day,WDay);
  WriteLn('Current date');
  WriteLn(DayStr[WDay],', ',Day,' ',MonthStr[Month],' ',Year,'.');
end.
