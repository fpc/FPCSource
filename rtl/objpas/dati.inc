{
    *********************************************************************
    $Id$
    Copyright (C) 1997, 1998 Gertjan Schouten

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************

    System Utilities For Free Pascal
}

{==============================================================================}
{   internal functions                                                         }
{==============================================================================}

const
   DayTable: array[Boolean, 1..12] of longint =
      ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
       (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));

Procedure GetLocalTime(var SystemTime: TSystemTime);
{$IFDEF GO32V2}
var Regs: Registers;
begin
Regs.ah := $2C;
RealIntr($21, Regs);
SystemTime.Hour := Regs.Ch;
SystemTime.Minute := Regs.Cl;
SystemTime.Second := Regs.Dh;
SystemTime.MilliSecond := Regs.Dl;
Regs.ah := $2A;
RealIntr($21, Regs);
SystemTime.Year := Regs.Cx;
SystemTime.Month := Regs.Dh;
SystemTime.Day := Regs.Dl;
end ;
{$ELSE}
{$IFDEF LINUX}
begin
linux.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second);
linux.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
SystemTime.MilliSecond := 0;
end ;
{$ELSE}
begin
end ;
{$ENDIF}
{$ENDIF}

function DoEncodeDate(Year, Month, Day: Word): longint;
var i: longint;
begin
Result := 0;
if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
   (Day >= 1) and (Day <= 31) then begin
   Day := Day + DayTable[IsLeapYear(Year), Month] - 1;
   I := Year - 1;
   result := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
   end ;
end ;

function DoEncodeTime(Hour, Minute, Second, MilliSecond: word): longint;
begin
result := (Hour * 3600000 + Minute * 60000 + Second * 1000 + MilliSecond) { div MSecsPerDay} ;
end ;

{==============================================================================}
{   Public functions                                                           }
{==============================================================================}

{   DateTimeToTimeStamp converts DateTime to a TTimeStamp   }

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
begin
result.Time := Trunc(Frac(DateTime) * MSecsPerDay);
result.Date := 1 + DateDelta + Trunc(Int(DateTime));
end ;

{   TimeStampToDateTime converts TimeStamp to a TDateTime value   }

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
begin
result := (TimeStamp.Date - DateDelta - 1) + (TimeStamp.Time / MSecsPerDay);
end ;

{   MSecsToTimeStamp   }

function MSecsToTimeStamp(MSecs: comp): TTimeStamp;
begin
result.Time := Trunc(MSecs);
result.Date := 0;
end ;

{   TimeStampToMSecs   }

function TimeStampToMSecs(const TimeStamp: TTimeStamp): comp;
begin
result := TimeStamp.Time;
end ;

{   EncodeDate packs three variables Year, Month and Day into a
    TDateTime value the result is the number of days since 12/30/1899   }

function EncodeDate(Year, Month, Day: word): TDateTime;
begin
result := DoEncodeDate(Year, Month, Day);
end ;

{   EncodeTime packs four variables Hour, Minute, Second and MilliSecond into
    a TDateTime value     }

function EncodeTime(Hour, Minute, Second, MilliSecond:word):TDateTime;
begin
Result := DoEncodeTime(hour, minute, second, millisecond) / MSecsPerDay;
end ;

{   DecodeDate unpacks the value Date into three values:
    Year, Month and Day   }

procedure DecodeDate(Date: TDateTime; var Year, Month, Day: word);
const
   D1 = 365;            { number of days in 1 year }
   D4 = D1 * 4 + 1;     { number of days in 4 years }
   D100 = D4 * 25 - 1;  { number of days in 100 years }
   D400 = D100 * 4 + 1; { number of days in 400 years }
var
   i:Longint;
   l:longint;
   ly:boolean;
begin
l := Trunc(Int(Date)) + DateDelta;
year := 1 + 400 * (l div D400); l := (l mod D400);
year := year + 100 * (l div D100);l := (l mod D100);
year := year + 4 * (l div D4);l := (l mod D4);
year := year + (l div D1);l := 1 + (l mod D1);
month := 0;
ly := IsLeapYear(Year);
while (month < 12) and (l > DayTable[ly, month + 1]) do
   inc(month);
day := l - DayTable[ly, month];
end ;

{   DecodeTime unpacks Time into four values:
    Hour, Minute, Second and MilliSecond    }

procedure DecodeTime(Time: TDateTime; var Hour, Minute, Second, MilliSecond: word);
var l:longint;
begin
l := Trunc(Frac(time) * MSecsPerDay);
Hour   := l div 3600000;l := l mod 3600000;
Minute := l div 60000;l := l mod 60000;
Second := l div 1000;l := l mod 1000;
MilliSecond := l;
end ;

{   DateTimeToSystemTime converts DateTime value to SystemTime   }

procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);
begin
DecodeDate(DateTime, SystemTime.Year, SystemTime.Month, SystemTime.Day);
DecodeTime(DateTime, SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond);
end ;

{   SystemTimeToDateTime converts SystemTime to a TDateTime value   }

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
result := DoEncodeDate(SystemTime.Year,
                       SystemTime.Month,
                       SystemTime.Day) +
          DoEncodeTime(SystemTime.Hour,
                       SystemTime.Minute,
                       SystemTime.Second,
                       SystemTime.MilliSecond) / MSecsPerDay;
end ;

{   DayOfWeek returns the Day of the week (sunday is day 1)  }

function DayOfWeek(DateTime: TDateTime): integer;
begin
Result := 1 + (Trunc(DateTime) mod 7);
end ;

{   Date returns the current Date   }

function Date: TDateTime;
var SystemTime: TSystemTime;
begin
GetLocalTime(SystemTime);
result := DoEncodeDate(SystemTime.Year,
                       SystemTime.Month,
                       SystemTime.Day);
end ;

{   Time returns the current Time   }

function Time: TDateTime;
var SystemTime: TSystemTime;
begin
GetLocalTime(SystemTime);
Result := DoEncodeTime(SystemTime.Hour,
                       SystemTime.Minute,
                       SystemTime.Second,
                       SystemTime.MilliSecond) / MSecsPerDay;
end ;

{   Now returns the current Date and Time    }

function Now: TDateTime;
var SystemTime: TSystemTime;
begin
GetLocalTime(SystemTime);
result := DoEncodeDate(SystemTime.Year,
                       SystemTime.Month,
                       SystemTime.Day) +
          DoEncodeTime(SystemTime.Hour,
                       SystemTime.Minute,
                       SystemTime.Second,
                       SystemTime.MilliSecond) / MSecsPerDay;
end ;

{   IncMonth increments DateTime with NumberOfMonths months,
    NumberOfMonths can be less than zero   }

function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer): TDateTime;
var Year, Month, Day: word;
begin
DecodeDate(DateTime, Year, Month, Day);
Month := Month - 1 + NumberOfMonths;      {   Months from 0 to 11   }
Year := Year + (NumberOfMonths div 12);
Month := Month mod 12;
if Month < 0 then begin
   Inc(Month, 12);
   Inc(Year, 1);
   end ;
Inc(Month, 1);                            {   Months from 1 to 12   }
if (Month = 2) and (IsLeapYear(Year)) and (Day > 28) then
   Day := 28;
result := Frac(DateTime) + DoEncodeDate(Year, Month, Day);
end ;

{  IsLeapYear returns true if Year is a leap year   }

function IsLeapYear(Year: Word): boolean;
begin
Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

{  DateToStr returns a string representation of Date using ShortDateFormat   }

function DateToStr(Date: TDateTime): string;
begin
result := FormatDateTime('ddddd', Date);
end ;

{  TimeToStr returns a string representation of Time using ShortTimeFormat   }

function TimeToStr(Time: TDateTime): string;
begin
result := FormatDateTime('t', Time);
end ;

{   DateTimeToStr returns a string representation of DateTime using ShortDateTimeFormat   }

function DateTimeToStr(DateTime: TDateTime): string;
begin
result := FormatDateTime('c', DateTime);
end ;

{   StrToDate converts the string S to a TDateTime value
    if S does not represent a valid date value
    an EConvertError will be raised   }

function StrToDate(const S: string): TDateTime;
var
   df:string;
   d,m,y:word;n,i:longint;c:word;
   s1:string[4];
   values:array[0..2] of longint;
   LocalTime:tsystemtime;
begin
df := UpperCase(ShortDateFormat);
d := 0;m := 0;y := 0;
for i := 0 to 2 do values[i] := 0;
s1 := '';
n := 0;
for i := 1 to length(s) do begin
   if (s[i] in ['0'..'9']) then s1 := s1 + s[i];
   if (s[i] in [dateseparator,' ']) or (i = length(s)) then begin
      val(s1, values[n], c);
      s1 := '';
      inc(n);
      end ;
   end ;
if (df = 'D/M/Y') then begin
   d := values[0];
   m := values[1];
   y := values[2];
   end
else if (df = 'M/D/Y') then begin
   if (n > 1) then begin
      m := values[0];
      d := values[1];
      y := values[2];
      end
   else { if there is just one value, it is the day of the month }
      d := values[0];
   end
else if (df = 'Y/M/D') then begin
   if (n = 3) then begin
      y := values[0];
   	m := values[1];
      d := values[2];
      end
   else if (n = 2) then begin
      m := values[0];
      d := values[1];
      end
   else if (n = 1) then
      d := values[0];
   end ;
if (n < 3) then begin
   getLocalTime(LocalTime);
   y := LocalTime.Year;
   if (n < 2) then
      m := LocalTime.Month;
   end ;
if (y >= 0) and (y < 100) then y := 1900 + y;
Result := DoEncodeDate(y, m, d);
end ;

{   StrToTime converts the string S to a TDateTime value
    if S does not represent a valid time value an
    EConvertError will be raised   }

function StrToTime(const s: string): TDateTime;
var
   Len, Current: integer; PM: boolean;

   function GetElement: integer;
   var i, j: integer; c: word;
   begin
   result := -1;
   Inc(Current);
   while (result = -1) and (Current < Len) do begin
      if S[Current] in ['0'..'9'] then begin
         j := Current;
         while (Current < Len) and (s[Current + 1] in ['0'..'9']) do
            Inc(Current);
         val(copy(S, j, 1 + Current - j), result, c);
         end
      else if (S[Current] = TimeAMString[1]) or (S[Current] in ['a', 'A']) then begin
         Current := 1 + Len;
         end
      else if (S[Current] = TimePMString[1]) or (S[Current] in ['p', 'P']) then begin
         Current := 1 + Len;
         PM := True;
         end
      else if (S[Current] = TimeSeparator) or (S[Current] = ' ') then
         Inc(Current)
      else exit; // raise EConvertError.Create();
      end ;
   end ;

var
   i: integer;
   TimeValues: array[0..4] of integer;

begin
Current := 0;
Len := length(s);
PM := False;
i := 0;
TimeValues[i] := GetElement;
while (i < 5) and (TimeValues[i] <> -1) do begin
   i := i + 1;
   TimeValues[i] := GetElement;
   end ;
if PM then Inc(TimeValues[0], 12);
result := EncodeTime(TimeValues[0], TimeValues[1], TimeValues[2], TimeValues[3]);
end ;

{   StrToDateTime converts the string S to a TDateTime value
    if S does not represent a valid date and time value
    an EConvertError will be raised   }

function StrToDateTime(const s: string): TDateTime;
var i: integer;
begin
i := pos(' ', s);
if i > 0 then result := StrToDate(Copy(S, 1, i - 1)) + StrToTime(Copy(S, i + 1, length(S)))
else result := StrToDate(S);
end ;

{   FormatDateTime formats DateTime to the given format string FormatStr   }

function FormatDateTime(FormatStr: string; DateTime: TDateTime):string;
type
   pstring = ^string;
const
   AP: array[0..1] of char = 'ap';
   TimeAMPMStrings: array[0..1] of pstring = (@TimeAMString, @TimePMString);
var
   i: longint;
   current: string;
   ch: char;
   e: longint;
   y, m, d, h, n, s, ms: word;
   mDate, mTime: double; Clock12: boolean;
begin
mDate := Int(DateTime);
mTime := Frac(DateTime);
DecodeDate(mDate, y, m, d);
DecodeTime(mTime, h, n, s, ms);
result := '';
Clock12 := False;
i := 0;
while i < length(FormatStr) do begin
   i := i + 1;
   if FormatStr[i] = '"' then begin
      i := i + 1;
      while (i < length(FormatStr)) and (FormatStr[i] <> '"') do
         i := i + 1;
      end
   else if FormatStr[i] = '''' then begin
      i := i + 1;
      while (i < length(FormatStr)) and (FormatStr[i] <> '''') do
         i := i + 1;
      end
   else if (copy(FormatStr, i, 3) = 'a/p') then begin
      FormatStr[i] := '"';
      FormatStr[i + 1] := AP[h div 12];
      FormatStr[i + 2] := '"';
      Clock12 := True;
      i := i + 2;
      end
   else if (copy(FormatStr, i, 5) = 'am/pm') then begin
      Delete(FormatStr, i, 5);
      if h < 12 then insert('"' + 'am' + '"', FormatStr, i)
      else insert('"' + 'pm' + '"', FormatStr, i);
      Clock12 := True;
      i := i + 3;
      end
   else if (copy(FormatStr, i, 4) = 'ampm') then begin
      Delete(FormatStr, i, 4);
      current := TimeAMPMStrings[h div 12]^;
      Insert('"' + current + '"', FormatStr, i);
      Clock12 := True;
      i := i + length(current) + 1;
      end
   else if copy(FormatStr, i, 2) = 'tt' then begin
      Delete(FormatStr, i, 2);
      Insert(LongTimeFormat, FormatStr, i);
      i := i - 1;
      end
   else if FormatStr[i] = 't' then begin
      Delete(FormatStr, i, 1);
      Insert(ShortTimeFormat, FormatStr, i);
      i := i - 1;
      end
   else if FormatStr[i] = 'c' then begin
      Delete(FormatStr, i, 1);
      Insert(ShortDateFormat + ' ' + ShortTimeFormat, FormatStr, i);
      i := i - 1;
      end
   else if copy(FormatStr, i, 5) = 'ddddd' then begin
      Delete(FormatStr, i, 5);
      Insert(ShortDateFormat, FormatStr, i);
      i := i - 1;
      end
   else if copy(FormatStr, i, 6) = 'dddddd' then begin
      Delete(FormatStr, i, 6);
      Insert(LongDateFormat, FormatStr, i);
      i := i - 1;
      end ;
   end ;
current := '';
i := 1;
e := 0;
while not(i > length(FormatStr)) do begin
   while not(FormatStr[i] in [' ','"','/',':','''']) and not(i > length(FormatStr)) do begin
      current := current + FormatStr[i];
      inc(i);
      end ;
   if (current <> '') then begin
      if (mTime <> 0) then begin
         if (current = 'h') then begin
            if clock12 then result := result + IntToStr(h mod 12)
				else result := result + IntToStr(h);
            end
         else if (current = 'hh') then begin
            if clock12 then result := result + RightStr('0' + IntToStr(h mod 12), 2)
            else result := result + RightStr('0' + IntToStr(h), 2);
            end
         else if (current = 'n') then result := result + IntToStr(n)
         else if (current = 'nn') then result := result + RightStr('0' + IntToStr(n), 2)
         else if (current = 's') then result := result + IntToStr(s)
         else if (current = 'ss') then result := result + RightStr('0' + IntToStr(s), 2);
         end ;
      if (mDate <> 0) then begin
         if (current = 'd') then result := result + IntToStr(d)
         else if (current = 'dd') then result := result + RightStr('0' + IntToStr(d), 2)
         else if (current = 'ddd') then result := result + ShortDayNames[DayOfWeek(DateTime)]
         else if (current = 'dddd') then result := result + LongDayNames[DayOfWeek(DateTime)]
         else if (current = 'm') then result := result + IntToStr(m)
         else if (current = 'mm') then result := result + RightStr('0' + IntToStr(m), 2)
         else if (current = 'mmm') then result := result + ShortMonthNames[m]
         else if (current = 'mmmm') then result := result + LongMonthNames[m]
         else if (current = 'y') then result := result + IntToStr(y)
         else if (current = 'yy') then result := result + RightStr(IntToStr(y), 2)
         else if (current = 'yyyy') or (current = 'yyy') then result := result + IntToStr(y);
         end ;
      current := '';
      end ;
   if FormatStr[i] = ' ' then result := result + ' '
   else if (FormatStr[i] = '/') and (mDate <> 0) then result := result + DateSeparator
   else if (FormatStr[i] = ':') and (mTime <> 0) then result := result + TimeSeparator
   else if (FormatStr[i] in ['"', '''']) then begin
      ch := FormatStr[i];
      inc(i);
      while (i <= length(FormatStr)) and (FormatStr[i] <> ch) do begin
         result := result + FormatStr[i];
         inc(i);
         end ;
      end ;
   inc(i);
   end ;
end ;

{   DateTimeToString formats DateTime to the given format in FormatStr   }

procedure DateTimeToString(var Result: string; const FormatStr: string; const DateTime: TDateTime);
begin
Result := FormatDateTime(FormatStr, DateTime);
end ;

{
  $Log$
  Revision 1.3  1998-09-16 08:28:36  michael
  Update from gertjan Schouten, plus small fix for linux

  Revision 1.1  1998/04/10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

  1998/08/25 Gertjan
  + uses Go32 instead of Dos unit
    GetLocalTime
    DayOfWeek
    DoDecodeDate
    DoEncodeDate
    FormatDateTime
}

