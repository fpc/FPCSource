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

{ date time functions }

function IsLeapYear(Year: Word): Boolean;
begin
IsLeapYear := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

function DoEncodeDate(Year, Month, Day: Word):longint;
var
   I: Longint;
begin
  DoEncodeDate := 0;
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
     (Day >= 1) and (Day <= 31) then
    begin
      Day := Day + DayTable[IsLeapYear(Year), Month] - 1;
      I := Year - 1;
      DoEncodeDate := I * 365 + I div 4 - I div 100 + I div 400 + Day;
    end;
end;

function  doEncodeTime(Hour,Minute,Second,MilliSecond:word):longint;
begin
  doEncodeTime := (Hour * 3600000 + Minute * 60000 + Second * 1000 + MilliSecond) { div MSecsPerDay} ;
end;

function  DateToStr(Date:TDateTime):string;
begin
  DateToStr := FormatDateTime('c', Date);
end;

function  TimeToStr(Time:TDateTime):string;
begin
  TimeToStr := FormatDateTime('t', Time);
end;

function  DateTimeToStr(DateTime:TDateTime):string;
begin
  DateTimeToStr := FormatDateTime('c t', DateTime);
end;

function  EncodeDate(Year, Month, Day :word):TDateTime;
begin
  EncodeDate := DoEncodeDate(Year, Month, Day);
end;

function  EncodeTime(Hour, Minute, Second, MilliSecond:word):TDateTime;
begin
  EncodeTime := doEncodeTime(hour, minute, second, millisecond) / double(MSecsPerDay);
end;

procedure DecodeDate(Date:TDateTime;var Year:word;var Month:word;var Day:word);
const
   D1 = 365;            { number of days in 1 year }
   D4 = D1 * 4 + 1;     { number of days in 4 years }
   D100 = D4 * 25 - 1;  { number of days in 100 years }
   D400 = D100 * 4 + 1; { number of days in 400 years }
var
   l:longint;
   ly:boolean;
begin
l := Trunc(Int(Date));
year := 1 + 400 * (l div D400); l := (l mod D400);
year := year + 100 * (l div D100);l := (l mod D100);
year := year + 4 * (l div D4);l := (l mod D4);
year := year + (l div D1);l := 1 + (l mod D1);
month := 0;
ly := IsLeapYear(Year);
while (month < 12) and (l > DayTable[ly, month + 1]) do
   inc(month);
day := l - DayTable[ly, month];
end;

procedure DecodeTime(Time:TDateTime;var Hour:word;var Minute:word;var Second:word;var MilliSecond:word);
var l:longint;
begin
l := Trunc(Frac(time) * MSecsPerDay);
Hour   := l div 3600000;l := l mod 3600000;
Minute := l div 60000;l := l mod 60000;
Second := l div 1000;l := l mod 1000;
MilliSecond := l;
end;

function  FormatDateTime(formatstr:string;DateTime:TDateTime):string;
var i:longint;result:string;current:string;e:longint;
    y,m,d,h,n,s,ms:word;
    mDate, mTime:double;
begin
mDate := int(DateTime);
mTime := frac(DateTime);
DecodeDate(mDate, y, m, d);
DecodeTime(mTime, h, n, s, ms);
result := '';
current := '';
i := 1;
e := 0;
while not(i > length(formatstr)) do begin
   while not(formatstr[i] in [' ','"','/',':','''']) and not(i > length(formatstr)) do begin
      current := current + formatstr[i];
      inc(i);
      end;
   if ((current = 'a') or (current = 'am')) and (formatstr[i] = '/') then begin
      inc(i);current := current + '/';
      while not(formatstr[i] in [' ','"','/',':','''']) and not(i > length(formatstr)) do begin
         current := current + formatstr[i];
         inc(i);
         end;
      end;
   if not(current = '') then begin
      if (current = 'c') then begin
         i := 1; result := ''; current := '';
         formatstr := ' ' + shortdateformat + '" "' + shorttimeformat;
         end;
      if not(mTime = 0) then begin
         if (current = 't') then begin
            formatstr := ' ' + shorttimeformat + copy(formatstr, i, length(formatstr));
            i := 1;
            end
         else if (current = 'tt') then begin
            formatstr := ' ' + longtimeformat + copy(formatstr,i,length(formatstr));
            i := 1;
            end
         else if (current = 'h') then result := result + inttostr(h)
         else if (current = 'hh') then result := result + right('0'+inttostr(h),2)
         else if (current = 'n') then result := result + inttostr(n)
         else if (current = 'nn') then result := result + right('0'+inttostr(n),2)
         else if (current = 's') then result := result + inttostr(s)
         else if (current = 'ss') then result := result + right('0'+inttostr(s),2)
         else if (current = 'am/pm') then begin
           if (h < 13) then result := result + 'am'
            else result := result + 'pm';
                           end
         else if (current = 'a/p') then begin
           if h < 13 then result := result + 'a'
            else result := result + 'p';
           end
         else if (current = 'ampm') then begin
            if h < 13 then strCat(result, TimeAMString)
            else strCat(result, TimePMString);
            end;
         end;
      if not(mDate = 0) then begin
         if (current = 'd') then result := result + inttostr(d)
         else if (current = 'dd') then result := result + right('0' + inttostr(d), 2)
         else if (current = 'ddd') then StrCat(result, shortdaynames[DayOfWeek(DateTime)])
         else if (current = 'dddd') then StrCat(result, longdaynames[DayOfWeek(DateTime)])
         else if (current = 'm') then result := result + inttostr(m)
         else if (current = 'mm') then result := result + right('0' + inttostr(m), 2)
         else if (current = 'mmm') then StrCat(result, shortmonthnames[m])
         else if (current = 'mmmm') then StrCat(result, longmonthnames[m])
         else if (current = 'y') then result := result + inttostr(y)
         else if (current = 'yy') then result := result + right(inttostr(y), 2)
         else if (current = 'yyyy') or (current = 'yyy') then result := result + inttostr(y);
         end;
      current := '';
      end;
   if (formatstr[i] = '/') and not(mDate = 0) then result := result + dateseparator
   else if (formatstr[i] = ':') and not(mTime = 0) then result := result + timeseparator
   else if (formatstr[i] in ['"','''']) then begin
      inc(i);
      while not(formatstr[i] in ['"','''']) and not(i > length(formatstr)) do begin
         result := result + formatstr[i];
         inc(i);
         end;
      end;
   inc(i);
   end;
FormatDateTime := Result;
end;

function  StrToDate(const s:string):TDateTime;
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
      end;
   end;
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
   end;
if (n < 3) then begin
   getLocalTime(LocalTime);
   y := LocalTime.wYear;
   if (n < 2) then
      m := LocalTime.wMonth;
   end;
if (y >= 0) and (y < 100) then y := 1900 + y;
StrToDate := DoEncodeDate(y, m, d);
end;


function  StrToTime(const s:string):TDateTime;
begin
end;


function  StrToDateTime(const s:string):TDateTime;
begin
end;


function  DayOfWeek(DateTime:TDateTime):longint;
begin
  DayOfWeek := (1 + Trunc(DateTime)) mod 7;
end;

procedure getlocaltime(var systemtime:tsystemtime);
var
  wDayOfWeek:word;
begin
  getdate(systemtime.wYear,systemtime.wMonth,systemtime.wDay,wDayOfWeek);
  gettime(systemtime.whour,systemtime.wminute,systemtime.wsecond,systemtime.wmillisecond);
  systemtime.wmillisecond := systemtime.wmillisecond * 10;
end;


function  Date:TDateTime;
var
  systemtime:tsystemtime;
begin
  getlocaltime(systemtime);
  date := doEncodeDate(systemtime.wYear,systemtime.wMonth,systemtime.wDay);
end;


function  Time:TDateTime;
var
  systemtime:tsystemtime;
begin
  getlocaltime(systemtime);
  time := doEncodeTime(systemtime.wHour,systemtime.wMinute,
            systemtime.wSecond,systemtime.wMillisecond) / double(MSecsPerDay);
end;


function  Now:TDateTime;
var
  systemtime:tsystemtime;
begin
  getlocaltime(systemtime);
  now := doEncodeDate(systemtime.wYear,systemtime.wMonth,systemtime.wDay) +
         doEncodeTime(systemtime.wHour,systemtime.wMinute,
           systemtime.wSecond,systemtime.wMillisecond) / double(MSecsPerDay);
end;

{
  $Log$
  Revision 1.2  1998-09-09 15:29:04  peter
    * removed some warnings

  Revision 1.1  1998/04/10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

}