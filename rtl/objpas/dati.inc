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

function DoEncodeDate(Year, Month, Day: Word): longint;
var
  c, ya: cardinal;
begin
if (Month > 0) and (Month < 13) and (Day > 0) and (Day < 32) then
   begin
     if month > 2 then Month := Month - 3 else
     begin
       Month := Month + 9;
       Year:= Year - 1;
     end;
     c:= Year DIV 100;
     ya:= Year - 100*c;
     result := (146097*c) SHR 2 + (1461*ya) SHR 2 + (153*Month+2) DIV 5 + Day -
693901;
   end else result:=0;
end;

function DoEncodeTime(Hour, Minute, Second, MilliSecond: word): longint;
begin
If ((hour>=0) and (Hour<24)) and
   ((Minute>=0) and (Minute<60)) and
   ((Second>=0) and (Second<60)) and
   ((MilliSecond>=0) and (Millisecond<1000)) then
  Result := (Hour * 3600000 + Minute * 60000 + Second * 1000 + MilliSecond)
else
  Result:=0;
end ;

{==============================================================================}
{   Public functions                                                           }
{==============================================================================}

{   DateTimeToTimeStamp converts DateTime to a TTimeStamp   }

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;


begin
result.Time := Trunc(Frac(DateTime) * MSecsPerDay);
result.Date := 1
               + DateDelta
               + Trunc(System.Int(DateTime));
end ;

{   TimeStampToDateTime converts TimeStamp to a TDateTime value   }

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
begin
result := (TimeStamp.Date - DateDelta - 1) + (TimeStamp.Time / MSecsPerDay);
end ;

{   MSecsToTimeStamp   }

function MSecsToTimeStamp(MSecs: comp): TTimeStamp;
begin
  result.Date := trunc(msecs / msecsperday);
  msecs:= msecs - comp(result.date * msecsperday);
  result.Time := Trunc(MSecs);
end ;

{   TimeStampToMSecs   }

function TimeStampToMSecs(const TimeStamp: TTimeStamp): comp;
begin
  result := TimeStamp.Time + timestamp.date*msecsperday;
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
var
  j : cardinal;
begin
  j := pred((Trunc(System.Int(Date)) + 693901) SHL 2);
  Year:= j DIV 146097;
  j:= j - 146097 * Year;
  Day := j SHR 2;
  j:=(Day SHL 2 + 3) DIV 1461;
  Day:= (Day SHL 2 + 7 - 1461*j) SHR 2;
  Month:=(5 * Day-3) DIV 153;
  Day:= (5 * Day +2 - 153*Month) DIV 5;
  Year:= 100 * Year + j;
  if Month < 10 then Month:= Month + 3 else begin
    Month:= Month-9;
    inc(Year);
  end;
end ;

{   DecodeTime unpacks Time into four values:
    Hour, Minute, Second and MilliSecond    }

procedure DecodeTime(Time: TDateTime; var Hour, Minute, Second, MilliSecond: word);
Var
  l : cardinal;
begin
 l := Trunc(Frac(time) * MSecsPerDay);
 Hour   := l div 3600000;
 l := l mod 3600000;
 Minute := l div 60000;
 l := l mod 60000;
 Second := l div 1000;
 l := l mod 1000;
 MilliSecond := l;
end;

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
    S : Integer;

begin
If NumberOfMonths>=0 then s:=1 else s:=-1;
DecodeDate(DateTime, Year, Month, Day);
Year := Year + (NumberOfMonths div 12);
Month := Month + (NumberOfMonths mod 12)-1 ; // Mod result always positive
if Month>11 then begin
   Dec(Month, S*12);
   Inc(Year, S);
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
   d,m,y:word;
   n,i:longint;
   c:word;
   dp,mp,yp,which : Byte;
   s1:string[4];
   values:array[0..2] of longint;
   LocalTime:tsystemtime;

begin
df := UpperCase(ShortDateFormat);
{ Determine order of D,M,Y }
Which:=0;
For I:=1 to Length(df) do
  Case df[i] of
    'Y' : yp:=which;
    'M' : mp:=which;
    'D' : dp:=which;
  else
    If df[i]=DateSeparator then
     Inc(Which);
  end;
{ Get actual values }
for i := 0 to 2 do values[i] := 0;
s1 := '';
n := 0;
for i := 1 to length(s) do begin
   if (s[i] in ['0'..'9']) then s1 := s1 + s[i];
   if (s[i] in [dateseparator,' ']) or (i = length(s)) then begin
      val(s1, values[n], c);
      if c<>0 then
        Raise EConvertError.Create('Invalid date format');
      s1 := '';
      inc(n);
      end ;
   end ;
// Fill in values.
If N=3 then
  begin
  y:=values[yp];
  m:=values[mp];
  d:=values[dp];
  end
Else
  begin
  getLocalTime(LocalTime);
  y := LocalTime.Year;
  If n<2 then
    begin
    d:=values[0];
    m := LocalTime.Month;
    end
  else
    If dp<mp then
      begin
      d:=values[0];
      m:=values[1];
      end
    else
      begin
      d:=values[1];
      m:=values[0];
      end;
  end;
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
   var
     j: integer; c: word;
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
      else
        raise EConvertError.Create('Invalid Time format');
      end ;
   end ;

var
   i: integer;
   TimeValues: array[0..4] of integer;

begin
Current := 0;
Len := length(s);
PM := False;
for i:=0 to 4 do
  timevalues[i]:=0;
i := 0;
TimeValues[i] := GetElement;
while (i < 5) and (TimeValues[i] <> -1) do begin
   i := i + 1;
   TimeValues[i] := GetElement;
   end ;
If (i<5) and (TimeValues[I]=-1) then
  TimeValues[I]:=0;
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

function FormatDateTime(FormatStr: string; DateTime: TDateTime): string;
var
   ResultLen: integer;
   ResultBuffer: array[0..255] of char;
   ResultCurrent: pchar;

   procedure StoreStr(Str: pchar; Len: integer);
   begin
   if ResultLen + Len < SizeOf(ResultBuffer) then begin
      StrMove(ResultCurrent, Str, Len);
      ResultCurrent := ResultCurrent + Len;
      ResultLen := ResultLen + Len;
      end ;
   end ;

   procedure StoreString(const Str: string);
   var Len: integer;
   begin
   Len := Length(Str);
   if ResultLen + Len < SizeOf(ResultBuffer) then begin
      StrMove(ResultCurrent, pchar(Str), Len);
      ResultCurrent := ResultCurrent + Len;
      ResultLen := ResultLen + Len;
      end;
   end;

   procedure StoreInt(Value, Digits: integer);
   var S: string; Len: integer;
   begin
   S := IntToStr(Value);
   Len := Length(S);
   if Len < Digits then begin
      S := copy('0000', 1, Digits - Len) + S;
      Len := Digits;
      end ;
   StoreStr(pchar(@S[1]), Len);
   end ;

   Function TimeReFormat(Const S : string) : string;
   // Change m into n for time formatting.
   Var i : longint;

   begin
     Result:=S;
     For I:=1 to Length(Result) do
       If Result[i]='m' then
         result[i]:='n';
   end;

var
   Year, Month, Day, DayOfWeek, Hour, Minute, Second, MilliSecond: word;

   procedure StoreFormat(const FormatStr: string);
   var
      Token: char;
      FormatCurrent: pchar;
      FormatEnd: pchar;
      Count: integer;
      Clock12: boolean;
      P: pchar;

   begin
   FormatCurrent := Pchar(FormatStr);
   FormatEnd := FormatCurrent + Length(FormatStr);
   Clock12 := false;
   P := FormatCurrent;
   while P < FormatEnd do begin
      Token := UpCase(P^);
      if Token in ['"', ''''] then begin
         P := P + 1;
         while (P < FormatEnd) and (P^ <> Token) do
            P := P + 1;
         end
      else if Token = 'A' then begin
         if (StrLIComp(P, 'A/P', 3) = 0) or
            (StrLIComp(P, 'AMPM', 4) = 0) or
            (StrLIComp(P, 'AM/PM', 5) = 0) then begin
            Clock12 := true;
            break;
            end ;
         end ;
      P := P + 1;
      end ;
   while FormatCurrent < FormatEnd do begin
      Token := UpCase(FormatCurrent^);
      Count := 1;
      P := FormatCurrent + 1;
         case Token of
            '''', '"': begin
               while (P < FormatEnd) and (p^ <> Token) do
                  P := P + 1;
               P := P + 1;
               Count := P - FormatCurrent;
               StoreStr(FormatCurrent + 1, Count - 2);
               end ;
            'A': begin
               if StrLIComp(FormatCurrent, 'AMPM', 4) = 0 then begin
                  Count := 4;
                  if Hour < 12 then StoreString(TimeAMString)
                  else StoreString(TimePMString);
                  end
               else if StrLIComp(FormatCurrent, 'AM/PM', 5) = 0 then begin
                  Count := 5;
                  if Hour < 12 then StoreStr('am', 2)
                  else StoreStr('pm', 2);
                  end
               else if StrLIComp(FormatCurrent, 'A/P', 3) = 0 then begin
                  Count := 3;
                  if Hour < 12 then StoreStr('a', 1)
                  else StoreStr('p', 1);
                  end
               else
                 Raise EConvertError.Create('Illegal character in format string');
               end ;
            '/': StoreStr(@DateSeparator, 1);
            ':': StoreStr(@TimeSeparator, 1);
            ' ', 'C', 'D', 'H', 'M', 'N', 'S', 'T', 'Y': begin
               while (P < FormatEnd) and (UpCase(P^) = Token) do
                  P := P + 1;
               Count := P - FormatCurrent;
                  case Token of
                     ' ': StoreStr(FormatCurrent, Count);
                     'Y': begin
                           case Count of
                              1: StoreInt(Year, 0);
                              2: StoreInt(Year mod 100, 2);
                              4: StoreInt(Year, 4);
                           end ;
                        end ;
                     'M': begin
                           case Count of
                              1: StoreInt(Month, 0);
                              2: StoreInt(Month, 2);
                              3: StoreString(ShortMonthNames[Month]);
                              4: StoreString(LongMonthNames[Month]);
                           end ;
                        end ;
                     'D': begin
                           case Count of
                              1: StoreInt(Day, 0);
                              2: StoreInt(Day, 2);
                              3: StoreString(ShortDayNames[DayOfWeek]);
                              4: StoreString(LongDayNames[DayOfWeek]);
                              5: StoreFormat(ShortDateFormat);
                              6: StoreFormat(LongDateFormat);
                           end ;
                        end ;
                     'H': begin
                        if Clock12 then begin
                           if Count = 1 then StoreInt(Hour mod 12, 0)
                           else StoreInt(Hour mod 12, 2);
                           end
                        else begin
                           if Count = 1 then StoreInt(Hour, 0)
                           else StoreInt(Hour, 2);
                           end ;
                        end ;
                     'N': begin
                        if Count = 1 then StoreInt(Minute, 0)
                        else StoreInt(Minute, 2);
                        end ;
                     'S': begin
                        if Count = 1 then StoreInt(Second, 0)
                        else StoreInt(Second, 2);
                        end ;
                     'T': begin
                        if Count = 1 then StoreFormat(timereformat(ShortTimeFormat))
                        else StoreFormat(TimeReformat(LongTimeFormat));
                        end ;
                     'C': StoreFormat(ShortDateFormat + ' ' + TimeReformat(ShortTimeFormat));
                  end ;
               end ;
            else
              Raise EConvertError.Create('Illegal character in format string');
         end ;
      FormatCurrent := FormatCurrent + Count;
      end ;
   end ;

begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minute, Second, MilliSecond);
  DayOfWeek := SysUtils.DayOfWeek(DateTime);
  ResultLen := 0;
  ResultCurrent := @ResultBuffer;
  StoreFormat(FormatStr);
  ResultBuffer[ResultLen] := #0;
  result := StrPas(@ResultBuffer);
end ;

{   DateTimeToString formats DateTime to the given format in FormatStr   }

procedure DateTimeToString(var Result: string; const FormatStr: string; const DateTime: TDateTime);
begin
  Result := FormatDateTime(FormatStr, DateTime);
end ;


Function DateTimeToFileDate(DateTime : TDateTime) : Longint;

Var YY,MM,DD,H,m,s,msec : Word;

begin
  Decodedate (DateTime,YY,MM,DD);
  If (YY<1980) or (YY>2099) then
    Result:=0
  else
    begin
    DecodeTime (DateTime,h,m,s,msec);
    Result:=(s shr 1) or (m shl 5) or (h shl 11);
    Result:=Result or DD shl 16 or (MM shl 21) or ((YY-1980) shl 25);
    end;
end;


Function FileDateToDateTime (Filedate : Longint) : TDateTime;

Var Date,Time : Word;

begin
  Date:=FileDate shl 16;
  Time:=FileDate and $ffff;
  Result:=EncodeDate((Date shr 9) + 1980,(Date shr 5) and 15, Date and 31) +
          EncodeTime(Time shr 11, (Time shr 5) and 63, (Time and 31) shl 1,0);
end;

{
  $Log$
  Revision 1.15  1999-07-24 11:21:14  peter
    * fixed encode/decode date/time

  Revision 1.14  1999/07/14 08:47:54  michael
  * faster En/Decodedate routines from Frank Reichert
  * Fixed FormatDateTime with short/longtimeformat.

  Revision 1.13  1999/05/31 20:50:44  peter
    * removed warnings

  Revision 1.12  1999/05/13 21:51:41  michael
  * several fixes

  Revision 1.11  1999/05/11 09:05:13  michael
  * SMall fixes to date/time routines

  Revision 1.10  1999/04/18 19:03:03  michael
  + Now EConvertError is used everywhere in conversions

  Revision 1.9  1999/04/08 11:31:02  peter
    * removed warnings

  Revision 1.8  1999/02/24 15:56:28  michael
  + Small fixes. Moved getlocaltime to system-dependent files

  Revision 1.7  1999/02/10 22:15:10  michael
  + Changed to ansistrings

  Revision 1.6  1999/02/09 12:38:42  michael
  * Fixed INt() proble. Defined THandle, included Filemode constants

  Revision 1.5  1998/10/15 09:39:12  michael
  Changes from Gretjan Schouten

  Revision 1.4  1998/10/11 13:40:52  michael
  + Added Conversion TDateTime <-> file date and time

  Revision 1.3  1998/09/16 08:28:36  michael
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

