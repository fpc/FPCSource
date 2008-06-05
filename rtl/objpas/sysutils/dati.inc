{
    *********************************************************************
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

Function DoEncodeDate(Year, Month, Day: Word): longint;

Var
  D : TDateTime;

begin
  If TryEncodeDate(Year,Month,Day,D) then
    Result:=Trunc(D)
  else
    Result:=0;
end;

function DoEncodeTime(Hour, Minute, Second, MilliSecond: word): TDateTime;

begin
  If not TryEncodeTime(Hour,Minute,Second,MilliSecond,Result) then
    Result:=0;
end;

{==============================================================================}
{   Public functions                                                           }
{==============================================================================}

{   ComposeDateTime converts a Date and a Time into one TDateTime   }
function ComposeDateTime(Date,Time : TDateTime) : TDateTime;

begin
  if Date < 0 then Result := trunc(Date) - Abs(frac(Time))
  else Result := trunc(Date) + Abs(frac(Time));
end;

{   DateTimeToTimeStamp converts DateTime to a TTimeStamp   }

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
begin
  result.Time := Round(abs(Frac(DateTime)) * MSecsPerDay);
  result.Date := DateDelta + trunc(DateTime);
end ;

{   TimeStampToDateTime converts TimeStamp to a TDateTime value   }

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
begin
  Result := ComposeDateTime(TimeStamp.Date - DateDelta,TimeStamp.Time / MSecsPerDay)
end;

{   MSecsToTimeStamp   }

function MSecsToTimeStamp(MSecs: comp): TTimeStamp;
begin
  result.Date := Trunc(msecs / msecsperday);
  msecs:= msecs-comp(result.date)*msecsperday;
  result.Time := Round(MSecs);
end ;

{   TimeStampToMSecs   }

function TimeStampToMSecs(const TimeStamp: TTimeStamp): comp;
begin
  result := TimeStamp.Time + comp(timestamp.date)*msecsperday;
end ;

Function TryEncodeDate(Year,Month,Day : Word; Out Date : TDateTime) : Boolean;

var
  c, ya: cardinal;
begin
  Result:=(Year>0) and (Year<10000) and
          (Month in [1..12]) and
          (Day>0) and (Day<=MonthDays[IsleapYear(Year),Month]);
 If Result then
   begin
     if month > 2 then
      Dec(Month,3)
     else
      begin
        Inc(Month,9);
        Dec(Year);
      end;
     c:= Year DIV 100;
     ya:= Year - 100*c;
     Date := (146097*c) SHR 2 + (1461*ya) SHR 2 + (153*cardinal(Month)+2) DIV 5 + cardinal(Day);
     // Note that this line can't be part of the line above, since TDateTime is
     // signed and c and ya are not
     Date := Date - 693900;
   end
end;

function TryEncodeTime(Hour, Min, Sec, MSec:word; Out Time : TDateTime) : boolean;

begin
  Result:=(Hour<24) and (Min<60) and (Sec<60) and (MSec<1000);
  If Result then
    Time:=TDateTime(cardinal(Hour)*3600000+cardinal(Min)*60000+cardinal(Sec)*1000+MSec)/MSecsPerDay;
end;

{   EncodeDate packs three variables Year, Month and Day into a
    TDateTime value the result is the number of days since 12/30/1899   }

function EncodeDate(Year, Month, Day: word): TDateTime;

begin
  If Not TryEncodeDate(Year,Month,Day,Result) then
    Raise EConvertError.CreateFmt('%d-%d-%d is not a valid date specification',
                              [Year,Month,Day]);
end;

{   EncodeTime packs four variables Hour, Minute, Second and MilliSecond into
    a TDateTime value     }

function EncodeTime(Hour, Minute, Second, MilliSecond:word):TDateTime;

begin
  If not TryEncodeTime(Hour,Minute,Second,MilliSecond,Result) then
    Raise EConvertError.CreateFmt('%d:%d:%d.%d is not a valid time specification',
                              [Hour,Minute,Second,MilliSecond]);
end;


{   DecodeDate unpacks the value Date into three values:
    Year, Month and Day   }

procedure DecodeDate(Date: TDateTime; out Year, Month, Day: word);
var
  ly,ld,lm,j : cardinal;
begin
  if Date <= -datedelta then  // If Date is before 1-1-1 then return 0-0-0
    begin
    Year := 0;
    Month := 0;
    Day := 0;
    end
  else
    begin
    j := pred((Trunc(System.Int(Date)) + 693900) SHL 2);
    ly:= j DIV 146097;
    j:= j - 146097 * cardinal(ly);
    ld := j SHR 2;
    j:=(ld SHL 2 + 3) DIV 1461;
    ld:= (cardinal(ld) SHL 2 + 7 - 1461*j) SHR 2;
    lm:=(5 * ld-3) DIV 153;
    ld:= (5 * ld +2 - 153*lm) DIV 5;
    ly:= 100 * cardinal(ly) + j;
    if lm < 10 then
     inc(lm,3)
    else
      begin
        dec(lm,9);
        inc(ly);
      end;
    year:=ly;
    month:=lm;
    day:=ld;
    end;
end;


function DecodeDateFully(const DateTime: TDateTime; out Year, Month, Day, DOW: Word): Boolean;
begin
  DecodeDate(DateTime,Year,Month,Day);
  DOW:=DayOfWeek(DateTime);
  Result:=IsLeapYear(Year);
end;


{   DecodeTime unpacks Time into four values:
    Hour, Minute, Second and MilliSecond    }

procedure DecodeTime(Time: TDateTime; out Hour, Minute, Second, MilliSecond: word);
Var
  l : cardinal;
begin
 l := Round(abs(Frac(time)) * MSecsPerDay);
 Hour   := l div 3600000;
 l := l mod 3600000;
 Minute := l div 60000;
 l := l mod 60000;
 Second := l div 1000;
 l := l mod 1000;
 MilliSecond := l;
end;

{   DateTimeToSystemTime converts DateTime value to SystemTime   }

procedure DateTimeToSystemTime(DateTime: TDateTime; out SystemTime: TSystemTime);
begin
  DecodeDate(DateTime, SystemTime.Year, SystemTime.Month, SystemTime.Day);
  DecodeTime(DateTime, SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond);
end ;

{   SystemTimeToDateTime converts SystemTime to a TDateTime value   }

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
  result := ComposeDateTime(DoEncodeDate(SystemTime.Year, SystemTime.Month, SystemTime.Day),
                            DoEncodeTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond));
end ;

{   DayOfWeek returns the Day of the week (sunday is day 1)  }

function DayOfWeek(DateTime: TDateTime): integer;
begin
  Result := 1 + (Abs(Trunc(DateTime) - 1) mod 7);
end ;

{   Date returns the current Date   }

function Date: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  result := DoEncodeDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
end ;

{   Time returns the current Time   }

function Time: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  Result := DoEncodeTime(SystemTime.Hour,SystemTime.Minute,SystemTime.Second,SystemTime.MilliSecond);
end ;

{   Now returns the current Date and Time    }

function Now: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  result := systemTimeToDateTime(SystemTime);
end;

{   IncMonth increments DateTime with NumberOfMonths months,
    NumberOfMonths can be less than zero   }

function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer = 1 ): TDateTime;
var
  Year, Month, Day : word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  IncAMonth(Year, Month, Day, NumberOfMonths);
  result := ComposeDateTime(DoEncodeDate(Year, Month, Day), DateTime);
end ;

{   IncAMonth is the same as IncMonth, but operates on decoded date  }

procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
var
  TempMonth, S: Integer;
begin
  If NumberOfMonths>=0 then
    s:=1
  else
    s:=-1;
  inc(Year,(NumberOfMonths div 12));
  TempMonth:=Month+(NumberOfMonths mod 12)-1;
  if (TempMonth>11) or
     (TempMonth<0) then
   begin
     Dec(TempMonth, S*12);
     Inc(Year, S);
   end;
  Month:=TempMonth+1;          {   Months from 1 to 12   }
  If (Day>MonthDays[IsLeapYear(Year)][Month]) then
    Day:=MonthDays[IsLeapYear(Year)][Month];
end;

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

{  TimeToStr returns a string representation of Time using LongTimeFormat   }

function TimeToStr(Time: TDateTime): string;
begin
  result := FormatDateTime('tt', Time);
end ;

{   DateTimeToStr returns a string representation of DateTime using LongDateTimeFormat   }

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
   d,m,y,ly:word;
   n,i:longint;
   c:word;
   dp,mp,yp,which : Byte;
   s1:string[4];
   values:array[1..3] of longint;
   LocalTime:tsystemtime;
   YearMoreThenTwoDigits : boolean;
begin
  YearMoreThenTwoDigits := False;
  df := UpperCase(ShortDateFormat);
  { Determine order of D,M,Y }
  yp:=0;
  mp:=0;
  dp:=0;
  Which:=0;
  i:=0;
  while (i<Length(df)) and (Which<3) do
   begin
     inc(i);
     Case df[i] of
       'Y' :
         if yp=0 then
          begin
            Inc(Which);
            yp:=which;
          end;
       'M' :
         if mp=0 then
          begin
            Inc(Which);
            mp:=which;
          end;
       'D' :
         if dp=0 then
          begin
            Inc(Which);
            dp:=which;
          end;
     end;
   end;
  if Which<>3 then
   Raise EConvertError.Create('Illegal format string');
{ Get actual values }
  for i := 1 to 3 do
    values[i] := 0;
  s1 := '';
  n := 0;
  for i := 1 to length(s) do
   begin
     if s[i] in ['0'..'9'] then
      s1 := s1 + s[i];

     { space can be part of the shortdateformat, and is defaultly in slovak
       windows, therefor it shouldn't be taken as separator (unless so specified)
       and ignored }
     if (DateSeparator <> ' ') and (s[i] = ' ') then
       Continue;

     if (s[i] = dateseparator) or ((i = length(s)) and (s[i] in ['0'..'9'])) then
      begin
        inc(n);
        if n>3 then
         Raise EConvertError.Create('Invalid date format');
         // Check if the year has more then two digits (if n=yp, then we are evaluating the year.)
        if (n=yp) and (length(s1)>2) then YearMoreThenTwoDigits := True;
        val(s1, values[n], c);
        if c<>0 then
         Raise EConvertError.Create('Invalid date format');
        s1 := '';
      end
     else if not (s[i] in ['0'..'9']) then
      Raise EConvertError.Create('Invalid date format');
   end ;
  // Fill in values.
  getLocalTime(LocalTime);
  ly := LocalTime.Year;
  If N=3 then
   begin
     y:=values[yp];
     m:=values[mp];
     d:=values[dp];
   end
  Else
  begin
    Y:=ly;
    If n<2 then
     begin
       d:=values[1];
       m := LocalTime.Month;
     end
    else
     If dp<mp then
      begin
        d:=values[1];
        m:=values[2];
      end
    else
      begin
        d:=values[2];
        m:=values[1];
      end;
  end;
  if (y >= 0) and (y < 100) and not YearMoreThenTwoDigits then
    begin
    ly := ly - TwoDigitYearCenturyWindow;
    Inc(Y, ly div 100 * 100);
    if (TwoDigitYearCenturyWindow > 0) and (Y < ly) then
      Inc(Y, 100);
    end;
  Result := EncodeDate(y, m, d);
end ;


{   StrToTime converts the string S to a TDateTime value
    if S does not represent a valid time value an
    EConvertError will be raised   }

function StrToTime(const s: string): TDateTime;
var
   Len, Current: integer; PM: integer;

   function GetElement: integer;
   var
     j, c: integer;
   begin
   result := -1;
   Inc(Current);
   while (result = -1) and (Current < Len) do 
     begin
       if S[Current] in ['0'..'9'] then 
          begin
            j := Current;
            while (Current < Len) and (s[Current + 1] in ['0'..'9']) do
              Inc(Current);
            val(copy(S, j, 1 + Current - j), result, c);
          end
       else if ((TimeAMString<>'') and (S[Current] = TimeAMString[1])) or (S[Current] in ['a', 'A']) then 
          begin
            pm:=1;
            Current := 1 + Len;
          end
       else if ((TimePMString<>'') and (S[Current] = TimePMString[1])) or (S[Current] in ['p', 'P']) then 
         begin
           Current := 1 + Len;
           PM := 2;
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
  PM := 0;
  for i:=0 to 4 do
    timevalues[i]:=0;
  i := 0;
  TimeValues[i] := GetElement;
  while (i < 5) and (TimeValues[i] <> -1) do 
    begin
     i := i + 1;
     TimeValues[i] := GetElement;
   end ;
  If (i<5) and (TimeValues[I]=-1) then
    TimeValues[I]:=0;
  if PM=2 then
    begin
     if (TimeValues[0] <> 12) then
       Inc(TimeValues[0], 12);
    end
  else
    begin
      if (pm=1) and ((TimeValues[0]=12)) then
        TimeValues[0]:=0;
    end;
  result := EncodeTime(TimeValues[0], TimeValues[1], TimeValues[2], TimeValues[3]);
end ;

{   StrToDateTime converts the string S to a TDateTime value
    if S does not represent a valid date and/or time value
    an EConvertError will be raised   }

function StrToDateTime(const s: string): TDateTime;
var
  i, j, k, l: integer;
  sd, st: string;
begin
  l := Length(s);
  i := 1;
  while (i <= l) and (s[i] = ' ') do
    Inc(i);
  j := i;
  while (j <= l) and (s[j] <> ' ') do
    Inc(j);
  k := j;
  while (k <= l) and (s[k] = ' ') do
    Inc(k);
  sd := Copy(s, i, j - i);
  st := Copy(s, k, l);
  if (st = '') and (Pos(TimeSeparator, sd) > 0) then
  begin
    st := sd;
    sd := '';
  end;
  if (sd <> '') and (st <> '') then
    Result := ComposeDateTime(StrToDate(sd), StrToTime(st))
  else if st = '' then
    Result := StrToDate(sd)
  else
    Result := StrToTime(st);
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
   if ResultLen + Len < SizeOf(ResultBuffer) then begin // strmove not safe
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

var
   Year, Month, Day, DayOfWeek, Hour, Minute, Second, MilliSecond: word;

   procedure StoreFormat(const FormatStr: string);
   var
      Token,lastformattoken: char;
      FormatCurrent: pchar;
      FormatEnd: pchar;
      Count: integer;
      Clock12: boolean;
      P: pchar;
      tmp:integer;

   begin
   FormatCurrent := Pchar(pointer(FormatStr));
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
   token:=#255;
   lastformattoken:=' ';
   while FormatCurrent < FormatEnd do
     begin
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
            ' ', 'C', 'D', 'H', 'M', 'N', 'S', 'T', 'Y','Z' :
              begin
                while (P < FormatEnd) and (UpCase(P^) = Token) do
                  P := P + 1;
                Count := P - FormatCurrent;
                case Token of
                   ' ': StoreStr(FormatCurrent, Count);
                   'Y': begin
                         if Count>2 then
                           StoreInt(Year, 4)
                         else
                           StoreInt(Year mod 100, 2);
                        end;
                   'M': begin
                         if lastformattoken='H' then
                           begin
                             if Count = 1 then
                               StoreInt(Minute, 0)
                             else
                               StoreInt(Minute, 2);

                           end
                         else
                           begin
                             case Count of
                                1: StoreInt(Month, 0);
                                2: StoreInt(Month, 2);
                                3: StoreString(ShortMonthNames[Month]);
                                4: StoreString(LongMonthNames[Month]);
                             end;
                           end;
                      end;
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
                         tmp:=hour mod 12;
                         if tmp=0 then tmp:=12;
                         if Count = 1 then StoreInt(tmp, 0)
                         else StoreInt(tmp, 2);
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
                   'Z': begin
                      if Count = 1 then StoreInt(MilliSecond, 0)
                      else StoreInt(MilliSecond, 3);
                      end ;
                   'T': begin
                      if Count = 1 then StoreFormat(ShortTimeFormat)
                      else StoreFormat(LongTimeFormat);
                      end ;
                   'C':
                     begin
                       StoreFormat(ShortDateFormat);
                       if (Hour<>0) or (Minute<>0) or (Second<>0) then
                        begin
                          StoreString(' ');
                          StoreFormat(LongTimeFormat);
                        end;
                     end;
                end;
                lastformattoken:=token;
              end;
            else
              StoreStr(@Token, 1);
         end ;
      FormatCurrent := FormatCurrent + Count;
      end ;
   end ;

begin
  DecodeDateFully(DateTime, Year, Month, Day, DayOfWeek);
  DecodeTime(DateTime, Hour, Minute, Second, MilliSecond);
  ResultLen := 0;
  ResultCurrent := @ResultBuffer[0];
  StoreFormat(FormatStr);
  ResultBuffer[ResultLen] := #0;
  result := StrPas(@ResultBuffer[0]);
end ;

{   DateTimeToString formats DateTime to the given format in FormatStr   }

procedure DateTimeToString(out Result: string; const FormatStr: string; const DateTime: TDateTime);
begin
  Result := FormatDateTime(FormatStr, DateTime);
end ;


Function DateTimeToFileDate(DateTime : TDateTime) : Longint;

Var YY,MM,DD,H,m,s,msec : Word;

begin
  Decodedate (DateTime,YY,MM,DD);
  DecodeTime (DateTime,h,m,s,msec);
{$ifndef unix}
  If (YY<1980) or (YY>2099) then
    Result:=0
  else
    begin
    Result:=(s shr 1) or (m shl 5) or (h shl 11);
    Result:=Result or longint(DD shl 16 or (MM shl 21) or (word(YY-1980) shl 25));
    end;
{$else unix}
  Result:=LocalToEpoch(yy,mm,dd,h,m,s);
{$endif unix}
end;

function CurrentYear:Word;
var yy,mm,dd : word;
begin
  Decodedate(now,yy,mm,dd);
  Result:=yy;
end;

Function FileDateToDateTime (Filedate : Longint) : TDateTime;

{$ifndef unix}
Var Date,Time : Word;

begin
  Date:=FileDate shr 16;
  Time:=FileDate and $ffff;
  Result:=ComposeDateTime(EncodeDate((Date shr 9) + 1980,(Date shr 5) and 15, Date and 31),
          EncodeTime(Time shr 11, (Time shr 5) and 63, (Time and 31) shl 1,0));
end;
{$else unix}
var
  y, mon, d, h, min, s: word;
begin
  EpochToLocal(FileDate,y,mon,d,h,min,s);
  Result:=ComposeDateTime(EncodeDate(y,mon,d),EncodeTime(h,min,s,0));
end;
{$endif unix}

// ieuw. These should  be written to work without exceptions?
function TryStrToDate(const S: string; out Value: TDateTime): Boolean;
  begin
    result:=true;
    try
      value:=StrToDate(s);
    except
      on EConvertError do
        result:=false
    end;
  end;


// function TryStrToDate(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;


function TryStrToTime(const S: string; out Value: TDateTime): Boolean;
  begin
    result:=true;
    try
      value:=StrToTime(s);
    except
      on EConvertError do
        result:=false
    end;
  end;


// function TryStrToTime(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;


function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
  begin
    result:=true;
    try
      value:=StrToDateTime(s);
    except
      on EConvertError do
        result:=false
    end;
  end;


// function TryStrToDateTime(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;


function StrToDateDef(const S: string; const Defvalue : TDateTime): TDateTime;
begin
  if not TryStrToDate(s,Result) Then
    result:=defvalue;
end;

function StrToTimeDef(const S: string; const Defvalue : TDateTime): TDateTime;
begin
  if not TryStrToTime(s,Result) Then
    result:=defvalue;
end;

function StrToDateTimeDef(const S: string; const Defvalue : TDateTime): TDateTime;
begin
  if not TryStrToDateTime(s,Result) Then
    result:=defvalue;
end;

procedure ReplaceTime(var dati:TDateTime; NewTime : TDateTime);inline;
begin
  dati:= ComposeDateTime(dati, newtime);
end;

procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime); inline;
var
  tmp : TDateTime;
begin
  tmp:=NewDate;
  ReplaceTime(tmp,DateTime);
  DateTime:=tmp;
end;

