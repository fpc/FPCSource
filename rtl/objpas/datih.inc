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


const
   SecsPerDay = 24 * 60 * 60; // Seconds and milliseconds per day
   MSecsPerDay = SecsPerDay * 1000;

   DateDelta = 693594;        // Days between 1/1/0001 and 12/31/1899

   TwoDigitYearCenturyWindow : word=0;
                             { Threshold to be subtracted from year before
                               age-detection.}

   {  date time formatting characters:
      c      : shortdateformat + ' ' + shorttimeformat
      d      : day of month
      dd     : day of month (leading zero)
      ddd    : day of week (abbreviation)
      dddd   : day of week (full)
      ddddd  : shortdateformat
      dddddd : longdateformat
      m      : month
      mm     : month (leading zero)
      mmm    : month (abbreviation)
      mmmm   : month (full)
      y      : year (four digits)
      yy     : year (two digits)
      yyyy   : year (with century)
      h      : hour
      hh     : hour (leading zero)
      n      : minute
      nn     : minute (leading zero)
      s      : second
      ss     : second (leading zero)
      t      : shorttimeformat
      tt     : longtimeformat
      am/pm  : use 12 hour clock and display am and pm accordingly
                a/p    : use 12 hour clock and display a and p accordingly
      /      : insert date seperator
      :      : insert time seperator
      "xx"   : literal text
      'xx'   : literal text
   }

type
{$ifndef win32}
   { Win32 reuses the struct from the Windows unit }
   TSystemTime = record
      Year, Month, Day: word;
      Hour, Minute, Second, MilliSecond: word;
   end ;
{$endif win32}

   TTimeStamp = record
      Time: integer;   { Number of milliseconds since midnight }
      Date: integer;   { One plus number of days since 1/1/0001 }
   end ;

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
function MSecsToTimeStamp(MSecs: Comp): TTimeStamp;
function TimeStampToMSecs(const TimeStamp: TTimeStamp): comp;
function EncodeDate(Year, Month, Day :word): TDateTime;
function EncodeTime(Hour, Minute, Second, MilliSecond:word): TDateTime;
procedure DecodeDate(Date: TDateTime; var Year, Month, Day: word);
procedure DecodeTime(Time: TDateTime; var Hour, Minute, Second, MilliSecond: word);
procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);
function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
function DayOfWeek(DateTime: TDateTime): integer;
function Date: TDateTime;
function Time: TDateTime;
function Now: TDateTime;
function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer): TDateTime;
function IsLeapYear(Year: Word): boolean;
function DateToStr(Date: TDateTime): string;
function TimeToStr(Time: TDateTime): string;
function DateTimeToStr(DateTime: TDateTime): string;
function StrToDate(const S: string): TDateTime;
function StrToTime(const S: string): TDateTime;
function StrToDateTime(const S: string): TDateTime;
function FormatDateTime(FormatStr: string; DateTime: TDateTime):string;
procedure DateTimeToString(var Result: string; const FormatStr: string; const DateTime: TDateTime);
Function DateTimeToFileDate(DateTime : TDateTime) : Longint;
Function FileDateToDateTime (Filedate : Longint) :TDateTime;

{ FPC Extra }
Procedure GetLocalTime(var SystemTime: TSystemTime);

{
  $Log$
  Revision 1.6  2002-10-02 21:04:06  peter
    * For win32 use the tsystemtime from the windows unit, that is changed
      to a variant record with compatibile field names

  Revision 1.5  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

}
