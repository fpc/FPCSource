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

   DateDelta = 693594; // Days between 1/1/0001 and 12/31/1899
   DateSeparator:char='-';
   TimeSeparator:char=':';
   TimeAMString: pchar = 'am';
   TimePMString: pchar = 'pm';
   ShortMonthNames: array[1..12] of pchar =
     ('jan','feb','mar','apr','mai','jun',
      'jul','aug','sep','oct','nov','dec');
   LongMonthNames: array[1..12] of pchar=
     ('january','february','march','april','mai','june',
      'july','august','september','october','november','december');
   ShortDayNames: array[1..7] of pchar=
     ('sun','mon','tue','wen','thu','fri','sat');
   LongDayNames: array[1..7] of pchar=
     ('sunday','monday','tuesday','wednesday','thursday','friday','saturday');

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

// these constant strings will be changed to pchars too, someday :)
   ShortDateFormat:string='d/m/y';
   LongDateFormat:string='dd" "mmmm" "yyyy';
   ShortTimeFormat:string='hh:nn';
   LongTimeFormat:string='hh:nn:ss';

   Eoln = #10; // or should that be #13, or $0d0a

type
   TSystemTime=record
      wYear:word;wMonth:word;wDay:word;
      wHour:word;wMinute:word;wSecond:word;wMilliSecond:word;
   end ;
   TDateTime = double;

{  Date and Time functions  }

function  DateToStr(Date:TDateTime):string;
function  TimeToStr(Time:TDateTime):string;
function  DateTimeToStr(DateTime:TDateTime):string;
function  EncodeDate(Year, Month, Day :word):TDateTime;
function  EncodeTime(Hour, Minute, Second, MilliSecond:word):TDateTime;
procedure DecodeDate(Date:TDateTime;var Year:word;var Month:word;var Day:word);
procedure DecodeTime(Time:TDateTime;var Hour:word;var Minute:word;var Second:word;var MilliSecond:word);
function  FormatDateTime(FormatStr:string;DateTime:TDateTime):string;
function  StrToDate(const s:string):TDateTime;
function  StrToTime(const s:string):TDateTime;
function  StrToDateTime(const s:string):TDateTime;
function  DayOfWeek(DateTime:TDateTime):longint;
function  Date:TDateTime;
function  Time:TDateTime;
function  Now:TDateTime;
procedure GetLocalTime(var systemtime:tsystemtime);


{

  $Log$
  Revision 1.1  1998-04-10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.


}