{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2015 by Florian Klaempfl

    This unit contain procedures specific for an extended pascal compatible mode.
    It should be platform independent.

    See the file COPYING.FPC, included in this distribution,
    For details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit extpas;

  interface

    type
      TimeStamp = packed record
        DateValid : Boolean;
        TimeValid : Boolean;
        year : Integer;
        month : 1..12;
        day : 1..31;
        hour : 0..23;
        minute : 0..59;
        second : 0..59;
        { implementation specific }
        second100 : 0..99;
      end;

    procedure GetTimeStamp(var ts : TimeStamp);

  implementation

    uses
      dos;

    procedure GetTimeStamp(var ts : TimeStamp);
      var
        year1,month1,mday1,wday1,
        year2,month2,mday2,wday2,
        hour,minute,second,sec100 : word;
      begin
        repeat
          GetDate(year1,month1,mday1,wday1);
          GetTime(hour,minute,second,sec100);
          GetDate(year2,month2,mday2,wday2);
        { the date may not have changed while we read the time }
        until (year1=year2) and (month1=month2) and (mday1=mday2);
        ts.DateValid:=true;
        ts.TimeValid:=true;
        ts.year:=year1;
        ts.month:=month1;
        ts.day:=mday1;
        ts.hour:=hour;
        ts.minute:=minute;
        ts.second:=second;
        ts.second100:=sec100;
      end;


end.
