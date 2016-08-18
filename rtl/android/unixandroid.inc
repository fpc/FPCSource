{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Yury Sidorov,
    member of the Free Pascal development team.

    Android-specific part of the System unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

type
   Ptm = ^tm;
   tm = record
        tm_sec : longint;
        tm_min : longint;
        tm_hour : longint;
        tm_mday : longint;
        tm_mon : longint;
        tm_year : longint;
        tm_wday : longint;
        tm_yday : longint;
        tm_isdst : longint;
        case boolean of
          false : (tm_gmtoff : longint;tm_zone : Pchar);
          true  : (__tm_gmtoff : longint;__tm_zone : Pchar);
	 end;

function localtime(t: PLongInt): Ptm; cdecl; external 'c' name 'localtime';

var
  c_tzname: array[0..1] of PAnsiChar; external 'c' name 'tzname';
  c_timezone: longint; external 'c' name 'timezone';
  c_daylignt: shortint; external 'c' name 'daylight';

procedure InitLocalTime;
var
  t: longint;
  tt: Ptm;
begin
  t:=fptime;
  tt:=localtime(@t);
  tzname[false]:=c_tzname[0];
  tzname[true]:=c_tzname[1];
  if tt <> nil then
    begin
      tzdaylight:=tt^.tm_isdst <> 0;
      tzseconds:=tt^.tm_gmtoff;
    end
  else
    begin
      tzdaylight:=c_daylignt <> 0;
      tzseconds:=-c_timezone;
    end;
end;
