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
  UErrorCode = SizeInt;
  int32_t = longint;
  uint32_t = longword;
  UBool = LongBool;

  UCalendar = pointer;
  UCalendarType = longint;
  UCalendarDisplayNameType = longint;
  UCalendarDateFields = longint;

const
  UCAL_STANDARD = 0;
  UCAL_SHORT_STANDARD = 1;
  UCAL_DST = 2;
  UCAL_SHORT_DST = 3;

  UCAL_ZONE_OFFSET = 15;
  UCAL_DST_OFFSET  = 16;

var
  ucal_open: function (zoneID: PUnicodeChar; len: int32_t; locale: PAnsiChar; ctype: UCalendarType; var status: UErrorCode): UCalendar; cdecl;
  ucal_close: procedure (cal: UCalendar); cdecl;
  ucal_getTimeZoneDisplayName: function (cal: UCalendar; dtype: UCalendarDisplayNameType; locale: PAnsiChar; result: PUnicodeChar; resultLength: int32_t;
                                         var status: UErrorCode): int32_t; cdecl;
  ucal_inDaylightTime: function (cal: UCalendar; var status: UErrorCode): UBool; cdecl;
  ucal_get: function (cal: UCalendar; field: UCalendarDateFields; var status: UErrorCode): int32_t; cdecl;

var
  TZStandardName: utf8string;
  TZDaylightName: utf8string;

function GetIcuProc(const Name: AnsiString; var ProcPtr; libId: longint): boolean; external name 'CWSTRING_GET_ICU_PROC';

procedure ReadTimeZoneFromICU;
var
  locale: utf8string;
  tz: unicodestring;
  res: unicodestring;
  err: UErrorCode;
  cal: UCalendar;
begin
  if not GetIcuProc('ucal_open', ucal_open, 1) then exit;
  if not GetIcuProc('ucal_close', ucal_close, 1) then exit;
  if not GetIcuProc('ucal_getTimeZoneDisplayName', ucal_getTimeZoneDisplayName, 1) then exit;
  if not GetIcuProc('ucal_inDaylightTime', ucal_inDaylightTime, 1) then exit;
  if not GetIcuProc('ucal_get', ucal_get, 1) then exit;

  locale:='en_US';
  tz:=unicodestring(GetSystemProperty('persist.sys.timezone'));
  err:=0;
  cal:=ucal_open(PUnicodeChar(tz), Length(tz), PAnsiChar(locale), 0, err);
  if cal = nil then
    exit;
  tzdaylight:=ucal_inDaylightTime(cal, err);

  SetLength(res, 200);
  SetLength(res, ucal_getTimeZoneDisplayName(cal, UCAL_SHORT_STANDARD, PAnsiChar(locale), PUnicodeChar(res), Length(res), err));
  TZStandardName:=utf8string(res);
  tzname[False]:=PAnsiChar(TZStandardName);

  SetLength(res, 200);
  SetLength(res, ucal_getTimeZoneDisplayName(cal, UCAL_SHORT_DST, PAnsiChar(locale), PUnicodeChar(res), Length(res), err));
  TZDaylightName:=utf8string(res);
  tzname[True]:=PAnsiChar(TZDaylightName);

  Tzseconds:=ucal_get(cal, UCAL_ZONE_OFFSET, err) div 1000;
  if tzdaylight then
    Tzseconds:=Tzseconds + ucal_get(cal, UCAL_DST_OFFSET, err) div 1000;

  ucal_close(cal);
end;

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

procedure ReadTimeZoneFromLibC;
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
    end;
end;

procedure InitLocalTime;
begin
  if SystemApiLevel > 10 then
    ReadTimeZoneFromLibC;
  // If cuurent Android version is too old and does not support timezone
  // in libc, use ICU library.
  if tzname[false] = nil then
    ReadTimeZoneFromICU;
end;
