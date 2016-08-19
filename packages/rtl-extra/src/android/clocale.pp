{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team.

    Android locale support.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

unit clocale;

{$mode objfpc}

interface

uses
  cwstring, SysUtils;

procedure GetAndroidFormatSettings(var ASettings: TFormatSettings; ALocale: utf8string = '');

implementation

type
  UErrorCode = SizeInt;
  int32_t = longint;
  uint32_t = longword;
  UBool = LongBool;

  UDateFormat = pointer;
  UDateFormatStyle = longint;
  UDate = double;
  UNumberFormat = pointer;
  UNumberFormatStyle = longint;
  UNumberFormatSymbol = longint;

const
  UDAT_NONE   = -1;
  UDAT_FULL   = 0;
  UDAT_LONG   = 1;
  UDAT_MEDIUM = 2;
  UDAT_SHORT  = 3;

  UNUM_DECIMAL_SEPARATOR_SYMBOL = 0;
  UNUM_GROUPING_SEPARATOR_SYMBOL = 1;
  UNUM_CURRENCY_SYMBOL = 8;

var
  FunctionsInited: boolean;

  udat_open: function (timeStyle: UDateFormatStyle; dateStyle: UDateFormatStyle; locale: PAnsiChar; tzID: PUnicodeChar; tzIDLength: int32_t;
                       pattern: PUnicodeChar; patternLength: int32_t; var status: UErrorCode): UDateFormat; cdecl;
  udat_close: procedure (format: UDateFormat); cdecl;
  udat_toPattern: function (format: UDateFormat; localized: UBool; result: PUnicodeChar; resultLength: int32_t; var status: UErrorCode): int32_t; cdecl;
  udat_applyPattern: procedure (format: UDateFormat; localized: UBool; pattern: PUnicodeChar; patternLength: int32_t); cdecl;
  udat_format: function (format: UDateFormat; dateToFormat: UDate; result: PUnicodeChar; resultLength: int32_t; position: pointer; var status: UErrorCode): int32_t; cdecl;

  unum_open: function (style: UNumberFormatStyle; pattern: PUnicodeChar; patternLength: int32_t; locale: PAnsiChar; parseErr: pointer;
                       var status: UErrorCode): UNumberFormat; cdecl;
  unum_close: procedure (fmt: UNumberFormat); cdecl;
  unum_getSymbol: function (fmt: UNumberFormat; symbol: UNumberFormatSymbol; result: PUnicodeChar; resultLength: int32_t; var status: UErrorCode): int32_t; cdecl;

function GetIcuProc(const Name: AnsiString; out ProcPtr; libId: longint = 0): boolean; external name 'CWSTRING_GET_ICU_PROC';

procedure InitIcuFunctions;
begin
  if FunctionsInited then exit;
  if not GetIcuProc('udat_open', udat_open, 1) then exit;
  if not GetIcuProc('udat_close', udat_close, 1) then exit;
  if not GetIcuProc('udat_toPattern', udat_toPattern, 1) then exit;
  if not GetIcuProc('udat_applyPattern', udat_applyPattern, 1) then exit;
  if not GetIcuProc('udat_format', udat_format, 1) then exit;
  if not GetIcuProc('unum_open', unum_open, 1) then exit;
  if not GetIcuProc('unum_close', unum_close, 1) then exit;
  if not GetIcuProc('unum_getSymbol', unum_getSymbol, 1) then exit;

  FunctionsInited:=True;
end;

{$ifdef android}

function GetCurrentLocaleStr: utf8string;
var
  s: utf8string;
begin
  Result:=GetSystemProperty('persist.sys.language');
  if Result = '' then
    exit;
  s:=GetSystemProperty('persist.sys.country');
  if s = '' then
    exit;
  Result:=Result + '_' + s;
end;

{$endif android}

function StrOfChar(c: AnsiChar; Count: integer): utf8string;
begin
  SetLength(Result, Count);
  FillChar(PAnsiChar(Result)^, Count, c);
end;

function ConvertFormatStr(const fmt: utf8string): utf8string;
var
  cnt: integer;
  c, q: AnsiChar;
  p: PAnsiChar;
  s: utf8string;
begin
  Result:='';
  q:=#0;
  cnt:=1;
  p:=PAnsiChar(fmt);
  while p^<>#0 do
    begin
      s:='';
      c:=p^;
      if c in ['''', '"'] then
        begin
          if q=#0 then
            q:=c
          else
            if c=q then
              begin
                q:=#0;
                cnt:=1;
              end;
          s:=c;
        end
      else if q <> #0 then
        s:=c
      else
        begin
          if (p+1)^=c then
            Inc(cnt)
          else
            begin
              case c of
                'y', 'Y':
                  begin
                    c:='y';
                    if cnt > 2 then
                      cnt:=4
                    else
                      cnt:=2;
                  end;
                'M', 'L':
                  begin
                    c:='m';
                    if cnt > 4 then
                      cnt:=3;
                  end;
                'd':
                  if cnt > 2 then
                    cnt:=2;
                'E', 'e', 'c':
                  begin
                    c:='d';
                    if (cnt < 3) or (cnt > 4) then
                      cnt:=3;
                  end;
                'a':
                  begin
                    cnt:=0;
                    s:='ampm';
                  end;
                'h', 'H', 'k', 'K':
                  begin
                    c:='h';
                    if cnt > 2 then
                      cnt:=2;
                  end;
                'm':
                  begin
                    c:='n';
                    if cnt>2 then
                      cnt:=2;
                  end;
                's':
                  if cnt>2 then
                    cnt:=2;
                'S':
                  begin
                    c:='z';
                    cnt:=1;
                  end;
                'G','u','Q','q','w','W','D','F','g','A','z','Z','v':
                  cnt:=0;
              end;
              if cnt>0 then
                s:=StrOfChar(c, cnt);
              cnt:=1;
            end;
        end;
      Inc(p);
      if s<>'' then
        Result:=Result+s;
    end;
end;

procedure GetAndroidFormatSettings(var ASettings: TFormatSettings; ALocale: utf8string);
const
  SGMT = 'GMT';

var
  locale: ansistring;

  function _GetFormat(dateStyle: UDateFormatStyle; timeStyle: UDateFormatStyle; const DefFormat: utf8string): utf8string;
  var
    fmt: UDateFormat;
    err: UErrorCode;
    res: unicodestring;
  begin
    Result:='';
    err:=0;
    fmt:=udat_open(timeStyle, dateStyle, PAnsiChar(locale), SGMT, Length(SGMT), nil, 0, err);
    if fmt <> nil then
      begin
        SetLength(res, 200);
        SetLength(res, udat_toPattern(fmt, False, PUnicodeChar(res), Length(res), err));
        udat_close(fmt);
        Result:=ConvertFormatStr(utf8string(res));
      end;
    if Result = '' then
      Result:=DefFormat;
  end;

  function _DateToStr(fmt: UDateFormat; const AFormat: unicodestring; AYear: integer; AMonth, ADay, AHour: byte): utf8string;
  var
    d: double;
    err: UErrorCode;
    res: unicodestring;
  begin
    d:=EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, 0, 0, 0) - UnixDateDelta;
    d:=MSecsPerDay*d;
    udat_applyPattern(fmt, False, PUnicodeChar(AFormat), Length(AFormat));
    err:=0;
    SetLength(res, 200);
    SetLength(res, udat_format(fmt, d, PUnicodeChar(res), Length(res), nil, err));
    Result:=utf8string(res);
  end;

  function _GetSeparator(dateStyle: UDateFormatStyle; timeStyle: UDateFormatStyle; DefSep: char): char;
  var
    fmt: UDateFormat;
    err: UErrorCode;
    s: utf8string;
    p: PAnsiChar;
    res: unicodestring;
  begin
    Result:=DefSep;
    err:=0;
    fmt:=udat_open(timeStyle, dateStyle, PAnsiChar(locale), SGMT, Length(SGMT), nil, 0, err);
    if fmt <> nil then
      begin
        SetLength(res, 200);
        SetLength(res, udat_toPattern(fmt, False, PUnicodeChar(res), Length(res), err));
        s:=_DateToStr(fmt, res, 2000, 1, 1, 0);
        udat_close(fmt);
        s:=Trim(s);
        p:=PAnsiChar(s);
        while p^<>#0 do
          if (p^>' ') and (p^<'A') and not (p^ in ['0'..'9']) then
            begin
              Result:=p^;
              break;
            end
          else
            Inc(p);
      end;
  end;

var
  fmt: UDateFormat;
  nfmt: UNumberFormat;
  err: UErrorCode;
  i: integer;
  res: unicodestring;
begin
  if not FunctionsInited then
    exit;
  locale:=ALocale;
{$ifdef android}
  if locale = '' then
    locale:=GetCurrentLocaleStr;
{$endif android}
  if locale = '' then
    locale:='en_US';
  err:=0;
  with ASettings do
    begin
      nfmt:=unum_open(2, nil, 0, PAnsiChar(locale), nil, err);
      if nfmt <> nil then
        begin
          SetLength(res, 200);
          SetLength(res, unum_getSymbol(nfmt, UNUM_DECIMAL_SEPARATOR_SYMBOL, PUnicodeChar(res), Length(res), err));
          if res <> '' then
            DecimalSeparator:=ansichar(res[1]);
          SetLength(res, 200);
          SetLength(res, unum_getSymbol(nfmt, UNUM_GROUPING_SEPARATOR_SYMBOL, PUnicodeChar(res), Length(res), err));
          if res <> '' then
            if Ord(res[1]) < 128 then
              ThousandSeparator:=ansichar(res[1])
            else
              ThousandSeparator:=' ';
          SetLength(res, 200);
          SetLength(res, unum_getSymbol(nfmt, 8, PUnicodeChar(res), Length(res), err));
          CurrencyString:=utf8string(res);
          unum_close(nfmt);
        end;

      DateSeparator:=_GetSeparator(UDAT_SHORT, UDAT_NONE, DateSeparator);
      TimeSeparator:=_GetSeparator(UDAT_NONE, UDAT_SHORT, TimeSeparator);

      LongDateFormat:=_GetFormat(UDAT_LONG, UDAT_NONE, LongDateFormat);
      ShortDateFormat:=_GetFormat(UDAT_SHORT, UDAT_NONE, ShortDateFormat);
      LongTimeFormat:=_GetFormat(UDAT_NONE, UDAT_MEDIUM, LongTimeFormat);
      ShortTimeFormat:=_GetFormat(UDAT_NONE, UDAT_SHORT, ShortTimeFormat);

      fmt:=udat_open(UDAT_NONE, UDAT_NONE, PAnsiChar(locale), SGMT, Length(SGMT), nil, 0, err);
      if fmt <> nil then
        begin
          for i:=1 to 12 do
            begin
              LongMonthNames[i]:=_DateToStr(fmt, 'LLLL', 2006, i, 1, 0);
              ShortMonthNames[i]:=_DateToStr(fmt, 'LLL', 2006, i, 1, 0);
            end;
          for i:=1 to 7 do
            begin
              LongDayNames[i]:=_DateToStr(fmt, 'cccc', 2006, 1, i, 0);
              ShortDayNames[i]:=_DateToStr(fmt, 'ccc', 2006, 1, i, 0);
            end;
          TimeAMString:=_DateToStr(fmt, 'a', 2006, 1, 1, 1);
          TimePMString:=_DateToStr(fmt, 'a', 2006, 1, 1, 13);
          udat_close(fmt);
        end;
    end;
end;

initialization
  InitIcuFunctions;
  GetAndroidFormatSettings(DefaultFormatSettings);

end.

