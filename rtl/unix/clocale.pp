{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Init rtl formating variables based on libc locales

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

{ Initial implementation by petr kristan }

unit clocale;

{$mode objfpc}

interface

implementation

{$linklib c}

Uses
  SysUtils, unixtype, initc;

Const
{$ifdef BSD}
  // Darwin and FreeBSD. Note the lead underscores are added.
 {$i clocale.inc}
{$else}
// checked for Linux only, but might be general glibc.
  __LC_CTYPE    = 0;
  __LC_NUMERIC  = 1;
  __LC_TIME     = 2;
  __LC_COLLATE  = 3;
  __LC_MONETARY = 4;
  __LC_MESSAGES = 5;
  __LC_ALL      = 6;
 ABDAY_1 = (__LC_TIME shl 16);
 DAY_1 = (ABDAY_1)+7;
 ABMON_1 = (ABDAY_1)+14;
 MON_1 = (ABDAY_1)+26;
 AM_STR = (ABDAY_1)+38;
 PM_STR = (ABDAY_1)+39;
 D_T_FMT = (ABDAY_1)+40;
 D_FMT = (ABDAY_1)+41;
 T_FMT = (ABDAY_1)+42;
 T_FMT_AMPM = (ABDAY_1)+43;

 __DECIMAL_POINT = (__LC_NUMERIC shl 16);
 RADIXCHAR = __DECIMAL_POINT;
 __THOUSANDS_SEP = (__DECIMAL_POINT)+1;

 __INT_CURR_SYMBOL = (__LC_MONETARY shl 16);
 __CURRENCY_SYMBOL = (__INT_CURR_SYMBOL)+1;
 __MON_DECIMAL_POINT = (__INT_CURR_SYMBOL)+2;
 __MON_THOUSANDS_SEP = (__INT_CURR_SYMBOL)+3;
 __MON_GROUPING = (__INT_CURR_SYMBOL)+4;
 __POSITIVE_SIGN = (__INT_CURR_SYMBOL)+5;
 __NEGATIVE_SIGN = (__INT_CURR_SYMBOL)+6;
 __INT_FRAC_DIGITS = (__INT_CURR_SYMBOL)+7;
 __FRAC_DIGITS = (__INT_CURR_SYMBOL)+8;
 __P_CS_PRECEDES = (__INT_CURR_SYMBOL)+9;
 __P_SEP_BY_SPACE = (__INT_CURR_SYMBOL)+10;
 __N_CS_PRECEDES = (__INT_CURR_SYMBOL)+11;
 __N_SEP_BY_SPACE = (__INT_CURR_SYMBOL)+12;
 __P_SIGN_POSN = (__INT_CURR_SYMBOL)+13;
 __N_SIGN_POSN = (__INT_CURR_SYMBOL)+14;
 _NL_MONETARY_CRNCYSTR = (__INT_CURR_SYMBOL)+15;
 {$endif}


function setlocale(category: cint; locale: pchar): pchar; cdecl; external clib name 'setlocale';
function nl_langinfo(__item: cint):Pchar;cdecl;external clib name 'nl_langinfo';

procedure GetFormatSettings;

  function GetLocaleStr(item: cint): string;
  begin
    GetLocaleStr := AnsiString(nl_langinfo(item));
  end;

  function GetLocaleChar(item: cint): char;
  begin
    GetLocaleChar := nl_langinfo(item)^;
  end;

  function FindSeparator(const s: string; Def: char): char;
  var
    i, l: integer;
  begin
    FindSeparator := Def;
    i := Pos('%', s);
    if i=0 then
      Exit;
    l := Length(s);
    inc(i);
    if (i<=l) and (s[i] in ['E', 'O']) then //possible modifier
      inc(i);
    inc(i); 
    if i<=l then
      FindSeparator := s[i];
  end;

  function TransformFormatStr(const s: string): string;
  var
    i, l: integer;
  begin
    TransformFormatStr := '';
    i := 1;
    l := Length(s);
    while i<=l do begin
      if s[i]='%' then begin
        inc(i);
        if (i<=l) and (s[i] in ['E', 'O']) then //ignore modifier
          inc(i);
        if i>l then
          Exit;
        case s[i] of
          'a': TransformFormatStr := TransformFormatStr + 'ddd';
          'A': TransformFormatStr := TransformFormatStr + 'dddd';
          'b': TransformFormatStr := TransformFormatStr + 'mmm';
          'B': TransformFormatStr := TransformFormatStr + 'mmmm';
          'c': TransformFormatStr := TransformFormatStr + 'c';
          //'C':
          'd': TransformFormatStr := TransformFormatStr + 'dd';
          'D': TransformFormatStr := TransformFormatStr + 'mm"/"dd"/"yy';
          'e': TransformFormatStr := TransformFormatStr + 'd';
          'F': TransformFormatStr := TransformFormatStr + 'yyyy-mm-dd';
          'g': TransformFormatStr := TransformFormatStr + 'yy';
          'G': TransformFormatStr := TransformFormatStr + 'yyyy';
          'h': TransformFormatStr := TransformFormatStr + 'mmm';
          'H': TransformFormatStr := TransformFormatStr + 'hh';
          'I': TransformFormatStr := TransformFormatStr + 'hhampm';
          //'j':
          'k': TransformFormatStr := TransformFormatStr + 'h';
          'l': TransformFormatStr := TransformFormatStr + 'hampm';
          'm': TransformFormatStr := TransformFormatStr + 'mm';
          'M': TransformFormatStr := TransformFormatStr + 'nn';
          'n': TransformFormatStr := TransformFormatStr + sLineBreak;
          'p': TransformFormatStr := TransformFormatStr + 'ampm';
          'P': TransformFormatStr := TransformFormatStr + 'ampm';
          'r': TransformFormatStr := TransformFormatStr + 'hhampm:nn:ss';
          'R': TransformFormatStr := TransformFormatStr + 'hh:nn';
          //'s':
          'S': TransformFormatStr := TransformFormatStr + 'ss';
          't': TransformFormatStr := TransformFormatStr + #9;
          'T': TransformFormatStr := TransformFormatStr + 'hh:nn:ss';
          //'u':
          //'U':
          //'V':
          //'w':
          //'W':
          'x': TransformFormatStr := TransformFormatStr + 'ddddd';
          'X': TransformFormatStr := TransformFormatStr + 't';
          'y': TransformFormatStr := TransformFormatStr + 'yy';
          'Y': TransformFormatStr := TransformFormatStr + 'yyyy';
          //'z':
          //'Z':
          '%': TransformFormatStr := TransformFormatStr + '%';
        end;
      end else
        TransformFormatStr := TransformFormatStr + s[i];
      inc(i);
    end;
  end;

const
  //                      sign  prec  sep
  NegFormatsTable: array [0..4, 0..1, 0..1] of byte = (
    ( (4, 15), (0, 14) ), //Parentheses surround the quantity and currency_symbol
    ( (5, 8), (1, 9) ), //The sign string precedes the quantity and currency_symbol
    ( (7, 10), (3, 11) ), //The sign string follows the quantity and currency_symbol
    ( (6, 13), (1, 9) ), //The sign string immediately precedes the currency_symbol
    ( (7, 10), (2, 12) )  //The sign string immediately follows the currency_symbol
  ); 
var
  i: integer;
  prec, sep, signp: byte;
  {$ifdef BSD}
   plocale : plconv;
  {$ENDIF}
begin
  setlocale(__LC_ALL,'');
  for i := 1 to 12 do
    begin
    ShortMonthNames[i]:=GetLocaleStr(ABMON_1+i-1);
    LongMonthNames[i]:=GetLocaleStr(MON_1+i-1);
    end;
  for i := 1 to 7 do
    begin
    ShortDayNames[i]:=GetLocaleStr(ABDAY_1+i-1);
    LongDayNames[i]:=GetLocaleStr(DAY_1+i-1);
    end;
  //Date stuff
  ShortDateFormat := GetLocaleStr(D_FMT);
  DateSeparator := FindSeparator(ShortDateFormat, DateSeparator);
  ShortDateFormat := TransformFormatStr(ShortDateFormat);
  LongDateFormat := GetLocaleStr(D_T_FMT);
  LongDateFormat := TransformFormatStr(LongDateFormat);
  //Time stuff
  TimeAMString := GetLocaleStr(AM_STR);
  TimePMString := GetLocaleStr(PM_STR);
  ShortTimeFormat := GetLocaleStr(T_FMT);
  TimeSeparator := FindSeparator(ShortTimeFormat, TimeSeparator);
  ShortTimeFormat := TransformFormatStr(ShortTimeFormat);
  LongTimeFormat := GetLocaleStr(T_FMT_AMPM);
  LongTimeFormat := TransformFormatStr(LongTimeFormat);

  {$Ifdef BSD}
     plocale:=localeconv;
     // for these fields there is a separate BSD derived POSIX function.
     if not assigned(plocale) then exit; // for now.
     CurrencyString:=plocale^.CURRENCY_SYMBOL;
     CurrencyString := Copy(CurrencyString, 2, Length(CurrencyString));
     CurrencyDecimals:=ord(plocale^.FRAC_DIGITS);
     prec:=ord(plocale^.P_CS_PRECEDES);
     sep:=ord(plocale^.P_SEP_BY_SPACE);
     if (prec<=1) and (sep<=1) then
       CurrencyFormat := byte(not boolean(prec)) + sep shl 1;
     prec := ord(plocale^.N_CS_PRECEDES);
     sep := ord(plocale^.N_SEP_BY_SPACE);
     signp := ord(plocale^.N_SIGN_POSN);
     if (signp in [0..4]) and (prec in [0, 1]) and (sep in [0, 1]) then
       NegCurrFormat := NegFormatsTable[signp, prec, sep];
  //Number stuff
     ThousandSeparator:=plocale^.THOUSANDS_SEP[0];
  {$else}
   //Currency stuff
  CurrencyString := GetLocaleStr(_NL_MONETARY_CRNCYSTR);
  CurrencyString := Copy(CurrencyString, 2, Length(CurrencyString));

  CurrencyDecimals := StrToIntDef(GetLocaleStr(__FRAC_DIGITS), CurrencyDecimals);
  prec := byte(GetLocaleChar(__P_CS_PRECEDES));
  sep := byte(GetLocaleChar(__P_SEP_BY_SPACE));
  if (prec<=1) and (sep<=1) then
    CurrencyFormat := byte(not boolean(prec)) + sep shl 1;
  prec := byte(GetLocaleChar(__N_CS_PRECEDES));
  sep := byte(GetLocaleChar(__N_SEP_BY_SPACE));
  signp := byte(GetLocaleChar(__N_SIGN_POSN));
  if (signp in [0..4]) and (prec in [0, 1]) and (sep in [0, 1]) then
    NegCurrFormat := NegFormatsTable[signp, prec, sep];
  //Number stuff
  ThousandSeparator:=GetLocaleChar(__THOUSANDS_SEP);
  {$endif}
  DecimalSeparator:=GetLocaleChar(RADIXCHAR);
end;

initialization
  GetFormatSettings;

end.
