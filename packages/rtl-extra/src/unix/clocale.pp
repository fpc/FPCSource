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

{$ifdef android}
  {$error This unit is not intended for Android. Something wrong with the make file. }
{$endif android}

{$mode objfpc}

interface

{$ifdef localedebug}
// for easier debugging, allows to print untransformed values in test
Type TOrgFormatSettings = record
                            ShortDateFormat,
                            LongDateFormat ,
                            ShortTimeFormat, 
                            LongTimeFormat ,
                            CurrencyString1, 
                            CurrencyString2: string;
                           end;

var OrgFormatSettings : TOrgFormatSettings;

{$endif}

implementation

{$linklib c}

Uses
  SysUtils, unixtype, initc;

Const
{$if defined(BSD) or defined(SUNOS) or defined(aix)}
  // Darwin, FreeBSD, Solaris, AIX. Note the lead underscores are added.
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

{$ifdef netbsd}
  { NetBSD has a new setlocale function defined in /usr/include/locale.h
    that should be used }
function setlocale(category: cint; locale: pchar): pchar; cdecl; external clib name '__setlocale_mb_len_max_32';
{$else}
function setlocale(category: cint; locale: pchar): pchar; cdecl; external clib name 'setlocale';
{$endif}

function nl_langinfo(__item: cint):Pchar;cdecl;external clib name 'nl_langinfo';

procedure GetFormatSettings(out fmts: TFormatSettings);

  function GetLocaleStr(item: cint): string;
  begin
    GetLocaleStr := AnsiString(nl_langinfo(item));
  end;

  function GetLocaleChar(item: cint): char;
  begin
    GetLocaleChar := nl_langinfo(item)^;
  end;

  function SkipModifiers(const s: string; var i: integer): string;
  var
    l: Integer;
  begin
    Result := '';
    l := Length(s);
    //possible flag, with specifier or modifier - glibc exension
    while (i<=l) and (s[i] in ['0'..'9', '_', '-', '^', '#', 'E', 'O']) do begin
      Result := Result + s[i];
      inc(i);
    end;
  end;

  function IsModifier(const Mods: string; m: char): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 1 to Length(Mods) do begin
      if Mods[i] = m then begin
        Result := True;
        Break;
      end;
    end;
  end;

  function FindSeparator(const s: string; Def: char): char;
  var
    i: integer;
  begin
    FindSeparator := Def;
    i := Pos('%', s);
    if i=0 then
      Exit;
    inc(i);
    SkipModifiers(s, i);
    inc(i);
    if i<=Length(s) then
      FindSeparator := s[i];
  end;

  function TransformFormatStr(const s: string): string;
  var
    i, l: integer;
    ampminstring : boolean;
    clock12:boolean;
    LastMod: string;
  begin
    clock12:=false; // should ampm get appended?
    ampminstring:=false; // setting clock12 in the loop fails  if ampm
                         // is before the 12h time specifiers , bug #39760
    TransformFormatStr := '';
    i := 1;
    l := Length(s);
    while i<=l do begin
      if s[i]='%' then begin
        inc(i);
        LastMod := SkipModifiers(s, i);
        if i>l then
          Exit;
        case s[i] of
          'a': TransformFormatStr := TransformFormatStr + 'ddd';
          'A': TransformFormatStr := TransformFormatStr + 'dddd';
          'b': TransformFormatStr := TransformFormatStr + 'mmm';
          'B': TransformFormatStr := TransformFormatStr + 'mmmm';
          'c': TransformFormatStr := TransformFormatStr + 'c';
          //'C':
          'd': if IsModifier(LastMod, '-') then
                 TransformFormatStr := TransformFormatStr + 'd'
               else
                 TransformFormatStr := TransformFormatStr + 'dd';
          'D': TransformFormatStr := TransformFormatStr + 'mm"/"dd"/"yy';
          'e': TransformFormatStr := TransformFormatStr + 'd';
          'F': TransformFormatStr := TransformFormatStr + 'yyyy-mm-dd';
          'g': TransformFormatStr := TransformFormatStr + 'yy';
          'G': TransformFormatStr := TransformFormatStr + 'yyyy';
          'h': TransformFormatStr := TransformFormatStr + 'mmm';
          'H': TransformFormatStr := TransformFormatStr + 'hh';
          'I': begin 
                 TransformFormatStr := TransformFormatStr + 'hh';
                 clock12:=true;
               end;
          //'j':
          'k': TransformFormatStr := TransformFormatStr + 'h';
          'l': begin
		  TransformFormatStr := TransformFormatStr + 'h';
                  clock12:=true;
               end;
          'm': if IsModifier(LastMod, '-') then
                 TransformFormatStr := TransformFormatStr + 'm'
               else
                 TransformFormatStr := TransformFormatStr + 'mm';
          'M': TransformFormatStr := TransformFormatStr + 'nn';
          'n': TransformFormatStr := TransformFormatStr + sLineBreak;
          'p','P': 
               begin
                 TransformFormatStr := TransformFormatStr + 'ampm';
                 clock12:=false;
                 ampminstring:=true;
               end;
          'r': begin
                 TransformFormatStr := TransformFormatStr + 'hh:nn:ss';
                 clock12:=true;  
               end;
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
    if ampminstring then
       clock12:=false;
    i:=length(TransformFormatStr);
    if clock12 and (i>0) then
      begin
        if transformformatstr[i]<>' ' then
          TransformFormatStr := TransformFormatStr + ' ';
        TransformFormatStr := TransformFormatStr + 'ampm';
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
  {$if defined(BSD) or defined(SUNOS) or defined(aix)}
   plocale : plconv;
  {$ENDIF}
begin
  setlocale(__LC_ALL,'');
  for i := 1 to 12 do
    begin
    fmts.ShortMonthNames[i]:=GetLocaleStr(ABMON_1+i-1);
    fmts.LongMonthNames[i]:=GetLocaleStr(MON_1+i-1);
    end;
  for i := 1 to 7 do
    begin
    fmts.ShortDayNames[i]:=GetLocaleStr(ABDAY_1+i-1);
    fmts.LongDayNames[i]:=GetLocaleStr(DAY_1+i-1);
    end;
  //Date stuff
  fmts.ShortDateFormat := GetLocaleStr(D_FMT);
 
{$ifdef localedebug}
  OrgFormatSettings.ShortDateFormat:=fmts.shortdateformat;
{$endif}
 
  fmts.DateSeparator := FindSeparator(fmts.ShortDateFormat, fmts.DateSeparator);
  fmts.ShortDateFormat := TransformFormatStr(fmts.ShortDateFormat);
  fmts.LongDateFormat := GetLocaleStr(D_FMT);
{$ifdef localedebug}
  OrgFormatSettings.LongDateFormat:=fmts.longdateformat;
{$endif}
  fmts.LongDateFormat := TransformFormatStr(fmts.LongDateFormat);
  //Time stuff
  fmts.TimeAMString := GetLocaleStr(AM_STR);
  fmts.TimePMString := GetLocaleStr(PM_STR);
  fmts.ShortTimeFormat := GetLocaleStr(T_FMT);
{$ifdef localedebug}
  OrgFormatSettings.ShortTimeFormat:=fmts.shorttimeformat;
{$endif}
  fmts.TimeSeparator := FindSeparator(fmts.ShortTimeFormat, fmts.TimeSeparator);
  fmts.ShortTimeFormat := TransformFormatStr(fmts.ShortTimeFormat);
  fmts.LongTimeFormat := GetLocaleStr(T_FMT_AMPM);
{$ifdef localedebug}
  OrgFormatSettings.LongTimeFormat:=fmts.longtimeformat;
{$endif}

  if (fmts.LongTimeFormat='') then
    fmts.LongTimeFormat:=fmts.ShortTimeFormat
  else
    fmts.LongTimeFormat := TransformFormatStr(fmts.LongTimeFormat);

  {$if defined(BSD) or defined(SUNOS) or defined(aix)}
     plocale:=localeconv;
     // for these fields there is a separate BSD derived POSIX function.
     if not assigned(plocale) then exit; // for now.

     fmts.CurrencyString:=plocale^.currency_symbol; // int_CURR_SYMBOL (in latin chars)
     if fmts.CurrencyString='' then
        fmts.CurrencyString:=plocale^.int_curr_symbol;
     fmts.CurrencyDecimals:=ord(plocale^.FRAC_DIGITS);
{$ifdef localedebug}
  OrgFormatSettings.CurrencyString1:=plocale^.currency_symbol;
  OrgFormatSettings.CurrencyString2:=plocale^.int_curr_symbol;
{$endif}
     prec:=ord(plocale^.P_CS_PRECEDES);
     sep:=ord(plocale^.P_SEP_BY_SPACE);
     if (prec<=1) and (sep<=1) then
       fmts.CurrencyFormat := byte(not boolean(prec)) + sep shl 1;
     prec := ord(plocale^.N_CS_PRECEDES);
     sep := ord(plocale^.N_SEP_BY_SPACE);
     signp := ord(plocale^.N_SIGN_POSN);
     if (signp in [0..4]) and (prec in [0, 1]) and (sep in [0, 1]) then
       fmts.NegCurrFormat := NegFormatsTable[signp, prec, sep];
  //Number stuff
     fmts.ThousandSeparator:=plocale^.THOUSANDS_SEP[0];
  {$else}
   //Currency stuff
  fmts.CurrencyString := GetLocaleStr(_NL_MONETARY_CRNCYSTR);
{$ifdef localedebug}
  OrgFormatSettings.CurrencyString1:=fmts.currencystring;
  OrgFormatSettings.CurrencyString2:='';
{$endif}
  fmts.CurrencyString := Copy(fmts.CurrencyString, 2, Length(fmts.CurrencyString));
  fmts.CurrencyDecimals := StrToIntDef(GetLocaleStr(__FRAC_DIGITS), fmts.CurrencyDecimals);
  prec := byte(GetLocaleChar(__P_CS_PRECEDES));
  sep := byte(GetLocaleChar(__P_SEP_BY_SPACE));
  if (prec<=1) and (sep<=1) then
    fmts.CurrencyFormat := byte(not boolean(prec)) + sep shl 1;
  prec := byte(GetLocaleChar(__N_CS_PRECEDES));
  sep := byte(GetLocaleChar(__N_SEP_BY_SPACE));
  signp := byte(GetLocaleChar(__N_SIGN_POSN));
  if (signp in [0..4]) and (prec in [0, 1]) and (sep in [0, 1]) then
    fmts.NegCurrFormat := NegFormatsTable[signp, prec, sep];
  //Number stuff
  fmts.ThousandSeparator:=GetLocaleChar(__THOUSANDS_SEP);
  Sep := ord(GetLocaleChar(__MON_THOUSANDS_SEP));
  if fmts.ThousandSeparator=#0 then
    fmts.ThousandSeparator := char(Sep);
  {$endif}
  fmts.DecimalSeparator:=GetLocaleChar(RADIXCHAR);
end;

initialization
  GetFormatSettings(DefaultFormatSettings);

end.
