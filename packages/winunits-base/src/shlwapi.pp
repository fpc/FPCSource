unit shlwapi;

{
    This file is part of the Free Pascal run time library.
    shlwapi calls are parked here for now.
    Copyright (c) 1999-2002 by Marco van de Voort,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ************************************************************************}

interface
{$mode delphi}

uses Windows;

  const
    SHLWAPIDLL='shlwapi.dll'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  SFBS_FLAGS = longint;
  STIF_FLAGS = longint;
  PSFBS_FLAGS  = ^SFBS_FLAGS;
  PSTIF_FLAGS  = ^STIF_FLAGS;
  PtagSFBS_FLAGS  = ^tagSFBS_FLAGS;
    tagSFBS_FLAGS = (SFBS_FLAGS_ROUND_TO_NEAREST_DISPLAYED_DIGIT = $0001,
      SFBS_FLAGS_TRUNCATE_UNDISPLAYED_DECIMAL_DIGITS = $0002
      );

  const
    STIF_DEFAULT = $00000000;    
    STIF_SUPPORT_HEX = $00000001;    

  function StrChrA(pszStart:PCSTR; wMatch:WORD):PCSTR;stdcall;external SHLWAPIDLL name 'StrChrA';
  function StrChrW(pszStart:PCWSTR; wMatch:WCHAR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrChrW';
  function StrChrIA(pszStart:PCSTR; wMatch:WORD):PCSTR;stdcall;external SHLWAPIDLL name 'StrChrIA';
  function StrChrIW(pszStart:PCWSTR; wMatch:WCHAR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrChrIW';
  function StrChrNW(pszStart:PCWSTR; wMatch:WCHAR; cchMax:UINT):PCWSTR;stdcall;external SHLWAPIDLL name 'StrChrNW';
  function StrChrNIW(pszStart:PCWSTR; wMatch:WCHAR; cchMax:UINT):PWSTR;stdcall;external SHLWAPIDLL name 'StrChrNIW';
  function StrCmpNA(psz1:PCSTR; psz2:PCSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNA';
  function StrCmpNW(psz1:PCWSTR; psz2:PCWSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNW';
  function StrCmpNIA(psz1:PCSTR; psz2:PCSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNIA';
  function StrCmpNIW(psz1:PCWSTR; psz2:PCWSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNIW';
  function StrCSpnA(pszStr:PCSTR; pszSet:PCSTR):longint;stdcall;external SHLWAPIDLL name 'StrCSpnA';
  function StrCSpnW(pszStr:PCWSTR; pszSet:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCSpnW';
  function StrCSpnIA(pszStr:PCSTR; pszSet:PCSTR):longint;stdcall;external SHLWAPIDLL name 'StrCSpnIA';
  function StrCSpnIW(pszStr:PCWSTR; pszSet:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCSpnIW';
  function StrDupA(pszSrch:PCSTR):PSTR;stdcall;external SHLWAPIDLL name 'StrDupA';
  function StrDupW(pszSrch:PCWSTR):PWSTR;stdcall;external SHLWAPIDLL name 'StrDupW';
  function StrFormatByteSizeEx(ull:ULONGLONG; flags:SFBS_FLAGS; pszBuf:PWSTR; cchBuf:UINT):HRESULT;stdcall;external SHLWAPIDLL name 'StrFormatByteSizeEx';
  function StrFormatByteSizeA(dw:DWORD; pszBuf:PSTR; cchBuf:UINT):PSTR;stdcall;external SHLWAPIDLL name 'StrFormatByteSizeA';
  function StrFormatByteSize64A(qdw:LONGLONG; pszBuf:PSTR; cchBuf:UINT):PSTR;stdcall;external SHLWAPIDLL name 'StrFormatByteSize64A';
  function StrFormatByteSizeW(qdw:LONGLONG; pszBuf:PWSTR; cchBuf:UINT):PWSTR;stdcall;external SHLWAPIDLL name 'StrFormatByteSizeW';
  function StrFormatKBSizeW(qdw:LONGLONG; pszBuf:PWSTR; cchBuf:UINT):PWSTR;stdcall;external SHLWAPIDLL name 'StrFormatKBSizeW';
  function StrFormatKBSizeA(qdw:LONGLONG; pszBuf:PSTR; cchBuf:UINT):PSTR;stdcall;external SHLWAPIDLL name 'StrFormatKBSizeA';
  function StrFromTimeIntervalA(pszOut:PSTR; cchMax:UINT; dwTimeMS:DWORD; digits:longint):longint;stdcall;external SHLWAPIDLL name 'StrFromTimeIntervalA';
  function StrFromTimeIntervalW(pszOut:PWSTR; cchMax:UINT; dwTimeMS:DWORD; digits:longint):longint;stdcall;external SHLWAPIDLL name 'StrFromTimeIntervalW';
  function StrIsIntlEqualA(fCaseSens:BOOL; pszString1:PCSTR; pszString2:PCSTR; nChar:longint):BOOL;stdcall;external SHLWAPIDLL name 'StrIsIntlEqualA';
  function StrIsIntlEqualW(fCaseSens:BOOL; pszString1:PCWSTR; pszString2:PCWSTR; nChar:longint):BOOL;stdcall;external SHLWAPIDLL name 'StrIsIntlEqualW';
  function StrNCatA(psz1:PSTR; psz2:PCSTR; cchMax:longint):PSTR;stdcall;external SHLWAPIDLL name 'StrNCatA';
  function StrNCatW(psz1:PWSTR; psz2:PCWSTR; cchMax:longint):PWSTR;stdcall;external SHLWAPIDLL name 'StrNCatW';
  function StrPBrkA(psz:PCSTR; pszSet:PCSTR):PCSTR;stdcall;external SHLWAPIDLL name 'StrPBrkA';
  function StrPBrkW(psz:PCWSTR; pszSet:PCWSTR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrPBrkW';
  function StrRChrA(pszStart:PCSTR; pszEnd:PCSTR; wMatch:WORD):PCSTR;stdcall;external SHLWAPIDLL name 'StrRChrA';
  function StrRChrW(pszStart:PCWSTR; pszEnd:PCWSTR; wMatch:WCHAR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrRChrW';
  function StrRChrIA(pszStart:PCSTR; pszEnd:PCSTR; wMatch:WORD):PCSTR;stdcall;external SHLWAPIDLL name 'StrRChrIA';
  function StrRChrIW(pszStart:PCWSTR; pszEnd:PCWSTR; wMatch:WCHAR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrRChrIW';
  function StrRStrIA(pszSource:PCSTR; pszLast:PCSTR; pszSrch:PCSTR):PCSTR;stdcall;external SHLWAPIDLL name 'StrRStrIA';
  function StrRStrIW(pszSource:PCWSTR; pszLast:PCWSTR; pszSrch:PCWSTR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrRStrIW';
  function StrSpnA(psz:PCSTR; pszSet:PCSTR):longint;stdcall;external SHLWAPIDLL name 'StrSpnA';
  function StrSpnW(psz:PCWSTR; pszSet:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrSpnW';
  function StrStrA(pszFirst:PCSTR; pszSrch:PCSTR):PCSTR;stdcall;external SHLWAPIDLL name 'StrStrA';
  function StrStrW(pszFirst:PCWSTR; pszSrch:PCWSTR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrStrW';
  function StrStrIA(pszFirst:PCSTR; pszSrch:PCSTR):PCSTR;stdcall;external SHLWAPIDLL name 'StrStrIA';
  function StrStrIW(pszFirst:PCWSTR; pszSrch:PCWSTR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrStrIW';
  function StrStrNW(pszFirst:PCWSTR; pszSrch:PCWSTR; cchMax:UINT):PCWSTR;stdcall;external SHLWAPIDLL name 'StrStrNW';
  function StrStrNIW(pszFirst:PCWSTR; pszSrch:PCWSTR; cchMax:UINT):PCWSTR;stdcall;external SHLWAPIDLL name 'StrStrNIW';
  function StrToIntA(pszSrc:PCSTR):longint;stdcall;external SHLWAPIDLL name 'StrToIntA';
  function StrToIntW(pszSrc:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrToIntW';
  function StrToIntExA(pszString:PCSTR; dwFlags:STIF_FLAGS; piRet:plongint):BOOL;stdcall;external SHLWAPIDLL name 'StrToIntExA';
  function StrToIntExW(pszString:PCWSTR; dwFlags:STIF_FLAGS; piRet:plongint):BOOL;stdcall;external SHLWAPIDLL name 'StrToIntExW';
  function StrToInt64ExA(pszString:PCSTR; dwFlags:STIF_FLAGS; pllRet:pLONGLONG):BOOL;stdcall;external SHLWAPIDLL name 'StrToInt64ExA';
  function StrToInt64ExW(pszString:PCWSTR; dwFlags:STIF_FLAGS; pllRet:pLONGLONG):BOOL;stdcall;external SHLWAPIDLL name 'StrToInt64ExW';
  function StrTrimA(psz:PSTR; pszTrimChars:PCSTR):BOOL;stdcall;external SHLWAPIDLL name 'StrTrimA';
  function StrTrimW(psz:PWSTR; pszTrimChars:PCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'StrTrimW';
  function StrCatW(psz1:PWSTR; psz2:PCWSTR):PWSTR;stdcall;external SHLWAPIDLL name 'StrCatW';
  function StrCmpW(psz1:PCWSTR; psz2:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpW';
  function StrCmpIW(psz1:PCWSTR; psz2:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpIW';
  function StrCpyW(psz1:PWSTR; psz2:PCWSTR):PWSTR;stdcall;external SHLWAPIDLL name 'StrCpyW';
  function StrCpyNW(pszDst:PWSTR; pszSrc:PCWSTR; cchMax:longint):PWSTR;stdcall;external SHLWAPIDLL name 'StrCpyNW';
  function StrCatBuffW(pszDest:PWSTR; pszSrc:PCWSTR; cchDestBuffSize:longint):PWSTR;stdcall;external SHLWAPIDLL name 'StrCatBuffW';
  function StrCatBuffA(pszDest:PSTR; pszSrc:PCSTR; cchDestBuffSize:longint):PSTR;stdcall;external SHLWAPIDLL name 'StrCatBuffA';
  function ChrCmpIA(w1:WORD; w2:WORD):BOOL;stdcall;external SHLWAPIDLL name 'ChrCmpIA';
  function ChrCmpIW(w1:WCHAR; w2:WCHAR):BOOL;stdcall;external SHLWAPIDLL name 'ChrCmpIW';
  function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';

implementation

end.