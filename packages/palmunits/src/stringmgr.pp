(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: StringMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    String manipulation functions
 *
 * History:
 *    11/09/94 RM    Created by Ron Marianetti
 *    08/26/98 kwk   Changed chr param in StrChr to WChar (was Int16)
 *    07/16/99 kwk   Added maxStrIToALen.
 *    05/14/00 vsm   Added StrCompareAscii.
 *    08/18/00 kwk   Added StrNCompareAscii.
 *
 *****************************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit stringmgr;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses PalmApi.Palmos,PalmApi.Coretraps;
{$ELSE FPC_DOTTEDUNITS}
uses palmos,coretraps;
{$ENDIF FPC_DOTTEDUNITS}

// Max length of string returned by StrIToA, for -2147483647, plus space
// for the terminating null.
const
  maxStrIToALen = 12;

// String Manipulation routines
function StrCopy(dst: PAnsiChar; const src: PAnsiChar): PAnsiChar; syscall sysTrapStrCopy;

function StrNCopy(dst: PAnsiChar; const src: PAnsiChar; n: Int16): PAnsiChar; syscall sysTrapStrNCopy;

function StrCat(dst: PAnsiChar; const src: PAnsiChar): PAnsiChar; syscall sysTrapStrCat;

function StrNCat(dst: PAnsiChar; const src: PAnsiChar; n: Int16): PAnsiChar; syscall sysTrapStrNCat;

function StrLen(const src: PAnsiChar): Int16; syscall sysTrapStrLen;

function StrCompareAscii(const s1, s2: PAnsiChar): Int16; syscall sysTrapStrCompareAscii;

function StrCompare(const s1, s2: PAnsiChar): Int16; syscall sysTrapStrCompare;

function StrNCompareAscii(const s1, s2: PAnsiChar; n: Int32): Int16; syscall sysTrapStrNCompareAscii;

function StrNCompare(const s1, s2: PAnsiChar;n: Int32): Int16; syscall sysTrapStrNCompare;

function StrCaselessCompare(const s1, s2: PAnsiChar): Int16; syscall sysTrapStrCaselessCompare;

function StrNCaselessCompare(const s1, s2: PAnsiChar; n: Int32): Int16; syscall sysTrapStrNCaselessCompare;

function StrToLower(dst: PAnsiChar; const src: PAnsiChar): PAnsiChar; syscall sysTrapStrToLower;

function StrIToA(s: PAnsiChar; i: Int32): PAnsiChar; syscall sysTrapStrIToA;

function StrIToH(s: PAnsiChar; i: UInt32): PAnsiChar; syscall sysTrapStrIToH;

function StrLocalizeNumber(s: PAnsiChar; thousandSeparator, decimalSeparator: AnsiChar): PAnsiChar; syscall sysTrapStrLocalizeNumber;

function StrDelocalizeNumber(s: PAnsiChar; thousandSeparator, decimalSeparator: AnsiChar): PAnsiChar; syscall sysTrapStrDelocalizeNumber;

function StrChr(const str: PAnsiChar; chr: WChar): PAnsiChar; syscall sysTrapStrChr;

function StrStr(const str, token: PAnsiChar): PAnsiChar; syscall sysTrapStrStr;

function StrAToI(const str: PAnsiChar): Int32; syscall sysTrapStrAToI;

//function StrPrintF(s: PAnsiChar; const AnsiChar *formatStr, ...): Int16; syscall sysTrapStrPrintF;

function StrVPrintF(s: PAnsiChar; const formatStr: PAnsiChar; arg: PAnsiChar): Int16; syscall sysTrapStrVPrintF;

implementation

end.
