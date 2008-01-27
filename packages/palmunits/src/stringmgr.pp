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

unit stringmgr;

interface

uses palmos,coretraps;

// Max length of string returned by StrIToA, for -2147483647, plus space
// for the terminating null.
const
  maxStrIToALen = 12;

// String Manipulation routines
function StrCopy(dst: PChar; const src: PChar): PChar; syscall sysTrapStrCopy;

function StrNCopy(dst: PChar; const src: PChar; n: Int16): PChar; syscall sysTrapStrNCopy;

function StrCat(dst: PChar; const src: PChar): PChar; syscall sysTrapStrCat;

function StrNCat(dst: PChar; const src: PChar; n: Int16): PChar; syscall sysTrapStrNCat;

function StrLen(const src: PChar): Int16; syscall sysTrapStrLen;

function StrCompareAscii(const s1, s2: PChar): Int16; syscall sysTrapStrCompareAscii;

function StrCompare(const s1, s2: PChar): Int16; syscall sysTrapStrCompare;

function StrNCompareAscii(const s1, s2: PChar; n: Int32): Int16; syscall sysTrapStrNCompareAscii;

function StrNCompare(const s1, s2: PChar;n: Int32): Int16; syscall sysTrapStrNCompare;

function StrCaselessCompare(const s1, s2: PChar): Int16; syscall sysTrapStrCaselessCompare;

function StrNCaselessCompare(const s1, s2: PChar; n: Int32): Int16; syscall sysTrapStrNCaselessCompare;

function StrToLower(dst: PChar; const src: PChar): PChar; syscall sysTrapStrToLower;

function StrIToA(s: PChar; i: Int32): PChar; syscall sysTrapStrIToA;

function StrIToH(s: PChar; i: UInt32): PChar; syscall sysTrapStrIToH;

function StrLocalizeNumber(s: PChar; thousandSeparator, decimalSeparator: Char): PChar; syscall sysTrapStrLocalizeNumber;

function StrDelocalizeNumber(s: PChar; thousandSeparator, decimalSeparator: Char): PChar; syscall sysTrapStrDelocalizeNumber;

function StrChr(const str: PChar; chr: WChar): PChar; syscall sysTrapStrChr;

function StrStr(const str, token: PChar): PChar; syscall sysTrapStrStr;

function StrAToI(const str: PChar): Int32; syscall sysTrapStrAToI;

//function StrPrintF(s: PChar; const Char *formatStr, ...): Int16; syscall sysTrapStrPrintF;

function StrVPrintF(s: PChar; const formatStr: PChar; arg: PChar): Int16; syscall sysTrapStrVPrintF;

implementation

end.
