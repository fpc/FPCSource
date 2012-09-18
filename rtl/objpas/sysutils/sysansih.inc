{
    *********************************************************************
    Copyright (C) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function AnsiCompareFileName(const S1, S2 : string) : SizeInt;
function AnsiLowerCaseFileName(const s : string) : string;
function AnsiUpperCaseFileName(const s : string) : string;
function AnsiPos(const substr,s : string) : SizeInt;
function AnsiStrPos(str,substr : PChar) : PChar;
function AnsiStrRScan(Str : PChar;Chr : Char) : PChar;
function AnsiStrScan(Str : PChar;Chr: Char) : PChar;
function HashName(Name: PAnsiChar): LongWord;
