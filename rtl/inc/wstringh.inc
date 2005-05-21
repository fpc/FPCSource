{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    This file implements support routines for WideStrings/Unicode with FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : WideString; l : SizeInt);
{$endif INTERNSETLENGTH}
procedure UniqueString(Var S : WideString);external name 'FPC_WIDESTR_UNIQUE';
{$ifndef INTERNLENGTH}
Function  Length (Const S : WideString) : SizeInt;
{$endif INTERNLENGTH}
{$ifndef InternCopy}
Function  Copy (Const S : WideString; Index,Size : SizeInt) : WideString;
{$endif interncopy}
Function Pos (Const Substr : WideString; Const Source : WideString) : SizeInt;
Function Pos (c : Char; Const s : WideString) : SizeInt;
Function Pos (c : WideChar; Const s : WideString) : SizeInt;
Function Pos (c : WideChar; Const s : AnsiString) : SizeInt;

Function UpCase(const s : WideString) : WideString;

Procedure Insert (Const Source : WideString; Var S : WideString; Index : SizeInt);
Procedure Delete (Var S : WideString; Index,Size: SizeInt);
Procedure SetString (Var S : WideString; Buf : PWideChar; Len : SizeInt);
Procedure SetString (Var S : WideString; Buf : PChar; Len : SizeInt);

function WideCharToString(S : PWideChar) : AnsiString;
function StringToWideChar(const Src : AnsiString;Dest : PWideChar;DestSize : SizeInt) : PWideChar;
function WideCharLenToString(S : PWideChar;Len : SizeInt) : AnsiString;
procedure WideCharLenToStrVar(Src : PWideChar;Len : SizeInt;var Dest : AnsiString);
procedure WideCharToStrVar(S : PWideChar;var Dest : AnsiString);

procedure DefaultWide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
procedure DefaultAnsi2WideMove(source:pchar;var dest:widestring;len:SizeInt);

Type
  TWideStringManager = record
    Wide2AnsiMoveProc : procedure(source:pwidechar;var dest:ansistring;len:SizeInt);
    Ansi2WideMoveProc : procedure(source:pchar;var dest:widestring;len:SizeInt);

//    UpperUTF8 : procedure(p:PUTF8String);

    UpperWideStringProc : function(const S: WideString): WideString;
//    UpperUCS4 : procedure(p:PUCS4Char);
//    LowerUTF8 : procedure(p:PUTF8String);
    LowerWideStringProc : function(const S: WideString): WideString;
//    LowerUCS4 : procedure(p:PUCS4Char);
{
    CompUTF8 : function(p1,p2:PUTF8String) : shortint;
    CompUCS2 : function(p1,p2:PUCS2Char) : shortint;
    CompUCS4 : function(p1,p2:PUC42Char) : shortint;
}    
    CompareWideStringProc : function(const s1, s2 : WideString) : PtrInt;
    CompareTextWideStringProc : function(const s1, s2 : WideString): PtrInt;
    CharLengthPCharProc : function(const Str: PChar): PtrInt;
    
    UpperAnsiStringProc : function(const s : ansistring) : ansistring;
    LowerAnsiStringProc : function(const s : ansistring) : ansistring;
    CompareStrAnsiStringProc : function(const S1, S2: ansistring): PtrInt;
    CompareTextAnsiStringProc : function(const S1, S2: ansistring): PtrInt;
    StrCompAnsiStringProc : function(S1, S2: PChar): PtrInt;
    StrICompAnsiStringProc : function(S1, S2: PChar): PtrInt;  
    StrLCompAnsiStringProc : function(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;  
    StrLICompAnsiStringProc : function(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
    StrLowerAnsiStringProc : function(Str: PChar): PChar;
    StrUpperAnsiStringProc : function(Str: PChar): PChar;
  end;


function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function UnicodeToUtf8(Dest: PChar; MaxDestBytes: SizeUInt; Source: PWideChar; SourceChars: SizeUInt): SizeUInt;
function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;
function UTF8Encode(const s : WideString) : UTF8String;
function UTF8Decode(const s : UTF8String): WideString;
function AnsiToUtf8(const s : ansistring): UTF8String;{$ifdef SYSTEMINLINE}inline;{$endif}
function Utf8ToAnsi(const s : UTF8String) : ansistring;{$ifdef SYSTEMINLINE}inline;{$endif}

var
  widestringmanager : TWideStringManager;

Procedure GetWideStringManager (Var Manager : TWideStringManager);
Procedure SetWideStringManager (Const New : TWideStringManager);
Procedure SetWideStringManager (Const New : TWideStringManager; Var Old: TWideStringManager);

{
  $Log: wstringh.inc,v $
  Revision 1.7  2005/03/14 21:09:04  florian
    * widestring manager can handle now ansi<->wide string conversions even if the lens don't match

  Revision 1.6  2005/03/12 14:56:22  florian
    + added Ansi* routines to widestring manager
    * made them using OS calls on windows

  Revision 1.5  2005/02/26 15:00:14  florian
    + WideSameStr

  Revision 1.4  2005/02/26 10:21:17  florian
    + implemented WideFormat
    + some Widestring stuff implemented
    * some Widestring stuff fixed

  Revision 1.3  2005/02/06 09:38:45  florian
    +  StrCharLength infrastructure

  Revision 1.2  2005/02/03 18:40:50  florian
    + infrastructure for WideCompareText implemented

  Revision 1.1  2005/02/01 20:22:49  florian
    * improved widestring infrastructure manager
}
