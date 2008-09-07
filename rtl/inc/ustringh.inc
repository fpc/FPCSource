{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    This file implements support routines for UnicodeStrings with FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Procedure UniqueString (Var S : UnicodeString);external name 'FPC_UNICODESTR_UNIQUE';
Function Pos (Const Substr : UnicodeString; Const Source : UnicodeString) : SizeInt;
Function Pos (c : Char; Const s : UnicodeString) : SizeInt;
Function Pos (c : UnicodeChar; Const s : UnicodeString) : SizeInt;
Function Pos (c : AnsiString; Const s : UnicodeString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Pos (c : UnicodeString; Const s : AnsiString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Pos (c : ShortString; Const s : UnicodeString) : SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}

Function UpCase(const s : UnicodeString) : UnicodeString;
Function  UpCase(c:UnicodeChar):UnicodeChar;

Procedure Insert (Const Source : UnicodeString; Var S : UnicodeString; Index : SizeInt);
Procedure Delete (Var S : UnicodeString; Index,Size: SizeInt);
Procedure SetString (Out S : UnicodeString; Buf : PUnicodeChar; Len : SizeInt);
Procedure SetString (Out S : UnicodeString; Buf : PChar; Len : SizeInt);

function WideCharToString(S : PWideChar) : AnsiString;
function StringToWideChar(const Src : AnsiString;Dest : PWideChar;DestSize : SizeInt) : PWideChar;
function WideCharLenToString(S : PWideChar;Len : SizeInt) : AnsiString;
procedure WideCharLenToStrVar(Src : PWideChar;Len : SizeInt;out Dest : AnsiString);
procedure WideCharToStrVar(S : PWideChar;out Dest : AnsiString);

function UnicodeCharToString(S : PUnicodeChar) : AnsiString;
function StringToUnicodeChar(const Src : AnsiString;Dest : PUnicodeChar;DestSize : SizeInt) : PUnicodeChar;
function UnicodeCharLenToString(S : PUnicodeChar;Len : SizeInt) : AnsiString;
procedure UnicodeCharLenToStrVar(Src : PUnicodeChar;Len : SizeInt;out Dest : AnsiString);
procedure UnicodeCharToStrVar(S : PUnicodeChar;out Dest : AnsiString);

procedure DefaultUnicode2AnsiMove(source:punicodechar;var dest:ansistring;len:SizeInt);
procedure DefaultAnsi2UnicodeMove(source:pchar;var dest:unicodestring;len:SizeInt);

Type
  { hooks for internationalization
    please add new procedures at the end, it makes it easier to detect new procedures }
  TUnicodeStringManager = record
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
    ThreadInitProc : procedure;
    ThreadFiniProc : procedure;

    { this is only different on windows }
    Unicode2AnsiMoveProc : procedure(source:punicodechar;var dest:ansistring;len:SizeInt);
    Ansi2UnicodeMoveProc : procedure(source:pchar;var dest:unicodestring;len:SizeInt);
    UpperUnicodeStringProc : function(const S: UnicodeString): UnicodeString;
    LowerUnicodeStringProc : function(const S: UnicodeString): UnicodeString;
    CompareUnicodeStringProc : function(const s1, s2 : UnicodeString) : PtrInt;
    CompareTextUnicodeStringProc : function(const s1, s2 : UnicodeString): PtrInt;
  end;

var
  widestringmanager : TUnicodeStringManager;

function UnicodeToUtf8(Dest: PChar; Source: PUnicodeChar; MaxBytes: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function UnicodeToUtf8(Dest: PChar; MaxDestBytes: SizeUInt; Source: PUnicodeChar; SourceChars: SizeUInt): SizeUInt;
function Utf8ToUnicode(Dest: PUnicodeChar; Source: PChar; MaxChars: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function Utf8ToUnicode(Dest: PUnicodeChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;
function UTF8Encode(const s : Ansistring) : UTF8String; inline;
function UTF8Encode(const s : UnicodeString) : UTF8String;
function UTF8Decode(const s : UTF8String): UnicodeString;
function AnsiToUtf8(const s : ansistring): UTF8String;{$ifdef SYSTEMINLINE}inline;{$endif}
function Utf8ToAnsi(const s : UTF8String) : ansistring;{$ifdef SYSTEMINLINE}inline;{$endif}
function UnicodeStringToUCS4String(const s : UnicodeString) : UCS4String;
function UCS4StringToUnicodeString(const s : UCS4String) : UnicodeString;
function WideStringToUCS4String(const s : WideString) : UCS4String;
function UCS4StringToWideString(const s : UCS4String) : WideString;

Procedure GetWideStringManager (Var Manager : TUnicodeStringManager);
Procedure SetWideStringManager (Const New : TUnicodeStringManager);
Procedure SetWideStringManager (Const New : TUnicodeStringManager; Var Old: TUnicodeStringManager);

Procedure GetUnicodeStringManager (Var Manager : TUnicodeStringManager);
Procedure SetUnicodeStringManager (Const New : TUnicodeStringManager);
Procedure SetUnicodeStringManager (Const New : TUnicodeStringManager; Var Old: TUnicodeStringManager);


