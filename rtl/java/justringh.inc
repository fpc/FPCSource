{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    member of the Free Pascal development team.

    This file implements support routines for UnicodeStrings with FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_HAS_BUILTIN_WIDESTR_MANAGER}
Type
  TCollatorThreadVar = class(JLThreadLocal)
   protected
    function initialValue: JLObject; override;
  end;

  TCharsetDecoderThreadvar = class(JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getForCodePage(cp: TSystemCodePage): JNCCharsetDecoder;
  end;

  TCharsetEncoderThreadvar = class(JLThreadLocal)
   protected
    function initialValue: JLObject; override;
   public
    function getForCodePage(cp: TSystemCodePage): JNCCharsetEncoder;
  end;

  { hooks for internationalization
    please add new procedures at the end, it makes it easier to detect new procedures }

  TUnicodeStringManager = class(JLObject)
   class var
    collator: TCollatorThreadVar;
    decoder: TCharsetDecoderThreadvar;
    encoder: TCharsetEncoderThreadvar;

    class constructor ClassCreate;

    procedure Wide2AnsiMoveProc(source:pwidechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt); virtual;
    procedure Ansi2WideMoveProc(source:pchar;cp : TSystemCodePage;var dest:widestring;len:SizeInt); virtual;

    function UpperWideStringProc(const S: WideString): WideString; virtual;
    function LowerWideStringProc(const S: WideString): WideString; virtual;
    function CompareWideStringProc(const s1, s2 : WideString) : PtrInt; virtual;
    function CompareTextWideStringProc(const s1, s2 : WideString): PtrInt; virtual;
    { return value: number of code points in the string. Whenever an invalid
      code point is encountered, all characters part of this invalid code point
      are considered to form one "character" and the next character is
      considered to be the start of a new (possibly also invalid) code point

      Note: different signature compared to version in native targets: extra
        "Index" parameter, since you cannot increment pchars to point to the
        next character here }
    function CharLengthPCharProc(const Str: PChar; Index: PtrInt): PtrInt; virtual;
    { return value:
      -1 if incomplete or invalid code point
      0 if NULL character,
      > 0 if that's the length in bytes of the code point

      Note: different signature compared to version in native targets: extra
        "Index" parameter, since you cannot increment pchars to point to the
        next character here }
    function CodePointLengthProc(const Str: PChar; Index, MaxLookAhead: PtrInt): Ptrint; virtual;

    function UpperAnsiStringProc(const s : ansistring) : ansistring; virtual;
    function LowerAnsiStringProc(const s : ansistring) : ansistring; virtual;
    function CompareStrAnsiStringProc(const S1, S2: ansistring): PtrInt; virtual;
    function CompareTextAnsiStringProc(const S1, S2: ansistring): PtrInt; virtual;
    function StrCompAnsiStringProc(S1, S2: PChar): PtrInt; virtual;
    function StrICompAnsiStringProc(S1, S2: PChar): PtrInt; virtual;
    function StrLCompAnsiStringProc(S1, S2: PChar; MaxLen: PtrUInt): PtrInt; virtual;
    function StrLICompAnsiStringProc(S1, S2: PChar; MaxLen: PtrUInt): PtrInt; virtual;
    function StrLowerAnsiStringProc(Str: PChar): PChar; virtual;
    function StrUpperAnsiStringProc(Str: PChar): PChar; virtual;

    // not possible to automatically run code when new thread is started in the
    // JVM -- and not needed either, because threadvars can do so when first
    // accessed from a thread
//    ThreadInitProc : procedure;
//    ThreadFiniProc : procedure;

    { this is only different on windows }
    procedure Unicode2AnsiMoveProc(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt); virtual;
    procedure Ansi2UnicodeMoveProc(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt); virtual;
    function UpperUnicodeStringProc(const S: UnicodeString): UnicodeString; virtual;
    function LowerUnicodeStringProc(const S: UnicodeString): UnicodeString; virtual;
    function CompareUnicodeStringProc(const s1, s2 : UnicodeString) : PtrInt; virtual;
    function CompareTextUnicodeStringProc(const s1, s2 : UnicodeString): PtrInt; virtual;
  end;


