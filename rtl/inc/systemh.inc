{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This File contains the OS independent declarations of the system unit

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                             Needed switches
****************************************************************************}

{$I-,Q-,H-,R-,V-}
{$mode objfpc}

{ Using inlining for small system functions/wrappers }
{$ifdef HASINLINE}
  {$inline on}
  {$define SYSTEMINLINE}
{$endif}

{ Use threadvars when the compiler supports it }
{$ifdef HASTHREADVAR}
  {$define SUPPORT_THREADVAR}
{$endif HASTHREADVAR}

{ don't use FPU registervariables on the i386 }
{$ifdef CPUI386}
  {$maxfpuregisters 0}
{$endif CPUI386}

{ the assembler helpers need this }
{$ifdef POWERPC}
  {$goto+}
{$endif POWERPC}


{ needed for insert,delete,readln }
{$P+}
{ stack checking always disabled
  for system unit. This is because
  the startup code might not
  have been called yet when we
  get a stack error, this will
  cause big crashes
}
{$S-}

{****************************************************************************
                         Global Types and Constants
****************************************************************************}

Type
{$Ifdef HAS_INTERNAL_INTTYPES}
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;
{$else HAS_INTERNAL_INTTYPES}
  ShortInt = -128..127;
  SmallInt = -32768..32767;
  { can't use -2147483648 because of a bug in 1.0.2's val() procedure (JM) }
  Longint  = +(-2147483647-1)..$7fffffff;
  Byte     = 0..255;
  Word     = 0..65535;
  {$ifndef ver1_0}
  DWord    = LongWord;
  Cardinal = LongWord;
  {$else}
  Longword = cardinal;
  Dword = cardinal;
  {$endif}
  Integer  = SmallInt;
{$endif HAS_INTERNAL_INTTYPES}

{$ifdef CPUI386}
  { for bootstrapping with 1.0.x }
  {$define CPU32}

  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  ValReal = Extended;
{$endif CPUI386}

{$ifdef CPUX86_64}
  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  ValReal = Extended;
{$endif CPUX86_64}

{$ifdef CPUM68K}
  ValReal = Real;

  {$define SUPPORT_SINGLE}
  {$IFDEF Unix}
    { Linux FPU emulator will be used }
    {$define SUPPORT_DOUBLE}
  {$ENDIF}
  {$IFOPT E-}
    { If not compiling with emulation }
    { then support double type.       }
    {$define SUPPORT_DOUBLE}
  {$ENDIF}
  { Comp type does not exist on fpu }
  Comp    = int64;
  PComp = ^Comp;
{$endif CPUM68K}

{$ifdef CPUPOWERPC}
  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  {$define FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}

  ValReal = Double;

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;
{$endif CPUPOWERPC}

{$ifdef CPUSPARC}
  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  {$define FPC_INCLUDE_SOFTWARE_SHIFT_INT64}

  ValReal = Double;

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;
{$endif CPUSPARC}

{$ifdef CPUARM}
  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  {$define FPC_INCLUDE_SOFTWARE_MOD_DIV}
  {$define FPC_INCLUDE_SOFTWARE_SHIFT_INT64}
  {$define FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}

  ValReal = Real;

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;
{$endif CPUARM}

{$ifdef CPU64}
  SizeInt = Int64;
  SizeUInt = QWord;
  PtrInt = Int64;
  PtrUInt = QWord;
  ValSInt = int64;
  ValUInt = qword;
{$endif CPU64}

{$ifdef CPU32}
  SizeInt = Longint;
  SizeUInt = DWord;
  PtrInt = Longint;
  PtrUInt = DWord;
  ValSInt = Longint;
  ValUInt = Cardinal;
{$endif CPU32}

{ Zero - terminated strings }
  PChar               = ^Char;
  PPChar              = ^PChar;

  { AnsiChar is equivalent of Char, so we need
    to use type renamings }
  TAnsiChar           = Char;
  AnsiChar            = Char;
  PAnsiChar           = PChar;
  PPAnsiChar          = PPChar;

  UCS4Char            = type LongWord;
  PUCS4Char           = ^UCS4Char;
  TUCS4CharArray      = array[0..$effffff] of UCS4Char;
  PUCS4CharArray      = ^TUCS4CharArray;
{$ifndef VER1_0}
  UCS4String          = array of UCS4Char;
{$endif VER1_0}

  UTF8String          = type ansistring;
  PUTF8String         = ^UTF8String;

{$ifndef HASCURRENCY}
  Currency            = Int64;
{$endif HASCURRENCY}
  HRESULT             = type Longint;
  TDateTime           = type Double;
  Error               = type Longint;

  PSingle             = ^Single;
  PDouble             = ^Double;
  PCurrency           = ^Currency;
{$ifdef SUPPORT_COMP}
  PComp               = ^Comp;
{$endif SUPPORT_COMP}
  PExtended           = ^Extended;

  PSmallInt           = ^Smallint;
  PShortInt           = ^Shortint;
  PInteger            = ^Longint;
  PByte               = ^Byte;
  PWord               = ^word;
  PDWord              = ^DWord;
  PLongWord           = ^LongWord;
  PLongint            = ^Longint;
  PCardinal           = ^Cardinal;
  PQWord              = ^QWord;
  PInt64              = ^Int64;
  PPtrInt             = ^PtrInt;
  PSizeInt            = ^SizeInt;

  PPointer            = ^Pointer;
  PPPointer           = ^PPointer;

  PBoolean            = ^Boolean;
  PWordBool           = ^WordBool;
  PLongBool           = ^LongBool;

  PShortString        = ^ShortString;
  PAnsiString         = ^AnsiString;

  PDate               = ^TDateTime;
  PError              = ^Error;
{$ifdef HASVARIANT}
  PVariant            = ^Variant;
  POleVariant         = ^OleVariant;
{$endif HASVARIANT}

{$ifdef HASWIDECHAR}
  PWideChar           = ^WideChar;
  PPWideChar          = ^PWideChar;
  { 1.0.x also has HASWIDECHAR defined, but doesn't support it
    fully, setting WChar to Word as fallback (PFV) }
  {$ifndef VER1_0}
  WChar               = Widechar;
  {$else}
  WChar               = Word;
  {$endif}
  UCS2Char            = WideChar;
  PUCS2Char           = PWideChar;
{$else}
  WChar               = Word;
{$endif HASWIDECHAR}
{$ifdef HASWIDESTRING}
  PWideString         = ^WideString;
{$endif HASWIDESTRING}

  { Needed for fpc_get_output }
  PText               = ^Text;
  
  TTextLineBreakStyle = (tlbsLF,tlbsCRLF,tlbsCR);

{ procedure type }
  TProcedure  = Procedure;

const
{ Maximum value of the biggest signed and unsigned integer type available}
  MaxSIntValue = High(ValSInt);
  MaxUIntValue = High(ValUInt);

{ max. values for longint and int}
  maxLongint  = $7fffffff;
  maxSmallint = 32767;

  maxint   = maxsmallint;

type
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
  PointerArray = array [0..512*1024*1024 - 2] of Pointer;
  PPointerArray = ^PointerArray;

{$ifndef VER1_0}
  TBoundArray = array of Integer;
{$endif VER1_0}

  TPCharArray = packed array[0..(MaxLongint div SizeOf(PChar))-1] of PChar;
  PPCharArray = ^TPCharArray;

const
{$ifdef cpui386}
  Test8086 : byte = 2;       { Always i386 or newer }
  Test8087 : byte = 3;       { Always 387 or newer. Emulated if needed. }
{$endif cpui386}
{$ifdef cpum68k}
  Test68000 : byte = 0;      { Must be determined at startup for both }
  Test68881 : byte = 0;
{$endif cpum68k}

{ max level in dumping on error }
  Max_Frame_Dump : Word = 8;

{ Exit Procedure handling consts and types  }
  ExitProc : pointer = nil;
  Erroraddr: pointer = nil;
  Errorcode: Word    = 0;

{ file input modes }
  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;
  fmAppend = $D7B4;
  ProcessID: SizeUInt = 1;
  Filemode : byte = 2;
  CmdLine : PChar = nil;
(* Value should be changed during system initialization as appropriate. *)

  { assume that this program will not spawn other threads, when the
    first thread is started the following constants need to be filled }
  IsMultiThread : boolean = FALSE;
  { Indicates if there was an error }
  StackError : boolean = FALSE;

var
  ExitCode    : Word; public name 'operatingsystem_result';
  RandSeed    : Cardinal;
  { Delphi compatibility }
  IsLibrary : boolean;
  IsConsole : boolean;
  { Threading support }
  fpc_threadvar_relocate_proc : pointer; public name 'FPC_THREADVAR_RELOCATE';

{$ifdef SUPPORT_THREADVAR}
ThreadVar
{$else SUPPORT_THREADVAR}
Var
{$endif SUPPORT_THREADVAR}
  ThreadID    : SizeUInt;
  { Standard In- and Output }
  ErrOutput,
  Output,
  Input,
  StdOut,
  StdErr      : Text;
  InOutRes    : Word;
  { Stack checking }
  StackBottom : Pointer;
  StackLength : Cardinal;

{****************************************************************************
                        Processor specific routines
****************************************************************************}

{$ifdef FPC_USE_LIBC}
{$ifdef SYSTEMINLINE}
{$define INLINEGENERICS}
{$endif}
{$endif}

Procedure Move(const source;var dest;count:SizeInt);{$ifdef INLINEGENERICS}inline;{$endif}
Procedure FillChar(Var x;count:SizeInt;Value:Boolean);{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure FillChar(Var x;count:SizeInt;Value:Char);{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure FillChar(Var x;count:SizeInt;Value:Byte);{$ifdef INLINEGENERICS}inline;{$endif}
procedure FillByte(var x;count:SizeInt;value:byte);{$ifdef INLINEGENERICS}inline;{$endif}
Procedure FillWord(Var x;count:SizeInt;Value:Word);
procedure FillDWord(var x;count:SizeInt;value:DWord);
function  IndexChar(const buf;len:SizeInt;b:char):SizeInt;
function  IndexByte(const buf;len:SizeInt;b:byte):SizeInt;{$ifdef INLINEGENERICS}inline;{$endif}
function  Indexword(const buf;len:SizeInt;b:word):SizeInt;
function  IndexDWord(const buf;len:SizeInt;b:DWord):SizeInt;
function  CompareChar(const buf1,buf2;len:SizeInt):SizeInt;
function  CompareByte(const buf1,buf2;len:SizeInt):SizeInt;{$ifdef INLINEGENERICS}inline;{$endif}
function  CompareWord(const buf1,buf2;len:SizeInt):SizeInt;
function  CompareDWord(const buf1,buf2;len:SizeInt):SizeInt;
procedure MoveChar0(const buf1;var buf2;len:SizeInt);
function  IndexChar0(const buf;len:SizeInt;b:char):SizeInt;
function  CompareChar0(const buf1,buf2;len:SizeInt):SizeInt;{$ifdef INLINEGENERICS}inline;{$endif}
procedure prefetch(const mem);


{****************************************************************************
                          Math Routines
****************************************************************************}

Function  lo(w:Word):byte;
Function  lo(l:Longint):Word;
Function  lo(l:DWord):Word;
Function  lo(i:Integer):byte;
Function  lo(B: Byte):Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  hi(w:Word):byte;
Function  hi(i:Integer):byte;
Function  hi(l:Longint):Word;
Function  hi(b : Byte) : Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  hi(l: DWord): Word;
Function  lo(q : QWord) : DWord;
Function  lo(i : Int64) : DWord;
Function  hi(q : QWord) : DWord;
Function  hi(i : Int64) : DWord;
Function  Swap (X:Word):Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Swap (X:Integer):Integer;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Swap (X:Cardinal):Cardinal;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Swap (X:LongInt):LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Swap (X:QWord):QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Swap (X:Int64):Int64;{$ifdef SYSTEMINLINE}inline;{$endif}

Function Align (Addr : PtrInt; Alignment : PtrInt) : PtrInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Align (Addr : Pointer; Alignment : PtrInt) : Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}

Function  Random(l:longint):longint;
Function  Random(l:int64):int64;
Function  Random: extended;
Procedure Randomize;

Function abs(l:Longint):Longint;{$ifdef SYSTEMINLINE}inline;{$endif}
Function abs(l:Int64):Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:Longint):Longint;{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:Int64):Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:QWord):QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Longint):Boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Longword):Boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Int64):Boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:QWord):Boolean;{$ifdef SYSTEMINLINE}inline;{$endif}

{ float math routines }
{$I mathh.inc}

operator ** (bas,expo : real) e: real;
operator ** (bas,expo : int64) i: int64;


{****************************************************************************
                         Addr/Pointer Handling
****************************************************************************}

Function  ptr(sel,off:Longint):farpointer;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Cseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Dseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Sseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}

{****************************************************************************
                      PChar and String Handling
****************************************************************************}

function strpas(p:pchar):shortstring;
function strlen(p:pchar):longint;{$ifdef INLINEGENERICS}inline;{$endif}

{ Shortstring functions }
{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S:ShortString;len:SizeInt);
{$endif INTERNSETLENGTH}
{$ifndef InternCopy}
Function  Copy(const s:shortstring;index:SizeInt;count:SizeInt):shortstring;
{$endif interncopy}
Procedure Delete(Var s:shortstring;index:SizeInt;count:SizeInt);
Procedure Insert(const source:shortstring;Var s:shortstring;index:SizeInt);
Procedure Insert(source:Char;Var s:shortstring;index:SizeInt);
Function  Pos(const substr:shortstring;const s:shortstring):SizeInt;
Function  Pos(C:Char;const s:shortstring):SizeInt;
Procedure SetString (Var S : Shortstring; Buf : PChar; Len : SizeInt);
Procedure SetString (Var S : AnsiString; Buf : PChar; Len : SizeInt);
{$ifndef INTERNLENGTH}
Function  Length(s:string):byte;
{$endif INTERNLENGTH}
Function  upCase(const s:shortstring):shortstring;
Function  lowerCase(const s:shortstring):shortstring; overload;
Function  Space(b:byte):shortstring;
Function  hexStr(Val:Longint;cnt:byte):shortstring;
Function  OctStr(Val:Longint;cnt:byte):shortstring;
Function  binStr(Val:Longint;cnt:byte):shortstring;
Function  hexStr(Val:int64;cnt:byte):shortstring;
Function  OctStr(Val:int64;cnt:byte):shortstring;
Function  binStr(Val:int64;cnt:byte):shortstring;

{ Char functions }
Function  Chr(b:byte):Char;
Function  upCase(c:Char):Char;
Function  lowerCase(c:Char):Char; overload;
{$ifndef InternCopy}
function  copy(c:char;index : SizeInt;count : SizeInt): shortstring;
{$endif interncopy}
function  pos(const substr : shortstring;c:char): SizeInt;
{$ifndef INTERNLENGTH}
function  length(c:char):byte;
{$endif INTERNLENGTH}


{****************************************************************************
                             AnsiString Handling
****************************************************************************}

{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : AnsiString; l : SizeInt);
{$endif INTERNSETLENGTH}
Procedure UniqueString (Var S : AnsiString);
{$ifndef INTERNLENGTH}
Function  Length (Const S : AnsiString) : SizeInt;
{$endif INTERNLENGTH}
{$ifndef InternCopy}
Function  Copy (Const S : AnsiString; Index,Size : SizeInt) : AnsiString;
{$endif interncopy}
Function  Pos (Const Substr : AnsiString; Const Source : AnsiString) : SizeInt;
Function  Pos (c : Char; Const s : AnsiString) : SizeInt;
Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : SizeInt);
Procedure Delete (Var S : AnsiString; Index,Size: SizeInt);
Function  StringOfChar(c : char;l : SizeInt) : AnsiString;
function  upcase(const s : ansistring) : ansistring;
function  lowercase(const s : ansistring) : ansistring;


{****************************************************************************
                             WideString Handling
****************************************************************************}

{$ifdef HASWIDESTRING}
{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : WideString; l : SizeInt);
{$endif INTERNSETLENGTH}
Procedure UniqueString (Var S : WideString);
{$ifndef INTERNLENGTH}
Function  Length (Const S : WideString) : SizeInt;
{$endif INTERNLENGTH}
{$ifndef InternCopy}
Function  Copy (Const S : WideString; Index,Size : SizeInt) : WideString;
{$endif interncopy}
Function  Pos (Const Substr : WideString; Const Source : WideString) : SizeInt;
Function  Pos (c : Char; Const s : WideString) : SizeInt;
Function  Pos (c : WideChar; Const s : WideString) : SizeInt;
Procedure Insert (Const Source : WideString; Var S : WideString; Index : SizeInt);
Procedure Delete (Var S : WideString; Index,Size: SizeInt);
Procedure SetString (Var S : WideString; Buf : PWideChar; Len : SizeInt);
Procedure SetString (Var S : WideString; Buf : PChar; Len : SizeInt);

function WideCharToString(S : PWideChar) : AnsiString;
function StringToWideChar(const Src : AnsiString;Dest : PWideChar;DestSize : SizeInt) : PWideChar;
function WideCharLenToString(S : PWideChar;Len : SizeInt) : AnsiString;
procedure WideCharLenToStrVar(Src : PWideChar;Len : SizeInt;var Dest : AnsiString);
procedure WideCharToStrVar(S : PWideChar;var Dest : AnsiString);

Type
  TWide2AnsiMove=procedure(source:pwidechar;dest:pchar;len:SizeInt);
  TAnsi2WideMove=procedure(source:pchar;dest:pwidechar;len:SizeInt);


  TWideStringManager = record
    Wide2AnsiMove : TWide2AnsiMove;
    Ansi2WideMove : TAnsi2WideMove;
{
    UpperUTF8 : procedure(p:PUTF8String);
    UpperUCS2 : procedure(p:PUCS2Char);
    UpperUCS4 : procedure(p:PUCS4Char);
    LowerUTF8 : procedure(p:PUTF8String);
    LowerUCS2 : procedure(p:PUCS2Char);
    LowerUCS4 : procedure(p:PUCS4Char);

    CompUTF8 : function(p1,p2:PUTF8String) : shortint;
    CompUCS2 : function(p1,p2:PUCS2Char) : shortint;
    CompUCS4 : function(p1,p2:PUC42Char) : shortint;
}
  end;


function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function UnicodeToUtf8(Dest: PChar; MaxDestBytes: SizeUInt; Source: PWideChar; SourceChars: SizeUInt): SizeUInt;
function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;
function UTF8Encode(const s : WideString) : UTF8String;
function UTF8Decode(const s : UTF8String): WideString;
function AnsiToUtf8(const s : ansistring): UTF8String;{$ifdef SYSTEMINLINE}inline;{$endif}
function Utf8ToAnsi(const s : UTF8String) : ansistring;{$ifdef SYSTEMINLINE}inline;{$endif}

Procedure GetWideStringManager (Var Manager : TWideStringManager);
Procedure SetWideStringManager (Const New : TWideStringManager);
Procedure SetWideStringManager (Const New : TWideStringManager; Var Old: TWideStringManager);

{$endif HASWIDESTRING}


{****************************************************************************
                          Untyped File Management
****************************************************************************}

Procedure Assign(Var f:File;const Name:string);
Procedure Assign(Var f:File;p:pchar);
Procedure Assign(Var f:File;c:char);
Procedure Rewrite(Var f:File;l:Longint);
Procedure Rewrite(Var f:File);
Procedure Reset(Var f:File;l:Longint);
Procedure Reset(Var f:File);
Procedure Close(Var f:File);
Procedure BlockWrite(Var f:File;Const Buf;Count:Longint;Var Result:Longint);
Procedure BlockWrite(Var f:File;Const Buf;Count:Cardinal;var Result:Cardinal);
Procedure BlockWrite(Var f:File;Const Buf;Count:Word;Var Result:Word);
Procedure BlockWrite(Var f:File;Const Buf;Count:Word;Var Result:Integer);
Procedure BlockWrite(Var f:File;Const Buf;Count:Longint);
Procedure BlockRead(Var f:File;Var Buf;count:Longint;Var Result:Longint);
Procedure BlockRead(Var f:File;Var Buf;count:Cardinal;Var Result:Cardinal);
Procedure BlockRead(Var f:File;Var Buf;count:Word;Var Result:Word);
Procedure BlockRead(Var f:File;Var Buf;count:Word;Var Result:Integer);
Procedure BlockRead(Var f:File;Var Buf;count:Longint);
Function  FilePos(Var f:File):Longint;
Function  FileSize(Var f:File):Longint;
Procedure Seek(Var f:File;Pos:Longint);
Function  EOF(Var f:File):Boolean;
Procedure Erase(Var f:File);
Procedure Rename(Var f:File;const s:string);
Procedure Rename(Var f:File;p:pchar);
Procedure Rename(Var f:File;c:char);
Procedure Truncate (Var F:File);


{****************************************************************************
                           Typed File Management
****************************************************************************}

Procedure Assign(Var f:TypedFile;const Name:string);
Procedure Assign(Var f:TypedFile;p:pchar);
Procedure Assign(Var f:TypedFile;c:char);
Procedure Rewrite(Var f:TypedFile);
Procedure Reset(Var f:TypedFile);


{****************************************************************************
                            Text File Management
****************************************************************************}

Procedure Assign(Var t:Text;const s:string);
Procedure Assign(Var t:Text;p:pchar);
Procedure Assign(Var t:Text;c:char);
Procedure Close(Var t:Text);
Procedure Rewrite(Var t:Text);
Procedure Reset(Var t:Text);
Procedure Append(Var t:Text);
Procedure Flush(Var t:Text);
Procedure Erase(Var t:Text);
Procedure Rename(Var t:Text;const s:string);
Procedure Rename(Var t:Text;p:pchar);
Procedure Rename(Var t:Text;c:char);
Function  EOF(Var t:Text):Boolean;
Function  EOF:Boolean;
Function  EOLn(Var t:Text):Boolean;
Function  EOLn:Boolean;
Function  SeekEOLn (Var t:Text):Boolean;
Function  SeekEOF (Var t:Text):Boolean;
Function  SeekEOLn:Boolean;
Function  SeekEOF:Boolean;
Procedure SetTextBuf(Var f:Text; Var Buf);
Procedure SetTextBuf(Var f:Text; Var Buf; Size:Longint);
Procedure SetTextLineEnding(Var f:Text; Ending:string);

{****************************************************************************
                            Directory Management
****************************************************************************}

Procedure chdir(const s:string);
Procedure mkdir(const s:string);
Procedure rmdir(const s:string);
Procedure getdir(drivenr:byte;Var dir:shortstring);
Procedure getdir(drivenr:byte;Var dir:ansistring);


{*****************************************************************************
                             Miscellaneous
*****************************************************************************}

{ os independent calls to allow backtraces }
function get_frame:pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
function get_caller_addr(framebp:pointer):pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
function get_caller_frame(framebp:pointer):pointer;{$ifdef SYSTEMINLINE}inline;{$endif}

Function IOResult:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Sptr:Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}


{*****************************************************************************
                          Init / Exit / ExitProc
*****************************************************************************}

Function  Paramcount:Longint;
Function  ParamStr(l:Longint):string;
Procedure Dump_Stack(var f : text;bp:pointer);
Procedure RunError(w:Word);
Procedure RunError;{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure halt(errnum:byte);
Procedure AddExitProc(Proc:TProcedure);
Procedure halt;{$ifdef SYSTEMINLINE}inline;{$endif}

{ Need to be exported for threads unit }
Procedure SysInitExceptions;
procedure SysInitStdIO;
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}

{*****************************************************************************
                         Abstract/Assert/Error Handling
*****************************************************************************}

function ArrayStringToPPchar(const S:Array of AnsiString;reserveentries:Longint):ppchar; // const ?
Function StringToPPChar(Var S:AnsiString;ReserveEntries:integer):ppchar;
Function StringToPPChar(S: PChar;ReserveEntries:integer):ppchar;


procedure AbstractError;
Function  SysBackTraceStr(Addr:Pointer): ShortString;
Procedure SysAssert(Const Msg,FName:ShortString;LineNo:Longint;ErrorAddr:Pointer);

{ Error handlers }
Type
  TBackTraceStrFunc = Function (Addr: Pointer): ShortString;
  TErrorProc = Procedure (ErrNo : Longint; Address,Frame : Pointer);
  TAbstractErrorProc = Procedure;
  TAssertErrorProc = Procedure(const msg,fname:ShortString;lineno:longint;erroraddr:pointer);



const
  BackTraceStrFunc  : TBackTraceStrFunc = @SysBackTraceStr;
  ErrorProc         : TErrorProc = nil;
  AbstractErrorProc : TAbstractErrorProc = nil;
  AssertErrorProc   : TAssertErrorProc = @SysAssert;


{*****************************************************************************
                          SetJmp/LongJmp
*****************************************************************************}

{$i setjumph.inc}


{*****************************************************************************
                       Object Pascal support
*****************************************************************************}

{$i objpash.inc}

{*****************************************************************************
                           Variant support
*****************************************************************************}

{$ifdef HASVARIANT}
{$i varianth.inc}
{$endif HASVARIANT}

{*****************************************************************************
                   Internal helper routines support
*****************************************************************************}

{$i dynarrh.inc}

{$i compproc.inc}

{
  $Log$
  Revision 1.107  2004-11-09 23:10:22  peter
    * use helper call to retrieve address of input/output to reduce
      code that is generated in the main program for loading the
      threadvar

  Revision 1.106  2004/11/04 09:32:31  peter
  ErrOutput added

  Revision 1.105  2004/10/30 20:49:10  marco
   * arraytostring added

  Revision 1.104  2004/10/14 17:39:33  florian
    + added system.align
    + threadvars are now aligned

  Revision 1.103  2004/10/09 21:00:46  jonas
    + cgenmath with libc math functions. Faster than the routines in genmath
      and also have full double support (exp() only has support for values in
      the single range in genmath, for example). Used in FPC_USE_LIBC is
      defined
    * several fixes to allow compilation with -dHASINLINE, but internalerrors
      because of missing support for inlining assembler code

  Revision 1.102  2004/09/22 05:56:11  hajny
    * compilation fix

  Revision 1.101  2004/09/21 23:43:26  hajny
    * SetTextLineEnding implemented, FileRec.Name position alignment for CPU64

  Revision 1.100  2004/09/21 15:04:22  peter
    * AnsiChar is equivalent of Char, use only type renamings

  Revision 1.99  2004/08/29 10:23:12  peter
  no message

  Revision 1.98  2004/07/18 20:21:44  florian
    + several unicode (to/from utf-8 conversion) stuff added
    * some longint -> SizeInt changes

  Revision 1.97  2004/07/07 15:15:40  daniel
    * Add inline directive to sysresetfpu

  Revision 1.96  2004/07/03 21:50:31  daniel
    * Modified bootstrap code so separate prt0.as/prt0_10.as files are no
      longer necessary

  Revision 1.95  2004/06/11 19:21:53  florian
    * fixed wrong commit

  Revision 1.94  2004/06/11 13:45:33  florian
    * fixed arm system unit compilation

  Revision 1.93  2004/05/30 16:51:38  peter
    * sparc need software shl shr

  Revision 1.92  2004/05/05 21:26:34  florian
    * some m68k and amiga related stuff fixed

  Revision 1.91  2004/05/01 23:55:18  peter
    * replace strlenint with sizeint

  Revision 1.90  2004/05/01 20:52:50  peter
    * ValSInt fixed for 64 bit

  Revision 1.89  2004/04/29 18:59:43  peter
    * str() helpers now also use valint/valuint
    * int64/qword helpers disabled for cpu64

  Revision 1.88  2004/04/22 17:10:38  peter
    * random(int64) added

  Revision 1.87  2004/03/23 22:35:45  peter
    * new compiler has smallint,longint,word internal defined

  Revision 1.86  2004/02/20 22:15:16  florian
    + x86_64 dependend sysutils part added
    * some 64 bit adaptions

  Revision 1.85  2004/02/20 11:01:20  daniel
    * Applied

  Revision 1.84  2004/02/02 20:39:27  florian
    + added prefetch(const mem)

  Revision 1.83  2004/01/26 11:48:24  florian
    * compilation on arm fixed

  Revision 1.82  2004/01/20 23:13:53  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.81  2003/12/29 19:24:12  florian
    + introduced PtrInt and PtrUInt
    * made strscan 64 bit safe

  Revision 1.80  2003/11/29 17:27:05  michael
  + Added overloaded version of SetWideStringManager without old parameter

  Revision 1.79  2003/11/28 20:36:13  michael
  + Added WideStringManager

  Revision 1.78  2003/11/15 19:01:27  florian
    * fixed rtl to work with the integrated fpc ppc assembler reader

  Revision 1.77  2003/11/03 09:42:28  marco
   * Peter's Cardinal<->Longint fixes patch

  Revision 1.76  2003/10/17 20:52:12  olle
    * Changed m68k to cpum68k, i386 to cpui386

  Revision 1.75  2003/10/16 15:43:13  peter
    * THandle is platform dependent

  Revision 1.74  2003/10/08 16:24:47  florian
    * fixed some variant issues
    * improved type declarations

  Revision 1.72  2003/09/04 16:07:31  florian
    * fixed qword_to_double conversion on powerpc

  Revision 1.71  2003/09/03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...

  Revision 1.70  2003/09/01 20:47:23  peter
    * pcomp added for sparc

  Revision 1.69  2003/05/01 08:05:23  florian
    * started to make the rtl 64 bit save by introducing SizeInt and SizeUInt (similar to size_t of C)

  Revision 1.68  2003/04/30 16:36:39  florian
    + support for generic pchar routines added
    + some basic rtl stuff for x86-64 added

  Revision 1.67  2003/04/25 21:09:44  peter
    * remove dos lf

  Revision 1.66  2003/04/23 22:46:41  florian
    + powerpc defines now pcomp as well

  Revision 1.65  2003/04/01 15:36:31  peter
    * make HResult unique type

  Revision 1.64  2003/03/17 14:30:11  peter
    * changed address parameter/return values to pointer instead
      of longint

  Revision 1.63  2003/01/13 14:37:11  florian
    * cpu defines fixed
    * ... = type ...; stuff reactived, should work now with 1.1

  Revision 1.62  2002/12/21 17:20:27  florian
    + some types for D6 compatibility added

  Revision 1.61  2002/12/15 22:33:12  peter
    * SetString(WideString,[PChar|PWideChar],Len) added

  Revision 1.60  2002/11/16 20:12:22  florian
    + sparc types added
    + UTF*/UCS* types added

  Revision 1.59  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.58  2002/10/06 13:56:47  carl
    - remove stack checking for every target in system unit

  Revision 1.57  2002/10/02 18:21:51  peter
    * Copy() changed to internal function calling compilerprocs
    * FPC_SHORTSTR_COPY renamed to FPC_SHORTSTR_ASSIGN because of the
      new copy functions

  Revision 1.56  2002/09/28 21:18:02  florian
    * map comp to int64 for the powerpc

  Revision 1.55  2002/09/26 21:50:37  florian
    + some WideString<->AnsiString conversion functions added

  Revision 1.54  2002/09/07 21:20:50  carl
    * cardinal -> longword
    - remove FPUInt64 variable

  Revision 1.53  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.52  2002/08/19 19:34:02  peter
    * SYSTEMINLINE define that will add inline directives for small
      functions and wrappers. This will be defined automaticly when
      the compiler defines the HASINLINE directive

  Revision 1.51  2002/08/06 20:53:38  michael
    + Added support for octal strings (using &)

  Revision 1.50  2002/07/26 22:46:06  florian
    * interface of system unit for Linux/PowerPC compiles

  Revision 1.49  2002/07/26 21:29:03  florian
    + powerpc type support

  Revision 1.48  2002/07/04 20:40:09  florian
    + some x86-64 support added

  Revision 1.47  2002/07/01 16:29:05  peter
    * sLineBreak changed to normal constant like Kylix

  Revision 1.46  2002/06/02 10:49:30  marco
   * Renamefest supports_double for FreeBSD too

  Revision 1.45  2002/04/21 15:51:51  carl
  * StackError is now a typed constant
  + $S can be used under unix

  Revision 1.44  2002/04/12 17:35:24  carl
  - removed unused variable

  Revision 1.43  2002/01/25 17:41:05  peter
    * moved array types to objpas unit

  Revision 1.42  2002/01/25 17:39:35  peter
    * array types added

  Revision 1.41  2002/01/24 18:27:06  peter
    * lowercase() overloaded

  Revision 1.40  2002/01/24 12:33:53  jonas
    * adapted ranges of native types to int64 (e.g. high cardinal is no
      longer longint($ffffffff), but just $fffffff in psystem)
    * small additional fix in 64bit rangecheck code generation for 32 bit
      processors
    * adaption of ranges required the matching talgorithm used for selecting
      which overloaded procedure to call to be adapted. It should now always
      select the closest match for ordinal parameters.
    + inttostr(qword) in sysstr.inc/sysstrh.inc
    + abs(int64), sqr(int64), sqr(qword) in systemh.inc/generic.inc (previous
      fixes were required to be able to add them)
    * is_in_limit() moved from ncal to types unit, should always be used
      instead of direct comparisons of low/high values of orddefs because
      qword is a special case

}
