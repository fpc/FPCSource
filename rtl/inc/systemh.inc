{
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
  DWord    = LongWord;
  Cardinal = LongWord;
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

{$ifdef FPC_HASNOFARPOINTER}
  FarPointer = Pointer;
{$endif FPC_HASNOFARPOINTER}
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

{$ifdef FPC_HASNOFARPOINTER}
  FarPointer = Pointer;
{$endif FPC_HASNOFARPOINTER}
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

{$ifdef FPC_HASNOFARPOINTER}
  FarPointer = Pointer;
{$endif FPC_HASNOFARPOINTER}
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

{$ifdef FPC_HASNOFARPOINTER}
  FarPointer = Pointer;
{$endif FPC_HASNOFARPOINTER}
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
  UCS4String          = array of UCS4Char;

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
  PInteger            = ^Integer;
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
  WChar               = Widechar;
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

{ platform dependent types }
{$i sysosh.inc}


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

  TBoundArray = array of Integer;

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
  Filemode : byte = 2;
  CmdLine : PChar = nil;
(* Value should be changed during system initialization as appropriate. *)

  { assume that this program will not spawn other threads, when the
    first thread is started the following constants need to be filled }
  IsMultiThread : boolean = FALSE;
  { set to true, if a threading helper is used before a thread
    manager has been installed }
  ThreadingAlreadyUsed : boolean = FALSE;
  { Indicates if there was an error }
  StackError : boolean = FALSE;
  InitProc : Pointer = nil;

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
  ThreadID    : TThreadID;
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


{ Numbers for routines that have compiler magic }
{$I innr.inc}


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
procedure prefetch(const mem);{$ifdef INTERNCONSTINTF}[internproc:fpc_in_prefetch_var];{$endif}


{****************************************************************************
                          Math Routines
****************************************************************************}

Function  lo(B: Byte):Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  hi(b : Byte) : Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  lo(i : Integer) : byte;  {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_lo_Word];{$endif}
Function  lo(w : Word) : byte;     {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_lo_Word];{$endif}
Function  lo(l : Longint) : Word;  {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_lo_long];{$endif}
Function  lo(l : DWord) : Word;    {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_lo_long];{$endif}
Function  lo(i : Int64) : DWord;   {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_lo_qword];{$endif}
Function  lo(q : QWord) : DWord;   {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_lo_qword];{$endif}
Function  hi(i : Integer) : byte;  {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_hi_Word];{$endif}
Function  hi(w : Word) : byte;     {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_hi_Word];{$endif}
Function  hi(l : Longint) : Word;  {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_hi_long];{$endif}
Function  hi(l : DWord) : Word;    {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_hi_long];{$endif}
Function  hi(i : Int64) : DWord;   {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_hi_qword];{$endif}
Function  hi(q : QWord) : DWord;   {$ifdef INTERNCONSTINTF}[INTERNPROC: fpc_in_hi_qword];{$endif}

Function swap (X : Word) : Word;{$ifdef SYSTEMINLINE}inline;{$endif}{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_swap_word];{$endif}
Function Swap (X : Integer) : Integer;{$ifdef SYSTEMINLINE}inline;{$endif}{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_swap_word];{$endif}
Function swap (X : Longint) : Longint;{$ifdef SYSTEMINLINE}inline;{$endif}{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_swap_long];{$endif}
Function Swap (X : Cardinal) : Cardinal;{$ifdef SYSTEMINLINE}inline;{$endif}{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_swap_long];{$endif}
Function Swap (X : QWord) : QWord;{$ifdef SYSTEMINLINE}inline;{$endif}{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_swap_qword];{$endif}
Function swap (X : Int64) : Int64;{$ifdef SYSTEMINLINE}inline;{$endif}{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_swap_qword];{$endif}

Function Align (Addr : PtrInt; Alignment : PtrInt) : PtrInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Align (Addr : Pointer; Alignment : PtrInt) : Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}

Function  Random(l:longint):longint;
Function  Random(l:int64):int64;
Function  Random: extended;
Procedure Randomize;

Function abs(l:Longint):Longint;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_abs];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function abs(l:Int64):Int64;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_abs];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:Longint):Longint;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_sqr];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:Int64):Int64;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_sqr];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:QWord):QWord;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_sqr];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Longint):Boolean;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_odd];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Longword):Boolean;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_odd];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Int64):Boolean;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_odd];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:QWord):Boolean;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_odd];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}

{ float math routines }
{$I mathh.inc}

{****************************************************************************
                         Addr/Pointer Handling
****************************************************************************}

Function  ptr(sel,off:Longint):farpointer;{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_ptr];{$endif}{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Cseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Dseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Sseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}

{****************************************************************************
                      PChar and String Handling
****************************************************************************}

function strpas(p:pchar):shortstring;external name 'FPC_PCHAR_TO_SHORTSTR';
function strlen(p:pchar):longint;external name 'FPC_PCHAR_LENGTH';

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
Function  hexStr(Val:Pointer):shortstring;

{ Char functions }
{$ifdef INTERNCONSTINTF}
Function chr(b : byte) : Char;      [INTERNPROC: fpc_in_chr_byte];
{$else}
Function  Chr(b:byte):Char;
{$endif}
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
Procedure UniqueString(Var S : AnsiString);external name 'FPC_ANSISTR_UNIQUE';
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
{$i wstringh.inc}
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
{$ifdef INTERNCONSTINTF}
Procedure Reset(var f : TypedFile);   [INTERNPROC: fpc_in_Reset_TypedFile];
Procedure Rewrite(var f : TypedFile); [INTERNPROC: fpc_in_Rewrite_TypedFile];
{$else}
Procedure Rewrite(Var f:TypedFile);
Procedure Reset(Var f:TypedFile);
{$endif}

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
Procedure SetTextBuf(Var f:Text; Var Buf);{$ifdef INTERNCONSTINTF}[INTERNPROC:fpc_in_settextbuf_file_x];{$endif}
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
Function Sptr:Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}{$ifdef INTERNCONSTINTF}[internconst:fpc_in_const_ptr];{$endif}
Function GetProcessID:SizeUInt;
Function GetThreadID:TThreadID;


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


procedure AbstractError;external name 'FPC_ABSTRACTERROR';
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

{*****************************************************************************
                               Heap
*****************************************************************************}

{$i heaph.inc}

{*****************************************************************************
                          Thread support
*****************************************************************************}

{ Generic threadmanager }
{$i threadh.inc}


{*****************************************************************************
                   FPDoc phony declarations.
*****************************************************************************}

{$ifdef fpdocsystem}
{$i system.fpd}
{$endif}

{
  $Log: systemh.inc,v $
  Revision 1.122  2005/04/03 19:29:28  florian
    * proper error message if the cthreads unit is included too late
      uses clause

  Revision 1.121  2005/02/25 12:34:46  peter
    * added HexStr(Pointer)

  Revision 1.120  2005/02/14 17:13:29  peter
    * truncate log

  Revision 1.119  2005/02/08 20:25:28  florian
    - killed power from system unit
    * move operator ** to math unit

  Revision 1.118  2005/02/06 20:37:31  florian
    * InitProc gets an inital value

  Revision 1.117  2005/02/06 20:35:54  florian
    + InitProc

  Revision 1.116  2005/02/06 11:20:52  peter
    * threading in system unit
    * removed systhrds unit

  Revision 1.115  2005/02/05 10:47:38  florian
    * fixed previous commit

  Revision 1.114  2005/02/05 10:07:21  florian
    * map farpointer to pointer on platforms not having farpointers

  Revision 1.113  2005/02/01 20:22:49  florian
    * improved widestring infrastructure manager

  Revision 1.112  2005/01/24 18:03:19  peter
    * pinteger in non-delphi/objfpc mode is psmallint

}
