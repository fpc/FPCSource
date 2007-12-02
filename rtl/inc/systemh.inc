{
    This file contains the OS independent declarations of the system unit

    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

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

{ At least 2.0.0 is required }
{$ifdef VER1}
  {$fatal You need at least FPC 2.0.0}
{$endif}

{ Using inlining for small system functions/wrappers }
{$inline on}
{$define SYSTEMINLINE}

{ don't use FPU registervariables on the i386 }
{$ifdef CPUI386}
  {$maxfpuregisters 0}
{$endif CPUI386}

{ the assembler helpers need this}
{$ifdef CPUPOWERPC}
  {$goto+}
{$endif CPUPOWERPC}


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

{$ifndef FPC_HAS_FEATURE_SUPPORT}
{$define FPC_HAS_FEATURE_HEAP}
{$define FPC_HAS_FEATURE_INITFINAL}
{$define FPC_HAS_FEATURE_RTTI}
{$define FPC_HAS_FEATURE_CLASSES}
{$define FPC_HAS_FEATURE_EXCEPTIONS}
{$define FPC_HAS_FEATURE_EXITCODE}
{$define FPC_HAS_FEATURE_ANSISTRINGS}
{$define FPC_HAS_FEATURE_WIDESTRINGS}
{$define FPC_HAS_FEATURE_TEXTIO}
{$define FPC_HAS_FEATURE_CONSOLEIO}
{$define FPC_HAS_FEATURE_FILEIO}
{$define FPC_HAS_FEATURE_RANDOM}
{$define FPC_HAS_FEATURE_VARIANTS}
{$define FPC_HAS_FEATURE_OBJECTS}
{$define FPC_HAS_FEATURE_DYNARRAYS}
{$define FPC_HAS_FEATURE_THREADING}
{$define FPC_HAS_FEATURE_COMMANDARGS}
{$define FPC_HAS_FEATURE_PROCESSES}
{$define FPC_HAS_FEATURE_STACKCHECK}
{$endif FPC_HAS_FEATURE_SUPPORT}

{****************************************************************************
                         Global Types and Constants
****************************************************************************}

Type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;
  UInt64   = QWord;

  { moved here from psystem.pas
    Delphi allows chose of overloaded procedure depending
    on Real <-> Double, so use type here, see also tw7425.pp (FK) }
{$ifndef VER2_0}
  Real = type Double;
{$endif VER2_0}

{$ifdef CPUI386}
  {$define CPU32}

  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  ValReal = Extended;
{$endif CPUI386}

{$ifdef CPUX86_64}
{$ifndef WIN64}
  { win64 doesn't support the legacy fpu }
  {$define DEFAULT_EXTENDED}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}
  ValReal = Extended;
{$else WIN64}
  {$define DEFAULT_DOUBLE}
  ValReal = Double;

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;
{$endif WIN64}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

{$endif CPUX86_64}

{$ifdef CPUM68K}
  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  ValReal = Real;

  { Comp type does not exist on fpu }
  Comp    = int64;
  PComp = ^Comp;

  FarPointer = Pointer;
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

  FarPointer = Pointer;
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

  FarPointer = Pointer;
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

  FarPointer = Pointer;
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

  UCS4Char            = type 0..$10ffff;
  PUCS4Char           = ^UCS4Char;
  TUCS4CharArray      = array[0..$effffff] of UCS4Char;
  PUCS4CharArray      = ^TUCS4CharArray;
  UCS4String          = array of UCS4Char;

  UTF8String          = type ansistring;
  PUTF8String         = ^UTF8String;

  HRESULT             = type Longint;
  TDateTime           = type Double;
  TError               = type Longint;

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
  PPtrUInt            = ^PtrUInt;
  PSizeInt            = ^SizeInt;

  PPointer            = ^Pointer;
  PPPointer           = ^PPointer;

  PBoolean            = ^Boolean;
  PWordBool           = ^WordBool;
  PLongBool           = ^LongBool;

  PShortString        = ^ShortString;
  PAnsiString         = ^AnsiString;

  PDate               = ^TDateTime;
  PError              = ^TError;
  PVariant            = ^Variant;
  POleVariant         = ^OleVariant;

  PWideChar           = ^WideChar;
  PPWideChar          = ^PWideChar;
  WChar               = Widechar;
  UCS2Char            = WideChar;
  PUCS2Char           = PWideChar;
  PWideString         = ^WideString;

  { Needed for fpc_get_output }
  PText               = ^Text;

  TTextLineBreakStyle = (tlbsLF,tlbsCRLF,tlbsCR);

{ procedure type }
  TProcedure  = Procedure;

{ platform dependent types }
{$i sysosh.inc}

type
  TEntryInformation = record
    InitFinalTable : Pointer;
    ThreadvarTablesTable : Pointer;
    asm_exit : Procedure;stdcall;
    PascalMain : Procedure;stdcall;
    valgrind_used : boolean;
  end;


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

  TBoundArray = array of SizeInt;

  TPCharArray = packed array[0..(MaxLongint div SizeOf(PChar))-1] of PChar;
  PPCharArray = ^TPCharArray;

(* CtrlBreak set to true signalizes Ctrl-Break signal, otherwise Ctrl-C. *)
(* Return value of true means that the signal has been processed, false  *)
(* means that default handling should be used. *)
  TCtrlBreakHandler = function (CtrlBreak: boolean): boolean;

const
{$ifdef cpui386}
  { Always i386 or newer }
  Test8086 : byte = 2;
  { Always 387 or newer. Emulated if needed. }
  Test8087 : byte = 3;
  { will be detected at startup }
  has_sse_support : boolean = false;
  has_mmx_support : boolean = false;
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
(* Value should be changed during system initialization as appropriate. *)

  { assume that this program will not spawn other threads, when the
    first thread is started the following constants need to be filled }
  IsMultiThread : longbool = FALSE;
  { set to true, if a threading helper is used before a thread
    manager has been installed }
  ThreadingAlreadyUsed : boolean = FALSE;
  { Indicates if there was an error }
  StackError : boolean = FALSE;
  InitProc : Pointer = nil;
  { compatibility }
  ModuleIsLib : Boolean = FALSE;
  ModuleIsPackage : Boolean = FALSE;
  ModuleIsCpp : Boolean = FALSE;

var
  ExitCode    : Longint; public name 'operatingsystem_result';
  RandSeed    : Cardinal;
  { Delphi compatibility }
  IsLibrary : boolean = false;
  IsConsole : boolean;
  { Threading support }
  fpc_threadvar_relocate_proc : pointer; public name 'FPC_THREADVAR_RELOCATE';

{$ifndef HAS_CMDLINE}
{Value should be changed during system initialization as appropriate.}
var cmdline:Pchar=nil;
{$endif}


ThreadVar
  ThreadID    : TThreadID;
  { Standard In- and Output }
  ErrOutput,
  Output,
  Input,
  StdOut,
  StdErr      : Text;
  InOutRes    : Word;
  { Stack checking }
  StackTop,
  StackBottom : Pointer;
  StackLength : SizeUInt;


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
Procedure FillChar(var x;count:SizeInt;Value:Byte);{$ifdef INLINEGENERICS}inline;{$endif}
Procedure FillChar(var x;count:SizeInt;Value:Boolean);{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure FillChar(var x;count:SizeInt;Value:Char);{$ifdef SYSTEMINLINE}inline;{$endif}
procedure FillByte(var x;count:SizeInt;value:byte);{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure FillWord(var x;count:SizeInt;Value:Word);
procedure FillDWord(var x;count:SizeInt;value:DWord);
function  IndexChar(const buf;len:SizeInt;b:char):SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function  IndexByte(const buf;len:SizeInt;b:byte):SizeInt;{$ifdef INLINEGENERICS}inline;{$endif}
function  Indexword(const buf;len:SizeInt;b:word):SizeInt;
function  IndexDWord(const buf;len:SizeInt;b:DWord):SizeInt;
function  CompareChar(const buf1,buf2;len:SizeInt):SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function  CompareByte(const buf1,buf2;len:SizeInt):SizeInt;{$ifdef INLINEGENERICS}inline;{$endif}
function  CompareWord(const buf1,buf2;len:SizeInt):SizeInt;
function  CompareDWord(const buf1,buf2;len:SizeInt):SizeInt;
procedure MoveChar0(const buf1;var buf2;len:SizeInt);
function  IndexChar0(const buf;len:SizeInt;b:char):SizeInt;
function  CompareChar0(const buf1,buf2;len:SizeInt):SizeInt;{$ifdef INLINEGENERICS}inline;{$endif}
procedure prefetch(const mem);[internproc:fpc_in_prefetch_var];
procedure ReadBarrier;{$ifdef INLINEGENERICS}inline;{$endif}
procedure ReadDependencyBarrier;{$ifdef INLINEGENERICS}inline;{$endif}
procedure ReadWriteBarrier;{$ifdef INLINEGENERICS}inline;{$endif}
procedure WriteBarrier;{$ifdef INLINEGENERICS}inline;{$endif}


{****************************************************************************
                          Math Routines
****************************************************************************}

Function  lo(B: Byte):Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  hi(b : Byte) : Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  lo(i : Integer) : byte;  [INTERNPROC: fpc_in_lo_Word];
Function  lo(w : Word) : byte;     [INTERNPROC: fpc_in_lo_Word];
Function  lo(l : Longint) : Word;  [INTERNPROC: fpc_in_lo_long];
Function  lo(l : DWord) : Word;    [INTERNPROC: fpc_in_lo_long];
Function  lo(i : Int64) : DWord;   [INTERNPROC: fpc_in_lo_qword];
Function  lo(q : QWord) : DWord;   [INTERNPROC: fpc_in_lo_qword];
Function  hi(i : Integer) : byte;  [INTERNPROC: fpc_in_hi_Word];
Function  hi(w : Word) : byte;     [INTERNPROC: fpc_in_hi_Word];
Function  hi(l : Longint) : Word;  [INTERNPROC: fpc_in_hi_long];
Function  hi(l : DWord) : Word;    [INTERNPROC: fpc_in_hi_long];
Function  hi(i : Int64) : DWord;   [INTERNPROC: fpc_in_hi_qword];
Function  hi(q : QWord) : DWord;   [INTERNPROC: fpc_in_hi_qword];

Function swap (X : Word) : Word;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:fpc_in_const_swap_word];
Function Swap (X : Integer) : Integer;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:fpc_in_const_swap_word];
Function swap (X : Longint) : Longint;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:fpc_in_const_swap_long];
Function Swap (X : Cardinal) : Cardinal;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:fpc_in_const_swap_long];
Function Swap (X : QWord) : QWord;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:fpc_in_const_swap_qword];
Function swap (X : Int64) : Int64;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:fpc_in_const_swap_qword];

Function Align (Addr : PtrUInt; Alignment : PtrUInt) : PtrUInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Align (Addr : Pointer; Alignment : PtrUInt) : Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}

{$ifdef FPC_HAS_FEATURE_RANDOM}
Function  Random(l:longint):longint;
Function  Random(l:int64):int64;
Function  Random: extended;
Procedure Randomize;
{$endif FPC_HAS_FEATURE_RANDOM}

{$ifdef FPC_HAS_INTERNAL_ABS_LONG and (defined(cpui386) or defined(cpux86_64))}
{$define FPC_SYSTEM_HAS_ABS_LONGINT}
Function abs(l:longint):longint;[internproc:fpc_in_abs_long];
{$else FPC_HAS_INTERNAL_ABS_LONG}
Function abs(l:Longint):Longint;[internconst:fpc_in_const_abs];{$ifdef SYSTEMINLINE}inline;{$endif}
{$endif FPC_HAS_INTERNAL_ABS_LONG}
Function abs(l:Int64):Int64;[internconst:fpc_in_const_abs];{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:Longint):Longint;[internconst:fpc_in_const_sqr];{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:Int64):Int64;[internconst:fpc_in_const_sqr];{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:QWord):QWord;[internconst:fpc_in_const_sqr];{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Longint):Boolean;[internconst:fpc_in_const_odd];{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Longword):Boolean;[internconst:fpc_in_const_odd];{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Int64):Boolean;[internconst:fpc_in_const_odd];{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:QWord):Boolean;[internconst:fpc_in_const_odd];{$ifdef SYSTEMINLINE}inline;{$endif}

function SwapEndian(const AValue: SmallInt): SmallInt;
function SwapEndian(const AValue: Word): Word;
function SwapEndian(const AValue: LongInt): LongInt;
function SwapEndian(const AValue: DWord): DWord;
function SwapEndian(const AValue: Int64): Int64;
function SwapEndian(const AValue: QWord): QWord;

function BEtoN(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function BEtoN(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
function BEtoN(const AValue: LongInt): LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function BEtoN(const AValue: DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
function BEtoN(const AValue: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
function BEtoN(const AValue: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}

function LEtoN(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function LEtoN(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
function LEtoN(const AValue: LongInt): LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function LEtoN(const AValue: DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
function LEtoN(const AValue: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
function LEtoN(const AValue: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}

function NtoBE(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoBE(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoBE(const AValue: LongInt): LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoBE(const AValue: DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoBE(const AValue: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoBE(const AValue: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}

function NtoLE(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoLE(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoLE(const AValue: LongInt): LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoLE(const AValue: DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoLE(const AValue: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
function NtoLE(const AValue: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}

{ float math routines }
{$I mathh.inc}

{****************************************************************************
                         Addr/Pointer Handling
****************************************************************************}

Function  ptr(sel,off:Longint):farpointer;[internconst:fpc_in_const_ptr];{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Cseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Dseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function  Sseg:Word;{$ifdef SYSTEMINLINE}inline;{$endif}

{****************************************************************************
                      PChar and String Handling
****************************************************************************}

function strpas(p:pchar):shortstring;{$ifdef SYSTEMINLINE}inline;{$endif}
function strlen(p:pchar):longint;external name 'FPC_PCHAR_LENGTH';

{ Shortstring functions }
Procedure Delete(var s:shortstring;index:SizeInt;count:SizeInt);
Procedure Insert(const source:shortstring;var s:shortstring;index:SizeInt);
Procedure Insert(source:Char;var s:shortstring;index:SizeInt);
Function  Pos(const substr:shortstring;const s:shortstring):SizeInt;
Function  Pos(C:Char;const s:shortstring):SizeInt;
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Function  Pos (const Substr : ShortString; const Source : AnsiString) : SizeInt;
Procedure SetString (out S : AnsiString; Buf : PChar; Len : SizeInt);
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
Procedure SetString (out S : Shortstring; Buf : PChar; Len : SizeInt);
function  ShortCompareText(const S1, S2: shortstring): SizeInt;
Function  upCase(const s:shortstring):shortstring;
Function  lowerCase(const s:shortstring):shortstring; overload;
Function  Space(b:byte):shortstring;
Function  hexStr(Val:Longint;cnt:byte):shortstring;
Function  OctStr(Val:Longint;cnt:byte):shortstring;
Function  binStr(Val:Longint;cnt:byte):shortstring;
Function  hexStr(Val:int64;cnt:byte):shortstring;
Function  OctStr(Val:int64;cnt:byte):shortstring;
Function  binStr(Val:int64;cnt:byte):shortstring;
Function  hexStr(Val:qword;cnt:byte):shortstring;
Function  OctStr(Val:qword;cnt:byte):shortstring;
Function  binStr(Val:qword;cnt:byte):shortstring;
Function  hexStr(Val:Pointer):shortstring;

{ Char functions }
Function chr(b : byte) : Char;      [INTERNPROC: fpc_in_chr_byte];
Function  upCase(c:Char):Char;
Function  lowerCase(c:Char):Char; overload;
function  pos(const substr : shortstring;c:char): SizeInt;


{****************************************************************************
                             AnsiString Handling
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Procedure UniqueString(var S : AnsiString);external name 'FPC_ANSISTR_UNIQUE';
Function  Pos (const Substr : AnsiString; const Source : AnsiString) : SizeInt;
Function  Pos (c : Char; const s : AnsiString) : SizeInt;
Procedure Insert (const Source : AnsiString; var S : AnsiString; Index : SizeInt);
Procedure Delete (var S : AnsiString; Index,Size: SizeInt);
Function  StringOfChar(c : char;l : SizeInt) : AnsiString;
function  upcase(const s : ansistring) : ansistring;
function  lowercase(const s : ansistring) : ansistring;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}


{****************************************************************************
                             WideString Handling
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
{$i wstringh.inc}
{$endif FPC_HAS_FEATURE_WIDESTRINGS}


{****************************************************************************
                          Untyped File Management
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_FILEIO}
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:File;const Name:string);
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:File;p:pchar);
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:File;c:char);
Procedure Rewrite(var f:File;l:Longint);
Procedure Rewrite(var f:File);
Procedure Reset(var f:File;l:Longint);
Procedure Reset(var f:File);
Procedure Close(var f:File);
Procedure BlockWrite(var f:File;const Buf;Count:Int64;var Result:Int64);
Procedure BlockWrite(var f:File;const Buf;Count:Longint;var Result:Longint);
Procedure BlockWrite(var f:File;const Buf;Count:Cardinal;var Result:Cardinal);
Procedure BlockWrite(var f:File;const Buf;Count:Word;var Result:Word);
Procedure BlockWrite(var f:File;const Buf;Count:Word;var Result:Integer);
Procedure BlockWrite(var f:File;const Buf;Count:Longint);
Procedure BlockRead(var f:File;var Buf;count:Int64;var Result:Int64);
Procedure BlockRead(var f:File;var Buf;count:Longint;var Result:Longint);
Procedure BlockRead(var f:File;var Buf;count:Cardinal;var Result:Cardinal);
Procedure BlockRead(var f:File;var Buf;count:Word;var Result:Word);
Procedure BlockRead(var f:File;var Buf;count:Word;var Result:Integer);
Procedure BlockRead(var f:File;var Buf;count:Int64);
Function  FilePos(var f:File):Int64;
Function  FileSize(var f:File):Int64;
Procedure Seek(var f:File;Pos:Int64);
Function  EOF(var f:File):Boolean;
Procedure Erase(var f:File);
Procedure Rename(var f:File;const s:string);
Procedure Rename(var f:File;p:pchar);
Procedure Rename(var f:File;c:char);
Procedure Truncate (var F:File);
{$endif FPC_HAS_FEATURE_FILEIO}


{****************************************************************************
                           Typed File Management
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_FILEIO}
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:TypedFile;const Name:string);
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:TypedFile;p:pchar);
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} f:TypedFile;c:char);
Procedure Reset(var f : TypedFile);   [INTERNPROC: fpc_in_Reset_TypedFile];
Procedure Rewrite(var f : TypedFile); [INTERNPROC: fpc_in_Rewrite_TypedFile];
{$endif FPC_HAS_FEATURE_FILEIO}

{****************************************************************************
                            Text File Management
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_TEXTIO}
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} t:Text;const s:string);
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} t:Text;p:pchar);
Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} t:Text;c:char);
Procedure Close(var t:Text);
Procedure Rewrite(var t:Text);
Procedure Reset(var t:Text);
Procedure Append(var t:Text);
Procedure Flush(var t:Text);
Procedure Erase(var t:Text);
Procedure Rename(var t:Text;const s:string);
Procedure Rename(var t:Text;p:pchar);
Procedure Rename(var t:Text;c:char);
Function  EOF(var t:Text):Boolean;
Function  EOF:Boolean;
Function  EOLn(var t:Text):Boolean;
Function  EOLn:Boolean;
Function  SeekEOLn (var t:Text):Boolean;
Function  SeekEOF (var t:Text):Boolean;
Function  SeekEOLn:Boolean;
Function  SeekEOF:Boolean;
Procedure SetTextBuf(var f:Text; var Buf);[INTERNPROC:fpc_in_settextbuf_file_x];
Procedure SetTextBuf(var f:Text; var Buf; Size:SizeInt);
Procedure SetTextLineEnding(var f:Text; Ending:string);
{$endif FPC_HAS_FEATURE_TEXTIO}

{****************************************************************************
                            Directory Management
****************************************************************************}


{$ifdef FPC_HAS_FEATURE_FILEIO}
Procedure chdir(const s:string);
Procedure mkdir(const s:string);
Procedure rmdir(const s:string);
Procedure getdir(drivenr:byte;var dir:shortstring);
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
Procedure getdir(drivenr:byte;var dir:ansistring);
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
{$endif FPC_HAS_FEATURE_FILEIO}


{*****************************************************************************
                             Miscellaneous
*****************************************************************************}

{ os independent calls to allow backtraces }
{$IFDEF INTERNAL_BACKTRACE}
// inserted in compiler/psystem.pas
//function get_frame:pointer;[INTERNPROC:fpc_in_get_frame];
(*
// still defined externally
function get_caller_addr(framebp:pointer):pointer;[INTERNPROC:fpc_in_get_caller_addr];
function get_caller_frame(framebp:pointer):pointer;[INTERNPROC:fpc_in_get_caller_frame];
*)
{$ELSE}
function get_frame:pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
{$ENDIF}

function get_caller_addr(framebp:pointer):pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
function get_caller_frame(framebp:pointer):pointer;{$ifdef SYSTEMINLINE}inline;{$endif}

Function IOResult:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Sptr:Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:fpc_in_const_ptr];

{$ifdef FPC_HAS_FEATURE_PROCESSES}
Function GetProcessID:SizeUInt;
Function GetThreadID:TThreadID;{$ifdef SYSTEMINLINE}inline;{$endif}
{$endif FPC_HAS_FEATURE_PROCESSES}

function InterLockedIncrement (var Target: longint) : longint; public name 'FPC_INTERLOCKEDINCREMENT';
function InterLockedDecrement (var Target: longint) : longint; public name 'FPC_INTERLOCKEDDECREMENT';
function InterLockedExchange (var Target: longint;Source : longint) : longint; public name 'FPC_INTERLOCKEDEXCHANGE';
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; public name 'FPC_INTERLOCKEDEXCHANGEADD';
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; public name 'FPC_INTERLOCKEDCOMPAREEXCHANGE';
{$ifdef cpu64}
function InterLockedIncrement64 (var Target: int64) : int64; public name 'FPC_INTERLOCKEDINCREMENT64';
function InterLockedDecrement64 (var Target: int64) : int64; public name 'FPC_INTERLOCKEDDECREMENT64';
function InterLockedExchange64 (var Target: int64;Source : int64) : int64; public name 'FPC_INTERLOCKEDEXCHANGE64';
function InterLockedExchangeAdd64 (var Target: int64;Source : int64) : int64; public name 'FPC_INTERLOCKEDEXCHANGEADD64';
function InterlockedCompareExchange64(var Target: int64; NewValue: int64; Comperand: int64): int64; public name 'FPC_INTERLOCKEDCOMPAREEXCHANGE64';
{$endif cpu64}
{ Pointer overloads }
{$ifdef cpu64}
function InterLockedIncrement (var Target: Pointer) : Pointer; external name 'FPC_INTERLOCKEDINCREMENT64';
function InterLockedDecrement (var Target: Pointer) : Pointer; external name 'FPC_INTERLOCKEDDECREMENT64';
function InterLockedExchange (var Target: Pointer;Source : Pointer) : Pointer; external name 'FPC_INTERLOCKEDEXCHANGE64';
function InterLockedExchangeAdd (var Target: Pointer;Source : Pointer) : Pointer; external name 'FPC_INTERLOCKEDEXCHANGEADD64';
function InterlockedCompareExchange(var Target: Pointer; NewValue: Pointer; Comperand: Pointer): Pointer; external name 'FPC_INTERLOCKEDCOMPAREEXCHANGE64';
{$else cpu64}
function InterLockedIncrement (var Target: Pointer) : Pointer; external name 'FPC_INTERLOCKEDINCREMENT';
function InterLockedDecrement (var Target: Pointer) : Pointer; external name 'FPC_INTERLOCKEDDECREMENT';
function InterLockedExchange (var Target: Pointer;Source : Pointer) : Pointer; external name 'FPC_INTERLOCKEDEXCHANGE';
function InterLockedExchangeAdd (var Target: Pointer;Source : Pointer) : Pointer; external name 'FPC_INTERLOCKEDEXCHANGEADD';
function InterlockedCompareExchange(var Target: Pointer; NewValue: Pointer; Comperand: Pointer): Pointer; external name 'FPC_INTERLOCKEDCOMPAREEXCHANGE';
{$endif cpu64}


{*****************************************************************************
                          Init / Exit / ExitProc
*****************************************************************************}

type
  TRuntimeError =
    (reNone, reOutOfMemory, reInvalidPtr, reDivByZero, reRangeError,
     reIntOverflow, reInvalidOp, reZeroDivide, reOverflow, reUnderflow,
     reInvalidCast, reAccessViolation, rePrivInstruction, reControlBreak,
     reStackOverflow, reVarTypeCast, reVarInvalidOp, reVarDispatch,
     reVarArrayCreate, reVarNotArray, reVarArrayBounds, reAssertionFailed,
     reExternalException, reIntfCastError, reSafeCallError, reQuit,
     reCodesetConversion);

Const
  // Please keep locations corresponding to location in array above
  RuntimeErrorExitCodes : Array[TRuntimeError] of Byte = (
     0  , 203, 204, 200,  201,
     215, 207, 200, 205,  206,
     219, 216, 218, 217,
     202, 220, 221, 222,
     223, 224, 225, 227,
     212, 228, 229, 233,
     234);

Procedure Error(RunTimeError : TRunTimeError);
{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
Function  Paramcount:Longint;
Function  ParamStr(l:Longint):string;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
Procedure Dump_Stack(var f : text;bp:pointer);
{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
procedure DumpExceptionBackTrace(var f:text);
{$endif FPC_HAS_FEATURE_EXCEPTIONS}
{$endif FPC_HAS_FEATURE_CONSOLEIO}

Procedure RunError(w:Word);
Procedure RunError;{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure halt(errnum:byte);
{$ifdef FPC_HAS_FEATURE_HEAP}
Procedure AddExitProc(Proc:TProcedure);
{$endif FPC_HAS_FEATURE_HEAP}
Procedure halt;{$ifdef SYSTEMINLINE}inline;{$endif}

{ Need to be exported for threads unit }
{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
Procedure SysInitExceptions;
{$endif FPC_HAS_FEATURE_EXCEPTIONS}
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
procedure SysInitStdIO;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}

{*****************************************************************************
                         Abstract/Assert/Error Handling
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_HEAP}
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
function ArrayStringToPPchar(const S:Array of AnsiString;reserveentries:Longint):ppchar; // const ?
Function StringToPPChar(var S:AnsiString;ReserveEntries:integer):ppchar;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}
Function StringToPPChar(S: PChar;ReserveEntries:integer):ppchar;
{$endif FPC_HAS_FEATURE_HEAP}


procedure AbstractError;external name 'FPC_ABSTRACTERROR';
Function  SysBackTraceStr(Addr:Pointer): ShortString;
Procedure SysAssert(const Msg,FName:ShortString;LineNo:Longint;ErrorAddr:Pointer);
(* Supposed to return address of previous CtrlBreakHandler *)
(* (may be nil), returned value of pointer (-1) means that *)
(* attempt to setup CtrlBreakHandler wasn't successful.    *)
function SysSetCtrlBreakHandler (Handler: TCtrlBreakHandler): TCtrlBreakHandler;

{ Error handlers }
Type
  TBackTraceStrFunc = Function (Addr: Pointer): ShortString;
  TErrorProc = Procedure (ErrNo : Longint; Address,Frame : Pointer);
  TAbstractErrorProc = Procedure;
  TAssertErrorProc = Procedure(const msg,fname:ShortString;lineno:longint;erroraddr:pointer);
  TSafeCallErrorProc = Procedure(error : HResult;addr : pointer);


const
  BackTraceStrFunc  : TBackTraceStrFunc = @SysBackTraceStr;
  ErrorProc         : TErrorProc = nil;
  AbstractErrorProc : TAbstractErrorProc = nil;
  AssertErrorProc   : TAssertErrorProc = @SysAssert;
  SafeCallErrorProc : TSafeCallErrorProc = nil;

{*****************************************************************************
                          SetJmp/LongJmp
*****************************************************************************}

{$i setjumph.inc}


{*****************************************************************************
                       Object Pascal support
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_CLASSES}
{$i objpash.inc}
{$endif FPC_HAS_FEATURE_CLASSES}

{*****************************************************************************
                           Variant support
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_VARIANTS}
{$i varianth.inc}
{$endif FPC_HAS_FEATURE_VARIANTS}

{*****************************************************************************
                   Internal helper routines support
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
{$i dynarrh.inc}
{$endif FPC_HAS_FEATURE_DYNARRAYS}

{ documenting compiler proc. is useless, they shouldn't be used by the user anyways }
{$ifndef fpdocsystem}
{$i compproc.inc}
{$endif fpdocsystem}

{*****************************************************************************
                               Heap
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_HEAP}
{$i heaph.inc}
{$endif FPC_HAS_FEATURE_HEAP}

{*****************************************************************************
                          Thread support
*****************************************************************************}

{ Generic threadmanager }
{$ifdef FPC_HAS_FEATURE_THREADING}
{$i threadh.inc}
{$endif FPC_HAS_FEATURE_THREADING}

{*****************************************************************************
                          Resources support
*****************************************************************************}

{$i resh.inc}

{*****************************************************************************
                   FPDoc phony declarations.
*****************************************************************************}

{$ifdef fpdocsystem}
{$i system.fpd}
{$endif}
