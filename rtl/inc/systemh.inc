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
{$ifdef COMPPROCINLINEFIXED}
{$define SYSTEMINLINE}
{$endif COMPPROCINLINEFIXED}

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

{****************************************************************************
                         Global Types and Constants
****************************************************************************}

Type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;

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

  UCS4Char            = type LongWord;
  PUCS4Char           = ^UCS4Char;
  TUCS4CharArray      = array[0..$effffff] of UCS4Char;
  PUCS4CharArray      = ^TUCS4CharArray;
  UCS4String          = array of UCS4Char;

  UTF8String          = type ansistring;
  PUTF8String         = ^UTF8String;

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
Procedure FillChar(var x;count:SizeInt;Value:Boolean);{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure FillChar(var x;count:SizeInt;Value:Char);{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure FillChar(var x;count:SizeInt;Value:Byte);{$ifdef INLINEGENERICS}inline;{$endif}
procedure FillByte(var x;count:SizeInt;value:byte);{$ifdef INLINEGENERICS}inline;{$endif}
Procedure FillWord(var x;count:SizeInt;Value:Word);
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
procedure prefetch(const mem);[internproc:fpc_in_prefetch_var];


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

Function Align (Addr : PtrInt; Alignment : PtrInt) : PtrInt;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Align (Addr : Pointer; Alignment : PtrInt) : Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}

Function  Random(l:longint):longint;
Function  Random(l:int64):int64;
Function  Random: extended;
Procedure Randomize;

Function abs(l:Longint):Longint;[internconst:fpc_in_const_abs];{$ifdef SYSTEMINLINE}inline;{$endif}
Function abs(l:Int64):Int64;[internconst:fpc_in_const_abs];{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:Longint):Longint;[internconst:fpc_in_const_sqr];{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:Int64):Int64;[internconst:fpc_in_const_sqr];{$ifdef SYSTEMINLINE}inline;{$endif}
Function sqr(l:QWord):QWord;[internconst:fpc_in_const_sqr];{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Longint):Boolean;[internconst:fpc_in_const_odd];{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Longword):Boolean;[internconst:fpc_in_const_odd];{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:Int64):Boolean;[internconst:fpc_in_const_odd];{$ifdef SYSTEMINLINE}inline;{$endif}
Function odd(l:QWord):Boolean;[internconst:fpc_in_const_odd];{$ifdef SYSTEMINLINE}inline;{$endif}

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

function strpas(p:pchar):shortstring;external name 'FPC_PCHAR_TO_SHORTSTR';
function strlen(p:pchar):longint;external name 'FPC_PCHAR_LENGTH';

{ Shortstring functions }
Procedure Delete(var s:shortstring;index:SizeInt;count:SizeInt);
Procedure Insert(const source:shortstring;var s:shortstring;index:SizeInt);
Procedure Insert(source:Char;var s:shortstring;index:SizeInt);
Function  Pos(const substr:shortstring;const s:shortstring):SizeInt;
Function  Pos(C:Char;const s:shortstring):SizeInt;
Function  Pos (const Substr : ShortString; const Source : AnsiString) : SizeInt;
Procedure SetString (var S : Shortstring; Buf : PChar; Len : SizeInt);
Procedure SetString (var S : AnsiString; Buf : PChar; Len : SizeInt);
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
Function chr(b : byte) : Char;      [INTERNPROC: fpc_in_chr_byte];
Function  upCase(c:Char):Char;
Function  lowerCase(c:Char):Char; overload;
function  pos(const substr : shortstring;c:char): SizeInt;


{****************************************************************************
                             AnsiString Handling
****************************************************************************}

Procedure UniqueString(var S : AnsiString);external name 'FPC_ANSISTR_UNIQUE';
Function  Pos (const Substr : AnsiString; const Source : AnsiString) : SizeInt;
Function  Pos (c : Char; const s : AnsiString) : SizeInt;
Procedure Insert (const Source : AnsiString; var S : AnsiString; Index : SizeInt);
Procedure Delete (var S : AnsiString; Index,Size: SizeInt);
Function  StringOfChar(c : char;l : SizeInt) : AnsiString;
function  upcase(const s : ansistring) : ansistring;
function  lowercase(const s : ansistring) : ansistring;


{****************************************************************************
                             WideString Handling
****************************************************************************}

{$i wstringh.inc}


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

Procedure Assign(var f:TypedFile;const Name:string);
Procedure Assign(var f:TypedFile;p:pchar);
Procedure Assign(var f:TypedFile;c:char);
Procedure Reset(var f : TypedFile);   [INTERNPROC: fpc_in_Reset_TypedFile];
Procedure Rewrite(var f : TypedFile); [INTERNPROC: fpc_in_Rewrite_TypedFile];

{****************************************************************************
                            Text File Management
****************************************************************************}

Procedure Assign(var t:Text;const s:string);
Procedure Assign(var t:Text;p:pchar);
Procedure Assign(var t:Text;c:char);
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

{****************************************************************************
                            Directory Management
****************************************************************************}

Procedure chdir(const s:string);
Procedure mkdir(const s:string);
Procedure rmdir(const s:string);
Procedure getdir(drivenr:byte;var dir:shortstring);
Procedure getdir(drivenr:byte;var dir:ansistring);


{*****************************************************************************
                             Miscellaneous
*****************************************************************************}

{ os independent calls to allow backtraces }
function get_frame:pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
function get_caller_addr(framebp:pointer):pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
function get_caller_frame(framebp:pointer):pointer;{$ifdef SYSTEMINLINE}inline;{$endif}

Function IOResult:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Sptr:Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:fpc_in_const_ptr];
Function GetProcessID:SizeUInt;
Function GetThreadID:TThreadID;{$ifdef SYSTEMINLINE}inline;{$endif}


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
Function StringToPPChar(var S:AnsiString;ReserveEntries:integer):ppchar;
Function StringToPPChar(S: PChar;ReserveEntries:integer):ppchar;


procedure AbstractError;external name 'FPC_ABSTRACTERROR';
Function  SysBackTraceStr(Addr:Pointer): ShortString;
Procedure SysAssert(const Msg,FName:ShortString;LineNo:Longint;ErrorAddr:Pointer);

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

{$i varianth.inc}

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
                          Resources support
*****************************************************************************}

{$i resh.inc}

{*****************************************************************************
                   FPDoc phony declarations.
*****************************************************************************}

{$ifdef fpdocsystem}
{$i system.fpd}
{$endif}

