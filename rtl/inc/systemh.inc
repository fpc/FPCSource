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
{$ifdef i386}
  {$maxfpuregisters 0}
{$endif i386}

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

{$ifdef i386}
  StrLenInt = LongInt;

  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Extended;
{$endif i386}

{$ifdef x86_64}
  StrLenInt = LongInt;

  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Extended;
{$endif x86_64}

{$ifdef m68k}
  StrLenInt = Longint;

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Real;

  { Comp type does not exist on fpu }
  Comp    = int64;
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
{$endif}

{$ifdef powerpc}
  StrLenInt = LongInt;

  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Double;

  { map comp to int64, but this doesn't mean to compile the comp support in! }
  Comp = Int64;
{$endif powerpc}

{$ifdef sparc}
  StrLenInt = LongInt;

  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Double;

  { map comp to int64, but this doesn't mean to compile the comp support in! }
  Comp = Int64;
{$endif sparc}

{ Zero - terminated strings }
  PChar               = ^Char;
  PPChar              = ^PChar;

  TAnsiChar           = Char;
  AnsiChar            = TAnsiChar;
  PAnsiChar           = ^TAnsiChar;
  PPAnsiChar          = ^PAnsiChar;

// UCS4Char            = type LongWord;
  UCS4Char            = LongWord;
  PUCS4Char           = ^UCS4Char;
  TUCS4CharArray      = array[0..$effffff] of UCS4Char;
  PUCS4CharArray      = ^TUCS4CharArray;
{$ifndef VER1_0}
  UCS4String          = array of UCS4Char;
{$endif VER1_0}


//  UTF8String          = type ansistring;
  UTF8String          = ansistring;
  PUTF8String         = ^UTF8String;

{$ifndef HASCURRENCY}
  Currency            = Int64;
{$endif HASCURRENCY}
  HRESULT             = Longint;
  TDateTime           = Double;
  Error               = Longint;

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
{$else}
  WChar               = Word;
{$endif HASWIDECHAR}
{$ifdef HASWIDESTRING}
  PWideString         = ^WideString;
{$endif HASWIDESTRING}

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

const
  maxint   = maxsmallint;

{ Compatibility With  TP }
const
{$ifdef i386}
  Test8086 : byte = 2;       { Always i386 or newer }
  Test8087 : byte = 3;       { Always 387 or newer. Emulated if needed. }
{$endif i386}
{$ifdef m68k}
  Test68000 : byte = 0;      { Must be determined at startup for both }
  Test68881 : byte = 0;
{$endif}

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

  { assume that this program will not spawn other threads, when the
    first thread is started the following constants need to be filled }
  IsMultiThread : boolean = FALSE;
  { Indicates if there was an error }
  StackError : boolean = FALSE;

var
  ExitCode    : Word;
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
  { Standard In- and Output }
  Output,
  Input,
  StdOut,
  StdErr      : Text;
  InOutRes    : Word;
  { Stack checking }
  StackBottom,
  StackLength : Cardinal;

{****************************************************************************
                        Processor specific routines
****************************************************************************}

Procedure Move(const source;var dest;count:Longint);
Procedure FillChar(Var x;count:Longint;Value:Boolean);{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure FillChar(Var x;count:Longint;Value:Char);{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure FillChar(Var x;count:Longint;Value:Byte);
procedure FillByte(var x;count:longint;value:byte);
Procedure FillWord(Var x;count:Longint;Value:Word);
procedure FillDWord(var x;count:longint;value:DWord);
function  IndexChar(const buf;len:longint;b:char):longint;
function  IndexByte(const buf;len:longint;b:byte):longint;
function  Indexword(const buf;len:longint;b:word):longint;
function  IndexDWord(const buf;len:longint;b:DWord):longint;
function  CompareChar(const buf1,buf2;len:longint):longint;
function  CompareByte(const buf1,buf2;len:longint):longint;
function  CompareWord(const buf1,buf2;len:longint):longint;
function  CompareDWord(const buf1,buf2;len:longint):longint;
procedure MoveChar0(const buf1;var buf2;len:longint);
function  IndexChar0(const buf;len:longint;b:char):longint;
function  CompareChar0(const buf1,buf2;len:longint):longint;


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

Function  Random(l:cardinal):cardinal;
Function  Random(l:longint):longint;
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
function strlen(p:pchar):longint;

{ Shortstring functions }
{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S:ShortString;len:StrLenInt);
{$endif INTERNSETLENGTH}
{$ifndef InternCopy}
Function  Copy(const s:shortstring;index:StrLenInt;count:StrLenInt):shortstring;
{$endif interncopy}
Procedure Delete(Var s:shortstring;index:StrLenInt;count:StrLenInt);
Procedure Insert(const source:shortstring;Var s:shortstring;index:StrLenInt);
Procedure Insert(source:Char;Var s:shortstring;index:StrLenInt);
Function  Pos(const substr:shortstring;const s:shortstring):StrLenInt;
Function  Pos(C:Char;const s:shortstring):StrLenInt;
Procedure SetString (Var S : Shortstring; Buf : PChar; Len : Longint);
Procedure SetString (Var S : AnsiString; Buf : PChar; Len : Longint);
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
function  copy(c:char;index : StrLenInt;count : StrLenInt): shortstring;
{$endif interncopy}
function  pos(const substr : shortstring;c:char): StrLenInt;
{$ifndef INTERNLENGTH}
function  length(c:char):byte;
{$endif INTERNLENGTH}


{****************************************************************************
                             AnsiString Handling
****************************************************************************}

{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : AnsiString; l : Longint);
{$endif INTERNSETLENGTH}
Procedure UniqueString (Var S : AnsiString);
{$ifndef INTERNLENGTH}
Function  Length (Const S : AnsiString) : Longint;
{$endif INTERNLENGTH}
{$ifndef InternCopy}
Function  Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;
{$endif interncopy}
Function  Pos (Const Substr : AnsiString; Const Source : AnsiString) : Longint;
Function  Pos (c : Char; Const s : AnsiString) : Longint;
Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : Longint);
Procedure Delete (Var S : AnsiString; Index,Size: Longint);
Function  StringOfChar(c : char;l : longint) : AnsiString;
function  upcase(const s : ansistring) : ansistring;
function  lowercase(const s : ansistring) : ansistring;


{****************************************************************************
                             WideString Handling
****************************************************************************}

{$ifdef HASWIDESTRING}
{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S : WideString; l : Longint);
{$endif INTERNSETLENGTH}
Procedure UniqueString (Var S : WideString);
{$ifndef INTERNLENGTH}
Function  Length (Const S : WideString) : Longint;
{$endif INTERNLENGTH}
{$ifndef InternCopy}
Function  Copy (Const S : WideString; Index,Size : Longint) : WideString;
{$endif interncopy}
Function  Pos (Const Substr : WideString; Const Source : WideString) : Longint;
Function  Pos (c : Char; Const s : WideString) : Longint;
Function  Pos (c : WideChar; Const s : WideString) : Longint;
Procedure Insert (Const Source : WideString; Var S : WideString; Index : Longint);
Procedure Delete (Var S : WideString; Index,Size: Longint);

function WideCharToString(S : PWideChar) : AnsiString;
function StringToWideChar(const Src : AnsiString;Dest : PWideChar;DestSize : LongInt) : PWideChar;
function WideCharLenToString(S : PWideChar;Len : LongInt) : AnsiString;
procedure WideCharLenToStrVar(Src : PWideChar;Len : LongInt;var Dest : AnsiString);
procedure WideCharToStrVar(S : PWideChar;var Dest : AnsiString);
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


{****************************************************************************
                            Directory Management
****************************************************************************}

Procedure chdir(const s:string);
Procedure mkdir(const s:string);
Procedure rmdir(const s:string);
Procedure getdir(drivenr:byte;Var dir:shortstring);
Procedure getdir(drivenr:byte;Var dir:ansistring);


{*****************************************************************************
                             Miscelleaous
*****************************************************************************}

{ os independent calls to allow backtraces }
function get_frame:longint;{$ifdef SYSTEMINLINE}inline;{$endif}
function get_caller_addr(framebp:longint):longint;{$ifdef SYSTEMINLINE}inline;{$endif}
function get_caller_frame(framebp:longint):longint;{$ifdef SYSTEMINLINE}inline;{$endif}

Function IOResult:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Function Sptr:Longint;{$ifdef SYSTEMINLINE}inline;{$endif}


{*****************************************************************************
                          Init / Exit / ExitProc
*****************************************************************************}

Function  Paramcount:Longint;
Function  ParamStr(l:Longint):string;
Procedure Dump_Stack(var f : text;bp:Longint);
Procedure RunError(w:Word);
Procedure RunError;{$ifdef SYSTEMINLINE}inline;{$endif}
Procedure halt(errnum:byte);
Procedure AddExitProc(Proc:TProcedure);
Procedure halt;{$ifdef SYSTEMINLINE}inline;{$endif}

{ Need to be exported for threads unit }
Procedure SysInitExceptions;
procedure SysInitStdIO;
Procedure SysResetFPU;

{*****************************************************************************
                         Abstract/Assert/Error Handling
*****************************************************************************}

procedure AbstractError;
Function  SysBackTraceStr(Addr: Longint): ShortString;
Procedure SysAssert(Const Msg,FName:ShortString;LineNo,ErrorAddr:Longint);

{ Error handlers }
Type
  TBackTraceStrFunc = Function (Addr: Longint): ShortString;
  TErrorProc = Procedure (ErrNo : Longint; Address,Frame : Pointer);
  TAbstractErrorProc = Procedure;
  TAssertErrorProc = Procedure(const msg,fname:ShortString;lineno,erroraddr:longint);



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
  Revision 1.60  2002-11-16 20:12:22  florian
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