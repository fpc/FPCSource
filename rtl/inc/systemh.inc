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

{ don't use FPU registervariables on the i386 }
{$ifdef i386}
  {$maxfpuregisters 0}
{$endif i386}

{ needed for insert,delete,readln }
{$P+}

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
  DWord    = Cardinal;
  LongWord = Cardinal;
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
{$endif powerpc}

{ Zero - terminated strings }
  PChar               = ^Char;
  PPChar              = ^PChar;

  TAnsiChar           = Char;
  AnsiChar            = TAnsiChar;
  PAnsiChar           = ^TAnsiChar;
  PPAnsiChar          = ^PAnsiChar;

  UCS4Char            = Cardinal;
  PUCS4Char           = ^UCS4Char;
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
{$ifdef SUPPORT_EXTENDED}
  PExtended           = ^Extended;
{$endif SUPPORT_EXTENDED}

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
  { code to use comps in int64mul and div code is commented out! (JM) }
  FPUInt64 : boolean = false; { set this to false if you don't want that }
                              { the fpu does int64*int64 and             }
                              { int64 div int64, if the * is overflow    }
                              { checked, it is done in software          }
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

 { Delphi Compatibility }
 { assume that this program will not spawn other threads. }
  IsMultiThread : boolean = FALSE;
 { Indicates if there was an error }
  StackError : boolean = FALSE;

var
{ Standard In- and Output }
  Output,
  Input,
  StdOut,
  StdErr      : Text;
  ExitCode    : Word;
  StackBottom,
  RandSeed    : Cardinal;
  { Delphi compatibility }
  IsLibrary : boolean;
  IsConsole : boolean;
{$ifdef MT}
ThreadVar
{$else MT}
Var
{$endif MT}
  InOutRes    : Word;

{****************************************************************************
                        Processor specific routines
****************************************************************************}

Procedure Move(const source;var dest;count:Longint);
Procedure FillChar(Var x;count:Longint;Value:Boolean);
Procedure FillChar(Var x;count:Longint;Value:Char);
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
Function  lo(B: Byte):Byte;
Function  hi(w:Word):byte;
Function  hi(i:Integer):byte;
Function  hi(l:Longint):Word;
Function  hi(b : Byte) : Byte;
Function  hi(l: DWord): Word;
Function  Swap (X:Word):Word;
Function  Swap (X:Integer):Integer;
Function  Swap (X:Cardinal):Cardinal;
Function  Swap (X:LongInt):LongInt;
Function  lo(q : QWord) : DWord;
Function  lo(i : Int64) : DWord;
Function  hi(q : QWord) : DWord;
Function  hi(i : Int64) : DWord;
Function  Swap (X:QWord):QWord;
Function  Swap (X:Int64):Int64;

Function  Random(l:cardinal):cardinal;
Function  Random(l:longint):longint;
Function  Random: extended;
Procedure Randomize;

Function abs(l:Longint):Longint;
Function abs(l:Int64):Int64;
Function sqr(l:Longint):Longint;
Function sqr(l:Int64):Int64;
Function sqr(l:QWord):QWord;
Function odd(l:Longint):Boolean;
Function odd(l:Cardinal):Boolean;
Function odd(l:Int64):Boolean;
Function odd(l:QWord):Boolean;

{ float math routines }
{$I mathh.inc}

{****************************************************************************
                         Addr/Pointer Handling
****************************************************************************}

Function  ptr(sel,off:Longint):farpointer;
Function  Cseg:Word;
Function  Dseg:Word;
Function  Sseg:Word;

{****************************************************************************
                      PChar and String Handling
****************************************************************************}

function strpas(p:pchar):shortstring;
function strlen(p:pchar):longint;

{ Shortstring functions }
{$ifndef INTERNSETLENGTH}
Procedure SetLength (Var S:ShortString;len:StrLenInt);
{$endif INTERNSETLENGTH}
Function  Copy(const s:shortstring;index:StrLenInt;count:StrLenInt):shortstring;
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
Function  binStr(Val:Longint;cnt:byte):shortstring;
Function  hexStr(Val:int64;cnt:byte):shortstring;
Function  binStr(Val:int64;cnt:byte):shortstring;

{ Char functions }
Function  Chr(b:byte):Char;
Function  upCase(c:Char):Char;
Function  lowerCase(c:Char):Char; overload;
function  copy(c:char;index : StrLenInt;count : StrLenInt): shortstring;
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
Function  Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;
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
Function  Copy (Const S : WideString; Index,Size : Longint) : WideString;
Function  Pos (Const Substr : WideString; Const Source : WideString) : Longint;
Function  Pos (c : Char; Const s : WideString) : Longint;
Function  Pos (c : WideChar; Const s : WideString) : Longint;
Procedure Insert (Const Source : WideString; Var S : WideString; Index : Longint);
Procedure Delete (Var S : WideString; Index,Size: Longint);
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
function get_frame:longint;
function get_caller_addr(framebp:longint):longint;
function get_caller_frame(framebp:longint):longint;

Function IOResult:Word;
Function Sptr:Longint;


{*****************************************************************************
                          Init / Exit / ExitProc
*****************************************************************************}

Function  Paramcount:Longint;
Function  ParamStr(l:Longint):string;
Procedure Dump_Stack(var f : text;bp:Longint);
Procedure RunError(w:Word);
Procedure RunError;
Procedure halt(errnum:byte);
Procedure AddExitProc(Proc:TProcedure);
Procedure halt;


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
  Revision 1.49  2002-07-26 21:29:03  florian
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

  Revision 1.39  2001/11/14 22:59:11  michael
  + Initial variant support

  Revision 1.38  2001/11/07 14:59:20  michael
  + Moved PathDelim,DriveDelim,PathSep to sysutilh

  Revision 1.37  2001/11/07 13:52:37  michael
  + Added PathDelim,DriveDelim,PathSep

  Revision 1.36  2001/10/09 02:32:51  carl
  * bugfix #1639 (IsMultiThread varialbe setting)

  Revision 1.35  2001/08/19 21:02:01  florian
    * fixed and added a lot of stuff to get the Jedi DX8 headers
      compiled

  Revision 1.34  2001/08/01 18:01:20  peter
    * WChar fix to compile also with 1.0.x

  Revision 1.33  2001/08/01 15:00:11  jonas
    + "compproc" helpers
    * renamed several helpers so that their name is the same as their
      "public alias", which should facilitate the conversion of processor
      specific code in the code generator to processor independent code
    * some small fixes to the val_ansistring and val_widestring helpers
      (always immediately exit if the source string is longer than 255
       chars)
    * fixed fpc_dynarray_high and fpc_dynarray_length if the dynarray is
      still nil (used to crash, now return resp -1 and 0)

  Revision 1.32  2001/07/31 08:57:22  marco
   * Either I did something wrong, or Peter's merge killed wchar decl. Fixed.

  Revision 1.31  2001/07/30 21:38:55  peter
    * m68k updates merged

  Revision 1.29  2001/07/29 13:49:15  peter
    * m68k updates merged

  Revision 1.28  2001/07/15 11:57:16  peter
    * merged m68k updates

  Revision 1.27  2001/07/10 18:04:37  peter
    * merged textfile, readlink and concat ansistring fixes

  Revision 1.26  2001/07/09 21:15:41  peter
    * Length made internal
    * Add array support for Length

  Revision 1.25  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.24  2001/06/04 11:43:51  peter
    * Formal const to var fixes
    * Hexstr(int64) added

  Revision 1.23  2001/06/03 20:17:06  peter
    * ucs4char added

  Revision 1.22  2001/05/16 17:44:25  jonas
    + odd() for cardinal, int64 and qword (merged)

  Revision 1.21  2001/05/09 19:57:07  peter
  *** empty log message ***

  Revision 1.20  2001/04/23 18:25:45  peter
    * m68k updates

  Revision 1.19  2001/04/13 18:06:07  peter
    * upcase, lowercase for ansistring

  Revision 1.18  2001/03/22 23:26:05  florian
    * some new types added like PBoolean

  Revision 1.17  2001/03/21 23:29:40  florian
    + sLineBreak and misc. stuff for Kylix compatiblity

  Revision 1.16  2001/01/24 21:47:18  florian
    + more MT stuff added

  Revision 1.15  2000/12/16 15:56:19  jonas
    - removed all ifdef cardinalmulfix code

  Revision 1.14  2000/12/08 14:04:43  jonas
    + added pos(char,ansistring), because there is also a pos(char,shortstring)
      and without the ansistring version, the shortstring version is always
      called when calling pos(char,pchar), even when using $h+ (because the
      first parameter matches exactly) (merged)

  Revision 1.13  2000/12/07 17:19:47  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.12  2000/11/13 13:40:04  marco
   * Renamefest

  Revision 1.11  2000/11/11 16:12:01  peter
    * ptr returns farpointer

  Revision 1.10  2000/11/06 21:35:59  peter
    * removed some warnings

  Revision 1.9  2000/11/06 20:34:24  peter
    * changed ver1_0 defines to temporary defs

  Revision 1.8  2000/10/23 16:15:40  jonas
    * renamed strlenint to longint since 1.0 doesn't know that type

  Revision 1.7  2000/10/23 14:00:59  florian
    * setlength export declaration for 1.0.2 fixed

  Revision 1.6  2000/10/21 18:20:17  florian
    * a lot of small changes:
       - setlength is internal
       - win32 graph unit extended
       ....

  Revision 1.5  2000/08/13 17:55:15  michael
  + Added some delphi compatibility types

  Revision 1.4  2000/08/08 22:11:45  sg
  * Added declarations for WideString utility functions

  Revision 1.3  2000/07/14 10:33:10  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:45  michael
  + removed logs
}
