{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    This File contains the OS independent declarations of the system unit

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
   Supported conditionnals:
   ------------------------
   RTLLITE         Create a somewhat smaller RTL
}

{****************************************************************************
                             Needed switches
****************************************************************************}

{$I-,Q-,H-,R-,V-}
{$mode objfpc}

{ needed for insert,delete,readln }
{$P+}

{ Stack check gives a note under linux }
{$ifndef linux}
  {$S-}
{$endif}

{****************************************************************************
                         Global Types and Constants
****************************************************************************}

Type
  Longint  = $80000000..$7fffffff; { $8000000 creates a longint overfow !! }
  Integer  = -32768..32767;
  shortint = -128..127;
  byte     = 0..255;
  Word     = 0..65535;

{ at least declare Turbo Pascal real types }
{$ifdef i386}
  StrLenInt = LongInt;

  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  { define SUPPORT_FIXED}

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Extended;
{$endif}

{$ifdef m68k}
  StrLenInt = Longint;

  ValSInt = Longint;
  ValUInt = Cardinal;
  ValReal = Real;

  {$define SUPPORT_SINGLE}
{$endif}

{ some type aliases }
  dword       = cardinal;
  longword    = cardinal;

{ Zero - terminated strings }
  PChar       = ^Char;
  PPChar      = ^PChar;

{ procedure type }
  TProcedure  = Procedure;

const
{ Maximum value of the biggest signed and unsigned integer type available}
  MaxSIntValue = High(ValSInt);
  MaxUIntValue = High(ValUInt);

{ max. values for longint and int}
  maxLongint = $7fffffff;
  maxint     = 32767;

{ Compatibility With  TP }
{$ifdef i386}
  Test8086 : byte = 2;       { Always i386 or newer }
  Test8087 : byte = 3;       { Always 387 or newer. Emulated if needed. }
  FPUInt64 : boolean = true; { set this to false if you don't want that }
                             { the fpu does int64*int64 and             }
                             { int64 div int64, if the * is overflow    }
                             { checked, it is done in software          }
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

var
{ Standard In- and Output }
  Output,
  Input,
  StdOut,
  StdErr      : Text;
  ExitCode,
  InOutRes    : Word;
  StackBottom,
  LowestStack,
  RandSeed    : Cardinal;


{****************************************************************************
                        Processor specific routines
****************************************************************************}

Procedure Move(Var source,dest;count:Longint);
Procedure FillChar(Var x;count:Longint;Value:Boolean);
Procedure FillChar(Var x;count:Longint;Value:Char);
Procedure FillChar(Var x;count:Longint;Value:Byte);
{$ifndef RTLLITE}
Procedure FillWord(Var x;count:Longint;Value:Word);
{$endif RTLLITE}

{****************************************************************************
                          Math Routines
****************************************************************************}

{$ifndef RTLLITE}
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
{$ifdef INT64}
Function  lo(q : QWord) : DWord;
Function  lo(i : Int64) : DWord;
Function  hi(q : QWord) : DWord;
Function  hi(i : Int64) : DWord;
Function  Swap (X:QWord):QWord;
Function  Swap (X:Int64):Int64;
{$endif}
{$endif RTLLITE}

Function  Random(l:Longint):Longint;
Function  Random:real;
Procedure Randomize;

Function abs(l:Longint):Longint;
Function sqr(l:Longint):Longint;
Function odd(l:Longint):Boolean;

{ float math routines }
{$I mathh.inc}

{****************************************************************************
                         Addr/Pointer Handling
****************************************************************************}

{$ifndef RTLLITE}
Function  ptr(sel,off:Longint):pointer;
Function  Cseg:Word;
Function  Dseg:Word;
Function  Sseg:Word;
{$endif RTLLITE}

{****************************************************************************
                      PChar and String Handling
****************************************************************************}

function strpas(p:pchar):shortstring;
function strlen(p:pchar):longint;

{ Shortstring functions }
Function  Copy(const s:shortstring;index:StrLenInt;count:StrLenInt):shortstring;
Procedure Delete(Var s:shortstring;index:StrLenInt;count:StrLenInt);
Procedure Insert(const source:shortstring;Var s:shortstring;index:StrLenInt);
Procedure Insert(source:Char;Var s:shortstring;index:StrLenInt);
Function  Pos(const substr:shortstring;const s:shortstring):StrLenInt;
Function  Pos(C:Char;const s:shortstring):StrLenInt;
Procedure SetLength(var s:shortstring;len:StrLenInt);
Procedure SetString (Var S : Shortstring; Buf : PChar; Len : Longint);
Function  Length(s:string):byte;
Function  upCase(const s:shortstring):shortstring;
{$ifndef RTLLITE}
Function  lowerCase(const s:shortstring):shortstring;
{$endif}
Function  Space(b:byte):shortstring;
{$ifndef RTLLITE}
Function  hexStr(Val:Longint;cnt:byte):shortstring;
Function  binStr(Val:Longint;cnt:byte):shortstring;
{$endif RTLLITE}

{ Char functions }
Function  Chr(b:byte):Char;
Function  upCase(c:Char):Char;
{$ifndef RTLLITE}
Function  lowerCase(c:Char):Char;
{$endif RTLLITE}
function  copy(c:char;index : StrLenInt;count : StrLenInt): shortstring;
function  pos(const substr : shortstring;c:char): StrLenInt;
function  length(c:char):byte;


{****************************************************************************
                             AnsiString Handling
****************************************************************************}

Procedure SetLength (Var S : AnsiString; l : Longint);
Procedure UniqueAnsiString (Var S : AnsiString);
Function  Length (Const S : AnsiString) : Longint;
Function  Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;
Function  Pos (Const Substr : AnsiString; Const Source : AnsiString) : Longint;
Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : Longint);
Procedure Delete (Var S : AnsiString; Index,Size: Longint);
Function StringOfChar(c : char;l : longint) : AnsiString;


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
Procedure BlockWrite(Var f:File;Var Buf;Count:Longint;Var Result:Longint);
Procedure BlockWrite(Var f:File;Var Buf;Count:Word;Var Result:Word);
Procedure BlockWrite(Var f:File;Var Buf;Count:Word;Var Result:Integer);
Procedure BlockWrite(Var f:File;Var Buf;Count:Longint);
Procedure BlockRead(Var f:File;Var Buf;count:Longint;Var Result:Longint);
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
{$ifndef RTLLITE}
Procedure Dump_Stack(var f : text;bp:Longint);
{$endif RTLLITE}
Procedure RunError(w:Word);
Procedure RunError;
Procedure halt(errnum:byte);
{$ifndef RTLLITE}
Procedure AddExitProc(Proc:TProcedure);
{$endif RTLLITE}
Procedure halt;


{*****************************************************************************
                         Abstract/Assert/Error Handling
*****************************************************************************}

procedure AbstractError;
Procedure SysAssert(Const Msg,FName:ShortString;LineNo,ErrorAddr:Longint);

{ Error handlers }
Type
  TErrorProc = Procedure (ErrNo : Longint; Address : Pointer);
  TAbstractErrorProc = Procedure;
  TAssertErrorProc = Procedure(const msg,fname:ShortString;lineno,erroraddr:longint);
const
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

{
  $Log$
  Revision 1.64  1999-10-27 14:19:10  florian
    + StringOfChar

  Revision 1.63  1999/10/26 12:31:00  peter
    * *errorproc are not procvars instead of pointers which allows better
      error checking for the parameters (shortstring<->ansistring)

  Revision 1.62  1999/08/19 11:16:13  peter
    * settextbuf size is now longint

  Revision 1.61  1999/07/05 20:04:28  peter
    * removed temp defines

  Revision 1.60  1999/07/03 01:24:21  peter
    * $ifdef int64

  Revision 1.59  1999/07/02 18:06:43  florian
    + qword/int64: lo/hi/swap

  Revision 1.58  1999/06/30 22:17:22  florian
    + fpuint64 to system unit interface added: if it is true, the rtl
      uses the fpu to do int64 operations, if possible

  Revision 1.57  1999/05/17 21:52:40  florian
    * most of the Object Pascal stuff moved to the system unit

  Revision 1.56  1999/05/06 09:05:14  peter
    * generic write_float str_float

  Revision 1.55  1999/04/17 13:10:26  peter
    * addr() internal

  Revision 1.54  1999/04/08 15:57:56  peter
    + subrange checking for readln()

  Revision 1.53  1999/03/16 17:49:37  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in text.inc: changed RTE 106 when read integer values are out of bounds to RTE 201
    * in systemh.inc: disabled "support_fixed" for the i386 because it gave internal errors,

  Revision 1.52  1999/03/10 22:15:30  florian
    + system.cmdline variable for go32v2 and win32 added

  Revision 1.51  1999/03/03 15:23:58  michael
  + Added setstring for Delphi compatibility

  Revision 1.50  1999/02/01 00:05:16  florian
    + functions lo/hi for DWord type implemented

  Revision 1.49  1999/01/29 09:23:10  pierre
   * Fillchar(..,..,boolean) added

  Revision 1.48  1999/01/22 12:39:24  pierre
   + added text arg for dump_stack

  Revision 1.47  1999/01/11 19:26:53  jonas
    * made inster(string,string,index) a bit faster
    + overloaded insert(char,string,index)

  Revision 1.46  1998/12/28 15:50:48  peter
    + stdout, which is needed when you write something in the system unit
      to the screen. Like the runtime error

  Revision 1.45  1998/12/15 22:43:04  peter
    * removed temp symbols

  Revision 1.44  1998/11/27 14:50:57  peter
    + open strings, $P switch support

  Revision 1.43  1998/11/26 23:16:13  jonas
    * changed RandSeed and OldRandSeed to Cardinal to avoid negative random numbers

  Revision 1.42  1998/11/24 17:12:43  peter
    * fixed length(char)
    - removed obsolete version.inc

  Revision 1.41  1998/11/16 10:21:26  peter
    * fixes for H+

  Revision 1.40  1998/11/05 10:29:37  pierre
   * fix for length(char) in const expressions

  Revision 1.39  1998/11/04 20:34:01  michael
  + Removed ifdef useansistrings

  Revision 1.38  1998/11/04 10:20:51  peter
    * ansistring fixes

  Revision 1.37  1998/10/10 15:28:47  peter
    + read single,fixed
    + val with code:longint
    + val for fixed

  Revision 1.36  1998/10/05 17:22:54  pierre
   * avoid overflow on $8000000 with $Q-

  Revision 1.35  1998/10/05 12:32:52  peter
    + assert() support

  Revision 1.34  1998/10/01 14:54:48  peter
    * export also stackframe functions

  Revision 1.33  1998/09/28 14:02:33  michael
  + AnsiString changes

  Revision 1.32  1998/09/22 15:30:55  peter
    * shortstring=string type added

  Revision 1.31  1998/09/20 17:49:09  florian
    * some ansistring fixes

  Revision 1.30  1998/09/17 16:34:17  peter
    * new eof,eoln,seekeoln,seekeof
    * speed upgrade for read_string
    * inoutres 104/105 updates for read_* and write_*

  Revision 1.29  1998/09/16 13:08:04  michael
  Added AbstractErrorHandler

  Revision 1.28  1998/09/14 10:48:22  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.27  1998/09/08 15:03:28  peter
    * moved getmem/freemem/memavail/maxavail to heaph.inc

  Revision 1.26  1998/09/04 18:16:14  peter
    * uniform filerec/textrec (with recsize:longint and name:0..255)

  Revision 1.25  1998/09/01 17:36:22  peter
    + internconst

  Revision 1.24  1998/08/11 21:39:08  peter
    * splitted default_extended from support_extended

  Revision 1.23  1998/08/11 00:05:27  peter
    * $ifdef ver0_99_5 updates

  Revision 1.22  1998/08/08 12:28:14  florian
    * a lot small fixes to the extended data type work

  Revision 1.21  1998/07/30 13:26:17  michael
  + Added support for ErrorProc variable. All internal functions are required
    to call HandleError instead of runerror from now on.
    This is necessary for exception support.

  Revision 1.20  1998/07/28 20:37:47  michael
  + added setjmp/longjmp and exception support

  Revision 1.19  1998/07/20 23:36:57  michael
  changes for ansistrings

  Revision 1.18  1998/07/18 17:14:24  florian
    * strlenint type implemented

  Revision 1.17  1998/07/10 11:02:39  peter
    * support_fixed, becuase fixed is not 100% yet for the m68k

  Revision 1.16  1998/07/02 12:13:18  carl
    * No SINGLE type for m68k or other non-intel processors!

  Revision 1.15  1998/07/01 14:43:46  carl
    - max_frame_dump reduced to 8, 20 is too much!

  Revision 1.14  1998/06/25 14:04:26  peter
    + internal inc/dec

  Revision 1.13  1998/06/25 09:44:21  daniel
  + RTLLITE directive to compile minimal RTL.

  Revision 1.12  1998/06/15 15:16:27  daniel

  * RTLLITE conditional added to produce smaller RTL

  Revision 1.11  1998/06/08 12:38:23  michael
  Implemented rtti, inserted ansistrings again

  Revision 1.10  1998/06/04 23:46:02  peter
    * comp,extended are only i386 added support_comp,support_extended

  Revision 1.9  1998/06/04 08:26:03  pierre
    * boolean internal definition again (needed to compile
      older RTL's)

  Revision 1.8  1998/06/03 23:39:53  peter
    + boolean=bytebool

  Revision 1.7  1998/05/22 12:34:11  peter
    * fixed the optimizes of daniel

  Revision 1.6  1998/05/21 19:31:00  peter
    * objects compiles for linux
    + assign(pchar), assign(char), rename(pchar), rename(char)
    * fixed read_text_as_array
    + read_text_as_pchar which was not yet in the rtl

  Revision 1.5  1998/05/12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

  Revision 1.4  1998/04/16 12:30:47  peter
    + inc(pchar), dec(pchar), incc(pchar,a),dec(pchar,a)
}
