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
  shortint = -128..127;
  SmallInt = -32768..32767;
  Longint  = $80000000..$7fffffff; { $8000000 creates a longint overfow !! }
  byte     = 0..255;
  Word     = 0..65535;
  dword    = cardinal;
  longword = cardinal;

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

{ Zero - terminated strings }
  PChar       = ^Char;
  PPChar      = ^PChar;
{$ifdef HASWIDECHAR}
  PWideChar   = ^WideChar;
{$endif HASWIDECHAR}

{ procedure type }
  TProcedure  = Procedure;

const
{ Maximum value of the biggest signed and unsigned integer type available}
  MaxSIntValue = High(ValSInt);
  MaxUIntValue = High(ValUInt);

{ max. values for longint and int}
  maxLongint  = $7fffffff;
  maxSmallint = 32767;

{ Integer type definition }
type
  Integer  = smallint;
const
  maxint   = maxsmallint;

{ Compatibility With  TP }
const
{$ifdef i386}
  Test8086 : byte = 2;       { Always i386 or newer }
  Test8087 : byte = 3;       { Always 387 or newer. Emulated if needed. }
  { code to use comps in int64mul and div code is commented out! (JM) }
  FPUInt64 : boolean = false; { set this to false if you don't want that }
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
procedure FillByte(var x;count:longint;value:byte);
Procedure FillWord(Var x;count:Longint;Value:Word);
procedure FillDWord(var x;count:longint;value:DWord);
function  IndexChar(var buf;len:longint;b:char):longint;
function  IndexByte(var buf;len:longint;b:byte):longint;
function  Indexword(var buf;len:longint;b:word):longint;
function  IndexDWord(var buf;len:longint;b:DWord):longint;
function  CompareChar(var buf1,buf2;len:longint):longint;
function  CompareByte(var buf1,buf2;len:longint):longint;
function  CompareWord(var buf1,buf2;len:longint):longint;
function  CompareDWord(var buf1,buf2;len:longint):longint;
procedure MoveChar0(var buf1,buf2;len:longint);
function  IndexChar0(var buf;len:longint;b:char):longint;
function  CompareChar0(var buf1,buf2;len:longint):longint;
{$endif}


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

Function  Random(l:cardinal):cardinal;
{$ifndef cardinalmulfixed}
Function  Random(l:longint):longint;
{$endif cardinalmulfixed}
Function  Random: extended;
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
Procedure SetString (Var S : AnsiString; Buf : PChar; Len : Longint);
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
  Revision 1.76  2000-01-21 15:32:07  jonas
    * set FPUInt64 to false for i386, because comp mul and div code for int64 is
      commented out in int64.inc

  Revision 1.75  2000/01/10 09:54:30  peter
    * primitives added

  Revision 1.74  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.73  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.72  1999/12/20 11:20:14  peter
    + smallint, maxsmallint
    * integer is redefined as smallint

  Revision 1.71  1999/12/18 14:55:05  florian
    * very basic widestring support

  Revision 1.70  1999/12/12 13:29:34  jonas
    * remove "random(longint): longint" if cardinalmulfixed is defined

  Revision 1.69  1999/12/01 12:37:13  jonas
    + function random(longint): longint

  Revision 1.68  1999/11/25 13:34:57  michael
  + Added Ansistring setstring call

  Revision 1.67  1999/11/20 12:48:09  jonas
    * reinstated old random generator, but modified it so the integer
      one now has a much longer period

  Revision 1.66  1999/11/09 20:14:12  daniel
  * Committed new random generator.

  Revision 1.65  1999/11/06 14:35:39  peter
    * truncated log

  Revision 1.64  1999/10/27 14:19:10  florian
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

}
