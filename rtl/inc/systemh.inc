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
                   Support for multiple compiler versions
****************************************************************************}

{$i version.inc}


{****************************************************************************
                         Global Types and Constants
****************************************************************************}

Type
  Longint  = $80000000..$7fffffff;
  Integer  = -32768..32767;
  shortint = -128..127;
  byte     = 0..255;
  Word     = 0..65535;

{ at least declare Turbo Pascal real types }
{$ifdef i386}
   StrLenInt = LongInt;
  {$define DEFAULT_EXTENDED}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}
  {$define SUPPORT_SINGLE}
  {$define SUPPORT_FIXED}
  Double = real;
  {$ifdef DEFAULT_EXTENDED}
    ValReal = Extended;
  {$else}
    ValReal = Double;
  {$endif}
{$endif}

{$ifdef m68k}
   StrLenInt = Integer;
   ValReal = Real;
   {$ifdef USEANSISTRINGS}
      {$error StrLenInt must be a longint if ansi strings are used}
   {$endif}
{$endif}

{ some type aliases }
  dword       = cardinal;
  longword    = cardinal;
  ShortString = String[255];

{ Zero - terminated strings }
  PChar       = ^Char;
  PPChar      = ^PChar;

{ procedure type }
  TProcedure  = Procedure;

const
{ max. values for longint and int}
  maxLongint = $7fffffff;
  maxint     = 32767;

{ Compatibility With  TP }
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

Type
  TErrorProc = Procedure (ErrNo : Longint; Address : Pointer);

var
{ Standard In- and Output }
  Output,
  Input,
  StdErr      : Text;
  ExitCode,
  InOutRes    : Word;
  StackBottom,
  LowestStack,
  RandSeed    : Longint;
{ Error handlers }
  ErrorProc         : Pointer;

{****************************************************************************
                        Processor specific routines
****************************************************************************}

Procedure Move(Var source,dest;count:Longint);
Procedure FillChar(Var x;count:Longint;Value:Char);
Procedure FillChar(Var x;count:Longint;Value:byte);
{$ifndef RTLLITE}
Procedure FillWord(Var x;count:Longint;Value:Word);
{$endif RTLLITE}

{****************************************************************************
                          Math Routines
****************************************************************************}

{$ifndef RTLLITE}
Function  lo(w:Word):byte;
Function  lo(l:Longint):Word;
Function  lo(i:Integer):byte;
Function  lo(B: Byte):Byte;
Function  hi(w:Word):byte;
Function  hi(i:Integer):byte;
Function  hi(l:Longint):Word;
Function  Hi(B : Byte): byte;
Function  Swap (X:Word):Word;
Function  Swap (X:Integer):Integer;
Function  Swap (X:Cardinal):Cardinal;
Function  Swap (X:Longint):Longint;
{$endif RTLLITE}

Function Chr(b:byte):Char;
Function Length(s:string):byte;

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
Function  Addr (Var X):pointer;
Function  Cseg:Word;
Function  Dseg:Word;
Function  Sseg:Word;
{$endif RTLLITE}

{****************************************************************************
                      PChar and String Handling
****************************************************************************}

function strpas(p:pchar):string;
function strlen(p:pchar):longint;

Function  Copy(const s:string;index:StrLenInt;count:StrLenInt):string;
Procedure Delete(Var s:string;index:StrLenInt;count:StrLenInt);
Procedure Insert(const source:string;Var s:string;index:StrLenInt);
Function  Pos(const substr:string;const s:string):byte;
Function  Pos(C:Char;const s:string):byte;
Function  upCase(c:Char):Char;
Function  upCase(const s:string):string;
{$ifndef RTLLITE}
Function  lowerCase(c:Char):Char;
Function  lowerCase(const s:string):string;
Function  hexStr(Val:Longint;cnt:byte):string;
Function  binStr(Val:Longint;cnt:byte):string;
{$endif RTLLITE}
Function  Space(b:byte):string;
Procedure Val(const s:string;Var l:Longint;Var code:Word);
Procedure Val(const s:string;Var l:Longint;Var code:Integer);
Procedure Val(const s:string;Var l:Longint);
Procedure Val(const s:string;Var b:byte;Var code:Word);
Procedure Val(const s:string;Var b:byte;Var code:Integer);
Procedure Val(const s:string;Var b:byte);
Procedure Val(const s:string;Var b:shortint;Var code:Word);
Procedure Val(const s:string;Var b:shortint;Var code:Integer);
Procedure Val(const s:string;Var b:shortint);
Procedure Val(const s:string;Var b:Word;Var code:Word);
Procedure Val(const s:string;Var b:Word;Var code:Integer);
Procedure Val(const s:string;Var b:Word);
Procedure Val(const s:string;Var b:Integer;Var code:Word);
Procedure Val(const s:string;Var b:Integer;Var code:Integer);
Procedure Val(const s:string;Var b:Integer);
Procedure Val(const s:string;Var v:cardinal;Var code:Word);
Procedure Val(const s:string;Var v:cardinal;Var code:Integer);
Procedure Val(const s:string;Var v:cardinal);
Procedure Val(const s:string;Var d:ValReal;Var code:Word);
Procedure Val(const s:string;Var d:ValReal;Var code:Integer);
Procedure Val(const s:string;Var d:ValReal);
{$ifdef SUPPORT_SINGLE}
  Procedure Val(const s:string;Var d:single;Var code:Word);
  Procedure Val(const s:string;Var d:single;Var code:Integer);
  Procedure Val(const s:string;Var d:single);
{$endif SUPPORT_SINGLE}
{$ifdef SUPPORT_COMP}
  Procedure Val(const s:string;Var d:comp;Var code:Word);
  Procedure Val(const s:string;Var d:comp;Var code:Integer);
  Procedure Val(const s:string;Var d:comp);
{$endif SUPPORT_COMP}
{$ifdef DEFAULT_EXTENDED}
  Procedure Val(const s:string;Var d:Real;Var code:Word);
  Procedure Val(const s:string;Var d:Real;Var code:Integer);
  Procedure Val(const s:string;Var d:Real);
{$else DEFAULT_EXTENDED}
  {$ifdef SUPPORT_EXTENDED}
    Procedure Val(const s:string;Var d:Extended;Var code:Word);
    Procedure Val(const s:string;Var d:Extended;Var code:Integer);
    Procedure Val(const s:string;Var d:Extended);
  {$endif}
{$endif DEFAULT_EXTENDED}

{****************************************************************************
                             AnsiString Handling
****************************************************************************}

{$ifdef UseAnsiStrings }

Procedure SetLength (Var S : AnsiString; l : Longint);
Procedure UniqueAnsiString (Var S : AnsiString);
Function  Length (Const S : AnsiString) : Longint;
Function  Copy (Const S : AnsiString; Index,Size : Longint) : AnsiString;
Function  Pos (Const Substr : AnsiString; Const Source : AnsiString) : Longint;
Procedure Insert (Const Source : AnsiString; Var S : AnsiString; Index : Longint);
Procedure Delete (Var S : AnsiString; Index,Size: Longint);
Procedure Val (Const S : AnsiString; var R : real; Var Code : Integer);
{
Procedure Val (Const S : AnsiString; var D : Double; Var Code : Integer);
}
Procedure Val (Const S : AnsiString; var E : Extended; Code : Integer);
Procedure Val (Const S : AnsiString; var C : Cardinal; Code : Integer);
Procedure Val (Const S : AnsiString; var L : Longint; Var Code : Integer);
Procedure Val (Const S : AnsiString; var W : Word; Var Code : Integer);
Procedure Val (Const S : AnsiString; var I : Integer; Var Code : Integer);
Procedure Val (Const S : AnsiString; var B : Byte; Var Code : Integer);
Procedure Val (Const S : AnsiString; var SI : ShortInt; Var  Code : Integer);
{
Procedure Str (Const R : Real;Len, fr : longint; Var S : AnsiString);
Procedure Str (Const D : Double;Len,fr : longint; Var S : AnsiString);
Procedure Str (Const E : Extended;Len,fr : longint; Var S : AnsiString);
Procedure Str (Const C : Cardinal;len : Longint; Var S : AnsiString);
Procedure Str (Const L : LongInt;len : longint; Var S : AnsiString);
Procedure Str (Const W : Word;len : longint; Var S : AnsiString);
Procedure Str (Const I : Integer;len : Longint; Var S : AnsiString);
Procedure Str (Const B : Byte; Len : longint; Var S : AnsiString);
Procedure Str (Const SI : ShortInt; Len : longint; Var S : AnsiString);
}
{$endif}


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
Procedure SetTextBuf(Var f:Text; Var Buf; Size:Word);

{****************************************************************************
                            Directory Management
****************************************************************************}

Procedure chdir(const s:string);
Procedure mkdir(const s:string);
Procedure rmdir(const s:string);
Procedure getdir(drivenr:byte;Var dir:string);

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
Procedure Dump_Stack(bp:Longint);
{$endif RTLLITE}
Procedure RunError(w:Word);
Procedure RunError;
Procedure halt(errnum:byte);
{$ifndef RTLLITE}
Procedure AddExitProc(Proc:TProcedure);
{$endif RTLLITE}
Procedure halt;

{*****************************************************************************
                              Abstract/Assert
*****************************************************************************}

procedure AbstractError;
Procedure SysAssert(Const Msg,FName:string;LineNo,ErrorAddr:Longint);

const
  AssertErrorProc   : Pointer=@SysAssert;
  AbstractErrorProc : Pointer=nil;


{*****************************************************************************
                          SetJmp/LongJmp
*****************************************************************************}

{$i setjumph.inc}

{
  $Log$
  Revision 1.35  1998-10-05 12:32:52  peter
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
