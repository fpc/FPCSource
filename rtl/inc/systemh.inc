{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
   This File contains the OS independent declarations of the system unit
*****************************************************************************}

{****************************************************************************
                   Support for multiple compiler versions
****************************************************************************}

{$i version.inc}

{TypedFile is standard from Version 0_9_3}
{$IFDEF VER_ABOVE0_9_3}
  {$DEFINE TypedFile}
{$ENDIF VER_ABOVE0_9_3}

{Ord() is Intern from Version 0_9_5}
{$IFDEF VER_ABOVE0_9_5}
  {$DEFINE OrdIntern}
  {$DEFINE TypedReset}
{$ENDIF VER_ABOVE0_9_5}

{Str() is Intern from Version 0_9_7}
{$IFDEF VER_ABOVE0_9_7}
  {$DEFINE str_intern}
{$ENDIF VER_ABOVE0_9_7}

{ ------------------------- FLOATING POINT DEFINES ----------------------- }
{ possible conditionals here:                                              }
{   comp_support = comp type supported.                                    }
{   ieee_support = ieee support otherwise only real support possible.      }
{$ifdef i386}
{$define comp_support}
{$define ieee_support}
{$endif}


{****************************************************************************
                         Global Types and Constants
****************************************************************************}

Type
{
  Longint is in TP declared as -2147483648..2147483647,
  But it can't be translated to Free Pascal, since the compiler
  first tries to convert 2147483648 to an integer constant
  and later ignores the number.
  But val can't use that...
  So, In 2 complement notation:
}
  Longint  = $80000000..$7fffffff;
  Integer  = -32768..32767;
  shortint = -128..127;
  byte     = 0..255;
  Word     = 0..65535;
{ at least declare Turbo Pascal real types:}
{$IFDEF i386}
  Double   = real;
{$ENDIF}
{$IFNDEF VER_ABOVE0_9_2}
  Extended = real;
  single   = real;
  comp     = real;
{$ELSE}
  {$DEFINE VER_HAS_EXTENDED}
{$ENDIF VER_ABOVE0_9_2}
{$IFDEF VER0_6}
  fixed = Longint;
{$ENDIF}

{ some type aliases }
{$IFDEF VER_ABOVE0_9_2}
  dword    = cardinal;
  longword = cardinal;
{$ENDIF VER_ABOVE0_9_2}

{ Zero - terminated strings }
  PChar  = ^Char;
  PPChar = ^PChar;

{ procedure type }
  TProcedure = Procedure;

const
{ max. values for longint and int}
  maxLongint = $7fffffff;
  maxint = 32767;
{ Compatibility With  TP }
{$ifdef i386}
  Test8086:byte = 2;       { Always i386 or newer }
  Test8087:byte = 3;       { Always 387 or newer. Emulated if needed. }
{$endif i386}
{$ifdef m68k}
  Test68000 : byte = 0; { Must be determined at startup for both }
  Test68881 : byte = 0;
{$endif}

{ max level in dumping on error }
  Max_Frame_Dump:Word = 20;
{ Exit Procedure handling consts and types  }
  Erroraddr:pointer = nil;
  Errorcode:Word    = 0;

{ file input modes }
  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;
  fmAppend = $D7B4;
  Filemode:byte = 2;

var
{ Standard In- and Output }
  Output,
  Input,
  StdErr      : Text;
  ExitProc    : pointer;
  ExitCode    : Word;
{$IFDEF Win32}
  InOutRes    : Longint;
{$ELSE Win32}
  InOutRes    : Word;
{$ENDIF Win32}
  StackBottom,
  LowestStack,
  RandSeed    : Longint;

{****************************************************************************
                        Processor specific routines
****************************************************************************}

Procedure Move(Var source,dest;count:Longint);
Procedure FillChar(Var x;count:Longint;Value:Char);
Procedure FillChar(Var x;count:Longint;Value:byte);
Procedure FillWord(Var x;count:Longint;Value:Word);

{****************************************************************************
                               Math Routines
****************************************************************************}

Function  lo(w:Word):byte;
Function  lo(l:Longint):Word;
Function  lo(i:Integer):byte;
Function  lo(B: Byte):Byte;
Function  hi(w:Word):byte;
Function  hi(i:Integer):byte;
Function  hi(l:Longint):Word;
Function  Hi(B : Byte): byte;

Procedure Inc(Var i:cardinal);
Procedure Inc(Var i:Longint);
Procedure Inc(Var i:Integer);
Procedure Inc(Var i:Word);
Procedure Inc(Var i:shortint);
Procedure Inc(Var i:byte);
Procedure Inc(Var c:Char);
Procedure Dec(Var i:cardinal);
Procedure Dec(Var i:Longint);
Procedure Dec(Var i:Integer);
Procedure Dec(Var i:Word);
Procedure Dec(Var i:shortint);
Procedure Dec(Var i:byte);
Procedure Dec(Var c:Char);
{$IFNDEF ORDINTERN}
  Function Ord(c:Char):byte;
  Function Ord(b:Boolean):byte;
{$ENDIF ORDINTERN}
Function Chr(b:byte):Char;
Function Length(s:string):byte;

Procedure Dec(Var i:cardinal;a:Longint);
Procedure Inc(Var i:cardinal;a:Longint);
Procedure Dec(Var i:Longint;a:Longint);
Procedure Inc(Var i:Longint;a:Longint);
Procedure Dec(Var i:Word;a:Longint);
Procedure Inc(Var i:Word;a:Longint);
Procedure Dec(Var i:Integer;a:Longint);
Procedure Inc(Var i:Integer;a:Longint);
Procedure Dec(Var i:byte;a:Longint);
Procedure Inc(Var i:byte;a:Longint);
Procedure Dec(Var i:shortint;a:Longint);
Procedure Inc(Var i:shortint;a:Longint);
Procedure Dec(Var c:Char;a:Longint);
Procedure Inc(Var c:Char;a:Longint);
Function  Swap (X:Word):Word;
Function  Swap (X:Integer):Integer;
Function  Swap (X:Cardinal):Cardinal;
Function  Swap (X:Longint):Longint;

Function  Random(l:Longint):Longint;
Function  Random:real;
Procedure Randomize;

Function abs(l:Longint):Longint;
Function sqr(l:Longint):Longint;
Function odd(l:Longint):Boolean;
{ float mathe routines }
{$I mathh.inc}


{****************************************************************************
                             Memory management
****************************************************************************}

Procedure getmem(Var p:pointer;Size:Longint);
Procedure freemem(Var p:pointer;Size:Longint);
Function  memavail:Longint;
Function  maxavail:Longint;
Function  ptr(sel,off:Longint):pointer;
Function  Addr (Var X):pointer;
Function  Cseg:Word;
Function  Dseg:Word;
Function  Sseg:Word;

{****************************************************************************
                              String Handling
****************************************************************************}

Function  copy(const s:string;index:Integer;count:Integer):string;
Procedure Delete(Var s:string;index:Integer;count:Integer);
Procedure Insert(const source:string;Var s:string;index:Integer);
Function  Pos(const substr:string;const s:string):byte;
Function  Pos(C:Char;const s:string):byte;
Function  upCase(c:Char):Char;
Function  upCase(const s:string):string;
Function  lowerCase(c:Char):Char;
Function  lowerCase(const s:string):string;
Function  Space(b:byte):string;
Function  hexStr(Val:Longint;cnt:byte):string;
Function  binStr(Val:Longint;cnt:byte):string;
{$IFNDEF STR_INTERN}
  Procedure Str(l:Longint;Var s:string);
{$ifdef support_ieee}
   Procedure Str(d:Double;Var s : string);
{$endif support_ieee}
{$ifndef support_ieee}
   Procedure Str(d : real;Var s : string);
{$endif support_ieee}
  Procedure Str(i:Integer;Var s:string);
  Procedure Str(si:shortint;Var s:string);
  Procedure Str(b:byte;Var s:string);
  Procedure Str(w:Word;Var s:string);
{$ENDIF STR_INTERN}
Procedure Val(const s:string;Var v:Longint;Var code:Word);
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
Procedure Val(const s:string;Var d:Real;Var code:Word);
Procedure Val(const s:string;Var d:Real;Var code:Integer);
Procedure Val(const s:string;Var d:Real);
{$IFDEF VER_ABOVE0_9_2}
 {$IFDEF ieee_support}
   Procedure Val(const s:string;Var d:single;Var code:Word);
   Procedure Val(const s:string;Var d:single;Var code:Integer);
   Procedure Val(const s:string;Var d:single);
 {$ENDIF ieee_support}
{$ENDIF VER_ABOVE0_9_2}
{$IFDEF VER_ABOVE0_9_7}
  {$IFDEF ieee_support}
   Procedure Val(const s:string;Var d:Extended;Var code:Word);
   Procedure Val(const s:string;Var d:Extended;Var code:Integer);
   Procedure Val(const s:string;Var d:Extended);
  {$ENDIF ieee_support}
  {$IFDEF comp_support}
   Procedure Val(const s:string;Var d:comp;Var code:Word);
   Procedure Val(const s:string;Var d:comp;Var code:Integer);
   Procedure Val(const s:string;Var d:comp);
  {$ENDIF comp_support}
{$ENDIF VER_ABOVE0_9_7}
{$IFDEF VER_ABOVE0_9_8}
  Procedure Val(const s:string;Var v:cardinal;Var code:Word);
  Procedure Val(const s:string;Var v:cardinal;Var code:Integer);
  Procedure Val(const s:string;Var v:cardinal);
{$ENDIF VER_ABOVE0_9_8}

{****************************************************************************
                          Untyped File Management
****************************************************************************}

Procedure Assign(Var f:File;const Name:string);
Procedure Rewrite(Var f:File;l:Word);
Procedure Rewrite(Var f:File);
Procedure Reset(Var f:File;l:Word);
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
Procedure Truncate (Var F:File);

{****************************************************************************
                           Typed File Management
****************************************************************************}

{$IFDEF TypedFile}
  Procedure Assign(Var f:TypedFile;const Name:string);
  Procedure Rewrite(Var f:TypedFile);
  Procedure Reset(Var f:TypedFile);
{$ENDIF TypedFile}

{****************************************************************************
                            Text File Management
****************************************************************************}

Procedure Assign(Var t:Text;const s:string);
Procedure Close(Var t:Text);
Procedure Rewrite(Var t:Text);
Procedure Reset(Var t:Text);
Procedure Append(Var t:Text);
Procedure Flush(Var t:Text);
Procedure Erase(Var t:Text);
Procedure Rename(Var t:Text;const s:string);
Function  EOF(Var t:Text):Boolean;
Function  EOF:Boolean;
Function  EOLn(Var t:Text):Boolean;
Function  EOLn:Boolean;
Function  SeekEOLn (Var F:Text):Boolean;
Function  SeekEOF (Var F:Text):Boolean;
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

Function IOResult:Word;
Function Sptr:Longint;

{****************************************************************************
                The whole Delphi stuff is in the unit objpas
*****************************************************************************}

{*****************************************************************************
                          Init / Exit / ExitProc
*****************************************************************************}

Function  Paramcount:Longint;
Function  ParamStr(l:Longint):string;
Procedure Dump_Stack(bp:Longint);
Procedure RunError(w:Word);
Procedure RunError;
Procedure halt(errnum:byte);
Procedure halt;
Procedure AddExitProc(Proc:TProcedure);


{
  $Log$
  Revision 1.3  1998-03-26 14:41:22  michael
  + Added comp support for val and read(ln)

  Revision 1.2  1998/03/25 23:39:17  florian
    * complete Object Pascal support moved to objpas unit

  Revision 1.1.1.1  1998/03/25 11:18:43  root
  * Restored version

  Revision 1.22  1998/03/24 15:50:25  peter
    + dword,longword=cardinal (for > 0.9.2 unlike Florian did with longword)

  Revision 1.21  1998/03/05 02:42:30  peter
    + blockread/blockwrite with integer result

  Revision 1.20  1998/03/04 14:22:07  florian
    + longword is synonyme for cardinal

  Revision 1.19  1998/01/26 11:59:57  michael
  + Added log at the end



  Working file: rtl/inc/systemh.inc
  description:
  ----------------------------
  revision 1.18
  date: 1998/01/23 22:21:48;  author: michael;  state: Exp;  lines: +12 -1
  + Set up things for Delphi Object model under DELPHI_EXTENSIONS
  ----------------------------
  revision 1.17
  date: 1998/01/23 15:30:45;  author: michael;  state: Exp;  lines: +2 -2
  * Daniel forgot to adapt systemh to changes he made in sstrings.inc.
  ----------------------------
  revision 1.16
  date: 1998/01/20 09:14:36;  author: michael;  state: Exp;  lines: +3 -1
  + implemented Hi and Lo for bytes. SHould be made an internalproc, though.
  ----------------------------
  revision 1.15
  date: 1998/01/16 17:58:51;  author: florian;  state: Exp;  lines: +3 -2
    * basic tobject added (create, destroy and free)
  ----------------------------
  revision 1.14
  date: 1998/01/12 02:32:58;  author: carl;  state: Exp;  lines: +40 -12
    + now works with sysatari
  ----------------------------
  revision 1.13
  date: 1998/01/11 02:45:43;  author: michael;  state: Exp;  lines: +341 -459
  + Moved file operations to
    - file.inc : Untyped file handling
    - text.inc text file handling
    - typefile.inc typed file handling
    - version stuff to version.inc
     By Peter Vreman.
  ----------------------------
  revision 1.12
  date: 1998/01/05 12:37:11;  author: michael;  state: Exp;  lines: +19 -30
  + undid changes by Carl, they rendered the system unit useless.
  ----------------------------
  revision 1.11
  date: 1998/01/05 00:47:55;  author: carl;  state: Exp;  lines: +30 -19
  + Now compatible with m68k floating point types
  ----------------------------
  revision 1.10
  date: 1998/01/01 16:56:09;  author: michael;  state: Exp;  lines: +8 -1
  + Implemented Addr() function.
  + Implemented Inc/Dec for cardinal
  + Implemented Inc/Dec (C : Char; A : longint);
  ----------------------------
  revision 1.9
  date: 1997/12/24 14:25:16;  author: michael;  state: Exp;  lines: +2 -1
  + Added SSEG function (From Peter Vreman)
  ----------------------------
  revision 1.8
  date: 1997/12/23 16:33:21;  author: michael;  state: Exp;  lines: +6 -3
  + Added dec() and inc() for char (from Peter Vreman)
  ----------------------------
  revision 1.7
  date: 1997/12/22 15:34:37;  author: michael;  state: Exp;  lines: +3 -2
   + added faster pos (const char; ..) seeking.
  ----------------------------
  revision 1.6
  date: 1997/12/13 19:02:14;  author: florian;  state: Exp;  lines: +26 -2
  + defines for version 0.99.0
  ----------------------------
  revision 1.5
  date: 1997/12/11 13:52:21;  author: florian;  state: Exp;  lines: +11 -1
  *** empty log message ***
  ----------------------------
  revision 1.4
  date: 1997/12/11 11:49:48;  author: michael;  state: Exp;  lines: +2 -1
  + added comp type for versions below 0.9.1.
  ----------------------------
  revision 1.3
  date: 1997/12/01 12:08:05;  author: michael;  state: Exp;  lines: +11 -4
  + added copyright reference header.
  ----------------------------
  revision 1.2
  date: 1997/11/27 16:29:38;  author: michael;  state: Exp;  lines: +3 -1
  Change submitted by Pierre Muller:
  Added check : version must be above 0.9.7 for extended type
  handling functions.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
