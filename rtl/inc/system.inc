{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    For details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                                Local types
****************************************************************************}

{
  TextRec and FileRec are put in a separate file to make it available to other
  units without putting it explicitly in systemh.
  This way we keep TP compatibility, and the TextRec definition is available
  for everyone who needs it.
}
{$i filerec.inc}
{$i textrec.inc}

type
  FileFunc = Procedure(var t : TextRec);

var
{ For Error Handling.}
  DoError   : Boolean;
  ErrorBase : Longint;

{****************************************************************************
                    Include processor specific routines
****************************************************************************}

{$IFDEF I386}
  {$IFDEF M68K}
    {$Error Can't determine processor type !}
  {$ENDIF}
  {$I i386.inc}  { Case dependent, don't change }
{$ELSE}
  {$IFDEF M68K}
    {$I m68k.inc}  { Case dependent, don't change }
  {$ELSE}
    {$Error Can't determine processor type !}
  {$ENDIF}
{$ENDIF}

{****************************************************************************
                     Routines which have compiler magic
****************************************************************************}

{$I innr.inc}

Function  lo(i : Integer) : byte;  [INTERNPROC: In_lo_Word];
Function  lo(w : Word) : byte;     [INTERNPROC: In_lo_Word];
Function  lo(l : Longint) : Word;  [INTERNPROC: In_lo_long];
Function  hi(i : Integer) : byte;  [INTERNPROC: In_hi_Word];
Function  hi(w : Word) : byte;     [INTERNPROC: In_hi_Word];
Function  hi(l : Longint) : Word;  [INTERNPROC: In_hi_long];
Procedure Inc(var i : Cardinal);   [INTERNPROC: In_Inc_DWord];
Procedure Inc(var i : Longint);    [INTERNPROC: In_Inc_DWord];
Procedure Inc(var i : Integer);    [INTERNPROC: In_Inc_Word];
Procedure Inc(var i : Word);       [INTERNPROC: In_Inc_Word];
Procedure Inc(var i : shortint);   [INTERNPROC: In_Inc_byte];
Procedure Inc(var i : byte);       [INTERNPROC: In_Inc_byte];
Procedure Inc(var c : Char);       [INTERNPROC: In_Inc_byte];
Procedure Inc(var p : PChar);      [INTERNPROC: In_Inc_DWord];
Procedure Dec(var i : Cardinal);   [INTERNPROC: In_Dec_DWord];
Procedure Dec(var i : Longint);    [INTERNPROC: In_Dec_DWord];
Procedure Dec(var i : Integer);    [INTERNPROC: In_Dec_Word];
Procedure Dec(var i : Word);       [INTERNPROC: In_Dec_Word];
Procedure Dec(var i : shortint);   [INTERNPROC: In_Dec_byte];
Procedure Dec(var i : byte);       [INTERNPROC: In_Dec_byte];
Procedure Dec(var c : Char);       [INTERNPROC: In_Dec_byte];
Procedure Dec(var p : PChar);      [INTERNPROC: In_Dec_DWord];

{$IFNDEF ORDINTERN}
  Function ord(c : Char) : byte;    [INTERNPROC: In_ord_Char];
{$ENDIF ORDINTERN}
Function chr(b : byte) : Char;      [INTERNPROC: In_chr_byte];
Function Length(s : string) : byte; [INTERNPROC: In_Length_string];

{$IFDEF VER_ABOVE0_9_5}
  Procedure Reset(var f : TypedFile);   [INTERNPROC: In_Reset_TypedFile];
  Procedure Rewrite(var f : TypedFile); [INTERNPROC: In_Rewrite_TypedFile];
{$ENDIF}

{****************************************************************************
                               Math Routines
****************************************************************************}

function Hi(b : byte): byte;

 begin
   Hi := b shr 4
 end;

function Lo(b : byte): byte;

 begin
   Lo := b and $0f
 end;

Procedure Inc(var i : Cardinal;a: Longint);
Begin
  I:=I+A;
End;

Procedure Dec(var i : Cardinal;a: Longint);
Begin
  I:=I-A;
End;

Procedure Inc(var i : Longint;a : Longint);
Begin
  i:=i+a;
End;

Procedure Dec(var i : Longint;a : Longint);
Begin
  i:=i-a;
End;

Procedure Dec(var i : Word;a : Longint);
Begin
  i:=i-a;
End;

Procedure Inc(var i : Word;a : Longint);
Begin
  i:=i+a;
End;

Procedure Dec(var i : Integer;a : Longint);
Begin
  i:=i-a;
End;

Procedure Inc(var i : Integer;a : Longint);
Begin
  i:=i+a;
End;

Procedure Dec(var i : byte;a : Longint);
Begin
  i:=i-a;
End;

Procedure Inc(var i : byte;a : Longint);
Begin
  i:=i+a;
End;

Procedure Dec(var i : shortint;a : Longint);
Begin
  i:=i-a;
End;

Procedure Inc(var i : shortint;a : Longint);
Begin
  i:=i+a;
End;

Procedure Dec(var c : Char;a : Longint);
Begin
  byte(c):=byte(c)-a;
End;

Procedure Inc(var c : Char;a : Longint);
Begin
  Byte(c):=byte(c)+a;
End;

Procedure Dec(var p : PChar;a : Longint);
Begin
  longint(p):=longint(p)-a;
End;

Procedure Inc(var p : PChar;a : Longint);
Begin
  longint(p):=longint(p)+a;
End;

Function swap (X : Word) : Word;
Begin
  swap:=(X and $ff) shl 8 + (X shr 8)
End;

Function Swap (X : Integer) : Integer;
Begin
  Swap:=Integer(Swap(Word(X)));
End;

Function swap (X : Longint) : Longint;
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

Function Swap (X : Cardinal) : Cardinal;
Begin
  Swap:=Swap(Longint(X));
End;


{$R-}

Function Random : real;
{
  I am not sure about the accuracy of such a value (PM)
}
Begin
  Random:=abs(Randseed);
  Random:=Random/(maxLongint+1.0);
  Randseed:=Randseed*134775813+1;
  Random:=(abs(Randseed)+Random)/(maxLongint+2.0);
End;

{ Include processor specific routines }
{$I math.inc}

{****************************************************************************
                                Set Handling
****************************************************************************}

{ Include set support which is processor specific}
{$I set.inc}

{****************************************************************************
                             Memory Management
****************************************************************************}

Function Ptr(sel,off : Longint) : pointer;
Begin
  sel:=0;
{$IFDEF DoMapping}
  {$IFDEF DoS}
    ptr:=pointer($e0000000+sel shl 4+off);
  {$ELSE}
    ptr:=pointer(sel shl 4+off);
  {$ENDIF}
{$ELSE}
   ptr:=pointer(off);
{$ENDIF}
End;

Function Addr (Var X) : Pointer;
Begin
  Addr:=@(X);
End;


Function CSeg : Word;
Begin
  Cseg:=0;
End;

Function DSeg : Word;
Begin
  Dseg:=0;
End;

Function SSeg : Word;
Begin
  Sseg:=0;
End;

{****************************************************************************
                Subroutines for short strings are in sstrings.inc
****************************************************************************}

{$i sstrings.inc}

{*****************************************************************************
                             Miscellaneous
*****************************************************************************}


Function IOResult:Word;
Begin
  IOResult:=InOutRes;
  InOutRes:=0;
End;



procedure fillchar(var x;count : longint;value : char);

  begin
     fillchar(x,count,byte(value));
  end;



{*****************************************************************************
                          Init / Exit / ExitProc
*****************************************************************************}

Procedure RunError;
Begin
  RunError (0);
End;


Procedure Halt;
Begin
  Halt(0);
End;


Procedure Initexception;[Public,Alias: 'INITEXCEPTION'];
Begin
  Writeln('Exception occurred during program initialization.');
  halt(216);
End;


Procedure dump_stack(bp : Longint);

  Procedure dump_frame(addr : Longint);
  Begin
  {To be used by symify}
    Writeln(stderr,'  0x',HexStr(addr,8));
    Flush(stderr);
  End;

var
  i, prevbp : Longint;
Begin
  prevbp:=bp-1;
  i:=0;
  while bp > prevbp Do
   Begin
     dump_frame(get_addr(bp));
     Inc(i);
     If i>max_frame_dump Then
      exit;
     prevbp:=bp;
     bp:=get_next_frame(bp);
   End;
End;


Procedure Do_exit;[Public,Alias: '__EXIT'];
{
  Don't call this direct, the call is generated by the compiler
}
var
  current_exit : Procedure;
Begin
  while exitProc<>nil Do
   Begin
     InOutRes:=0;
     current_exit:=tProcedure(exitProc);
     exitProc:=nil;
     current_exit();
   End;
  If DoError Then
   Begin
     Writeln('Run time error  ',Errorcode,' at 0x',hexstr(Longint(Erroraddr),8));
     dump_stack(ErrorBase);
   End;
{ this is wrong at least for dos !!!
  in dos input output and stderr must be left open !! }
{$ifndef DOS}
{$ifndef GO32V2}
  Close(Output);
  Close(StdErr);
{$endif GO32V2}
{$endif DOS}
End;


Type
  PExitProcInfo = ^TExitProcInfo;
  TExitProcInfo = Record
    Next     : PExitProcInfo;
    SaveExit : Pointer;
    Proc     : TProcedure;
  End;
const
  ExitProcList: PExitProcInfo = nil;

Procedure DoExitProc;
var
  P    : PExitProcInfo;
  Proc : TProcedure;
Begin
  P:=ExitProcList;
  ExitProcList:=P^.Next;
  ExitProc:=P^.SaveExit;
  Proc:=P^.Proc;
  DisPose(P);
  Proc();
End;


Procedure AddExitProc(Proc: TProcedure);
var
  P : PExitProcInfo;
Begin
  New(P);
  P^.Next:=ExitProcList;
  P^.SaveExit:=ExitProc;
  P^.Proc:=Proc;
  ExitProcList:=P;
  ExitProc:=@DoExitProc;
End;

{
  $Log$
  Revision 1.4  1998-04-16 12:30:47  peter
    + inc(pchar), dec(pchar), incc(pchar,a),dec(pchar,a)

  Revision 1.3  1998/04/08 07:53:32  michael
  + Changed Random() function. Moved from system to processor dependent files (from Pedro Gimeno)

  Revision 1.2  1998/03/25 23:39:17  florian
    * complete Object Pascal support moved to objpas unit

  Revision 1.1.1.1  1998/03/25 11:18:43  root
  * Restored version

  Revision 1.29  1998/02/07 05:31:22  carl
    * bugfix of Swap with cardinal (no return value was set)

  Revision 1.28  1998/01/30 17:19:18  pierre
    * output and stderr must not be closed at exit under dos

  Revision 1.27  1998/01/26 11:59:53  michael
  + Added log at the end


  
  Working file: rtl/inc/system.inc
  description:
  ----------------------------
  revision 1.26
  date: 1998/01/23 22:21:49;  author: michael;  state: Exp;  lines: +18 -6
  + Set up things for Delphi Object model under DELPHI_EXTENSIONS
  ----------------------------
  revision 1.25
  date: 1998/01/20 09:14:35;  author: michael;  state: Exp;  lines: +13 -1
  + implemented Hi and Lo for bytes. SHould be made an internalproc, though.
  ----------------------------
  revision 1.24
  date: 1998/01/19 10:21:32;  author: michael;  state: Exp;  lines: +10 -1
  * moved Fillchar t(..,char) to system.inc
  ----------------------------
  revision 1.23
  date: 1998/01/16 17:58:51;  author: florian;  state: Exp;  lines: +8 -2
    * basic tobject added (create, destroy and free)
  ----------------------------
  revision 1.22
  date: 1998/01/11 02:45:43;  author: michael;  state: Exp;  lines: +322 -1380
  + Moved file operations to
    - file.inc : Untyped file handling
    - text.inc text file handling
    - typefile.inc typed file handling
    - version stuff to version.inc
     By Peter Vreman.
  ----------------------------
  revision 1.21
  date: 1998/01/06 00:29:33;  author: michael;  state: Exp;  lines: +45 -81
  Implemented a system independent sequence of reset/rewrite/append fileopenfunc etc system \n (from Peter Vreman)
  ----------------------------
  revision 1.20
  date: 1998/01/05 12:32:14;  author: michael;  state: Exp;  lines: +143 -748
  * Undid changes by Carl, it rendered the system unit unusable !
  ----------------------------
  revision 1.19
  date: 1998/01/05 00:47:10;  author: carl;  state: Exp;  lines: +748 -143
  + Compatible with m68k floating point types
  + Test68000/Test68882 variables added when in m68k mode
  ----------------------------
  revision 1.18
  date: 1998/01/03 00:44:19;  author: michael;  state: Exp;  lines: +6 -25
  * Shorter WRITE_TEXT_BOOLEAN (From Peter Vreman)
  - Removed unused vars in READ_TEXT_CHAR (From Peter Vreman)
  - Removed 'far' from DoExitProc (From Peter Vreman)
  ----------------------------
  revision 1.17
  date: 1998/01/01 16:56:08;  author: michael;  state: Exp;  lines: +60 -1
  + Implemented Addr() function.
  + Implemented Inc/Dec for cardinal
  + Implemented Inc/Dec (C : Char; A : longint);
  ----------------------------
  revision 1.16
  date: 1997/12/24 14:08:38;  author: michael;  state: Exp;  lines: +7 -2
  + Added SSEG function, (From Peter Vreman)
  * fixed bug in append, default filehandle should be 1, not 0 (From Peter
    Vreman)
  ----------------------------
  revision 1.15
  date: 1997/12/23 16:32:21;  author: michael;  state: Exp;  lines: +24 -26
  * More efficient treating of append, reset and rewrite (From Peter Vreman)
  ----------------------------
  revision 1.14
  date: 1997/12/22 18:53:18;  author: michael;  state: Exp;  lines: +6 -752
  + All 255-length string handling routines have been moved to sstrings.inc.
  ----------------------------
  revision 1.13
  date: 1997/12/22 15:35:30;  author: michael;  state: Exp;  lines: +10 -9
  * Fixed bug introduced by previous commit.
  ----------------------------
  revision 1.12
  date: 1997/12/22 11:11:54;  author: michael;  state: Exp;  lines: +138 -86
  * Implemented better (faster) string handling routines from Peter Vreman.
  ----------------------------
  revision 1.11
  date: 1997/12/22 10:44:26;  author: pierre;  state: Exp;  lines: +5 -5
    * tipping error in READ_TEXT_INTEGER and so on
  ----------------------------
  revision 1.10
  date: 1997/12/19 11:37:55;  author: pierre;  state: Exp;  lines: +53 -1
    * added read_text for integer word byte and shortint
  ----------------------------
  revision 1.9
  date: 1997/12/11 11:45:32;  author: michael;  state: Exp;  lines: +45 -45
  + undid changes to pos/delete/copy/insert. Version 1.8 was UNUSABLE.
  ----------------------------
  revision 1.8
  date: 1997/12/10 12:13:20;  author: michael;  state: Exp;  lines: +89 -89
  * changed DateiFunc to FileFunc
  ----------------------------
  revision 1.7
  date: 1997/12/02 16:08:53;  author: pierre;  state: Exp;  lines: +7 -1
     * bug fix in val(string,longint,word) for '-2147483648'
  ----------------------------
  revision 1.6
  date: 1997/12/01 12:08:05;  author: michael;  state: Exp;  lines: +10 -4
  + added copyright reference header.
  ----------------------------
  revision 1.5
  date: 1997/11/28 19:45:22;  author: pierre;  state: Exp;  lines: +14 -12
    * one more bug fix with namelength
    + fixed math in fixed_math define (does not compile yet)
  ----------------------------
  revision 1.4
  date: 1997/11/28 12:21:51;  author: michael;  state: Exp;  lines: +2 -2
  Removed the WRITE_TEXT_CARDINAL for version 0.9.1 and less.
  ----------------------------
  revision 1.3
  date: 1997/11/27 22:49:05;  author: florian;  state: Exp;  lines: +5 -0
  - CPU.PP added
  - some bugs in DOS fixed (espsecially for go32v1)
  - the win32 system unit is now compilable
  ----------------------------
  revision 1.2
  date: 1997/11/27 16:29:37;  author: michael;  state: Exp;  lines: +3 -3
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
