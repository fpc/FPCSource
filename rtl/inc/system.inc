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

Function chr(b : byte) : Char;      [INTERNPROC: In_chr_byte];
Function Length(s : string) : byte; [INTERNPROC: In_Length_string];

Procedure Reset(var f : TypedFile);   [INTERNPROC: In_Reset_TypedFile];
Procedure Rewrite(var f : TypedFile); [INTERNPROC: In_Rewrite_TypedFile];


{****************************************************************************
                                Set Handling
****************************************************************************}

{ Include set support which is processor specific}
{$I set.inc}

{****************************************************************************
                  Subroutines for String handling
****************************************************************************}

{ Needs to be before RTTI handling }

{$i sstrings.inc}

{$ifdef UseAnsiStrings}

{$i astrings.pp}

{$else}

{ Provide dummy procedures needed for rtti}
Procedure decr_ansi_ref (P : pointer);[Alias : 'DECR_ANSI_REF'];
  begin
  end; 

Procedure incr_ansi_ref (P : pointer);[Alias : 'INCR_ANSI_REF'];
  begin
  end; 

{$endif}


{****************************************************************************
                         Run-Time Type Information (RTTI)
****************************************************************************}


{$i rtti.inc}

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
                             Memory Management
****************************************************************************}

Function Ptr(sel,off : Longint) : pointer;
Begin
  sel:=0;
  ptr:=pointer(off);
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

{ Seems not to be used (PFV)
Procedure Initexception;[Public,Alias: 'INITEXCEPTION'];
Begin
  Writeln('Exception occurred during program initialization.');
  halt(216);
End;
}

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
  Flush(stderr);

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
  Revision 1.9  1998-06-10 07:46:45  michael
  + Forgot to commit some changes

  Revision 1.8  1998/06/08 12:38:24  michael
  Implemented rtti, inserted ansistrings again

  Revision 1.7  1998/06/04 23:46:01  peter
    * comp,extended are only i386 added support_comp,support_extended

  Revision 1.6  1998/05/20 11:23:09  cvs
  * test commit. Shouldn't be allowed.

  Revision 1.5  1998/05/12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

  Revision 1.4  1998/04/16 12:30:47  peter
    + inc(pchar), dec(pchar), incc(pchar,a),dec(pchar,a)

  Revision 1.3  1998/04/08 07:53:32  michael
  + Changed Random() function. Moved from system to processor dependent files (from Pedro Gimeno)
}
