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

const
{ Random / Randomize constants }
  OldRandSeed : Longint = 0;
  InitialSeed : Boolean = TRUE;
  Seed1 : Longint = 0;
  Seed2 : Longint = 0;
  Seed3 : Longint = 0;

{ For Error Handling.}
  DoError   : Boolean = FALSE;
  ErrorBase : Longint = 0;

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
{$ifndef INTERN_INC}
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
{$endif INTERN_INC}

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

Type
   PLongint = ^Longint;
   PByte = ^Byte;

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

{$ifndef VER0_99_5}
{$i rtti.inc}
{$endif  VER0_99_5}

{****************************************************************************
                               Math Routines
****************************************************************************}

{$ifndef RTLLITE}

function Hi(b : byte): byte;
begin
   Hi := b shr 4
end;

function Lo(b : byte): byte;
begin
   Lo := b and $0f
end;

{$ifndef INTERN_INC}

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

{$endif INTERN_INC}

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


{$endif RTLLITE}
{****************************************************************************
                              Random function routines

	This implements a very long cycle random number generator by combining
   three independant generators.  The technique was described in the March
   1987 issue of Byte.
   Taken and modified with permission from the PCQ Pascal rtl code.
****************************************************************************}

{$R-}
{$Q-}

{ PLEASE DO NOT OPTIMIZE BECAUSE THEY ACTUALLY WORK CORRECTLY - unless }
{ you want me to go violent :) (CEC)                                   }

Procedure UseSeed(seed : Longint);Forward;


Function Random : Real;
var
    ReturnValue : Real;
begin
    if (InitialSeed) OR ((RandSeed <> OldRandSeed) AND NOT InitialSeed) then
    Begin
       OldRandSeed:=RandSeed;
    { This is a pretty complicated affair                             }
    {  Initially we must call UseSeed when RandSeed is initalized     }
    {  We must also call UseSeed each time RandSeed is reinitialized  }
    {  DO NOT CHANGE THE ORDER OF DECLARATIONS IN THIS BLOCK          }
    {  UNLESS YOU WANT RANDON TO CRASH OF COURSE (CEC)                }
      InitialSeed:=FALSE;
      UseSeed(Randseed);
    end;
    Inc(Seed1);
    Seed1 := (Seed1 * 706) mod 500009;
    INC(Seed2);
    Seed2 := (Seed2 * 774) MOD 600011;
    INC(Seed3);
    Seed3 := (Seed3 * 871) MOD 765241;
    ReturnValue := Seed1/500009.0 +
            Seed2/600011.0 +
           Seed3/765241.0;
    Random := frac(ReturnValue);
end;


Function Random(l : Longint) : Longint;
begin
    if (InitialSeed) OR ((RandSeed <> OldRandSeed) AND NOT InitialSeed) then
    Begin
       OldRandSeed:=RandSeed;
    { This is a pretty complicated affair                             }
    {  Initially we must call UseSeed when RandSeed is initalized     }
    {  We must also call UseSeed each time RandSeed is reinitialized  }
    {  DO NOT CHANGE THE ORDER OF DECLARATIONS IN THIS BLOCK          }
    {  UNLESS YOU WANT RANDON TO CRASH OF COURSE (CEC)                }
      InitialSeed:=FALSE;
      UseSeed(Randseed);
    end;
    Inc(Seed1);
    Seed1 := (Seed1 * 998) mod 1000003;
    Random := Seed1 mod Succ(l);
end;


Procedure UseSeed(seed : Longint);
begin
    Seed1 := seed mod 1000003;
    Seed2 := (Random(65000) * Random(65000)) mod 600011;
    Seed3 := (Random(65000) * Random(65000)) mod 765241;
end;



{ Include processor specific routines }
{$I math.inc}

{****************************************************************************
                                                         Memory Management
****************************************************************************}

{$ifndef RTLLITE}

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

{$endif RTLLITE}

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


Procedure dump_stack(bp : Longint);

  Procedure dump_frame(addr : Longint);
  Begin
  {To be used by symify}
    Writeln(stderr,'  0x',HexStr(addr,8));
{$IFNDEF NEW_READWRITE}
    Flush(stderr);
{$ENDIF NEW_READWRITE}
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
{$IFNDEF NEW_READWRITE}
  Flush(stderr);
{$ENDIF NEW_READWRITE}
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
  Revision 1.21  1998-07-15 12:09:35  carl
    * would not compile under FPC v0.99.5

  Revision 1.20  1998/07/13 21:19:12  florian
    * some problems with ansi string support fixed

  Revision 1.19  1998/07/08 11:56:55  carl
    * randon and Random(l) now work correctly - don't touch it works!

  Revision 1.18  1998/07/02 13:01:55  carl
    * hmmm... it is luck (BSS zeroed with GAS) that DoError and ErrorBase work.
      Now they are initilized instead.

  Revision 1.17  1998/07/02 12:53:09  carl
    * DOERROR RESOTRED! DON'T TOUCH :)

  Revision 1.16  1998/07/02 12:11:50  carl
    * no SINGLE in m68k and other processors!

  Revision 1.15  1998/07/02 09:25:05  peter
    * fixed do_error in runtimeerror

  Revision 1.14  1998/07/01 15:29:59  peter
    * better readln/writeln

  Revision 1.13  1998/06/26 08:21:09  daniel
  - Doerror removed.

  Revision 1.12  1998/06/25 14:04:25  peter
    + internal inc/dec

  Revision 1.11  1998/06/25 09:44:20  daniel
  + RTLLITE directive to compile minimal RTL.

  Revision 1.10  1998/06/15 15:16:26  daniel
  * RTLLITE conditional added to produce smaller RTL

  Revision 1.9  1998/06/10 07:46:45  michael
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
