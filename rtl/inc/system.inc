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

Procedure HandleError (Errno : Longint); forward;

type
  FileFunc = Procedure(var t : TextRec);

const
{ Random / Randomize constants }
  OldRandSeed : Longint = 0;
  InitialSeed : Boolean = TRUE;
  Seed2 : Cardinal = 0;
  Seed3 : Cardinal = 0;

{ For Error Handling.}
  DoError   : Boolean = FALSE;
  ErrorBase : Longint = 0;

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

Function chr(b : byte) : Char;      [INTERNPROC: In_chr_byte];
Function Length(s : string) : byte; [INTERNPROC: In_Length_string];
Function Length(c : char) : byte;   [INTERNPROC: In_Length_string];

Procedure Reset(var f : TypedFile);   [INTERNPROC: In_Reset_TypedFile];
Procedure Rewrite(var f : TypedFile); [INTERNPROC: In_Rewrite_TypedFile];

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
                                Set Handling
****************************************************************************}

{ Include set support which is processor specific}
{$I set.inc}

{****************************************************************************
                  Subroutines for String handling
****************************************************************************}

{ Needs to be before RTTI handling }

{$i sstrings.inc}

Type
   PLongint = ^Longint;
   PByte = ^Byte;

{$i astrings.pp}


{****************************************************************************
                         Run-Time Type Information (RTTI)
****************************************************************************}

{$i rtti.inc}


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

Function swap (X : Word) : Word;[internconst:in_const_swap_word];
Begin
  swap:=(X and $ff) shl 8 + (X shr 8)
End;

Function Swap (X : Integer) : Integer;[internconst:in_const_swap_word];
Begin
  Swap:=Integer(Swap(Word(X)));
End;

Function swap (X : Longint) : Longint;[internconst:in_const_swap_long];
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

Function Swap (X : Cardinal) : Cardinal;[internconst:in_const_swap_long];
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

Procedure UseSeed(seed : Longint);Forward;


Function Random : Real;
var
    ReturnValue : Real;
begin
    if (InitialSeed) OR ((RandSeed <> OldRandSeed) AND (NOT InitialSeed)) then
    Begin
    { This is a pretty complicated affair                             }
    {  Initially we must call UseSeed when RandSeed is initalized     }
    {  We must also call UseSeed each time RandSeed is reinitialized  }
    {  DO NOT CHANGE THE ORDER OF DECLARATIONS IN THIS BLOCK          }
    {  UNLESS YOU WANT RANDON TO CRASH OF COURSE (CEC)                }
      InitialSeed:=FALSE;
      OldRandSeed:=RandSeed;
      UseSeed(RandSeed);
    end;
    Inc(RandSeed);
    RandSeed := (RandSeed * 706) mod 500009;
    OldRandSeed:=RandSeed;
    INC(Seed2);
    Seed2 := (Seed2 * 774) MOD 600011;
    INC(Seed3);
    Seed3 := (Seed3 * 871) MOD 765241;
    ReturnValue := RandSeed/500009.0 +
            Seed2/600011.0 +
           Seed3/765241.0;
    Random := frac(ReturnValue);
end;


Function Random(l : Longint) : Longint;
begin
    if (InitialSeed) OR ((RandSeed <> OldRandSeed) AND (NOT InitialSeed)) then
    Begin
    { This is a pretty complicated affair                             }
    {  Initially we must call UseSeed when RandSeed is initalized     }
    {  We must also call UseSeed each time RandSeed is reinitialized  }
    {  DO NOT CHANGE THE ORDER OF DECLARATIONS IN THIS BLOCK          }
    {  UNLESS YOU WANT RANDON TO CRASH OF COURSE (CEC)                }
      InitialSeed:=FALSE;
      OldRandSeed:=RandSeed;
      UseSeed(Randseed);
    end;
    Inc(RandSeed);
    RandSeed := (RandSeed * 998) mod 1000003;
    OldRandSeed:=RandSeed;
    Random := RandSeed mod l;
end;


Procedure UseSeed(seed : Longint);
begin
    randseed := seed mod 1000003;
    Seed2 := (Random(65000) * Random(65000)) mod 600011;
    Seed3 := (Random(65000) * Random(65000)) mod 765241;
end;


{ Include processor specific routines }
{$I math.inc}

{****************************************************************************
                            Memory Management
****************************************************************************}

{$ifndef RTLLITE}

Function Ptr(sel,off : Longint) : pointer;[internconst:in_const_ptr];
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

procedure int_overflow;[public,alias: {$ifdef FPCNAMES}'FPC_OVERFLOW'{$else}'RE_OVERFLOW'{$endif}];
var
  addr : longint;
begin
   addr:=get_caller_addr(get_frame);
   If ErrorProc<>Nil then
     TErrorProc (ErrorProc)(215,Pointer(Addr));
{$ifndef RTLLITE}
   Writeln('Overflow at 0x',HexStr(addr,8));
{$endif}
   HandleError(215);
end;


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

Procedure HandleError (Errno : longint);[public,alias : 'FPC_HANDLEERROR'];
{
  Procedure to handle internal errors, i.e. not user-invoked errors
  Internal function should ALWAYS call HandleError instead of RunError.
}
var
  addr : longint;
begin
  addr:=get_caller_addr(get_frame);
  If ErrorProc<>Nil then
    TErrorProc (ErrorProc)(Errno,pointer(addr));
  errorcode:=Errno;
  exitcode:=Errno;
  erroraddr:=pointer(addr);
  errorbase:=get_caller_frame(get_frame);
  DoError:=true;
  halt(errorcode);
end;


procedure runerror(w : word);[alias: 'FPC_RUNERROR'];
begin
  errorcode:=w;
  exitcode:=w;
  erroraddr:=pointer(get_caller_addr(get_frame));
  errorbase:=get_caller_frame(get_frame);
  DoError:=true;
  halt(errorcode);
end;


Procedure RunError;
Begin
  RunError (0);
End;


Procedure Halt;
Begin
  Halt(0);
End;


Procedure dump_stack(bp : Longint);
var
  i, prevbp : Longint;
Begin
  prevbp:=bp-1;
  i:=0;
  while bp > prevbp Do
   Begin
     Writeln(stderr,'  0x',HexStr(get_caller_addr(bp),8));
     Inc(i);
     If i>max_frame_dump Then
      exit;
     prevbp:=bp;
     bp:=get_caller_frame(bp);
   End;
End;


Procedure do_exit;[Public,Alias: {$ifdef FPCNAMES}'FPC_DO_EXIT'{$else}'__EXIT'{$endif}];
{
  Don't call this direct, the call is generated by the compiler
  and by the halt procedure.
  NOTICE: (CEC - 14/Aug/1998)
  The order of calling this routine must not be changed, especially
  regarding doerror, doerror should only be set by handlerror
  and runerror and nowhere else, as certain system units require
  exit procedures to clean up, and they rely on this behavior as not
  to call themselves recursively.
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


{*****************************************************************************
                          Abstract/Assert support.
*****************************************************************************}

procedure AbstractError;[public,alias : 'FPC_ABSTRACTERROR'];
Type
  TAbstractErrorProc=Procedure;
begin
  If AbstractErrorProc<>nil then
    TAbstractErrorProc(AbstractErrorProc);
  RunError(211);
end;


Procedure int_assert(Const Msg,FName:string;LineNo,ErrorAddr:Longint); [Public,Alias : 'FPC_ASSERT'];
type
  TAssertErrorProc=procedure(const msg,fname:string;lineno,erroraddr:longint);
begin
  if AssertErrorProc<>nil then
   TAssertErrorProc(AssertErrorProc)(Msg,FName,LineNo,ErrorAddr)
  else
   HandleError(227);
end;


Procedure SysAssert(Const Msg,FName:string;LineNo,ErrorAddr:Longint);
begin
  If msg='' then
    write(stderr,'Assertion failed')
  else
    write(stderr,msg);
  writeln(stderr,' (',FName,', line ',LineNo,').');
end;


{*****************************************************************************
      SetJmp/LongJmp support.
*****************************************************************************}

{$i setjump.inc}


{
  $Log$
  Revision 1.41  1998-11-05 10:29:36  pierre
   * fix for length(char) in const expressions

  Revision 1.40  1998/11/04 20:34:02  michael
  + Removed ifdef useansistrings

  Revision 1.39  1998/10/12 22:11:28  jonas
    * fixed RandSeed bug

  Revision 1.38  1998/10/12 12:43:37  florian
    * made FPC_HANDLEERROR public

  Revision 1.37  1998/10/07 11:40:08  jonas
    * changed seed2 and seed3 to cardinal to prevent overflow


  Revision 1.36  1998/10/05 12:32:51  peter
    + assert() support

  Revision 1.35  1998/10/02 09:25:11  peter
    * more constant expression evals

  Revision 1.34  1998/09/22 15:30:54  peter
    * shortstring=string type added

  Revision 1.33  1998/09/16 13:08:03  michael
  Added AbstractErrorHandler

  Revision 1.32  1998/09/16 12:37:07  michael
  Added FPC_ prefix to abstracterror

  Revision 1.31  1998/09/15 17:12:32  michael
  + Merged changes from fixes branch


  Revision 1.30  1998/09/14 10:48:20  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.29.2.1  1998/09/15 17:08:43  michael
  + Added abstracterror call

  Revision 1.29  1998/09/01 17:36:21  peter
    + internconst

  Revision 1.28  1998/08/17 12:24:16  carl
    + important comment added

  Revision 1.27  1998/08/13 16:22:11  jonas
  * random now returns a value between 0 and max-1 instead of between 0 and max

  Revision 1.26  1998/08/11 00:05:26  peter
    * $ifdef ver0_99_5 updates

  Revision 1.25  1998/07/30 13:26:18  michael
  + Added support for ErrorProc variable. All internal functions are required
    to call HandleError instead of runerror from now on.
    This is necessary for exception support.

  Revision 1.24  1998/07/28 20:37:45  michael
  + added setjmp/longjmp and exception support

  Revision 1.23  1998/07/23 19:53:20  michael
  + Adapted assert to Delphi format

  Revision 1.22  1998/07/23 13:08:41  michael
  + Implemented DO_ASSERT function.

  Revision 1.21  1998/07/15 12:09:35  carl
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
