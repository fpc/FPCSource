{
    $Id$

    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

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
Procedure HandleErrorFrame (Errno : longint;frame : longint); forward;

type
  FileFunc = Procedure(var t : TextRec);

  PByte    = ^Byte;
  PWord    = ^word;
  PDWord   = ^DWord;
  PLongint = ^Longint;

const
{ Random / Randomize constants }
  OldRandSeed : Cardinal = 0;
  InitialSeed : Boolean = TRUE;
  Seed2 : Cardinal = 0;
  Seed3 : Cardinal = 0;

{ For Error Handling.}
  ErrorBase : Longint = 0;

{ Used by the ansistrings and maybe also other things in the future }
var
  emptychar : char;public name 'FPC_EMPTYCHAR';


{****************************************************************************
                     Routines which have compiler magic
****************************************************************************}

{$I innr.inc}

Function  lo(i : Integer) : byte;  [INTERNPROC: In_lo_Word];
Function  lo(w : Word) : byte;     [INTERNPROC: In_lo_Word];
Function  lo(l : Longint) : Word;  [INTERNPROC: In_lo_long];
Function  lo(l : DWord) : Word;    [INTERNPROC: In_lo_long];
Function  hi(i : Integer) : byte;  [INTERNPROC: In_hi_Word];
Function  hi(w : Word) : byte;     [INTERNPROC: In_hi_Word];
Function  hi(l : Longint) : Word;  [INTERNPROC: In_hi_long];
Function  hi(l : DWord) : Word;    [INTERNPROC: In_hi_long];

{$ifdef INT64}
Function  lo(q : QWord) : DWord;  [INTERNPROC: In_lo_qword];
Function  lo(i : Int64) : DWord;  [INTERNPROC: In_lo_qword];
Function  hi(q : QWord) : DWord;  [INTERNPROC: In_hi_qword];
Function  hi(i : Int64) : DWord;  [INTERNPROC: In_hi_qword];
{$endif}

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

{ Include generic pascal only routines which are not defined in the processor
  specific include file }
{$I generic.inc}


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

{$i astrings.inc}

{$ifdef haswidechar}
{$i wstrings.inc}
{$endif haswidechar}

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
  swap:=(X and $ff) shl 8 + (X shr 8)
End;

Function swap (X : Longint) : Longint;[internconst:in_const_swap_long];
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

Function Swap (X : Cardinal) : Cardinal;[internconst:in_const_swap_long];
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

{$ifdef INT64}
Function Swap (X : QWord) : QWord;
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;

Function swap (X : Int64) : Int64;
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;
{$endif}

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

Procedure NewSeed;Forward;


Function Random : Extended;
begin
    if (InitialSeed) OR (RandSeed <> OldRandSeed) then
    Begin
    { This is a pretty complicated affair                             }
    {  Initially we must call NewSeed when RandSeed is initalized     }
    {  We must also call NewSeed each time RandSeed is reinitialized  }
    {  DO NOT CHANGE THE ORDER OF DECLARATIONS IN THIS BLOCK          }
    {  UNLESS YOU WANT RANDON TO CRASH OF COURSE (CEC)                }
      InitialSeed:=FALSE;
      OldRandSeed:=RandSeed;
      NewSeed;
    end;
    Inc(RandSeed);
    RandSeed := (RandSeed * 706) mod 500009;
    OldRandSeed:=RandSeed;
    INC(Seed2);
    Seed2 := (Seed2 * 774) MOD 600011;
    INC(Seed3);
    Seed3 := (Seed3 * 871) MOD 765241;
    Random :=
      frac(RandSeed/500009.0 +
           Seed2/600011.0 +
           Seed3/765241.0);
end;

Function internRandom(l : Cardinal) : Cardinal;
begin
    if (InitialSeed) OR (RandSeed <> OldRandSeed) then
      Begin
      { This is a pretty complicated affair                             }
      {  Initially we must call NewSeed when RandSeed is initalized     }
      {  We must also call NewSeed each time RandSeed is reinitialized  }
      {  DO NOT CHANGE THE ORDER OF DECLARATIONS IN THIS BLOCK          }
      {  UNLESS YOU WANT RANDOM TO CRASH OF COURSE (CEC)                }
        InitialSeed:=FALSE;
        OldRandSeed:=RandSeed;
        NewSeed;
      end;
    Inc(RandSeed);
    RandSeed := (RandSeed * 998) mod 1000003;
    OldRandSeed:=RandSeed;
    if l<>0 then
      begin
        internRandom := RandSeed mod l;
      end
    else internRandom:=0;
end;

function random(l:cardinal): cardinal;
begin
  random := trunc(random()*l);
end;

{$ifndef cardinalmulfixed}
function random(l:longint): longint;
begin
  random := trunc(random()*l);
end;
{$endif cardinalmulfixed}

Procedure NewSeed;
begin
    randseed := randseed mod 1000003;
    Seed2 := (internRandom(65000) * internRandom(65000)) mod 600011;
    Seed3 := (internRandom(65000) * internRandom(65000)) mod 765241;
end;

{ Include processor specific routines }
{$I math.inc}

{$ifdef INT64}
{$I int64.inc}
{$endif INT64}

{****************************************************************************
                            Memory Management
****************************************************************************}

{$ifndef RTLLITE}

Function Ptr(sel,off : Longint) : pointer;[internconst:in_const_ptr];
Begin
  sel:=0;
  ptr:=pointer(off);
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
                             Directory support.
*****************************************************************************}

Procedure getdir(drivenr:byte;Var dir:ansistring);
{ this is needed to also allow ansistrings, the shortstring version is
  OS dependent }
var
  s : shortstring;
begin
  getdir(drivenr,s);
  dir:=s;
end;


{*****************************************************************************
                             Miscellaneous
*****************************************************************************}

procedure int_overflow;[public,alias:'FPC_OVERFLOW'];
begin
  HandleErrorFrame(215,get_frame);
end;


{$ifdef HASSAVEREGISTERS}
procedure int_iocheck(addr : longint);[saveregisters,public,alias:'FPC_IOCHECK'];
var
  l : longint;
begin
  if InOutRes<>0 then
   begin
     l:=InOutRes;
     InOutRes:=0;
     HandleErrorFrame(l,get_frame);
   end;
end;
{$endif}


Function IOResult:Word;
Begin
  IOResult:=InOutRes;
  InOutRes:=0;
End;


procedure fillchar(var x;count : longint;value : boolean);
begin
  fillchar(x,count,byte(value));
end;


procedure fillchar(var x;count : longint;value : char);
begin
  fillchar(x,count,byte(value));
end;


{*****************************************************************************
                        Initialization / Finalization
*****************************************************************************}

const
  maxunits=1024; { See also files.pas of the compiler source }
type
  TInitFinalRec=record
    InitProc,
    FinalProc : TProcedure;
  end;
  TInitFinalTable=record
    TableCount,
    InitCount  : longint;
    Procs      : array[1..maxunits] of TInitFinalRec;
  end;

var
  InitFinalTable : TInitFinalTable;external name 'INITFINAL';

procedure InitializeUnits;[public,alias:'FPC_INITIALIZEUNITS'];
var
  i : longint;
begin
  with InitFinalTable do
   begin
     for i:=1to TableCount do
      begin
        if assigned(Procs[i].InitProc) then
         Procs[i].InitProc();
        InitCount:=i;
      end;
   end;
end;


procedure FinalizeUnits;[public,alias:'FPC_FINALIZEUNITS'];
begin
  with InitFinalTable do
   begin
     while (InitCount>0) do
      begin
        if assigned(Procs[InitCount].FinalProc) then
         Procs[InitCount].FinalProc();
        dec(InitCount);
      end;
   end;
end;


{*****************************************************************************
                          Error / Exit / ExitProc
*****************************************************************************}

Procedure system_exit;forward;

Procedure do_exit;[Public,Alias:'FPC_DO_EXIT'];
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
  { Finalize units }
  FinalizeUnits;
  { Show runtime error }
  If erroraddr<>nil Then
   Begin
     Writeln(stdout,'Runtime error ',Errorcode,' at 0x',hexstr(Longint(Erroraddr),8));
     { to get a nice symify }
     Writeln(stdout,BackTraceStrFunc(Longint(Erroraddr)));
     dump_stack(stdout,ErrorBase);
     Writeln(stdout,'');
   End;
  { call system dependent exit code }
  System_exit;
End;


Procedure Halt(ErrNum: Byte);
Begin
  ExitCode:=Errnum;
  Do_Exit;
end;


function SysBackTraceStr (Addr: longint): ShortString;
begin
  SysBackTraceStr:='  0x'+HexStr(addr,8);
end;


Procedure HandleErrorFrame (Errno : longint;frame : longint);
{
  Procedure to handle internal errors, i.e. not user-invoked errors
  Internal function should ALWAYS call HandleError instead of RunError.
  Can be used for exception handlers to specify the frame
}
var
  addr : longint;
begin
  addr:=get_caller_addr(frame);
  If pointer(ErrorProc)<>Nil then
    ErrorProc(Errno,pointer(addr));
  errorcode:=Errno;
  exitcode:=Errno;
  erroraddr:=pointer(addr);
  errorbase:=get_caller_frame(frame);
  halt(errorcode);
end;


Procedure HandleError (Errno : longint);[public,alias : 'FPC_HANDLEERROR'];
{
  Procedure to handle internal errors, i.e. not user-invoked errors
  Internal function should ALWAYS call HandleError instead of RunError.
}
begin
  HandleErrorFrame(Errno,get_frame);
end;


procedure runerror(w : word);[alias: 'FPC_RUNERROR'];
begin
  errorcode:=w;
  exitcode:=w;
  erroraddr:=pointer(get_caller_addr(get_frame));
  errorbase:=get_caller_frame(get_frame);
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


Procedure dump_stack(var f : text;bp : Longint);
var
  i, prevbp : Longint;
Begin
  prevbp:=bp-1;
  i:=0;
  while bp > prevbp Do
   Begin
     Writeln(f,BackTraceStrFunc(get_caller_addr(bp)));
     Inc(i);
     If i>max_frame_dump Then
      exit;
     prevbp:=bp;
     bp:=get_caller_frame(bp);
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
begin
  If pointer(AbstractErrorProc)<>nil then
    AbstractErrorProc();
  HandleErrorFrame(211,get_frame);
end;


Procedure int_assert(Const Msg,FName:Shortstring;LineNo,ErrorAddr:Longint); [{$ifdef HASSAVEREGISTERS}SaveRegisters,{$endif}Public,Alias : 'FPC_ASSERT'];
begin
  if pointer(AssertErrorProc)<>nil then
    AssertErrorProc(Msg,FName,LineNo,ErrorAddr)
  else
    HandleErrorFrame(227,get_frame);
end;


Procedure SysAssert(Const Msg,FName:Shortstring;LineNo,ErrorAddr:Longint);
begin
  If msg='' then
    write(stderr,'Assertion failed')
  else
    write(stderr,msg);
  Writeln(stderr,' (',FName,', line ',LineNo,').');
  Writeln(stderr,'');
end;


{*****************************************************************************
                       SetJmp/LongJmp support.
*****************************************************************************}

{$i setjump.inc}


{*****************************************************************************
                        Object Pascal support
*****************************************************************************}

{$i objpas.inc}

{
  $Log$
  Revision 1.82  2000-02-09 12:17:51  peter
    * moved halt to system.inc
    * syslinux doesn't use direct asm anymore

  Revision 1.81  2000/02/06 17:19:22  peter
    * lineinfo unit added which uses stabs to get lineinfo for backtraces

  Revision 1.80  2000/01/10 09:54:30  peter
    * primitives added

  Revision 1.79  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.78  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.77  1999/12/21 11:10:22  pierre
   * allow v09912 to compile system

  Revision 1.76  1999/12/18 14:54:34  florian
    * very basic widestring support

  Revision 1.75  1999/12/12 13:29:34  jonas
    * remove "random(longint): longint" if cardinalmulfixed is defined

  Revision 1.74  1999/12/01 12:37:13  jonas
    + function random(longint): longint

  Revision 1.73  1999/11/20 12:48:09  jonas
    * reinstated old random generator, but modified it so the integer
      one now has a much longer period

  Revision 1.72  1999/11/15 21:49:47  peter
    * exception address fixes

  Revision 1.71  1999/11/09 22:40:12  pierre
   + get also first BackTrace address with '  0x' prefix

  Revision 1.70  1999/11/09 20:14:12  daniel
  * Committed new random generator.

  Revision 1.69  1999/11/06 14:35:39  peter
    * truncated log

  Revision 1.68  1999/10/26 12:31:00  peter
    * *errorproc are not procvars instead of pointers which allows better
      error checking for the parameters (shortstring<->ansistring)

  Revision 1.67  1999/09/18 16:05:12  jonas
    * dump_stack now actually dumps its info to f (was still hardcoded
      to stderr)

  Revision 1.66  1999/08/05 23:45:14  peter
    * saveregister is now working and used for assert and iocheck (which has
      been moved to system.inc because it's now system independent)

  Revision 1.65  1999/07/28 12:58:22  peter
    * fixed assert() to push/pop registers

  Revision 1.64  1999/07/05 20:04:27  peter
    * removed temp defines

  Revision 1.63  1999/07/03 01:24:19  peter
    * $ifdef int64

  Revision 1.62  1999/07/02 18:06:42  florian
    + qword/int64: lo/hi/swap

  Revision 1.61  1999/07/01 15:39:51  florian
    + qword/int64 type released

  Revision 1.60  1999/06/11 11:47:00  peter
    * random doesn't rte 200 with random(0)

  Revision 1.59  1999/06/05 20:45:12  michael
  + AbstractErro should call HandleError, not runerror.

  Revision 1.58  1999/05/17 21:52:39  florian
    * most of the Object Pascal stuff moved to the system unit

  Revision 1.57  1999/04/17 13:10:25  peter
    * addr() internal

  Revision 1.56  1999/04/15 12:20:01  peter
    + finalization support

  Revision 1.55  1999/03/01 15:41:03  peter
    * use external names
    * removed all direct assembler modes

  Revision 1.54  1999/02/01 00:05:14  florian
    + functions lo/hi for DWord type implemented

  Revision 1.53  1999/01/29 09:23:09  pierre
   * Fillchar(..,..,boolean) added

  Revision 1.52  1999/01/22 12:39:23  pierre
   + added text arg for dump_stack

  Revision 1.51  1999/01/18 10:05:52  pierre
   + system_exit procedure added

  Revision 1.50  1998/12/28 15:50:46  peter
    + stdout, which is needed when you write something in the system unit
      to the screen. Like the runtime error

  Revision 1.49  1998/12/21 14:28:21  pierre
    * HandleError -> HandleErrorFrame to avoid problem in
      assembler code in i386.inc
      (call to overloaded function in assembler block !)

}
