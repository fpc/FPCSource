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


const
  STACK_MARGIN = 16384;    { Stack size margin for stack checking }
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
  initialstklen : longint;external name '__stklen';


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

Function  lo(q : QWord) : DWord;  [INTERNPROC: In_lo_qword];
Function  lo(i : Int64) : DWord;  [INTERNPROC: In_lo_qword];
Function  hi(q : QWord) : DWord;  [INTERNPROC: In_hi_qword];
Function  hi(i : Int64) : DWord;  [INTERNPROC: In_hi_qword];

Function chr(b : byte) : Char;      [INTERNPROC: In_chr_byte];
{$ifndef INTERNLENGTH}
Function Length(s : string) : byte; [INTERNPROC: In_Length_string];
Function Length(c : char) : byte;   [INTERNPROC: In_Length_string];
{$endif INTERNLENGTH}

Procedure Reset(var f : TypedFile);   [INTERNPROC: In_Reset_TypedFile];
Procedure Rewrite(var f : TypedFile); [INTERNPROC: In_Rewrite_TypedFile];


{****************************************************************************
                    Include processor specific routines
****************************************************************************}

{$ifdef i386}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$define ENDIAN_LITTLE}
  {$i i386.inc}  { Case dependent, don't change }
{$endif i386}

{$ifdef m68k}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$define ENDIAN_BIG}
  {$i m68k.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif m68k}

{$ifdef x86_64}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$define ENDIAN_LITTLE}
  {$i x86_64.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif x86_64}

{$ifdef powerpc}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$define ENDIAN_BIG}
  {$i powerpc.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif powerpc}

{$ifdef alpha}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$define ENDIAN_BIG}
  {$i alpha.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif alpha}

{$ifdef iA64}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$define ENDIAN_LITTLE}
  {$i ia64.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif iA64}


procedure fillchar(var x;count : longint;value : boolean);{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  fillchar(x,count,byte(value));
end;

procedure fillchar(var x;count : longint;value : char);{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  fillchar(x,count,byte(value));
end;

{ Include generic pascal only routines which are not defined in the processor
  specific include file }
{$I generic.inc}


{****************************************************************************
                                Set Handling
****************************************************************************}

{ Include set support which is processor specific}
{$i set.inc}
{ Include generic pascal routines for sets if the processor }
{ specific routines are not available.                      }
{$i genset.inc}


{****************************************************************************
                               Math Routines
****************************************************************************}

function Hi(b : byte): byte;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   Hi := b shr 4
end;

function Lo(b : byte): byte;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   Lo := b and $0f
end;

Function swap (X : Word) : Word;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_swap_word];
Begin
  swap:=(X and $ff) shl 8 + (X shr 8)
End;

Function Swap (X : Integer) : Integer;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_swap_word];
Begin
  swap:=(X and $ff) shl 8 + (X shr 8)
End;

Function swap (X : Longint) : Longint;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_swap_long];
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

Function Swap (X : Cardinal) : Cardinal;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_swap_long];
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

Function Swap (X : QWord) : QWord;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_swap_qword];
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;

Function swap (X : Int64) : Int64;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_swap_qword];
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;

operator := (b:real48) d:double;

begin
 D:=real2double(b);
end;

{$ifdef SUPPORT_EXTENDED}
operator := (b:real48) e:extended;

begin
 e:=real2double(b);
end;
{$endif SUPPORT_EXTENDED}

{ Include processor specific routines }
{$I math.inc}
{ Include generic version }
{$I genmath.inc}

{****************************************************************************
                  Subroutines for String handling
****************************************************************************}

{ Needs to be before RTTI handling }

{$i sstrings.inc}

{ requires sstrings.inc for initval }
{$I int64.inc}

{Requires int64.inc, since that contains the VAL functions for int64 and qword}
{$i astrings.inc}

{$ifdef HASWIDESTRING}
{$i wstrings.inc}
{$endif HASWIDESTRING}

{$i aliases.inc}

{*****************************************************************************
                        Dynamic Array support
*****************************************************************************}

{$i dynarr.inc}

{*****************************************************************************
                        Object Pascal support
*****************************************************************************}

{$i objpas.inc}

{*****************************************************************************
                            Variant support
*****************************************************************************}

{$ifdef HASVARIANT}
{$i variant.inc}
{$endif HASVARIANT}
{****************************************************************************
                         Run-Time Type Information (RTTI)
****************************************************************************}

{$i rtti.inc}

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

function random(l:longint): longint;
begin
  random := trunc(random()*l);
end;

Procedure NewSeed;
begin
    randseed := randseed mod 1000003;
    Seed2 := (internRandom(65000) * internRandom(65000)) mod 600011;
    Seed3 := (internRandom(65000) * internRandom(65000)) mod 765241;
end;


{****************************************************************************
                            Memory Management
****************************************************************************}

Function Ptr(sel,off : Longint) : farpointer;{$ifdef SYSTEMINLINE}inline;{$endif}[internconst:in_const_ptr];
Begin
  ptr:=farpointer((sel shl 4)+off);
End;

Function CSeg : Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Cseg:=0;
End;

Function DSeg : Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Dseg:=0;
End;

Function SSeg : Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Sseg:=0;
End;


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

{$ifopt R+}
{$define RangeCheckWasOn}
{$R-}
{$endif opt R+}

{$ifopt I+}
{$define IOCheckWasOn}
{$I-}
{$endif opt I+}

{$ifopt Q+}
{$define OverflowCheckWasOn}
{$Q-}
{$endif opt Q+}

{*****************************************************************************
                             Miscellaneous
*****************************************************************************}

procedure fpc_rangeerror;[public,alias:'FPC_RANGEERROR']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  HandleErrorFrame(201,get_frame);
end;


procedure fpc_overflow;[public,alias:'FPC_OVERFLOW']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  HandleErrorFrame(215,get_frame);
end;


procedure fpc_iocheck(addr : longint);[saveregisters,public,alias:'FPC_IOCHECK']; {$ifdef hascompilerproc} compilerproc; {$endif}
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


Function IOResult:Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  IOResult:=InOutRes;
  InOutRes:=0;
End;


{*****************************************************************************
                         Stack check code
*****************************************************************************}

{$IFNDEF NO_GENERIC_STACK_CHECK}

{$IFOPT S+}
{$DEFINE STACKCHECK}
{$ENDIF}
{$S-}
procedure fpc_stackcheck(stack_size:Cardinal);[saveregisters,public,alias:'FPC_STACKCHECK'];
var
  c : cardinal;
begin
  { Avoid recursive calls when called from the exit routines }
  if StackError then
   exit;
  c := cardinal(Sptr) - stack_size - STACK_MARGIN;
  if (c <= StackBottom) then
   begin
     StackError:=true;
     HandleError(202);
   end;
end;
{$IFDEF STACKCHECK}
{$S+}
{$ENDIF}
{$UNDEF STACKCHECK}

{$ENDIF NO_GENERIC_STACK_CHECK}

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

procedure fpc_InitializeUnits;[public,alias:'FPC_INITIALIZEUNITS']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
        // we've to decrement the cound before calling the final. code
        // else a halt in the final. code leads to a endless loop
        dec(InitCount);
        if assigned(Procs[InitCount+1].FinalProc) then
         Procs[InitCount+1].FinalProc();
      end;
   end;
end;

{*****************************************************************************
                          Error / Exit / ExitProc
*****************************************************************************}

Procedure system_exit;forward;

Procedure InternalExit;
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
  { Show runtime error and exit }
  If erroraddr<>nil Then
   Begin
     Writeln(stdout,'Runtime error ',Errorcode,' at 0x',hexstr(Longint(Erroraddr),8));
     { to get a nice symify }
     Writeln(stdout,BackTraceStrFunc(Longint(Erroraddr)));
     dump_stack(stdout,ErrorBase);
     Writeln(stdout,'');
   End;
End;


Procedure do_exit;[Public,Alias:'FPC_DO_EXIT'];
begin
  InternalExit;
  System_exit;
end;


Procedure lib_exit;saveregisters;[Public,Alias:'FPC_LIB_EXIT'];
begin
  InternalExit;
end;


Procedure Halt(ErrNum: Byte);
Begin
  ExitCode:=Errnum;
  Do_Exit;
end;


function SysBackTraceStr (Addr: longint): ShortString;
begin
  SysBackTraceStr:='  0x'+HexStr(addr,8);
end;


Procedure HandleErrorAddrFrame (Errno : longint;addr,frame : longint);[public,alias:'FPC_BREAK_ERROR'];
begin
  If pointer(ErrorProc)<>Nil then
    ErrorProc(Errno,pointer(addr),pointer(frame));
  errorcode:=word(Errno);
  exitcode:=word(Errno);
  erroraddr:=pointer(addr);
  errorbase:=frame;
  halt(errorcode);
end;

Procedure HandleErrorFrame (Errno : longint;frame : longint);
{
  Procedure to handle internal errors, i.e. not user-invoked errors
  Internal function should ALWAYS call HandleError instead of RunError.
  Can be used for exception handlers to specify the frame
}
begin
  HandleErrorAddrFrame(Errno,get_caller_addr(frame),get_caller_frame(frame));
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


Procedure RunError;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  RunError (0);
End;


Procedure Halt;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Halt(0);
End;


function do_isdevice(handle:longint):boolean;forward;

Procedure dump_stack(var f : text;bp : Longint);
var
  i, prevbp : Longint;
  is_dev : boolean;
  caller_addr : longint;
Begin
  prevbp:=bp-1;
  i:=0;
  is_dev:=do_isdevice(textrec(f).Handle);
  while bp > prevbp Do
   Begin
     caller_addr := get_caller_addr(bp);
     if caller_addr <> 0 then
         Writeln(f,BackTraceStrFunc(caller_addr));
     Inc(i);
     If ((i>max_frame_dump) and is_dev) or (i>256) Then
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

{$ifdef hascompilerproc}
{ alias for internal usage in the compiler }
procedure fpc_AbstractErrorIntern;  compilerproc; external name 'FPC_ABSTRACTERROR';
{$endif hascompilerproc}

Procedure fpc_assert(Const Msg,FName:Shortstring;LineNo,ErrorAddr:Longint); [SaveRegisters,Public,Alias : 'FPC_ASSERT']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
  Halt(227);
end;


{*****************************************************************************
                       SetJmp/LongJmp support.
*****************************************************************************}

{$i setjump.inc}


{$ifdef IOCheckWasOn}
{$I+}
{$endif}

{$ifdef RangeCheckWasOn}
{$R+}
{$endif}

{$ifdef OverflowCheckWasOn}
{$Q+}
{$endif}

{
  $Log$
  Revision 1.38  2002-12-07 14:36:33  carl
    - avoid warnings (add typecast)

  Revision 1.37  2002/11/18 18:33:51  peter
    * Swap(QWord) constant support

  Revision 1.36  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.35  2002/09/18 18:32:01  carl
    * assert now halts with exitcode 227 (as Delphi does)

  Revision 1.34  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.33  2002/08/19 19:34:02  peter
    * SYSTEMINLINE define that will add inline directives for small
      functions and wrappers. This will be defined automaticly when
      the compiler defines the HASINLINE directive

  Revision 1.32  2002/07/28 20:43:48  florian
    * several fixes for linux/powerpc
    * several fixes to MT

  Revision 1.31  2002/07/26 22:46:06  florian
    * interface of system unit for Linux/PowerPC compiles

  Revision 1.30  2002/07/26 16:42:00  florian
    * endian directive for PowerPC fixed

  Revision 1.29  2002/07/04 20:40:09  florian
    + some x86-64 support added

  Revision 1.28  2002/04/21 15:51:50  carl
  * StackError is now a typed constant
  + $S can be used under unix

  Revision 1.27  2002/04/15 19:38:40  peter
    * stackcheck protected against infinite recursive after stack error
    * stackcheck requires saveregisters, because it can be called from
      iocheck and then will destroy the result of the original function

  Revision 1.26  2002/04/15 18:51:20  carl
  + generic stack checking can be overriden

  Revision 1.25  2002/04/12 17:37:36  carl
  + generic stack checking

}
