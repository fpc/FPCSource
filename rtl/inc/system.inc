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

Function  lo(q : QWord) : DWord;  [INTERNPROC: In_lo_qword];
Function  lo(i : Int64) : DWord;  [INTERNPROC: In_lo_qword];
Function  hi(q : QWord) : DWord;  [INTERNPROC: In_hi_qword];
Function  hi(i : Int64) : DWord;  [INTERNPROC: In_hi_qword];

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

Function Swap (X : QWord) : QWord;
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;

Function swap (X : Int64) : Int64;
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;

{$endif RTLLITE}

{ Include processor specific routines }
{$I math.inc}

{****************************************************************************
                  Subroutines for String handling
****************************************************************************}

{ Needs to be before RTTI handling }

{$i sstrings.inc}

{ requires sstrings.inc for initval }
{$I int64.inc}

{Requires int64.inc, since that contains the VAL functions for int64 and qword}
{$i astrings.inc}

{$ifdef haswidechar}
{$i wstrings.inc}
{$endif haswidechar}

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

procedure int_overflow;[public,alias:'FPC_OVERFLOW'];
begin
  HandleErrorFrame(215,get_frame);
end;


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


Procedure HandleErrorAddrFrame (Errno : longint;addr,frame : longint);[public,alias:'FPC_BREAK_ERROR'];
begin
  If pointer(ErrorProc)<>Nil then
    ErrorProc(Errno,pointer(addr),pointer(frame));
  errorcode:=Errno;
  exitcode:=Errno;
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


Procedure RunError;
Begin
  RunError (0);
End;


Procedure Halt;
Begin
  Halt(0);
End;

function do_isdevice(handle:longint):boolean;forward;


Procedure dump_stack(var f : text;bp : Longint);
var
  i, prevbp : Longint;
  is_dev : boolean;
Begin
  prevbp:=bp-1;
  i:=0;
  is_dev:=do_isdevice(textrec(f).Handle);
  while bp > prevbp Do
   Begin
     Writeln(f,BackTraceStrFunc(get_caller_addr(bp)));
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


Procedure int_assert(Const Msg,FName:Shortstring;LineNo,ErrorAddr:Longint); [SaveRegisters,Public,Alias : 'FPC_ASSERT'];
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


{$ifdef IOCheckWasOn}
{$I+}
{$endif}

{$ifdef RangeCheckWasOn}
{$R+}
{$endif}

{$ifdef OverflowCheckWasOn}
{$Q+}
{$endif}

{*****************************************************************************
                        Object Pascal support
*****************************************************************************}

{$i objpas.inc}

{
  $Log$
  Revision 1.6  2000-10-13 12:04:03  peter
    * FPC_BREAK_ERROR added

  Revision 1.5  2000/08/13 17:55:14  michael
  + Added some delphi compatibility types

  Revision 1.4  2000/08/09 19:31:18  marco
   * fixes for val(int64 or qword) to ansistring

  Revision 1.3  2000/07/14 10:33:10  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:45  michael
  + removed logs

}
