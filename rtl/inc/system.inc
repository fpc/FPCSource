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
  stacklength : longint;external name '__stklen';


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

{$IFDEF I386}
  {$IFNDEF ENDIAN_LITTLE}
    {$DEFINE ENDIAN_LITTLE}
  {$ENDIF}
  {$IFDEF M68K}
    {$Error Can't determine processor type !}
  {$ENDIF}
  {$I i386.inc}  { Case dependent, don't change }
{$ELSE}
  {$IFDEF M68K}
  {$IFNDEF ENDIAN_BIG}
    {$DEFINE ENDIAN_BIG}
  {$ENDIF}
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
{ Include generic pascal routines for sets if the processor }
{ specific routines are not available.                      }
{$I genset.inc}


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

operator := (b:real48) d:double;

begin
 D:=real2double(b);
end;

operator := (b:real48) e:extended;

begin
 e:=real2double(b);
end;

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

Function Ptr(sel,off : Longint) : farpointer;[internconst:in_const_ptr];
Begin
  ptr:=farpointer((sel shl 4)+off);
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
                         Stack check code
*****************************************************************************}
{$IFOPT S+}
{$DEFINE STACKCHECK}
{$ENDIF}
{$S-}
procedure fpc_stackcheck(stack_size:longint);[public,alias:'FPC_STACKCHECK'];
var
 c: cardinal;
begin
 c := cardinal(Sptr) - cardinal(stack_size) - STACK_MARGIN;
 if (c <= cardinal(StackBottom)) then
     HandleError(202);
end;
{$IFDEF STACKCHECK}
{$S+}
{$ENDIF}
{$UNDEF STACKCHECK}


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
  Revision 1.25  2002-04-12 17:37:36  carl
  + generic stack checking

  Revision 1.24  2001/12/13 20:23:19  michael
  + Added double2real function from main branch

  Revision 1.23  2001/11/19 02:40:24  carl
  + don't print stack information if previous frame = 0

  Revision 1.22  2001/08/19 21:02:01  florian
    * fixed and added a lot of stuff to get the Jedi DX( headers
      compiled

  Revision 1.21  2001/08/01 15:00:10  jonas
    + "compproc" helpers
    * renamed several helpers so that their name is the same as their
      "public alias", which should facilitate the conversion of processor
      specific code in the code generator to processor independent code
    * some small fixes to the val_ansistring and val_widestring helpers
      (always immediately exit if the source string is longer than 255
       chars)
    * fixed fpc_dynarray_high and fpc_dynarray_length if the dynarray is
      still nil (used to crash, now return resp -1 and 0)

  Revision 1.20  2001/07/30 21:38:55  peter
    * m68k updates merged

  Revision 1.19  2001/07/29 14:05:55  peter
    * include lowmath for m68k (merged)

  Revision 1.18  2001/07/29 13:49:15  peter
    * m68k updates merged

  Revision 1.17  2001/07/09 21:15:41  peter
    * Length made internal
    * Add array support for Length

  Revision 1.16  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.15  2001/06/13 18:32:05  peter
    * big endian updates (merged)

  Revision 1.14  2001/06/03 15:15:58  peter
    * lib_exit added

  Revision 1.13  2001/05/09 19:57:07  peter
  *** empty log message ***

  Revision 1.12  2001/04/13 18:06:28  peter
    * removed rtllite define

  Revision 1.11  2000/12/16 15:56:19  jonas
    - removed all ifdef cardinalmulfix code

  Revision 1.10  2000/11/13 14:47:46  jonas
    * support for range checking when converting from 64bit to something
      smaller (32bit, 16bit, 8bit)
    * fixed range checking between longint/cardinal and for array indexing
      with cardinal (values > $7fffffff were considered negative)

  Revision 1.9  2000/11/11 16:12:01  peter
    * ptr returns farpointer

  Revision 1.8  2000/11/06 21:35:59  peter
    * removed some warnings

  Revision 1.7  2000/11/04 17:52:46  florian
    * fixed linker errors

  Revision 1.6  2000/10/13 12:04:03  peter
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
