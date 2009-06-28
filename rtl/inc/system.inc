{

    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2008 by the Free Pascal development team

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

{$ifdef FPC_HAS_FEATURE_EXITCODE}
  {$ifdef FPC_OBJFPC_EXTENDED_IF}
    {$if High(errorcode)<>maxExitCode}
      {$define FPC_LIMITED_EXITCODE}
    {$endif}
  {$else}
    {$define FPC_LIMITED_EXITCODE}
  {$endif FPC_OBJFPC_EXTENDED_IF}
{$endif FPC_HAS_FEATURE_EXITCODE}

Procedure HandleError (Errno : Longint); forward;
Procedure HandleErrorFrame (Errno : longint;frame : Pointer); forward;

type
  FileFunc = Procedure(var t : TextRec);


const
  STACK_MARGIN = 16384;    { Stack size margin for stack checking }
{ Random / Randomize constants }
  OldRandSeed : Cardinal = 0;

{ For Error Handling.}
  ErrorBase : Pointer = nil;

{ Used by the ansi/widestrings and maybe also other things in the future }
var
  { widechar, because also used by widestring -> pwidechar conversions }
  emptychar : widechar;public name 'FPC_EMPTYCHAR';
{$ifndef FPC_NO_GENERIC_STACK_CHECK}
  { if the OS does the stack checking, we don't need any stklen from the
    main program }
  initialstklen : SizeUint;external name '__stklen';
{$endif FPC_NO_GENERIC_STACK_CHECK}

{ checks whether the given suggested size for the stack of the current
 thread is acceptable. If this is the case, returns it unaltered.
 Otherwise it should return an acceptable value.

 Operating systems that automatically expand their stack on demand, should
 simply return a very large value.
 Operating systems which do not have a possibility to retrieve stack size
 information, should simply return the given stklen value (This is the default
 implementation).
}
{$ifdef FPC_HAS_FEATURE_STACKCHECK}
function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt; forward;
{$endif FPC_HAS_FEATURE_STACKCHECK}

{*****************************************************************************
                       OS dependent Helpers/Syscalls
*****************************************************************************}

{ for some OSes do_isdevice is defined in sysos.inc, but for others (win32)
  it isn't, and is used before the actual definition is encountered         }

{$ifdef FPC_HAS_FEATURE_FILEIO}
function do_isdevice(handle:thandle):boolean;forward;
{$endif FPC_HAS_FEATURE_FILEIO}


{$i sysos.inc}


{****************************************************************************
                    Include processor specific routines
****************************************************************************}

{$ifdef FPC_USE_LIBC}
{ Under Haiku, bcopy cause a problem when searching for include file
  in the compiler. So, we use the internal implementation for now
  under BeOS and Haiku.  }
{$ifndef BEOS}
{ prefer libc implementations over our own, as they're most likely faster }
{$i cgeneric.inc}
{ is now declared as external reference to another routine in the interface }
{$i cgenstr.inc}
{$endif}
{$endif FPC_USE_LIBC}

{$ifdef cpui386}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i i386.inc}  { Case dependent, don't change }
{$endif cpui386}

{$ifdef cpum68k}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i m68k.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpum68k}

{$ifdef cpux86_64}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i x86_64.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpux86_64}

{$ifdef cpupowerpc32}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i powerpc.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpupowerpc32}

{$ifdef cpupowerpc64}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i powerpc64.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpupowerpc64}

{$ifdef cpualpha}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i alpha.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpualpha}

{$ifdef cpuiA64}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i ia64.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpuiA64}

{$ifdef cpusparc}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i sparc.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpusparc}

{$ifdef cpuarm}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i arm.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpuarm}

{$ifdef cpuavr}
  {$ifdef SYSPROCDEFINED}
    {$Error Can't determine processor type !}
  {$endif}
  {$i avr.inc}  { Case dependent, don't change }
  {$define SYSPROCDEFINED}
{$endif cpuavr}


procedure fillchar(var x;count : SizeInt;value : boolean);
begin
  fillchar(x,count,byte(value));
end;


procedure fillchar(var x;count : SizeInt;value : char);
begin
  fillchar(x,count,byte(value));
end;


procedure FillByte (var x;count : SizeInt;value : byte );
begin
  FillChar (X,Count,VALUE);
end;


function IndexChar(Const buf;len:SizeInt;b:char):SizeInt;
begin
  IndexChar:=IndexByte(Buf,Len,byte(B));
end;


function CompareChar(Const buf1,buf2;len:SizeInt):SizeInt;
begin
  CompareChar:=CompareByte(buf1,buf2,len);
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

Function Swap (X : Word) : Word;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Swap := SwapEndian(X);
End;

Function Swap (X : Integer) : Integer;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Swap := SwapEndian(X);
End;

Function Swap (X : Longint) : Longint;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

Function Swap (X : Cardinal) : Cardinal;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Swap:=(X and $ffff) shl 16 + (X shr 16)
End;

Function Swap (X : QWord) : QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;

Function swap (X : Int64) : Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Swap:=(X and $ffffffff) shl 32 + (X shr 32);
End;

{$ifdef SUPPORT_DOUBLE}
operator := (b:real48) d:double;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
 D:=real2double(b);
end;
{$endif SUPPORT_DOUBLE}

{$ifdef SUPPORT_EXTENDED}
operator := (b:real48) e:extended;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
 e:=real2double(b);
end;
{$endif SUPPORT_EXTENDED}

{$ifndef FPUNONE}
{$ifdef FPC_USE_LIBC}
{ Include libc versions }
{$i cgenmath.inc}
{$endif FPC_USE_LIBC}
{ Include processor specific routines }
{$I math.inc}
{ Include generic version }
{$I genmath.inc}
{$endif}

{$i gencurr.inc}


function aligntoptr(p : pointer) : pointer;inline;
  begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    result:=align(p,sizeof(p));
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
    result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  end;


{****************************************************************************
                  Subroutines for String handling
****************************************************************************}

{ Needs to be before RTTI handling }

{$i sstrings.inc}

{ requires sstrings.inc for initval }
{$I int64p.inc}
{$I int64.inc}

{Requires int64.inc, since that contains the VAL functions for int64 and qword}
{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}
{$i astrings.inc}
{$endif FPC_HAS_FEATURE_ANSISTRINGS}

{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
{ this is for bootstrappung with 2.2.x }
{$ifdef VER2_2}
{$i wustring22.inc}
{$else VER2_2}
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
    {$i wstrings.inc}
  {$endif FPC_WIDESTRING_EQUAL_UNICODESTRING}
  {$i ustrings.inc}
{$endif VER2_2}
{$endif FPC_HAS_FEATURE_WIDESTRINGS}

{$i aliases.inc}

{*****************************************************************************
                        Dynamic Array support
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
{$i dynarr.inc}
{$endif FPC_HAS_FEATURE_DYNARRAYS}

{*****************************************************************************
                        Object Pascal support
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_CLASSES}
{$i objpas.inc}
{$endif FPC_HAS_FEATURE_CLASSES}

{*****************************************************************************
                            Variant support
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_VARIANTS}
{$i variant.inc}
{$endif FPC_HAS_FEATURE_VARIANTS}

{****************************************************************************
                         Run-Time Type Information (RTTI)
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_RTTI}
{$i rtti.inc}
{$endif FPC_HAS_FEATURE_VARIANTS}

{$if defined(FPC_HAS_FEATURE_RANDOM)}

{----------------------------------------------------------------------
   Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
   Pseudo-Random Number Generator.

   What is Mersenne Twister?
   Mersenne Twister(MT) is a pseudorandom number generator developped by
   Makoto Matsumoto and Takuji Nishimura (alphabetical order) during
   1996-1997. MT has the following merits:
   It is designed with consideration on the flaws of various existing
   generators.
   Far longer period and far higher order of equidistribution than any
   other implemented generators. (It is proved that the period is 2^19937-1,
   and 623-dimensional equidistribution property is assured.)
   Fast generation. (Although it depends on the system, it is reported that
   MT is sometimes faster than the standard ANSI-C library in a system
   with pipeline and cache memory.)
   Efficient use of the memory. (The implemented C-code mt19937.c
   consumes only 624 words of working area.)

   home page
     http://www.math.keio.ac.jp/~matumoto/emt.html
   original c source
     http://www.math.keio.ac.jp/~nisimura/random/int/mt19937int.c

   Coded by Takuji Nishimura, considering the suggestions by
   Topher Cooper and Marc Rieffel in July-Aug. 1997.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later
   version.
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU Library General Public License for more details.
   You should have received a copy of the GNU Library General
   Public License along with this library; if not, write to the
   Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307  USA

   Copyright (C) 1997, 1999 Makoto Matsumoto and Takuji Nishimura.
   When you use this, send an email to: matumoto@math.keio.ac.jp
   with an appropriate reference to your work.

   REFERENCE
   M. Matsumoto and T. Nishimura,
   "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
   Pseudo-Random Number Generator",
   ACM Transactions on Modeling and Computer Simulation,
   Vol. 8, No. 1, January 1998, pp 3--30.


  Translated to OP and Delphi interface added by Roman Krejci (6.12.1999)

  http://www.rksolution.cz/delphi/tips.htm

  Revised 21.6.2000: Bug in the function RandInt_MT19937 fixed

  2003/10/26: adapted to use the improved intialisation mentioned at
  <http://www.math.keio.ac.jp/~matumoto/MT2002/emt19937ar.html> and
  removed the assembler code

 ----------------------------------------------------------------------}

{$R-} {range checking off}
{$Q-} {overflow checking off}

{ Period parameter }
Const
  MT19937N=624;

Type
  tMT19937StateArray = array [0..MT19937N-1] of longint; // the array for the state vector

{ Period parameters }
const
  MT19937M=397;
  MT19937MATRIX_A  =$9908b0df;  // constant vector a
  MT19937UPPER_MASK=longint($80000000);  // most significant w-r bits
  MT19937LOWER_MASK=longint($7fffffff);  // least significant r bits

{ Tempering parameters }
  TEMPERING_MASK_B=longint($9d2c5680);
  TEMPERING_MASK_C=longint($efc60000);


VAR
  mt : tMT19937StateArray;

const
  mti: longint=MT19937N+1; // mti=MT19937N+1 means mt[] is not initialized

{ Initializing the array with a seed }
procedure sgenrand_MT19937(seed: longint);
var
  i: longint;
begin
  mt[0] := seed;
  for i := 1 to MT19937N-1 do
    begin
      mt[i] := 1812433253 * (mt[i-1] xor (mt[i-1] shr 30)) + i;
      { See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. }
      { In the previous versions, MSBs of the seed affect   }
      { only MSBs of the array mt[].                        }
      { 2002/01/09 modified by Makoto Matsumoto             }
    end;
  mti := MT19937N;
end;


function genrand_MT19937: longint;
const
  mag01 : array [0..1] of longint =(0, longint(MT19937MATRIX_A));
var
  y: longint;
  kk: longint;
begin
  if RandSeed<>OldRandSeed then
    mti:=MT19937N+1;
  if (mti >= MT19937N) { generate MT19937N longints at one time }
  then begin
     if mti = (MT19937N+1) then  // if sgenrand_MT19937() has not been called,
       begin
         sgenrand_MT19937(randseed);   // default initial seed is used
         { hack: randseed is not used more than once in this algorithm. Most }
         {  user changes are re-initialising reandseed with the value it had }
         {  at the start -> with the "not", we will detect this change.      }
         {  Detecting other changes is not useful, since the generated       }
         {  numbers will be different anyway.                                }
         randseed := not(randseed);
         oldrandseed := randseed;
       end;
     for kk:=0 to MT19937N-MT19937M-1 do begin
        y := (mt[kk] and MT19937UPPER_MASK) or (mt[kk+1] and MT19937LOWER_MASK);
        mt[kk] := mt[kk+MT19937M] xor (y shr 1) xor mag01[y and $00000001];
     end;
     for kk:= MT19937N-MT19937M to MT19937N-2 do begin
       y := (mt[kk] and MT19937UPPER_MASK) or (mt[kk+1] and MT19937LOWER_MASK);
       mt[kk] := mt[kk+(MT19937M-MT19937N)] xor (y shr 1) xor mag01[y and $00000001];
     end;
     y := (mt[MT19937N-1] and MT19937UPPER_MASK) or (mt[0] and MT19937LOWER_MASK);
     mt[MT19937N-1] := mt[MT19937M-1] xor (y shr 1) xor mag01[y and $00000001];
     mti := 0;
  end;
  y := mt[mti]; inc(mti);
  y := y xor (y shr 11);
  y := y xor (y shl 7)  and TEMPERING_MASK_B;
  y := y xor (y shl 15) and TEMPERING_MASK_C;
  y := y xor (y shr 18);
  Result := y;
end;


function random(l:longint): longint;
begin
  { otherwise we can return values = l (JM) }
  if (l < 0) then
    inc(l);
  random := longint((int64(cardinal(genrand_MT19937))*l) shr 32);
end;

function random(l:int64): int64;
begin
  { always call random, so the random generator cycles (TP-compatible) (JM) }
  random := int64((qword(cardinal(genrand_MT19937)) or ((qword(cardinal(genrand_MT19937)) shl 32))) and $7fffffffffffffff);
  if (l<>0) then
    random := random mod l
  else
    random := 0;
end;

{$ifndef FPUNONE}
function random: extended;
begin
  random := cardinal(genrand_MT19937) * (1.0/(int64(1) shl 32));
end;
{$endif}
{$endif FPC_HAS_FEATURE_RANDOM}


{****************************************************************************
                            Memory Management
****************************************************************************}

Function Ptr(sel,off : Longint) : farpointer;{$ifdef SYSTEMINLINE}inline;{$endif}
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

{$if defined(FPC_HAS_FEATURE_FILEIO) and defined(FPC_HAS_FEATURE_ANSISTRINGS)}
Procedure getdir(drivenr:byte;Var dir:ansistring);
{ this is needed to also allow ansistrings, the shortstring version is
  OS dependent }
var
  s : shortstring;
begin
  getdir(drivenr,s);
  dir:=s;
end;
{$endif}

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

procedure fpc_rangeerror;[public,alias:'FPC_RANGEERROR']; compilerproc;
begin
  HandleErrorFrame(201,get_frame);
end;


procedure fpc_divbyzero;[public,alias:'FPC_DIVBYZERO']; compilerproc;
begin
  HandleErrorFrame(200,get_frame);
end;


procedure fpc_overflow;[public,alias:'FPC_OVERFLOW']; compilerproc;
begin
  HandleErrorFrame(215,get_frame);
end;


procedure fpc_threaderror; [public,alias:'FPC_THREADERROR'];
begin
  HandleErrorFrame(6,get_frame);
end;


procedure fpc_iocheck;[public,alias:'FPC_IOCHECK']; compilerproc;
var
  l : longint;
  HInoutRes : PWord;
begin
  HInOutRes:=@InoutRes;
  if HInOutRes^<>0 then
   begin
     l:=HInOutRes^;
     HInOutRes^:=0;
     HandleErrorFrame(l,get_frame);
   end;
end;


Function IOResult:Word;
var
  HInoutRes : PWord;
Begin
  HInoutRes:=@InoutRes;
  IOResult:=HInOutRes^;
  HInOutRes^:=0;
End;


Function GetThreadID:TThreadID;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
(* ThreadID is stored in a threadvar and made available in interface *)
(* to allow setup of this value during thread initialization.        *)
  GetThreadID := ThreadID;
end;


function fpc_safecallcheck(res : hresult) : hresult;[public,alias:'FPC_SAFECALLCHECK']; compilerproc; {$ifdef CPU86} register; {$endif}
begin
  if res<0 then
    begin
      if assigned(SafeCallErrorProc) then
        SafeCallErrorProc(res,get_frame);
      HandleErrorFrame(229,get_frame);
    end;
  result:=res;
end;


{*****************************************************************************
                         Stack check code
*****************************************************************************}

{ be compatible with old code }
{$ifdef FPC_NO_GENERIC_STACK_CHECK}
{$define NO_GENERIC_STACK_CHECK}
{$endif FPC_NO_GENERIC_STACK_CHECK}

{$IFNDEF NO_GENERIC_STACK_CHECK}

{$IFOPT S+}
{$DEFINE STACKCHECK}
{$ENDIF}
{$S-}
procedure fpc_stackcheck(stack_size:SizeUInt);[public,alias:'FPC_STACKCHECK'];
var
  c : Pointer;
begin
  { Avoid recursive calls when called from the exit routines }
  if StackError then
   exit;
  { don't use sack_size, since the stack pointer has already been
    decreased when this routine is called
  }
  c := Sptr - STACK_MARGIN;
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
  TInitFinalTable = record
    TableCount,
    InitCount  : longint;
    Procs      : array[1..maxunits] of TInitFinalRec;
  end;
  PInitFinalTable = ^TInitFinalTable;


{$ifndef FPC_HAS_INDIRECT_MAIN_INFORMATION}
var
  InitFinalTable : TInitFinalTable;external name 'INITFINAL';
{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}


procedure fpc_InitializeUnits;[public,alias:'FPC_INITIALIZEUNITS']; compilerproc;
var
  i : longint;
begin
  { call cpu/fpu initialisation routine }
  fpc_cpuinit;
  with {$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}PInitFinalTable(EntryInformation.{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}
    InitFinalTable
    {$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION})^{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION} do
   begin
     for i:=1 to TableCount do
      begin
        if assigned(Procs[i].InitProc) then
         Procs[i].InitProc();
        InitCount:=i;
      end;
   end;
  if assigned(InitProc) then
    TProcedure(InitProc)();
end;


procedure internal_initializeunits; external name 'FPC_INITIALIZEUNITS';

procedure fpc_LibInitializeUnits;[public,alias:'FPC_LIBINITIALIZEUNITS'];
begin
  IsLibrary:=true;
  internal_initializeunits;
end;


procedure FinalizeUnits;[public,alias:'FPC_FINALIZEUNITS'];
begin
  with {$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}PInitFinalTable(EntryInformation.{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}
    InitFinalTable
    {$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION})^{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION} do
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
{$ifdef FPC_HAS_FEATURE_HEAP}
{$ifndef HAS_MEMORYMANAGER}
//not needed if independant memory manager
Procedure FinalizeHeap;forward;
{$endif HAS_MEMORYMANAGER}
{$endif FPC_HAS_FEATURE_HEAP}


Procedure InternalExit;
var
  current_exit : Procedure;
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  pstdout : ^Text;
{$endif}
{$if defined(MSWINDOWS) or defined(OS2)}
  i : longint;
{$endif}
Begin
{$ifdef SYSTEMDEBUG}
  writeln('InternalExit');
{$endif SYSTEMDEBUG}
  while exitProc<>nil Do
   Begin
     InOutRes:=0;
     current_exit:=tProcedure(exitProc);
     exitProc:=nil;
     current_exit();
   End;
  { Finalize units }
  FinalizeUnits;

{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  { Show runtime error and exit }
  pstdout:=@stdout;
  If erroraddr<>nil Then
   Begin
     Writeln(pstdout^,'Runtime error ',Errorcode,' at $',hexstr(erroraddr));
     { to get a nice symify }
     Writeln(pstdout^,BackTraceStrFunc(Erroraddr));
     dump_stack(pstdout^,ErrorBase);
     Writeln(pstdout^,'');
   End;

  { Make sure that all output is written to the redirected file }
  if Textrec(Output).Mode=fmOutput then
    Flush(Output);
  if Textrec(ErrOutput).Mode=fmOutput then
    Flush(ErrOutput);
  if Textrec(pstdout^).Mode=fmOutput then
    Flush(pstdout^);
  if Textrec(StdErr).Mode=fmOutput then
    Flush(StdErr);
{$endif FPC_HAS_FEATURE_CONSOLEIO}

{$if defined(MSWINDOWS) or defined(OS2)}
  { finally release the heap if possible, especially
    important for DLLs }
  for i:=0 to argc do
    sysfreemem(argv[i]);
  sysfreemem(argv);
{$endif}
{$ifdef LINUX}
  {sysfreemem already checks for nil}
  sysfreemem(calculated_cmdline);
{$endif}
{$ifdef BSD}
  sysfreemem(cmdline);
{$endif}

{$ifdef FPC_HAS_FEATURE_HEAP}
{$ifndef HAS_MEMORYMANAGER}
  FinalizeHeap;
{$endif HAS_MEMORYMANAGER}
{$endif FPC_HAS_FEATURE_HEAP}
End;


Procedure do_exit;[Public,Alias:'FPC_DO_EXIT'];
begin
  InternalExit;
  System_exit;
end;


Procedure lib_exit;[Public,Alias:'FPC_LIB_EXIT'];
begin
  InternalExit;
end;


Procedure Halt(ErrNum: Byte);
Begin
  ExitCode:=Errnum;
  Do_Exit;
end;


function SysBackTraceStr (Addr: Pointer): ShortString;
begin
  SysBackTraceStr:='  $'+hexstr(addr);
end;


Procedure HandleErrorAddrFrame (Errno : longint;addr,frame : Pointer);[public,alias:'FPC_BREAK_ERROR']; {$ifdef CPU86} register; {$endif}
begin
  If pointer(ErrorProc)<>Nil then
    ErrorProc(Errno,addr,frame);
  errorcode:=word(Errno);
  erroraddr:=addr;
  errorbase:=frame;
{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
  if ExceptAddrStack <> nil then
    raise TObject(nil) at addr,frame;
{$endif FPC_HAS_FEATURE_EXCEPTIONS}

{$ifdef FPC_HAS_FEATURE_EXITCODE}
{$ifdef FPC_LIMITED_EXITCODE}
  if errorcode > maxExitCode then
    halt(255)
  else
{$endif FPC_LIMITED_EXITCODE}
    halt(errorcode);
{$else FPC_HAS_FEATURE_EXITCODE}
  halt;
{$endif FPC_HAS_FEATURE_EXITCODE}
end;

Procedure HandleErrorFrame (Errno : longint;frame : Pointer);
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


procedure RunError(w : word);[alias: 'FPC_RUNERROR'];
begin
  errorcode:=w;
  erroraddr:=get_caller_addr(get_frame);
  errorbase:=get_caller_frame(get_frame);
{$ifdef FPC_HAS_FEATURE_EXITCODE}
{$ifdef FPC_LIMITED_EXITCODE}
  if errorcode > maxExitCode then
    halt(255)
  else
{$endif FPC_LIMITED_EXITCODE}
    halt(errorcode);
{$else FPC_HAS_FEATURE_EXITCODE}
  halt;
{$endif FPC_HAS_FEATURE_EXITCODE}
end;


Procedure RunError;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  RunError (0);
End;


Procedure Halt;{$ifdef SYSTEMINLINE}inline;{$endif}
Begin
  Halt(0);
End;

Procedure Error(RunTimeError : TRunTimeError);

begin
  RunError(RuntimeErrorExitCodes[RunTimeError]);
end;


{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
Procedure dump_stack(var f : text;bp : Pointer);
var
  i : Longint;
  prevbp : Pointer;
  is_dev : boolean;
  caller_frame,
  caller_addr : Pointer;
Begin
{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
  try
{$endif FPC_HAS_FEATURE_EXCEPTIONS}
    prevbp:=bp-1;
    i:=0;
    is_dev:=do_isdevice(textrec(f).Handle);
    while bp > prevbp Do
     Begin
       caller_addr := get_caller_addr(bp);
       caller_frame := get_caller_frame(bp);
       if (caller_addr=nil) then
         break;
       Writeln(f,BackTraceStrFunc(caller_addr));
       if (caller_frame=nil) then
         break;
       Inc(i);
       If ((i>max_frame_dump) and is_dev) or (i>256) Then
         break;
       prevbp:=bp;
       bp:=caller_frame;
     End;
{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
   except
     { prevent endless dump if an exception occured }
   end;
{$endif FPC_HAS_FEATURE_EXCEPTIONS}
End;


{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
procedure DumpExceptionBackTrace(var f:text);
var
  FrameNumber,
  FrameCount   : longint;
  Frames       : PPointer;
begin
  if RaiseList=nil then
    exit;
  WriteLn(f,BackTraceStrFunc(RaiseList^.Addr));
  FrameCount:=RaiseList^.Framecount;
  Frames:=RaiseList^.Frames;
  for FrameNumber := 0 to FrameCount-1 do
    WriteLn(f,BackTraceStrFunc(Frames[FrameNumber]));
end;
{$endif FPC_HAS_FEATURE_EXCEPTIONS}

{$endif FPC_HAS_FEATURE_CONSOLEIO}


{$ifdef FPC_HAS_FEATURE_HEAP}
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
{$endif FPC_HAS_FEATURE_HEAP}


{$ifdef FPC_HAS_FEATURE_HEAP}
function ArrayStringToPPchar(const S:Array of AnsiString;reserveentries:Longint):ppchar; // const ?
// Extra allocate reserveentries pchar's at the beginning (default param=0 after 1.0.x ?)
// Note: for internal use by skilled programmers only
// if "s" goes out of scope in the parent procedure, the pointer is dangling.

var p   : ppchar;
    i   : LongInt;
begin
  if High(s)<Low(s) Then Exit(NIL);
  Getmem(p,sizeof(pchar)*(high(s)-low(s)+ReserveEntries+2));  // one more for NIL, one more
                                              // for cmd
  if p=nil then
    begin
      {$ifdef xunix}
      fpseterrno(ESysEnomem);
      {$endif}
      exit(NIL);
    end;
  for i:=low(s) to high(s) do
     p[i+Reserveentries]:=pchar(s[i]);
  p[high(s)+1+Reserveentries]:=nil;
  ArrayStringToPPchar:=p;
end;


Function StringToPPChar(Var S:AnsiString;ReserveEntries:integer):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially usefull for creating an ArgV for Exec-calls
}

begin
  StringToPPChar:=StringToPPChar(PChar(S),ReserveEntries);
end;


Function StringToPPChar(S: PChar;ReserveEntries:integer):ppchar;

var
  i,nr  : longint;
  Buf : ^char;
  p   : ppchar;

begin
  buf:=s;
  nr:=1;
  while (buf^<>#0) do                   // count nr of args
   begin
     while (buf^ in [' ',#9,#10]) do    // Kill separators.
      inc(buf);
     inc(nr);
     if buf^='"' Then                   // quotes argument?
      begin
        inc(buf);
        while not (buf^ in [#0,'"']) do // then end of argument is end of string or next quote
         inc(buf);
        if buf^='"' then                // skip closing quote.
          inc(buf);
      end
     else
       begin                            // else std
         while not (buf^ in [' ',#0,#9,#10]) do
           inc(buf);
       end;
   end;
  getmem(p,(ReserveEntries+nr)*sizeof(pchar));
  StringToPPChar:=p;
  if p=nil then
   begin
     {$ifdef xunix}
     fpseterrno(ESysEnomem);
     {$endif}
     exit;
   end;
  for i:=1 to ReserveEntries do inc(p); // skip empty slots
  buf:=s;
  while (buf^<>#0) do
   begin
     while (buf^ in [' ',#9,#10]) do    // Kill separators.
      begin
       buf^:=#0;
       inc(buf);
      end;
     if buf^='"' Then                   // quotes argument?
      begin
        inc(buf);
        p^:=buf;
        inc(p);
        p^:=nil;
        while not (buf^ in [#0,'"']) do // then end of argument is end of string or next quote
         inc(buf);
        if buf^='"' then                // skip closing quote.
          begin
            buf^:=#0;
            inc(buf);
          end;
      end
     else
       begin
        p^:=buf;
        inc(p);
        p^:=nil;
         while not (buf^ in [' ',#0,#9,#10]) do
           inc(buf);
       end;
   end;
end;
{$endif FPC_HAS_FEATURE_HEAP}


{*****************************************************************************
                          Abstract/Assert support.
*****************************************************************************}

procedure fpc_AbstractErrorIntern;compilerproc;[public,alias : 'FPC_ABSTRACTERROR'];
begin
  If pointer(AbstractErrorProc)<>nil then
    AbstractErrorProc();
  HandleErrorFrame(211,get_frame);
end;


Procedure fpc_assert(Const Msg,FName:Shortstring;LineNo:Longint;ErrorAddr:Pointer); [Public,Alias : 'FPC_ASSERT']; compilerproc;
begin
  if pointer(AssertErrorProc)<>nil then
    AssertErrorProc(Msg,FName,LineNo,ErrorAddr)
  else
    HandleErrorFrame(227,get_frame);
end;


Procedure SysAssert(Const Msg,FName:Shortstring;LineNo:Longint;ErrorAddr:Pointer);
begin
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  If msg='' then
    write(stderr,'Assertion failed')
  else
    write(stderr,msg);
  Writeln(stderr,' (',FName,', line ',LineNo,').');
  Writeln(stderr,'');
{$ifdef FPC_HAS_FEATURE_EXITCODE}
  Halt(227);
{$else FPC_HAS_FEATURE_EXITCODE}
  halt;
{$endif FPC_HAS_FEATURE_EXITCODE}
{$endif FPC_HAS_FEATURE_CONSOLEIO}
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
                               Heap
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_HEAP}
{$i sysheap.inc}

{$i heap.inc}
{$endif FPC_HAS_FEATURE_HEAP}

{*****************************************************************************
                          Thread support
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_THREADING}
{ Generic threadmanager }
{$i thread.inc}

{ Generic threadvar support }
{$i threadvr.inc}

{$ifdef DISABLE_NO_THREAD_MANAGER}
{ OS Dependent implementation }
{$i systhrd.inc}
{$endif DISABLE_NO_THREAD_MANAGER}
{$endif FPC_HAS_FEATURE_THREADING}


{*****************************************************************************
                            File Handling
*****************************************************************************}


{$ifdef FPC_HAS_FEATURE_FILEIO}
{ Allow slash and backslash as separators }
procedure DoDirSeparators(p:Pchar);
var
  i : longint;
begin
  for i:=0 to strlen(p) do
    if p[i] in AllowDirectorySeparators then
      p[i]:=DirectorySeparator;
end;

procedure DoDirSeparators(var p:shortstring);
var
  i : longint;
begin
  for i:=1 to length(p) do
    if p[i] in AllowDirectorySeparators then
      p[i]:=DirectorySeparator;
end;
{$endif FPC_HAS_FEATURE_FILEIO}

{ OS dependent low level file functions }
{$ifdef FPC_HAS_FEATURE_FILEIO}
{$i sysfile.inc}
{$endif FPC_HAS_FEATURE_FILEIO}

{ Text file }
{$ifdef FPC_HAS_FEATURE_TEXTIO}
{$i text.inc}
{$endif FPC_HAS_FEATURE_TEXTIO}

{$ifdef FPC_HAS_FEATURE_FILEIO}
{ Untyped file }
{$i file.inc}

{ Typed file }
{$i typefile.inc}
{$endif FPC_HAS_FEATURE_FILEIO}


{*****************************************************************************
                            Directory Handling
*****************************************************************************}

{ OS dependent dir functions }
{$i sysdir.inc}

{*****************************************************************************
                            Resources support
*****************************************************************************}

{$i sysres.inc}

const
  CtrlBreakHandler: TCtrlBreakHandler = nil;

{$IFNDEF FPC_HAS_SETCTRLBREAKHANDLER}
(* It is possible to provide platform specific implementation performing   *)
(* special initialization; default implementation just sets the procedural *)
(* variable to make it available for use from the exception handler.       *)
function SysSetCtrlBreakHandler (Handler: TCtrlBreakHandler): TCtrlBreakHandler;
begin
  (* Return either nil or previous handler *)
  SysSetCtrlBreakHandler := CtrlBreakHandler;
  CtrlBreakHandler := Handler;
end;
{$ENDIF FPC_HAS_SETCTRLBREAKHANDLER}
