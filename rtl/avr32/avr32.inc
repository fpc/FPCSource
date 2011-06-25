{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    AVR32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode gas}

{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_FPC_MOVE}
{$endif FPC_SYSTEM_HAS_MOVE}

{$if not(defined(FPUSOFT)) and not(defined(FPULIBGCC))}

{$define FPC_SYSTEM_HAS_SYSINITFPU}
{$if not defined(FPUAVR32)}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
end;
{$else}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { Enable FPU exceptions, but disable INEXACT, UNDERFLOW, DENORMAL }
  asm
    
  end;  
end;
{$endif}
{$endif}

procedure fpc_cpuinit;
begin
  { don't let libraries influence the FPU cw set by the host program }
  if not IsLibrary then
    SysInitFPU;
end;

{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
asm
  mov    r12,sp
end;
{$ENDIF not INTERNAL_BACKTRACE}

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;nostackframe;
label Lg_a_null;
asm
  tst r12,r12
  breq Lg_a_null
  ld.w r12,r12[0]
Lg_a_null:
end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;nostackframe;
label Lgnf_null;
asm
  tst r12,r12
  breq Lgnf_null
  ld.w r12,r12[-4]
Lgnf_null:
end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
asm
  mov    r12,sp
end;


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);assembler;nostackframe;
label Lloop;
asm
Lloop:
  st.b r12++,value
  sub count,1
  brne Lloop
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}

function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
label Loop;
asm
Loop:
  ssrf 5
  ld.w r10,target
  sub r10,1
  stcond target,r10
  brne Loop
  
  mov r12,r10
end;


function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
label Loop;
asm
Loop:
  ssrf 5
  ld.w r10,target
  sub r10,-1
  stcond target,r10
  brne Loop
  
  mov r12,r10
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
label Loop;
asm
Loop:
  ssrf 5
  ld.w r10,target
  stcond target,source
  brne Loop
  
  mov r12,r10
end;

function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
label Loop;
asm
Loop:
  ssrf 5
  ld.w r10,target
  add r1,r10,source
  stcond target,r1
  brne Loop
  
  mov r12,r10
end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler; nostackframe;
label Loop,doExit;
asm
Loop:
  ssrf 5
  ld.w r10,target
  cp.w r10,comperand
  brne doExit
  stcond target,newvalue
  brne Loop
doExit:
  mov r12,r10
end;

{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function declocked(var l: longint) : boolean; inline;
begin
  Result:=InterLockedDecrement(l) = 0;
end;

{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l: longint); inline;
begin
  InterLockedIncrement(l);
end;

procedure fpc_cpucodeinit;
begin

end;

{include hand-optimized assembler division code}
{ $i divide.inc}

