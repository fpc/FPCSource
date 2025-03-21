{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    Xtensa

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  softfloat_exception_flags:=[];
end;

{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  softfloat_exception_mask:=[float_flag_underflow,float_flag_inexact,float_flag_denormal];
  softfloat_exception_flags:=[];
end;

{$ifdef fpc_abi_windowed}
procedure forceSpilledRegs; assembler; public name 'forcespilledregs';
  asm
    movi a2, 0
    syscall
  end;

procedure fixCodeAddress(var addr: pointer);
  begin
    // Check if valid code address
    if ptruint(addr) and $C0000000 >= $40000000 then
      begin
        // Replace windowed call prefix
        addr:=codepointer((ptruint(addr)and$00FFFFFF) or $40000000);
        // Rewind to call instruction address
        dec(addr,3);
      end
    else
      addr:=nil;
  end;
{$endif fpc_abi_windowed}

{$IFNDEF INTERNAL_BACKTRACE}
  {$define FPC_SYSTEM_HAS_GET_FRAME}
  function get_frame:pointer;assembler;
    asm
      {$ifdef fpc_abi_windowed}
        // Force registers to spill onto stack
        call8 forcespilledregs
        // now get frame pointer of caller
        addi a2, a1, -12
        l32i a2, a2, 0
      {$else}
        mov a2, a1
      {$endif}
    end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;
  begin
    {$ifdef fpc_abi_windowed}
      forceSpilledRegs;
      if (ptruint(framebp)>$3ff00000)and(ptruint(framebp)<$40000000) then
        begin
          get_caller_addr:=pointer((framebp-16)^);
          fixCodeAddress(get_caller_addr);
        end
      else
        get_caller_addr:=nil;
    {$else}
      get_caller_addr:=nil;
    {$endif}
  end;

{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;
  begin
    {$ifdef fpc_abi_windowed}
      if (ptruint(framebp)>$3ff00000)and(ptruint(framebp)<$40000000) then
        begin
          forceSpilledRegs;
          get_caller_frame:=pointer((framebp-12)^);
        end
      else
        get_caller_frame:=nil;
    {$else}
      get_caller_frame:=nil;
    {$endif}
  end;


{$ifdef fpc_abi_windowed}
  {$define FPC_SYSTEM_HAS_GET_CALLER_STACKINFO}
  procedure get_caller_stackinfo(var framebp : pointer; var addr : codepointer);
    begin
      if (ptruint(framebp)>$3ff00000)and(ptruint(framebp)<$40000000) then
        begin
          forceSpilledRegs;
          addr:=codepointer((framebp-16)^);
          framebp := pointer((framebp-12)^);
          fixCodeAddress(addr);
        end
      else
        begin
          addr:=nil;
          framebp:=nil;
        end;
    end;
{$endif fpc_abi_windowed}


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;
  asm
    mov a2,a1
  end;


{$ifdef VER3_2}
function InterLockedDecrement (var Target: longint) : longint;
  begin
    Result:=Target-1;
    Target:=Result;
  end;


function InterLockedIncrement (var Target: longint) : longint;
  begin
    Result:=Target+1;
    Target:=Result;
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    Target:=Source;
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    Target:=Result+Source;
  end;


function InterLockedDecrement (var Target: smallint) : smallint;
  begin
    Result:=Target-1;
    Target:=Result;
  end;


function InterLockedIncrement (var Target: smallint) : smallint;
  begin
    Result:=Target+1;
    Target:=Result;
  end;


function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
  begin
    Result:=Target;
    Target:=Source;
  end;


function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
  begin
    Result:=Target;
    Target:=Result+Source;
  end;
{$endif VER3_2}


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue : longint; Comparand: longint): longint; [public,alias:'FPC_ATOMIC_CMP_XCHG_32'];
{$endif VER3_2}
  begin
    Result:=Target;
    if Result={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;
  end;


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_16}
function fpc_atomic_cmp_xchg_16 (var Target: smallint; NewValue : smallint; Comparand: smallint): smallint;
{$endif VER3_2}
  begin
    Result:=Target;
    if Result={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;
  end;


