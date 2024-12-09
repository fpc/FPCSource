{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015-2016 by Karoly Balogh,
    member of the Free Pascal development team.

    Amiga specific m68k functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ The Amiga hardware doesn't support the m68k CPU's atomic operations
  like TAS, CAS, CAS2 and so on. Therefore we must "emulate" them from
  software. The easiest way is the Forbid()/Permit() OS call pair around
  the ops themselves. It of course won't be hardware-atomic, but should
  be safe for multithreading. (KB) }

{$DEFINE FPC_SYSTEM_HAS_INTERLOCKEDFUNCS}

{$ifdef VER3_2}
function InterLockedDecrement (var Target: longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_DEC_32}
function fpc_atomic_dec_32 (var Target: longint) : longint;
{$endif VER3_2}
  begin
    Forbid;
    Dec(Target);
    Result := Target;
    Permit;
  end;


{$ifdef VER3_2}
function InterLockedIncrement (var Target: longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_INC_32}
function fpc_atomic_inc_32 (var Target: longint) : longint;
{$endif VER3_2}
  begin
    Forbid;
    Inc(Target);
    Result := Target;
    Permit;
  end;


{$ifdef VER3_2}
function InterLockedExchange (var Target: longint;Source : longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_32}
function fpc_atomic_xchg_32 (var Target: longint;Source : longint) : longint;
{$endif VER3_2}
  begin
    Forbid;
    Result := Target;
    Target := Source;
    Permit;
  end;


{$ifdef VER3_2}
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_ADD_32}
function fpc_atomic_add_32 (var Target: longint;Value : longint) : longint;
{$endif VER3_2}
  begin
    Forbid;
    Result := Target;
    Target := Target + {$ifdef VER3_2}Source{$else}Value{$endif};
    Permit;
  end;


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue: longint; Comparand: longint) : longint; [public;alias:'FPC_ATOMIC_CMP_XCHG_32'];
{$endif VER3_2}
  begin
    Forbid;
    Result := Target;
    if Target = {$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target := NewValue;
    Permit;
  end;


{ AmigaOS tells us what CPU we run on, so just use that }
{$DEFINE FPC_SYSTEM_HAS_TEST68K}
procedure Test68k(var CPU: byte; var FPU: byte);
  var
    flags: DWord;
  begin
    flags:=PExecBase(AOS_ExecBase)^.AttnFlags;

    CPU:=0;
    if (flags and AFF_68010) > 0 then CPU:=1;
    if (flags and AFF_68020) > 0 then CPU:=2;
    if (flags and AFF_68030) > 0 then CPU:=3;
    if (flags and AFF_68040) > 0 then CPU:=4;
    if (flags and AFF_68060) > 0 then CPU:=6;

    FPU:=0;
    if (flags and AFF_68881) > 0 then FPU:=1;
    if (flags and AFF_68882) > 0 then FPU:=2;
    if (flags and AFF_FPU40) > 0 then FPU:=CPU; // 040 or 060 with FPU
  end;
