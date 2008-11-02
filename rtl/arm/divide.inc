{
 Divide/modulo for Acorn RISC Machine

 ... taken from a GP2X Ogg Tremor port by Dzz and converted to
 Pascal.

 Copyright (c) 2007, Daniel Mantione
 Copyright (c) 2006, Dzz
 Copyright (c) 2002, Xiph.org Foundation

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 - Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

- Neither the name of the Xiph.org Foundation nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION
  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

{$ifndef FPC_SYSTEM_HAS_DIV_DWORD}
{$define FPC_SYSTEM_HAS_DIV_DWORD}
function fpc_div_dword(n,z:dword):dword;[public,alias: 'FPC_DIV_DWORD'];assembler;nostackframe;

asm
  mov r3, #0
  rsbs r2, r0, r1, LSR#3
  bcc .Ldiv_3bits
  rsbs r2, r0, r1, LSR#8
  bcc .Ldiv_8bits
  mov r0, r0, LSL#8
  orr r3, r3, #0xFF000000
  rsbs r2, r0, r1, LSR#4
  bcc .Ldiv_4bits
  rsbs r2, r0, r1, LSR#8
  bcc .Ldiv_8bits
  mov r0, r0, LSL#8
  orr r3, r3, #0x00FF0000
  rsbs r2, r0, r1, LSR#8
  movcs r0, r0, LSL#8
  orrcs r3, r3, #0x0000FF00
  rsbs r2, r0, r1, LSR#4
  bcc .Ldiv_4bits
  rsbs r2, r0, #0
  bcs .Ldiv_by_0
.Ldiv_loop:
  movcs r0, r0, LSR#8
.Ldiv_8bits:
  rsbs r2, r0, r1, LSR#7
  subcs r1, r1, r0, LSL#7
  adc r3, r3, r3
  rsbs r2, r0, r1, LSR#6
  subcs r1, r1, r0, LSL#6
  adc r3, r3, r3
  rsbs r2, r0, r1, LSR#5
  subcs r1, r1, r0, LSL#5
  adc r3, r3, r3
  rsbs r2, r0, r1, LSR#4
  subcs r1, r1, r0, LSL#4
  adc r3, r3, r3
.Ldiv_4bits:
  rsbs r2, r0, r1, LSR#3
  subcs r1, r1, r0, LSL#3
  adc r3, r3, r3
.Ldiv_3bits:
  rsbs r2, r0, r1, LSR#2
  subcs r1, r1, r0, LSL#2
  adc r3, r3, r3
  rsbs r2, r0, r1, LSR#1
  subcs r1, r1, r0, LSL#1
  adc r3, r3, r3
  rsbs r2, r0, r1
  subcs r1, r1, r0
  adcs r3, r3, r3
.Ldiv_next:
  bcs .Ldiv_loop
  mov r0, r3
  mov pc, lr
.Ldiv_by_0:
  mov r0, #200
  mov r1, r11
  bl handleerrorframe
  mov pc, lr
end;

{It is a compilerproc (systemh.inc), make an alias for internal use.}
function fpc_div_dword(n,z:dword):dword;external name 'FPC_DIV_DWORD';
{$endif}

{$ifndef FPC_SYSTEM_HAS_DIV_LONGINT}
{$define FPC_SYSTEM_HAS_DIV_LONGINT}
function fpc_div_longint(n,z:longint):longint;[public,alias: 'FPC_DIV_LONGINT'];assembler;nostackframe;

asm
  stmfd sp!, {lr}
  ands r12, r0, #1<<31       (* r12:=r0 and $80000000 *)
  rsbmi r0, r0, #0           (* if signed(r0) then r0:=0-r0 *)
  eors r12, r12, r1, ASR#32  (* r12:=r12 xor (r1 asr 32) *)
  rsbcs r1, r1, #0           (* if signed(r12) then r1:=0-r1 *)
  bl fpc_div_dword
  movs r12, r12, LSL#1       (* carry:=sign(r12) *)
  rsbcs r0, r0, #0
  rsbmi r1, r1, #0
  ldmfd sp!, {pc}
end;

{It is a compilerproc (systemh.inc), make an alias for internal use.}
function fpc_div_longint(n,z:longint):longint;external name 'FPC_DIV_LONGINT';
{$endif}

{$ifndef FPC_SYSTEM_HAS_MOD_DWORD}
{$define FPC_SYSTEM_HAS_MOD_DWORD}
function fpc_mod_dword(n,z:dword):dword;[public,alias: 'FPC_MOD_DWORD'];assembler;nostackframe;

asm
  stmfd sp!, {lr}
  bl fpc_div_dword
  mov r0, r1
  ldmfd sp!, {pc}
end;

{It is a compilerproc (systemh.inc), make an alias for internal use.}
function fpc_mod_dword(n,z:dword):dword;external name 'FPC_MOD_DWORD';
{$endif}

{$ifndef FPC_SYSTEM_HAS_MOD_LONGINT}
{$define FPC_SYSTEM_HAS_MOD_LONGINT}
function fpc_mod_longint(n,z:longint):longint;[public,alias: 'FPC_MOD_LONGINT'];assembler;nostackframe;

asm
  stmfd sp!, {lr}
  bl fpc_div_longint
  mov r0, r1
  ldmfd sp!, {pc}
end;

{It is a compilerproc (systemh.inc), make an alias for internal use.}
function fpc_mod_longint(n,z:longint):longint;external name 'FPC_MOD_LONGINT';
{$endif}
