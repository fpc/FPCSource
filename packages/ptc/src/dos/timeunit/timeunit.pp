{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$MODE objfpc}
{$ASMMODE intel}
{$GOTO on}

unit timeunit;

interface

type
  TGetClockTics = function: QWord;

var
  TimerResolution: Double;
  CPS: Double;
  GetClockTics: TGetClockTics;

implementation

var
  UseRDTSC: Boolean;
  Clk1Lo, Clk1Hi, Clk2Lo, Clk2Hi: DWord;
  Clk1, Clk2: QWord;
  ClkDelta: QWord;
  CpuFlags: DWord;

function GetClockTics_RDTSC: QWord; Assembler;

Asm
  rdtsc
end;

function GetClockTics_LAME: QWord;

begin
  GetClockTics_LAME := MemL[$46C];
end;

procedure DetectCPUSpeed_RDTSC;

begin
  {word absolute $46C}
  Asm
    mov di, fs:[046Ch]
@@1:
    cmp di, fs:[046Ch]
    je @@1
    rdtsc
    mov ebx, eax
    mov ecx, edx
    mov di, fs:[046Ch]
@@2:
    mov ax, fs:[046Ch]
    sub ax, di
    cmp ax, 32
    jb @@2
    rdtsc
    mov [Clk1Lo], ebx
    mov [Clk1Hi], ecx
    mov [Clk2Lo], eax
    mov [Clk2Hi], edx
  end ['EAX','EBX','ECX','EDX','EDI'];
{  Clk1 := Clk1Lo or (QWord(Clk1Hi) shl 32);
  Clk2 := Clk2Lo or (QWord(Clk2Hi) shl 32);}
  Clk1 := Clk1Hi;
  Clk1 := Clk1 shl 32;
  Clk1 := Clk1 + Clk1Lo;
  Clk2 := Clk2Hi;
  Clk2 := Clk2 shl 32;
  Clk2 := Clk2 + Clk2Lo;
  ClkDelta := Clk2 - Clk1;
  CPS := (ClkDelta * 18.2) / 32;
  TimerResolution := 1 / CPS;
end;

procedure _CPU; Assembler;

Label
  nocpuid;

Asm
  mov CpuFlags, 0
  pushf
  pop eax
  mov ecx, eax
  xor eax, 40000h
  push eax
  popf
  pushf
  pop eax
  xor eax, ecx
  jz nocpuid
  push ecx
  popf
  mov eax, ecx
  xor eax, 200000h
  push eax
  popf
  pushf
  pop eax
  xor eax, ecx
  je nocpuid

  pusha
  mov eax, 1
  cpuid
  mov CpuFlags, edx
  popa

nocpuid:
end;

procedure DetectCPU;

begin
  _CPU;
  if (CpuFlags and $10) <> 0 then
    UseRDTSC := True
  else
    UseRDTSC := False;

  if UseRDTSC then
  begin
    DetectCPUSpeed_RDTSC;
    GetClockTics := @GetClockTics_RDTSC;
  end
  else
  begin
    TimerResolution := 1 / 18.2;
    GetClockTics := @GetClockTics_LAME;
  end;
end;

initialization

begin
  DetectCPU;
end;

end.
