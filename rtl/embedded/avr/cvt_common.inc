{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team.

    Common startup code for the compact vector table option
    for AVR controllers.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
  __dtors_end: record end; external name '__dtors_end';

procedure NMI_ISR; external name 'NMI_ISR'; // Interrupt 1
procedure LVL1_ISR; external name 'LVL1_ISR'; // Interrupt 2
procedure LVL0_ISR; external name 'LVL0_ISR'; // Interrupt 3

procedure Default_IRQ_handler_cvt; public name '_Default_IRQ_handler_cvt'; noreturn;
begin
  repeat until false;
end;

{ Enable CVT option of interrupt controller }
procedure initCVT; assembler; nostackframe; noreturn; public name 'initCVT'; section '.init3';
const
  CPU_CCP = $34;
  CPU_CCP_IOREG = $D8;
  CPUINT_CTRLA = $110;
  CPUINT_CTRLA_CVT = 1 shl 5;
asm
  { Disable configuration change protection, then set the CVT bit in CPUINT.CTRLA }
  ldi r26, CPU_CCP_IOREG
  lds r27, CPUINT_CTRLA
  ori r27, CPUINT_CTRLA_CVT
  out CPU_CCP, r26
  sts CPUINT_CTRLA, r27
end;


