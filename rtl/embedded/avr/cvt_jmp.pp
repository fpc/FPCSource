{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team.

    Vector table for compact vector table option for AVR controllers
    supporting the jmp instruction.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit cvt_jmp;

interface

implementation

{$include cvt_common.inc}

procedure _FPC_start_cvt; assembler; nostackframe; noreturn; public name '_START_CVT'; section '.init.cvt';
asm
  jmp __dtors_end
  jmp NMI_ISR
  jmp LVL1_ISR
  jmp LVL0_ISR

  .weak NMI_ISR
  .weak LVL1_ISR
  .weak LVL0_ISR

  .set NMI_ISR, Default_IRQ_handler_cvt
  .set LVL1_ISR, Default_IRQ_handler_cvt
  .set LVL0_ISR, Default_IRQ_handler_cvt
end;

end.

