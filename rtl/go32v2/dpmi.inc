{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
TYPE
     TDPMIRegisters = record
       EDI, ESI, EBP, Reserved, EBX, EDX, ECX, EAX : longint;
       Flags, ES, DS, FS, GS, IP, CS, SP, SS : word;
     end;



Procedure RealIntr(IntNo : word; var Regs:TDPMIRegisters); assembler;
(*********************************************************************)
(*  PROCEDURE RealModeInt(IntNo: word; Var Regs: TDPMIRegisters)     *)
(*   Calls the DPMI server to switch to real mode and call the       *)
(*   real mode interrupt. ALL MEMORY REGISTERS (if used) SHOULD      *)
(*   contain REAL MODE ADRESSES!                                     *)
(*       IntNo -> Real mode interrupt to call (0-255)                *)
(*       Regs -> Registers to pass on to interrupt.                  *)
(*        (ALL UNUSED REGISTERS SHOULD BE SET TO 0 ON ENTRY!)        *)
(*********************************************************************)
asm
    PUSH BP                                          { Save BP, just in case }
    MOV BX,IntNo                         { Move the Interrupt number into BX }
    XOR CX,CX                                                     { Clear CX }
    LES DI,Regs                              { Load the registers into ES:DI }
    MOV AX,$300                                { Set function number to 300h }
    INT $31                             { Call Interrupt 31h - DPMI Services }
    JC @Exit                                         { Jump to exit on carry }
    XOR AX,AX                                                     { Clear AX }
    @Exit:                                                      { Exit label }
    POP BP                                                      { Restore BP }
  end;

{
$Log$
Revision 1.4  2000-02-09 16:59:28  peter
  * truncated log

Revision 1.3  2000/01/07 16:41:31  daniel
  * copyright 2000

Revision 1.2  2000/01/07 16:32:23  daniel
  * copyright 2000 added

Revision 1.1  1999/11/08 11:15:21  peter
  * move graph.inc to the target dir

}
