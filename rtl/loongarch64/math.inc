{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Implementation of mathematical Routines (only for real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    {$define FPC_SYSTEM_HAS_INT}
    function fpc_int_real(d : ValReal) : ValReal;assembler;nostackframe;compilerproc;
      asm
        movfcsr2gr $t0, $r3
        li.d $t1, 0x100
        movgr2fcsr $r3, $t1
        frint.d $fa0, $fa0
        movgr2fcsr $r3, $t0
      end;

    {$define FPC_SYSTEM_HAS_FRAC}
    function fpc_frac_real(d : ValReal) : ValReal;assembler;nostackframe;compilerproc;
      asm
        movfr2gr.d $t0, $fa0
        li.d $t1, 0x7ff0000000000000
        and $t0, $t0, $t1
        beq $t0, $t1, .LNaN
        movfcsr2gr $t0, $r3
        li.d $t1, 0x100
        movgr2fcsr $r3, $t1
        frint.d $fa1, $fa0
        movgr2fcsr $r3, $t0
        fsub.d $fa0, $fa0, $fa1
        jr $ra
      .LNaN:
        fsub.d $fa0, $fa0, $fa0
        movgr2fr.d $fa1, $zero
        fdiv.d $fa0, $fa0, $fa1
        // jr $ra
      end;
