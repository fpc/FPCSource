{
    $Id$
    Copyright (c) 1999-2002 by Mazen Neifer

    Contains the assembler object for the SPARC

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit aasmcpu;

{$i fpcdefs.inc}

interface

uses
  cclasses,aasmtai,
  aasmbase,globals,verbose,
  cpubase,cpuinfo;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 0;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 1;

    type
      taicpu = class(taicpu_abstract)
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_ref(op : tasmop;const _op1 : treference);
         constructor op_const(op : tasmop;_op1 : aword);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: aword);
         constructor op_const_reg(op:tasmop; _op1: aword; _op2: tregister);
         constructor op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_ref_reg(op:tasmop;_op1:TRegister;_op2:TReference;_op3:tregister);
         constructor op_reg_const_reg(op:tasmop;_op1:TRegister;_op2:aword;_op3:tregister);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);

         { register allocation }
         function is_nop:boolean;override;
         function is_move:boolean;override;
         function spill_registers(list:Taasmoutput;
                                  rgget:Trggetproc;
                                  rgunget:Trgungetproc;
                                  r:Tsupregset;
                                  var unusedregsint:Tsupregset;
                                  const spilltemplist:Tspill_temp_list):boolean; override;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;


implementation

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taicpu.op_none(op : tasmop);
      begin
         inherited create(op);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031207);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
         inherited create(op);
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : aword);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031205);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031206);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aword);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031208);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: aword; _op2: tregister);
      begin
         inherited create(op);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031209);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031210);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);
      begin
         inherited create(op);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031210);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031211);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031212);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031213);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_reg_ref_reg(op:tasmop;_op1:TRegister;_op2:TReference;_op3:tregister);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031214);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031215);
         { only allowed to load the address }
         if not(_op2.symaddr in [refs_lo,refs_hi]) then
           internalerror(200305311);
         ops:=3;
         loadreg(0,_op1);
         loadref(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_reg_const_reg(op:tasmop;_op1:TRegister;_op2:aword;_op3:tregister);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031216);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031217);
         ops:=3;
         loadreg(0,_op1);
         loadconst(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_cond_sym(op:tasmop;cond:TAsmCond;_op1:tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


    function taicpu.is_nop:boolean;
      begin
        result:=(opcode=A_NOP);
      end;


    function taicpu.is_move:boolean;
      begin
        result:=(opcode=A_MOV) and
                (oper[0].typ=top_reg) and
                (oper[1].typ=top_reg);
      end;


    function taicpu.spill_registers(list:Taasmoutput;
                             rgget:Trggetproc;
                             rgunget:Trgungetproc;
                             r:Tsupregset;
                             var unusedregsint:Tsupregset;
                              const spilltemplist:Tspill_temp_list): boolean;

      function decode_loadstore(op: tasmop; var counterpart: tasmop; var wasload: boolean): boolean;

        begin
          result := true;
          wasload := true;
          case op of
            A_LDSB,
            A_LDUB :
              begin
                counterpart := A_STB;
              end;
            A_LDSH,
            A_LDUH:
              begin
                counterpart := A_STH;
              end;
            A_LD :
              begin
                counterpart := A_ST;
                wasload := false;
              end;
            A_LDD:
              begin
                counterpart := A_STD;
                wasload := false;
              end;
            else
              result := false;
          end;
       end;


    var i:byte;
        supreg, reg1, reg2, reg3: Tsuperregister;
        helpreg:Tregister;
        helpins:Taicpu;
        op:Tasmop;
        pos:Tai;
        wasload: boolean;

      begin
        spill_registers:=false;
        if (ops = 2) and
           (oper[1].typ=top_ref) and
           { oper[1] can also be ref in case of "lis r3,symbol@ha" or so }
           decode_loadstore(opcode,op,wasload) then
          begin
            { the register that's being stored/loaded }
            supreg:=oper[0].reg.number shr 8;
            if supreg in r then
              begin
                // Example:
                //   l?? r20d, 8(r1)   ; r20d must be spilled into -60(r1)
                //
                //   Change into:
                //
                //   l?? r21d, 8(r1)
                //   st? r21d, -60(r1)
                //
                // And:
                //
                //   st? r20d, 8(r1)   ; r20d must be spilled into -60(r1)
                //
                //   Change into:
                //
                //   l?? r21d, -60(r1)
                //   st? r21d, 8(r1)

                pos := get_insert_pos(Tai(previous),oper[0].reg.number shr 8,
                                      oper[1].ref^.base.number shr 8,
                                      oper[1].ref^.index.number shr 8,
                                      unusedregsint);
                rgget(list,pos,0,helpreg);
                spill_registers := true;
                if wasload then
                  begin
                    helpins := taicpu.op_reg_ref(opcode,helpreg,oper[1].ref^);
                    loadref(1,spilltemplist[supreg]);
                    opcode := op;
                  end
                else
                  helpins := taicpu.op_reg_ref(op,helpreg,spilltemplist[supreg]);
                if pos=nil then
                  list.insertafter(helpins,list.first)
                else
                  list.insertafter(helpins,pos.next);
                loadreg(0,helpreg);
                rgunget(list,helpins,helpreg);
                forward_allocation(tai(helpins.next),unusedregsint);
{
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
              end;

            { now the registers used in the reference }
            { a) base                                 }
            supreg := oper[1].ref^.base.number shr 8;
            if supreg in r then
              begin
                if wasload then
                  pos:=get_insert_pos(Tai(previous),oper[1].ref^.index.number shr 8,oper[0].reg.number shr 8,0,unusedregsint)
                else
                  pos:=get_insert_pos(Tai(previous),oper[1].ref^.index.number shr 8,0,0,unusedregsint);
                rgget(list,pos,0,helpreg);
                spill_registers:=true;
                helpins:=Taicpu.op_reg_ref(A_LD,helpreg,spilltemplist[supreg]);
                if pos=nil then
                  list.insertafter(helpins,list.first)
                else
                  list.insertafter(helpins,pos.next);
                oper[1].ref^.base:=helpreg;
                rgunget(list,helpins,helpreg);
                forward_allocation(Tai(helpins.next),unusedregsint);
{
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
              end;

            { b) index }
            supreg := oper[1].ref^.index.number shr 8;
            if supreg in r then
              begin
                if wasload then
                  pos:=get_insert_pos(Tai(previous),oper[1].ref^.base.number shr 8,oper[0].reg.number shr 8,0,unusedregsint)
                else
                  pos:=get_insert_pos(Tai(previous),oper[1].ref^.base.number shr 8,0,0,unusedregsint);
                rgget(list,pos,0,helpreg);
                spill_registers:=true;
                helpins:=Taicpu.op_reg_ref(A_LD,helpreg,spilltemplist[supreg]);
                if pos=nil then
                  list.insertafter(helpins,list.first)
                else
                  list.insertafter(helpins,pos.next);
                oper[1].ref^.index:=helpreg;
                rgunget(list,helpins,helpreg);
                forward_allocation(Tai(helpins.next),unusedregsint);
{
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
              end;
            { load/store is done }
            exit;
          end;

        { all other instructions the compiler generates are the same (I hope):   }
        { operand 0 is a register and is the destination, the others are sources }
        { and can be either registers or constants                               }
        { exception: branches (is_jmp isn't always set for them)                 }
        if oper[0].typ <> top_reg then
          exit;
        reg1 := oper[0].reg.number shr 8;
        if oper[1].typ = top_reg then
          reg2 := oper[1].reg.number shr 8
        else
          reg2 := 0;
        if (ops >= 3) and
           (oper[2].typ = top_reg) then
          reg3 := oper[2].reg.number shr 8
        else
          reg3 := 0;

        supreg:=reg1;
        if supreg in r then
          begin
            // Example:
            //   add r20d, r21d, r22d   ; r20d must be spilled into -60(r1)
            //
            //   Change into:
            //
            //   lwz r23d, -60(r1)
            //   add r23d, r21d, r22d
            //   stw r23d, -60(r1)

            pos := get_insert_pos(Tai(previous),reg1,reg2,reg3,unusedregsint);
            rgget(list,pos,0,helpreg);
            spill_registers := true;
            helpins := taicpu.op_reg_ref(A_ST,helpreg,spilltemplist[supreg]);
            list.insertafter(helpins,self);
            helpins := taicpu.op_reg_ref(A_LD,helpreg,spilltemplist[supreg]);
            if pos=nil then
              list.insertafter(helpins,list.first)
            else
              list.insertafter(helpins,pos.next);
            loadreg(0,helpreg);
            rgunget(list,helpins,helpreg);
            forward_allocation(tai(helpins.next),unusedregsint);
{
            writeln('spilling!');
            list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
          end;

        for i := 1 to 2 do
          if (oper[i].typ = top_reg) then
            begin
              supreg:=oper[i].reg.number shr 8;
              if supreg in r then
                begin
                  // Example:
                  //   add r20d, r21d, r22d   ; r20d must be spilled into -60(r1)
                  //
                  //   Change into:
                  //
                  //   lwz r23d, -60(r1)
                  //   add r23d, r21d, r22d
                  //   stw r23d, -60(r1)

                  pos := get_insert_pos(Tai(previous),reg1,reg2,reg3,unusedregsint);
                  rgget(list,pos,0,helpreg);
                  spill_registers := true;
                  helpins := taicpu.op_reg_ref(A_LD,helpreg,spilltemplist[supreg]);
                  if pos=nil then
                    list.insertafter(helpins,list.first)
                  else
                    list.insertafter(helpins,pos.next);
                  loadreg(i,helpreg);
                  rgunget(list,helpins,helpreg);
                  forward_allocation(tai(helpins.next),unusedregsint);
{
                  writeln('spilling!');
                  list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
}
                end;
            end;
      end;

    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;

end.
{
  $Log$
  Revision 1.31  2003-08-11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.30  2003/06/14 14:53:50  jonas
    * fixed newra cycle for x86
    * added constants for indicating source and destination operands of the
      "move reg,reg" instruction to aasmcpu (and use those in rgobj)

  Revision 1.29  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.28  2003/06/01 01:03:41  peter
    * remove unsupported combinations
    * reg_ref_reg only allowed for refs_lo,refs_hi

  Revision 1.27  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

}
