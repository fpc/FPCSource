{
    $Id$
    Copyright (c) 2003 by Florian Klaempfl

    Contains the assembler object for the ARM

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
  cpubase,cpuinfo,cgbase;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

    type
      taicpu = class(taicpu_abstract)
         oppostfix : TOpPostfix;
         roundingmode : troundingmode;
         procedure loadshifterop(opidx:longint;const so:tshifterop);
         procedure loadregset(opidx:longint;const s:tcpuregisterset);
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : longint);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: aword);

         constructor op_ref_regset(op:tasmop; _op1: treference; _op2: tcpuregisterset);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aword);
         constructor op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
         constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
         constructor op_reg_reg_shifterop(op : tasmop;_op1,_op2 : tregister;_op3 : tshifterop);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);

         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         constructor op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
         constructor op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         { register spilling code }
         function spilling_create_load(const ref:treference;r:tregister): tai;override;
         function spilling_create_store(r:tregister; const ref:treference): tai;override;

         function spilling_get_operation_type(opnr: longint): topertype;override;
      end;
      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    function setoppostfix(i : taicpu;pf : toppostfix) : taicpu;
    function setroundingmode(i : taicpu;rm : troundingmode) : taicpu;
    function setcondition(i : taicpu;c : tasmcond) : taicpu;

    { inserts pc relative symbols at places where they are reachable }
    procedure insertpcrelativedata(list,listtoinsert : taasmoutput);

    procedure InitAsm;
    procedure DoneAsm;


implementation

  uses
    cutils,rgobj,itcpugas;


    procedure taicpu.loadshifterop(opidx:longint;const so:tshifterop);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            if typ<>top_shifterop then
              begin
                clearop(opidx);
                new(shifterop);
              end;
            shifterop^:=so;
            typ:=top_shifterop;
            if assigned(add_reg_instruction_hook) then
              add_reg_instruction_hook(self,shifterop^.rs);
          end;
      end;


    procedure taicpu.loadregset(opidx:longint;const s:tcpuregisterset);
      var
        i : byte;
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_regset then
             clearop(opidx);
           new(regset);
           regset^:=s;
           typ:=top_regset;
           for i:=RS_R0 to RS_R15 do
             begin
               if assigned(add_reg_instruction_hook) and (i in regset^) then
                 add_reg_instruction_hook(self,newreg(R_INTREGISTER,i,R_SUBWHOLE));
             end;
         end;
      end;


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
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : longint);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,aword(_op1));
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aword);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,aword(_op2));
      end;


    constructor taicpu.op_ref_regset(op:tasmop; _op1: treference; _op2: tcpuregisterset);
      begin
         inherited create(op);
         ops:=2;
         loadref(0,_op1);
         loadregset(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;


     constructor taicpu.op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aword);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,aword(_op3));
      end;


     constructor taicpu.op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadsymbol(0,_op3,_op3ofs);
      end;


     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


     constructor taicpu.op_reg_reg_shifterop(op : tasmop;_op1,_op2 : tregister;_op3 : tshifterop);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadshifterop(2,_op3);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
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


     constructor taicpu.op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,_op2ofs);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;


{ ****************************** newra stuff *************************** }

    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        { allow the register allocator to remove unnecessary moves }
        result:=(((opcode=A_MOV) and (regtype = R_INTREGISTER)) or
                 ((opcode=A_MVF) and (regtype = R_FPUREGISTER))
                ) and
                (condition=C_None) and
                (ops=2) and
                (oper[0]^.typ=top_reg) and
                (oper[1]^.typ=top_reg) and
                (oper[0]^.reg=oper[1]^.reg);
      end;


    function taicpu.spilling_create_load(const ref:treference;r:tregister): tai;
      begin
        internalerror(200401261);
      end;


    function taicpu.spilling_create_store(r:tregister; const ref:treference): tai;
      begin
        internalerror(200401262);
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        case opcode of
          A_ADC,A_ADD,A_AND,
          A_EOR,A_CLZ,
          A_LDR,A_LDRB,A_LDRD,A_LDRBT,A_LDRH,A_LDRSB,
          A_LDRSH,A_LDRT,
          A_MOV,A_MVN,A_MLA,A_MUL,
          A_ORR,A_RSB,A_RSC,A_SBC,A_SUB,
          A_SWP,A_SWPB,
          A_LDF,A_FLT,A_FIX,
          A_ADF,A_DVF,A_FDV,A_FML,
          A_RFS,A_RFC,A_RDF,
          A_RMF,A_RPW,A_RSF,A_SUF,A_ABS,A_ACS,A_ASN,A_ATN,A_COS,
          A_EXP,A_LOG,A_LGN,A_MVF,A_MNF,A_FRD,A_MUF,A_POL,A_RND,A_SIN,A_SQT,A_TAN:
            if opnr=0 then
              result:=operand_write
            else
              result:=operand_read;
          A_BIC,A_BKPT,A_B,A_BL,A_BLX,A_BX,
          A_CMN,A_CMP,A_TEQ,A_TST,
          A_CMF,A_CMFE,A_WFS,A_CNF:
            result:=operand_read;
          A_SMLAL,A_UMLAL:
            if opnr in [0,1] then
              result:=operand_readwrite
            else
              result:=operand_read;
           A_SMULL,A_UMULL:
            if opnr in [0,1] then
              result:=operand_write
            else
              result:=operand_read;
          A_STR,A_STRB,A_STRBT,A_STRD,
          A_STRH,A_STRT,A_STF:
            { important is what happens with the involved registers }
            if opnr=0 then
              result := operand_read
            else
              { check for pre/post indexed }
              result := operand_read
          else
            internalerror(200403151);
        end;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;


    function setoppostfix(i : taicpu;pf : toppostfix) : taicpu;
      begin
        i.oppostfix:=pf;
        result:=i;
      end;


    function setroundingmode(i : taicpu;rm : troundingmode) : taicpu;
      begin
        i.roundingmode:=rm;
        result:=i;
      end;


    function setcondition(i : taicpu;c : tasmcond) : taicpu;
      begin
        i.condition:=c;
        result:=i;
      end;


    procedure insertpcrelativedata(list,listtoinsert : taasmoutput);
      var
        curpos : longint;
        lastpos : longint;
        curop : longint;
        curtai : tai;
        curdatatai,hp : tai;
        curdata : taasmoutput;
        l : tasmlabel;
      begin
        curdata:=taasmoutput.create;
        lastpos:=-1;
        curpos:=0;
        curtai:=tai(list.first);
        while assigned(curtai) do
          begin
            { instruction? }
            if curtai.typ=ait_instruction then
              begin
                { walk through all operand of the instruction }
                for curop:=0 to taicpu(curtai).ops-1 do
                  begin
                    { reference? }
                    if (taicpu(curtai).oper[curop]^.typ=top_ref) then
                      begin
                        { pc relative symbol? }
                        curdatatai:=tai(taicpu(curtai).oper[curop]^.ref^.symboldata);
                        if assigned(curdatatai) then
                          begin
                            { if yes, insert till next symbol }
                            repeat
                              hp:=tai(curdatatai.next);
                              listtoinsert.remove(curdatatai);
                              curdata.concat(curdatatai);
                              curdatatai:=hp;
                            until (curdatatai=nil) or (curdatatai.typ=ait_label);
                            if lastpos=-1 then
                              lastpos:=curpos;
                          end;
                      end;
                  end;
                inc(curpos);
              end;

            if (curpos-lastpos)>1020 then
              begin
                lastpos:=curpos;
                hp:=tai(curtai.next);
                objectlibrary.getlabel(l);
                curdata.insert(taicpu.op_sym(A_B,l));
                curdata.concat(tai_label.create(l));
                list.insertlistafter(curtai,curdata);
                curtai:=hp;
              end
            else
              curtai:=tai(curtai.next);
          end;
        list.concatlist(curdata);
        curdata.free;
      end;

end.
{
  $Log$
  Revision 1.30  2004-03-15 22:20:13  florian
    * handling of spilling improved

  Revision 1.29  2004/03/14 16:15:39  florian
    * spilling problem fixed
    * handling of floating point memory references fixed

  Revision 1.28  2004/02/09 22:48:45  florian
    * several fixes to parameter handling on arm

  Revision 1.27  2004/02/08 23:10:21  jonas
    * taicpu.is_same_reg_move() now gets a regtype parameter so it only
      removes moves of that particular register type. This is necessary so
      we don't remove the live_start instruction of a register before it
      has been processed

  Revision 1.26  2004/02/08 20:15:42  jonas
    - removed taicpu.is_reg_move because it's not used anymore
    + support tracking fpu register moves by rgobj for the ppc

  Revision 1.25  2004/01/26 19:05:56  florian
    * fixed several arm issues

  Revision 1.24  2004/01/24 20:19:46  florian
    * fixed some spilling stuff
    + not(<int64>) implemented
    + small set comparisations implemented

  Revision 1.23  2004/01/23 15:12:49  florian
    * fixed generic shl/shr operations
    + added register allocation hook calls for arm specific operand types:
      register set and shifter op

  Revision 1.22  2004/01/21 19:01:03  florian
    * fixed handling of max. distance of pc relative symbols

  Revision 1.21  2004/01/20 21:02:55  florian
    * fixed symbol type writing for arm-linux
    * fixed assembler generation for abs

  Revision 1.20  2003/12/28 16:20:09  jonas
    - removed unused methods from old generic spilling code

  Revision 1.19  2003/12/26 14:02:30  peter
    * sparc updates
    * use registertype in spill_register

  Revision 1.18  2003/12/18 17:06:21  florian
    * arm compiler compilation fixed

  Revision 1.17  2003/12/03 17:39:05  florian
    * fixed several arm calling conventions issues
    * fixed reference reading in the assembler reader
    * fixed a_loadaddr_ref_reg

  Revision 1.16  2003/11/30 19:35:29  florian
    * fixed several arm related problems

  Revision 1.15  2003/11/29 17:36:56  peter
    * fixed is_move

  Revision 1.14  2003/11/24 15:17:37  florian
    * changed some types to prevend range check errors

  Revision 1.13  2003/11/02 14:30:03  florian
    * fixed ARM for new reg. allocation scheme

  Revision 1.12  2003/09/11 11:54:59  florian
    * improved arm code generation
    * move some protected and private field around
    * the temp. register for register parameters/arguments are now released
      before the move to the parameter register is done. This improves
      the code in a lot of cases.

  Revision 1.11  2003/09/06 11:21:49  florian
    * fixed stm and ldm to be usable with preindex operand

  Revision 1.10  2003/09/04 21:07:03  florian
    * ARM compiler compiles again

  Revision 1.9  2003/09/04 00:15:29  florian
    * first bunch of adaptions of arm compiler for new register type

  Revision 1.8  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.7  2003/08/29 21:36:28  florian
    * fixed procedure entry/exit code
    * started to fix reference handling

  Revision 1.6  2003/08/28 00:05:29  florian
    * today's arm patches

  Revision 1.5  2003/08/27 00:27:56  florian
    + same procedure as very day: today's work on arm

  Revision 1.4  2003/08/25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.3  2003/08/24 12:27:26  florian
    * continued to work on the arm port

  Revision 1.2  2003/08/20 15:50:12  florian
    * more arm stuff

  Revision 1.1  2003/08/16 13:23:01  florian
    * several arm related stuff fixed
}
