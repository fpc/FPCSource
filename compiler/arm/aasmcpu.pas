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
  cpubase,cpuinfo;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

    type
      taicpu = class(taicpu_abstract)
         roundingmode : troundingmode;
         procedure loadshifterop(opidx:longint;const so:tshifterop);
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : longint);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: longint);

         constructor op_const_const(op : tasmop;_op1,_op2 : longint);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: Longint);
         constructor op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
         constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
         constructor op_const_reg_reg(op : tasmop;_op1 : longint;_op2, _op3 : tregister);
         constructor op_const_reg_const(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : longint);
         constructor op_reg_reg_shifterop(op : tasmop;_op1,_op2 : tregister;_op3 : tshifterop);

         constructor op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);

         constructor op_reg_reg_reg_const_const(op : tasmop;_op1,_op2,_op3 : tregister;_op4,_op5 : Longint);
         constructor op_reg_reg_const_const_const(op : tasmop;_op1,_op2 : tregister;_op3,_op4,_op5 : Longint);


         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_const_const_sym(op : tasmop;_op1,_op2 : longint;_op3: tasmsymbol);


         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         constructor op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
         constructor op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

         function is_nop: boolean; override;
         function is_move:boolean; override;
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

  uses
    cutils,rgobj;


    procedure taicpu.loadshifterop(opidx:longint;const so:tshifterop);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
          begin
            if typ<>top_shifterop then
              new(shifterop);
            shifterop^:=so;
            typ:=top_shifterop;
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
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031207);
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
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031205);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031206);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: longint);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031208);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,aword(_op2));
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


    constructor taicpu.op_const_const(op : tasmop;_op1,_op2 : longint);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,aword(_op1));
         loadconst(1,aword(_op2));
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

     constructor taicpu.op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: Longint);
       begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031214);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031215);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,aword(_op3));
      end;

     constructor taicpu.op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
       begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031216);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031217);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadsymbol(0,_op3,_op3ofs);
      end;


     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
       begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031218);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031219);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


    constructor taicpu.op_const_reg_reg(op : tasmop;_op1 : longint;_op2, _op3 : tregister);
      begin
         inherited create(op);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031221);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031220);
         ops:=3;
         loadconst(0,aword(_op1));
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;


     constructor taicpu.op_const_reg_const(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : longint);
      begin
         inherited create(op);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031222);
         ops:=3;
         loadconst(0,aword(_op1));
         loadreg(1,_op2);
         loadconst(2,aword(_op3));
      end;


     constructor taicpu.op_reg_reg_shifterop(op : tasmop;_op1,_op2 : tregister;_op3 : tshifterop);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(200308233);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(200308233);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadshifterop(2,_op3);
      end;


     constructor taicpu.op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031223);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031224);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031225);
         if (_op4.enum = R_INTREGISTER) and (_op4.number = NR_NO) then
           internalerror(2003031226);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;


     constructor taicpu.op_reg_reg_reg_const_const(op : tasmop;_op1,_op2,_op3 : tregister;_op4,_op5 : Longint);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031232);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031233);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031233);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadconst(3,cardinal(_op4));
         loadconst(4,cardinal(_op5));
      end;

     constructor taicpu.op_reg_reg_const_const_const(op : tasmop;_op1,_op2 : tregister;_op3,_op4,_op5 : Longint);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031232);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031233);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,aword(_op3));
         loadconst(3,cardinal(_op4));
         loadconst(4,cardinal(_op5));
      end;

    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;

     constructor taicpu.op_const_const_sym(op : tasmop;_op1,_op2 : longint; _op3: tasmsymbol);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,aword(_op1));
         loadconst(1,aword(_op2));
         loadsymbol(2,_op3,0);
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

    function taicpu.is_nop: boolean;
      begin
        { we don't insert any more nops than necessary }
        is_nop := false;
      end;


    function taicpu.is_move:boolean;
      begin
        is_move := opcode = A_MOV;
      end;


    function taicpu.spill_registers(list:Taasmoutput;
                             rgget:Trggetproc;
                             rgunget:Trgungetproc;
                             r:Tsupregset;
                             var unusedregsint:Tsupregset;
                              const spilltemplist:Tspill_temp_list): boolean;
{$ifdef dummy}
      function get_insert_pos(p:Tai;huntfor1,huntfor2,huntfor3:Tsuperregister):Tai;

      var back:Tsupregset;

      begin
        back:=unusedregsint;
        get_insert_pos:=p;
        while (p<>nil) and (p.typ=ait_regalloc) do
          begin
            {Rewind the register allocation.}
            if Tai_regalloc(p).allocation then
              include(unusedregsint,Tai_regalloc(p).reg.number shr 8)
            else
              begin
                exclude(unusedregsint,Tai_regalloc(p).reg.number shr 8);
                if Tai_regalloc(p).reg.number shr 8=huntfor1 then
                  begin
                    get_insert_pos:=Tai(p.previous);
                    back:=unusedregsint;
                  end;
                if Tai_regalloc(p).reg.number shr 8=huntfor2 then
                  begin
                    get_insert_pos:=Tai(p.previous);
                    back:=unusedregsint;
                  end;
                if Tai_regalloc(p).reg.number shr 8=huntfor3 then
                  begin
                    get_insert_pos:=Tai(p.previous);
                    back:=unusedregsint;
                  end;
              end;
            p:=Tai(p.previous);
          end;
        unusedregsint:=back;
      end;

      procedure forward_allocation(p:Tai);

      begin
        {Forward the register allocation again.}
        while (p<>self) do
          begin
            if p.typ<>ait_regalloc then
              internalerror(200305311);
            if Tai_regalloc(p).allocation then
              exclude(unusedregsint,Tai_regalloc(p).reg.number shr 8)
            else
              include(unusedregsint,Tai_regalloc(p).reg.number shr 8);
            p:=Tai(p.next);
          end;
      end;


      function decode_loadstore(op: tasmop; var counterpart: tasmop; wasload: boolean): boolean;

        begin
          result := true;
          wasload := true;
          case op of
            A_LBZ:
              begin
                counterpart := A_STB;
              end;
            A_LBZX:
              begin
                counterpart := A_STBX;
              end;
            A_LHZ,A_LHA:
              begin
                counterpart := A_STH;
              end;
            A_LHZX,A_LHAX:
              begin
                counterpart := A_STHX;
              end;
            A_LWZ:
              begin
                counterpart := A_STW;
              end;
            A_LWZX:
              begin
                counterpart := A_STWX;
              end;
            A_STB:
              begin
                counterpart := A_LBZ;
                wasload := false;
              end;
            A_STBX:
              begin
                counterpart := A_LBZX;
                wasload := false;
              end;
            A_STH:
              begin
                counterpart := A_LHZ;
                wasload := false;
              end;
            A_STHX:
              begin
                counterpart := A_LHZX;
                wasload := false;
              end;
            A_STW:
              begin
                counterpart := A_LWZ;
                wasload := false;
              end;
            A_STWX:
              begin
                counterpart := A_LWZX;
                wasload := false;
              end;
            A_LBZU,A_LBZUX,A_LHZU,A_LHZUX,A_LHAU,A_LHAUX,
            A_LWZU,A_LWZUX,A_STBU,A_STBUX,A_STHU,A_STHUX,
            A_STWU,A_STWUX:
              internalerror(2003070602);
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
                                      oper[1].ref^.base.number shr 8,oper[1].ref^.index.number shr 8);
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
                forward_allocation(tai(helpins.next));
{$ifdef debugra}
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
{$endif debugra}
              end;

            { now the registers used in the reference }
            { a) base                                 }
            supreg := oper[1].ref^.base.number shr 8;
            if supreg in r then
              begin
                if wasload then
                  pos:=get_insert_pos(Tai(previous),oper[1].ref^.index.number shr 8,oper[0].reg.number shr 8,0)
                else
                  pos:=get_insert_pos(Tai(previous),oper[1].ref^.index.number shr 8,0,0);
                rgget(list,pos,0,helpreg);
                spill_registers:=true;
                helpins:=Taicpu.op_reg_ref(A_LWZ,helpreg,spilltemplist[supreg]);
                if pos=nil then
                  list.insertafter(helpins,list.first)
                else
                  list.insertafter(helpins,pos.next);
                oper[1].ref^.base:=helpreg;
                rgunget(list,helpins,helpreg);
                forward_allocation(Tai(helpins.next));
{$ifdef debugra}
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
{$endif debugra}
              end;

            { b) index }
            supreg := oper[1].ref^.index.number shr 8;
            if supreg in r then
              begin
                if wasload then
                  pos:=get_insert_pos(Tai(previous),oper[1].ref^.base.number shr 8,oper[0].reg.number shr 8,0)
                else
                  pos:=get_insert_pos(Tai(previous),oper[1].ref^.base.number shr 8,0,0);
                rgget(list,pos,0,helpreg);
                spill_registers:=true;
                helpins:=Taicpu.op_reg_ref(A_LWZ,helpreg,spilltemplist[supreg]);
                if pos=nil then
                  list.insertafter(helpins,list.first)
                else
                  list.insertafter(helpins,pos.next);
                oper[1].ref^.index:=helpreg;
                rgunget(list,helpins,helpreg);
                forward_allocation(Tai(helpins.next));
{$ifdef debugra}
                writeln('spilling!');
                list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
{$endif debugra}
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

            pos := get_insert_pos(Tai(previous),reg1,reg2,reg3);
            rgget(list,pos,0,helpreg);
            spill_registers := true;
            helpins := taicpu.op_reg_ref(A_STW,helpreg,spilltemplist[supreg]);
            list.insertafter(helpins,self);
            helpins := taicpu.op_reg_ref(A_LWZ,helpreg,spilltemplist[supreg]);
            if pos=nil then
              list.insertafter(helpins,list.first)
            else
              list.insertafter(helpins,pos.next);
            loadreg(0,helpreg);
            rgunget(list,helpins,helpreg);
            forward_allocation(tai(helpins.next));
{$ifdef debugra}
            writeln('spilling!');
            list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
{$endif debugra}
          end;

        for i := 1 to 2 do
          if (oper[i].typ = top_reg) then
            begin
              supreg:=oper[i].reg.number;
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

                  pos := get_insert_pos(Tai(previous),reg1,reg2,reg3);
                  rgget(list,pos,0,helpreg);
                  spill_registers := true;
                  helpins := taicpu.op_reg_ref(A_LWZ,helpreg,spilltemplist[supreg]);
                  if pos=nil then
                    list.insertafter(helpins,list.first)
                  else
                    list.insertafter(helpins,pos.next);
                  loadreg(i,helpreg);
                  rgunget(list,helpins,helpreg);
                  forward_allocation(tai(helpins.next));
{$ifdef debugra}
                  writeln('spilling!');
                  list.insertafter(tai_comment.Create(strpnew('Spilling!')),helpins);
{$endif debugra}
                end;
            end;
      end;
{$else dummy}
      begin
      end;
{$endif dummy}


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;

end.
{
  $Log$
  Revision 1.4  2003-08-25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.3  2003/08/24 12:27:26  florian
    * continued to work on the arm port

  Revision 1.2  2003/08/20 15:50:12  florian
    * more arm stuff

  Revision 1.1  2003/08/16 13:23:01  florian
    * several arm related stuff fixed
}
