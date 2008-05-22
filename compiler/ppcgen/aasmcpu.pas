{
    Copyright (c) 1999-2002 by Jonas Maebe and Thomas Schatzl

    Contains the assembler object for the PowerPC 32 and PowerPC 64

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
  globtype,verbose,
  aasmbase,aasmtai,aasmdata,aasmsym,
  cpubase,cgbase,cgutils;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;


    type
      taicpu = class(tai_cpu_abstract_sym)
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : aint);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: aint);
         constructor op_const_reg(op:tasmop; _op1: aint; _op2: tregister);

         constructor op_const_const(op : tasmop;_op1,_op2 : aint);

         constructor op_reg_reg_const_const(op: tasmop; _op1, _op2: tregister; _op3, _op4: aint);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);
         constructor op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: aint);
         constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
         constructor op_const_reg_reg(op : tasmop;_op1 : aint;_op2, _op3 : tregister);
         constructor op_const_reg_const(op : tasmop;_op1 : aint;_op2 : tregister;_op3 : aint);
         constructor op_const_const_const(op : tasmop;_op1 : aint;_op2 : aint;_op3 : aint);

         constructor op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
         constructor op_reg_bool_reg_reg(op : tasmop;_op1: tregister;_op2:boolean;_op3,_op4:tregister);
         constructor op_reg_bool_reg_const(op : tasmop;_op1: tregister;_op2:boolean;_op3:tregister;_op4: aint);

         constructor op_reg_reg_reg_const_const(op : tasmop;_op1,_op2,_op3 : tregister;_op4,_op5 : aint);
         constructor op_reg_reg_const_const_const(op : tasmop;_op1,_op2 : tregister;_op3,_op4,_op5 : aint);


         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_const_const_sym(op : tasmop;_op1,_op2 : aint;_op3: tasmsymbol);


         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:aint);
         constructor op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : aint);
         constructor op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:aint;const _op2 : treference);

         procedure loadbool(opidx:aint;_b:boolean);

         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;
         function spilling_get_operation_type_ref(opnr: longint; reg: tregister): topertype;override;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

    procedure fixup_jmps(list: TAsmList);

implementation

uses cutils, cclasses;

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    procedure taicpu.loadbool(opidx:aint;_b:boolean);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx]^ do
         begin
           if typ=top_ref then
            dispose(ref);
           b:=_b;
           typ:=top_bool;
         end;
      end;


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


    constructor taicpu.op_const(op : tasmop;_op1 : aint);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: aint; _op2: tregister);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tasmop;_op1,_op2 : aint);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_reg_reg_const_const(op: tasmop; _op1, _op2: tregister; _op3, _op4: aint);
      begin
        inherited create(op);
        ops := 4;
        loadreg(0, _op1);
        loadreg(1, _op2);
        loadconst(2, _op3);
        loadconst(3, _op4);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

     constructor taicpu.op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
      end;

     constructor taicpu.op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: aint);
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

    constructor taicpu.op_const_reg_reg(op : tasmop;_op1 : aint;_op2, _op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

     constructor taicpu.op_const_reg_const(op : tasmop;_op1 : aint;_op2 : tregister;_op3 : aint);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
      end;


     constructor taicpu.op_const_const_const(op : tasmop;_op1 : aint;_op2 : aint;_op3 : aint);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadconst(1,_op2);
         loadconst(2,_op3);
      end;


     constructor taicpu.op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_bool_reg_reg(op : tasmop;_op1: tregister;_op2:boolean;_op3,_op4:tregister);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadbool(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_bool_reg_const(op : tasmop;_op1: tregister;_op2:boolean;_op3:tregister;_op4: aint);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadbool(0,_op2);
         loadreg(0,_op3);
         loadconst(0,cardinal(_op4));
      end;


     constructor taicpu.op_reg_reg_reg_const_const(op : tasmop;_op1,_op2,_op3 : tregister;_op4,_op5 : aint);
      begin
         inherited create(op);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadconst(3,cardinal(_op4));
         loadconst(4,cardinal(_op5));
      end;

     constructor taicpu.op_reg_reg_const_const_const(op : tasmop;_op1,_op2 : tregister;_op3,_op4,_op5 : aint);
      begin
         inherited create(op);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
         loadconst(3,_op4);
         loadconst(4,_op5);
      end;

    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;

     constructor taicpu.op_const_const_sym(op : tasmop;_op1,_op2 : aint; _op3: tasmsymbol);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadconst(1,_op2);
         loadsymbol(2,_op3,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:aint);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


     constructor taicpu.op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : aint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,_op2ofs);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:aint;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;


{ ****************************** newra stuff *************************** }

    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result :=
          (((opcode=A_MR) and
            (regtype = R_INTREGISTER)) or
           ((opcode = A_FMR) and
            (regtype = R_FPUREGISTER))) and
          { these opcodes can only have registers as operands }
          (oper[0]^.reg=oper[1]^.reg);
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        result := operand_read;
        case opcode of
          A_STMW,A_LMW:
            internalerror(2005021805);
          A_STBU, A_STBUX, A_STHU, A_STHUX,
          A_STWU, A_STWUX,
          A_STFSU, A_STFSUX, A_STFDU, A_STFDUX,
          A_STB, A_STBX, A_STH, A_STHX,
          A_STW, A_STWX,
          A_STFS, A_STFSX, A_STFD, A_STFDX, A_STFIWX, A_STHBRX, A_STWBRX, A_STWCX_,
          A_CMP, A_CMPI, A_CMPL, A_CMPLI,
          A_DCBA, A_DCBI, A_DCBST, A_DCBT, A_DCBTST, A_DCBZ, A_DCBF, A_ICBI,
          A_ECOWX, A_FCMPO, A_FCMPU, A_MTMSR, A_TLBIE, A_TW, A_TWI,
          A_CMPWI, A_CMPW, A_CMPLWI, A_CMPLW, A_MT, A_MTLR, A_MTCTR
{$ifdef cpu64bit}
          , A_STDU, A_STDUX,
          A_STD, A_STDX,
          A_STDCX_,
          A_CMPD, A_CMPDI, A_CMPLD, A_CMPLDI,
          A_MFXER
{$endif cpu64bit}
            : ;
          A_RLWIMI, A_RLWIMI_
{$ifdef cpu64bit}
          , A_INSRDI, A_INSRDI_, A_RLDIMI
{$endif not cpu64bit}
          :
            if opnr = 0 then
              result := operand_readwrite;
          else
            if opnr = 0 then
              result := operand_write;
        end;
      end;


    function taicpu.spilling_get_operation_type_ref(opnr: longint; reg: tregister): topertype;
      begin
        result := operand_read;
        case opcode of
          A_STBU, A_STBUX, A_STHU, A_STHUX, A_STWU, A_STWUX, 
{$ifdef cpu64bit}
          A_STDU, A_STDUX,
{$endif cpu64bit}
          A_STFSU, A_STFSUX, A_STFDU, A_STFDUX:
            if (oper[opnr]^.ref^.base = reg) then
              result := operand_readwrite;
        end;
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER:
{$ifdef cpu64bit}
            result:=taicpu.op_reg_ref(A_LD,r,ref);
{$else cpu64bit}
            result:=taicpu.op_reg_ref(A_LWZ,r,ref);
{$endif cpu64bit}
          R_FPUREGISTER:
            result:=taicpu.op_reg_ref(A_LFD,r,ref);
          else
            internalerror(2005123101);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER:
{$ifdef cpu64bit}
            result:=taicpu.op_reg_ref(A_STD,r,ref);
{$else cpu64bit}
            result:=taicpu.op_reg_ref(A_STW,r,ref);
{$endif cpu64bit}
          R_FPUREGISTER:
            result:=taicpu.op_reg_ref(A_STFD,r,ref);
          else
            internalerror(2005123102);
        end;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;


    procedure fixup_jmps(list: TAsmList);
      var
        p: tai;
        newjmp: taicpu;
        labelpositions: TFPList;
        instrpos: ptrint;
        l: tasmlabel;
        inserted_something: boolean;
      begin
        // if certainly not enough instructions to cause an overflow, don't bother
        if (list.count <= (high(smallint) div 4)) then
          exit;
        labelpositions := TFPList.create;
        p := tai(list.first);
        instrpos := 1;
        // record label positions
        while assigned(p) do
          begin
            if p.typ = ait_label then
              begin
                if (tai_label(p).labsym.labelnr >= labelpositions.count) then
                  labelpositions.count := tai_label(p).labsym.labelnr * 2;
                labelpositions[tai_label(p).labsym.labelnr] := pointer(instrpos);
              end;
            { ait_const is for jump tables }
            case p.typ of
              ait_instruction:
                inc(instrpos);
              ait_const:
                begin
                  if (tai_const(p).consttype<>aitconst_32bit) then
                    internalerror(2008052101); 
                  inc(instrpos);
                end;
            end;
            p := tai(p.next);
          end;

        // check and fix distances
        repeat
          inserted_something := false;
          p := tai(list.first);
          instrpos := 1;
          while assigned(p) do
            begin
              case p.typ of
                ait_label:
                  // update labelposition in case it changed due to insertion
                  // of jumps
                  begin
                    // can happen because of newly inserted labels
                    if (tai_label(p).labsym.labelnr > labelpositions.count) then
                      labelpositions.count := tai_label(p).labsym.labelnr * 2;
                    labelpositions[tai_label(p).labsym.labelnr] := pointer(instrpos);
                  end;
                ait_instruction:
                  begin
                    inc(instrpos);
                    case taicpu(p).opcode of
                      A_BC:
                        if (taicpu(p).oper[0]^.typ = top_ref) and
                           assigned(taicpu(p).oper[0]^.ref^.symbol) and
                           (taicpu(p).oper[0]^.ref^.symbol is tasmlabel) and
                           (labelpositions[tasmlabel(taicpu(p).oper[0]^.ref^.symbol).labelnr] <> NIL) and
{$ifopt q+}
{$q-}
{$define overflowon}
{$endif}
                           (ptruint(abs(ptrint(labelpositions[tasmlabel(taicpu(p).oper[0]^.ref^.symbol).labelnr]-instrpos)) - (low(smallint) div 4)) > ptruint((high(smallint) - low(smallint)) div 4)) then
{$ifdef overflowon}
{$q+}
{$undef overflowon}
{$endif}
                          begin
                            // add a new label after this jump
                            current_asmdata.getjumplabel(l);
                            { new label -> may have to increase array size }
                            if (l.labelnr >= labelpositions.count) then
                              labelpositions.count := l.labelnr + 10;
                            { newjmp will be inserted before the label, and it's inserted after }
                            { the current jump -> instrpos+2                                    }
                            labelpositions[l.labelnr] := pointer(instrpos+2);
                            list.insertafter(tai_label.create(l),p);
                            // add a new unconditional jump between this jump and the label
                            newjmp := taicpu.op_sym(A_B,taicpu(p).oper[0]^.ref^.symbol);
                            newjmp.is_jmp := true;
                            newjmp.fileinfo := taicpu(p).fileinfo;
                            list.insertafter(newjmp,p);
                            inc(instrpos);
                            // change the conditional jump to point to the newly inserted label
                            tasmlabel(taicpu(p).oper[0]^.ref^.symbol).decrefs;
                            taicpu(p).oper[0]^.ref^.symbol := l;
                            l.increfs;
                            // and invert its condition code
                            taicpu(p).condition := inverse_cond(taicpu(p).condition);
                            // we inserted an instruction, so will have to check everything again
                            inserted_something := true;
                          end;
                    end;
                  end;
                ait_const:
                  inc(instrpos);
              end;
              p := tai(p.next);
            end;
         until not inserted_something;
        labelpositions.free;
      end;


begin
  cai_align:=tai_align;
  cai_cpu:=taicpu;
end.

