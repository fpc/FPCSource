{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the code generator for the SPARC

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
unit cgcpu;

{$i fpcdefs.inc}

interface

    uses
       globtype,parabase,
       cgbase,cgutils,cgobj,
       cg64f32,
       aasmbase,aasmtai,aasmdata,aasmcpu,
       cpubase,cpuinfo,
       node,symconst,SymType,symdef,
       rgcpu,
       cgsparc;

    type
      TCGSparc=class(TCGSparcGen)
        procedure a_load_reg_reg(list : TAsmList; fromsize,tosize : tcgsize; reg1,reg2 : tregister);override;
        procedure a_load_const_reg(list : TAsmList; size : TCGSize; a : tcgint; reg : TRegister);override;
      end;

      TCg64Sparc=class(tcg64f32)
      private
        procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp;checkoverflow : boolean);
      public
        procedure a_load64_reg_ref(list : TAsmList;reg : tregister64;const ref : treference);override;
        procedure a_load64_ref_reg(list : TAsmList;const ref : treference;reg : tregister64);override;
        procedure a_load64_ref_cgpara(list : TAsmList;const r : treference;const paraloc : tcgpara);override;
        procedure a_op64_reg_reg(list:TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst:TRegister64);override;
        procedure a_op64_const_reg(list:TAsmList;op:TOpCG;size : tcgsize;value:int64;regdst:TRegister64);override;
        procedure a_op64_const_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64);override;
        procedure a_op64_reg_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);override;
        procedure a_op64_const_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64;setflags : boolean;var ovloc : tlocation);override;
        procedure a_op64_reg_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64;setflags : boolean;var ovloc : tlocation);override;
      end;

    procedure create_codegen;

  implementation

    uses
      verbose,
      systems;

    procedure TCGSparc.a_load_reg_reg(list:TAsmList;fromsize,tosize:tcgsize;reg1,reg2:tregister);
      var
        instr : taicpu;
      begin
         if (tcgsize2size[fromsize] > tcgsize2size[tosize]) or
            ((tcgsize2size[fromsize] = tcgsize2size[tosize]) and
             (fromsize <> tosize)) or
            { needs to mask out the sign in the top 16 bits }
            ((fromsize = OS_S8) and
             (tosize = OS_16)) then
           case tosize of
             OS_8 :
               list.concat(taicpu.op_reg_const_reg(A_AND,reg1,$ff,reg2));
             OS_16 :
               begin
                 list.concat(taicpu.op_reg_const_reg(A_SLL,reg1,16,reg2));
                 list.concat(taicpu.op_reg_const_reg(A_SRL,reg2,16,reg2));
               end;
             OS_32,
             OS_S32 :
               begin
                 instr:=taicpu.op_reg_reg(A_MOV,reg1,reg2);
                 list.Concat(instr);
                 { Notify the register allocator that we have written a move instruction so
                  it can try to eliminate it. }
                 add_move_instruction(instr);
               end;
             OS_S8 :
               begin
                 list.concat(taicpu.op_reg_const_reg(A_SLL,reg1,24,reg2));
                 list.concat(taicpu.op_reg_const_reg(A_SRA,reg2,24,reg2));
               end;
             OS_S16 :
               begin
                 list.concat(taicpu.op_reg_const_reg(A_SLL,reg1,16,reg2));
                 list.concat(taicpu.op_reg_const_reg(A_SRA,reg2,16,reg2));
               end;
             else
               internalerror(2002090901);
           end
         else
           begin
             instr:=taicpu.op_reg_reg(A_MOV,reg1,reg2);
             list.Concat(instr);
             { Notify the register allocator that we have written a move instruction so
              it can try to eliminate it. }
             add_move_instruction(instr);
           end;
      end;


    procedure TCGSparc.a_load_const_reg(list : TAsmList;size : TCGSize;a : tcgint;reg : TRegister);
      begin
        { we don't use the set instruction here because it could be evalutated to two
          instructions which would cause problems with the delay slot (FK) }
        if (a=0) then
          list.concat(taicpu.op_reg(A_CLR,reg))
        else if (a>=simm13lo) and (a<=simm13hi) then
          list.concat(taicpu.op_const_reg(A_MOV,a,reg))
        else
          begin
            list.concat(taicpu.op_const_reg(A_SETHI,aint(a) shr 10,reg));
            if (aint(a) and aint($3ff))<>0 then
              list.concat(taicpu.op_reg_const_reg(A_OR,reg,aint(a) and aint($3ff),reg));
          end;
      end;


{****************************************************************************
                               TCG64Sparc
****************************************************************************}

    procedure tcg64sparc.a_load64_reg_ref(list : TAsmList;reg : tregister64;const ref : treference);
      var
        tmpref: treference;
      begin
        { Override this function to prevent loading the reference twice }
        tmpref:=ref;
        cg.a_load_reg_ref(list,OS_32,OS_32,reg.reghi,tmpref);
        inc(tmpref.offset,4);
        cg.a_load_reg_ref(list,OS_32,OS_32,reg.reglo,tmpref);
      end;


    procedure tcg64sparc.a_load64_ref_reg(list : TAsmList;const ref : treference;reg : tregister64);
      var
        tmpref: treference;
      begin
        { Override this function to prevent loading the reference twice }
        tmpref:=ref;
        cg.a_load_ref_reg(list,OS_32,OS_32,tmpref,reg.reghi);
        inc(tmpref.offset,4);
        cg.a_load_ref_reg(list,OS_32,OS_32,tmpref,reg.reglo);
      end;


    procedure tcg64sparc.a_load64_ref_cgpara(list : TAsmList;const r : treference;const paraloc : tcgpara);
      var
        hreg64 : tregister64;
      begin
        { Override this function to prevent loading the reference twice.
          Use here some extra registers, but those are optimized away by the RA }
        hreg64.reglo:=cg.GetIntRegister(list,OS_32);
        hreg64.reghi:=cg.GetIntRegister(list,OS_32);
        a_load64_ref_reg(list,r,hreg64);
        a_load64_reg_cgpara(list,hreg64,paraloc);
      end;


    procedure TCg64Sparc.get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp;checkoverflow : boolean);
      begin
        case op of
          OP_ADD :
            begin
              op1:=A_ADDCC;
              if checkoverflow then
                op2:=A_ADDXCC
              else
                op2:=A_ADDX;
            end;
          OP_SUB :
            begin
              op1:=A_SUBCC;
              if checkoverflow then
                op2:=A_SUBXCC
              else
                op2:=A_SUBX;
            end;
          OP_XOR :
            begin
              op1:=A_XOR;
              op2:=A_XOR;
            end;
          OP_OR :
            begin
              op1:=A_OR;
              op2:=A_OR;
            end;
          OP_AND :
            begin
              op1:=A_AND;
              op2:=A_AND;
            end;
          else
            internalerror(200203241);
        end;
      end;


    procedure TCg64Sparc.a_op64_reg_reg(list:TAsmList;op:TOpCG;size : tcgsize;regsrc,regdst:TRegister64);
      begin
        case op of
          OP_NEG :
            begin
              { Use the simple code: y=0-z }
              list.concat(taicpu.op_reg_reg_reg(A_SUBcc,NR_G0,regsrc.reglo,regdst.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_SUBX,NR_G0,regsrc.reghi,regdst.reghi));
            end;
          OP_NOT :
            begin
              list.concat(taicpu.op_reg_reg_reg(A_XNOR,regsrc.reglo,NR_G0,regdst.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_XNOR,regsrc.reghi,NR_G0,regdst.reghi));
            end;
        else
          a_op64_reg_reg_reg(list,op,size,regsrc,regdst,regdst);
        end;
      end;


    procedure TCg64Sparc.a_op64_const_reg(list:TAsmList;op:TOpCG;size : tcgsize;value:int64;regdst:TRegister64);
      begin
        a_op64_const_reg_reg(list,op,size,value,regdst,regdst);
      end;


    procedure tcg64sparc.a_op64_const_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;value : int64; regsrc,regdst : tregister64);
      var
        l : tlocation;
      begin
        a_op64_const_reg_reg_checkoverflow(list,op,size,value,regsrc,regdst,false,l);
      end;


    procedure tcg64sparc.a_op64_reg_reg_reg(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);
      var
        l : tlocation;
      begin
        a_op64_reg_reg_reg_checkoverflow(list,op,size,regsrc1,regsrc2,regdst,false,l);
      end;


    procedure tcg64sparc.a_op64_const_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64;setflags : boolean;var ovloc : tlocation);
      var
        op1,op2:TAsmOp;
      begin
        case op of
          OP_NEG,
          OP_NOT :
            internalerror(200306017);
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_const_reg_reg(list,op,OS_INT,tcgint(lo(value)),regsrc.reglo,regdst.reglo);
              cg.a_op_const_reg_reg(list,op,OS_INT,tcgint(hi(value)),regsrc.reghi,regdst.reghi);
            end;
        else
          get_64bit_ops(op,op1,op2,setflags);
          tcgsparc(cg).handle_reg_const_reg(list,op1,regsrc.reglo,tcgint(lo(value)),regdst.reglo);
          tcgsparc(cg).handle_reg_const_reg(list,op2,regsrc.reghi,tcgint(hi(value)),regdst.reghi);
        end;
      end;


    procedure tcg64sparc.a_op64_reg_reg_reg_checkoverflow(list: TAsmList;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64;setflags : boolean;var ovloc : tlocation);
      var
        op1,op2:TAsmOp;
      begin
        case op of
          OP_NEG,
          OP_NOT :
            internalerror(200306017);
        end;
        get_64bit_ops(op,op1,op2,setflags);
        list.concat(taicpu.op_reg_reg_reg(op1,regsrc2.reglo,regsrc1.reglo,regdst.reglo));
        list.concat(taicpu.op_reg_reg_reg(op2,regsrc2.reghi,regsrc1.reghi,regdst.reghi));
      end;


    procedure create_codegen;
      begin
        cg:=TCgSparc.Create;
        if target_info.system=system_sparc_linux then
          TCgSparc(cg).use_unlimited_pic_mode:=true
        else
          TCgSparc(cg).use_unlimited_pic_mode:=false;
        cg64:=TCg64Sparc.Create;
      end;

end.

