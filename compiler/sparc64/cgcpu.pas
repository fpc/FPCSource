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
       aasmbase,aasmtai,aasmdata,aasmcpu,
       cpubase,cpuinfo,
       node,symconst,SymType,symdef,
       rgcpu,
       cgsparc;

    type
      TCGSparc64=class(TCGSparcGen)
       procedure a_load_reg_reg(list : TAsmList; fromsize,tosize : tcgsize; reg1,reg2 : tregister);override;
       procedure a_load_ref_reg_unaligned(list : TAsmList; fromsize,tosize : tcgsize; const ref : treference; register : tregister);override;
       procedure a_load_reg_ref_unaligned(list : TAsmList; fromsize,tosize : tcgsize; register : tregister; const ref : treference);override;
       procedure a_load_const_reg(list : TAsmList; size : TCGSize; a : tcgint; reg : TRegister);override;
      end;

    procedure create_codegen;

  implementation

    uses
      verbose,
      systems;

    procedure TCGSparc64.a_load_reg_reg(list:TAsmList;fromsize,tosize:tcgsize;reg1,reg2:tregister);
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
                 list.concat(taicpu.op_reg_const_reg(A_SLLX,reg1,48,reg2));
                 list.concat(taicpu.op_reg_const_reg(A_SRLX,reg2,48,reg2));
               end;
             OS_32 :
               begin
                 list.concat(taicpu.op_reg_const_reg(A_SLLX,reg1,32,reg2));
                 list.concat(taicpu.op_reg_const_reg(A_SRLX,reg2,32,reg2));
               end;
             OS_S32 :
               list.concat(taicpu.op_reg_reg_reg(A_SRA,reg1,NR_G0,reg2));
             OS_64,
             OS_S64 :
               begin
                 instr:=taicpu.op_reg_reg(A_MOV,reg1,reg2);
                 list.Concat(instr);
                 { Notify the register allocator that we have written a move instruction so
                  it can try to eliminate it. }
                 add_move_instruction(instr);
               end;
             OS_S8 :
               begin
                 list.concat(taicpu.op_reg_const_reg(A_SLLX,reg1,56,reg2));
                 list.concat(taicpu.op_reg_const_reg(A_SRAX,reg2,56,reg2));
               end;
             OS_S16 :
               begin
                 list.concat(taicpu.op_reg_const_reg(A_SLLX,reg1,48,reg2));
                 list.concat(taicpu.op_reg_const_reg(A_SRAX,reg2,48,reg2));
               end;
             else
               internalerror(2017060501);
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


    procedure TCGSparc64.a_load_ref_reg_unaligned(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; register: tregister);
      var
        href: treference;
        hreg1, hreg2, tmpreg: tregister;
      begin
        if fromsize in [OS_64,OS_S64] then
          begin
            { split into two 32 bit loads }
            hreg1:=getintregister(list,OS_32);
            hreg2:=getintregister(list,OS_32);
            a_load_ref_reg(list,OS_32,OS_32,ref,hreg1);
            href:=ref;
            inc(href.offset,4);
            a_load_ref_reg(list,OS_32,OS_32,href,hreg2);
            a_op_const_reg_reg(list,OP_SHL,OS_64,32,hreg1,register);
            a_op_reg_reg_reg(list,OP_OR,OS_64,hreg2,register,register);
          end
       else
         inherited;
      end;


    procedure TCGSparc64.a_load_reg_ref_unaligned(list : TAsmList;fromsize,tosize : tcgsize;register : tregister;const ref : treference);
      var
        href: treference;
        hreg1: tregister;
      begin
        if fromsize in [OS_64,OS_S64] then
          begin
            { split into two 32 bit stores }
            href:=ref;
            if not(TCGSparc64(cg).IsSimpleRef(href)) then
              begin
                hreg1:=getintregister(list,OS_ADDR);
                a_loadaddr_ref_reg(list,href,hreg1);
                reference_reset_base(href,hreg1,0,href.alignment,href.volatility);
              end;
            inc(href.offset,4);
            a_load_reg_ref(list,OS_32,OS_32,register,href);
            hreg1:=getintregister(list,OS_32);
            a_op_const_reg_reg(list,OP_SHR,OS_64,32,register,hreg1);
            dec(href.offset,4);
            a_load_reg_ref(list,OS_32,OS_32,hreg1,href);
          end
       else
         inherited;
      end;


    procedure TCGSparc64.a_load_const_reg(list : TAsmList;size : TCGSize;a : tcgint;reg : TRegister);
      var
        hreg : TRegister;
      begin
        { we don't use the set instruction here because it could be evalutated to two
          instructions which would cause problems with the delay slot (FK) }
        if a=0 then
          list.concat(taicpu.op_reg(A_CLR,reg))
        else if (a>=simm13lo) and (a<=simm13hi) then
          list.concat(taicpu.op_const_reg(A_MOV,a,reg))
        else if (a>=0) and (a<=$ffffffff) then
          begin
            list.concat(taicpu.op_const_reg(A_SETHI,aint(a) shr 10,reg));
            if (aint(a) and aint($3ff))<>0 then
              list.concat(taicpu.op_reg_const_reg(A_OR,reg,aint(a) and aint($3ff),reg));
          end
        else if (a>=-4294967296) and (a<=-1) then
          begin
            list.concat(taicpu.op_const_reg(A_SETHI,(not(aint(a)) shr 10) and $3fffff,reg));
            if (aint(a) and aint($3ff)) or aint($1c00)<>0 then
              list.concat(taicpu.op_reg_const_reg(A_XOR,reg,(aint(a) and aint($3ff)) or aint($1c00),reg));
          end
        else
          begin
            hreg:=getintregister(list,OS_64);
            list.concat(taicpu.op_const_reg(A_SETHI,(aint(a) shr 10) and $3fffff,reg));
            list.concat(taicpu.op_const_reg(A_SETHI,aint(a) shr 42,hreg));
            if ((aint(a) shr 32) and aint($3ff))<>0 then
              list.concat(taicpu.op_reg_const_reg(A_OR,hreg,(aint(a) shr 32) and aint($3ff),hreg));
            if (aint(a) and aint($3ff))<>0 then
              list.concat(taicpu.op_reg_const_reg(A_OR,reg,aint(a) and aint($3ff),reg));
            a_op_const_reg_reg(list,OP_SHL,OS_64,32,hreg,hreg);
            list.concat(taicpu.op_reg_reg_reg(A_OR,reg,hreg,reg));
          end;
      end;


    procedure create_codegen;
      begin
        cg:=TCgSparc64.Create;
        if target_info.system=system_sparc64_linux then
          TCgSparc64(cg).use_unlimited_pic_mode:=true
        else
          TCgSparc64(cg).use_unlimited_pic_mode:=false;
        cg128:=tcg128.create;
      end;

end.

