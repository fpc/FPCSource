{
    $Id$

    Copyright (c) 2003 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generator for the ARM

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
       symtype,
       cgbase,cgobj,
       aasmbase,aasmcpu,aasmtai,
       cpubase,cpuinfo,node,cg64f32,cginfo;


    type
      tcgarm = class(tcg)
        procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);override;
        procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);override;
        procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);override;

        procedure a_call_name(list : taasmoutput;const s : string);override;
        procedure a_call_reg(list : taasmoutput;reg: tregister); override;
        procedure a_call_ref(list : taasmoutput;const ref : treference);override;

        procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; reg: TRegister); override;
        procedure a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;

        procedure a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; a: aword; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; src1, src2, dst: tregister); override;

        { move instructions }
        procedure a_load_const_reg(list : taasmoutput; size: tcgsize; a : aword;reg : tregister);override;
        procedure a_load_reg_ref(list : taasmoutput; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_ref_reg(list : taasmoutput; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);override;
        procedure a_load_reg_reg(list : taasmoutput; fromsize, tosize : tcgsize;reg1,reg2 : tregister);override;

        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list: taasmoutput; size: tcgsize; reg1, reg2: tregister); override;
        procedure a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference); override;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
          l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

        procedure a_jmp_always(list : taasmoutput;l: tasmlabel); override;
        procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: TResFlags; reg: TRegister); override;

        procedure g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:integer);override;
        procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
        procedure g_return_from_proc(list : taasmoutput;parasize : aword); override;
        procedure g_restore_frame_pointer(list : taasmoutput);override;

        procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);override;

        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;

        procedure g_overflowcheck(list: taasmoutput; const l: tlocation; def: tdef); override;

        procedure g_save_standard_registers(list : taasmoutput; usedinproc : Tsupregset);override;
        procedure g_restore_standard_registers(list : taasmoutput; usedinproc : Tsupregset);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;accused,acchiused:boolean);override;

        procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);

        procedure a_load_store(list:taasmoutput;op: tasmop;reg:tregister;
                    ref: treference);

        { creates the correct branch instruction for a given combination }
        { of asmcondflags and destination addressing mode                }
        procedure a_jmp(list: taasmoutput; op: tasmop;
                        c: tasmcond; l: tasmlabel);

     end;

     tcg64farm = class(tcg64f32)
       procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);override;
       procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);override;
       procedure a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : qword;regsrc,regdst : tregister64);override;
       procedure a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);override;
     end;

    {!!!!
    const
      TOpCG2AsmOpConstLo: Array[topcg] of TAsmOp = (A_NONE,A_ADDI,A_ANDI_,A_DIVWU,
                            A_DIVW,A_MULLW, A_MULLW, A_NONE,A_NONE,A_ORI,
                            A_SRAWI,A_SLWI,A_SRWI,A_SUBI,A_XORI);
      TOpCG2AsmOpConstHi: Array[topcg] of TAsmOp = (A_NONE,A_ADDIS,A_ANDIS_,
                            A_DIVWU,A_DIVW, A_MULLW,A_MULLW,A_NONE,A_NONE,
                            A_ORIS,A_NONE, A_NONE,A_NONE,A_SUBIS,A_XORIS);

      TOpCmp2AsmCond: Array[topcmp] of TAsmCondFlag = (C_NONE,C_EQ,C_GT,
                           C_LT,C_GE,C_LE,C_NE,C_LE,C_LT,C_GE,C_GT);
    }

   function is_shifter_const(d : dword;var imm_shift : byte) : boolean;

  implementation


    uses
       globtype,globals,verbose,systems,cutils,symconst,symdef,symsym,rgobj,tgobj,cpupi;


    procedure tcgarm.a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);
      var
        ref: treference;
      begin
        case locpara.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load_const_reg(list,size,a,locpara.register);
          LOC_REFERENCE:
            begin
               reference_reset(ref);
               ref.base:=locpara.reference.index;
               ref.offset:=locpara.reference.offset;
               a_load_const_ref(list,size,a,ref);
            end;
          else
            internalerror(2002081101);
        end;
        if locpara.sp_fixup<>0 then
          internalerror(2002081102);
      end;


    procedure tcgarm.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);
      var
        ref: treference;
        tmpreg: tregister;
      begin
        case locpara.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load_ref_reg(list,size,size,r,locpara.register);
          LOC_REFERENCE:
            begin
               reference_reset(ref);
               ref.base:=locpara.reference.index;
               ref.offset:=locpara.reference.offset;
               tmpreg := rg.getregisterint(list,size);
               a_load_ref_reg(list,size,size,r,tmpreg);
               a_load_reg_ref(list,size,size,tmpreg,ref);
               rg.ungetregisterint(list,tmpreg);
            end;
          LOC_FPUREGISTER,LOC_CFPUREGISTER:
            case size of
               OS_F32, OS_F64:
                 a_loadfpu_ref_reg(list,size,r,locpara.register);
               else
                 internalerror(2002072801);
            end;
          else
            internalerror(2002081103);
        end;
        if locpara.sp_fixup<>0 then
          internalerror(2002081104);
      end;


    procedure tcgarm.a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);
      var
        ref: treference;
        tmpreg: tregister;
      begin
         case locpara.loc of
            LOC_REGISTER,LOC_CREGISTER:
              a_loadaddr_ref_reg(list,r,locpara.register);
            LOC_REFERENCE:
              begin
                reference_reset(ref);
                ref.base := locpara.reference.index;
                ref.offset := locpara.reference.offset;
                tmpreg := rg.getregisterint(list,OS_ADDR);
                a_loadaddr_ref_reg(list,r,tmpreg);
                a_load_reg_ref(list,OS_ADDR,OS_ADDR,tmpreg,ref);
                rg.ungetregisterint(list,tmpreg);
              end;
            else
              internalerror(2002080701);
         end;
      end;


    procedure tcgarm.a_call_name(list : taasmoutput;const s : string);
      begin
         list.concat(taicpu.op_sym(A_BL,objectlibrary.newasmsymbol(s)));
         if not(pi_do_call in current_procinfo.flags) then
           internalerror(2003060703);
      end;


    procedure tcgarm.a_call_reg(list : taasmoutput;reg: tregister);
      var
         r : tregister;
      begin
        r.enum:=R_INTREGISTER;
        r.number:=NR_PC;
        list.concat(taicpu.op_reg_reg(A_MOV,r,reg));
        if not(pi_do_call in current_procinfo.flags) then
          internalerror(2003060704);
      end;


    procedure tcgarm.a_call_ref(list : taasmoutput;const ref : treference);
      var
         r : tregister;
      begin
        r.enum:=R_INTREGISTER;
        r.number:=NR_PC;
        a_load_ref_reg(list,OS_ADDR,OS_ADDR,ref,r);
        if not(pi_do_call in current_procinfo.flags) then
          internalerror(2003060705);
      end;


     procedure tcgarm.a_op_const_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; reg: TRegister);
       begin
{
            shifterop_reset(so);
            { determine operator }
            if nodetype=shln then
              so.shiftertype:=SO_LSL
            else
              so.shiftertype:=SO_LSR;
            { shifting by a constant directly coded: }
            if (right.nodetype=ordconstn) then
              begin
                so.shiftimm:=tordconstnode(right).value and 31;
                a_op_reg_reg_shifterop(exprasmlist,op,OS_32,hregister1,resultreg,so)
              end
            else
              begin
                { load shift count in a register if necessary }
                location_force_reg(exprasmlist,right.location,def_cgsize(right.resulttype.def),true);
                hregister2 := right.location.register;
                so.rs:=hregister2;
                a_op_reg_reg_reg(exprasmlist,op,OS_32,hregister2,hregister1,resultreg);
                rg.UnGetRegisterInt(exprasmlist,hregister2);
              end;
}
       end;


     procedure tcgarm.a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister);
       begin
       end;


     procedure tcgarm.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
       size: tcgsize; a: aword; src, dst: tregister);
       begin
       end;


     procedure tcgarm.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
       size: tcgsize; src1, src2, dst: tregister);
       begin
       end;


     function rotl(d : dword;b : byte) : dword;
       begin
          result:=(d shr (32-b)) or (d shl b);
       end;


     function is_shifter_const(d : dword;var imm_shift : byte) : boolean;
       var
          i : longint;
       begin
          for i:=0 to 15 do
            begin
               if (d and not(rotl($ff,i)))=0 then
                 begin
                    imm_shift:=i;
                    result:=true;
                    exit;
                 end;
            end;
          result:=false;
       end;


     procedure tcgarm.a_load_const_reg(list : taasmoutput; size: tcgsize; a : aword;reg : tregister);
       var
          imm_shift : byte;
          l : tasmlabel;
          hr : treference;
       begin
          if not(size in [OS_8,OS_S8,OS_16,OS_S16,OS_32,OS_S32]) then
            internalerror(2002090902);
          if is_shifter_const(a,imm_shift) then
            list.concat(taicpu.op_reg_const(A_MOV,reg,a))
          else if is_shifter_const(not(a),imm_shift) then
            list.concat(taicpu.op_reg_const(A_MVN,reg,not(a)))
          else
            begin
               objectlibrary.getdatalabel(l);
               current_procinfo.aktlocaldata.concat(Tai_const_symbol.Create(l));
               current_procinfo.aktlocaldata.concat(Tai_const.Create_32bit(a));
               reference_reset(hr);
               hr.symbol:=l;
               list.concat(taicpu.op_reg_ref(A_LDR,reg,hr));
            end;
       end;


     procedure tcgarm.a_load_reg_ref(list : taasmoutput; fromsize, tosize: tcgsize; reg : tregister;const ref : treference);
       begin
       end;


     procedure tcgarm.a_load_ref_reg(list : taasmoutput; fromsize, tosize : tcgsize;const Ref : treference;reg : tregister);
       begin
       end;


     procedure tcgarm.a_load_reg_reg(list : taasmoutput; fromsize, tosize : tcgsize;reg1,reg2 : tregister);
       begin
       end;


     procedure tcgarm.a_loadfpu_reg_reg(list: taasmoutput; size: tcgsize; reg1, reg2: tregister);
       begin
       end;


     procedure tcgarm.a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister);
       begin
       end;


     procedure tcgarm.a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference);
       begin
       end;


     {  comparison operations }
     procedure tcgarm.a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
       l : tasmlabel);
       begin
       end;


     procedure tcgarm.a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
       begin
       end;


     procedure tcgarm.a_jmp_always(list : taasmoutput;l: tasmlabel);
       begin
         list.concat(taicpu.op_sym(A_B,objectlibrary.newasmsymbol(l.name)));
       end;


     procedure tcgarm.a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel);
       begin
       end;


     procedure tcgarm.g_flags2reg(list: taasmoutput; size: TCgSize; const f: TResFlags; reg: TRegister);
       begin
       end;


     procedure tcgarm.g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:integer);
       begin
       end;


     procedure tcgarm.g_stackframe_entry(list : taasmoutput;localsize : longint);
       begin
       end;


     procedure tcgarm.g_return_from_proc(list : taasmoutput;parasize : aword);
       begin
       end;


     procedure tcgarm.g_restore_frame_pointer(list : taasmoutput);
       begin
       end;


     procedure tcgarm.a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);
       begin
       end;


     procedure tcgarm.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);
       begin
       end;


     procedure tcgarm.g_overflowcheck(list: taasmoutput; const l: tlocation; def: tdef);
       begin
       end;


     procedure tcgarm.g_save_standard_registers(list : taasmoutput; usedinproc : Tsupregset);
       begin
       end;


     procedure tcgarm.g_restore_standard_registers(list : taasmoutput; usedinproc : Tsupregset);
       begin
       end;


     procedure tcgarm.g_save_all_registers(list : taasmoutput);
       begin
       end;


     procedure tcgarm.g_restore_all_registers(list : taasmoutput;accused,acchiused:boolean);
       begin
       end;


     procedure tcgarm.a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);
       begin
       end;


     { contains the common code of a_load_reg_ref and a_load_ref_reg }
     procedure tcgarm.a_load_store(list:taasmoutput;op: tasmop;reg:tregister;
                 ref: treference);
       begin
       end;


     { creates the correct branch instruction for a given combination }
     { of asmcondflags and destination addressing mode                }
     procedure tcgarm.a_jmp(list: taasmoutput; op: tasmop;
                     c: tasmcond; l: tasmlabel);
       begin
       end;


     procedure tcg64farm.a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);
       begin
       end;


     procedure tcg64farm.a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);
       begin
       end;


     procedure tcg64farm.a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : qword;regsrc,regdst : tregister64);
       begin
       end;


     procedure tcg64farm.a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);
       begin
       end;


begin
  cg:=tcgarm.create;
  cg64:=tcg64farm.create;
end.
{
  $Log$
  Revision 1.5  2003-08-25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.4  2003/08/24 12:27:26  florian
    * continued to work on the arm port

  Revision 1.3  2003/08/21 03:14:00  florian
    * arm compiler can be compiled; far from being working

  Revision 1.2  2003/08/20 15:50:12  florian
    * more arm stuff

  Revision 1.1  2003/07/21 16:35:30  florian
    * very basic stuff for the arm
}
