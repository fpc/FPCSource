 {
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements the code generator for the PowerPC

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

{$i defines.inc}

  interface

    uses
       cgbase,cgobj,aasm,cpuasm,cpubase,cpuinfo,cg64f32;

    type
      tcgppc = class(tcg64f32)
        { passing parameters, per default the parameter is pushed }
        { nr gives the number of the parameter (enumerated from   }
        { left to right), this allows to move the parameter to    }
        { register, if the cpu supports register calling          }
        { conventions                                             }
        procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;nr : longint);override;
        procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;nr : longint);override;
        procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;nr : longint);override;
        procedure a_paramaddr_ref(list : taasmoutput;const r : treference;nr : longint);override;


        procedure a_call_name(list : taasmoutput;const s : string;
          offset : longint);override;

        procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister); override;
        procedure a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;

        procedure a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; a: aword; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; src1, src2, dst: tregister); override;

        { move instructions }
        procedure a_load_const_reg(list : taasmoutput; size: tcgsize; a : aword;reg : tregister);override;
        procedure a_load_reg_ref(list : taasmoutput; size: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_ref_reg(list : taasmoutput;size : tcgsize;const Ref : treference;reg : tregister);override;
        procedure a_load_reg_reg(list : taasmoutput;size : tcgsize;reg1,reg2 : tregister);override;
        procedure a_load_sym_ofs_reg(list: taasmoutput; const sym: tasmsymbol; ofs: longint; reg: tregister); override;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
          l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

        procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel); override;
        procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: taasmoutput; const f: TResFlags; reg: TRegister); override;


        procedure g_stackframe_entry_sysv(list : taasmoutput;localsize : longint);
        procedure g_stackframe_entry_mac(list : taasmoutput;localsize : longint);
        procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
        procedure g_restore_frame_pointer(list : taasmoutput);override;
        procedure g_return_from_proc(list : taasmoutput;parasize : aword); override;

        procedure a_loadaddress_ref_reg(list : taasmoutput;const ref2 : treference;r : tregister);override;

        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;

        { find out whether a is of the form 11..00..11b or 00..11...00. If }
        { that's the case, we can use rlwinm to do an AND operation        }
        function get_rlwi_const(a: longint; var l1, l2: longint): boolean;

        private

        procedure g_return_from_proc_sysv(list : taasmoutput;parasize : aword);
        procedure g_return_from_proc_mac(list : taasmoutput;parasize : aword);


        { Make sure ref is a valid reference for the PowerPC and sets the }
        { base to the value of the index if (base = R_NO).                }
        procedure fixref(list: taasmoutput; var ref: treference);

        { contains the common code of a_load_reg_ref and a_load_ref_reg }
        procedure a_load_store(list:taasmoutput;op: tasmop;reg:tregister;
                    ref: treference);

        { creates the correct branch instruction for a given combination }
        { of asmcondflags and destination addressing mode                }
        procedure a_jmp(list: taasmoutput; op: tasmop;
                        c: tasmcondflag; crval: longint; l: tasmlabel);

     end;

const
{
  TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_ADD,A_AND,A_DIVWU,
                 A_DIVW,A_MULLW, A_MULLW, A_NEG,A_NOT,A_OR,
                 A_SRAW,A_SLW,A_SRW,A_SUB,A_XOR);
}

  TOpCG2AsmOpConstLo: Array[topcg] of TAsmOp = (A_ADDI,A_ANDI_,A_DIVWU,
                        A_DIVW,A_MULLW, A_MULLW, A_NONE,A_NONE,A_ORI,
                        A_SRAWI,A_SLWI,A_SRWI,A_SUBI,A_XORI);
  TOpCG2AsmOpConstHi: Array[topcg] of TAsmOp = (A_ADDIS,A_ANDIS_,
                        A_DIVWU,A_DIVW, A_MULLW,A_MULLW,A_NONE,A_NONE,
                        A_ORIS,A_NONE, A_NONE,A_NONE,A_SUBIS,A_XORIS);

  TOpCmp2AsmCond: Array[topcmp] of TAsmCondFlag = (C_NONE,C_EQ,C_GT,
                       C_LT,C_GE,C_LE,C_NE,C_LE,C_NG,C_GE,C_NL);

  LoadInstr: Array[OS_8..OS_S32,boolean, boolean] of TAsmOp =
                         { indexed? updating?}
             (((A_LBZ,A_LBZU),(A_LBZX,A_LBZUX)),
              ((A_LHZ,A_LHZU),(A_LHZX,A_LHZUX)),
              ((A_LWZ,A_LWZU),(A_LWZX,A_LWZUX)),
              { 64bit stuff should be handled separately }
              ((A_NONE,A_NONE),(A_NONE,A_NONE)),
              { there's no load-byte-with-sign-extend :( }
              ((A_LBZ,A_LBZU),(A_LBZX,A_LBZUX)),
              ((A_LHA,A_LHAU),(A_LHAX,A_LHAUX)),
              ((A_LWZ,A_LWZU),(A_LWZX,A_LWZUX)));

  StoreInstr: Array[OS_8..OS_32,boolean, boolean] of TAsmOp =
                          { indexed? updating?}
             (((A_STB,A_STBU),(A_STBX,A_STBUX)),
              ((A_STH,A_STHU),(A_STHX,A_STHUX)),
              ((A_STW,A_STWU),(A_STWX,A_STWUX)));


  implementation

    uses
       globtype,globals,verbose,systems,cutils, tgcpu;

{ parameter passing... Still needs extra support from the processor }
{ independent code generator                                        }

    procedure tcgppc.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;nr : longint);

      var
        ref: treference;

      begin
  {$ifdef para_sizes_known}
        if (nr <= max_param_regs_int) then
          a_load_reg_reg(list,size,r,param_regs_int[nr])
        else
          begin
            reset_reference(ref);
            ref.base := stack_pointer;
            ref.offset := LinkageAreaSize+para_size_till_now;
            a_load_reg_ref(list,size,reg,ref);
          end;
  {$endif para_sizes_known}
      end;


    procedure tcgppc.a_param_const(list : taasmoutput;size : tcgsize;a : aword;nr : longint);

      var
        ref: treference;

      begin
  {$ifdef para_sizes_known}
        if (nr <= max_param_regs_int) then
          a_load_const_reg(list,size,a,param_regs_int[nr])
        else
          begin
            reset_reference(ref);
            ref.base := stack_pointer;
            ref.offset := LinkageAreaSize+para_size_till_now;
            a_load_const_ref(list,size,a,ref);
          end;
  {$endif para_sizes_known}
      end;


    procedure tcgppc.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;nr : longint);

      var
        ref: treference;
        tmpreg: tregister;

      begin
  {$ifdef para_sizes_known}
        if (nr <= max_param_regs_int) then
          a_load_ref_reg(list,size,r,param_regs_int[nr])
        else
          begin
            reset_reference(ref);
            ref.base := stack_pointer;
            ref.offset := LinkageAreaSize+para_size_till_now;
            tmpreg := get_scratch_reg(list);
            a_load_ref_reg(list,size,r,tmpreg);
            a_load_reg_ref(list,size,tmpreg,ref);
            free_scratch_reg(list,tmpreg);
          end;
  {$endif para_sizes_known}
      end;


    procedure tcgppc.a_paramaddr_ref(list : taasmoutput;const r : treference;nr : longint);

      var
        ref: treference;
        tmpreg: tregister;

      begin
  {$ifdef para_sizes_known}
        if (nr <= max_param_regs_int) then
          a_loadaddress_ref_reg(list,size,r,param_regs_int[nr])
        else
          begin
            reset_reference(ref);
            ref.base := stack_pointer;
            ref.offset := LinkageAreaSize+para_size_till_now;
            tmpreg := get_scratch_reg(list);
            a_loadaddress_ref_reg(list,size,r,tmpreg);
            a_load_reg_ref(list,size,tmpreg,ref);
            free_scratch_reg(list,tmpreg);
          end;
  {$endif para_sizes_known}
      end;

{ calling a code fragment by name }

    procedure tcgppc.a_call_name(list : taasmoutput;const s : string;
      offset : longint);

      begin
 { save our RTOC register value. Only necessary when doing pointer based    }
 { calls or cross TOC calls, but currently done always                      }
         list.concat(taicpu.op_reg_ref(A_STW,R_RTOC,
           new_reference(stack_pointer,LA_RTOC)));
         list.concat(taicpu.op_sym(A_BL,newasmsymbol(s)));
         list.concat(taicpu.op_reg_ref(A_LWZ,R_RTOC,
           new_reference(stack_pointer,LA_RTOC)));
      end;

{********************** load instructions ********************}

     procedure tcgppc.a_load_const_reg(list : taasmoutput; size: TCGSize; a : aword; reg : TRegister);

       begin
          if (a and $ffff) <> 0 Then
            begin
              list.concat(taicpu.op_reg_const(A_LI,reg,a and $ffff));
              if (longint(a) < low(smallint)) or
                 (longint(a) > high(smallint))  then
                list.concat(taicpu.op_reg_const(A_ADDIS,reg,
                  (a shr 16)+ord(smallint(a and $ffff) < 0)))
            end
          else
            list.concat(taicpu.op_reg_const(A_LIS,reg,a shr 16));
       end;


     procedure tcgppc.a_load_reg_ref(list : taasmoutput; size: TCGSize; reg : tregister;const ref : treference);

       var
         op: TAsmOp;
         ref2: TReference;

       begin
         ref2 := ref;
         FixRef(list,ref2);
         if size in [OS_S8..OS_S16] then
           { storing is the same for signed and unsigned values }
           size := tcgsize(ord(size)-(ord(OS_S8)-ord(OS_8)));
         { 64 bit stuff should be handled separately }
         if size in [OS_64,OS_S64] then
           internalerror(200109236);
         op := storeinstr[size,ref2.index<>R_NO,false];
         a_load_store(list,op,reg,ref2);
       End;


     procedure tcgppc.a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref: treference;reg : tregister);

       var
         op: tasmop;
         tmpreg: tregister;
         ref2, tmpref: treference;

       begin
         if ref.is_immediate then
           a_load_const_reg(list,size,ref.offset,reg)
         else
           begin
             ref2 := ref;
             fixref(list,ref2);
             op := loadinstr[size,ref2.index<>R_NO,false];
             a_load_store(list,op,reg,ref2);
             { sign extend shortint if necessary, since there is no }
             { load instruction that does that automatically (JM)   }
             if size = OS_S8 then
               list.concat(taicpu.op_reg_reg(A_EXTSB,reg,reg));
          end;
       end;


     procedure tcgppc.a_load_reg_reg(list : taasmoutput;size : tcgsize;reg1,reg2 : tregister);

       begin
         list.concat(taicpu.op_reg_reg(A_MR,reg2,reg1));
       end;

     procedure tcgppc.a_load_sym_ofs_reg(list: taasmoutput; const sym: tasmsymbol; ofs: longint; reg: tregister);

       begin
         { can't use op_sym_ofs_reg because sym+ofs can be > 32767!! }
         internalerror(200112293);
       end;

     procedure tcgppc.a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister);

       var
         scratch_register: TRegister;

       begin
         case op of
           OP_DIV, OP_IDIV, OP_IMUL, OP_MUL, OP_ADD, OP_AND, OP_OR, OP_SUB,
           OP_XOR:
             a_op_const_reg_reg(list,op,OS_32,a,reg,reg);
           OP_SHL,OP_SHR,OP_SAR:
             begin
               if (a and 31) <> 0 then
                 list.concat(taicpu.op_reg_reg_const(
                   TOpCG2AsmOpConstLo[op],reg,reg,a and 31));
               if (a shr 5) <> 0 then
                 internalError(68991);
             end
           else internalError(68992);
         end;
       end;


      procedure tcgppc.a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister);

         begin
           a_op_reg_reg_reg(list,op,OS_32,src,dst,dst);
         end;


    procedure tcgppc.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
                       size: tcgsize; a: aword; src, dst: tregister);
      var
        l1,l2: longint;

      var
        oplo, ophi: tasmop;
        scratchreg: tregister;
        useReg: boolean;

      begin
        ophi := TOpCG2AsmOpConstHi[op];
        oplo := TOpCG2AsmOpConstLo[op];
        { constants in a PPC instruction are always interpreted as signed }
        { 16bit values, so if the value is between low(smallint) and      }
        { high(smallint), it's easy                                       }
        if (op in [OP_ADD,OP_SUB,OP_AND,OP_OR,OP_XOR]) then
          begin
            if (longint(a) >= low(smallint)) and
               (longint(a) <= high(smallint)) then
              begin
                list.concat(taicpu.op_reg_reg_const(oplo,dst,src,a));
                exit;
              end;
            { all basic constant instructions also have a shifted form that }
            { works only on the highest 16bits, so if low(a) is 0, we can   }
            { use that one                                                  }
            if (lo(a) = 0) then
              begin
                list.concat(taicpu.op_reg_reg_const(ophi,dst,src,hi(a)));
                exit;
              end;
          end;
        { otherwise, the instructions we can generate depend on the }
        { operation                                                 }
        useReg := false;
        case op of
           OP_DIV, OP_IDIV, OP_IMUL, OP_MUL:
             if (Op = OP_IMUL) and (longint(a) >= -32768) and
                (longint(a) <= 32767) then
               list.concat(taicpu.op_reg_reg_const(A_MULLI,dst,src,a))
             else
               usereg := true;
          OP_ADD,OP_SUB:
            begin
              list.concat(taicpu.op_reg_reg_const(oplo,dst,src,low(a)));
              list.concat(taicpu.op_reg_reg_const(ophi,dst,dst,
                high(a) + ord(smallint(a) < 0)));
            end;
          OP_OR:
            { try to use rlwimi }
            if get_rlwi_const(a,l1,l2) then
              begin
                if src <> dst then
                  list.concat(taicpu.op_reg_reg(A_MR,dst,src));
                scratchreg := get_scratch_reg(list);
                list.concat(taicpu.op_reg_const(A_LI,scratchreg,-1));
                list.concat(taicpu.op_reg_reg_const_const_const(A_RLWIMI,dst,
                  scratchreg,0,l1,l2));
                free_scratch_reg(list,scratchreg);
              end
            else
              useReg := true;
          OP_AND:
            { try to use rlwinm }
            if get_rlwi_const(a,l1,l2) then
              list.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,dst,
                src,0,l1,l2))
            else
              useReg := true;
          OP_XOR:
            useReg := true;
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_reg_reg_const(
                  TOpCG2AsmOpConstLo[Op],dst,src,a and 31));
              if (a shr 5) <> 0 then
                internalError(68991);
            end
          else
            internalerror(200109091);
        end;
        { if all else failed, load the constant in a register and then }
        { perform the operation                                        }
        if useReg then
          begin
            scratchreg := get_scratch_reg(list);
            a_load_const_reg(list,OS_32,a,scratchreg);
            a_op_reg_reg_reg(list,op,OS_32,scratchreg,src,dst);
            free_scratch_reg(list,scratchreg);
          end;
      end;


    procedure tcgppc.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
      size: tcgsize; src1, src2, dst: tregister);

      const
        op_reg_reg_opcg2asmop: array[TOpCG] of tasmop =
          (A_ADD,A_AND,A_DIVWU,A_DIVW,A_MULLW,A_MULLW,A_NEG,A_NOT,A_OR,
           A_SRAW,A_SLW,A_SRW,A_SUB,A_XOR);

       begin
         case op of
           OP_NEG,OP_NOT:
             list.concat(taicpu.op_reg_reg(op_reg_reg_opcg2asmop[op],dst,dst));
           else
             list.concat(taicpu.op_reg_reg_reg(op_reg_reg_opcg2asmop[op],dst,src2,src1));
         end;
       end;


{*************** compare instructructions ****************}

      procedure tcgppc.a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
        l : tasmlabel);

        var
          p: taicpu;
          scratch_register: TRegister;
          signed: boolean;

        begin
          signed := cmp_op in [OC_GT,OC_LT,OC_GTE,OC_LTE];
          if signed then
            if (longint(a) >= low(smallint)) and (longint(a) <= high(smallint)) Then
              list.concat(taicpu.op_reg_reg_const(A_CMPI,R_CR0,reg,a))
            else
              begin
                scratch_register := get_scratch_reg(list);
                a_load_const_reg(list,OS_32,a,scratch_register);
                list.concat(taicpu.op_reg_reg_reg(A_CMP,R_CR0,reg,scratch_register));
                free_scratch_reg(list,scratch_register);
              end
          else
            if (a <= $ffff) then
              list.concat(taicpu.op_reg_reg_const(A_CMPLI,R_CR0,reg,a))
            else
              begin
                scratch_register := get_scratch_reg(list);
                a_load_const_reg(list,OS_32,a,scratch_register);
                list.concat(taicpu.op_reg_reg_reg(A_CMPL,R_CR0,reg,scratch_register));
                free_scratch_reg(list,scratch_register);
              end;
          a_jmp(list,A_BC,TOpCmp2AsmCond[cmp_op],0,l);
        end;


      procedure tcgppc.a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;
        reg1,reg2 : tregister;l : tasmlabel);

        var
          p: taicpu;
            op: tasmop;

        begin
          if cmp_op in [OC_GT,OC_LT,OC_GTE,OC_LTE] then
            op := A_CMP
          else op := A_CMPL;
          list.concat(taicpu.op_reg_reg_reg(op,R_CR0,reg1,reg2));
          a_jmp(list,A_BC,TOpCmp2AsmCond[cmp_op],0,l);
        end;


     procedure tcgppc.a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);

       begin
         a_jmp(list,A_BC,TOpCmp2AsmCond[cond],0,l);
       end;

     procedure tcgppc.a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel);

       var
         c: tasmcond;
       begin
         c := flags_to_cond(f);
         a_jmp(list,A_BC,c.cond,ord(c.cr)-ord(R_CR0),l);
       end;

     procedure tcgppc.g_flags2reg(list: taasmoutput; const f: TResFlags; reg: TRegister);

       var
         testbit: byte;
         bitvalue: boolean;

       begin
         { get the bit to extract from the conditional register + its }
         { requested value (0 or 1)                                   }
         testbit := (ord(f.cr) * 4);
         case f.flag of
           F_EQ,F_NE:
             bitvalue := f.flag = F_EQ;
           F_LT,F_GE:
             begin
               inc(testbit);
               bitvalue := f.flag = F_LT;
             end;
           F_GT,F_LE:
             begin
               inc(testbit,2);
               bitvalue := f.flag = F_GT;
             end;
           else
             internalerror(200112261);
         end;
         { load the conditional register in the destination reg }
         list.concat(taicpu.op_reg(A_MFCR,reg));
         { we will move the bit that has to be tested to bit 31 -> rotate }
         { left by bitpos+1 (remember, this is big-endian!)               }
         testbit := (testbit + 1) and 31;
         { extract bit }
         list.concat(taicpu.op_reg_reg_const_const_const(
           A_RLWINM,reg,reg,testbit,31,31));
         { if we need the inverse, xor with 1 }
         if not bitvalue then
           list.concat(taicpu.op_reg_reg_const(A_XORI,reg,reg,1));
       end;

(*
     procedure tcgppc.g_cond2reg(list: taasmoutput; const f: TAsmCond; reg: TRegister);

       var
         testbit: byte;
         bitvalue: boolean;

       begin
         { get the bit to extract from the conditional register + its }
         { requested value (0 or 1)                                   }
         case f.simple of
           false:
             begin
               { we don't generate this in the compiler }
               internalerror(200109062);
             end;
           true:
             case f.cond of
               C_None:
                 internalerror(200109063);
               C_LT..C_NU:
                 begin
                   testbit := (ord(f.cr) - ord(R_CR0))*4;
                   inc(testbit,AsmCondFlag2BI[f.cond]);
                   bitvalue := AsmCondFlagTF[f.cond];
                 end;
               C_T,C_F,C_DNZT,C_DNZF,C_DZT,C_DZF:
                 begin
                   testbit := f.crbit
                   bitvalue := AsmCondFlagTF[f.cond];
                 end;
               else
                 internalerror(200109064);
             end;
         end;
         { load the conditional register in the destination reg }
         list.concat(taicpu.op_reg_reg(A_MFCR,reg));
         { we will move the bit that has to be tested to bit 31 -> rotate }
         { left by bitpos+1 (remember, this is big-endian!)               }
         if bitpos <> 31 then
           inc(bitpos)
         else
           bitpos := 0;
         { extract bit }
         list.concat(taicpu.op_reg_reg_const_const_const(
           A_RLWINM,reg,reg,bitpos,31,31));
         { if we need the inverse, xor with 1 }
         if not bitvalue then
           list.concat(taicpu.op_reg_reg_const(A_XORI,reg,reg,1));
       end;
*)

{ *********** entry/exit code and address loading ************ }

    procedure tcgppc.g_stackframe_entry(list : taasmoutput;localsize : longint);

      begin
        case target_info.target of
          target_powerpc_macos:
            g_stackframe_entry_mac(list,localsize);
          target_powerpc_linux:
            g_stackframe_entry_sysv(list,localsize)
          else
            internalerror(2204001);
        end;
      end;


    procedure tcgppc.g_stackframe_entry_sysv(list : taasmoutput;localsize : longint);
 { generated the entry code of a procedure/function. Note: localsize is the }
 { sum of the size necessary for local variables and the maximum possible   }
 { combined size of ALL the parameters of a procedure called by the current }
 { one                                                                      }
     var regcounter: TRegister;

      begin
        if (localsize mod 8) <> 0 then internalerror(58991);
 { CR and LR only have to be saved in case they are modified by the current }
 { procedure, but currently this isn't checked, so save them always         }
        { following is the entry code as described in "Altivec Programming }
        { Interface Manual", bar the saving of AltiVec registers           }
        a_reg_alloc(list,stack_pointer);
        a_reg_alloc(list,R_0);
        { allocate registers containing reg parameters }
        for regcounter := R_3 to R_10 do
          a_reg_alloc(list,regcounter);
        { save return address... }
        list.concat(taicpu.op_reg_reg(A_MFSPR,R_0,R_LR));
        { ... in caller's frame }
        list.concat(taicpu.op_reg_ref(A_STW,R_0,new_reference(STACK_POINTER,4)));
        a_reg_dealloc(list,R_0);
        a_reg_alloc(list,R_11);
        { save end of fpr save area }
        list.concat(taicpu.op_reg_reg_const(A_ORI,R_11,STACK_POINTER,0));
        a_reg_alloc(list,R_12);
        { 0 or 8 based on SP alignment }
        list.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,
          R_12,STACK_POINTER,0,28,28));
        { add in stack length }
        list.concat(taicpu.op_reg_reg_const(A_SUBFIC,R_12,R_12,
          -localsize));
        { establish new alignment }
        list.concat(taicpu.op_reg_reg_reg(A_STWUX,STACK_POINTER,STACK_POINTER,R_12));
        a_reg_dealloc(list,R_12);
        { save floating-point registers }
        { !!! has to be optimized: only save registers that are used }
        list.concat(taicpu.op_sym_ofs(A_BL,newasmsymbol('_savefpr_14'),0));
        { compute end of gpr save area }
        list.concat(taicpu.op_reg_reg_const(A_ADDI,R_11,R_11,-144));
        { save gprs and fetch GOT pointer }
        { !!! has to be optimized: only save registers that are used }
        list.concat(taicpu.op_sym_ofs(A_BL,newasmsymbol('_savegpr_14_go'),0));
        a_reg_alloc(list,R_31);
        { place GOT ptr in r31 }
        list.concat(taicpu.op_reg_reg(A_MFSPR,R_31,R_LR));
        { save the CR if necessary ( !!! always done currently ) }
        { still need to find out where this has to be done for SystemV
        a_reg_alloc(list,R_0);
        list.concat(taicpu.op_reg_reg(A_MFSPR,R_0,R_CR);
        list.concat(taicpu.op_reg_ref(A_STW,scratch_register,
          new_reference(stack_pointer,LA_CR)));
        a_reg_dealloc(list,R_0); }
        { save pointer to incoming arguments }
        list.concat(taicpu.op_reg_reg_const(A_ADDI,R_30,R_11,144));
        { now comes the AltiVec context save, not yet implemented !!! }
      end;


    procedure tcgppc.g_stackframe_entry_mac(list : taasmoutput;localsize : longint);
 { generated the entry code of a procedure/function. Note: localsize is the }
 { sum of the size necessary for local variables and the maximum possible   }
 { combined size of ALL the parameters of a procedure called by the current }
 { one                                                                      }
     var regcounter: TRegister;

      begin
        if (localsize mod 8) <> 0 then internalerror(58991);
 { CR and LR only have to be saved in case they are modified by the current }
 { procedure, but currently this isn't checked, so save them always         }
        { following is the entry code as described in "Altivec Programming }
        { Interface Manual", bar the saving of AltiVec registers           }
        a_reg_alloc(list,STACK_POINTER);
        a_reg_alloc(list,R_0);
        { allocate registers containing reg parameters }
        for regcounter := R_3 to R_10 do
          a_reg_alloc(list,regcounter);
        { save return address... }
        list.concat(taicpu.op_reg_reg(A_MFSPR,R_0,R_LR));
        { ... in caller's frame }
        list.concat(taicpu.op_reg_ref(A_STW,R_0,new_reference(STACK_POINTER,8)));
        a_reg_dealloc(list,R_0);
        { save floating-point registers }
        { !!! has to be optimized: only save registers that are used }
        list.concat(taicpu.op_sym_ofs(A_BL,newasmsymbol('_savef14'),0));
        { save gprs in gpr save area }
        { !!! has to be optimized: only save registers that are used }
        list.concat(taicpu.op_reg_ref(A_STMW,R_13,new_reference(STACK_POINTER,-220)));
        { save the CR if necessary ( !!! always done currently ) }
        a_reg_alloc(list,R_0);
        list.concat(taicpu.op_reg_reg(A_MFSPR,R_0,R_CR));
        list.concat(taicpu.op_reg_ref(A_STW,R_0,
          new_reference(stack_pointer,LA_CR)));
        a_reg_dealloc(list,R_0);
        { save pointer to incoming arguments }
        list.concat(taicpu.op_reg_reg_const(A_ORI,R_31,STACK_POINTER,0));
        a_reg_alloc(list,R_12);
        { 0 or 8 based on SP alignment }
        list.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,
          R_12,STACK_POINTER,0,28,28));
        { add in stack length }
        list.concat(taicpu.op_reg_reg_const(A_SUBFIC,R_12,R_12,
          -localsize));
        { establish new alignment }
        list.concat(taicpu.op_reg_reg_reg(A_STWUX,STACK_POINTER,STACK_POINTER,R_12));
        a_reg_dealloc(list,R_12);
        { now comes the AltiVec context save, not yet implemented !!! }
      end;


    procedure tcgppc.g_restore_frame_pointer(list : taasmoutput);

      begin
 { no frame pointer on the PowerPC (maybe there is one in the SystemV ABI?)}
      end;


    procedure tcgppc.g_return_from_proc(list : taasmoutput;parasize : aword);

      begin
        case target_info.target of
          target_powerpc_macos:
            g_return_from_proc_mac(list,parasize);
          target_powerpc_linux:
            g_return_from_proc_sysv(list,parasize)
          else
            internalerror(2204001);
        end;
      end;


     procedure tcgppc.a_loadaddress_ref_reg(list : taasmoutput;const ref2 : treference;r : tregister);

       var tmpreg: tregister;
           ref, tmpref: treference;

       begin
         ref := ref2;
         FixRef(list,ref);
         if assigned(ref.symbol) then
           { add the symbol's value to the base of the reference, and if the }
           { reference doesn't have a base, create one                       }
           begin
             tmpreg := get_scratch_reg(list);
             reset_reference(tmpref);
             tmpref.symbol := ref.symbol;
             tmpref.symaddr := refs_ha;
             tmpref.is_immediate := true;
             if ref.base <> R_NO then
               list.concat(taicpu.op_reg_reg_ref(A_ADDIS,tmpreg,
                 ref.base,newreference(tmpref)))
             else
               list.concat(taicpu.op_reg_ref(A_LIS,tmpreg,
                  newreference(tmpref)));
             ref.base := tmpreg;
             ref.symaddr := refs_l;
             { can be folded with one of the next instructions by the }
             { optimizer probably                                     }
             list.concat(taicpu.op_reg_reg_ref(A_ADDI,tmpreg,tmpreg,
                newreference(tmpref)));
           end;
         if ref.offset <> 0 Then
           if ref.base <> R_NO then
             a_op_const_reg_reg(list,OP_ADD,OS_32,ref.offset,ref.base,r)
  { FixRef makes sure that "(ref.index <> R_NO) and (ref.offset <> 0)" never}
  { occurs, so now only ref.offset has to be loaded                         }
           else a_load_const_reg(list, OS_32, ref.offset, r)
         else
           if ref.index <> R_NO Then
             list.concat(taicpu.op_reg_reg_reg(A_ADD,r,ref.base,ref.index))
           else
             if r <> ref.base then
               list.concat(taicpu.op_reg_reg(A_MR,r,ref.base));
         if assigned(ref.symbol) then
           free_scratch_reg(list,tmpreg);
       end;

{ ************* concatcopy ************ }

    procedure tcgppc.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);

      var
        t: taicpu;
        countreg, tempreg: TRegister;
        src, dst: TReference;
        lab: tasmlabel;
        count, count2: aword;

      begin
        { make sure source and dest are valid }
        src := source;
        fixref(list,src);
        dst := dest;
        fixref(list,dst);
        reset_reference(src);
        reset_reference(dst);
        { load the address of source into src.base }
        src.base := get_scratch_reg(list);
        if loadref then
          a_load_ref_reg(list,OS_32,source,src.base)
        else a_loadaddress_ref_reg(list,source,src.base);
        if delsource then
          del_reference(source);
        { load the address of dest into dst.base }
        dst.base := get_scratch_reg(list);
        a_loadaddress_ref_reg(list,dest,dst.base);
        count := len div 4;
        if count > 3 then
          { generate a loop }
          begin
            { the offsets are zero after the a_loadaddress_ref_reg and just }
            { have to be set to 4. I put an Inc there so debugging may be   }
            { easier (should offset be different from zero here, it will be }
            { easy to notice in the genreated assembler                     }
            Inc(dst.offset,4);
            Inc(src.offset,4);
            list.concat(taicpu.op_reg_reg_const(A_SUBI,src.base,src.base,4));
            list.concat(taicpu.op_reg_reg_const(A_SUBI,dst.base,dst.base,4));
            countreg := get_scratch_reg(list);
            a_load_const_reg(list,OS_32,count-1,countreg);
            { explicitely allocate R_0 since it can be used safely here }
            { (for holding date that's being copied)                    }
            tempreg := R_0;
            a_reg_alloc(list,R_0);
            getlabel(lab);
            a_label(list, lab);
            list.concat(taicpu.op_reg_ref(A_LWZU,tempreg,
              newreference(src)));
            list.concat(taicpu.op_reg_reg_const(A_CMPI,R_CR0,countreg,0));
            list.concat(taicpu.op_reg_ref(A_STWU,tempreg,newreference(dst)));
            list.concat(taicpu.op_reg_reg_const(A_SUBI,countreg,countreg,1));
            a_jmp(list,A_BC,C_NE,0,lab);
            free_scratch_reg(list,countreg);
          end
        else
          { unrolled loop }
          begin
            tempreg := get_scratch_reg(list);
            for count2 := 1 to count do
              begin
                a_load_ref_reg(list,OS_32,src,tempreg);
                a_load_reg_ref(list,OS_32,tempreg,dst);
                inc(src.offset,4);
                inc(dst.offset,4);
              end
          end;
       { copy the leftovers }
       if (len and 2) <> 0 then
         begin
           a_load_ref_reg(list,OS_16,src,tempreg);
           a_load_reg_ref(list,OS_16,tempreg,dst);
           inc(src.offset,2);
           inc(dst.offset,2);
         end;
       if (len and 1) <> 0 then
         begin
           a_load_ref_reg(list,OS_8,src,tempreg);
           a_load_reg_ref(list,OS_8,tempreg,dst);
         end;
       a_reg_dealloc(list,tempreg);
       free_scratch_reg(list,src.base);
       free_scratch_reg(list,dst.base);
      end;

{***************** This is private property, keep out! :) *****************}

    procedure tcgppc.g_return_from_proc_sysv(list : taasmoutput;parasize : aword);

      var
        regcounter: TRegister;

      begin
        { release parameter registers }
        for regcounter := R_3 to R_10 do
          a_reg_dealloc(list,regcounter);
        { AltiVec context restore, not yet implemented !!! }

        { address of gpr save area to r11 }
        list.concat(taicpu.op_reg_reg_const(A_ADDI,R_11,R_31,-144));
        { restore gprs }
        list.concat(taicpu.op_sym_ofs(A_BL,newasmsymbol('_restgpr_14'),0));
        { address of fpr save area to r11 }
        list.concat(taicpu.op_reg_reg_const(A_ADDI,R_11,R_11,144));
        { restore fprs and return }
        list.concat(taicpu.op_sym_ofs(A_BL,newasmsymbol('_restfpr_14_x'),0));
      end;


    procedure tcgppc.g_return_from_proc_mac(list : taasmoutput;parasize : aword);

      var
        regcounter: TRegister;

      begin
        { release parameter registers }
        for regcounter := R_3 to R_10 do
          a_reg_dealloc(list,regcounter);
        { AltiVec context restore, not yet implemented !!! }

        { restore SP }
        list.concat(taicpu.op_reg_reg_const(A_ORI,STACK_POINTER,R_31,0));
        { restore gprs }
        list.concat(taicpu.op_reg_ref(A_LMW,R_13,new_reference(STACK_POINTER,-220)));
        { restore return address ... }
        list.concat(taicpu.op_reg_ref(A_LWZ,R_0,new_reference(STACK_POINTER,8)));
        { ... and return from _restf14 }
        list.concat(taicpu.op_sym_ofs(A_B,newasmsymbol('_restf14'),0));
      end;


    procedure tcgppc.fixref(list: taasmoutput; var ref: treference);

       begin
         If (ref.base <> R_NO) then
           begin
             if (ref.index <> R_NO) and
                ((ref.offset <> 0) or assigned(ref.symbol)) then
               begin
                 if not assigned(ref.symbol) and
                    (cardinal(ref.offset-low(smallint)) <=
                      high(smallint)-low(smallint)) then
                   begin
                     list.concat(taicpu.op_reg_reg_const(
                       A_ADDI,ref.base,ref.base,ref.offset));
                     ref.offset := 0;
                   end
                 else
                   begin
                     list.concat(taicpu.op_reg_reg_reg(
                       A_ADD,ref.base,ref.base,ref.index));
                     ref.index := R_NO;
                   end;
               end
           end
         else
           begin
             ref.base := ref.index;
             ref.index := R_NO
           end
       end;


    { find out whether a is of the form 11..00..11b or 00..11...00. If }
    { that's the case, we can use rlwinm to do an AND operation        }
    function tcgppc.get_rlwi_const(a: longint; var l1, l2: longint): boolean;

      var
        temp, testbit: longint;
        compare: boolean;

      begin
        get_rlwi_const := false;
        { start with the lowest bit }
        testbit := 1;
        { check its value }
        compare := boolean(a and testbit);
        { find out how long the run of bits with this value is            }
        { (it's impossible that all bits are 1 or 0, because in that case }
        { this function wouldn't have been called)                        }
        l1 := 31;
        while (((a and testbit) <> 0) = compare) do
          begin
            testbit := testbit shl 1;
            dec(l1);
          end;

        { check the length of the run of bits that comes next }
        compare := not compare;
        l2 := l1;
        while (((a and testbit) <> 0) = compare) and
               (l2 >= 0) do
          begin
            testbit := testbit shl 1;
            dec(l2);
          end;

        { and finally the check whether the rest of the bits all have the }
        { same value                                                      }
        compare := not compare;
        temp := l2;
        if temp >= 0 then
          if (a shr (31-temp)) <> ((-ord(compare)) shr (31-temp)) then
            exit;

        { we have done "not(not(compare))", so compare is back to its   }
        { initial value. If the lowest bit was 0, a is of the form      }
        { 00..11..00 and we need "rlwinm reg,reg,0,l2+1,l1", (+1        }
        { because l2 now contains the position of the last zero of the  }
        { first run instead of that of the first 1) so switch l1 and l2 }
        { in that case (we will generate "rlwinm reg,reg,0,l1,l2")      }
        if not compare then
          begin
            temp := l1;
            l1 := l2+1;
            l2 := temp;
          end
        else
          { otherwise, l1 currently contains the position of the last   }
          { zero instead of that of the first 1 of the second run -> +1 }
          inc(l1);
        { the following is the same as "if l1 = -1 then l1 := 31;" }
        l1 := l1 and 31;
        l2 := l2 and 31;
        get_rlwi_const := true;
      end;

    procedure tcgppc.a_load_store(list:taasmoutput;op: tasmop;reg:tregister;
       ref: treference);

      var
        tmpreg: tregister;
        tmpref: treference;

      begin
        if assigned(ref.symbol) then
          begin
            tmpreg := get_scratch_reg(list);
            reset_reference(tmpref);
            tmpref.symbol := ref.symbol;
            tmpref.symaddr := refs_ha;
            tmpref.is_immediate := true;
            if ref.base <> R_NO then
              list.concat(taicpu.op_reg_reg_ref(A_ADDIS,tmpreg,
                ref.base,newreference(tmpref)))
            else
              list.concat(taicpu.op_reg_ref(A_LIS,tmpreg,
                 newreference(tmpref)));
            ref.base := tmpreg;
            ref.symaddr := refs_l;
          end;
        list.concat(taicpu.op_reg_ref(op,reg,newreference(ref)));
        if assigned(ref.symbol) then
          free_scratch_reg(list,tmpreg);
      end;


    procedure tcgppc.a_jmp(list: taasmoutput; op: tasmop; c: tasmcondflag;
                crval: longint; l: tasmlabel);
      var
        p: taicpu;

      begin
        p := taicpu.op_sym(op,newasmsymbol(l.name));
        create_cond_norm(c,crval,p.condition);
        p.is_jmp := true;
        list.concat(p)
      end;

begin
  cg := tcgppc.create;
end.
{
  $Log$
  Revision 1.11  2002-01-02 14:53:04  jonas
    * fixed small bug in a_jmp_flags

  Revision 1.10  2001/12/30 17:24:48  jonas
    * range checking is now processor independent (part in cgobj, part in    cg64f32) and should work correctly again (it needed some changes after    the changes of the low and high of tordef's to int64)  * maketojumpbool() is now processor independent (in ncgutil)  * getregister32 is now called getregisterint

  Revision 1.9  2001/12/29 15:28:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.8  2001/10/28 14:16:49  jonas
    * small fixes

  Revision 1.7  2001/09/29 21:33:30  jonas
    * small optimization

  Revision 1.6  2001/09/28 20:40:05  jonas
    * several additions, almost complete (only some problems with resflags left)

  Revision 1.5  2001/09/16 10:33:21  jonas
    * some fixes to operations with constants

  Revision 1.3  2001/09/06 15:25:55  jonas
    * changed type of tcg from object to class ->  abstract methods are now
      a lot cleaner :)
    + more updates: load_*_loc methods, op_*_* methods, g_flags2reg method
      (if possible with generic implementation and necessary ppc
       implementations)
    * worked a bit further on cgflw, now working on exitnode

  Revision 1.2  2001/09/05 20:21:03  jonas
    * new cgflow based on n386flw with all nodes until forn "translated"
    + a_cmp_loc_*_label methods for tcg
    + base implementatino for a_cmp_ref_*_label methods
    * small bugfixes to powerpc cg

  Revision 1.1  2001/08/26 13:31:04  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.2  2001/08/26 13:29:33  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.1  2000/07/13 06:30:12  michael
    + Initial import

  Revision 1.12  2000/04/22 14:25:04  jonas
    * aasm.pas: pai_align instead of pai_align_abstract if cpu <> i386
    + systems.pas: info for macos/ppc
    * new/cgobj.pas: compiles again without newst define
    * new/powerpc/cgcpu: generate different entry/exit code depending on
      whether target_os is MacOs or Linux

  Revision 1.11  2000/01/07 01:14:57  peter
    * updated copyright to 2000

  Revision 1.10  1999/12/24 22:48:10  jonas
    * compiles again

  Revision 1.9  1999/11/05 07:05:56  jonas
    + a_jmp_cond()

  Revision 1.8  1999/10/24 09:22:18  jonas
    + entry/exitcode for SystemV (Linux) and AIX/Mac from the Altivec
      PIM (no AltiVec support yet though)
    * small fix to the a_cmp_* methods

  Revision 1.7  1999/10/20 12:23:24  jonas
    * fixed a_loadaddress_ref_reg (mentioned as ToDo in rev. 1.5)
    * small bugfix in a_load_store

  Revision 1.6  1999/09/15 20:35:47  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.5  1999/09/03 13:14:11  jonas
    + implemented some parameter passing methods, but they require
      some more helper routines
    * fix for loading symbol addresses (still needs to be done in a_loadaddress)
    * several changes to the way conditional branches are handled

  Revision 1.4  1999/08/26 14:53:41  jonas
    * first implementation of concatcopy (requires 4 scratch regs)

  Revision 1.3  1999/08/25 12:00:23  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.2  1999/08/18 17:05:57  florian
    + implemented initilizing of data for the new code generator
      so it should compile now simple programs

  Revision 1.1  1999/08/06 16:41:11  jonas
    * PowerPC compiles again, several routines implemented in cgcpu.pas
    * added constant to cpubase of alpha and powerpc for maximum
      number of operands
}
