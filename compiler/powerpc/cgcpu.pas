{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

  interface

    uses
       cgbase,cgobj,
       aasmbase,aasmcpu,aasmtai,
       cpubase,cpuinfo,node,cg64f32,cginfo;

    type
      tcgppc = class(tcg)
        { passing parameters, per default the parameter is pushed }
        { nr gives the number of the parameter (enumerated from   }
        { left to right), this allows to move the parameter to    }
        { register, if the cpu supports register calling          }
        { conventions                                             }
        procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);override;
        procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);override;
        procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);override;


        procedure a_call_name(list : taasmoutput;const s : string);override;
        procedure a_call_reg(list : taasmoutput;reg: tregister); override;
        procedure a_call_ref(list : taasmoutput;const ref : treference);override;

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
        procedure a_load_reg_reg(list : taasmoutput;fromsize, tosize : tcgsize;reg1,reg2 : tregister);override;

        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list: taasmoutput; reg1, reg2: tregister); override;
        procedure a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference); override;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
          l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;

        procedure a_jmp_always(list : taasmoutput;l: tasmlabel); override;
        procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: TResFlags; reg: TRegister); override;


        procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
        procedure g_return_from_proc(list : taasmoutput;parasize : aword); override;
        procedure g_restore_frame_pointer(list : taasmoutput);override;

        procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);override;

        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;

        procedure g_overflowcheck(list: taasmoutput; const p: tnode); override;
        { find out whether a is of the form 11..00..11b or 00..11...00. If }
        { that's the case, we can use rlwinm to do an AND operation        }
        function get_rlwi_const(a: aword; var l1, l2: longint): boolean;

        procedure g_save_standard_registers(list : taasmoutput; usedinproc : tregisterset);override;
        procedure g_restore_standard_registers(list : taasmoutput; usedinproc : tregisterset);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);override;

        procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);

      private

        procedure g_stackframe_entry_sysv(list : taasmoutput;localsize : longint);
        procedure g_return_from_proc_sysv(list : taasmoutput;parasize : aword);
        procedure g_stackframe_entry_mac(list : taasmoutput;localsize : longint);
        procedure g_return_from_proc_mac(list : taasmoutput;parasize : aword);


        { Make sure ref is a valid reference for the PowerPC and sets the }
        { base to the value of the index if (base = R_NO).                }
        { Returns true if the reference contained a base, index and an    }
        { offset or symbol, in which case the base will have been changed }
        { to a tempreg (which has to be freed by the caller) containing   }
        { the sum of part of the original reference                       }
        function fixref(list: taasmoutput; var ref: treference): boolean;

        { returns whether a reference can be used immediately in a powerpc }
        { instruction                                                      }
        function issimpleref(const ref: treference): boolean;

        { contains the common code of a_load_reg_ref and a_load_ref_reg }
        procedure a_load_store(list:taasmoutput;op: tasmop;reg:tregister;
                    ref: treference);

        { creates the correct branch instruction for a given combination }
        { of asmcondflags and destination addressing mode                }
        procedure a_jmp(list: taasmoutput; op: tasmop;
                        c: tasmcondflag; crval: longint; l: tasmlabel);

     end;

     tcg64fppc = class(tcg64f32)
       procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);override;
       procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);override;
       procedure a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : qword;regsrc,regdst : tregister64);override;
       procedure a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);override;
     end;


const
  TOpCG2AsmOpConstLo: Array[topcg] of TAsmOp = (A_NONE,A_ADDI,A_ANDI_,A_DIVWU,
                        A_DIVW,A_MULLW, A_MULLW, A_NONE,A_NONE,A_ORI,
                        A_SRAWI,A_SLWI,A_SRWI,A_SUBI,A_XORI);
  TOpCG2AsmOpConstHi: Array[topcg] of TAsmOp = (A_NONE,A_ADDIS,A_ANDIS_,
                        A_DIVWU,A_DIVW, A_MULLW,A_MULLW,A_NONE,A_NONE,
                        A_ORIS,A_NONE, A_NONE,A_NONE,A_SUBIS,A_XORIS);

  TOpCmp2AsmCond: Array[topcmp] of TAsmCondFlag = (C_NONE,C_EQ,C_GT,
                       C_LT,C_GE,C_LE,C_NE,C_LE,C_LT,C_GE,C_GT);

  implementation

    uses
       globtype,globals,verbose,systems,cutils,symconst,symdef,rgobj,tgobj,cpupi;

{ parameter passing... Still needs extra support from the processor }
{ independent code generator                                        }

    procedure tcgppc.a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);

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


    procedure tcgppc.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);

      var
        ref: treference;
        tmpreg: tregister;

      begin
        case locpara.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_load_ref_reg(list,size,r,locpara.register);
          LOC_REFERENCE:
            begin
               reference_reset(ref);
               ref.base:=locpara.reference.index;
               ref.offset:=locpara.reference.offset;
               tmpreg := get_scratch_reg_int(list);
               a_load_ref_reg(list,size,r,tmpreg);
               a_load_reg_ref(list,size,tmpreg,ref);
               free_scratch_reg(list,tmpreg);
            end;
          LOC_FPUREGISTER,LOC_CFPUREGISTER:
            case size of
               OS_32:
                 a_loadfpu_ref_reg(list,OS_F32,r,locpara.register);
               OS_64:
                 a_loadfpu_ref_reg(list,OS_F64,r,locpara.register);
               else
                 internalerror(2002072801);
            end;
          else
            internalerror(2002081103);
        end;
        if locpara.sp_fixup<>0 then
          internalerror(2002081104);
      end;


    procedure tcgppc.a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);

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
                tmpreg := get_scratch_reg_address(list);
                a_loadaddr_ref_reg(list,r,tmpreg);
                a_load_reg_ref(list,OS_ADDR,tmpreg,ref);
                free_scratch_reg(list,tmpreg);
              end;
            else
              internalerror(2002080701);
         end;
      end;


    { calling a code fragment by name }
    procedure tcgppc.a_call_name(list : taasmoutput;const s : string);
      var
        href : treference;
      begin
         list.concat(taicpu.op_sym(A_BL,objectlibrary.newasmsymbol(s)));
         if target_info.system=system_powerpc_macos then
           list.concat(taicpu.op_none(A_NOP));
         procinfo.flags:=procinfo.flags or pi_do_call;
      end;


    procedure tcgppc.a_call_reg(list : taasmoutput;reg: tregister);
      begin
        list.concat(taicpu.op_reg(A_MTCTR,reg));
        list.concat(taicpu.op_none(A_BCTRL));
        if target_info.system=system_powerpc_macos then
          list.concat(taicpu.op_none(A_NOP));
        procinfo.flags:=procinfo.flags or pi_do_call;
      end;


    { calling a code fragment through a reference }
    procedure tcgppc.a_call_ref(list : taasmoutput;const ref : treference);
      var
        tmpreg : tregister;
      begin
        tmpreg := get_scratch_reg_int(list);
        a_load_ref_reg(list,OS_ADDR,ref,tmpreg);
        list.concat(taicpu.op_reg(A_MTCTR,tmpreg));
        free_scratch_reg(list,tmpreg);
        list.concat(taicpu.op_none(A_BCTRL));
        if target_info.system=system_powerpc_macos then
          list.concat(taicpu.op_none(A_NOP));
        procinfo.flags:=procinfo.flags or pi_do_call;
      end;

{********************** load instructions ********************}

     procedure tcgppc.a_load_const_reg(list : taasmoutput; size: TCGSize; a : aword; reg : TRegister);

       begin
          if not(size in [OS_8,OS_S8,OS_16,OS_S16,OS_32,OS_S32]) then
            internalerror(2002090902);
          if (longint(a) >= low(smallint)) and
             (longint(a) <= high(smallint)) then
            list.concat(taicpu.op_reg_const(A_LI,reg,smallint(a)))
          else if ((a and $ffff) <> 0) then
            begin
              list.concat(taicpu.op_reg_const(A_LI,reg,smallint(a and $ffff)));
              if ((a shr 16) <> 0) or
                 (smallint(a and $ffff) < 0) then
                list.concat(taicpu.op_reg_reg_const(A_ADDIS,reg,reg,
                  smallint((a shr 16)+ord(smallint(a and $ffff) < 0))))
            end
          else
            list.concat(taicpu.op_reg_const(A_LIS,reg,smallint(a shr 16)));
       end;


     procedure tcgppc.a_load_reg_ref(list : taasmoutput; size: TCGSize; reg : tregister;const ref : treference);

       const
         StoreInstr: Array[OS_8..OS_32,boolean, boolean] of TAsmOp =
                                 { indexed? updating?}
                    (((A_STB,A_STBU),(A_STBX,A_STBUX)),
                     ((A_STH,A_STHU),(A_STHX,A_STHUX)),
                     ((A_STW,A_STWU),(A_STWX,A_STWUX)));
       var
         op: TAsmOp;
         ref2: TReference;
         freereg: boolean;
       begin
         ref2 := ref;
         freereg := fixref(list,ref2);
         if size in [OS_S8..OS_S16] then
           { storing is the same for signed and unsigned values }
           size := tcgsize(ord(size)-(ord(OS_S8)-ord(OS_8)));
         { 64 bit stuff should be handled separately }
         if size in [OS_64,OS_S64] then
           internalerror(200109236);
         op := storeinstr[tcgsize2unsigned[size],ref2.index<>R_NO,false];
         a_load_store(list,op,reg,ref2);
         if freereg then
           cg.free_scratch_reg(list,ref2.base);
       End;


     procedure tcgppc.a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref: treference;reg : tregister);

       const
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
       var
         op: tasmop;
         tmpreg: tregister;
         ref2, tmpref: treference;
         freereg: boolean;

       begin
          if not(size in [OS_8,OS_S8,OS_16,OS_S16,OS_32,OS_S32]) then
            internalerror(2002090902);
          ref2 := ref;
          freereg := fixref(list,ref2);
          op := loadinstr[size,ref2.index<>R_NO,false];
          a_load_store(list,op,reg,ref2);
          if freereg then
            free_scratch_reg(list,ref2.base);
          { sign extend shortint if necessary, since there is no }
          { load instruction that does that automatically (JM)   }
          if size = OS_S8 then
            list.concat(taicpu.op_reg_reg(A_EXTSB,reg,reg));
       end;


     procedure tcgppc.a_load_reg_reg(list : taasmoutput;fromsize, tosize : tcgsize;reg1,reg2 : tregister);

       begin
         if (reg1 <> reg2) or
            (tcgsize2size[tosize] < tcgsize2size[fromsize]) or
            ((tcgsize2size[tosize] = tcgsize2size[fromsize]) and
             (tosize <> fromsize) and
             not(fromsize in [OS_32,OS_S32])) then
           begin
             case fromsize of
               OS_8:
                 list.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,
                   reg2,reg1,0,31-8+1,31));
               OS_S8:
                 list.concat(taicpu.op_reg_reg(A_EXTSB,reg2,reg1));
               OS_16:
                 list.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,
                   reg2,reg1,0,31-16+1,31));
               OS_S16:
                 list.concat(taicpu.op_reg_reg(A_EXTSH,reg2,reg1));
               OS_32,OS_S32:
                 list.concat(taicpu.op_reg_reg(A_MR,reg2,reg1));
               else internalerror(2002090901);
             end;
           end;
       end;


     procedure tcgppc.a_loadfpu_reg_reg(list: taasmoutput; reg1, reg2: tregister);

       begin
         list.concat(taicpu.op_reg_reg(A_FMR,reg2,reg1));
       end;


     procedure tcgppc.a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister);

       const
         FpuLoadInstr: Array[OS_F32..OS_F64,boolean, boolean] of TAsmOp =
                          { indexed? updating?}
                    (((A_LFS,A_LFSU),(A_LFSX,A_LFSUX)),
                     ((A_LFD,A_LFDU),(A_LFDX,A_LFDUX)));
       var
         op: tasmop;
         ref2: treference;
         freereg: boolean;

       begin
          { several functions call this procedure with OS_32 or OS_64 }
          { so this makes life easier (FK)                            }
          case size of
             OS_32,OS_F32:
               size:=OS_F32;
             OS_64,OS_F64:
               size:=OS_F64;
             else
               internalerror(200201121);
          end;
         ref2 := ref;
         freereg := fixref(list,ref2);
         op := fpuloadinstr[size,ref2.index <> R_NO,false];
         a_load_store(list,op,reg,ref2);
         if freereg then
           cg.free_scratch_reg(list,ref2.base);
       end;


     procedure tcgppc.a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference);

       const
         FpuStoreInstr: Array[OS_F32..OS_F64,boolean, boolean] of TAsmOp =
                            { indexed? updating?}
                    (((A_STFS,A_STFSU),(A_STFSX,A_STFSUX)),
                     ((A_STFD,A_STFDU),(A_STFDX,A_STFDUX)));
       var
         op: tasmop;
         ref2: treference;
         freereg: boolean;

       begin
         if not(size in [OS_F32,OS_F64]) then
           internalerror(200201122);
         ref2 := ref;
         freereg := fixref(list,ref2);
         op := fpustoreinstr[size,ref2.index <> R_NO,false];
         a_load_store(list,op,reg,ref2);
         if freereg then
           cg.free_scratch_reg(list,ref2.base);
       end;


     procedure tcgppc.a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister);

       var
         scratch_register: TRegister;

       begin
         a_op_const_reg_reg(list,op,OS_32,a,reg,reg);
       end;


      procedure tcgppc.a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister);

         begin
           a_op_reg_reg_reg(list,op,OS_32,src,dst,dst);
         end;


    procedure tcgppc.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
                       size: tcgsize; a: aword; src, dst: tregister);
      var
        l1,l2: longint;
        oplo, ophi: tasmop;
        scratchreg: tregister;
        useReg, gotrlwi: boolean;


        procedure do_lo_hi;
          begin
            list.concat(taicpu.op_reg_reg_const(oplo,dst,src,word(a)));
            list.concat(taicpu.op_reg_reg_const(ophi,dst,dst,word(a shr 16)));
          end;

      begin
        if op = OP_SUB then
          begin
{$ifopt q+}
{$q-}
{$define overflowon}
{$endif}
            a_op_const_reg_reg(list,OP_ADD,size,aword(-a),src,dst);
{$ifdef overflowon}
{$q+}
{$undef overflowon}
{$endif}
            exit;
          end;
        ophi := TOpCG2AsmOpConstHi[op];
        oplo := TOpCG2AsmOpConstLo[op];
        gotrlwi := get_rlwi_const(a,l1,l2);
        if (op in [OP_AND,OP_OR,OP_XOR]) then
          begin
            if (a = 0) then
              begin
                if op = OP_AND then
                  list.concat(taicpu.op_reg_const(A_LI,dst,0));
                exit;
              end
            else if (a = high(aword)) then
              begin
                case op of
                  OP_OR:
                    list.concat(taicpu.op_reg_const(A_LI,dst,-1));
                  OP_XOR:
                    list.concat(taicpu.op_reg_reg(A_NOT,dst,src));
                end;
                exit;
              end
            else if (a <= high(word)) and
               ((op <> OP_AND) or
                not gotrlwi) then
              begin
                list.concat(taicpu.op_reg_reg_const(oplo,dst,src,word(a)));
                exit;
              end;
            { all basic constant instructions also have a shifted form that }
            { works only on the highest 16bits, so if lo(a) is 0, we can    }
            { use that one                                                  }
            if (word(a) = 0) and
               (not(op = OP_AND) or
                not gotrlwi) then
              begin
                list.concat(taicpu.op_reg_reg_const(ophi,dst,src,word(a shr 16)));
                exit;
              end;
          end
        else if (op = OP_ADD) then
          if a = 0 then
            exit
          else if (longint(a) >= low(smallint)) and
              (longint(a) <= high(smallint)) then
             begin
               list.concat(taicpu.op_reg_reg_const(A_ADDI,dst,src,smallint(a)));
               exit;
             end;

        { otherwise, the instructions we can generate depend on the }
        { operation                                                 }
        useReg := false;
        case op of
          OP_DIV,OP_IDIV:
             if (a = 0) then
               internalerror(200208103)
             else if (a = 1) then
               begin
                 a_load_reg_reg(list,OS_INT,OS_INT,src,dst);
                 exit
               end
            else if ispowerof2(a,l1) then
              begin
                case op of
                  OP_DIV:
                    list.concat(taicpu.op_reg_reg_const(A_SRWI,dst,src,l1));
                  OP_IDIV:
                    begin
                       list.concat(taicpu.op_reg_reg_const(A_SRAWI,dst,src,l1));
                       list.concat(taicpu.op_reg_reg(A_ADDZE,dst,dst));
                    end;
                end;
                exit;
              end
            else
              usereg := true;
           OP_IMUL, OP_MUL:
             if (a = 0) then
               begin
                 list.concat(taicpu.op_reg_const(A_LI,dst,0));
                 exit
               end
             else if (a = 1) then
               begin
                 a_load_reg_reg(list,OS_INT,OS_INT,src,dst);
                 exit
               end
             else if ispowerof2(a,l1) then
               list.concat(taicpu.op_reg_reg_const(A_SLWI,dst,src,l1))
             else if (longint(a) >= low(smallint)) and
                (longint(a) <= high(smallint)) then
               list.concat(taicpu.op_reg_reg_const(A_MULLI,dst,src,smallint(a)))
             else
               usereg := true;
          OP_ADD:
            begin
              list.concat(taicpu.op_reg_reg_const(oplo,dst,src,smallint(a)));
              list.concat(taicpu.op_reg_reg_const(ophi,dst,dst,
                smallint((a shr 16) + ord(smallint(a) < 0))));
            end;
          OP_OR:
            { try to use rlwimi }
            if gotrlwi and
               (src = dst) then
              begin
                scratchreg := get_scratch_reg_int(list);
                list.concat(taicpu.op_reg_const(A_LI,scratchreg,-1));
                list.concat(taicpu.op_reg_reg_const_const_const(A_RLWIMI,dst,
                  scratchreg,0,l1,l2));
                free_scratch_reg(list,scratchreg);
              end
            else
              do_lo_hi;
          OP_AND:
            { try to use rlwinm }
            if gotrlwi then
              list.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,dst,
                src,0,l1,l2))
            else
              useReg := true;
          OP_XOR:
            do_lo_hi;
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
            scratchreg := get_scratch_reg_int(list);
            a_load_const_reg(list,OS_32,a,scratchreg);
            a_op_reg_reg_reg(list,op,OS_32,scratchreg,src,dst);
            free_scratch_reg(list,scratchreg);
          end;
      end;


    procedure tcgppc.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
      size: tcgsize; src1, src2, dst: tregister);

      const
        op_reg_reg_opcg2asmop: array[TOpCG] of tasmop =
          (A_NONE,A_ADD,A_AND,A_DIVWU,A_DIVW,A_MULLW,A_MULLW,A_NEG,A_NOT,A_OR,
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
          { in the following case, we generate more efficient code when }
          { signed is true                                              }
          if (cmp_op in [OC_EQ,OC_NE]) and
             (a > $ffff) then
            signed := true;
          if signed then
            if (longint(a) >= low(smallint)) and (longint(a) <= high(smallint)) Then
              list.concat(taicpu.op_reg_reg_const(A_CMPWI,R_CR0,reg,longint(a)))
            else
              begin
                scratch_register := get_scratch_reg_int(list);
                a_load_const_reg(list,OS_32,a,scratch_register);
                list.concat(taicpu.op_reg_reg_reg(A_CMPW,R_CR0,reg,scratch_register));
                free_scratch_reg(list,scratch_register);
              end
          else
            if (a <= $ffff) then
              list.concat(taicpu.op_reg_reg_const(A_CMPLWI,R_CR0,reg,a))
            else
              begin
                scratch_register := get_scratch_reg_int(list);
                a_load_const_reg(list,OS_32,a,scratch_register);
                list.concat(taicpu.op_reg_reg_reg(A_CMPLW,R_CR0,reg,scratch_register));
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
            op := A_CMPW
          else op := A_CMPLW;
          list.concat(taicpu.op_reg_reg_reg(op,R_CR0,reg1,reg2));
          a_jmp(list,A_BC,TOpCmp2AsmCond[cmp_op],0,l);
        end;


     procedure tcgppc.g_save_standard_registers(list : taasmoutput; usedinproc : tregisterset);
       begin
         {$warning FIX ME}
       end;

     procedure tcgppc.g_restore_standard_registers(list : taasmoutput; usedinproc : tregisterset);
       begin
         {$warning FIX ME}
       end;

     procedure tcgppc.g_save_all_registers(list : taasmoutput);
       begin
         {$warning FIX ME}
       end;

     procedure tcgppc.g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);
       begin
         {$warning FIX ME}
       end;

     procedure tcgppc.a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);

       begin
         a_jmp(list,A_BC,TOpCmp2AsmCond[cond],0,l);
       end;

     procedure tcgppc.a_jmp_always(list : taasmoutput;l: tasmlabel);

       begin
         a_jmp(list,A_B,C_None,0,l);
       end;

     procedure tcgppc.a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel);

       var
         c: tasmcond;
       begin
         c := flags_to_cond(f);
         a_jmp(list,A_BC,c.cond,ord(c.cr)-ord(R_CR0),l);
       end;

     procedure tcgppc.g_flags2reg(list: taasmoutput; size: TCgSize; const f: TResFlags; reg: TRegister);

       var
         testbit: byte;
         bitvalue: boolean;

       begin
         { get the bit to extract from the conditional register + its }
         { requested value (0 or 1)                                   }
         testbit := ((ord(f.cr)-ord(R_CR0)) * 4);
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
         { we will move the bit that has to be tested to bit 0 by rotating }
         { left                                                            }
         testbit := (32 - testbit) and 31;
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
        case target_info.system of
          system_powerpc_macos:
            g_stackframe_entry_mac(list,localsize);
          system_powerpc_linux:
            g_stackframe_entry_sysv(list,localsize)
          else
            internalerror(2204001);
        end;
      end;

    procedure tcgppc.g_return_from_proc(list : taasmoutput;parasize : aword);

      begin
        case target_info.system of
          system_powerpc_macos:
            g_return_from_proc_mac(list,parasize);
          system_powerpc_linux:
            g_return_from_proc_sysv(list,parasize)
          else
            internalerror(2204001);
        end;
      end;


    procedure tcgppc.g_stackframe_entry_sysv(list : taasmoutput;localsize : longint);
     { generated the entry code of a procedure/function. Note: localsize is the }
     { sum of the size necessary for local variables and the maximum possible   }
     { combined size of ALL the parameters of a procedure called by the current }
     { one                                                                      }
     var regcounter,firstregfpu,firstreggpr : TRegister;
         href : treference;
         usesfpr,usesgpr,gotgot : boolean;
         parastart : aword;
         offset : aword;

      begin
        { we do our own localsize calculation }
        localsize:=0;
        { CR and LR only have to be saved in case they are modified by the current }
        { procedure, but currently this isn't checked, so save them always         }
        { following is the entry code as described in "Altivec Programming }
        { Interface Manual", bar the saving of AltiVec registers           }
        a_reg_alloc(list,STACK_POINTER_REG);
        a_reg_alloc(list,R_0);
        { allocate registers containing reg parameters }
        for regcounter := R_3 to R_10 do
          a_reg_alloc(list,regcounter);

        usesfpr:=false;
        for regcounter:=R_F14 to R_F31 do
          if regcounter in rg.usedbyproc then
            begin
               usesfpr:=true;
               firstregfpu:=regcounter;
               break;
            end;

        usesgpr:=false;
        for regcounter:=R_14 to R_31 do
          if regcounter in rg.usedbyproc then
            begin
               usesgpr:=true;
               firstreggpr:=regcounter;
               break;
            end;

        { save link register? }
        if (procinfo.flags and pi_do_call)<>0 then
          begin
             { save return address... }
             list.concat(taicpu.op_reg(A_MFLR,R_0));
             { ... in caller's rframe }
             reference_reset_base(href,STACK_POINTER_REG,4);
             list.concat(taicpu.op_reg_ref(A_STW,R_0,href));
             a_reg_dealloc(list,R_0);
          end;

        if usesfpr or usesgpr then
          begin
             a_reg_alloc(list,R_11);
             { save end of fpr save area }
             list.concat(taicpu.op_reg_reg_const(A_ORI,R_11,STACK_POINTER_REG,0));
          end;

        { calculate the size of the locals }
        if usesgpr then
          inc(localsize,(ord(R_31)-ord(firstreggpr)+1)*4);
        if usesfpr then
          inc(localsize,(ord(R_F31)-ord(firstregfpu)+1)*8);

        { align to 16 bytes }
        localsize:=align(localsize,16);

        inc(localsize,tg.lasttemp);

        localsize:=align(localsize,16);

        tppcprocinfo(procinfo).localsize:=localsize;

        reference_reset_base(href,R_1,-localsize);
        a_load_store(list,A_STWU,R_1,href);

        { no GOT pointer loaded yet }
        gotgot:=false;
        if usesfpr then
          begin
             { save floating-point registers
             if (cs_create_pic in aktmoduleswitches) and not(usesgpr) then
               begin
                  a_call_name(objectlibrary.newasmsymbol('_savefpr_'+tostr(ord(firstregfpu)-ord(R_F14)+14)+'_g');
                  gotgot:=true;
               end
             else
               a_call_name(objectlibrary.newasmsymbol('_savefpr_'+tostr(ord(firstregfpu)-ord(R_F14)+14));
             }
             for regcounter:=firstregfpu to R_F31 do
               if regcounter in rg.usedbyproc then
                 begin
                    { reference_reset_base(href,R_1,-localsize);
                    a_load_store(list,A_STWU,R_1,href);
                    }
                 end;

             { compute end of gpr save area }
             list.concat(taicpu.op_reg_reg_const(A_ADDI,R_11,R_11,-(ord(R_F31)-ord(firstregfpu)+1)*8));
          end;

        { save gprs and fetch GOT pointer }
        if usesgpr then
          begin
             {
             if cs_create_pic in aktmoduleswitches then
               begin
                  a_call_name(objectlibrary.newasmsymbol('_savegpr_'+tostr(ord(firstreggpr)-ord(R_14)+14)+'_g');
                  gotgot:=true;
               end
             else
               a_call_name(objectlibrary.newasmsymbol('_savegpr_'+tostr(ord(firstreggpr)-ord(R_14)+14))
             }
             reference_reset_base(href,R_11,-(ord(R_31)-ord(firstreggpr)+1)*4);
             list.concat(taicpu.op_reg_ref(A_STMW,firstreggpr,href));
          end;

        if usesfpr or usesgpr then
          a_reg_dealloc(list,R_11);

        { PIC code support, }
        if cs_create_pic in aktmoduleswitches then
          begin
             { if we didn't get the GOT pointer till now, we've to calculate it now }
             if not(gotgot) then
               begin
                  {!!!!!!!!!!!!!}
               end;
             a_reg_alloc(list,R_31);
             { place GOT ptr in r31 }
             list.concat(taicpu.op_reg_reg(A_MFSPR,R_31,R_LR));
          end;
        { save the CR if necessary ( !!! always done currently ) }
        { still need to find out where this has to be done for SystemV
        a_reg_alloc(list,R_0);
        list.concat(taicpu.op_reg_reg(A_MFSPR,R_0,R_CR);
        list.concat(taicpu.op_reg_ref(A_STW,scratch_register,
          new_reference(STACK_POINTER_REG,LA_CR)));
        a_reg_dealloc(list,R_0); }
        { now comes the AltiVec context save, not yet implemented !!! }
      end;

    procedure tcgppc.g_return_from_proc_sysv(list : taasmoutput;parasize : aword);

      var
         regcounter,firstregfpu,firstreggpr : TRegister;
         href : treference;
         usesfpr,usesgpr,genret : boolean;

      begin
        { release parameter registers }
        for regcounter := R_3 to R_10 do
          a_reg_dealloc(list,regcounter);
        { AltiVec context restore, not yet implemented !!! }

        usesfpr:=false;
        for regcounter:=R_F14 to R_F31 do
          if regcounter in rg.usedbyproc then
            begin
               usesfpr:=true;
               firstregfpu:=regcounter;
               break;
            end;

        usesgpr:=false;
        for regcounter:=R_14 to R_30 do
          if regcounter in rg.usedbyproc then
            begin
               usesgpr:=true;
               firstreggpr:=regcounter;
               break;
            end;

        { no return (blr) generated yet }
        genret:=true;
        if usesgpr then
          begin
             { address of gpr save area to r11 }
             if usesfpr then
               list.concat(taicpu.op_reg_reg_const(A_ADDI,R_11,R_1,tppcprocinfo(procinfo).localsize-(ord(R_F31)-ord(firstregfpu)+1)*8))
             else
               list.concat(taicpu.op_reg_reg_const(A_ADDI,R_11,R_1,tppcprocinfo(procinfo).localsize));

             { restore gprs }
             { at least for now we use LMW }
             {
             a_call_name(objectlibrary.newasmsymbol('_restgpr_14');
             }
             reference_reset_base(href,R_11,-(ord(R_31)-ord(firstreggpr)+1)*4);
             list.concat(taicpu.op_reg_ref(A_LMW,firstreggpr,href));
          end;

        { restore fprs and return }
        if usesfpr then
          begin
             { address of fpr save area to r11 }
             list.concat(taicpu.op_reg_reg_const(A_ADDI,R_11,R_11,(ord(R_F31)-ord(firstregfpu)+1)*8));
             {
             if (procinfo.flags and pi_do_call)<>0 then
               a_call_name(objectlibrary.newasmsymbol('_restfpr_'+tostr(ord(firstregfpu)-ord(R_F14)+14)+
                 '_x')
             else
               { leaf node => lr haven't to be restored }
               a_call_name('_restfpr_'+tostr(ord(firstregfpu)-ord(R_F14)+14)+
                 '_l');
             genret:=false;
             }
          end;
        { if we didn't generate the return code, we've to do it now }
        if genret then
          begin
             { adjust r1 }
             a_op_const_reg(list,OP_ADD,tppcprocinfo(procinfo).localsize,R_1);
             { load link register? }
             if (procinfo.flags and pi_do_call)<>0 then
               begin
                  reference_reset_base(href,STACK_POINTER_REG,4);
                  list.concat(taicpu.op_reg_ref(A_LWZ,R_0,href));
                  list.concat(taicpu.op_reg(A_MTLR,R_0));
               end;
             list.concat(taicpu.op_none(A_BLR));
          end;
      end;

    function save_regs(list : taasmoutput):longint;
    {Generates code which saves used non-volatile registers in
     the save area right below the address the stackpointer point to.
     Returns the actual used save area size.}

     var regcounter,firstregfpu,firstreggpr: TRegister;
         usesfpr,usesgpr: boolean;
         href : treference;
         offset: integer;

    begin
      usesfpr:=false;
      for regcounter:=R_F14 to R_F31 do
        if regcounter in rg.usedbyproc then
          begin
             usesfpr:=true;
             firstregfpu:=regcounter;
             break;
          end;

      usesgpr:=false;
      for regcounter:=R_13 to R_31 do
        if regcounter in rg.usedbyproc then
          begin
             usesgpr:=true;
             firstreggpr:=regcounter;
             break;
          end;

      offset:= 0;

      { save floating-point registers }
      if usesfpr then
        for regcounter := firstregfpu to R_F31 do
          begin
            offset:= offset - 8;
            reference_reset_base(href, STACK_POINTER_REG, offset);
            list.concat(taicpu.op_reg_ref(A_STFD, regcounter, href));
          end;
        (* Optimiztion in the future:  a_call_name(list,'_savefXX'); *)

      { save gprs in gpr save area }
      if usesgpr then
        if firstreggpr < R_30 then
          begin
            offset:= offset - 4 * (ord(R_31) - ord(firstreggpr) + 1);
            reference_reset_base(href,STACK_POINTER_REG,offset);
            list.concat(taicpu.op_reg_ref(A_STMW,firstreggpr,href));
              {STMW stores multiple registers}
          end
        else
          begin
            for regcounter := firstreggpr to R_31 do
              begin
                offset:= offset - 4;
                reference_reset_base(href, STACK_POINTER_REG, offset);
                list.concat(taicpu.op_reg_ref(A_STW, regcounter, href));
              end;
          end;

      { now comes the AltiVec context save, not yet implemented !!! }

      save_regs:= -offset;
    end;

    procedure restore_regs(list : taasmoutput);
    {Generates code which restores used non-volatile registers from
    the save area right below the address the stackpointer point to.}

     var regcounter,firstregfpu,firstreggpr: TRegister;
         usesfpr,usesgpr: boolean;
         href : treference;
         offset: integer;

    begin
      usesfpr:=false;
      for regcounter:=R_F14 to R_F31 do
        if regcounter in rg.usedbyproc then
          begin
             usesfpr:=true;
             firstregfpu:=regcounter;
             break;
          end;

      usesgpr:=false;
      for regcounter:=R_13 to R_31 do
        if regcounter in rg.usedbyproc then
          begin
             usesgpr:=true;
             firstreggpr:=regcounter;
             break;
          end;

      offset:= 0;

      { restore fp registers }
      if usesfpr then
        for regcounter := firstregfpu to R_F31 do
          begin
            offset:= offset - 8;
            reference_reset_base(href, STACK_POINTER_REG, offset);
            list.concat(taicpu.op_reg_ref(A_LFD, regcounter, href));
          end;
        (* Optimiztion in the future: a_call_name(list,'_restfXX'); *)

      { restore gprs }
      if usesgpr then
        if firstreggpr < R_30 then
          begin
            offset:= offset - 4 * (ord(R_31) - ord(firstreggpr) + 1);
            reference_reset_base(href,STACK_POINTER_REG,offset); //-220
            list.concat(taicpu.op_reg_ref(A_LMW,firstreggpr,href));
              {LMW loads multiple registers}
          end
        else
          begin
            for regcounter := firstreggpr to R_31 do
              begin
                offset:= offset - 4;
                reference_reset_base(href, STACK_POINTER_REG, offset);
                list.concat(taicpu.op_reg_ref(A_LWZ, regcounter, href));
              end;
          end;

      { now comes the AltiVec context restore, not yet implemented !!! }
    end;


    procedure tcgppc.g_stackframe_entry_mac(list : taasmoutput;localsize : longint);
 { generated the entry code of a procedure/function. Note: localsize is the }
 { sum of the size necessary for local variables and the maximum possible   }
 { combined size of ALL the parameters of a procedure called by the current }
 { one                                                                     }

     const
         macosLinkageAreaSize = 24;

     var regcounter: TRegister;
         href : treference;
         registerSaveAreaSize : longint;

      begin
        if (localsize mod 8) <> 0 then internalerror(58991);
        { CR and LR only have to be saved in case they are modified by the current }
        { procedure, but currently this isn't checked, so save them always         }
        { following is the entry code as described in "Altivec Programming }
        { Interface Manual", bar the saving of AltiVec registers           }
        a_reg_alloc(list,STACK_POINTER_REG);
        a_reg_alloc(list,R_0);

        { allocate registers containing reg parameters }
        for regcounter := R_3 to R_10 do
          a_reg_alloc(list,regcounter);
        {TODO: Allocate fp and altivec parameter registers also}

        { save return address in callers frame}
        list.concat(taicpu.op_reg_reg(A_MFSPR,R_0,R_LR));
        { ... in caller's frame }
        reference_reset_base(href,STACK_POINTER_REG,8);
        list.concat(taicpu.op_reg_ref(A_STW,R_0,href));
        a_reg_dealloc(list,R_0);

        { save non-volatile registers in callers frame}
        registerSaveAreaSize:= save_regs(list);

        { save the CR if necessary in callers frame ( !!! always done currently ) }
        a_reg_alloc(list,R_0);
        list.concat(taicpu.op_reg_reg(A_MFSPR,R_0,R_CR));
        reference_reset_base(href,stack_pointer_reg,LA_CR);
        list.concat(taicpu.op_reg_ref(A_STW,R_0,href));
        a_reg_dealloc(list,R_0);

        (*
        { save pointer to incoming arguments }
        list.concat(taicpu.op_reg_reg_const(A_ORI,R_31,STACK_POINTER_REG,0));
        *)

        (*
        a_reg_alloc(list,R_12);

        { 0 or 8 based on SP alignment }
        list.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,
          R_12,STACK_POINTER_REG,0,28,28));
        { add in stack length }
        list.concat(taicpu.op_reg_reg_const(A_SUBFIC,R_12,R_12,
          -localsize));
        { establish new alignment }
        list.concat(taicpu.op_reg_reg_reg(A_STWUX,STACK_POINTER_REG,STACK_POINTER_REG,R_12));

        a_reg_dealloc(list,R_12);
        *)

        { allocate stack frame }
        localsize:= align(localsize + macosLinkageAreaSize + registerSaveAreaSize, 16);
        inc(localsize,tg.lasttemp);
        localsize:=align(localsize,16);
        tppcprocinfo(procinfo).localsize:=localsize;

        reference_reset_base(href,R_1,-localsize);
        a_load_store(list,A_STWU,R_1,href);
          { this also stores the old stack pointer in the new stack frame }
      end;

    procedure tcgppc.g_return_from_proc_mac(list : taasmoutput;parasize : aword);

      var
        regcounter: TRegister;
        href : treference;
      begin
        { release parameter registers }
        for regcounter := R_3 to R_10 do
          a_reg_dealloc(list,regcounter);
        {TODO: Release fp and altivec parameter registers also}

        a_reg_alloc(list,R_0);

        { restore stack pointer }
        reference_reset_base(href,stack_pointer_reg,LA_SP);
        list.concat(taicpu.op_reg_ref(A_LWZ,STACK_POINTER_REG,href));
        (*
        list.concat(taicpu.op_reg_reg_const(A_ORI,STACK_POINTER_REG,R_31,0));
        *)

        { restore the CR if necessary from callers frame
            ( !!! always done currently ) }
        reference_reset_base(href,STACK_POINTER_REG,LA_CR);
        list.concat(taicpu.op_reg_ref(A_LWZ,R_0,href));
        list.concat(taicpu.op_reg_reg(A_MTSPR,R_0,R_CR));
        a_reg_dealloc(list,R_0);

        (*
        { restore return address from callers frame }
        reference_reset_base(href,STACK_POINTER_REG,8);
        list.concat(taicpu.op_reg_ref(A_LWZ,R_0,href));
        *)

        { restore non-volatile registers from callers frame }
        restore_regs(list);

        (*
        { return to caller }
        list.concat(taicpu.op_reg_reg(A_MTSPR,R_0,R_LR));
        list.concat(taicpu.op_none(A_BLR));
        *)

        { restore return address from callers frame }
        reference_reset_base(href,STACK_POINTER_REG,8);
        list.concat(taicpu.op_reg_ref(A_LWZ,R_0,href));

        { return to caller }
        list.concat(taicpu.op_reg_reg(A_MTSPR,R_0,R_LR));
        list.concat(taicpu.op_none(A_BLR));
      end;


    procedure tcgppc.g_restore_frame_pointer(list : taasmoutput);

      begin
         { no frame pointer on the PowerPC (maybe there is one in the SystemV ABI?)}
      end;


     procedure tcgppc.a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);

       var
         ref2, tmpref: treference;
         freereg: boolean;

       begin
         ref2 := ref;
         freereg := fixref(list,ref2);
         if assigned(ref2.symbol) then
           begin
             if target_info.system = system_powerpc_macos then
               begin
                 if ref2.base <> R_NO then
                   internalerror(2002103102); //TODO: Implement this if needed

                 reference_reset(tmpref);
                 tmpref.offset := ref2.offset;
                 tmpref.symbol := ref2.symbol;
                 tmpref.symaddr := refs_full;
                 tmpref.base := R_NO;
                 list.concat(taicpu.op_reg_reg_ref(A_ADDI,r,R_TOC,tmpref));
               end
             else
               begin

                 { add the symbol's value to the base of the reference, and if the }
                 { reference doesn't have a base, create one                       }
                 reference_reset(tmpref);
                 tmpref.offset := ref2.offset;
                 tmpref.symbol := ref2.symbol;
                 tmpref.symaddr := refs_ha;
                 if ref2.base <> R_NO then
                   begin
                     list.concat(taicpu.op_reg_reg_ref(A_ADDIS,r,
                       ref2.base,tmpref));
                     if freereg then
                       begin
                         cg.free_scratch_reg(list,ref2.base);
                         freereg := false;
                       end;
                   end
                 else
                   list.concat(taicpu.op_reg_ref(A_LIS,r,tmpref));
                 tmpref.base := R_NO;
                 tmpref.symaddr := refs_l;
                 { can be folded with one of the next instructions by the }
                 { optimizer probably                                     }
                 list.concat(taicpu.op_reg_reg_ref(A_ADDI,r,r,tmpref));
               end
           end
         else if ref2.offset <> 0 Then
           if ref2.base <> R_NO then
             a_op_const_reg_reg(list,OP_ADD,OS_32,ref2.offset,ref2.base,r)
  { FixRef makes sure that "(ref.index <> R_NO) and (ref.offset <> 0)" never}
  { occurs, so now only ref.offset has to be loaded                         }
           else a_load_const_reg(list,OS_32,ref2.offset,r)
         else if ref.index <> R_NO Then
           list.concat(taicpu.op_reg_reg_reg(A_ADD,r,ref2.base,ref2.index))
         else if (ref2.base <> R_NO) and
                 (r <> ref2.base) then
           list.concat(taicpu.op_reg_reg(A_MR,r,ref2.base));
         if freereg then
           cg.free_scratch_reg(list,ref2.base);
       end;

{ ************* concatcopy ************ }

    procedure tcgppc.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);

      var
        countreg: TRegister;
        src, dst: TReference;
        lab: tasmlabel;
        count, count2: aword;
        orgsrc, orgdst: boolean;

      begin
{$ifdef extdebug}
        if len > high(longint) then
          internalerror(2002072704);
{$endif extdebug}

        { make sure short loads are handled as optimally as possible }
        if not loadref then
          if (len <= 8) and
             (byte(len) in [1,2,4,8]) then
            begin
              if len < 8 then
                begin
                  a_load_ref_ref(list,int_cgsize(len),source,dest);
                  if delsource then
                    reference_release(list,source);
                end
              else
                begin
                  a_reg_alloc(list,R_F0);
                  a_loadfpu_ref_reg(list,OS_F64,source,R_F0);
                  if delsource then
                    reference_release(list,source);
                  a_loadfpu_reg_ref(list,OS_F64,R_F0,dest);
                  a_reg_dealloc(list,R_F0);
                end;
              exit;
            end;

        reference_reset(src);
        reference_reset(dst);
        { load the address of source into src.base }
        if loadref then
          begin
            src.base := get_scratch_reg_address(list);
            a_load_ref_reg(list,OS_32,source,src.base);
            orgsrc := false;
          end
        else if not issimpleref(source) or
                ((source.index <> R_NO) and
                 ((source.offset + longint(len)) > high(smallint))) then
          begin
            src.base := get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list,source,src.base);
            orgsrc := false;
          end
        else
          begin
            src := source;
            orgsrc := true;
          end;
        if not orgsrc and delsource then
          reference_release(list,source);
        { load the address of dest into dst.base }
        if not issimpleref(dest) or
           ((dest.index <> R_NO) and
            ((dest.offset + longint(len)) > high(smallint))) then
          begin
            dst.base := get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list,dest,dst.base);
            orgdst := false;
          end
        else
          begin
            dst := dest;
            orgdst := true;
          end;

        count := len div 8;
        if count > 4 then
          { generate a loop }
          begin
            { the offsets are zero after the a_loadaddress_ref_reg and just }
            { have to be set to 8. I put an Inc there so debugging may be   }
            { easier (should offset be different from zero here, it will be }
            { easy to notice in the generated assembler                     }
            inc(dst.offset,8);
            inc(src.offset,8);
            list.concat(taicpu.op_reg_reg_const(A_SUBI,src.base,src.base,8));
            list.concat(taicpu.op_reg_reg_const(A_SUBI,dst.base,dst.base,8));
            countreg := get_scratch_reg_int(list);
            a_load_const_reg(list,OS_32,count,countreg);
            { explicitely allocate R_0 since it can be used safely here }
            { (for holding date that's being copied)                    }
            a_reg_alloc(list,R_F0);
            objectlibrary.getlabel(lab);
            a_label(list, lab);
            list.concat(taicpu.op_reg_reg_const(A_SUBIC_,countreg,countreg,1));
            list.concat(taicpu.op_reg_ref(A_LFDU,R_F0,src));
            list.concat(taicpu.op_reg_ref(A_STFDU,R_F0,dst));
            a_jmp(list,A_BC,C_NE,0,lab);
            free_scratch_reg(list,countreg);
            a_reg_dealloc(list,R_F0);
            len := len mod 8;
          end;

        count := len div 8;
        if count > 0 then
          { unrolled loop }
          begin
            a_reg_alloc(list,R_F0);
            for count2 := 1 to count do
              begin
                a_loadfpu_ref_reg(list,OS_F64,src,R_F0);
                a_loadfpu_reg_ref(list,OS_F64,R_F0,dst);
                inc(src.offset,8);
                inc(dst.offset,8);
              end;
            a_reg_dealloc(list,R_F0);
            len := len mod 8;
          end;

        if (len and 4) <> 0 then
          begin
            a_reg_alloc(list,R_0);
            a_load_ref_reg(list,OS_32,src,R_0);
            a_load_reg_ref(list,OS_32,R_0,dst);
            inc(src.offset,4);
            inc(dst.offset,4);
            a_reg_dealloc(list,R_0);
          end;
       { copy the leftovers }
       if (len and 2) <> 0 then
         begin
           a_reg_alloc(list,R_0);
           a_load_ref_reg(list,OS_16,src,R_0);
           a_load_reg_ref(list,OS_16,R_0,dst);
           inc(src.offset,2);
           inc(dst.offset,2);
           a_reg_dealloc(list,R_0);
         end;
       if (len and 1) <> 0 then
         begin
           a_reg_alloc(list,R_0);
           a_load_ref_reg(list,OS_8,src,R_0);
           a_load_reg_ref(list,OS_8,R_0,dst);
           a_reg_dealloc(list,R_0);
         end;
       if orgsrc then
         begin
           if delsource then
             reference_release(list,source);
         end
       else
         free_scratch_reg(list,src.base);
       if not orgdst then
         free_scratch_reg(list,dst.base);
      end;


    procedure tcgppc.g_overflowcheck(list: taasmoutput; const p: tnode);

      var
         hl : tasmlabel;
      begin
         if not(cs_check_overflow in aktlocalswitches) then
          exit;
         objectlibrary.getlabel(hl);
         if not ((p.resulttype.def.deftype=pointerdef) or
                ((p.resulttype.def.deftype=orddef) and
                 (torddef(p.resulttype.def).typ in [u64bit,u16bit,u32bit,u8bit,uchar,
                                                  bool8bit,bool16bit,bool32bit]))) then
           begin
             list.concat(taicpu.op_reg(A_MCRXR,R_CR7));
             a_jmp(list,A_BC,C_OV,7,hl)
           end
         else
           a_jmp_cond(list,OC_AE,hl);
         a_call_name(list,'FPC_OVERFLOW');
         a_label(list,hl);
      end;


{***************** This is private property, keep out! :) *****************}

    function tcgppc.issimpleref(const ref: treference): boolean;

      begin
        if (ref.base = R_NO) and
           (ref.index <> R_NO) then
          internalerror(200208101);
        result :=
          not(assigned(ref.symbol)) and
          (((ref.index = R_NO) and
            (ref.offset >= low(smallint)) and
            (ref.offset <= high(smallint))) or
           ((ref.index <> R_NO) and
            (ref.offset = 0)));
      end;


    function tcgppc.fixref(list: taasmoutput; var ref: treference): boolean;

       var
         tmpreg: tregister;
       begin
         result := false;
         if (ref.base <> R_NO) then
           begin
             if (ref.index <> R_NO) and
                ((ref.offset <> 0) or assigned(ref.symbol)) then
               begin
                 result := true;
                 tmpreg := cg.get_scratch_reg_int(list);
                 if not assigned(ref.symbol) and
                    (cardinal(ref.offset-low(smallint)) <=
                      high(smallint)-low(smallint)) then
                   begin
                     list.concat(taicpu.op_reg_reg_const(
                       A_ADDI,tmpreg,ref.base,ref.offset));
                     ref.offset := 0;
                   end
                 else
                   begin
                     list.concat(taicpu.op_reg_reg_reg(
                       A_ADD,tmpreg,ref.base,ref.index));
                     ref.index := R_NO;
                   end;
                 ref.base := tmpreg;
               end
           end
         else
           if ref.index <> R_NO then
             internalerror(200208102);
       end;


    { find out whether a is of the form 11..00..11b or 00..11...00. If }
    { that's the case, we can use rlwinm to do an AND operation        }
    function tcgppc.get_rlwi_const(a: aword; var l1, l2: longint): boolean;

      var
        temp, testbit: longint;
        compare: boolean;

      begin
        get_rlwi_const := false;
        if (a = 0) or (a = $ffffffff) then
          exit;
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
        tmpreg := R_NO;
        if assigned(ref.symbol) or
           (cardinal(ref.offset-low(smallint)) >
            high(smallint)-low(smallint)) then
          begin
            if target_info.system = system_powerpc_macos then
              begin
                if ref.base <> R_NO then
                  begin
                    {Generates
                      add   tempreg, ref.base, RTOC
                      op    reg, symbolplusoffset, tempreg
                    which is eqvivalent to the more comprehensive
                      addi  tempreg, RTOC, symbolplusoffset
                      add   tempreg, ref.base, RTOC
                      op    reg, tempreg
                    but which saves one instruction.}

                    tmpreg := get_scratch_reg_address(list);
                    reference_reset(tmpref);
                    tmpref.symbol := ref.symbol;
                    tmpref.offset := ref.offset;
                    tmpref.symaddr := refs_full;
                    tmpref.base:= tmpreg;

                    list.concat(taicpu.op_reg_reg_reg(A_ADD,tmpreg,
                        ref.base,R_TOC));
                    list.concat(taicpu.op_reg_ref(op,reg,tmpref));
                  end
                else
                  begin
                    reference_reset(tmpref);
                    tmpref.symbol := ref.symbol;
                    tmpref.offset := ref.offset;
                    tmpref.symaddr := refs_full;
                    tmpref.base:= R_TOC;
                    list.concat(taicpu.op_reg_ref(op,reg,tmpref));
                  end;
              end
            else
              begin
                tmpreg := get_scratch_reg_address(list);
                reference_reset(tmpref);
                tmpref.symbol := ref.symbol;
                tmpref.offset := ref.offset;
                tmpref.symaddr := refs_ha;
                if ref.base <> R_NO then
                  list.concat(taicpu.op_reg_reg_ref(A_ADDIS,tmpreg,
                    ref.base,tmpref))
                else
                  list.concat(taicpu.op_reg_ref(A_LIS,tmpreg,tmpref));
                ref.base := tmpreg;
                ref.symaddr := refs_l;
                list.concat(taicpu.op_reg_ref(op,reg,ref));
              end
          end
        else
          list.concat(taicpu.op_reg_ref(op,reg,ref));
        if (tmpreg <> R_NO) then
          free_scratch_reg(list,tmpreg);
      end;


    procedure tcgppc.a_jmp(list: taasmoutput; op: tasmop; c: tasmcondflag;
                crval: longint; l: tasmlabel);
      var
        p: taicpu;

      begin
        p := taicpu.op_sym(op,objectlibrary.newasmsymbol(l.name));
        if op <> A_B then
          create_cond_norm(c,crval,p.condition);
        p.is_jmp := true;
        list.concat(p)
      end;


    procedure tcg64fppc.a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);
      begin
        a_op64_reg_reg_reg(list,op,regsrc,regdst,regdst);
      end;


    procedure tcg64fppc.a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);
      begin
        a_op64_const_reg_reg(list,op,value,reg,reg);
      end;


    procedure tcg64fppc.a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_reg_reg_reg(list,op,OS_32,regsrc1.reglo,regsrc2.reglo,regdst.reglo);
              cg.a_op_reg_reg_reg(list,op,OS_32,regsrc1.reghi,regsrc2.reghi,regdst.reghi);
            end;
          OP_ADD:
            begin
              list.concat(taicpu.op_reg_reg_reg(A_ADDC,regdst.reglo,regsrc1.reglo,regsrc2.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_ADDE,regdst.reghi,regsrc1.reghi,regsrc2.reghi));
            end;
          OP_SUB:
            begin
              list.concat(taicpu.op_reg_reg_reg(A_SUBC,regdst.reglo,regsrc2.reglo,regsrc1.reglo));
              list.concat(taicpu.op_reg_reg_reg(A_SUBFE,regdst.reghi,regsrc1.reghi,regsrc2.reghi));
            end;
          else
            internalerror(2002072801);
        end;
      end;


    procedure tcg64fppc.a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : qword;regsrc,regdst : tregister64);

      const
        ops: array[boolean,1..3] of tasmop = ((A_ADDIC,A_ADDC,A_ADDZE),
                                              (A_SUBIC,A_SUBC,A_ADDME));
      var
        tmpreg: tregister;
        tmpreg64: tregister64;
        issub: boolean;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_const_reg_reg(list,op,OS_32,cardinal(value),regsrc.reglo,regdst.reglo);
              cg.a_op_const_reg_reg(list,op,OS_32,value shr 32,regsrc.reghi,
                regdst.reghi);
            end;
          OP_ADD, OP_SUB:
            begin
              if (longint(value) <> 0) then
                begin
                  issub := op = OP_SUB;
                  if (longint(value)-ord(issub) >= -32768) and
                     (longint(value)-ord(issub) <= 32767) then
                    begin
                      list.concat(taicpu.op_reg_reg_const(ops[issub,1],
                        regdst.reglo,regsrc.reglo,longint(value)));
                      list.concat(taicpu.op_reg_reg(ops[issub,3],
                        regdst.reghi,regsrc.reghi));
                    end
                  else if ((value shr 32) = 0) then
                    begin
                      tmpreg := cg.get_scratch_reg_int(list);
                      cg.a_load_const_reg(list,OS_32,cardinal(value),tmpreg);
                      list.concat(taicpu.op_reg_reg_reg(ops[issub,2],
                        regdst.reglo,regsrc.reglo,tmpreg));
                      cg.free_scratch_reg(list,tmpreg);
                      list.concat(taicpu.op_reg_reg(ops[issub,3],
                        regdst.reghi,regsrc.reghi));
                    end
                  else
                    begin
                      tmpreg64.reglo := cg.get_scratch_reg_int(list);
                      tmpreg64.reghi := cg.get_scratch_reg_int(list);
                      a_load64_const_reg(list,value,tmpreg64);
                      a_op64_reg_reg_reg(list,op,tmpreg64,regsrc,regdst);
                      cg.free_scratch_reg(list,tmpreg64.reghi);
                      cg.free_scratch_reg(list,tmpreg64.reglo);
                    end
                end
              else
                begin
                  cg.a_load_reg_reg(list,OS_INT,OS_INT,regsrc.reglo,regdst.reglo);
                  cg.a_op_const_reg_reg(list,op,OS_32,value shr 32,regsrc.reghi,
                    regdst.reghi);
                end;
            end;
          else
            internalerror(2002072802);
        end;
      end;


begin
  cg := tcgppc.create;
  cg64 :=tcg64fppc.create;
end.
{
  $Log$
  Revision 1.66  2002-11-28 10:55:16  olle
    * macos: changing code gen for references to globals

  Revision 1.65  2002/11/07 15:50:23  jonas
    * fixed bctr(l) problems

  Revision 1.64  2002/11/04 18:24:19  olle
    * macos: globals are located in TOC and relative r2, instead of absolute

  Revision 1.63  2002/10/28 22:24:28  olle
    * macos entry/exit: only used registers are saved
    - macos entry/exit: stackptr not saved in r31 anymore
    * macos entry/exit: misc fixes

  Revision 1.62  2002/10/19 23:51:48  olle
    * macos stack frame size computing updated
    + macos epilogue: control register now restored
    * macos prologue and epilogue: fp reg now saved and restored

  Revision 1.61  2002/10/19 12:50:36  olle
    * reorganized prologue and epilogue routines

  Revision 1.60  2002/10/02 21:49:51  florian
    * all A_BL instructions replaced by calls to a_call_name

  Revision 1.59  2002/10/02 13:24:58  jonas
    * changed a_call_* so that no superfluous code is generated anymore

  Revision 1.58  2002/09/17 18:54:06  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.57  2002/09/10 21:22:25  jonas
    + added some internal errors
    * fixed bug in sysv exit code

  Revision 1.56  2002/09/08 20:11:56  jonas
    * fixed TOpCmp2AsmCond array (some unsigned equivalents were wrong)

  Revision 1.55  2002/09/08 13:03:26  jonas
    * several large offset-related fixes

  Revision 1.54  2002/09/07 17:54:58  florian
    * first part of PowerPC fixes

  Revision 1.53  2002/09/07 15:25:14  peter
    * old logs removed and tabs fixed

  Revision 1.52  2002/09/02 10:14:51  jonas
    + a_call_reg()
    * small fix in a_call_ref()

  Revision 1.51  2002/09/02 06:09:02  jonas
    * fixed range error

  Revision 1.50  2002/09/01 21:04:49  florian
    * several powerpc related stuff fixed

  Revision 1.49  2002/09/01 12:09:27  peter
    + a_call_reg, a_call_loc added
    * removed exprasmlist references

  Revision 1.48  2002/08/31 21:38:02  jonas
    * fixed a_call_ref (it should load ctr, not lr)

  Revision 1.47  2002/08/31 21:30:45  florian
    * fixed several problems caused by Jonas' commit :)

  Revision 1.46  2002/08/31 19:25:50  jonas
    + implemented a_call_ref()

  Revision 1.45  2002/08/18 22:16:14  florian
    + the ppc gas assembler writer adds now registers aliases
      to the assembler file

  Revision 1.44  2002/08/17 18:23:53  florian
    * some assembler writer bugs fixed

  Revision 1.43  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite

  Revision 1.42  2002/08/16 14:24:59  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.41  2002/08/15 08:13:54  carl
    - a_load_sym_ofs_reg removed
    * loadvmt now calls loadaddr_ref_reg instead

  Revision 1.40  2002/08/11 14:32:32  peter
    * renamed current_library to objectlibrary

  Revision 1.39  2002/08/11 13:24:18  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.38  2002/08/11 11:39:31  jonas
    + powerpc-specific genlinearlist

  Revision 1.37  2002/08/10 17:15:31  jonas
    * various fixes and optimizations

  Revision 1.36  2002/08/06 20:55:23  florian
    * first part of ppc calling conventions fix

  Revision 1.35  2002/08/06 07:12:05  jonas
    * fixed bug in g_flags2reg()
    * and yet more constant operation fixes :)

  Revision 1.34  2002/08/05 08:58:53  jonas
    * fixed compilation problems

  Revision 1.33  2002/08/04 12:57:55  jonas
    * more misc. fixes, mostly constant-related

}
