{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the common parts of the code generator for the i386 and the x86-64.

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
{ This unit implements the common parts of the code generator for the i386 and the x86-64.
}
unit cgx86;

{$i fpcdefs.inc}

  interface

    uses
       cginfo,cgbase,cgobj,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,
       symconst,symtype;

    type
      tcgx86 = class(tcg)

        { passing parameters, per default the parameter is pushed }
        { nr gives the number of the parameter (enumerated from   }
        { left to right), this allows to move the parameter to    }
        { register, if the cpu supports register calling          }
        { conventions                                             }
        procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const locpara : tparalocation);override;
        procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);override;
        procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);override;
        procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);override;

        procedure a_call_name(list : taasmoutput;const s : string);override;
        procedure a_call_ref(list : taasmoutput;const ref : treference);override;
        procedure a_call_reg(list : taasmoutput;reg : tregister);override;


        procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; reg: TRegister); override;
        procedure a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; const ref: TReference); override;
        procedure a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;
        procedure a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister); override;
        procedure a_op_reg_ref(list : taasmoutput; Op: TOpCG; size: TCGSize;reg: TRegister; const ref: TReference); override;

        procedure a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; a: aword; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; src1, src2, dst: tregister); override;

        { move instructions }
        procedure a_load_const_reg(list : taasmoutput; tosize: tcgsize; a : aword;reg : tregister);override;
        procedure a_load_const_ref(list : taasmoutput; tosize: tcgsize; a : aword;const ref : treference);override;
        procedure a_load_reg_ref(list : taasmoutput;fromsize,tosize: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_ref_reg(list : taasmoutput;fromsize,tosize: tcgsize;const ref : treference;reg : tregister);override;
        procedure a_load_reg_reg(list : taasmoutput;fromsize,tosize: tcgsize;reg1,reg2 : tregister);override;
        procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);override;

        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list: taasmoutput; size: tcgsize; reg1, reg2: tregister); override;
        procedure a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference); override;

        { vector register move instructions }
        procedure a_loadmm_reg_reg(list: taasmoutput; reg1, reg2: tregister); override;
        procedure a_loadmm_ref_reg(list: taasmoutput; const ref: treference; reg: tregister); override;
        procedure a_loadmm_reg_ref(list: taasmoutput; reg: tregister; const ref: treference); override;
        procedure a_parammm_reg(list: taasmoutput; reg: tregister); override;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
          l : tasmlabel);override;
        procedure a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const ref : treference;
          l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;
        procedure a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;const ref: treference; reg : tregister; l : tasmlabel); override;

        procedure a_jmp_always(list : taasmoutput;l: tasmlabel); override;
        procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: tresflags; reg: TRegister); override;
        procedure g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref: TReference); override;

        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;

        procedure g_exception_reason_save(list : taasmoutput; const href : treference);override;
        procedure g_exception_reason_save_const(list : taasmoutput; const href : treference; a: aword);override;
        procedure g_exception_reason_load(list : taasmoutput; const href : treference);override;

        class function reg_cgsize(const reg: tregister): tcgsize; override;

        { entry/exit code helpers }
        procedure g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:integer);override;
        procedure g_interrupt_stackframe_entry(list : taasmoutput);override;
        procedure g_interrupt_stackframe_exit(list : taasmoutput;accused,acchiused:boolean);override;
        procedure g_profilecode(list : taasmoutput);override;
        procedure g_stackpointer_alloc(list : taasmoutput;localsize : longint);override;
        procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
        procedure g_restore_frame_pointer(list : taasmoutput);override;
        procedure g_return_from_proc(list : taasmoutput;parasize : aword);override;
        procedure g_save_standard_registers(list:Taasmoutput;usedinproc:Tsuperregisterset);override;
        procedure g_restore_standard_registers(list:Taasmoutput;usedinproc:Tsuperregisterset);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;accused,acchiused:boolean);override;

        procedure g_overflowcheck(list: taasmoutput; const l:tlocation;def:tdef);override;

      protected
        procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);
        procedure check_register_size(size:tcgsize;reg:tregister);
      private
        procedure sizes2load(s1,s2 : tcgsize;var op: tasmop; var s3: topsize);

        procedure floatload(list: taasmoutput; t : tcgsize;const ref : treference);
        procedure floatstore(list: taasmoutput; t : tcgsize;const ref : treference);
        procedure floatloadops(t : tcgsize;var op : tasmop;var s : topsize);
        procedure floatstoreops(t : tcgsize;var op : tasmop;var s : topsize);
      end;

   const
      TCGSize2OpSize: Array[tcgsize] of topsize =
        (S_NO,S_B,S_W,S_L,S_L,S_B,S_W,S_L,S_L,
         S_FS,S_FL,S_FX,S_IQ,S_FXX,
         S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO);


  implementation

    uses
       globtype,globals,verbose,systems,cutils,
       symdef,symsym,defutil,paramgr,
       rgobj,tgobj,rgcpu;

{$ifndef NOTARGETWIN32}
    const
      winstackpagesize = 4096;
{$endif NOTARGETWIN32}

      TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_NONE,A_ADD,A_AND,A_DIV,
                            A_IDIV,A_MUL, A_IMUL, A_NEG,A_NOT,A_OR,
                            A_SAR,A_SHL,A_SHR,A_SUB,A_XOR);

      TOpCmp2AsmCond: Array[topcmp] of TAsmCond = (C_NONE,
          C_E,C_G,C_L,C_GE,C_LE,C_NE,C_BE,C_B,C_AE,C_A);


{****************************************************************************
                       This is private property, keep out! :)
****************************************************************************}

    procedure tcgx86.sizes2load(s1,s2 : tcgsize; var op: tasmop; var s3: topsize);

       begin
         case s2 of
           OS_8,OS_S8 :
             if S1 in [OS_8,OS_S8] then
               s3 := S_B
             else internalerror(200109221);
           OS_16,OS_S16:
             case s1 of
               OS_8,OS_S8:
                 s3 := S_BW;
               OS_16,OS_S16:
                 s3 := S_W;
               else
                 internalerror(200109222);
             end;
           OS_32,OS_S32:
             case s1 of
               OS_8,OS_S8:
                 s3 := S_BL;
               OS_16,OS_S16:
                 s3 := S_WL;
               OS_32,OS_S32:
                 s3 := S_L;
               else
                 internalerror(200109223);
             end;
{$ifdef x86_64}
           OS_64,OS_S64:
             case s1 of
               OS_8,OS_S8:
                 s3 := S_BQ;
               OS_16,OS_S16:
                 s3 := S_WQ;
               OS_32,OS_S32:
                 s3 := S_LQ;
               OS_64,OS_S64:
                 s3 := S_Q;
               else
                 internalerror(200304302);
             end;
{$endif x86_64}
           else
             internalerror(200109227);
         end;
         if s3 in [S_B,S_W,S_L,S_Q] then
           op := A_MOV
         else if s1 in [OS_8,OS_16,OS_32,OS_64] then
           op := A_MOVZX
         else
           op := A_MOVSX;
       end;


    procedure tcgx86.floatloadops(t : tcgsize;var op : tasmop;var s : topsize);

      begin
         case t of
            OS_F32 :
              begin
                 op:=A_FLD;
                 s:=S_FS;
              end;
            OS_F64 :
              begin
                 op:=A_FLD;
                 { ???? }
                 s:=S_FL;
              end;
            OS_F80 :
              begin
                 op:=A_FLD;
                 s:=S_FX;
              end;
            OS_C64 :
              begin
                 op:=A_FILD;
                 s:=S_IQ;
              end;
            else
              internalerror(200204041);
         end;
      end;


    procedure tcgx86.floatload(list: taasmoutput; t : tcgsize;const ref : treference);

      var
         op : tasmop;
         s : topsize;

      begin
         floatloadops(t,op,s);
         list.concat(Taicpu.Op_ref(op,s,ref));
         inc(trgcpu(rg).fpuvaroffset);
      end;


    procedure tcgx86.floatstoreops(t : tcgsize;var op : tasmop;var s : topsize);

      begin
         case t of
            OS_F32 :
              begin
                 op:=A_FSTP;
                 s:=S_FS;
              end;
            OS_F64 :
              begin
                 op:=A_FSTP;
                 s:=S_FL;
              end;
            OS_F80 :
              begin
                  op:=A_FSTP;
                  s:=S_FX;
               end;
            OS_C64 :
               begin
                  op:=A_FISTP;
                  s:=S_IQ;
               end;
            else
               internalerror(200204042);
         end;
      end;


    procedure tcgx86.floatstore(list: taasmoutput; t : tcgsize;const ref : treference);

      var
         op : tasmop;
         s : topsize;

      begin
         floatstoreops(t,op,s);
         list.concat(Taicpu.Op_ref(op,s,ref));
         dec(trgcpu(rg).fpuvaroffset);
      end;


    procedure tcgx86.check_register_size(size:tcgsize;reg:tregister);
      begin
        if TCGSize2OpSize[size]<>TCGSize2OpSize[reg_cgsize(reg)] then
          internalerror(200306031);
      end;


{****************************************************************************
                              Assembler code
****************************************************************************}

    class function tcgx86.reg_cgsize(const reg: tregister): tcgsize;
      const
        opsize_2_cgsize: array[topsize] of tcgsize = (OS_NO,
          OS_8,OS_16,OS_32,OS_NO,OS_NO,OS_NO,
          OS_32,OS_64,OS_64,
          OS_F32,OS_F64,OS_F80,OS_F32,OS_F64,OS_NO,OS_NO,
          OS_NO,OS_NO,OS_NO
        );
      begin
        result := opsize_2_cgsize[reg2opsize(reg)];
      end;


    { currently does nothing }
    procedure tcgx86.a_jmp_always(list : taasmoutput;l: tasmlabel);
     begin
       a_jmp_cond(list, OC_NONE, l);
     end;

    { we implement the following routines because otherwise we can't }
    { instantiate the class since it's abstract                      }

    procedure tcgx86.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const locpara : tparalocation);
      begin
        check_register_size(size,r);
        case size of
          OS_8,OS_S8,
          OS_16,OS_S16:
            begin
              if target_info.alignment.paraalign = 2 then
                setsubreg(r,R_SUBW)
              else
                setsubreg(r,R_SUBD);
              list.concat(taicpu.op_reg(A_PUSH,S_L,r));
            end;
          OS_32,OS_S32:
            begin
              if getsubreg(r)<>R_SUBD then
                internalerror(7843);
              list.concat(taicpu.op_reg(A_PUSH,S_L,r));
            end
          else
            internalerror(2002032212);
        end;
      end;


    procedure tcgx86.a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);

      begin
        case size of
          OS_8,OS_S8,OS_16,OS_S16:
            begin
              if target_info.alignment.paraalign = 2 then
                list.concat(taicpu.op_const(A_PUSH,S_W,a))
              else
                list.concat(taicpu.op_const(A_PUSH,S_L,a));
            end;
          OS_32,OS_S32:
            list.concat(taicpu.op_const(A_PUSH,S_L,a));
          else
            internalerror(2002032213);
        end;
      end;


    procedure tcgx86.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);

      var
        pushsize : tcgsize;
        tmpreg : tregister;

      begin
        case size of
          OS_8,OS_S8,
          OS_16,OS_S16:
            begin
              if target_info.alignment.paraalign = 2 then
                pushsize:=OS_16
              else
                pushsize:=OS_32;
              tmpreg:=rg.getregisterint(list,pushsize);
              a_load_ref_reg(list,size,pushsize,r,tmpreg);
              list.concat(taicpu.op_reg(A_PUSH,TCgsize2opsize[pushsize],tmpreg));
              rg.ungetregisterint(list,tmpreg);
            end;
          OS_32,OS_S32:
            list.concat(taicpu.op_ref(A_PUSH,S_L,r));
          OS_64,OS_S64:
            list.concat(taicpu.op_ref(A_PUSH,S_Q,r));
          else
            internalerror(2002032214);
        end;
      end;


    procedure tcgx86.a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);
      var
        tmpreg: tregister;
        baseno,indexno:boolean;
      begin
        if (r.segment<>NR_NO) then
          CGMessage(cg_e_cant_use_far_pointer_there);
        baseno:=(r.base=NR_NO);
        indexno:=(r.index=NR_NO);
        if baseno and indexno then
          begin
            if assigned(r.symbol) then
              list.concat(Taicpu.Op_sym_ofs(A_PUSH,S_L,r.symbol,r.offset))
            else
              list.concat(Taicpu.Op_const(A_PUSH,S_L,r.offset));
          end
        else if baseno and not indexno and
                (r.offset=0) and (r.scalefactor=0) and (r.symbol=nil) then
          list.concat(Taicpu.Op_reg(A_PUSH,S_L,r.index))
        else if not baseno and indexno and
                (r.offset=0) and (r.symbol=nil) then
          list.concat(Taicpu.Op_reg(A_PUSH,S_L,r.base))
        else
          begin
            tmpreg:=rg.getaddressregister(list);
            a_loadaddr_ref_reg(list,r,tmpreg);
            list.concat(taicpu.op_reg(A_PUSH,S_L,tmpreg));
            rg.ungetregisterint(list,tmpreg);
          end;
      end;


    procedure tcgx86.a_call_name(list : taasmoutput;const s : string);
      begin
        list.concat(taicpu.op_sym(A_CALL,S_NO,objectlibrary.newasmsymbol(s)));
      end;


    procedure tcgx86.a_call_ref(list : taasmoutput;const ref : treference);
      begin
        list.concat(taicpu.op_ref(A_CALL,S_NO,ref));
      end;


    procedure tcgx86.a_call_reg(list : taasmoutput;reg : tregister);

      begin
        list.concat(taicpu.op_reg(A_CALL,S_NO,reg));
      end;


{********************** load instructions ********************}

    procedure tcgx86.a_load_const_reg(list : taasmoutput; tosize: TCGSize; a : aword; reg : TRegister);

      begin
        check_register_size(tosize,reg);
        { the optimizer will change it to "xor reg,reg" when loading zero, }
        { no need to do it here too (JM)                                   }
        list.concat(taicpu.op_const_reg(A_MOV,TCGSize2OpSize[tosize],a,reg))
      end;


    procedure tcgx86.a_load_const_ref(list : taasmoutput; tosize: tcgsize; a : aword;const ref : treference);

      begin
          list.concat(taicpu.op_const_ref(A_MOV,TCGSize2OpSize[tosize],a,ref));
      end;


    procedure tcgx86.a_load_reg_ref(list : taasmoutput; fromsize,tosize: TCGSize; reg : tregister;const ref : treference);
      var
        op: tasmop;
        s: topsize;
      begin
        check_register_size(fromsize,reg);
        sizes2load(fromsize,tosize,op,s);
        list.concat(taicpu.op_reg_ref(op,s,reg,ref));
      end;


    procedure tcgx86.a_load_ref_reg(list : taasmoutput;fromsize,tosize : tcgsize;const ref: treference;reg : tregister);
      var
        op: tasmop;
        s: topsize;
      begin
        check_register_size(tosize,reg);
        sizes2load(fromsize,tosize,op,s);
        list.concat(taicpu.op_ref_reg(op,s,ref,reg));
      end;


    procedure tcgx86.a_load_reg_reg(list : taasmoutput;fromsize,tosize : tcgsize;reg1,reg2 : tregister);

      var
        op: tasmop;
        s: topsize;
        eq:boolean;
        instr:Taicpu;

      begin
        check_register_size(fromsize,reg1);
        check_register_size(tosize,reg2);
        sizes2load(fromsize,tosize,op,s);
        eq:=getsupreg(reg1)=getsupreg(reg2);
        if eq then
         begin
           { "mov reg1, reg1" doesn't make sense }
           if op = A_MOV then
             exit;
         end;
        instr:=taicpu.op_reg_reg(op,s,reg1,reg2);
        {Notify the register allocator that we have written a move instruction so
         it can try to eliminate it.}
        rg.add_move_instruction(instr);
        list.concat(instr);
      end;



    procedure tcgx86.a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);

      begin
        if assigned(ref.symbol) and
           (ref.base=NR_NO) and
           (ref.index=NR_NO) then
         list.concat(taicpu.op_sym_ofs_reg(A_MOV,S_L,ref.symbol,ref.offset,r))
        else
         list.concat(taicpu.op_ref_reg(A_LEA,S_L,ref,r));
      end;


    { all fpu load routines expect that R_ST[0-7] means an fpu regvar and }
    { R_ST means "the current value at the top of the fpu stack" (JM)     }
    procedure tcgx86.a_loadfpu_reg_reg(list: taasmoutput; size: tcgsize; reg1, reg2: tregister);

       begin
         if (reg1<>NR_ST) then
           begin
             list.concat(taicpu.op_reg(A_FLD,S_NO,
               trgcpu(rg).correct_fpuregister(reg1,trgcpu(rg).fpuvaroffset)));
             inc(trgcpu(rg).fpuvaroffset);
           end;
         if (reg2<>NR_ST) then
           begin
             list.concat(taicpu.op_reg(A_FSTP,S_NO,
                 trgcpu(rg).correct_fpuregister(reg2,trgcpu(rg).fpuvaroffset)));
             dec(trgcpu(rg).fpuvaroffset);
           end;
       end;


    procedure tcgx86.a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister);
       begin
         floatload(list,size,ref);
         if (reg<>NR_ST) then
           a_loadfpu_reg_reg(list,size,NR_ST,reg);
       end;


    procedure tcgx86.a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference);
       begin
         if reg<>NR_ST then
           a_loadfpu_reg_reg(list,size,reg,NR_ST);
         floatstore(list,size,ref);
       end;


    procedure tcgx86.a_loadmm_reg_reg(list: taasmoutput; reg1, reg2: tregister);

       begin
         list.concat(taicpu.op_reg_reg(A_MOVQ,S_NO,reg1,reg2));
       end;


    procedure tcgx86.a_loadmm_ref_reg(list: taasmoutput; const ref: treference; reg: tregister);

       begin
         list.concat(taicpu.op_ref_reg(A_MOVQ,S_NO,ref,reg));
       end;


    procedure tcgx86.a_loadmm_reg_ref(list: taasmoutput; reg: tregister; const ref: treference);

       begin
         list.concat(taicpu.op_reg_ref(A_MOVQ,S_NO,reg,ref));
       end;


    procedure tcgx86.a_parammm_reg(list: taasmoutput; reg: tregister);
       var
         href : treference;
       begin
         list.concat(taicpu.op_const_reg(A_SUB,S_L,8,NR_ESP));
         reference_reset_base(href,NR_ESP,0);
         list.concat(taicpu.op_reg_ref(A_MOVQ,S_NO,reg,href));
       end;


    procedure tcgx86.a_op_const_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; reg: TRegister);

      var
        opcode: tasmop;
        power: longint;

      begin
        check_register_size(size,reg);
        case op of
          OP_DIV, OP_IDIV:
            begin
              if ispowerof2(a,power) then
                begin
                  case op of
                    OP_DIV:
                      opcode := A_SHR;
                    OP_IDIV:
                      opcode := A_SAR;
                  end;
                  list.concat(taicpu.op_const_reg(opcode,TCgSize2OpSize[size],power,reg));
                  exit;
                end;
              { the rest should be handled specifically in the code      }
              { generator because of the silly register usage restraints }
              internalerror(200109224);
            end;
          OP_MUL,OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                begin
                  list.concat(taicpu.op_const_reg(A_SHL,TCgSize2OpSize[size],power,reg));
                  exit;
                end;
              if op = OP_IMUL then
                list.concat(taicpu.op_const_reg(A_IMUL,TCgSize2OpSize[size],a,reg))
              else
                { OP_MUL should be handled specifically in the code        }
                { generator because of the silly register usage restraints }
                internalerror(200109225);
            end;
          OP_ADD, OP_AND, OP_OR, OP_SUB, OP_XOR:
            if not(cs_check_overflow in aktlocalswitches) and
               (a = 1) and
               (op in [OP_ADD,OP_SUB]) then
              if op = OP_ADD then
                list.concat(taicpu.op_reg(A_INC,TCgSize2OpSize[size],reg))
              else
                list.concat(taicpu.op_reg(A_DEC,TCgSize2OpSize[size],reg))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                list.concat(taicpu.op_const_reg(A_MOV,TCgSize2OpSize[size],0,reg))
            else if (a = high(aword)) and
                    (op in [OP_AND,OP_OR,OP_XOR]) then
                   begin
                     case op of
                       OP_AND:
                         exit;
                       OP_OR:
                         list.concat(taicpu.op_const_reg(A_MOV,TCgSize2OpSize[size],high(aword),reg));
                       OP_XOR:
                         list.concat(taicpu.op_reg(A_NOT,TCgSize2OpSize[size],reg));
                     end
                   end
            else
              list.concat(taicpu.op_const_reg(TOpCG2AsmOp[op],TCgSize2OpSize[size],a,reg));
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_const_reg(TOpCG2AsmOp[op],TCgSize2OpSize[size],a and 31,reg));
              if (a shr 5) <> 0 Then
                internalerror(68991);
            end
          else internalerror(68992);
        end;
      end;


     procedure tcgx86.a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; const ref: TReference);

      var
        opcode: tasmop;
        power: longint;

      begin
        Case Op of
          OP_DIV, OP_IDIV:
            Begin
              if ispowerof2(a,power) then
                begin
                  case op of
                    OP_DIV:
                      opcode := A_SHR;
                    OP_IDIV:
                      opcode := A_SAR;
                  end;
                  list.concat(taicpu.op_const_ref(opcode,
                    TCgSize2OpSize[size],power,ref));
                  exit;
                end;
              { the rest should be handled specifically in the code      }
              { generator because of the silly register usage restraints }
              internalerror(200109231);
            End;
          OP_MUL,OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                begin
                  list.concat(taicpu.op_const_ref(A_SHL,TCgSize2OpSize[size],
                    power,ref));
                  exit;
                end;
              { can't multiply a memory location directly with a constant }
              if op = OP_IMUL then
                inherited a_op_const_ref(list,op,size,a,ref)
              else
                { OP_MUL should be handled specifically in the code        }
                { generator because of the silly register usage restraints }
                internalerror(200109232);
            end;
          OP_ADD, OP_AND, OP_OR, OP_SUB, OP_XOR:
            if not(cs_check_overflow in aktlocalswitches) and
               (a = 1) and
               (op in [OP_ADD,OP_SUB]) then
              if op = OP_ADD then
                list.concat(taicpu.op_ref(A_INC,TCgSize2OpSize[size],ref))
              else
                list.concat(taicpu.op_ref(A_DEC,TCgSize2OpSize[size],ref))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                a_load_const_ref(list,size,0,ref)
            else if (a = high(aword)) and
                    (op in [OP_AND,OP_OR,OP_XOR]) then
                   begin
                     case op of
                       OP_AND:
                         exit;
                       OP_OR:
                         list.concat(taicpu.op_const_ref(A_MOV,TCgSize2OpSize[size],high(aword),ref));
                       OP_XOR:
                         list.concat(taicpu.op_ref(A_NOT,TCgSize2OpSize[size],ref));
                     end
                   end
            else
              list.concat(taicpu.op_const_ref(TOpCG2AsmOp[op],
                TCgSize2OpSize[size],a,ref));
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 then
                list.concat(taicpu.op_const_ref(
                  TOpCG2AsmOp[op],TCgSize2OpSize[size],a and 31,ref));
              if (a shr 5) <> 0 Then
                internalerror(68991);
            end
          else internalerror(68992);
        end;
      end;


     procedure tcgx86.a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister);

        var
          dstsize: topsize;
          tmpreg : tregister;
          instr:Taicpu;

        begin
          check_register_size(size,src);
          check_register_size(size,dst);
          dstsize := tcgsize2opsize[size];
          case op of
            OP_NEG,OP_NOT:
              begin
                if src<>dst then
                  a_load_reg_reg(list,size,size,src,dst);
                list.concat(taicpu.op_reg(TOpCG2AsmOp[op],dstsize,dst));
              end;
            OP_MUL,OP_DIV,OP_IDIV:
              { special stuff, needs separate handling inside code }
              { generator                                          }
              internalerror(200109233);
            OP_SHR,OP_SHL,OP_SAR:
              begin
                tmpreg:=rg.getexplicitregisterint(list,NR_CL);
                a_load_reg_reg(list,size,OS_8,dst,tmpreg);
                list.concat(taicpu.op_reg_reg(Topcg2asmop[op],S_B,src,
                            tmpreg));
                rg.ungetregisterint(list,tmpreg);
              end;
            else
              begin
                if reg2opsize(src) <> dstsize then
                  internalerror(200109226);
                instr:=taicpu.op_reg_reg(TOpCG2AsmOp[op],dstsize,src,dst);
                list.concat(instr);
              end;
          end;
        end;


    procedure tcgx86.a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister);
      begin
        check_register_size(size,reg);
        case op of
          OP_NEG,OP_NOT,OP_IMUL:
            begin
              inherited a_op_ref_reg(list,op,size,ref,reg);
            end;
          OP_MUL,OP_DIV,OP_IDIV:
            { special stuff, needs separate handling inside code }
            { generator                                          }
            internalerror(200109239);
          else
            begin
              reg := rg.makeregsize(reg,size);
              list.concat(taicpu.op_ref_reg(TOpCG2AsmOp[op],tcgsize2opsize[size],ref,reg));
            end;
        end;
      end;


    procedure tcgx86.a_op_reg_ref(list : taasmoutput; Op: TOpCG; size: TCGSize;reg: TRegister; const ref: TReference);
      begin
        check_register_size(size,reg);
        case op of
          OP_NEG,OP_NOT:
            begin
              if reg<>NR_NO then
                internalerror(200109237);
              list.concat(taicpu.op_ref(TOpCG2AsmOp[op],tcgsize2opsize[size],ref));
            end;
          OP_IMUL:
            begin
              { this one needs a load/imul/store, which is the default }
              inherited a_op_ref_reg(list,op,size,ref,reg);
            end;
          OP_MUL,OP_DIV,OP_IDIV:
            { special stuff, needs separate handling inside code }
            { generator                                          }
            internalerror(200109238);
          else
            begin
              list.concat(taicpu.op_reg_ref(TOpCG2AsmOp[op],tcgsize2opsize[size],reg,ref));
            end;
        end;
      end;


    procedure tcgx86.a_op_const_reg_reg(list: taasmoutput; op: TOpCg; size: tcgsize; a: aword; src, dst: tregister);
      var
        tmpref: treference;
        power: longint;
      begin
        check_register_size(size,src);
        check_register_size(size,dst);
        if not (size in [OS_32,OS_S32]) then
          begin
            inherited a_op_const_reg_reg(list,op,size,a,src,dst);
            exit;
          end;
        { if we get here, we have to do a 32 bit calculation, guaranteed }
        case op of
          OP_DIV, OP_IDIV, OP_MUL, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
          OP_SAR:
            { can't do anything special for these }
            inherited a_op_const_reg_reg(list,op,size,a,src,dst);
          OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                { can be done with a shift }
                begin
                  inherited a_op_const_reg_reg(list,op,size,a,src,dst);
                  exit;
                end;
              list.concat(taicpu.op_const_reg_reg(A_IMUL,S_L,a,src,dst));
            end;
          OP_ADD, OP_SUB:
            if (a = 0) then
              a_load_reg_reg(list,size,size,src,dst)
            else
              begin
                reference_reset(tmpref);
                tmpref.base := src;
                tmpref.offset := longint(a);
                if op = OP_SUB then
                  tmpref.offset := -tmpref.offset;
                list.concat(taicpu.op_ref_reg(A_LEA,S_L,tmpref,dst));
              end
          else internalerror(200112302);
        end;
      end;


    procedure tcgx86.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;size: tcgsize; src1, src2, dst: tregister);
      var
        tmpref: treference;
      begin
        check_register_size(size,src1);
        check_register_size(size,src2);
        check_register_size(size,dst);
        if not(size in [OS_32,OS_S32]) then
          begin
            inherited a_op_reg_reg_reg(list,op,size,src1,src2,dst);
            exit;
          end;
        { if we get here, we have to do a 32 bit calculation, guaranteed }
        Case Op of
          OP_DIV, OP_IDIV, OP_MUL, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
          OP_SAR,OP_SUB,OP_NOT,OP_NEG:
            { can't do anything special for these }
            inherited a_op_reg_reg_reg(list,op,size,src1,src2,dst);
          OP_IMUL:
            list.concat(taicpu.op_reg_reg_reg(A_IMUL,S_L,src1,src2,dst));
          OP_ADD:
            begin
              reference_reset(tmpref);
              tmpref.base := src1;
              tmpref.index := src2;
              tmpref.scalefactor := 1;
              list.concat(taicpu.op_ref_reg(A_LEA,S_L,tmpref,dst));
            end
          else internalerror(200112303);
        end;
      end;

{*************** compare instructructions ****************}

    procedure tcgx86.a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
      l : tasmlabel);

      begin
        if (a = 0) then
          list.concat(taicpu.op_reg_reg(A_TEST,tcgsize2opsize[size],reg,reg))
        else
          list.concat(taicpu.op_const_reg(A_CMP,tcgsize2opsize[size],a,reg));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const ref : treference;
      l : tasmlabel);

      begin
        list.concat(taicpu.op_const_ref(A_CMP,TCgSize2OpSize[size],a,ref));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;
      reg1,reg2 : tregister;l : tasmlabel);

      begin
        check_register_size(size,reg1);
        check_register_size(size,reg2);
        list.concat(taicpu.op_reg_reg(A_CMP,TCgSize2OpSize[size],reg1,reg2));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;const ref: treference; reg : tregister;l : tasmlabel);
      begin
        check_register_size(size,reg);
        list.concat(taicpu.op_ref_reg(A_CMP,TCgSize2OpSize[size],ref,reg));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);
      var
        ai : taicpu;
      begin
        if cond=OC_None then
          ai := Taicpu.Op_sym(A_JMP,S_NO,l)
        else
          begin
            ai:=Taicpu.Op_sym(A_Jcc,S_NO,l);
            ai.SetCondition(TOpCmp2AsmCond[cond]);
          end;
        ai.is_jmp:=true;
        list.concat(ai);
      end;


     procedure tcgx86.a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel);
       var
         ai : taicpu;
       begin
         ai := Taicpu.op_sym(A_Jcc,S_NO,l);
         ai.SetCondition(flags_to_cond(f));
         ai.is_jmp := true;
         list.concat(ai);
       end;


    procedure tcgx86.g_flags2reg(list: taasmoutput; size: TCgSize; const f: tresflags; reg: TRegister);
      var
        ai : taicpu;
        hreg : tregister;
      begin
        hreg:=rg.makeregsize(reg,OS_8);
        ai:=Taicpu.op_reg(A_SETcc,S_B,hreg);
        ai.setcondition(flags_to_cond(f));
        list.concat(ai);
        if (reg<>hreg) then
          a_load_reg_reg(list,OS_8,size,hreg,reg);
      end;


     procedure tcgx86.g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref: TReference);
       var
         ai : taicpu;
       begin
          if not(size in [OS_8,OS_S8]) then
            a_load_const_ref(list,size,0,ref);
          ai:=Taicpu.op_ref(A_SETcc,S_B,ref);
          ai.setcondition(flags_to_cond(f));
          list.concat(ai);
       end;


{ ************* concatcopy ************ }

    procedure Tcgx86.g_concatcopy(list:Taasmoutput;const source,dest:Treference;
                                  len:aword;delsource,loadref:boolean);
    var srcref,dstref:Treference;
        srcreg,destreg,countreg,r:Tregister;
        helpsize:aword;
        copysize:byte;
        cgsize:Tcgsize;

    begin
      helpsize:=12;
      if cs_littlesize in aktglobalswitches then
        helpsize:=8;
      if not loadref and (len<=helpsize) then
        begin
          dstref:=dest;
          srcref:=source;
          copysize:=4;
          cgsize:=OS_32;
          while len<>0 do
            begin
              if len<2 then
                begin
                  copysize:=1;
                  cgsize:=OS_8;
                end
              else if len<4 then
                begin
                  copysize:=2;
                  cgsize:=OS_16;
                end;
              dec(len,copysize);
              r:=rg.getregisterint(list,cgsize);
              a_load_ref_reg(list,cgsize,cgsize,srcref,r);
              if (len=0) and delsource then
                reference_release(list,source);
              a_load_reg_ref(list,cgsize,cgsize,r,dstref);
              inc(srcref.offset,copysize);
              inc(dstref.offset,copysize);
              rg.ungetregisterint(list,r);
            end;
        end
      else
        begin
          destreg:=rg.getexplicitregisterint(list,NR_EDI);
          a_loadaddr_ref_reg(list,dest,destreg);
          srcreg:=rg.getexplicitregisterint(list,NR_ESI);
          if loadref then
            a_load_ref_reg(list,OS_ADDR,OS_ADDR,source,srcreg)
          else
            begin
              a_loadaddr_ref_reg(list,source,srcreg);
              if delsource then
                begin
                  srcref:=source;
                  { Don't release ESI register yet, it's needed
                    by the movsl }
                  if (srcref.base=NR_ESI) then
                    srcref.base:=NR_NO
                  else if (srcref.index=NR_ESI) then
                    srcref.index:=NR_NO;
                  reference_release(list,srcref);
                end;
            end;

          countreg:=rg.getexplicitregisterint(list,NR_ECX);

          list.concat(Taicpu.op_none(A_CLD,S_NO));
          if cs_littlesize in aktglobalswitches  then
            begin
              a_load_const_reg(list,OS_INT,len,countreg);
              list.concat(Taicpu.op_none(A_REP,S_NO));
              list.concat(Taicpu.op_none(A_MOVSB,S_NO));
            end
          else
            begin
              helpsize:=len shr 2;
              len:=len and 3;
              if helpsize>1 then
                begin
                  a_load_const_reg(list,OS_INT,helpsize,countreg);
                  list.concat(Taicpu.op_none(A_REP,S_NO));
                end;
              if helpsize>0 then
                list.concat(Taicpu.op_none(A_MOVSD,S_NO));
              if len>1 then
                begin
                  dec(len,2);
                  list.concat(Taicpu.op_none(A_MOVSW,S_NO));
                end;
              if len=1 then
                list.concat(Taicpu.op_none(A_MOVSB,S_NO));
              end;
          rg.ungetregisterint(list,countreg);
          rg.ungetregisterint(list,srcreg);
          rg.ungetregisterint(list,destreg);
        end;
      if delsource then
        tg.ungetiftemp(list,source);
    end;


    procedure tcgx86.g_exception_reason_save(list : taasmoutput; const href : treference);

    begin
        list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EAX));
    end;

    procedure tcgx86.g_exception_reason_save_const(list : taasmoutput;const href : treference; a: aword);

    begin
        list.concat(Taicpu.op_const(A_PUSH,S_L,a));
    end;

    procedure tcgx86.g_exception_reason_load(list : taasmoutput; const href : treference);

    begin
        list.concat(Taicpu.op_reg(A_POP,S_L,NR_EAX));
    end;


{****************************************************************************
                              Entry/Exit Code Helpers
****************************************************************************}

    procedure tcgx86.g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:integer);
      var
        power,len  : longint;
        opsize : topsize;
{$ifndef __NOWINPECOFF__}
        again,ok : tasmlabel;
{$endif}
        r : tregister;
      begin
        { get stack space }
        r:=NR_EDI;
        rg.getexplicitregisterint(list,r);
        list.concat(Taicpu.op_ref_reg(A_MOV,S_L,lenref,r));
        list.concat(Taicpu.op_reg(A_INC,S_L,r));
        if (elesize<>1) then
         begin
           if ispowerof2(elesize, power) then
             list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,r))
           else
             list.concat(Taicpu.op_const_reg(A_IMUL,S_L,elesize,r));
         end;
{$ifndef __NOWINPECOFF__}
        { windows guards only a few pages for stack growing, }
        { so we have to access every page first              }
        if target_info.system=system_i386_win32 then
          begin
             objectlibrary.getlabel(again);
             objectlibrary.getlabel(ok);
             a_label(list,again);
             list.concat(Taicpu.op_const_reg(A_CMP,S_L,winstackpagesize,r));
             a_jmp_cond(list,OC_B,ok);
             list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize-4,NR_ESP));
             list.concat(Taicpu.op_reg(A_PUSH,S_L,r));
             list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize,r));
             a_jmp_always(list,again);

             a_label(list,ok);
             list.concat(Taicpu.op_reg_reg(A_SUB,S_L,r,NR_ESP));
             rg.ungetregisterint(list,r);
             { now reload EDI }
             rg.getexplicitregisterint(list,r);
             list.concat(Taicpu.op_ref_reg(A_MOV,S_L,lenref,r));
             list.concat(Taicpu.op_reg(A_INC,S_L,r));

             if (elesize<>1) then
              begin
                if ispowerof2(elesize, power) then
                  list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,r))
                else
                  list.concat(Taicpu.op_const_reg(A_IMUL,S_L,elesize,r));
              end;
          end
        else
{$endif __NOWINPECOFF__}
          list.concat(Taicpu.op_reg_reg(A_SUB,S_L,r,NR_ESP));
        { align stack on 4 bytes }
        list.concat(Taicpu.op_const_reg(A_AND,S_L,$fffffff4,NR_ESP));
        { load destination }
        a_load_reg_reg(list,OS_INT,OS_INT,NR_ESP,r);

        { don't destroy the registers! }
        list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_ECX));
        list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_ESI));

        { load count }
        a_load_ref_reg(list,OS_INT,OS_INT,lenref,NR_ECX);

        { load source }
        a_load_ref_reg(list,OS_INT,OS_INT,ref,NR_ESI);

        { scheduled .... }
        list.concat(Taicpu.op_reg(A_INC,S_L,NR_ECX));

        { calculate size }
        len:=elesize;
        opsize:=S_B;
        if (len and 3)=0 then
         begin
           opsize:=S_L;
           len:=len shr 2;
         end
        else
         if (len and 1)=0 then
          begin
            opsize:=S_W;
            len:=len shr 1;
          end;

        if ispowerof2(len, power) then
          list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,NR_ECX))
        else
          list.concat(Taicpu.op_const_reg(A_IMUL,S_L,len,NR_ECX));
        list.concat(Taicpu.op_none(A_REP,S_NO));
        case opsize of
          S_B : list.concat(Taicpu.Op_none(A_MOVSB,S_NO));
          S_W : list.concat(Taicpu.Op_none(A_MOVSW,S_NO));
          S_L : list.concat(Taicpu.Op_none(A_MOVSD,S_NO));
        end;
        rg.ungetregisterint(list,r);
        list.concat(Taicpu.op_reg(A_POP,S_L,NR_ESI));
        list.concat(Taicpu.op_reg(A_POP,S_L,NR_ECX));

        { patch the new address }
        a_load_reg_ref(list,OS_INT,OS_INT,NR_ESP,ref);
      end;


    procedure tcgx86.g_interrupt_stackframe_entry(list : taasmoutput);

    begin
        { .... also the segment registers }
        list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_GS));
        list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_FS));
        list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_ES));
        list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_DS));
        { save the registers of an interrupt procedure }
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EDI));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_ESI));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EDX));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_ECX));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EBX));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EAX));
    end;


    procedure tcgx86.g_interrupt_stackframe_exit(list : taasmoutput;accused,acchiused:boolean);

      begin
        if accused then
          list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,NR_ESP))
        else
          list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EAX));
        list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EBX));
        list.concat(Taicpu.Op_reg(A_POP,S_L,NR_ECX));
        if acchiused then
          list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,NR_ESP))
        else
          list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EDX));
        list.concat(Taicpu.Op_reg(A_POP,S_L,NR_ESI));
        list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EDI));
        { .... also the segment registers }
        list.concat(Taicpu.Op_reg(A_POP,S_W,NR_DS));
        list.concat(Taicpu.Op_reg(A_POP,S_W,NR_ES));
        list.concat(Taicpu.Op_reg(A_POP,S_W,NR_FS));
        list.concat(Taicpu.Op_reg(A_POP,S_W,NR_GS));
        { this restores the flags }
        list.concat(Taicpu.Op_none(A_IRET,S_NO));
      end;


    procedure tcgx86.g_profilecode(list : taasmoutput);
      var
        pl : tasmlabel;
      begin
        case target_info.system of
        {$ifndef NOTARGETWIN32}
           system_i386_win32,
        {$endif}
           system_i386_freebsd,
           system_i386_wdosx,
           system_i386_linux:
             begin
                objectlibrary.getaddrlabel(pl);
                list.concat(Tai_section.Create(sec_data));
                list.concat(Tai_align.Create(4));
                list.concat(Tai_label.Create(pl));
                list.concat(Tai_const.Create_32bit(0));
                list.concat(Tai_section.Create(sec_code));
                list.concat(Taicpu.Op_sym_ofs_reg(A_MOV,S_L,pl,0,NR_EDX));
                a_call_name(list,target_info.Cprefix+'mcount');
                include(rg.used_in_proc_int,RS_EDX);
             end;

           system_i386_go32v2:
             begin
               a_call_name(list,'MCOUNT');
             end;
        end;
      end;


    procedure tcgx86.g_stackpointer_alloc(list : taasmoutput;localsize : longint);
      var
        href : treference;
        i : integer;
        again : tasmlabel;
        r : Tregister;
      begin
        if localsize>0 then
         begin
{$ifndef NOTARGETWIN32}
           { windows guards only a few pages for stack growing, }
           { so we have to access every page first              }
           if (target_info.system=system_i386_win32) and
              (localsize>=winstackpagesize) then
             begin
               if localsize div winstackpagesize<=5 then
                 begin
                    list.concat(Taicpu.Op_const_reg(A_SUB,S_L,localsize-4,NR_ESP));
                    for i:=1 to localsize div winstackpagesize do
                      begin
                         reference_reset_base(href,NR_ESP,localsize-i*winstackpagesize);
                         list.concat(Taicpu.op_const_ref(A_MOV,S_L,0,href));
                      end;
                    list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EAX));
                 end
               else
                 begin
                    objectlibrary.getlabel(again);
                    r:=NR_EDI;
                    rg.getexplicitregisterint(list,r);
                    list.concat(Taicpu.op_const_reg(A_MOV,S_L,localsize div winstackpagesize,r));
                    a_label(list,again);
                    list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize-4,NR_ESP));
                    list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EAX));
                    list.concat(Taicpu.op_reg(A_DEC,S_L,r));
                    a_jmp_cond(list,OC_NE,again);
                    rg.ungetregisterint(list,r);
                    list.concat(Taicpu.op_const_reg(A_SUB,S_L,localsize mod winstackpagesize,NR_ESP));
                 end
             end
           else
{$endif NOTARGETWIN32}
            list.concat(Taicpu.Op_const_reg(A_SUB,S_L,localsize,NR_ESP));
         end;
      end;


    procedure tcgx86.g_stackframe_entry(list : taasmoutput;localsize : longint);

    begin
      list.concat(tai_regalloc.alloc(NR_EBP));
      include(rg.savedintbyproc,RS_EBP);
      list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EBP));
      list.concat(Taicpu.op_reg_reg(A_MOV,S_L,NR_ESP,NR_EBP));
      if localsize>0 then
        g_stackpointer_alloc(list,localsize);
    end;


    procedure tcgx86.g_restore_frame_pointer(list : taasmoutput);

    begin
      list.concat(tai_regalloc.dealloc(NR_EBP));
      list.concat(Taicpu.op_none(A_LEAVE,S_NO));
    end;


    procedure tcgx86.g_return_from_proc(list : taasmoutput;parasize : aword);
      begin
        { Routines with the poclearstack flag set use only a ret }
        { also routines with parasize=0     }
        if (po_clearstack in current_procinfo.procdef.procoptions) then
         begin
           { complex return values are removed from stack in C code PM }
           if paramanager.ret_in_param(current_procinfo.procdef.rettype.def,
                                       current_procinfo.procdef.proccalloption) then
             list.concat(Taicpu.Op_const(A_RET,S_NO,4))
           else
             list.concat(Taicpu.Op_none(A_RET,S_NO));
         end
        else if (parasize=0) then
         list.concat(Taicpu.Op_none(A_RET,S_NO))
        else
         begin
           { parameters are limited to 65535 bytes because }
           { ret allows only imm16                    }
           if (parasize>65535) then
             CGMessage(cg_e_parasize_too_big);
           list.concat(Taicpu.Op_const(A_RET,S_NO,parasize));
         end;
      end;


    procedure tcgx86.g_save_standard_registers(list:Taasmoutput;usedinproc:Tsuperregisterset);

    begin
      if (RS_EBX in usedinproc) then
        list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EBX));
      list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_ESI));
      list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EDI));
      include(rg.savedintbyproc,RS_EBX);
      include(rg.savedintbyproc,RS_ESI);
      include(rg.savedintbyproc,RS_EDI);
    end;


    procedure tcgx86.g_restore_standard_registers(list:Taasmoutput;usedinproc:Tsuperregisterset);

    begin
        list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EDI));
        list.concat(Taicpu.Op_reg(A_POP,S_L,NR_ESI));
        if (RS_EBX in usedinproc) then
         list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EBX));
    end;


    procedure tcgx86.g_save_all_registers(list : taasmoutput);
      begin
        list.concat(Taicpu.Op_none(A_PUSHA,S_L));
      end;


    procedure tcgx86.g_restore_all_registers(list : taasmoutput;accused,acchiused:boolean);
      var
        href : treference;
      begin
        if acchiused then
         begin
           reference_reset_base(href,NR_ESP,20);
           list.concat(Taicpu.Op_reg_ref(A_MOV,S_L,NR_EDX,href));
         end;
        if accused then
         begin
           reference_reset_base(href,NR_ESP,28);
           list.concat(Taicpu.Op_reg_ref(A_MOV,S_L,NR_EAX,href));
         end;
        list.concat(Taicpu.Op_none(A_POPA,S_L));
        { We add a NOP because of the 386DX CPU bugs with POPAD }
        list.concat(taicpu.op_none(A_NOP,S_L));
      end;


    { produces if necessary overflowcode }
    procedure tcgx86.g_overflowcheck(list: taasmoutput; const l:tlocation;def:tdef);
      var
         hl : tasmlabel;
         ai : taicpu;
         cond : TAsmCond;
      begin
         if not(cs_check_overflow in aktlocalswitches) then
          exit;
         objectlibrary.getlabel(hl);
         if not ((def.deftype=pointerdef) or
                ((def.deftype=orddef) and
                 (torddef(def).typ in [u64bit,u16bit,u32bit,u8bit,uchar,
                                       bool8bit,bool16bit,bool32bit]))) then
           cond:=C_NO
         else
           cond:=C_NB;
         ai:=Taicpu.Op_Sym(A_Jcc,S_NO,hl);
         ai.SetCondition(cond);
         ai.is_jmp:=true;
         list.concat(ai);

         a_call_name(list,'FPC_OVERFLOW');
         a_label(list,hl);
      end;


end.
{
  $Log$
  Revision 1.59  2003-09-03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.58.2.5  2003/08/31 20:40:50  daniel
    * Fixed add_edges_used

  Revision 1.58.2.4  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.58.2.3  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.58.2.2  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.58.2.1  2003/08/27 21:06:34  peter
    * more updates

  Revision 1.58  2003/08/20 19:28:21  daniel
    * Small NOTARGETWIN32 conditional tweak

  Revision 1.57  2003/07/03 18:59:25  peter
    * loadfpu_reg_reg size specifier

  Revision 1.56  2003/06/14 14:53:50  jonas
    * fixed newra cycle for x86
    * added constants for indicating source and destination operands of the
      "move reg,reg" instruction to aasmcpu (and use those in rgobj)

  Revision 1.55  2003/06/13 21:19:32  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.54  2003/06/12 18:31:18  peter
    * fix newra cycle for i386

  Revision 1.53  2003/06/07 10:24:10  peter
    * fixed copyvaluepara for left-to-right pushing

  Revision 1.52  2003/06/07 10:06:55  jonas
    * fixed cycling problem

  Revision 1.51  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.50  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.49  2003/06/01 21:38:07  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.48  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.47  2003/05/22 21:33:31  peter
    * removed some unit dependencies

  Revision 1.46  2003/05/16 14:33:31  peter
    * regvar fixes

  Revision 1.45  2003/05/15 18:58:54  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.44  2003/04/30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.43  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.42  2003/04/23 14:42:08  daniel
    * Further register allocator work. Compiler now smaller with new
      allocator than without.
    * Somebody forgot to adjust ppu version number

  Revision 1.41  2003/04/23 09:51:16  daniel
    * Removed usage of edi in a lot of places when new register allocator used
    + Added newra versions of g_concatcopy and secondadd_float

  Revision 1.40  2003/04/22 13:47:08  peter
    * fixed C style array of const
    * fixed C array passing
    * fixed left to right with high parameters

  Revision 1.39  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.38  2003/04/17 16:48:21  daniel
    * Added some code to keep track of move instructions in register
      allocator

  Revision 1.37  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.36  2003/03/18 18:17:46  peter
    * reg2opsize()

  Revision 1.35  2003/03/13 19:52:23  jonas
    * and more new register allocator fixes (in the i386 code generator this
      time). At least now the ppc cross compiler can compile the linux
      system unit again, but I haven't tested it.

  Revision 1.34  2003/02/27 16:40:32  daniel
    * Fixed ie 200301234 problem on Win32 target

  Revision 1.33  2003/02/26 21:15:43  daniel
    * Fixed the optimizer

  Revision 1.32  2003/02/19 22:00:17  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.31  2003/01/21 10:41:13  daniel
    * Fixed another 200301081

  Revision 1.30  2003/01/13 23:00:18  daniel
    * Fixed internalerror

  Revision 1.29  2003/01/13 14:54:34  daniel
    * Further work to convert codegenerator register convention;
      internalerror bug fixed.

  Revision 1.28  2003/01/09 20:41:00  daniel
    * Converted some code in cgx86.pas to new register numbering

  Revision 1.27  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.26  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.25  2003/01/02 16:17:50  peter
    * align stack on 4 bytes in copyvalueopenarray

  Revision 1.24  2002/12/24 15:56:50  peter
    * stackpointer_alloc added for adjusting ESP. Win32 needs
      this for the pageprotection

  Revision 1.23  2002/11/25 18:43:34  carl
   - removed the invalid if <> checking (Delphi is strange on this)
   + implemented abstract warning on instance creation of class with
      abstract methods.
   * some error message cleanups

  Revision 1.22  2002/11/25 17:43:29  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.21  2002/11/18 17:32:01  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.20  2002/11/09 21:18:31  carl
    * flags2reg() was not extending the byte register to the correct result size

  Revision 1.19  2002/10/16 19:01:43  peter
    + $IMPLICITEXCEPTIONS switch to turn on/off generation of the
      implicit exception frames for procedures with initialized variables
      and for constructors. The default is on for compatibility

  Revision 1.18  2002/10/05 12:43:30  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.17  2002/09/17 18:54:06  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.16  2002/09/16 19:08:47  peter
    * support references without registers and symbol in paramref_addr. It
      pushes only the offset

  Revision 1.15  2002/09/16 18:06:29  peter
    * move CGSize2Opsize to interface

  Revision 1.14  2002/09/01 14:42:41  peter
    * removevaluepara added to fix the stackpointer so restoring of
      saved registers works

  Revision 1.13  2002/09/01 12:09:27  peter
    + a_call_reg, a_call_loc added
    * removed exprasmlist references

  Revision 1.12  2002/08/17 09:23:50  florian
    * first part of procinfo rewrite

  Revision 1.11  2002/08/16 14:25:00  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.10  2002/08/15 08:13:54  carl
    - a_load_sym_ofs_reg removed
    * loadvmt now calls loadaddr_ref_reg instead

  Revision 1.9  2002/08/11 14:32:33  peter
    * renamed current_library to objectlibrary

  Revision 1.8  2002/08/11 13:24:20  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.7  2002/08/10 10:06:04  jonas
    * fixed stupid bug of mine in g_flags2reg() when optimizations are on

  Revision 1.6  2002/08/09 19:18:27  carl
    * fix generic exception handling

  Revision 1.5  2002/08/04 19:52:04  carl
    + updated exception routines

  Revision 1.4  2002/07/27 19:53:51  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.3  2002/07/26 21:15:46  florian
    * rewrote the system handling

  Revision 1.2  2002/07/21 16:55:34  jonas
    * fixed bug in op_const_reg_reg() for imul

  Revision 1.1  2002/07/20 19:28:47  florian
    * splitting of i386\cgcpu.pas into x86\cgx86.pas and i386\cgcpu.pas
      cgx86.pas will contain the common code for i386 and x86_64

}
