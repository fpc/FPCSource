{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements the code generator for the i386

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
       cginfo,cgbase,cgobj,cg64f32,aasm,cpuasm,cpubase,cpuinfo,symconst;

    type
      tcg386 = class(tcg64f32)

        { passing parameters, per default the parameter is pushed }
        { nr gives the number of the parameter (enumerated from   }
        { left to right), this allows to move the parameter to    }
        { register, if the cpu supports register calling          }
        { conventions                                             }
        procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;nr : longint);override;
        procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;nr : longint);override;
        procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;nr : longint);override;
        procedure a_paramaddr_ref(list : taasmoutput;const r : treference;nr : longint);override;


        procedure a_call_name(list : taasmoutput;const s : string);override;
        procedure a_call_ref(list : taasmoutput;const ref : treference);override;


        procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister); override;
        procedure a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; const ref: TReference); override;
        procedure a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;
        procedure a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister); override;
        procedure a_op_reg_ref(list : taasmoutput; Op: TOpCG; size: TCGSize;reg: TRegister; const ref: TReference); override;

        procedure a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; a: aword; src, dst: tregister); override;
        procedure a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
          size: tcgsize; src1, src2, dst: tregister); override;

        { move instructions }
        procedure a_load_const_reg(list : taasmoutput; size: tcgsize; a : aword;reg : tregister);override;
        procedure a_load_const_ref(list : taasmoutput; size: tcgsize; a : aword;const ref : treference);override;
        procedure a_load_reg_ref(list : taasmoutput; size: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref : treference;reg : tregister);override;
        procedure a_load_reg_reg(list : taasmoutput;size : tcgsize;reg1,reg2 : tregister);override;
        procedure a_load_sym_ofs_reg(list: taasmoutput; const sym: tasmsymbol; ofs: longint; reg: tregister); override;
        procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);override;

        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list: taasmoutput; reg1, reg2: tregister); override;
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

        procedure g_flags2reg(list: taasmoutput; const f: tresflags; reg: TRegister); override;

        procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reglo,reghi : tregister);override;
        procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;reglosrc,reghisrc,reglodst,reghidst : tregister);override;
        procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;valuelosrc,valuehisrc:AWord;reglodst,reghidst : tregister);override;
        procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;valuelosrc,valuehisrc:AWord;const ref : treference);override;

        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;

        procedure g_push_exception(list : taasmoutput;const exceptbuf:treference;l:AWord; exceptlabel:TAsmLabel);override;
        procedure g_pop_exception(list : taasmoutput;endexceptlabel:tasmlabel);override;

        class function reg_cgsize(const reg: tregister): tcgsize; override;

        { entry/exit code helpers }
        procedure g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer);override;
        procedure g_interrupt_stackframe_entry(list : taasmoutput);override;
        procedure g_interrupt_stackframe_exit(list : taasmoutput;selfused,accused,acchiused:boolean);override;
        procedure g_profilecode(list : taasmoutput);override;
        procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
        procedure g_restore_frame_pointer(list : taasmoutput);override;
        procedure g_return_from_proc(list : taasmoutput;parasize : aword);override;
        procedure g_call_constructor_helper(list : taasmoutput);override;
        procedure g_call_destructor_helper(list : taasmoutput);override;
        procedure g_call_fail_helper(list : taasmoutput);override;
        procedure g_save_standard_registers(list : taasmoutput);override;
        procedure g_restore_standard_registers(list : taasmoutput);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);override;

       private

        procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);
        procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
        procedure sizes2load(s1 : tcgsize;s2 : topsize; var op: tasmop; var s3: topsize);

        procedure floatload(list: taasmoutput; t : tcgsize;const ref : treference);
        procedure floatstore(list: taasmoutput; t : tcgsize;const ref : treference);
        procedure floatloadops(t : tcgsize;var op : tasmop;var s : topsize);
        procedure floatstoreops(t : tcgsize;var op : tasmop;var s : topsize);

      end;

    const

      TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_NONE,A_ADD,A_AND,A_DIV,
                            A_IDIV,A_MUL, A_IMUL, A_NEG,A_NOT,A_OR,
                            A_SAR,A_SHL,A_SHR,A_SUB,A_XOR);

      TOpCmp2AsmCond: Array[topcmp] of TAsmCond = (C_NONE,
          C_E,C_G,C_L,C_GE,C_LE,C_NE,C_BE,C_B,C_AE,C_A);

      TCGSize2OpSize: Array[tcgsize] of topsize =
        (S_NO,S_B,S_W,S_L,S_L,S_B,S_W,S_L,S_L,
         S_FS,S_FL,S_FX,S_IQ,
         S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO);


  implementation

    uses
       globtype,globals,verbose,systems,cutils,
       symtable,symdef,symsym,types,
       rgobj,tgobj,rgcpu,tainst;

{$ifndef NOTARGETWIN32}
    const
      winstackpagesize = 4096;
{$endif NOTARGETWIN32}


{****************************************************************************
                       This is private property, keep out! :)
****************************************************************************}

    procedure tcg386.sizes2load(s1 : tcgsize;s2: topsize; var op: tasmop; var s3: topsize);

       begin
         case s2 of
           S_B:
             if S1 in [OS_8,OS_S8] then
               s3 := S_B
             else internalerror(200109221);
           S_W:
             case s1 of
               OS_8,OS_S8:
                 s3 := S_BW;
               OS_16,OS_S16:
                 s3 := S_W;
               else internalerror(200109222);
             end;
           S_L:
             case s1 of
               OS_8,OS_S8:
                 s3 := S_BL;
               OS_16,OS_S16:
                 s3 := S_WL;
               OS_32,OS_S32:
                 s3 := S_L;
               else internalerror(200109223);
             end;
           else internalerror(200109227);
         end;
         if s3 in [S_B,S_W,S_L] then
           op := A_MOV
         else if s1 in [OS_8,OS_16,OS_32] then
           op := A_MOVZX
         else
           op := A_MOVSX;
       end;


    procedure tcg386.floatloadops(t : tcgsize;var op : tasmop;var s : topsize);

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


    procedure tcg386.floatload(list: taasmoutput; t : tcgsize;const ref : treference);

      var
         op : tasmop;
         s : topsize;

      begin
         floatloadops(t,op,s);
         list.concat(Taicpu.Op_ref(op,s,ref));
         inc(trgcpu(rg).fpuvaroffset);
      end;


    procedure tcg386.floatstoreops(t : tcgsize;var op : tasmop;var s : topsize);

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


    procedure tcg386.floatstore(list: taasmoutput; t : tcgsize;const ref : treference);

      var
         op : tasmop;
         s : topsize;

      begin
         floatstoreops(t,op,s);
         list.concat(Taicpu.Op_ref(op,s,ref));
         dec(trgcpu(rg).fpuvaroffset);
      end;


{****************************************************************************
                              Assembler code
****************************************************************************}

    function tcg386.reg_cgsize(const reg: tregister): tcgsize;
      const
        regsize_2_cgsize: array[S_B..S_L] of tcgsize = (OS_8,OS_16,OS_32);
      begin
        result := regsize_2_cgsize[reg2opsize[reg]];
      end;


    { currently does nothing }
    procedure tcg386.a_jmp_always(list : taasmoutput;l: tasmlabel);
     begin
       a_jmp_cond(list, OC_NONE, l);
     end;

    { we implement the following routines because otherwise we can't }
    { instantiate the class since it's abstract                      }

    procedure tcg386.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;nr : longint);
      begin
        case size of
          OS_8,OS_S8,
          OS_16,OS_S16:
            begin
              if target_info.alignment.paraalign = 2 then
                list.concat(taicpu.op_reg(A_PUSH,S_W,rg.makeregsize(r,OS_16)))
              else
                list.concat(taicpu.op_reg(A_PUSH,S_L,rg.makeregsize(r,OS_32)));
            end;
          OS_32,OS_S32:
            list.concat(taicpu.op_reg(A_PUSH,S_L,r));
          else
            internalerror(2002032212);
        end;
      end;


    procedure tcg386.a_param_const(list : taasmoutput;size : tcgsize;a : aword;nr : longint);

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


    procedure tcg386.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;nr : longint);

      var
        tmpreg: tregister;

      begin
        case size of
          OS_8,OS_S8,
          OS_16,OS_S16:
            begin
              tmpreg := get_scratch_reg(list);
              a_load_ref_reg(list,size,r,tmpreg);
              if target_info.alignment.paraalign = 2 then
                list.concat(taicpu.op_reg(A_PUSH,S_W,rg.makeregsize(tmpreg,OS_16)))
              else
                list.concat(taicpu.op_reg(A_PUSH,S_L,tmpreg));
              free_scratch_reg(list,tmpreg);
            end;
          OS_32,OS_S32:
            list.concat(taicpu.op_ref(A_PUSH,S_L,r));
          else
            internalerror(2002032214);
        end;
      end;


    procedure tcg386.a_paramaddr_ref(list : taasmoutput;const r : treference;nr : longint);

      var
        tmpreg: tregister;

      begin
        if r.segment<>R_NO then
          CGMessage(cg_e_cant_use_far_pointer_there);
        if (r.base=R_NO) and (r.index=R_NO) then
          list.concat(Taicpu.Op_sym_ofs(A_PUSH,S_L,r.symbol,r.offset))
        else if (r.base=R_NO) and (r.index<>R_NO) and
                (r.offset=0) and (r.scalefactor=0) and (r.symbol=nil) then
          list.concat(Taicpu.Op_reg(A_PUSH,S_L,r.index))
        else if (r.base<>R_NO) and (r.index=R_NO) and
                (r.offset=0) and (r.symbol=nil) then
          list.concat(Taicpu.Op_reg(A_PUSH,S_L,r.base))
        else
          begin
            tmpreg := get_scratch_reg(list);
            a_loadaddr_ref_reg(list,r,tmpreg);
            list.concat(taicpu.op_reg(A_PUSH,S_L,tmpreg));
            free_scratch_reg(list,tmpreg);
          end;
      end;

    procedure tcg386.a_call_name(list : taasmoutput;const s : string);

      begin
        list.concat(taicpu.op_sym(A_CALL,S_NO,newasmsymbol(s)));
      end;

    procedure tcg386.a_call_ref(list : taasmoutput;const ref : treference);

      begin
        list.concat(taicpu.op_ref(A_CALL,S_NO,ref));
      end;



{********************** load instructions ********************}

    procedure tcg386.a_load_const_reg(list : taasmoutput; size: TCGSize; a : aword; reg : TRegister);

      begin
        { the optimizer will change it to "xor reg,reg" when loading zero, }
        { no need to do it here too (JM)                                   }
        list.concat(taicpu.op_const_reg(A_MOV,TCGSize2OpSize[size],a,reg))
      end;


    procedure tcg386.a_load_const_ref(list : taasmoutput; size: tcgsize; a : aword;const ref : treference);

      begin
{$ifdef OPTLOAD0}
        { zero is often used several times in succession -> load it in a  }
        { register and then store it to memory, so the optimizer can then }
        { remove the unnecessary loads of registers and you get smaller   }
        { (and faster) code                                               }
        if (a = 0) and
           (size in [OS_32,OS_S32]) then
          inherited a_load_const_ref(list,size,a,ref)
        else
{$endif OPTLOAD0}
          list.concat(taicpu.op_const_ref(A_MOV,TCGSize2OpSize[size],a,ref));
      end;


    procedure tcg386.a_load_reg_ref(list : taasmoutput; size: TCGSize; reg : tregister;const ref : treference);

      begin
        list.concat(taicpu.op_reg_ref(A_MOV,TCGSize2OpSize[size],reg,
          ref));
      End;


    procedure tcg386.a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref: treference;reg : tregister);

      var
        op: tasmop;
        s: topsize;

      begin
        sizes2load(size,reg2opsize[reg],op,s);
        list.concat(taicpu.op_ref_reg(op,s,ref,reg));
      end;


    procedure tcg386.a_load_reg_reg(list : taasmoutput;size : tcgsize;reg1,reg2 : tregister);

      var
        op: tasmop;
        s: topsize;

      begin
        sizes2load(size,reg2opsize[reg2],op,s);
        if (rg.makeregsize(reg1,OS_INT) = rg.makeregsize(reg2,OS_INT)) then
         begin
           { "mov reg1, reg1" doesn't make sense }
           if op = A_MOV then
             exit;
           { optimize movzx with "and ffff,<reg>" operation }
           if (op = A_MOVZX) then
            begin
              case size of
                OS_8:
                  begin
                    list.concat(taicpu.op_const_reg(A_AND,reg2opsize[reg2],255,reg2));
                    exit;
                  end;
                OS_16:
                  begin
                    list.concat(taicpu.op_const_reg(A_AND,reg2opsize[reg2],65535,reg2));
                    exit;
                  end;
              end;
            end;
         end;
        list.concat(taicpu.op_reg_reg(op,s,reg1,reg2));
      end;


    procedure tcg386.a_load_sym_ofs_reg(list: taasmoutput; const sym: tasmsymbol; ofs: longint; reg: tregister);

      begin
        list.concat(taicpu.op_sym_ofs_reg(A_MOV,S_L,sym,ofs,reg));
      end;


    procedure tcg386.a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);

      begin
        if assigned(ref.symbol) and
           (ref.base=R_NO) and
           (ref.index=R_NO) then
         list.concat(taicpu.op_sym_ofs_reg(A_MOV,S_L,ref.symbol,ref.offset,r))
        else
         list.concat(taicpu.op_ref_reg(A_LEA,S_L,ref,r));
      end;


    { all fpu load routines expect that R_ST[0-7] means an fpu regvar and }
    { R_ST means "the current value at the top of the fpu stack" (JM)     }
    procedure tcg386.a_loadfpu_reg_reg(list: taasmoutput; reg1, reg2: tregister);

       begin
         if (reg1 <> R_ST) then
           begin
             list.concat(taicpu.op_reg(A_FLD,S_NO,
               trgcpu(rg).correct_fpuregister(reg1,trgcpu(rg).fpuvaroffset)));
             inc(trgcpu(rg).fpuvaroffset);
           end;
         if (reg2 <> R_ST) then
           begin
             list.concat(taicpu.op_reg(A_FSTP,S_NO,
                 trgcpu(rg).correct_fpuregister(reg2,trgcpu(rg).fpuvaroffset)));
             dec(trgcpu(rg).fpuvaroffset);
           end;
       end;


    procedure tcg386.a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister);

       begin
         floatload(list,size,ref);
         if (reg <> R_ST) then
           a_loadfpu_reg_reg(list,R_ST,reg);
       end;


    procedure tcg386.a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference);

       begin
         if reg <> R_ST then
           a_loadfpu_reg_reg(list,reg,R_ST);
         floatstore(list,size,ref);
       end;


    procedure tcg386.a_loadmm_reg_reg(list: taasmoutput; reg1, reg2: tregister);

       begin
         list.concat(taicpu.op_reg_reg(A_MOVQ,S_NO,reg1,reg2));
       end;


    procedure tcg386.a_loadmm_ref_reg(list: taasmoutput; const ref: treference; reg: tregister);

       begin
         list.concat(taicpu.op_ref_reg(A_MOVQ,S_NO,ref,reg));
       end;


    procedure tcg386.a_loadmm_reg_ref(list: taasmoutput; reg: tregister; const ref: treference);

       begin
         list.concat(taicpu.op_reg_ref(A_MOVQ,S_NO,reg,ref));
       end;


    procedure tcg386.a_parammm_reg(list: taasmoutput; reg: tregister);
       var
         href : treference;
       begin
         list.concat(taicpu.op_const_reg(A_SUB,S_L,8,R_ESP));
         reference_reset_base(href,R_ESP,0);
         list.concat(taicpu.op_reg_ref(A_MOVQ,S_NO,reg,href));
       end;


    procedure tcg386.a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister);

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
                  list.concat(taicpu.op_const_reg(opcode,reg2opsize[reg],power,
                    reg));
                  exit;
                end;
              { the rest should be handled specifically in the code      }
              { generator because of the silly register usage restraints }
              internalerror(200109224);
            End;
          OP_MUL,OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                begin
                  list.concat(taicpu.op_const_reg(A_SHL,reg2opsize[reg],power,
                    reg));
                  exit;
                end;
              if op = OP_IMUL then
                list.concat(taicpu.op_const_reg(A_IMUL,reg2opsize[reg],
                  a,reg))
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
                list.concat(taicpu.op_reg(A_INC,reg2opsize[reg],reg))
              else
                list.concat(taicpu.op_reg(A_DEC,reg2opsize[reg],reg))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                list.concat(taicpu.op_const_reg(A_MOV,reg2opsize[reg],0,reg))
            else if (a = high(aword)) and
                    (op in [OP_AND,OP_OR,OP_XOR]) then
                   begin
                     case op of
                       OP_AND:
                         exit;
                       OP_OR:
                         list.concat(taicpu.op_const_reg(A_MOV,reg2opsize[reg],high(aword),reg));
                       OP_XOR:
                         list.concat(taicpu.op_reg(A_NOT,reg2opsize[reg],reg));
                     end
                   end
            else
              list.concat(taicpu.op_const_reg(TOpCG2AsmOp[op],reg2opsize[reg],
                a,reg));
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_const_reg(
                  TOpCG2AsmOp[op],reg2opsize[reg],a and 31,reg));
              if (a shr 5) <> 0 Then
                internalerror(68991);
            end
          else internalerror(68992);
        end;
      end;


     procedure tcg386.a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; const ref: TReference);

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
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_const_ref(
                  TOpCG2AsmOp[op],TCgSize2OpSize[size],a and 31,ref));
              if (a shr 5) <> 0 Then
                internalerror(68991);
            end
          else internalerror(68992);
        end;
      end;


     procedure tcg386.a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; src, dst: TRegister);

        var
          regloadsize: tcgsize;
          dstsize: topsize;
          tmpreg : tregister;
          popecx : boolean;

        begin
          dstsize := tcgsize2opsize[size];
          dst := rg.makeregsize(dst,size);
          case op of
            OP_NEG,OP_NOT:
              begin
                if src <> R_NO then
                  internalerror(200112291);
                list.concat(taicpu.op_reg(TOpCG2AsmOp[op],dstsize,dst));
              end;
            OP_MUL,OP_DIV,OP_IDIV:
              { special stuff, needs separate handling inside code }
              { generator                                          }
              internalerror(200109233);
            OP_SHR,OP_SHL,OP_SAR:
              begin
                tmpreg := R_NO;
                { we need cl to hold the shift count, so if the destination }
                { is ecx, save it to a temp for now                         }
                if dst in [R_ECX,R_CX,R_CL] then
                  begin
                    case reg2opsize[dst] of
                      S_B: regloadsize := OS_8;
                      S_W: regloadsize := OS_16;
                      else regloadsize := OS_32;
                    end;
                    tmpreg := get_scratch_reg(list);
                    a_load_reg_reg(list,regloadsize,src,tmpreg);
                  end;
                if not(src in [R_ECX,R_CX,R_CL]) then
                  begin
                    { is ecx still free (it's also free if it was allocated }
                    { to dst, since we've moved dst somewhere else already) }
                    if not((dst = R_ECX) or
                           ((R_ECX in rg.unusedregsint) and
                            { this will always be true, it's just here to }
                            { allocate ecx                                }
                            (rg.getexplicitregisterint(list,R_ECX) = R_ECX))) then
                      begin
                        list.concat(taicpu.op_reg(A_PUSH,S_L,R_ECX));
                        popecx := true;
                      end;
                    a_load_reg_reg(list,OS_8,rg.makeregsize(src,OS_8),R_CL);
                  end
                else
                  src := R_CL;
                { do the shift }
                if tmpreg = R_NO then
                  list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],dstsize,
                    R_CL,dst))
                else
                  begin
                    list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],S_L,
                      R_CL,tmpreg));
                    { move result back to the destination }
                    a_load_reg_reg(list,OS_32,tmpreg,R_ECX);
                    free_scratch_reg(list,tmpreg);
                  end;
                if popecx then
                  list.concat(taicpu.op_reg(A_POP,S_L,R_ECX))
                else if not (dst in [R_ECX,R_CX,R_CL]) then
                  rg.ungetregisterint(list,R_ECX);
              end;
            else
              begin
                if reg2opsize[src] <> dstsize then
                  internalerror(200109226);
                list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],dstsize,
                  src,dst));
              end;
          end;
        end;


     procedure tcg386.a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister);
       begin
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


     procedure tcg386.a_op_reg_ref(list : taasmoutput; Op: TOpCG; size: TCGSize;reg: TRegister; const ref: TReference);

       var
         opsize: topsize;

       begin
         case op of
           OP_NEG,OP_NOT:
             begin
               if reg <> R_NO then
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
               opsize := tcgsize2opsize[size];
               list.concat(taicpu.op_reg_ref(TOpCG2AsmOp[op],opsize,reg,ref));
             end;
         end;
       end;


    procedure tcg386.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
        size: tcgsize; a: aword; src, dst: tregister);
      var
        tmpref: treference;
        power: longint;
        opsize: topsize;
      begin
        opsize := reg2opsize[src];
        if (opsize <> S_L) or
           not (size in [OS_32,OS_S32]) then
          begin
            inherited a_op_const_reg_reg(list,op,size,a,src,dst);
            exit;
          end;
        { if we get here, we have to do a 32 bit calculation, guaranteed }
        Case Op of
          OP_DIV, OP_IDIV, OP_MUL, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
          OP_SAR:
            { can't do anything special for these }
            inherited a_op_const_reg_reg(list,op,size,a,src,dst);
          OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                { can be done with a shift }
                inherited a_op_const_reg_reg(list,op,size,a,src,dst);
              list.concat(taicpu.op_const_reg_reg(A_IMUL,S_L,a,src,dst));
            end;
          OP_ADD, OP_SUB:
            if (a = 0) then
              a_load_reg_reg(list,size,src,dst)
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

    procedure tcg386.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
        size: tcgsize; src1, src2, dst: tregister);
      var
        tmpref: treference;
        opsize: topsize;
      begin
        opsize := reg2opsize[src1];
        if (opsize <> S_L) or
           (reg2opsize[src2] <> S_L) or
           not (size in [OS_32,OS_S32]) then
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

      procedure tcg386.a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
        l : tasmlabel);

        begin
          if (a = 0) then
            list.concat(taicpu.op_reg_reg(A_TEST,reg2opsize[reg],reg,reg))
          else
            list.concat(taicpu.op_const_reg(A_CMP,reg2opsize[reg],a,reg));
          a_jmp_cond(list,cmp_op,l);
        end;


      procedure tcg386.a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const ref : treference;
        l : tasmlabel);

        begin
          list.concat(taicpu.op_const_ref(A_CMP,TCgSize2OpSize[size],a,ref));
          a_jmp_cond(list,cmp_op,l);
        end;

      procedure tcg386.a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;
        reg1,reg2 : tregister;l : tasmlabel);

        begin
          if reg2opsize[reg1] <> reg2opsize[reg2] then
            internalerror(200109226);
          list.concat(taicpu.op_reg_reg(A_CMP,reg2opsize[reg1],reg1,reg2));
          a_jmp_cond(list,cmp_op,l);
        end;


     procedure tcg386.a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;const ref: treference; reg : tregister;l : tasmlabel);
        begin
          reg := rg.makeregsize(reg,size);
          list.concat(taicpu.op_ref_reg(A_CMP,tcgsize2opsize[size],ref,reg));
          a_jmp_cond(list,cmp_op,l);
        end;


     procedure tcg386.a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);

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


     procedure tcg386.a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel);
       var
         ai : taicpu;
       begin
         ai := Taicpu.op_sym(A_Jcc,S_NO,l);
         ai.SetCondition(flags_to_cond(f));
         ai.is_jmp := true;
         list.concat(ai);
       end;


     procedure tcg386.g_flags2reg(list: taasmoutput; const f: tresflags; reg: TRegister);

       var
         ai : taicpu;
         hreg : tregister;
       begin
          hreg := rg.makeregsize(reg,OS_8);
          ai:=Taicpu.Op_reg(A_Setcc,S_B,hreg);
          ai.SetCondition(flags_to_cond(f));
          list.concat(ai);
          if hreg<>reg then
           a_load_reg_reg(list,OS_8,hreg,reg);
       end;


{ ************* 64bit operations ************ }

    procedure tcg386.get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      begin
        case op of
          OP_ADD :
            begin
              op1:=A_ADD;
              op2:=A_ADC;
            end;
          OP_SUB :
            begin
              op1:=A_SUB;
              op2:=A_SBB;
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


    procedure tcg386.a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reglo,reghi : tregister);
      var
        op1,op2 : TAsmOp;
        tempref : treference;
      begin
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_ref_reg(op1,S_L,ref,reglo));
        tempref:=ref;
        inc(tempref.offset,4);
        list.concat(taicpu.op_ref_reg(op2,S_L,tempref,reghi));
      end;


    procedure tcg386.a_op64_reg_reg(list : taasmoutput;op:TOpCG;reglosrc,reghisrc,reglodst,reghidst : tregister);
      var
        op1,op2 : TAsmOp;
      begin
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_reg_reg(op1,S_L,reglosrc,reglodst));
        list.concat(taicpu.op_reg_reg(op2,S_L,reghisrc,reghidst));
      end;


    procedure tcg386.a_op64_const_reg(list : taasmoutput;op:TOpCG;valuelosrc,valuehisrc:AWord;reglodst,reghidst : tregister);
      var
        op1,op2 : TAsmOp;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              a_op_const_reg(list,op,valuelosrc,reglodst);
              a_op_const_reg(list,op,valuehisrc,reghidst);
            end;
          OP_ADD, OP_SUB:
            begin
              // can't use a_op_const_ref because this may use dec/inc
              get_64bit_ops(op,op1,op2);
              list.concat(taicpu.op_const_reg(op1,S_L,valuelosrc,reglodst));
              list.concat(taicpu.op_const_reg(op2,S_L,valuehisrc,reghidst));
            end;
          else
            internalerror(200204021);
        end;
      end;


    procedure tcg386.a_op64_const_ref(list : taasmoutput;op:TOpCG;valuelosrc,valuehisrc:AWord;const ref : treference);
      var
        op1,op2 : TAsmOp;
        tempref : treference;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              a_op_const_ref(list,op,OS_32,valuelosrc,ref);
              tempref:=ref;
              inc(tempref.offset,4);
              a_op_const_ref(list,op,OS_32,valuehisrc,tempref);
            end;
          OP_ADD, OP_SUB:
            begin
              get_64bit_ops(op,op1,op2);
              // can't use a_op_const_ref because this may use dec/inc
              list.concat(taicpu.op_const_ref(op1,S_L,valuelosrc,ref));
              tempref:=ref;
              inc(tempref.offset,4);
              list.concat(taicpu.op_const_ref(op2,S_L,valuehisrc,tempref));
            end;
          else
            internalerror(200204022);
        end;
      end;


{ ************* concatcopy ************ }

    procedure tcg386.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);
      var
         ecxpushed : boolean;
         helpsize : longint;
         i : byte;
         reg8,reg32 : tregister;
         srcref,dstref : treference;
         swap : boolean;

         procedure maybepushecx;
         begin
           if not(R_ECX in rg.unusedregsint) then
             begin
               list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_ECX));
               ecxpushed:=true;
             end
           else rg.getexplicitregisterint(list,R_ECX);
         end;

      begin
         if (not loadref) and
            ((len<=8) or
             (not(cs_littlesize in aktglobalswitches ) and (len<=12))) then
           begin
              helpsize:=len shr 2;
              rg.getexplicitregisterint(list,R_EDI);
              dstref:=dest;
              srcref:=source;
              for i:=1 to helpsize do
                begin
                   a_load_ref_reg(list,OS_32,srcref,R_EDI);
                   If (len = 4) and delsource then
                     reference_release(list,source);
                   a_load_reg_ref(list,OS_32,R_EDI,dstref);
                   inc(srcref.offset,4);
                   inc(dstref.offset,4);
                   dec(len,4);
                end;
              if len>1 then
                begin
                   a_load_ref_reg(list,OS_16,srcref,R_DI);
                   If (len = 2) and delsource then
                     reference_release(list,source);
                   a_load_reg_ref(list,OS_16,R_DI,dstref);
                   inc(srcref.offset,2);
                   inc(dstref.offset,2);
                   dec(len,2);
                end;
              rg.ungetregisterint(list,R_EDI);
              if len>0 then
                begin
                   { and now look for an 8 bit register }
                   swap:=false;
                   if R_EAX in rg.unusedregsint then reg8:=rg.makeregsize(rg.getexplicitregisterint(list,R_EAX),OS_8)
                   else if R_EDX in rg.unusedregsint then reg8:=rg.makeregsize(rg.getexplicitregisterint(list,R_EDX),OS_8)
                   else if R_EBX in rg.unusedregsint then reg8:=rg.makeregsize(rg.getexplicitregisterint(list,R_EBX),OS_8)
                   else if R_ECX in rg.unusedregsint then reg8:=rg.makeregsize(rg.getexplicitregisterint(list,R_ECX),OS_8)
                   else
                      begin
                         swap:=true;
                         { we need only to check 3 registers, because }
                         { one is always not index or base          }
                         if (dest.base<>R_EAX) and (dest.index<>R_EAX) then
                           begin
                              reg8:=R_AL;
                              reg32:=R_EAX;
                           end
                         else if (dest.base<>R_EBX) and (dest.index<>R_EBX) then
                           begin
                              reg8:=R_BL;
                              reg32:=R_EBX;
                           end
                         else if (dest.base<>R_ECX) and (dest.index<>R_ECX) then
                           begin
                              reg8:=R_CL;
                              reg32:=R_ECX;
                           end;
                      end;
                   if swap then
                     { was earlier XCHG, of course nonsense }
                     begin
                       rg.getexplicitregisterint(list,R_EDI);
                       a_load_reg_reg(list,OS_32,reg32,R_EDI);
                     end;
                   a_load_ref_reg(list,OS_8,srcref,reg8);
                   If delsource and (len=1) then
                     reference_release(list,source);
                   a_load_reg_ref(list,OS_8,reg8,dstref);
                   if swap then
                     begin
                       a_load_reg_reg(list,OS_32,R_EDI,reg32);
                       rg.ungetregisterint(list,R_EDI);
                     end
                   else
                     rg.ungetregister(list,reg8);
                end;
           end
         else
           begin
              rg.getexplicitregisterint(list,R_EDI);
              a_loadaddr_ref_reg(list,dest,R_EDI);
              list.concat(Tairegalloc.Alloc(R_ESI));
              if loadref then
                a_load_ref_reg(list,OS_ADDR,source,R_ESI)
              else
                begin
                  a_loadaddr_ref_reg(list,source,R_ESI);
                  if delsource then
                    reference_release(list,source);
                end;

              list.concat(Taicpu.Op_none(A_CLD,S_NO));
              ecxpushed:=false;
              if cs_littlesize in aktglobalswitches  then
                begin
                   maybepushecx;
                   a_load_const_reg(list,OS_INT,len,R_ECX);
                   list.concat(Taicpu.Op_none(A_REP,S_NO));
                   list.concat(Taicpu.Op_none(A_MOVSB,S_NO));
                end
              else
                begin
                   helpsize:=len shr 2;
                   len:=len and 3;
                   if helpsize>1 then
                    begin
                      maybepushecx;
                      a_load_const_reg(list,OS_INT,helpsize,R_ECX);
                      list.concat(Taicpu.Op_none(A_REP,S_NO));
                    end;
                   if helpsize>0 then
                    list.concat(Taicpu.Op_none(A_MOVSD,S_NO));
                   if len>1 then
                     begin
                        dec(len,2);
                        list.concat(Taicpu.Op_none(A_MOVSW,S_NO));
                     end;
                   if len=1 then
                     list.concat(Taicpu.Op_none(A_MOVSB,S_NO));
                end;
              rg.ungetregisterint(list,R_EDI);
              list.concat(Tairegalloc.DeAlloc(R_ESI));
              if ecxpushed then
                list.concat(Taicpu.Op_reg(A_POP,S_L,R_ECX))
              else
                rg.ungetregisterint(list,R_ECX);

              { loading SELF-reference again }
              g_maybe_loadself(list);
           end;
         if delsource then
          tg.ungetiftemp(list,source);
      end;


    procedure tcg386.g_push_exception(list : taasmoutput;const exceptbuf:treference;l:AWord; exceptlabel:TAsmLabel);
      var
        tempaddr,tempbuf : treference;
      begin
         tempaddr:=exceptbuf;
         tempbuf:=exceptbuf;
         inc(tempbuf.offset,12);
         a_paramaddr_ref(list,tempaddr,3);
         a_paramaddr_ref(list,tempbuf,2);
         a_param_const(list,OS_INT,l,1);
         a_call_name(list,'FPC_PUSHEXCEPTADDR');

         a_reg_alloc(list,accumulator);
         a_param_reg(list,OS_ADDR,accumulator,1);
         a_reg_dealloc(list,accumulator);
         a_call_name(list,'FPC_SETJMP');
         list.concat(Tairegalloc.Alloc(accumulator));
         list.concat(Taicpu.op_reg(A_PUSH,S_L,accumulator));
         list.concat(Tairegalloc.DeAlloc(accumulator));
         a_cmp_const_reg_label(list,OS_ADDR,OC_NE,0,accumulator,exceptlabel);
      end;


    procedure tcg386.g_pop_exception(list : taasmoutput;endexceptlabel:tasmlabel);
      begin
        a_call_name(list,'FPC_POPADDRSTACK');
        a_reg_alloc(list,accumulator);
        list.concat(Taicpu.op_reg(A_POP,S_L,accumulator));
        a_reg_dealloc(list,accumulator);
        a_cmp_const_reg_label(list,OS_ADDR,OC_EQ,0,accumulator,endexceptlabel);
      end;


{****************************************************************************
                              Entry/Exit Code Helpers
****************************************************************************}

    procedure tcg386.g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer);
      var
        lenref : treference;
        power,len  : longint;
        opsize : topsize;
{$ifndef __NOWINPECOFF__}
        again,ok : tasmlabel;
{$endif}
      begin
        lenref:=ref;
        inc(lenref.offset,4);
        { get stack space }
        rg.getexplicitregisterint(list,R_EDI);
        list.concat(Taicpu.op_ref_reg(A_MOV,S_L,lenref,R_EDI));
        list.concat(Taicpu.op_reg(A_INC,S_L,R_EDI));
        if (elesize<>1) then
         begin
           if ispowerof2(elesize, power) then
             list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,R_EDI))
           else
             list.concat(Taicpu.op_const_reg(A_IMUL,S_L,elesize,R_EDI));
         end;
{$ifndef __NOWINPECOFF__}
        { windows guards only a few pages for stack growing, }
        { so we have to access every page first              }
        if target_info.target=target_i386_win32 then
          begin
             getlabel(again);
             getlabel(ok);
             a_label(list,again);
             list.concat(Taicpu.op_const_reg(A_CMP,S_L,winstackpagesize,R_EDI));
             a_jmp_cond(list,OC_B,ok);
             list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP));
             list.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
             list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize,R_EDI));
             a_jmp_always(list,again);

             a_label(list,ok);
             list.concat(Taicpu.op_reg_reg(A_SUB,S_L,R_EDI,R_ESP));
             rg.ungetregisterint(list,R_EDI);
             { now reload EDI }
             rg.getexplicitregisterint(list,R_EDI);
             list.concat(Taicpu.op_ref_reg(A_MOV,S_L,lenref,R_EDI));
             list.concat(Taicpu.op_reg(A_INC,S_L,R_EDI));

             if (elesize<>1) then
              begin
                if ispowerof2(elesize, power) then
                  list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,R_EDI))
                else
                  list.concat(Taicpu.op_const_reg(A_IMUL,S_L,elesize,R_EDI));
              end;
          end
        else
{$endif __NOWINPECOFF__}
          list.concat(Taicpu.op_reg_reg(A_SUB,S_L,R_EDI,R_ESP));
        { load destination }
        list.concat(Taicpu.op_reg_reg(A_MOV,S_L,R_ESP,R_EDI));

        { don't destroy the registers! }
        list.concat(Taicpu.op_reg(A_PUSH,S_L,R_ECX));
        list.concat(Taicpu.op_reg(A_PUSH,S_L,R_ESI));

        { load count }
        list.concat(Taicpu.op_ref_reg(A_MOV,S_L,lenref,R_ECX));

        { load source }
        list.concat(Taicpu.op_ref_reg(A_MOV,S_L,ref,R_ESI));

        { scheduled .... }
        list.concat(Taicpu.op_reg(A_INC,S_L,R_ECX));

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
          list.concat(Taicpu.op_const_reg(A_SHL,S_L,power,R_ECX))
        else
          list.concat(Taicpu.op_const_reg(A_IMUL,S_L,len,R_ECX));
        list.concat(Taicpu.op_none(A_REP,S_NO));
        case opsize of
          S_B : list.concat(Taicpu.Op_none(A_MOVSB,S_NO));
          S_W : list.concat(Taicpu.Op_none(A_MOVSW,S_NO));
          S_L : list.concat(Taicpu.Op_none(A_MOVSD,S_NO));
        end;
        rg.ungetregisterint(list,R_EDI);
        list.concat(Taicpu.op_reg(A_POP,S_L,R_ESI));
        list.concat(Taicpu.op_reg(A_POP,S_L,R_ECX));

        { patch the new address }
        list.concat(Taicpu.op_reg_ref(A_MOV,S_L,R_ESP,ref));
      end;


    procedure tcg386.g_interrupt_stackframe_entry(list : taasmoutput);
      begin
        { .... also the segment registers }
        list.concat(Taicpu.Op_reg(A_PUSH,S_W,R_GS));
        list.concat(Taicpu.Op_reg(A_PUSH,S_W,R_FS));
        list.concat(Taicpu.Op_reg(A_PUSH,S_W,R_ES));
        list.concat(Taicpu.Op_reg(A_PUSH,S_W,R_DS));
        { save the registers of an interrupt procedure }
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_ESI));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDX));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_ECX));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EBX));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EAX));
      end;


    procedure tcg386.g_interrupt_stackframe_exit(list : taasmoutput;selfused,accused,acchiused:boolean);
      begin
        if accused then
         list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,R_ESP))
        else
         list.concat(Taicpu.Op_reg(A_POP,S_L,R_EAX));
        list.concat(Taicpu.Op_reg(A_POP,S_L,R_EBX));
        list.concat(Taicpu.Op_reg(A_POP,S_L,R_ECX));
        if acchiused then
         list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,R_ESP))
        else
         list.concat(Taicpu.Op_reg(A_POP,S_L,R_EDX));
        if selfused then
         list.concat(Taicpu.Op_const_reg(A_ADD,S_L,4,R_ESP))
        else
         list.concat(Taicpu.Op_reg(A_POP,S_L,R_ESI));
        list.concat(Taicpu.Op_reg(A_POP,S_L,R_EDI));
        { .... also the segment registers }
        list.concat(Taicpu.Op_reg(A_POP,S_W,R_DS));
        list.concat(Taicpu.Op_reg(A_POP,S_W,R_ES));
        list.concat(Taicpu.Op_reg(A_POP,S_W,R_FS));
        list.concat(Taicpu.Op_reg(A_POP,S_W,R_GS));
        { this restores the flags }
        list.concat(Taicpu.Op_none(A_IRET,S_NO));
      end;


    procedure tcg386.g_profilecode(list : taasmoutput);
      var
        pl : tasmlabel;
      begin
        case target_info.target of
           target_i386_win32,
           target_i386_freebsd,
           target_i386_wdosx,
           target_i386_linux:
             begin
                getaddrlabel(pl);
                list.concat(Tai_section.Create(sec_data));
                list.concat(Tai_align.Create(4));
                list.concat(Tai_label.Create(pl));
                list.concat(Tai_const.Create_32bit(0));
                list.concat(Tai_section.Create(sec_code));
                list.concat(Taicpu.Op_sym_ofs_reg(A_MOV,S_L,pl,0,R_EDX));
                a_call_name(list,target_info.Cprefix+'mcount');
                include(rg.usedinproc,R_EDX);
             end;

           target_i386_go32v2:
             begin
               a_call_name(list,'MCOUNT');
             end;
        end;
      end;


    procedure tcg386.g_stackframe_entry(list : taasmoutput;localsize : longint);
      var
        href : treference;
        i : integer;
        again : tasmlabel;
      begin
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EBP));
        list.concat(Taicpu.Op_reg_reg(A_MOV,S_L,R_ESP,R_EBP));
        if localsize>0 then
         begin
{$ifndef NOTARGETWIN32}
           { windows guards only a few pages for stack growing, }
           { so we have to access every page first              }
           if (target_info.target=target_i386_win32) and
              (localsize>=winstackpagesize) then
             begin
               if localsize div winstackpagesize<=5 then
                 begin
                    list.concat(Taicpu.Op_const_reg(A_SUB,S_L,localsize-4,R_ESP));
                    for i:=1 to localsize div winstackpagesize do
                      begin
                         reference_reset_base(href,R_ESP,localsize-i*winstackpagesize);
                         list.concat(Taicpu.op_const_ref(A_MOV,S_L,0,href));
                      end;
                    list.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
                 end
               else
                 begin
                    getlabel(again);
                    rg.getexplicitregisterint(list,R_EDI);
                    list.concat(Taicpu.op_const_reg(A_MOV,S_L,localsize div winstackpagesize,R_EDI));
                    a_label(list,again);
                    list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP));
                    list.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
                    list.concat(Taicpu.op_reg(A_DEC,S_L,R_EDI));
                    a_jmp_cond(list,OC_NE,again);
                    rg.ungetregisterint(list,R_EDI);
                    list.concat(Taicpu.op_const_reg(A_SUB,S_L,localsize mod winstackpagesize,R_ESP));
                 end
             end
           else
{$endif NOTARGETWIN32}
            list.concat(Taicpu.Op_const_reg(A_SUB,S_L,localsize,R_ESP));
         end;
      end;


    procedure tcg386.g_restore_frame_pointer(list : taasmoutput);
      begin
        list.concat(Taicpu.Op_none(A_LEAVE,S_NO));
      end;


    procedure tcg386.g_return_from_proc(list : taasmoutput;parasize : aword);
      begin
        { Routines with the poclearstack flag set use only a ret }
        { also routines with parasize=0     }
        if (po_clearstack in aktprocdef.procoptions) then
         begin
           { complex return values are removed from stack in C code PM }
           if ret_in_param(aktprocdef.rettype.def) then
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


    procedure tcg386.g_call_constructor_helper(list : taasmoutput);
      begin
        if is_class(procinfo^._class) then
          begin
            procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            a_call_name(list,'FPC_NEW_CLASS');
            list.concat(Taicpu.Op_cond_sym(A_Jcc,C_Z,S_NO,faillabel));
          end
        else if is_object(procinfo^._class) then
          begin
            rg.getexplicitregisterint(list,R_EDI);
            a_load_const_reg(list,OS_ADDR,procinfo^._class.vmt_offset,R_EDI);
            a_call_name(list,'FPC_HELP_CONSTRUCTOR');
            list.concat(Taicpu.Op_cond_sym(A_Jcc,C_Z,S_NO,faillabel));
          end
        else
          internalerror(200006161);
      end;


    procedure tcg386.g_call_destructor_helper(list : taasmoutput);
      var
        nofinal : tasmlabel;
        href : treference;
      begin
        if is_class(procinfo^._class) then
         begin
           a_call_name(list,'FPC_DISPOSE_CLASS')
         end
        else if is_object(procinfo^._class) then
         begin
           { must the object be finalized ? }
           if procinfo^._class.needs_inittable then
            begin
              getlabel(nofinal);
              reference_reset_base(href,R_EBP,8);
              a_cmp_const_ref_label(list,OS_ADDR,OC_EQ,0,href,nofinal);
              reference_reset_base(href,R_ESI,0);
              g_finalize(list,procinfo^._class,href,false);
              a_label(list,nofinal);
            end;
           rg.getexplicitregisterint(list,R_EDI);
           a_load_const_reg(list,OS_ADDR,procinfo^._class.vmt_offset,R_EDI);
           rg.ungetregisterint(list,R_EDI);
           a_call_name(list,'FPC_HELP_DESTRUCTOR')
         end
        else
         internalerror(200006162);
      end;


    procedure tcg386.g_call_fail_helper(list : taasmoutput);
      var
        href : treference;
      begin
        if is_class(procinfo^._class) then
          begin
            reference_reset_base(href,procinfo^.framepointer,8);
            a_load_ref_reg(list,OS_ADDR,href,R_ESI);
            a_call_name(list,'FPC_HELP_FAIL_CLASS');
          end
        else if is_object(procinfo^._class) then
          begin
            reference_reset_base(href,procinfo^.framepointer,12);
            a_load_ref_reg(list,OS_ADDR,href,R_ESI);
            rg.getexplicitregisterint(list,R_EDI);
            a_load_const_reg(list,OS_ADDR,procinfo^._class.vmt_offset,R_EDI);
            a_call_name(list,'FPC_HELP_FAIL');
            rg.ungetregisterint(list,R_EDI);
          end
        else
          internalerror(200006163);
      end;


    procedure tcg386.g_save_standard_registers(list : taasmoutput);
      begin
        if (R_EBX in aktprocdef.usedregisters) then
          list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EBX));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_ESI));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
      end;


    procedure tcg386.g_restore_standard_registers(list : taasmoutput);
      begin
        list.concat(Taicpu.Op_reg(A_POP,S_L,R_EDI));
        list.concat(Taicpu.Op_reg(A_POP,S_L,R_ESI));
        if (R_EBX in aktprocdef.usedregisters) then
         list.concat(Taicpu.Op_reg(A_POP,S_L,R_EBX));
      end;


    procedure tcg386.g_save_all_registers(list : taasmoutput);
      begin
        list.concat(Taicpu.Op_none(A_PUSHA,S_L));
      end;


    procedure tcg386.g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);
      var
        href : treference;
      begin
        if selfused then
         begin
           reference_reset_base(href,R_ESP,4);
           list.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_ESI,href));
         end;
        if acchiused then
         begin
           reference_reset_base(href,R_ESP,20);
           list.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDX,href));
         end;
        if accused then
         begin
           reference_reset_base(href,R_ESP,28);
           list.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EAX,href));
         end;
        list.concat(Taicpu.Op_none(A_POPA,S_L));
        { We add a NOP because of the 386DX CPU bugs with POPAD }
        list.concat(taicpu.op_none(A_NOP,S_L));
      end;



begin
  cg := tcg386.create;
end.
{
  $Log$
  Revision 1.16  2002-05-12 19:59:05  carl
  * some small portability fixes

  Revision 1.15  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.14  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.13  2002/04/21 15:31:05  carl
  * changeregsize -> rg.makeregsize
  + a_jmp_always added

  Revision 1.12  2002/04/15 19:44:20  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.11  2002/04/04 19:06:10  peter
    * removed unused units
    * use tlocation.size in a_*loc*() routines

  Revision 1.10  2002/04/02 20:29:02  jonas
    * optimized the code generated by the a_op_const_* and a_op64_const
      methods

  Revision 1.9  2002/04/02 17:11:33  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.8  2002/03/31 20:26:37  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.7  2002/03/04 19:10:12  peter
    * removed compiler warnings

  Revision 1.6  2001/12/30 17:24:46  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.5  2001/12/29 15:29:59  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.4  2001/10/04 14:33:28  jonas
    * fixed range check errors

  Revision 1.3  2001/09/30 16:17:18  jonas
    * made most constant and mem handling processor independent

  Revision 1.2  2001/09/29 21:32:19  jonas
    * fixed bug in a_load_reg_reg + implemented a_call

  Revision 1.1  2001/09/28 20:39:33  jonas
    * changed all flow control structures (except for exception handling
      related things) to processor independent code (in new ncgflw unit)
    + generic cgobj unit which contains lots of code generator helpers with
      global "cg" class instance variable
    + cgcpu unit for i386 (implements processor specific routines of the above
      unit)
    * updated cgbase and cpubase for the new code generator units
    * include ncgflw unit in cpunode unit

}
