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
       cpubase,cpuinfo,cpupara,
       node,symconst;

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
        procedure a_load_reg_reg(list : taasmoutput;fromsize,tosize : tcgsize;reg1,reg2 : tregister);override;
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

        procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: tresflags; reg: TRegister); override;
        procedure g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref: TReference); override;

        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;

        procedure g_exception_reason_save(list : taasmoutput; const href : treference);override;
        procedure g_exception_reason_save_const(list : taasmoutput; const href : treference; a: aword);override;
        procedure g_exception_reason_load(list : taasmoutput; const href : treference);override;

        class function reg_cgsize(const reg: tregister): tcgsize; override;

        { entry/exit code helpers }
        procedure g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer);override;
        procedure g_removevaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer);override;
        procedure g_interrupt_stackframe_entry(list : taasmoutput);override;
        procedure g_interrupt_stackframe_exit(list : taasmoutput;selfused,accused,acchiused:boolean);override;
        procedure g_profilecode(list : taasmoutput);override;
        procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
        procedure g_restore_frame_pointer(list : taasmoutput);override;
        procedure g_return_from_proc(list : taasmoutput;parasize : aword);override;
{$ifndef TEST_GENERIC}
        procedure g_call_constructor_helper(list : taasmoutput);override;
        procedure g_call_destructor_helper(list : taasmoutput);override;
        procedure g_call_fail_helper(list : taasmoutput);override;
{$endif}
        procedure g_save_standard_registers(list : taasmoutput; usedinproc : tregisterset);override;
        procedure g_restore_standard_registers(list : taasmoutput; usedinproc : tregisterset);override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);override;

        procedure g_overflowcheck(list: taasmoutput; const p: tnode);override;

      private
        procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);
        procedure sizes2load(s1 : tcgsize;s2 : topsize; var op: tasmop; var s3: topsize);

        procedure floatload(list: taasmoutput; t : tcgsize;const ref : treference);
        procedure floatstore(list: taasmoutput; t : tcgsize;const ref : treference);
        procedure floatloadops(t : tcgsize;var op : tasmop;var s : topsize);
        procedure floatstoreops(t : tcgsize;var op : tasmop;var s : topsize);
      end;

   const
      TCGSize2OpSize: Array[tcgsize] of topsize =
        (S_NO,S_B,S_W,S_L,S_L,S_B,S_W,S_L,S_L,
         S_FS,S_FL,S_FX,S_IQ,
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

    procedure tcgx86.sizes2load(s1 : tcgsize;s2: topsize; var op: tasmop; var s3: topsize);

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


{****************************************************************************
                              Assembler code
****************************************************************************}

    class function tcgx86.reg_cgsize(const reg: tregister): tcgsize;
      const
        regsize_2_cgsize: array[S_B..S_L] of tcgsize = (OS_8,OS_16,OS_32);
      begin
        result := regsize_2_cgsize[reg2opsize[reg]];
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
        tmpreg: tregister;

      begin
        case size of
          OS_8,OS_S8,
          OS_16,OS_S16:
            begin
              tmpreg := get_scratch_reg_address(list);
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


    procedure tcgx86.a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);

      var
        tmpreg: tregister;

      begin
        if r.segment<>R_NO then
          CGMessage(cg_e_cant_use_far_pointer_there);
        if (r.base=R_NO) and (r.index=R_NO) then
          begin
            if assigned(r.symbol) then
              list.concat(Taicpu.Op_sym_ofs(A_PUSH,S_L,r.symbol,r.offset))
            else
              list.concat(Taicpu.Op_const(A_PUSH,S_L,r.offset));
          end
        else if (r.base=R_NO) and (r.index<>R_NO) and
                (r.offset=0) and (r.scalefactor=0) and (r.symbol=nil) then
          list.concat(Taicpu.Op_reg(A_PUSH,S_L,r.index))
        else if (r.base<>R_NO) and (r.index=R_NO) and
                (r.offset=0) and (r.symbol=nil) then
          list.concat(Taicpu.Op_reg(A_PUSH,S_L,r.base))
        else
          begin
            tmpreg := get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list,r,tmpreg);
            list.concat(taicpu.op_reg(A_PUSH,S_L,tmpreg));
            free_scratch_reg(list,tmpreg);
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

    procedure tcgx86.a_load_const_reg(list : taasmoutput; size: TCGSize; a : aword; reg : TRegister);

      begin
        { the optimizer will change it to "xor reg,reg" when loading zero, }
        { no need to do it here too (JM)                                   }
        list.concat(taicpu.op_const_reg(A_MOV,TCGSize2OpSize[size],a,reg))
      end;


    procedure tcgx86.a_load_const_ref(list : taasmoutput; size: tcgsize; a : aword;const ref : treference);

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


    procedure tcgx86.a_load_reg_ref(list : taasmoutput; size: TCGSize; reg : tregister;const ref : treference);

      begin
        list.concat(taicpu.op_reg_ref(A_MOV,TCGSize2OpSize[size],reg,
          ref));
      End;


    procedure tcgx86.a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref: treference;reg : tregister);

      var
        op: tasmop;
        s: topsize;

      begin
        sizes2load(size,reg2opsize[reg],op,s);
        list.concat(taicpu.op_ref_reg(op,s,ref,reg));
      end;


    procedure tcgx86.a_load_reg_reg(list : taasmoutput;fromsize,tosize : tcgsize;reg1,reg2 : tregister);

      var
        op: tasmop;
        s: topsize;

      begin
        sizes2load(fromsize,reg2opsize[reg2],op,s);
        if (rg.makeregsize(reg1,OS_INT) = rg.makeregsize(reg2,OS_INT)) then
         begin
           { "mov reg1, reg1" doesn't make sense }
           if op = A_MOV then
             exit;
           { optimize movzx with "and ffff,<reg>" operation }
           if (op = A_MOVZX) then
            begin
              case fromsize of
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



    procedure tcgx86.a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);

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
    procedure tcgx86.a_loadfpu_reg_reg(list: taasmoutput; reg1, reg2: tregister);

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


    procedure tcgx86.a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister);

       begin
         floatload(list,size,ref);
         if (reg <> R_ST) then
           a_loadfpu_reg_reg(list,R_ST,reg);
       end;


    procedure tcgx86.a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference);

       begin
         if reg <> R_ST then
           a_loadfpu_reg_reg(list,reg,R_ST);
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
         list.concat(taicpu.op_const_reg(A_SUB,S_L,8,R_ESP));
         reference_reset_base(href,R_ESP,0);
         list.concat(taicpu.op_reg_ref(A_MOVQ,S_NO,reg,href));
       end;


    procedure tcgx86.a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister);

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
              if (a and 31) <> 0 Then
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
                popecx := false;
                { we need cl to hold the shift count, so if the destination }
                { is ecx, save it to a temp for now                         }
                if dst in [R_ECX,R_CX,R_CL] then
                  begin
                    case reg2opsize[dst] of
                      S_B: regloadsize := OS_8;
                      S_W: regloadsize := OS_16;
                      else regloadsize := OS_32;
                    end;
                    tmpreg := get_scratch_reg_int(list);
                    a_load_reg_reg(list,regloadsize,regloadsize,src,tmpreg);
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
                    a_load_reg_reg(list,OS_32,OS_32,rg.makeregsize(src,OS_32),R_ECX);
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
                    a_load_reg_reg(list,OS_32,OS_32,tmpreg,R_ECX);
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


     procedure tcgx86.a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister);
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


     procedure tcgx86.a_op_reg_ref(list : taasmoutput; Op: TOpCG; size: TCGSize;reg: TRegister; const ref: TReference);

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


    procedure tcgx86.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
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

    procedure tcgx86.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
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

      procedure tcgx86.a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
        l : tasmlabel);

        begin
          if (a = 0) then
            list.concat(taicpu.op_reg_reg(A_TEST,reg2opsize[reg],reg,reg))
          else
            list.concat(taicpu.op_const_reg(A_CMP,reg2opsize[reg],a,reg));
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
          if reg2opsize[reg1] <> reg2opsize[reg2] then
            internalerror(200109226);
          list.concat(taicpu.op_reg_reg(A_CMP,reg2opsize[reg1],reg1,reg2));
          a_jmp_cond(list,cmp_op,l);
        end;


     procedure tcgx86.a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;const ref: treference; reg : tregister;l : tasmlabel);
        begin
          reg := rg.makeregsize(reg,size);
          list.concat(taicpu.op_ref_reg(A_CMP,tcgsize2opsize[size],ref,reg));
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
          hreg := rg.makeregsize(reg,OS_8);
          ai:=Taicpu.Op_reg(A_Setcc,S_B,hreg);
          ai.SetCondition(flags_to_cond(f));
          list.concat(ai);
          if (reg <> hreg) then
            a_load_reg_reg(list,OS_8,size,hreg,reg);
       end;


     procedure tcgx86.g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref: TReference);

       var
         ai : taicpu;
       begin
          if not(size in [OS_8,OS_S8]) then
            a_load_const_ref(list,size,0,ref);
          ai:=Taicpu.Op_ref(A_Setcc,S_B,ref);
          ai.SetCondition(flags_to_cond(f));
          list.concat(ai);
       end;


{ ************* concatcopy ************ }

    procedure tcgx86.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);
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
                       a_load_reg_reg(list,OS_32,OS_32,reg32,R_EDI);
                     end;
                   a_load_ref_reg(list,OS_8,srcref,reg8);
                   If delsource and (len=1) then
                     reference_release(list,source);
                   a_load_reg_ref(list,OS_8,reg8,dstref);
                   if swap then
                     begin
                       a_load_reg_reg(list,OS_32,OS_32,R_EDI,reg32);
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
              list.concat(tai_regalloc.Alloc(R_ESI));
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
              list.concat(tai_regalloc.DeAlloc(R_ESI));
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



    procedure tcgx86.g_exception_reason_save(list : taasmoutput; const href : treference);
     begin
        list.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
     end;

    procedure tcgx86.g_exception_reason_save_const(list : taasmoutput;const href : treference; a: aword);
     begin
        list.concat(Taicpu.op_const(A_PUSH,S_L,a));
     end;

    procedure tcgx86.g_exception_reason_load(list : taasmoutput; const href : treference);
     begin
        list.concat(Taicpu.op_reg(A_POP,S_L,R_EAX));
     end;


{****************************************************************************
                              Entry/Exit Code Helpers
****************************************************************************}

    procedure tcgx86.g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer);
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
        if target_info.system=system_i386_win32 then
          begin
             objectlibrary.getlabel(again);
             objectlibrary.getlabel(ok);
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


    procedure tcgx86.g_removevaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer);
      var
        lenref : treference;
        power,len  : longint;
      begin
        lenref:=ref;
        inc(lenref.offset,4);
        { caluclate size and adjust stack space }
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
        list.concat(Taicpu.op_reg_reg(A_ADD,S_L,R_EDI,R_ESP));
      end;


    procedure tcgx86.g_interrupt_stackframe_entry(list : taasmoutput);
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


    procedure tcgx86.g_interrupt_stackframe_exit(list : taasmoutput;selfused,accused,acchiused:boolean);
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


    procedure tcgx86.g_profilecode(list : taasmoutput);
      var
        pl : tasmlabel;
      begin
        case target_info.system of
           system_i386_win32,
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
                list.concat(Taicpu.Op_sym_ofs_reg(A_MOV,S_L,pl,0,R_EDX));
                a_call_name(list,target_info.Cprefix+'mcount');
                include(rg.usedinproc,R_EDX);
             end;

           system_i386_go32v2:
             begin
               a_call_name(list,'MCOUNT');
             end;
        end;
      end;


    procedure tcgx86.g_stackframe_entry(list : taasmoutput;localsize : longint);
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
           if (target_info.system=system_i386_win32) and
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
                    objectlibrary.getlabel(again);
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


    procedure tcgx86.g_restore_frame_pointer(list : taasmoutput);
      begin
        list.concat(Taicpu.Op_none(A_LEAVE,S_NO));
      end;


    procedure tcgx86.g_return_from_proc(list : taasmoutput;parasize : aword);
      begin
        { Routines with the poclearstack flag set use only a ret }
        { also routines with parasize=0     }
        if (po_clearstack in aktprocdef.procoptions) then
         begin
           { complex return values are removed from stack in C code PM }
           if paramanager.ret_in_param(aktprocdef.rettype.def,aktprocdef.proccalloption) then
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

{$ifndef TEST_GENERIC}
    procedure tcgx86.g_call_constructor_helper(list : taasmoutput);
      begin
        if is_class(procinfo._class) then
          begin
            if (cs_implicit_exceptions in aktmoduleswitches) then
              procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
            a_call_name(list,'FPC_NEW_CLASS');
            list.concat(Taicpu.Op_cond_sym(A_Jcc,C_Z,S_NO,faillabel));
          end
        else if is_object(procinfo._class) then
          begin
            rg.getexplicitregisterint(list,R_EDI);
            a_load_const_reg(list,OS_ADDR,procinfo._class.vmt_offset,R_EDI);
            a_call_name(list,'FPC_HELP_CONSTRUCTOR');
            list.concat(Taicpu.Op_cond_sym(A_Jcc,C_Z,S_NO,faillabel));
          end
        else
          internalerror(200006161);
      end;

    procedure tcgx86.g_call_destructor_helper(list : taasmoutput);
      var
        nofinal : tasmlabel;
        href : treference;
      begin
        if is_class(procinfo._class) then
         begin
           a_call_name(list,'FPC_DISPOSE_CLASS')
         end
        else if is_object(procinfo._class) then
         begin
           { must the object be finalized ? }
           if procinfo._class.needs_inittable then
            begin
              objectlibrary.getlabel(nofinal);
              reference_reset_base(href,R_EBP,8);
              a_cmp_const_ref_label(list,OS_ADDR,OC_EQ,0,href,nofinal);
              reference_reset_base(href,R_ESI,0);
              g_finalize(list,procinfo._class,href,false);
              a_label(list,nofinal);
            end;
           rg.getexplicitregisterint(list,R_EDI);
           a_load_const_reg(list,OS_ADDR,procinfo._class.vmt_offset,R_EDI);
           rg.ungetregisterint(list,R_EDI);
           a_call_name(list,'FPC_HELP_DESTRUCTOR')
         end
        else
         internalerror(200006162);
      end;

    procedure tcgx86.g_call_fail_helper(list : taasmoutput);
      var
        href : treference;
      begin
        if is_class(procinfo._class) then
          begin
            reference_reset_base(href,procinfo.framepointer,8);
            a_load_ref_reg(list,OS_ADDR,href,R_ESI);
            a_call_name(list,'FPC_HELP_FAIL_CLASS');
          end
        else if is_object(procinfo._class) then
          begin
            reference_reset_base(href,procinfo.framepointer,12);
            a_load_ref_reg(list,OS_ADDR,href,R_ESI);
            rg.getexplicitregisterint(list,R_EDI);
            a_load_const_reg(list,OS_ADDR,procinfo._class.vmt_offset,R_EDI);
            a_call_name(list,'FPC_HELP_FAIL');
            rg.ungetregisterint(list,R_EDI);
          end
        else
          internalerror(200006163);
      end;
{$endif}


    procedure tcgx86.g_save_standard_registers(list : taasmoutput; usedinproc : tregisterset);
      begin
        if (R_EBX in usedinproc) then
          list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EBX));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_ESI));
        list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
      end;


    procedure tcgx86.g_restore_standard_registers(list : taasmoutput; usedinproc : tregisterset);
      begin
        list.concat(Taicpu.Op_reg(A_POP,S_L,R_EDI));
        list.concat(Taicpu.Op_reg(A_POP,S_L,R_ESI));
        if (R_EBX in usedinproc) then
         list.concat(Taicpu.Op_reg(A_POP,S_L,R_EBX));
      end;


    procedure tcgx86.g_save_all_registers(list : taasmoutput);
      begin
        list.concat(Taicpu.Op_none(A_PUSHA,S_L));
      end;


    procedure tcgx86.g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);
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


    { produces if necessary overflowcode }
    procedure tcgx86.g_overflowcheck(list: taasmoutput; const p: tnode);
      var
         hl : tasmlabel;
         ai : taicpu;
         cond : TAsmCond;
      begin
         if not(cs_check_overflow in aktlocalswitches) then
          exit;
         objectlibrary.getlabel(hl);
         if not ((p.resulttype.def.deftype=pointerdef) or
                ((p.resulttype.def.deftype=orddef) and
                 (torddef(p.resulttype.def).typ in [u64bit,u16bit,u32bit,u8bit,uchar,
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
  Revision 1.22  2002-11-25 17:43:29  peter
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
