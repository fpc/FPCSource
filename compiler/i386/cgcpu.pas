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
       cgbase,cgobj,cg64f32,aasm,cpuasm,cpubase,cpuinfo;

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


        procedure a_call_name(list : taasmoutput;const s : string;
          offset : longint);override;


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

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
          l : tasmlabel);override;
        procedure a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const ref : treference;
          l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;
        procedure a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;const ref: treference; reg : tregister; l : tasmlabel); override;

        procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel); override;
        procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: taasmoutput; const f: tresflags; reg: TRegister); override;


        procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
        procedure g_restore_frame_pointer(list : taasmoutput);override;
        procedure g_push_exception_value_reg(list : taasmoutput;reg : tregister);override;
        procedure g_push_exception_value_const(list : taasmoutput;reg : tregister);override;
        procedure g_pop_exception_value_reg(list : taasmoutput;reg : tregister);override;
        procedure g_return_from_proc(list : taasmoutput;parasize : aword); override;

        procedure a_loadaddress_ref_reg(list : taasmoutput;const ref : treference;r : tregister);override;

        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;

        function makeregsize(var reg: tregister; size: tcgsize): topsize; override;

        class function reg_cgsize(const reg: tregister): tcgsize; override;

       private

        procedure sizes2load(s1: tcgsize; s2: topsize; var op: tasmop; var s3: topsize);


      end;

    const

      TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_ADD,A_AND,A_DIV,
                            A_IDIV,A_MUL, A_IMUL, A_NEG,A_NOT,A_OR,
                            A_SAR,A_SHL,A_SHR,A_SUB,A_XOR);

      TOpCmp2AsmCond: Array[topcmp] of TAsmCond = (C_NONE,C_E,C_G,
                           C_L,C_GE,C_LE,C_NE,C_BE,C_B,C_AE,C_A);

      TCGSize2OpSize: Array[tcgsize] of topsize = (S_NO,S_B,S_W,S_L,S_L,
                                                        S_B,S_W,S_L,S_L);


  implementation

    uses
       globtype,globals,verbose,systems,cutils,cga,tgcpu;


    { we implement the following routines because otherwise we can't }
    { instantiate the class since it's abstract                      }

    procedure tcg386.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;nr : longint);

      begin
        runerror(211);
      end;


    procedure tcg386.a_param_const(list : taasmoutput;size : tcgsize;a : aword;nr : longint);

      begin
        runerror(211);
      end;


    procedure tcg386.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;nr : longint);

      var
        tmpreg: tregister;

      begin
        case size of
          OS_8,OS_S8,OS_16,OS_S16:
            begin
              tmpreg := get_scratch_reg(list);
              a_load_ref_reg(list,size,r,tmpreg);
              if target_info.alignment.paraalign = 2 then
                list.concat(taicpu.op_reg(A_PUSH,S_W,makereg16(tmpreg)))
              else
                list.concat(taicpu.op_reg(A_PUSH,S_L,tmpreg));
            end;
          OS_32,OS_S32:
            list.concat(taicpu.op_ref(A_PUSH,S_L,newreference(r)));
          else
            internalerror(200109301);
        end;
      end;


    procedure tcg386.a_paramaddr_ref(list : taasmoutput;const r : treference;nr : longint);

      begin
        runerror(211);
      end;

    procedure tcg386.a_call_name(list : taasmoutput;const s : string;
      offset : longint);

      begin
        list.concat(taicpu.op_sym_ofs(A_CALL,S_NO,newasmsymbol(s),offset));
      end;



{********************** load instructions ********************}

    procedure tcg386.a_load_const_reg(list : taasmoutput; size: TCGSize; a : aword; reg : TRegister);

      begin
        { the optimizer will change it to "xor reg,reg" when loading zero, }
        { no need to do it here too (JM)                                   }
        list.concat(taicpu.op_const_reg(A_MOV,TCGSize2OpSize[size],
          longint(a),reg))
      end;


    procedure tcg386.a_load_const_ref(list : taasmoutput; size: tcgsize; a : aword;const ref : treference);

      begin
        { zero is often used several times in succession -> load it in a  }
        { register and then store it to memory, so the optimizer can then }
        { remove the unnecessary loads of registers and you get smaller   }
        { (and faster) code                                               }
        if (a = 0) and
           (size in [OS_32,OS_S32]) then
          inherited a_load_const_ref(list,size,a,ref)
        else
          list.concat(taicpu.op_const_ref(A_MOV,TCGSize2OpSize[size],
            longint(a),newreference(ref)));
      end;


    procedure tcg386.a_load_reg_ref(list : taasmoutput; size: TCGSize; reg : tregister;const ref : treference);

      begin
        list.concat(taicpu.op_reg_ref(A_MOV,TCGSize2OpSize[size],reg,
          newreference(ref)));
      End;


    procedure tcg386.a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref: treference;reg : tregister);

      var
        op: tasmop;
        s: topsize;

      begin
        if ref.is_immediate then
          a_load_const_reg(list,size,aword(ref.offset),reg)
        else
          begin
            sizes2load(size,regsize(reg),op,s);
            list.concat(taicpu.op_ref_reg(op,s,newreference(ref),reg));
          end;
      end;


    procedure tcg386.a_load_reg_reg(list : taasmoutput;size : tcgsize;reg1,reg2 : tregister);

      var
        op: tasmop;
        s: topsize;

      begin
        sizes2load(size,regsize(reg2),op,s);
        if (makereg32(reg1) = makereg32(reg2)) then
          { "mov reg1, reg1" doesn't make sense }
          if op = A_MOV then
            exit
          else if (op = A_MOVZX) then
            case size of
              OS_8:
                begin
                  list.concat(taicpu.op_const_reg(A_AND,regsize(reg2),255,reg2));
                  exit;
                end;
              OS_16:
                begin
                  list.concat(taicpu.op_const_reg(A_AND,S_L,65535,reg1));
                  exit;
                end;
            end;
        list.concat(taicpu.op_reg_reg(op,s,reg1,reg2));
      end;


    procedure tcg386.a_load_sym_ofs_reg(list: taasmoutput; const sym: tasmsymbol; ofs: longint; reg: tregister);

      begin
        list.concat(taicpu.op_sym_ofs_reg(A_MOV,S_L,sym,ofs,reg));
      end;

    procedure tcg386.a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister);

      var
        opcode: tasmop;
        power: longint;

      begin
        Case Op of
          OP_DIV, OP_IDIV:
            Begin
              if ispowerof2(longint(a),power) then
                begin
                  case op of
                    OP_DIV:
                      opcode := A_SHR;
                    OP_IDIV:
                      opcode := A_SAR;
                  end;
                  list.concat(taicpu.op_const_reg(opcode,regsize(reg),power,
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
                 ispowerof2(longint(a),power) then
                begin
                  list.concat(taicpu.op_const_reg(A_SHL,regsize(reg),power,
                    reg));
                  exit;
                end;
              if op = OP_IMUL then
                list.concat(taicpu.op_const_reg(A_IMUL,regsize(reg),
                  longint(a),reg))
              else
                { OP_MUL should be handled specifically in the code        }
                { generator because of the silly register usage restraints }
                internalerror(200109225);
            end;
          OP_ADD, OP_AND, OP_OR, OP_SUB, OP_XOR:
            if (a = 1) and
               (op in [OP_ADD,OP_SUB]) then
              if op = OP_ADD then
                list.concat(taicpu.op_reg(A_INC,regsize(reg),reg))
              else
                list.concat(taicpu.op_reg(A_DEC,regsize(reg),reg))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                list.concat(taicpu.op_const_reg(A_MOV,regsize(reg),0,reg))
            else
              list.concat(taicpu.op_const_reg(TOpCG2AsmOp[op],regsize(reg),
                longint(a),reg));
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_const_reg(
                  TOpCG2AsmOp[op],regsize(reg),a and 31,reg));
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
              if ispowerof2(longint(a),power) then
                begin
                  case op of
                    OP_DIV:
                      opcode := A_SHR;
                    OP_IDIV:
                      opcode := A_SAR;
                  end;
                  list.concat(taicpu.op_const_ref(opcode,
                    TCgSize2OpSize[size],power,newreference(ref)));
                  exit;
                end;
              { the rest should be handled specifically in the code      }
              { generator because of the silly register usage restraints }
              internalerror(200109231);
            End;
          OP_MUL,OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(longint(a),power) then
                begin
                  list.concat(taicpu.op_const_ref(A_SHL,TCgSize2OpSize[size],
                    power,newreference(ref)));
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
            if (a = 1) and
               (op in [OP_ADD,OP_SUB]) then
              if op = OP_ADD then
                list.concat(taicpu.op_ref(A_INC,TCgSize2OpSize[size],
                  newreference(ref)))
              else
                list.concat(taicpu.op_ref(A_DEC,TCgSize2OpSize[size],
                  newreference(ref)))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                a_load_const_ref(list,size,0,ref)
            else
              list.concat(taicpu.op_const_ref(TOpCG2AsmOp[op],
                TCgSize2OpSize[size],longint(a),newreference(ref)));
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_const_ref(
                  TOpCG2AsmOp[op],TCgSize2OpSize[size],a and 31,newreference(ref)));
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
          dstsize := makeregsize(dst,size);
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
                    case regsize(dst) of
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
                           ((R_ECX in unused) and
                            { this will always be true, it's just here to }
                            { allocate ecx                                }
                            (getexplicitregister32(R_ECX) = R_ECX))) then
                      begin
                        list.concat(taicpu.op_reg(A_PUSH,S_L,R_ECX));
                        popecx := true;
                      end;
                    a_load_reg_reg(list,OS_8,makereg8(src),R_CL);
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
                  ungetregister32(R_ECX);
              end;
            else
              begin
                if regsize(src) <> dstsize then
                  internalerror(200109226);
                list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],dstsize,
                  src,dst));
              end;
          end;
        end;


     procedure tcg386.a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister);

       var
         opsize: topsize;

       begin
          if ref.is_immediate then
            a_op_const_reg(list,op,aword(ref.offset),reg)
          else
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
                    opsize := makeregsize(reg,size);
                    list.concat(taicpu.op_ref_reg(TOpCG2AsmOp[op],opsize,
                      newreference(ref),reg));
                  end;
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
               list.concat(taicpu.op_ref(TOpCG2AsmOp[op],tcgsize2opsize[size],
                 newreference(ref)));
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
               list.concat(taicpu.op_reg_ref(TOpCG2AsmOp[op],opsize,reg,
                 newreference(ref)));
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
        opsize := regsize(src);
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
                 ispowerof2(longint(a),power) then
                { can be done with a shift }
                inherited a_op_const_reg_reg(list,op,size,a,src,dst);
              list.concat(taicpu.op_const_reg_reg(A_IMUL,S_L,longint(a),src,dst));
            end;
          OP_ADD, OP_SUB:
            if (a = 0) then
              a_load_reg_reg(list,size,src,dst)
            else
              begin
                reset_reference(tmpref);
                tmpref.base := src;
                tmpref.offset := longint(a);
                if op = OP_SUB then
                  tmpref.offset := -tmpref.offset;
                list.concat(taicpu.op_ref_reg(A_LEA,S_L,newreference(tmpref),
                  dst));
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
        opsize := regsize(src1);
        if (opsize <> S_L) or
           (regsize(src2) <> S_L) or
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
              reset_reference(tmpref);
              tmpref.base := src1;
              tmpref.index := src2;
              tmpref.scalefactor := 1;
              list.concat(taicpu.op_ref_reg(A_LEA,S_L,newreference(tmpref),
                dst));
            end
          else internalerror(200112303);
        end;
      end;

{*************** compare instructructions ****************}

      procedure tcg386.a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
        l : tasmlabel);

        begin
          if a <> 0 then
            list.concat(taicpu.op_const_reg(A_CMP,regsize(reg),longint(a),
              reg))
          else
            list.concat(taicpu.op_reg_reg(A_TEST,regsize(reg),reg,reg));
          a_jmp_cond(list,cmp_op,l);
        end;

      procedure tcg386.a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const ref : treference;
        l : tasmlabel);

        begin
          list.concat(taicpu.op_const_ref(A_CMP,TCgSize2OpSize[size],
            longint(a),newreference(ref)));
          a_jmp_cond(list,cmp_op,l);
        end;

      procedure tcg386.a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;
        reg1,reg2 : tregister;l : tasmlabel);

        begin
          if regsize(reg1) <> regsize(reg2) then
            internalerror(200109226);
          list.concat(taicpu.op_reg_reg(A_CMP,regsize(reg1),reg1,reg2));
          a_jmp_cond(list,cmp_op,l);
        end;

     procedure tcg386.a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;const ref: treference; reg : tregister;l : tasmlabel);

        var
          opsize: topsize;

        begin
          opsize := makeregsize(reg,size);
          list.concat(taicpu.op_ref_reg(A_CMP,opsize,newreference(ref),reg));
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
          hreg := makereg8(reg);
          ai:=Taicpu.Op_reg(A_Setcc,S_B,hreg);
          ai.SetCondition(flags_to_cond(f));
          list.concat(ai);
          if hreg<>reg then
           begin
             if reg in regset16bit then
              emit_to_reg16(hreg)
             else
              emit_to_reg32(hreg);
           end;
       end;


{ *********** entry/exit code and address loading ************ }

    procedure tcg386.g_stackframe_entry(list : taasmoutput;localsize : longint);

      begin
        runerror(211);
      end;


    procedure tcg386.g_restore_frame_pointer(list : taasmoutput);

      begin
        runerror(211);
      end;


    procedure tcg386.g_push_exception_value_reg(list : taasmoutput;reg : tregister);

      begin
        runerror(211);
      end;


    procedure tcg386.g_push_exception_value_const(list : taasmoutput;reg : tregister);

      begin
        runerror(211);
      end;


    procedure tcg386.g_pop_exception_value_reg(list : taasmoutput;reg : tregister);

      begin
        runerror(211);
      end;


    procedure tcg386.g_return_from_proc(list : taasmoutput;parasize : aword);

      begin
        runerror(211);
      end;

     procedure tcg386.a_loadaddress_ref_reg(list : taasmoutput;const ref : treference;r : tregister);

       begin
         list.concat(taicpu.op_ref_reg(A_LEA,S_L,newreference(ref),r));
       end;

    function tcg386.makeregsize(var reg: tregister; size: tcgsize): topsize;

      begin
        { this function only allows downsizing a register, because otherwise }
        { we may start working with garbage (JM)                             }
        case size of
          OS_32,OS_S32:
            begin
              if not (reg in [R_EAX..R_EDI]) then
                internalerror(2001092313);
              result := S_L;
            end;
          OS_8,OS_S8:
            begin
              reg := makereg8(reg);
              result := S_B;
            end;
          OS_16,OS_S16:
            begin
              if reg in [R_AL..R_BH] then
                internalerror(2001092314);
              reg := makereg16(reg);
              result := S_W;
            end;
          else
            internalerror(2001092312);
        end;
      end;



{ ************* concatcopy ************ }

    procedure tcg386.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);

      { temp implementation, until it's permanenty moved here from cga.pas }

      var
        oldlist: taasmoutput;

      begin
        if list <> exprasmlist then
          begin
            oldlist := exprasmlist;
            exprasmlist := list;
          end;
        cga.concatcopy(source,dest,len,delsource,loadref);
        if list <> exprasmlist then
          list := oldlist;
      end;


    function tcg386.reg_cgsize(const reg: tregister): tcgsize;
      const
        regsize_2_cgsize: array[S_B..S_L] of tcgsize = (OS_8,OS_16,OS_32);
      begin
        result := regsize_2_cgsize[regsize(reg)];
      end;


{***************** This is private property, keep out! :) *****************}

    procedure tcg386.sizes2load(s1: tcgsize; s2: topsize; var op: tasmop; var s3: topsize);

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


begin
  cg := tcg386.create;
end.
{
  $Log$
  Revision 1.7  2002-03-04 19:10:12  peter
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
