{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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

  interface

    uses
       cgbase,cgobj,aasm,cpuasm,cpubase,cpuinfo;

    type
       pcgppc = ^tcgppc;

       tcgppc = object(tcg)
          procedure a_call_name(list : paasmoutput;const s : string;
            offset : longint);virtual;

          procedure a_op_reg_const(list : paasmoutput; Op: TOpCG; size: TCGSize; reg: TRegister; a: AWord); virtual;

          { move instructions }
          procedure a_load_const_reg(list : paasmoutput; size: tcgsize; a : aword;reg : tregister);virtual;
          procedure a_load_reg_ref(list : paasmoutput; size: tcgsize; reg : tregister;const ref2 : treference);virtual;
          procedure a_load_ref_reg(list : paasmoutput;size : tcgsize;const Ref2 : treference;reg : tregister);virtual;
          procedure a_load_reg_reg(list : paasmoutput;size : tcgsize;reg1,reg2 : tregister);virtual;

          {  comparison operations }
          procedure a_cmp_reg_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
            l : pasmlabel);virtual;
          procedure a_cmp_reg_reg_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : pasmlabel);
 {         procedure a_cmp_reg_ref_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg : tregister;
              const ref: treference; l : pasmlabel);
          procedure a_cmp_ref_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;l : longint;reg : tregister;
            l : pasmlabel);}

          procedure a_loadaddress_ref_reg(list : paasmoutput;const ref2 : treference;r : tregister);virtual;
          procedure g_stackframe_entry(list : paasmoutput;localsize : longint);virtual;
          procedure g_restore_frame_pointer(list : paasmoutput);virtual;


          private

          procedure a_op_reg_reg_const32(list: PAasmOutPut; OpLo, OpHi: TAsmOp;
                                          reg1, reg2: TRegister; a: AWord);
          procedure fixref(var ref: treference);
       end;

const
          TOpCG2AsmOpLo: Array[TOpCG] of TAsmOp = (A_ADDI,A_ANDI_,A_DIVWU,
                              A_DIVW,A_MULLW, A_MULLW, A_NONE,A_NONE,A_ORI,
                              A_SRAWI,A_SLWI,A_SRWI,A_SUBI,A_XORI);
          TOpCG2AsmOpHi: Array[TOpCG] of TAsmOp = (A_ADDIS,A_ANDIS_,
                              A_DIVWU,A_DIVW, A_MULLW,A_MULLW,A_NONE,A_NONE,
                              A_ORIS,A_NONE, A_NONE,A_NONE,A_SUBIS,A_XORIS);

         TOpCmp2AsmCond: Array[topcmp] of TAsmCond = (C_EQ,C_GT,C_LT,C_GE,
                           C_LE,C_NE,C_LE,C_NG,C_GE,C_NL);


  implementation

    uses
       globtype,globals,verbose;

    procedure tcgppc.a_call_name(list : paasmoutput;const s : string;
      offset : longint);

      begin
 { save our RTOC register value. Only necessary when doing pointer based    }
 { calls or cross TOC calls, but currently done always                      }
         list^.concat(new(paippc,op_reg_ref(A_STW,R_RTOC,
           new_reference(stack_pointer,LA_RTOC))));
         list^.concat(new(paippc,op_sym(A_BL,newasmsymbol(s))));
         list^.concat(new(paippc,op_reg_ref(A_LWZ,R_RTOC,
           new_reference(stack_pointer,LA_RTOC))));
      end;

{********************** load instructions ********************}

     procedure tcgppc.a_load_const_reg(list : paasmoutput; size: TCGSize; a : aword; reg : TRegister);

       begin
          If (a and $ffff) <> 0 Then
            Begin
              list^.concat(new(paippc,op_reg_const(A_LI,reg,a and $ffff)));
              If (a shr 16) <> 0 Then
                list^.concat(new(paippc,op_reg_const(A_ORIS,reg,a shr 16)))
            End
          Else
            list^.concat(new(paippc,op_reg_const(A_LIS,reg,a shr 16)));
       end;

       procedure tcgppc.a_load_reg_ref(list : paasmoutput; size: TCGSize; reg : tregister;const ref2 : treference);

       Var
         op: TAsmOp;
         ref: TReference;

         begin
           ref := ref2;
           FixRef(ref);
           Case size of
             OS_8 : op := A_STB;
             OS_16: op := A_STH;
             OS_32: op := A_STW;
             Else InternalError(68993)
           End;
           list^.concat(new(paippc,op_reg_ref(op,reg,newreference(ref))));
         End;

     procedure tcgppc.a_load_ref_reg(list : paasmoutput;size : tcgsize;const ref2: treference;reg : tregister);

     Var
       op: TAsmOp;
       ref: TReference;

       begin
         ref := ref2;
         FixRef(ref);
         Case size of
           OS_8 : op := A_LBZ;
           OS_16: op := A_LHZ;
           OS_32: op := A_LWZ
           Else InternalError(68994)
         End;
         list^.concat(new(paippc,op_reg_ref(op,reg,newreference(ref))));
       end;

     procedure tcgppc.a_load_reg_reg(list : paasmoutput;size : tcgsize;reg1,reg2 : tregister);

       begin
         list^.concat(new(paippc,op_reg_reg(A_MR,reg2,reg1)));
       end;

     procedure tcgppc.a_op_reg_const(list : paasmoutput; Op: TOpCG; size: TCGSize; reg: TRegister; a: AWord);

     var scratch_register: TRegister;

       begin
         Case Op of
           OP_DIV, OP_IDIV, OP_IMUL, OP_MUL:
             If (Op = OP_IMUL) And (longint(a) >= -32768) And
                (longint(a) <= 32767) Then
               list^.concat(new(paippc,op_reg_reg_const(A_MULLI,reg,reg,a)))
             Else
               Begin
                 scratch_register := get_scratch_reg(list);
                 a_load_const_reg(list, OS_32, a, scratch_register);
                 list^.concat(new(paippc,op_reg_reg_reg(TOpCG2AsmOpLo[Op],
                   reg,reg,scratch_register)));
                 free_scratch_reg(list,scratch_register);
               End;
           OP_ADD, OP_AND, OP_OR, OP_SUB,OP_XOR:
             a_op_reg_reg_const32(list,TOpCG2AsmOpLo[Op],
               TOpCG2AsmOpHi[Op],reg,reg,a);
           OP_SHL,OP_SHR,OP_SAR:
             Begin
               if (a and $ffff) <> 0 Then
                 list^.concat(new(paippc,op_reg_reg_const(
                   TOpCG2AsmOpLo[Op],reg,reg,a and $ffff)));
               If (a shr 16) <> 0 Then
                 InternalError(68991);
             End
           Else InternalError(68992);
         end;
       end;


{*************** compare instructructions ****************}

      procedure tcgppc.a_cmp_reg_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
        l : pasmlabel);

      var AsmCond: TAsmCond;
          scratch_register: TRegister;
          signed: boolean;

        begin
          signed := cmp_op in [OC_GT,OC_LT,OC_GTE,OC_LTE];
          If signed Then
            If (longint(a) >= -32768) and (longint(a) <= 32767) Then
              list^.concat(new(paippc,op_const_reg_const(A_CMPI,0,reg,a)))
            else
              begin
                scratch_register := get_scratch_reg(list);
                a_load_const_reg(list,OS_32,a,scratch_register);
                list^.concat(new(paippc,op_const_reg_reg(A_CMP,0,reg,scratch_register)));
                free_scratch_reg(list,scratch_register);
             end
           else
             if (a <= $ffff) then
              list^.concat(new(paippc,op_const_reg_const(A_CMPLI,0,reg,a)))
            else
              begin
                scratch_register := get_scratch_reg(list);
                a_load_const_reg(list,OS_32,a,scratch_register);
                list^.concat(new(paippc,op_const_reg_reg(A_CMPL,0,reg,scratch_register)));
                free_scratch_reg(list,scratch_register);
             end;
           AsmCond := TOpCmp2AsmCond[cmp_op];
           list^.concat(new(paippc,op_const_const_sym(A_BC,AsmCond2BO[AsmCond],
             AsmCond2BI[AsmCond],newasmsymbol(l^.name))));
        end;


      procedure tcgppc.a_cmp_reg_reg_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;
        reg1,reg2 : tregister;l : pasmlabel);

      var AsmCond: TAsmCond;

      begin
        list^.concat(new(paippc,op_const_reg_reg(A_CMPL,0,reg1,reg2)));
        AsmCond := TOpCmp2AsmCond[cmp_op];
        list^.concat(new(paippc,op_const_const_sym(A_BC,AsmCond2BO[AsmCond],
          AsmCond2BI[AsmCond],newasmsymbol(l^.name))));
      end;
{
      procedure tcgpp.a_cmp_reg_ref_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg : tregister;
        const ref: treference; l : pasmlabel);

      var scratch_register: TRegister;

      begin
        scratch_register := get_scratch_reg(list);
        a_load_ref_reg(list,ref,scratch_register);
        a_cmp_reg_reg_label(list,size,cmp_op,reg,scratch_register,l)
        free_scratch_reg(list,scratch_register);
     end;

      procedure tcgpp.a_cmp_ref_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;l : longint;reg : tregister;
        l : pasmlabel);

      var sr: TRegister;

        begin
          sr := get_scratch_register(list);

          a_cmp
}

     procedure tcgppc.a_loadaddress_ref_reg(list : paasmoutput;const ref2 : treference;r : tregister);

     Var
       ref: TReference;

       begin
         ref := ref2;
         FixRef(ref);
         If ref.offset <> 0 Then
           If ref.base <> R_NO then
             a_op_reg_reg_const32(list,A_ADDI,A_ADDIS,r,r,ref.offset)
  { FixRef makes sure that "(ref.index <> R_NO) and (ref.offset <> 0)" never}
  { occurs, so now only ref.offset has to be loaded                         }
           else a_load_const_reg(list, OS_32, ref.offset, r)
         else
           if ref.index <> R_NO Then
             list^.concat(new(paippc,op_reg_reg_reg(A_ADD,r,ref.base,ref.index)))
           else list^.concat(new(paippc,op_reg_reg(A_MR,r,ref.base)))
       end;

{ *********** entry/exit code and address loading ************ }

    procedure tcgppc.g_stackframe_entry(list : paasmoutput;localsize : longint);
 { generated the entry code of a procedure/function. Note: localsize is the }
 { sum of the size necessary for local variables and the maximum possible   }
 { combined size of ALL the parameters of a procedure called by the current }
 { one                                                                      }
     var scratch_register: TRegister;

      begin
        if (localsize mod 8) <> 0 then internalerror(58991);
 { CR and LR only have to be saved in case they are modified by the current }
 { procedure, but currently this isn't checked, so save them always         }
        scratch_register := get_scratch_reg(list);
        list^.concat(new(paippc,op_reg(A_MFCR,scratch_register)));
        list^.concat(new(paippc,op_reg_ref(A_STW,scratch_register,
          new_reference(stack_pointer,LA_CR))));
        free_scratch_reg(list,scratch_register);
        scratch_register := get_scratch_reg(list);
        list^.concat(new(paippc,op_reg_reg(A_MFSPR,scratch_register,
          R_LR)));
        list^.concat(new(paippc,op_reg_ref(A_STW,scratch_register,
          new_reference(stack_pointer,LA_LR))));
        free_scratch_reg(list,scratch_register);
{ if the current procedure is a leaf procedure, we can use the Red Zone,    }
{ but this is not yet implemented                                           }
{        if (procinfo.flags and pi_do_call) <> 0 Then}
           Begin
             if localsize<>0 then
               begin
 { allocate space for the local variable, parameter and linkage area and   }
 { save the stack pointer at the end of the linkage area                   }
                 if localsize <= $ffff Then
                   list^.concat(new(paippc,op_reg_ref
                     (A_STWU,stack_pointer, new_reference(stack_pointer,localsize+
                      LinkageAreaSize))))
                 else
                   Begin
                     scratch_register := get_scratch_reg(list);
                     a_load_const_reg(list,OS_32,localsize,scratch_register);
                     list^.concat(new(paippc,op_reg_reg_reg(A_STWUX,stack_pointer,
                       stack_pointer,scratch_register)));
                     free_scratch_reg(list,scratch_register);
                  End;
               End
           End;
       end;

    procedure tcgppc.g_restore_frame_pointer(list : paasmoutput);

      begin
 { no frame pointer on the PowerPC                                          }
      end;


{***************** This is private property, keep out! :) *****************}

    procedure tcgppc.fixref(var ref: treference);

 { Make sure ref is a valid reference for the PowerPC and sets the base to  }
 { the value of the index if (base = R_NO). (Index <> R_NO) is not checked  }
 { because the less conditional jumps, the better                           }

       begin
         If (ref.base <> R_NO) and (ref.index <> R_NO) and
            (ref.offset <> 0) Then Internalerror(58992);
         if (ref.base = R_NO) Then
           begin
             ref.base := ref.index;
             ref.index := R_NO
           end
       end;

    procedure tcgppc.a_op_reg_reg_const32(list: PAasmOutput; OpLo, OpHi:
                       TAsmOp; reg1, reg2: TRegister; a: AWord);
 { Generates                                                                }
 {   OpLo reg1, reg2, (a and $ffff) and/or                                  }
 {   OpHi reg1, reg2, (a shr 16)                                            }
 { depending on the value of a                                              }

     Begin
       if (a and $ffff) <> 0 Then
         list^.concat(new(paippc,op_reg_reg_const(OpLo,reg1,reg2,a and $ffff)));
       If (a shr 16) <> 0 Then
         list^.concat(new(paippc,op_reg_reg_const(OpHi,reg1,reg2,a shr 16)))
     End;

end.
{
  $Log$
  Revision 1.2  1999-08-18 17:05:57  florian
    + implemented initilizing of data for the new code generator
      so it should compile now simple programs

  Revision 1.1  1999/08/06 16:41:11  jonas
    * PowerPC compiles again, several routines implemented in cgcpu.pas
    * added constant to cpubase of alpha and powerpc for maximum
      number of operands


}
