{
    $Id$
    Copyright (c) 1998-2002 by the FPC team

    This unit implements the code generator for the 680x0

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
       cginfo,cgbase,cgobj,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,cpupara,
       node,symconst,cg64f32;

    type
      tcg68k = class(tcg)
          procedure a_call_name(list : taasmoutput;const s : string);override;
          procedure a_call_ref(list : taasmoutput;const ref : treference);override;
          procedure a_call_reg(list : taasmoutput;reg : tregister);override;
          procedure a_load_const_reg(list : taasmoutput;size : tcgsize;a : aword;register : tregister);override;
          procedure a_load_reg_ref(list : taasmoutput;size : tcgsize;register : tregister;const ref : treference);override;
          procedure a_load_reg_reg(list : taasmoutput;fromsize,tosize : tcgsize;reg1,reg2 : tregister);override;
          procedure a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref : treference;register : tregister);override;
          procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);override;
          procedure a_loadfpu_reg_reg(list: taasmoutput; reg1, reg2: tregister); override;
          procedure a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister); override;
          procedure a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference); override;
          procedure a_loadmm_reg_reg(list: taasmoutput; reg1, reg2: tregister); override;
          procedure a_loadmm_ref_reg(list: taasmoutput; const ref: treference; reg: tregister); override;
          procedure a_loadmm_reg_ref(list: taasmoutput; reg: tregister; const ref: treference); override;
          procedure a_parammm_reg(list: taasmoutput; reg: tregister); override;
          procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister); override;
          procedure a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; reg1, reg2: TRegister); override;
          procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
            l : tasmlabel);override;
          procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;
          procedure a_jmp_always(list : taasmoutput;l: tasmlabel); override;
          procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); override;
          procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: tresflags; reg: TRegister); override;

          procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword;delsource,loadref : boolean);override;
          { generates overflow checking code for a node }
          procedure g_overflowcheck(list: taasmoutput; const p: tnode); override;
          procedure g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer); override;
          procedure g_stackframe_entry(list : taasmoutput;localsize : longint);override;
          procedure g_restore_frame_pointer(list : taasmoutput);override;
          procedure g_return_from_proc(list : taasmoutput;parasize : aword);override;
          procedure g_save_standard_registers(list:Taasmoutput;usedinproc:Tsupregset);override;
          procedure g_restore_standard_registers(list:Taasmoutput;usedinproc:Tsupregset);override;
          procedure g_save_all_registers(list : taasmoutput);override;
          procedure g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);override;
          { for address register allocation }
          function get_scratch_reg_address(list : taasmoutput) : tregister;override;
          function get_scratch_reg_int(list:Taasmoutput;size:Tcgsize):Tregister; override;


     protected
         function fixref(list: taasmoutput; var ref: treference): boolean;
     private
          { # Sign or zero extend the register to a full 32-bit value.
              The new value is left in the same register.
          }
          procedure sign_extend(list: taasmoutput;_oldsize : tcgsize; reg: tregister);
          procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);

     end;

     tcg64f68k = class(tcg64f32)
       procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);override;
       procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);override;
     end;

     { This function returns true if the reference+offset is valid.
       Otherwise extra code must be generated to solve the reference.

       On the m68k, this verifies that the reference is valid
       (e.g : if index register is used, then the max displacement
        is 256 bytes, if only base is used, then max displacement
        is 32K
     }
     function isvalidrefoffset(const ref: treference): boolean;

const
      TCGSize2OpSize: Array[tcgsize] of topsize =
        (S_NO,S_B,S_W,S_L,S_L,S_B,S_W,S_L,S_L,
         S_FS,S_FD,S_FX,S_NO,S_NO,
         S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO);




Implementation

    uses
       globtype,globals,verbose,systems,cutils,
       symdef,symsym,defutil,paramgr,
       rgobj,tgobj,rgcpu;


    const
      { opcode table lookup }
      topcg2tasmop: Array[topcg] of tasmop =
      (
       A_NONE,
       A_ADD,
       A_AND,
       A_DIVU,
       A_DIVS,
       A_MULS,
       A_MULU,
       A_NEG,
       A_NOT,
       A_OR,
       A_ASR,
       A_LSL,
       A_LSR,
       A_SUB,
       A_EOR
      );


      TOpCmp2AsmCond: Array[topcmp] of TAsmCond =
      (
       C_NONE,
       C_EQ,
       C_GT,
       C_LT,
       C_GE,
       C_LE,
       C_NE,
       C_LS,
       C_CS,
       C_CC,
       C_HI
      );


     function isvalidrefoffset(const ref: treference): boolean;
      begin
         isvalidrefoffset := true;
         if ref.index.enum <> R_NO then
           begin
             if ref.base.enum <> R_NO then
                internalerror(20020814);
             if (ref.offset < low(shortint)) or (ref.offset > high(shortint)) then
                isvalidrefoffset := false
           end
         else
           begin
             if (ref.offset < low(smallint)) or (ref.offset > high(smallint)) then
                isvalidrefoffset := false;
           end;
      end;

    function tcg68k.get_scratch_reg_int(list:Taasmoutput;size:Tcgsize):Tregister;

    var r:Tregister;
        rs:Tsuperregister;
        i:longint;

      begin
         if unusedscratchregisters=[] then
           internalerror(68996);

         if RS_D0 in unusedscratchregisters then
            rs:=RS_D0
         else if RS_D1 in unusedscratchregisters then
            rs:=RS_D1
         else
           internalerror(10);
         r.enum:=R_INTREGISTER;
         r.number:=rs shl 8 or cgsize2subreg(size);

         exclude(unusedscratchregisters,rs);
         a_reg_alloc(list,r);
         get_scratch_reg_int:=r;
      end;


    function tcg68k.get_scratch_reg_address(list:Taasmoutput):Tregister;

    var r:Tregister;
        rs:Tsuperregister;
        i:longint;

      begin
         if unusedscratchregisters=[] then
           internalerror(68996);
        
         if RS_A0 in unusedscratchregisters then
           rs:=RS_A0
         else if RS_A1 in unusedscratchregisters then
           rs:=RS_A1
         else
           internalerror(10);
         r.enum:=R_INTREGISTER;
         r.number:=rs shl 8 or R_SUBWHOLE;

         exclude(unusedscratchregisters,rs);
         a_reg_alloc(list,r);
         get_scratch_reg_address:=r;
      end;


{****************************************************************************}
{                               TCG68K                                       }
{****************************************************************************}
    function tcg68k.fixref(list: taasmoutput; var ref: treference): boolean;

       var
         tmpreg: tregister;
       begin
         result := false;
         { The Coldfire and MC68020+ have extended
           addressing capabilities with a 32-bit
           displacement.
         }
         if (aktoptprocessor <> MC68000) then
           exit;
         if (ref.base.enum <> R_NO) then
           begin
             if (ref.index.enum <> R_NO) and assigned(ref.symbol) then
                internalerror(20020814);
             { base + reg }
             if ref.index.enum <> R_NO then
                begin
                   { base + reg + offset }
                   if (ref.offset < low(shortint)) or (ref.offset > high(shortint)) then
                     begin
                        list.concat(taicpu.op_const_reg(A_ADD,S_L,ref.offset,ref.base));
                        fixref := true;
                        ref.offset := 0;
                        exit;
                     end;
                end
             else
             { base + offset }
             if (ref.offset < low(smallint)) or (ref.offset > high(smallint)) then
               begin
                 list.concat(taicpu.op_const_reg(A_ADD,S_L,ref.offset,ref.base));
                 fixref := true;
                 ref.offset := 0;
                 exit;
               end;
           end;
       end;



    procedure tcg68k.a_call_name(list : taasmoutput;const s : string);

      begin
        list.concat(taicpu.op_sym(A_JSR,S_NO,objectlibrary.newasmsymbol(s)));
      end;


    procedure tcg68k.a_call_ref(list : taasmoutput;const ref : treference);
      var
       href : treference;
      begin
        href := ref;
        fixref(list,href);
        list.concat(taicpu.op_ref(A_JSR,S_NO,href));
      end;

    procedure tcg68k.a_call_reg(list : taasmoutput;reg : tregister);
     var
       href : treference;
     begin
       reference_reset_base(href, reg, 0);
       a_call_ref(list,href);
     end;



    procedure tcg68k.a_load_const_reg(list : taasmoutput;size : tcgsize;a : aword;register : tregister);
      begin
        if (rg.isaddressregister(register)) then
         begin
           list.concat(taicpu.op_const_reg(A_MOVE,S_L,longint(a),register))
         end
        else
        if a = 0 then
           list.concat(taicpu.op_reg(A_CLR,S_L,register))
        else
         begin
           if (longint(a) >= low(shortint)) and (longint(a) <= high(shortint)) then
              list.concat(taicpu.op_const_reg(A_MOVEQ,S_L,longint(a),register))
           else
              list.concat(taicpu.op_const_reg(A_MOVE,S_L,longint(a),register))
         end;
      end;

    procedure tcg68k.a_load_reg_ref(list : taasmoutput;size : tcgsize;register : tregister;const ref : treference);
      var
       href : treference;
      begin
         href := ref;
         fixref(list,href);
         { move to destination reference }
         list.concat(taicpu.op_reg_ref(A_MOVE,TCGSize2OpSize[size],register,href));
      end;

    procedure tcg68k.a_load_reg_reg(list : taasmoutput;fromsize,tosize : tcgsize;reg1,reg2 : tregister);
      begin
         { move to destination register }
         list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,reg2));
         { zero/sign extend register to 32-bit }
         sign_extend(list, fromsize, reg2);
      end;

    procedure tcg68k.a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref : treference;register : tregister);
      var
       href : treference;
      begin
         href := ref;
         fixref(list,href);
         list.concat(taicpu.op_ref_reg(A_MOVE,TCGSize2OpSize[size],href,register));
         { extend the value in the register }
         sign_extend(list, size, register);
      end;


    procedure tcg68k.a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);
     var
       href : treference;
      begin
        if (not rg.isaddressregister(r)) then
          begin
            internalerror(2002072901);
          end;
        href:=ref;
        fixref(list, href);
        list.concat(taicpu.op_ref_reg(A_LEA,S_L,href,r));
      end;

    procedure tcg68k.a_loadfpu_reg_reg(list: taasmoutput; reg1, reg2: tregister);
      begin
        { in emulation mode, only 32-bit single is supported }
        if cs_fp_emulation in aktmoduleswitches then
          list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,reg2))
        else
          list.concat(taicpu.op_reg_reg(A_FMOVE,S_FD,reg1,reg2));
      end;


    procedure tcg68k.a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister);
     var
      opsize : topsize;
      href : treference;
      begin
        opsize := tcgsize2opsize[size];
        { extended is not supported, since it is not available on Coldfire }
        if opsize = S_FX then
          internalerror(20020729);
        href := ref;
        fixref(list,href);
        { in emulation mode, only 32-bit single is supported }
        if cs_fp_emulation in aktmoduleswitches then
           list.concat(taicpu.op_ref_reg(A_MOVE,S_L,href,reg))
        else
           list.concat(taicpu.op_ref_reg(A_FMOVE,opsize,href,reg));
      end;

    procedure tcg68k.a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference);
      var
       opsize : topsize;
      begin
        opsize := tcgsize2opsize[size];
        { extended is not supported, since it is not available on Coldfire }
        if opsize = S_FX then
          internalerror(20020729);
        { in emulation mode, only 32-bit single is supported }
        if cs_fp_emulation in aktmoduleswitches then
          list.concat(taicpu.op_reg_ref(A_MOVE,S_L,reg, ref))
        else
          list.concat(taicpu.op_reg_ref(A_FMOVE,opsize,reg, ref));
      end;

    procedure tcg68k.a_loadmm_reg_reg(list: taasmoutput; reg1, reg2: tregister);
      begin
        internalerror(20020729);
      end;

    procedure tcg68k.a_loadmm_ref_reg(list: taasmoutput; const ref: treference; reg: tregister);
      begin
        internalerror(20020729);
      end;

    procedure tcg68k.a_loadmm_reg_ref(list: taasmoutput; reg: tregister; const ref: treference);
      begin
        internalerror(20020729);
      end;

    procedure tcg68k.a_parammm_reg(list: taasmoutput; reg: tregister);
      begin
        internalerror(20020729);
      end;


    procedure tcg68k.a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister);
      var
       scratch_reg : tregister;
       scratch_reg2: tregister;
       opcode : tasmop;
       r,r2 : Tregister;
      begin
        { need to emit opcode? }
        if optimize_op_const_reg(list, op, a, reg) then
           exit;
        opcode := topcg2tasmop[op];
        case op of
          OP_ADD :
              Begin
                if (a >= 1) and (a <= 8) then
                    list.concat(taicpu.op_const_reg(A_ADDQ,S_L,a, reg))
                else
                  begin
                    { all others, including coldfire }
                    list.concat(taicpu.op_const_reg(A_ADD,S_L,a, reg));
                  end;
              end;
          OP_AND,
          OP_OR:
              Begin
                 list.concat(taicpu.op_const_reg(topcg2tasmop[op],S_L,longint(a), reg));
              end;
          OP_DIV :
              Begin
                 internalerror(20020816);
              end;
          OP_IDIV :
              Begin
                 internalerror(20020816);
              end;
          OP_IMUL :
              Begin
             if aktoptprocessor = MC68000 then
                   begin
                     r.enum:=R_INTREGISTER;
                     r.number:=NR_D0;
                     r2.enum:=R_INTREGISTER;
                     r2.number:=NR_D1;
                     rg.getexplicitregisterint(list,NR_D0);
                     rg.getexplicitregisterint(list,NR_D1);
                     list.concat(taicpu.op_const_reg(A_MOVE,S_L,a, r));
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, r2));
                     cg.a_call_name(list,'FPC_MUL_LONGINT');
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r, reg));
                     rg.ungetregisterint(list,r);
                     rg.ungetregisterint(list,r2);
                   end
                  else
                    begin
                      if (rg.isaddressregister(reg)) then
                       begin
                         scratch_reg := cg.get_scratch_reg_int(list,OS_INT);
                         list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, scratch_reg));
                         list.concat(taicpu.op_const_reg(A_MULS,S_L,a,scratch_reg));
                         list.concat(taicpu.op_reg_reg(A_MOVE,S_L,scratch_reg,reg));
                         cg.free_scratch_reg(list,scratch_reg);
                       end
                      else
                         list.concat(taicpu.op_const_reg(A_MULS,S_L,a,reg));
                    end;
              end;
          OP_MUL :
              Begin
                 if aktoptprocessor = MC68000 then
                   begin
                     r.enum:=R_INTREGISTER;
                     r.number:=NR_D0;
                     r2.enum:=R_INTREGISTER;
                     r2.number:=NR_D1;
                     rg.getexplicitregisterint(list,NR_D0);
                     rg.getexplicitregisterint(list,NR_D1);
                     list.concat(taicpu.op_const_reg(A_MOVE,S_L,a, r));
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, r2));
                     cg.a_call_name(list,'FPC_MUL_LONGWORD');
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r, reg));
                     rg.ungetregisterint(list,r);
                     rg.ungetregisterint(list,r2);
                   end
                  else
                    begin
                      if (rg.isaddressregister(reg)) then
                       begin
                         scratch_reg := cg.get_scratch_reg_int(list,OS_INT);
                         list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, scratch_reg));
                         list.concat(taicpu.op_const_reg(A_MULU,S_L,a,scratch_reg));
                         list.concat(taicpu.op_reg_reg(A_MOVE,S_L,scratch_reg,reg));
                         cg.free_scratch_reg(list,scratch_reg);
                       end
                      else
                         list.concat(taicpu.op_const_reg(A_MULU,S_L,a,reg));
                    end;
              end;
          OP_SAR,
          OP_SHL,
          OP_SHR :
              Begin
                if (a >= 1) and (a <= 8) then
                 begin
                   { now allowed to shift an address register }
                   if (rg.isaddressregister(reg)) then
                     begin
                       scratch_reg := cg.get_scratch_reg_int(list,OS_INT);
                       list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, scratch_reg));
                       list.concat(taicpu.op_const_reg(opcode,S_L,a, scratch_reg));
                       list.concat(taicpu.op_reg_reg(A_MOVE,S_L,scratch_reg,reg));
                       cg.free_scratch_reg(list,scratch_reg);
                     end
                   else
                     list.concat(taicpu.op_const_reg(opcode,S_L,a, reg));
                 end
                else
                 begin
                   { we must load the data into a register ... :() }
                   scratch_reg := cg.get_scratch_reg_int(list,OS_INT);
                   list.concat(taicpu.op_const_reg(A_MOVE,S_L,a, scratch_reg));
                   { again... since shifting with address register is not allowed }
                   if (rg.isaddressregister(reg)) then
                     begin
                       scratch_reg2 := cg.get_scratch_reg_int(list,OS_INT);
                       list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, scratch_reg2));
                       list.concat(taicpu.op_reg_reg(opcode,S_L,scratch_reg, scratch_reg2));
                       list.concat(taicpu.op_reg_reg(A_MOVE,S_L,scratch_reg2,reg));
                       cg.free_scratch_reg(list,scratch_reg2);
                     end
                   else
                     list.concat(taicpu.op_reg_reg(opcode,S_L,scratch_reg, reg));
                   cg.free_scratch_reg(list,scratch_reg);
                 end;
              end;
          OP_SUB :
              Begin
                if (a >= 1) and (a <= 8) then
                    list.concat(taicpu.op_const_reg(A_SUBQ,S_L,a,reg))
                else
                  begin
                    { all others, including coldfire }
                    list.concat(taicpu.op_const_reg(A_SUB,S_L,a, reg));
                  end;
              end;
          OP_XOR :
              Begin
                 list.concat(taicpu.op_const_reg(A_EORI,S_L,a, reg));
              end;
        else
            internalerror(20020729);
         end;
      end;

    procedure tcg68k.a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; reg1, reg2: TRegister);
      var
       hreg1,hreg2,r,r2: tregister;
      begin
        case op of
          OP_ADD :
              Begin
                 if aktoptprocessor = ColdFire then
                  begin
                    { operation only allowed only a longword }
                    sign_extend(list, size, reg1);
                    sign_extend(list, size, reg2);
                    list.concat(taicpu.op_reg_reg(A_ADD,S_L,reg1, reg2));
                  end
                 else
                  begin
                    list.concat(taicpu.op_reg_reg(A_ADD,TCGSize2OpSize[size],reg1, reg2));
                  end;
              end;
          OP_AND,OP_OR,
          OP_SAR,OP_SHL,
          OP_SHR,OP_SUB,OP_XOR :
              Begin
                 { load to data registers }
                 if (rg.isaddressregister(reg1)) then
                   begin
                     hreg1 := cg.get_scratch_reg_int(list,OS_INT);
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,hreg1));
                   end
                 else
                   hreg1 := reg1;

                 if (rg.isaddressregister(reg2))  then
                   begin
                      hreg2:= cg.get_scratch_reg_int(list,OS_INT);
                      list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2,hreg2));
                   end
                 else
                   hreg2 := reg2;

                 if aktoptprocessor = ColdFire then
                  begin
                    { operation only allowed only a longword }
                    {!***************************************
                      in the case of shifts, the value to
                      shift by, should already be valid, so
                      no need to sign extend the value
                     !
                    }
                    if op in [OP_AND,OP_OR,OP_SUB,OP_XOR] then
                       sign_extend(list, size, hreg1);
                    sign_extend(list, size, hreg2);
                    list.concat(taicpu.op_reg_reg(topcg2tasmop[op],S_L,hreg1, hreg2));
                  end
                 else
                  begin
                    list.concat(taicpu.op_reg_reg(topcg2tasmop[op],TCGSize2OpSize[size],hreg1, hreg2));
                  end;

                 if reg1.enum <> hreg1.enum then
                    cg.free_scratch_reg(list,hreg1);
                 { move back result into destination register }
                 if reg2.enum <> hreg2.enum then
                   begin
                      list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg2,reg2));
                      cg.free_scratch_reg(list,hreg2);
                   end;
              end;
          OP_DIV :
              Begin
                 internalerror(20020816);
              end;
          OP_IDIV :
              Begin
                 internalerror(20020816);
              end;
          OP_IMUL :
              Begin
                 sign_extend(list, size,reg1);
                 sign_extend(list, size,reg2);
                 if aktoptprocessor = MC68000 then
                   begin
                     r.enum:=R_INTREGISTER;
                     r.number:=NR_D0;
                     r2.enum:=R_INTREGISTER;
                     r2.number:=NR_D1;
                     rg.getexplicitregisterint(list,NR_D0);
                     rg.getexplicitregisterint(list,NR_D1);
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1, r));
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2, r2));
                     cg.a_call_name(list,'FPC_MUL_LONGINT');
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r, reg2));
                     rg.ungetregisterint(list,r);
                     rg.ungetregisterint(list,r2);
                   end
                  else
                    begin
                     if (rg.isaddressregister(reg1)) then
                       hreg1 := cg.get_scratch_reg_int(list,OS_INT)
                     else
                       hreg1 := reg1;
                     if (rg.isaddressregister(reg2))  then
                       hreg2:= cg.get_scratch_reg_int(list,OS_INT)
                     else
                       hreg2 := reg2;

                     if reg1.number <> hreg1.number then
                          list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,hreg1));
                     if reg2.number <> hreg2.number then
                          list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2,hreg2));

                     list.concat(taicpu.op_reg_reg(A_MULS,S_L,reg1,reg2));

                     if reg1.enum <> hreg1.enum then
                       cg.free_scratch_reg(list,hreg1);
                     { move back result into destination register }
                     if reg2.enum <> hreg2.enum then
                       begin
                          list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg2,reg2));
                          cg.free_scratch_reg(list,hreg2);
                       end;
                    end;
              end;
          OP_MUL :
              Begin
                 sign_extend(list, size,reg1);
                 sign_extend(list, size,reg2);
                 if aktoptprocessor = MC68000 then
                   begin
                     r.enum:=R_INTREGISTER;
                     r.number:=NR_D0;
                     r2.enum:=R_INTREGISTER;
                     r2.number:=NR_D1;
                     rg.getexplicitregisterint(list,NR_D0);
                     rg.getexplicitregisterint(list,NR_D1);
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1, r));
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2, r2));
                     cg.a_call_name(list,'FPC_MUL_LONGWORD');
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r, reg2));
                     rg.ungetregisterint(list,r);
                     rg.ungetregisterint(list,r2);
                   end
                  else
                    begin
                     if (rg.isaddressregister(reg1)) then
                      begin
                       hreg1 := cg.get_scratch_reg_int(list,OS_INT);
                       list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,hreg1));
                      end
                     else
                       hreg1 := reg1;

                     if (rg.isaddressregister(reg2))  then
                      begin
                       hreg2:= cg.get_scratch_reg_int(list,OS_INT);
                       list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2,hreg2));
                      end
                     else
                       hreg2 := reg2;


                     list.concat(taicpu.op_reg_reg(A_MULU,S_L,reg1,reg2));

                     if reg1.number <> hreg1.number then
                       cg.free_scratch_reg(list,hreg1);
                     { move back result into destination register }
                     if reg2.number <> hreg2.number then
                       begin
                          list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg2,reg2));
                          cg.free_scratch_reg(list,hreg2);
                       end;
                    end;
              end;
          OP_NEG,
          OP_NOT :
              Begin
                { if there are two operands, move the register,
                  since the operation will only be done on the result
                  register.
                }
                if reg1.enum <> R_NO then
                  cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,reg1,reg2);

                if (rg.isaddressregister(reg2)) then
                  begin
                     hreg2 := cg.get_scratch_reg_int(list,OS_INT);
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2,hreg2));
                   end
                  else
                    hreg2 := reg2;

                { coldfire only supports long version }
                if aktoptprocessor = ColdFire then
                  begin
                    sign_extend(list, size,hreg2);
                    list.concat(taicpu.op_reg(topcg2tasmop[op],S_L,hreg2));
                  end
                else
                  begin
                    list.concat(taicpu.op_reg(topcg2tasmop[op],TCGSize2OpSize[size],hreg2));
                  end;

                if reg2.enum <> hreg2.enum then
                  begin
                    list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg2,reg2));
                    cg.free_scratch_reg(list,hreg2);
                  end;

              end;
        else
            internalerror(20020729);
         end;
      end;



    procedure tcg68k.a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
            l : tasmlabel);
      var
       hregister : tregister;
      begin
       if a = 0 then
         begin
           list.concat(taicpu.op_reg(A_TST,TCGSize2OpSize[size],reg));
         end
       else
         begin
           if (aktoptprocessor = ColdFire) then
             begin
               {
                 only longword comparison is supported,
                 and only on data registers.
               }
               hregister := cg.get_scratch_reg_int(list,OS_INT);
               { always move to a data register }
               list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg,hregister));
               { sign/zero extend the register }
               sign_extend(list, size,hregister);
               list.concat(taicpu.op_const_reg(A_CMPI,S_L,a,hregister));
               cg.free_scratch_reg(list,hregister);
             end
           else
             begin
               list.concat(taicpu.op_const_reg(A_CMPI,TCGSize2OpSize[size],a,reg));
             end;
         end;
         { emit the actual jump to the label }
         a_jmp_cond(list,cmp_op,l);
      end;

    procedure tcg68k.a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
      begin
         list.concat(taicpu.op_reg_reg(A_CMP,tcgsize2opsize[size],reg1,reg2));
         { emit the actual jump to the label }
         a_jmp_cond(list,cmp_op,l);
      end;

    procedure tcg68k.a_jmp_always(list : taasmoutput;l: tasmlabel);
      var
       ai: taicpu;
      begin
         ai := Taicpu.op_sym(A_JMP,S_NO,l);
         ai.is_jmp := true;
         list.concat(ai);
      end;

    procedure tcg68k.a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel);
       var
         ai : taicpu;
       begin
         ai := Taicpu.op_sym(A_BXX,S_NO,l);
         ai.SetCondition(flags_to_cond(f));
         ai.is_jmp := true;
         list.concat(ai);
       end;

    procedure tcg68k.g_flags2reg(list: taasmoutput; size: TCgSize; const f: tresflags; reg: TRegister);
       var
         ai : taicpu;
         hreg : tregister;
       begin
          { move to a Dx register? }
          if (rg.isaddressregister(reg)) then
            begin
              hreg := get_scratch_reg_int(list,OS_INT);
              a_load_const_reg(list,size,0,hreg);
              ai:=Taicpu.Op_reg(A_Sxx,S_B,hreg);
              ai.SetCondition(flags_to_cond(f));
              list.concat(ai);

              if (aktoptprocessor = ColdFire) then
                begin
                 { neg.b does not exist on the Coldfire
                   so we need to sign extend the value
                   before doing a neg.l
                 }
                 list.concat(taicpu.op_reg(A_EXTB,S_L,hreg));
                 list.concat(taicpu.op_reg(A_NEG,S_L,hreg));
                end
              else
                begin
                  list.concat(taicpu.op_reg(A_NEG,S_B,hreg));
                end;
             list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg,reg));
             free_scratch_reg(list,hreg);
            end
          else
          begin
            a_load_const_reg(list,size,0,reg);
            ai:=Taicpu.Op_reg(A_Sxx,S_B,reg);
            ai.SetCondition(flags_to_cond(f));
            list.concat(ai);

            if (aktoptprocessor = ColdFire) then
              begin
                 { neg.b does not exist on the Coldfire
                   so we need to sign extend the value
                   before doing a neg.l
                 }
                 list.concat(taicpu.op_reg(A_EXTB,S_L,reg));
                 list.concat(taicpu.op_reg(A_NEG,S_L,reg));
              end
            else
              begin
               list.concat(taicpu.op_reg(A_NEG,S_B,reg));
              end;
          end;
       end;



    procedure tcg68k.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword;delsource,loadref : boolean);
     var
         helpsize : longint;
         i : byte;
         reg8,reg32 : tregister;
         swap : boolean;
         hregister : tregister;
         iregister : tregister;
         jregister : tregister;
         hp1 : treference;
         hp2 : treference;
         hl : tasmlabel;
         hl2: tasmlabel;
         popaddress : boolean;
         srcref,dstref : treference;

      begin
         popaddress := false;

         { this should never occur }
         if len > 65535 then
           internalerror(0);
         hregister := get_scratch_reg_int(list,OS_INT);
         if delsource then
            reference_release(list,source);


         { from 12 bytes movs is being used }
         if (not loadref) and ((len<=8) or (not(cs_littlesize in aktglobalswitches) and (len<=12))) then
           begin
              srcref := source;
              dstref := dest;
              helpsize:=len div 4;
              { move a dword x times }
              for i:=1 to helpsize do
                begin
                   a_load_ref_reg(list,OS_INT,srcref,hregister);
                   a_load_reg_ref(list,OS_INT,hregister,dstref);
                   inc(srcref.offset,4);
                   inc(dstref.offset,4);
                   dec(len,4);
                end;
              { move a word }
              if len>1 then
                begin
                   a_load_ref_reg(list,OS_16,srcref,hregister);
                   a_load_reg_ref(list,OS_16,hregister,dstref);
                   inc(srcref.offset,2);
                   inc(dstref.offset,2);
                   dec(len,2);
                end;
              { move a single byte }
              if len>0 then
                begin
                   a_load_ref_reg(list,OS_8,srcref,hregister);
                   a_load_reg_ref(list,OS_8,hregister,dstref);
                end

           end
         else
           begin
              iregister := get_scratch_reg_address(list);
              jregister := get_scratch_reg_address(list);
              { reference for move (An)+,(An)+ }
              reference_reset(hp1);
              hp1.base := iregister;   { source register }
              hp1.direction := dir_inc;
              reference_reset(hp2);
              hp2.base := jregister;
              hp2.direction := dir_inc;
              { iregister = source }
              { jregister = destination }

              if loadref then
                 a_load_ref_reg(list,OS_INT,source,iregister)
              else
                 a_loadaddr_ref_reg(list,source,iregister);

              a_loadaddr_ref_reg(list,dest,jregister);

              { double word move only on 68020+ machines }
              { because of possible alignment problems   }
              { use fast loop mode }
              if (aktoptprocessor=MC68020) then
                begin
                   helpsize := len - len mod 4;
                   len := len mod 4;
                   list.concat(taicpu.op_const_reg(A_MOVE,S_L,helpsize div 4,hregister));
                   objectlibrary.getlabel(hl2);
                   a_jmp_always(list,hl2);
                   objectlibrary.getlabel(hl);
                   a_label(list,hl);
                   list.concat(taicpu.op_ref_ref(A_MOVE,S_L,hp1,hp2));
                   cg.a_label(list,hl2);
                   list.concat(taicpu.op_reg_sym(A_DBRA,S_L,hregister,hl));
                   if len > 1 then
                     begin
                        dec(len,2);
                        list.concat(taicpu.op_ref_ref(A_MOVE,S_W,hp1,hp2));
                     end;
                   if len = 1 then
                     list.concat(taicpu.op_ref_ref(A_MOVE,S_B,hp1,hp2));
                end
              else
                begin
                   { Fast 68010 loop mode with no possible alignment problems }
                   helpsize := len;
                   list.concat(taicpu.op_const_reg(A_MOVE,S_L,helpsize,hregister));
                   objectlibrary.getlabel(hl2);
                   a_jmp_always(list,hl2);
                   objectlibrary.getlabel(hl);
                   a_label(list,hl);
                   list.concat(taicpu.op_ref_ref(A_MOVE,S_B,hp1,hp2));
                   a_label(list,hl2);
                   list.concat(taicpu.op_reg_sym(A_DBRA,S_L,hregister,hl));
                end;

              { restore the registers that we have just used olny if they are used! }
              free_scratch_reg(list, iregister);
              free_scratch_reg(list, jregister);
              if jregister.enum = R_A1 then
                hp2.base.enum := R_NO;
              if iregister.enum = R_A0 then
                hp1.base.enum := R_NO;
              reference_release(list,hp1);
              reference_release(list,hp2);
           end;

           { loading SELF-reference again }
           g_maybe_loadself(list);

           if delsource then
               tg.ungetiftemp(list,source);

           free_scratch_reg(list,hregister);
    end;

    procedure tcg68k.g_overflowcheck(list: taasmoutput; const p: tnode);
      begin
      end;

    procedure tcg68k.g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer);
      begin
      end;

    procedure tcg68k.g_stackframe_entry(list : taasmoutput;localsize : longint);
    
    var r,r2,rsp:Tregister;
    
      begin
        r.enum:=frame_pointer_reg;
        rsp.enum:=stack_pointer_reg;
        if localsize<>0 then
           begin
             { Not to complicate the code generator too much, and since some  }
             { of the systems only support this format, the localsize cannot }
             { exceed 32K in size.                                            }
             if (localsize < low(smallint)) or (localsize > high(smallint)) then
                CGMessage(cg_e_localsize_too_big);
             list.concat(taicpu.op_reg_const(A_LINK,S_W,r,-localsize));
           end { endif localsize <> 0 }
          else
           begin
             r2.enum:=R_SPPUSH;
             list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r,r2));
             list.concat(taicpu.op_reg_reg(A_MOVE,S_L,rsp,r));
           end;
      end;

    procedure tcg68k.g_restore_frame_pointer(list : taasmoutput);
    
    var r:Tregister;
    
      begin
        r.enum:=frame_pointer_reg;
        list.concat(taicpu.op_reg(A_UNLK,S_NO,r));
      end;

    procedure tcg68k.g_return_from_proc(list : taasmoutput;parasize : aword);
      var
       r,hregister : tregister;
      begin
       {Routines with the poclearstack flag set use only a ret.}
       { also routines with parasize=0     }
         if (po_clearstack in aktprocdef.procoptions) then
           begin
             { complex return values are removed from stack in C code PM }
             if paramanager.ret_in_param(aktprocdef.rettype.def,aktprocdef.proccalloption) then
               list.concat(taicpu.op_const(A_RTD,S_NO,4))
             else
               list.concat(taicpu.op_none(A_RTS,S_NO));
           end
         else if (parasize=0) then
           begin
             list.concat(taicpu.op_none(A_RTS,S_NO));
           end
         else
           begin
            { return with immediate size possible here }
            { signed!                                  }
            { RTD is not supported on the coldfire     }
            if (aktoptprocessor = MC68020) and (parasize < $7FFF) then
                list.concat(taicpu.op_const(A_RTD,S_NO,parasize))
            { manually restore the stack }
            else
              begin
                { We must pull the PC Counter from the stack, before  }
                { restoring the stack pointer, otherwise the PC would }
                { point to nowhere!                                   }

                { save the PC counter (pop it from the stack)         }
                hregister := get_scratch_reg_address(list);
                r.enum:=R_SPPULL;
                list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r,hregister));
                { can we do a quick addition ... }
                r.enum:=R_SP;
                if (parasize > 0) and (parasize < 9) then
                   list.concat(taicpu.op_const_reg(A_ADDQ,S_L,parasize,r))
                else { nope ... }
                   list.concat(taicpu.op_const_reg(A_ADD,S_L,parasize,r));

                { restore the PC counter (push it on the stack)       }
                r.enum:=R_SPPUSH;
                list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hregister,r));
                list.concat(taicpu.op_none(A_RTS,S_NO));
                free_scratch_reg(list,hregister);
               end;
           end;

      end;

    procedure Tcg68k.g_save_standard_registers(list:Taasmoutput;usedinproc:Tsupregset);

    var tosave:Tsupregset;
        r:Tregister;
         
    begin
      tosave:=std_saved_registers;
      { only save the registers which are not used and must be saved }
      tosave:=tosave*usedinproc;
      r.enum:=R_SPPUSH;
      if tosave<>[] then
        list.concat(taicpu.op_reglist_reg(A_MOVEM,S_L,tosave,r));
    end;

    procedure Tcg68k.g_restore_standard_registers(list:Taasmoutput;usedinproc:Tsupregset);

    var torestore:Tsupregset;
        r:Tregister;

    begin
      torestore:=std_saved_registers;
      { should be intersected with used regs, no ? }
      torestore:=torestore*usedinproc;
      r.enum:=R_SPPULL;
      if torestore<>[] then
        list.concat(taicpu.op_reg_reglist(A_MOVEM,S_L,r,torestore));
    end;

    procedure tcg68k.g_save_all_registers(list : taasmoutput);
      begin
      end;

    procedure tcg68k.g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);
      begin
      end;

    procedure tcg68k.sign_extend(list: taasmoutput;_oldsize : tcgsize; reg: tregister);
      begin
        case _oldsize of
         { sign extend }
         OS_S8:
              begin
                if (rg.isaddressregister(reg)) then
                   internalerror(20020729);
                if (aktoptprocessor = MC68000) then
                  begin
                    list.concat(taicpu.op_reg(A_EXT,S_W,reg));
                    list.concat(taicpu.op_reg(A_EXT,S_L,reg));
                  end
                else
                  begin
                    list.concat(taicpu.op_reg(A_EXTB,S_L,reg));
                  end;
              end;
         OS_S16:
              begin
                if (rg.isaddressregister(reg)) then
                   internalerror(20020729);
                list.concat(taicpu.op_reg(A_EXT,S_L,reg));
              end;
         { zero extend }
         OS_8:
              begin
                list.concat(taicpu.op_const_reg(A_AND,S_L,$FF,reg));
              end;
         OS_16:
              begin
                list.concat(taicpu.op_const_reg(A_AND,S_L,$FFFF,reg));
              end;
        end; { otherwise the size is already correct }
      end;

     procedure tcg68k.a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel);

       var
         ai : taicpu;

       begin
         if cond=OC_None then
           ai := Taicpu.Op_sym(A_JMP,S_NO,l)
         else
           begin
             ai:=Taicpu.Op_sym(A_Bxx,S_NO,l);
             ai.SetCondition(TOpCmp2AsmCond[cond]);
           end;
         ai.is_jmp:=true;
         list.concat(ai);
       end;

{****************************************************************************}
{                               TCG64F68K                                    }
{****************************************************************************}
 procedure tcg64f68k.a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);
  var
   hreg1, hreg2 : tregister;
   opcode : tasmop;
  begin
    opcode := topcg2tasmop[op];
    case op of
      OP_ADD :
         begin
            { if one of these three registers is an address
              register, we'll really get into problems!
            }
            if rg.isaddressregister(regdst.reglo) or
               rg.isaddressregister(regdst.reghi) or
               rg.isaddressregister(regsrc.reghi) then
                 internalerror(20020817);
            list.concat(taicpu.op_reg_reg(A_ADD,S_L,regsrc.reglo,regdst.reglo));
            list.concat(taicpu.op_reg_reg(A_ADDX,S_L,regsrc.reghi,regdst.reghi));
         end;
      OP_AND,OP_OR :
          begin
            { at least one of the registers must be a data register }
            if (rg.isaddressregister(regdst.reglo) and
                rg.isaddressregister(regsrc.reglo)) or
               (rg.isaddressregister(regsrc.reghi) and
                rg.isaddressregister(regdst.reghi))
               then
                 internalerror(20020817);
            cg.a_op_reg_reg(list,op,OS_32,regsrc.reglo,regdst.reglo);
            cg.a_op_reg_reg(list,op,OS_32,regsrc.reghi,regdst.reghi);
          end;
      { this is handled in 1st pass for 32-bit cpu's (helper call) }
      OP_IDIV,OP_DIV,
      OP_IMUL,OP_MUL: internalerror(2002081701);
      { this is also handled in 1st pass for 32-bit cpu's (helper call) }
      OP_SAR,OP_SHL,OP_SHR: internalerror(2002081702);
      OP_SUB:
         begin
            { if one of these three registers is an address
              register, we'll really get into problems!
            }
            if rg.isaddressregister(regdst.reglo) or
               rg.isaddressregister(regdst.reghi) or
               rg.isaddressregister(regsrc.reghi) then
                 internalerror(20020817);
            list.concat(taicpu.op_reg_reg(A_SUB,S_L,regsrc.reglo,regdst.reglo));
            list.concat(taicpu.op_reg_reg(A_SUBX,S_L,regsrc.reghi,regdst.reghi));
         end;
      OP_XOR:
        begin
            if rg.isaddressregister(regdst.reglo) or
               rg.isaddressregister(regsrc.reglo) or
               rg.isaddressregister(regsrc.reghi) or
               rg.isaddressregister(regdst.reghi) then
                 internalerror(20020817);
            list.concat(taicpu.op_reg_reg(A_EOR,S_L,regsrc.reglo,regdst.reglo));
            list.concat(taicpu.op_reg_reg(A_EOR,S_L,regsrc.reghi,regdst.reghi));
        end;
    end; { end case }
  end;


 procedure tcg64f68k.a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);
  var
   lowvalue : cardinal;
   highvalue : cardinal;
  begin
    { is it optimized out ? }
    if optimize64_op_const_reg(list,op,value,reg) then
       exit;

    lowvalue := cardinal(value);
    highvalue:= value shr 32;

   { the destination registers must be data registers }
   if  rg.isaddressregister(reg.reglo) or
       rg.isaddressregister(reg.reghi) then
         internalerror(20020817);
   case op of
      OP_ADD :
         begin
            list.concat(taicpu.op_const_reg(A_ADD,S_L,lowvalue,reg.reglo));
            list.concat(taicpu.op_const_reg(A_ADDX,S_L,highvalue,reg.reglo));
         end;
      OP_AND :
          begin
            { should already be optimized out }
            internalerror(2002081801);
          end;
      OP_OR :
          begin
            { should already be optimized out }
            internalerror(2002081802);
          end;
      { this is handled in 1st pass for 32-bit cpu's (helper call) }
      OP_IDIV,OP_DIV,
      OP_IMUL,OP_MUL: internalerror(2002081701);
      { this is also handled in 1st pass for 32-bit cpu's (helper call) }
      OP_SAR,OP_SHL,OP_SHR: internalerror(2002081702);
      OP_SUB:
         begin
            list.concat(taicpu.op_const_reg(A_SUB,S_L,lowvalue,reg.reglo));
            list.concat(taicpu.op_const_reg(A_SUBX,S_L,highvalue,reg.reglo));
         end;
      OP_XOR:
        begin
            list.concat(taicpu.op_const_reg(A_EOR,S_L,lowvalue,reg.reglo));
            list.concat(taicpu.op_const_reg(A_EOR,S_L,highvalue,reg.reglo));
        end;
    end; { end case }
  end;

begin
  cg := tcg68k.create;
  cg64 :=tcg64f68k.create;
end.

{
  $Log$
  Revision 1.18  2003-02-19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.17  2003/02/12 22:11:13  carl
    * some small m68k bugfixes

  Revision 1.16  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.15  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.14  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.13  2002/12/01 22:12:36  carl
    * rename an error message

  Revision 1.12  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.11  2002/11/18 17:32:00  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.10  2002/09/22 14:15:31  carl
    + a_call_reg

  Revision 1.9  2002/09/17 18:54:05  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.8  2002/09/08 15:12:45  carl
    + a_call_reg

  Revision 1.7  2002/09/07 20:53:28  carl
    * cardinal -> longword

  Revision 1.6  2002/09/07 15:25:12  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/08/19 18:17:48  carl
    + optimize64_op_const_reg implemented (optimizes 64-bit constant opcodes)
    * more fixes to m68k for 64-bit operations

  Revision 1.4  2002/08/16 14:24:59  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.3  2002/08/15 08:13:54  carl
    - a_load_sym_ofs_reg removed
    * loadvmt now calls loadaddr_ref_reg instead

  Revision 1.2  2002/08/14 19:16:34  carl
    + m68k type conversion nodes
    + started some mathematical nodes
    * out of bound references should now be handled correctly

  Revision 1.1  2002/08/13 18:30:22  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.5  2002/08/12 15:08:43  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.2  2002/08/05 17:27:52  carl
    + updated m68k

  Revision 1.1  2002/07/29 17:51:32  carl
    + restart m68k support

}


