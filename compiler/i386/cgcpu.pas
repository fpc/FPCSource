{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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
{ This unit implements the code generator for the i386.
}
unit cgcpu;

{$i fpcdefs.inc}

  interface

    uses
       cgbase,cgobj,cg64f32,cgx86,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,
       node,symconst
{$ifdef delphi}
       ,dmisc
{$endif}
       ;

    type
      tcg386 = class(tcgx86)
         class function reg_cgsize(const reg: tregister): tcgsize; override;
     end;

      tcg64f386 = class(tcg64f32)
        procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);override;
        procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);override;
        procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);override;
        procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;value : qword;const ref : treference);override;
      private
        procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      end;

  implementation

    uses
       globtype,globals,verbose,systems,cutils,
       symdef,symsym,defutil,paramgr,
       tgobj;


    class function tcg386.reg_cgsize(const reg: tregister): tcgsize;
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


{ ************* 64bit operations ************ }

    procedure tcg64f386.get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
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


    procedure tcg64f386.a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);
      var
        op1,op2 : TAsmOp;
        tempref : treference;
      begin
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_ref_reg(op1,S_L,ref,reg.reglo));
        tempref:=ref;
        inc(tempref.offset,4);
        list.concat(taicpu.op_ref_reg(op2,S_L,tempref,reg.reghi));
      end;


    procedure tcg64f386.a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);
      var
        op1,op2 : TAsmOp;
      begin
        case op of
          OP_NEG :
            begin
              if (regsrc.reglo<>regdst.reglo) then
                a_load64_reg_reg(list,regsrc,regdst,false);
              list.concat(taicpu.op_reg(A_NOT,S_L,regdst.reghi));
              list.concat(taicpu.op_reg(A_NEG,S_L,regdst.reglo));
              list.concat(taicpu.op_const_reg(A_SBB,S_L,aword(-1),regdst.reghi));
              exit;
            end;
          OP_NOT :
            begin
              if (regsrc.reglo<>regdst.reglo) then
                a_load64_reg_reg(list,regsrc,regdst,false);
              list.concat(taicpu.op_reg(A_NOT,S_L,regdst.reghi));
              list.concat(taicpu.op_reg(A_NOT,S_L,regdst.reglo));
              exit;
            end;
        end;
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_reg_reg(op1,S_L,regsrc.reglo,regdst.reglo));
        list.concat(taicpu.op_reg_reg(op2,S_L,regsrc.reghi,regdst.reghi));
      end;


    procedure tcg64f386.a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;reg : tregister64);
      var
        op1,op2 : TAsmOp;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_const_reg(list,op,OS_32,lo(value),reg.reglo);
              cg.a_op_const_reg(list,op,OS_32,hi(value),reg.reghi);
            end;
          OP_ADD, OP_SUB:
            begin
              // can't use a_op_const_ref because this may use dec/inc
              get_64bit_ops(op,op1,op2);
              list.concat(taicpu.op_const_reg(op1,S_L,lo(value),reg.reglo));
              list.concat(taicpu.op_const_reg(op2,S_L,hi(value),reg.reghi));
            end;
          else
            internalerror(200204021);
        end;
      end;


    procedure tcg64f386.a_op64_const_ref(list : taasmoutput;op:TOpCG;value : qword;const ref : treference);
      var
        op1,op2 : TAsmOp;
        tempref : treference;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            begin
              cg.a_op_const_ref(list,op,OS_32,lo(value),ref);
              tempref:=ref;
              inc(tempref.offset,4);
              cg.a_op_const_ref(list,op,OS_32,hi(value),tempref);
            end;
          OP_ADD, OP_SUB:
            begin
              get_64bit_ops(op,op1,op2);
              // can't use a_op_const_ref because this may use dec/inc
              list.concat(taicpu.op_const_ref(op1,S_L,lo(value),ref));
              tempref:=ref;
              inc(tempref.offset,4);
              list.concat(taicpu.op_const_ref(op2,S_L,hi(value),tempref));
            end;
          else
            internalerror(200204022);
        end;
      end;

begin
  cg := tcg386.create;
  cg64 := tcg64f386.create;
end.
{
  $Log$
  Revision 1.40  2003-10-10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.39  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.38  2003/09/25 13:13:32  florian
    * more x86-64 fixes

  Revision 1.37  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.36.2.1  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.36  2003/06/12 18:31:18  peter
    * fix newra cycle for i386

  Revision 1.35  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.34  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.33  2003/05/22 21:32:28  peter
    * removed some unit dependencies

  Revision 1.32  2002/11/25 17:43:26  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.31  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.30  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.29  2002/07/20 19:28:47  florian
    * splitting of i386\cgcpu.pas into x86\cgx86.pas and i386\cgcpu.pas
      cgx86.pas will contain the common code for i386 and x86_64

  Revision 1.28  2002/07/20 11:58:00  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.27  2002/07/11 14:41:32  florian
    * start of the new generic parameter handling

  Revision 1.26  2002/07/07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.25  2002/07/01 18:46:30  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.24  2002/07/01 16:23:55  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.23  2002/06/16 08:16:59  carl
  * bugfix of missing popecx for shift operations

  Revision 1.22  2002/05/22 19:02:16  carl
  + generic FPC_HELP_FAIL
  + generic FPC_HELP_DESTRUCTOR instated (original from Pierre)
  + generic FPC_DISPOSE_CLASS
  + TEST_GENERIC define

  Revision 1.21  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.20  2002/05/18 13:34:22  peter
    * readded missing revisions

  Revision 1.19  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.17  2002/05/13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.16  2002/05/12 19:59:05  carl
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

}
