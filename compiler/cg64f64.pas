{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generation for 64 bit int
    arithmethics on 64 bit processors

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
{# This unit implements the code generation for 64 bit int arithmethics on
   64 bit processors.
}
unit cg64f64;

  {$i fpcdefs.inc}

  interface

    uses
       aasm,
       cpuinfo, cpubase,
       cginfo, cgobj,
       node,symtype;

    type
      {# Defines all the methods required on 32-bit processors
         to handle 64-bit integers.
      }
      tcg64f32 = class(tcg64)
        procedure a_load64_const_ref(list : taasmoutput;valuelo, valuehi : AWord;const ref : treference);override;
        procedure a_load64_reg_ref(list : taasmoutput;reglo, reghi : tregister;const ref : treference);override;
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reglo,reghi : tregister);override;
        procedure a_load64_reg_reg(list : taasmoutput;reglosrc,reghisrc,reglodst,reghidst : tregister);override;
        procedure a_load64_const_reg(list : taasmoutput;valuelosrc,valuehisrc:AWord;reglodst,reghidst : tregister);override;
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reglo,reghi : tregister);override;
        procedure a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);override;
        procedure a_load64_const_loc(list : taasmoutput;valuelo, valuehi : AWord;const l : tlocation);override;
        procedure a_load64_reg_loc(list : taasmoutput;reglo, reghi : tregister;const l : tlocation);override;

        procedure a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);override;
        procedure a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);override;
        procedure a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);override;
        procedure a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);override;
        procedure a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);override;
        procedure a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);override;

        procedure a_op64_const_loc(list : taasmoutput;op:TOpCG;valuelosrc,valuehisrc:aword;const l: tlocation);override;
        procedure a_op64_reg_loc(list : taasmoutput;op:TOpCG;reglo,reghi : tregister;const l : tlocation);override;
        procedure a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reglo,reghi : tregister);override;

        procedure a_param64_reg(list : taasmoutput;reglo,reghi : tregister;nr : longint);override;
        procedure a_param64_const(list : taasmoutput;valuelo,valuehi : aword;nr : longint);override;
        procedure a_param64_ref(list : taasmoutput;const r : treference;nr : longint);override;
        procedure a_param64_loc(list : taasmoutput;const l : tlocation;nr : longint);override;

        { override to catch 64bit rangechecks }
        procedure g_rangecheck(list: taasmoutput; const p: tnode;
          const todef: tdef); override;
      end;

  implementation

    procedure tcg64f64.a_load64_const_ref(list : taasmoutput;value : qword;const ref : treference);
      begin
         cg.a_load_const_ref(list,OS_64,value,ref);
      end;

    procedure tcg64f64.a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);
      begin
         cg.a_load_const_ref(list,OS_64,reg,ref);
      end;

    procedure tcg64f64.a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);
      begin
         cg.a_load_ref_ref(list,OS_64,ref,reg);
      end;

    procedure tcg64f64.a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64);
      begin
         cg.a_load_reg_reg(list,OS_64,regsrc,regdst);
      end;

    procedure tcg64f64.a_load64_const_reg(list : taasmoutput;value : qword;reg : tregister64);
      begin
         cg.a_load_const_reg(list,OS_64,value,reg);
      end;

    procedure tcg64f64.a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64);
      begin
         cg.a_load_loc_reg(list,l,reg);
      end;

    procedure tcg64f64.a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);
      begin
         cg.a_load_loc_ref(list,OS_64,l,ref);
      end;

    procedure tcg64f64.a_load64_const_loc(list : taasmoutput;value : qword;const l : tlocation);
      begin
         cg.a_load_const_loc(list,value,l);
      end;

    procedure tcg64f64.a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);
      begin
         cg.a_load_reg_loc(list,reg,l);
      end;

    procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reglo,reg : tregister64);
      begin
         cg.a_op_ref_reg(list,
      end;

    procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);
      begin
      end;

    procedure a_op64_reg_ref(list : taasmoutput;op:TOpCG;regsrc : tregister64;const ref : treference);
      begin
      end;

    procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;regdst : tregister64);
      begin
      end;

    procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;value : qword;const ref : treference);
      begin
      end;

    procedure a_op64_const_loc(list : taasmoutput;op:TOpCG;value : qword;const l: tlocation);
      begin
      end;

    procedure a_op64_reg_loc(list : taasmoutput;op:TOpCG;reg : tregister64;const l : tlocation);
      begin
      end;

    procedure a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reg64 : tregister64);
      begin
      end;

    procedure a_param64_reg(list : taasmoutput;reg64 : tregister;nr : longint);
      begin
      end;

    procedure a_param64_const(list : taasmoutput;value : qword;nr : longint);
      begin
      end;

    procedure a_param64_ref(list : taasmoutput;const r : treference;nr : longint);
      begin
      end;

    procedure a_param64_loc(list : taasmoutput;const l : tlocation;nr : longint);
      begin
      end;

    procedure g_rangecheck64(list: taasmoutput; const p: tnode;
      const todef: tdef);
      begin
      end;

end.
{
  $Log$
  Revision 1.2  2002-07-01 16:23:52  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.1  2002/06/08 19:36:54  florian
    * initial release

}