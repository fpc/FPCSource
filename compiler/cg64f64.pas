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
       aasmbase,aasmtai,aasmcpu,
       cpuinfo, cpubase,
       cginfo, cgobj,
       symtype;

    type
      {# Defines all the methods required on 32-bit processors
         to handle 64-bit integers.
      }
      tcg64f64 = class(tcg64)
        procedure a_reg_alloc(list : taasmoutput;r : tregister64);override;
        procedure a_reg_dealloc(list : taasmoutput;r : tregister64);override;
        procedure a_load64_const_ref(list : taasmoutput;value : AWord;const ref : treference);override;
        procedure a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);override;
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);override;
        procedure a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64);override;
        procedure a_load64_const_reg(list : taasmoutput;value: qword;reg : tregister64);override;
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64);override;
        procedure a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);override;
        procedure a_load64_const_loc(list : taasmoutput;value : qword;const l : tlocation);override;
        procedure a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);override;

        procedure a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);override;
        procedure a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);override;
        procedure a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);override;
        procedure a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);override;
        procedure a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);override;
        procedure a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);override;

        procedure a_op64_reg_ref(list : taasmoutput;op:TOpCG;regsrc : tregister64;const ref : treference);override;
        procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);override;
        procedure a_op64_const_loc(list : taasmoutput;op:TOpCG;value:qword;const l: tlocation);override;
        procedure a_op64_reg_loc(list : taasmoutput;op:TOpCG;reg : tregister64;const l : tlocation);override;
        procedure a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reg : tregister64);override;
        procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;regdst : tregister64);override;
        procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;value : qword;const ref : treference);override;

        procedure a_param64_reg(list : taasmoutput;reg : tregister64;const locpara : tparalocation);override;
        procedure a_param64_const(list : taasmoutput;value : qword;const locpara : tparalocation);override;
        procedure a_param64_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);override;
        procedure a_param64_loc(list : taasmoutput;const l : tlocation;const locpara : tparalocation);override;

        function optimize64_op_const_reg(list: taasmoutput; var op: topcg; var a : qword; var reg: tregister64): boolean;override;

        { override to catch 64bit rangechecks }
        procedure g_rangecheck64(list: taasmoutput; const l: tlocation;fromdef,todef: tdef); override;
      end;

  implementation

    procedure tcg64f64.a_load64_const_ref(list : taasmoutput;value : qword;const ref : treference);
      begin
         cg.a_load_const_ref(list,OS_64,value,ref);
      end;

    procedure tcg64f64.a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);
      begin
         cg.a_load_reg_ref(list,OS_64,reg,ref);
      end;

    procedure tcg64f64.a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);
      begin
         cg.a_load_ref_reg(list,OS_64,ref,reg);
      end;

    procedure tcg64f64.a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64);
      begin
         cg.a_load_reg_reg(list,OS_64,OS_64,regsrc,regdst);
      end;

    procedure tcg64f64.a_load64_const_reg(list : taasmoutput;value : qword;reg : tregister64);
      begin
         cg.a_load_const_reg(list,OS_64,value,reg);
      end;

    procedure tcg64f64.a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64);
      begin
         cg.a_load_loc_reg(list,l.size,l,reg);
      end;

    procedure tcg64f64.a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);
      begin
         cg.a_load_loc_ref(list,l,ref);
      end;

    procedure tcg64f64.a_load64_const_loc(list : taasmoutput;value : qword;const l : tlocation);
      begin
         cg.a_load_const_loc(list,value,l);
      end;

    procedure tcg64f64.a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);
      begin
         cg.a_load_reg_loc(list,OS_64,reg,l);
      end;

    procedure tcg64f64.a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
      begin
      end;

    procedure tcg64f64.a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
      begin
      end;

    procedure tcg64f64.a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
      begin
      end;

    procedure tcg64f64.a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
      begin
      end;

    procedure tcg64f64.a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);
      begin
      end;

    procedure tcg64f64.a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);
      begin
      end;


    procedure tcg64f64.a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);
      begin
         cg.a_op_ref_reg(list,op,OS_64,ref,reg);
      end;

    procedure tcg64f64.a_op64_reg_ref(list : taasmoutput;op:TOpCG;regsrc : tregister64;const ref : treference);
      begin
      end;

    procedure tcg64f64.a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;regdst : tregister64);
      begin
      end;

    procedure tcg64f64.a_op64_const_ref(list : taasmoutput;op:TOpCG;value : qword;const ref : treference);
      begin
      end;

    procedure tcg64f64.a_op64_const_loc(list : taasmoutput;op:TOpCG;value : qword;const l: tlocation);
      begin
      end;

    procedure tcg64f64.a_op64_reg_loc(list : taasmoutput;op:TOpCG;reg : tregister64;const l : tlocation);
      begin
      end;

    procedure tcg64f64.a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reg : tregister64);
      begin
      end;

    procedure tcg64f64.a_param64_reg(list : taasmoutput;reg64 : tregister;const locpara : tparalocation);
      begin
      end;

    procedure tcg64f64.a_param64_const(list : taasmoutput;value : qword;const locpara : tparalocation);
      begin
      end;

    procedure tcg64f64.a_param64_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);
      begin
      end;

    procedure tcg64f64.a_param64_loc(list : taasmoutput;const l : tlocation;const locpara : tparalocation);
      begin
      end;

    procedure tcg64f64.g_rangecheck64(list: taasmoutput; const p: tnode;def: tdef);
      begin
      end;

    function tcg64f64.optimize64_op_const_reg(list: taasmoutput; var op: topcg; var a : qword; var reg: tregister64): boolean;
     begin
       { this should be the same routine as optimize_const_reg!!!!!!!! }
     end;


    procedure tcg64f64.a_reg_alloc(list : taasmoutput;r : tregister64);

      begin
         list.concat(tai_regalloc.alloc(r));
      end;

    procedure tcg64f64.a_reg_dealloc(list : taasmoutput;r : tregister64);

      begin
         list.concat(tai_regalloc.dealloc(r));
      end;


end.
{
  $Log$
  Revision 1.8  2003-06-03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.7  2003/05/30 23:49:18  jonas
    * a_load_loc_reg now has an extra size parameter for the destination
      register (properly fixes what I worked around in revision 1.106 of
      ncgutil.pas)

  Revision 1.6  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.5  2002/09/07 15:25:00  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/08/19 18:17:48  carl
    + optimize64_op_const_reg implemented (optimizes 64-bit constant opcodes)
    * more fixes to m68k for 64-bit operations

  Revision 1.3  2002/08/17 22:09:43  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.2  2002/07/01 16:23:52  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.1  2002/06/08 19:36:54  florian
    * initial release

}
