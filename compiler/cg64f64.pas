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
       cpubase,
       cgbase,cgobj,
       symtype;

{$ifndef cpu64bit}
    type
      {# Defines all the methods required on 32-bit processors
         to handle 64-bit integers.
      }
      tcg64f64 = class(tcg64)
        procedure a_reg_alloc(list : taasmoutput;r : tregister64);override;
        procedure a_reg_dealloc(list : taasmoutput;r : tregister64);override;
        procedure a_load64_const_ref(list : taasmoutput;value : int64;const ref : treference);override;
        procedure a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);override;
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);override;
        procedure a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64);override;
        procedure a_load64_const_reg(list : taasmoutput;value: int64;reg : tregister64);override;
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64);override;
        procedure a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);override;
        procedure a_load64_const_loc(list : taasmoutput;value : int64;const l : tlocation);override;
        procedure a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);override;

        procedure a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);override;
        procedure a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);override;
        procedure a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);override;
        procedure a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);override;
        procedure a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);override;
        procedure a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);override;

        procedure a_op64_reg_ref(list : taasmoutput;op:TOpCG;regsrc : tregister64;const ref : treference);override;
        procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);override;
        procedure a_op64_const_loc(list : taasmoutput;op:TOpCG;value:int64;const l: tlocation);override;
        procedure a_op64_reg_loc(list : taasmoutput;op:TOpCG;reg : tregister64;const l : tlocation);override;
        procedure a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reg : tregister64);override;
        procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : int64;regdst : tregister64);override;
        procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;value : int64;const ref : treference);override;

        procedure a_param64_reg(list : taasmoutput;reg : tregister64;const locpara : tparalocation);override;
        procedure a_param64_const(list : taasmoutput;value : int64;const locpara : tparalocation);override;
        procedure a_param64_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);override;
        procedure a_param64_loc(list : taasmoutput;const l : tlocation;const locpara : tparalocation);override;

        function optimize64_op_const_reg(list: taasmoutput; var op: topcg; var a : int64; var reg: tregister64): boolean;override;

        { override to catch 64bit rangechecks }
        procedure g_rangecheck64(list: taasmoutput; const l:tlocation;fromdef,todef: tdef); override;
      end;
{$endif cpu64bit}

  implementation

    uses
      verbose;

{$ifndef cpu64bit}
    procedure tcg64f64.a_load64_const_ref(list : taasmoutput;value : int64;const ref : treference);
      begin
         cg.a_load_const_ref(list,OS_64,value,ref);
      end;


    procedure tcg64f64.a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);
      begin
         cg.a_load_reg_ref(list,OS_64,OS_64,reg,ref);
      end;


    procedure tcg64f64.a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);
      begin
         cg.a_load_ref_reg(list,OS_64,OS_64,ref,reg);
      end;


    procedure tcg64f64.a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64);
      begin
         cg.a_load_reg_reg(list,OS_64,OS_64,regsrc,regdst);
      end;


    procedure tcg64f64.a_load64_const_reg(list : taasmoutput;value : int64;reg : tregister64);
      begin
         cg.a_load_const_reg(list,OS_64,value,reg);
      end;


    procedure tcg64f64.a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64);
      begin
         cg.a_load_loc_reg(list,l.size,l,reg);
      end;


    procedure tcg64f64.a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);
      begin
         cg.a_load_loc_ref(list,l.size,l,ref);
      end;


    procedure tcg64f64.a_load64_const_loc(list : taasmoutput;value : int64;const l : tlocation);
      begin
         cg.a_load_const_loc(list,value,l);
      end;


    procedure tcg64f64.a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);
      begin
         cg.a_load_reg_loc(list,OS_64,reg,l);
      end;


    procedure tcg64f64.a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
      begin
        internalerror(200404211);
      end;


    procedure tcg64f64.a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
      begin
        internalerror(200404212);
      end;


    procedure tcg64f64.a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
      begin
        internalerror(200404213);
      end;


    procedure tcg64f64.a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
      begin
        internalerror(200404214);
      end;


    procedure tcg64f64.a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);
      begin
        internalerror(200404215);
      end;

    procedure tcg64f64.a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);
      begin
        internalerror(200404216);
      end;


    procedure tcg64f64.a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);
      begin
         cg.a_op_ref_reg(list,op,OS_64,ref,reg);
      end;


    procedure tcg64f64.a_op64_reg_ref(list : taasmoutput;op:TOpCG;regsrc : tregister64;const ref : treference);
      begin
        cg.a_op_reg_ref(list,op,OS_64,regsrc,ref);
      end;


    procedure tcg64f64.a_op64_const_reg(list : taasmoutput;op:TOpCG;value : int64;regdst : tregister64);
      begin
        cg.a_op_const_reg(list,op,OS_64,value,regdst);
      end;


    procedure tcg64f64.a_op64_const_ref(list : taasmoutput;op:TOpCG;value : int64;const ref : treference);
      begin
        cg.a_op_const_ref(list,op,OS_64,value,ref);
      end;


    procedure tcg64f64.a_op64_const_loc(list : taasmoutput;op:TOpCG;value : int64;const l: tlocation);
      begin
        cg.a_op_const_loc(list,op,value,l);
      end;


    procedure tcg64f64.a_op64_reg_loc(list : taasmoutput;op:TOpCG;reg : tregister64;const l : tlocation);
      begin
        cg.a_op_reg_loc(list,op,reg,l);
      end;


    procedure tcg64f64.a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reg : tregister64);
      begin
        internalerror(200404217);
      end;


    procedure tcg64f64.a_param64_reg(list : taasmoutput;reg : tregister64;const locpara : tparalocation);
      begin
        cg.a_param_reg(list,OS_64,reg,locpara);
      end;


    procedure tcg64f64.a_param64_const(list : taasmoutput;value : int64;const locpara : tparalocation);
      begin
        cg.a_param_const(list,OS_64,value,locpara);
      end;


    procedure tcg64f64.a_param64_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);
      begin
        cg.a_param_ref(list,OS_64,r,locpara);
      end;


    procedure tcg64f64.a_param64_loc(list : taasmoutput;const l : tlocation;const locpara : tparalocation);
      begin
        cg.a_param_loc(list,l,locpara);
      end;


    procedure tcg64f64.g_rangecheck64(list: taasmoutput; const l:tlocation;fromdef,todef: tdef);
      begin
        cg.g_rangecheck(list,l,fromdef,todef);
      end;


    function tcg64f64.optimize64_op_const_reg(list: taasmoutput; var op: topcg; var a : int64; var reg: tregister64): boolean;
      begin
        result:=cg.optimize_op_const_reg(list,op,a,reg);
      end;


    procedure tcg64f64.a_reg_alloc(list : taasmoutput;r : tregister64);
      begin
        list.concat(tai_regalloc.alloc(r));
      end;


    procedure tcg64f64.a_reg_dealloc(list : taasmoutput;r : tregister64);
      begin
        list.concat(tai_regalloc.dealloc(r));
      end;
{$endif cpu64bit}


end.
{
  $Log$
  Revision 1.14  2004-10-31 21:45:02  peter
    * generic tlocation
    * move tlocation to cgutils

  Revision 1.13  2004/06/20 08:55:28  florian
    * logs truncated

  Revision 1.12  2004/06/16 20:07:07  florian
    * dwarf branch merged

  Revision 1.11.2.2  2004/04/27 18:18:25  peter
    * aword -> aint

  Revision 1.11.2.1  2004/04/21 18:54:29  florian
    + implemented most methods as redirections to cg

  Revision 1.11  2004/01/13 18:08:58  florian
    * x86-64 compilation fixed

}
