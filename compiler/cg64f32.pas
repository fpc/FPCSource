{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generation for 64 bit int
    arithmethics on 32 bit processors

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

unit cg64f32;

  {$i defines.inc}

  interface

    uses
       aasm, cgobj, cpubase;

    type
      tcg64f32 = class(tcg)
        procedure a_load64_reg_ref(list : taasmoutput;reglo, reghi : tregister;const ref : treference);
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reglo,reghi : tregister);
        procedure a_load64_reg_reg(list : taasmoutput;reglosrc,reghisrc,reglodst,reghidst : tregister);
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reglo,reghi : tregister);
      end;

  implementation

    uses
       globals,systems,cgbase,verbose;

    procedure tcg64f32.a_load64_reg_ref(list : taasmoutput;reglo, reghi : tregister;const ref : treference);
      var
        tmpreg: tregister;
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          begin
            tmpreg := reglo;
            reglo := reghi;
            reghi := tmpreg;
          end;
        a_load_reg_ref(list,OS_32,reglo,ref);
        tmpref := ref;
        inc(tmpref.offset,4);
        a_load_reg_ref(list,OS_32,reghi,tmpref);
      end;

    procedure tcg64f32.a_load64_ref_reg(list : taasmoutput;const ref : treference;reglo,reghi : tregister);
      var
        tmpreg: tregister;
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          begin
            tmpreg := reglo;
            reglo := reghi;
            reghi := tmpreg;
          end;
        a_load_ref_reg(list,OS_32,ref,reglo);
        tmpref := ref;
        inc(tmpref.offset,4);
        a_load_ref_reg(list,OS_32,tmpref,reghi);
      end;

    procedure tcg64f32.a_load64_reg_reg(list : taasmoutput;reglosrc,reghisrc,reglodst,reghidst : tregister);

      begin
        a_load_reg_reg(list,OS_32,reglosrc,reglodst);
        a_load_reg_reg(list,OS_32,reghisrc,reghidst);
      end;


    procedure tcg64f32.a_load64_loc_reg(list : taasmoutput;const l : tlocation;reglo,reghi : tregister);

      begin
        case l.loc of
          LOC_REFERENCE, LOC_MEM:
            a_load64_ref_reg(list,l.reference,reglo,reghi);
          LOC_REGISTER,LOC_CREGISTER:
            a_load64_reg_reg(list,l.registerlow,l.registerhigh,reglo,reghi);
          else
            internalerror(200112292);
        end;
      end;

(*
    procedure int64f32_assignment_int64_reg(p : passignmentnode);

      begin
      end;


begin
   p2_assignment:=@int64f32_assignement_int64;
*)
end.
{
  $Log$
  Revision 1.1  2001-12-29 15:29:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.1  2000/07/13 06:30:07  michael
  + Initial import

  Revision 1.1  2000/03/01 15:36:13  florian
    * some new stuff for the new cg

}