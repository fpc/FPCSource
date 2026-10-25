{
    Copyright (c) 1998-2002 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generation for 128 bit int
    arithmetics on 64 bit processors

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
{# This unit implements the code generation for 128 bit int arithmetics on
   64 bit processors.
}
unit cg128f64;

  {$i fpcdefs.inc}

  interface

    uses
      aasmdata,
      cgbase,cgobj,cgutils;

    type
      {# Defines all the methods required on 64-bit processors
         to handle 128-bit integers.
      }
      tcg128f64 = class(tcg128)
        procedure a_load128_loc_reg(list : TAsmList;const l : tlocation;reg : tregister128);override;

        procedure a_op128_ref_reg(list : TAsmList;op:TOpCG;size : tcgsize;const ref : treference;reg : tregister128);override;
        procedure a_op128_loc_reg(list : TAsmList;op:TOpCG;size : tcgsize;const l : tlocation;reg128 : tregister128);override;
      end;


  implementation

    uses
      globtype,
      verbose;

{****************************************************************************
                                  TCG128F64
****************************************************************************}

    procedure tcg128f64.a_load128_loc_reg(list : TAsmList;const l : tlocation;reg : tregister128);
      begin
        case l.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_load128_ref_reg(list,l.reference,reg);
          LOC_REGISTER,LOC_CREGISTER:
            a_load128_reg_reg(list,l.register128,reg);
          LOC_CONSTANT :
            a_load128_const_reg(list,hlcgint_lo(l.value128),hlcgint_hi(l.value128),reg);
          //LOC_SUBSETREF, LOC_CSUBSETREF:
          //  a_load128_subsetref_reg(list,l.sref,reg);
          else
            internalerror(2026102503);
        end;
      end;


    procedure tcg128f64.a_op128_ref_reg(list : TAsmList;op:TOpCG;size : tcgsize;const ref : treference;reg : tregister128);
      var
        tempreg: tregister128;
      begin
        tempreg.reghi:=cg.getintregister(list,OS_64);
        tempreg.reglo:=cg.getintregister(list,OS_64);
        a_load128_ref_reg(list,ref,tempreg);
        a_op128_reg_reg(list,op,size,tempreg,reg);
      end;


    procedure tcg128f64.a_op128_loc_reg(list : TAsmList;op:TOpCG;size : tcgsize;const l : tlocation;reg128 : tregister128);
      begin
        case l.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_op128_ref_reg(list,op,size,l.reference,reg128);
          LOC_REGISTER,LOC_CREGISTER:
            a_op128_reg_reg(list,op,size,l.register128,reg128);
          LOC_CONSTANT :
            a_op128_const_reg(list,op,size,hlcgint_lo(l.value128),hlcgint_hi(l.value128),reg128);
          else
            internalerror(2026102501);
        end;
      end;


end.
