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

end.
