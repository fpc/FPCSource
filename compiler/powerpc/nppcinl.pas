{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 inline nodes

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
unit nppcinl;

{$i fpcdefs.inc}

interface

    uses
       node,ninl,ncginl;

    type
       tppcinlinenode = class(tcginlinenode)
          { first pass override
            so that the code generator will actually generate
            these nodes.
          }
          function first_abs_real: tnode; override;
          function first_sqr_real: tnode; override;
          procedure second_abs_real; override;
          procedure second_sqr_real; override;
       private
          procedure load_fpu_location;
       end;

implementation

    uses
      cutils,globals,
      aasmtai,aasmcpu,
      symconst,symdef,
      cginfo,cgbase,pass_2,
      cpubase,ncgutil,
      cgobj,rgobj;


{*****************************************************************************
                              TPPCINLINENODE
*****************************************************************************}

     function tppcinlinenode.first_abs_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_abs_real := nil;
      end;

     function tppcinlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_sqr_real := nil;
      end;

       { load the FPU into the an fpu register }
       procedure tppcinlinenode.load_fpu_location;
         begin
           location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
           secondpass(left);
           location_force_fpureg(exprasmlist,location,true);
           location_copy(location,left.location);
           if (location.loc = LOC_CFPUREGISTER) then
             begin
               location.loc := LOC_FPUREGISTER;
               location.register := rg.getregisterfpu(exprasmlist,OS_F64);
             end;
         end;

     procedure tppcinlinenode.second_abs_real;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         exprasmlist.concat(taicpu.op_reg_reg(A_FABS,location.register,
           left.location.register));
       end;

     procedure tppcinlinenode.second_sqr_real;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         exprasmlist.concat(taicpu.op_reg_reg_reg(A_FMUL,location.register,
           location.register,left.location.register));
       end;

begin
   cinlinenode:=tppcinlinenode;
end.
{
  $Log$
  Revision 1.8  2003-06-13 17:03:38  jonas
    * fixed bugs in case the left node was a LOC_(C)REFERENCE

  Revision 1.7  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.6  2003/05/24 13:39:32  jonas
    * fsqrt is an optional instruction in the ppc architecture and isn't
      implemented by any current ppc afaik, so use the generic sqrt routine
      instead (adapted so it works with compilerproc)

  Revision 1.5  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.4  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.3  2002/09/18 09:19:37  jonas
    * fixed LOC_REFERENCE/LOC_CREFERENCE problems

  Revision 1.2  2002/08/19 17:35:42  jonas
    * fixes

  Revision 1.1  2002/08/10 17:15:00  jonas
    + abs, sqr, sqrt implementations


}

