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
          procedure second_prefetch;override;
       private
          procedure load_fpu_location;
       end;

implementation

    uses
      cutils,globals,verbose,
      aasmtai,aasmcpu,
      symconst,symdef,
      defutil,
      cgbase,pass_2,
      cpubase,ncgutil,
      cgobj,rgobj;


{*****************************************************************************
                              TPPCINLINENODE
*****************************************************************************}

     function tppcinlinenode.first_abs_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersint:=left.registersint;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_abs_real := nil;
      end;

     function tppcinlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersint:=left.registersint;
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
           location_force_fpureg(exprasmlist,left.location,true);
           location_copy(location,left.location);
           if (location.loc = LOC_CFPUREGISTER) then
             begin
               location.loc := LOC_FPUREGISTER;
               location.register := cg.getfpuregister(exprasmlist,OS_F64);
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
           left.location.register,left.location.register));
       end;


     procedure tppcinlinenode.second_prefetch;
       var
         r: tregister;
       begin
         secondpass(left);
         case left.location.loc of
           LOC_CREFERENCE,
           LOC_REFERENCE:
             begin
               r:=cg.getintregister(exprasmlist,OS_ADDR);
               if (left.location.reference.offset = 0) and
                  not assigned(left.location.reference.symbol) then
                 begin
                   if (left.location.reference.index = NR_NO) then
                     exprasmlist.concat(taicpu.op_const_reg(A_DCBT,0,left.location.reference.base))
                   else
                     exprasmlist.concat(taicpu.op_reg_reg(A_DCBT,left.location.reference.base,left.location.reference.index));
                   location_release(exprasmlist,left.location);
                 end
               else
                 begin
                   cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,r);
                   location_release(exprasmlist,left.location);
                   exprasmlist.concat(taicpu.op_const_reg(A_DCBT,0,r));
                   cg.ungetregister(exprasmlist,r);
                 end;
             end;
           else
             internalerror(200402021);
         end;
       end;

begin
   cinlinenode:=tppcinlinenode;
end.
{
  $Log$
  Revision 1.15  2004-06-20 08:55:32  florian
    * logs truncated

  Revision 1.14  2004/05/31 11:57:48  jonas
    * fixed second_sqr_real (mainly for regvars)

  Revision 1.13  2004/02/03 22:32:54  peter
    * renamed xNNbittype to xNNinttype
    * renamed registers32 to registersint
    * replace some s32bit,u32bit with torddef([su]inttype).def.typ

  Revision 1.12  2004/02/02 21:22:19  jonas
    + implemented second_prefetch

}

