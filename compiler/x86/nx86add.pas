{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

    Common code generation for add nodes on the i386 and x86

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
{
Common code generation for add nodes on the i386 and x86
}
unit nx86add;

{$i fpcdefs.inc}

  interface

    uses
       node,nadd,ncgadd,cpubase;

    type
       tx86addnode = class(tcgaddnode)
         procedure second_floataddsse;
       end;


  implementation

    uses
      verbose,
      aasmtai,
      cgbase,cgobj,
      ncgutil,
      defutil;

    procedure tx86addnode.second_floataddsse;
      var
        op : topcg;
      begin
        pass_left_right;
        if (nf_swaped in flags) then
          swapleftright;

        case nodetype of
          addn :
            op:=OP_ADD;
          muln :
            op:=OP_MUL;
          subn :
            op:=OP_SUB;
          slashn :
            op:=OP_DIV;
          else
            internalerror(200312231);
        end;

        location_reset(location,LOC_MMREGISTER,def_cgsize(resulttype.def));
        { we can use only right as left operand if the operation is commutative }
        if (right.location.loc=LOC_MMREGISTER) and (op in [OP_ADD,OP_MUL]) then
          begin
            location.register:=right.location.register;
            cg.a_opmm_loc_reg(exprasmlist,op,location.size,left.location,location.register,mms_movescalar);
            location_release(exprasmlist,left.location);
          end
        else
          begin
            location_force_mmregscalar(exprasmlist,left.location,false);
            location.register:=left.location.register;
            cg.a_opmm_loc_reg(exprasmlist,op,location.size,right.location,location.register,mms_movescalar);
            location_release(exprasmlist,right.location);
          end;
      end;

end.
{
  $Log$
  Revision 1.2  2003-12-23 14:38:07  florian
    + second_floataddsse implemented

  Revision 1.1  2003/10/13 01:58:04  florian
    * some ideas for mm support implemented
}
