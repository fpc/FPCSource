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
         procedure second_addfloat;override;
         procedure second_addfloatsse;
         procedure pass_left_and_right(var pushedfpu:boolean);
       end;


  implementation

    uses
      globals,
      verbose,
      aasmtai,
      cpuinfo,
      cgbase,cgobj,cgx86,cga,
      pass_2,ncgutil,
      defutil;

{*****************************************************************************
                                AddFloat
*****************************************************************************}

    procedure tx86addnode.pass_left_and_right(var pushedfpu:boolean);
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
         swapleftright;
        secondpass(left);

        { are too few registers free? }
        if location.loc=LOC_FPUREGISTER then
          pushedfpu:=maybe_pushfpu(exprasmlist,right.registersfpu,left.location)
        else
          pushedfpu:=false;
        secondpass(right);
      end;


    procedure tx86addnode.second_addfloat;
      var
        op         : TAsmOp;
        resflags   : tresflags;
        pushedfpu,
        cmpop      : boolean;
      begin
        if (is_single(resulttype.def) and (aktfputype in sse_singlescalar)) or
          (is_double(resulttype.def) and (aktfputype in sse_doublescalar)) then
          begin
            second_addfloatsse;
            exit;
          end;
        pass_left_and_right(pushedfpu);

        cmpop:=false;
        case nodetype of
          addn :
            op:=A_FADDP;
          muln :
            op:=A_FMULP;
          subn :
            op:=A_FSUBP;
          slashn :
            op:=A_FDIVP;
          ltn,lten,gtn,gten,
          equaln,unequaln :
            begin
              op:=A_FCOMPP;
              cmpop:=true;
            end;
          else
            internalerror(2003042214);
        end;

        if (right.location.loc<>LOC_FPUREGISTER) then
         begin
           cg.a_loadfpu_loc_reg(exprasmlist,right.location,NR_ST);
           if (right.location.loc <> LOC_CFPUREGISTER) and
              pushedfpu then
             location_freetemp(exprasmlist,left.location);
           if (left.location.loc<>LOC_FPUREGISTER) then
            begin
              cg.a_loadfpu_loc_reg(exprasmlist,left.location,NR_ST);
              if (left.location.loc <> LOC_CFPUREGISTER) and
                 pushedfpu then
                location_freetemp(exprasmlist,left.location);
            end
           else
            begin
              { left was on the stack => swap }
              toggleflag(nf_swaped);
            end;

           { releases the right reference }
           location_release(exprasmlist,right.location);
         end
        { the nominator in st0 }
        else if (left.location.loc<>LOC_FPUREGISTER) then
         begin
           cg.a_loadfpu_loc_reg(exprasmlist,left.location,NR_ST);
           if (left.location.loc <> LOC_CFPUREGISTER) and
              pushedfpu then
             location_freetemp(exprasmlist,left.location);
         end
        else
         begin
           { fpu operands are always in the wrong order on the stack }
           toggleflag(nf_swaped);
         end;

        { releases the left reference }
        if (left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
          location_release(exprasmlist,left.location);

        { if we swaped the tree nodes, then use the reverse operator }
        if nf_swaped in flags then
          begin
             if (nodetype=slashn) then
               op:=A_FDIVRP
             else if (nodetype=subn) then
               op:=A_FSUBRP;
          end;
        { to avoid the pentium bug
        if (op=FDIVP) and (opt_processors=pentium) then
          cg.a_call_name(exprasmlist,'EMUL_FDIVP')
        else
        }
        { the Intel assemblers want operands }
        if op<>A_FCOMPP then
          begin
             emit_reg_reg(op,S_NO,NR_ST,NR_ST1);
             tcgx86(cg).dec_fpu_stack;
          end
        else
          begin
             emit_none(op,S_NO);
             tcgx86(cg).dec_fpu_stack;
             tcgx86(cg).dec_fpu_stack;
          end;

        { on comparison load flags }
        if cmpop then
         begin
           cg.getexplicitregister(exprasmlist,NR_AX);
           emit_reg(A_FNSTSW,S_NO,NR_AX);
           emit_none(A_SAHF,S_NO);
           cg.ungetregister(exprasmlist,NR_AX);
           if nf_swaped in flags then
            begin
              case nodetype of
                  equaln : resflags:=F_E;
                unequaln : resflags:=F_NE;
                     ltn : resflags:=F_A;
                    lten : resflags:=F_AE;
                     gtn : resflags:=F_B;
                    gten : resflags:=F_BE;
              end;
            end
           else
            begin
              case nodetype of
                  equaln : resflags:=F_E;
                unequaln : resflags:=F_NE;
                     ltn : resflags:=F_B;
                    lten : resflags:=F_BE;
                     gtn : resflags:=F_A;
                    gten : resflags:=F_AE;
              end;
            end;
           location_reset(location,LOC_FLAGS,OS_NO);
           location.resflags:=resflags;
         end
        else
         begin
           location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
           location.register:=NR_ST;
         end;
      end;


    procedure tx86addnode.second_addfloatsse;
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
  Revision 1.3  2003-12-25 01:07:09  florian
    + $fputype directive support
    + single data type operations with sse unit
    * fixed more x86-64 stuff

  Revision 1.2  2003/12/23 14:38:07  florian
    + second_floataddsse implemented

  Revision 1.1  2003/10/13 01:58:04  florian
    * some ideas for mm support implemented
}
