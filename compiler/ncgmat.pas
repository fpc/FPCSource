{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate generic mathematical nodes

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
unit ncgmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,cpubase,cgbase,cginfo;

type
      tcgunaryminusnode = class(tunaryminusnode)
         procedure pass_2;override;
         { This routine is called to change the sign of the 
           floating point value in the floating point 
           register r.
           
           This routine should be overriden, since
           the generic version is not optimal at all. The
           generic version assumes that floating
           point values are stored in the register
           in IEEE-754 format.
         }  
         procedure emit_float_sign_change(r: tregister; _size : tcgsize);virtual;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,defbase,
      pass_1,pass_2,
      ncon,
      cpuinfo,
      tgobj,ncgutil,cgobj,rgobj,rgcpu,cg64f32;

{*****************************************************************************
                          TCGUNARYMINUSNODE
*****************************************************************************}
    procedure tcgunaryminusnode.emit_float_sign_change(r: tregister; _size : tcgsize);
     var
       href : treference;
       hreg : tregister;
      begin
        { get a temporary memory reference to store the floating
          point value
        }
        tg.gettempofsizereference(exprasmlist,tcgsize2size[_size],href);
        { store the floating point value in the temporary memory area }
        cg.a_loadfpu_reg_ref(exprasmlist,_size,r,href);
        { only single and double ieee are supported }
        if _size = OS_F64 then
          begin
            { on little-endian machine the most significant
              32-bit value is stored at the highest address
            }  
            if target_info.endian = endian_little then
              inc(href.offset,4);
          end
        else 
        if _size <> OS_F32 then
           internalerror(20020814);
        hreg := rg.getregisterint(exprasmlist);
        { load value }
        cg.a_load_ref_reg(exprasmlist,OS_32,href,hreg);
        { bitwise complement copied value }
        cg.a_op_reg_reg(exprasmlist,OP_NOT,OS_32,hreg,hreg);
        { sign-bit is bit 31/63 of single/double }
        cg.a_op_const_reg(exprasmlist,OP_AND,$80000000,hreg);
        { or with value in reference memory }
        cg.a_op_reg_ref(exprasmlist,OP_OR,OS_32,hreg,href);
        rg.ungetregister(exprasmlist,hreg);
        { store the floating point value in the temporary memory area }
        if _size = OS_F64 then
          begin
            { on little-endian machine the most significant
              32-bit value is stored at the highest address
            }  
            if target_info.endian = endian_little then
              dec(href.offset,4);
          end;
        cg.a_loadfpu_ref_reg(exprasmlist,_size,href,r);
      end;


    procedure tcgunaryminusnode.pass_2;


      begin
         if is_64bitint(left.resulttype.def) then
           begin
              secondpass(left);

              { load left operator in a register }
              location_copy(location,left.location);
              location_force_reg(exprasmlist,location,OS_64,false);
              cg64.a_op64_loc_reg(exprasmlist,OP_NEG,
                 location,joinreg64(location.registerlow,location.registerhigh));
           end
         else
           begin
              secondpass(left);
              location_reset(location,LOC_REGISTER,OS_INT);
              case left.location.loc of
                 LOC_REGISTER:
                   begin
                      location.register:=left.location.register;
                      cg.a_op_reg_reg(exprasmlist,OP_NEG,OS_INT,location.register,
                         location.register);
                   end;
                 LOC_CREGISTER:
                   begin
                      location.register:=rg.getregisterint(exprasmlist);
                      cg.a_load_reg_reg(exprasmlist,OS_INT,left.location.register,
                        location.register);
                      cg.a_op_reg_reg(exprasmlist,OP_NEG,OS_INT,location.register,
                         location.register);
                   end;
                 LOC_REFERENCE,
                 LOC_CREFERENCE:
                   begin
                      reference_release(exprasmlist,left.location.reference);
                      if (left.resulttype.def.deftype=floatdef) then
                        begin
                           location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
                           location.register:=rg.getregisterfpu(exprasmlist);
                           cg.a_loadfpu_ref_reg(exprasmlist,
                              def_cgsize(left.resulttype.def),
                              left.location.reference,location.register);
                           emit_float_sign_change(location.register,def_cgsize(left.resulttype.def));   
                        end
                      else
                        begin
                           location.register:=rg.getregisterint(exprasmlist);
                           { why is the size is OS_INT, since in pass_1 we convert
                             everything to a signed natural value anyways
                           }  
                           cg.a_load_ref_reg(exprasmlist,OS_INT,
                               left.location.reference,location.register);
                           cg.a_op_reg_reg(exprasmlist,OP_NEG,OS_INT,location.register,
                               location.register);
                        end;
                   end;
                 LOC_FPUREGISTER:
                   begin
                      location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
                      location.register:=left.location.register;
                      emit_float_sign_change(location.register,def_cgsize(left.resulttype.def));   
                   end;
                 LOC_CFPUREGISTER:
                   begin
                      location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
                      location.register:=rg.getregisterfpu(exprasmlist);
                      cg.a_loadfpu_reg_reg(exprasmlist,left.location.register,location.register);
                      emit_float_sign_change(location.register,def_cgsize(left.resulttype.def));   
                   end;
                 else
                    internalerror(200203225);
              end;
           end;
      end;




begin
   cunaryminusnode:=tcgunaryminusnode;
end.
{
  $Log$
  Revision 1.1  2002-08-14 19:26:55  carl
    + generic int_to_real type conversion
    + generic unaryminus node

}
