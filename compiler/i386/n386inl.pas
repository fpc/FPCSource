{
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
unit n386inl;

{$i fpcdefs.inc}

interface

    uses
       node,nx86inl;

    type
       ti386inlinenode = class(tx86inlinenode)
       public
         function first_sar: tnode; override;
         procedure second_rox_sar; override;
       end;

implementation

  uses
    globtype,globals,
    defutil,
    aasmbase,aasmdata,
    cgbase,pass_2,
    cpuinfo,cpubase,
    cga,cgutils,cgx86,cgobj,hlcgobj,
    ninl,ncon,ncal;


  function ti386inlinenode.first_sar: tnode;
    begin
      if is_64bitint(resultdef) and (
        (inlinenumber=in_sar_x) or (
          (inlinenumber=in_sar_x_y) and
          (tcallparanode(left).left.nodetype=ordconstn)
      )) then
        begin
          result:=nil;
          expectloc:=LOC_REGISTER;
        end
      else
        result:=inherited first_sar;
    end;


  procedure ti386inlinenode.second_rox_sar;
    var
      op1: tnode;
      hreg64hi,hreg64lo: tregister;
      v: aint;
    begin
      if is_64bitint(resultdef) and (
        (inlinenumber=in_sar_x) or (
          (inlinenumber=in_sar_x_y) and
          (tcallparanode(left).left.nodetype=ordconstn)
      )) then
        begin
          { x sar constant }
          if (left.nodetype=callparan) and
             assigned(tcallparanode(left).right) then
            begin
              op1:=tcallparanode(tcallparanode(left).right).left;
              secondpass(tcallparanode(left).left);
              v:=Tordconstnode(tcallparanode(left).left).value.svalue and 63;
            end
          else
            begin
              op1:=left;
              v:=1;
            end;
          secondpass(op1);

          location_reset(location,LOC_REGISTER,def_cgsize(resultdef));

          { load left operator in a register }
          hlcg.location_force_reg(current_asmdata.CurrAsmList,op1.location,op1.resultdef,resultdef,false);
          hreg64hi:=op1.location.register64.reghi;
          hreg64lo:=op1.location.register64.reglo;

          if (v=63) then
            begin
              emit_const_reg(A_SAR,S_L,31,hreg64hi);
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,hreg64hi,hreg64lo);
            end
          else if (v>31) then
            begin
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,hreg64hi,hreg64lo);
              emit_const_reg(A_SAR,S_L,31,hreg64hi);
              emit_const_reg(A_SAR,S_L,v and 31,hreg64lo);
            end
          else
            begin
              emit_const_reg_reg(A_SHRD,S_L,v and 31,hreg64hi,hreg64lo);
              emit_const_reg(A_SAR,S_L,v and 31,hreg64hi);
            end;
          location.register64.reghi:=hreg64hi;
          location.register64.reglo:=hreg64lo;
        end
      else
        inherited second_rox_sar;
    end;


begin
   cinlinenode:=ti386inlinenode;
end.
