{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the powerpc specific class for the register
    allocator

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

unit rgcpu;

{$i fpcdefs.inc}

  interface

     uses
       aasmbase,aasmtai,aasmdata,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
{
         function getcpuregisterint(list: TAsmList; reg: Tnewregister): tregister; override;
         procedure ungetregisterint(list: TAsmList; reg: tregister); override;
         function getcpuregisterfpu(list : TAsmList; r : Toldregister) : tregister;override;
         procedure ungetregisterfpu(list: TAsmList; r : tregister; size:TCGsize);override;
         procedure cleartempgen; override;
        private
         usedpararegs: Tsupregset;
         usedparafpuregs: tregisterset;
}
       end;

  implementation

    uses
      cgobj, verbose, cutils;

(*
    function trgcpu.getcpuregisterint(list: TAsmList; reg: Tnewregister): tregister;

      begin
        if ((reg shr 8) in [RS_R0]) and
           not((reg shr 8) in is_reg_var_int) then
          begin
            if (reg shr 8) in usedpararegs then
              internalerror(2003060701);
{              comment(v_warning,'Double allocation of register '+tostr((reg shr 8)-1));}
            include(usedpararegs,reg shr 8);
            result.enum:=R_INTREGISTER;
            result.number:=reg;
            cg.a_reg_alloc(list,result);
          end
        else result := inherited getcpuregisterint(list,reg);
      end;


    procedure trgcpu.ungetregisterint(list: TAsmList; reg: tregister);

      begin
        if ((reg.number shr 8) in [RS_R0]) and
            not((reg.number shr 8) in is_reg_var_int) then
          begin
            if not((reg.number shr 8) in usedpararegs) then
              internalerror(2003060702);
{               comment(v_warning,'Double free of register '+tostr((reg.number shr 8)-1));}
            exclude(usedpararegs,reg.number shr 8);
            cg.a_reg_dealloc(list,reg);
          end
        else
          inherited ungetregisterint(list,reg);
      end;


    function trgcpu.getcpuregisterfpu(list : TAsmList; r : Toldregister) : tregister;
      begin
        if (r in [R_F1..R_F13]) and
           not is_reg_var_other[r] then
          begin
            if r in usedparafpuregs then
              internalerror(2003060902);
            include(usedparafpuregs,r);
            result.enum := r;
            cg.a_reg_alloc(list,result);
          end
        else
          result := inherited getcpuregisterfpu(list,r);
      end;


    procedure trgcpu.ungetregisterfpu(list: TAsmList; r : tregister; size:TCGsize);
      begin
        if (r.enum in [R_F1..R_F13]) and
           not is_reg_var_other[r.enum] then
          begin
            if not(r.enum in usedparafpuregs) then
              internalerror(2003060903);
            exclude(usedparafpuregs,r.enum);
            cg.a_reg_dealloc(list,r);
          end
        else
          inherited ungetregisterfpu(list,r,size);
      end;


    procedure trgcpu.cleartempgen;

      begin
        inherited cleartempgen;
        usedpararegs := [];
        usedparafpuregs := [];
      end;
*)

end.
