{
    $Id$
    Copyright (c) 1998-2003 by Florian Klaempfl

    This unit implements the arm specific class for the register
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
       aasmbase,aasmtai,
       cginfo,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         function getexplicitregisterint(list: taasmoutput; reg: Tnewregister): tregister; override;
         procedure ungetregisterint(list: taasmoutput; reg: tregister); override;
         function getexplicitregisterfpu(list : taasmoutput; r : Toldregister) : tregister;override;
         procedure ungetregisterfpu(list: taasmoutput; r : tregister; size:TCGsize);override;
{$ifndef newra}
         procedure saveusedintregisters(list:Taasmoutput;
                                         var saved:Tpushedsavedint;
                                         const s:Tsupregset);override;
         procedure saveusedotherregisters(list:Taasmoutput;
                                           var saved:Tpushedsavedother;
                                           const s:Tregisterset);override;
{$endif newra}
         procedure cleartempgen; override;
        private
         usedpararegs: Tsupregset;
         usedparafpuregs: tregisterset;
       end;

  implementation

    uses
      cgobj, verbose, cutils;

    function trgcpu.getexplicitregisterint(list: taasmoutput; reg: Tnewregister): tregister;

      begin
        if ((reg shr 8) in [RS_R0{$ifndef newra},RS_R2..RS_R12{$endif}]) and
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
        else result := inherited getexplicitregisterint(list,reg);
      end;


    procedure trgcpu.ungetregisterint(list: taasmoutput; reg: tregister);

      begin
        if ((reg.number shr 8) in [RS_R0{$ifndef newra},RS_R2..RS_R12{$endif newra}]) and
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


    function trgcpu.getexplicitregisterfpu(list : taasmoutput; r : Toldregister) : tregister;
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
          result := inherited getexplicitregisterfpu(list,r);
      end;


    procedure trgcpu.ungetregisterfpu(list: taasmoutput; r : tregister; size:TCGsize);
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


{$ifndef newra}
    procedure trgcpu.saveusedintregisters(list:Taasmoutput;
                                         var saved:Tpushedsavedint;
                                         const s:Tsupregset);
      begin
        // saving/restoring is done by the callee (except for registers
        // which already contain parameters, but those aren't allocated
        // correctly yet)
        filldword(saved,sizeof(saved) div 4,reg_not_saved);
      end;


    procedure trgcpu.saveusedotherregisters(list:Taasmoutput;
                                           var saved:Tpushedsavedother;
                                           const s:Tregisterset);
      begin
        // saving/restoring is done by the callee (except for registers
        // which already contain parameters, but those aren't allocated
        // correctly yet)
        filldword(saved,sizeof(saved) div 4,reg_not_saved);
      end;
{$endif newra}


    procedure trgcpu.cleartempgen;

      begin
        inherited cleartempgen;
        usedpararegs := [];
        usedparafpuregs := [];
      end;

initialization
  rg := trgcpu.create(last_supreg-first_supreg+1);
end.

{
  $Log$
  Revision 1.1  2003-08-16 13:23:01  florian
    * several arm related stuff fixed
}
