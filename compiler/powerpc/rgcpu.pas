{
    $Id$
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
       aasmbase,aasmtai,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         function getexplicitregisterint(list: taasmoutput; reg: Tnewregister): tregister; override;
         procedure ungetregisterint(list: taasmoutput; reg: tregister); override;
         function getexplicitregisterfpu(list : taasmoutput; r : Toldregister) : tregister;override;
         procedure ungetregisterfpu(list: taasmoutput; r : tregister);override;
         procedure saveusedintregisters(list:Taasmoutput;
                                         var saved:Tpushedsavedint;
                                         const s:Tsupregset);override;
         procedure saveusedotherregisters(list:Taasmoutput;
                                           var saved:Tpushedsavedother;
                                           const s:Tregisterset);override;
         procedure cleartempgen; override;
        private
         usedpararegs: Tsupregset;
         usedparafpuregs: tregisterset;
       end;

  implementation

    uses
      cgobj, verbose;

    function trgcpu.getexplicitregisterint(list: taasmoutput; reg: Tnewregister): tregister;

      begin
        if ((reg shr 8) in [RS_R0,RS_R2..RS_R12]) and
           not((reg shr 8) in is_reg_var_int) then
          begin
            if (reg shr 8) in usedpararegs then
              internalerror(2003060701);
            include(usedpararegs,reg shr 8);
            result.enum:=R_INTREGISTER;
            result.number:=reg;
            cg.a_reg_alloc(list,result);
          end
        else result := inherited getexplicitregisterint(list,reg);
      end;


    procedure trgcpu.ungetregisterint(list: taasmoutput; reg: tregister);

      begin
        if ((reg.number shr 8) in [RS_R0,RS_R2..RS_R12]) and
            not((reg.number shr 8) in is_reg_var_int) then
          begin
            if not((reg.number shr 8) in usedpararegs) then
              internalerror(2003060702);
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


    procedure trgcpu.ungetregisterfpu(list: taasmoutput; r : tregister);
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
          inherited ungetregisterfpu(list,r);
      end;


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


    procedure trgcpu.cleartempgen;

      begin
        inherited cleartempgen;
        usedpararegs := [];
        usedparafpuregs := [];
      end;

initialization
  rg := trgcpu.create(32);  {PPC has 32 registers.}
end.

{
  $Log$
  Revision 1.9  2003-06-09 14:54:26  jonas
    * (de)allocation of registers for parameters is now performed properly
      (and checked on the ppc)
    - removed obsolete allocation of all parameter registers at the start
      of a procedure (and deallocation at the end)

  Revision 1.8  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.7  2003/05/24 13:38:04  jonas
    * don't save callee-save registers in the caller as well (the ppc code
      that we generate is slow enough as it is without resorting to doing
      double work :)

  Revision 1.6  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.5  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.4  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.3  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled

  Revision 1.2  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.1  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

}
