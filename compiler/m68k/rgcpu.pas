{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the register allocator for m68k

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

{$i fpcdefs.inc}
unit rgcpu;

{$i fpcdefs.inc}

  interface

     uses
       aasmbase,aasmtai,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
          unusedregsaddr,usableregsaddr:Tsupregset;
          countunusedregsaddr,
          countusableregsaddr : byte;
          procedure saveStateForInline(var state: pointer);override;
          procedure restoreStateAfterInline(var state: pointer);override;
          procedure saveUnusedState(var state: pointer);override;
          procedure restoreUnusedState(var state: pointer);override;
          function isaddressregister(reg: tregister): boolean; override;
          function getaddressregister(list: taasmoutput): tregister; override;
          procedure ungetaddressregister(list: taasmoutput; r: tregister); override;
          procedure resetusableregisters;override;
          procedure restoreusedintregisters(list:Taasmoutput;
                                            const saved:Tpushedsavedint);override;
          procedure saveusedintregisters(list:Taasmoutput;
                                         var saved:Tpushedsavedint;
                                         const s:Tsupregset);override;
          procedure cleartempgen;override;

       end;

  implementation

    uses
      cgobj,tgobj,cpuinfo;

     procedure trgcpu.ungetaddressregister(list: taasmoutput; r: tregister);
       begin
         ungetregistergenint(list,r,usableregsaddr,unusedregsaddr,
           countunusedregsaddr);
       end;


    function trgcpu.getaddressregister(list: taasmoutput): tregister;

    begin
      result:=getregistergenint(list,
                                R_SUBWHOLE,
                                firstsaveaddrreg,
                                lastsaveaddrreg,
                                usedintbyproc,
                                usedintinproc,
                                unusedregsint,
                                countunusedregsint);
 
    end;
     
    function trgcpu.isaddressregister(reg: tregister): boolean;
    
    begin
      isaddressregister := reg.enum in addrregs;
    end;


    procedure trgcpu.resetusableregisters;

      begin
        inherited resetusableregisters;
        { initialize fields with constant values from cpubase }
        countusableregsaddr := cpubase.c_countusableregsaddr;
        usableregsaddr := cpubase.usableregsaddr;
      end;


    procedure Trgcpu.restoreusedintregisters(list:Taasmoutput;
                                             const saved:Tpushedsavedint);
    var r:Tsuperregister;
        r2,r3:Tregister;
        hr:Treference;

    begin
      inherited restoreusedintregisters(list, saved);

      for r:=lastsaveaddrreg downto firstsaveaddrreg do
        begin
          if saved[r].ofs<>reg_not_saved then
            begin
              r2.enum:=R_INTREGISTER;
              r2.number:=NR_FRAME_POINTER_REG;
              reference_reset_base(hr,r2,saved[r].ofs);
              r3.enum:=R_INTREGISTER;
              r3.number:=r shl 8 or R_SUBWHOLE;
              cg.a_reg_alloc(list,r3);
              cg.a_load_ref_reg(list,OS_ADDR,hr,r3);
              if not (r in unusedregsaddr) then
                { internalerror(10)
                  in n386cal we always save/restore the reg *state*
                  using save/restoreunusedstate -> the current state
                  may not be real (JM) }
              else
                begin
                  dec(countunusedregsaddr);
                  exclude(unusedregsaddr,r);
                end;
              tg.ungettemp(list,hr);
            end;
        end;
    end;


    procedure Trgcpu.saveusedintregisters(list:Taasmoutput;
                                          var saved:Tpushedsavedint;
                                          const s:Tsupregset);
    var r:Tsuperregister;
        r2:Tregister;
        hr:Treference;

    begin
      inherited saveusedintregisters(list,saved,s);
      for r:=firstsaveaddrreg to lastsaveaddrreg do
        begin
          saved[r].ofs:=reg_not_saved;
          { if the register is used by the calling subroutine and if }
          { it's not a regvar (those are handled separately)         }
          if not(r in is_reg_var_int) and (r in s) and
               { and is present in use }
               not(r in unusedregsaddr) then
            begin
              { then save it }
              tg.gettemp(list,pointer_size,tt_persistant,hr);
              saved[r].ofs:=hr.offset;
              r2.enum:=R_INTREGISTER;
              r2.number:=r shl 8 or R_SUBWHOLE;
              cg.a_load_reg_ref(list,OS_ADDR,r2,hr);
              cg.a_reg_dealloc(list,r2);
              include(unusedregsaddr,r);
              inc(countunusedregsaddr);
            end;
        end;
    end;



    procedure trgcpu.saveStateForInline(var state: pointer);
      begin
        inherited savestateforinline(state);
        psavedstate(state)^.unusedregsaddr := unusedregsaddr;
        psavedstate(state)^.usableregsaddr := usableregsaddr;
        psavedstate(state)^.countunusedregsaddr := countunusedregsaddr;
      end;


    procedure trgcpu.restoreStateAfterInline(var state: pointer);
      begin
        unusedregsaddr := psavedstate(state)^.unusedregsaddr;
        usableregsaddr := psavedstate(state)^.usableregsaddr;
        countunusedregsaddr := psavedstate(state)^.countunusedregsaddr;
        inherited restoreStateAfterInline(state);
      end;


    procedure trgcpu.saveUnusedState(var state: pointer);
      begin
        inherited saveUnusedState(state);
        punusedstate(state)^.unusedregsaddr := unusedregsaddr;
        punusedstate(state)^.countunusedregsaddr := countunusedregsaddr;
      end;


    procedure trgcpu.restoreUnusedState(var state: pointer);
      begin
        unusedregsaddr := punusedstate(state)^.unusedregsaddr;
        countunusedregsaddr := punusedstate(state)^.countunusedregsaddr;
        inherited restoreUnusedState(state);
      end;

    procedure trgcpu.cleartempgen;

      begin
         inherited cleartempgen;
         countunusedregsaddr:=countusableregsaddr;
         unusedregsaddr:=usableregsaddr;
      end;


initialization
  rg := trgcpu.create(16);
end.

{
  $Log$
  Revision 1.8  2003-04-22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.7  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.6  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.5  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.4  2002/09/07 15:25:14  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/08/23 16:14:50  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.2  2002/08/12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.1  2002/08/05 17:26:09  carl
    + updated m68k

}
