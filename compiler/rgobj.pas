{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the base class for the register allocator

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

unit rgobj;

  interface

    uses
      cpubase,
      cpuinfo,
      cpuasm,
      tainst,
      cclasses,globtype,cginfo,cgbase,aasm,node;

    type
       regvar_longintarray = array[firstreg..lastreg] of longint;
       regvar_booleanarray = array[firstreg..lastreg] of boolean;
       regvar_ptreearray = array[firstreg..lastreg] of tnode;

       tpushedsavedloc = record
         case byte of
           0: (pushed: boolean);
           1: (ofs: longint);
       end;

       tpushedsaved = array[firstreg..lastreg] of tpushedsavedloc;

       trgobj = class
          { The "usableregsxxx" contain all registers of type "xxx" that }
          { aren't currently allocated to a regvar. The "unusedregsxxx"  }
          { contain all registers of type "xxx" that aren't currenly     }
          { allocated                                                    }
          unusedregsint,usableregsint : tregisterset;
          unusedregsfpu,usableregsfpu : tregisterset;
          unusedregsmm,usableregsmm : tregisterset;
          { these counters contain the number of elements in the }
          { unusedregsxxx/usableregsxxx sets                     }
          countunusedregsint,
          countunusedregsfpu,
          countunusedregsmm : byte;
          countusableregsint,
          countusableregsfpu,
          countusableregsmm : byte;

          usedinproc : tregisterset;

          reg_pushes : regvar_longintarray;
          is_reg_var : regvar_booleanarray;
          regvar_loaded: regvar_booleanarray;


          { tries to hold the amount of times which the current tree is processed  }
          t_times: longint;

          constructor create;

          function getregisterint(list: taasmoutput) : tregister; virtual;
          procedure ungetregisterint(list: taasmoutput; r : tregister); virtual;

          function getregisterfpu(list: taasmoutput) : tregister; virtual;
          procedure ungetregisterfpu(list: taasmoutput; r : tregister); virtual;

          function getregistermm(list: taasmoutput) : tregister; virtual;
          procedure ungetregistermm(list: taasmoutput; r : tregister); virtual;

          function getaddressregister(list: taasmoutput): tregister; virtual;
          procedure ungetaddressregister(list: taasmoutput; r: tregister); virtual;
          { the following must only be called for address and integer }
          { registers, otherwise the result is undefined              }
          function isaddressregister(reg: tregister): boolean; virtual;

          {# tries to allocate the passed register, if possible }
          function getexplicitregisterint(list: taasmoutput; r : tregister) : tregister;virtual;

          {# deallocate any kind of register }
          procedure ungetregister(list: taasmoutput; r : tregister); virtual;

          {# deallocate any kind of register }
          procedure ungetreference(list: taasmoutput; const ref : treference); virtual;

          {# reset the register allocator information (usable registers etc) }
          procedure cleartempgen;virtual;
          
          {# convert a register to a specified register size, and return that register size }
          function makeregsize(reg: tregister; size: tcgsize): tregister; virtual;
          

          { saves register variables (restoring happens automatically) }
          procedure saveregvars(list: taasmoutput; const s: tregisterset);

          { saves and restores used registers }
          procedure saveusedregisters(list: taasmoutput;
            var saved : tpushedsaved;const s: tregisterset);virtual;
          procedure restoreusedregisters(list: taasmoutput;
            const saved : tpushedsaved);virtual;

          { used when deciding which registers to use for regvars }
          procedure incrementregisterpushed(const s: tregisterset);
          procedure clearregistercount;
          procedure resetusableregisters;virtual;

          procedure makeregvar(reg: tregister);

          procedure saveStateForInline(var state: pointer);
          procedure restoreStateAfterInline(var state: pointer);

          procedure saveUnusedState(var state: pointer);
          procedure restoreUnusedState(var state: pointer);
       protected
          { the following two contain the common (generic) code for all }
          { get- and ungetregisterxxx functions/procedures              }
          function getregistergen(list: taasmoutput; const lowreg, highreg: tregister;
              var unusedregs: tregisterset; var countunusedregs: byte): tregister;
          procedure ungetregistergen(list: taasmoutput; const r: tregister;
              const usableregs: tregisterset; var unusedregs: tregisterset; var countunusedregs: byte);
{$ifdef TEMPREGDEBUG}
         reg_user   : regvar_ptreearray;
         reg_releaser : regvar_ptreearray;
{$endif TEMPREGDEBUG}

{$ifdef TEMPREGDEBUG}
          procedure testregisters;
{$endif TEMPREGDEBUGx}
       end;

     const
       { this value is used in tsaved, if the register isn't saved }
       reg_not_saved = $7fffffff;

     var
       rg: trgobj;

     { trerefence handling }
     procedure reference_reset(var ref : treference);
     procedure reference_reset_base(var ref : treference;base : tregister;offset : longint);
     procedure reference_reset_symbol(var ref : treference;sym : tasmsymbol;offset : longint);
     procedure reference_release(list: taasmoutput; const ref : treference);

     { tlocation handling }
     procedure location_reset(var l : tlocation;lt:TLoc;lsize:TCGSize);
     procedure location_release(list: taasmoutput; const l : tlocation);
     procedure location_freetemp(list: taasmoutput; const l : tlocation);
     procedure location_copy(var destloc,sourceloc : tlocation);
     procedure location_swap(var destloc,sourceloc : tlocation);


  implementation

    uses
       systems,
       globals,verbose,
       cgobj,tgobj,regvars;

    type
      psavedstate = ^tsavedstate;
      tsavedstate = record
        unusedregsint,usableregsint : tregisterset;
        unusedregsfpu,usableregsfpu : tregisterset;
        unusedregsmm,usableregsmm : tregisterset;
        countunusedregsint,
        countunusedregsfpu,
        countunusedregsmm : byte;
        countusableregsint,
        countusableregsfpu,
        countusableregsmm : byte;
        usedinproc : tregisterset;
        reg_pushes : regvar_longintarray;
        is_reg_var : regvar_booleanarray;
        regvar_loaded: regvar_booleanarray;
{$ifdef TEMPREGDEBUG}
         reg_user   : regvar_ptreearray;
         reg_releaser : regvar_ptreearray;
{$endif TEMPREGDEBUG}
      end;


      punusedstate = ^tunusedstate;
      tunusedstate = record
        unusedregsint : tregisterset;
        unusedregsfpu : tregisterset;
        unusedregsmm : tregisterset;
        countunusedregsint,
        countunusedregsfpu,
        countunusedregsmm : byte;
      end;


    constructor trgobj.create;

     begin
       usedinproc := [];
       t_times := 0;
       resetusableregisters;
{$ifdef TEMPREGDEBUG}
       fillchar(reg_user,sizeof(reg_user),0);
       fillchar(reg_releaser,sizeof(reg_releaser),0);
{$endif TEMPREGDEBUG}
     end;


    function trgobj.getregistergen(list: taasmoutput; const lowreg, highreg: tregister;
        var unusedregs: tregisterset; var countunusedregs: byte): tregister;
      var
        i: tregister;
      begin
         for i:=lowreg to highreg do
           begin
              if i in unusedregs then
                begin
                   exclude(unusedregs,i);
                   include(usedinproc,i);
                   dec(countunusedregs);
                   list.concat(tairegalloc.alloc(i));
                   result := i;
                   exit;
                end;
           end;
         internalerror(10);
      end;


    procedure trgobj.ungetregistergen(list: taasmoutput; const r: tregister;
        const usableregs: tregisterset; var unusedregs: tregisterset; var countunusedregs: byte);
      begin
         { takes much time }
         if not(r in usableregs) then
           exit;
{$ifdef TEMPREGDEBUG}
         if (r in unusedregs) then
{$ifdef EXTTEMPREGDEBUG}
           begin
             Comment(V_Debug,'register freed twice '+std_reg2str[r]);
             testregisters32;
             exit;
           end
{$else EXTTEMPREGDEBUG}
           exit
{$endif EXTTEMPREGDEBUG}
         else
{$endif TEMPREGDEBUG}
          inc(countunusedregs);
        include(unusedregs,r);
        list.concat(tairegalloc.dealloc(r));
      end;


    function trgobj.getregisterint(list : taasmoutput) : tregister;

      begin
         if countunusedregsint=0 then
           internalerror(10);
{$ifdef TEMPREGDEBUG}
         if curptree^^.usableregs-countunusedregsint>curptree^^.registers32 then
           internalerror(10);
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
         if curptree^^.usableregs-countunusedregsint>curptree^^.reallyusedregs then
           curptree^^.reallyusedregs:=curptree^^.usableregs-countunusedregsint;
{$endif EXTTEMPREGDEBUG}
         result := getregistergen(list,firstsaveintreg,lastsaveintreg,
                     unusedregsint,countunusedregsint);
{$ifdef TEMPREGDEBUG}
         reg_user[result]:=curptree^;
         testregisters32;
{$endif TEMPREGDEBUG}
      end;


    procedure trgobj.ungetregisterint(list : taasmoutput; r : tregister);

      begin
         ungetregistergen(list,r,usableregsint,unusedregsint,
           countunusedregsint);
{$ifdef TEMPREGDEBUG}
        reg_releaser[r]:=curptree^;
        testregisters32;
{$endif TEMPREGDEBUG}
      end;


    { tries to allocate the passed register, if possible }
    function trgobj.getexplicitregisterint(list : taasmoutput; r : tregister) : tregister;

      begin
         if r in unusedregsint then
           begin
              dec(countunusedregsint);
{$ifdef TEMPREGDEBUG}
              if curptree^^.usableregs-countunusedregsint>curptree^^.registers32 then
                internalerror(10);
              reg_user[r]:=curptree^;
{$endif TEMPREGDEBUG}
              exclude(unusedregsint,r);
              include(usedinproc,r);
              list.concat(tairegalloc.alloc(r));
              getexplicitregisterint:=r;
{$ifdef TEMPREGDEBUG}
              testregisters32;
{$endif TEMPREGDEBUG}
           end
         else
           getexplicitregisterint:=getregisterint(list);
      end;


    function trgobj.getregisterfpu(list: taasmoutput) : tregister;
      begin
        if countunusedregsfpu=0 then
           internalerror(10);
       result := getregistergen(list,firstsavefpureg,lastsavefpureg,
                   unusedregsfpu,countunusedregsfpu);
      end;


    procedure trgobj.ungetregisterfpu(list : taasmoutput; r : tregister);

      begin
         ungetregistergen(list,r,usableregsfpu,unusedregsfpu,
           countunusedregsfpu);
      end;


    function trgobj.getregistermm(list: taasmoutput) : tregister;
      begin
        if countunusedregsmm=0 then
           internalerror(10);
       result := getregistergen(list,firstsavemmreg,lastsavemmreg,
                   unusedregsmm,countunusedregsmm);
      end;


    procedure trgobj.ungetregistermm(list: taasmoutput; r: tregister);
      begin
       ungetregistergen(list,r,usableregsmm,unusedregsmm,
         countunusedregsmm);
      end;


    function trgobj.getaddressregister(list: taasmoutput): tregister;
      begin
        result := getregisterint(list);
      end;


    procedure trgobj.ungetaddressregister(list: taasmoutput; r: tregister);
      begin
        ungetregisterint(list,r);
      end;


    function trgobj.isaddressregister(reg: tregister): boolean;
      begin
        result := true;
      end;


    procedure trgobj.ungetregister(list: taasmoutput; r : tregister);

      begin
         if r in intregs then
           ungetregisterint(list,r)
         else if r in fpuregs then
           ungetregisterfpu(list,r)
         else if r in mmregs then
           ungetregistermm(list,r)
         else internalerror(18);
      end;


    procedure trgobj.cleartempgen;

      begin
         countunusedregsint:=countusableregsint;
         countunusedregsfpu:=countusableregsfpu;
         countunusedregsmm:=countusableregsmm;
         unusedregsint:=usableregsint;
         unusedregsfpu:=usableregsfpu;
         unusedregsmm:=usableregsmm;
      end;


    procedure trgobj.ungetreference(list : taasmoutput; const ref : treference);

      begin
         ungetregister(list,ref.base);
         ungetregister(list,ref.index);
      end;


    procedure trgobj.saveregvars(list: taasmoutput; const s: tregisterset);
      var
        r: tregister;
      begin
        if not(cs_regalloc in aktglobalswitches) then
          exit;
        for r := firstsaveintreg to lastsaveintreg do
          if is_reg_var[r] and
             (r in s) then
            store_regvar(list,r);
        if firstsavefpureg <> R_NO then
          for r := firstsavefpureg to lastsavefpureg do
            if is_reg_var[r] and
               (r in s) then
              store_regvar(list,r);
        if firstsavemmreg <> R_NO then
          for r := firstsavemmreg to lastsavemmreg do
            if is_reg_var[r] and
               (r in s) then
              store_regvar(list,r);
      end;


    procedure trgobj.saveusedregisters(list: taasmoutput;
        var saved : tpushedsaved; const s: tregisterset);

      var
         r : tregister;
         hr : treference;

      begin
        usedinproc:=usedinproc + s;
        for r:=firstsaveintreg to lastsaveintreg do
          begin
            saved[r].ofs:=reg_not_saved;
            { if the register is used by the calling subroutine and if }
            { it's not a regvar (those are handled separately)         }
            if not is_reg_var[r] and
               (r in s) and
               { and is present in use }
               not(r in unusedregsint) then
              begin
                { then save it }
                tg.gettempofsizereferencepersistant(list,sizeof(aword),hr);
                saved[r].ofs:=hr.offset;
                cg.a_load_reg_ref(list,OS_INT,r,hr);
                cg.a_reg_dealloc(list,r);
                include(unusedregsint,r);
                inc(countunusedregsint);
              end;
          end;

        { don't try to save the fpu registers if not desired (e.g. for }
        { the 80x86)                                                   }
        if firstsavefpureg <> R_NO then
          for r:=firstsavefpureg to lastsavefpureg do
            begin
              saved[r].ofs:=reg_not_saved;
              { if the register is used by the calling subroutine and if }
              { it's not a regvar (those are handled separately)         }
              if not is_reg_var[r] and
                 (r in s) and
                 { and is present in use }
                 not(r in unusedregsfpu) then
                begin
                  { then save it }
                  tg.gettempofsizereferencepersistant(list,extended_size,hr);
                  saved[r].ofs:=hr.offset;
                  cg.a_loadfpu_reg_ref(list,OS_FLOAT,r,hr);
                  cg.a_reg_dealloc(list,r);
                  include(unusedregsfpu,r);
                  inc(countunusedregsfpu);
                end;
            end;

        { don't save the vector registers if there's no support for them }
        if firstsavemmreg <> R_NO then
          for r:=firstsavemmreg to lastsavemmreg do
            begin
              saved[r].ofs:=reg_not_saved;
              { if the register is in use and if it's not a regvar (those }
              { are handled separately), save it                          }
              if not is_reg_var[r] and
                 (r in s) and
                 { and is present in use }
                 not(r in unusedregsmm) then
                begin
                  { then save it }
                  tg.gettempofsizereferencepersistant(list,mmreg_size,hr);
                  saved[r].ofs:=hr.offset;
                  cg.a_loadmm_reg_ref(list,r,hr);
                  cg.a_reg_dealloc(list,r);
                  include(unusedregsmm,r);
                  inc(countunusedregsmm);
               end;
            end;
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;


    procedure trgobj.restoreusedregisters(list : taasmoutput;
        const saved : tpushedsaved);

      var
         r : tregister;
         hr : treference;

      begin
        if firstsavemmreg <> R_NO then
          for r:=lastsavemmreg downto firstsavemmreg do
            begin
              if saved[r].ofs <> reg_not_saved then
                begin
                  reference_reset_base(hr,FRAME_POINTER_REG,saved[r].ofs);
                  cg.a_reg_alloc(list,r);
                  cg.a_loadmm_ref_reg(list,hr,r);
                  if not (r in unusedregsmm) then
                    { internalerror(10)
                      in n386cal we always save/restore the reg *state*
                      using save/restoreunusedstate -> the current state
                      may not be real (JM) }
                  else
                    begin
                      dec(countunusedregsmm);
                      exclude(unusedregsmm,r);
                    end;
                  tg.ungetpersistanttemp(list,hr.offset);
                end;
            end;

        if firstsavefpureg <> R_NO then
          for r:=lastsavefpureg downto firstsavefpureg do
            begin
              if saved[r].ofs <> reg_not_saved then
                begin
                  reference_reset_base(hr,FRAME_POINTER_REG,saved[r].ofs);
                  cg.a_reg_alloc(list,r);
                  cg.a_loadfpu_ref_reg(list,OS_FLOAT,hr,r);
                  if not (r in unusedregsfpu) then
                    { internalerror(10)
                      in n386cal we always save/restore the reg *state*
                      using save/restoreunusedstate -> the current state
                      may not be real (JM) }
                  else
                    begin
                      dec(countunusedregsfpu);
                      exclude(unusedregsfpu,r);
                    end;
                  tg.ungetpersistanttemp(list,hr.offset);
                end;
            end;

        for r:=lastsaveintreg downto firstsaveintreg do
          begin
            if saved[r].ofs <> reg_not_saved then
              begin
                reference_reset_base(hr,FRAME_POINTER_REG,saved[r].ofs);
                cg.a_reg_alloc(list,r);
                cg.a_load_ref_reg(list,OS_INT,hr,r);
                if not (r in unusedregsint) then
                  { internalerror(10)
                    in n386cal we always save/restore the reg *state*
                    using save/restoreunusedstate -> the current state
                    may not be real (JM) }
                else
                  begin
                    dec(countunusedregsint);
                    exclude(unusedregsint,r);
                  end;
                tg.ungetpersistanttemp(list,hr.offset);
              end;
          end;
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;


    procedure trgobj.incrementregisterpushed(const s: tregisterset);

      var
         regi : tregister;

      begin
         for regi:=firstsaveintreg to lastsaveintreg do
           begin
              if (regi in s) then
                inc(reg_pushes[regi],t_times*2);
           end;
         if firstsavefpureg <> R_NO then
           for regi:=firstsavefpureg to lastsavefpureg do
             begin
                if (regi in s) then
                  inc(reg_pushes[regi],t_times*2);
             end;
         if firstsavemmreg <> R_NO then
           for regi:=firstsavemmreg to lastsavemmreg do
             begin
                if (regi in s) then
                  inc(reg_pushes[regi],t_times*2);
             end;
      end;


    procedure trgobj.clearregistercount;

      begin
        fillchar(reg_pushes,sizeof(reg_pushes),0);
        fillchar(is_reg_var,sizeof(is_reg_var),false);
        fillchar(regvar_loaded,sizeof(regvar_loaded),false);
      end;


    procedure trgobj.resetusableregisters;

      begin
        { initialize fields with constant values from cpubase }
        countusableregsint := cpubase.c_countusableregsint;
        countusableregsfpu := cpubase.c_countusableregsfpu;
        countusableregsmm := cpubase.c_countusableregsmm;
        usableregsint := cpubase.usableregsint;
        usableregsfpu := cpubase.usableregsfpu;
        usableregsmm := cpubase.usableregsmm;
        clearregistercount;
      end;


    procedure trgobj.makeregvar(reg: tregister);
      begin
        if reg in intregs then
          begin
            dec(countusableregsint);
            dec(countunusedregsint);
            exclude(usableregsint,reg);
            exclude(unusedregsint,reg);
          end
        else if reg in fpuregs then
          begin
             dec(countusableregsfpu);
             dec(countunusedregsfpu);
             exclude(usableregsfpu,reg);
             exclude(unusedregsfpu,reg);
          end
        else if reg in mmregs then
          begin
             dec(countusableregsmm);
             dec(countunusedregsmm);
             exclude(usableregsmm,reg);
             exclude(unusedregsmm,reg);
          end;
        is_reg_var[reg]:=true;
      end;


{$ifdef TEMPREGDEBUG}
    procedure trgobj.testregisters;
      var
        r: tregister;
        test : byte;
      begin
        test:=0;
        for r := firstsaveintreg to lastsaveintreg do
          inc(test,ord(r in unusedregsint));
        if test<>countunusedregsint then
          internalerror(10);
      end;
{$endif TEMPREGDEBUG}


    procedure trgobj.saveStateForInline(var state: pointer);
      begin
        new(psavedstate(state));
        psavedstate(state)^.unusedregsint := unusedregsint;
        psavedstate(state)^.usableregsint := usableregsint;
        psavedstate(state)^.unusedregsfpu := unusedregsfpu;
        psavedstate(state)^.usableregsfpu := usableregsfpu;
        psavedstate(state)^.unusedregsmm := unusedregsmm;
        psavedstate(state)^.usableregsmm := usableregsmm;
        psavedstate(state)^.countunusedregsint := countunusedregsint;
        psavedstate(state)^.countunusedregsfpu := countunusedregsfpu;
        psavedstate(state)^.countunusedregsmm := countunusedregsmm;
        psavedstate(state)^.countusableregsint := countusableregsint;
        psavedstate(state)^.countusableregsfpu := countusableregsfpu;
        psavedstate(state)^.countusableregsmm := countusableregsmm;
        psavedstate(state)^.usedinproc := usedinproc;
        psavedstate(state)^.reg_pushes := reg_pushes;
        psavedstate(state)^.is_reg_var := is_reg_var;
        psavedstate(state)^.regvar_loaded := regvar_loaded;
{$ifdef TEMPREGDEBUG}
        psavedstate(state)^.reg_user := reg_user;
        psavedstate(state)^.reg_releaser := reg_releaser;
{$endif TEMPREGDEBUG}
      end;


    procedure trgobj.restoreStateAfterInline(var state: pointer);
      begin
        unusedregsint := psavedstate(state)^.unusedregsint;
        usableregsint := psavedstate(state)^.usableregsint;
        unusedregsfpu := psavedstate(state)^.unusedregsfpu;
        usableregsfpu := psavedstate(state)^.usableregsfpu;
        unusedregsmm := psavedstate(state)^.unusedregsmm;
        usableregsmm := psavedstate(state)^.usableregsmm;
        countunusedregsint := psavedstate(state)^.countunusedregsint;
        countunusedregsfpu := psavedstate(state)^.countunusedregsfpu;
        countunusedregsmm := psavedstate(state)^.countunusedregsmm;
        countusableregsint := psavedstate(state)^.countusableregsint;
        countusableregsfpu := psavedstate(state)^.countusableregsfpu;
        countusableregsmm := psavedstate(state)^.countusableregsmm;
        usedinproc := psavedstate(state)^.usedinproc;
        reg_pushes := psavedstate(state)^.reg_pushes;
        is_reg_var := psavedstate(state)^.is_reg_var;
        regvar_loaded := psavedstate(state)^.regvar_loaded;
{$ifdef TEMPREGDEBUG}
        reg_user := psavedstate(state)^.reg_user;
        reg_releaser := psavedstate(state)^.reg_releaser;
{$endif TEMPREGDEBUG}
        dispose(psavedstate(state));
        state := nil;
      end;


    procedure trgobj.saveUnusedState(var state: pointer);
      begin
        new(punusedstate(state));
        punusedstate(state)^.unusedregsint := unusedregsint;
        punusedstate(state)^.unusedregsfpu := unusedregsfpu;
        punusedstate(state)^.unusedregsmm := unusedregsmm;
        punusedstate(state)^.countunusedregsint := countunusedregsint;
        punusedstate(state)^.countunusedregsfpu := countunusedregsfpu;
        punusedstate(state)^.countunusedregsmm := countunusedregsmm;
      end;


    procedure trgobj.restoreUnusedState(var state: pointer);
      begin
        unusedregsint := punusedstate(state)^.unusedregsint;
        unusedregsfpu := punusedstate(state)^.unusedregsfpu;
        unusedregsmm := punusedstate(state)^.unusedregsmm;
        countunusedregsint := punusedstate(state)^.countunusedregsint;
        countunusedregsfpu := punusedstate(state)^.countunusedregsfpu;
        countunusedregsmm := punusedstate(state)^.countunusedregsmm;
        dispose(punusedstate(state));
        state := nil;
      end;


{****************************************************************************
                                  TReference
****************************************************************************}

    procedure reference_reset(var ref : treference);
      begin
        FillChar(ref,sizeof(treference),0);
      end;


    procedure reference_reset_base(var ref : treference;base : tregister;offset : longint);
      begin
        FillChar(ref,sizeof(treference),0);
        ref.base:=base;
        ref.offset:=offset;
      end;


    procedure reference_reset_symbol(var ref : treference;sym : tasmsymbol;offset : longint);
          begin
        FillChar(ref,sizeof(treference),0);
        ref.symbol:=sym;
        ref.offset:=offset;
      end;


    procedure reference_release(list: taasmoutput; const ref : treference);
      begin
        rg.ungetreference(list,ref);
      end;
      
 { on most processors , this routine does nothing, overriden currently  }
 { only by 80x86 processor.                                             }
 function trgobj.makeregsize(reg: tregister; size: tcgsize): tregister; 
   begin
     makeregsize := reg;
   end;
      


{****************************************************************************
                                  TLocation
****************************************************************************}

    procedure location_reset(var l : tlocation;lt:TLoc;lsize:TCGSize);
      begin
        FillChar(l,sizeof(tlocation),0);
        l.loc:=lt;
        l.size:=lsize;
      end;


    procedure location_release(list: taasmoutput; const l : tlocation);
      begin
        case l.loc of
          LOC_REGISTER,LOC_CREGISTER :
            begin
              rg.ungetregisterint(list,l.register);
              if l.size in [OS_64,OS_S64] then
               rg.ungetregisterint(list,l.registerhigh);
            end;
          LOC_CREFERENCE,LOC_REFERENCE :
            rg.ungetreference(list, l.reference);
        end;
      end;


    procedure location_freetemp(list:taasmoutput; const l : tlocation);
      begin
        if (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
         tg.ungetiftemp(list,l.reference);
      end;


    procedure location_copy(var destloc,sourceloc : tlocation);
      begin
        destloc:=sourceloc;
      end;


    procedure location_swap(var destloc,sourceloc : tlocation);
      var
        swapl : tlocation;
      begin
        swapl := destloc;
        destloc := sourceloc;
        sourceloc := swapl;
      end;



finalization
  rg.free;
end.

{
  $Log$
  Revision 1.10  2002-05-16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.8  2002/04/21 15:23:03  carl
  + makeregsize
  + changeregsize is now a local routine

  Revision 1.7  2002/04/20 21:32:25  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.6  2002/04/15 19:03:31  carl
  + reg2str -> std_reg2str()

  Revision 1.5  2002/04/06 18:13:01  jonas
    * several powerpc-related additions and fixes

  Revision 1.4  2002/04/04 19:06:04  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.3  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.2  2002/04/01 19:24:25  jonas
    * fixed different parameter name in interface and implementation
      declaration of a method (only 1.0.x detected this)

  Revision 1.1  2002/03/31 20:26:36  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}
