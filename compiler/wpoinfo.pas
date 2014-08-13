{
    Copyright (c) 2008 by Jonas Maebe

    Whole program optimisation information collection

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

unit wpoinfo;

{$i fpcdefs.inc}

interface

uses
  globtype,cclasses,
  symtype,
  wpobase,
  ppu;

type
  pderefarray = ^tderefarray;
  tderefarray = array[0..1024*1024-1] of tderef;

  tunitwpoinfo = class(tunitwpoinfobase)
   { devirtualisation information -- begin }
   private
    fcreatedobjtypesderefs: pderefarray;
    fcreatedclassrefobjtypesderefs: pderefarray;
    fmaybecreatedbyclassrefdeftypesderefs: pderefarray;
    fcalledvmtentriestemplist: tfpobjectlist;
   { devirtualisation information -- end }

    procedure clearderefinfo;
   public

    destructor destroy; override;

    procedure ppuwrite(ppufile:tcompilerppufile);
    constructor ppuload(ppufile:tcompilerppufile);

    procedure deref;
    procedure derefimpl;
    procedure buildderef;
    procedure buildderefimpl;
  end;


  { twpoinfomanager }

  twpoinfomanager = class(twpoinfomanagerbase)
    function can_be_devirtualized(objdef, procdef: tdef; out name: TSymStr): boolean; override;
    function optimized_name_for_vmt(objdef, procdef: tdef; out name: TSymStr): boolean; override;
    function symbol_live(const name: shortstring): boolean; override;
  end;


implementation

  uses
    globals,
    symdef,
    verbose;

  procedure tunitwpoinfo.clearderefinfo;
    begin
      if assigned(fcreatedobjtypesderefs) then
        begin
          freemem(fcreatedobjtypesderefs);
          fcreatedobjtypesderefs:=nil;
        end;
      if assigned(fcreatedclassrefobjtypesderefs) then
        begin
          freemem(fcreatedclassrefobjtypesderefs);
          fcreatedclassrefobjtypesderefs:=nil;
        end;
      if assigned(fmaybecreatedbyclassrefdeftypesderefs) then
        begin
          freemem(fmaybecreatedbyclassrefdeftypesderefs);
          fmaybecreatedbyclassrefdeftypesderefs:=nil;
        end;

      if assigned(fcalledvmtentriestemplist) then
        begin
          fcalledvmtentriestemplist.free;
          fcalledvmtentriestemplist:=nil;
        end;
    end;

  destructor tunitwpoinfo.destroy;
    begin
      clearderefinfo;
      inherited destroy;
    end;
    
    
  procedure tunitwpoinfo.ppuwrite(ppufile:tcompilerppufile);
    var
      i: longint;
    begin
      { write the number of instantiated object types in this module,
        followed by the derefs of those types
      }
      ppufile.putlongint(fcreatedobjtypes.count);
      for i:=0 to fcreatedobjtypes.count-1 do
        ppufile.putderef(fcreatedobjtypesderefs^[i]);
      ppufile.putlongint(fcreatedclassrefobjtypes.count);
      for i:=0 to fcreatedclassrefobjtypes.count-1 do
        ppufile.putderef(fcreatedclassrefobjtypesderefs^[i]);
      ppufile.putlongint(fmaybecreatedbyclassrefdeftypes.count);
      for i:=0 to fmaybecreatedbyclassrefdeftypes.count-1 do
        ppufile.putderef(fmaybecreatedbyclassrefdeftypesderefs^[i]);

      ppufile.putlongint(fcalledvmtentriestemplist.count);
      for i:=0 to fcalledvmtentriestemplist.count-1 do
        tcalledvmtentries(fcalledvmtentriestemplist[i]).ppuwrite(ppufile);

      ppufile.writeentry(ibcreatedobjtypes);

      { don't free deref arrays immediately after use, as the types may need
        re-resolving in case a unit needs to be reloaded
      }
    end;


  constructor tunitwpoinfo.ppuload(ppufile:tcompilerppufile);
    var
      i, len: longint;
    begin
      { load start of definition section, which holds the amount of defs }
      if ppufile.readentry<>ibcreatedobjtypes then
        message(unit_f_ppu_read_error);

      { don't load the wpo info from the units if we are not generating
        a wpo feedback file (that would just take time and memory)
      }
      if (init_settings.genwpoptimizerswitches=[]) then
        ppufile.skipdata(ppufile.entrysize)
      else
        begin
          len:=ppufile.getlongint;
          fcreatedobjtypes:=tfpobjectlist.create(false);
          fcreatedobjtypes.count:=len;
          getmem(fcreatedobjtypesderefs,len*sizeof(tderef));
          for i:=0 to len-1 do
            ppufile.getderef(fcreatedobjtypesderefs^[i]);

          len:=ppufile.getlongint;
          fcreatedclassrefobjtypes:=tfpobjectlist.create(false);
          fcreatedclassrefobjtypes.count:=len;
          getmem(fcreatedclassrefobjtypesderefs,len*sizeof(tderef));
          for i:=0 to len-1 do
            ppufile.getderef(fcreatedclassrefobjtypesderefs^[i]);

          len:=ppufile.getlongint;
          fmaybecreatedbyclassrefdeftypes:=tfpobjectlist.create(false);
          fmaybecreatedbyclassrefdeftypes.count:=len;
          getmem(fmaybecreatedbyclassrefdeftypesderefs,len*sizeof(tderef));
          for i:=0 to len-1 do
            ppufile.getderef(fmaybecreatedbyclassrefdeftypesderefs^[i]);

          len:=ppufile.getlongint;
          fcalledvmtentriestemplist:=tfpobjectlist.create(false);
          fcalledvmtentriestemplist.count:=len;
          fcalledvmtentries:=tfphashlist.create;
          for i:=0 to len-1 do
            fcalledvmtentriestemplist[i]:=tcalledvmtentries.ppuload(ppufile);
        end;
    end;


  procedure tunitwpoinfo.buildderef;
    var
      i: longint;
    begin
      { ppuload may have already been called before -> deref info structures
        may already have been allocated }
      clearderefinfo;

      getmem(fcreatedobjtypesderefs,fcreatedobjtypes.count*sizeof(tderef));
      for i:=0 to fcreatedobjtypes.count-1 do
        fcreatedobjtypesderefs^[i].build(fcreatedobjtypes[i]);

      getmem(fcreatedclassrefobjtypesderefs,fcreatedclassrefobjtypes.count*sizeof(tderef));
      for i:=0 to fcreatedclassrefobjtypes.count-1 do
        fcreatedclassrefobjtypesderefs^[i].build(fcreatedclassrefobjtypes[i]);

      getmem(fmaybecreatedbyclassrefdeftypesderefs,fmaybecreatedbyclassrefdeftypes.count*sizeof(tderef));
      for i:=0 to fmaybecreatedbyclassrefdeftypes.count-1 do
        fmaybecreatedbyclassrefdeftypesderefs^[i].build(fmaybecreatedbyclassrefdeftypes[i]);

      fcalledvmtentriestemplist:=tfpobjectlist.create(false);
      fcalledvmtentriestemplist.count:=fcalledvmtentries.count;
      for i:=0 to fcalledvmtentries.count-1 do
        begin
          tcalledvmtentries(fcalledvmtentries[i]).buildderef;
          { necessary in case we have unit1 loads unit2, unit2 is recompiled,
            then unit1 derefs unit2 -> in this case we have buildderef for unit2
            -> ppuwrite for unit2 -> deref for unit2 (without a load) -> ensure
            that the fcalledvmtentriestemplist, normally constructed by ppuload,
            is created here as well since deref needs it }
          fcalledvmtentriestemplist[i]:=tobject(fcalledvmtentries[i]);
        end;
    end;


  procedure tunitwpoinfo.buildderefimpl;
    var
      i: longint;
    begin
      for i:=0 to fcalledvmtentriestemplist.count-1 do
        begin
          tcalledvmtentries(fcalledvmtentriestemplist[i]).buildderefimpl;
        end;
    end;


  procedure tunitwpoinfo.deref;
    var
      i: longint;

    begin
      if (init_settings.genwpoptimizerswitches=[]) or
         not assigned(fcalledvmtentriestemplist) then
        exit;

      { don't free deref arrays immediately after use, as the types may need
        re-resolving in case a unit needs to be reloaded
      }
      for i:=0 to fcreatedobjtypes.count-1 do
        fcreatedobjtypes[i]:=fcreatedobjtypesderefs^[i].resolve;

      for i:=0 to fcreatedclassrefobjtypes.count-1 do
        fcreatedclassrefobjtypes[i]:=fcreatedclassrefobjtypesderefs^[i].resolve;

      for i:=0 to fmaybecreatedbyclassrefdeftypes.count-1 do
        fmaybecreatedbyclassrefdeftypes[i]:=fmaybecreatedbyclassrefdeftypesderefs^[i].resolve;

      { in case we are re-resolving, free previous batch }
      if (fcalledvmtentries.count<>0) then
        fcalledvmtentries.clear;
      { allocate enough internal memory in one go }
      fcalledvmtentries.capacity:=fcalledvmtentriestemplist.count;
      { now resolve all items in the list and add them to the hash table }
      for i:=0 to fcalledvmtentriestemplist.count-1 do
        begin
          with tcalledvmtentries(fcalledvmtentriestemplist[i]) do
            begin
              deref;
              fcalledvmtentries.add(tobjectdef(objdef).vmt_mangledname,
                fcalledvmtentriestemplist[i]);
            end;
        end;
    end;


  procedure tunitwpoinfo.derefimpl;
    var
      i: longint;
    begin
      if (init_settings.genwpoptimizerswitches=[]) or
         not assigned(fcalledvmtentriestemplist) then
        exit;

      for i:=0 to fcalledvmtentriestemplist.count-1 do
        begin
          tcalledvmtentries(fcalledvmtentriestemplist[i]).derefimpl;
        end;
    end;


  { twpoinfomanager }

  { devirtualisation }

  function twpoinfomanager.can_be_devirtualized(objdef, procdef: tdef; out name: TSymStr): boolean;
    begin
      if not assigned(wpoinfouse[wpo_devirtualization_context_insensitive]) or
         not(cs_wpo_devirtualize_calls in current_settings.dowpoptimizerswitches) then
        begin
          result:=false;
          exit;
        end;
      result:=twpodevirtualisationhandler(wpoinfouse[wpo_devirtualization_context_insensitive]).staticnameforcallingvirtualmethod(objdef,procdef,name);
    end;


  function twpoinfomanager.optimized_name_for_vmt(objdef, procdef: tdef; out name: TSymStr): boolean;
    begin
      if not assigned(wpoinfouse[wpo_devirtualization_context_insensitive]) or
         not(cs_wpo_optimize_vmts in current_settings.dowpoptimizerswitches) then
        begin
          result:=false;
          exit;
        end;
      result:=twpodevirtualisationhandler(wpoinfouse[wpo_devirtualization_context_insensitive]).staticnameforvmtentry(objdef,procdef,name);
    end;


  { symbol liveness }

  function twpoinfomanager.symbol_live(const name: shortstring): boolean;
    begin
      if not assigned(wpoinfouse[wpo_live_symbol_information]) or
         not(cs_wpo_symbol_liveness in current_settings.dowpoptimizerswitches) then
        begin
          { if we don't know, say that the symbol is live }
          result:=true;
          exit;
        end;
      result:=twpodeadcodehandler(wpoinfouse[wpo_live_symbol_information]).symbolinfinalbinary(name);
    end;


end.
