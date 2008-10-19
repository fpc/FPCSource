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
  cclasses,
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
   { devirtualisation information -- end }

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
    function can_be_devirtualized(objdef, procdef: tdef; out name: shortstring): boolean; override;
    function optimized_name_for_vmt(objdef, procdef: tdef; out name: shortstring): boolean; override;
    function symbol_live(const name: shortstring): boolean; override;
  end;


implementation

  uses
    globtype,
    globals,
    symdef,
    verbose;


  destructor tunitwpoinfo.destroy;
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

      ppufile.writeentry(ibcreatedobjtypes);
      freemem(fcreatedobjtypesderefs);
      fcreatedobjtypesderefs:=nil;
      freemem(fcreatedclassrefobjtypesderefs);
      fcreatedclassrefobjtypesderefs:=nil;
    end;


  constructor tunitwpoinfo.ppuload(ppufile:tcompilerppufile);
    var
      i, len: longint;
    begin
      { load start of definition section, which holds the amount of defs }
      if ppufile.readentry<>ibcreatedobjtypes then
        cgmessage(unit_f_ppu_read_error);
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
    end;


  procedure tunitwpoinfo.buildderef;
    var
      i: longint;
    begin
      getmem(fcreatedobjtypesderefs,fcreatedobjtypes.count*sizeof(tderef));
      for i:=0 to fcreatedobjtypes.count-1 do
        fcreatedobjtypesderefs^[i].build(fcreatedobjtypes[i]);

      getmem(fcreatedclassrefobjtypesderefs,fcreatedclassrefobjtypes.count*sizeof(tderef));
      for i:=0 to fcreatedclassrefobjtypes.count-1 do
        fcreatedclassrefobjtypesderefs^[i].build(fcreatedclassrefobjtypes[i]);
    end;


  procedure tunitwpoinfo.buildderefimpl;
    begin
    end;


  procedure tunitwpoinfo.deref;
    var
      i: longint;
    begin
      for i:=0 to fcreatedobjtypes.count-1 do
        fcreatedobjtypes[i]:=fcreatedobjtypesderefs^[i].resolve;
      freemem(fcreatedobjtypesderefs);
      fcreatedobjtypesderefs:=nil;

      for i:=0 to fcreatedclassrefobjtypes.count-1 do
        fcreatedclassrefobjtypes[i]:=fcreatedclassrefobjtypesderefs^[i].resolve;
      freemem(fcreatedclassrefobjtypesderefs);
      fcreatedclassrefobjtypesderefs:=nil;
    end;


  procedure tunitwpoinfo.derefimpl;
    begin
    end;


  { twpoinfomanager }

  { devirtualisation }

  function twpoinfomanager.can_be_devirtualized(objdef, procdef: tdef; out name: shortstring): boolean;
    begin
      if not assigned(wpoinfouse[wpo_devirtualization_context_insensitive]) or
         not(cs_wpo_devirtualize_calls in current_settings.dowpoptimizerswitches) then
        begin
          result:=false;
          exit;
        end;
      result:=twpodevirtualisationhandler(wpoinfouse[wpo_devirtualization_context_insensitive]).staticnameforcallingvirtualmethod(objdef,procdef,name);
    end;


  function twpoinfomanager.optimized_name_for_vmt(objdef, procdef: tdef; out name: shortstring): boolean;
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
