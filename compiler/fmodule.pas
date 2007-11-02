{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the first loading and searching of the modules

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
unit fmodule;

{$i fpcdefs.inc}

{$ifdef go32v2}
  {$define shortasmprefix}
{$endif}
{$ifdef watcom}
  {$define shortasmprefix}
{$endif}
{$ifdef tos}
  {$define shortasmprefix}
{$endif}
{$ifdef OS2}
  { Allthough OS/2 supports long filenames I play it safe and
    use 8.3 filenames, because this allows the compiler to run
    on a FAT partition. (DM) }
  {$define shortasmprefix}
{$endif}

interface

    uses
       cutils,cclasses,cfileutl,
       globtype,finput,ogbase,
       symbase,symsym,aasmbase,aasmtai,aasmdata;


    const
      UNSPECIFIED_LIBRARY_NAME = '<none>';

    type
      trecompile_reason = (rr_unknown,
        rr_noppu,rr_sourcenewer,rr_build,rr_crcchanged
      );

      tlinkcontaineritem=class(tlinkedlistitem)
      public
         data : pshortstring;
         needlink : cardinal;
         constructor Create(const s:string;m:cardinal);
         destructor Destroy;override;
      end;

      tlinkcontainer=class(tlinkedlist)
         procedure add(const s : string;m:cardinal);
         function get(var m:cardinal) : string;
         function getusemask(mask:cardinal) : string;
         function find(const s:string):boolean;
      end;

      tmodule = class;
      tused_unit = class;

      tunitmaprec = record
        u        : tmodule;
        { number of references }
        refs     : longint;
        { index in the derefmap }
        derefidx : longint;
      end;
      punitmap = ^tunitmaprec;

      tderefmaprec = record
        u           : tmodule;
        { modulename, used during ppu load }
        modulename  : pshortstring;
      end;
      pderefmap = ^tderefmaprec;

      tmodule = class(tmodulebase)
      private
        FImportLibraryList : TFPHashObjectList;
      public
        do_reload,                { force reloading of the unit }
        do_compile,               { need to compile the sources }
        sources_avail,            { if all sources are reachable }
        interface_compiled,       { if the interface section has been parsed/compiled/loaded }
        is_dbginfo_written,
        is_reset,
        is_unit,
        in_interface,             { processing the implementation part? }
        { allow global settings }
        in_global     : boolean;
        { Whether a mode switch is still allowed at this point in the parsing.}
        mode_switch_allowed,
        { generate pic helper which loads eip in ecx (for leave procedures) }
        requires_ecx_pic_helper,
        { generate pic helper which loads eip in ebx (for non leave procedures) }
        requires_ebx_pic_helper : boolean;
        mainfilepos   : tfileposinfo;
        recompile_reason : trecompile_reason;  { the reason why the unit should be recompiled }
        crc,
        interface_crc : cardinal;
        flags         : cardinal;  { the PPU flags }
        islibrary     : boolean;  { if it is a library (win32 dll) }
        IsPackage     : boolean;
        moduleid      : longint;
        unitmap       : punitmap; { mapping of all used units }
        unitmapsize   : longint;  { number of units in the map }
        derefmap      : pderefmap; { mapping of all units needed for deref }
        derefmapcnt   : longint;  { number of units in the map }
        derefmapsize  : longint;  { number of units in the map }
        derefdataintflen : longint;
        derefdata     : tdynamicarray;
        deflist,
        symlist       : TFPObjectList;
        globalsymtable,           { pointer to the global symtable of this unit }
        localsymtable : TSymtable;{ pointer to the local symtable of this unit }
        globalmacrosymtable,           { pointer to the global macro symtable of this unit }
        localmacrosymtable : TSymtable;{ pointer to the local macro symtable of this unit }
        scanner       : TObject;  { scanner object used }
        procinfo      : TObject;  { current procedure being compiled }
        asmdata       : TObject;  { Assembler data }
        asmprefix     : pshortstring;  { prefix for the smartlink asmfiles }
        debuginfo     : TObject;
        loaded_from   : tmodule;
        _exports      : tlinkedlist;
        dllscannerinputlist : TFPHashList;
        resourcefiles : TCmdStrList;
        linkunitofiles,
        linkunitstaticlibs,
        linkunitsharedlibs,
        linkotherofiles,           { objects,libs loaded from the source }
        linkothersharedlibs,       { using $L or $LINKLIB or import lib (for linux) }
        linkotherstaticlibs,
        linkotherframeworks  : tlinkcontainer;

        used_units           : tlinkedlist;
        dependent_units      : tlinkedlist;

        localunitsearchpath,           { local searchpaths }
        localobjectsearchpath,
        localincludesearchpath,
        locallibrarysearchpath,
        localframeworksearchpath : TSearchPathList;

        {create creates a new module which name is stored in 's'. LoadedFrom
        points to the module calling it. It is nil for the first compiled
        module. This allow inheritence of all path lists. MUST pay attention
        to that when creating link.res!!!!(mazen)}
        constructor create(LoadedFrom:TModule;const s:string;_is_unit:boolean);
        destructor destroy;override;
        procedure reset;virtual;
        procedure adddependency(callermodule:tmodule);
        procedure flagdependent(callermodule:tmodule);
        function  addusedunit(hp:tmodule;inuses:boolean;usym:tunitsym):tused_unit;
        procedure updatemaps;
        function  derefidx_unit(id:longint):longint;
        function  resolve_unit(id:longint):tmodule;
        procedure allunitsused;
        procedure setmodulename(const s:string);
        procedure AddExternalImport(const libname,symname:string;OrdNr: longint;isvar:boolean;ImportByOrdinalOnly:boolean);
        property ImportLibraryList : TFPHashObjectList read FImportLibraryList;
      end;

       tused_unit = class(tlinkedlistitem)
          checksum,
          interface_checksum : cardinal;
          in_uses,
          in_interface    : boolean;
          u               : tmodule;
          unitsym         : tunitsym;
          constructor create(_u : tmodule;intface,inuses:boolean;usym:tunitsym);
       end;

       tdependent_unit = class(tlinkedlistitem)
          u : tmodule;
          constructor create(_u : tmodule);
       end;

    var
       main_module       : tmodule;     { Main module of the program }
       current_module    : tmodule;     { Current module which is compiled or loaded }
       compiled_module   : tmodule;     { Current module which is compiled }
       usedunits         : tlinkedlist; { Used units for this program }
       loaded_units      : tlinkedlist; { All loaded units }
       SmartLinkOFiles   : TCmdStrList; { List of .o files which are generated,
                                          used to delete them after linking }


    procedure set_current_module(p:tmodule);
    function get_module(moduleindex : longint) : tmodule;
    function get_source_file(moduleindex,fileindex : longint) : tinputfile;
    procedure addloadedunit(hp:tmodule);
    function find_module_from_symtable(st:tsymtable):tmodule;


implementation

    uses
      SysUtils,globals,
      verbose,systems,
      scanner,ppu,dbgbase,
      procinfo;

{$ifdef MEMDEBUG}
    var
      memsymtable : TMemDebug;
{$endif}

{*****************************************************************************
                             Global Functions
*****************************************************************************}

    function find_module_from_symtable(st:tsymtable):tmodule;
      var
        hp : tmodule;
      begin
        result:=nil;
        hp:=tmodule(loaded_units.first);
        while assigned(hp) do
          begin
            if (hp.globalsymtable=st) or
               (hp.localsymtable=st) then
              begin
                result:=hp;
                exit;
              end;
            hp:=tmodule(hp.next);
         end;
      end;

    procedure set_current_module(p:tmodule);
      begin
        { save the state of the scanner }
        if assigned(current_scanner) then
          current_scanner.tempcloseinputfile;
        { set new module }
        current_module:=p;
        { restore previous module settings }
        Fillchar(current_filepos,0,sizeof(current_filepos));
        if assigned(current_module) then
          begin
            current_asmdata:=tasmdata(current_module.asmdata);
            current_debuginfo:=tdebuginfo(current_module.debuginfo);
            { restore scanner and file positions }
            current_scanner:=tscannerfile(current_module.scanner);
            if assigned(current_scanner) then
              begin
                current_scanner.tempopeninputfile;
                current_scanner.gettokenpos;
                parser_current_file:=current_scanner.inputfile.name^;
              end
            else
              begin
                current_filepos.moduleindex:=current_module.unit_index;
                parser_current_file:='';
              end;
          end
        else
          begin
            current_asmdata:=nil;
            current_scanner:=nil;
            current_debuginfo:=nil;
          end;
      end;


    function get_module(moduleindex : longint) : tmodule;
      var
         hp : tmodule;
      begin
         result:=nil;
         if moduleindex=0 then
           exit;
         result:=current_module;
         if not(assigned(loaded_units)) then
           exit;
         hp:=tmodule(loaded_units.first);
         while assigned(hp) and (hp.unit_index<>moduleindex) do
           hp:=tmodule(hp.next);
         result:=hp;
      end;


    function get_source_file(moduleindex,fileindex : longint) : tinputfile;
      var
         hp : tmodule;
      begin
         hp:=get_module(moduleindex);
         if assigned(hp) then
          get_source_file:=hp.sourcefiles.get_file(fileindex)
         else
          get_source_file:=nil;
      end;


    procedure addloadedunit(hp:tmodule);
      begin
        hp.moduleid:=loaded_units.count;
        loaded_units.concat(hp);
      end;


{****************************************************************************
                             TLinkContainerItem
 ****************************************************************************}

    constructor TLinkContainerItem.Create(const s:string;m:cardinal);
      begin
        inherited Create;
        data:=stringdup(s);
        needlink:=m;
      end;


    destructor TLinkContainerItem.Destroy;
      begin
        stringdispose(data);
      end;


{****************************************************************************
                           TLinkContainer
 ****************************************************************************}

    procedure TLinkContainer.add(const s : string;m:cardinal);
      begin
        inherited concat(TLinkContainerItem.Create(s,m));
      end;


    function TLinkContainer.get(var m:cardinal) : string;
      var
        p : tlinkcontaineritem;
      begin
        p:=tlinkcontaineritem(inherited getfirst);
        if p=nil then
         begin
           get:='';
           m:=0;
         end
        else
         begin
           get:=p.data^;
           m:=p.needlink;
           p.free;
         end;
      end;


    function TLinkContainer.getusemask(mask:cardinal) : string;
      var
         p : tlinkcontaineritem;
         found : boolean;
      begin
        found:=false;
        repeat
          p:=tlinkcontaineritem(inherited getfirst);
          if p=nil then
           begin
             getusemask:='';
             exit;
           end;
          getusemask:=p.data^;
          found:=(p.needlink and mask)<>0;
          p.free;
        until found;
      end;


    function TLinkContainer.find(const s:string):boolean;
      var
        newnode : tlinkcontaineritem;
      begin
        find:=false;
        newnode:=tlinkcontaineritem(First);
        while assigned(newnode) do
         begin
           if newnode.data^=s then
            begin
              find:=true;
              exit;
            end;
           newnode:=tlinkcontaineritem(newnode.next);
         end;
      end;


{****************************************************************************
                              TUSED_UNIT
 ****************************************************************************}

    constructor tused_unit.create(_u : tmodule;intface,inuses:boolean;usym:tunitsym);
      begin
        u:=_u;
        in_interface:=intface;
        in_uses:=inuses;
        unitsym:=usym;
        if _u.state=ms_compiled then
         begin
           checksum:=u.crc;
           interface_checksum:=u.interface_crc;
         end
        else
         begin
           checksum:=0;
           interface_checksum:=0;
         end;
      end;


{****************************************************************************
                            TDENPENDENT_UNIT
 ****************************************************************************}

    constructor tdependent_unit.create(_u : tmodule);
      begin
         u:=_u;
      end;


{****************************************************************************
                                  TMODULE
 ****************************************************************************}

    constructor tmodule.create(LoadedFrom:TModule;const s:string;_is_unit:boolean);
      var
        n : string;
      begin
        n:=ChangeFileExt(ExtractFileName(s),'');
        { Programs have the name 'Program' to don't conflict with dup id's }
        if _is_unit then
         inherited create(n)
        else
         inherited create('Program');
        mainsource:=stringdup(s);
        { Dos has the famous 8.3 limit :( }
{$ifdef shortasmprefix}
        asmprefix:=stringdup(FixFileName('as'));
{$else}
        asmprefix:=stringdup(FixFileName(n));
{$endif}
        setfilename(s,true);
        localunitsearchpath:=TSearchPathList.Create;
        localobjectsearchpath:=TSearchPathList.Create;
        localincludesearchpath:=TSearchPathList.Create;
        locallibrarysearchpath:=TSearchPathList.Create;
        localframeworksearchpath:=TSearchPathList.Create;
        used_units:=TLinkedList.Create;
        dependent_units:=TLinkedList.Create;
        resourcefiles:=TCmdStrList.Create;
        linkunitofiles:=TLinkContainer.Create;
        linkunitstaticlibs:=TLinkContainer.Create;
        linkunitsharedlibs:=TLinkContainer.Create;
        linkotherofiles:=TLinkContainer.Create;
        linkotherstaticlibs:=TLinkContainer.Create;
        linkothersharedlibs:=TLinkContainer.Create;
        linkotherframeworks:=TLinkContainer.Create;
        FImportLibraryList:=TFPHashObjectList.Create(true);
        crc:=0;
        interface_crc:=0;
        flags:=0;
        scanner:=nil;
        unitmap:=nil;
        unitmapsize:=0;
        derefmap:=nil;
        derefmapsize:=0;
        derefmapcnt:=0;
        derefdata:=TDynamicArray.Create(1024);
        derefdataintflen:=0;
        deflist:=TFPObjectList.Create(false);
        symlist:=TFPObjectList.Create(false);
        globalsymtable:=nil;
        localsymtable:=nil;
        globalmacrosymtable:=nil;
        localmacrosymtable:=nil;
        loaded_from:=LoadedFrom;
        do_reload:=false;
        do_compile:=false;
        sources_avail:=true;
        mainfilepos.line:=0;
        mainfilepos.column:=0;
        mainfilepos.fileindex:=0;
        recompile_reason:=rr_unknown;
        in_interface:=true;
        in_global:=true;
        is_unit:=_is_unit;
        islibrary:=false;
        ispackage:=false;
        is_dbginfo_written:=false;
        is_reset:=false;
        mode_switch_allowed:= true;
        _exports:=TLinkedList.Create;
        dllscannerinputlist:=TFPHashList.Create;
        asmdata:=TAsmData.create(realmodulename^);
        InitDebugInfo(self);
      end;


    destructor tmodule.Destroy;
      var
        i : longint;
        hpi : tprocinfo;
      begin
        if assigned(unitmap) then
          freemem(unitmap);
        if assigned(derefmap) then
          begin
            for i:=0 to derefmapcnt-1 do
              stringdispose(derefmap[i].modulename);
            freemem(derefmap);
          end;
        if assigned(_exports) then
         _exports.free;
        if assigned(dllscannerinputlist) then
         dllscannerinputlist.free;
        if assigned(scanner) then
         begin
            { also update current_scanner if it was pointing
              to this module }
            if current_scanner=tscannerfile(scanner) then
             current_scanner:=nil;
            tscannerfile(scanner).free;
         end;
        if assigned(asmdata) then
          begin
            if current_asmdata=asmdata then
              current_asmdata:=nil;
             asmdata.free;
          end;
        if assigned(procinfo) then
          begin
            if current_procinfo=tprocinfo(procinfo) then
             current_procinfo:=nil;
            { release procinfo tree }
            while assigned(procinfo) do
             begin
               hpi:=tprocinfo(procinfo).parent;
               tprocinfo(procinfo).free;
               procinfo:=hpi;
             end;
          end;
        used_units.free;
        dependent_units.free;
        resourcefiles.Free;
        linkunitofiles.Free;
        linkunitstaticlibs.Free;
        linkunitsharedlibs.Free;
        linkotherofiles.Free;
        linkotherstaticlibs.Free;
        linkothersharedlibs.Free;
        linkotherframeworks.Free;
        FImportLibraryList.Free;
        stringdispose(objfilename);
        stringdispose(asmfilename);
        stringdispose(ppufilename);
        stringdispose(importlibfilename);
        stringdispose(staticlibfilename);
        stringdispose(sharedlibfilename);
        stringdispose(exefilename);
        stringdispose(outputpath);
        stringdispose(path);
        stringdispose(realmodulename);
        stringdispose(mainsource);
        stringdispose(asmprefix);
        localunitsearchpath.Free;
        localobjectsearchpath.free;
        localincludesearchpath.free;
        locallibrarysearchpath.free;
        localframeworksearchpath.free;
{$ifdef MEMDEBUG}
        memsymtable.start;
{$endif}
        derefdata.free;
        deflist.free;
        symlist.free;
        if assigned(globalsymtable) then
          globalsymtable.free;
        if assigned(localsymtable) then
          localsymtable.free;
        if assigned(globalmacrosymtable) then
          globalmacrosymtable.free;
        if assigned(localmacrosymtable) then
          localmacrosymtable.free;
{$ifdef MEMDEBUG}
        memsymtable.stop;
{$endif}
        stringdispose(modulename);
        inherited Destroy;
      end;


    procedure tmodule.reset;
      var
        hpi : tprocinfo;
        i   : longint;
      begin
        if assigned(scanner) then
          begin
            { also update current_scanner if it was pointing
              to this module }
            if current_scanner=tscannerfile(scanner) then
             current_scanner:=nil;
            tscannerfile(scanner).free;
            scanner:=nil;
          end;
        if assigned(procinfo) then
          begin
            if current_procinfo=tprocinfo(procinfo) then
             current_procinfo:=nil;
            { release procinfo tree }
            while assigned(procinfo) do
             begin
               hpi:=tprocinfo(procinfo).parent;
               tprocinfo(procinfo).free;
               procinfo:=hpi;
             end;
          end;
        if assigned(asmdata) then
          begin
            if current_asmdata=TAsmData(asmdata) then
             current_asmdata:=nil;
            asmdata.free;
            asmdata:=nil;
          end;
        DoneDebugInfo(self);
        if assigned(globalsymtable) then
          begin
            globalsymtable.free;
            globalsymtable:=nil;
          end;
        if assigned(localsymtable) then
          begin
            localsymtable.free;
            localsymtable:=nil;
          end;
        if assigned(globalmacrosymtable) then
          begin
            globalmacrosymtable.free;
            globalmacrosymtable:=nil;
          end;
        if assigned(localmacrosymtable) then
          begin
            localmacrosymtable.free;
            localmacrosymtable:=nil;
          end;
        deflist.free;
        deflist:=TFPObjectList.Create(false);
        symlist.free;
        symlist:=TFPObjectList.Create(false);
        derefdata.free;
        derefdata:=TDynamicArray.Create(1024);
        if assigned(unitmap) then
          begin
            freemem(unitmap);
            unitmap:=nil;
          end;
        if assigned(derefmap) then
          begin
            for i:=0 to derefmapcnt-1 do
              stringdispose(derefmap[i].modulename);
            freemem(derefmap);
            derefmap:=nil;
          end;
        unitmapsize:=0;
        derefmapsize:=0;
        derefmapcnt:=0;
        derefdataintflen:=0;
        sourcefiles.free;
        sourcefiles:=tinputfilemanager.create;
        asmdata:=TAsmData.create(realmodulename^);
        DoneDebugInfo(self);
        InitDebugInfo(self);
        _exports.free;
        _exports:=tlinkedlist.create;
        dllscannerinputlist.free;
        dllscannerinputlist:=TFPHashList.create;
        used_units.free;
        used_units:=TLinkedList.Create;
        dependent_units.free;
        dependent_units:=TLinkedList.Create;
        resourcefiles.Free;
        resourcefiles:=TCmdStrList.Create;
        linkunitofiles.Free;
        linkunitofiles:=TLinkContainer.Create;
        linkunitstaticlibs.Free;
        linkunitstaticlibs:=TLinkContainer.Create;
        linkunitsharedlibs.Free;
        linkunitsharedlibs:=TLinkContainer.Create;
        linkotherofiles.Free;
        linkotherofiles:=TLinkContainer.Create;
        linkotherstaticlibs.Free;
        linkotherstaticlibs:=TLinkContainer.Create;
        linkothersharedlibs.Free;
        linkothersharedlibs:=TLinkContainer.Create;
        linkotherframeworks.Free;
        linkotherframeworks:=TLinkContainer.Create;
        FImportLibraryList.Free;
        FImportLibraryList:=TFPHashObjectList.Create;
        do_compile:=false;
        do_reload:=false;
        interface_compiled:=false;
        in_interface:=true;
        in_global:=true;
        mode_switch_allowed:=true;
        is_dbginfo_written:=false;
        is_reset:=false;
        crc:=0;
        interface_crc:=0;
        flags:=0;
        mainfilepos.line:=0;
        mainfilepos.column:=0;
        mainfilepos.fileindex:=0;
        recompile_reason:=rr_unknown;
        {
          The following fields should not
          be reset:
           mainsource
           state
           loaded_from
           sources_avail
        }
      end;


    procedure tmodule.adddependency(callermodule:tmodule);
      begin
        { This is not needed for programs }
        if not callermodule.is_unit then
          exit;
        Message2(unit_u_add_depend_to,callermodule.modulename^,modulename^);
        dependent_units.concat(tdependent_unit.create(callermodule));
      end;


    procedure tmodule.flagdependent(callermodule:tmodule);
      var
        pm : tdependent_unit;
      begin
        { flag all units that depend on this unit for reloading }
        pm:=tdependent_unit(current_module.dependent_units.first);
        while assigned(pm) do
         begin
           { We do not have to reload the unit that wants to load
             this unit, unless this unit is already compiled during
             the loading }
           if (pm.u=callermodule) and
              (pm.u.state<>ms_compiled) then
             Message1(unit_u_no_reload_is_caller,pm.u.modulename^)
           else
            if pm.u.state=ms_second_compile then
              Message1(unit_u_no_reload_in_second_compile,pm.u.modulename^)
           else
            begin
              pm.u.do_reload:=true;
              Message1(unit_u_flag_for_reload,pm.u.modulename^);
            end;
           pm:=tdependent_unit(pm.next);
         end;
      end;


    function tmodule.addusedunit(hp:tmodule;inuses:boolean;usym:tunitsym):tused_unit;
      var
        pu : tused_unit;
      begin
        pu:=tused_unit.create(hp,in_interface,inuses,usym);
        used_units.concat(pu);
        addusedunit:=pu;
      end;


    procedure tmodule.updatemaps;
      var
        oldmapsize : longint;
        hp  : tmodule;
        i   : longint;
      begin
        { Extend unitmap }
        oldmapsize:=unitmapsize;
        unitmapsize:=loaded_units.count;
        reallocmem(unitmap,unitmapsize*sizeof(tunitmaprec));
        fillchar(unitmap[oldmapsize],(unitmapsize-oldmapsize)*sizeof(tunitmaprec),0);

        { Extend Derefmap }
        oldmapsize:=derefmapsize;
        derefmapsize:=loaded_units.count;
        reallocmem(derefmap,derefmapsize*sizeof(tderefmaprec));
        fillchar(derefmap[oldmapsize],(derefmapsize-oldmapsize)*sizeof(tderefmaprec),0);

        { Add all units to unitmap }
        hp:=tmodule(loaded_units.first);
        i:=0;
        while assigned(hp) do
          begin
            if hp.moduleid>=unitmapsize then
              internalerror(200501151);
            { Verify old entries }
            if (i<oldmapsize) then
              begin
                if (hp.moduleid<>i) or
                   (unitmap[hp.moduleid].u<>hp) then
                  internalerror(200501156);
              end
            else
              begin
                unitmap[hp.moduleid].u:=hp;
                unitmap[hp.moduleid].derefidx:=-1;
              end;
            inc(i);
            hp:=tmodule(hp.next);
          end;
      end;


    function tmodule.derefidx_unit(id:longint):longint;
      begin
        if id>=unitmapsize then
          internalerror(2005011511);
        if unitmap[id].derefidx=-1 then
          begin
            unitmap[id].derefidx:=derefmapcnt;
            inc(derefmapcnt);
            derefmap[unitmap[id].derefidx].u:=unitmap[id].u;
          end;
        if unitmap[id].derefidx>=derefmapsize then
          internalerror(2005011514);
        result:=unitmap[id].derefidx;
      end;


    function tmodule.resolve_unit(id:longint):tmodule;
      var
        hp : tmodule;
      begin
        if id>=derefmapsize then
          internalerror(200306231);
        result:=derefmap[id].u;
        if not assigned(result) then
          begin
            if not assigned(derefmap[id].modulename) or
               (derefmap[id].modulename^='') then
              internalerror(200501159);
            hp:=tmodule(loaded_units.first);
            while assigned(hp) do
              begin
                { only check for units. The main program is also
                  as a unit in the loaded_units list. We simply need
                  to ignore this entry (PFV) }
                if hp.is_unit and
                   (hp.modulename^=derefmap[id].modulename^) then
                  break;
                hp:=tmodule(hp.next);
              end;
            if not assigned(hp) then
              internalerror(2005011510);
            derefmap[id].u:=hp;
            result:=hp;
          end;
      end;


    procedure tmodule.allunitsused;
      var
        pu : tused_unit;
      begin
        pu:=tused_unit(used_units.first);
        while assigned(pu) do
          begin
            if assigned(pu.u.globalsymtable) then
              begin
                if unitmap[pu.u.moduleid].u<>pu.u then
                  internalerror(200501157);
                { Give a note when the unit is not referenced, skip
                  this is for units with an initialization/finalization }
                if (unitmap[pu.u.moduleid].refs=0) and
                   ((pu.u.flags and (uf_init or uf_finalize))=0) then
                  CGMessagePos2(pu.unitsym.fileinfo,sym_n_unit_not_used,pu.u.realmodulename^,realmodulename^);
              end;
            pu:=tused_unit(pu.next);
          end;
      end;


    procedure tmodule.setmodulename(const s:string);
      begin
        stringdispose(modulename);
        stringdispose(realmodulename);
        modulename:=stringdup(upper(s));
        realmodulename:=stringdup(s);
        { also update asmlibrary names }
        current_asmdata.name:=modulename^;
        current_asmdata.realname:=realmodulename^;
      end;


    procedure TModule.AddExternalImport(const libname,symname:string;OrdNr: longint;isvar:boolean;ImportByOrdinalOnly:boolean);
      var
        ImportLibrary : TImportLibrary;
        ImportSymbol  : TFPHashObject;
      begin
        ImportLibrary:=TImportLibrary(ImportLibraryList.Find(libname));
        if not assigned(ImportLibrary) then
          ImportLibrary:=TImportLibrary.Create(ImportLibraryList,libname);
        ImportSymbol:=TFPHashObject(ImportLibrary.ImportSymbolList.Find(symname));
        if not assigned(ImportSymbol) then
          begin
            if not ImportByOrdinalOnly then
              { negative ordinal number indicates import by name with ordinal number as hint }
              OrdNr:=-OrdNr;
            ImportSymbol:=TImportSymbol.Create(ImportLibrary.ImportSymbolList,symname,OrdNr,isvar);
          end;
      end;


initialization
{$ifdef MEMDEBUG}
  memsymtable:=TMemDebug.create('Symtables');
  memsymtable.stop;
{$endif MEMDEBUG}

finalization
{$ifdef MEMDEBUG}
  memsymtable.free;
{$endif MEMDEBUG}

end.
