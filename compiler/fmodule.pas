{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

{$ifdef go32v1}
  {$define SHORTASMPREFIX}
{$endif}
{$ifdef go32v2}
  {$define SHORTASMPREFIX}
{$endif}
{$ifdef OS2}
  { Allthough OS/2 supports long filenames I play it safe and
    use 8.3 filenames, because this allows the compiler to run
    on a FAT partition. (DM) }
  {$define SHORTASMPREFIX}
{$endif}

interface

    uses
       cutils,cobjects,
       globals,ppu,finput;

    const
       maxunits = 1024;

    type
       trecompile_reason = (rr_unknown,
         rr_noppu,rr_sourcenewer,rr_build,rr_libolder,rr_objolder,
         rr_asmolder,rr_crcchanged
       );

       plinkcontaineritem=^tlinkcontaineritem;
       tlinkcontaineritem=object(tcontaineritem)
          data     : pstring;
          needlink : longint;
          constructor init(const s:string;m:longint);
          destructor  done;virtual;
       end;

       plinkcontainer=^tlinkcontainer;
       tlinkcontainer=object(tcontainer)
          constructor Init;
          procedure insert(const s : string;m:longint);
          function get(var m:longint) : string;
          function getusemask(mask:longint) : string;
          function find(const s:string):boolean;
       end;

       pmodule = ^tmodule;

{$ifndef NEWMAP}
       tunitmap = array[0..maxunits-1] of pointer;
       punitmap = ^tunitmap;
{$else NEWMAP}
       tunitmap = array[0..maxunits-1] of pmodule;
       punitmap = ^tunitmap;
{$endif NEWMAP}

       tmodule = object(tlinkedlist_item)
          ppufile       : pppufile; { the PPU file }
          crc,
          interface_crc,
          flags         : longint;  { the PPU flags }

          compiled,                 { unit is already compiled }
          do_reload,                { force reloading of the unit }
          do_assemble,              { only assemble the object, don't recompile }
          do_compile,               { need to compile the sources }
          sources_avail,            { if all sources are reachable }
          sources_checked,          { if there is already done a check for the sources }
          is_unit,
          in_compile,               { is it being compiled ?? }
          in_second_compile,        { is this unit being compiled for the 2nd time? }
          in_second_load,           { is this unit PPU loaded a 2nd time? }
          in_implementation,        { processing the implementation part? }
          in_global     : boolean;  { allow global settings }
          recompile_reason : trecompile_reason;  { the reason why the unit should be recompiled }

          islibrary     : boolean;  { if it is a library (win32 dll) }
          map           : punitmap; { mapping of all used units }
          unitcount     : word;     { local unit counter }
          unit_index    : word;     { global counter for browser }
          globalsymtable,           { pointer to the local/static symtable of this unit }
          localsymtable : pointer;  { pointer to the psymtable of this unit }
          scanner       : pointer;  { scanner object used }
          loaded_from   : pmodule;
          uses_imports  : boolean;  { Set if the module imports from DLL's.}
          imports       : plinkedlist;
          _exports      : plinkedlist;

          sourcefiles   : pinputfilemanager;
          resourcefiles : tstringcontainer;

          linkunitofiles,
          linkunitstaticlibs,
          linkunitsharedlibs,
          linkotherofiles,           { objects,libs loaded from the source }
          linkothersharedlibs,       { using $L or $LINKLIB or import lib (for linux) }
          linkotherstaticlibs  : tlinkcontainer;

          used_units           : tlinkedlist;
          dependent_units      : tlinkedlist;

          localunitsearchpath,           { local searchpaths }
          localobjectsearchpath,
          localincludesearchpath,
          locallibrarysearchpath : TSearchPathList;

          path,                     { path where the module is find/created }
          outputpath,               { path where the .s / .o / exe are created }
          modulename,               { name of the module in uppercase }
          objfilename,              { fullname of the objectfile }
          asmfilename,              { fullname of the assemblerfile }
          ppufilename,              { fullname of the ppufile }
          staticlibfilename,        { fullname of the static libraryfile }
          sharedlibfilename,        { fullname of the shared libraryfile }
          exefilename,              { fullname of the exefile }
          asmprefix,                { prefix for the smartlink asmfiles }
          mainsource    : pstring;  { name of the main sourcefile }
{$ifdef Test_Double_checksum}
          crc_array : pointer;
          crc_size : longint;
          crc_array2 : pointer;
          crc_size2 : longint;
{$endif def Test_Double_checksum}
          constructor init(const s:string;_is_unit:boolean);
          destructor done;virtual;
          procedure reset;
          procedure setfilename(const fn:string;allowoutput:boolean);
          function  openppu:boolean;
          function  search_unit(const n : string;onlysource:boolean):boolean;
       end;

       pused_unit = ^tused_unit;
       tused_unit = object(tlinkedlist_item)
          unitid          : word;
          name            : pstring;
          checksum,
          interface_checksum : longint;
          loaded          : boolean;
          in_uses,
          in_interface,
          is_stab_written : boolean;
          u               : pmodule;
          constructor init(_u : pmodule;intface:boolean);
          constructor init_to_load(const n:string;c,intfc:longint;intface:boolean);
          destructor done;virtual;
       end;

       pdependent_unit = ^tdependent_unit;
       tdependent_unit = object(tlinkedlist_item)
          u : pmodule;
          constructor init(_u : pmodule);
       end;

    var
       main_module       : pmodule;     { Main module of the program }
       current_module    : pmodule;     { Current module which is compiled or loaded }
       compiled_module   : pmodule;     { Current module which is compiled }
       current_ppu       : pppufile;    { Current ppufile which is read }
       global_unit_count : word;
       usedunits         : tlinkedlist; { Used units for this program }
       loaded_units      : tlinkedlist; { All loaded units }
       SmartLinkOFiles   : TStringContainer; { List of .o files which are generated,
                                               used to delete them after linking }

function get_source_file(moduleindex,fileindex : word) : pinputfile;


implementation

uses
{$ifdef delphi}
  dmisc,
{$else}
  dos,
{$endif}
  globtype,verbose,systems,
  symtable,scanner;


{*****************************************************************************
                             Global Functions
*****************************************************************************}

    function get_source_file(moduleindex,fileindex : word) : pinputfile;
      var
         hp : pmodule;
         f : pinputfile;
      begin
         hp:=pmodule(loaded_units.first);
         while assigned(hp) and (hp^.unit_index<>moduleindex) do
           hp:=pmodule(hp^.next);
         get_source_file:=nil;
         if not assigned(hp) then
           exit;
         f:=pinputfile(hp^.sourcefiles^.files);
         while assigned(f) do
           begin
              if f^.ref_index=fileindex then
                begin
                   get_source_file:=f;
                   exit;
                end;
              f:=pinputfile(f^.ref_next);
           end;
      end;


{****************************************************************************
                             TLinkContainerItem
 ****************************************************************************}

constructor TLinkContainerItem.Init(const s:string;m:longint);
begin
  inherited Init;
  data:=stringdup(s);
  needlink:=m;
end;


destructor TLinkContainerItem.Done;
begin
  stringdispose(data);
end;


{****************************************************************************
                           TLinkContainer
 ****************************************************************************}

    constructor TLinkContainer.Init;
      begin
        inherited init;
      end;


    procedure TLinkContainer.insert(const s : string;m:longint);
      var
        newnode : plinkcontaineritem;
      begin
         {if find(s) then
          exit; }
         new(newnode,init(s,m));
         inherited insert(newnode);
      end;


    function TLinkContainer.get(var m:longint) : string;
      var
        p : plinkcontaineritem;
      begin
        p:=plinkcontaineritem(inherited get);
        if p=nil then
         begin
           get:='';
           m:=0;
           exit;
         end;
        get:=p^.data^;
        m:=p^.needlink;
        dispose(p,done);
      end;


    function TLinkContainer.getusemask(mask:longint) : string;
      var
         p : plinkcontaineritem;
         found : boolean;
      begin
        found:=false;
        repeat
          p:=plinkcontaineritem(inherited get);
          if p=nil then
           begin
             getusemask:='';
             exit;
           end;
          getusemask:=p^.data^;
          found:=(p^.needlink and mask)<>0;
          dispose(p,done);
        until found;
      end;


    function TLinkContainer.find(const s:string):boolean;
      var
        newnode : plinkcontaineritem;
      begin
        find:=false;
        newnode:=plinkcontaineritem(root);
        while assigned(newnode) do
         begin
           if newnode^.data^=s then
            begin
              find:=true;
              exit;
            end;
           newnode:=plinkcontaineritem(newnode^.next);
         end;
      end;


{****************************************************************************
                                  TMODULE
 ****************************************************************************}

    procedure tmodule.setfilename(const fn:string;allowoutput:boolean);
      var
        p : dirstr;
        n : NameStr;
        e : ExtStr;
      begin
         stringdispose(objfilename);
         stringdispose(asmfilename);
         stringdispose(ppufilename);
         stringdispose(staticlibfilename);
         stringdispose(sharedlibfilename);
         stringdispose(exefilename);
         stringdispose(outputpath);
         stringdispose(path);
         { Create names }
         fsplit(fn,p,n,e);
         n:=FixFileName(n);
         { set path }
         path:=stringdup(FixPath(p,false));
         { obj,asm,ppu names }
         p:=path^;
         if AllowOutput then
          begin
            if (OutputUnitDir<>'') then
             p:=OutputUnitDir
            else
             if (OutputExeDir<>'') then
              p:=OutputExeDir;
          end;
         outputpath:=stringdup(p);
         objfilename:=stringdup(p+n+target_info.objext);
         asmfilename:=stringdup(p+n+target_info.asmext);
         ppufilename:=stringdup(p+n+target_info.unitext);
         { lib and exe could be loaded with a file specified with -o }
         if AllowOutput and (OutputFile<>'') and (compile_level=1) then
          n:=OutputFile;
         staticlibfilename:=stringdup(p+target_os.libprefix+n+target_os.staticlibext);
         if target_info.target=target_i386_WIN32 then
           sharedlibfilename:=stringdup(p+n+target_os.sharedlibext)
         else
           sharedlibfilename:=stringdup(p+target_os.libprefix+n+target_os.sharedlibext);
         { output dir of exe can be specified separatly }
         if AllowOutput and (OutputExeDir<>'') then
          p:=OutputExeDir
         else
          p:=path^;
         exefilename:=stringdup(p+n+target_info.exeext);
      end;


    function tmodule.openppu:boolean;
      var
        objfiletime,
        ppufiletime,
        asmfiletime : longint;
      begin
        openppu:=false;
        Message1(unit_t_ppu_loading,ppufilename^);
      { Get ppufile time (also check if the file exists) }
        ppufiletime:=getnamedfiletime(ppufilename^);
        if ppufiletime=-1 then
         exit;
      { Open the ppufile }
        Message1(unit_u_ppu_name,ppufilename^);
        ppufile:=new(pppufile,init(ppufilename^));
        ppufile^.change_endian:=source_os.endian<>target_os.endian;
        if not ppufile^.open then
         begin
           dispose(ppufile,done);
           Message(unit_u_ppu_file_too_short);
           exit;
         end;
      { check for a valid PPU file }
        if not ppufile^.CheckPPUId then
         begin
           dispose(ppufile,done);
           Message(unit_u_ppu_invalid_header);
           exit;
         end;
      { check for allowed PPU versions }
        if not (ppufile^.GetPPUVersion = CurrentPPUVersion) then
         begin
           dispose(ppufile,done);
           Message1(unit_u_ppu_invalid_version,tostr(ppufile^.GetPPUVersion));
           exit;
         end;
      { check the target processor }
        if ttargetcpu(ppufile^.header.cpu)<>target_cpu then
         begin
           dispose(ppufile,done);
           Message(unit_u_ppu_invalid_processor);
           exit;
         end;
      { check target }
        if ttarget(ppufile^.header.target)<>target_info.target then
         begin
           dispose(ppufile,done);
           Message(unit_u_ppu_invalid_target);
           exit;
         end;
      { Load values to be access easier }
        flags:=ppufile^.header.flags;
        crc:=ppufile^.header.checksum;
        interface_crc:=ppufile^.header.interface_checksum;
      { Show Debug info }
        Message1(unit_u_ppu_time,filetimestring(ppufiletime));
        Message1(unit_u_ppu_flags,tostr(flags));
        Message1(unit_u_ppu_crc,tostr(ppufile^.header.checksum));
        Message1(unit_u_ppu_crc,tostr(ppufile^.header.interface_checksum)+' (intfc)');
      { check the object and assembler file to see if we need only to
        assemble, only if it's not in a library }
        do_compile:=false;
        if (flags and uf_in_library)=0 then
         begin
           if (flags and uf_smart_linked)<>0 then
            begin
              objfiletime:=getnamedfiletime(staticlibfilename^);
              Message2(unit_u_check_time,staticlibfilename^,filetimestring(objfiletime));
              if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
                begin
                  recompile_reason:=rr_libolder;
                  Message(unit_u_recompile_staticlib_is_older);
                  do_compile:=true;
                  exit;
                end;
            end;
           if (flags and uf_static_linked)<>0 then
            begin
              { the objectfile should be newer than the ppu file }
              objfiletime:=getnamedfiletime(objfilename^);
              Message2(unit_u_check_time,objfilename^,filetimestring(objfiletime));
              if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
               begin
                 { check if assembler file is older than ppu file }
                 asmfileTime:=GetNamedFileTime(asmfilename^);
                 Message2(unit_u_check_time,asmfilename^,filetimestring(asmfiletime));
                 if (asmfiletime<0) or (ppufiletime>asmfiletime) then
                  begin
                    Message(unit_u_recompile_obj_and_asm_older);
                    recompile_reason:=rr_objolder;
                    do_compile:=true;
                    exit;
                  end
                 else
                  begin
                    Message(unit_u_recompile_obj_older_than_asm);
                    if not(cs_asm_extern in aktglobalswitches) then
                     begin
                       do_compile:=true;
                       recompile_reason:=rr_asmolder;
                       exit;
                     end;
                  end;
               end;
            end;
         end;
        openppu:=true;
      end;


    function tmodule.search_unit(const n : string;onlysource:boolean):boolean;
      var
         singlepathstring,
         filename : string;

         Function UnitExists(const ext:string):boolean;
         begin
           Message1(unit_t_unitsearch,Singlepathstring+filename+ext);
           UnitExists:=FileExists(Singlepathstring+FileName+ext);
         end;

         Function PPUSearchPath(const s:string):boolean;
         var
           found   : boolean;
         begin
           Found:=false;
           singlepathstring:=FixPath(s,false);
         { Check for PPU file }
           Found:=UnitExists(target_info.unitext);
           if Found then
            Begin
              SetFileName(SinglePathString+FileName,false);
              Found:=OpenPPU;
            End;
           PPUSearchPath:=Found;
         end;

         Function SourceSearchPath(const s:string):boolean;
         var
           found   : boolean;
           ext     : string[8];
         begin
           Found:=false;
           singlepathstring:=FixPath(s,false);
         { Check for Sources }
           ppufile:=nil;
           do_compile:=true;
           recompile_reason:=rr_noppu;
         {Check for .pp file}
           Found:=UnitExists(target_os.sourceext);
           if Found then
            Ext:=target_os.sourceext
           else
            begin
            {Check for .pas}
              Found:=UnitExists(target_os.pasext);
              if Found then
               Ext:=target_os.pasext;
            end;
           stringdispose(mainsource);
           if Found then
            begin
              sources_avail:=true;
            {Load Filenames when found}
              mainsource:=StringDup(SinglePathString+FileName+Ext);
              SetFileName(SinglePathString+FileName,false);
            end
           else
            sources_avail:=false;
           SourceSearchPath:=Found;
         end;

         Function SearchPath(const s:string):boolean;
         var
           found : boolean;
         begin
           { First check for a ppu, then for the source }
           found:=false;
           if not onlysource then
            found:=PPUSearchPath(s);
           if not found then
            found:=SourceSearchPath(s);
           SearchPath:=found;
         end;

         Function SearchPathList(list:TSearchPathList):boolean;
         var
           hp : PStringQueueItem;
           found : boolean;
         begin
           found:=false;
           hp:=list.First;
           while assigned(hp) do
            begin
              found:=SearchPath(hp^.data^);
              if found then
               break;
              hp:=hp^.next;
            end;
           SearchPathList:=found;
         end;

       var
         fnd : boolean;
       begin
         filename:=FixFileName(n);
         { try to find unit
            1. look for ppu in cwd
            2. look for ppu in outputpath if set, this is tp7 compatible (PFV)
            3. look for source in cwd
            4. local unit pathlist
            5. global unit pathlist }
         fnd:=false;
         if not onlysource then
          begin
            fnd:=PPUSearchPath('.');
            if (not fnd) and (current_module^.outputpath^<>'') then
             fnd:=PPUSearchPath(current_module^.outputpath^);
           end;
         if (not fnd) then
          fnd:=SourceSearchPath('.');
         if (not fnd) then
          fnd:=SearchPathList(current_module^.LocalUnitSearchPath);
         if (not fnd) then
          fnd:=SearchPathList(UnitSearchPath);

         { try to find a file with the first 8 chars of the modulename, like
           dos }
         if (not fnd) and (length(filename)>8) then
          begin
            filename:=copy(filename,1,8);
            fnd:=SearchPath('.');
            if (not fnd) then
             fnd:=SearchPathList(current_module^.LocalUnitSearchPath);
            if not fnd then
             fnd:=SearchPathList(UnitSearchPath);
          end;
         search_unit:=fnd;
      end;



    procedure tmodule.reset;
      var
         pm : pdependent_unit;
      begin
        if assigned(scanner) then
          pscannerfile(scanner)^.invalid:=true;
        if assigned(globalsymtable) then
          begin
            dispose(punitsymtable(globalsymtable),done);
            globalsymtable:=nil;
          end;
        if assigned(localsymtable) then
          begin
            dispose(punitsymtable(localsymtable),done);
            localsymtable:=nil;
          end;
        if assigned(map) then
         begin
           dispose(map);
           map:=nil;
         end;
        if assigned(ppufile) then
         begin
           dispose(ppufile,done);
           ppufile:=nil;
         end;
        sourcefiles^.done;
        sourcefiles^.init;
        imports^.done;
        imports^.init;
        _exports^.done;
        _exports^.init;
        used_units.done;
        used_units.init;
        { all units that depend on this one must be recompiled ! }
        pm:=pdependent_unit(dependent_units.first);
        while assigned(pm) do
          begin
            if pm^.u^.in_second_compile then
             Comment(v_debug,'No reload already in second compile: '+pm^.u^.modulename^)
            else
             begin
               pm^.u^.do_reload:=true;
               Comment(v_debug,'Reloading '+pm^.u^.modulename^+' needed because '+modulename^+' is reloaded');
             end;
            pm:=pdependent_unit(pm^.next);
          end;
        dependent_units.done;
        dependent_units.init;
        resourcefiles.done;
        resourcefiles.init;
        linkunitofiles.done;
        linkunitofiles.init;
        linkunitstaticlibs.done;
        linkunitstaticlibs.init;
        linkunitsharedlibs.done;
        linkunitsharedlibs.init;
        linkotherofiles.done;
        linkotherofiles.init;
        linkotherstaticlibs.done;
        linkotherstaticlibs.init;
        linkothersharedlibs.done;
        linkothersharedlibs.init;
        uses_imports:=false;
        do_assemble:=false;
        do_compile:=false;
        { sources_avail:=true;
        should not be changed PM }
        compiled:=false;
        in_implementation:=false;
        in_global:=true;
        {loaded_from:=nil;
        should not be changed PFV }
        flags:=0;
        crc:=0;
        interface_crc:=0;
        unitcount:=1;
        recompile_reason:=rr_unknown;
      end;


    constructor tmodule.init(const s:string;_is_unit:boolean);
      var
        p : dirstr;
        n : namestr;
        e : extstr;
      begin
        FSplit(s,p,n,e);
      { Programs have the name program to don't conflict with dup id's }
        if _is_unit then
{$ifdef UNITALIASES}
          modulename:=stringdup(GetUnitAlias(Upper(n)))
{$else}
          modulename:=stringdup(Upper(n))
{$endif}
        else
          modulename:=stringdup('PROGRAM');
        mainsource:=stringdup(s);
        ppufilename:=nil;
        objfilename:=nil;
        asmfilename:=nil;
        staticlibfilename:=nil;
        sharedlibfilename:=nil;
        exefilename:=nil;
        { Dos has the famous 8.3 limit :( }
{$ifdef SHORTASMPREFIX}
        asmprefix:=stringdup(FixFileName('as'));
{$else}
        asmprefix:=stringdup(FixFileName(n));
{$endif}
        outputpath:=nil;
        path:=nil;
        setfilename(p+n,true);
        localunitsearchpath.init;
        localobjectsearchpath.init;
        localincludesearchpath.init;
        locallibrarysearchpath.init;
        used_units.init;
        dependent_units.init;
        new(sourcefiles,init);
        resourcefiles.init;
        linkunitofiles.init;
        linkunitstaticlibs.init;
        linkunitsharedlibs.init;
        linkotherofiles.init;
        linkotherstaticlibs.init;
        linkothersharedlibs.init;
        ppufile:=nil;
        scanner:=nil;
        map:=nil;
        globalsymtable:=nil;
        localsymtable:=nil;
        loaded_from:=nil;
        flags:=0;
        crc:=0;
        interface_crc:=0;
        do_reload:=false;
        unitcount:=1;
        inc(global_unit_count);
        unit_index:=global_unit_count;
        do_assemble:=false;
        do_compile:=false;
        sources_avail:=true;
        sources_checked:=false;
        compiled:=false;
        recompile_reason:=rr_unknown;
        in_second_load:=false;
        in_compile:=false;
        in_second_compile:=false;
        in_implementation:=false;
        in_global:=true;
        is_unit:=_is_unit;
        islibrary:=false;
        uses_imports:=false;
        imports:=new(plinkedlist,init);
        _exports:=new(plinkedlist,init);
      { search the PPU file if it is an unit }
        if is_unit then
         begin
           search_unit(modulename^,false);
           { it the sources_available is changed then we know that
             the sources aren't available }
           if not sources_avail then
            sources_checked:=true;
         end;
      end;


    destructor tmodule.done;
{$ifdef MEMDEBUG}
      var
        d : tmemdebug;
{$endif}
      begin
        if assigned(map) then
         dispose(map);
        if assigned(ppufile) then
         dispose(ppufile,done);
        ppufile:=nil;
        if assigned(imports) then
         dispose(imports,done);
        imports:=nil;
        if assigned(_exports) then
         dispose(_exports,done);
        _exports:=nil;
        if assigned(scanner) then
          pscannerfile(scanner)^.invalid:=true;
        if assigned(sourcefiles) then
         dispose(sourcefiles,done);
        sourcefiles:=nil;
        used_units.done;
        dependent_units.done;
        resourcefiles.done;
        linkunitofiles.done;
        linkunitstaticlibs.done;
        linkunitsharedlibs.done;
        linkotherofiles.done;
        linkotherstaticlibs.done;
        linkothersharedlibs.done;
        stringdispose(objfilename);
        stringdispose(asmfilename);
        stringdispose(ppufilename);
        stringdispose(staticlibfilename);
        stringdispose(sharedlibfilename);
        stringdispose(exefilename);
        stringdispose(outputpath);
        stringdispose(path);
        stringdispose(modulename);
        stringdispose(mainsource);
        stringdispose(asmprefix);
        localunitsearchpath.done;
        localobjectsearchpath.done;
        localincludesearchpath.done;
        locallibrarysearchpath.done;
{$ifdef MEMDEBUG}
        d.init('symtable');
{$endif}
        if assigned(globalsymtable) then
          dispose(punitsymtable(globalsymtable),done);
        globalsymtable:=nil;
        if assigned(localsymtable) then
          dispose(punitsymtable(localsymtable),done);
        localsymtable:=nil;
{$ifdef MEMDEBUG}
        d.done;
{$endif}
        inherited done;
      end;


{****************************************************************************
                              TUSED_UNIT
 ****************************************************************************}

    constructor tused_unit.init(_u : pmodule;intface:boolean);
      begin
        u:=_u;
        in_interface:=intface;
        in_uses:=false;
        is_stab_written:=false;
        loaded:=true;
        name:=stringdup(_u^.modulename^);
        checksum:=_u^.crc;
        interface_checksum:=_u^.interface_crc;
        unitid:=0;
      end;


    constructor tused_unit.init_to_load(const n:string;c,intfc:longint;intface:boolean);
      begin
        u:=nil;
        in_interface:=intface;
        in_uses:=false;
        is_stab_written:=false;
        loaded:=false;
        name:=stringdup(n);
        checksum:=c;
        interface_checksum:=intfc;
        unitid:=0;
      end;


    destructor tused_unit.done;
      begin
        stringdispose(name);
        inherited done;
      end;


{****************************************************************************
                            TDENPENDENT_UNIT
 ****************************************************************************}

    constructor tdependent_unit.init(_u : pmodule);
      begin
         u:=_u;
      end;

end.
{
  $Log$
  Revision 1.2  2000-09-24 15:06:16  peter
    * use defines.inc

  Revision 1.1  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

}
