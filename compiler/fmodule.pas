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
  {$define SHORTASMprefix}
{$endif}
{$ifdef go32v2}
  {$define SHORTASMprefix}
{$endif}
{$ifdef OS2}
  { Allthough OS/2 supports long filenames I play it safe and
    use 8.3 filenames, because this allows the compiler to run
    on a FAT partition. (DM) }
  {$define SHORTASMprefix}
{$endif}

interface

    uses
       cutils,cclasses,
       globals,ppu,finput,
       symbase;

    const
       maxunits = 1024;

    type
       trecompile_reason = (rr_unknown,
         rr_noppu,rr_sourcenewer,rr_build,rr_crcchanged
       );

       TExternalsItem=class(TLinkedListItem)
       public
         found : longbool;
         data  : pstring;
         constructor Create(const s:string);
         Destructor Destroy;override;
       end;

       tlinkcontaineritem=class(tlinkedlistitem)
       public
          data : pstring;
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

{$ifndef NEWMAP}
       tunitmap = array[0..maxunits-1] of pointer;
       punitmap = ^tunitmap;
{$else NEWMAP}
       tunitmap = array[0..maxunits-1] of tmodule;
       punitmap = ^tunitmap;
{$endif NEWMAP}

       tmodule = class(tmodulebase)
          ppufile       : pppufile; { the PPU file }
          crc,
          interface_crc,
          flags         : longint;  { the PPU flags }

          compiled,                 { unit is already compiled }
          do_reload,                { force reloading of the unit }
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
          unitcount     : longint;  { local unit counter }
          globalsymtable,           { pointer to the global symtable of this unit }
          localsymtable : tsymtable;{ pointer to the local symtable of this unit }
          scanner       : pointer;  { scanner object used }
          loaded_from   : tmodule;
          uses_imports  : boolean;  { Set if the module imports from DLL's.}
          imports       : tlinkedlist;
          _exports      : tlinkedlist;
          externals     : tlinkedlist; {Only for DLL scanners by using Unix-style $LINKLIB }
          resourcefiles : tstringlist;

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

          asmprefix     : pstring;  { prefix for the smartlink asmfiles }
{$ifdef Test_Double_checksum}
          crc_array : pointer;
          crc_size : longint;
          crc_array2 : pointer;
          crc_size2 : longint;
{$endif def Test_Double_checksum}
          constructor create(const s:string;_is_unit:boolean);
          destructor destroy;override;
          procedure reset;
          procedure setfilename(const fn:string;allowoutput:boolean);
          function  openppu:boolean;
          function  search_unit(const n : string;onlysource:boolean):boolean;
       end;

       tused_unit = class(tlinkedlistitem)
          unitid          : longint;
          name            : pstring;
          checksum,
          interface_checksum : longint;
          loaded          : boolean;
          in_uses,
          in_interface,
          is_stab_written : boolean;
          u               : tmodule;
          constructor create(_u : tmodule;intface:boolean);
          constructor create_to_load(const n:string;c,intfc:longint;intface:boolean);
          destructor destroy;override;
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
       SmartLinkOFiles   : TStringList; { List of .o files which are generated,
                                          used to delete them after linking }

function get_source_file(moduleindex,fileindex : longint) : tinputfile;


implementation

uses
{$ifdef delphi}
  dmisc,
{$else}
  dos,
{$endif}
  globtype,verbose,systems,
  scanner;


{*****************************************************************************
                             Global Functions
*****************************************************************************}

    function get_source_file(moduleindex,fileindex : longint) : tinputfile;
      var
         hp : tmodule;
      begin
         hp:=tmodule(loaded_units.first);
         while assigned(hp) and (hp.unit_index<>moduleindex) do
           hp:=tmodule(hp.next);
         if assigned(hp) then
          get_source_file:=hp.sourcefiles.get_file(fileindex)
         else
          get_source_file:=nil;
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
                              TExternalsItem
 ****************************************************************************}

    constructor tExternalsItem.Create(const s:string);
      begin
        inherited Create;
        found:=false;
        data:=stringdup(s);
      end;


    destructor tExternalsItem.Destroy;
      begin
        stringdispose(data);
        inherited;
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
         staticlibfilename:=stringdup(p+target_info.libprefix+n+target_info.staticlibext);
         if target_info.target=target_i386_WIN32 then
           sharedlibfilename:=stringdup(p+n+target_info.sharedlibext)
         else
           sharedlibfilename:=stringdup(p+target_info.libprefix+n+target_info.sharedlibext);
         { output dir of exe can be specified separatly }
         if AllowOutput and (OutputExeDir<>'') then
          p:=OutputExeDir
         else
          p:=path^;
         exefilename:=stringdup(p+n+target_info.exeext);
      end;


    function tmodule.openppu:boolean;
      var
        ppufiletime : longint;
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
        ppufile^.change_endian:=source_info.endian<>target_info.endian;
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
        do_compile:=false;
        openppu:=true;
      end;


    function tmodule.search_unit(const n : string;onlysource:boolean):boolean;
      var
         singlepathstring,
         filename : string;

         Function UnitExists(const ext:string;var foundfile:string):boolean;
         begin
           Message1(unit_t_unitsearch,Singlepathstring+filename+ext);
           UnitExists:=FindFile(FileName+ext,Singlepathstring,foundfile);
         end;

         Function PPUSearchPath(const s:string):boolean;
         var
           found : boolean;
           hs    : string;
         begin
           Found:=false;
           singlepathstring:=FixPath(s,false);
         { Check for PPU file }
           Found:=UnitExists(target_info.unitext,hs);
           if Found then
            Begin
              SetFileName(hs,false);
              Found:=OpenPPU;
            End;
           PPUSearchPath:=Found;
         end;

         Function SourceSearchPath(const s:string):boolean;
         var
           found   : boolean;
           hs      : string;
         begin
           Found:=false;
           singlepathstring:=FixPath(s,false);
         { Check for Sources }
           ppufile:=nil;
           do_compile:=true;
           recompile_reason:=rr_noppu;
         {Check for .pp file}
           Found:=UnitExists(target_info.sourceext,hs);
           if not Found then
            begin
              { Check for .pas }
              Found:=UnitExists(target_info.pasext,hs);
            end;
           stringdispose(mainsource);
           if Found then
            begin
              sources_avail:=true;
              { Load Filenames when found }
              mainsource:=StringDup(hs);
              SetFileName(hs,false);
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
           hp : TStringListItem;
           found : boolean;
         begin
           found:=false;
           hp:=TStringListItem(list.First);
           while assigned(hp) do
            begin
              found:=SearchPath(hp.Str);
              if found then
               break;
              hp:=TStringListItem(hp.next);
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
            if (not fnd) and (current_module.outputpath^<>'') then
             fnd:=PPUSearchPath(current_module.outputpath^);
           end;
         if (not fnd) then
          fnd:=SourceSearchPath('.');
         if (not fnd) then
          fnd:=SearchPathList(current_module.LocalUnitSearchPath);
         if (not fnd) then
          fnd:=SearchPathList(UnitSearchPath);

         { try to find a file with the first 8 chars of the modulename, like
           dos }
         if (not fnd) and (length(filename)>8) then
          begin
            filename:=copy(filename,1,8);
            fnd:=SearchPath('.');
            if (not fnd) then
             fnd:=SearchPathList(current_module.LocalUnitSearchPath);
            if not fnd then
             fnd:=SearchPathList(UnitSearchPath);
          end;
         search_unit:=fnd;
      end;



    procedure tmodule.reset;
      var
         pm : tdependent_unit;
      begin
        if assigned(scanner) then
          tscannerfile(scanner).invalid:=true;
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
        sourcefiles.free;
        sourcefiles:=tinputfilemanager.create;
        imports.free;
        imports:=tlinkedlist.create;
        _exports.free;
        _exports:=tlinkedlist.create;
        externals.free;
        externals:=tlinkedlist.create;
        used_units.free;
        used_units:=TLinkedList.Create;
        { all units that depend on this one must be recompiled ! }
        pm:=tdependent_unit(dependent_units.first);
        while assigned(pm) do
          begin
            if pm.u.in_second_compile then
             Comment(v_debug,'No reload already in second compile: '+pm.u.modulename^)
            else
             begin
               pm.u.do_reload:=true;
               Comment(v_debug,'Reloading '+pm.u.modulename^+' needed because '+modulename^+' is reloaded');
             end;
            pm:=tdependent_unit(pm.next);
          end;
        dependent_units.free;
        dependent_units:=TLinkedList.Create;
        resourcefiles.Free;
        resourcefiles:=TStringList.Create;
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
        uses_imports:=false;
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


    constructor tmodule.create(const s:string;_is_unit:boolean);
      var
        p : dirstr;
        n : namestr;
        e : extstr;
      begin
        FSplit(s,p,n,e);
        { Programs have the name 'Program' to don't conflict with dup id's }
        if _is_unit then
         inherited create(n)
        else
         inherited create('Program');
        mainsource:=stringdup(s);
        { Dos has the famous 8.3 limit :( }
{$ifdef SHORTASMprefix}
        asmprefix:=stringdup(FixFileName('as'));
{$else}
        asmprefix:=stringdup(FixFileName(n));
{$endif}
        setfilename(p+n,true);
        localunitsearchpath:=TSearchPathList.Create;
        localobjectsearchpath:=TSearchPathList.Create;
        localincludesearchpath:=TSearchPathList.Create;
        locallibrarysearchpath:=TSearchPathList.Create;
        used_units:=TLinkedList.Create;
        dependent_units:=TLinkedList.Create;
        resourcefiles:=TStringList.Create;
        linkunitofiles:=TLinkContainer.Create;
        linkunitstaticlibs:=TLinkContainer.Create;
        linkunitsharedlibs:=TLinkContainer.Create;
        linkotherofiles:=TLinkContainer.Create;
        linkotherstaticlibs:=TLinkContainer.Create;
        linkothersharedlibs:=TLinkContainer.Create;
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
        imports:=TLinkedList.Create;
        _exports:=TLinkedList.Create;
        externals:=TLinkedList.Create;
      { search the PPU file if it is an unit }
        if is_unit then
         begin
           { use the realmodulename so we can also find a case sensitive
             source filename }
           search_unit(realmodulename^,false);
           { it the sources_available is changed then we know that
             the sources aren't available }
           if not sources_avail then
            sources_checked:=true;
         end;
      end;


    destructor tmodule.Destroy;
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
         imports.free;
        imports:=nil;
        if assigned(_exports) then
         _exports.free;
        _exports:=nil;
        if assigned(externals) then
         externals.free;
        externals:=nil;
        if assigned(scanner) then
          tscannerfile(scanner).invalid:=true;
        used_units.free;
        dependent_units.free;
        resourcefiles.Free;
        linkunitofiles.Free;
        linkunitstaticlibs.Free;
        linkunitsharedlibs.Free;
        linkotherofiles.Free;
        linkotherstaticlibs.Free;
        linkothersharedlibs.Free;
        stringdispose(objfilename);
        stringdispose(asmfilename);
        stringdispose(ppufilename);
        stringdispose(staticlibfilename);
        stringdispose(sharedlibfilename);
        stringdispose(exefilename);
        stringdispose(outputpath);
        stringdispose(path);
        stringdispose(modulename);
        stringdispose(realmodulename);
        stringdispose(mainsource);
        stringdispose(asmprefix);
        localunitsearchpath.Free;
        localobjectsearchpath.free;
        localincludesearchpath.free;
        locallibrarysearchpath.free;
{$ifdef MEMDEBUG}
        d.init('symtable');
{$endif}
        if assigned(globalsymtable) then
          globalsymtable.free;
        globalsymtable:=nil;
        if assigned(localsymtable) then
          localsymtable.free;
        localsymtable:=nil;
{$ifdef MEMDEBUG}
        d.free;
{$endif}
        inherited Destroy;
      end;


{****************************************************************************
                              TUSED_UNIT
 ****************************************************************************}

    constructor tused_unit.create(_u : tmodule;intface:boolean);
      begin
        u:=_u;
        in_interface:=intface;
        in_uses:=false;
        is_stab_written:=false;
        loaded:=true;
        name:=stringdup(_u.modulename^);
        checksum:=_u.crc;
        interface_checksum:=_u.interface_crc;
        unitid:=0;
      end;


    constructor tused_unit.create_to_load(const n:string;c,intfc:longint;intface:boolean);
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


    destructor tused_unit.destroy;
      begin
        stringdispose(name);
        inherited destroy;
      end;


{****************************************************************************
                            TDENPENDENT_UNIT
 ****************************************************************************}

    constructor tdependent_unit.create(_u : tmodule);
      begin
         u:=_u;
      end;

end.
{
  $Log$
  Revision 1.13  2001-04-18 22:01:53  peter
    * registration of targets and assemblers

  Revision 1.12  2001/04/13 18:08:37  peter
    * scanner object to class

  Revision 1.11  2001/04/13 01:22:07  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.10  2001/04/02 21:20:29  peter
    * resulttype rewrite

  Revision 1.9  2001/03/13 18:45:06  peter
    * fixed some memory leaks

  Revision 1.8  2001/03/06 18:28:02  peter
    * patch from Pavel with a new and much faster DLL Scanner for
      automatic importing so $linklib works for DLLs. Thanks Pavel!

  Revision 1.7  2001/02/20 21:41:15  peter
    * new fixfilename, findfile for unix. Look first for lowercase, then
      NormalCase and last for UPPERCASE names.

  Revision 1.6  2000/12/25 00:07:25  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.5  2000/11/07 20:48:33  peter
    * removed ref_count from pinputfile it's not used

  Revision 1.4  2000/10/31 22:02:46  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/15 07:47:51  peter
    * unit names and procedure names are stored mixed case

  Revision 1.2  2000/09/24 15:06:16  peter
    * use defines.inc

  Revision 1.1  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

}
