{
    $Id$
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
unit fppu;

{$i fpcdefs.inc}

{ close ppufiles on system that are
  short on file handles like DOS system PM }
{$ifdef GO32V2}
  {$define SHORT_ON_FILE_HANDLES}
{$endif GO32V2}

interface

    uses
       cutils,cclasses,
       globtype,globals,finput,fmodule,
       symbase,symppu,ppu;

    type
       tppumodule = class(tmodule)
          ppufile    : tcompilerppufile; { the PPU file }
{$ifdef Test_Double_checksum}
          crc_array  : pointer;
          crc_size   : longint;
          crc_array2 : pointer;
          crc_size2  : longint;
{$endif def Test_Double_checksum}
          constructor create(const s:string;const fn:string;_is_unit:boolean);
          destructor destroy;override;
          procedure reset;override;
          function  openppu:boolean;
          function  search_unit(const n : string;const fn:string;onlysource:boolean):boolean;
          procedure getppucrc;
          procedure writeppu;
          procedure loadppu;
       private
          procedure load_interface;
          procedure load_implementation;
          procedure load_symtable_refs;
          procedure load_usedunits;
          procedure writeusedmacro(p:TNamedIndexItem;arg:pointer);
          procedure writeusedmacros;
          procedure writesourcefiles;
          procedure writeusedunit;
          procedure writelinkcontainer(var p:tlinkcontainer;id:byte;strippath:boolean);
          procedure putasmsymbol_in_idx(s:tnamedindexitem;arg:pointer);
          procedure writeasmsymbols;
          procedure readusedmacros;
          procedure readsourcefiles;
          procedure readloadunit;
          procedure readlinkcontainer(var p:tlinkcontainer);
          procedure readasmsymbols;
       end;


    function loadunit(const s : stringid;const fn:string) : tmodule;


implementation

uses
  verbose,systems,version,
  symtable,
  scanner,
  aasmbase,
  parser;


{****************************************************************************
                                TPPUMODULE
 ****************************************************************************}

    constructor tppumodule.create(const s:string;const fn:string;_is_unit:boolean);
      begin
        inherited create(s,_is_unit);
        ppufile:=nil;
      { search the PPU file if it is an unit }
        if is_unit then
         begin
           { use the realmodulename so we can also find a case sensitive
             source filename }
           search_unit(realmodulename^,fn,false);
           { it the sources_available is changed then we know that
             the sources aren't available }
           if not sources_avail then
            sources_checked:=true;
         end;
      end;


    destructor tppumodule.Destroy;
      begin
        if assigned(ppufile) then
         ppufile.free;
        ppufile:=nil;
        inherited Destroy;
      end;


    procedure tppumodule.reset;
      begin
        if assigned(ppufile) then
         begin
           ppufile.free;
           ppufile:=nil;
         end;
        inherited reset;
      end;


    function tppumodule.openppu:boolean;
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
        ppufile:=tcompilerppufile.create(ppufilename^);
        if not ppufile.openfile then
         begin
           ppufile.free;
           ppufile:=nil;
           Message(unit_u_ppu_file_too_short);
           exit;
         end;
      { check for a valid PPU file }
        if not ppufile.CheckPPUId then
         begin
           ppufile.free;
           ppufile:=nil;
           Message(unit_u_ppu_invalid_header);
           exit;
         end;
      { check for allowed PPU versions }
        if not (ppufile.GetPPUVersion = CurrentPPUVersion) then
         begin
           Message1(unit_u_ppu_invalid_version,tostr(ppufile.GetPPUVersion));
           ppufile.free;
           ppufile:=nil;
           exit;
         end;
      { check the target processor }
        if tsystemcpu(ppufile.header.cpu)<>target_cpu then
         begin
           ppufile.free;
           ppufile:=nil;
           Message(unit_u_ppu_invalid_processor);
           exit;
         end;
      { check target }
        if tsystem(ppufile.header.target)<>target_info.system then
         begin
           ppufile.free;
           ppufile:=nil;
           Message(unit_u_ppu_invalid_target);
           exit;
         end;
       { check if floating point emulation is on?}
        if ((ppufile.header.flags and uf_fpu_emulation)<>0) and 
            (cs_fp_emulation in aktmoduleswitches) then
         begin
           ppufile.free;
           ppufile:=nil;
           Message(unit_u_ppu_invalid_fpumode);
           exit;
         end;
       
      { Load values to be access easier }
        flags:=ppufile.header.flags;
        crc:=ppufile.header.checksum;
        interface_crc:=ppufile.header.interface_checksum;
      { Show Debug info }
        Message1(unit_u_ppu_time,filetimestring(ppufiletime));
        Message1(unit_u_ppu_flags,tostr(flags));
        Message1(unit_u_ppu_crc,hexstr(ppufile.header.checksum,8));
        Message1(unit_u_ppu_crc,hexstr(ppufile.header.interface_checksum,8)+' (intfc)');
        do_compile:=false;
        openppu:=true;
      end;


    function tppumodule.search_unit(const n : string;const fn:string;onlysource:boolean):boolean;
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
         hs  : string;
       begin
         filename:=FixFileName(n);
         { try to find unit
            1. look for ppu in cwd
            2. look for ppu in outputpath if set, this is tp7 compatible (PFV)
            3. look for the specified source file (from the uses line)
            4. look for source in cwd
            5. local unit pathlist
            6. global unit pathlist }
         fnd:=false;
         if not onlysource then
          begin
            fnd:=PPUSearchPath('.');
            if (not fnd) and (outputpath^<>'') then
             fnd:=PPUSearchPath(outputpath^);
           end;
         if (not fnd) and (fn<>'') then
          begin
            { the full filename is specified so we can't use here the
              searchpath (PFV) }
            Message1(unit_t_unitsearch,AddExtension(fn,target_info.sourceext));
            fnd:=FindFile(AddExtension(fn,target_info.sourceext),'',hs);
            if not fnd then
             begin
               Message1(unit_t_unitsearch,AddExtension(fn,target_info.pasext));
               fnd:=FindFile(AddExtension(fn,target_info.pasext),'',hs);
             end;
            if fnd then
             begin
               sources_avail:=true;
               do_compile:=true;
               recompile_reason:=rr_noppu;
               stringdispose(mainsource);
               mainsource:=StringDup(hs);
               SetFileName(hs,false);
             end;
          end;
         if (not fnd) then
          fnd:=SourceSearchPath('.');
         if (not fnd) then
          fnd:=SearchPathList(LocalUnitSearchPath);
         if (not fnd) then
          fnd:=SearchPathList(UnitSearchPath);

         { try to find a file with the first 8 chars of the modulename, like
           dos }
         if (not fnd) and (length(filename)>8) then
          begin
            filename:=copy(filename,1,8);
            fnd:=SearchPath('.');
            if (not fnd) then
             fnd:=SearchPathList(LocalUnitSearchPath);
            if not fnd then
             fnd:=SearchPathList(UnitSearchPath);
          end;
         search_unit:=fnd;
      end;


{**********************************
    PPU Reading/Writing Helpers
***********************************}

    procedure tppumodule.writeusedmacro(p:TNamedIndexItem;arg:pointer);
      begin
        if tmacro(p).is_used or tmacro(p).defined_at_startup then
          begin
            ppufile.putstring(p.name);
            ppufile.putbyte(byte(tmacro(p).defined_at_startup));
            ppufile.putbyte(byte(tmacro(p).is_used));
          end;
      end;


    procedure tppumodule.writeusedmacros;
      begin
        ppufile.do_crc:=false;
        tscannerfile(scanner).macros.foreach({$ifdef FPCPROCVAR}@{$endif}writeusedmacro,nil);
        ppufile.writeentry(ibusedmacros);
        ppufile.do_crc:=true;
      end;


    procedure tppumodule.writesourcefiles;
      var
        hp  : tinputfile;
        i,j : longint;
      begin
      { second write the used source files }
        ppufile.do_crc:=false;
        hp:=sourcefiles.files;
      { write source files directly in good order }
        j:=0;
        while assigned(hp) do
          begin
            inc(j);
            hp:=hp.ref_next;
          end;
        while j>0 do
          begin
            hp:=sourcefiles.files;
            for i:=1 to j-1 do
              hp:=hp.ref_next;
            ppufile.putstring(hp.name^);
            dec(j);
         end;
        ppufile.writeentry(ibsourcefiles);
        ppufile.do_crc:=true;
      end;


    procedure tppumodule.writeusedunit;
      var
        hp : tused_unit;
      begin
        { renumber the units for derefence writing }
        numberunits;
        { write a reference for each used unit }
        hp:=tused_unit(used_units.first);
        while assigned(hp) do
         begin
           { implementation units should not change
             the CRC PM }
           ppufile.do_crc:=hp.in_interface;
           ppufile.putstring(hp.realname^);
           { the checksum should not affect the crc of this unit ! (PFV) }
           ppufile.do_crc:=false;
           ppufile.putlongint(longint(hp.checksum));
           ppufile.putlongint(longint(hp.interface_checksum));
           ppufile.putbyte(byte(hp.in_interface));
           ppufile.do_crc:=true;
           hp:=tused_unit(hp.next);
         end;
        ppufile.do_interface_crc:=true;
        ppufile.writeentry(ibloadunit);
      end;


    procedure tppumodule.writelinkcontainer(var p:tlinkcontainer;id:byte;strippath:boolean);
      var
        hcontainer : tlinkcontainer;
        s : string;
        mask : cardinal;
      begin
        hcontainer:=TLinkContainer.Create;
        while not p.empty do
         begin
           s:=p.get(mask);
           if strippath then
            ppufile.putstring(SplitFileName(s))
           else
            ppufile.putstring(s);
           ppufile.putlongint(mask);
           hcontainer.add(s,mask);
         end;
        ppufile.writeentry(id);
        p.Free;
        p:=hcontainer;
      end;


    procedure tppumodule.putasmsymbol_in_idx(s:tnamedindexitem;arg:pointer);
      begin
        if tasmsymbol(s).ppuidx<>-1 then
         librarydata.asmsymbolidx^[tasmsymbol(s).ppuidx]:=tasmsymbol(s);
      end;


    procedure tppumodule.writeasmsymbols;
      var
        s : tasmsymbol;
        i : longint;
      begin
        { get an ordered list of all symbols to put in the ppu }
        getmem(librarydata.asmsymbolidx,librarydata.asmsymbolppuidx*sizeof(pointer));
        librarydata.symbolsearch.foreach({$ifdef FPCPROCVAR}@{$endif}putasmsymbol_in_idx,nil);
        { write the number of symbols }
        ppufile.putlongint(librarydata.asmsymbolppuidx);
        { write the symbols from the indexed list to the ppu }
        for i:=0 to librarydata.asmsymbolppuidx-1 do
         begin
           s:=librarydata.asmsymbolidx^[i];
           if not assigned(s) then
            internalerror(200208071);
           ppufile.putstring(s.name);
           ppufile.putbyte(byte(s.defbind));
           ppufile.putbyte(byte(s.typ));
         end;
        ppufile.writeentry(ibasmsymbols);
      end;


    procedure tppumodule.readusedmacros;
      var
        hs : string;
        mac : tmacro;
        was_defined_at_startup,
        was_used : boolean;
      begin
        { only possible when we've a scanner of the current file }
        if not assigned(current_scanner) then
         exit;
        while not ppufile.endofentry do
         begin
           hs:=ppufile.getstring;
           was_defined_at_startup:=boolean(ppufile.getbyte);
           was_used:=boolean(ppufile.getbyte);
           mac:=tmacro(tscannerfile(current_scanner).macros.search(hs));
           if assigned(mac) then
             begin
{$ifndef EXTDEBUG}
           { if we don't have the sources why tell }
              if sources_avail then
{$endif ndef EXTDEBUG}
               if (not was_defined_at_startup) and
                  was_used and
                  mac.defined_at_startup then
                Message2(unit_h_cond_not_set_in_last_compile,hs,mainsource^);
             end
           else { not assigned }
             if was_defined_at_startup and
                was_used then
              Message2(unit_h_cond_not_set_in_last_compile,hs,mainsource^);
         end;
      end;


    procedure tppumodule.readsourcefiles;
      var
        temp,hs       : string;
        temp_dir      : string;
        main_dir      : string;
        incfile_found,
        main_found,
        is_main       : boolean;
        ppufiletime,
        source_time   : longint;
        hp            : tinputfile;
      begin
        ppufiletime:=getnamedfiletime(ppufilename^);
        sources_avail:=true;
        is_main:=true;
        main_dir:='';
        while not ppufile.endofentry do
         begin
           hs:=ppufile.getstring;
           temp_dir:='';
           if (flags and uf_in_library)<>0 then
            begin
              sources_avail:=false;
              temp:=' library';
            end
           else if pos('Macro ',hs)=1 then
            begin
              { we don't want to find this file }
              { but there is a problem with file indexing !! }
              temp:='';
            end
           else
            begin
              { check the date of the source files }
              Source_Time:=GetNamedFileTime(path^+hs);
              incfile_found:=false;
              main_found:=false;
              if Source_Time<>-1 then
                hs:=path^+hs
              else
               if not(is_main) then
                begin
                  Source_Time:=GetNamedFileTime(main_dir+hs);
                  if Source_Time<>-1 then
                    hs:=main_dir+hs;
                end;
              if (Source_Time=-1) then
                begin
                  if is_main then
                    main_found:=unitsearchpath.FindFile(hs,temp_dir)
                  else
                    incfile_found:=includesearchpath.FindFile(hs,temp_dir);
                  if incfile_found or main_found then
                   begin
                     Source_Time:=GetNamedFileTime(temp_dir);
                     if Source_Time<>-1 then
                      hs:=temp_dir;
                   end;
                end;
              if Source_Time=-1 then
               begin
                 sources_avail:=false;
                 temp:=' not found';
               end
              else
               begin
                 if main_found then
                   main_dir:=temp_dir;
                 { time newer? But only allow if the file is not searched
                   in the include path (PFV), else you've problems with
                   units which use the same includefile names }
                 if incfile_found then
                  temp:=' found'
                 else
                  begin
                    temp:=' time '+filetimestring(source_time);
                    if (source_time>ppufiletime) then
                     begin
                       if {is_main or} ((flags and uf_release)=0) then
                        begin
                          do_compile:=true;
                          recompile_reason:=rr_sourcenewer;
                        end
                       else
                        Message2(unit_h_source_modified,hs,ppufilename^);
                       temp:=temp+' *';
                     end;
                  end;
               end;
              hp:=tinputfile.create(hs);
              { the indexing is wrong here PM }
              sourcefiles.register_file(hp);
            end;
           if is_main then
             begin
               stringdispose(mainsource);
               mainsource:=stringdup(hs);
             end;
           Message1(unit_u_ppu_source,hs+temp);
           is_main:=false;
         end;
      { check if we want to rebuild every unit, only if the sources are
        available }
        if do_build and sources_avail and
           ((flags and uf_release)=0) then
          begin
             do_compile:=true;
             recompile_reason:=rr_build;
          end;
      end;


    procedure tppumodule.readloadunit;
      var
        hs : string;
        intfchecksum,
        checksum : cardinal;
        in_interface : boolean;
      begin
        while not ppufile.endofentry do
         begin
           hs:=ppufile.getstring;
           checksum:=cardinal(ppufile.getlongint);
           intfchecksum:=cardinal(ppufile.getlongint);
           in_interface:=(ppufile.getbyte<>0);
           used_units.concat(tused_unit.create_to_load(hs,checksum,intfchecksum,in_interface));
         end;
      end;


    procedure tppumodule.readlinkcontainer(var p:tlinkcontainer);
      var
        s : string;
        m : longint;
      begin
        while not ppufile.endofentry do
         begin
           s:=ppufile.getstring;
           m:=ppufile.getlongint;
           p.add(s,m);
         end;
      end;


    procedure tppumodule.readasmsymbols;
      var
        i     : longint;
        name  : string;
        bind  : TAsmSymBind;
        typ   : TAsmSymType;
      begin
        librarydata.asmsymbolppuidx:=ppufile.getlongint;
        if librarydata.asmsymbolppuidx>0 then
         begin
           getmem(librarydata.asmsymbolidx,librarydata.asmsymbolppuidx*sizeof(pointer));
           for i:=0 to librarydata.asmsymbolppuidx-1 do
            begin
              name:=ppufile.getstring;
              bind:=tasmsymbind(ppufile.getbyte);
              typ:=tasmsymtype(ppufile.getbyte);
              librarydata.asmsymbolidx^[i]:=librarydata.newasmsymboltype(name,bind,typ);
            end;
         end;
      end;


    procedure tppumodule.load_interface;
      var
        b : byte;
        newmodulename : string;
      begin
       { read interface part }
         repeat
           b:=ppufile.readentry;
           case b of
             ibmodulename :
               begin
                 newmodulename:=ppufile.getstring;
                 if (cs_check_unit_name in aktglobalswitches) and
                    (upper(newmodulename)<>modulename^) then
                   Message2(unit_f_unit_name_error,realmodulename^,newmodulename);
                 stringdispose(modulename);
                 stringdispose(realmodulename);
                 modulename:=stringdup(upper(newmodulename));
                 realmodulename:=stringdup(newmodulename);
               end;
             ibsourcefiles :
               readsourcefiles;
             ibusedmacros :
               readusedmacros;
             ibloadunit :
               readloadunit;
             iblinkunitofiles :
               readlinkcontainer(LinkUnitOFiles);
             iblinkunitstaticlibs :
               readlinkcontainer(LinkUnitStaticLibs);
             iblinkunitsharedlibs :
               readlinkcontainer(LinkUnitSharedLibs);
             iblinkotherofiles :
               readlinkcontainer(LinkotherOFiles);
             iblinkotherstaticlibs :
               readlinkcontainer(LinkotherStaticLibs);
             iblinkothersharedlibs :
               readlinkcontainer(LinkotherSharedLibs);
             ibendinterface :
               break;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
      end;


    procedure tppumodule.load_implementation;
      var
        b : byte;
      begin
       { read interface part }
         repeat
           b:=ppufile.readentry;
           case b of
             ibasmsymbols :
               readasmsymbols;
             ibendimplementation :
               break;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;

         { we can now derefence all pointers to the objectdata }
         tstoredsymtable(globalsymtable).derefobjectdata;
         if assigned(localsymtable) then
           tstoredsymtable(localsymtable).derefobjectdata;
      end;


    procedure tppumodule.load_symtable_refs;
      var
         b : byte;
         unitindex : word;
      begin
        { load local symtable first }
        if ((flags and uf_local_browser)<>0) then
          begin
             localsymtable:=tstaticsymtable.create(modulename^);
             tstaticsymtable(localsymtable).load(ppufile);
          end;

        { load browser }
        if (flags and uf_has_browser)<>0 then
          begin
            tstoredsymtable(globalsymtable).load_references(ppufile,true);
            unitindex:=1;
            while assigned(map^[unitindex]) do
             begin
               { each unit wrote one browser entry }
               tstoredsymtable(globalsymtable).load_references(ppufile,false);
               inc(unitindex);
             end;
            b:=ppufile.readentry;
            if b<>ibendbrowser then
             Message1(unit_f_ppu_invalid_entry,tostr(b));
          end;
        if ((flags and uf_local_browser)<>0) then
          tstaticsymtable(localsymtable).load_references(ppufile,true);
      end;


    procedure tppumodule.writeppu;
      var
        pu : tused_unit;
      begin
         Message1(unit_u_ppu_write,realmodulename^);

         { create unit flags }
{$ifdef GDB}
         if cs_gdb_dbx in aktglobalswitches then
          flags:=flags or uf_has_dbx;
{$endif GDB}
         if cs_browser in aktmoduleswitches then
          flags:=flags or uf_has_browser;
         if cs_local_browser in aktmoduleswitches then
          flags:=flags or uf_local_browser;
         if do_release then
          flags:=flags or uf_release;
         if (cs_fp_emulation in aktmoduleswitches) then
           flags:=flags or uf_fpu_emulation;
{$ifdef Test_Double_checksum_write}
         Assign(CRCFile,s+'.IMP');
         Rewrite(CRCFile);
{$endif def Test_Double_checksum_write}

         { create new ppufile }
         ppufile:=tcompilerppufile.create(ppufilename^);
         if not ppufile.createfile then
          Message(unit_f_ppu_cannot_write);

         { first the unitname }
         ppufile.putstring(realmodulename^);
         ppufile.writeentry(ibmodulename);

         writesourcefiles;
         writeusedmacros;
         writeusedunit;

         { write the objectfiles and libraries that come for this unit,
           preserve the containers becuase they are still needed to load
           the link.res. All doesn't depend on the crc! It doesn't matter
           if a unit is in a .o or .a file }
         ppufile.do_crc:=false;
         writelinkcontainer(linkunitofiles,iblinkunitofiles,true);
         writelinkcontainer(linkunitstaticlibs,iblinkunitstaticlibs,true);
         writelinkcontainer(linkunitsharedlibs,iblinkunitsharedlibs,true);
         writelinkcontainer(linkotherofiles,iblinkotherofiles,false);
         writelinkcontainer(linkotherstaticlibs,iblinkotherstaticlibs,true);
         writelinkcontainer(linkothersharedlibs,iblinkothersharedlibs,true);
         ppufile.do_crc:=true;

         ppufile.writeentry(ibendinterface);

         { write the symtable entries }
         tstoredsymtable(globalsymtable).write(ppufile);

         { everything after this doesn't affect the crc }
         ppufile.do_crc:=false;

         { write asmsymbols }
         writeasmsymbols;

         { end of implementation }
         ppufile.writeentry(ibendimplementation);

         { write static symtable
           needed for local debugging of unit functions }
         if ((flags and uf_local_browser)<>0) and
            assigned(localsymtable) then
           tstoredsymtable(localsymtable).write(ppufile);

         { write all browser section }
         if (flags and uf_has_browser)<>0 then
          begin
            tstoredsymtable(globalsymtable).write_references(ppufile,true);
            pu:=tused_unit(used_units.first);
            while assigned(pu) do
             begin
               tstoredsymtable(pu.u.globalsymtable).write_references(ppufile,false);
               pu:=tused_unit(pu.next);
             end;
            ppufile.writeentry(ibendbrowser);
          end;
         if ((flags and uf_local_browser)<>0) and
            assigned(localsymtable) then
           tstaticsymtable(localsymtable).write_references(ppufile,true);

         { the last entry ibend is written automaticly }

         { flush to be sure }
         ppufile.flush;
         { create and write header }
         ppufile.header.size:=ppufile.size;
         ppufile.header.checksum:=ppufile.crc;
         ppufile.header.interface_checksum:=ppufile.interface_crc;
         ppufile.header.compiler:=wordversion;
         ppufile.header.cpu:=word(target_cpu);
         ppufile.header.target:=word(target_info.system);
         ppufile.header.flags:=flags;
         ppufile.writeheader;

         { save crc in current module also }
         crc:=ppufile.crc;
         interface_crc:=ppufile.interface_crc;

{$ifdef Test_Double_checksum_write}
         close(CRCFile);
{$endif Test_Double_checksum_write}

         ppufile.closefile;
         ppufile.free;
         ppufile:=nil;
      end;


    procedure tppumodule.getppucrc;
      begin
{$ifdef Test_Double_checksum_write}
         Assign(CRCFile,s+'.INT')
         Rewrite(CRCFile);
{$endif def Test_Double_checksum_write}

         { create new ppufile }
         ppufile:=tcompilerppufile.create(ppufilename^);
         ppufile.crc_only:=true;
         if not ppufile.createfile then
           Message(unit_f_ppu_cannot_write);

         { first the unitname }
         ppufile.putstring(realmodulename^);
         ppufile.writeentry(ibmodulename);

         { the interface units affect the crc }
         writeusedunit;

         ppufile.writeentry(ibendinterface);

         { write the symtable entries }
         tstoredsymtable(globalsymtable).write(ppufile);

         { save crc  }
         crc:=ppufile.crc;
         interface_crc:=ppufile.interface_crc;

{$ifdef Test_Double_checksum}
         crc_array:=ppufile.crc_test;
         ppufile.crc_test:=nil;
         crc_size:=ppufile.crc_index2;
         crc_array2:=ppufile.crc_test2;
         ppufile.crc_test2:=nil;
         crc_size2:=ppufile.crc_index2;
{$endif Test_Double_checksum}

{$ifdef Test_Double_checksum_write}
         close(CRCFile);
{$endif Test_Double_checksum_write}

         ppufile.closefile;
         ppufile.free;
         ppufile:=nil;
      end;


    procedure tppumodule.load_usedunits;
      var
        pu           : tused_unit;
        loaded_unit  : tmodule;
        load_refs    : boolean;
        nextmapentry : longint;
      begin
        load_refs:=true;
        { init the map }
        new(map);
        fillchar(map^,sizeof(tunitmap),#0);
{$ifdef NEWMAP}
        map^[0]:=current_module;
{$endif NEWMAP}
        nextmapentry:=1;
        { load the used units from interface }
        in_implementation:=false;
        pu:=tused_unit(used_units.first);
        while assigned(pu) do
         begin
           if (not pu.loaded) and (pu.in_interface) then
            begin
              loaded_unit:=loadunit(pu.realname^,'');
              if compiled then
               exit;
              { register unit in used units }
              pu.u:=loaded_unit;
              pu.loaded:=true;
              { doubles are not important for that list PM }
              pu.u.dependent_units.concat(tdependent_unit.create(self));
              { need to recompile the current unit ? }
              if loaded_unit.crc<>pu.checksum then
               begin
                 Message2(unit_u_recompile_crc_change,realmodulename^,pu.realname^);
                 recompile_reason:=rr_crcchanged;
                 do_compile:=true;
                 dispose(map);
                 map:=nil;
                 exit;
               end;
            { setup the map entry for deref }
{$ifndef NEWMAP}
              map^[nextmapentry]:=loaded_unit.globalsymtable;
{$else NEWMAP}
              map^[nextmapentry]:=loaded_unit;
{$endif NEWMAP}
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
            end;
           pu:=tused_unit(pu.next);
         end;
        { ok, now load the interface of this unit }
        current_module:=self;
        SetCompileModule(current_module);
        globalsymtable:=tglobalsymtable.create(modulename^);
        tstoredsymtable(globalsymtable).load(ppufile);
        { now only read the implementation uses }
        in_implementation:=true;
        pu:=tused_unit(used_units.first);
        while assigned(pu) do
         begin
           if (not pu.loaded) and (not pu.in_interface) then
            begin
              loaded_unit:=loadunit(pu.realname^,'');
              if compiled then
               exit;
            { register unit in used units }
              pu.u:=loaded_unit;
              pu.loaded:=true;
            { need to recompile the current unit ? }
              if (loaded_unit.interface_crc<>pu.interface_checksum) {and
                 not(current_module.in_second_compile) } then
                begin
                  Message2(unit_u_recompile_crc_change,realmodulename^,pu.realname^+' {impl}');
                  recompile_reason:=rr_crcchanged;
                  do_compile:=true;
                  dispose(map);
                  map:=nil;
                  exit;
                end;
            { setup the map entry for deref }
{$ifndef NEWMAP}
              map^[nextmapentry]:=loaded_unit.globalsymtable;
{$else NEWMAP}
              map^[nextmapentry]:=loaded_unit;
{$endif NEWMAP}
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
            end;
           pu:=tused_unit(pu.next);
         end;

        { read the implementation/object part }
        load_implementation;

        { load browser info if stored }
        if ((flags and uf_has_browser)<>0) and load_refs then
         begin
           current_module:=self;
           load_symtable_refs;
         end;
        { remove the map, it's not needed anymore }
        dispose(map);
        map:=nil;
      end;


    procedure tppumodule.loadppu;
      var
        name : string;
      begin
        { load interface section }
          if not do_compile then
           load_interface;
        { only load units when we don't recompile }
          if not do_compile then
           load_usedunits;
        { recompile if set }
          if do_compile then
           begin
           { we don't need the ppufile anymore }
             if assigned(ppufile) then
              begin
                ppufile.free;
                ppufile:=nil;
              end;
           { recompile the unit or give a fatal error if sources not available }
             if not(sources_avail) and
                not(sources_checked) then
               if (not search_unit(modulename^,'',true))
                  and (length(modulename^)>8) then
                 search_unit(copy(modulename^,1,8),'',true);
             if not(sources_avail) then
               begin
                  if recompile_reason=rr_noppu then
                    Message1(unit_f_cant_find_ppu,modulename^)
                  else
                    Message1(unit_f_cant_compile_unit,modulename^);
               end
             else
              begin
                if in_compile then
                  begin
                    in_second_compile:=true;
                    Message1(parser_d_compiling_second_time,modulename^);
                  end;
                if assigned(current_scanner) then
                  current_scanner.tempcloseinputfile;
                name:=mainsource^;
                { compile this module }
                current_module:=self;
                compile(name);
                in_second_compile:=false;
                { the scanner can be reset }
                if assigned(current_scanner) then
                  current_scanner.tempopeninputfile;
              end;
           end;
         if assigned(ppufile) then
           begin
              ppufile.closefile;
              ppufile.free;
              ppufile:=nil;
           end;
        end;


{*****************************************************************************
                                  LoadUnit
*****************************************************************************}

    function loadunit(const s : stringid;const fn:string) : tmodule;
      const
        ImplIntf : array[boolean] of string[15]=('interface','implementation');
      var
        st : tglobalsymtable;
        second_time : boolean;
        old_current_module,hp2 : tmodule;
        hp : tppumodule;
        scanner : tscannerfile;
        dummy : tmodule;
        ups   : stringid;
      begin
         old_current_module:=current_module;
         { Info }
         Message3(unit_u_load_unit,current_module.modulename^,ImplIntf[current_module.in_implementation],s);
         ups:=upper(s);
         { unit not found }
         st:=nil;
         dummy:=nil;
         { search all loaded units }
         hp:=tppumodule(loaded_units.first);
         while assigned(hp) do
           begin
              if hp.modulename^=ups then
                begin
                   { forced to reload ? }
                   if hp.do_reload then
                    begin
                      hp.do_reload:=false;
                      break;
                    end;
                   { only check for units. The main program is also
                     as a unit in the loaded_units list. We simply need
                     to ignore this entry (PFV) }
                   if hp.is_unit then
                    begin
                      { the unit is already registered   }
                      { and this means that the unit     }
                      { is already compiled              }
                      { else there is a cyclic unit use  }
                      if assigned(hp.globalsymtable) then
                        st:=tglobalsymtable(hp.globalsymtable)
                      else
                       begin
                         { both units in interface ? }
                         if (not current_module.in_implementation) and
                            (not hp.in_implementation) then
                          begin
                            { check for a cycle }
                            hp2:=current_module.loaded_from;
                            while assigned(hp2) and (hp2<>hp) do
                             begin
                               if hp2.in_implementation then
                                 hp2:=nil
                               else
                                 hp2:=hp2.loaded_from;
                             end;
                            if assigned(hp2) then
                              Message2(unit_f_circular_unit_reference,current_module.modulename^,hp.modulename^);
                          end;
                       end;
                      break;
                    end;
                end
              else if copy(hp.modulename^,1,8)=ups then
                dummy:=hp;
              { the next unit }
              hp:=tppumodule(hp.next);
           end;
         if assigned(dummy) and not assigned(hp) then
           Message2(unit_w_unit_name_error,s,dummy.modulename^);
       { the unit is not in the loaded units, we must load it first }
         if (not assigned(st)) then
          begin
            if assigned(hp) then
             begin
               { remove the old unit, but save the scanner }
               loaded_units.remove(hp);
               hp.reset;
               { try to reopen ppu }
               hp.search_unit(s,fn,false);
               { try to load the unit a second time first }
               current_module:=hp;
               current_module.in_second_load:=true;
               Message1(unit_u_second_load_unit,current_module.modulename^);
               second_time:=true;
             end
            else
          { generates a new unit info record }
             begin
                current_module:=tppumodule.create(s,fn,true);
                second_time:=false;
             end;
            { close old_current_ppu on system that are
              short on file handles like DOS PM }
{$ifdef SHORT_ON_FILE_HANDLES}
            if old_current_module.is_unit and
               assigned(tppumodule(old_current_module).ppufile) then
              tppumodule(old_current_module).ppufile.tempclose;
{$endif SHORT_ON_FILE_HANDLES}
          { now we can register the unit }
            current_module.loaded_from:=old_current_module;
            loaded_units.insert(current_module);
          { now realy load the ppu }
            tppumodule(current_module).loadppu;
          { set compiled flag }
            current_module.compiled:=true;
          { load return pointer }
            hp:=tppumodule(current_module);
          { for a second_time recompile reload all dependent units,
            for a first time compile register the unit _once_ }
            if second_time then
             begin
               { now reload all dependent units }
               hp2:=tmodule(loaded_units.first);
               while assigned(hp2) do
                begin
                  if hp2.do_reload then
                   dummy:=loadunit(hp2.modulename^,'');
                  hp2:=tmodule(hp2.next);
                end;
             end
            else
             usedunits.concat(tused_unit.create(current_module,true));
          end;
         { set the old module }
{$ifdef SHORT_ON_FILE_HANDLES}
         if old_current_module.is_unit and
            assigned(tppumodule(old_current_module).ppufile) then
           tppumodule(old_current_module).ppufile.tempopen;
{$endif SHORT_ON_FILE_HANDLES}
         { we are back }
         current_module:=old_current_module;
         SetCompileModule(current_module);
         loadunit:=hp;
      end;



end.
{
  $Log$
  Revision 1.21  2002-08-15 15:09:41  carl
    + fpu emulation helpers (ppu checking also)

  Revision 1.20  2002/08/12 16:46:04  peter
    * tscannerfile is now destroyed in tmodule.reset and current_scanner
      is updated accordingly. This removes all the loading and saving of
      the old scanner and the invalid flag marking

  Revision 1.19  2002/08/11 14:28:19  peter
    * TScannerFile.SetInvalid added that will also reset inputfile

  Revision 1.18  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.17  2002/07/26 21:15:37  florian
    * rewrote the system handling

  Revision 1.16  2002/05/16 19:46:36  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.15  2002/05/14 19:34:41  peter
    * removed old logs and updated copyright year

  Revision 1.14  2002/05/12 16:53:05  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.13  2002/04/04 19:05:56  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.12  2002/03/28 20:46:44  carl
  - remove go32v1 support

  Revision 1.11  2002/01/19 14:20:13  peter
    * check for -Un when loading ppu with wrong name

}
