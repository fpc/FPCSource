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
          sourcefn   : pstring; { Source specified with "uses .. in '..'" }
{$ifdef Test_Double_checksum}
          crc_array  : pointer;
          crc_size   : longint;
          crc_array2 : pointer;
          crc_size2  : longint;
{$endif def Test_Double_checksum}
          constructor create(LoadedFrom:TModule;const s:string;const fn:string;_is_unit:boolean);
          destructor destroy;override;
          procedure reset;override;
          function  openppu:boolean;
          procedure getppucrc;
          procedure writeppu;
          procedure loadppu;
       private
          function  search_unit(onlysource,shortname:boolean):boolean;
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

    function registerunit(callermodule:tmodule;const s : stringid;const fn:string) : tppumodule;


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

    constructor tppumodule.create(LoadedFrom:TModule;const s:string;const fn:string;_is_unit:boolean);
      begin
        inherited create(LoadedFrom,s,_is_unit);
        ppufile:=nil;
        sourcefn:=stringdup(fn);
      end;


    destructor tppumodule.Destroy;
      begin
        if assigned(ppufile) then
         ppufile.free;
        ppufile:=nil;
        stringdispose(sourcefn);
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
{$ifdef cpufpemu}
       { check if floating point emulation is on?}
        if ((ppufile.header.flags and uf_fpu_emulation)<>0) and
            (cs_fp_emulation in aktmoduleswitches) then
         begin
           ppufile.free;
           ppufile:=nil;
           Message(unit_u_ppu_invalid_fpumode);
           exit;
         end;
{$endif cpufpemu}

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


    function tppumodule.search_unit(onlysource,shortname:boolean):boolean;
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
         if shortname then
          filename:=FixFileName(Copy(realmodulename^,1,8))
         else
          filename:=FixFileName(realmodulename^);
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
         if (not fnd) and (sourcefn^<>'') then
          begin
            { the full filename is specified so we can't use here the
              searchpath (PFV) }
            Message1(unit_t_unitsearch,AddExtension(sourcefn^,target_info.sourceext));
            fnd:=FindFile(AddExtension(sourcefn^,target_info.sourceext),'',hs);
            if not fnd then
             begin
               Message1(unit_t_unitsearch,AddExtension(sourcefn^,target_info.pasext));
               fnd:=FindFile(AddExtension(sourcefn^,target_info.pasext),'',hs);
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
         if not fnd then
           fnd:=SourceSearchPath('.');
         if (not fnd) and Assigned(Loaded_From) then
           fnd:=SearchPathList(Loaded_From.LocalUnitSearchPath);
         if not fnd then
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
            ppufile.putlongint(hp.getfiletime);
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
           ppufile.putstring(hp.u.realmodulename^);
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
         librarydata.asmsymbolidx^[tasmsymbol(s).ppuidx-1]:=tasmsymbol(s);
      end;


    procedure tppumodule.writeasmsymbols;
      var
        s : tasmsymbol;
        i : longint;
        asmsymtype : byte;
      begin
        { get an ordered list of all symbols to put in the ppu }
        getmem(librarydata.asmsymbolidx,librarydata.asmsymbolppuidx*sizeof(pointer));
        fillchar(librarydata.asmsymbolidx^,librarydata.asmsymbolppuidx*sizeof(pointer),0);
        librarydata.symbolsearch.foreach({$ifdef FPCPROCVAR}@{$endif}putasmsymbol_in_idx,nil);
        { write the number of symbols }
        ppufile.putlongint(librarydata.asmsymbolppuidx);
        { write the symbols from the indexed list to the ppu }
        for i:=1 to librarydata.asmsymbolppuidx do
         begin
           s:=librarydata.asmsymbolidx^[i-1];
           if not assigned(s) then
            internalerror(200208071);
           asmsymtype:=1;
           if s.Classtype=tasmlabel then
            begin
              if tasmlabel(s).is_addr then
               asmsymtype:=4
              else if tasmlabel(s).typ=AT_DATA then
               asmsymtype:=3
              else
               asmsymtype:=2;
            end;
           ppufile.putbyte(asmsymtype);
           case asmsymtype of
             1 :
               ppufile.putstring(s.name);
             2 :
               ppufile.putlongint(tasmlabel(s).labelnr);
           end;
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
              Message2(unit_h_cond_set_in_last_compile,hs,mainsource^);
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
        orgfiletime,
        source_time   : longint;
        hp            : tinputfile;
      begin
        sources_avail:=true;
        is_main:=true;
        main_dir:='';
        while not ppufile.endofentry do
         begin
           hs:=ppufile.getstring;
           orgfiletime:=ppufile.getlongint;
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
                    if (orgfiletime<>-1) and
                       (source_time<>orgfiletime) then
                     begin
                       if ((flags and uf_release)=0) then
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
        pu : tused_unit;
        hp : tppumodule;
        intfchecksum,
        checksum : cardinal;
      begin
        while not ppufile.endofentry do
         begin
           hs:=ppufile.getstring;
           checksum:=cardinal(ppufile.getlongint);
           intfchecksum:=cardinal(ppufile.getlongint);
           in_interface:=(ppufile.getbyte<>0);
           { set the state of this unit before registering, this is
             needed for a correct circular dependency check }
           hp:=registerunit(self,hs,'');
           pu:=addusedunit(hp,false);
           pu.checksum:=checksum;
           pu.interface_checksum:=intfchecksum;
         end;
        in_interface:=false;
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
        labelnr,
        i     : longint;
        name  : string;
        bind  : TAsmSymBind;
        typ   : TAsmSymType;
        asmsymtype : byte;
      begin
        librarydata.asmsymbolppuidx:=ppufile.getlongint;
        if librarydata.asmsymbolppuidx>0 then
         begin
           getmem(librarydata.asmsymbolidx,librarydata.asmsymbolppuidx*sizeof(pointer));
           fillchar(librarydata.asmsymbolidx^,librarydata.asmsymbolppuidx*sizeof(pointer),0);
           for i:=1 to librarydata.asmsymbolppuidx do
            begin
              asmsymtype:=ppufile.getbyte;
              case asmsymtype of
                1 :
                  name:=ppufile.getstring;
                2..4 :
                  labelnr:=ppufile.getlongint;
                else
                  internalerror(200208192);
              end;
              bind:=tasmsymbind(ppufile.getbyte);
              typ:=tasmsymtype(ppufile.getbyte);
              case asmsymtype of
                1 :
                 librarydata.asmsymbolidx^[i-1]:=librarydata.newasmsymboltype(name,bind,typ);
                2 :
                 librarydata.asmsymbolidx^[i-1]:=librarydata.newasmlabel(labelnr,false,false);
                3 :
                 librarydata.asmsymbolidx^[i-1]:=librarydata.newasmlabel(labelnr,true,false);
                4 :
                 librarydata.asmsymbolidx^[i-1]:=librarydata.newasmlabel(labelnr,false,true);
              end;
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
        oldobjectlibrary : tasmlibrarydata;
      begin
         { read implementation part }
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

         { we can now derefence all pointers to the implementation parts }
         oldobjectlibrary:=objectlibrary;
         objectlibrary:=librarydata;
         tstoredsymtable(globalsymtable).derefimpl;
         if assigned(localsymtable) then
           tstoredsymtable(localsymtable).derefimpl;
         objectlibrary:=oldobjectlibrary;
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
             tstaticsymtable(localsymtable).ppuload(ppufile);
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
{$ifdef cpufpemu}
         if (cs_fp_emulation in aktmoduleswitches) then
           flags:=flags or uf_fpu_emulation;
{$endif cpufpemu}
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
         tstoredsymtable(globalsymtable).ppuwrite(ppufile);

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
           tstoredsymtable(localsymtable).ppuwrite(ppufile);

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
         tstoredsymtable(globalsymtable).ppuwrite(ppufile);

         { save crc  }
         crc:=ppufile.crc;
         interface_crc:=ppufile.interface_crc;

         { end of implementation, to generate a correct ppufile
           for ppudump when using INTFPPU define }
         ppufile.writeentry(ibendimplementation);

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
        load_refs    : boolean;
        nextmapentry : longint;
      begin
        if current_module<>self then
         internalerror(200212284);
        load_refs:=true;
        { init the map }
        new(map);
        fillchar(map^,sizeof(tunitmap),#0);
        map^[0]:=self;
        nextmapentry:=1;
        { load the used units from interface }
        in_interface:=true;
        pu:=tused_unit(used_units.first);
        while assigned(pu) do
         begin
           if pu.in_interface then
            begin
              tppumodule(pu.u).loadppu;
              { if this unit is compiled we can stop }
              if state=ms_compiled then
               exit;
              { add this unit to the dependencies }
              pu.u.adddependency(self);
              { need to recompile the current unit ? }
              if pu.u.crc<>pu.checksum then
               begin
                 Message2(unit_u_recompile_crc_change,realmodulename^,pu.u.realmodulename^);
                 recompile_reason:=rr_crcchanged;
                 do_compile:=true;
                 dispose(map);
                 map:=nil;
                 exit;
               end;
              { setup the map entry for deref }
              map^[nextmapentry]:=pu.u;
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
            end;
           pu:=tused_unit(pu.next);
         end;

        { ok, now load the interface of this unit }
        if current_module<>self then
         internalerror(200208187);
        globalsymtable:=tglobalsymtable.create(modulename^);
        tstoredsymtable(globalsymtable).ppuload(ppufile);

        { now only read the implementation uses }
        in_interface:=false;
        pu:=tused_unit(used_units.first);
        while assigned(pu) do
         begin
           if (not pu.in_interface) then
            begin
              tppumodule(pu.u).loadppu;
              { if this unit is compiled we can stop }
              if state=ms_compiled then
               exit;
              { add this unit to the dependencies }
              pu.u.adddependency(self);
              { need to recompile the current unit ? }
              if (pu.u.interface_crc<>pu.interface_checksum) then
                begin
                  Message2(unit_u_recompile_crc_change,realmodulename^,pu.u.realmodulename^+' {impl}');
                  recompile_reason:=rr_crcchanged;
                  do_compile:=true;
                  dispose(map);
                  map:=nil;
                  exit;
                end;
              { setup the map entry for deref }
              map^[nextmapentry]:=pu.u;
              inc(nextmapentry);
              if nextmapentry>maxunits then
               Message(unit_f_too_much_units);
            end;
           pu:=tused_unit(pu.next);
         end;

        { read the implementation/objectdata part }
        load_implementation;

        { load browser info if stored }
        if ((flags and uf_has_browser)<>0) and load_refs then
         begin
           if current_module<>self then
            internalerror(200208188);
           load_symtable_refs;
         end;

        { remove the map, it's not needed anymore }
        dispose(map);
        map:=nil;
      end;


    procedure tppumodule.loadppu;
      const
        ImplIntf : array[boolean] of string[15]=('implementation','interface');
      var
        second_time : boolean;
        hp,
        old_current_module : tmodule;
      begin
        old_current_module:=current_module;
        Message3(unit_u_load_unit,old_current_module.modulename^,
                 ImplIntf[old_current_module.in_interface],
                 modulename^);
        { check if the globalsymtable is already available, but
          we must reload when the do_reload flag is set }
        if do_reload then
         begin
           Comment(V_Used,'Forced reloading');
           do_reload:=false;
         end
        else
         begin
           if assigned(globalsymtable) then
            exit;
         end;
        { reset }
        second_time:=false;
        current_module:=self;
        SetCompileModule(current_module);
        Fillchar(aktfilepos,0,sizeof(aktfilepos));

        { we are loading a new module, save the state of the scanner
          and reset scanner+module }
        if assigned(current_scanner) then
          current_scanner.tempcloseinputfile;
        current_scanner:=nil;

        { loading the unit for a second time? }
        if state=ms_registered then
         state:=ms_load
        else
         begin
           { try to load the unit a second time first }
           Message1(unit_u_second_load_unit,modulename^);
           Comment(V_Used,'Previous state '+modulename^+': '+ModuleStateStr[state]);
           { Flag modules to reload }
           flagdependent(old_current_module);
           { Reset the module }
           reset;
           if state=ms_compile then
             begin
               Comment(V_Used,'Already compiling '+modulename^+' setting second compile');
               state:=ms_second_compile;
               do_compile:=true;
             end
           else
             state:=ms_second_load;
           second_time:=true;
         end;

        { close old_current_ppu on system that are
          short on file handles like DOS PM }
{$ifdef SHORT_ON_FILE_HANDLES}
        if old_current_module.is_unit and
           assigned(tppumodule(old_current_module).ppufile) then
          tppumodule(old_current_module).ppufile.tempclose;
{$endif SHORT_ON_FILE_HANDLES}

        { try to opening ppu, skip this when we already
          know that we need to compile the unit }
        if not do_compile then
         begin
           Comment(V_Used,'Loading module '+modulename^);
           search_unit(false,false);
           if not do_compile then
            begin
              load_interface;
              if not do_compile then
               begin
                 load_usedunits;
                 if not do_compile then
                   Comment(V_Used,'Finished loading module '+modulename^);
               end;
            end;
           { PPU is not needed anymore }
           if assigned(ppufile) then
            begin
               ppufile.closefile;
               ppufile.free;
               ppufile:=nil;
            end;
         end;

        { Do we need to recompile the unit }
        if do_compile then
         begin
           { recompile the unit or give a fatal error if sources not available }
           if not(sources_avail) then
            begin
              if (not search_unit(true,false)) and
                 (length(modulename^)>8) then
                search_unit(true,true);
              if not(sources_avail) then
               begin
                 if recompile_reason=rr_noppu then
                   Message1(unit_f_cant_find_ppu,modulename^)
                 else
                   Message1(unit_f_cant_compile_unit,modulename^);
               end;
            end;
           { Flag modules to reload }
           flagdependent(old_current_module);
           { Reset the module }
           reset;
           { compile this module }
           if not(state in [ms_compile,ms_second_compile]) then
             state:=ms_compile;
           compile(mainsource^);
         end;

        { set compiled flag }
        if current_module<>self then
          internalerror(200212282);
        state:=ms_compiled;

        if in_interface then
          internalerror(200212283);

        { for a second_time recompile reload all dependent units,
          for a first time compile register the unit _once_ }
        if second_time then
         begin
           { now reload all dependent units }
           hp:=tmodule(loaded_units.first);
           while assigned(hp) do
            begin
              if hp.do_reload then
                tppumodule(hp).loadppu;
              hp:=tmodule(hp.next);
            end;
         end
        else
         usedunits.concat(tused_unit.create(self,true,false));

        { reopen the old module }
{$ifdef SHORT_ON_FILE_HANDLES}
        if old_current_module.is_unit and
           assigned(tppumodule(old_current_module).ppufile) then
          tppumodule(old_current_module).ppufile.tempopen;
{$endif SHORT_ON_FILE_HANDLES}

        { we are back, restore current_module and current_scanner }
        current_module:=old_current_module;
        current_scanner:=tscannerfile(current_module.scanner);
        if assigned(current_scanner) then
         begin
           current_scanner.tempopeninputfile;
           current_scanner.gettokenpos;
         end
        else
         fillchar(aktfilepos,sizeof(aktfilepos),0);
        SetCompileModule(current_module);
      end;


{*****************************************************************************
                               RegisterUnit
*****************************************************************************}


    function registerunit(callermodule:tmodule;const s : stringid;const fn:string) : tppumodule;
      var
        ups   : stringid;
        hp    : tppumodule;
        hp2,
        shortnamehp : tmodule;
      begin
        { Info }
        ups:=upper(s);
        { search all loaded units }
        shortnamehp:=nil;
        hp:=tppumodule(loaded_units.first);
        while assigned(hp) do
         begin
           if hp.modulename^=ups then
            begin
              { only check for units. The main program is also
                as a unit in the loaded_units list. We simply need
                to ignore this entry (PFV) }
              if hp.is_unit then
               begin
                 { both units in interface ? }
                 if callermodule.in_interface and
                    hp.in_interface then
                  begin
                    { check for a cycle }
                    hp2:=callermodule.loaded_from;
                    while assigned(hp2) and (hp2<>hp) do
                     begin
                       if hp2.in_interface then
                         hp2:=hp2.loaded_from
                       else
                         hp2:=nil;
                     end;
                    if assigned(hp2) then
                      Message2(unit_f_circular_unit_reference,callermodule.modulename^,hp.modulename^);
                  end;
                 break;
               end;
            end
           else
            if copy(hp.modulename^,1,8)=ups then
             shortnamehp:=hp;
           { the next unit }
           hp:=tppumodule(hp.next);
         end;
        if assigned(shortnamehp) and not assigned(hp) then
          Message2(unit_w_unit_name_error,s,shortnamehp.modulename^);
        { the unit is not in the loaded units,
          we create an entry and register the unit }
        if not assigned(hp) then
         begin
           Comment(V_Used,'Registering new unit '+Upper(s));
           hp:=tppumodule.create(callermodule,s,fn,true);
           hp.loaded_from:=callermodule;
           loaded_units.insert(hp);
         end;
        { return }
        registerunit:=hp;
      end;

end.
{
  $Log$
  Revision 1.32  2003-04-27 07:29:50  peter
    * aktprocdef cleanup, aktprocdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.31  2003/04/26 00:30:52  peter
    * reset aktfilepos when setting new module for compile

  Revision 1.30  2003/03/27 17:44:13  peter
    * fixed small mem leaks

  Revision 1.29  2002/12/29 14:57:50  peter
    * unit loading changed to first register units and load them
      afterwards. This is needed to support uses xxx in yyy correctly
    * unit dependency check fixed

  Revision 1.28  2002/12/06 16:56:57  peter
    * only compile cs_fp_emulation support when cpufpuemu is defined
    * define cpufpuemu for m68k only

  Revision 1.27  2002/11/20 12:36:24  mazen
  * $UNITPATH directive is now working

  Revision 1.26  2002/11/15 01:58:46  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.25  2002/10/20 14:49:31  peter
    * store original source time in ppu so it can be compared instead of
      comparing with the ppu time

  Revision 1.24  2002/10/04 20:13:10  peter
    * set in_second_load flag before resetting the module, this is
      required to skip some checkings

  Revision 1.23  2002/08/19 19:36:42  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.22  2002/08/18 19:58:28  peter
    * more current_scanner fixes

  Revision 1.21  2002/08/15 15:09:41  carl
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
