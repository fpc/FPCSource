{
    Copyright (c) 2013-2016 by Free Pascal development team

    This unit implements the loading and searching of package files

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
unit fpcp;

{$i fpcdefs.inc}

interface

  uses
    cclasses,cstreams,
    globtype,
    pcp,finput,fpkg;

  type
    tpcppackage=class(tpackage)
    private
      loaded : boolean;
      pcpfile : tpcpfile;
    private
      function openpcp:boolean;
      function search_package(ashortname:boolean):boolean;
      function search_package_file:boolean;
      procedure setfilename(const fn:string;allowoutput:boolean);
      procedure writecontainernames;
      procedure writecontainedunits;
      procedure writerequiredpackages;
      procedure writepputable;
      procedure writeppudata;
      procedure readcontainernames;
      procedure readcontainedunits;
      procedure readrequiredpackages;
      procedure readpputable;
    public
      constructor create(const pn:string);
      destructor destroy; override;
      procedure loadpcp;
      procedure savepcp;
      function getmodulestream(module:tmodulebase):tcstream;
      procedure initmoduleinfo(module:tmodulebase);
      procedure addunit(module:tmodulebase);
      procedure add_required_package(pkg:tpackage);
    end;

implementation

  uses
    sysutils,
    cfileutl,cutils,
    systems,globals,version,
    verbose,
    ppu,
    entfile,pkgutil;

{ tpcppackage }

  function tpcppackage.openpcp: boolean;
    var
      pcpfiletime : longint;
    begin
      result:=false;
      Message1(package_t_pcp_loading,pcpfilename);
      { Get pcpfile time (also check if the file exists) }
      pcpfiletime:=getnamedfiletime(pcpfilename);
      if pcpfiletime=-1 then
       exit;
    { Open the pcpfile }
      Message1(package_u_pcp_name,pcpfilename);
      pcpfile:=tpcpfile.create(pcpfilename);
      if not pcpfile.openfile then
       begin
         pcpfile.free;
         pcpfile:=nil;
         Message(package_u_pcp_file_too_short);
         exit;
       end;
    { check for a valid PPU file }
      if not pcpfile.checkpcpid then
       begin
         pcpfile.free;
         pcpfile:=nil;
         Message(package_u_pcp_invalid_header);
         exit;
       end;
    { check for allowed PCP versions }
      if not (pcpfile.getversion=CurrentPCPVersion) then
       begin
         Message1(package_u_pcp_invalid_version,tostr(pcpfile.getversion));
         pcpfile.free;
         pcpfile:=nil;
         exit;
       end;
    { check the target processor }
      if tsystemcpu(pcpfile.header.common.cpu)<>target_cpu then
       begin
         pcpfile.free;
         pcpfile:=nil;
         Message(package_u_pcp_invalid_processor);
         exit;
       end;
    { check target }
      if tsystem(pcpfile.header.common.target)<>target_info.system then
       begin
         pcpfile.free;
         pcpfile:=nil;
         Message(package_u_pcp_invalid_target);
         exit;
       end;
  {$ifdef cpufpemu}
     { check if floating point emulation is on?
       fpu emulation isn't unit levelwise because it affects calling convention }
     if ((pcpfile.header.common.flags and uf_fpu_emulation)<>0) xor
          (cs_fp_emulation in current_settings.moduleswitches) then
       begin
         pcpfile.free;
         pcpfile:=nil;
         Message(package_u_pcp_invalid_fpumode);
         exit;
       end;
  {$endif cpufpemu}

    { Load values to be access easier }
      //flags:=pcpfile.header.common.flags;
      //crc:=pcpfile.header.checksum;
    { Show Debug info }
      Message1(package_u_pcp_time,filetimestring(pcpfiletime));
      Message1(package_u_pcp_flags,tostr(pcpfile.header.common.flags{flags}));
      Message1(package_u_pcp_crc,hexstr(pcpfile.header.checksum,8));
      (*Message1(package_u_pcp_crc,hexstr(ppufile.header.interface_checksum,8)+' (intfc)');
      Message1(package_u_pcp_crc,hexstr(ppufile.header.indirect_checksum,8)+' (indc)');
      Comment(V_used,'Number of definitions: '+tostr(ppufile.header.deflistsize));
      Comment(V_used,'Number of symbols: '+tostr(ppufile.header.symlistsize));
      do_compile:=false;*)
      result:=true;
    end;

  function tpcppackage.search_package(ashortname:boolean):boolean;
    var
      singlepathstring,
      filename : TCmdStr;

    function package_exists(const ext:string;var foundfile:TCmdStr):boolean;
      begin
        if CheckVerbosity(V_Tried) then
          Message1(package_t_packagesearch,Singlepathstring+filename+ext);
        result:=FindFile(filename+ext,singlepathstring,true,foundfile);
      end;

    function package_search_path(const s:TCmdStr):boolean;
      var
        found : boolean;
        hs    : TCmdStr;
      begin
        found:=false;
        singlepathstring:=FixPath(s,false);
        { Check for package file }
        { TODO }
        found:=package_exists({target_info.pkginfoext}'.pcp',hs);
        if found then
          begin
            setfilename(hs,false);
            found:=openpcp;
          end;
        result:=found;
      end;

    function search_path_list(list:TSearchPathList):boolean;
      var
        hp : TCmdStrListItem;
        found : boolean;
      begin
        found:=false;
        hp:=TCmdStrListItem(list.First);
        while assigned(hp) do
         begin
           found:=package_search_path(hp.Str);
           if found then
            break;
           hp:=TCmdStrListItem(hp.next);
         end;
        result:=found;
      end;

    begin
      filename:=packagename^;
      result:=search_path_list(packagesearchpath);
    end;

  function tpcppackage.search_package_file: boolean;
    var
      found : boolean;
    begin
      found:=false;
      if search_package(false) then
        found:=true;
      if not found and
          (length(packagename^)>8) and
         search_package(true) then
        found:=true;
      result:=found;
    end;

  procedure tpcppackage.setfilename(const fn:string;allowoutput:boolean);
    var
      p,n : tpathstr;
    begin
      p:=FixPath(ExtractFilePath(fn),false);
      n:=FixFileName(ChangeFileExt(ExtractFileName(fn),''));
      { pcp name }
      if allowoutput then
        if (OutputUnitDir<>'') then
          p:=OutputUnitDir
        else
          if (OutputExeDir<>'') then
            p:=OutputExeDir;
      pcpfilename:=p+n+{target_info.pkginfoext}'.pcp';
    end;

  procedure tpcppackage.writecontainernames;
    begin
      pcpfile.putstring(pplfilename);
      //pcpfile.putstring(ppafilename);
      pcpfile.writeentry(ibpackagefiles);
    end;

  procedure tpcppackage.writecontainedunits;
    var
      p : pcontainedunit;
      i : longint;
    begin
      pcpfile.putlongint(containedmodules.count);
      pcpfile.writeentry(ibstartcontained);
      { for now we write the unit name and the ppu file name }
      for i:=0 to containedmodules.count-1 do
        begin
          p:=pcontainedunit(containedmodules.items[i]);
          pcpfile.putstring(p^.module.modulename^);
          pcpfile.putstring(p^.ppufile);
        end;
      pcpfile.writeentry(ibendcontained);
    end;

  procedure tpcppackage.writerequiredpackages;
    var
      i : longint;
    begin
      pcpfile.putlongint(requiredpackages.count);
      pcpfile.writeentry(ibstartrequireds);
      for i:=0 to requiredpackages.count-1 do
        begin
          pcpfile.putstring(requiredpackages.NameOfIndex(i));
        end;
      pcpfile.writeentry(ibendrequireds);
    end;

  procedure tpcppackage.writepputable;
    var
      module : pcontainedunit;
      i : longint;
    begin
      { no need to write the count again; it's the same as for the contained units }
      for i:=0 to containedmodules.count-1 do
        begin
          module:=pcontainedunit(containedmodules[i]);
          pcpfile.putlongint(module^.offset);
          pcpfile.putlongint(module^.size);
        end;
      pcpfile.writeentry(ibpputable);
    end;

  procedure tpcppackage.writeppudata;
    const
      align: array[0..15] of byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
    var
      i,
      pos,
      rem : longint;
      module : pcontainedunit;
      stream : TCStream;
    begin
      pcpfile.flush;

      for i:=0 to containedmodules.count-1 do
        begin
          module:=pcontainedunit(containedmodules[i]);

          pos:=pcpfile.position;
          { align to 16 byte so that it can be nicely viewed in hex editors;
            maybe we could also use 512 byte alignment instead }
          rem:=$f-(pos and $f);
          pcpfile.stream.write(align[0],rem+1);
          pcpfile.flush;
          module^.offset:=pcpfile.position;

          { retrieve substream for the current position }
          stream:=pcpfile.substream(module^.offset,-1);
          rewriteppu(module^.module.ppufilename,stream);
          module^.size:=stream.position;
          stream.free;
        end;

      pos:=pcpfile.position;
      { align to 16 byte so that it can be nicely viewed in hex editors;
        maybe we could also use 512 byte alignment instead }
      rem:=$f-(pos and $f);
      pcpfile.stream.write(align[0],rem+1);
    end;

  procedure tpcppackage.readcontainernames;
    begin
      if pcpfile.readentry<>ibpackagefiles then
        begin
          message(package_f_pcp_read_error);
          internalerror(424242);
        end;
      pplfilename:=pcpfile.getstring;

      message1(package_u_ppl_filename,pplfilename);
    end;

  procedure tpcppackage.readcontainedunits;
    var
      cnt,i : longint;
      name,path : string;
      p : pcontainedunit;
    begin
      if pcpfile.readentry<>ibstartcontained then
        begin
          message(package_f_pcp_read_error);
          internalerror(424242);
        end;
      cnt:=pcpfile.getlongint;
      if pcpfile.readentry<>ibendcontained then
        begin
          message(package_f_pcp_read_error);
          internalerror(424242);
        end;
      for i:=0 to cnt-1 do
        begin
          name:=pcpfile.getstring;
          path:=pcpfile.getstring;
          new(p);
          p^.module:=nil;
          p^.ppufile:=path;
          p^.offset:=0;
          p^.size:=0;
          containedmodules.add(name,p);
          message1(package_u_contained_unit,name);
        end;
    end;

  procedure tpcppackage.readrequiredpackages;
    var
      cnt,i : longint;
      name : string;
    begin
      if pcpfile.readentry<>ibstartrequireds then
        begin
          message(package_f_pcp_read_error);
          internalerror(2014110901);
        end;
      cnt:=pcpfile.getlongint;
      if pcpfile.readentry<>ibendrequireds then
        begin
          message(package_f_pcp_read_error);
          internalerror(2014110902);
        end;
      for i:=0 to cnt-1 do
        begin
          name:=pcpfile.getstring;
          requiredpackages.add(name,nil);
          message1(package_u_required_package,name);
        end;
    end;

  procedure tpcppackage.readpputable;
    var
      module : pcontainedunit;
      i : longint;
    begin
      if pcpfile.readentry<>ibpputable then
        begin
          message(package_f_pcp_read_error);
          internalerror(2015103001);
        end;
      for i:=0 to containedmodules.count-1 do
        begin
          module:=pcontainedunit(containedmodules[i]);
          module^.offset:=pcpfile.getlongint;
          module^.size:=pcpfile.getlongint;
        end;
    end;

    constructor tpcppackage.create(const pn: string);
    begin
      inherited create(pn);

      setfilename(pn+'.ppk',true);
    end;

  destructor tpcppackage.destroy;
    begin
      pcpfile.free;
      inherited destroy;
    end;

  procedure tpcppackage.loadpcp;
    var
      newpackagename : string;
    begin
      if loaded then
        exit;

      if not search_package_file then
        begin
          Message1(package_f_cant_find_pcp,realpackagename^);
          exit;
        end
      else
        Message1(package_u_pcp_found,realpackagename^);

      if not assigned(pcpfile) then
        internalerror(2013053101);

      if pcpfile.readentry<>ibpackagename then
        Message1(package_f_cant_read_pcp,realpackagename^);
      newpackagename:=pcpfile.getstring;
      if upper(newpackagename)<>packagename^ then
        Comment(V_Error,'Package was renamed: '+realpackagename^);

      readcontainernames;

      readrequiredpackages;

      readcontainedunits;

      readpputable;
    end;

  procedure tpcppackage.savepcp;
    var
      tablepos,
      oldpos : longint;
    begin
      { create new ppufile }
      pcpfile:=tpcpfile.create(pcpfilename);
      if not pcpfile.createfile then
        Message2(package_f_cant_create_pcp,realpackagename^,pcpfilename);

      pcpfile.putstring(realpackagename^);
      pcpfile.writeentry(ibpackagename);

      writecontainernames;

      writerequiredpackages;

      writecontainedunits;

      { the offsets and the contents of the ppus are not crc'd }
      pcpfile.do_crc:=false;

      pcpfile.flush;
      tablepos:=pcpfile.position;

      { this will write a table with empty entries }
      writepputable;

      pcpfile.do_crc:=true;

      { the last entry ibend is written automatically }

      { flush to be sure }
      pcpfile.flush;
      { create and write header }
      pcpfile.header.common.size:=pcpfile.size;
      pcpfile.header.checksum:=pcpfile.crc;
      pcpfile.header.common.compiler:=wordversion;
      pcpfile.header.common.cpu:=word(target_cpu);
      pcpfile.header.common.target:=word(target_info.system);
      //pcpfile.header.flags:=flags;
      pcpfile.header.ppulistsize:=containedmodules.count;
      pcpfile.header.requiredlistsize:=requiredpackages.count;
      pcpfile.writeheader;

      { write the ppu table which will also fill the offsets/sizes }
      writeppudata;

      pcpfile.flush;
      oldpos:=pcpfile.position;

      { now write the filled PPU table at the previously stored position }
      pcpfile.position:=tablepos;
      writepputable;

      pcpfile.position:=oldpos;

      { save crc in current module also }
      //crc:=pcpfile.crc;

      pcpfile.closefile;
      pcpfile.free;
      pcpfile:=nil;
    end;

  function tpcppackage.getmodulestream(module:tmodulebase):tcstream;
    var
      i : longint;
      contained : pcontainedunit;
    begin
      for i:=0 to containedmodules.count-1 do
        begin
          contained:=pcontainedunit(containedmodules[i]);
          if contained^.module=module then
            begin
              result:=pcpfile.substream(contained^.offset,contained^.size);
              exit;
            end;
        end;
      result:=nil;
    end;

  procedure tpcppackage.initmoduleinfo(module: tmodulebase);
    begin
      pplfilename:=extractfilename(module.sharedlibfilename);
    end;

  procedure tpcppackage.addunit(module: tmodulebase);
    var
      containedunit : pcontainedunit;
    begin
      new(containedunit);
      containedunit^.module:=module;
      containedunit^.ppufile:=extractfilename(module.ppufilename);
      containedunit^.offset:=0;
      containedunit^.size:=0;
      containedmodules.add(module.modulename^,containedunit);
    end;


  procedure tpcppackage.add_required_package(pkg:tpackage);
    var
      p : tpackage;
    begin
      p:=tpackage(requiredpackages.find(pkg.packagename^));
      if not assigned(p) then
        requiredpackages.Add(pkg.packagename^,pkg)
      else
        if p<>pkg then
          internalerror(2015112302);
    end;


end.

