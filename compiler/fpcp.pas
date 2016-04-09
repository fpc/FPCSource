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
    cclasses,
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
      procedure readcontainernames;
      procedure readcontainedunits;
    public
      constructor create(const pn:string);
      destructor destroy; override;
      procedure loadpcp;
      procedure savepcp;
      procedure initmoduleinfo(module:tmodulebase);
      procedure addunit(module:tmodulebase);
    end;

implementation

  uses
    sysutils,
    cfileutl,cutils,
    systems,globals,version,
    verbose,
    entfile,fppu,ppu;

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

  procedure tpcppackage.readcontainernames;
    begin
      if pcpfile.readentry<>ibpackagefiles then
        begin
          message(package_f_pcp_read_error);
          internalerror(424242);
        end;
      pplfilename:=pcpfile.getstring;

      writeln('PPL filename: ',pplfilename);
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
          path:=ChangeFileExt(pcpfile.getstring,'.ppl.ppu');
          new(p);
          p^.module:=nil;
          p^.ppufile:=path;
          containedmodules.add(name,p);
          message1(package_u_contained_unit,name);
        end;
    end;

    constructor tpcppackage.create(const pn: string);
    begin
      inherited create(pn);
      setfilename(pn,true);
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

      //readrequiredpackages

      readcontainedunits;
    end;

  procedure tpcppackage.savepcp;
    begin
      { create new ppufile }
      pcpfile:=tpcpfile.create(pcpfilename);
      if not pcpfile.createfile then
        Message2(package_f_cant_create_pcp,realpackagename^,pcpfilename);

      pcpfile.putstring(realpackagename^);
      pcpfile.writeentry(ibpackagename);

      writecontainernames;

      //writerequiredpackages;

      writecontainedunits;

      //writeppus;

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

      { save crc in current module also }
      //crc:=pcpfile.crc;

      pcpfile.closefile;
      pcpfile.free;
      pcpfile:=nil;
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
      containedmodules.add(module.modulename^,containedunit);
    end;

end.

