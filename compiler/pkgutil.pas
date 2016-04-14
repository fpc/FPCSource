{
    Copyright (c) 2013-2016 by Free Pascal Development Team

    This unit implements basic parts of the package system

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

unit pkgutil;

{$i fpcdefs.inc}

interface

  uses
    fmodule,fpkg;

  procedure createimportlibfromexternals;
  Function RewritePPU(const PPUFn,PPLFn:String):Boolean;
  procedure export_unit(u:tmodule);
  procedure load_packages;
  procedure add_package(const name:string;ignoreduplicates:boolean);

implementation

  uses
    sysutils,
    globtype,systems,
    cutils,cclasses,
    globals,verbose,
    symtype,symconst,symsym,symdef,symbase,symtable,
    ppu,entfile,fpcp,
    export;

  procedure procexport(const s : string);
    var
      hp : texported_item;
    begin
      hp:=texported_item.create;
      hp.name:=stringdup(s);
      hp.options:=hp.options+[eo_name];
      exportlib.exportprocedure(hp);
    end;


  procedure varexport(const s : string);
    var
      hp : texported_item;
    begin
      hp:=texported_item.create;
      hp.name:=stringdup(s);
      hp.options:=hp.options+[eo_name];
      exportlib.exportvar(hp);
    end;


  procedure exportprocsym(sym:tprocsym;symtable:tsymtable);
    var
      i : longint;
      item : TCmdStrListItem;
    begin
      for i:=0 to tprocsym(sym).ProcdefList.Count-1 do
        begin
          if not(tprocdef(tprocsym(sym).ProcdefList[i]).proccalloption in [pocall_internproc]) and
            ((tprocdef(tprocsym(sym).ProcdefList[i]).procoptions*[po_external])=[]) and
            ((symtable.symtabletype in [globalsymtable,recordsymtable,objectsymtable]) or
             ((symtable.symtabletype=staticsymtable) and (po_public in tprocdef(tprocsym(sym).ProcdefList[i]).procoptions))
            ) then
            begin
              exportallprocdefnames(tprocsym(sym),tprocdef(tprocsym(sym).ProcdefList[i]),[]);
            end;
        end;
    end;


  procedure exportabstractrecorddef(def:tabstractrecorddef); forward;


  procedure exportabstractrecordsymproc(sym:tobject;arg:pointer);
    var
      def : tabstractrecorddef;
    begin
      case tsym(sym).typ of
        typesym:
          begin
            case ttypesym(sym).typedef.typ of
              objectdef,
              recorddef:
                exportabstractrecorddef(tabstractrecorddef(ttypesym(sym).typedef));
            end;
          end;
        procsym:
          begin
            { don't export methods of interfaces }
            if is_interface(tdef(tabstractrecordsymtable(arg).defowner)) then
              exit;
            exportprocsym(tprocsym(sym),tsymtable(arg));
          end;
        staticvarsym:
          begin
            varexport(tsym(sym).mangledname);
          end;
      end;
    end;


  procedure exportabstractrecorddef(def:tabstractrecorddef);
    var
      hp : texported_item;
    begin
      def.symtable.SymList.ForEachCall(@exportabstractrecordsymproc,def.symtable);
      { don't export generics or their nested types }
      if df_generic in def.defoptions then
        exit;
      if (def.typ=objectdef) and (oo_has_vmt in tobjectdef(def).objectoptions) then
        begin
          hp:=texported_item.create;
          hp.name:=stringdup(tobjectdef(def).vmt_mangledname);
          hp.options:=hp.options+[eo_name];
          exportlib.exportvar(hp);
        end;
    end;


  procedure insert_export(sym : TObject;arg:pointer);
    var
      i : longint;
      item : TCmdStrListItem;
      publiconly : boolean;
    begin
      publiconly:=tsymtable(arg).symtabletype=staticsymtable;
      case TSym(sym).typ of
        { ignore: }
        unitsym,
        syssym,
        constsym,
        namespacesym,
        propertysym,
        enumsym:
          ;
        typesym:
          begin
            case ttypesym(sym).typedef.typ of
              recorddef,
              objectdef:
                exportabstractrecorddef(tabstractrecorddef(ttypesym(sym).typedef));
            end;
          end;
        procsym:
          begin
            exportprocsym(tprocsym(sym),tsymtable(arg));
          end;
        staticvarsym:
          begin
            if publiconly and not (vo_is_public in tstaticvarsym(sym).varoptions) then
              exit;
            varexport(tsym(sym).mangledname);
          end;
        else
          begin
            writeln('unknown: ',ord(TSym(sym).typ));
          end;
      end;
    end;


  procedure export_unit(u: tmodule);
    begin
      u.globalsymtable.symlist.ForEachCall(@insert_export,u.globalsymtable);
      { check localsymtable for exports too to get public symbols }
      u.localsymtable.symlist.ForEachCall(@insert_export,u.localsymtable);

      { create special exports }
      if (u.flags and uf_init)<>0 then
        procexport(make_mangledname('INIT$',u.globalsymtable,''));
      if (u.flags and uf_finalize)<>0 then
        procexport(make_mangledname('FINALIZE$',u.globalsymtable,''));
      if (u.flags and uf_threadvars)=uf_threadvars then
        varexport(make_mangledname('THREADVARLIST',u.globalsymtable,''));
    end;

  Function RewritePPU(const PPUFn,PPLFn:String):Boolean;
    Var
      MakeStatic : Boolean;
    Var
      buffer : array[0..$1fff] of byte;
      inppu,
      outppu : tppufile;
      b,
      untilb : byte;
      l,m    : longint;
      f      : file;
      ext,
      s      : string;
      ppuversion : dword;
    begin
      Result:=false;
      MakeStatic:=False;
      inppu:=tppufile.create(PPUFn);
      if not inppu.openfile then
       begin
         inppu.free;
         Comment(V_Error,'Could not open : '+PPUFn);
         Exit;
       end;
    { Check the ppufile }
      if not inppu.CheckPPUId then
       begin
         inppu.free;
         Comment(V_Error,'Not a PPU File : '+PPUFn);
         Exit;
       end;
      ppuversion:=inppu.getversion;
      if ppuversion<CurrentPPUVersion then
       begin
         inppu.free;
         Comment(V_Error,'Wrong PPU Version '+tostr(ppuversion)+' in '+PPUFn);
         Exit;
       end;
    { No .o file generated for this ppu, just skip }
      if (inppu.header.common.flags and uf_no_link)<>0 then
       begin
         inppu.free;
         Result:=true;
         Exit;
       end;
    { Already a lib? }
      if (inppu.header.common.flags and uf_in_library)<>0 then
       begin
         inppu.free;
         Comment(V_Error,'PPU is already in a library : '+PPUFn);
         Exit;
       end;
    { We need a static linked unit }
      if (inppu.header.common.flags and uf_static_linked)=0 then
       begin
         inppu.free;
         Comment(V_Error,'PPU is not static linked : '+PPUFn);
         Exit;
       end;
    { Check if shared is allowed }
      if tsystem(inppu.header.common.target) in [system_i386_go32v2] then
       begin
         Comment(V_Error,'Shared library not supported for ppu target, switching to static library');
         MakeStatic:=true;
       end;
    { Create the new ppu }
      if PPUFn=PPLFn then
       outppu:=tppufile.create('ppumove.$$$')
      else
       outppu:=tppufile.create(PPLFn);
      outppu.createfile;
    { Create new header, with the new flags }
      outppu.header:=inppu.header;
      outppu.header.common.flags:=outppu.header.common.flags or uf_in_library;
      if MakeStatic then
       outppu.header.common.flags:=outppu.header.common.flags or uf_static_linked
      else
       outppu.header.common.flags:=outppu.header.common.flags or uf_shared_linked;
    { read until the object files are found }
      untilb:=iblinkunitofiles;
      repeat
        b:=inppu.readentry;
        if b in [ibendinterface,ibend] then
         begin
           inppu.free;
           outppu.free;
           Comment(V_Error,'No files to be linked found : '+PPUFn);
           Exit;
         end;
        if b<>untilb then
         begin
           repeat
             inppu.getdatabuf(buffer,sizeof(buffer),l);
             outppu.putdata(buffer,l);
           until l<sizeof(buffer);
           outppu.writeentry(b);
         end;
      until (b=untilb);
    { we have now reached the section for the files which need to be added,
      now add them to the list }
      case b of
        iblinkunitofiles :
          begin
            { add all o files, and save the entry when not creating a static
              library to keep staticlinking possible }
            while not inppu.endofentry do
             begin
               s:=inppu.getstring;
               m:=inppu.getlongint;
               if not MakeStatic then
                begin
                  outppu.putstring(s);
                  outppu.putlongint(m);
                end;
               current_module.linkotherofiles.add(s,link_always);;
             end;
            if not MakeStatic then
             outppu.writeentry(b);
          end;
    {    iblinkunitstaticlibs :
          begin
            AddToLinkFiles(ExtractLib(inppu.getstring));
            if not inppu.endofentry then
             begin
               repeat
                 inppu.getdatabuf(buffer^,bufsize,l);
                 outppu.putdata(buffer^,l);
               until l<bufsize;
               outppu.writeentry(b);
             end;
           end; }
      end;
    { just add a new entry with the new lib }
      if MakeStatic then
       begin
         outppu.putstring('imp'+current_module.realmodulename^);
         outppu.putlongint(link_static);
         outppu.writeentry(iblinkunitstaticlibs)
       end
      else
       begin
         outppu.putstring('imp'+current_module.realmodulename^);
         outppu.putlongint(link_shared);
         outppu.writeentry(iblinkunitsharedlibs);
       end;
    { read all entries until the end and write them also to the new ppu }
      repeat
        b:=inppu.readentry;
      { don't write ibend, that's written automatically }
        if b<>ibend then
         begin
           if b=iblinkothersharedlibs then
             begin
               while not inppu.endofentry do
                 begin
                   s:=inppu.getstring;
                   m:=inppu.getlongint;

                   outppu.putstring(s);
                   outppu.putlongint(m);

                   { strip lib prefix }
                   if copy(s,1,3)='lib' then
                     delete(s,1,3);
                   ext:=ExtractFileExt(s);
                   if ext<>'' then
                     delete(s,length(s)-length(ext)+1,length(ext));

                   current_module.linkOtherSharedLibs.add(s,link_always);
                 end;
             end
           else
             repeat
               inppu.getdatabuf(buffer,sizeof(buffer),l);
               outppu.putdata(buffer,l);
             until l<sizeof(buffer);
           outppu.writeentry(b);
         end;
      until b=ibend;
    { write the last stuff and close }
      outppu.flush;
      outppu.writeheader;
      outppu.free;
      inppu.free;
    { rename }
      if PPUFn=PPLFn then
       begin
         {$push}{$I-}
          assign(f,PPUFn);
          erase(f);
          assign(f,'ppumove.$$$');
          rename(f,PPUFn);
         {$pop}
         if ioresult<>0 then;
       end;
      Result:=True;
    end;


  procedure load_packages;
    var
      i : longint;
      pcp: tpcppackage;
      entry : ppackageentry;
    begin
      if not (tf_supports_packages in target_info.flags) then
        exit;
      for i:=0 to packagelist.count-1 do
        begin
          entry:=ppackageentry(packagelist[i]);
          if assigned(entry^.package) then
            internalerror(2013053104);
          Comment(V_Info,'Loading package: '+entry^.realpkgname);
          pcp:=tpcppackage.create(entry^.realpkgname);
          pcp.loadpcp;
          entry^.package:=pcp;
        end;
    end;


  procedure add_package(const name:string;ignoreduplicates:boolean);
    var
      entry : ppackageentry;
      i : longint;
    begin
      for i:=0 to packagelist.count-1 do
        begin
          if packagelist.nameofindex(i)=name then
            begin
              if not ignoreduplicates then
                Message1(package_e_duplicate_package,name);
              exit;
            end;
        end;
      new(entry);
      entry^.package:=nil;
      entry^.realpkgname:=name;
      packagelist.add(name,entry);
    end;


  procedure createimportlibfromexternals;
    var
      alreadyloaded : tfpobjectlist;


    procedure import_proc_symbol(pd:tprocdef;pkg:tpackage);
      var
        item : TCmdStrListItem;
      begin
        item := TCmdStrListItem(pd.aliasnames.first);
        while assigned(item) do
          begin
            current_module.addexternalimport(pkg.pplfilename,item.str,item.str,0,false,false);
            item := TCmdStrListItem(item.next);
          end;
       end;


    procedure processimportedsyms(syms:tfpobjectlist);
      var
        i,j,k,l : longint;
        pkgentry : ppackageentry;
        sym : TSymEntry;
        srsymtable : tsymtable;
        module : tmodule;
        unitentry : pcontainedunit;
        name : tsymstr;
        pd : tprocdef;
      begin
        for i:=0 to syms.count-1 do
          begin
            sym:=tsymentry(syms[i]);
            if not (sym.typ in [staticvarsym,procsym]) then
              continue;
            if alreadyloaded.indexof(sym)>=0 then
              continue;
            { determine the unit of the symbol }
            srsymtable:=sym.owner;
            while not (srsymtable.symtabletype in [staticsymtable,globalsymtable]) do
              srsymtable:=srsymtable.defowner.owner;
            module:=tmodule(loaded_units.first);
            while assigned(module) do
              begin
                if (module.globalsymtable=srsymtable) or (module.localsymtable=srsymtable) then
                  break;
                module:=tmodule(module.next);
              end;
            if not assigned(module) then
              internalerror(2014101001);
            if (uf_in_library and module.flags)=0 then
              { unit is not part of a package, so no need to handle it }
              continue;
            { loaded by a package? }
            for j:=0 to packagelist.count-1 do
              begin
                pkgentry:=ppackageentry(packagelist[j]);
                for k:=0 to pkgentry^.package.containedmodules.count-1 do
                  begin
                    unitentry:=pcontainedunit(pkgentry^.package.containedmodules[k]);
                    if unitentry^.module=module then
                      begin
                        case sym.typ of
                          staticvarsym:
                            begin
                              name:=tstaticvarsym(sym).mangledname;
                              current_module.addexternalimport(pkgentry^.package.pplfilename,name,name+suffix_indirect,0,true,false);
                            end;
                          procsym:
                            begin
                              for l:=0 to tprocsym(sym).procdeflist.count-1 do
                                begin
                                  pd:=tprocdef(tprocsym(sym).procdeflist[l]);
                                  import_proc_symbol(pd,pkgentry^.package);
                                end;
                            end;
                          else
                            internalerror(2014101001);
                        end;
                        alreadyloaded.add(sym);
                      end;
                  end;
              end;
          end;
      end;


    var
      unitentry : pcontainedunit;
      module : tmodule;
    begin
      { check each external asm symbol of each unit of the package whether it is
        contained in the unit of a loaded package (and thus an import entry
        is needed) }
      alreadyloaded:=tfpobjectlist.create(false);

      { first pass to find all symbols that were not loaded by asm name }
      module:=tmodule(loaded_units.first);
      while assigned(module) do
        begin
          //if not assigned(module.package) then
          if (uf_in_library and module.flags)=0 then
            processimportedsyms(module.unitimportsyms);
          module:=tmodule(module.next);
        end;

      alreadyloaded.free;
    end;


end.

