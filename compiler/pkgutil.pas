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
    fmodule;

  procedure createimportlibfromexports;
  Function RewritePPU(const PPUFn,PPLFn:String):Boolean;
  procedure export_unit(u:tmodule);
  procedure load_packages;

implementation

  uses
    sysutils,
    globtype,systems,
    cutils,cclasses,
    globals,verbose,
    symtype,symconst,symsym,symdef,symbase,symtable,
    ppu,entfile,fpcp,fpkg,
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
                begin
                  def:=tabstractrecorddef(ttypesym(sym).typedef);
                  def.symtable.symlist.foreachcall(@exportabstractrecordsymproc,def.symtable);
                end;
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


  procedure insert_export(sym : TObject;arg:pointer);
    var
      i : longint;
      item : TCmdStrListItem;
      def : tabstractrecorddef;
      hp : texported_item;
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
                begin
                  def:=tabstractrecorddef(ttypesym(sym).typedef);
                  def.symtable.SymList.ForEachCall(@exportabstractrecordsymproc,def.symtable);
                  if (def.typ=objectdef) and (oo_has_vmt in tobjectdef(def).objectoptions) then
                    begin
                      hp:=texported_item.create;
                      hp.name:=stringdup(tobjectdef(def).vmt_mangledname);
                      hp.options:=hp.options+[eo_name];
                      exportlib.exportvar(hp);
                    end;
                end;
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


  procedure createimportlibfromexports;
    var
      hp : texported_item;
    begin
      hp:=texported_item(current_module._exports.first);
      while assigned(hp) do
        begin
          current_module.AddExternalImport(current_module.realmodulename^,hp.name^,hp.name^,hp.index,hp.is_var,false);
          hp:=texported_item(hp.next);
        end;
    end;

end.

