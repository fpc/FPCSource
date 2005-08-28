{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Linux target

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
unit t_linux;

{$i fpcdefs.inc}

interface

  uses
    symsym,symdef,ppu,
    import,export,link;

  type
    timportliblinux=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tglobalvarsym;const name,module:string);override;
      procedure generatelib;override;
    end;

    texportliblinux=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

    tlinkerlinux=class(texternallinker)
    private
      libctype:(libc5,glibc2,glibc21,uclibc);
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
      function  postprocessexecutable(const fn : string;isdll:boolean):boolean;
    end;


implementation

  uses
    cutils,cclasses,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,dos
    ,aasmbase,aasmtai,aasmcpu,cpubase,cgobj
    ,i_linux
    ;

{*****************************************************************************
                               TIMPORTLIBLINUX
*****************************************************************************}

procedure timportliblinux.preparelib(const s : string);
begin
end;


procedure timportliblinux.importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
end;


procedure timportliblinux.importvariable(vs:tglobalvarsym;const name,module:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  vs.set_mangledname(name);
  exclude(vs.varoptions,vo_is_dll_var);
end;


procedure timportliblinux.generatelib;
begin
end;


{*****************************************************************************
                               TEXPORTLIBLINUX
*****************************************************************************}

procedure texportliblinux.preparelib(const s:string);
begin
end;


procedure texportliblinux.exportprocedure(hp : texported_item);
var
  hp2 : texported_item;
begin
  { first test the index value }
  if (hp.options and eo_index)<>0 then
   begin
     Message1(parser_e_no_export_with_index_for_target,'linux');
     exit;
   end;
  { now place in correct order }
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) and
     (hp.name^>hp2.name^) do
    hp2:=texported_item(hp2.next);
  { insert hp there !! }
  if assigned(hp2) and (hp2.name^=hp.name^) then
    begin
      { this is not allowed !! }
      Message1(parser_e_export_name_double,hp.name^);
      exit;
    end;
  if hp2=texported_item(current_module._exports.first) then
    current_module._exports.concat(hp)
  else if assigned(hp2) then
    begin
       hp.next:=hp2;
       hp.previous:=hp2.previous;
       if assigned(hp2.previous) then
         hp2.previous.next:=hp;
       hp2.previous:=hp;
    end
  else
    current_module._exports.concat(hp);
end;


procedure texportliblinux.exportvar(hp : texported_item);
begin
  hp.is_var:=true;
  exportprocedure(hp);
end;


procedure texportliblinux.generatelib;
var
  hp2 : texported_item;
begin
  new_section(asmlist[al_code],sec_code,'',0);
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) do
   begin
     if (not hp2.is_var) and
        (hp2.sym.typ=procsym) then
      begin
        { the manglednames can already be the same when the procedure
          is declared with cdecl }
        if tprocsym(hp2.sym).first_procdef.mangledname<>hp2.name^ then
         begin
           { place jump in al_code }
           asmlist[al_code].concat(tai_align.create(target_info.alignment.procalign));
           asmlist[al_code].concat(Tai_symbol.Createname_global(hp2.name^,AT_FUNCTION,0));
           cg.a_jmp_name(asmlist[al_code],tprocsym(hp2.sym).first_procdef.mangledname);
           asmlist[al_code].concat(Tai_symbol_end.Createname(hp2.name^));
         end;
      end
     else
      message1(parser_e_no_export_of_variables_for_target,'linux');
     hp2:=texported_item(hp2.next);
   end;
end;


{*****************************************************************************
                                  TLINKERLINUX
*****************************************************************************}

Constructor TLinkerLinux.Create;
begin
  Inherited Create;
  if not Dontlinkstdlibpath Then
{$ifdef x86_64}
   LibrarySearchPath.AddPath('/lib64;/usr/lib64;/usr/X11R6/lib64',true);
{$else}
   LibrarySearchPath.AddPath('/lib;/usr/lib;/usr/X11R6/lib',true);
{$endif x86_64}
end;


procedure TLinkerLinux.SetDefaultInfo;
{
  This will also detect which libc version will be used
}

const
{$ifdef i386}   platform_select='-b elf32-i386 -m elf_i386';{$endif}
{$ifdef x86_64} platform_select='-b elf64-x86-64 -m elf_x86_64';{$endif}
{$ifdef powerpc}platform_select='-b elf32-powerpc -m elf32ppclinux';{$endif}
{$ifdef sparc}  platform_select='-b elf32-sparc -m elf32_sparc';{$endif}
{$ifdef arm}    platform_select='';{$endif} {unknown :( }

{$ifdef m68k}
var
  St : SearchRec;
{$endif m68k}
begin
  with Info do
   begin
     ExeCmd[1]:='ld '+platform_select+' $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE -T $RES';
     DllCmd[1]:='ld '+platform_select+' $OPT $INIT $FINI $SONAME -shared -L. -o $EXE -T $RES';
     DllCmd[2]:='strip --strip-unneeded $EXE';
{$ifdef m68k}
     libctype:=glibc2;
     FindFirst('/lib/ld*',AnyFile,st);
     while DosError=0 do
      begin
        if copy(st.name,1,5)='ld-2.' then
         begin
           DynamicLinker:='/lib/'+St.name;
           if st.name[6]<>'0' then
             libctype:=glibc21;
           break;
         end;
         FindNext(St);
      end;
     FindClose(St);
{$endif m68k}

{$ifdef i386}
     { first try glibc2 }
     DynamicLinker:='/lib/ld-linux.so.2';
     if FileExists(DynamicLinker) then
       { Check for 2.0 files, else use the glibc 2.1 stub }
       if FileExists('/lib/ld-2.0.*') then
         libctype:=glibc2
       else
         libctype:=glibc21
     else
       if fileexists('/lib/ld-uClibc.so.0') then
         begin
           libctype:=uclibc;
           dynamiclinker:='/lib/ld-uClibc.so.0';
         end
       else
         DynamicLinker:='/lib/ld-linux.so.1';
{$endif i386}

{$ifdef x86_64}
     DynamicLinker:='/lib64/ld-linux-x86-64.so.2';
     libctype:=glibc2;
{$endif x86_64}

{$ifdef sparc}
     DynamicLinker:='/lib/ld-linux.so.2';
     libctype:=glibc2;
{$endif sparc}

{$ifdef powerpc}
     DynamicLinker:='/lib/ld.so.1';
     libctype:=glibc2;
{$endif powerpc}

{$ifdef arm}
     DynamicLinker:='/lib/ld-linux.so.2';
     libctype:=glibc2;
{$endif arm}
   end;
end;


Function TLinkerLinux.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
  HPath        : TStringListItem;
  s,s1,s2      : string;
  found1,
  found2,
  linklibc     : boolean;
begin
  WriteResponseFile:=False;
{ set special options for some targets }
  linklibc:=(SharedLibFiles.Find('c')<>nil);
  if isdll then
   begin
     prtobj:='dllprt0';
     cprtobj:='dllprt0';
     gprtobj:='dllprt0';
   end
  else
   begin
     prtobj:='prt0';
     case libctype of
       glibc21:
         begin
           cprtobj:='cprt21';
           gprtobj:='gprt21';
         end;
       uclibc:
         begin
           cprtobj:='ucprt0';
           gprtobj:='ugprt0';
         end
       else
         cprtobj:='cprt0';
         gprtobj:='gprt0';
     end;
   end;
  if cs_profile in aktmoduleswitches then
   begin
     prtobj:=gprtobj;
     if not(libctype in [glibc2,glibc21]) then
       AddSharedLibrary('gmon');
     AddSharedLibrary('c');
     linklibc:=true;
   end
  else
   begin
     if linklibc then
      prtobj:=cprtobj;
   end;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT(');
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   LinkRes.AddFileName(maybequoted(FindObjectFile(prtobj,'',false)));
  { try to add crti and crtbegin if linking to C }
  if linklibc then
   begin
     if librarysearchpath.FindFile('crtbegin.o',s) then
      LinkRes.AddFileName(s);
     if librarysearchpath.FindFile('crti.o',s) then
      LinkRes.AddFileName(s);
   end;
  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(maybequoted(s));
   end;
  LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(maybequoted(s))
      end;
     LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  if not SharedLibFiles.Empty then
   begin
     LinkRes.Add('INPUT(');
     While not SharedLibFiles.Empty do
      begin
        S:=SharedLibFiles.GetFirst;
        if s<>'c' then
         begin
           i:=Pos(target_info.sharedlibext,S);
           if i>0 then
            Delete(S,i,255);
           LinkRes.Add('-l'+s);
         end
        else
         begin
           linklibc:=true;
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
      LinkRes.Add('-lc');
     { when we have -static for the linker the we also need libgcc }
     if (cs_link_staticflag in aktglobalswitches) then
      LinkRes.Add('-lgcc');
     LinkRes.Add(')');
   end;

  { objects which must be at the end }
  if linklibc and (libctype<>uclibc) then
   begin
     found1:=librarysearchpath.FindFile('crtend.o',s1);
     found2:=librarysearchpath.FindFile('crtn.o',s2);
     if found1 or found2 then
      begin
        LinkRes.Add('INPUT(');
        if found1 then
         LinkRes.AddFileName(s1);
        if found2 then
         LinkRes.AddFileName(s2);
        LinkRes.Add(')');
      end;
   end;
  {Entry point.}
  linkres.add('ENTRY(_start)');

  {Sections.}
{
  commented out because it cause problems on several machines with different ld versions (FK)
  linkres.add('SECTIONS');
  linkres.add('{');
  {Read-only sections, merged into text segment:}
  linkres.add('  PROVIDE (__executable_start = 0x010000); . = 0x010000 +0x100;');
  linkres.add('  .interp         : { *(.interp) }');
  linkres.add('  .hash           : { *(.hash) }');
  linkres.add('  .dynsym         : { *(.dynsym) }');
  linkres.add('  .dynstr         : { *(.dynstr) }');
  linkres.add('  .gnu.version    : { *(.gnu.version) }');
  linkres.add('  .gnu.version_d  : { *(.gnu.version_d) }');
  linkres.add('  .gnu.version_r  : { *(.gnu.version_r) }');
  linkres.add('  .rel.dyn        :');
  linkres.add('    {');
  linkres.add('      *(.rel.init)');
  linkres.add('      *(.rel.text .rel.text.* .rel.gnu.linkonce.t.*)');
  linkres.add('      *(.rel.fini)');
  linkres.add('      *(.rel.rodata .rel.rodata.* .rel.gnu.linkonce.r.*)');
  linkres.add('      *(.rel.data.rel.ro*)');
  linkres.add('      *(.rel.data .rel.data.* .rel.gnu.linkonce.d.*)');
  linkres.add('      *(.rel.tdata .rel.tdata.* .rel.gnu.linkonce.td.*)');
  linkres.add('      *(.rel.tbss .rel.tbss.* .rel.gnu.linkonce.tb.*)');
  linkres.add('      *(.rel.got)');
  linkres.add('      *(.rel.bss .rel.bss.* .rel.gnu.linkonce.b.*)');
  linkres.add('    }');
  linkres.add('  .rela.dyn       :');
  linkres.add('    {');
  linkres.add('      *(.rela.init)');
  linkres.add('      *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*)');
  linkres.add('      *(.rela.fini)');
  linkres.add('      *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*)');
  linkres.add('      *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*)');
  linkres.add('      *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*)');
  linkres.add('      *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*)');
  linkres.add('      *(.rela.got)');
  linkres.add('      *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*)');
  linkres.add('    }');
  linkres.add('  .rel.plt        : { *(.rel.plt) }');
  linkres.add('  .rela.plt       : { *(.rela.plt) }');
  linkres.add('  .init           :');
  linkres.add('  {');
  linkres.add('    KEEP (*(.init))');
  linkres.add('  } =0x90909090');
  linkres.add('  .plt            : { *(.plt) }');
  linkres.add('  .text           :');
  linkres.add('  {');
  linkres.add('    *(.text .stub .text.* .gnu.linkonce.t.*)');
  linkres.add('    KEEP (*(.text.*personality*))');
                   {.gnu.warning sections are handled specially by elf32.em.}
  linkres.add('    *(.gnu.warning)');
  linkres.add('  } =0x90909090');
  linkres.add('  .fini           :');
  linkres.add('  {');
  linkres.add('    KEEP (*(.fini))');
  linkres.add('  } =0x90909090');
  linkres.add('  PROVIDE (_etext = .);');
  linkres.add('  .rodata         : { *(.rodata .rodata.* .gnu.linkonce.r.*) }');
  {Adjust the address for the data segment.  We want to adjust up to
   the same address within the page on the next page up.}
  linkres.add('  . = ALIGN (0x1000) - ((0x1000 - .) & (0x1000 - 1)); . = DATA_SEGMENT_ALIGN (0x1000, 0x1000);');
  linkres.add('  .dynamic        : { *(.dynamic) }');
  linkres.add('  .got            : { *(.got) }');
  linkres.add('  .got.plt        : { *(.got.plt) }');
  linkres.add('  .data           :');
  linkres.add('  {');
  linkres.add('    *(.data .data.* .gnu.linkonce.d.*)');
  linkres.add('    KEEP (*(.gnu.linkonce.d.*personality*))');
  linkres.add('  }');
  linkres.add('  _edata = .;');
  linkres.add('  PROVIDE (edata = .);');
{$ifdef zsegment_threadvars}
  linkres.add('  _z = .;');
  linkres.add('  .threadvar 0 : AT (_z) { *(.threadvar .threadvar.* .gnu.linkonce.tv.*) }');
  linkres.add('  PROVIDE (_threadvar_size = SIZEOF(.threadvar));');
  linkres.add('  . = _z + SIZEOF (.threadvar);');
{$else}
  linkres.add('  .threadvar : { *(.threadvar .threadvar.* .gnu.linkonce.tv.*) }');
{$endif}
  linkres.add('  __bss_start = .;');
  linkres.add('  .bss            :');
  linkres.add('  {');
  linkres.add('   *(.dynbss)');
  linkres.add('   *(.bss .bss.* .gnu.linkonce.b.*)');
  linkres.add('   *(COMMON)');
  {Align here to ensure that the .bss section occupies space up to
   _end.  Align after .bss to ensure correct alignment even if the
   .bss section disappears because there are no input sections.}
  linkres.add('   . = ALIGN(32 / 8);');
  linkres.add('  }');
  linkres.add('  . = ALIGN(32 / 8);');
  linkres.add('  _end = .;');
  linkres.add('  PROVIDE (end = .);');
  linkres.add('  . = DATA_SEGMENT_END (.);');
  {Stabs debugging sections.}
  linkres.add('  .stab          0 : { *(.stab) }');
  linkres.add('  .stabstr       0 : { *(.stabstr) }');
  linkres.add('}');
}

{ Write and Close response }
  LinkRes.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;


function TLinkerLinux.MakeExecutable:boolean;
var
  binstr : String;
  cmdstr  : TCmdStr;
  success : boolean;
  DynLinkStr : string[60];
  GCSectionsStr,
  StaticStr,
  StripStr   : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  GCSectionsStr:='';
  DynLinkStr:='';
  if (cs_link_staticflag in aktglobalswitches) then
   StaticStr:='-static';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';
  if (cs_link_smart in aktglobalswitches) and
     (tf_smartlink_sections in target_info.flags) then
   GCSectionsStr:='--gc-sections';
  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   begin
     DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;
     if cshared Then
       DynLinkStr:='--shared ' + DynLinkStr;
     if rlinkpath<>'' Then
       DynLinkStr:='--rpath-link '+rlinkpath + ' '+ DynLinkStr;
   End;

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(utilsprefix+BinStr),CmdStr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);
   
  if (success) then
    success:=PostProcessExecutable(current_module.exefilename^,false);
    
 
  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerLinux.MakeSharedLibrary:boolean;
var
  InitStr,
  FiniStr,
  SoNameStr : string[80];
  binstr : String;
  cmdstr  : TCmdStr;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true);

 { Create some replacements }
  InitStr:='-init FPC_LIB_START';
  FiniStr:='-fini FPC_LIB_EXIT';
  SoNameStr:='-soname '+SplitFileName(current_module.sharedlibfilename^);

{ Call linker }
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$INIT',InitStr);
  Replace(cmdstr,'$FINI',FiniStr);
  Replace(cmdstr,'$SONAME',SoNameStr);
  success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);

{ Strip the library ? }
  if success and (cs_link_strip in aktglobalswitches) then
   begin
     { only remove non global symbols and debugging info for a library }
     Info.DllCmd[2]:='strip --discard-all --strip-debug $EXE';
     SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
     Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename^));
     success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);
   end;

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
end;

function tlinkerLinux.postprocessexecutable(const fn : string;isdll:boolean):boolean;

Var
  cmdstr: string;
  found : boolean;
  hp    : tused_unit;

begin
  postprocessexecutable:=True;
  if target_res.id=res_elf then
    begin
    hp:=tused_unit(usedunits.first);
    found:=false;
    While Assigned(hp) and not Found do
      begin
      Found:=((hp.u.flags and uf_has_resourcefiles)=uf_has_resourcefiles);
      hp:=tused_unit(hp.next);
      end;
    if found then
      begin  
      cmdstr:=' -f -i '+maybequoted(fn);
      postprocessexecutable:=DoExec(FindUtil(utilsprefix+'fpcres'),cmdstr,false,false);
      end;
    end;  
end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

initialization
{$ifdef i386}
  RegisterExternalLinker(system_i386_linux_info,TLinkerLinux);
  RegisterImport(system_i386_linux,timportliblinux);
  RegisterExport(system_i386_linux,texportliblinux);
  RegisterTarget(system_i386_linux_info);
  RegisterRes(res_elf32_info);

  RegisterExternalLinker(system_x86_6432_linux_info,TLinkerLinux);
  RegisterImport(system_x86_6432_linux,timportliblinux);
  RegisterExport(system_x86_6432_linux,texportliblinux);
  RegisterTarget(system_x86_6432_linux_info);
{$endif i386}
{$ifdef m68k}
  RegisterExternalLinker(system_m68k_linux_info,TLinkerLinux);
  RegisterImport(system_m68k_linux,timportliblinux);
  RegisterExport(system_m68k_linux,texportliblinux);
  RegisterTarget(system_m68k_linux_info);
{$endif m68k}
{$ifdef powerpc}
  RegisterExternalLinker(system_powerpc_linux_info,TLinkerLinux);
  RegisterImport(system_powerpc_linux,timportliblinux);
  RegisterExport(system_powerpc_linux,texportliblinux);
  RegisterTarget(system_powerpc_linux_info);
{$endif powerpc}
{$ifdef alpha}
  RegisterExternalLinker(system_alpha_linux_info,TLinkerLinux);
  RegisterImport(system_alpha_linux,timportliblinux);
  RegisterExport(system_alpha_linux,texportliblinux);
  RegisterTarget(system_alpha_linux_info);
{$endif alpha}
{$ifdef x86_64}
  RegisterExternalLinker(system_x86_64_linux_info,TLinkerLinux);
  RegisterImport(system_x86_64_linux,timportliblinux);
  RegisterExport(system_x86_64_linux,texportliblinux);
  RegisterTarget(system_x86_64_linux_info);
  RegisterRes(res_elf64_info);
{$endif x86_64}
{$ifdef SPARC}
  RegisterExternalLinker(system_sparc_linux_info,TLinkerLinux);
  RegisterImport(system_SPARC_linux,timportliblinux);
  RegisterExport(system_SPARC_linux,texportliblinux);
  RegisterTarget(system_SPARC_linux_info);
{$endif SPARC}
{$ifdef ARM}
  RegisterExternalLinker(system_arm_linux_info,TLinkerLinux);
  RegisterImport(system_arm_linux,timportliblinux);
  RegisterExport(system_arm_linux,texportliblinux);
  RegisterTarget(system_arm_linux_info);
{$endif ARM}
end.
