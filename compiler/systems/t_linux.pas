{
    Copyright (c) 1998-2008 by Peter Vreman

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
    aasmdata,
    symsym,symdef,ppu,
    import,export,expunix,link;

  type
    timportliblinux=class(timportlib)
      procedure generatelib;override;
    end;

    texportliblinux=class(texportlibunix)
      procedure setfininame(list: TAsmList; const s: string); override;
    end;

    tlinkerlinux=class(texternallinker)
    private
      libctype:(libc5,glibc2,glibc21,uclibc);
      cprtobj,
      gprtobj,
      prtobj  : string[80];
      reorder : boolean;
      linklibc: boolean;
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      procedure InitSysInitUnitName;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
      procedure LoadPredefinedLibraryOrder; override;
    end;


implementation

  uses
    SysUtils,
    cutils,cfileutl,cclasses,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,
    aasmbase,aasmtai,aasmcpu,cpubase,
    cgbase,cgobj,cgutils,ogbase,ncgutil,
    comprsrc,
    rescmn, i_linux
    ;

{*****************************************************************************
                               TIMPORTLIBLINUX
*****************************************************************************}

    procedure timportliblinux.generatelib;
      var
        i : longint;
        ImportLibrary : TImportLibrary;
      begin
        for i:=0 to current_module.ImportLibraryList.Count-1 do
          begin
            ImportLibrary:=TImportLibrary(current_module.ImportLibraryList[i]);
            current_module.linkothersharedlibs.add(ImportLibrary.Name,link_always);
          end;
      end;


{*****************************************************************************
                               TEXPORTLIBLINUX
*****************************************************************************}

    procedure texportliblinux.setfininame(list: TAsmList; const s: string);
      begin
        { the problem with not having a .fini section is that a finalization
          routine in regular code can get "smart" linked away -> reference it
          just like the debug info }
        list.concat(Tai_section.create(sec_fpc,'links',0));
        list.concat(Tai_const.Createname(s,0));
        inherited setfininame(list,s);
      end;

{*****************************************************************************
                                  TLINKERLINUX
*****************************************************************************}

Constructor TLinkerLinux.Create;
begin
  Inherited Create;
  if not Dontlinkstdlibpath Then
{$ifdef x86_64}
   LibrarySearchPath.AddPath(sysrootpath,'/lib64;/usr/lib64;/usr/X11R6/lib64',true);
{$else}
{$ifdef powerpc64}
   LibrarySearchPath.AddPath(sysrootpath,'/lib64;/usr/lib64;/usr/X11R6/lib64',true);
{$else powerpc64}
   LibrarySearchPath.AddPath(sysrootpath,'/lib;/usr/lib;/usr/X11R6/lib',true);
{$endif powerpc64}
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
{$ifdef POWERPC64}  platform_select='-b elf64-powerpc -m elf64ppc';{$endif}
{$ifdef sparc}  platform_select='-b elf32-sparc -m elf32_sparc';{$endif}
{$ifdef arm}    platform_select='';{$endif} {unknown :( }
{$ifdef m68k}    platform_select='';{$endif} {unknown :( }

var
  defdynlinker: string;
begin
  with Info do
   begin
     ExeCmd[1]:='ld '+platform_select+' $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE';
     { when we want to cross-link we need to override default library paths }
     if length(sysrootpath) > 0 then
       ExeCmd[1]:=ExeCmd[1]+' -T';
     ExeCmd[1]:=ExeCmd[1]+' $RES';
     DllCmd[1]:='ld '+platform_select+' $OPT $INIT $FINI $SONAME -shared -L. -o $EXE $RES';
     DllCmd[2]:='strip --strip-unneeded $EXE';
     ExtDbgCmd[1]:='objcopy --only-keep-debug $EXE $DBG';
     ExtDbgCmd[2]:='objcopy --add-gnu-debuglink=$DBG $EXE';
     ExtDbgCmd[3]:='strip --strip-unneeded $EXE';

{$ifdef m68k}
     { experimental, is this correct? }
     defdynlinker:='/lib/ld-linux.so.2';
{$endif m68k}

{$ifdef i386}
     defdynlinker:='/lib/ld-linux.so.2';
{$endif}

{$ifdef x86_64}
     defdynlinker:='/lib64/ld-linux-x86-64.so.2';
{$endif x86_64}

{$ifdef sparc}
     defdynlinker:='/lib/ld-linux.so.2';
{$endif sparc}

{$ifdef powerpc}
     defdynlinker:='/lib/ld.so.1';
{$endif powerpc}

{$ifdef powerpc64}
     defdynlinker:='/lib64/ld64.so.1';
{$endif powerpc64}

{$ifdef arm}
{$ifdef FPC_ARMEL}
     defdynlinker:='/lib/ld-linux.so.3';
{$else FPC_ARMEL}
     defdynlinker:='/lib/ld-linux.so.2';
{$endif FPC_ARMEL}
{$endif arm}

     {
       Search order:
         glibc 2.1+
         uclibc
         glibc 2.0
       If none is found (e.g. when cross compiling) glibc21 is assumed
     }
     if fileexists(sysrootpath+defdynlinker,false) then
       begin
         DynamicLinker:=defdynlinker;
{$ifdef i386}
         libctype:=glibc21;
{$else i386}
         libctype:=glibc2;
{$endif i386}
       end
     else if fileexists(sysrootpath+'/lib/ld-uClibc.so.0',false) then
       begin
         dynamiclinker:='/lib/ld-uClibc.so.0';
         libctype:=uclibc;
       end
{$ifdef i386}
     else if FileExists(sysrootpath+'/lib/ld-linux.so.1',false) then
       begin
         DynamicLinker:='/lib/ld-linux.so.1';
         libctype:=glibc2;
       end
{$endif i386}
     else
       begin
         { when no dyn. linker is found, we are probably
           cross compiling, so use the default dyn. linker }
         DynamicLinker:=defdynlinker;
         {
           the default c startup script is gcrt0.as on all platforms
           except i386
         }
{$ifdef i386}
         libctype:=glibc21;
{$else i386}
         libctype:=glibc2;
{$endif i386}
       end;
   end;
end;


procedure TLinkerLinux.LoadPredefinedLibraryOrder;
// put your linkorder/linkalias overrides here.
// Note: assumes only called when reordering/aliasing is used.
Begin
   if not (cs_link_no_default_lib_order in  current_settings.globalswitches) Then
        Begin
          LinkLibraryOrder.add('gcc','',15);
          LinkLibraryOrder.add('c','',100);
          LinkLibraryOrder.add('gmon','',120);
          LinkLibraryOrder.add('dl','',140);
          LinkLibraryOrder.add('pthread','',160);
         end;
End;

Procedure TLinkerLinux.InitSysInitUnitName;
var
  csysinitunit,
  gsysinitunit : string[20];
  hp           : tmodule;
begin
  hp:=tmodule(loaded_units.first);
  while assigned(hp) do
   begin
     linklibc := hp.linkothersharedlibs.find('c');
     if linklibc then break;
     hp:=tmodule(hp.next);
   end;
  reorder := linklibc and ReOrderEntries;
  if current_module.islibrary then
   begin
     sysinitunit:='dll';
     csysinitunit:='dll';
     gsysinitunit:='dll';
     prtobj:='dllprt0';
     cprtobj:='dllprt0';
     gprtobj:='dllprt0';
   end
  else
   begin
     prtobj:='prt0';
     sysinitunit:='prc';
     case libctype of
       glibc21:
         begin
           cprtobj:='cprt21';
           gprtobj:='gprt21';
           csysinitunit:='c21';
           gsysinitunit:='c21g';
         end;
       uclibc:
         begin
           cprtobj:='ucprt0';
           gprtobj:='ugprt0';
           csysinitunit:='uc';
           gsysinitunit:='ucg';
         end
       else
         cprtobj:='cprt0';
         gprtobj:='gprt0';
         csysinitunit:='c';
         gsysinitunit:='g';
     end;
   end;
  if cs_profile in current_settings.moduleswitches then
   begin
     prtobj:=gprtobj;
     sysinitunit:=gsysinitunit;
     linklibc:=true;
   end
  else
   begin
     if linklibc then
      begin
       prtobj:=cprtobj;
       sysinitunit:=csysinitunit;
      end;
   end;
  sysinitunit:='si_'+sysinitunit;
end;

Function TLinkerLinux.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  HPath        : TCmdStrListItem;
  s,s1,s2      : TCmdStr;
  found1,
  found2       : boolean;
begin
  result:=False;
{ set special options for some targets }
  if cs_profile in current_settings.moduleswitches then
   begin
     if not(libctype in [glibc2,glibc21]) then
       AddSharedLibrary('gmon');
     AddSharedLibrary('c');
   end;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);
  with linkres do
    begin
      { Write path to search libraries }
      HPath:=TCmdStrListItem(current_module.locallibrarysearchpath.First);
      while assigned(HPath) do
       begin
         Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
         HPath:=TCmdStrListItem(HPath.Next);
       end;
      HPath:=TCmdStrListItem(LibrarySearchPath.First);
      while assigned(HPath) do
       begin
         Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
         HPath:=TCmdStrListItem(HPath.Next);
       end;

      { force local symbol resolution (i.e., inside the shared }
      { library itself) for all non-exorted symbols, otherwise }
      { several RTL symbols of FPC-compiled shared libraries   }
      { will be bound to those of a single shared library or   }
      { to the main program                                    }
      if (isdll) then
        begin
          add('VERSION');
          add('{');
          add('  {');
          if not texportlibunix(exportlib).exportedsymnames.empty then
            begin
              add('    global:');
              repeat
                add('      '+texportlibunix(exportlib).exportedsymnames.getfirst+';');
              until texportlibunix(exportlib).exportedsymnames.empty;
            end;
          add('    local:');
          add('      *;');
          add('  };');
          add('}');
        end;

      StartSection('INPUT(');
      { add objectfiles, start with prt0 always }
      if not (target_info.system in system_internal_sysinit) and (prtobj<>'') then
       AddFileName(maybequoted(FindObjectFile(prtobj,'',false)));
      { try to add crti and crtbegin if linking to C }
      if linklibc and (libctype<>uclibc) then
       begin
         { crti.o must come first }
         if librarysearchpath.FindFile('crti.o',false,s) then
           AddFileName(s);
         { then the crtbegin* }
         { x86_64 requires this to use entry/exit code with pic,
           see also issue #8210 regarding a discussion
           no idea about the other non i386 CPUs (FK)
         }
{$ifdef x86_64}
         if current_module.islibrary then
           begin
             if librarysearchpath.FindFile('crtbeginS.o',false,s) then
               AddFileName(s);
           end
         else
{$endif x86_64}
           if (cs_link_staticflag in current_settings.globalswitches) and
              librarysearchpath.FindFile('crtbeginT.o',false,s) then
             AddFileName(s)
           else if librarysearchpath.FindFile('crtbegin.o',false,s) then
             AddFileName(s);
       end;
      { main objectfiles }
      while not ObjectFiles.Empty do
       begin
         s:=ObjectFiles.GetFirst;
         if s<>'' then
          AddFileName(maybequoted(s));
       end;
      EndSection(')');

      { Write staticlibraries }
      if not StaticLibFiles.Empty then
       begin
         Add('GROUP(');
         While not StaticLibFiles.Empty do
          begin
            S:=StaticLibFiles.GetFirst;
            AddFileName(maybequoted(s))
          end;
         Add(')');
       end;

      // we must reorder here because the result could empty sharedlibfiles
      if reorder Then
        ExpandAndApplyOrder(SharedLibFiles);
      // after this point addition of shared libs not allowed.

      { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
        here to be sure that it gets linked this is needed for glibc2 systems (PFV) }

      if not SharedLibFiles.Empty then
       begin

         if (SharedLibFiles.Count<>1) or
            (TCmdStrListItem(SharedLibFiles.First).Str<>'c') or
            reorder then
           begin
             Add('INPUT(');
             While not SharedLibFiles.Empty do
              begin
                S:=SharedLibFiles.GetFirst;
                if (s<>'c') or reorder then
                 begin
                   i:=Pos(target_info.sharedlibext,S);
                   if i>0 then
                    Delete(S,i,255);
                   Add('-l'+s);
                 end
                else
                 begin
                   linklibc:=true;
                 end;
              end;
             Add(')');
           end
         else
           linklibc:=true;
         if (cs_link_staticflag in current_settings.globalswitches) or
            (linklibc and not reorder) then
           begin
             Add('GROUP(');
             { when we have -static for the linker the we also need libgcc }
             if (cs_link_staticflag in current_settings.globalswitches) then
               begin
                 Add('-lgcc');
                 if librarysearchpath.FindFile('libgcc_eh.a',false,s1) then
                   Add('-lgcc_eh');
               end;
             { be sure that libc is the last lib }
             if linklibc and not reorder then
               Add('-lc');
             Add(')');
           end;
       end;

      { objects which must be at the end }
      if linklibc and (libctype<>uclibc) then
       begin
         { x86_64 requires this to use entry/exit code with pic,
           see also issue #8210 regarding a discussion
           no idea about the other non i386 CPUs (FK)
         }
{$ifdef x86_64}
         if current_module.islibrary then
           found1:=librarysearchpath.FindFile('crtendS.o',false,s1)
         else
{$endif x86_64}
           found1:=librarysearchpath.FindFile('crtend.o',false,s1);
         found2:=librarysearchpath.FindFile('crtn.o',false,s2);
         if found1 or found2 then
          begin
            Add('INPUT(');
            if found1 then
             AddFileName(s1);
            if found2 then
             AddFileName(s2);
            Add(')');
          end;
       end;

      {Entry point.}
      add('ENTRY(_start)');

{$ifdef x86_64}
{$define LINKERSCRIPT_INCLUDED}
      add('SECTIONS');
      add('{');
      {Read-only sections, merged into text segment:}
      if current_module.islibrary  then
        add('  . = 0 +  SIZEOF_HEADERS;')
      else
        add('  PROVIDE (__executable_start = 0x0400000); . = 0x0400000 +  SIZEOF_HEADERS;');
      add('  . = 0 +  SIZEOF_HEADERS;');
      add('  .interp         : { *(.interp) }');
      add('  .hash           : { *(.hash) }');
      add('  .dynsym         : { *(.dynsym) }');
      add('  .dynstr         : { *(.dynstr) }');
      add('  .gnu.version    : { *(.gnu.version) }');
      add('  .gnu.version_d  : { *(.gnu.version_d) }');
      add('  .gnu.version_r  : { *(.gnu.version_r) }');
      add('  .rel.dyn        :');
      add('    {');
      add('      *(.rel.init)');
      add('      *(.rel.text .rel.text.* .rel.gnu.linkonce.t.*)');
      add('      *(.rel.fini)');
      add('      *(.rel.rodata .rel.rodata.* .rel.gnu.linkonce.r.*)');
      add('      *(.rel.data.rel.ro*)');
      add('      *(.rel.data .rel.data.* .rel.gnu.linkonce.d.*)');
      add('      *(.rel.tdata .rel.tdata.* .rel.gnu.linkonce.td.*)');
      add('      *(.rel.tbss .rel.tbss.* .rel.gnu.linkonce.tb.*)');
      add('      *(.rel.got)');
      add('      *(.rel.bss .rel.bss.* .rel.gnu.linkonce.b.*)');
      add('    }');
      add('  .rela.dyn       :');
      add('    {');
      add('      *(.rela.init)');
      add('      *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*)');
      add('      *(.rela.fini)');
      add('      *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*)');
      add('      *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*)');
      add('      *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*)');
      add('      *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*)');
      add('      *(.rela.got)');
      add('      *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*)');
      add('    }');
      add('  .rel.plt        : { *(.rel.plt) }');
      add('  .rela.plt       : { *(.rela.plt) }');
      add('  .init           :');
      add('  {');
      add('    KEEP (*(.init))');
      add('  } =0x90909090');
      add('  .plt            : { *(.plt) }');
      add('  .text           :');
      add('  {');
      add('    *(.text .stub .text.* .gnu.linkonce.t.*)');
      add('    KEEP (*(.text.*personality*))');
      {.gnu.warning sections are handled specially by elf32.em.}
      add('    *(.gnu.warning)');
      add('  } =0x90909090');
      add('  .fini           :');
      add('  {');
      add('    KEEP (*(.fini))');
      add('  } =0x90909090');
      add('  PROVIDE (_etext = .);');
      add('  .rodata         :');
      add('  {');
      add('    *(.rodata .rodata.* .gnu.linkonce.r.*)');
      add('  }');
      {Adjust the address for the data segment.  We want to adjust up to
       the same address within the page on the next page up.}
      add('  . = ALIGN (0x1000) - ((0x1000 - .) & (0x1000 - 1));');
      add('  .dynamic        : { *(.dynamic) }');
      add('  .got            : { *(.got .toc) }');
      add('  .got.plt        : { *(.got.plt .toc.plt) }');
      add('  .data           :');
      add('  {');
      add('    *(.data .data.* .gnu.linkonce.d.*)');
      add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      add('    KEEP (*(.gnu.linkonce.d.*personality*))');
      add('  }');
      add('  PROVIDE (_edata = .);');
      add('  PROVIDE (edata = .);');
    {$ifdef zsegment_threadvars}
      add('  _z = .;');
      add('  .threadvar 0 : AT (_z) { *(.threadvar .threadvar.* .gnu.linkonce.tv.*) }');
      add('  PROVIDE (_threadvar_size = SIZEOF(.threadvar));');
      add('  . = _z + SIZEOF (.threadvar);');
    {$else}
      add('  .threadvar : { *(.threadvar .threadvar.* .gnu.linkonce.tv.*) }');
    {$endif}
      add('  __bss_start = .;');
      add('  .bss            :');
      add('  {');
      add('   *(.dynbss)');
      add('   *(.bss .bss.* .gnu.linkonce.b.*)');
      add('   *(COMMON)');
      {Align here to ensure that the .bss section occupies space up to
       _end.  Align after .bss to ensure correct alignment even if the
       .bss section disappears because there are no input sections.}
      add('   . = ALIGN(32 / 8);');
      add('  }');
      add('  . = ALIGN(32 / 8);');
      add('  PROVIDE (_end = .);');
      add('  PROVIDE (end = .);');
      {Stabs debugging sections.}
      add('  .stab          0 : { *(.stab) }');
      add('  .stabstr       0 : { *(.stabstr) }');
      add('  /* DWARF debug sections.');
      add('     Symbols in the DWARF debugging sections are relative to the beginning');
      add('     of the section so we begin them at 0.  */');
      add('  /* DWARF 1 */');
      add('  .debug          0 : { *(.debug) }');
      add('  .line           0 : { *(.line) }');
      add('  /* GNU DWARF 1 extensions */');
      add('  .debug_srcinfo  0 : { *(.debug_srcinfo) }');
      add('  .debug_sfnames  0 : { *(.debug_sfnames) }');
      add('  /* DWARF 1.1 and DWARF 2 */');
      add('  .debug_aranges  0 : { *(.debug_aranges) }');
      add('  .debug_pubnames 0 : { *(.debug_pubnames) }');
      add('  /* DWARF 2 */');
      add('  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }');
      add('  .debug_abbrev   0 : { *(.debug_abbrev) }');
      add('  .debug_line     0 : { *(.debug_line) }');
      add('  .debug_frame    0 : { *(.debug_frame) }');
      add('  .debug_str      0 : { *(.debug_str) }');
      add('  .debug_loc      0 : { *(.debug_loc) }');
      add('  .debug_macinfo  0 : { *(.debug_macinfo) }');
      add('  /* SGI/MIPS DWARF 2 extensions */');
      add('  .debug_weaknames 0 : { *(.debug_weaknames) }');
      add('  .debug_funcnames 0 : { *(.debug_funcnames) }');
      add('  .debug_typenames 0 : { *(.debug_typenames) }');
      add('  .debug_varnames  0 : { *(.debug_varnames) }');
      add('  /DISCARD/ : { *(.note.GNU-stack) }');
      add('}');
{$endif x86_64}

{$ifdef ARM}
      if target_info.abi=abi_eabi then
        begin
          { from GNU ld (CodeSourcery Sourcery G++ Lite 2007q3-53) 2.18.50.20070820 }
          add('/* Script for -z combreloc: combine and sort reloc sections */');
          add('OUTPUT_FORMAT("elf32-littlearm", "elf32-bigarm",');
          add('	      "elf32-littlearm")');
          add('OUTPUT_ARCH(arm)');
          add('ENTRY(_start)');
          add('SEARCH_DIR("=/usr/local/lib"); SEARCH_DIR("=/lib"); SEARCH_DIR("=/usr/lib");');
          add('SECTIONS');
          add('{');
          add('  /* Read-only sections, merged into text segment: */');
          add('  PROVIDE (__executable_start = 0x8000); . = 0x8000 + SIZEOF_HEADERS;');
          add('  .interp         : { *(.interp) }');
          add('  .note.gnu.build-id : { *(.note.gnu.build-id) }');
          add('  .hash           : { *(.hash) }');
          add('  .gnu.hash       : { *(.gnu.hash) }');
          add('  .dynsym         : { *(.dynsym) }');
          add('  .dynstr         : { *(.dynstr) }');
          add('  .gnu.version    : { *(.gnu.version) }');
          add('  .gnu.version_d  : { *(.gnu.version_d) }');
          add('  .gnu.version_r  : { *(.gnu.version_r) }');
          add('  .rel.dyn        :');
          add('    {');
          add('      *(.rel.init)');
          add('      *(.rel.text .rel.text.* .rel.gnu.linkonce.t.*)');
          add('      *(.rel.fini)');
          add('      *(.rel.rodata .rel.rodata.* .rel.gnu.linkonce.r.*)');
          add('      *(.rel.data.rel.ro* .rel.gnu.linkonce.d.rel.ro.*)');
          add('      *(.rel.data .rel.data.* .rel.gnu.linkonce.d.*)');
          add('      *(.rel.tdata .rel.tdata.* .rel.gnu.linkonce.td.*)');
          add('      *(.rel.tbss .rel.tbss.* .rel.gnu.linkonce.tb.*)');
          add('      *(.rel.ctors)');
          add('      *(.rel.dtors)');
          add('      *(.rel.got)');
          add('      *(.rel.bss .rel.bss.* .rel.gnu.linkonce.b.*)');
          add('    }');
          add('  .rela.dyn       :');
          add('    {');
          add('      *(.rela.init)');
          add('      *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*)');
          add('      *(.rela.fini)');
          add('      *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*)');
          add('      *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*)');
          add('      *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*)');
          add('      *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*)');
          add('      *(.rela.ctors)');
          add('      *(.rela.dtors)');
          add('      *(.rela.got)');
          add('      *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*)');
          add('    }');
          add('  .rel.plt        : { *(.rel.plt) }');
          add('  .rela.plt       : { *(.rela.plt) }');
          add('  .init           :');
          add('  {');
          add('    KEEP (*(.init))');
          add('  } =0');
          add('  .plt            : { *(.plt) }');
          add('  .text           :');
          add('  {');
          add('    *(.text .stub .text.* .gnu.linkonce.t.*)');
          add('    KEEP (*(.text.*personality*))');
          add('    /* .gnu.warning sections are handled specially by elf32.em.  */');
          add('    *(.gnu.warning)');
          add('    *(.glue_7t) *(.glue_7) *(.vfp11_veneer)');
          add('  } =0');
          add('  .fini           :');
          add('  {');
          add('    KEEP (*(.fini))');
          add('  } =0');
          add('  PROVIDE (__etext = .);');
          add('  PROVIDE (_etext = .);');
          add('  PROVIDE (etext = .);');
          add('  .rodata         : { *(.rodata .rodata.* .gnu.linkonce.r.*) }');
          add('  .rodata1        : { *(.rodata1) }');
          add('  .ARM.extab   : { *(.ARM.extab* .gnu.linkonce.armextab.*) }');
          add('   __exidx_start = .;');
          add('  .ARM.exidx   : { *(.ARM.exidx* .gnu.linkonce.armexidx.*) }');
          add('   __exidx_end = .;');
          add('  .eh_frame_hdr : { *(.eh_frame_hdr) }');
          add('  .eh_frame       : ONLY_IF_RO { KEEP (*(.eh_frame)) }');
          add('  .gcc_except_table   : ONLY_IF_RO { *(.gcc_except_table .gcc_except_table.*) }');
          add('  /* Adjust the address for the data segment.  We want to adjust up to');
          add('     the same address within the page on the next page up.  */');
          add('  . = ALIGN(CONSTANT (MAXPAGESIZE)) + (. & (CONSTANT (MAXPAGESIZE) - 1));');
          add('  /* Exception handling  */');
          add('  .eh_frame       : ONLY_IF_RW { KEEP (*(.eh_frame)) }');
          add('  .gcc_except_table   : ONLY_IF_RW { *(.gcc_except_table .gcc_except_table.*) }');
          add('  /* Thread Local Storage sections  */');
          add('  .tdata	  : { *(.tdata .tdata.* .gnu.linkonce.td.*) }');
          add('  .tbss		  : { *(.tbss .tbss.* .gnu.linkonce.tb.*) *(.tcommon) }');
          add('  .preinit_array     :');
          add('  {');
          add('    PROVIDE_HIDDEN (__preinit_array_start = .);');
          add('    KEEP (*(.preinit_array))');
          add('    PROVIDE_HIDDEN (__preinit_array_end = .);');
          add('  }');
          add('  .init_array     :');
          add('  {');
          add('     PROVIDE_HIDDEN (__init_array_start = .);');
          add('     KEEP (*(SORT(.init_array.*)))');
          add('     KEEP (*(.init_array))');
          add('     PROVIDE_HIDDEN (__init_array_end = .);');
          add('  }');
          add('  .fini_array     :');
          add('  {');
          add('    PROVIDE_HIDDEN (__fini_array_start = .);');
          add('    KEEP (*(.fini_array))');
          add('    KEEP (*(SORT(.fini_array.*)))');
          add('    PROVIDE_HIDDEN (__fini_array_end = .);');
          add('  }');
          add('  .ctors          :');
          add('  {');
          add('    /* gcc uses crtbegin.o to find the start of');
          add('       the constructors, so we make sure it is');
          add('       first.  Because this is a wildcard, it');
          add('       doesn''t matter if the user does not');
          add('       actually link against crtbegin.o; the');
          add('       linker won''t look for a file to match a');
          add('       wildcard.  The wildcard also means that it');
          add('       doesn''t matter which directory crtbegin.o');
          add('       is in.  */');
          add('    KEEP (*crtbegin.o(.ctors))');
          add('    KEEP (*crtbegin?.o(.ctors))');
          add('    /* We don''t want to include the .ctor section from');
          add('       the crtend.o file until after the sorted ctors.');
          add('       The .ctor section from the crtend file contains the');
          add('       end of ctors marker and it must be last */');
          add('    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o ) .ctors))');
          add('    KEEP (*(SORT(.ctors.*)))');
          add('    KEEP (*(.ctors))');
          add('  }');
          add('  .dtors          :');
          add('  {');
          add('    KEEP (*crtbegin.o(.dtors))');
          add('    KEEP (*crtbegin?.o(.dtors))');
          add('    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o ) .dtors))');
          add('    KEEP (*(SORT(.dtors.*)))');
          add('    KEEP (*(.dtors))');
          add('  }');
          add('  .jcr            : { KEEP (*(.jcr)) }');
          add('  .data.rel.ro : { *(.data.rel.ro.local* .gnu.linkonce.d.rel.ro.local.*) *(.data.rel.ro* .gnu.linkonce.d.rel.ro.*) }');
          add('  .dynamic        : { *(.dynamic) }');
          add('  .got            : { *(.got.plt) *(.got) }');
          add('  .data           :');
          add('  {');
          add('    __data_start = . ;');
          add('    *(.data .data.* .gnu.linkonce.d.*)');

          { extra by FPC }
          add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');

          add('    KEEP (*(.gnu.linkonce.d.*personality*))');
          add('    SORT(CONSTRUCTORS)');
          add('  }');
          add('  .data1          : { *(.data1) }');
          add('  _edata = .; PROVIDE (edata = .);');
          add('  __bss_start = .;');
          add('  __bss_start__ = .;');
          add('  .bss            :');
          add('  {');
          add('   *(.dynbss)');
          add('   *(.bss .bss.* .gnu.linkonce.b.*)');
          add('   *(COMMON)');
          add('   /* Align here to ensure that the .bss section occupies space up to');
          add('      _end.  Align after .bss to ensure correct alignment even if the');
          add('      .bss section disappears because there are no input sections.');
          add('      FIXME: Why do we need it? When there is no .bss section, we don''t');
          add('      pad the .data section.  */');
          add('   . = ALIGN(. != 0 ? 32 / 8 : 1);');
          add('  }');
          add('  _bss_end__ = . ; __bss_end__ = . ;');
          add('  . = ALIGN(32 / 8);');
          add('  . = ALIGN(32 / 8);');
          add('  __end__ = . ;');
          add('  _end = .; PROVIDE (end = .);');
          add('  /* Stabs debugging sections.  */');
          add('  .stab          0 : { *(.stab) }');
          add('  .stabstr       0 : { *(.stabstr) }');
          add('  .stab.excl     0 : { *(.stab.excl) }');
          add('  .stab.exclstr  0 : { *(.stab.exclstr) }');
          add('  .stab.index    0 : { *(.stab.index) }');
          add('  .stab.indexstr 0 : { *(.stab.indexstr) }');
          add('  .comment       0 : { *(.comment) }');
          add('  /* DWARF debug sections.');
          add('     Symbols in the DWARF debugging sections are relative to the beginning');
          add('     of the section so we begin them at 0.  */');
          add('  /* DWARF 1 */');
          add('  .debug          0 : { *(.debug) }');
          add('  .line           0 : { *(.line) }');
          add('  /* GNU DWARF 1 extensions */');
          add('  .debug_srcinfo  0 : { *(.debug_srcinfo) }');
          add('  .debug_sfnames  0 : { *(.debug_sfnames) }');
          add('  /* DWARF 1.1 and DWARF 2 */');
          add('  .debug_aranges  0 : { *(.debug_aranges) }');
          add('  .debug_pubnames 0 : { *(.debug_pubnames) }');
          add('  /* DWARF 2 */');
          add('  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }');
          add('  .debug_abbrev   0 : { *(.debug_abbrev) }');
          add('  .debug_line     0 : { *(.debug_line) }');
          add('  .debug_frame    0 : { *(.debug_frame) }');
          add('  .debug_str      0 : { *(.debug_str) }');
          add('  .debug_loc      0 : { *(.debug_loc) }');
          add('  .debug_macinfo  0 : { *(.debug_macinfo) }');
          add('  /* SGI/MIPS DWARF 2 extensions */');
          add('  .debug_weaknames 0 : { *(.debug_weaknames) }');
          add('  .debug_funcnames 0 : { *(.debug_funcnames) }');
          add('  .debug_typenames 0 : { *(.debug_typenames) }');
          add('  .debug_varnames  0 : { *(.debug_varnames) }');
          add('  /* DWARF 3 */');
          add('  .debug_pubtypes 0 : { *(.debug_pubtypes) }');
          add('  .debug_ranges   0 : { *(.debug_ranges) }');
          add('    .stack         0x80000 :');
          add('  {');
          add('    _stack = .;');
          add('    *(.stack)');
          add('  }');
          add('  .ARM.attributes 0 : { KEEP (*(.ARM.attributes)) KEEP (*(.gnu.attributes)) }');
          add('  .note.gnu.arm.ident 0 : { KEEP (*(.note.gnu.arm.ident)) }');
          add('  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) }');
          add('}');
        end
      else
{$endif ARM}

{$ifndef LINKERSCRIPT_INCLUDED}
        begin
          {Sections.}
          add('SECTIONS');
          add('{');
          {Read-only sections, merged into text segment:}
          add('  PROVIDE (__executable_start = 0x010000); . = 0x010000 + SIZEOF_HEADERS;');
          add('  .interp         : { *(.interp) }');
          add('  .hash           : { *(.hash) }');
          add('  .dynsym         : { *(.dynsym) }');
          add('  .dynstr         : { *(.dynstr) }');
          add('  .gnu.version    : { *(.gnu.version) }');
          add('  .gnu.version_d  : { *(.gnu.version_d) }');
          add('  .gnu.version_r  : { *(.gnu.version_r) }');
          add('  .rel.dyn        :');
          add('    {');
          add('      *(.rel.init)');
          add('      *(.rel.text .rel.text.* .rel.gnu.linkonce.t.*)');
          add('      *(.rel.fini)');
          add('      *(.rel.rodata .rel.rodata.* .rel.gnu.linkonce.r.*)');
          add('      *(.rel.data.rel.ro*)');
          add('      *(.rel.data .rel.data.* .rel.gnu.linkonce.d.*)');
          add('      *(.rel.tdata .rel.tdata.* .rel.gnu.linkonce.td.*)');
          add('      *(.rel.tbss .rel.tbss.* .rel.gnu.linkonce.tb.*)');
          add('      *(.rel.got)');
          add('      *(.rel.bss .rel.bss.* .rel.gnu.linkonce.b.*)');
          add('    }');
          add('  .rela.dyn       :');
          add('    {');
          add('      *(.rela.init)');
          add('      *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*)');
          add('      *(.rela.fini)');
          add('      *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*)');
          add('      *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*)');
          add('      *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*)');
          add('      *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*)');
          add('      *(.rela.got)');
          add('      *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*)');
          add('    }');
          add('  .rel.plt        : { *(.rel.plt) }');
          add('  .rela.plt       : { *(.rela.plt) }');
          add('  .init           :');
          add('  {');
          add('    KEEP (*(.init))');
          add('  } =0x90909090');
          add('  .plt            : { *(.plt) }');
          add('  .text           :');
          add('  {');
          add('    *(.text .stub .text.* .gnu.linkonce.t.*)');
          add('    KEEP (*(.text.*personality*))');
          {.gnu.warning sections are handled specially by elf32.em.}
          add('    *(.gnu.warning)');
          add('  } =0x90909090');
          add('  .fini           :');
          add('  {');
          add('    KEEP (*(.fini))');
          add('  } =0x90909090');
          add('  PROVIDE (_etext = .);');
          add('  .rodata         :');
          add('  {');
          add('    *(.rodata .rodata.* .gnu.linkonce.r.*)');
          add('  }');
          {Adjust the address for the data segment.  We want to adjust up to
           the same address within the page on the next page up.}
          add('  . = ALIGN (0x1000) - ((0x1000 - .) & (0x1000 - 1));');
          add('  .dynamic        : { *(.dynamic) }');
          add('  .got            : { *(.got) }');
          add('  .got.plt        : { *(.got.plt) }');
          add('  .data           :');
          add('  {');
          add('    *(.data .data.* .gnu.linkonce.d.*)');
          add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
          add('    KEEP (*(.gnu.linkonce.d.*personality*))');
          add('  }');
          add('  PROVIDE (_edata = .);');
          add('  PROVIDE (edata = .);');
        {$ifdef zsegment_threadvars}
          add('  _z = .;');
          add('  .threadvar 0 : AT (_z) { *(.threadvar .threadvar.* .gnu.linkonce.tv.*) }');
          add('  PROVIDE (_threadvar_size = SIZEOF(.threadvar));');
          add('  . = _z + SIZEOF (.threadvar);');
        {$else}
          add('  .threadvar : { *(.threadvar .threadvar.* .gnu.linkonce.tv.*) }');
        {$endif}
          add('  __bss_start = .;');
          add('  .bss            :');
          add('  {');
          add('   *(.dynbss)');
          add('   *(.bss .bss.* .gnu.linkonce.b.*)');
          add('   *(COMMON)');
          {Align here to ensure that the .bss section occupies space up to
           _end.  Align after .bss to ensure correct alignment even if the
           .bss section disappears because there are no input sections.}
          add('   . = ALIGN(32 / 8);');
          add('  }');
          add('  . = ALIGN(32 / 8);');
          add('  PROVIDE (_end = .);');
          add('  PROVIDE (end = .);');
          {Stabs debugging sections.}
          add('  .stab          0 : { *(.stab) }');
          add('  .stabstr       0 : { *(.stabstr) }');
          add('}');
        end;
{$endif LINKERSCRIPT_INCLUDED}
      { Write and Close response }
      writetodisk;
      Free;
    end;

  WriteResponseFile:=True;
end;


function TLinkerLinux.MakeExecutable:boolean;
var
  i : longint;
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
  DynLinkStr : string;
  GCSectionsStr,
  StaticStr,
  StripStr   : string[40];
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  GCSectionsStr:='';
  DynLinkStr:='';
  if (cs_link_staticflag in current_settings.globalswitches) then
   StaticStr:='-static';
  if (cs_link_strip in current_settings.globalswitches) and
     not(cs_link_separate_dbg_file in current_settings.globalswitches) then
   StripStr:='-s';
  if (cs_link_map in current_settings.globalswitches) then
   StripStr:='-Map '+maybequoted(ChangeFileExt(current_module.exefilename^,'.map'));
  if create_smartlink_sections then
   GCSectionsStr:='--gc-sections';
  If (cs_profile in current_settings.moduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   begin
     DynLinkStr:='--dynamic-linker='+Info.DynamicLinker;
     if cshared then
       DynLinkStr:=DynLinkStr+' --shared ';
     if rlinkpath<>'' then
       DynLinkStr:=DynLinkStr+' --rpath-link '+rlinkpath;
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

  { create dynamic symbol table? }
  if HasExports then
    cmdstr:=cmdstr+' -E';

  success:=DoExec(FindUtil(utilsprefix+BinStr),CmdStr,true,false);

  { Create external .dbg file with debuginfo }
  if success and (cs_link_separate_dbg_file in current_settings.globalswitches) then
    begin
      for i:=1 to 3 do
        begin
          SplitBinCmd(Info.ExtDbgCmd[i],binstr,cmdstr);
          Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
          Replace(cmdstr,'$DBGFN',maybequoted(extractfilename(current_module.dbgfilename^)));
          Replace(cmdstr,'$DBG',maybequoted(current_module.dbgfilename^));
          success:=DoExec(FindUtil(utilsprefix+BinStr),CmdStr,true,false);
          if not success then
            break;
        end;
    end;

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
   DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerLinux.MakeSharedLibrary:boolean;
var
  InitStr,
  FiniStr,
  SoNameStr : string[80];
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true);

 { Create some replacements }
 { note: linux does not use exportlib.initname/fininame due to the custom startup code }
  InitStr:='-init FPC_SHARED_LIB_START';
  FiniStr:='-fini FPC_SHARED_LIB_EXIT';
  SoNameStr:='-soname '+ExtractFileName(current_module.sharedlibfilename^);

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
  if success and (cs_link_strip in current_settings.globalswitches) then
   begin
     { only remove non global symbols and debugging info for a library }
     Info.DllCmd[2]:='strip --discard-all --strip-debug $EXE';
     SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
     Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename^));
     success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);
   end;

{ Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
   DeleteFile(outputexedir+Info.ResName);

  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
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
{$ifdef powerpc64}
  RegisterExternalLinker(system_powerpc64_linux_info,TLinkerLinux);
  RegisterImport(system_powerpc64_linux,timportliblinux);
  RegisterExport(system_powerpc64_linux,texportliblinux);
  RegisterTarget(system_powerpc64_linux_info);
{$endif powerpc64}
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
  RegisterRes(res_elf_info,TWinLikeResourceFile);
end.
