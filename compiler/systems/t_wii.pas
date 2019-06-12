{
    Copyright (c) 2011 by Francesco Lombardi

    This unit implements support import, export, link routines
    for the Wii (PowerPC) target

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
unit t_wii;

{$i fpcdefs.inc}

interface


implementation

    uses
       aasmbase,
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,cscript,fmodule,i_wii,link;

    type
       TlinkerWii=class(texternallinker)
       private
          Function  WriteResponseFile: Boolean;
       public
          constructor Create; override;
          procedure SetDefaultInfo; override;
          function  MakeExecutable:boolean; override;
       end;



{****************************************************************************
                               TLinkerWii
****************************************************************************}

Constructor TLinkerWii.Create;
begin
  Inherited Create;
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerWii.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld -g $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE -T $RES';
   end;
end;


Function TLinkerWii.WriteResponseFile : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TCmdStrListItem;
  s,s1,s2  : TCmdStr;
  linklibc,
  linklibgcc  : boolean;
  found1,
  found2   : boolean;    
begin
  WriteResponseFile:=False;
  linklibc:=(SharedLibFiles.Find('c')<>nil);
  linklibgcc:=(SharedLibFiles.Find('gcc')<>nil);
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);

  { Write path to search libraries }
  HPath:=TCmdStrListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
    s:=HPath.Str;
    if (cs_link_on_target in current_settings.globalswitches) then
     s:=ScriptFixFileName(s);
    LinkRes.Add('-L'+s);
    HPath:=TCmdStrListItem(HPath.Next);
   end;
  HPath:=TCmdStrListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
    s:=HPath.Str;
    if s<>'' then
     LinkRes.Add('SEARCH_DIR("'+s+'")');
    HPath:=TCmdStrListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT (');
  { add objectfiles, start with prt0 always }
//  s:=FindObjectFile('prt0','',false);
//  LinkRes.AddFileName(s);
  { try to add crti and crtbegin if linking to C }
  if linklibc then
   begin
     if librarysearchpath.FindFile('ecrti.o',false,s) then
      LinkRes.AddFileName(s);
   end;
  if linklibgcc then
   begin
     if librarysearchpath.FindFile('crtbegin.o',false,s) then
      LinkRes.AddFileName(s);
   end;
  if linklibc or linklibgcc then
   begin
     if librarysearchpath.FindFile('crtmain.o',false,s) then
      LinkRes.AddFileName(s);
   end;
   
   
  while not ObjectFiles.Empty do
   begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
     begin
      { vlink doesn't use SEARCH_DIR for object files }
      if not(cs_link_on_target in current_settings.globalswitches) then
       s:=FindObjectFile(s,'',false);
      LinkRes.AddFileName((maybequoted(s)));
     end;
   end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
    { vlink doesn't need, and doesn't support GROUP }
    if (cs_link_on_target in current_settings.globalswitches) then
     begin
      LinkRes.Add(')');
      LinkRes.Add('GROUP(');
     end;
    while not StaticLibFiles.Empty do
     begin
      S:=StaticLibFiles.GetFirst;
      LinkRes.AddFileName((maybequoted(s)));
     end;
   end;

  if (cs_link_on_target in current_settings.globalswitches) then
   begin
    LinkRes.Add(')');

    { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
      here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
    linklibc:=false;
    linklibgcc:=false;
    while not SharedLibFiles.Empty do
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
        LinkRes.Add('-l'+s);
        linklibc:=true;
        linklibgcc:=true;
       end;
     end;
    { be sure that libc&libgcc is the last lib }
    if linklibgcc then
     begin
      LinkRes.Add('-lgcc');
     end;
    if linklibc then
     begin
      LinkRes.Add('-lc');
     end;
     
   end
  else
   begin
    while not SharedLibFiles.Empty do
     begin
      S:=SharedLibFiles.GetFirst;
      LinkRes.Add('lib'+s+target_info.staticlibext);
     end;
    LinkRes.Add(')');
   end;

  { objects which must be at the end }
  if linklibgcc then
   begin
     found1:=librarysearchpath.FindFile('crtend.o',false,s1);
     if found1 then
      begin
        LinkRes.Add('INPUT(');
        if found1 then
         LinkRes.AddFileName(s1);
        LinkRes.Add(')');
      end;
   end;   
  if linklibc then
   begin
     found2:=librarysearchpath.FindFile('ecrtn.o',false,s2);
     if found2 then
      begin
        LinkRes.Add('INPUT(');
        if found2 then
         LinkRes.AddFileName(s2);
        LinkRes.Add(')');
      end;
   end;   
  with linkres do
   begin
    add('/*');
    add(' * Linkscript for Wii');
    add(' */');
    add('');
    add('OUTPUT_FORMAT("elf32-powerpc", "elf32-powerpc", "elf32-powerpc");');
    add('OUTPUT_ARCH(powerpc:common);');
    add('EXTERN(_start);');
    add('ENTRY(_start);');
    add('');
    add('PHDRS');
    add('{');
    add('  stub PT_LOAD FLAGS(5);');
    add('  text PT_LOAD FLAGS(5);');
    add('  data PT_LOAD FLAGS(6);');
    add('  bss1 PT_LOAD;');
    add('  bss2 PT_LOAD;');
    add('');
    add('}');
    add('');
    add('SECTIONS');
    add('{');
    add('	/* stub is loaded at physical address 0x00003400 (though both 0x80003400 and 0x00003400 are equivalent for IOS) */');
    add('	/* This can also be used to load an arbitrary standalone stub at an arbitrary address in memory, for any purpose */');
    add('	/* Use -Wl,--section-start,.stub=0xADDRESS to change */');
    add('	. = 0x00003400;');
    add('');
    add('	.stub :');
    add('	{');
    add('		KEEP(*(.stub))');
    add('	} :stub = 0');
    add('');
    add('	/* default base address */');
    add('	/* use -Wl,--section-start,.init=0xADDRESS to change */');
    add('	. = 0x80004000;');
    add('');
    add('	/* Program */');
    add('	.init          :');
    add('	{');
    add('		KEEP (*crt0.o(*.init))');
    add('		KEEP (*(.init))');
    add('	} :text = 0');
    add('	.plt      : { *(.plt)	}');
    add('	.interp			: { *(.interp) 	}');
    add('	.hash			: { *(.hash) }');
    add('	.dynsym			: { *(.dynsym) }');
    add('	.dynstr			: { *(.dynstr) }');
    add('	.gnu.version	: { *(.gnu.version) }');
    add('	.gnu.version_d	: { *(.gnu.version_d) }');
    add('	.gnu.version_r	: { *(.gnu.version_r) }');
    add('	.rel.init		: { *(.rel.init) }');
    add('	.rela.init		: { *(.rela.init) }');
    add('	.rel.text		: { *(.rel.text .rel.text.* .rel.gnu.linkonce.t.*) }');
    add('	.rela.text		: { *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*) }');
    add('	.rel.fini		: { *(.rel.fini) }');
    add('	.rela.fini		: { *(.rela.fini) }');
    add('	.rel.rodata		: { *(.rel.rodata .rel.rodata.* .rel.gnu.linkonce.r.*) }');
    add('	.rela.rodata	: { *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*) }');
    add('	.rel.data		: { *(.rel.data .rel.data.* .rel.gnu.linkonce.d.*) }');
    add('	.rela.data		: { *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*) }');
    add('	.rel.tdata		: { *(.rel.tdata .rel.tdata.* .rel.gnu.linkonce.td.*) }');
    add('	.rela.tdata		: { *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*) }');
    add('	.rel.tbss		: { *(.rel.tbss .rel.tbss.* .rel.gnu.linkonce.tb.*) }');
    add('	.rela.tbss		: { *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*) }');
    add('	.rel.ctors		: { *(.rel.ctors) }');
    add('	.rela.ctors		: { *(.rela.ctors) }');
    add('	.rel.dtors		: { *(.rel.dtors) }');
    add('	.rela.dtors		: { *(.rela.dtors) }');
    add('	.rel.got		: { *(.rel.got)	}');
    add('	.rela.got		: { *(.rela.got) }');
    add('	.rela.got1		: { *(.rela.got1) }');
    add('	.rela.got2		: { *(.rela.got2) }');
    add('	.rel.sdata		: { *(.rel.sdata .rel.sdata.* .rel.gnu.linkonce.s.*) }');
    add('	.rela.sdata		: { *(.rela.sdata .rela.sdata.* .rela.gnu.linkonce.s.*) }');
    add('	.rel.sbss		: { *(.rel.sbss .rel.sbss.* .rel.gnu.linkonce.sb.*) }');
    add('	.rela.sbss		: { *(.rela.sbss .rela.sbss.* .rel.gnu.linkonce.sb.*) }');
    add('	.rel.sdata2		: { *(.rel.sdata2 .rel.sdata2.* .rel.gnu.linkonce.s2.*) }');
    add('	.rela.sdata2	: { *(.rela.sdata2 .rela.sdata2.* .rela.gnu.linkonce.s2.*) }');
    add('	.rel.sbss2		: { *(.rel.sbss2 .rel.sbss2.* .rel.gnu.linkonce.sb2.*) }');
    add('	.rela.sbss2		: { *(.rela.sbss2 .rela.sbss2.* .rela.gnu.linkonce.sb2.*) }');
    add('	.rel.bss		: { *(.rel.bss .rel.bss.* .rel.gnu.linkonce.b.*) }');
    add('	.rela.bss		: { *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*) }');
    add('	.rel.plt		: { *(.rel.plt) }');
    add('	.rela.plt		: { *(.rela.plt) }');
    add('');
    add('	.text      :');
    add('	{');
    add('		*(.text)');
    add('		*(.text.*)');
    add('		/* .gnu.warning sections are handled specially by elf32.em.  */');
    add('		*(.gnu.warning)');
    add('		*(.gnu.linkonce.t.*)');
    add('		. = ALIGN(32);   /* REQUIRED. LD is flaky without it. */');
    add('	} = 0');
    add('');
    add('	.fini      :');
    add('	{');
    add('		KEEP (*(.fini))');
    add('		. = ALIGN(32);   /* REQUIRED. LD is flaky without it. */');
    add('	} = 0');
    add('	');
    add('	PROVIDE (__etext = .);');
    add('	PROVIDE (_etext = .);');
    add('	PROVIDE (etext = .);');
    add('');
    add('	.rodata   : { *(.rodata) *(.rodata.*) *(.gnu.linkonce.r.*) } :data');
    add('	.rodata1   : { *(.rodata1) }');
    add('	.sdata2   : {');
    add('		PROVIDE(_SDA2_BASE_ = .);');
    add('		*(.sdata2)');
    add('		*(.sdata2.*)');
    add('		*(.gnu.linkonce.s2.*)');
    add('	}');
    add('	.sbss2   : { *(.sbss2) *(.sbss2.*) *(.gnu.linkonce.sb2.*) }');
    add('  /* Adjust the address for the data segment.  We want to adjust up to');
    add('     the same address within the page on the next page up.  */');
    add('  /* Ensure the __preinit_array_start label is properly aligned.  We');
    add('     could instead move the label definition inside the section, but');
    add('     the linker would then create the section even if it turns out to');
    add('     be empty, which isn''t pretty.  */');
    add('	. = ALIGN(32 / 8);');
    add('	PROVIDE (__preinit_array_start = .);');
    add('	.preinit_array     : { *(.preinit_array) }');
    add('	PROVIDE (__preinit_array_end = .);');
    add('	PROVIDE (__init_array_start = .);');
    add('	.init_array     : { *(.init_array) }');
    add('	PROVIDE (__init_array_end = .);');
    add('	PROVIDE (__fini_array_start = .);');
    add('	.fini_array     : { *(.fini_array) }');
    add('	PROVIDE (__fini_array_end = .);');
    add('	.data    :');
    add('	{');
    add('		*(.data)');
    add('		*(.data.*)');
    add('		*(.gnu.linkonce.d.*)');
    add('		SORT(CONSTRUCTORS)');
    add('		. = ALIGN(32);   /* REQUIRED. LD is flaky without it. */');
    add('	}');
    add('');
    add('	.data1   : { *(.data1) }');
    add('	.tdata	  : { *(.tdata .tdata.* .gnu.linkonce.td.*) }');
    add('	.tbss		  : { *(.tbss .tbss.* .gnu.linkonce.tb.*) *(.tcommon) }');
    add('	.eh_frame : { KEEP (*(.eh_frame)) }');
    add('	.gcc_except_table : { *(.gcc_except_table) }');
    add('	.fixup          : { *(.fixup) }');
    add('	.got1           : { *(.got1) }');
    add('	.got2           : { *(.got2) }');
    add('	.dynamic       : { *(.dynamic) }');
    add('');
    add('	.ctors   :');
    add('	{');
    add('	/*	gcc uses crtbegin.o to find the start of');
    add('		the constructors, so we make sure it is');
    add('		first.  Because this is a wildcard, it');
    add('		doesn''t matter if the user does not');
    add('		actually link against crtbegin.o; the');
    add('		linker won''t look for a file to match a');
    add('		wildcard.  The wildcard also means that it');
    add('		doesn''t matter which directory crtbegin.o');
    add('		is in.  */');
    add('');
    add('		KEEP (*crtbegin.o(.ctors))');
    add('');
    add('    /*	We don''t want to include the .ctor section from');
    add('		from the crtend.o file until after the sorted ctors.');
    add('		The .ctor section from the crtend file contains the');
    add('		end of ctors marker and it must be last */');
    add('');
    add('		KEEP (*(EXCLUDE_FILE (*crtend.o ) .ctors))');
    add('		KEEP (*(SORT(.ctors.*)))');
    add('		KEEP (*(.ctors))');
    add('		. = ALIGN(32);   /* REQUIRED. LD is flaky without it. */');
    add('	}');
    add('');
    add('	.dtors         :');
    add('	{');
    add('		KEEP (*crtbegin.o(.dtors))');
    add('		KEEP (*(EXCLUDE_FILE (*crtend.o ) .dtors))');
    add('		KEEP (*(SORT(.dtors.*)))');
    add('		KEEP (*(.dtors))');
    add('		. = ALIGN(32);   /* REQUIRED. LD is flaky without it. */');
    add('	}');
    add('');
    add('	.jcr            : { KEEP (*(.jcr)) }');
    add('	.got		  : { *(.got.plt) *(.got) }');
    add('');
    add('');
    add('	/*	We want the small data sections together, so single-instruction offsets');
    add('		can access them all, and initialized data all before uninitialized, so');
    add('		we can shorten the on-disk segment size.  */');
    add('');
    add('	.sdata     :');
    add('	{');
    add('		PROVIDE(_SDA_BASE_ = .);');
    add('		*(.sdata)');
    add('		*(.sdata.*)');
    add('		*(.gnu.linkonce.s.*)');
    add('		. = ALIGN(32);   /* REQUIRED. LD is flaky without it. */');
    add('	}');
    add('');
    add('	_edata = .;');
    add('	PROVIDE (edata = .);');
    add('	');
    add('	.sbss      :');
    add('	{');
    add('		__sbss_start = .;');
    add('		PROVIDE (__sbss_start = .);');
    add('		PROVIDE (___sbss_start = .);');
    add('		*(.dynsbss)');
    add('		*(.sbss)');
    add('		*(.sbss.*)');
    add('		*(.gnu.linkonce.sb.*)');
    add('		*(.scommon)');
    add('		PROVIDE (__sbss_end = .);');
    add('		PROVIDE (___sbss_end = .);');
    add('		. = ALIGN(32);   /* REQUIRED. LD is flaky without it. */');
    add('		__sbss_end = .;');
    add('	} :bss1');
    add('');
    add('	.bss       :');
    add('	{');
    add('		__bss_start = .;');
    add('		PROVIDE (__bss_start = .);');
    add('		*(.dynbss)');
    add('		*(.bss)');
    add('		*(.bss.*)');
    add('		*(.gnu.linkonce.b.*)');
    add('		*(COMMON)');
    add('	/*	Align here to ensure that the .bss section occupies space up to');
    add('		_end.  Align after .bss to ensure correct alignment even if the');
    add('		.bss section disappears because there are no input sections.  */');
    add('');
    add('		. = ALIGN(32);');
    add('');
    add('		PROVIDE (__bss_end = .);');
    add('		__bss_end = .;');
    add('	} :bss2');
    add('');
    add('	_end = .;');
    add('	PROVIDE(end = .);');
    add('	/* Stabs debugging sections.  */');
    add('	.stab 0 : { *(.stab) }');
    add('	.stabstr 0 : { *(.stabstr) }');
    add('	.stab.excl 0 : { *(.stab.excl) }');
    add('	.stab.exclstr 0 : { *(.stab.exclstr) }');
    add('	.stab.index 0 : { *(.stab.index) }');
    add('	.stab.indexstr 0 : { *(.stab.indexstr) }');
    add('	.comment 0 : { *(.comment) }');
    add('	/*	DWARF debug sections.');
    add('		Symbols in the DWARF debugging sections are relative to the beginning');
    add('		of the section so we begin them at 0.  */');
    add('	/* DWARF 1 */');
    add('	.debug          0 : { *(.debug) }');
    add('	.line           0 : { *(.line) }');
    add('	/* GNU DWARF 1 extensions */');
    add('	.debug_srcinfo  0 : { *(.debug_srcinfo) }');
    add('	.debug_sfnames  0 : { *(.debug_sfnames) }');
    add('	/* DWARF 1.1 and DWARF 2 */');
    add('	.debug_aranges  0 : { *(.debug_aranges) }');
    add('	.debug_pubnames 0 : { *(.debug_pubnames) }');
    add('	/* DWARF 2 */');
    add('	.debug_info     0 : { *(.debug_info) }');
    add('	.debug_abbrev   0 : { *(.debug_abbrev) }');
    add('	.debug_line     0 : { *(.debug_line) }');
    add('	.debug_frame    0 : { *(.debug_frame) }');
    add('	.debug_str      0 : { *(.debug_str) }');
    add('	.debug_loc      0 : { *(.debug_loc) }');
    add('	.debug_macinfo  0 : { *(.debug_macinfo) }');
    add('	/* SGI/MIPS DWARF 2 extensions */');
    add('	.debug_weaknames 0 : { *(.debug_weaknames) }');
    add('	.debug_funcnames 0 : { *(.debug_funcnames) }');
    add('	.debug_typenames 0 : { *(.debug_typenames) }');
    add('	.debug_varnames  0 : { *(.debug_varnames) }');
    add('	/* These must appear regardless of  .  */');
    add('}');
    add('');
    add('__isIPL = 0;');
    add('__stack_addr = (__bss_start + SIZEOF(.bss) + 0x20000 + 7) & (-8);');
    add('__stack_end = (__bss_start + SIZEOF(.bss));');
    add('__intrstack_addr = (__stack_addr + 0x4000);');
    add('__intrstack_end = (__stack_addr);');
    add('__Arena1Lo = (__intrstack_addr + 31) & (-32);');
    add('__Arena1Hi = (0x817FEFF0);');
    add('__Arena2Lo = (0x90002000);');
    add('__Arena2Hi = (0x933E0000);');
    add('');
    add('__gxregs = (__Arena1Hi + 31) & (-32);');
    add('__ipcbufferLo = (0x933e0000);');
    add('__ipcbufferHi = (0x93400000);');
    add('');
    add('/* for backward compatibility with old crt0 */');
    add('PROVIDE (__stack = (0x817FEFF0));');
    add('');
    add('PROVIDE(__isIPL = __isIPL);');
    add('PROVIDE(__stack_addr = __stack_addr);');
    add('PROVIDE(__stack_end = __stack_end);');
    add('PROVIDE(__intrstack_addr = __intrstack_addr);');
    add('PROVIDE(__intrstack_end = __intrstack_end);');
    add('PROVIDE(__Arena1Lo = __Arena1Lo);');
    add('PROVIDE(__Arena1Hi = __Arena1Hi);');
    add('PROVIDE(__Arena2Lo = __Arena2Lo);');
    add('PROVIDE(__Arena2Hi = __Arena2Hi);');
    add('PROVIDE(__ipcbufferLo = __ipcbufferLo);');
    add('PROVIDE(__ipcbufferHi = __ipcbufferHi);');
    add('PROVIDE(__gxregs = __gxregs);');
   end;
   
   
{ Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;

end;


function TLinkerWii.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
  StaticStr,
  GCSectionsStr,
  DynLinkStr,
  StripStr: string;
begin
  StaticStr:='';
  StripStr:='';
  GCSectionsStr:='';
  DynLinkStr:='';

  if (cs_link_strip in current_settings.globalswitches) and
     not(cs_link_separate_dbg_file in current_settings.globalswitches) then
   StripStr:='-s';
  if (cs_link_map in current_settings.globalswitches) then
   StripStr:='-Map '+maybequoted(ChangeFileExt(current_module.exefilename,'.map'));
  if create_smartlink_sections then
   GCSectionsStr:='--gc-sections';
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename);


{ Write used files and libraries }
  WriteResponseFile();

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',(maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename,'.elf')))));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);

  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
   DeleteFile(outputexedir+Info.ResName);
   
{ Post process }

  if success then 
   begin
    success:=DoExec(FindUtil('elf2dol'),ChangeFileExt(current_module.exefilename,'.elf')+' '+ 
     current_module.exefilename,true,false);
   end;
  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterLinker(ld_wii,TLinkerWii);
  RegisterTarget(system_powerpc_wii_info);
end.
