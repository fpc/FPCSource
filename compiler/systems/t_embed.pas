{
    This unit implements support import,export,link routines
    for the (arm) GameBoy Advance target

    Copyright (c) 2001-2002 by Peter Vreman

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
unit t_embed;

{$i fpcdefs.inc}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_embed,link,
       cpuinfo;

    type
       TlinkerEmbedded=class(texternallinker)
       private
          Function  WriteResponseFile: Boolean;
       public
          constructor Create; override;
          procedure SetDefaultInfo; override;
          function  MakeExecutable:boolean; override;
       end;



{*****************************************************************************
                                  TlinkerEmbedded
*****************************************************************************}

Constructor TlinkerEmbedded.Create;
begin
  Inherited Create;
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TlinkerEmbedded.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld -g $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE -T $RES';
   end;
end;


Function TlinkerEmbedded.WriteResponseFile: Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TCmdStrListItem;
  s,s1,s2  : TCmdStr;
  prtobj,
  cprtobj  : string[80];
  linklibc : boolean;
  found1,
  found2   : boolean;
{$ifdef AVR32}
  flashsize,ramsize: longword;
{$endif AVR32}
begin
  WriteResponseFile:=False;
  linklibc:=(SharedLibFiles.Find('c')<>nil);
{$if defined(ARM) or defined(i386) or defined(AVR) or defined(AVR32)}
  prtobj:='';
{$else}
  prtobj:='prt0';
{$endif}
  cprtobj:='cprt0';
  if linklibc then
    prtobj:=cprtobj;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

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
     LinkRes.Add('SEARCH_DIR('+(maybequoted(s))+')');
    HPath:=TCmdStrListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT (');
  { add objectfiles, start with prt0 always }
  //s:=FindObjectFile('prt0','',false);
  if prtobj<>'' then
    begin
      s:=FindObjectFile(prtobj,'',false);
      LinkRes.AddFileName(s);
    end;
  { try to add crti and crtbegin if linking to C }
  if linklibc then
   begin
     if librarysearchpath.FindFile('crtbegin.o',false,s) then
      LinkRes.AddFileName(s);
     if librarysearchpath.FindFile('crti.o',false,s) then
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
       end;
     end;
    { be sure that libc&libgcc is the last lib }
    if linklibc then
     begin
      LinkRes.Add('-lc');
      LinkRes.Add('-lgcc');
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
  if linklibc then
   begin
     found1:=librarysearchpath.FindFile('crtend.o',false,s1);
     found2:=librarysearchpath.FindFile('crtn.o',false,s2);
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

{$ifdef ARM}
  case current_settings.controllertype of
    ct_none:
      ;
    ct_lpc2114,
    ct_lpc2124,
    ct_lpc2194:
      with linkres do
        begin
          Add('ENTRY(_START)');
          Add('MEMORY');
          Add('{');
          Add('    flash : ORIGIN = 0, LENGTH = 256K');
          Add('    ram : ORIGIN = 0x40000000, LENGTH = 16K');
          Add('}');
          Add('_stack_top = 0x40003FFC;');
        end;
      ct_at91sam7s256,
      ct_at91sam7se256,
      ct_at91sam7x256,
      ct_at91sam7xc256:
      with linkres do
        begin
          Add('ENTRY(_START)');
          Add('MEMORY');
          Add('{');
          Add('    flash : ORIGIN = 0, LENGTH = 256K');
          Add('    ram : ORIGIN = 0x200000, LENGTH = 64K');
          Add('}');
          Add('_stack_top = 0x20FFFC;');
        end;
      ct_stm32f103re:
      with linkres do
        begin
          Add('ENTRY(_START)');
          Add('MEMORY');
          Add('{');
          Add('    flash : ORIGIN = 0x08000000, LENGTH = 512K');
          Add('    ram : ORIGIN = 0x20000000, LENGTH = 64K');
          Add('}');
          Add('_stack_top = 0x2000FFFC;');
        end;

    else
      internalerror(200902011);
  end;

  with linkres do
    begin
      Add('SECTIONS');
      Add('{');
      Add('     .text :');
      Add('    {');
      Add('    KEEP(*(.init, .init.*))');
      Add('    *(.text, .text.*)');
      Add('    *(.strings)');
      Add('    *(.rodata, .rodata.*)');
      Add('    *(.comment)');
      Add('    _etext = .;');
      Add('    } >flash');
      Add('    .data :');
      Add('    {');
      Add('    _data = .;');
      Add('    *(.data, .data.*)');
      Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      Add('    _edata = .;');
      Add('    } >ram AT >flash');
      Add('    .bss :');
      Add('    {');
      Add('    _bss_start = .;');
      Add('    *(.bss, .bss.*)');
      Add('    *(COMMON)');
      Add('    } >ram');
      Add('. = ALIGN(4);');
      Add('_bss_end = . ;');
      Add('}');
      Add('_end = .;');
    end;
{$endif ARM}

{$ifdef i386}
  with linkres do
    begin
      Add('ENTRY(_START)');
      Add('SECTIONS');
      Add('{');
      Add('     . = 0x100000;');
      Add('     .text ALIGN (0x1000) :');
      Add('    {');
      Add('    KEEP(*(.init, .init.*))');
      Add('    *(.text, .text.*)');
      Add('    *(.strings)');
      Add('    *(.rodata, .rodata.*)');
      Add('    *(.comment)');
      Add('    _etext = .;');
      Add('    }');
      Add('    .data ALIGN (0x1000) :');
      Add('    {');
      Add('    _data = .;');
      Add('    *(.data, .data.*)');
      Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      Add('    _edata = .;');
      Add('    }');
      Add('    . = ALIGN(4);');
      Add('    .bss :');
      Add('    {');
      Add('    _bss_start = .;');
      Add('    *(.bss, .bss.*)');
      Add('    *(COMMON)');
      Add('    }');
      Add('_bss_end = . ;');
      Add('}');
      Add('_end = .;');
    end;
{$endif i386}

{$ifdef AVR}
  with linkres do
    begin
      { linker script from ld 2.19 }
      Add('ENTRY(_START)');
      Add('OUTPUT_FORMAT("elf32-avr","elf32-avr","elf32-avr")');
      Add('OUTPUT_ARCH(avr:2)');
      Add('MEMORY');
      Add('{');
      Add('  text      (rx)   : ORIGIN = 0, LENGTH = 8K');
      Add('  data      (rw!x) : ORIGIN = 0x800060, LENGTH = 0xffa0');
      Add('  eeprom    (rw!x) : ORIGIN = 0x810000, LENGTH = 64K');
      Add('  fuse      (rw!x) : ORIGIN = 0x820000, LENGTH = 1K');
      Add('  lock      (rw!x) : ORIGIN = 0x830000, LENGTH = 1K');
      Add('  signature (rw!x) : ORIGIN = 0x840000, LENGTH = 1K');
      Add('}');
      Add('SECTIONS');
      Add('{');
      Add('  /* Read-only sections, merged into text segment: */');
      Add('  .hash          : { *(.hash)		}');
      Add('  .dynsym        : { *(.dynsym)		}');
      Add('  .dynstr        : { *(.dynstr)		}');
      Add('  .gnu.version   : { *(.gnu.version)	}');
      Add('  .gnu.version_d   : { *(.gnu.version_d)	}');
      Add('  .gnu.version_r   : { *(.gnu.version_r)	}');
      Add('  .rel.init      : { *(.rel.init)		}');
      Add('  .rela.init     : { *(.rela.init)	}');
      Add('  .rel.text      :');
      Add('    {');
      Add('      *(.rel.text)');
      Add('      *(.rel.text.*)');
      Add('      *(.rel.gnu.linkonce.t*)');
      Add('    }');
      Add('  .rela.text     :');
      Add('    {');
      Add('      *(.rela.text)');
      Add('      *(.rela.text.*)');
      Add('      *(.rela.gnu.linkonce.t*)');
      Add('    }');
      Add('  .rel.fini      : { *(.rel.fini)		}');
      Add('  .rela.fini     : { *(.rela.fini)	}');
      Add('  .rel.rodata    :');
      Add('    {');
      Add('      *(.rel.rodata)');
      Add('      *(.rel.rodata.*)');
      Add('      *(.rel.gnu.linkonce.r*)');
      Add('    }');
      Add('  .rela.rodata   :');
      Add('    {');
      Add('      *(.rela.rodata)');
      Add('      *(.rela.rodata.*)');
      Add('      *(.rela.gnu.linkonce.r*)');
      Add('    }');
      Add('  .rel.data      :');
      Add('    {');
      Add('      *(.rel.data)');
      Add('      *(.rel.data.*)');
      Add('      *(.rel.gnu.linkonce.d*)');
      Add('    }');
      Add('  .rela.data     :');
      Add('    {');
      Add('      *(.rela.data)');
      Add('      *(.rela.data.*)');
      Add('      *(.rela.gnu.linkonce.d*)');
      Add('    }');
      Add('  .rel.ctors     : { *(.rel.ctors)	}');
      Add('  .rela.ctors    : { *(.rela.ctors)	}');
      Add('  .rel.dtors     : { *(.rel.dtors)	}');
      Add('  .rela.dtors    : { *(.rela.dtors)	}');
      Add('  .rel.got       : { *(.rel.got)		}');
      Add('  .rela.got      : { *(.rela.got)		}');
      Add('  .rel.bss       : { *(.rel.bss)		}');
      Add('  .rela.bss      : { *(.rela.bss)		}');
      Add('  .rel.plt       : { *(.rel.plt)		}');
      Add('  .rela.plt      : { *(.rela.plt)		}');
      Add('  /* Internal text space or external memory.  */');
      Add('  .text   :');
      Add('  {');
      Add('    *(.vectors)');
      Add('    KEEP(*(.vectors))');
      Add('    /* For data that needs to reside in the lower 64k of progmem.  */');
      Add('    *(.progmem.gcc*)');
      Add('    *(.progmem*)');
      Add('    . = ALIGN(2);');
      Add('     __trampolines_start = . ;');
      Add('    /* The jump trampolines for the 16-bit limited relocs will reside here.  */');
      Add('    *(.trampolines)');
      Add('    *(.trampolines*)');
      Add('     __trampolines_end = . ;');
      Add('    /* For future tablejump instruction arrays for 3 byte pc devices.');
      Add('       We don''t relax jump/call instructions within these sections.  */');
      Add('    *(.jumptables)');
      Add('    *(.jumptables*)');
      Add('    /* For code that needs to reside in the lower 128k progmem.  */');
      Add('    *(.lowtext)');
      Add('    *(.lowtext*)');
      Add('     __ctors_start = . ;');
      Add('     *(.ctors)');
      Add('     __ctors_end = . ;');
      Add('     __dtors_start = . ;');
      Add('     *(.dtors)');
      Add('     __dtors_end = . ;');
      Add('    KEEP(SORT(*)(.ctors))');
      Add('    KEEP(SORT(*)(.dtors))');
      Add('    /* From this point on, we don''t bother about wether the insns are');
      Add('       below or above the 16 bits boundary.  */');
      Add('    *(.init0)  /* Start here after reset.  */');
      Add('    KEEP (*(.init0))');
      Add('    *(.init1)');
      Add('    KEEP (*(.init1))');
      Add('    *(.init2)  /* Clear __zero_reg__, set up stack pointer.  */');
      Add('    KEEP (*(.init2))');
      Add('    *(.init3)');
      Add('    KEEP (*(.init3))');
      Add('    *(.init4)  /* Initialize data and BSS.  */');
      Add('    KEEP (*(.init4))');
      Add('    *(.init5)');
      Add('    KEEP (*(.init5))');
      Add('    *(.init6)  /* C++ constructors.  */');
      Add('    KEEP (*(.init6))');
      Add('    *(.init7)');
      Add('    KEEP (*(.init7))');
      Add('    *(.init8)');
      Add('    KEEP (*(.init8))');
      Add('    *(.init9)  /* Call main().  */');
      Add('    KEEP (*(.init9))');
      Add('    *(.text)');
      Add('    . = ALIGN(2);');
      Add('    *(.text.*)');
      Add('    . = ALIGN(2);');
      Add('    *(.fini9)  /* _exit() starts here.  */');
      Add('    KEEP (*(.fini9))');
      Add('    *(.fini8)');
      Add('    KEEP (*(.fini8))');
      Add('    *(.fini7)');
      Add('    KEEP (*(.fini7))');
      Add('    *(.fini6)  /* C++ destructors.  */');
      Add('    KEEP (*(.fini6))');
      Add('    *(.fini5)');
      Add('    KEEP (*(.fini5))');
      Add('    *(.fini4)');
      Add('    KEEP (*(.fini4))');
      Add('    *(.fini3)');
      Add('    KEEP (*(.fini3))');
      Add('    *(.fini2)');
      Add('    KEEP (*(.fini2))');
      Add('    *(.fini1)');
      Add('    KEEP (*(.fini1))');
      Add('    *(.fini0)  /* Infinite loop after program termination.  */');
      Add('    KEEP (*(.fini0))');
      Add('     _etext = . ;');
      Add('  }  > text');
      Add('  .data	  : AT (ADDR (.text) + SIZEOF (.text))');
      Add('  {');
      Add('     PROVIDE (__data_start = .) ;');
      Add('    *(.data)');
      Add('    *(.data*)');
      Add('    *(.rodata)  /* We need to include .rodata here if gcc is used */');
      Add('    *(.rodata*) /* with -fdata-sections.  */');
      Add('    *(.gnu.linkonce.d*)');
      Add('    . = ALIGN(2);');
      Add('     _edata = . ;');
      Add('     PROVIDE (__data_end = .) ;');
      Add('  }  > data');
      Add('  .bss   : AT (ADDR (.bss))');
      Add('  {');
      Add('     PROVIDE (__bss_start = .) ;');
      Add('    *(.bss)');
      Add('    *(.bss*)');
      Add('    *(COMMON)');
      Add('     PROVIDE (__bss_end = .) ;');
      Add('  }  > data');
      Add('   __data_load_start = LOADADDR(.data);');
      Add('   __data_load_end = __data_load_start + SIZEOF(.data);');
      Add('  /* Global data not cleared after reset.  */');
      Add('  .noinit  :');
      Add('  {');
      Add('     PROVIDE (__noinit_start = .) ;');
      Add('    *(.noinit*)');
      Add('     PROVIDE (__noinit_end = .) ;');
      Add('     _end = . ;');
      Add('     PROVIDE (__heap_start = .) ;');
      Add('  }  > data');
      Add('  .eeprom  :');
      Add('  {');
      Add('    *(.eeprom*)');
      Add('     __eeprom_end = . ;');
      Add('  }  > eeprom');
      Add('  .fuse  :');
      Add('  {');
      Add('    KEEP(*(.fuse))');
      Add('    KEEP(*(.lfuse))');
      Add('    KEEP(*(.hfuse))');
      Add('    KEEP(*(.efuse))');
      Add('  }  > fuse');
      Add('  .lock  :');
      Add('  {');
      Add('    KEEP(*(.lock*))');
      Add('  }  > lock');
      Add('  .signature  :');
      Add('  {');
      Add('    KEEP(*(.signature*))');
      Add('  }  > signature');
      Add('  /* Stabs debugging sections.  */');
      Add('  .stab 0 : { *(.stab) }');
      Add('  .stabstr 0 : { *(.stabstr) }');
      Add('  .stab.excl 0 : { *(.stab.excl) }');
      Add('  .stab.exclstr 0 : { *(.stab.exclstr) }');
      Add('  .stab.index 0 : { *(.stab.index) }');
      Add('  .stab.indexstr 0 : { *(.stab.indexstr) }');
      Add('  .comment 0 : { *(.comment) }');
      Add('  /* DWARF debug sections.');
      Add('     Symbols in the DWARF debugging sections are relative to the beginning');
      Add('     of the section so we begin them at 0.  */');
      Add('  /* DWARF 1 */');
      Add('  .debug          0 : { *(.debug) }');
      Add('  .line           0 : { *(.line) }');
      Add('  /* GNU DWARF 1 extensions */');
      Add('  .debug_srcinfo  0 : { *(.debug_srcinfo) }');
      Add('  .debug_sfnames  0 : { *(.debug_sfnames) }');
      Add('  /* DWARF 1.1 and DWARF 2 */');
      Add('  .debug_aranges  0 : { *(.debug_aranges) }');
      Add('  .debug_pubnames 0 : { *(.debug_pubnames) }');
      Add('  /* DWARF 2 */');
      Add('  .debug_info     0 : { *(.debug_info) *(.gnu.linkonce.wi.*) }');
      Add('  .debug_abbrev   0 : { *(.debug_abbrev) }');
      Add('  .debug_line     0 : { *(.debug_line) }');
      Add('  .debug_frame    0 : { *(.debug_frame) }');
      Add('  .debug_str      0 : { *(.debug_str) }');
      Add('  .debug_loc      0 : { *(.debug_loc) }');
      Add('  .debug_macinfo  0 : { *(.debug_macinfo) }');
      Add('}');
    end;
{$endif AVR}

{$ifdef AVR32}
  case current_settings.controllertype of
    ct_none:
      ;
    ct_at32uc3l016:
      begin
        flashsize:=16;
        ramsize:=8;
      end;
    ct_at32uc3l032:
      begin
        flashsize:=32;
        ramsize:=16;
      end;
    ct_at32uc3l064:
      begin
        flashsize:=64;
        ramsize:=16;
      end;
    ct_at32uc3b1256:
      begin
        flashsize:=256;
        ramsize:=32;
      end;
    else
      internalerror(200902011);
  end;


  if current_settings.controllertype<>ct_none then
    begin
      with linkres do
        begin
          Add('ENTRY(_START)');
          Add('MEMORY');
          Add('{');
          Add(Format('    flash : ORIGIN = 0x80000000, LENGTH = %DK',[flashsize]));
          Add(Format('    ram : ORIGIN = 0, LENGTH = %DK',[ramsize]));
          Add('}');
          Add(format('_stack_top = 0x%X;',[ramsize*1024-4]));
        end;
    end;

  with linkres do
    begin
      Add('SECTIONS');
      Add('{');
      Add('  .rel.init      : { *(.rel.init)		}');
      Add('  .rela.init     : { *(.rela.init)	}');
      Add('  .rel.text      :');
      Add('    {');
      Add('      *(.rel.text)');
      Add('      *(.rel.text.*)');
      Add('      *(.rel.gnu.linkonce.t*)');
      Add('    }');
      Add('  .rela.text     :');
      Add('    {');
      Add('      *(.rela.text)');
      Add('      *(.rela.text.*)');
      Add('      *(.rela.gnu.linkonce.t*)');
      Add('    }');
      Add('  .rel.fini      : { *(.rel.fini)		}');
      Add('  .rela.fini     : { *(.rela.fini)	}');
      Add('  .rel.rodata    :');
      Add('    {');
      Add('      *(.rel.rodata)');
      Add('      *(.rel.rodata.*)');
      Add('      *(.rel.gnu.linkonce.r*)');
      Add('    }');
      Add('  .rela.rodata   :');
      Add('    {');
      Add('      *(.rela.rodata)');
      Add('      *(.rela.rodata.*)');
      Add('      *(.rela.gnu.linkonce.r*)');
      Add('    }');
      Add('  .rel.data      :');
      Add('    {');
      Add('      *(.rel.data)');
      Add('      *(.rel.data.*)');
      Add('      *(.rel.gnu.linkonce.d*)');
      Add('    }');
      Add('  .rela.data     :');
      Add('    {');
      Add('      *(.rela.data)');
      Add('      *(.rela.data.*)');
      Add('      *(.rela.gnu.linkonce.d*)');
      Add('    }');
      Add('  .rel.ctors     : { *(.rel.ctors)	}');
      Add('  .rela.ctors    : { *(.rela.ctors)	}');
      Add('  .rel.dtors     : { *(.rel.dtors)	}');
      Add('  .rela.dtors    : { *(.rela.dtors)	}');
      Add('  .rel.got       : { *(.rel.got)		}');
      Add('  .rela.got      : { *(.rela.got)		} >flash');
      Add('  .rel.bss       : { *(.rel.bss)		}');
      Add('  .rela.bss      : { *(.rela.bss)		}');
      Add('  .rel.plt       : { *(.rel.plt)		}');
      Add('  .rela.plt      : { *(.rela.plt)		}');
      Add('     .text :');
      Add('    {');
      Add('    KEEP(*(.init))');
      Add('    KEEP(*(.init.*))');
      Add('    *(.text)');
      Add('    *(.text.*)');
      Add('    *(.strings)');
      Add('    *(.rodata)');
      Add('    *(.rodata.*)');
      Add('    *(.comment)');
      Add('    . = ALIGN(4);');
      Add('    _etext = .;');
      Add('    } >flash =0xd703d703');
      Add('    .data : ALIGN(4) ');
      Add('    {');
      Add('    _data = .;');
      Add('    *(.data)');
      Add('    *(.data.*)');
      Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      Add('    . = ALIGN(4);');
      Add('    _edata = .;');
      Add('    } >ram AT >flash');
      Add('    .bss : ');
      Add('    {');
      Add('    _bss_start = .;');
      Add('    *(.bss)');
      Add('    *(.bss.*)');
      Add('    *(COMMON)');
      Add('    } >ram');
      Add('. = ALIGN(4);');
      Add('_bss_end = . ;');
      Add('}');
      Add('_end = .;');
    end;
{$endif AVR32}

  { Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;

end;


function TlinkerEmbedded.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
  StaticStr,
  GCSectionsStr,
  DynLinkStr,
  StripStr: string;
begin
  { for future use }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';

  GCSectionsStr:='--gc-sections';
  //if not(cs_link_extern in current_settings.globalswitches) then
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Write used files and libraries }
  WriteResponseFile();

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  if not(cs_link_on_target in current_settings.globalswitches) then
   begin
    Replace(cmdstr,'$EXE',(maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename^,'.elf')))));
    Replace(cmdstr,'$RES',(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
    Replace(cmdstr,'$STATIC',StaticStr);
    Replace(cmdstr,'$STRIP',StripStr);
    Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
    Replace(cmdstr,'$DYNLINK',DynLinkStr);
   end
  else
   begin
    Replace(cmdstr,'$EXE',maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename^,'.elf'))));
    Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
    Replace(cmdstr,'$STATIC',StaticStr);
    Replace(cmdstr,'$STRIP',StripStr);
    Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
    Replace(cmdstr,'$DYNLINK',DynLinkStr);
   end;
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if success and not(cs_link_nolink in current_settings.globalswitches) then
   DeleteFile(outputexedir+Info.ResName);

{ Post process }
  if success and (target_info.system=system_arm_embedded) then
    begin
      success:=DoExec(FindUtil(utilsprefix+'objcopy'),'-O ihex '+
        ChangeFileExt(current_module.exefilename^,'.elf')+' '+
        ChangeFileExt(current_module.exefilename^,'.hex'),true,false);
    end;

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef arm}
  RegisterExternalLinker(system_arm_embedded_info,TlinkerEmbedded);
  RegisterTarget(system_arm_embedded_info);
{$endif arm}

{$ifdef avr}
  RegisterExternalLinker(system_avr_embedded_info,TlinkerEmbedded);
  RegisterTarget(system_avr_embedded_info);
{$endif avr}

{$ifdef i386}
  RegisterExternalLinker(system_i386_embedded_info,TlinkerEmbedded);
  RegisterTarget(system_i386_embedded_info);
{$endif i386}

{$ifdef avr32}
  RegisterExternalLinker(system_avr32_embedded_info,TlinkerEmbedded);
  RegisterTarget(system_avr32_embedded_info);
{$endif avr32}
end.
