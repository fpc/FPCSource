{
    Copyright (c) 2005-2017 by Free Pascal Compiler team

    This unit implements support import, export, link routines
    for the FreeRTOS Target

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
unit t_freertos;

{$i fpcdefs.inc}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,comphook,cscript,fmodule,i_freertos,link,
       cpuinfo;

    type
       TlinkerFreeRTOS=class(texternallinker)
       private
          Function  WriteResponseFile: Boolean;
       public
          constructor Create; override;
          procedure SetDefaultInfo; override;
          function  MakeExecutable:boolean; override;
          function postprocessexecutable(const fn : string;isdll:boolean):boolean;
       end;

       var
         IDF_PATH: string;

{*****************************************************************************
                                  TlinkerEmbedded
*****************************************************************************}

Constructor TlinkerFreeRTOS.Create;
begin
  Inherited Create;
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TlinkerFreeRTOS.SetDefaultInfo;
const
{$ifdef mips}
  {$ifdef mipsel}
    platform_select='-EL';
  {$else}
    platform_select='-EB';
  {$endif}
{$else}
  platform_select='';
{$endif}
begin
  with Info do
   begin
{$ifdef xtensa}
     if target_info.system=system_xtensa_freertos then
       if current_settings.controllertype = ct_esp8266 then
         begin
           IDF_PATH := 'IDF_PATH8266';
           ExeCmd[1]:=ExeCmd[1]+
             '-u call_user_start_cpu -u esp_app_desc -L $IDF_PATH8266/libs -T build-linker-script.ld'
         end
       else
         begin
           ExeCmd[1]:=ExeCmd[1]+
             '-u call_user_start_cpu0 -u ld_include_panic_highint_hdl -u esp_app_desc -u vfs_include_syscalls_impl -u pthread_include_pthread_impl -u pthread_include_pthread_cond_impl -u pthread_include_pthread_local_storage_impl -u newlib_include_locks_impl -u newlib_include_heap_impl -u newlib_include_syscalls_impl -u newlib_include_pthread_impl -u app_main -u uxTopUsedPriority '+
             '-L $IDF_PATH/libs -T build-linker-script.ld';
           IDF_PATH := 'IDF_PATH';
         end;
{$endif xtensa}
   end;
end;


Function TlinkerFreeRTOS.WriteResponseFile: Boolean;
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
{$if defined(ARM)}
  LinkStr  : string;
{$endif}
begin
  WriteResponseFile:=False;
  linklibc:=(SharedLibFiles.Find('c')<>nil);
{$if defined(ARM) or defined(i386) or defined(x86_64) or defined(AVR) or defined(MIPSEL) or defined(RISCV32) or defined(XTENSA)}
  prtobj:='';
{$else}
  prtobj:='prt0';
{$endif}
  cprtobj:='cprt0';
  if linklibc then
    prtobj:=cprtobj;

  { Open link.res file }
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
  if not(StaticLibFiles.Empty) then
    begin
      LinkRes.Add(')');
      LinkRes.Add('GROUP(');
      while not StaticLibFiles.Empty do
        begin
          S:=StaticLibFiles.GetFirst;
          LinkRes.AddFileName((maybequoted(s)));
        end;
    end;

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
  with embedded_controllers[current_settings.controllertype] do
    with linkres do
      begin
        Add('ENTRY(_START)');
        Add('MEMORY');
        Add('{');
        if flashsize<>0 then
          begin
            LinkStr := '    flash : ORIGIN = 0x' + IntToHex(flashbase,8)
              + ', LENGTH = 0x' + IntToHex(flashsize,8);
            Add(LinkStr);
          end;

        LinkStr := '    ram : ORIGIN = 0x' + IntToHex(srambase,8)
          + ', LENGTH = 0x' + IntToHex(sramsize,8);
        Add(LinkStr);

        Add('}');
        Add('_stack_top = 0x' + IntToHex(sramsize+srambase,8) + ';');
        Add('SECTIONS');
        Add('{');
        Add('    .text :');
        Add('    {');
        Add('    _text_start = .;');
        Add('    KEEP(*(.init .init.*))');
        Add('    *(.text .text.*)');
        Add('    *(.strings)');
        Add('    *(.rodata .rodata.*)');
        Add('    *(.comment)');
        Add('    . = ALIGN(4);');
        Add('    _etext = .;');
        if flashsize<>0 then
          begin
            Add('    } >flash');
            Add('    .note.gnu.build-id : { *(.note.gnu.build-id) } >flash ');
          end
        else
          begin
            Add('    } >ram');
            Add('    .note.gnu.build-id : { *(.note.gnu.build-id) } >ram ');
          end;

        Add('    .data :');
        Add('    {');
        Add('    _data = .;');
        Add('    *(.data .data.*)');
        Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
        Add('    _edata = .;');
      if flashsize<>0 then
        begin
          Add('    } >ram AT >flash');
        end
      else
        begin
          Add('    } >ram');
        end;
      Add('    .bss :');
      Add('    {');
      Add('    _bss_start = .;');
      Add('    *(.bss .bss.*)');
      Add('    *(COMMON)');
      Add('    } >ram');
      Add('    . = ALIGN(4);');
      Add('    _bss_end = . ;');
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
      Add('    _text = .;');
      Add('    KEEP(*(.init .init.*))');
      Add('    *(.text .text.*)');
      Add('    *(.strings)');
      Add('    *(.rodata .rodata.*)');
      Add('    *(.comment)');
      Add('    _etext = .;');
      Add('    }');
      Add('    .data ALIGN (0x1000) :');
      Add('    {');
      Add('    _data = .;');
      Add('    *(.data .data.*)');
      Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      Add('    _edata = .;');
      Add('    }');
      Add('    . = ALIGN(4);');
      Add('    .bss :');
      Add('    {');
      Add('    _bss_start = .;');
      Add('    *(.bss .bss.*)');
      Add('    *(COMMON)');
      Add('    }');
      Add('_bss_end = . ;');
      Add('}');
      Add('_end = .;');
    end;
{$endif i386}

{$ifdef x86_64}
  with linkres do
    begin
      Add('ENTRY(_START)');
      Add('SECTIONS');
      Add('{');
      Add('     . = 0x100000;');
      Add('     .text ALIGN (0x1000) :');
      Add('    {');
      Add('    _text = .;');
      Add('    KEEP(*(.init .init.*))');
      Add('    *(.text .text.*)');
      Add('    *(.strings)');
      Add('    *(.rodata .rodata.*)');
      Add('    *(.comment)');
      Add('    _etext = .;');
      Add('    }');
      Add('    .data ALIGN (0x1000) :');
      Add('    {');
      Add('    _data = .;');
      Add('    *(.data .data.*)');
      Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      Add('    _edata = .;');
      Add('    }');
      Add('    . = ALIGN(4);');
      Add('    .bss :');
      Add('    {');
      Add('    _bss_start = .;');
      Add('    *(.bss .bss.*)');
      Add('    *(COMMON)');
      Add('    }');
      Add('_bss_end = . ;');
      Add('}');
      Add('_end = .;');
    end;
{$endif x86_64}

{$ifdef AVR}
  with linkres do
    begin
      { linker script from ld 2.19 }
      Add('ENTRY(_START)');
      Add('OUTPUT_FORMAT("elf32-avr","elf32-avr","elf32-avr")');
      case current_settings.cputype of
       cpu_avr1:
         Add('OUTPUT_ARCH(avr:1)');
       cpu_avr2:
         Add('OUTPUT_ARCH(avr:2)');
       cpu_avr25:
         Add('OUTPUT_ARCH(avr:25)');
       cpu_avr3:
         Add('OUTPUT_ARCH(avr:3)');
       cpu_avr31:
         Add('OUTPUT_ARCH(avr:31)');
       cpu_avr35:
         Add('OUTPUT_ARCH(avr:35)');
       cpu_avr4:
         Add('OUTPUT_ARCH(avr:4)');
       cpu_avr5:
         Add('OUTPUT_ARCH(avr:5)');
       cpu_avr51:
         Add('OUTPUT_ARCH(avr:51)');
       cpu_avr6:
         Add('OUTPUT_ARCH(avr:6)');
       cpu_avrxmega3:
         Add('OUTPUT_ARCH(avr:103)');
       cpu_avrtiny:
         Add('OUTPUT_ARCH(avr:100)');
       else
         Internalerror(2015072701);
      end;
      Add('MEMORY');
      with embedded_controllers[current_settings.controllertype] do
        begin
          Add('{');
          Add('  text      (rx)   : ORIGIN = 0, LENGTH = 0x'+IntToHex(flashsize,6));
          Add('  data      (rw!x) : ORIGIN = 0x'+IntToHex($800000+srambase,6)+', LENGTH = 0x'+IntToHex(sramsize,6));
          Add('  eeprom    (rw!x) : ORIGIN = 0x810000, LENGTH = 0x'+IntToHex(eepromsize,6));
          Add('  fuse      (rw!x) : ORIGIN = 0x820000, LENGTH = 1K');
          Add('  lock      (rw!x) : ORIGIN = 0x830000, LENGTH = 1K');
          Add('  signature (rw!x) : ORIGIN = 0x840000, LENGTH = 1K');
          Add('}');
          Add('_stack_top = 0x' + IntToHex(srambase+sramsize-1,4) + ';');
        end;
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
      Add('    KEEP(*(.init .init.*))');
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

{$ifdef MIPSEL}
  case current_settings.controllertype of
      ct_none:
           begin
           end;
      ct_pic32mx110f016b,
      ct_pic32mx110f016c,
      ct_pic32mx110f016d,
      ct_pic32mx120f032b,
      ct_pic32mx120f032c,
      ct_pic32mx120f032d,
      ct_pic32mx130f064b,
      ct_pic32mx130f064c,
      ct_pic32mx130f064d,
      ct_pic32mx150f128b,
      ct_pic32mx150f128c,
      ct_pic32mx150f128d,
      ct_pic32mx210f016b,
      ct_pic32mx210f016c,
      ct_pic32mx210f016d,
      ct_pic32mx220f032b,
      ct_pic32mx220f032c,
      ct_pic32mx220f032d,
      ct_pic32mx230f064b,
      ct_pic32mx230f064c,
      ct_pic32mx230f064d,
      ct_pic32mx250f128b,
      ct_pic32mx250f128c,
      ct_pic32mx250f128d,
      ct_pic32mx775f256h,
      ct_pic32mx775f256l,
      ct_pic32mx775f512h,
      ct_pic32mx775f512l,
      ct_pic32mx795f512h,
      ct_pic32mx795f512l:
        begin
         with embedded_controllers[current_settings.controllertype] do
          with linkres do
            begin
              Add('OUTPUT_FORMAT("elf32-tradlittlemips")');
              Add('OUTPUT_ARCH(pic32mx)');
              Add('ENTRY(_reset)');
              Add('PROVIDE(_vector_spacing = 0x00000001);');
              Add('_ebase_address = 0x'+IntToHex(flashbase,8)+';');
              Add('_RESET_ADDR              = 0xBFC00000;');
              Add('_BEV_EXCPT_ADDR          = 0xBFC00380;');
              Add('_DBG_EXCPT_ADDR          = 0xBFC00480;');
              Add('_GEN_EXCPT_ADDR          = _ebase_address + 0x180;');
              Add('MEMORY');
              Add('{');
              if flashsize<>0 then
                begin
                  Add('  kseg0_program_mem          : ORIGIN = 0x'+IntToHex(flashbase,8)+', LENGTH = 0x'+IntToHex(flashsize,8));
                  //TODO This should better be placed into the controllertype records
                  Add('  kseg1_boot_mem             : ORIGIN = 0xBFC00000, LENGTH = 0xbef');
                  Add('  config3                    : ORIGIN = 0xBFC00BF0, LENGTH = 0x4');
                  Add('  config2                    : ORIGIN = 0xBFC00BF4, LENGTH = 0x4');
                  Add('  config1                    : ORIGIN = 0xBFC00BF8, LENGTH = 0x4');
                  Add('  config0                    : ORIGIN = 0xBFC00BFC, LENGTH = 0x4');
                end;

              Add('  ram                        : ORIGIN = 0x' + IntToHex(srambase,8)
              	+ ', LENGTH = 0x' + IntToHex(sramsize,8));

              Add('}');
              Add('_stack_top = 0x' + IntToHex(sramsize+srambase,8) + ';');
            end;
        end
  end;

  with linkres do
    begin
      Add('SECTIONS');
      Add('{');
      Add('    .reset _RESET_ADDR :');
      Add('    {');
      Add('      KEEP(*(.reset .reset.*))');
      Add('      KEEP(*(.startup .startup.*))');
      Add('    } > kseg1_boot_mem');
      Add('    .bev_excpt _BEV_EXCPT_ADDR :');
      Add('    {');
      Add('      KEEP(*(.bev_handler))');
      Add('    } > kseg1_boot_mem');

      Add('    .text :');
      Add('    {');
      Add('    _text_start = .;');
      Add('    . = _text_start + 0x180;');
      Add('    KEEP(*(.gen_handler))');
      Add('    . = _text_start + 0x200;');
      Add('    KEEP(*(.init .init.*))');
      Add('    *(.text .text.*)');
      Add('    *(.strings)');
      Add('    *(.rodata .rodata.*)');
      Add('    *(.comment)');
      Add('    _etext = .;');
      if embedded_controllers[current_settings.controllertype].flashsize<>0 then
        begin
          Add('    } >kseg0_program_mem');
        end
      else
        begin
          Add('    } >ram');
        end;
      Add('    .note.gnu.build-id : { *(.note.gnu.build-id) }');

      Add('    .data :');
      Add('    {');
      Add('    _data = .;');
      Add('    *(.data .data.*)');
      Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      Add('    . = .;');
      Add('    _gp = ALIGN(16) + 0x7ff0;');
      Add('    _edata = .;');
      if embedded_controllers[current_settings.controllertype].flashsize<>0 then
        begin
          Add('    } >ram AT >kseg0_program_mem');
        end
      else
        begin
          Add('    } >ram');
        end;
      Add('  .config_BFC00BF0 : {');
      Add('    KEEP(*(.config_BFC00BF0))');
      Add('  } > config3');
      Add('  .config_BFC00BF4 : {');
      Add('    KEEP(*(.config_BFC00BF4))');
      Add('  } > config2');
      Add('  .config_BFC00BF8 : {');
      Add('    KEEP(*(.config_BFC00BF8))');
      Add('  } > config1');
      Add('  .config_BFC00BFC : {');
      Add('    KEEP(*(.config_BFC00BFC))');
      Add('  } > config0');
      Add('    .bss :');
      Add('    {');
      Add('    _bss_start = .;');
      Add('    *(.bss .bss.*)');
      Add('    *(COMMON)');
      Add('    } >ram');
      Add('. = ALIGN(4);');
      Add('_bss_end = . ;');
      Add('  .comment       0 : { *(.comment) }');
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
      Add('  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }');
      Add('  .debug_abbrev   0 : { *(.debug_abbrev) }');
      Add('  /DISCARD/         : { *(.debug_line) }');
      Add('  .debug_frame    0 : { *(.debug_frame) }');
      Add('  .debug_str      0 : { *(.debug_str) }');
      Add('  /DISCARD/         : { *(.debug_loc) }');
      Add('  .debug_macinfo  0 : { *(.debug_macinfo) }');
      Add('  /* SGI/MIPS DWARF 2 extensions */');
      Add('  .debug_weaknames 0 : { *(.debug_weaknames) }');
      Add('  .debug_funcnames 0 : { *(.debug_funcnames) }');
      Add('  .debug_typenames 0 : { *(.debug_typenames) }');
      Add('  .debug_varnames  0 : { *(.debug_varnames) }');
      Add('  /* DWARF 3 */');
      Add('  .debug_pubtypes 0 : { *(.debug_pubtypes) }');
      Add('  .debug_ranges   0 : { *(.debug_ranges) }');
      Add('  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }');
      Add('  .gptab.sdata : { *(.gptab.data) *(.gptab.sdata) }');
      Add('  .gptab.sbss : { *(.gptab.bss) *(.gptab.sbss) }');
      Add('  .mdebug.abi32 : { KEEP(*(.mdebug.abi32)) }');
      Add('  .mdebug.abiN32 : { KEEP(*(.mdebug.abiN32)) }');
      Add('  .mdebug.abi64 : { KEEP(*(.mdebug.abi64)) }');
      Add('  .mdebug.abiO64 : { KEEP(*(.mdebug.abiO64)) }');
      Add('  .mdebug.eabi32 : { KEEP(*(.mdebug.eabi32)) }');
      Add('  .mdebug.eabi64 : { KEEP(*(.mdebug.eabi64)) }');
      Add('  /DISCARD/ : { *(.rel.dyn) }');
      Add('  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }');
      Add('}');
      Add('_end = .;');
    end;
{$endif MIPSEL}

{$ifdef RISCV32}
  with linkres do
    begin
      Add('OUTPUT_ARCH("riscv")');
      Add('ENTRY(_START)');
      Add('MEMORY');
      with embedded_controllers[current_settings.controllertype] do
        begin
          Add('{');
          Add('  flash      (rx)   : ORIGIN = 0x'+IntToHex(flashbase,6)+', LENGTH = 0x'+IntToHex(flashsize,6));
          Add('  ram        (rw!x) : ORIGIN = 0x'+IntToHex(srambase,6)+', LENGTH = 0x'+IntToHex(sramsize,6));
          Add('}');
          Add('_stack_top = 0x' + IntToHex(srambase+sramsize-1,4) + ';');
        end;
      Add('SECTIONS');
      Add('{');
      Add('  .text :');
      Add('  {');
      Add('    _text_start = .;');
      Add('    KEEP(*(.init .init.*))');
      Add('    *(.text .text.*)');
      Add('    *(.strings)');
      Add('    *(.rodata .rodata.*)');
      Add('    *(.comment)');
      Add('    . = ALIGN(4);');
      Add('    _etext = .;');
      if embedded_controllers[current_settings.controllertype].flashsize<>0 then
        begin
          Add('  } >flash');
          //Add('    .note.gnu.build-id : { *(.note.gnu.build-id) } >flash ');
        end
      else
        begin
          Add('  } >ram');
          //Add('    .note.gnu.build-id : { *(.note.gnu.build-id) } >ram ');
        end;

      Add('  .data :');
      Add('  {');
      Add('    _data = .;');
      Add('    *(.data .data.*)');
      Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      Add('    _edata = .;');
      if embedded_controllers[current_settings.controllertype].flashsize<>0 then
        begin
          Add('  } >ram AT >flash');
        end
      else
        begin
          Add('  } >ram');
        end;
      Add('  .bss :');
      Add('  {');
      Add('    _bss_start = .;');
      Add('    *(.bss .bss.*)');
      Add('    *(COMMON)');
      Add('  } >ram');
      Add('  . = ALIGN(4);');
      Add('  _bss_end = . ;');
      Add('  /* Stabs debugging sections.  */');
      Add('  .stab          0 : { *(.stab) }');
      Add('  .stabstr       0 : { *(.stabstr) }');
      Add('  .stab.excl     0 : { *(.stab.excl) }');
      Add('  .stab.exclstr  0 : { *(.stab.exclstr) }');
      Add('  .stab.index    0 : { *(.stab.index) }');
      Add('  .stab.indexstr 0 : { *(.stab.indexstr) }');
      Add('  .comment       0 : { *(.comment) }');
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
      Add('  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }');
      Add('  .debug_abbrev   0 : { *(.debug_abbrev) }');
      Add('  .debug_line     0 : { *(.debug_line) }');
      Add('  .debug_frame    0 : { *(.debug_frame) }');
      Add('  .debug_str      0 : { *(.debug_str) }');
      Add('  .debug_loc      0 : { *(.debug_loc) }');
      Add('  .debug_macinfo  0 : { *(.debug_macinfo) }');
      Add('  /* SGI/MIPS DWARF 2 extensions */');
      Add('  .debug_weaknames 0 : { *(.debug_weaknames) }');
      Add('  .debug_funcnames 0 : { *(.debug_funcnames) }');
      Add('  .debug_typenames 0 : { *(.debug_typenames) }');
      Add('  .debug_varnames  0 : { *(.debug_varnames) }');
      Add('  /* DWARF 3 */');
      Add('  .debug_pubtypes 0 : { *(.debug_pubtypes) }');
      Add('  .debug_ranges   0 : { *(.debug_ranges) }');

      Add('}');
      Add('_end = .;');

    end;
  {$endif RISCV32}

  {$ifdef XTENSA}
  with linkres do
    begin
      if current_settings.controllertype = ct_esp32 then
        begin
          //Add('SECTIONS');
          //Add('{');
          //Add('  .data :');
          //Add('  {');
          //Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
          //Add('  }');
          //Add('}');
          Add('/* esp32_out.ld */');
          Add('MEMORY');
          Add('{');
          Add('  iram0_0_seg (RX) : org = 0x40080000, len = 0x20000');
          Add('  iram0_2_seg (RX) : org = 0x400D0020, len = 0x330000-0x20');
          Add('  dram0_0_seg (RW) : org = 0x3FFB0000 + 0x0,');
          Add('                                     len = 0x2c200 - 0x0');
          Add('  drom0_0_seg (R) : org = 0x3F400020, len = 0x400000-0x20');
          Add('  rtc_iram_seg(RWX) : org = 0x400C0000, len = 0x2000');
          Add('  rtc_data_seg(RW) : org = 0x3ff80000, len = 0x2000 - 0');
          Add('  rtc_slow_seg(RW) : org = 0x50000000 + 0,');
          Add('                                     len = 0x1000 - 0');
          Add('  extern_ram_seg(RWX) : org = 0x3F800000,');
          Add('                                     len = 0x400000');
          Add('}');
          Add('_static_data_end = _bss_end;');
          Add('_heap_end = 0x40000000 - 0x0;');
          Add('_data_seg_org = ORIGIN(rtc_data_seg);');
          Add('REGION_ALIAS("rtc_data_location", rtc_slow_seg );');
          Add('  REGION_ALIAS("default_code_seg", iram0_2_seg);');
          Add('  REGION_ALIAS("default_rodata_seg", drom0_0_seg);');
          Add('/* esp32.project.ld */');
          Add('ENTRY(call_start_cpu0);');
          Add('SECTIONS');
          Add('{');
          Add('  .rtc.text :');
          Add('  {');
          Add('    . = ALIGN(4);');
          Add('    *(EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .rtc.literal EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .rtc.text EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .rtc.text.*)');
          Add('    *libsoc.a:uart_hal_iram.*( .rtc.literal  .rtc.text  .rtc.text.*)');
          Add('    *rtc_wake_stub*.*(.literal .text .literal.* .text.*)');
          Add('    _rtc_text_end = ABSOLUTE(.);');
          Add('  } > rtc_iram_seg');
          Add('  .rtc.dummy :');
          Add('  {');
          Add('    _rtc_dummy_start = ABSOLUTE(.);');
          Add('    _rtc_fast_start = ABSOLUTE(.);');
          Add('    . = SIZEOF(.rtc.text);');
          Add('    _rtc_dummy_end = ABSOLUTE(.);');
          Add('  } > rtc_data_seg');
          Add('  .rtc.force_fast :');
          Add('  {');
          Add('    . = ALIGN(4);');
          Add('    _rtc_force_fast_start = ABSOLUTE(.);');
          Add('    *(.rtc.force_fast .rtc.force_fast.*)');
          Add('    . = ALIGN(4) ;');
          Add('    _rtc_force_fast_end = ABSOLUTE(.);');
          Add('  } > rtc_data_seg');
          Add('  .rtc.data :');
          Add('  {');
          Add('    _rtc_data_start = ABSOLUTE(.);');
          Add('    *(EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .rtc.data EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .rtc.data.* EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .rtc.rodata EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .rtc.rodata.*)');
          Add('    *libsoc.a:uart_hal_iram.*( .rtc.data  .rtc.data.*  .rtc.rodata  .rtc.rodata.*)');
          Add('    *rtc_wake_stub*.*(.data .rodata .data.* .rodata.* .bss .bss.*)');
          Add('    _rtc_data_end = ABSOLUTE(.);');
          Add('  } > rtc_data_location');
          Add('  .rtc.bss (NOLOAD) :');
          Add('  {');
          Add('    _rtc_bss_start = ABSOLUTE(.);');
          Add('    *rtc_wake_stub*.*(.bss .bss.*)');
          Add('    *rtc_wake_stub*.*(COMMON)');
          Add('    *(EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .rtc.bss)');
          Add('    *libsoc.a:uart_hal_iram.*( .rtc.bss)');
          Add('    _rtc_bss_end = ABSOLUTE(.);');
          Add('  } > rtc_data_location');
          Add('  .rtc_noinit (NOLOAD):');
          Add('  {');
          Add('    . = ALIGN(4);');
          Add('    _rtc_noinit_start = ABSOLUTE(.);');
          Add('    *(.rtc_noinit .rtc_noinit.*)');
          Add('    . = ALIGN(4) ;');
          Add('    _rtc_noinit_end = ABSOLUTE(.);');
          Add('  } > rtc_data_location');
          Add('  .rtc.force_slow :');
          Add('  {');
          Add('    . = ALIGN(4);');
          Add('    _rtc_force_slow_start = ABSOLUTE(.);');
          Add('    *(.rtc.force_slow .rtc.force_slow.*)');
          Add('    . = ALIGN(4) ;');
          Add('    _rtc_force_slow_end = ABSOLUTE(.);');
          Add('  } > rtc_slow_seg');
          Add('  _rtc_slow_length = (ORIGIN(rtc_slow_seg) == ORIGIN(rtc_data_location))');
          Add('                        ? (_rtc_force_slow_end - _rtc_data_start)');
          Add('                        : (_rtc_force_slow_end - _rtc_force_slow_start);');
          Add('  _rtc_fast_length = (ORIGIN(rtc_slow_seg) == ORIGIN(rtc_data_location))');
          Add('                        ? (_rtc_force_fast_end - _rtc_fast_start)');
          Add('                        : (_rtc_noinit_end - _rtc_fast_start);');
          Add('  ASSERT((_rtc_slow_length <= LENGTH(rtc_slow_seg)),');
          Add('          "RTC_SLOW segment data does not fit.")');
          Add('  ASSERT((_rtc_fast_length <= LENGTH(rtc_data_seg)),');
          Add('          "RTC_FAST segment data does not fit.")');
          Add('  .iram0.vectors :');
          Add('  {');
          Add('    _iram_start = ABSOLUTE(.);');
          Add('    /* Vectors go to IRAM */');
          Add('    _init_start = ABSOLUTE(.);');
          Add('    /* Vectors according to builds/RF-2015.2-win32/esp108_v1_2_s5_512int_2/config.html */');
          Add('    . = 0x0;');
          Add('    KEEP(*(.WindowVectors.text));');
          Add('    . = 0x180;');
          Add('    KEEP(*(.Level2InterruptVector.text));');
          Add('    . = 0x1c0;');
          Add('    KEEP(*(.Level3InterruptVector.text));');
          Add('    . = 0x200;');
          Add('    KEEP(*(.Level4InterruptVector.text));');
          Add('    . = 0x240;');
          Add('    KEEP(*(.Level5InterruptVector.text));');
          Add('    . = 0x280;');
          Add('    KEEP(*(.DebugExceptionVector.text));');
          Add('    . = 0x2c0;');
          Add('    KEEP(*(.NMIExceptionVector.text));');
          Add('    . = 0x300;');
          Add('    KEEP(*(.KernelExceptionVector.text));');
          Add('    . = 0x340;');
          Add('    KEEP(*(.UserExceptionVector.text));');
          Add('    . = 0x3C0;');
          Add('    KEEP(*(.DoubleExceptionVector.text));');
          Add('    . = 0x400;');
          Add('    *(.*Vector.literal)');
          Add('    *(.UserEnter.literal);');
          Add('    *(.UserEnter.text);');
          Add('    . = ALIGN (16);');
          Add('    *(.entry.text)');
          Add('    *(.init.literal)');
          Add('    *(.init)');
          Add('    _init_end = ABSOLUTE(.);');
          Add('  } > iram0_0_seg');
          Add('  .iram0.text :');
          Add('  {');
          Add('    _iram_text_start = ABSOLUTE(.);');
          Add('    *(EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .iram1 EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .iram1.*)');
          Add('    *libsoc.a:rtc_init.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:rtc_time.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:ledc_hal_iram.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:rtc_wdt.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:spi_flash_hal_gpspi.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:spi_slave_hal_iram.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:lldesc.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:rtc_periph.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:rtc_pm.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:rtc_clk_init.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:uart_hal_iram.*( .iram1  .iram1.*)');
          Add('    *libsoc.a:spi_hal_iram.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:rtc_sleep.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:spi_flash_hal_iram.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:cpu_util.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:rtc_clk.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libsoc.a:i2c_hal_iram.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libgcc.a:_divsf3.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libgcc.a:lib2funcs.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libxtensa.a:eri.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libspi_flash.a:spi_flash_rom_patch.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libspi_flash.a:spi_flash_chip_issi.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libspi_flash.a:spi_flash_chip_gd.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libspi_flash.a:spi_flash_chip_generic.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libspi_flash.a:memspi_host_driver.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libesp32.a:panic.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libnewlib.a:heap.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libhal.a:( .literal  .literal.*  .text  .text.*)');
          Add('    *libesp_ringbuf.a:( .literal  .literal.*  .text  .text.*)');
          Add('    *libfreertos.a:( .literal  .literal.*  .text  .text.*)');
          Add('    *libgcov.a:( .literal  .literal.*  .text  .text.*)');
          Add('    *librtc.a:( .literal  .literal.*  .text  .text.*)');
          Add('    *libheap.a:multi_heap_poisoning.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libheap.a:multi_heap.*( .literal  .literal.*  .text  .text.*)');
          Add('    _iram_text_end = ABSOLUTE(.);');
          Add('  } > iram0_0_seg');
          Add('  .dram0.data :');
          Add('  {');
          Add('    _data_start = ABSOLUTE(.);');
          Add('    _bt_data_start = ABSOLUTE(.);');
          Add('    *libbt.a:(.data .data.*)');
          Add('    . = ALIGN (4);');
          Add('    _bt_data_end = ABSOLUTE(.);');
          Add('    _btdm_data_start = ABSOLUTE(.);');
          Add('    *libbtdm_app.a:(.data .data.*)');
          Add('    . = ALIGN (4);');
          Add('    _btdm_data_end = ABSOLUTE(.);');
          Add('    _nimble_data_start = ABSOLUTE(.);');
          Add('    *libnimble.a:(.data .data.*)');
          Add('    . = ALIGN (4);');
          Add('    _nimble_data_end = ABSOLUTE(.);');
          Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
          Add('    *(.gnu.linkonce.d.*)');
          Add('    *(.data1)');
          Add('    *(.sdata)');
          Add('    *(.sdata.*)');
          Add('    *(.gnu.linkonce.s.*)');
          Add('    *(.sdata2)');
          Add('    *(.sdata2.*)');
          Add('    *(.gnu.linkonce.s2.*)');
          Add('    *(.jcr)');
          Add('    *(EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .data EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .data.* EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .dram1 EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .dram1.*)');
          Add('    *libsoc.a:spi_flash_hal_gpspi.*( .rodata  .rodata.*)');
          Add('    *libsoc.a:uart_hal_iram.*( .data  .data.*  .dram1  .dram1.*)');
          Add('    *libsoc.a:spi_flash_hal_iram.*( .rodata  .rodata.*)');
          Add('    *libsoc.a:rtc_clk.*( .rodata  .rodata.*)');
          Add('    *libsoc.a:i2c_hal_iram.*( .rodata  .rodata.*)');
          Add('    *libgcc.a:_divsf3.*( .rodata  .rodata.*)');
          Add('    *libspi_flash.a:spi_flash_chip_issi.*( .rodata  .rodata.*)');
          Add('    *libspi_flash.a:spi_flash_chip_gd.*( .rodata  .rodata.*)');
          Add('    *libspi_flash.a:spi_flash_chip_generic.*( .rodata  .rodata.*)');
          Add('    *libspi_flash.a:memspi_host_driver.*( .rodata  .rodata.*)');
          Add('    *libphy.a:( .rodata  .rodata.*)');
          Add('    *libesp32.a:panic.*( .rodata  .rodata.*)');
          Add('    *libnewlib.a:heap.*( .rodata  .rodata.*)');
          Add('    *libgcov.a:( .rodata  .rodata.*)');
          Add('    *libheap.a:multi_heap_poisoning.*( .rodata  .rodata.*)');
          Add('    *libheap.a:multi_heap.*( .rodata  .rodata.*)');
          Add('    _data_end = ABSOLUTE(.);');
          Add('    . = ALIGN(4);');
          Add('  } > dram0_0_seg');
          Add('  .noinit (NOLOAD):');
          Add('  {');
          Add('    . = ALIGN(4);');
          Add('    _noinit_start = ABSOLUTE(.);');
          Add('    *(.noinit .noinit.*)');
          Add('    . = ALIGN(4) ;');
          Add('    _noinit_end = ABSOLUTE(.);');
          Add('  } > dram0_0_seg');
          Add('  .dram0.bss (NOLOAD) :');
          Add('  {');
          Add('    . = ALIGN (8);');
          Add('    _bss_start = ABSOLUTE(.);');
          Add('    *(.ext_ram.bss*)');
          Add('    _bt_bss_start = ABSOLUTE(.);');
          Add('    *libbt.a:(.bss .bss.* COMMON)');
          Add('    . = ALIGN (4);');
          Add('    _bt_bss_end = ABSOLUTE(.);');
          Add('    _btdm_bss_start = ABSOLUTE(.);');
          Add('    *libbtdm_app.a:(.bss .bss.* COMMON)');
          Add('    . = ALIGN (4);');
          Add('    _btdm_bss_end = ABSOLUTE(.);');
          Add('    _nimble_bss_start = ABSOLUTE(.);');
          Add('    *libnimble.a:(.bss .bss.* COMMON)');
          Add('    . = ALIGN (4);');
          Add('    _nimble_bss_end = ABSOLUTE(.);');
          Add('    *(EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .bss EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .bss.* EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) COMMON)');
          Add('    *libsoc.a:uart_hal_iram.*( .bss  .bss.*  COMMON)');
          Add('    *(.dynsbss)');
          Add('    *(.sbss)');
          Add('    *(.sbss.*)');
          Add('    *(.gnu.linkonce.sb.*)');
          Add('    *(.scommon)');
          Add('    *(.sbss2)');
          Add('    *(.sbss2.*)');
          Add('    *(.gnu.linkonce.sb2.*)');
          Add('    *(.dynbss)');
          Add('    *(.share.mem)');
          Add('    *(.gnu.linkonce.b.*)');
          Add('    . = ALIGN (8);');
          Add('    _bss_end = ABSOLUTE(.);');
          Add('  } > dram0_0_seg');
          Add('  ASSERT(((_bss_end - ORIGIN(dram0_0_seg)) <= LENGTH(dram0_0_seg)),');
          Add('          "DRAM segment data does not fit.")');
          Add('  .flash.rodata :');
          Add('  {');
          Add('    _rodata_start = ABSOLUTE(.);');
          Add('    *(.rodata_desc .rodata_desc.*)               /* Should be the first.  App version info.        DO NOT PUT ANYTHING BEFORE IT! */');
          Add('    *(.rodata_custom_desc .rodata_custom_desc.*) /* Should be the second. Custom app version info. DO NOT PUT ANYTHING BEFORE IT! */');
          Add('    *(EXCLUDE_FILE(*libsoc.a:i2c_hal_iram.* *libsoc.a:rtc_clk.* *libsoc.a:spi_flash_hal_iram.* *libsoc.a:uart_hal_iram.* *libsoc.a:spi_flash_hal_gpspi.* *libgcc.a:_divsf3.* *libspi_flash.a:memspi_host_driver.* *libspi_flash.a:spi_flash_chip_generic.* *libspi_flash.a:spi_flash_chip_gd.* *libspi_flash.a:spi_flash_chip_issi.* *libphy.a *libesp32.a:panic.* *libnewlib.a:heap.* *libgcov.a *libheap.a:multi_heap.* *libheap.a:multi_heap_poisoning.*) .rodata EXCLUDE_FILE(*libsoc.a:i2c_hal_iram.* *libsoc.a:rtc_clk.* *libsoc.a:spi_flash_hal_iram.* *libsoc.a:uart_hal_iram.* *libsoc.a:spi_flash_hal_gpspi.* *libgcc.a:_divsf3.* *libspi_flash.a:memspi_host_driver.* *libspi_flash.a:spi_flash_chip_generic.* *libspi_flash.a:spi_flash_chip_gd.* *libspi_flash.a:spi_flash_chip_issi.* *libphy.a *libesp32.a:panic.* *libnewlib.a:heap.* *libgcov.a *libheap.a:multi_heap.* *libheap.a:multi_heap_poisoning.*) .rodata.*)');
          Add('    *libsoc.a:uart_hal_iram.*( .rodata  .rodata.*)');
          Add('    *(.irom1.text) /* catch stray ICACHE_RODATA_ATTR */');
          Add('    *(.gnu.linkonce.r.*)');
          Add('    *(.rodata1)');
          Add('    __XT_EXCEPTION_TABLE_ = ABSOLUTE(.);');
          Add('    *(.xt_except_table)');
          Add('    *(.gcc_except_table .gcc_except_table.*)');
          Add('    *(.gnu.linkonce.e.*)');
          Add('    *(.gnu.version_r)');
          Add('    . = (. + 3) & ~ 3;');
          Add('    __eh_frame = ABSOLUTE(.);');
          Add('    KEEP(*(.eh_frame))');
          Add('    . = (. + 7) & ~ 3;');
          Add('    __init_array_start = ABSOLUTE(.);');
          Add('    KEEP (*(EXCLUDE_FILE (*crtend.* *crtbegin.*) .ctors .ctors.*))');
          Add('    __init_array_end = ABSOLUTE(.);');
          Add('    KEEP (*crtbegin.*(.dtors))');
          Add('    KEEP (*(EXCLUDE_FILE (*crtend.*) .dtors))');
          Add('    KEEP (*(SORT(.dtors.*)))');
          Add('    KEEP (*(.dtors))');
          Add('    __XT_EXCEPTION_DESCS_ = ABSOLUTE(.);');
          Add('    *(.xt_except_desc)');
          Add('    *(.gnu.linkonce.h.*)');
          Add('    __XT_EXCEPTION_DESCS_END__ = ABSOLUTE(.);');
          Add('    *(.xt_except_desc_end)');
          Add('    *(.dynamic)');
          Add('    *(.gnu.version_d)');
          Add('    soc_reserved_memory_region_start = ABSOLUTE(.);');
          Add('    KEEP (*(.reserved_memory_address))');
          Add('    soc_reserved_memory_region_end = ABSOLUTE(.);');
          Add('    _rodata_end = ABSOLUTE(.);');
          Add('    _lit4_start = ABSOLUTE(.);');
          Add('    *(*.lit4)');
          Add('    *(.lit4.*)');
          Add('    *(.gnu.linkonce.lit4.*)');
          Add('    _lit4_end = ABSOLUTE(.);');
          Add('    . = ALIGN(4);');
          Add('    _thread_local_start = ABSOLUTE(.);');
          Add('    *(.tdata)');
          Add('    *(.tdata.*)');
          Add('    *(.tbss)');
          Add('    *(.tbss.*)');
          Add('    _thread_local_end = ABSOLUTE(.);');
          Add('    . = ALIGN(4);');
          Add('  } >default_rodata_seg');
          Add('  .flash.text :');
          Add('  {');
          Add('    _stext = .;');
          Add('    _text_start = ABSOLUTE(.);');
          Add('    *(EXCLUDE_FILE(*libsoc.a:i2c_hal_iram.* *libsoc.a:rtc_clk.* *libsoc.a:cpu_util.* *libsoc.a:spi_flash_hal_iram.* *libsoc.a:rtc_sleep.* *libsoc.a:spi_hal_iram.* *libsoc.a:uart_hal_iram.* *libsoc.a:rtc_clk_init.* *libsoc.a:rtc_pm.* *libsoc.a:rtc_periph.* *libsoc.a:lldesc.* *libsoc.a:spi_slave_hal_iram.* *libsoc.a:spi_flash_hal_gpspi.* *libsoc.a:rtc_wdt.* *libsoc.a:ledc_hal_iram.* *libsoc.a:rtc_time.* *libsoc.a:rtc_init.* *libgcc.a:lib2funcs.* *libgcc.a:_divsf3.* *libxtensa.a:eri.* *libspi_flash.a:memspi_host_driver.* *libspi_flash.a:spi_flash_chip_generic.* *libspi_flash.a:spi_flash_chip_gd.* *libspi_flash.a:spi_flash_chip_issi.* *libspi_flash.a:spi_flash_rom_patch.* *libesp32.a:panic.* *libnewlib.a:heap.* *libhal.a *libesp_ringbuf.a *libfreertos.a *libgcov.a *librtc.a *libheap.a:multi_heap.* *libheap.a:multi_heap_poisoning.*) .literal EXCLUDE_FILE(*libsoc.a:i2c_hal_iram.* *libsoc.a:rtc_clk.* *libsoc.a:cpu_util.* *libsoc.a:spi_flash_hal_iram.* *libsoc.a:rtc_sleep.* *libsoc.a:spi_hal_iram.* *libsoc.a:uart_hal_iram.* *libsoc.a:rtc_clk_init.* *libsoc.a:rtc_pm.* *libsoc.a:rtc_periph.* *libsoc.a:lldesc.* *libsoc.a:spi_slave_hal_iram.* *libsoc.a:spi_flash_hal_gpspi.* *libsoc.a:rtc_wdt.* *libsoc.a:ledc_hal_iram.* *libsoc.a:rtc_time.* *libsoc.a:rtc_init.* *libgcc.a:lib2funcs.* *libgcc.a:_divsf3.* *libxtensa.a:eri.* *libspi_flash.a:memspi_host_driver.* *libspi_flash.a:spi_flash_chip_generic.* *libspi_flash.a:spi_flash_chip_gd.* *libspi_flash.a:spi_flash_chip_issi.* *libspi_flash.a:spi_flash_rom_patch.* *libesp32.a:panic.* *libnewlib.a:heap.* *libhal.a *libesp_ringbuf.a *libfreertos.a *libgcov.a *librtc.a *libheap.a:multi_heap.* *libheap.a:multi_heap_poisoning.*) .literal.* EXCLUDE_FILE(*libsoc.a:i2c_hal_iram.* *libsoc.a:rtc_clk.* *libsoc.a:cpu_util.* *libsoc.a:spi_flash_hal_iram.* *libsoc.a:rtc_sleep.* *libsoc.a:spi_hal_iram.* *libsoc.a:uart_hal_iram.* *libsoc.a:rtc_clk_init.* *libsoc.a:rtc_pm.* *libsoc.a:rtc_periph.* *libsoc.a:lldesc.* *libsoc.a:spi_slave_hal_iram.* *libsoc.a:spi_flash_hal_gpspi.* *libsoc.a:rtc_wdt.* *libsoc.a:ledc_hal_iram.* *libsoc.a:rtc_time.* *libsoc.a:rtc_init.* *libgcc.a:lib2funcs.* *libgcc.a:_divsf3.* *libxtensa.a:eri.* *libspi_flash.a:memspi_host_driver.* *libspi_flash.a:spi_flash_chip_generic.* *libspi_flash.a:spi_flash_chip_gd.* *libspi_flash.a:spi_flash_chip_issi.* *libspi_flash.a:spi_flash_rom_patch.* *libesp32.a:panic.* *libnewlib.a:heap.* *libhal.a *libesp_ringbuf.a *libfreertos.a *libgcov.a *librtc.a *libheap.a:multi_heap.* *libheap.a:multi_heap_poisoning.*) .text EXCLUDE_FILE(*libsoc.a:i2c_hal_iram.* *libsoc.a:rtc_clk.* *libsoc.a:cpu_util.* *libsoc.a:spi_flash_hal_iram.* *libsoc.a:rtc_sleep.* *libsoc.a:spi_hal_iram.* *libsoc.a:uart_hal_iram.* *libsoc.a:rtc_clk_init.* *libsoc.a:rtc_pm.* *libsoc.a:rtc_periph.* *libsoc.a:lldesc.* *libsoc.a:spi_slave_hal_iram.* *libsoc.a:spi_flash_hal_gpspi.* *libsoc.a:rtc_wdt.* *libsoc.a:ledc_hal_iram.* *libsoc.a:rtc_time.* *libsoc.a:rtc_init.* *libgcc.a:lib2funcs.* *libgcc.a:_divsf3.* *libxtensa.a:eri.* *libspi_flash.a:memspi_host_driver.* *libspi_flash.a:spi_flash_chip_generic.* *libspi_flash.a:spi_flash_chip_gd.* *libspi_flash.a:spi_flash_chip_issi.* *libspi_flash.a:spi_flash_rom_patch.* *libesp32.a:panic.* *libnewlib.a:heap.* *libhal.a *libesp_ringbuf.a *libfreertos.a *libgcov.a *librtc.a *libheap.a:multi_heap.* *libheap.a:multi_heap_poisoning.*) .text.* EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .wifi0iram EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .wifi0iram.* EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .wifirxiram EXCLUDE_FILE(*libsoc.a:uart_hal_iram.*) .wifirxiram.*)');
          Add('    *libsoc.a:uart_hal_iram.*( .literal  .literal.*  .text  .text.*  .wifi0iram  .wifi0iram.*  .wifirxiram  .wifirxiram.*)');
          Add('    *(.stub .gnu.warning .gnu.linkonce.literal.* .gnu.linkonce.t.*.literal .gnu.linkonce.t.*)');
          Add('    *(.irom0.text) /* catch stray ICACHE_RODATA_ATTR */');
          Add('    *(.fini.literal)');
          Add('    *(.fini)');
          Add('    *(.gnu.version)');
          Add('    _text_end = ABSOLUTE(.);');
          Add('    _etext = .;');
          Add('    _flash_cache_start = ABSOLUTE(0);');
          Add('  } >default_code_seg');
          Add('  .iram0.text_end (NOLOAD) :');
          Add('  {');
          Add('    . = ALIGN (4);');
          Add('    _iram_end = ABSOLUTE(.);');
          Add('  } > iram0_0_seg');
          Add('  .dram0.heap_start (NOLOAD) :');
          Add('  {');
          Add('    . = ALIGN (8);');
          Add('    _heap_start = ABSOLUTE(.);');
          Add('  } > dram0_0_seg');
          Add('}');
          Add('ASSERT(((_iram_text_end - ORIGIN(iram0_0_seg)) <= LENGTH(iram0_0_seg)),');
          Add('          "IRAM0 segment data does not fit.")');
          Add('ASSERT(((_heap_start - ORIGIN(dram0_0_seg)) <= LENGTH(dram0_0_seg)),');
          Add('          "DRAM segment data does not fit.")');
          Add('/*  esp32.rom.ld */');
          Add('PROVIDE ( Add2SelfBigHex256 = 0x40015b7c );');
          Add('PROVIDE ( AddBigHex256 = 0x40015b28 );');
          Add('PROVIDE ( AddBigHexModP256 = 0x40015c98 );');
          Add('PROVIDE ( AddP256 = 0x40015c74 );');
          Add('PROVIDE ( AddPdiv2_256 = 0x40015ce0 );');
          Add('PROVIDE ( app_gpio_arg = 0x3ffe003c );');
          Add('PROVIDE ( app_gpio_handler = 0x3ffe0040 );');
          Add('PROVIDE ( BasePoint_x_256 = 0x3ff97488 );');
          Add('PROVIDE ( BasePoint_y_256 = 0x3ff97468 );');
          Add('PROVIDE ( bigHexInversion256 = 0x400168f0 );');
          Add('PROVIDE ( bigHexP256 = 0x3ff973bc );');
          Add('PROVIDE ( btdm_r_ble_bt_handler_tab_p_get = 0x40019b0c );');
          Add('PROVIDE ( btdm_r_btdm_option_data_p_get = 0x40010004 );');
          Add('PROVIDE ( btdm_r_btdm_rom_version_get = 0x40010078 );');
          Add('PROVIDE ( btdm_r_data_init = 0x4001002c );');
          Add('PROVIDE ( btdm_r_import_rf_phy_func_p_get = 0x40054298 );');
          Add('PROVIDE ( btdm_r_ip_func_p_get = 0x40019af0 );');
          Add('PROVIDE ( btdm_r_ip_func_p_set = 0x40019afc );');
          Add('PROVIDE ( btdm_r_modules_func_p_get = 0x4005427c );');
          Add('PROVIDE ( btdm_r_modules_func_p_set = 0x40054270 );');
          Add('PROVIDE ( btdm_r_plf_func_p_set = 0x40054288 );');
          Add('PROVIDE ( bt_util_buf_env = 0x3ffb8bd4 );');
          Add('PROVIDE ( cache_flash_mmu_set_rom = 0x400095e0 );');
          Add('PROVIDE ( Cache_Flush_rom = 0x40009a14 );');
          Add('PROVIDE ( Cache_Read_Disable_rom = 0x40009ab8 );');
          Add('PROVIDE ( Cache_Read_Enable_rom = 0x40009a84 );');
          Add('PROVIDE ( Cache_Read_Init_rom = 0x40009950 );');
          Add('PROVIDE ( cache_sram_mmu_set_rom = 0x400097f4 );');
          Add('PROVIDE ( calc_rtc_memory_crc = 0x40008170 );');
          Add('PROVIDE ( __clear_cache = 0x40063860 );');
          Add('PROVIDE ( co_default_bdaddr = 0x3ffae704 );');
          Add('PROVIDE ( co_null_bdaddr = 0x3ffb80e0 );');
          Add('PROVIDE ( co_sca2ppm = 0x3ff971e8 );');
          Add('PROVIDE ( crc16_be = 0x4005d09c );');
          Add('PROVIDE ( crc16_le = 0x4005d05c );');
          Add('PROVIDE ( crc32_be = 0x4005d024 );');
          Add('PROVIDE ( crc32_le = 0x4005cfec );');
          Add('PROVIDE ( crc8_be = 0x4005d114 );');
          Add('PROVIDE ( crc8_le = 0x4005d0e0 );');
          Add('PROVIDE ( _data_end_rom = 0x4000d5c8 );');
          Add('PROVIDE ( _data_end_btdm_rom = 0x4000d4f8 );');
          Add('PROVIDE ( _data_start_rom = 0x4000d4f8 );');
          Add('PROVIDE ( _data_start_btdm_rom = 0x4000d4f4 );');
          Add('PROVIDE ( _data_start_btdm = 0x3ffae6e0);');
          Add('PROVIDE ( _data_end_btdm = 0x3ffaff10);');
          Add('PROVIDE ( _bss_start_btdm = 0x3ffb8000);');
          Add('PROVIDE ( _bss_end_btdm = 0x3ffbff70);');
          Add('PROVIDE ( dbg_default_handler = 0x3ff97218 );');
          Add('PROVIDE ( dbg_default_state = 0x3ff97220 );');
          Add('PROVIDE ( dbg_state = 0x3ffb8d5d );');
          Add('PROVIDE ( DebugE256PublicKey_x = 0x3ff97428 );');
          Add('PROVIDE ( DebugE256PublicKey_y = 0x3ff97408 );');
          Add('PROVIDE ( DebugE256SecretKey = 0x3ff973e8 );');
          Add('PROVIDE ( debug_timer = 0x3ffe042c );');
          Add('PROVIDE ( debug_timerfn = 0x3ffe0430 );');
          Add('PROVIDE ( dh_group14_generator = 0x3ff9ac60 );');
          Add('PROVIDE ( dh_group14_prime = 0x3ff9ab60 );');
          Add('PROVIDE ( dh_group15_generator = 0x3ff9ab5f );');
          Add('PROVIDE ( dh_group15_prime = 0x3ff9a9df );');
          Add('PROVIDE ( dh_group16_generator = 0x3ff9a9de );');
          Add('PROVIDE ( dh_group16_prime = 0x3ff9a7de );');
          Add('PROVIDE ( dh_group17_generator = 0x3ff9a7dd );');
          Add('PROVIDE ( dh_group17_prime = 0x3ff9a4dd );');
          Add('PROVIDE ( dh_group18_generator = 0x3ff9a4dc );');
          Add('PROVIDE ( dh_group18_prime = 0x3ff9a0dc );');
          Add('PROVIDE ( dh_group1_generator = 0x3ff9ae03 );');
          Add('PROVIDE ( dh_group1_prime = 0x3ff9ada3 );');
          Add('PROVIDE ( dh_group2_generator = 0x3ff9ada2 );');
          Add('PROVIDE ( dh_group2_prime = 0x3ff9ad22 );');
          Add('PROVIDE ( dh_group5_generator = 0x3ff9ad21 );');
          Add('PROVIDE ( dh_group5_prime = 0x3ff9ac61 );');
          Add('PROVIDE ( g_rom_spiflash_dummy_len_plus = 0x3ffae290 );');
          Add('PROVIDE ( ecc_env = 0x3ffb8d60 );');
          Add('PROVIDE ( ecc_Jacobian_InfinityPoint256 = 0x3ff972e8 );');
          Add('PROVIDE ( em_buf_env = 0x3ffb8d74 );');
          Add('PROVIDE ( esp_crc8 = 0x4005d144 );');
          Add('PROVIDE ( _etext = 0x4000d66c );');
          Add('PROVIDE ( ets_readySet_ = 0x3ffe01f0 );');
          Add('PROVIDE ( ets_startup_callback = 0x3ffe0404 );');
          Add('PROVIDE ( rwip_coex_cfg = 0x3ff9914c );');
          Add('PROVIDE ( rwip_priority = 0x3ff99159 );');
          Add('PROVIDE ( exc_cause_table = 0x3ff991d0 );');
          Add('PROVIDE ( GF_Jacobian_Point_Addition256 = 0x400163a4 );');
          Add('PROVIDE ( GF_Jacobian_Point_Double256 = 0x40016260 );');
          Add('PROVIDE ( GF_Point_Jacobian_To_Affine256 = 0x40016b0c );');
          Add('PROVIDE ( g_phyFuns_instance = 0x3ffae0c4 );');
          Add('PROVIDE ( g_rom_flashchip = 0x3ffae270 );');
          Add('PROVIDE ( gTxMsg = 0x3ffe0050 );');
          Add('PROVIDE ( hci_cmd_desc_root_tab = 0x3ff976d4 );');
          Add('PROVIDE ( hci_cmd_desc_tab_ctrl_bb = 0x3ff97b70 );');
          Add('PROVIDE ( hci_cmd_desc_tab_info_par = 0x3ff97b1c );');
          Add('PROVIDE ( hci_cmd_desc_tab_le = 0x3ff97870 );');
          Add('PROVIDE ( hci_cmd_desc_tab_lk_ctrl = 0x3ff97fc0 );');
          Add('PROVIDE ( hci_cmd_desc_tab_lk_pol = 0x3ff97f3c );');
          Add('PROVIDE ( hci_cmd_desc_tab_stat_par = 0x3ff97ac8 );');
          Add('PROVIDE ( hci_cmd_desc_tab_testing = 0x3ff97a98 );');
          Add('PROVIDE ( hci_cmd_desc_tab_vs = 0x3ff97714 );');
          Add('PROVIDE ( hci_command_handler = 0x4004c928 );');
          Add('PROVIDE ( hci_env = 0x3ffb9350 );');
          Add('PROVIDE ( rwip_env = 0x3ffb8bcc );');
          Add('PROVIDE ( hci_evt_dbg_desc_tab = 0x3ff9750c );');
          Add('PROVIDE ( hci_evt_desc_tab = 0x3ff9751c );');
          Add('PROVIDE ( hci_evt_le_desc_tab = 0x3ff974b4 );');
          Add('PROVIDE ( hci_fc_env = 0x3ffb9340 );');
          Add('PROVIDE ( jd_decomp = 0x400613e8 );');
          Add('PROVIDE ( jd_prepare = 0x40060fa8 );');
          Add('PROVIDE ( ke_env = 0x3ffb93cc );');
          Add('PROVIDE ( lb_default_handler = 0x3ff982b8 );');
          Add('PROVIDE ( lb_default_state_tab_p_get = 0x4001c198 );');
          Add('PROVIDE ( lb_env = 0x3ffb9424 );');
          Add('PROVIDE ( lb_hci_cmd_handler_tab_p_get = 0x4001c18c );');
          Add('PROVIDE ( lb_state = 0x3ffb94e8 );');
          Add('PROVIDE ( lc_default_handler = 0x3ff98648 );');
          Add('PROVIDE ( lc_default_state_tab_p_get = 0x4002f494 );');
          Add('PROVIDE ( lc_env = 0x3ffb94ec );');
          Add('PROVIDE ( lc_hci_cmd_handler_tab_p_get = 0x4002f488 );');
          Add('PROVIDE ( lc_state = 0x3ffb9508 );');
          Add('PROVIDE ( ld_acl_br_sizes = 0x3ff98a2a );');
          Add('PROVIDE ( ld_acl_br_types = 0x3ff98a36 );');
          Add('PROVIDE ( ld_acl_edr_sizes = 0x3ff98a14 );');
          Add('PROVIDE ( ld_acl_edr_types = 0x3ff98a22 );');
          Add('PROVIDE ( ld_env = 0x3ffb9510 );');
          Add('PROVIDE ( ld_pcm_settings_dft = 0x3ff98a0c );');
          Add('PROVIDE ( ld_sched_params = 0x3ffb96c0 );');
          Add('PROVIDE ( ld_sync_train_channels = 0x3ff98a3c );');
          Add('PROVIDE ( llc_default_handler = 0x3ff98b3c );');
          Add('PROVIDE ( llc_default_state_tab_p_get = 0x40046058 );');
          Add('PROVIDE ( llc_env = 0x3ffb96d0 );');
          Add('PROVIDE ( llc_hci_acl_data_tx_handler = 0x40042398 );');
          Add('PROVIDE ( llc_hci_cmd_handler_tab_p_get = 0x40042358 );');
          Add('PROVIDE ( llc_hci_command_handler = 0x40042360 );');
          Add('PROVIDE ( llcp_pdu_handler_tab_p_get = 0x40043f64 );');
          Add('PROVIDE ( llc_state = 0x3ffb96f8 );');
          Add('PROVIDE ( lldesc_build_chain = 0x4000a850 );');
          Add('PROVIDE ( lldesc_num2link = 0x4000a948 );');
          Add('PROVIDE ( lldesc_set_owner = 0x4000a974 );');
          Add('PROVIDE ( lld_evt_deferred_elt_push = 0x400466b4 );');
          Add('PROVIDE ( lld_evt_deferred_elt_pop = 0x400466dc );');
          Add('PROVIDE ( lld_evt_winsize_change = 0x40046730 );');
          Add('PROVIDE ( lld_evt_rxwin_compute = 0x400467c8 );');
          Add('PROVIDE ( lld_evt_slave_time_compute = 0x40046818 );');
          Add('PROVIDE ( lld_evt_env = 0x3ffb9704 );');
          Add('PROVIDE ( lld_evt_elt_wait_get = 0x400468e4 );');
          Add('PROVIDE ( lld_evt_get_next_free_slot = 0x4004692c );');
          Add('PROVIDE ( lld_pdu_adv_pk_desc_tab = 0x3ff98c70 );');
          Add('PROVIDE ( lld_pdu_llcp_pk_desc_tab = 0x3ff98b68 );');
          Add('PROVIDE ( lld_pdu_tx_flush_list = 0x4004a760 );');
          Add('PROVIDE ( lld_pdu_pack = 0x4004ab14 );');
          Add('PROVIDE ( LLM_AA_CT1 = 0x3ff98d8a );');
          Add('PROVIDE ( LLM_AA_CT2 = 0x3ff98d88 );');
          Add('PROVIDE ( llm_default_handler = 0x3ff98d80 );');
          Add('PROVIDE ( llm_default_state_tab_p_get = 0x4004e718 );');
          Add('PROVIDE ( llm_hci_cmd_handler_tab_p_get = 0x4004c920 );');
          Add('PROVIDE ( llm_le_env = 0x3ffb976c );');
          Add('PROVIDE ( llm_local_cmds = 0x3ff98d38 );');
          Add('PROVIDE ( llm_local_data_len_values = 0x3ff98d1c );');
          Add('PROVIDE ( llm_local_le_feats = 0x3ff98d30 );');
          Add('PROVIDE ( llm_local_le_states = 0x3ff98d28 );');
          Add('PROVIDE ( llm_state = 0x3ffb985c );');
          Add('PROVIDE ( lm_default_handler = 0x3ff990e0 );');
          Add('PROVIDE ( lm_default_state_tab_p_get = 0x40054268 );');
          Add('PROVIDE ( lm_env = 0x3ffb9860 );');
          Add('PROVIDE ( lm_hci_cmd_handler_tab_p_get = 0x4005425c );');
          Add('PROVIDE ( lm_local_supp_feats = 0x3ff990ee );');
          Add('PROVIDE ( lm_n_page_tab = 0x3ff990e8 );');
          Add('PROVIDE ( lmp_desc_tab = 0x3ff96e6c );');
          Add('PROVIDE ( lmp_ext_desc_tab = 0x3ff96d9c );');
          Add('PROVIDE ( lm_state = 0x3ffb9a1c );');
          Add('PROVIDE ( maxSecretKey_256 = 0x3ff97448 );');
          Add('PROVIDE ( mmu_init = 0x400095a4 );');
          Add('PROVIDE ( MultiplyBigHexByUint32_256 = 0x40016214 );');
          Add('PROVIDE ( MultiplyBigHexModP256 = 0x400160b8 );');
          Add('PROVIDE ( MultiplyByU32ModP256 = 0x40015fdc );');
          Add('PROVIDE ( multofup = 0x4000ab8c );');
          Add('PROVIDE ( mz_adler32 = 0x4005edbc );');
          Add('PROVIDE ( mz_crc32 = 0x4005ee88 );');
          Add('PROVIDE ( mz_free = 0x4005eed4 );');
          Add('PROVIDE ( notEqual256 = 0x40015b04 );');
          Add('PROVIDE ( one_bits = 0x3ff971f8 );');
          Add('PROVIDE ( phy_get_romfuncs = 0x40004100 );');
          Add('PROVIDE ( _Pri_4_HandlerAddress = 0x3ffe0648 );');
          Add('PROVIDE ( _Pri_5_HandlerAddress = 0x3ffe064c );');
          Add('PROVIDE ( r_btdm_option_data = 0x3ffae6e0 );');
          Add('PROVIDE ( r_bt_util_buf_acl_rx_alloc = 0x40010218 );');
          Add('PROVIDE ( r_bt_util_buf_acl_rx_free = 0x40010234 );');
          Add('PROVIDE ( r_bt_util_buf_acl_tx_alloc = 0x40010268 );');
          Add('PROVIDE ( r_bt_util_buf_acl_tx_free = 0x40010280 );');
          Add('PROVIDE ( r_bt_util_buf_init = 0x400100e4 );');
          Add('PROVIDE ( r_bt_util_buf_lmp_tx_alloc = 0x400101d0 );');
          Add('PROVIDE ( r_bt_util_buf_lmp_tx_free = 0x400101ec );');
          Add('PROVIDE ( r_bt_util_buf_sync_clear = 0x400103c8 );');
          Add('PROVIDE ( r_bt_util_buf_sync_init = 0x400102c4 );');
          Add('PROVIDE ( r_bt_util_buf_sync_rx_alloc = 0x40010468 );');
          Add('PROVIDE ( r_bt_util_buf_sync_rx_free = 0x4001049c );');
          Add('PROVIDE ( r_bt_util_buf_sync_tx_alloc = 0x400103ec );');
          Add('PROVIDE ( r_bt_util_buf_sync_tx_free = 0x40010428 );');
          Add('PROVIDE ( r_co_bdaddr_compare = 0x40014324 );');
          Add('PROVIDE ( r_co_bytes_to_string = 0x400142e4 );');
          Add('PROVIDE ( r_co_list_check_size_available = 0x400142c4 );');
          Add('PROVIDE ( r_co_list_extract = 0x4001404c );');
          Add('PROVIDE ( r_co_list_extract_after = 0x40014118 );');
          Add('PROVIDE ( r_co_list_find = 0x4001419c );');
          Add('PROVIDE ( r_co_list_init = 0x40013f14 );');
          Add('PROVIDE ( r_co_list_insert_after = 0x40014254 );');
          Add('PROVIDE ( r_co_list_insert_before = 0x40014200 );');
          Add('PROVIDE ( r_co_list_merge = 0x400141bc );');
          Add('PROVIDE ( r_co_list_pool_init = 0x40013f30 );');
          Add('PROVIDE ( r_co_list_pop_front = 0x40014028 );');
          Add('PROVIDE ( r_co_list_push_back = 0x40013fb8 );');
          Add('PROVIDE ( r_co_list_push_front = 0x40013ff4 );');
          Add('PROVIDE ( r_co_list_size = 0x400142ac );');
          Add('PROVIDE ( r_co_nb_good_channels = 0x40014360 );');
          Add('PROVIDE ( r_co_slot_to_duration = 0x40014348 );');
          Add('PROVIDE ( r_dbg_init = 0x40014394 );');
          Add('PROVIDE ( r_dbg_platform_reset_complete = 0x400143d0 );');
          Add('PROVIDE ( r_dbg_swdiag_init = 0x40014470 );');
          Add('PROVIDE ( r_dbg_swdiag_read = 0x400144a4 );');
          Add('PROVIDE ( r_dbg_swdiag_write = 0x400144d0 );');
          Add('PROVIDE ( r_E1 = 0x400108e8 );');
          Add('PROVIDE ( r_E21 = 0x40010968 );');
          Add('PROVIDE ( r_E22 = 0x400109b4 );');
          Add('PROVIDE ( r_E3 = 0x40010a58 );');
          Add('PROVIDE ( lm_n192_mod_mul = 0x40011dc0 );');
          Add('PROVIDE ( lm_n192_mod_add = 0x40011e9c );');
          Add('PROVIDE ( lm_n192_mod_sub = 0x40011eec );');
          Add('PROVIDE ( r_ea_alarm_clear = 0x40015ab4 );');
          Add('PROVIDE ( r_ea_alarm_set = 0x40015a10 );');
          Add('PROVIDE ( r_ea_elt_cancel = 0x400150d0 );');
          Add('PROVIDE ( r_ea_elt_create = 0x40015264 );');
          Add('PROVIDE ( r_ea_elt_insert = 0x400152a8 );');
          Add('PROVIDE ( r_ea_elt_remove = 0x400154f0 );');
          Add('PROVIDE ( r_ea_finetimer_isr = 0x400155d4 );');
          Add('PROVIDE ( r_ea_init = 0x40015228 );');
          Add('PROVIDE ( r_ea_interval_create = 0x4001555c );');
          Add('PROVIDE ( r_ea_interval_delete = 0x400155a8 );');
          Add('PROVIDE ( r_ea_interval_duration_req = 0x4001597c );');
          Add('PROVIDE ( r_ea_interval_insert = 0x4001557c );');
          Add('PROVIDE ( r_ea_interval_remove = 0x40015590 );');
          Add('PROVIDE ( ea_conflict_check = 0x40014e9c );');
          Add('PROVIDE ( ea_prog_timer = 0x40014f88 );');
          Add('PROVIDE ( r_ea_offset_req = 0x40015748 );');
          Add('PROVIDE ( r_ea_sleep_check = 0x40015928 );');
          Add('PROVIDE ( r_ea_sw_isr = 0x40015724 );');
          Add('PROVIDE ( r_ea_time_get_halfslot_rounded = 0x40015894 );');
          Add('PROVIDE ( r_ea_time_get_slot_rounded = 0x400158d4 );');
          Add('PROVIDE ( r_ecc_abort_key256_generation = 0x40017070 );');
          Add('PROVIDE ( r_ecc_generate_key256 = 0x40016e00 );');
          Add('PROVIDE ( r_ecc_gen_new_public_key = 0x400170c0 );');
          Add('PROVIDE ( r_ecc_gen_new_secret_key = 0x400170e4 );');
          Add('PROVIDE ( r_ecc_get_debug_Keys = 0x40017224 );');
          Add('PROVIDE ( r_ecc_init = 0x40016dbc );');
          Add('PROVIDE ( ecc_point_multiplication_uint8_256 = 0x40016804);');
          Add('PROVIDE ( RecvBuff = 0x3ffe009c );');
          Add('PROVIDE ( r_em_buf_init = 0x4001729c );');
          Add('PROVIDE ( r_em_buf_rx_buff_addr_get = 0x400173e8 );');
          Add('PROVIDE ( r_em_buf_rx_free = 0x400173c4 );');
          Add('PROVIDE ( r_em_buf_tx_buff_addr_get = 0x40017404 );');
          Add('PROVIDE ( r_em_buf_tx_free = 0x4001741c );');
          Add('PROVIDE ( r_F1_256 = 0x400133e4 );');
          Add('PROVIDE ( r_F2_256 = 0x40013568 );');
          Add('PROVIDE ( r_F3_256 = 0x40013664 );');
          Add('PROVIDE ( RFPLL_ICP_TABLE = 0x3ffb8b7c );');
          Add('PROVIDE ( r_G_256 = 0x40013470 );');
          Add('PROVIDE ( r_H3 = 0x40013760 );');
          Add('PROVIDE ( r_H4 = 0x40013830 );');
          Add('PROVIDE ( r_h4tl_init = 0x40017878 );');
          Add('PROVIDE ( r_h4tl_start = 0x40017924 );');
          Add('PROVIDE ( r_h4tl_stop = 0x40017934 );');
          Add('PROVIDE ( r_h4tl_write = 0x400178d0 );');
          Add('PROVIDE ( r_H5 = 0x400138dc );');
          Add('PROVIDE ( r_hashConcat = 0x40013a38 );');
          Add('PROVIDE ( r_hci_acl_tx_data_alloc = 0x4001951c );');
          Add('PROVIDE ( r_hci_acl_tx_data_received = 0x40019654 );');
          Add('PROVIDE ( r_hci_bt_acl_bdaddr_register = 0x40018900 );');
          Add('PROVIDE ( r_hci_bt_acl_bdaddr_unregister = 0x400189ac );');
          Add('PROVIDE ( r_hci_bt_acl_conhdl_register = 0x4001895c );');
          Add('PROVIDE ( r_hci_cmd_get_max_param_size = 0x400192d0 );');
          Add('PROVIDE ( r_hci_cmd_received = 0x400192f8 );');
          Add('PROVIDE ( r_hci_evt_filter_add = 0x40018a64 );');
          Add('PROVIDE ( r_hci_evt_mask_set = 0x400189e4 );');
          Add('PROVIDE ( r_hci_fc_acl_buf_size_set = 0x40017988 );');
          Add('PROVIDE ( r_hci_fc_acl_en = 0x400179d8 );');
          Add('PROVIDE ( r_hci_fc_acl_packet_sent = 0x40017a3c );');
          Add('PROVIDE ( r_hci_fc_check_host_available_nb_acl_packets = 0x40017aa4 );');
          Add('PROVIDE ( r_hci_fc_check_host_available_nb_sync_packets = 0x40017ac8 );');
          Add('PROVIDE ( r_hci_fc_host_nb_acl_pkts_complete = 0x40017a6c );');
          Add('PROVIDE ( r_hci_fc_host_nb_sync_pkts_complete = 0x40017a88 );');
          Add('PROVIDE ( r_hci_fc_init = 0x40017974 );');
          Add('PROVIDE ( r_hci_fc_sync_buf_size_set = 0x400179b0 );');
          Add('PROVIDE ( r_hci_fc_sync_en = 0x40017a30 );');
          Add('PROVIDE ( r_hci_fc_sync_packet_sent = 0x40017a54 );');
          Add('PROVIDE ( r_hci_init = 0x40018538 );');
          Add('PROVIDE ( r_hci_look_for_cmd_desc = 0x40018454 );');
          Add('PROVIDE ( r_hci_look_for_dbg_evt_desc = 0x400184c4 );');
          Add('PROVIDE ( r_hci_look_for_evt_desc = 0x400184a0 );');
          Add('PROVIDE ( r_hci_look_for_le_evt_desc = 0x400184e0 );');
          Add('PROVIDE ( r_hci_reset = 0x4001856c );');
          Add('PROVIDE ( r_hci_send_2_host = 0x400185bc );');
          Add('PROVIDE ( r_hci_sync_tx_data_alloc = 0x40019754 );');
          Add('PROVIDE ( r_hci_sync_tx_data_received = 0x400197c0 );');
          Add('PROVIDE ( r_hci_tl_init = 0x40019290 );');
          Add('PROVIDE ( r_hci_tl_send = 0x40019228 );');
          Add('PROVIDE ( r_hci_util_pack = 0x40019874 );');
          Add('PROVIDE ( r_hci_util_unpack = 0x40019998 );');
          Add('PROVIDE ( r_hci_voice_settings_get = 0x40018bdc );');
          Add('PROVIDE ( r_hci_voice_settings_set = 0x40018be8 );');
          Add('PROVIDE ( r_HMAC = 0x40013968 );');
          Add('PROVIDE ( r_import_rf_phy_func = 0x3ffb8354 );');
          Add('PROVIDE ( r_import_rf_phy_func_p = 0x3ffafd64 );');
          Add('PROVIDE ( r_ip_funcs = 0x3ffae710 );');
          Add('PROVIDE ( r_ip_funcs_p = 0x3ffae70c );');
          Add('PROVIDE ( r_ke_check_malloc = 0x40019de0 );');
          Add('PROVIDE ( r_ke_event_callback_set = 0x40019ba8 );');
          Add('PROVIDE ( r_ke_event_clear = 0x40019c2c );');
          Add('PROVIDE ( r_ke_event_flush = 0x40019ccc );');
          Add('PROVIDE ( r_ke_event_get = 0x40019c78 );');
          Add('PROVIDE ( r_ke_event_get_all = 0x40019cc0 );');
          Add('PROVIDE ( r_ke_event_init = 0x40019b90 );');
          Add('PROVIDE ( r_ke_event_schedule = 0x40019cdc );');
          Add('PROVIDE ( r_ke_event_set = 0x40019be0 );');
          Add('PROVIDE ( r_ke_flush = 0x4001a374 );');
          Add('PROVIDE ( r_ke_free = 0x4001a014 );');
          Add('PROVIDE ( r_ke_get_max_mem_usage = 0x4001a1c8 );');
          Add('PROVIDE ( r_ke_get_mem_usage = 0x4001a1a0 );');
          Add('PROVIDE ( r_ke_init = 0x4001a318 );');
          Add('PROVIDE ( r_ke_is_free = 0x4001a184 );');
          Add('PROVIDE ( r_ke_malloc = 0x40019eb4 );');
          Add('PROVIDE ( r_ke_mem_init = 0x40019d3c );');
          Add('PROVIDE ( r_ke_mem_is_empty = 0x40019d8c );');
          Add('PROVIDE ( r_ke_msg_alloc = 0x4001a1e0 );');
          Add('PROVIDE ( r_ke_msg_dest_id_get = 0x4001a2e0 );');
          Add('PROVIDE ( r_ke_msg_discard = 0x4001a850 );');
          Add('PROVIDE ( r_ke_msg_forward = 0x4001a290 );');
          Add('PROVIDE ( r_ke_msg_forward_new_id = 0x4001a2ac );');
          Add('PROVIDE ( r_ke_msg_free = 0x4001a2cc );');
          Add('PROVIDE ( r_ke_msg_in_queue = 0x4001a2f8 );');
          Add('PROVIDE ( r_ke_msg_save = 0x4001a858 );');
          Add('PROVIDE ( r_ke_msg_send = 0x4001a234 );');
          Add('PROVIDE ( r_ke_msg_send_basic = 0x4001a26c );');
          Add('PROVIDE ( r_ke_msg_src_id_get = 0x4001a2ec );');
          Add('PROVIDE ( r_ke_queue_extract = 0x40055fd0 );');
          Add('PROVIDE ( r_ke_queue_insert = 0x40056020 );');
          Add('PROVIDE ( r_ke_sleep_check = 0x4001a3d8 );');
          Add('PROVIDE ( r_ke_state_get = 0x4001a7d8 );');
          Add('PROVIDE ( r_ke_state_set = 0x4001a6fc );');
          Add('PROVIDE ( r_ke_stats_get = 0x4001a3f0 );');
          Add('PROVIDE ( r_ke_task_check = 0x4001a8a4 );');
          Add('PROVIDE ( r_ke_task_create = 0x4001a674 );');
          Add('PROVIDE ( r_ke_task_delete = 0x4001a6c0 );');
          Add('PROVIDE ( r_ke_task_init = 0x4001a650 );');
          Add('PROVIDE ( r_ke_task_msg_flush = 0x4001a860 );');
          Add('PROVIDE ( r_ke_timer_active = 0x4001ac08 );');
          Add('PROVIDE ( r_ke_timer_adjust_all = 0x4001ac30 );');
          Add('PROVIDE ( r_ke_timer_clear = 0x4001ab90 );');
          Add('PROVIDE ( r_ke_timer_init = 0x4001aa9c );');
          Add('PROVIDE ( r_ke_timer_set = 0x4001aac0 );');
          Add('PROVIDE ( r_ke_timer_sleep_check = 0x4001ac50 );');
          Add('PROVIDE ( r_KPrimC = 0x40010ad4 );');
          Add('PROVIDE ( r_lb_clk_adj_activate = 0x4001ae70 );');
          Add('PROVIDE ( r_lb_clk_adj_id_get = 0x4001af14 );');
          Add('PROVIDE ( r_lb_clk_adj_period_update = 0x4001af20 );');
          Add('PROVIDE ( r_lb_init = 0x4001acd4 );');
          Add('PROVIDE ( r_lb_mst_key = 0x4001afc0 );');
          Add('PROVIDE ( r_lb_mst_key_cmp = 0x4001af74 );');
          Add('PROVIDE ( r_lb_mst_key_restart_enc = 0x4001b0d4 );');
          Add('PROVIDE ( r_lb_mst_start_act_bcst_enc = 0x4001b198 );');
          Add('PROVIDE ( r_lb_mst_stop_act_bcst_enc = 0x4001b24c );');
          Add('PROVIDE ( r_lb_reset = 0x4001ad38 );');
          Add('PROVIDE ( r_lb_send_lmp = 0x4001adbc );');
          Add('PROVIDE ( r_lb_send_pdu_clk_adj = 0x4001af3c );');
          Add('PROVIDE ( r_lb_util_get_csb_mode = 0x4001ada4 );');
          Add('PROVIDE ( r_lb_util_get_nb_broadcast = 0x4001ad80 );');
          Add('PROVIDE ( r_lb_util_get_res_lt_addr = 0x4001ad98 );');
          Add('PROVIDE ( r_lb_util_set_nb_broadcast = 0x4001ad8c );');
          Add('PROVIDE ( r_lc_afh_set = 0x4001cc74 );');
          Add('PROVIDE ( r_lc_afh_start = 0x4001d240 );');
          Add('PROVIDE ( r_lc_auth_cmp = 0x4001cd54 );');
          Add('PROVIDE ( r_lc_calc_link_key = 0x4001ce7c );');
          Add('PROVIDE ( r_lc_chg_pkt_type_cmp = 0x4001d038 );');
          Add('PROVIDE ( r_lc_chg_pkt_type_cont = 0x4001cfbc );');
          Add('PROVIDE ( r_lc_chg_pkt_type_retry = 0x4001d0ac );');
          Add('PROVIDE ( r_lc_chk_to = 0x4001d2a8 );');
          Add('PROVIDE ( r_lc_cmd_stat_send = 0x4001c914 );');
          Add('PROVIDE ( r_lc_comb_key_svr = 0x4001d30c );');
          Add('PROVIDE ( r_lc_con_cmp = 0x4001d44c );');
          Add('PROVIDE ( r_lc_con_cmp_evt_send = 0x4001d4fc );');
          Add('PROVIDE ( r_lc_conn_seq_done = 0x40021334 );');
          Add('PROVIDE ( r_lc_detach = 0x4002037c );');
          Add('PROVIDE ( r_lc_dhkey = 0x4001d564 );');
          Add('PROVIDE ( r_lc_enc_cmp = 0x4001d8bc );');
          Add('PROVIDE ( r_lc_enc_key_refresh = 0x4001d720 );');
          Add('PROVIDE ( r_lc_end_chk_colli = 0x4001d858 );');
          Add('PROVIDE ( r_lc_end_of_sniff_nego = 0x4001d9a4 );');
          Add('PROVIDE ( r_lc_enter_sniff_mode = 0x4001ddb8 );');
          Add('PROVIDE ( r_lc_epr_change_lk = 0x4001db38 );');
          Add('PROVIDE ( r_lc_epr_cmp = 0x4001da88 );');
          Add('PROVIDE ( r_lc_epr_resp = 0x4001e0b4 );');
          Add('PROVIDE ( r_lc_epr_rsw_cmp = 0x4001dd40 );');
          Add('PROVIDE ( r_lc_ext_feat = 0x40020d6c );');
          Add('PROVIDE ( r_lc_feat = 0x40020984 );');
          Add('PROVIDE ( r_lc_hl_connect = 0x400209e8 );');
          Add('PROVIDE ( r_lc_init = 0x4001c948 );');
          Add('PROVIDE ( r_lc_init_calc_f3 = 0x4001deb0 );');
          Add('PROVIDE ( r_lc_initiator_epr = 0x4001e064 );');
          Add('PROVIDE ( r_lc_init_passkey_loop = 0x4001dfc0 );');
          Add('PROVIDE ( r_lc_init_start_mutual_auth = 0x4001df60 );');
          Add('PROVIDE ( r_lc_key_exch_end = 0x4001e140 );');
          Add('PROVIDE ( r_lc_legacy_pair = 0x4001e1c0 );');
          Add('PROVIDE ( r_lc_local_switch = 0x4001e22c );');
          Add('PROVIDE ( r_lc_local_trans_mode = 0x4001e2e4 );');
          Add('PROVIDE ( r_lc_local_untrans_mode = 0x4001e3a0 );');
          Add('PROVIDE ( r_lc_loc_auth = 0x40020ecc );');
          Add('PROVIDE ( r_lc_locepr_lkref = 0x4001d648 );');
          Add('PROVIDE ( r_lc_locepr_rsw = 0x4001d5d0 );');
          Add('PROVIDE ( r_lc_loc_sniff = 0x40020a6c );');
          Add('PROVIDE ( r_lc_max_slot_mgt = 0x4001e410 );');
          Add('PROVIDE ( r_lc_mst_key = 0x4001e7c0 );');
          Add('PROVIDE ( r_lc_mst_qos_done = 0x4001ea80 );');
          Add('PROVIDE ( r_lc_mst_send_mst_key = 0x4001e8f4 );');
          Add('PROVIDE ( r_lc_mutual_auth_end = 0x4001e670 );');
          Add('PROVIDE ( r_lc_mutual_auth_end2 = 0x4001e4f4 );');
          Add('PROVIDE ( r_lc_packet_type = 0x40021038 );');
          Add('PROVIDE ( r_lc_pair = 0x40020ddc );');
          Add('PROVIDE ( r_lc_pairing_cont = 0x4001eafc );');
          Add('PROVIDE ( r_lc_passkey_comm = 0x4001ed20 );');
          Add('PROVIDE ( r_lc_prepare_all_links_for_clk_adj = 0x40021430 );');
          Add('PROVIDE ( r_lc_proc_rcv_dhkey = 0x4001edec );');
          Add('PROVIDE ( r_lc_ptt = 0x4001ee2c );');
          Add('PROVIDE ( r_lc_ptt_cmp = 0x4001eeec );');
          Add('PROVIDE ( r_lc_qos_setup = 0x4001ef50 );');
          Add('PROVIDE ( r_lc_rd_rem_name = 0x4001efd0 );');
          Add('PROVIDE ( r_lc_release = 0x4001f8a8 );');
          Add('PROVIDE ( r_lc_rem_enc = 0x4001f124 );');
          Add('PROVIDE ( r_lc_rem_name_cont = 0x4001f290 );');
          Add('PROVIDE ( r_lc_rem_nego_trans_mode = 0x4001f1b4 );');
          Add('PROVIDE ( r_lc_rem_sniff = 0x40020ca4 );');
          Add('PROVIDE ( r_lc_rem_sniff_sub_rate = 0x40020b10 );');
          Add('PROVIDE ( r_lc_rem_switch = 0x4001f070 );');
          Add('PROVIDE ( r_lc_rem_trans_mode = 0x4001f314 );');
          Add('PROVIDE ( r_lc_rem_unsniff = 0x400207a0 );');
          Add('PROVIDE ( r_lc_rem_untrans_mode = 0x4001f36c );');
          Add('PROVIDE ( r_lc_reset = 0x4001c99c );');
          Add('PROVIDE ( r_lc_resp_auth = 0x4001f518 );');
          Add('PROVIDE ( r_lc_resp_calc_f3 = 0x4001f710 );');
          Add('PROVIDE ( r_lc_resp_num_comp = 0x40020074 );');
          Add('PROVIDE ( r_lc_resp_oob_nonce = 0x4001f694 );');
          Add('PROVIDE ( r_lc_resp_oob_wait_nonce = 0x4001f66c );');
          Add('PROVIDE ( r_lc_resp_pair = 0x400208a4 );');
          Add('PROVIDE ( r_lc_resp_sec_auth = 0x4001f4a0 );');
          Add('PROVIDE ( r_lc_resp_wait_dhkey_cont = 0x4001f86c );');
          Add('PROVIDE ( r_lc_restart_enc = 0x4001f8ec );');
          Add('PROVIDE ( r_lc_restart_enc_cont = 0x4001f940 );');
          Add('PROVIDE ( r_lc_restore_afh_reporting = 0x4001f028 );');
          Add('PROVIDE ( r_lc_restore_to = 0x4001f9e0 );');
          Add('PROVIDE ( r_lc_ret_sniff_max_slot_chg = 0x4001fa30 );');
          Add('PROVIDE ( r_lc_rsw_clean_up = 0x4001dc70 );');
          Add('PROVIDE ( r_lc_rsw_done = 0x4001db94 );');
          Add('PROVIDE ( r_lc_sco_baseband_ack = 0x40022b00 );');
          Add('PROVIDE ( r_lc_sco_detach = 0x40021e40 );');
          Add('PROVIDE ( r_lc_sco_host_accept = 0x40022118 );');
          Add('PROVIDE ( r_lc_sco_host_reject = 0x400222b8 );');
          Add('PROVIDE ( r_lc_sco_host_request = 0x40021f4c );');
          Add('PROVIDE ( r_lc_sco_host_request_disc = 0x4002235c );');
          Add('PROVIDE ( r_lc_sco_init = 0x40021dc8 );');
          Add('PROVIDE ( r_lc_sco_peer_accept = 0x40022780 );');
          Add('PROVIDE ( r_lc_sco_peer_accept_disc = 0x40022a08 );');
          Add('PROVIDE ( r_lc_sco_peer_reject = 0x40022824 );');
          Add('PROVIDE ( r_lc_sco_peer_reject_disc = 0x40022a8c );');
          Add('PROVIDE ( r_lc_sco_peer_request = 0x4002240c );');
          Add('PROVIDE ( r_lc_sco_peer_request_disc = 0x400228ec );');
          Add('PROVIDE ( r_lc_sco_release = 0x40021eec );');
          Add('PROVIDE ( r_lc_sco_reset = 0x40021dfc );');
          Add('PROVIDE ( r_lc_sco_timeout = 0x40022bd4 );');
          Add('PROVIDE ( r_lc_sec_auth_compute_sres = 0x4001f3ec );');
          Add('PROVIDE ( r_lc_semi_key_cmp = 0x40020294 );');
          Add('PROVIDE ( r_lc_send_enc_chg_evt = 0x4002134c );');
          Add('PROVIDE ( r_lc_send_enc_mode = 0x40020220 );');
          Add('PROVIDE ( r_lc_send_lmp = 0x4001c1a8 );');
          Add('PROVIDE ( r_lc_send_pdu_acc = 0x4001c21c );');
          Add('PROVIDE ( r_lc_send_pdu_acc_ext4 = 0x4001c240 );');
          Add('PROVIDE ( r_lc_send_pdu_au_rand = 0x4001c308 );');
          Add('PROVIDE ( r_lc_send_pdu_auto_rate = 0x4001c5d0 );');
          Add('PROVIDE ( r_lc_send_pdu_clk_adj_ack = 0x4001c46c );');
          Add('PROVIDE ( r_lc_send_pdu_clk_adj_req = 0x4001c494 );');
          Add('PROVIDE ( r_lc_send_pdu_comb_key = 0x4001c368 );');
          Add('PROVIDE ( r_lc_send_pdu_dhkey_chk = 0x4001c8e8 );');
          Add('PROVIDE ( r_lc_send_pdu_encaps_head = 0x4001c440 );');
          Add('PROVIDE ( r_lc_send_pdu_encaps_payl = 0x4001c410 );');
          Add('PROVIDE ( r_lc_send_pdu_enc_key_sz_req = 0x4001c670 );');
          Add('PROVIDE ( r_lc_send_pdu_esco_lk_rem_req = 0x4001c5a8 );');
          Add('PROVIDE ( r_lc_send_pdu_feats_ext_req = 0x4001c6ec );');
          Add('PROVIDE ( r_lc_send_pdu_feats_res = 0x4001c694 );');
          Add('PROVIDE ( r_lc_send_pdu_in_rand = 0x4001c338 );');
          Add('PROVIDE ( r_lc_send_pdu_io_cap_res = 0x4001c72c );');
          Add('PROVIDE ( r_lc_send_pdu_lsto = 0x4001c64c );');
          Add('PROVIDE ( r_lc_send_pdu_max_slot = 0x4001c3c8 );');
          Add('PROVIDE ( r_lc_send_pdu_max_slot_req = 0x4001c3ec );');
          Add('PROVIDE ( r_lc_send_pdu_not_acc = 0x4001c26c );');
          Add('PROVIDE ( r_lc_send_pdu_not_acc_ext4 = 0x4001c294 );');
          Add('PROVIDE ( r_lc_send_pdu_num_comp_fail = 0x4001c770 );');
          Add('PROVIDE ( r_lc_send_pdu_pause_enc_aes_req = 0x4001c794 );');
          Add('PROVIDE ( r_lc_send_pdu_paus_enc_req = 0x4001c7c0 );');
          Add('PROVIDE ( r_lc_send_pdu_ptt_req = 0x4001c4c0 );');
          Add('PROVIDE ( r_lc_send_pdu_qos_req = 0x4001c82c );');
          Add('PROVIDE ( r_lc_send_pdu_resu_enc_req = 0x4001c7e4 );');
          Add('PROVIDE ( r_lc_send_pdu_sco_lk_rem_req = 0x4001c580 );');
          Add('PROVIDE ( r_lc_send_pdu_set_afh = 0x4001c2c8 );');
          Add('PROVIDE ( r_lc_send_pdu_setup_cmp = 0x4001c808 );');
          Add('PROVIDE ( r_lc_send_pdu_slot_off = 0x4001c854 );');
          Add('PROVIDE ( r_lc_send_pdu_sniff_req = 0x4001c5f0 );');
          Add('PROVIDE ( r_lc_send_pdu_sp_cfm = 0x4001c518 );');
          Add('PROVIDE ( r_lc_send_pdu_sp_nb = 0x4001c4e8 );');
          Add('PROVIDE ( r_lc_send_pdu_sres = 0x4001c548 );');
          Add('PROVIDE ( r_lc_send_pdu_tim_acc = 0x4001c6cc );');
          Add('PROVIDE ( r_lc_send_pdu_unit_key = 0x4001c398 );');
          Add('PROVIDE ( r_lc_send_pdu_unsniff_req = 0x4001c894 );');
          Add('PROVIDE ( r_lc_send_pdu_vers_req = 0x4001c8b4 );');
          Add('PROVIDE ( r_lc_skip_hl_oob_req = 0x400201bc );');
          Add('PROVIDE ( r_lc_sniff_init = 0x40022cac );');
          Add('PROVIDE ( r_lc_sniff_max_slot_chg = 0x40020590 );');
          Add('PROVIDE ( r_lc_sniff_reset = 0x40022cc8 );');
          Add('PROVIDE ( r_lc_sniff_slot_unchange = 0x40021100 );');
          Add('PROVIDE ( r_lc_sniff_sub_mode = 0x400204fc );');
          Add('PROVIDE ( r_lc_sp_end = 0x400213a8 );');
          Add('PROVIDE ( r_lc_sp_fail = 0x40020470 );');
          Add('PROVIDE ( r_lc_sp_oob_tid_fail = 0x400204cc );');
          Add('PROVIDE ( r_lc_ssr_nego = 0x4002125c );');
          Add('PROVIDE ( r_lc_start = 0x4001ca28 );');
          Add('PROVIDE ( r_lc_start_enc = 0x4001fb28 );');
          Add('PROVIDE ( r_lc_start_enc_key_size = 0x4001fd9c );');
          Add('PROVIDE ( r_lc_start_key_exch = 0x4001fe10 );');
          Add('PROVIDE ( r_lc_start_lmp_to = 0x4001fae8 );');
          Add('PROVIDE ( r_lc_start_oob = 0x4001fffc );');
          Add('PROVIDE ( r_lc_start_passkey = 0x4001feac );');
          Add('PROVIDE ( r_lc_start_passkey_loop = 0x4001ff88 );');
          Add('PROVIDE ( r_lc_stop_afh_report = 0x40020184 );');
          Add('PROVIDE ( r_lc_stop_enc = 0x40020110 );');
          Add('PROVIDE ( r_lc_switch_cmp = 0x40020448 );');
          Add('PROVIDE ( r_lc_unit_key_svr = 0x400206d8 );');
          Add('PROVIDE ( r_lc_unsniff = 0x40020c50 );');
          Add('PROVIDE ( r_lc_unsniff_cmp = 0x40020810 );');
          Add('PROVIDE ( r_lc_unsniff_cont = 0x40020750 );');
          Add('PROVIDE ( r_lc_upd_to = 0x4002065c );');
          Add('PROVIDE ( r_lc_util_convert_pref_rate_to_packet_type = 0x4002f9b0 );');
          Add('PROVIDE ( r_lc_util_get_max_packet_size = 0x4002f4ac );');
          Add('PROVIDE ( r_lc_util_get_offset_clke = 0x4002f538 );');
          Add('PROVIDE ( r_lc_util_get_offset_clkn = 0x4002f51c );');
          Add('PROVIDE ( r_lc_util_set_loc_trans_coll = 0x4002f500 );');
          Add('PROVIDE ( r_lc_version = 0x40020a30 );');
          Add('PROVIDE ( lc_set_encap_pdu_data_p192 = 0x4002e4c8 );');
          Add('PROVIDE ( lc_set_encap_pdu_data_p256 = 0x4002e454 );');
          Add('PROVIDE ( lm_get_auth_method = 0x40023420);');
          Add('PROVIDE ( lmp_accepted_ext_handler = 0x40027290 );');
          Add('PROVIDE ( lmp_not_accepted_ext_handler = 0x40029c54 );');
          Add('PROVIDE ( lmp_clk_adj_handler = 0x40027468 );');
          Add('PROVIDE ( lmp_clk_adj_ack_handler = 0x400274f4 );');
          Add('PROVIDE ( lm_get_auth_method = 0x40023420);');
          Add('PROVIDE ( lmp_accepted_ext_handler = 0x40027290 );');
          Add('PROVIDE ( lmp_not_accepted_ext_handler = 0x40029c54 );');
          Add('PROVIDE ( lmp_clk_adj_handler = 0x40027468 );');
          Add('PROVIDE ( lmp_clk_adj_ack_handler = 0x400274f4 );');
          Add('PROVIDE ( lmp_clk_adj_req_handler = 0x4002751c );');
          Add('PROVIDE ( lmp_feats_res_ext_handler = 0x4002cac4 );');
          Add('PROVIDE ( lmp_feats_req_ext_handler = 0x4002ccb0 );');
          Add('PROVIDE ( lmp_pkt_type_tbl_req_handler = 0x40027574 );');
          Add('PROVIDE ( lmp_esco_link_req_handler = 0x40027610 );');
          Add('PROVIDE ( lmp_rmv_esco_link_req_handler = 0x400276e8 );');
          Add('PROVIDE ( lmp_ch_class_req_handler = 0x40027730 );');
          Add('PROVIDE ( lmp_ch_class_handler = 0x4002ca18 );');
          Add('PROVIDE ( lmp_ssr_req_handler = 0x4002780c );');
          Add('PROVIDE ( lmp_ssr_res_handler = 0x40027900 );');
          Add('PROVIDE ( lmp_pause_enc_aes_req_handler = 0x400279a4 );');
          Add('PROVIDE ( lmp_pause_enc_req_handler = 0x4002df90 );');
          Add('PROVIDE ( lmp_resume_enc_req_handler = 0x4002e084 );');
          Add('PROVIDE ( lmp_num_comparison_fail_handler = 0x40027a74 );');
          Add('PROVIDE ( lmp_passkey_fail_handler = 0x40027aec );');
          Add('PROVIDE ( lmp_keypress_notif_handler = 0x4002c5c8 );');
          Add('PROVIDE ( lmp_pwr_ctrl_req_handler = 0x400263bc );');
          Add('PROVIDE ( lmp_pwr_ctrl_res_handler = 0x40026480 );');
          Add('PROVIDE ( lmp_auto_rate_handler = 0x40026548 );');
          Add('PROVIDE ( lmp_pref_rate_handler = 0x4002657c );');
          Add('PROVIDE ( lmp_name_req_handler = 0x40025050 );');
          Add('PROVIDE ( lmp_name_res_handler = 0x400250bc );');
          Add('PROVIDE ( lmp_not_accepted_handler = 0x400251d0 );');
          Add('PROVIDE ( lmp_accepted_handler = 0x4002e894 );');
          Add('PROVIDE ( lmp_clk_off_req_handler = 0x40025a44 );');
          Add('PROVIDE ( lmp_clk_off_res_handler = 0x40025ab8 );');
          Add('PROVIDE ( lmp_detach_handler = 0x40025b74 );');
          Add('PROVIDE ( lmp_tempkey_handler = 0x4002b6b0 );');
          Add('PROVIDE ( lmp_temprand_handler = 0x4002b74c );');
          Add('PROVIDE ( lmp_sres_handler = 0x4002b840 );');
          Add('PROVIDE ( lmp_aurand_handler = 0x4002bda0 );');
          Add('PROVIDE ( lmp_unitkey_handler = 0x4002c13c );');
          Add('PROVIDE ( lmp_combkey_handler = 0x4002c234 );');
          Add('PROVIDE ( lmp_inrand_handler = 0x4002c414 );');
          Add('PROVIDE ( lmp_oob_fail_handler = 0x40027b84 );');
          Add('PROVIDE ( lmp_ping_req_handler = 0x40027c08 );');
          Add('PROVIDE ( lmp_ping_res_handler = 0x40027c5c );');
          Add('PROVIDE ( lmp_enc_mode_req_handler = 0x40025c60 );');
          Add('PROVIDE ( lmp_enc_key_size_req_handler = 0x40025e54 );');
          Add('PROVIDE ( lmp_switch_req_handler = 0x40025f84 );');
          Add('PROVIDE ( lmp_start_enc_req_handler = 0x4002e124 );');
          Add('PROVIDE ( lmp_stop_enc_req_handler = 0x4002de30 );');
          Add('PROVIDE ( lmp_sniff_req_handler = 0x400260c8 );');
          Add('PROVIDE ( lmp_unsniff_req_handler = 0x400261e0 );');
          Add('PROVIDE ( lmp_incr_pwr_req_handler = 0x4002629c );');
          Add('PROVIDE ( lmp_decr_pwr_req_handler = 0x400262f8 );');
          Add('PROVIDE ( lmp_max_pwr_handler = 0x40026354 );');
          Add('PROVIDE ( lmp_min_pwr_handler = 0x40026388 );');
          Add('PROVIDE ( lmp_ver_req_handler = 0x400265f0 );');
          Add('PROVIDE ( lmp_ver_res_handler = 0x40026670 );');
          Add('PROVIDE ( lmp_qos_handler = 0x40026790 );');
          Add('PROVIDE ( lmp_qos_req_handler = 0x40026844 );');
          Add('PROVIDE ( lmp_sco_link_req_handler = 0x40026930 );');
          Add('PROVIDE ( lmp_rmv_sco_link_req_handler = 0x40026a10 );');
          Add('PROVIDE ( lmp_max_slot_handler = 0x40026a54 );');
          Add('PROVIDE ( lmp_max_slot_req_handler = 0x40026aac );');
          Add('PROVIDE ( lmp_timing_accu_req_handler = 0x40026b54 );');
          Add('PROVIDE ( lmp_timing_accu_res_handler = 0x40026bcc );');
          Add('PROVIDE ( lmp_setup_cmp_handler = 0x40026c84 );');
          Add('PROVIDE ( lmp_feats_res_handler = 0x4002b548 );');
          Add('PROVIDE ( lmp_feats_req_handler = 0x4002b620 );');
          Add('PROVIDE ( lmp_host_con_req_handler = 0x4002b3d8 );');
          Add('PROVIDE ( lmp_use_semi_perm_key_handler = 0x4002b4c4 );');
          Add('PROVIDE ( lmp_slot_off_handler = 0x40026cc8 );');
          Add('PROVIDE ( lmp_page_mode_req_handler = 0x40026d0c );');
          Add('PROVIDE ( lmp_page_scan_mode_req_handler = 0x40026d4c );');
          Add('PROVIDE ( lmp_supv_to_handler = 0x40026d94 );');
          Add('PROVIDE ( lmp_test_activate_handler = 0x40026e7c );');
          Add('PROVIDE ( lmp_test_ctrl_handler = 0x40026ee4 );');
          Add('PROVIDE ( lmp_enc_key_size_mask_req_handler = 0x40027038 );');
          Add('PROVIDE ( lmp_enc_key_size_mask_res_handler = 0x400270a4 );');
          Add('PROVIDE ( lmp_set_afh_handler = 0x4002b2e4 );');
          Add('PROVIDE ( lmp_encaps_hdr_handler = 0x40027120 );');
          Add('PROVIDE ( lmp_encaps_payl_handler = 0x4002e590 );');
          Add('PROVIDE ( lmp_sp_nb_handler = 0x4002acf0 );');
          Add('PROVIDE ( lmp_sp_cfm_handler = 0x4002b170 );');
          Add('PROVIDE ( lmp_dhkey_chk_handler = 0x4002ab48 );');
          Add('PROVIDE ( lmp_pause_enc_aes_req_handler = 0x400279a4 );');
          Add('PROVIDE ( lmp_io_cap_res_handler = 0x4002c670 );');
          Add('PROVIDE ( lmp_io_cap_req_handler = 0x4002c7a4 );');
          Add('PROVIDE ( lc_cmd_cmp_bd_addr_send = 0x4002cec4 );');
          Add('PROVIDE ( ld_acl_tx_packet_type_select = 0x4002fb40 );');
          Add('PROVIDE ( ld_acl_sched = 0x40033268 );');
          Add('PROVIDE ( ld_acl_sniff_sched = 0x4003340c );');
          Add('PROVIDE ( ld_acl_rx = 0x4003274c );');
          Add('PROVIDE ( ld_acl_tx = 0x4002ffdc );');
          Add('PROVIDE ( ld_acl_rx_sync = 0x4002fbec );');
          Add('PROVIDE ( ld_acl_rx_sync2 = 0x4002fd8c );');
          Add('PROVIDE ( ld_acl_rx_no_sync = 0x4002fe78 );');
          Add('PROVIDE ( ld_acl_clk_isr = 0x40030cf8 );');
          Add('PROVIDE ( ld_acl_rsw_frm_cbk = 0x40033bb0 );');
          Add('PROVIDE ( ld_sco_modify = 0x40031778 );');
          Add('PROVIDE ( lm_cmd_cmp_send = 0x40051838 );');
          Add('PROVIDE ( ld_sco_frm_cbk = 0x400349dc );');
          Add('PROVIDE ( ld_acl_sniff_frm_cbk = 0x4003482c );');
          Add('PROVIDE ( ld_inq_end = 0x4003ab48 );');
          Add('PROVIDE ( ld_inq_sched = 0x4003aba4 );');
          Add('PROVIDE ( ld_inq_frm_cbk = 0x4003ae4c );');
          Add('PROVIDE ( r_ld_acl_active_hop_types_get = 0x40036e10 );');
          Add('PROVIDE ( r_ld_acl_afh_confirm = 0x40036d40 );');
          Add('PROVIDE ( r_ld_acl_afh_prepare = 0x40036c84 );');
          Add('PROVIDE ( r_ld_acl_afh_set = 0x40036b60 );');
          Add('PROVIDE ( r_ld_acl_allowed_tx_packet_types_set = 0x40036810 );');
          Add('PROVIDE ( r_ld_acl_bcst_rx_dec = 0x40036394 );');
          Add('PROVIDE ( r_ld_acl_bit_off_get = 0x40036b18 );');
          Add('PROVIDE ( r_ld_acl_clk_adj_set = 0x40036a00 );');
          Add('PROVIDE ( r_ld_acl_clk_off_get = 0x40036b00 );');
          Add('PROVIDE ( r_ld_acl_clk_set = 0x40036950 );');
          Add('PROVIDE ( r_ld_acl_clock_offset_get = 0x400364c0 );');
          Add('PROVIDE ( r_ld_acl_current_tx_power_get = 0x400368f0 );');
          Add('PROVIDE ( r_ld_acl_data_flush = 0x400357bc );');
          Add('PROVIDE ( r_ld_acl_data_tx = 0x4003544c );');
          Add('PROVIDE ( r_ld_acl_edr_set = 0x4003678c );');
          Add('PROVIDE ( r_ld_acl_enc_key_load = 0x40036404 );');
          Add('PROVIDE ( r_ld_acl_flow_off = 0x40035400 );');
          Add('PROVIDE ( r_ld_acl_flow_on = 0x4003541c );');
          Add('PROVIDE ( r_ld_acl_flush_timeout_get = 0x40035f9c );');
          Add('PROVIDE ( r_ld_acl_flush_timeout_set = 0x40035fe0 );');
          Add('PROVIDE ( r_ld_acl_init = 0x40034d08 );');
          Add('PROVIDE ( r_ld_acl_lmp_flush = 0x40035d80 );');
          Add('PROVIDE ( r_ld_acl_lmp_tx = 0x40035b34 );');
          Add('PROVIDE ( r_ld_acl_lsto_get = 0x400366b4 );');
          Add('PROVIDE ( r_ld_acl_lsto_set = 0x400366f8 );');
          Add('PROVIDE ( r_ld_acl_reset = 0x40034d24 );');
          Add('PROVIDE ( r_ld_acl_role_get = 0x40036b30 );');
          Add('PROVIDE ( r_ld_acl_rssi_delta_get = 0x40037028 );');
          Add('PROVIDE ( r_ld_acl_rsw_req = 0x40035e74 );');
          Add('PROVIDE ( r_ld_acl_rx_enc = 0x40036344 );');
          Add('PROVIDE ( r_ld_acl_rx_max_slot_get = 0x40036e58 );');
          Add('PROVIDE ( r_ld_acl_rx_max_slot_set = 0x40036ea0 );');
          Add('PROVIDE ( r_ld_acl_slot_offset_get = 0x4003653c );');
          Add('PROVIDE ( r_ld_acl_slot_offset_set = 0x40036658 );');
          Add('PROVIDE ( r_ld_acl_sniff = 0x4003617c );');
          Add('PROVIDE ( r_ld_acl_sniff_trans = 0x400360a8 );');
          Add('PROVIDE ( r_ld_acl_ssr_set = 0x40036274 );');
          Add('PROVIDE ( r_ld_acl_start = 0x40034ddc );');
          Add('PROVIDE ( r_ld_acl_stop = 0x4003532c );');
          Add('PROVIDE ( r_ld_acl_test_mode_set = 0x40036f24 );');
          Add('PROVIDE ( r_ld_acl_timing_accuracy_set = 0x4003673c );');
          Add('PROVIDE ( r_ld_acl_t_poll_get = 0x40036024 );');
          Add('PROVIDE ( r_ld_acl_t_poll_set = 0x40036068 );');
          Add('PROVIDE ( r_ld_acl_tx_enc = 0x400362f8 );');
          Add('PROVIDE ( r_ld_acl_unsniff = 0x400361e0 );');
          Add('PROVIDE ( r_ld_active_check = 0x4003cac4 );');
          Add('PROVIDE ( r_ld_afh_ch_assess_data_get = 0x4003caec );');
          Add('PROVIDE ( r_ld_bcst_acl_data_tx = 0x40038d3c );');
          Add('PROVIDE ( r_ld_bcst_acl_init = 0x40038bd0 );');
          Add('PROVIDE ( r_ld_bcst_acl_reset = 0x40038bdc );');
          Add('PROVIDE ( r_ld_bcst_acl_start = 0x4003882c );');
          Add('PROVIDE ( r_ld_bcst_afh_update = 0x40038f3c );');
          Add('PROVIDE ( r_ld_bcst_enc_key_load = 0x4003906c );');
          Add('PROVIDE ( r_ld_bcst_lmp_tx = 0x40038bf8 );');
          Add('PROVIDE ( r_ld_bcst_tx_enc = 0x40038ff8 );');
          Add('PROVIDE ( r_ld_bd_addr_get = 0x4003ca20 );');
          Add('PROVIDE ( r_ld_channel_assess = 0x4003c184 );');
          Add('PROVIDE ( r_ld_class_of_dev_get = 0x4003ca34 );');
          Add('PROVIDE ( r_ld_class_of_dev_set = 0x4003ca50 );');
          Add('PROVIDE ( r_ld_csb_rx_afh_update = 0x40039af4 );');
          Add('PROVIDE ( r_ld_csb_rx_init = 0x40039690 );');
          Add('PROVIDE ( r_ld_csb_rx_reset = 0x4003969c );');
          Add('PROVIDE ( r_ld_csb_rx_start = 0x4003972c );');
          Add('PROVIDE ( r_ld_csb_rx_stop = 0x40039bb8 );');
          Add('PROVIDE ( r_ld_csb_tx_afh_update = 0x4003a5fc );');
          Add('PROVIDE ( r_ld_csb_tx_clr_data = 0x4003a71c );');
          Add('PROVIDE ( r_ld_csb_tx_dis = 0x4003a5e8 );');
          Add('PROVIDE ( r_ld_csb_tx_en = 0x4003a1c0 );');
          Add('PROVIDE ( r_ld_csb_tx_init = 0x4003a0e8 );');
          Add('PROVIDE ( r_ld_csb_tx_reset = 0x4003a0f8 );');
          Add('PROVIDE ( r_ld_csb_tx_set_data = 0x4003a6c0 );');
          Add('PROVIDE ( r_ld_fm_clk_isr = 0x4003a7a8 );');
          Add('PROVIDE ( r_ld_fm_frame_isr = 0x4003a82c );');
          Add('PROVIDE ( r_ld_fm_init = 0x4003a760 );');
          Add('PROVIDE ( r_ld_fm_prog_check = 0x4003ab28 );');
          Add('PROVIDE ( r_ld_fm_prog_disable = 0x4003a984 );');
          Add('PROVIDE ( r_ld_fm_prog_enable = 0x4003a944 );');
          Add('PROVIDE ( r_ld_fm_prog_push = 0x4003a9d4 );');
          Add('PROVIDE ( r_ld_fm_reset = 0x4003a794 );');
          Add('PROVIDE ( r_ld_fm_rx_isr = 0x4003a7f4 );');
          Add('PROVIDE ( r_ld_fm_sket_isr = 0x4003a8a4 );');
          Add('PROVIDE ( r_ld_init = 0x4003c294 );');
          Add('PROVIDE ( r_ld_inq_init = 0x4003b15c );');
          Add('PROVIDE ( r_ld_inq_reset = 0x4003b168 );');
          Add('PROVIDE ( r_ld_inq_start = 0x4003b1f0 );');
          Add('PROVIDE ( r_ld_inq_stop = 0x4003b4f0 );');
          Add('PROVIDE ( r_ld_iscan_eir_get = 0x4003c118 );');
          Add('PROVIDE ( r_ld_iscan_eir_set = 0x4003bfa0 );');
          Add('PROVIDE ( r_ld_iscan_init = 0x4003b9f0 );');
          Add('PROVIDE ( r_ld_iscan_reset = 0x4003ba14 );');
          Add('PROVIDE ( r_ld_iscan_restart = 0x4003ba44 );');
          Add('PROVIDE ( r_ld_iscan_start = 0x4003bb28 );');
          Add('PROVIDE ( r_ld_iscan_stop = 0x4003bf1c );');
          Add('PROVIDE ( r_ld_iscan_tx_pwr_get = 0x4003c138 );');
          Add('PROVIDE ( r_ld_page_init = 0x4003d808 );');
          Add('PROVIDE ( r_ld_page_reset = 0x4003d814 );');
          Add('PROVIDE ( r_ld_page_start = 0x4003d848 );');
          Add('PROVIDE ( r_ld_page_stop = 0x4003da54 );');
          Add('PROVIDE ( r_ld_pca_coarse_clock_adjust = 0x4003e324 );');
          Add('PROVIDE ( r_ld_pca_init = 0x4003deb4 );');
          Add('PROVIDE ( r_ld_pca_initiate_clock_dragging = 0x4003e4ac );');
          Add('PROVIDE ( r_ld_pca_local_config = 0x4003df6c );');
          Add('PROVIDE ( r_ld_pca_mws_frame_sync = 0x4003e104 );');
          Add('PROVIDE ( r_ld_pca_mws_moment_offset_gt = 0x4003e278 );');
          Add('PROVIDE ( r_ld_pca_mws_moment_offset_lt = 0x4003e280 );');
          Add('PROVIDE ( r_ld_pca_reporting_enable = 0x4003e018 );');
          Add('PROVIDE ( r_ld_pca_reset = 0x4003df0c );');
          Add('PROVIDE ( r_ld_pca_update_target_offset = 0x4003e050 );');
          Add('PROVIDE ( r_ld_pscan_evt_handler = 0x4003f238 );');
          Add('PROVIDE ( r_ld_pscan_init = 0x4003f474 );');
          Add('PROVIDE ( r_ld_pscan_reset = 0x4003f498 );');
          Add('PROVIDE ( r_ld_pscan_restart = 0x4003f4b8 );');
          Add('PROVIDE ( r_ld_pscan_start = 0x4003f514 );');
          Add('PROVIDE ( r_ld_pscan_stop = 0x4003f618 );');
          Add('PROVIDE ( r_ld_read_clock = 0x4003c9e4 );');
          Add('PROVIDE ( r_ld_reset = 0x4003c714 );');
          Add('PROVIDE ( r_ld_sched_acl_add = 0x4003f978 );');
          Add('PROVIDE ( r_ld_sched_acl_remove = 0x4003f99c );');
          Add('PROVIDE ( r_ld_sched_compute = 0x4003f6f8 );');
          Add('PROVIDE ( r_ld_sched_init = 0x4003f7ac );');
          Add('PROVIDE ( r_ld_sched_inq_add = 0x4003f8a8 );');
          Add('PROVIDE ( r_ld_sched_inq_remove = 0x4003f8d0 );');
          Add('PROVIDE ( r_ld_sched_iscan_add = 0x4003f7e8 );');
          Add('PROVIDE ( r_ld_sched_iscan_remove = 0x4003f808 );');
          Add('PROVIDE ( r_ld_sched_page_add = 0x4003f910 );');
          Add('PROVIDE ( r_ld_sched_page_remove = 0x4003f938 );');
          Add('PROVIDE ( r_ld_sched_pscan_add = 0x4003f828 );');
          Add('PROVIDE ( r_ld_sched_pscan_remove = 0x4003f848 );');
          Add('PROVIDE ( r_ld_sched_reset = 0x4003f7d4 );');
          Add('PROVIDE ( r_ld_sched_sco_add = 0x4003fa4c );');
          Add('PROVIDE ( r_ld_sched_sco_remove = 0x4003fa9c );');
          Add('PROVIDE ( r_ld_sched_sniff_add = 0x4003f9c4 );');
          Add('PROVIDE ( r_ld_sched_sniff_remove = 0x4003fa0c );');
          Add('PROVIDE ( r_ld_sched_sscan_add = 0x4003f868 );');
          Add('PROVIDE ( r_ld_sched_sscan_remove = 0x4003f888 );');
          Add('PROVIDE ( r_ld_sco_audio_isr = 0x40037cc8 );');
          Add('PROVIDE ( r_ld_sco_data_tx = 0x40037ee8 );');
          Add('PROVIDE ( r_ld_sco_start = 0x40037110 );');
          Add('PROVIDE ( r_ld_sco_stop = 0x40037c40 );');
          Add('PROVIDE ( r_ld_sco_update = 0x40037a74 );');
          Add('PROVIDE ( r_ld_sscan_activated = 0x4004031c );');
          Add('PROVIDE ( r_ld_sscan_init = 0x400402f0 );');
          Add('PROVIDE ( r_ld_sscan_reset = 0x400402fc );');
          Add('PROVIDE ( r_ld_sscan_start = 0x40040384 );');
          Add('PROVIDE ( r_ld_strain_init = 0x400409f4 );');
          Add('PROVIDE ( r_ld_strain_reset = 0x40040a00 );');
          Add('PROVIDE ( r_ld_strain_start = 0x40040a8c );');
          Add('PROVIDE ( r_ld_strain_stop = 0x40040df0 );');
          Add('PROVIDE ( r_ld_timing_accuracy_get = 0x4003caac );');
          Add('PROVIDE ( r_ld_util_active_master_afh_map_get = 0x4004131c );');
          Add('PROVIDE ( r_ld_util_active_master_afh_map_set = 0x40041308 );');
          Add('PROVIDE ( r_ld_util_bch_create = 0x40040fcc );');
          Add('PROVIDE ( r_ld_util_fhs_pk = 0x400411c8 );');
          Add('PROVIDE ( r_ld_util_fhs_unpk = 0x40040e54 );');
          Add('PROVIDE ( r_ld_util_stp_pk = 0x400413f4 );');
          Add('PROVIDE ( r_ld_util_stp_unpk = 0x40041324 );');
          Add('PROVIDE ( r_ld_version_get = 0x4003ca6c );');
          Add('PROVIDE ( r_ld_wlcoex_set = 0x4003caf8 );');
          Add('PROVIDE ( r_llc_ch_assess_get_current_ch_map = 0x40041574 );');
          Add('PROVIDE ( r_llc_ch_assess_get_local_ch_map = 0x4004150c );');
          Add('PROVIDE ( r_llc_ch_assess_local = 0x40041494 );');
          Add('PROVIDE ( r_llc_ch_assess_merge_ch = 0x40041588 );');
          Add('PROVIDE ( r_llc_ch_assess_reass_ch = 0x400415c0 );');
          Add('PROVIDE ( r_llc_common_cmd_complete_send = 0x40044eac );');
          Add('PROVIDE ( r_llc_common_cmd_status_send = 0x40044ee0 );');
          Add('PROVIDE ( r_llc_common_enc_change_evt_send = 0x40044f6c );');
          Add('PROVIDE ( r_llc_common_enc_key_ref_comp_evt_send = 0x40044f38 );');
          Add('PROVIDE ( r_llc_common_flush_occurred_send = 0x40044f0c );');
          Add('PROVIDE ( r_llc_common_nb_of_pkt_comp_evt_send = 0x40045000 );');
          Add('PROVIDE ( r_llc_con_update_complete_send = 0x40044d68 );');
          Add('PROVIDE ( r_llc_con_update_finished = 0x4004518c );');
          Add('PROVIDE ( r_llc_con_update_ind = 0x40045038 );');
          Add('PROVIDE ( r_llc_discon_event_complete_send = 0x40044a30 );');
          Add('PROVIDE ( r_llc_end_evt_defer = 0x40046330 );');
          Add('PROVIDE ( r_llc_feats_rd_event_send = 0x40044e0c );');
          Add('PROVIDE ( r_llc_init = 0x40044778 );');
          Add('PROVIDE ( r_llc_le_con_cmp_evt_send = 0x40044a78 );');
          Add('PROVIDE ( r_llc_llcp_ch_map_update_pdu_send = 0x40043f94 );');
          Add('PROVIDE ( r_llc_llcp_con_param_req_pdu_send = 0x400442fc );');
          Add('PROVIDE ( r_llc_llcp_con_param_rsp_pdu_send = 0x40044358 );');
          Add('PROVIDE ( r_llc_llcp_con_update_pdu_send = 0x400442c4 );');
          Add('PROVIDE ( r_llc_llcp_enc_req_pdu_send = 0x40044064 );');
          Add('PROVIDE ( r_llc_llcp_enc_rsp_pdu_send = 0x40044160 );');
          Add('PROVIDE ( r_llc_llcp_feats_req_pdu_send = 0x400443b4 );');
          Add('PROVIDE ( r_llc_llcp_feats_rsp_pdu_send = 0x400443f0 );');
          Add('PROVIDE ( r_llc_llcp_get_autorize = 0x4004475c );');
          Add('PROVIDE ( r_llc_llcp_length_req_pdu_send = 0x40044574 );');
          Add('PROVIDE ( r_llc_llcp_length_rsp_pdu_send = 0x400445ac );');
          Add('PROVIDE ( r_llc_llcp_pause_enc_req_pdu_send = 0x40043fd8 );');
          Add('PROVIDE ( r_llc_llcp_pause_enc_rsp_pdu_send = 0x40044010 );');
          Add('PROVIDE ( r_llc_llcp_ping_req_pdu_send = 0x4004454c );');
          Add('PROVIDE ( r_llc_llcp_ping_rsp_pdu_send = 0x40044560 );');
          Add('PROVIDE ( r_llc_llcp_recv_handler = 0x40044678 );');
          Add('PROVIDE ( r_llc_llcp_reject_ind_pdu_send = 0x4004425c );');
          Add('PROVIDE ( r_llc_llcp_start_enc_req_pdu_send = 0x4004441c );');
          Add('PROVIDE ( r_llc_llcp_start_enc_rsp_pdu_send = 0x400441f8 );');
          Add('PROVIDE ( r_llc_llcp_terminate_ind_pdu_send = 0x400444b0 );');
          Add('PROVIDE ( r_llc_llcp_tester_send = 0x400445e4 );');
          Add('PROVIDE ( r_llc_llcp_unknown_rsp_send_pdu = 0x40044534 );');
          Add('PROVIDE ( r_llc_llcp_version_ind_pdu_send = 0x40043f6c );');
          Add('PROVIDE ( r_llc_lsto_con_update = 0x40045098 );');
          Add('PROVIDE ( r_llc_ltk_req_send = 0x40044dc0 );');
          Add('PROVIDE ( r_llc_map_update_finished = 0x40045260 );');
          Add('PROVIDE ( r_llc_map_update_ind = 0x400450f0 );');
          Add('PROVIDE ( r_llc_pdu_acl_tx_ack_defer = 0x400464dc );');
          Add('PROVIDE ( r_llc_pdu_defer = 0x40046528 );');
          Add('PROVIDE ( r_llc_pdu_llcp_tx_ack_defer = 0x400463ac );');
          Add('PROVIDE ( r_llc_reset = 0x400447b8 );');
          Add('PROVIDE ( r_llc_start = 0x400447f4 );');
          Add('PROVIDE ( r_llc_stop = 0x400449ac );');
          Add('PROVIDE ( r_llc_util_bw_mgt = 0x4004629c );');
          Add('PROVIDE ( r_llc_util_clear_operation_ptr = 0x40046234 );');
          Add('PROVIDE ( r_llc_util_dicon_procedure = 0x40046130 );');
          Add('PROVIDE ( r_llc_util_get_free_conhdl = 0x400460c8 );');
          Add('PROVIDE ( r_llc_util_get_nb_active_link = 0x40046100 );');
          Add('PROVIDE ( r_llc_util_set_auth_payl_to_margin = 0x400461f4 );');
          Add('PROVIDE ( r_llc_util_set_llcp_discard_enable = 0x400461c8 );');
          Add('PROVIDE ( r_llc_util_update_channel_map = 0x400461ac );');
          Add('PROVIDE ( r_llc_version_rd_event_send = 0x40044e60 );');
          Add('PROVIDE ( r_lld_adv_start = 0x40048b38 );');
          Add('PROVIDE ( r_lld_adv_stop = 0x40048ea0 );');
          Add('PROVIDE ( r_lld_ch_map_ind = 0x4004a2f4 );');
          Add('PROVIDE ( r_lld_con_param_req = 0x40049f0c );');
          Add('PROVIDE ( r_lld_con_param_rsp = 0x40049e00 );');
          Add('PROVIDE ( r_lld_con_start = 0x400491f8 );');
          Add('PROVIDE ( r_lld_con_stop = 0x40049fdc );');
          Add('PROVIDE ( r_lld_con_update_after_param_req = 0x40049bcc );');
          Add('PROVIDE ( r_lld_con_update_ind = 0x4004a30c );');
          Add('PROVIDE ( r_lld_con_update_req = 0x40049b60 );');
          Add('PROVIDE ( r_lld_core_reset = 0x40048a9c );');
          Add('PROVIDE ( r_lld_crypt_isr = 0x4004a324 );');
          Add('PROVIDE ( r_lld_evt_adv_create = 0x400481f4 );');
          Add('PROVIDE ( r_lld_evt_canceled = 0x400485c8 );');
          Add('PROVIDE ( r_lld_evt_channel_next = 0x40046aac );');
          Add('PROVIDE ( r_lld_evt_deffered_elt_handler = 0x400482bc );');
          Add('PROVIDE ( r_lld_evt_delete_elt_handler = 0x40046974 );');
          Add('PROVIDE ( r_lld_evt_delete_elt_push = 0x40046a3c );');
          Add('PROVIDE ( r_lld_evt_drift_compute = 0x40047670 );');
          Add('PROVIDE ( r_lld_evt_elt_delete = 0x40047538 );');
          Add('PROVIDE ( r_lld_evt_elt_insert = 0x400474c8 );');
          Add('PROVIDE ( r_lld_evt_end = 0x400483e8 );');
          Add('PROVIDE ( r_lld_evt_end_isr = 0x4004862c );');
          Add('PROVIDE ( r_lld_evt_init = 0x40046b3c );');
          Add('PROVIDE ( r_lld_evt_init_evt = 0x40046cd0 );');
          Add('PROVIDE ( r_lld_evt_move_to_master = 0x40047ba0 );');
          Add('PROVIDE ( r_lld_evt_move_to_slave = 0x40047e18 );');
          Add('PROVIDE ( r_lld_evt_prevent_stop = 0x40047adc );');
          Add('PROVIDE ( r_lld_evt_restart = 0x40046d50 );');
          Add('PROVIDE ( r_lld_evt_rx = 0x40048578 );');
          Add('PROVIDE ( r_lld_evt_rx_isr = 0x40048678 );');
          Add('PROVIDE ( r_lld_evt_scan_create = 0x40047ae8 );');
          Add('PROVIDE ( r_lld_evt_schedule = 0x40047908 );');
          Add('PROVIDE ( r_lld_evt_schedule_next = 0x400477dc );');
          Add('PROVIDE ( r_lld_evt_schedule_next_instant = 0x400476a8 );');
          Add('PROVIDE ( r_lld_evt_slave_update = 0x40048138 );');
          Add('PROVIDE ( r_lld_evt_update_create = 0x40047cd8 );');
          Add('PROVIDE ( r_lld_get_mode = 0x40049ff8 );');
          Add('PROVIDE ( r_lld_init = 0x4004873c );');
          Add('PROVIDE ( r_lld_move_to_master = 0x400499e0 );');
          Add('PROVIDE ( r_lld_move_to_slave = 0x4004a024 );');
          Add('PROVIDE ( r_lld_pdu_adv_pack = 0x4004b488 );');
          Add('PROVIDE ( r_lld_pdu_check = 0x4004ac34 );');
          Add('PROVIDE ( r_lld_pdu_data_send = 0x4004b018 );');
          Add('PROVIDE ( r_lld_pdu_data_tx_push = 0x4004aecc );');
          Add('PROVIDE ( r_lld_pdu_rx_handler = 0x4004b4d4 );');
          Add('PROVIDE ( r_lld_pdu_send_packet = 0x4004b774 );');
          Add('PROVIDE ( r_lld_pdu_tx_flush = 0x4004b414 );');
          Add('PROVIDE ( r_lld_pdu_tx_loop = 0x4004ae40 );');
          Add('PROVIDE ( r_lld_pdu_tx_prog = 0x4004b120 );');
          Add('PROVIDE ( r_lld_pdu_tx_push = 0x4004b080 );');
          Add('PROVIDE ( r_lld_ral_renew_req = 0x4004a73c );');
          Add('PROVIDE ( r_lld_scan_start = 0x40048ee0 );');
          Add('PROVIDE ( r_lld_scan_stop = 0x40049190 );');
          Add('PROVIDE ( r_lld_test_mode_rx = 0x4004a540 );');
          Add('PROVIDE ( r_lld_test_mode_tx = 0x4004a350 );');
          Add('PROVIDE ( r_lld_test_stop = 0x4004a710 );');
          Add('PROVIDE ( r_lld_util_anchor_point_move = 0x4004bacc );');
          Add('PROVIDE ( r_lld_util_compute_ce_max = 0x4004bc0c );');
          Add('PROVIDE ( r_lld_util_connection_param_set = 0x4004ba40 );');
          Add('PROVIDE ( r_lld_util_dle_set_cs_fields = 0x4004ba90 );');
          Add('PROVIDE ( r_lld_util_eff_tx_time_set = 0x4004bd88 );');
          Add('PROVIDE ( r_lld_util_elt_programmed = 0x4004bce0 );');
          Add('PROVIDE ( r_lld_util_flush_list = 0x4004bbd8 );');
          Add('PROVIDE ( r_lld_util_freq2chnl = 0x4004b9e4 );');
          Add('PROVIDE ( r_lld_util_get_bd_address = 0x4004b8ac );');
          Add('PROVIDE ( r_lld_util_get_local_offset = 0x4004ba10 );');
          Add('PROVIDE ( r_lld_util_get_peer_offset = 0x4004ba24 );');
          Add('PROVIDE ( r_lld_util_get_tx_pkt_cnt = 0x4004bd80 );');
          Add('PROVIDE ( r_lld_util_instant_get = 0x4004b890 );');
          Add('PROVIDE ( r_lld_util_instant_ongoing = 0x4004bbfc );');
          Add('PROVIDE ( r_lld_util_priority_set = 0x4004bd10 );');
          Add('PROVIDE ( r_lld_util_priority_update = 0x4004bd78 );');
          Add('PROVIDE ( r_lld_util_ral_force_rpa_renew = 0x4004b980 );');
          Add('PROVIDE ( r_lld_util_set_bd_address = 0x4004b8f8 );');
          Add('PROVIDE ( r_lld_wlcoex_set = 0x4004bd98 );');
          Add('PROVIDE ( r_llm_ble_ready = 0x4004cc34 );');
          Add('PROVIDE ( r_llm_common_cmd_complete_send = 0x4004d288 );');
          Add('PROVIDE ( r_llm_common_cmd_status_send = 0x4004d2b4 );');
          Add('PROVIDE ( r_llm_con_req_ind = 0x4004cc54 );');
          Add('PROVIDE ( r_llm_con_req_tx_cfm = 0x4004d158 );');
          Add('PROVIDE ( r_llm_create_con = 0x4004de78 );');
          Add('PROVIDE ( r_llm_encryption_done = 0x4004dff8 );');
          Add('PROVIDE ( r_llm_encryption_start = 0x4004e128 );');
          Add('PROVIDE ( r_llm_end_evt_defer = 0x4004eb6c );');
          Add('PROVIDE ( r_llm_init = 0x4004c9f8 );');
          Add('PROVIDE ( r_llm_le_adv_report_ind = 0x4004cdf4 );');
          Add('PROVIDE ( r_llm_pdu_defer = 0x4004ec48 );');
          Add('PROVIDE ( r_llm_ral_clear = 0x4004e1fc );');
          Add('PROVIDE ( r_llm_ral_dev_add = 0x4004e23c );');
          Add('PROVIDE ( r_llm_ral_dev_rm = 0x4004e3bc );');
          Add('PROVIDE ( r_llm_ral_get_rpa = 0x4004e400 );');
          Add('PROVIDE ( r_llm_ral_set_timeout = 0x4004e4a0 );');
          Add('PROVIDE ( r_llm_ral_update = 0x4004e4f8 );');
          Add('PROVIDE ( r_llm_set_adv_data = 0x4004d960 );');
          Add('PROVIDE ( r_llm_set_adv_en = 0x4004d7ec );');
          Add('PROVIDE ( r_llm_set_adv_param = 0x4004d5f4 );');
          Add('PROVIDE ( r_llm_set_scan_en = 0x4004db64 );');
          Add('PROVIDE ( r_llm_set_scan_param = 0x4004dac8 );');
          Add('PROVIDE ( r_llm_set_scan_rsp_data = 0x4004da14 );');
          Add('PROVIDE ( r_llm_test_mode_start_rx = 0x4004d534 );');
          Add('PROVIDE ( r_llm_test_mode_start_tx = 0x4004d2fc );');
          Add('PROVIDE ( r_llm_util_adv_data_update = 0x4004e8fc );');
          Add('PROVIDE ( r_llm_util_apply_bd_addr = 0x4004e868 );');
          Add('PROVIDE ( r_llm_util_bd_addr_in_ral = 0x4004eb08 );');
          Add('PROVIDE ( r_llm_util_bd_addr_in_wl = 0x4004e788 );');
          Add('PROVIDE ( r_llm_util_bd_addr_wl_position = 0x4004e720 );');
          Add('PROVIDE ( r_llm_util_bl_add = 0x4004e9ac );');
          Add('PROVIDE ( r_llm_util_bl_check = 0x4004e930 );');
          Add('PROVIDE ( r_llm_util_bl_rem = 0x4004ea70 );');
          Add('PROVIDE ( r_llm_util_check_address_validity = 0x4004e7e4 );');
          Add('PROVIDE ( r_llm_util_check_evt_mask = 0x4004e8b0 );');
          Add('PROVIDE ( r_llm_util_check_map_validity = 0x4004e800 );');
          Add('PROVIDE ( r_llm_util_get_channel_map = 0x4004e8d4 );');
          Add('PROVIDE ( r_llm_util_get_supp_features = 0x4004e8e8 );');
          Add('PROVIDE ( r_llm_util_set_public_addr = 0x4004e89c );');
          Add('PROVIDE ( r_llm_wl_clr = 0x4004dc54 );');
          Add('PROVIDE ( r_llm_wl_dev_add = 0x4004dcc0 );');
          Add('PROVIDE ( r_llm_wl_dev_add_hdl = 0x4004dd38 );');
          Add('PROVIDE ( r_llm_wl_dev_rem = 0x4004dcfc );');
          Add('PROVIDE ( r_llm_wl_dev_rem_hdl = 0x4004dde0 );');
          Add('PROVIDE ( r_lm_acl_disc = 0x4004f148 );');
          Add('PROVIDE ( r_LM_AddSniff = 0x40022d20 );');
          Add('PROVIDE ( r_lm_add_sync = 0x40051358 );');
          Add('PROVIDE ( r_lm_afh_activate_timer = 0x4004f444 );');
          Add('PROVIDE ( r_lm_afh_ch_ass_en_get = 0x4004f3f8 );');
          Add('PROVIDE ( r_lm_afh_host_ch_class_get = 0x4004f410 );');
          Add('PROVIDE ( r_lm_afh_master_ch_map_get = 0x4004f43c );');
          Add('PROVIDE ( r_lm_afh_peer_ch_class_set = 0x4004f418 );');
          Add('PROVIDE ( r_lm_check_active_sync = 0x40051334 );');
          Add('PROVIDE ( r_LM_CheckEdrFeatureRequest = 0x4002f90c );');
          Add('PROVIDE ( r_LM_CheckSwitchInstant = 0x4002f8c0 );');
          Add('PROVIDE ( r_lm_check_sync_hl_rsp = 0x4005169c );');
          Add('PROVIDE ( r_lm_clk_adj_ack_pending_clear = 0x4004f514 );');
          Add('PROVIDE ( r_lm_clk_adj_instant_pending_set = 0x4004f4d8 );');
          Add('PROVIDE ( r_LM_ComputePacketType = 0x4002f554 );');
          Add('PROVIDE ( r_LM_ComputeSniffSubRate = 0x400233ac );');
          Add('PROVIDE ( r_lm_debug_key_compare_192 = 0x4004f3a8 );');
          Add('PROVIDE ( r_lm_debug_key_compare_256 = 0x4004f3d0 );');
          Add('PROVIDE ( r_lm_dhkey_calc_init = 0x40013234 );');
          Add('PROVIDE ( r_lm_dhkey_compare = 0x400132d8 );');
          Add('PROVIDE ( r_lm_dut_mode_en_get = 0x4004f3ec );');
          Add('PROVIDE ( r_LM_ExtractMaxEncKeySize = 0x4001aca4 );');
          Add('PROVIDE ( r_lm_f1 = 0x40012bb8 );');
          Add('PROVIDE ( r_lm_f2 = 0x40012cfc );');
          Add('PROVIDE ( r_lm_f3 = 0x40013050 );');
          Add('PROVIDE ( r_lm_g = 0x40012f90 );');
          Add('PROVIDE ( r_LM_GetAFHSwitchInstant = 0x4002f86c );');
          Add('PROVIDE ( r_lm_get_auth_en = 0x4004f1ac );');
          Add('PROVIDE ( r_lm_get_common_pkt_types = 0x4002fa1c );');
          Add('PROVIDE ( r_LM_GetConnectionAcceptTimeout = 0x4004f1f4 );');
          Add('PROVIDE ( r_LM_GetFeature = 0x4002f924 );');
          Add('PROVIDE ( r_LM_GetLinkTimeout = 0x400233ec );');
          Add('PROVIDE ( r_LM_GetLocalNameSeg = 0x4004f200 );');
          Add('PROVIDE ( r_lm_get_loopback_mode = 0x4004f248 );');
          Add('PROVIDE ( r_LM_GetMasterEncKeySize = 0x4001b29c );');
          Add('PROVIDE ( r_LM_GetMasterEncRand = 0x4001b288 );');
          Add('PROVIDE ( r_LM_GetMasterKey = 0x4001b260 );');
          Add('PROVIDE ( r_LM_GetMasterKeyRand = 0x4001b274 );');
          Add('PROVIDE ( r_lm_get_min_sync_intv = 0x400517a8 );');
          Add('PROVIDE ( r_lm_get_nb_acl = 0x4004ef9c );');
          Add('PROVIDE ( r_lm_get_nb_sync_link = 0x4005179c );');
          Add('PROVIDE ( r_lm_get_nonce = 0x400131c4 );');
          Add('PROVIDE ( r_lm_get_oob_local_commit = 0x4004f374 );');
          Add('PROVIDE ( r_lm_get_oob_local_data_192 = 0x4004f2d4 );');
          Add('PROVIDE ( r_lm_get_oob_local_data_256 = 0x4004f318 );');
          Add('PROVIDE ( r_LM_GetPINType = 0x4004f1e8 );');
          Add('PROVIDE ( r_lm_get_priv_key_192 = 0x4004f278 );');
          Add('PROVIDE ( r_lm_get_priv_key_256 = 0x4004f2b8 );');
          Add('PROVIDE ( r_lm_get_pub_key_192 = 0x4004f258 );');
          Add('PROVIDE ( r_lm_get_pub_key_256 = 0x4004f298 );');
          Add('PROVIDE ( r_LM_GetQoSParam = 0x4002f6e0 );');
          Add('PROVIDE ( r_lm_get_sec_con_host_supp = 0x4004f1d4 );');
          Add('PROVIDE ( r_LM_GetSniffSubratingParam = 0x4002325c );');
          Add('PROVIDE ( r_lm_get_sp_en = 0x4004f1c0 );');
          Add('PROVIDE ( r_LM_GetSwitchInstant = 0x4002f7f8 );');
          Add('PROVIDE ( r_lm_get_synchdl = 0x4005175c );');
          Add('PROVIDE ( r_lm_get_sync_param = 0x400503b4 );');
          Add('PROVIDE ( r_lm_init = 0x4004ed34 );');
          Add('PROVIDE ( r_lm_init_sync = 0x400512d8 );');
          Add('PROVIDE ( r_lm_is_acl_con = 0x4004f47c );');
          Add('PROVIDE ( r_lm_is_acl_con_role = 0x4004f49c );');
          Add('PROVIDE ( r_lm_is_clk_adj_ack_pending = 0x4004f4e8 );');
          Add('PROVIDE ( r_lm_is_clk_adj_instant_pending = 0x4004f4c8 );');
          Add('PROVIDE ( r_lm_local_ext_fr_configured = 0x4004f540 );');
          Add('PROVIDE ( r_lm_look_for_stored_link_key = 0x4002f948 );');
          Add('PROVIDE ( r_lm_look_for_sync = 0x40051774 );');
          Add('PROVIDE ( r_lm_lt_addr_alloc = 0x4004ef1c );');
          Add('PROVIDE ( r_lm_lt_addr_free = 0x4004ef74 );');
          Add('PROVIDE ( r_lm_lt_addr_reserve = 0x4004ef48 );');
          Add('PROVIDE ( r_LM_MakeCof = 0x4002f84c );');
          Add('PROVIDE ( r_LM_MakeRandVec = 0x400112d8 );');
          Add('PROVIDE ( r_lm_master_clk_adj_req_handler = 0x40054180 );');
          Add('PROVIDE ( r_LM_MaxSlot = 0x4002f694 );');
          Add('PROVIDE ( r_lm_modif_sync = 0x40051578 );');
          Add('PROVIDE ( r_lm_n_is_zero = 0x40012170 );');
          Add('PROVIDE ( r_lm_num_clk_adj_ack_pending_set = 0x4004f500 );');
          Add('PROVIDE ( r_lm_oob_f1 = 0x40012e54 );');
          Add('PROVIDE ( r_lm_pca_sscan_link_get = 0x4004f560 );');
          Add('PROVIDE ( r_lm_pca_sscan_link_set = 0x4004f550 );');
          Add('PROVIDE ( nvds_null_read = 0x400542a0 );');
          Add('PROVIDE ( nvds_null_write = 0x400542a8 );');
          Add('PROVIDE ( nvds_null_erase = 0x400542b0 );');
          Add('PROVIDE ( nvds_read = 0x400542c4 );');
          Add('PROVIDE ( nvds_write = 0x400542fc );');
          Add('PROVIDE ( nvds_erase = 0x40054334 );');
          Add('PROVIDE ( nvds_init_memory = 0x40054358 );');
          Add('PROVIDE ( r_lmp_pack = 0x4001135c );');
          Add('PROVIDE ( r_lmp_unpack = 0x4001149c );');
          Add('PROVIDE ( r_lm_read_features = 0x4004f0d8 );');
          Add('PROVIDE ( r_LM_RemoveSniff = 0x40023124 );');
          Add('PROVIDE ( r_LM_RemoveSniffSubrating = 0x400233c4 );');
          Add('PROVIDE ( r_lm_remove_sync = 0x400517c8 );');
          Add('PROVIDE ( r_lm_reset_sync = 0x40051304 );');
          Add('PROVIDE ( r_lm_role_switch_finished = 0x4004f028 );');
          Add('PROVIDE ( r_lm_role_switch_start = 0x4004efe0 );');
          Add('PROVIDE ( r_lm_sco_nego_end = 0x40051828 );');
          Add('PROVIDE ( r_LM_SniffSubrateNegoRequired = 0x40023334 );');
          Add('PROVIDE ( r_LM_SniffSubratingHlReq = 0x40023154 );');
          Add('PROVIDE ( r_LM_SniffSubratingPeerReq = 0x400231dc );');
          Add('PROVIDE ( r_lm_sp_debug_mode_get = 0x4004f398 );');
          Add('PROVIDE ( r_lm_sp_n192_convert_wnaf = 0x400123c0 );');
          Add('PROVIDE ( r_lm_sp_n_one = 0x400123a4 );');
          Add('PROVIDE ( r_lm_sp_p192_add = 0x40012828 );');
          Add('PROVIDE ( r_lm_sp_p192_dbl = 0x4001268c );');
          Add('PROVIDE ( r_lm_sp_p192_invert = 0x40012b6c );');
          Add('PROVIDE ( r_lm_sp_p192_point_jacobian_to_affine = 0x40012468 );');
          Add('PROVIDE ( r_lm_sp_p192_points_jacobian_to_affine = 0x400124e4 );');
          Add('PROVIDE ( r_lm_sp_p192_point_to_inf = 0x40012458 );');
          Add('PROVIDE ( r_lm_sp_pre_compute_points = 0x40012640 );');
          Add('PROVIDE ( r_lm_sp_sha256_calculate = 0x400121a0 );');
          Add('PROVIDE ( r_LM_SuppressAclPacket = 0x4002f658 );');
          Add('PROVIDE ( r_lm_sync_flow_ctrl_en_get = 0x4004f404 );');
          Add('PROVIDE ( r_LM_UpdateAclEdrPacketType = 0x4002f5d8 );');
          Add('PROVIDE ( r_LM_UpdateAclPacketType = 0x4002f584 );');
          Add('PROVIDE ( r_modules_funcs = 0x3ffafd6c );');
          Add('PROVIDE ( r_modules_funcs_p = 0x3ffafd68 );');
          Add('PROVIDE ( r_nvds_del = 0x400544c4 );');
          Add('PROVIDE ( r_nvds_get = 0x40054488 );');
          Add('PROVIDE ( r_nvds_init = 0x40054410 );');
          Add('PROVIDE ( r_nvds_lock = 0x400544fc );');
          Add('PROVIDE ( r_nvds_put = 0x40054534 );');
          Add('PROVIDE ( rom_abs_temp = 0x400054f0 );');
          Add('PROVIDE ( rom_bb_bss_bw_40_en = 0x4000401c );');
          Add('PROVIDE ( rom_bb_bss_cbw40_dig = 0x40003bac );');
          Add('PROVIDE ( rom_bb_rx_ht20_cen_bcov_en = 0x40003734 );');
          Add('PROVIDE ( rom_bb_tx_ht20_cen = 0x40003760 );');
          Add('PROVIDE ( rom_bb_wdg_test_en = 0x40003b70 );');
          Add('PROVIDE ( rom_cbw2040_cfg = 0x400040b0 );');
          Add('PROVIDE ( rom_check_noise_floor = 0x40003c78 );');
          Add('PROVIDE ( rom_chip_i2c_readReg = 0x40004110 );');
          Add('PROVIDE ( rom_chip_i2c_writeReg = 0x40004168 );');
          Add('PROVIDE ( rom_chip_v7_bt_init = 0x40004d8c );');
          Add('PROVIDE ( rom_chip_v7_rx_init = 0x40004cec );');
          Add('PROVIDE ( rom_chip_v7_rx_rifs_en = 0x40003d90 );');
          Add('PROVIDE ( rom_chip_v7_tx_init = 0x40004d18 );');
          Add('PROVIDE ( rom_clk_force_on_vit = 0x40003710 );');
          Add('PROVIDE ( rom_correct_rf_ana_gain = 0x400062a8 );');
          Add('PROVIDE ( rom_dc_iq_est = 0x400055c8 );');
          Add('PROVIDE ( rom_disable_agc = 0x40002fa4 );');
          Add('PROVIDE ( rom_enable_agc = 0x40002fcc );');
          Add('PROVIDE ( rom_en_pwdet = 0x4000506c );');
          Add('PROVIDE ( rom_gen_rx_gain_table = 0x40003e3c );');
          Add('PROVIDE ( rom_get_data_sat = 0x4000312c );');
          Add('PROVIDE ( rom_get_fm_sar_dout = 0x40005204 );');
          Add('PROVIDE ( rom_get_power_db = 0x40005fc8 );');
          Add('PROVIDE ( rom_get_pwctrl_correct = 0x400065d4 );');
          Add('PROVIDE ( rom_get_rfcal_rxiq_data = 0x40005bbc );');
          Add('PROVIDE ( rom_get_rf_gain_qdb = 0x40006290 );');
          Add('PROVIDE ( rom_get_sar_dout = 0x40006564 );');
          Add('PROVIDE ( rom_i2c_readReg = 0x40004148 );');
          Add('PROVIDE ( rom_i2c_readReg_Mask = 0x400041c0 );');
          Add('PROVIDE ( rom_i2c_writeReg = 0x400041a4 );');
          Add('PROVIDE ( rom_i2c_writeReg_Mask = 0x400041fc );');
          Add('PROVIDE ( rom_index_to_txbbgain = 0x40004df8 );');
          Add('PROVIDE ( rom_iq_est_disable = 0x40005590 );');
          Add('PROVIDE ( rom_iq_est_enable = 0x40005514 );');
          Add('PROVIDE ( rom_linear_to_db = 0x40005f64 );');
          Add('PROVIDE ( rom_loopback_mode_en = 0x400030f8 );');
          Add('PROVIDE ( rom_meas_tone_pwr_db = 0x40006004 );');
          Add('PROVIDE ( rom_mhz2ieee = 0x4000404c );');
          Add('PROVIDE ( rom_noise_floor_auto_set = 0x40003bdc );');
          Add('PROVIDE ( rom_pbus_debugmode = 0x40004458 );');
          Add('PROVIDE ( rom_pbus_force_mode = 0x40004270 );');
          Add('PROVIDE ( rom_pbus_force_test = 0x400043c0 );');
          Add('PROVIDE ( rom_pbus_rd = 0x40004414 );');
          Add('PROVIDE ( rom_pbus_rd_addr = 0x40004334 );');
          Add('PROVIDE ( rom_pbus_rd_shift = 0x40004374 );');
          Add('PROVIDE ( rom_pbus_rx_dco_cal = 0x40005620 );');
          Add('PROVIDE ( rom_pbus_set_dco = 0x40004638 );');
          Add('PROVIDE ( rom_pbus_set_rxgain = 0x40004480 );');
          Add('PROVIDE ( rom_pbus_workmode = 0x4000446c );');
          Add('PROVIDE ( rom_pbus_xpd_rx_off = 0x40004508 );');
          Add('PROVIDE ( rom_pbus_xpd_rx_on = 0x4000453c );');
          Add('PROVIDE ( rom_pbus_xpd_tx_off = 0x40004590 );');
          Add('PROVIDE ( rom_pbus_xpd_tx_on = 0x400045e0 );');
          Add('PROVIDE ( rom_phy_disable_agc = 0x40002f6c );');
          Add('PROVIDE ( rom_phy_disable_cca = 0x40003000 );');
          Add('PROVIDE ( rom_phy_enable_agc = 0x40002f88 );');
          Add('PROVIDE ( rom_phy_enable_cca = 0x4000302c );');
          Add('PROVIDE ( rom_phy_freq_correct = 0x40004b44 );');
          Add('PROVIDE ( rom_phyFuns = 0x3ffae0c0 );');
          Add('PROVIDE ( rom_phy_get_noisefloor = 0x40003c2c );');
          Add('PROVIDE ( rom_phy_get_vdd33 = 0x4000642c );');
          Add('PROVIDE ( rom_pow_usr = 0x40003044 );');
          Add('PROVIDE ( rom_read_sar_dout = 0x400051c0 );');
          Add('PROVIDE ( rom_restart_cal = 0x400046e0 );');
          Add('PROVIDE ( rom_rfcal_pwrctrl = 0x40006058 );');
          Add('PROVIDE ( rom_rfcal_rxiq = 0x40005b4c );');
          Add('PROVIDE ( rom_rfcal_txcap = 0x40005dec );');
          Add('PROVIDE ( rom_rfpll_reset = 0x40004680 );');
          Add('PROVIDE ( rom_rfpll_set_freq = 0x400047f8 );');
          Add('PROVIDE ( rom_rtc_mem_backup = 0x40003db4 );');
          Add('PROVIDE ( rom_rtc_mem_recovery = 0x40003df4 );');
          Add('PROVIDE ( rom_rx_gain_force = 0x4000351c );');
          Add('PROVIDE ( rom_rxiq_cover_mg_mp = 0x40005a68 );');
          Add('PROVIDE ( rom_rxiq_get_mis = 0x400058e4 );');
          Add('PROVIDE ( rom_rxiq_set_reg = 0x40005a00 );');
          Add('PROVIDE ( rom_set_cal_rxdc = 0x400030b8 );');
          Add('PROVIDE ( rom_set_chan_cal_interp = 0x40005ce0 );');
          Add('PROVIDE ( rom_set_channel_freq = 0x40004880 );');
          Add('PROVIDE ( rom_set_loopback_gain = 0x40003060 );');
          Add('PROVIDE ( rom_set_noise_floor = 0x40003d48 );');
          Add('PROVIDE ( rom_set_pbus_mem = 0x400031a4 );');
          Add('PROVIDE ( rom_set_rf_freq_offset = 0x40004ca8 );');
          Add('PROVIDE ( rom_set_rxclk_en = 0x40003594 );');
          Add('PROVIDE ( rom_set_txcap_reg = 0x40005d50 );');
          Add('PROVIDE ( rom_set_txclk_en = 0x40003564 );');
          Add('PROVIDE ( rom_spur_coef_cfg = 0x40003ac8 );');
          Add('PROVIDE ( rom_spur_reg_write_one_tone = 0x400037f0 );');
          Add('PROVIDE ( rom_start_tx_tone = 0x400036b4 );');
          Add('PROVIDE ( rom_start_tx_tone_step = 0x400035d0 );');
          Add('PROVIDE ( rom_stop_tx_tone = 0x40003f98 );');
          Add('PROVIDE ( _rom_store = 0x4000d66c );');
          Add('PROVIDE ( _rom_store_table = 0x4000d4f8 );');
          Add('PROVIDE ( rom_target_power_add_backoff = 0x40006268 );');
          Add('PROVIDE ( rom_tx_atten_set_interp = 0x400061cc );');
          Add('PROVIDE ( rom_txbbgain_to_index = 0x40004dc0 );');
          Add('PROVIDE ( rom_txcal_work_mode = 0x4000510c );');
          Add('PROVIDE ( rom_txdc_cal_init = 0x40004e10 );');
          Add('PROVIDE ( rom_txdc_cal_v70 = 0x40004ea4 );');
          Add('PROVIDE ( rom_txiq_cover = 0x4000538c );');
          Add('PROVIDE ( rom_txiq_get_mis_pwr = 0x400052dc );');
          Add('PROVIDE ( rom_txiq_set_reg = 0x40005154 );');
          Add('PROVIDE ( rom_tx_pwctrl_bg_init = 0x4000662c );');
          Add('PROVIDE ( rom_txtone_linear_pwr = 0x40005290 );');
          Add('PROVIDE ( rom_wait_rfpll_cal_end = 0x400047a8 );');
          Add('PROVIDE ( rom_write_gain_mem = 0x4000348c );');
          Add('PROVIDE ( rom_write_rfpll_sdm = 0x40004740 );');
          Add('PROVIDE ( roundup2 = 0x4000ab7c );');
          Add('PROVIDE ( r_plf_funcs_p = 0x3ffb8360 );');
          Add('PROVIDE ( r_rf_rw_bt_init = 0x40054868 );');
          Add('PROVIDE ( r_rf_rw_init = 0x40054b0c );');
          Add('PROVIDE ( r_rf_rw_le_init = 0x400549d0 );');
          Add('PROVIDE ( r_rwble_activity_ongoing_check = 0x40054d8c );');
          Add('PROVIDE ( r_rwble_init = 0x40054bf4 );');
          Add('PROVIDE ( r_rwble_isr = 0x40054e08 );');
          Add('PROVIDE ( r_rwble_reset = 0x40054ce8 );');
          Add('PROVIDE ( r_rwble_sleep_check = 0x40054d78 );');
          Add('PROVIDE ( r_rwble_version = 0x40054dac );');
          Add('PROVIDE ( r_rwbt_init = 0x40055160 );');
          Add('PROVIDE ( r_rwbt_isr = 0x40055248 );');
          Add('PROVIDE ( r_rwbt_reset = 0x400551bc );');
          Add('PROVIDE ( r_rwbt_sleep_check = 0x4005577c );');
          Add('PROVIDE ( r_rwbt_sleep_enter = 0x400557a4 );');
          Add('PROVIDE ( r_rwbt_sleep_wakeup = 0x400557fc );');
          Add('PROVIDE ( r_rwbt_sleep_wakeup_end = 0x400558cc );');
          Add('PROVIDE ( r_rwbt_version = 0x4005520c );');
          Add('PROVIDE ( r_rwip_assert_err = 0x40055f88 );');
          Add('PROVIDE ( r_rwip_check_wakeup_boundary = 0x400558fc );');
          Add('PROVIDE ( r_rwip_ext_wakeup_enable = 0x40055f3c );');
          Add('PROVIDE ( r_rwip_init = 0x4005595c );');
          Add('PROVIDE ( r_rwip_pca_clock_dragging_only = 0x40055f48 );');
          Add('PROVIDE ( r_rwip_prevent_sleep_clear = 0x40055ec8 );');
          Add('PROVIDE ( r_rwip_prevent_sleep_set = 0x40055e64 );');
          Add('PROVIDE ( r_rwip_reset = 0x40055ab8 );');
          Add('PROVIDE ( r_rwip_schedule = 0x40055b38 );');
          Add('PROVIDE ( r_rwip_sleep = 0x40055b5c );');
          Add('PROVIDE ( r_rwip_sleep_enable = 0x40055f30 );');
          Add('PROVIDE ( r_rwip_version = 0x40055b20 );');
          Add('PROVIDE ( r_rwip_wakeup = 0x40055dc4 );');
          Add('PROVIDE ( r_rwip_wakeup_delay_set = 0x40055e4c );');
          Add('PROVIDE ( r_rwip_wakeup_end = 0x40055e18 );');
          Add('PROVIDE ( r_rwip_wlcoex_set = 0x40055f60 );');
          Add('PROVIDE ( r_SHA_256 = 0x40013a90 );');
          Add('PROVIDE ( rwip_coex_cfg = 0x3ff9914c );');
          Add('PROVIDE ( rwip_priority = 0x3ff99159 );');
          Add('PROVIDE ( rwip_rf = 0x3ffbdb28 );');
          Add('PROVIDE ( rwip_rf_p_get = 0x400558f4 );');
          Add('PROVIDE ( r_XorKey = 0x400112c0 );');
          Add('PROVIDE ( sha_blk_bits = 0x3ff99290 );');
          Add('PROVIDE ( sha_blk_bits_bytes = 0x3ff99288 );');
          Add('PROVIDE ( sha_blk_hash_bytes = 0x3ff9928c );');
          Add('PROVIDE ( sig_matrix = 0x3ffae293 );');
          Add('PROVIDE ( sip_after_tx_complete = 0x4000b358 );');
          Add('PROVIDE ( sip_alloc_to_host_evt = 0x4000ab9c );');
          Add('PROVIDE ( sip_get_ptr = 0x4000b34c );');
          Add('PROVIDE ( sip_get_state = 0x4000ae2c );');
          Add('PROVIDE ( sip_init_attach = 0x4000ae58 );');
          Add('PROVIDE ( sip_install_rx_ctrl_cb = 0x4000ae10 );');
          Add('PROVIDE ( sip_install_rx_data_cb = 0x4000ae20 );');
          Add('PROVIDE ( sip_is_active = 0x4000b3c0 );');
          Add('PROVIDE ( sip_post_init = 0x4000aed8 );');
          Add('PROVIDE ( sip_reclaim_from_host_cmd = 0x4000adbc );');
          Add('PROVIDE ( sip_reclaim_tx_data_pkt = 0x4000ad5c );');
          Add('PROVIDE ( sip_send = 0x4000af54 );');
          Add('PROVIDE ( sip_to_host_chain_append = 0x4000aef8 );');
          Add('PROVIDE ( sip_to_host_evt_send_done = 0x4000ac04 );');
          Add('PROVIDE ( slc_add_credits = 0x4000baf4 );');
          Add('PROVIDE ( slc_enable = 0x4000b64c );');
          Add('PROVIDE ( slc_from_host_chain_fetch = 0x4000b7e8 );');
          Add('PROVIDE ( slc_from_host_chain_recycle = 0x4000bb10 );');
          Add('PROVIDE ( slc_has_pkt_to_host = 0x4000b5fc );');
          Add('PROVIDE ( slc_init_attach = 0x4000b918 );');
          Add('PROVIDE ( slc_init_credit = 0x4000badc );');
          Add('PROVIDE ( slc_reattach = 0x4000b62c );');
          Add('PROVIDE ( slc_send_to_host_chain = 0x4000b6a0 );');
          Add('PROVIDE ( slc_set_host_io_max_window = 0x4000b89c );');
          Add('PROVIDE ( slc_to_host_chain_recycle = 0x4000b758 );');
          Add('PROVIDE ( specialModP256 = 0x4001600c );');
          Add('PROVIDE ( __stack = 0x3ffe3f20 );');
          Add('PROVIDE ( __stack_app = 0x3ffe7e30 );');
          Add('PROVIDE ( _stack_sentry = 0x3ffe1320 );');
          Add('PROVIDE ( _stack_sentry_app = 0x3ffe5230 );');
          Add('PROVIDE ( _start = 0x40000704 );');
          Add('PROVIDE ( start_tb_console = 0x4005a980 );');
          Add('PROVIDE ( _stat_r = 0x4000bcb4 );');
          Add('PROVIDE ( _stext = 0x40000560 );');
          Add('PROVIDE ( SubtractBigHex256 = 0x40015bcc );');
          Add('PROVIDE ( SubtractBigHexMod256 = 0x40015e8c );');
          Add('PROVIDE ( SubtractBigHexUint32_256 = 0x40015f8c );');
          Add('PROVIDE ( SubtractFromSelfBigHex256 = 0x40015c20 );');
          Add('PROVIDE ( SubtractFromSelfBigHexSign256 = 0x40015dc8 );');
          Add('PROVIDE ( sw_to_hw = 0x3ffb8d40 );');
          Add('PROVIDE ( syscall_table_ptr_app = 0x3ffae020 );');
          Add('PROVIDE ( syscall_table_ptr_pro = 0x3ffae024 );');
          Add('PROVIDE ( tdefl_compress = 0x400600bc );');
          Add('PROVIDE ( tdefl_compress_buffer = 0x400607f4 );');
          Add('PROVIDE ( tdefl_compress_mem_to_mem = 0x40060900 );');
          Add('PROVIDE ( tdefl_compress_mem_to_output = 0x400608e0 );');
          Add('PROVIDE ( tdefl_get_adler32 = 0x400608d8 );');
          Add('PROVIDE ( tdefl_get_prev_return_status = 0x400608d0 );');
          Add('PROVIDE ( tdefl_init = 0x40060810 );');
          Add('PROVIDE ( tdefl_write_image_to_png_file_in_memory = 0x4006091c );');
          Add('PROVIDE ( tdefl_write_image_to_png_file_in_memory_ex = 0x40060910 );');
          Add('PROVIDE ( tinfl_decompress = 0x4005ef30 );');
          Add('PROVIDE ( tinfl_decompress_mem_to_callback = 0x40060090 );');
          Add('PROVIDE ( tinfl_decompress_mem_to_mem = 0x40060050 );');
          Add('PROVIDE ( UartDev = 0x3ffe019c );');
          Add('PROVIDE ( user_code_start = 0x3ffe0400 );');
          Add('PROVIDE ( veryBigHexP256 = 0x3ff9736c );');
          Add('PROVIDE ( xthal_bcopy = 0x4000c098 );');
          Add('PROVIDE ( xthal_copy123 = 0x4000c124 );');
          Add('PROVIDE ( xthal_get_ccompare = 0x4000c078 );');
          Add('PROVIDE ( xthal_get_ccount = 0x4000c050 );');
          Add('PROVIDE ( xthal_get_interrupt = 0x4000c1e4 );');
          Add('PROVIDE ( xthal_get_intread = 0x4000c1e4 );');
          Add('PROVIDE ( Xthal_intlevel = 0x3ff9c2b4 );');
          Add('PROVIDE ( xthal_memcpy = 0x4000c0bc );');
          Add('PROVIDE ( xthal_set_ccompare = 0x4000c058 );');
          Add('PROVIDE ( xthal_set_intclear = 0x4000c1ec );');
          Add('PROVIDE ( _xtos_set_intlevel = 0x4000bfdc );');
          Add('PROVIDE ( g_ticks_per_us_pro = 0x3ffe01e0 );');
          Add('PROVIDE ( g_ticks_per_us_app = 0x3ffe40f0 );');
          Add('PROVIDE ( esp_rom_spiflash_config_param = 0x40063238 );');
          Add('PROVIDE ( esp_rom_spiflash_read_user_cmd = 0x400621b0 );');
          Add('PROVIDE ( esp_rom_spiflash_write_encrypted_disable = 0x40062e60 );');
          Add('PROVIDE ( esp_rom_spiflash_write_encrypted_enable = 0x40062df4 );');
          Add('PROVIDE ( esp_rom_spiflash_prepare_encrypted_data = 0x40062e1c );');
          Add('PROVIDE ( esp_rom_spiflash_select_qio_pins = 0x40061ddc );');
          Add('PROVIDE ( esp_rom_spiflash_attach = 0x40062a6c );');
          Add('PROVIDE ( esp_rom_spiflash_config_clk = 0x40062bc8 );');
          Add('PROVIDE ( g_rom_spiflash_chip = 0x3ffae270 );');
          Add('PROVIDE ( hci_le_rd_rem_used_feats_cmd_handler = 0x400417b4 );');
          Add('PROVIDE ( llcp_length_req_handler = 0x40043808 );');
          Add('PROVIDE ( llcp_unknown_rsp_handler = 0x40043ba8 );');
          Add('PROVIDE ( FilePacketSendDeflatedReqMsgProc = 0x40008b24 );');
          Add('PROVIDE ( FilePacketSendReqMsgProc = 0x40008860 );');
          Add('PROVIDE ( FlashDwnLdDeflatedStartMsgProc = 0x40008ad8 );');
          Add('PROVIDE ( FlashDwnLdParamCfgMsgProc = 0x4000891c );');
          Add('PROVIDE ( FlashDwnLdStartMsgProc = 0x40008820 );');
          Add('PROVIDE ( FlashDwnLdStopDeflatedReqMsgProc = 0x40008c18 );');
          Add('PROVIDE ( FlashDwnLdStopReqMsgProc = 0x400088ec );');
          Add('PROVIDE ( MemDwnLdStartMsgProc = 0x40008948 );');
          Add('PROVIDE ( MemDwnLdStopReqMsgProc = 0x400089dc );');
          Add('PROVIDE ( MemPacketSendReqMsgProc = 0x40008978 );');
          Add('PROVIDE ( uart_baudrate_detect = 0x40009034 );');
          Add('PROVIDE ( uart_buff_switch = 0x400093c0 );');
          Add('PROVIDE ( UartConnCheck = 0x40008738 );');
          Add('PROVIDE ( UartConnectProc = 0x40008a04 );');
          Add('PROVIDE ( UartDwnLdProc = 0x40008ce8 );');
          Add('PROVIDE ( UartRegReadProc = 0x40008a58 );');
          Add('PROVIDE ( UartRegWriteProc = 0x40008a14 );');
          Add('PROVIDE ( UartSetBaudProc = 0x40008aac );');
          Add('PROVIDE ( UartSpiAttachProc = 0x40008a6c );');
          Add('PROVIDE ( UartSpiReadProc = 0x40008a80 );');
          Add('PROVIDE ( VerifyFlashMd5Proc = 0x40008c44 );');
          Add('PROVIDE ( GetUartDevice = 0x40009598 );');
          Add('PROVIDE ( RcvMsg = 0x4000954c );');
          Add('PROVIDE ( SendMsg = 0x40009384 );');
          Add('PROVIDE ( UartGetCmdLn = 0x40009564 );');
          Add('PROVIDE ( UartRxString = 0x400092fc );');
          Add('PROVIDE ( Uart_Init = 0x40009120 );');
          Add('PROVIDE ( recv_packet = 0x40009424 );');
          Add('PROVIDE ( send_packet = 0x40009340 );');
          Add('PROVIDE ( uartAttach = 0x40008fd0 );');
          Add('PROVIDE ( uart_div_modify = 0x400090cc );');
          Add('PROVIDE ( uart_rx_intr_handler = 0x40008f4c );');
          Add('PROVIDE ( uart_rx_one_char = 0x400092d0 );');
          Add('PROVIDE ( uart_rx_one_char_block = 0x400092a4 );');
          Add('PROVIDE ( uart_rx_readbuff = 0x40009394 );');
          Add('PROVIDE ( uart_tx_flush = 0x40009258 );');
          Add('PROVIDE ( uart_tx_one_char = 0x40009200 );');
          Add('PROVIDE ( uart_tx_one_char2 = 0x4000922c );');
          Add('PROVIDE ( uart_tx_switch = 0x40009028 );');
          Add('PROVIDE ( gpio_output_set = 0x40009b24 );');
          Add('PROVIDE ( gpio_output_set_high = 0x40009b5c );');
          Add('PROVIDE ( gpio_input_get = 0x40009b88 );');
          Add('PROVIDE ( gpio_input_get_high = 0x40009b9c );');
          Add('PROVIDE ( gpio_matrix_in = 0x40009edc );');
          Add('PROVIDE ( gpio_matrix_out = 0x40009f0c );');
          Add('PROVIDE ( gpio_pad_select_gpio = 0x40009fdc );');
          Add('PROVIDE ( gpio_pad_set_drv = 0x4000a11c );');
          Add('PROVIDE ( gpio_pad_pulldown = 0x4000a348 );');
          Add('PROVIDE ( gpio_pad_pullup = 0x4000a22c );');
          Add('PROVIDE ( gpio_pad_hold = 0x4000a734 );');
          Add('PROVIDE ( gpio_pad_unhold = 0x4000a484 );');
          Add('PROVIDE ( ets_aes_crypt = 0x4005c9b8 );');
          Add('PROVIDE ( ets_aes_disable = 0x4005c8f8 );');
          Add('PROVIDE ( ets_aes_enable = 0x4005c8cc );');
          Add('PROVIDE ( ets_aes_set_endian = 0x4005c928 );');
          Add('PROVIDE ( ets_aes_setkey_dec = 0x4005c994 );');
          Add('PROVIDE ( ets_aes_setkey_enc = 0x4005c97c );');
          Add('PROVIDE ( ets_bigint_disable = 0x4005c4e0 );');
          Add('PROVIDE ( ets_bigint_enable = 0x4005c498 );');
          Add('PROVIDE ( ets_bigint_mod_mult_getz = 0x4005c818 );');
          Add('PROVIDE ( ets_bigint_mod_mult_prepare = 0x4005c7b4 );');
          Add('PROVIDE ( ets_bigint_mod_power_getz = 0x4005c614 );');
          Add('PROVIDE ( ets_bigint_mod_power_prepare = 0x4005c54c );');
          Add('PROVIDE ( ets_bigint_montgomery_mult_getz = 0x4005c7a4 );');
          Add('PROVIDE ( ets_bigint_montgomery_mult_prepare = 0x4005c6fc );');
          Add('PROVIDE ( ets_bigint_mult_getz = 0x4005c6e8 );');
          Add('PROVIDE ( ets_bigint_mult_prepare = 0x4005c630 );');
          Add('PROVIDE ( ets_bigint_wait_finish = 0x4005c520 );');
          Add('PROVIDE ( ets_post = 0x4000673c );');
          Add('PROVIDE ( ets_run = 0x400066bc );');
          Add('PROVIDE ( ets_set_idle_cb = 0x40006674 );');
          Add('PROVIDE ( ets_task = 0x40006688 );');
          Add('PROVIDE ( ets_efuse_get_8M_clock = 0x40008710 );');
          Add('PROVIDE ( ets_efuse_get_spiconfig = 0x40008658 );');
          Add('PROVIDE ( ets_efuse_program_op = 0x40008628 );');
          Add('PROVIDE ( ets_efuse_read_op = 0x40008600 );');
          Add('PROVIDE ( ets_intr_lock = 0x400067b0 );');
          Add('PROVIDE ( ets_intr_unlock = 0x400067c4 );');
          Add('PROVIDE ( ets_isr_attach = 0x400067ec );');
          Add('PROVIDE ( ets_waiti0 = 0x400067d8 );');
          Add('PROVIDE ( intr_matrix_set = 0x4000681c );');
          Add('PROVIDE ( check_pos = 0x400068b8 );');
          Add('PROVIDE ( ets_set_appcpu_boot_addr = 0x4000689c );');
          Add('PROVIDE ( ets_set_startup_callback = 0x4000688c );');
          Add('PROVIDE ( ets_set_user_start = 0x4000687c );');
          Add('PROVIDE ( ets_unpack_flash_code = 0x40007018 );');
          Add('PROVIDE ( ets_unpack_flash_code_legacy = 0x4000694c );');
          Add('PROVIDE ( rom_main = 0x400076c4 );');
          Add('PROVIDE ( ets_write_char_uart = 0x40007cf8 );');
          Add('PROVIDE ( ets_install_putc1 = 0x40007d18 );');
          Add('PROVIDE ( ets_install_putc2 = 0x40007d38 );');
          Add('PROVIDE ( ets_install_uart_printf = 0x40007d28 );');
          Add('PROVIDE ( ets_printf = 0x40007d54 );');
          Add('PROVIDE ( rtc_boot_control = 0x4000821c );');
          Add('PROVIDE ( rtc_get_reset_reason = 0x400081d4 );');
          Add('PROVIDE ( rtc_get_wakeup_cause = 0x400081f4 );');
          Add('PROVIDE ( rtc_select_apb_bridge = 0x40008288 );');
          Add('PROVIDE ( set_rtc_memory_crc = 0x40008208 );');
          Add('PROVIDE ( software_reset = 0x4000824c );');
          Add('PROVIDE ( software_reset_cpu = 0x40008264 );');
          Add('PROVIDE ( ets_secure_boot_check = 0x4005cb40 );');
          Add('PROVIDE ( ets_secure_boot_check_finish = 0x4005cc04 );');
          Add('PROVIDE ( ets_secure_boot_check_start = 0x4005cbcc );');
          Add('PROVIDE ( ets_secure_boot_finish = 0x4005ca84 );');
          Add('PROVIDE ( ets_secure_boot_hash = 0x4005cad4 );');
          Add('PROVIDE ( ets_secure_boot_obtain = 0x4005cb14 );');
          Add('PROVIDE ( ets_secure_boot_rd_abstract = 0x4005cba8 );');
          Add('PROVIDE ( ets_secure_boot_rd_iv = 0x4005cb84 );');
          Add('PROVIDE ( ets_secure_boot_start = 0x4005ca34 );');
          Add('PROVIDE ( ets_sha_disable = 0x4005c0a8 );');
          Add('PROVIDE ( ets_sha_enable = 0x4005c07c );');
          Add('PROVIDE ( ets_sha_finish = 0x4005c104 );');
          Add('PROVIDE ( ets_sha_init = 0x4005c0d4 );');
          Add('PROVIDE ( ets_sha_update = 0x4005c2a0 );');
          Add('PROVIDE ( ets_delay_us = 0x40008534 );');
          Add('PROVIDE ( ets_get_cpu_frequency = 0x4000855c );');
          Add('PROVIDE ( ets_get_detected_xtal_freq = 0x40008588 );');
          Add('PROVIDE ( ets_get_xtal_scale = 0x4000856c );');
          Add('PROVIDE ( ets_update_cpu_frequency_rom = 0x40008550 );');
          Add('PROVIDE ( hci_tl_env = 0x3ffb8154 );');
          Add('PROVIDE ( ld_acl_env = 0x3ffb8258 );');
          Add('PROVIDE ( ea_env = 0x3ffb80ec );');
          Add('PROVIDE ( lc_sco_data_path_config = 0x3ffb81f8 );');
          Add('PROVIDE ( lc_sco_env = 0x3ffb81fc );');
          Add('PROVIDE ( ld_active_ch_map = 0x3ffb8334 );');
          Add('PROVIDE ( ld_bcst_acl_env = 0x3ffb8274 );');
          Add('PROVIDE ( ld_csb_rx_env = 0x3ffb8278 );');
          Add('PROVIDE ( ld_csb_tx_env = 0x3ffb827c );');
          Add('PROVIDE ( ld_env = 0x3ffb9510 );');
          Add('PROVIDE ( ld_fm_env = 0x3ffb8284 );');
          Add('PROVIDE ( ld_inq_env = 0x3ffb82e4 );');
          Add('PROVIDE ( ld_iscan_env = 0x3ffb82e8 );');
          Add('PROVIDE ( ld_page_env = 0x3ffb82f0 );');
          Add('PROVIDE ( ld_pca_env = 0x3ffb82f4 );');
          Add('PROVIDE ( ld_pscan_env = 0x3ffb8308 );');
          Add('PROVIDE ( ld_sched_env = 0x3ffb830c );');
          Add('PROVIDE ( ld_sched_params = 0x3ffb96c0 );');
          Add('PROVIDE ( ld_sco_env = 0x3ffb824c );');
          Add('PROVIDE ( ld_sscan_env = 0x3ffb832c );');
          Add('PROVIDE ( ld_strain_env = 0x3ffb8330 );');
          Add('PROVIDE ( LM_Sniff = 0x3ffb8230 );');
          Add('PROVIDE ( LM_SniffSubRate = 0x3ffb8214 );');
          Add('PROVIDE ( prbs_64bytes = 0x3ff98992 );');
          Add('PROVIDE ( nvds_env = 0x3ffb8364 );');
          Add('PROVIDE ( nvds_magic_number = 0x3ff9912a );');
          Add('PROVIDE ( TASK_DESC_LLD = 0x3ff98b58 );');
          Add('/* esp32.rom.libgcc.ld */');
          Add('__absvdi2 = 0x4006387c;');
          Add('__absvsi2 = 0x40063868;');
          Add('__adddf3 = 0x40002590;');
          Add('__addsf3 = 0x400020e8;');
          Add('__addvdi3 = 0x40002cbc;');
          Add('__addvsi3 = 0x40002c98;');
          Add('__ashldi3 = 0x4000c818;');
          Add('__ashrdi3 = 0x4000c830;');
          Add('__bswapdi2 = 0x40064b08;');
          Add('__bswapsi2 = 0x40064ae0;');
          Add('__clrsbdi2 = 0x40064b7c;');
          Add('__clrsbsi2 = 0x40064b64;');
          Add('__clzdi2 = 0x4000ca50;');
          Add('__clzsi2 = 0x4000c7e8;');
          Add('__cmpdi2 = 0x40063820;');
          Add('__ctzdi2 = 0x4000ca64;');
          Add('__ctzsi2 = 0x4000c7f0;');
          Add('__divdc3 = 0x400645a4;');
          Add('__divdf3 = 0x40002954;');
          Add('__divdi3 = 0x4000ca84;');
          Add('__divsi3 = 0x4000c7b8;');
          Add('__eqdf2 = 0x400636a8;');
          Add('__eqsf2 = 0x40063374;');
          Add('__extendsfdf2 = 0x40002c34;');
          Add('__ffsdi2 = 0x4000ca2c;');
          Add('__ffssi2 = 0x4000c804;');
          Add('__fixdfdi = 0x40002ac4;');
          Add('__fixdfsi = 0x40002a78;');
          Add('__fixsfdi = 0x4000244c;');
          Add('__fixsfsi = 0x4000240c;');
          Add('__fixunsdfsi = 0x40002b30;');
          Add('__fixunssfdi = 0x40002504;');
          Add('__fixunssfsi = 0x400024ac;');
          Add('__floatdidf = 0x4000c988;');
          Add('__floatdisf = 0x4000c8c0;');
          Add('__floatsidf = 0x4000c944;');
          Add('__floatsisf = 0x4000c870;');
          Add('__floatundidf = 0x4000c978;');
          Add('__floatundisf = 0x4000c8b0;');
          Add('__floatunsidf = 0x4000c938;');
          Add('__floatunsisf = 0x4000c864;');
          Add('__gcc_bcmp = 0x40064a70;');
          Add('__gedf2 = 0x40063768;');
          Add('__gesf2 = 0x4006340c;');
          Add('__gtdf2 = 0x400636dc;');
          Add('__gtsf2 = 0x400633a0;');
          Add('__ledf2 = 0x40063704;');
          Add('__lesf2 = 0x400633c0;');
          Add('__lshrdi3 = 0x4000c84c;');
          Add('__ltdf2 = 0x40063790;');
          Add('__ltsf2 = 0x4006342c;');
          Add('__moddi3 = 0x4000cd4c;');
          Add('__modsi3 = 0x4000c7c0;');
          Add('__muldc3 = 0x40063c90;');
          Add('__muldf3 = 0x4006358c;');
          Add('__muldi3 = 0x4000c9fc;');
          Add('__mulsf3 = 0x400632c8;');
          Add('__mulsi3 = 0x4000c7b0;');
          Add('__mulvdi3 = 0x40002d78;');
          Add('__mulvsi3 = 0x40002d60;');
          Add('__nedf2 = 0x400636a8;');
          Add('__negdf2 = 0x400634a0;');
          Add('__negdi2 = 0x4000ca14;');
          Add('__negsf2 = 0x400020c0;');
          Add('__negvdi2 = 0x40002e98;');
          Add('__negvsi2 = 0x40002e78;');
          Add('__nesf2 = 0x40063374;');
          Add('__nsau_data = 0x3ff96544;');
          Add('__paritysi2 = 0x40002f3c;');
          Add('__popcount_tab = 0x3ff96544;');
          Add('__popcountdi2 = 0x40002ef8;');
          Add('__popcountsi2 = 0x40002ed0;');
          Add('__powidf2 = 0x400638e4;');
          Add('__subdf3 = 0x400026e4;');
          Add('__subsf3 = 0x400021d0;');
          Add('__subvdi3 = 0x40002d20;');
          Add('__subvsi3 = 0x40002cf8;');
          Add('__truncdfsf2 = 0x40002b90;');
          Add('__ucmpdi2 = 0x40063840;');
          Add('__udiv_w_sdiv = 0x40064bec;');
          Add('__udivdi3 = 0x4000cff8;');
          Add('__udivmoddi4 = 0x40064bf4;');
          Add('__udivsi3 = 0x4000c7c8;');
          Add('__umoddi3 = 0x4000d280;');
          Add('__umodsi3 = 0x4000c7d0;');
          Add('__umulsidi3 = 0x4000c7d8;');
          Add('__unorddf2 = 0x400637f4;');
          Add('__unordsf2 = 0x40063478;');
          Add('/* esp32.rom.newlib-data.ld */');
          Add('_ctype_ = 0x3ff96354;');
          Add('__ctype_ptr__ = 0x3ff96350;');
          Add('_daylight = 0x3ffae0a4;');
          Add('environ = 0x3ffae0b4;');
          Add('_global_impure_ptr = 0x3ffae0b0;');
          Add('__mb_cur_max = 0x3ff96530;');
          Add('__month_lengths = 0x3ff9609c;');
          Add('__sf_fake_stderr = 0x3ff96458;');
          Add('__sf_fake_stdin = 0x3ff96498;');
          Add('__sf_fake_stdout = 0x3ff96478;');
          Add('_timezone = 0x3ffae0a0;');
          Add('_tzname = 0x3ffae030;');
          Add('__wctomb = 0x3ff96540;');
          Add('/* esp32.rom.syscalls.ld */');
          Add('close = 0x40001778;');
          Add('open = 0x4000178c;');
          Add('read = 0x400017dc;');
          Add('sbrk = 0x400017f4;');
          Add('times = 0x40001808;');
          Add('write = 0x4000181c;');
          Add('/* esp32.newlib-funcs.ld */');
          Add('abs = 0x40056340;');
          Add('__ascii_wctomb = 0x40058ef0;');
          Add('atoi = 0x400566c4;');
          Add('_atoi_r = 0x400566d4;');
          Add('atol = 0x400566ec;');
          Add('_atol_r = 0x400566fc;');
          Add('bzero = 0x4000c1f4;');
          Add('_cleanup = 0x40001df8;');
          Add('_cleanup_r = 0x40001d48;');
          Add('creat = 0x40000e8c;');
          Add('div = 0x40056348;');
          Add('__dummy_lock = 0x4000c728;');
          Add('__dummy_lock_try = 0x4000c730;');
          Add('__env_lock = 0x40001fd4;');
          Add('__env_unlock = 0x40001fe0;');
          Add('fclose = 0x400020ac;');
          Add('_fclose_r = 0x40001fec;');
          Add('fflush = 0x40059394;');
          Add('_fflush_r = 0x40059320;');
          Add('_findenv_r = 0x40001f44;');
          Add('__fp_lock_all = 0x40001f1c;');
          Add('__fp_unlock_all = 0x40001f30;');
          Add('__fputwc = 0x40058da0;');
          Add('fputwc = 0x40058ea8;');
          Add('_fputwc_r = 0x40058e4c;');
          Add('_fwalk = 0x4000c738;');
          Add('_fwalk_reent = 0x4000c770;');
          Add('_getenv_r = 0x40001fbc;');
          Add('isalnum = 0x40000f04;');
          Add('isalpha = 0x40000f18;');
          Add('isascii = 0x4000c20c;');
          Add('_isatty_r = 0x40000ea0;');
          Add('isblank = 0x40000f2c;');
          Add('iscntrl = 0x40000f50;');
          Add('isdigit = 0x40000f64;');
          Add('isgraph = 0x40000f94;');
          Add('islower = 0x40000f78;');
          Add('isprint = 0x40000fa8;');
          Add('ispunct = 0x40000fc0;');
          Add('isspace = 0x40000fd4;');
          Add('isupper = 0x40000fe8;');
          Add('__itoa = 0x40056678;');
          Add('itoa = 0x400566b4;');
          Add('labs = 0x40056370;');
          Add('ldiv = 0x40056378;');
          Add('longjmp = 0x400562cc;');
          Add('memccpy = 0x4000c220;');
          Add('memchr = 0x4000c244;');
          Add('memcmp = 0x4000c260;');
          Add('memcpy = 0x4000c2c8;');
          Add('memmove = 0x4000c3c0;');
          Add('memrchr = 0x4000c400;');
          Add('memset = 0x4000c44c;');
          Add('qsort = 0x40056424;');
          Add('rand = 0x40001058;');
          Add('rand_r = 0x400010d4;');
          Add('__sccl = 0x4000c498;');
          Add('__sclose = 0x400011b8;');
          Add('__seofread = 0x40001148;');
          Add('setjmp = 0x40056268;');
          Add('__sflush_r = 0x400591e0;');
          Add('__sfmoreglue = 0x40001dc8;');
          Add('__sfp = 0x40001e90;');
          Add('__sfp_lock_acquire = 0x40001e08;');
          Add('__sfp_lock_release = 0x40001e14;');
          Add('__sfvwrite_r = 0x4005893c;');
          Add('__sinit = 0x40001e38;');
          Add('__sinit_lock_acquire = 0x40001e20;');
          Add('__sinit_lock_release = 0x40001e2c;');
          Add('__smakebuf_r = 0x40059108;');
          Add('srand = 0x40001004;');
          Add('__sread = 0x40001118;');
          Add('__srefill_r = 0x400593d4;');
          Add('__sseek = 0x40001184;');
          Add('strcasecmp = 0x400011cc;');
          Add('strcasestr = 0x40001210;');
          Add('strcat = 0x4000c518;');
          Add('strchr = 0x4000c53c;');
          Add('strcmp = 0x40001274;');
          Add('strcoll = 0x40001398;');
          Add('strcpy = 0x400013ac;');
          Add('strcspn = 0x4000c558;');
          Add('strdup = 0x4000143c;');
          Add('_strdup_r = 0x40001450;');
          Add('strlcat = 0x40001470;');
          Add('strlcpy = 0x4000c584;');
          Add('strlen = 0x400014c0;');
          Add('strlwr = 0x40001524;');
          Add('strncasecmp = 0x40001550;');
          Add('strncat = 0x4000c5c4;');
          Add('strncmp = 0x4000c5f4;');
          Add('strncpy = 0x400015d4;');
          Add('strndup = 0x400016b0;');
          Add('_strndup_r = 0x400016c4;');
          Add('strnlen = 0x4000c628;');
          Add('strrchr = 0x40001708;');
          Add('strsep = 0x40001734;');
          Add('strspn = 0x4000c648;');
          Add('strstr = 0x4000c674;');
          Add('__strtok_r = 0x4000c6a8;');
          Add('strtok_r = 0x4000c70c;');
          Add('strtol = 0x4005681c;');
          Add('_strtol_r = 0x40056714;');
          Add('strtoul = 0x4005692c;');
          Add('_strtoul_r = 0x40056834;');
          Add('strupr = 0x4000174c;');
          Add('__submore = 0x40058f3c;');
          Add('__swbuf = 0x40058cb4;');
          Add('__swbuf_r = 0x40058bec;');
          Add('__swrite = 0x40001150;');
          Add('__swsetup_r = 0x40058cc8;');
          Add('toascii = 0x4000c720;');
          Add('tolower = 0x40001868;');
          Add('toupper = 0x40001884;');
          Add('__tzcalc_limits = 0x400018a0;');
          Add('__tz_lock = 0x40001a04;');
          Add('__tz_unlock = 0x40001a10;');
          Add('ungetc = 0x400590f4;');
          Add('_ungetc_r = 0x40058fa0;');
          Add('__utoa = 0x400561f0;');
          Add('utoa = 0x40056258;');
          Add('wcrtomb = 0x40058920;');
          Add('_wcrtomb_r = 0x400588d8;');
          Add('_wctomb_r = 0x40058f14;');
          Add('/* esp32.peripherals.ld */');
          Add('PROVIDE ( UART0 = 0x3ff40000 );');
          Add('PROVIDE ( SPI1 = 0x3ff42000 );');
          Add('PROVIDE ( SPI0 = 0x3ff43000 );');
          Add('PROVIDE ( GPIO = 0x3ff44000 );');
          Add('PROVIDE ( SIGMADELTA = 0x3ff44f00 );');
          Add('PROVIDE ( RTCCNTL = 0x3ff48000 );');
          Add('PROVIDE ( RTCIO = 0x3ff48400 );');
          Add('PROVIDE ( SENS = 0x3ff48800 );');
          Add('PROVIDE ( HINF = 0x3ff4B000 );');
          Add('PROVIDE ( UHCI1 = 0x3ff4C000 );');
          Add('PROVIDE ( I2S0 = 0x3ff4F000 );');
          Add('PROVIDE ( UART1 = 0x3ff50000 );');
          Add('PROVIDE ( I2C0 = 0x3ff53000 );');
          Add('PROVIDE ( UHCI0 = 0x3ff54000 );');
          Add('PROVIDE ( HOST = 0x3ff55000 );');
          Add('PROVIDE ( RMT = 0x3ff56000 );');
          Add('PROVIDE ( RMTMEM = 0x3ff56800 );');
          Add('PROVIDE ( PCNT = 0x3ff57000 );');
          Add('PROVIDE ( SLC = 0x3ff58000 );');
          Add('PROVIDE ( LEDC = 0x3ff59000 );');
          Add('PROVIDE ( MCPWM0 = 0x3ff5E000 );');
          Add('PROVIDE ( TIMERG0 = 0x3ff5F000 );');
          Add('PROVIDE ( TIMERG1 = 0x3ff60000 );');
          Add('PROVIDE ( SPI2 = 0x3ff64000 );');
          Add('PROVIDE ( SPI3 = 0x3ff65000 );');
          Add('PROVIDE ( SYSCON = 0x3ff66000 );');
          Add('PROVIDE ( I2C1 = 0x3ff67000 );');
          Add('PROVIDE ( SDMMC = 0x3ff68000 );');
          Add('PROVIDE ( EMAC_DMA = 0x3ff69000 );');
          Add('PROVIDE ( EMAC_EXT = 0x3ff69800 );');
          Add('PROVIDE ( EMAC_MAC = 0x3ff6A000 );');
          Add('PROVIDE ( CAN = 0x3ff6B000 );');
          Add('PROVIDE ( MCPWM1 = 0x3ff6C000 );');
          Add('PROVIDE ( I2S1 = 0x3ff6D000 );');
          Add('PROVIDE ( UART2 = 0x3ff6E000 );');
        end
      else // for ESP8266 use internal linker script
        begin
          Add('/* esp8266_out.ld */');
          Add('MEMORY');
          Add('{');
          Add('  iram0_0_seg (RX) : org = 0x40100000, len = 0xC000');
          Add('  iram0_2_seg (RX) : org = 0x40200010 + (0x10000 & (0x100000 - 1)),');
          Add('                                     len = 0xf0000 - 0x10');
          Add('  dram0_0_seg (RW) : org = 0x3FFE8000, len = 0x18000');
          Add('  rtc_data_seg(RW) : org = 0x60001200, len = 0x200');
          Add('}');
          Add('/* esp8266.project.ld */');
          Add('ENTRY(call_start_cpu);');
          Add('SECTIONS');
          Add('{');
          Add('  .rtc.data :');
          Add('  {');
          Add('    _rtc_data_start = ABSOLUTE(.);');
          Add('    *( .rtc.data  .rtc.data.*  .rtc.rodata  .rtc.rodata.*)');
          Add('    _rtc_data_end = ABSOLUTE(.);');
          Add('  } > rtc_data_seg');
          Add('  .rtc.bss (NOLOAD) :');
          Add('  {');
          Add('    _rtc_bss_start = ABSOLUTE(.);');
          Add('    *( .rtc.bss)');
          Add('    _rtc_bss_end = ABSOLUTE(.);');
          Add('  } > rtc_data_seg');
          Add('  .rtc_noinit (NOLOAD):');
          Add('  {');
          Add('    . = ALIGN(4);');
          Add('    _rtc_noinit_start = ABSOLUTE(.);');
          Add('    *(.rtc_noinit .rtc_noinit.*)');
          Add('    . = ALIGN(4) ;');
          Add('    _rtc_noinit_end = ABSOLUTE(.);');
          Add('  } > rtc_data_seg');
          Add('  ASSERT(((_rtc_noinit_end - ORIGIN(rtc_data_seg)) <= LENGTH(rtc_data_seg)),');
          Add('        "RTC segment data does not fit.")');
          Add('  .iram0.vectors :');
          Add('  {');
          Add('    _iram_start = ABSOLUTE(.);');
          Add('    _init_start = ABSOLUTE(.);');
          Add('    KEEP(*(.SystemInfoVector.text));');
          Add('    . = 0x10;');
          Add('    KEEP(*(.DebugExceptionVector.text));');
          Add('    . = 0x20;');
          Add('    KEEP(*(.NMIExceptionVector.text));');
          Add('    . = 0x30;');
          Add('    KEEP(*(.KernelExceptionVector.text));');
          Add('    . = 0x50;');
          Add('    KEEP(*(.UserExceptionVector.text));');
          Add('    . = 0x70;');
          Add('    KEEP(*(.DoubleExceptionVector.text));');
          Add('    *(.text .literal)');
          Add('    *(.*Vector.literal)');
          Add('    *(.UserEnter.literal);');
          Add('    *(.UserEnter.text);');
          Add('    . = ALIGN (16);');
          Add('    *(.entry.text)');
          Add('    *(.init.literal)');
          Add('    *(.init)');
          Add('    _init_end = ABSOLUTE(.);');
          Add('  } > iram0_0_seg');
          Add('  .iram0.text :');
          Add('  {');
          Add('    _iram_text_start = ABSOLUTE(.);');
          Add('    *( .iram1  .iram1.*)');
          Add('    *libphy.a:( .literal  .literal.*  .text  .text.*)');
          Add('    *libspi_flash.a:spi_flash_raw.*( .literal  .literal.*  .text  .text.*)');
          Add('    *libpp.a:( .literal  .literal.*  .text  .text.*)');
          Add('    _iram_text_end = ABSOLUTE(.);');
          Add('  } > iram0_0_seg');
          Add('  .iram0.bss :');
          Add('  {');
          Add('    . = ALIGN (4);');
          Add('    _iram_bss_start = ABSOLUTE(.);');
          Add('    *libcore.a:( .bss  .bss.*  COMMON)');
          Add('    *libfreertos.a:freertos_hooks.*( .bss  .bss.*  COMMON)');
          Add('    *libfreertos.a:impure.*( .bss  .bss.*  COMMON)');
          Add('    *libfreertos.a:timers.*( .bss  .bss.*  COMMON)');
          Add('    *libfreertos.a:queue.*( .bss  .bss.*  COMMON)');
          Add('    *libfreertos.a:stream_buffer.*( .bss  .bss.*  COMMON)');
          Add('    *libfreertos.a:event_groups.*( .bss  .bss.*  COMMON)');
          Add('    *libfreertos.a:list.*( .bss  .bss.*  COMMON)');
          Add('    *libfreertos.a:tasks.*( .bss  .bss.*  COMMON)');
          Add('    *liblwip.a:( .bss  .bss.*  COMMON)');
          Add('    . = ALIGN (4);');
          Add('    _iram_bss_end = ABSOLUTE(.);');
          Add('    _iram_end = ABSOLUTE(.);');
          Add('  } > iram0_0_seg');
          Add('  ASSERT(((_iram_end - ORIGIN(iram0_0_seg)) <= LENGTH(iram0_0_seg)),');
          Add('          "IRAM0 segment data does not fit.")');
          Add('  .dram0.data :');
          Add('  {');
          Add('    _data_start = ABSOLUTE(.);');
          Add('    *(.gnu.linkonce.d.*)');
          Add('    *(.data1)');
          Add('    *(.sdata)');
          Add('    *(.sdata.*)');
          Add('    *(.gnu.linkonce.s.*)');
          Add('    *(.sdata2)');
          Add('    *(.sdata2.*)');
          Add('    *(.gnu.linkonce.s2.*)');
          Add('    *(.jcr)');
          Add('    *(.dram0 .dram0.*)');
          Add('    *( .data  .data.*  .dram1  .dram1.*)');
          Add('    *libspi_flash.a:spi_flash_raw.*( .rodata  .rodata.*)');
          Add('    *liblog.a:( .rodata  .rodata.*)');
          Add('    _data_end = ABSOLUTE(.);');
          Add('    . = ALIGN(4);');
          Add('  } > dram0_0_seg');
          Add('  .noinit (NOLOAD):');
          Add('  {');
          Add('    . = ALIGN(4);');
          Add('    _noinit_start = ABSOLUTE(.);');
          Add('    *(.noinit .noinit.*)');
          Add('    . = ALIGN(4) ;');
          Add('    _noinit_end = ABSOLUTE(.);');
          Add('  } > dram0_0_seg');
          Add('  .dram0.bss (NOLOAD) :');
          Add('  {');
          Add('    . = ALIGN (8);');
          Add('    _bss_start = ABSOLUTE(.);');
          Add('    *(EXCLUDE_FILE(*libcore.a *libfreertos.a:tasks.* *libfreertos.a:list.* *libfreertos.a:event_groups.* *libfreertos.a:stream_buffer.* *libfreertos.a:queue.* *libfreertos.a:timers.* *libfreertos.a:impure.* *libfreertos.a:freertos_hooks.* *liblwip.a) .bss EXCLUDE_FILE(*libcore.a *libfreertos.a:tasks.* *libfreertos.a:list.* *libfreertos.a:event_groups.* *libfreertos.a:stream_buffer.* *libfreertos.a:queue.* *libfreertos.a:timers.* *libfreertos.a:impure.* *libfreertos.a:freertos_hooks.* *liblwip.a) .bss.* EXCLUDE_FILE(*libcore.a *libfreertos.a:tasks.* *libfreertos.a:list.* *libfreertos.a:event_groups.* *libfreertos.a:stream_buffer.* *libfreertos.a:queue.* *libfreertos.a:timers.* *libfreertos.a:impure.* *libfreertos.a:freertos_hooks.* *liblwip.a) COMMON)');
          Add('    *(.dynsbss)');
          Add('    *(.sbss)');
          Add('    *(.sbss.*)');
          Add('    *(.gnu.linkonce.sb.*)');
          Add('    *(.scommon)');
          Add('    *(.sbss2)');
          Add('    *(.sbss2.*)');
          Add('    *(.gnu.linkonce.sb2.*)');
          Add('    *(.dynbss)');
          Add('    *(.share.mem)');
          Add('    *(.gnu.linkonce.b.*)');
          Add('    . = ALIGN (8);');
          Add('    _bss_end = ABSOLUTE(.);');
          Add('  } > dram0_0_seg');
          Add('  ASSERT(((_bss_end - ORIGIN(dram0_0_seg)) <= LENGTH(dram0_0_seg)),');
          Add('          "DRAM segment data does not fit.")');
          Add('  .flash.text :');
          Add('  {');
          Add('    _stext = .;');
          Add('    _text_start = ABSOLUTE(.);');
          Add('    *(EXCLUDE_FILE(*libphy.a *libspi_flash.a:spi_flash_raw.* *libpp.a) .literal EXCLUDE_FILE(*libphy.a *libspi_flash.a:spi_flash_raw.* *libpp.a) .literal.* EXCLUDE_FILE(*libphy.a *libspi_flash.a:spi_flash_raw.* *libpp.a) .text EXCLUDE_FILE(*libphy.a *libspi_flash.a:spi_flash_raw.* *libpp.a) .text.*  .wifi0iram  .wifi0iram.*)');
          Add('    *(.irom0.literal .irom0.text)');
          Add('    *(.irom.literal .irom.text .irom.text.literal)');
          Add('    *(.text2 .text2.* .literal2 .literal2.*)');
          Add('    *(.stub .gnu.warning .gnu.linkonce.literal.* .gnu.linkonce.t.*.literal .gnu.linkonce.t.*)');
          Add('    *(.irom0.text)');
          Add('    *(.fini.literal)');
          Add('    *(.fini)');
          Add('    *(.gnu.version)');
          Add('    _text_end = ABSOLUTE(.);');
          Add('    _etext = .;');
          Add('    _flash_cache_start = ABSOLUTE(0);');
          Add('  } >iram0_2_seg');
          Add('  .flash.rodata ALIGN(4) :');
          Add('  {');
          Add('    _rodata_start = ABSOLUTE(.);');
          Add('    . = 0x8;');
          Add('    *(.rodata_desc .rodata_desc.*)');
          Add('    *(.rodata_custom_desc .rodata_custom_desc.*)');
          Add('    *(.rodata2 .rodata2.*)');
          Add('    *(EXCLUDE_FILE(*libspi_flash.a:spi_flash_raw.* *liblog.a) .rodata EXCLUDE_FILE(*libspi_flash.a:spi_flash_raw.* *liblog.a) .rodata.*)');
          Add('    *(.irom1.text)');
          Add('    *(.gnu.linkonce.r.*)');
          Add('    *(.rodata1)');
          Add('    __XT_EXCEPTION_TABLE_ = ABSOLUTE(.);');
          Add('    *(.xt_except_table)');
          Add('    *(.gcc_except_table .gcc_except_table.*)');
          Add('    *(.gnu.linkonce.e.*)');
          Add('    *(.gnu.version_r)');
          Add('    . = (. + 3) & ~ 3;');
          Add('    __eh_frame = ABSOLUTE(.);');
          Add('    KEEP(*(.eh_frame))');
          Add('    . = (. + 7) & ~ 3;');
          Add('    __init_array_start = ABSOLUTE(.);');
          Add('    KEEP (*(EXCLUDE_FILE (*crtend.* *crtbegin.*) .ctors .ctors.*))');
          Add('    __init_array_end = ABSOLUTE(.);');
          Add('    KEEP (*crtbegin.*(.dtors))');
          Add('    KEEP (*(EXCLUDE_FILE (*crtend.*) .dtors))');
          Add('    KEEP (*(SORT(.dtors.*)))');
          Add('    KEEP (*(.dtors))');
          Add('    __XT_EXCEPTION_DESCS_ = ABSOLUTE(.);');
          Add('    *(.xt_except_desc)');
          Add('    *(.gnu.linkonce.h.*)');
          Add('    __XT_EXCEPTION_DESCS_END__ = ABSOLUTE(.);');
          Add('    *(.xt_except_desc_end)');
          Add('    *(.dynamic)');
          Add('    *(.gnu.version_d)');
          Add('    soc_reserved_memory_region_start = ABSOLUTE(.);');
          Add('    KEEP (*(.reserved_memory_address))');
          Add('    soc_reserved_memory_region_end = ABSOLUTE(.);');
          Add('    _rodata_end = ABSOLUTE(.);');
          Add('    _lit4_start = ABSOLUTE(.);');
          Add('    *(*.lit4)');
          Add('    *(.lit4.*)');
          Add('    *(.gnu.linkonce.lit4.*)');
          Add('    _lit4_end = ABSOLUTE(.);');
          Add('    . = ALIGN(4);');
          Add('    _thread_local_start = ABSOLUTE(.);');
          Add('    *(.tdata)');
          Add('    *(.tdata.*)');
          Add('    *(.tbss)');
          Add('    *(.tbss.*)');
          Add('    _thread_local_end = ABSOLUTE(.);');
          Add('    . = ALIGN(4);');
          Add('  } >iram0_2_seg');
          Add('}');
          Add('/* esp8266-rom.ld */');
          Add('PROVIDE ( SPI_sector_erase = 0x400040c0 );');
          Add('PROVIDE ( SPI_page_program = 0x40004174 );');
          Add('PROVIDE ( SPI_read_data = 0x400042ac );');
          Add('PROVIDE ( SPI_read_status = 0x400043c8 );');
          Add('PROVIDE ( SPI_write_status = 0x40004400 );');
          Add('PROVIDE ( SPI_write_enable = 0x4000443c );');
          Add('PROVIDE ( Wait_SPI_Idle = 0x4000448c );');
          Add('PROVIDE ( Enable_QMode = 0x400044c0 );');
          Add('PROVIDE ( Disable_QMode = 0x40004508 );');
          Add('PROVIDE ( Cache_Read_Enable = 0x40004678 );');
          Add('PROVIDE ( Cache_Read_Disable = 0x400047f0 );');
          Add('PROVIDE ( lldesc_build_chain = 0x40004f40 );');
          Add('PROVIDE ( lldesc_num2link = 0x40005050 );');
          Add('PROVIDE ( lldesc_set_owner = 0x4000507c );');
          Add('PROVIDE ( __adddf3 = 0x4000c538 );');
          Add('PROVIDE ( __addsf3 = 0x4000c180 );');
          Add('PROVIDE ( __divdf3 = 0x4000cb94 );');
          Add('PROVIDE ( __divdi3 = 0x4000ce60 );');
          Add('PROVIDE ( __divsi3 = 0x4000dc88 );');
          Add('PROVIDE ( __extendsfdf2 = 0x4000cdfc );');
          Add('PROVIDE ( __fixdfsi = 0x4000ccb8 );');
          Add('PROVIDE ( __fixunsdfsi = 0x4000cd00 );');
          Add('PROVIDE ( __fixunssfsi = 0x4000c4c4 );');
          Add('PROVIDE ( __floatsidf = 0x4000e2f0 );');
          Add('PROVIDE ( __floatsisf = 0x4000e2ac );');
          Add('PROVIDE ( __floatunsidf = 0x4000e2e8 );');
          Add('PROVIDE ( __floatunsisf = 0x4000e2a4 );');
          Add('PROVIDE ( __muldf3 = 0x4000c8f0 );');
          Add('PROVIDE ( __muldi3 = 0x40000650 );');
          Add('PROVIDE ( __mulsf3 = 0x4000c3dc );');
          Add('PROVIDE ( __subdf3 = 0x4000c688 );');
          Add('PROVIDE ( __subsf3 = 0x4000c268 );');
          Add('PROVIDE ( __truncdfsf2 = 0x4000cd5c );');
          Add('PROVIDE ( __udivdi3 = 0x4000d310 );');
          Add('PROVIDE ( __udivsi3 = 0x4000e21c );');
          Add('PROVIDE ( __umoddi3 = 0x4000d770 );');
          Add('PROVIDE ( __umodsi3 = 0x4000e268 );');
          Add('PROVIDE ( __umulsidi3 = 0x4000dcf0 );');
          Add('PROVIDE ( bzero = 0x4000de84 );');
          Add('PROVIDE ( memcmp = 0x4000dea8 );');
          Add('PROVIDE ( memcpy = 0x4000df48 );');
          Add('PROVIDE ( memmove = 0x4000e04c );');
          Add('PROVIDE ( memset = 0x4000e190 );');
          Add('PROVIDE ( strcmp = 0x4000bdc8 );');
          Add('PROVIDE ( strcpy = 0x4000bec8 );');
          Add('PROVIDE ( strlen = 0x4000bf4c );');
          Add('PROVIDE ( strncmp = 0x4000bfa8 );');
          Add('PROVIDE ( strncpy = 0x4000c0a0 );');
          Add('PROVIDE ( strstr = 0x4000e1e0 );');
          Add('PROVIDE ( gpio_input_get = 0x40004cf0 );');
          Add('PROVIDE ( gpio_pin_wakeup_disable = 0x40004ed4 );');
          Add('PROVIDE ( gpio_pin_wakeup_enable = 0x40004e90 );');
          Add('PROVIDE ( ets_io_vprintf = 0x40001f00 );');
          Add('PROVIDE ( uart_rx_one_char = 0x40003b8c );');
          Add('PROVIDE ( rom_i2c_readReg = 0x40007268 );');
          Add('PROVIDE ( rom_i2c_readReg_Mask = 0x4000729c );');
          Add('PROVIDE ( rom_i2c_writeReg = 0x400072d8 );');
          Add('PROVIDE ( rom_i2c_writeReg_Mask = 0x4000730c );');
          Add('PROVIDE ( rom_software_reboot = 0x40000080 );');
          Add('/* esp8266.peripherals.ld */');
          Add('PROVIDE ( GPIO = 0x60000300);');
          Add('PROVIDE ( uart0 = 0x60000000 );');
          Add('PROVIDE ( uart1 = 0x60000f00 );');
          Add('PROVIDE ( frc1 = 0x60000600 );');
          Add('PROVIDE ( rtc_sys_info = 0x60001100 );');
          Add('PROVIDE ( SLC0 = 0x60000B00 );');
          Add('PROVIDE ( I2S0 = 0x60000e00 );');
          Add('PROVIDE ( SPI1 = 0x60000100 );');
          Add('PROVIDE ( SPI0 = 0x60000200 );');
        end;
    end;
{$endif XTENSA}

  { Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;

end;


function TlinkerFreeRTOS.MakeExecutable:boolean;
var
  StaticStr,
  S,
  binstr,
  cmdstr,
  mapstr: Ansistring;
  success : boolean;
  GCSectionsStr,
  DynLinkStr,
  StripStr,
  FixedExeFileName: string;
  t: Text;
  hp: TCmdStrListItem;
  filepath: TCmdStr;
begin
  IDF_PATH:='';
{$ifdef xtensa}
  if (target_info.system=system_xtensa_freertos) then
    if (current_settings.controllertype = ct_esp32) then
      IDF_PATH := 'IDF_PATH'
    else
      IDF_PATH := 'IDF_PATH8266';
{$endif xtensa}

  { for future use }
  StaticStr:='';
  StripStr:='';
  mapstr:='';
  DynLinkStr:='';

  success:=true;
  Result:=false;

{$ifdef xtensa}
  if (target_info.system=system_xtensa_freertos) and (current_settings.controllertype = ct_esp32) then
    begin
      Info.ExeCmd[1]:='ld -g $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP $MAP '+
        '-u call_user_start_cpu0 -u ld_include_panic_highint_hdl -u esp_app_desc '+
        '-u vfs_include_syscalls_impl -u pthread_include_pthread_impl '+
        '-u pthread_include_pthread_cond_impl -u pthread_include_pthread_local_storage_impl '+
        '-u newlib_include_locks_impl -u newlib_include_heap_impl -u newlib_include_syscalls_impl '+
        '-u newlib_include_pthread_impl -u app_main -u uxTopUsedPriority '+
        '-L. -o $EXE -T $RES';
    end
  else  // esp8266
    begin
      Info.ExeCmd[1]:='ld -g $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP $MAP '+
       '-u call_user_start_cpu -u esp_app_desc '+
       '-L. -o $EXE -T $RES';
    end;
{$endif xtensa}

  Replace(Info.ExeCmd[1],'$'+IDF_PATH,maybequoted(GetEnvironmentVariable(IDF_PATH)));
  FixedExeFileName:=maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename,'.elf')));

  GCSectionsStr:='--gc-sections';
  //if not(cs_link_extern in current_settings.globalswitches) then
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename);

  if (cs_link_map in current_settings.globalswitches) then
    mapstr:='-Map '+maybequoted(ChangeFileExt(current_module.exefilename,'.map'));

{ Write used files and libraries }
  WriteResponseFile();

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  if target_info.system=system_xtensa_freertos then
    Replace(cmdstr,'$'+IDF_PATH,maybequoted(GetEnvironmentVariable(IDF_PATH)));
  if not(cs_link_on_target in current_settings.globalswitches) then
   begin
    Replace(cmdstr,'$EXE',FixedExeFileName);
    Replace(cmdstr,'$RES',(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
    Replace(cmdstr,'$STATIC',StaticStr);
    Replace(cmdstr,'$STRIP',StripStr);
    Replace(cmdstr,'$MAP',mapstr);
    Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
    Replace(cmdstr,'$DYNLINK',DynLinkStr);
   end
  else
   begin
    Replace(cmdstr,'$EXE',FixedExeFileName);
    Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
    Replace(cmdstr,'$STATIC',StaticStr);
    Replace(cmdstr,'$STRIP',StripStr);
    Replace(cmdstr,'$MAP',mapstr);
    Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
    Replace(cmdstr,'$DYNLINK',DynLinkStr);
   end;
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if success and not(cs_link_nolink in current_settings.globalswitches) then
   DeleteFile(outputexedir+Info.ResName);

{ Post process }
  if success and not(cs_link_nolink in current_settings.globalswitches) then
    success:=PostProcessExecutable(FixedExeFileName,false);

{$ifdef xtensa}
  if success and (target_info.system=system_xtensa_freertos) then
    if (current_settings.controllertype = ct_esp32) then
      begin
        binstr:='$'+IDF_PATH+'/components/esptool_py/esptool/esptool.py';
        Replace(binstr,'$'+IDF_PATH,maybequoted(GetEnvironmentVariable(IDF_PATH)));
        success:=DoExec(binstr,'--chip esp32 elf2image --flash_mode dio --flash_freq 40m '+
          '--flash_size '+tostr(embedded_controllers[current_settings.controllertype].flashsize div (1024*1024))+'MB '+
          '--elf-sha256-offset 0xb0 '+
          '-o '+maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename,'.bin')))+' '+
          FixedExeFileName,
          true,false);
      end
    else if (current_settings.controllertype = ct_esp8266) then
      begin
        binstr:='$'+IDF_PATH+'/components/esptool_py/esptool/esptool.py';
        Replace(binstr,'$'+IDF_PATH,maybequoted(GetEnvironmentVariable(IDF_PATH)));
        success:=DoExec(binstr,'--chip esp8266 elf2image --flash_mode dout --flash_freq 40m '+
          '--flash_size '+tostr(embedded_controllers[current_settings.controllertype].flashsize div (1024*1024))+'MB '+
          '--version=3 '+
          '-o '+maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename,'.bin')))+' '+
          FixedExeFileName,
          true,false);
      end
  else
{$endif xtensa}
    if success then
      success:=DoExec(FindUtil(utilsprefix+'objcopy'),'-O binary '+
        FixedExeFileName+' '+
        maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename,'.bin'))),true,false);
  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


function TlinkerFreeRTOS.postprocessexecutable(const fn : string;isdll:boolean):boolean;
  type
    TElf32header=packed record
      magic0123         : longint;
      file_class        : byte;
      data_encoding     : byte;
      file_version      : byte;
      padding           : array[$07..$0f] of byte;

      e_type            : word;
      e_machine         : word;
      e_version         : longint;
      e_entry           : longint;          { entrypoint }
      e_phoff           : longint;          { program header offset }

      e_shoff           : longint;          { sections header offset }
      e_flags           : longint;
      e_ehsize          : word;             { elf header size in bytes }
      e_phentsize       : word;             { size of an entry in the program header array }
      e_phnum           : word;             { 0..e_phnum-1 of entrys }
      e_shentsize       : word;             { size of an entry in sections header array }
      e_shnum           : word;             { 0..e_shnum-1 of entrys }
      e_shstrndx        : word;             { index of string section header }
    end;
    TElf32sechdr=packed record
      sh_name           : longint;
      sh_type           : longint;
      sh_flags          : longint;
      sh_addr           : longint;

      sh_offset         : longint;
      sh_size           : longint;
      sh_link           : longint;
      sh_info           : longint;

      sh_addralign      : longint;
      sh_entsize        : longint;
    end;

  function MayBeSwapHeader(h : telf32header) : telf32header;
    begin
      result:=h;
      if source_info.endian<>target_info.endian then
        with h do
          begin
            result.e_type:=swapendian(e_type);
            result.e_machine:=swapendian(e_machine);
            result.e_version:=swapendian(e_version);
            result.e_entry:=swapendian(e_entry);
            result.e_phoff:=swapendian(e_phoff);
            result.e_shoff:=swapendian(e_shoff);
            result.e_flags:=swapendian(e_flags);
            result.e_ehsize:=swapendian(e_ehsize);
            result.e_phentsize:=swapendian(e_phentsize);
            result.e_phnum:=swapendian(e_phnum);
            result.e_shentsize:=swapendian(e_shentsize);
            result.e_shnum:=swapendian(e_shnum);
            result.e_shstrndx:=swapendian(e_shstrndx);
          end;
    end;

  function MaybeSwapSecHeader(h : telf32sechdr) : telf32sechdr;
    begin
      result:=h;
      if source_info.endian<>target_info.endian then
        with h do
          begin
            result.sh_name:=swapendian(sh_name);
            result.sh_type:=swapendian(sh_type);
            result.sh_flags:=swapendian(sh_flags);
            result.sh_addr:=swapendian(sh_addr);
            result.sh_offset:=swapendian(sh_offset);
            result.sh_size:=swapendian(sh_size);
            result.sh_link:=swapendian(sh_link);
            result.sh_info:=swapendian(sh_info);
            result.sh_addralign:=swapendian(sh_addralign);
            result.sh_entsize:=swapendian(sh_entsize);
          end;
    end;

  var
    f : file;

  function ReadSectionName(pos : longint) : String;
    var
      oldpos : longint;
      c : char;
    begin
      oldpos:=filepos(f);
      seek(f,pos);
      Result:='';
      while true do
        begin
          blockread(f,c,1);
          if c=#0 then
            break;
          Result:=Result+c;
        end;
      seek(f,oldpos);
    end;

  var
    elfheader : TElf32header;
    secheader : TElf32sechdr;
    i : longint;
    stringoffset : longint;
    secname : string;
  begin
    postprocessexecutable:=false;
    { open file }
    assign(f,fn);
    {$push}{$I-}
    reset(f,1);
    if ioresult<>0 then
      Message1(execinfo_f_cant_open_executable,fn);
    { read header }
    blockread(f,elfheader,sizeof(tElf32header));
    elfheader:=MayBeSwapHeader(elfheader);
    seek(f,elfheader.e_shoff);
    { read string section header }
    seek(f,elfheader.e_shoff+sizeof(TElf32sechdr)*elfheader.e_shstrndx);
    blockread(f,secheader,sizeof(secheader));
    secheader:=MaybeSwapSecHeader(secheader);
    stringoffset:=secheader.sh_offset;

    seek(f,elfheader.e_shoff);
    status.datasize:=0;
    for i:=0 to elfheader.e_shnum-1 do
      begin
        blockread(f,secheader,sizeof(secheader));
        secheader:=MaybeSwapSecHeader(secheader);
        secname:=ReadSectionName(stringoffset+secheader.sh_name);
        if secname='.text' then
          begin
            Message1(execinfo_x_codesize,tostr(secheader.sh_size));
            status.codesize:=secheader.sh_size;
          end
        else if secname='.data' then
          begin
            Message1(execinfo_x_initdatasize,tostr(secheader.sh_size));
            inc(status.datasize,secheader.sh_size);
          end
        else if secname='.bss' then
          begin
            Message1(execinfo_x_uninitdatasize,tostr(secheader.sh_size));
            inc(status.datasize,secheader.sh_size);
          end;

      end;
    close(f);
    {$pop}
    if ioresult<>0 then
      ;
    postprocessexecutable:=true;
  end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef arm}
  RegisterLinker(ld_freertos,TlinkerFreeRTOS);
  RegisterTarget(system_arm_freertos_info);
{$endif arm}

{$ifdef avr}
  RegisterLinker(ld_freertos,TLinkerEmbedded);
  RegisterTarget(system_avr_embedded_info);
{$endif avr}

{$ifdef i386}
  RegisterLinker(ld_freertos,TLinkerEmbedded);
  RegisterTarget(system_i386_embedded_info);
{$endif i386}

{$ifdef x86_64}
  RegisterLinker(ld_freertos,TLinkerEmbedded);
  RegisterTarget(system_x86_64_embedded_info);
{$endif x86_64}

{$ifdef i8086}
  { no need to register linker ld_embedded, because i8086_embedded uses the
    regular msdos linker. In case a flat binary, relocated for a specific
    segment address is needed (e.g. for a BIOS or a real mode bootloader), it
    can be produced post-compilation with exe2bin or a similar tool. }
  RegisterTarget(system_i8086_embedded_info);
{$endif i8086}

{$ifdef mipsel}
  RegisterLinker(ld_freertos,TLinkerEmbedded);
  RegisterTarget(system_mipsel_embedded_info);
{$endif mipsel}

{$ifdef m68k}
  RegisterLinker(ld_freertos,TLinkerEmbedded);
  RegisterTarget(system_m68k_embedded_info);
{$endif m68k}

{$ifdef riscv32}
  RegisterLinker(ld_freertos,TLinkerEmbedded);
  RegisterTarget(system_riscv32_embedded_info);
{$endif riscv32}

{$ifdef riscv64}
  RegisterLinker(ld_freertos,TLinkerEmbedded);
  RegisterTarget(system_riscv64_embedded_info);
{$endif riscv64}

{$ifdef xtensa}
  RegisterLinker(ld_freertos,TlinkerFreeRTOS);
  RegisterTarget(system_xtensa_freertos_info);
{$endif xtensa}
end.
