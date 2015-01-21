{
    Copyright (c) 1998-2008 by Peter Vreman

    This unit implements support information structures for linux

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
{ This unit implements support information structures for linux. }
unit i_linux;

{$i fpcdefs.inc}

  interface

    uses
       systems, rescmn;

    const
       system_i386_linux_info : tsysteminfo =
          (
            system       : system_i386_LINUX;
            name         : 'Linux for i386';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_pic_uses_got,tf_smartlink_sections{,tf_winlikewidestring},
{$ifdef segment_threadvars}
                            tf_section_threadvars,
{$endif segment_threadvars}
                            tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_smartlink_library,tf_needs_dwarf_cfi,tf_has_winlike_resources,
                            tf_safecall_exceptions, tf_safecall_clearstack];
            cpu          : cpu_i386;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_i386_elf32;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 16;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 8;
                varalignmin     : 0;
                varalignmax     : 16;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 16;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 8*1024*1024;
            stackalign   : 4;
            abi : abi_default
          );

       system_x86_6432_linux_info : tsysteminfo =
          (
            system       : system_x86_6432_LINUX;
            name         : 'Linux for x64_6432';
            shortname    : 'Linux6432';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_pic_uses_got,tf_smartlink_sections,
                            tf_smartlink_library,tf_has_winlike_resources];
            cpu          : cpu_x86_64;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_i386_elf32;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 16;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 16;
                varalignmin     : 0;
                varalignmax     : 16;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 16;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 8*1024*1024;
            stackalign   : 16;
            abi : abi_default
          );

       system_m68k_linux_info : tsysteminfo =
          (
            system       : system_m68k_linux;
            name         : 'Linux for m68k';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_smartlink_sections,
                            tf_requires_proper_alignment, { Coldfire seems to need this at least (KB) }
                            tf_smartlink_library,tf_has_winlike_resources];
            cpu          : cpu_m68k;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 32*1024*1024;
            stackalign   : 4;
            abi : abi_default
          );

       system_powerpc_linux_info : tsysteminfo =
          (
            system       : system_powerpc_LINUX;
            name         : 'Linux for PowerPC';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_smartlink_sections,
                            tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_smartlink_library,tf_has_winlike_resources];
            cpu          : cpu_powerpc;
            unit_env     : '';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 8
              );
            first_parm_offset : 8;
            stacksize    : 32*1024*1024;
            stackalign   : 16;
            abi : abi_powerpc_sysv;
          );

       system_powerpc64_linux_info : tsysteminfo =
          (
            system       : system_powerpc64_LINUX;
            name         : 'Linux for PowerPC64';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment,tf_smartlink_sections,tf_has_winlike_resources];
            cpu          : cpu_powerpc64;
            unit_env     : '';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_dwarf2;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 8;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 4;
                constalignmax   : 16;
                varalignmin     : 4;
                varalignmax     : 16;
                localalignmin   : 0;
                localalignmax   : 16;
                recordalignmin  : 0;
                recordalignmax  : 16;
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            stacksize    : 10*1024*1024;
            stackalign   : 16;
            abi : abi_powerpc_sysv
          );

       system_alpha_linux_info : tsysteminfo =
          (
            system       : system_alpha_LINUX;
            name         : 'Linux for Alpha';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_smartlink_library,tf_has_winlike_resources];
            cpu          : cpu_alpha;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 32*1024*1024;
            stackalign   : 8;  { ??? }
            abi : abi_default
          );

       system_x86_64_linux_info : tsysteminfo =
          (
            system       : system_x86_64_LINUX;
            name         : 'Linux for x86-64';
            shortname    : 'Linux';
            flags        : [tf_smartlink_sections,tf_needs_symbol_size,tf_needs_dwarf_cfi,tf_smartlink_library,
                            tf_library_needs_pic,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_has_winlike_resources,tf_safecall_exceptions,tf_safecall_clearstack];
            cpu          : cpu_x86_64;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_x86_64_elf64;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_dwarf2;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 16;
                loopalign       : 8;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 8;
                varalignmin     : 0;
                varalignmax     : 16;
                localalignmin   : 4;
                localalignmax   : 16;
                recordalignmin  : 0;
                recordalignmax  : 16;
                maxCrecordalign : 16
              );
            first_parm_offset : 16;
            stacksize    : 8*1024*1024;
            stackalign   : 16;
            abi : abi_default
          );

       system_sparc_linux_info : tsysteminfo =
          (
            system       : system_SPARC_Linux;
            name         : 'Linux for SPARC';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_library_needs_pic,tf_smartlink_sections,
                            tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_smartlink_library,tf_pic_uses_got,
                            tf_requires_proper_alignment,tf_safecall_exceptions, tf_safecall_clearstack,
                            tf_has_winlike_resources];
            cpu          : cpu_SPARC;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 4;
                constalignmax   : 8;
                varalignmin     : 4;
                varalignmax     : 8;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 92;
            stacksize    : 8*1024*1024;
            stackalign   : 8;
            abi : abi_default
          );

{$ifdef FPC_ARMHF}
       system_arm_linux_info : tsysteminfo =
          (
            system       : system_arm_Linux;
            name         : 'Linux for ARMHF';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_smartlink_library,tf_pic_uses_got,
                            tf_has_winlike_resources];
            cpu          : cpu_arm;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX;CPUARMHF';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 8;
                varalignmin     : 0;
                varalignmax     : 8;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 8;
            stacksize    : 8*1024*1024;
            stackalign   : 8;
            abi : abi_eabihf
          );
{$else FPC_ARMHF}
{$ifdef FPC_ARMEL}
       system_arm_linux_info : tsysteminfo =
          (
            system       : system_arm_Linux;
            name         : 'Linux for ARMEL';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_smartlink_library,tf_pic_uses_got,
                            tf_has_winlike_resources];
            cpu          : cpu_arm;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX;CPUARMEL';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 8;
                varalignmin     : 0;
                varalignmax     : 8;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 8;
            stacksize    : 8*1024*1024;
            stackalign   : 8;
            abi : abi_eabi
          );
{$else FPC_ARMEL}
{$ifdef FPC_ARMEB}
       system_arm_linux_info : tsysteminfo =
          (
            system       : system_arm_Linux;
            name         : 'Linux for ARMEB';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_smartlink_library,tf_pic_uses_got,
                            tf_has_winlike_resources];
            cpu          : cpu_arm;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX;CPUARMEB';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 8*1024*1024;
            stackalign   : 4;
            abi : abi_default
          );
{$else FPC_ARMEB}
       system_arm_linux_info : tsysteminfo =
          (
            system       : system_arm_Linux;
            name         : 'Linux for ARM';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_smartlink_library,tf_has_winlike_resources];
            cpu          : cpu_arm;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 8*1024*1024;
            stackalign   : 4;
            abi : abi_default
          );
{$endif FPC_ARMEB}
{$endif FPC_ARMEL}
{$endif FPC_ARMHF}

       system_mipseb_linux_info : tsysteminfo =
          (
            system       : system_mipseb_LINUX;
            name         : 'Linux for MIPSEB';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment,tf_pic_uses_got,tf_safecall_exceptions,
                            tf_smartlink_sections,tf_smartlink_library,tf_has_winlike_resources];
            cpu          : cpu_mipseb;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
//            p_ext_support : false;
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 8;
                varalignmin     : 0;
                varalignmax     : 8;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 0;
            stacksize    : 32*1024*1024;
            stackalign   : 8;
            abi : abi_default
          );

       system_mipsel_linux_info : tsysteminfo =
          (
            system       : system_mipsel_LINUX;
            name         : 'Linux for MIPSEL';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment,tf_pic_uses_got,tf_safecall_exceptions,
                            tf_smartlink_sections,tf_smartlink_library,tf_has_winlike_resources];
            cpu          : cpu_mipsel;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
//            p_ext_support : false;
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_linux;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 8;
                varalignmin     : 0;
                varalignmax     : 8;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 0;
            stacksize    : 32*1024*1024;
            stackalign   : 8;
            abi : abi_default
          );

  implementation

initialization
{$ifdef CPUI386}
  {$ifdef linux}
    { some FreeBSD versions define linux as well }
    {$ifndef FreeBSD}
      set_source_info(system_i386_linux_info);
    {$endif FreeBSD}
  {$endif}
{$endif CPUI386}
{$ifdef CPU68}
  {$ifdef linux}
    set_source_info(system_m68k_linux_info);
  {$endif linux}
{$endif CPU68}
{$ifdef CPUX86_64}
  {$ifdef linux}
    set_source_info(system_x86_64_linux_info);
  {$endif linux}
{$endif CPUX86_64}
{$ifdef CPUALPHA}
  {$ifdef linux}
    set_source_info(system_alpha_linux_info);
  {$endif linux}
{$endif CPUALPHA}
{$ifdef CPUSPARC}
  {$ifdef linux}
    set_source_info(system_sparc_linux_info);
  {$endif linux}
{$endif CPUSPARC}
{$ifdef CPUPOWERPC32}
  {$ifdef linux}
    set_source_info(system_powerpc_linux_info);
  {$endif linux}
{$endif CPUPOWERPC32}
{$ifdef CPUPOWERPC64}
  {$ifdef linux}
    set_source_info(system_powerpc64_linux_info);
  {$endif linux}
{$endif CPUPOWERPC64}
{$ifdef CPUARM}
  {$ifdef linux}
    set_source_info(system_arm_linux_info);
  {$endif linux}
{$endif CPUARM}
{$ifdef CPUMIPSEB}
  {$ifdef linux}
    set_source_info(system_mipseb_linux_info);
  {$endif linux}
{$endif CPUMIPSEB}
{$ifdef CPUMIPSEL}
  {$ifdef linux}
    set_source_info(system_mipsel_linux_info);
  {$endif linux}
{$endif CPUMIPSEL}
end.
