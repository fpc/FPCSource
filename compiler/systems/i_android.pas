{
    Copyright (c) 1998-2012 by Peter Vreman

    This unit implements support information structures for Android

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
{ This unit implements support information structures for android. }
unit i_android;

{$i fpcdefs.inc}

  interface

    uses
       systems, rescmn;

    const
       system_arm_android_info : tsysteminfo =
          (
            system       : system_arm_Android;
            name         : 'Android for ARMEL';
            shortname    : 'Android';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment, tf_safecall_exceptions,
                            tf_pic_uses_got, tf_pic_default,
                            tf_smartlink_sections,tf_has_winlike_resources];
            cpu          : cpu_arm;
            unit_env     : 'ANDROIDUNITS';
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
            linkextern   : ld_android;
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
            abi : abi_eabi;
            llvmdatalayout : 'e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32-S64';
          );

       system_i386_android_info : tsysteminfo =
          (
            system       : system_i386_ANDROID;
            name         : 'Android for i386';
            shortname    : 'Android';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_needs_dwarf_cfi,tf_has_winlike_resources,
                            tf_pic_uses_got, tf_pic_default, tf_smartlink_sections,
                            tf_safecall_exceptions];
            cpu          : cpu_i386;
            unit_env     : 'ANDROIDUNITS';
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
            linkextern   : ld_android;
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
            abi : abi_default;
            llvmdatalayout : 'e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128';
          );

        system_aarch64_android_info  : tsysteminfo =
           (
             system       : system_aarch64_android;
             name         : 'Android for AArch64';
             shortname    : 'Android';
             flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                             tf_requires_proper_alignment, tf_safecall_exceptions,
                             tf_pic_uses_got, tf_pic_default,
                             tf_smartlink_sections,tf_has_winlike_resources];
             cpu          : cpu_aarch64;
             unit_env     : 'ANDROIDUNITS';
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
             linkextern   : ld_android;
             ar           : ar_gnu_ar;
             res          : res_elf;
             dbg          : dbg_dwarf2;
             script       : script_unix;
             endian       : endian_little;
             alignment    :
               (
                 procalign       : 8;
                 loopalign       : 4;
                 jumpalign       : 0;
                 constalignmin   : 0;
                 constalignmax   : 16;
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
             abi : abi_default;
             llvmdatalayout : 'e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-n32:64-S128'
           );

        system_x86_64_android_info : tsysteminfo =
           (
             system       : system_x86_64_android;
             name         : 'Android for x86-64';
             shortname    : 'Android';
             flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                             tf_needs_dwarf_cfi,tf_has_winlike_resources,
                             {tf_pic_uses_got,} tf_pic_default, tf_smartlink_sections,
                             tf_safecall_exceptions, tf_safecall_clearstack];
             cpu          : cpu_x86_64;
             unit_env     : 'ANDROIDUNITS';
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
             linkextern   : ld_android;
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
                 constalignmax   : 16;
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
             abi : abi_default;
             llvmdatalayout : 'e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128';
           );

       system_mipsel_android_info : tsysteminfo =
          (
            system       : system_mipsel_android;
            name         : 'Android for MIPSEL';
            shortname    : 'Android';
            flags        : [tf_needs_symbol_size,tf_needs_symbol_type,tf_files_case_sensitive,
                            tf_requires_proper_alignment,tf_pic_default,tf_safecall_exceptions,
                            tf_smartlink_sections,tf_has_winlike_resources];
            cpu          : cpu_mipsel;
            unit_env     : 'ANDROIDUNITS';
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
            linkextern   : ld_android;
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
            abi : abi_default;
            llvmdatalayout : 'e-p:32:32:32-i1:8:8-i8:8:32-i16:16:32-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-n32-S64';
          );

implementation

initialization
{$ifdef CPUI386}
  {$ifdef android}
    set_source_info(system_i386_android_info);
  {$endif}
{$endif CPUI386}
{$ifdef CPUX86_64}
  {$ifdef android}
    set_source_info(system_x86_64_android_info);
  {$endif}
{$endif CPUX86_64}
{$ifdef CPUARM}
  {$ifdef android}
    set_source_info(system_arm_android_info);
  {$endif android}
{$endif CPUARM}
{$ifdef CPUAARCH64}
  {$ifdef android}
    set_source_info(system_aarch64_android_info);
  {$endif android}
{$endif CPUAARCH64}
{$ifdef CPUMIPSEL}
  {$ifdef android}
    set_source_info(system_mipsel_android_info);
  {$endif android}
{$endif CPUMIPSEL}
end.
