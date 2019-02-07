{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for arosOS

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
{ This unit implements support information structures for the arosOS. }
unit i_aros;

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const
       system_i386_aros_info : tsysteminfo =
          (
            system       : system_i386_aros;
            name         : 'AROS for i386';
            shortname    : 'AROS';
            flags        : [tf_files_case_aware, tf_smartlink_sections, tf_has_winlike_resources];
            cpu          : cpu_i386;
            unit_env     : 'AROSUNITS';
            extradefines : 'HASAMIGA;AROS_ABIv0';
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
            sharedlibext : '.library';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.library';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_i386_elf32;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_aros;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_amiga;
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
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 16;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 262144;
            stackalign   : 4;
            abi : abi_default;
            llvmdatalayout : 'todo';
          );

       system_x86_64_aros_info : tsysteminfo =
          (
            system       : system_x86_64_aros;
            name         : 'AROS for x86_64';
            shortname    : 'AROS';
            flags        : [tf_files_case_aware, tf_smartlink_sections, tf_has_winlike_resources];
            cpu          : cpu_x86_64;
            unit_env     : 'AROSUNITS';
            extradefines : 'HASAMIGA;AROS_BINCOMPAT;AROS_ABIv1';
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
            sharedlibext : '.library';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.library';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_x86_64_elf64;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_aros;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_amiga;
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
            stacksize    : 1*1024*1024;
            stackalign   : 16; { fix me: this is a wild guess for now (KB) }
            abi : abi_default;
            llvmdatalayout : 'todo';
          );
       system_arm_aros_info : tsysteminfo =
          (
            system       : system_arm_aros;
            name         : 'AROS for ARM';
            shortname    : 'AROS';
            flags        : [tf_files_case_aware, tf_smartlink_sections, tf_has_winlike_resources];
            cpu          : cpu_arm;
            unit_env     : 'AROSUNITS';
            extradefines : 'HASAMIGA;AROS_BINCOMPAT;AROS_ABIv0';
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
            sharedlibext : '.library';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.library';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_arm_elf32;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_aros;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_stabs;
            script       : script_amiga;
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
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 16;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 262144;
            stackalign   : 4;
            abi : abi_default;
            llvmdatalayout : 'todo';
          );

  implementation

initialization
{$ifdef CPU86}
  {$ifdef aros}
    set_source_info(system_i386_aros_info);
  {$endif aros}
{$endif CPU86}
{$ifdef CPUX86_64}
  {$ifdef AROS}
    set_source_info(system_x86_64_aros_info);
  {$endif AROS}
{$endif CPUX86_64}
{$ifdef CPUARM}
  {$ifdef AROS}
    set_source_info(system_arm_aros_info);
  {$endif AROS}
{$endif CPUX86_64}

end.
