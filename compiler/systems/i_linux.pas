{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

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

  interface

    uses
       systems;

    const
       system_i386_linux_info : tsysteminfo =
          (
            system       : system_i386_LINUX;
            name         : 'Linux for i386';
            shortname    : 'Linux';
            flags        : [tf_needs_symbol_size{,tf_smartlink_sections}];
            cpu          : cpu_i386;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_i386_elf32;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
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
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_default
          );

       system_m68k_linux_info : tsysteminfo =
          (
            system       : system_m68k_linux;
            name         : 'Linux for m68k';
            shortname    : 'linux';
            flags        : [tf_needs_symbol_size];
            cpu          : cpu_m68k;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_gas;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
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
            heapsize     : 128*1024;
            stacksize    : 32*1024*1024;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_default
          );

       system_powerpc_linux_info : tsysteminfo =
          (
            system       : system_powerpc_LINUX;
            name         : 'Linux for PowerPC';
            shortname    : 'linux';
            flags        : [tf_needs_symbol_size];
            cpu          : cpu_powerpc;
            unit_env     : '';
            extradefines : 'UNIX;HASUNIX';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_gas;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
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
            heapsize     : 256*1024;
            stacksize    : 32*1024*1024;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_powerpc_sysv;
          );

       system_alpha_linux_info : tsysteminfo =
          (
            system       : system_alpha_LINUX;
            name         : 'Linux for Alpha';
            shortname    : 'linux';
            flags        : [tf_needs_symbol_size];
            cpu          : cpu_alpha;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_gas;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
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
            heapsize     : 256*1024;
            stacksize    : 32*1024*1024;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_default
          );

       system_x86_64_linux_info : tsysteminfo =
          (
            system       : system_x86_64_LINUX;
            name         : 'Linux for x86-64';
            shortname    : 'linux';
            flags        : [tf_needs_symbol_size,tf_needs_dwarf_cfi];
            cpu          : cpu_x86_64;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_gas;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 8;
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
            first_parm_offset : 16;
            heapsize     : 256*1024;
            stacksize    : 256*1024;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_default
          );

       system_sparc_linux_info : tsysteminfo =
          (
            system       : system_SPARC_Linux;
            name         : 'Linux for SPARC';
            shortname    : 'linux';
            flags        : [tf_needs_symbol_size];
            cpu          : cpu_SPARC;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_gas;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
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
            first_parm_offset : 92;
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_default
          );

       system_arm_linux_info : tsysteminfo =
          (
            system       : system_arm_Linux;
            name         : 'Linux for ARM';
            shortname    : 'linux';
            flags        : [tf_needs_symbol_size];
            cpu          : cpu_arm;
            unit_env     : 'LINUXUNITS';
            extradefines : 'UNIX;HASUNIX';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_gas;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
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
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_default
          );

  implementation

initialization
{$ifdef CPU86}
  {$ifdef linux}
    { some FreeBSD versions define linux as well }
    {$ifndef FreeBSD}
      set_source_info(system_i386_linux_info);
    {$endif FreeBSD}
  {$endif}
{$endif CPU86}
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
{$ifdef CPUPOWERPC}
  {$ifdef linux}
    set_source_info(system_powerpc_linux_info);
  {$endif linux}
{$endif CPUPOWERPC}
{$ifdef CPUARM}
  {$ifdef linux}
    set_source_info(system_arm_linux_info);
  {$endif linux}
{$endif CPUARM}
end.
{
  $Log$
  Revision 1.30  2004-10-24 13:36:26  peter
    * gc-sections added when section smartlinking is used

  Revision 1.29  2004/08/15 13:30:18  florian
    * fixed alignment of variant records
    * more alignment problems fixed

  Revision 1.28  2004/06/29 21:00:08  peter
    * only enable dwarf for supported platforms

  Revision 1.27  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.26  2004/06/16 20:07:11  florian
    * dwarf branch merged

  Revision 1.25.2.5  2004/05/31 22:08:21  peter
    * fix passing of >6 arguments

  Revision 1.25.2.4  2004/05/19 21:34:58  peter
    * disable section smartlink, it is not working stable

  Revision 1.25.2.3  2004/05/10 21:28:34  peter
    * section_smartlink enabled for gas under linux

  Revision 1.25.2.2  2004/05/02 14:17:27  peter
    * fix cpux86_64 ifdef

}
