{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for FreeBSD/NetBSD

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
{ This unit implements support information structures for FreeBSD/NetBSD.
  OpenBSD and Darwin must be added still.}

unit i_bsd;

  interface

    uses
       systems;

    const
       system_i386_freebsd_info : tsysteminfo =
          (
            system       : system_i386_FreeBSD;
            name         : 'FreeBSD/ELF for i386';
            shortname    : 'FreeBSD';
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : 'BSDUNITS';
            extradefines : 'UNIX;BSD;HASUNIX';
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
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize    : 256*1024;
            stacksize   : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_i386_netbsd_info : tsysteminfo =
          (
            system       : system_i386_NetBSD;
            name         : 'NetBSD for i386';
            shortname    : 'NetBSD';
            flags        : [tf_under_development];
            cpu          : cpu_i386;
            unit_env     : 'BSDUNITS';
            extradefines : 'UNIX;BSD;HASUNIX';
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
            Cprefix      : '_';
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
                constalignmax   : 1;
                varalignmin     : 0;
                varalignmax     : 1;
                localalignmin   : 0;
                localalignmax   : 1;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize    : 256*1024;
            stacksize   : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_m68k_netbsd_info : tsysteminfo =
          (
            system       : system_m68k_NetBSD;
            name         : 'NetBSD for m68k';
            shortname    : 'NetBSD';
            flags        : [tf_under_development];
            cpu          : cpu_m68k;
            unit_env     : 'BSDUNITS';
            extradefines : 'UNIX;BSD;HASUNIX';
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
                constalignmax   : 1;
                varalignmin     : 0;
                varalignmax     : 1;
                localalignmin   : 0;
                localalignmax   : 1;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize    : 256*1024;
            stacksize   : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_powerpc_netbsd_info : tsysteminfo =
          (
            system       : system_powerpc_netbsd;
            name         : 'NetBSD for PowerPC';
            shortname    : 'NetBSD';
            flags        : [];
            cpu          : cpu_powerpc;
            unit_env     : '';
            extradefines : 'UNIX;BSD;HASUNIX';
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
            staticlibext : '.s';
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
                localalignmin   : 0;
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
            { abi_powerpc_sysv doesn't work yet }
            abi : abi_powerpc_aix;
          );


       system_powerpc_darwin_info  : tsysteminfo =
          (
            system       : system_powerpc_darwin;
            name         : 'Darwin for PowerPC';
            shortname    : 'Darwin';
            flags        : [tf_under_development];
            cpu          : cpu_powerpc;
            unit_env     : 'BSDUNITS';
            extradefines : 'UNIX;BSD;HASUNIX';
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
                procalign       : 16;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 1;
                varalignmin     : 0;
                varalignmax     : 1;
                localalignmin   : 0;
                localalignmax   : 1;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize    : 256*1024;
            stacksize   : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_powerpc_aix;
          );

  implementation

initialization
{$ifdef cpu86}
  {$ifdef FreeBSD}
     set_source_info(system_i386_FreeBSD_info);
  {$endif}
  {$ifdef NetBSD}
     set_source_info(system_i386_NetBSD_info);
  {$endif}
{$endif cpu86}
{$ifdef cpu68}
  {$ifdef NetBSD}
     set_source_info(system_m68k_NetBSD_info);
  {$endif NetBSD}
{$endif cpu68}
{$ifdef cpupowerpc}
  {$ifdef Darwin}
     set_source_info(system_powerpc_darwin_info);
  {$endif Darwin}
  {$ifdef NetBSD}
     set_source_info(system_powerpc_netbsd_info);
  {$endif}
{$endif cpu68}
end.
{
  $Log$
  Revision 1.4  2003-10-03 22:09:49  peter
    * removed paraalign

  Revision 1.3  2003/05/28 23:18:31  florian
    * started to fix and clean up the sparc port

  Revision 1.2  2003/05/25 23:15:04  marco
   * NetBSD target support. OpenBSD reserved in the enum, for future use.

  Revision 1.1  2003/05/20 23:54:00  florian
    + basic darwin support added

  Revision 1.2  2003/01/11 16:35:15  marco
   * HASUNIX defined for now.

  Revision 1.1  2002/09/06 15:03:51  carl
    * moved files to systems directory

  Revision 1.3  2002/08/13 18:01:51  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.2  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.1  2002/07/26 21:15:38  florian
    * rewrote the system handling
}
