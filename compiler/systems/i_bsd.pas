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
  OpenBSD is only added for i386 for now, though it exists for most
  other common CPU's too}

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
            stacksize   : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );


       system_x86_64_freebsd_info : tsysteminfo =
          (
            system       : system_x86_64_freebsd;
            name         : 'FreeBSD for x86-64';
            shortname    : 'FreeBSD';
            flags        : [tf_needs_symbol_size,tf_smartlink_sections];
            cpu          : cpu_x86_64;
            unit_env     : 'BSDUNITS';
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
            stacksize    : 256*1024;
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
            stacksize   : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_i386_openbsd_info : tsysteminfo =
          (
            system       : system_i386_OpenBSD;
            name         : 'OpenBSD for i386';
            shortname    : 'OpenBSD';
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
            stacksize   : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_powerpc_netbsd_info : tsysteminfo =
          (
            system       : system_powerpc_netbsd;
            name         : 'NetBSD for PowerPC';
            shortname    : 'NetBSD';
            flags        : [tf_under_development];
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
                maxCrecordalign : 4     // should be 8 probably
              );
            first_parm_offset : 8;
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
            flags        : [];
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
            sharedlibext : '.dylib';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.dylib';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_darwin;
            assemextern  : as_darwin;
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
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 24;
            stacksize   : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : false;
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
  {$ifdef OpenBSD}
     set_source_info(system_i386_NetBSD_info);
  {$endif}
{$endif cpu86}
{$ifdef cpux86_64}
   {$ifdef FreeBSD}
     set_source_info(system_x86_64_FreeBSD_info);
   {$endif}
{$endif}
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
  Revision 1.14  2004-10-25 15:38:41  peter
    * heap and heapsize removed
    * checkpointer fixes

  Revision 1.13  2004/09/12 20:33:21  olle
    * Updated compiler status

  Revision 1.12  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.11  2004/06/16 20:07:11  florian
    * dwarf branch merged

  Revision 1.10.2.1  2004/05/11 17:07:55  marco
   * x86_64 freebsd target support

  Revision 1.10  2004/03/29 21:17:39  florian
    * fixed dyn. lib extension on darwin

  Revision 1.9  2004/02/08 20:03:05  jonas
    * corrected first_param_offset for darwin

  Revision 1.8  2004/01/29 15:48:10  marco
   * fix added as comment till I can test (8 byte C alignment on netbsd/ppc)

}
