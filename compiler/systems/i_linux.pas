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
            flags        : [];
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
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_m68k_linux_info : tsysteminfo =
          (
            system       : system_m68k_linux;
            name         : 'Linux for m68k';
            shortname    : 'linux';
            flags        : [];
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
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 128*1024;
            stacksize    : 32*1024*1024;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_powerpc_linux_info : tsysteminfo =
          (
            system       : system_powerpc_LINUX;
            name         : 'Linux for PowerPC';
            shortname    : 'linux';
            flags        : [];
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

       system_alpha_linux_info : tsysteminfo =
          (
            system       : system_alpha_LINUX;
            name         : 'Linux for Alpha';
            shortname    : 'linux';
            flags        : [];
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
            use_function_relative_addresses : true
          );

       system_x86_64_linux_info : tsysteminfo =
          (
            system       : system_x86_64_LINUX;
            name         : 'Linux for x86-64';
            shortname    : 'linux';
            flags        : [];
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
                constalignmax   : 1;
                varalignmin     : 0;
                varalignmax     : 1;
                localalignmin   : 0;
                localalignmax   : 1;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 16;
            heapsize     : 256*1024;
            stacksize    : 16*1024;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_SPARC_linux_info : tsysteminfo =
          (
            system       : system_SPARC_Linux;
            name         : 'Linux for SPARC';
            shortname    : 'linux';
            flags        : [];
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
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 4
              );
            first_parm_offset : (16+1)*4;
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_arm_linux_info : tsysteminfo =
          (
            system       : system_arm_Linux;
            name         : 'Linux for ARM';
            shortname    : 'linux';
            flags        : [];
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
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 4;
                recordalignmax  : 4;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
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
{$ifdef CPU86_64}
  {$ifdef linux}
    set_source_info(system_x86_64_linux_info);
  {$endif linux}
{$endif CPU86_64}
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
  Revision 1.15  2003-10-03 22:09:49  peter
    * removed paraalign

  Revision 1.14  2003/09/23 17:58:38  peter
    * recordminalign for sparc 0

  Revision 1.13  2003/09/07 09:08:26  olle
    * reverted last change, linux on powerpc still uses aix abi

  Revision 1.12  2003/09/06 10:46:56  olle
    * linux on powerpc now uses sysv abi

  Revision 1.11  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.10  2003/07/21 11:52:57  florian
    * very basic stuff for the arm

  Revision 1.9  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.8  2003/05/31 18:14:06  jonas
    * add default system for ppc

  Revision 1.7  2003/05/19 12:15:28  florian
    * fixed calling sequence for subroutines using the aix abi

  Revision 1.6  2003/05/18 15:15:59  florian
    + added abi field to tsysteminfo

  Revision 1.5  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.4  2003/02/06 22:36:55  mazen
  * fixing bug related to errornous program main entry stack frame

  Revision 1.3  2003/01/11 16:35:15  marco
   * HASUNIX defined for now.

  Revision 1.2  2002/10/04 21:57:42  mazen
  * register allocation for parameters now done in cpupara, but InternalError(200109223) in cgcpu.pas:1053 is still not fixed du to location_force problem in ncgutils.pas:419

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
