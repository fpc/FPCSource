{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for Netware modules

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
{ This unit implements support information structures for Netware modules. }
unit i_nwm;

  interface

    uses
       systems;

    const
       system_i386_netware_info : tsysteminfo =
          (
            system       : system_i386_netware;
            name         : 'Netware for i386';
            shortname    : 'Netware';
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : 'NETWAREUNITS';
            extradefines : '';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.nlm';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppn';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.on';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.nlm';
            staticlibext : '.a';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.nlm';
            staticClibext : '.a';
            staticClibprefix : '';
            sharedClibprefix : '';
            Cprefix      : '';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
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
                paraalign       : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 8192;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

  implementation

initialization
{$ifdef CPU86}
  {$ifdef netware}
    set_source_info(target_i386_netware_info);
  {$endif netware}
{$endif CPU86}
end.
{
  $Log$
  Revision 1.2  2002-08-12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.1  2002/07/26 21:15:38  florian
    * rewrote the system handling
}
