{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for MacOS

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
{ This unit implements support information structures for MacOS. }
unit i_macos;

  interface

    uses
       systems;
     const
       system_powerpc_macos_info : tsysteminfo =
          (
            system       : system_powerpc_MACOS;
            name         : 'MacOs (PowerPC)';
            shortname    : 'MacOs/PPC';
            flags        : [];
            cpu          : cpu_powerpc;
            short_name   : 'MACOS';
            unit_env     : '';
            extradefines : '';
            sharedlibext : 'Lib';
            staticlibext : 'Lib';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '';
            smartext     : '.sl';
            unitext      : '.ppt';
            unitlibext   : '.ppl';
            asmext       : '.a';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '';
            newline      : #13;
            assem        : as_powerpc_mpw;
            assemextern  : as_powerpc_mpw;
            link         : ld_powerpc_macos;
            linkextern   : ld_powerpc_macos;
            ar           : ar_powerpc_ar;
            res          : res_powerpc_mpw;
            script       : script_unix;
            endian       : endian_big;
            stackalignment : 8;
            maxCrecordalignment : 32;
            size_of_longint : 4;
            heapsize     : 256*1024;
            stacksize    : 8192;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

  implementation

initialization
{$ifdef cpupowerpc}
  {$ifdef macos}
    set_source_info(system_powerpc_macos_info);
  {$endif macos}
{$endif cpupowerpc}
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
