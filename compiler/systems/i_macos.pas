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
            shortname    : 'MacOS';
            flags        : [];
            cpu          : cpu_powerpc;
            unit_env     : '';
            extradefines : '';
            sourceext    : '.pp';
            pasext       : '.p';
            exeext       : '';
            defext       : '';
            scriptext    : '';
            smartext     : '.sl';
            unitext      : '.pput';
            unitlibext   : '.ppl';
            asmext       : '.a';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : 'Lib';
            staticlibext : 'Lib';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : 'Lib';
            staticClibext : 'Lib';
            staticClibprefix : '';
            sharedClibprefix : '';
            Cprefix      : '';
            newline      : #10;
            dirsep       : ':';
            files_case_relevent : true;
            assem        : as_powerpc_mpw;
            assemextern  : as_powerpc_mpw;
            link         : nil;
            linkextern   : nil;
            ar           : ar_mpw_ar;
            res          : res_mpw_res;
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
                localalignmin   : 8;
                localalignmax   : 8;
                paraalign       : 8;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 262144;
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
  Revision 1.4  2002-10-02 21:50:19  florian
    * importing via external is now possible for macos

  Revision 1.3  2002/10/02 21:29:34  florian
    * ppus have the extension ppu on macos as well now

  Revision 1.2  2002/09/11 19:59:14  florian
    * renamed target macosppc to macppc

  Revision 1.1  2002/09/06 15:03:51  carl
    * moved files to systems directory

  Revision 1.3  2002/08/20 21:40:44  florian
    + target macos for ppc added
    + frame work for mpw assembler output

  Revision 1.2  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.1  2002/07/26 21:15:38  florian
    * rewrote the system handling
}
