{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for win32

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
{ This unit implements support information structures for win32. }
unit i_win32;

  interface

    uses
       systems;

    const
      res_gnu_windres_info : tresinfo =
          (
            id     : res_gnu_windres;
            resbin : 'windres';
            rescmd : '--include $INC -O coff -o $OBJ $RES'
          );

    const
       system_i386_win32_info : tsysteminfo =
          (
            system       : system_i386_WIN32;
            name         : 'Win32 for i386';
            shortname    : 'Win32';
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : 'WIN32UNITS';
            extradefines : 'MSWINDOWS';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.rc';
            resobjext    : '.or';
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            assem        : as_i386_pecoff;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_gnu_windres;
            script       : script_dos;
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
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:true;
            use_function_relative_addresses : true
          );

  implementation

initialization
{$ifdef CPU86}
  {$ifdef WIN32}
    {$ifndef WDOSX}
      set_source_info(system_i386_win32_info);
    {$endif WDOSX}
  {$endif WIN32}
{$endif CPU86}
end.
{
  $Log$
  Revision 1.5  2003-10-03 22:09:49  peter
    * removed paraalign

  Revision 1.4  2003/10/02 21:17:08  peter
    * use as,ld,ar instead of asw,ldw,arw for win32

  Revision 1.3  2003/03/23 23:31:54  hajny
    + platform extensions unified

  Revision 1.2  2002/09/07 15:25:15  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/09/06 15:03:51  carl
    * moved files to systems directory

  Revision 1.2  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.1  2002/07/26 21:15:38  florian
    * rewrote the system handling
}
