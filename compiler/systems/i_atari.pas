{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for atari

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
{ This unit implements support information structures for atari. }
unit i_atari;

  interface

    uses
       systems;

    const
       system_m68k_atari_info : tsysteminfo =
          (
            system       : target_m68k_Atari;
            name         : 'Atari ST/STE';
            shortname    : 'atari';
            flags        : [];
            cpu          : cpu_m68k;
            short_name   : 'ATARI';
            unit_env     : '';
            extradefines : '';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.tpp';
            defext       : '';
            scriptext    : '';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_m68k_atari;
            linkextern   : ld_m68k_atari;
            ar           : ar_m68k_ar;
            res          : res_none;
            script       : script_unix;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 4;
            heapsize     : 16*1024;
            stacksize    : 8192;
            DllScanSupported:false;
            use_function_relative_addresses : false
          );

  implementation

initialization
{$ifdef cpu68}
  {$ifdef atari}
    set_source_info(system_m68k_atari_info);
  {$endif atari}
{$endif cpu68}
end.
{
  $Log$
  Revision 1.2  2003-03-23 23:31:54  hajny
    + platform extensions unified

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
