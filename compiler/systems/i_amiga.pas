{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for AmigaOS

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
{ This unit implements support information structures for the AmigaOS. }
unit i_amiga;

  interface

    uses
       systems;

    const
       system_m68k_amiga_info : tsysteminfo =
          (
            system       : system_m68k_Amiga;
            name         : 'Commodore Amiga';
            shortname    : 'amiga';
            flags        : [];
            cpu          : cpu_m68k;
            unit_env     : '';
            extradefines : '';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.asm';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.library';
            staticlibext : '.a';
            staticlibprefix : 'lib';
            sharedlibprefix : '';
            sharedClibext : '.library';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
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
            script       : script_amiga;
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
                paraalign       : 4;
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

  implementation

initialization
{$ifdef cpu68}
  {$ifdef AMIGA}
    set_source_info(system_m68k_Amiga_info);
  {$endif amiga}
{$endif cpu68}
end.
{
  $Log$
  Revision 1.2  2003-02-02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

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
