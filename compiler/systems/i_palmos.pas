{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for PalmOS

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
{ This unit implements support information structures for PalmOS. }
unit i_palmos;

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const
       system_m68k_palmos_info : tsysteminfo =
          (
            system       : system_m68k_PalmOS;
            name         : 'PalmOS';
            shortname    : 'PalmOS';
            flags        : [tf_code_small,tf_static_reg_based,tf_smartlink_sections];
            cpu          : cpu_m68k;
            unit_env     : 'PALMUNITS';
            extradefines : '';
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
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_palmos;
            ar           : ar_gnu_ar;
            res          : res_none;
            dbg          : dbg_stabs;
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
            stacksize    : 8192;
            stackalign   : 2;
            abi : abi_default;
            llvmdatalayout : 'todo';
          );

       res_m68k_palmos_info : tresinfo =
          (
            id     : res_m68k_palmos;
            resbin : 'pilrc';
            rescmd : '-I $INC $RES';
            rcbin  : '';
            rccmd  : '';
            resourcefileclass : nil;
            resflags : [];
          );

       system_arm_palmos_info : tsysteminfo =
          (
            system       : system_arm_PalmOS;
            name         : 'PalmOS';
            shortname    : 'PalmOS';
            flags        : [tf_code_small,tf_static_reg_based,tf_smartlink_sections,tf_requires_proper_alignment];
            cpu          : cpu_arm;
            unit_env     : 'PALMUNITS';
            extradefines : '';
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
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_palmos;
            ar           : ar_gnu_ar;
            res          : res_none;
            dbg          : dbg_stabs;
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
            stacksize    : 8192;
            stackalign   : 4;
            abi : abi_default;
            llvmdatalayout : 'todo';
          );

       res_arm_palmos_info : tresinfo =
          (
            id     : res_m68k_palmos;
            resbin : 'pilrc';
            rescmd : '-I $INC $RES';
            rcbin  : '';
            rccmd  : '';
            resourcefileclass : nil;
            resflags : [];
          );

implementation

initialization
{$ifdef cpu68}
  {$ifdef palmos}
    set_source_info(system_m68k_palmos_info);
  {$endif palmos}
{$endif cpu68}
{$ifdef cpuarm}
  {$ifdef palmos}
    set_source_info(system_arm_palmos_info);
  {$endif palmos}
{$endif cpuarm}
end.
