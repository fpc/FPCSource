{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for Watcom

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
{ This unit implements support information structures for Watcom. }
unit i_watcom;

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const
       system_i386_watcom_info : tsysteminfo =
          (
            system       : system_i386_Watcom;
            name         : 'Watcom compatible DOS extenders';
            shortname    : 'WATCOM';
            flags        : [tf_use_8_3,tf_use_function_relative_addresses];
            cpu          : cpu_i386;
            unit_env     : 'WATCOMUNITS';
            extradefines : 'DPMI';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.asm';
            objext       : '.obj';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : '';
            sharedClibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            assem        : as_i386_wasm;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
            dbg          : dbg_stabs;
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
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 16384;
            abi          : abi_default;
          );

  implementation

initialization
{$ifdef cpu86}
  {$ifdef watcom}
    set_source_info(system_i386_watcom_info);
  {$endif watcom}
{$endif cpu86}
end.
