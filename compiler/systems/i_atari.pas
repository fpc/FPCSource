{
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

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const
       system_m68k_atari_info : tsysteminfo =
          (
            system       : target_m68k_Atari;
            name         : 'Atari ST/STE';
            shortname    : 'atari';
            flags        : [tf_use_8_3];
            cpu          : cpu_m68k;
            short_name   : 'ATARI';
            unit_env     : '';
            extradefines : '';
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
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            importlibprefix : 'libimp';
            importlibext : '.a';
            p_ext_support : false;
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
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 4;
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
