{
    $Id$
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

  interface

    uses
       systems;

    const
       system_m68k_palmos_info : tsysteminfo =
          (
            system       : system_m68k_PalmOS;
            name         : 'PalmOS';
            shortname    : 'palmos';
            flags        : [tf_code_small,tf_static_a5_based];
            cpu          : cpu_m68k;
            short_name   : 'PALMOS';
            unit_env     : 'PALMUNITS';
            extradefines : '';
            sharedlibext : '.so';
            staticlibext : '.a';
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
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_m68k_as;
            assemextern  : as_m68k_as;
            link         : ld_m68k_palmos;
            linkextern   : ld_m68k_palmos;
            ar           : ar_m68k_ar;
            res          : res_none;
            script       : script_unix;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 4;
            heapsize     : 128*1024;
            stacksize    : 8192;
            DllScanSupported:false;
            use_function_relative_addresses : false
          );

  implementation

initialization
{$ifdef cpu68}
  {$ifdef palmos}
    set_source_info(system_m68k_palmos_info);
  {$endif palmos}
{$endif cpu68}
end.
{
  $Log$
  Revision 1.1  2002-07-26 21:15:38  florian
    * rewrote the system handling
}
