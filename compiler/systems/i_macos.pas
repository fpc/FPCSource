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
            name         : 'MacOS (PowerPC)';
            shortname    : 'MacOS';
            flags        : [];
            cpu          : cpu_powerpc;
            unit_env     : '';
            extradefines : '';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
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
            newline      : #13;
            dirsep       : ':';
            files_case_relevent : false;
            assem        : as_powerpc_mpw;
            assemextern  : as_powerpc_mpw;
            link         : nil;
            linkextern   : nil;
            ar           : ar_mpw_ar;
            res          : res_powerpc_mpw;
            script       : script_mpw;
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
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true;
            abi : abi_powerpc_aix;
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
  Revision 1.16  2004-07-05 21:26:35  olle
    + allow fileextension .p, in mode macpas

  Revision 1.15  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.14  2004/05/03 09:48:15  olle
    * changed .pas back to .p

  Revision 1.13  2004/02/19 20:40:20  olle
    + Support for Link on target especially for MacOS
    + TLinkerMPW
    + TAsmScriptMPW

}
