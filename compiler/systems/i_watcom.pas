{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for go32v2

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
{ This unit implements support information structures for go32v2. }
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
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : 'WATCOMUNITS';
            extradefines : 'DPMI';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            files_case_relevent : false;
            assem        : as_i386_coff;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_none;
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
                paraalign       : 2;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 2048*1024;
            stacksize    : 16384;
            DllScanSupported : false;
            use_function_relative_addresses : true
          );

  implementation

initialization
{$ifdef cpu86}
  {$ifdef watcom}
    set_source_info(system_i386_watcom_info);
  {$endif watcom}
{$endif cpu86}
end.
{
  $Log$
  Revision 1.1  2003-09-06 10:01:11  florian
    + added *_watcom units
}
