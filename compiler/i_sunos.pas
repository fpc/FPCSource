{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for SunOS

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
{ This unit implements support information structures for SunOS. }
unit i_sunos;

  interface

    uses
       systems;

    const
       system_i386_sunos_info : tsysteminfo =
          (
            system       : system_i386_sunos;
            name         : 'SunOS/ELF for i386';
            shortname    : 'SunOS';
            flags        : [tf_under_development];
            cpu          : cpu_i386;
            unit_env     : 'SUNOSUNITS';
            extradefines : 'UNIX;SOLARIS;LIBC';
            sourceext    : '.pp';
            pasext       : '.pas';
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_i386_as;
            assemextern  : as_i386_as;
            link         : ld_i386_sunos;
            linkextern   : ld_i386_sunos;
            ar           : ar_gnu_ar;
            res          : res_none;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 1;
                varalignmin     : 0;
                varalignmax     : 1;
                localalignmin   : 0;
                localalignmax   : 1;
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
{$ifdef CPU86}
  {$ifdef sunos}
    set_source_info(system_i386_sunos_info);
  {$endif sunos}
{$endif CPU86}
end.
{
  $Log$
  Revision 1.1  2002-07-26 21:15:38  florian
    * rewrote the system handling
}
