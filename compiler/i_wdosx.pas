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
{ This unit implements support information structures for wdosx. }
unit i_wdosx;

  interface

    uses
       systems;

    const
       system_i386_wdosx_info : tsysteminfo =
          (
            system       : system_i386_wdosx;
            name         : 'WDOSX DOS extender';
            shortname    : 'WDOSX';
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : 'WDOSXUNITS';
            extradefines : 'MSWINDOWS';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sld';
            unitext      : '.ppd';
            unitlibext   : '.ppl';
            asmext       : '.sd';
            objext       : '.od';
            resext       : '.rc';
            resobjext    : '.odr';
            sharedlibext : '.dll';
            staticlibext : '.ad';
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
            assem        : as_i386_pecoffwdosx;
            assemextern  : as_i386_aswdosx;
            link         : ld_i386_wdosx;
            linkextern   : ld_i386_wdosx;
            ar           : ar_gnu_arw;
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
                paraalign       : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 32*1024*1024;
            DllScanSupported:true;
            use_function_relative_addresses : true
          );

  implementation

initialization
{$ifdef CPU86}
  {$ifdef WIN32}
    {$ifdef WDOSX}
      set_source_info(system_i386_wdosx_info);
    {$endif WDOSX}
  {$endif WIN32}
{$endif CPU86}
end.
{
  $Log$
  Revision 1.1  2002-07-26 21:15:38  florian
    * rewrote the system handling
}