{
    $Id: i_nwl.pas,v 1.5 2005/03/20 22:36:45 olle Exp $
    Copyright (c) 1998-2004 by Peter Vreman

    This unit implements support information structures for Netware (libc) modules

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
 ****************************************************************************}

{ This unit implements support information structures for Netware libc modules. }
unit i_nwl;

  interface

    uses
       systems;

    const
       system_i386_netwlibc_info : tsysteminfo =
          (
            system       : system_i386_netwlibc;
            name         : 'Netware for i386(libc)';
            shortname    : 'Netwlibc';
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : 'NETWLIBCUNITS';
            extradefines : 'NETWARE;NETWARE_LIBC';
            exeext       : '.nlm';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.nlm';
            staticlibext : '.a';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.nlm';
            staticClibext : '.a';
            staticClibprefix : '';
            sharedClibprefix : '';
            p_ext_support : false;
            Cprefix      : '';
            newline      : #13#10;
            dirsep       : '/';
            files_case_relevent : false;
            assem        : as_i386_elf32;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
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
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 16384;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

  implementation

initialization
{$ifdef CPU86}
  {$ifdef netwlibc}
    set_source_info(system_i386_netwlibc_info);
  {$endif netwlibc}
{$endif CPU86}
end.
{
  $Log: i_nwl.pas,v $
  Revision 1.5  2005/03/20 22:36:45  olle
    * Cleaned up handling of source file extension.
    + Added support for .p extension for macos and darwin

  Revision 1.4  2005/02/14 17:13:10  peter
    * truncate log

}
