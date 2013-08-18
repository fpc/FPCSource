{
    Copyright (c) 1998-2008 by Peter Vreman
    Copyright (c) 2011 by Jonas Maebe

    This unit implements support information structures for AIX

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
unit i_aix;

{$i fpcdefs.inc}

  interface

    uses
       systems, rescmn;

    const
       system_powerpc_aix_info : tsysteminfo =
          (
            system       : system_powerpc_aix;
            name         : 'AIX for PowerPC';
            shortname    : 'AIX';
            flags        : [tf_files_case_sensitive,tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_has_winlike_resources];
            cpu          : cpu_powerpc;
            unit_env     : '';
            extradefines : 'UNIX;HASUNIX';
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
            sharedlibext : '.a';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.a';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_powerpc_xcoff;
            assemextern  : as_powerpc_xcoff;
            link         : ld_none;
            linkextern   : ld_aix;
            ar           : ar_gnu_ar;
            res          : res_xcoff;
            dbg          : dbg_stabx;
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
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 8
              );
            first_parm_offset : 8;
            stacksize    : 32*1024*1024;
            stackalign   : 16;
            abi : abi_powerpc_aix;
          );

       system_powerpc64_aix_info : tsysteminfo =
          (
            system       : system_powerpc64_aix;
            name         : 'AIX for PowerPC64';
            shortname    : 'AIX';
            flags        : [tf_files_case_sensitive,tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_has_winlike_resources];
            cpu          : cpu_powerpc64;
            unit_env     : '';
            extradefines : 'UNIX;HASUNIX';
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
            sharedlibext : '.a';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.a';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_powerpc_xcoff;
            assemextern  : as_powerpc_xcoff;
            link         : ld_none;
            linkextern   : ld_aix;
            ar           : ar_gnu_ar;
            res          : res_xcoff;
            dbg          : dbg_stabx;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 8;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 8;
                constalignmax   : 16;
                varalignmin     : 8;
                varalignmax     : 16;
                localalignmin   : 0;
                localalignmax   : 16;
                recordalignmin  : 0;
                recordalignmax  : 16;
                maxCrecordalign : 8
              );
            first_parm_offset : 8;
            stacksize    : 10*1024*1024;
            stackalign   : 16;
            abi : abi_powerpc_aix
          );

  implementation

initialization
{$ifdef CPUPOWERPC32}
  {$ifdef aix}
    set_source_info(system_powerpc_aix_info);
  {$endif aix}
{$endif CPUPOWERPC32}
{$ifdef CPUPOWERPC64}
  {$ifdef aix}
    set_source_info(system_powerpc64_aix_info);
  {$endif aix}
{$endif CPUPOWERPC64}
end.
