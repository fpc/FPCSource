{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for solaris

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
{ This unit implements support information structures for solaris. }
unit i_sunos;

  interface

    uses
       systems;

    const
       system_i386_solaris_info : tsysteminfo =
          (
            system       : system_i386_solaris;
            name         : 'Solaris for i386';
            shortname    : 'solaris';
            flags        : [tf_under_development,tf_files_case_sensitive,tf_use_function_relative_addresses];
            cpu          : cpu_i386;
            unit_env     : 'SOLARISUNITS';
            extradefines : 'UNIX;LIBC;SUNOS;HASUNIX';
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
            assem        : as_ggas;
            assemextern  : as_ggas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_gar;
            res          : res_none;
            dbg          : dbg_stabs;
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
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 262144;
          );

       system_sparc_solaris_info : tsysteminfo =
          (
            system       : system_sparc_solaris;
            name         : 'Solaris for SPARC';
            shortname    : 'solaris';
            flags        : [tf_needs_symbol_size,tf_under_development,tf_files_case_sensitive,tf_use_function_relative_addresses];
            cpu          : cpu_SPARC;
            unit_env     : 'SOLARISUNITS';
            extradefines : 'UNIX;LIBC;SUNOS;HASUNIX';
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
            assem        : as_ggas;
            assemextern  : as_ggas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_gar;
            res          : res_none;
            dbg          : dbg_stabs;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 4;
                constalignmax   : 8;
                varalignmin     : 4;
                varalignmax     : 8;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 92;
            stacksize    : 262144;
          );

  implementation

initialization
{$ifdef CPU86}
  {$ifdef solaris}
    set_source_info(system_i386_solaris_info);
  {$endif solaris}
{$endif CPU86}
{$ifdef CPUSparc}
  {$ifdef solaris}
    set_source_info(system_sparc_solaris_info);
  {$endif solaris}
{$endif CPUSparc}

end.
