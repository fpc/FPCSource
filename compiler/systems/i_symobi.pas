{
    Copyright (c) 2011 by Sven Barth

    This unit implements support information structures for Symobi

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

unit i_symobi;

{$i fpcdefs.inc}

  interface

    uses
       systems, rescmn;

    const

       system_i386_symobi_info : tsysteminfo =
          (
            system       : system_i386_symobi;
            name         : 'Symobi for i386';
            shortname    : 'Symobi';
            flags        : [tf_smartlink_library
                            ,tf_smartlink_sections{,tf_section_threadvars}{,tf_needs_dwarf_cfi},
                            tf_no_pic_supported,
                            tf_no_generic_stackcheck,tf_has_winlike_resources,
                            tf_dwarf_only_local_labels,
                            tf_safecall_exceptions];
            cpu          : cpu_i386;
            unit_env     : 'SYMOBIUNITS';
            extradefines : 'SYMOBI;UNIX;BSD;HASSYMOBI';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib.';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib.';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_i386_pecoff;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_ext;
            dbg          : dbg_stabs;
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
                localalignmin   : 0;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize   : 262144;
            abi          : abi_default;
          );

  implementation

initialization
{$ifdef cpu86}
  {$ifdef Symobi}
     set_source_info(system_i386_Symobi_info);
  {$endif}
{$endif cpu86}
end.
