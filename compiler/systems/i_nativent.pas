{
    Copyright (c) 2009 by Sven Barth

    This unit implements support information structures for nativent
    Based on Peter Vreman's i_win

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
{ This unit implements support information structures for nativent. }
unit i_nativent;

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const
       system_i386_nativent_info : tsysteminfo =
          (
            system       : system_i386_NATIVENT;
            name         : 'Native NT for i386';
            shortname    : 'NativeNT';
            flags        : [tf_files_case_aware,tf_use_function_relative_addresses,tf_smartlink_library
                            ,tf_smartlink_sections{,tf_section_threadvars}{,tf_needs_dwarf_cfi},
                            tf_no_pic_supported,
                            tf_no_generic_stackcheck{,tf_has_winlike_resources},tf_under_development,
                            tf_dwarf_only_local_labels];
            cpu          : cpu_i386;
            unit_env     : 'NTUNITS';
            extradefines : 'NATIVENT,FPC_OS_UNICODE';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
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
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            assem        : as_i386_pecoff;
            assemextern  : as_gas;
            link         : ld_int_nativent;
            linkextern   : ld_none;
            ar           : ar_gnu_ar;
            res          : res_gnu_windres;
            dbg          : dbg_stabs;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 16;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 16;
                varalignmin     : 0;
                varalignmax     : 16;
                localalignmin   : 4;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            stacksize    : 16*1024*1024;
            stackalign   : 4;
            abi          : abi_default;
          );

  implementation

initialization
{$ifdef CPUI386}
  {$ifdef NATIVENT}
    set_source_info(system_i386_nativent_info);
  {$endif NATIVENT}
{$endif CPUI386}
end.
