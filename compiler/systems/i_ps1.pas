{
    Copyright (c) 2024 by Kirill Kranz

    This unit implements support information structures for ps1

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
{ This unit implements support information structures for ps1. }
unit i_ps1;

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const

       system_mipsel_ps1_info : tsysteminfo =
          (
            system       : system_mipsel_ps1;
            name         : 'PlayStation 1 for MIPSEL';
            shortname    : 'ps1';
            flags        : [tf_no_pic_supported, tf_smartlink_sections, tf_files_case_sensitive, tf_requires_proper_alignment];
            cpu          : cpu_mipsel;
            unit_env     : '';
            extradefines : '';
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
            importlibprefix : 'libimp';
            importlibext : '.a';
//            p_ext_support : false;
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_ps1;
            ar           : ar_gnu_ar;
            res          : res_elf;
            dbg          : dbg_none;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 8;
                loopalign       : 8;
                jumpalign       : 8;
                jumpalignskipmax    : 8;
                coalescealign   : 0;
                coalescealignskipmax: 8;
                constalignmin   : 0;
                constalignmax   : 8;
                varalignmin     : 0;
                varalignmax     : 8;
                localalignmin   : 0;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 0;
            stacksize    : 32*1024*1024;
            stackalign   : 8;
            abi : abi_default;
            llvmdatalayout : 'e-p:32:32:32-i1:8:8-i8:8:32-i16:16:32-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-n32-S64';
          );

  implementation

initialization

    set_source_info(system_mipsel_ps1_info);

end.

