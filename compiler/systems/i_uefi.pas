{
    Copyright (c) 2014 by Olivier Coursière

    This unit implements support information structures for uefi,
    Based on Sven Barth's i_nativent
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
{ This unit implements support information structures for uefi. }
unit i_uefi;

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const
       system_i386_uefi_info : tsysteminfo =
          (
            system       : system_i386_uefi;
            name         : 'UEFI for i386';
            shortname    : 'uefi';
            flags        : [tf_files_case_aware,tf_use_function_relative_addresses,tf_smartlink_library
                            ,tf_smartlink_sections{,tf_section_threadvars}{,tf_needs_dwarf_cfi},
                            tf_no_generic_stackcheck{,tf_has_winlike_resources},tf_under_development,
                            tf_dwarf_only_local_labels{,tf_pic_uses_got}];
            cpu          : cpu_i386;
            unit_env     : 'UEFIUNITS';
            extradefines : 'UEFI,FPC_OS_UNICODE';
            exeext       : '.efi';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.efi';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.efi';
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
            link         : ld_int_uefi;
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
            stackalign   : 16;
            abi          : abi_default;
            { note: default LLVM stack alignment is 16 bytes for this target }
            { TODO : check this... Took from Haiku, but should probably come from Windows ? }
            llvmdatalayout : 'e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S32';
          );

       system_x86_64_uefi_info : tsysteminfo =
          (
            system       : system_x86_64_uefi;
            name         : 'UEFI for x86_64';
            shortname    : 'uefi';
            flags        : [tf_files_case_aware,tf_use_function_relative_addresses
                            ,tf_smartlink_sections{,tf_section_threadvars}{,tf_needs_dwarf_cfi},
                            tf_no_generic_stackcheck{,tf_has_winlike_resources},tf_under_development,
                            tf_dwarf_only_local_labels{,tf_pic_uses_got},tf_pic_default,tf_library_needs_pic];
            cpu          : cpu_x86_64;
            unit_env     : 'UEFIUNITS';
            extradefines : 'UEFI,FPC_OS_UNICODE';
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
            sharedlibext : '.efi';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.efi';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            importlibprefix : 'libimp';
            importlibext : '.a';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            assem        : as_x86_64_pecoff;
            assemextern  : as_gas;
            link         : ld_int_uefi;
            linkextern   : ld_none;
            ar           : ar_gnu_ar;
            res          : res_gnu_windres;
            dbg          : dbg_dwarf2;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 16;
                loopalign       : 8;
                jumpalign       : 4;
                constalignmin   : 0;
                constalignmax   : 16;
                varalignmin     : 0;
                varalignmax     : 16;
                localalignmin   : 8;
                localalignmax   : 16;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 16
              );
            first_parm_offset : 16;
            stacksize    : 16*1024*1024;
            stackalign   : 16;
            abi          : abi_default;
            { note: default LLVM stack alignment is 16 bytes for this target }
            { TODO : check this... Took from Win64 }
            llvmdatalayout : 'e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128';
          );

  implementation
  
initialization
{$ifdef CPUI386}
  {$ifdef UEFI}
    set_source_info(system_i386_uefi_info);
  {$endif UEFI}
{$endif CPUI386}
{$ifdef CPUX86_64}
  {$ifdef UEFI}
    set_source_info(system_x86_64_uefi_info);
  {$endif UEFI}
{$endif CPUX86_64}

end.
