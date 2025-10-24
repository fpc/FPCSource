{
    This unit implements support information structures for the FPC FreeRTOS target

    Copyright (c) 1998-2006 by Peter Vreman

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
unit i_freertos;

{$i fpcdefs.inc}

{$ifdef go32v2}
  { As wlib uses a different Dos-Extender, long-command line
    encoding for DJGPP does not work here.
    Put all inside a script file instead }
  {$define USE_SCRIPTED_WLIB}
{$endif}

  interface

    uses
       systems;

    const
       system_xtensa_freertos_info : tsysteminfo =
          (
            system       : system_xtensa_freertos;
            name         : 'FreeRTOS';
            shortname    : 'freertos';
            flags        : [tf_needs_symbol_size,tf_files_case_sensitive,tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_init_final_units_by_calls];
            cpu          : cpu_xtensa;
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_freertos;
            ar           : ar_gnu_ar;
            res          : res_none;
            dbg          : dbg_dwarf2;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                jumpalignskipmax    : 0;
                coalescealign   : 0;
                coalescealignskipmax: 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 16;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 8;
            stacksize    : 65536;
            stackalign   : 16;
            abi : abi_xtensa_windowed;
            llvmdatalayout : 'e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32-S32';
            mos6502page0alloc : [];
          );

       system_arm_freertos_info : tsysteminfo =
          (
            system       : system_arm_freertos;
            name         : 'FreeRTOS';
            shortname    : 'freertos';
            flags        : [tf_needs_symbol_size,tf_files_case_sensitive,tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_init_final_units_by_calls];
            cpu          : cpu_arm;
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_freertos;
            ar           : ar_gnu_ar;
            res          : res_none;
            dbg          : dbg_dwarf2;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                jumpalignskipmax    : 0;
                coalescealign   : 0;
                coalescealignskipmax: 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 16;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 65536;
            stackalign   : 16;
            abi : abi_default;
            llvmdatalayout : 'e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32-S32';
            mos6502page0alloc : [];
          );


       system_riscv32_freertos_info : tsysteminfo =
          (
            system       : system_riscv32_freertos;
            name         : 'FreeRTOS';
            shortname    : 'freertos';
            flags        : [tf_needs_symbol_size,tf_files_case_sensitive,tf_requires_proper_alignment,
                            tf_smartlink_sections,tf_init_final_units_by_calls];
            cpu          : cpu_riscv32;
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
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_gas;
            assemextern  : as_gas;
            link         : ld_none;
            linkextern   : ld_freertos;
            ar           : ar_gnu_ar;
            res          : res_none;
            dbg          : dbg_dwarf2;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                jumpalignskipmax    : 0;
                coalescealign   : 0;
                coalescealignskipmax: 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 16;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 8
              );
            first_parm_offset : 8;
            stacksize    : 65536;
            stackalign   : 16;
            abi : abi_riscv_ilp32;
            llvmdatalayout : 'e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:64:128-a0:0:64-n32-S32';
            mos6502page0alloc : [];
          );


implementation

initialization
{$ifdef CPUARM}
  {$ifdef embedded}
    set_source_info(system_arm_freertos_info);
  {$endif embedded}
{$endif CPUARM}
{$ifdef CPUAVR}
  {$ifdef embedded}
    set_source_info(system_avr_freertosd_info);
  {$endif embedded}
{$endif CPUAVR}
{$ifdef CPUMIPSEL}
  {$ifdef embedded}
    set_source_info(system_mipsel_freertos_info);
  {$endif embedded}
{$endif CPUMIPSEL}
{$ifdef CPUI386}
  {$ifdef embedded}
    set_source_info(system_i386_freertos_info);
  {$endif embedded}
{$endif CPUI386}
{$ifdef CPUX86_64}
  {$ifdef embedded}
    set_source_info(system_x86_64_freertos_info);
  {$endif embedded}
{$endif CPUX86_64}
{$ifdef cpu8086}
  {$ifdef embedded}
    set_source_info(system_i8086_freertos_info);
  {$endif embedded}
{$endif cpu8086}
{$ifdef cpum68k}
  {$ifdef embedded}
    set_source_info(system_m68k_freertos_info);
  {$endif embedded}
{$endif cpum68k}
{$ifdef cpuriscv32}
  {$ifdef embedded}
    set_source_info(system_riscv32_freertos_info);
  {$endif embedded}
{$endif cpuriscv32}
{$ifdef cpuriscv64}
  {$ifdef embedded}
    set_source_info(system_riscv64_freertos_info);
  {$endif embedded}
{$endif cpuriscv64}
{$ifdef cpuxtensa}
  {$ifdef embedded}
    set_source_info(system_xtensa_freertos_info);
  {$endif embedded}
{$endif cpuxtensa}
end.

