{
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
{ This unit implements support information structures for win32. }
unit i_win;

  interface

    uses
       systems;

    const
       system_i386_win32_info : tsysteminfo =
          (
            system       : system_i386_WIN32;
            name         : 'Win32 for i386';
            shortname    : 'Win32';
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : 'WIN32UNITS';
            extradefines : 'MSWINDOWS';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.rc';
            resobjext    : '.or';
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            p_ext_support : false;
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : true;
            assem        : as_i386_pecoff;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_gnu_windres;
            dbg          : dbg_stabs;
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
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            stacksize    : 262144;
            DllScanSupported:true;
            use_function_relative_addresses : true
          );

       system_x64_win64_info : tsysteminfo =
          (
            system       : system_x86_64_win64;
            name         : 'Win64 for x64';
            shortname    : 'Win64';
            flags        : [];
            cpu          : cpu_x86_64;
            unit_env     : 'WIN64UNITS';
            extradefines : 'MSWINDOWS';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.rc';
            resobjext    : '.or';
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            p_ext_support : false;
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : true;
            assem        : as_x86_64_pecoff;
            assemextern  : as_x86_64_masm;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_gnu_windres;
            dbg          : dbg_stabs;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 8;
                loopalign       : 8;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 8;
                varalignmin     : 0;
                varalignmax     : 8;
                localalignmin   : 8;
                localalignmax   : 8;
                recordalignmin  : 0;
                recordalignmax  : 8;
                maxCrecordalign : 16
              );
            first_parm_offset : 16;
            stacksize    : 262144;
            DllScanSupported:true;
            use_function_relative_addresses : true
          );

       system_arm_wince_info : tsysteminfo =
          (
            system       : system_arm_wince;
            name         : 'WinCE for ARM';
            shortname    : 'WinCE';
            flags        : [];
            cpu          : cpu_arm;
            unit_env     : '';
            extradefines : 'UNDER_CE';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.rc';
            resobjext    : '.or';
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            p_ext_support : false;
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : true;
            assem        : as_gas;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_gnu_wince_windres;
            dbg          : dbg_stabs;
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
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );

       system_i386_wince_info : tsysteminfo =
          (
            system       : system_i386_wince;
            name         : 'WinCE for i386';
            shortname    : 'WinCE';
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : '';
            extradefines : 'UNDER_CE';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.rc';
            resobjext    : '.or';
            sharedlibext : '.dll';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            p_ext_support : false;
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : true;
            assem        : as_i386_pecoffwince;
            assemextern  : as_gas;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_gnu_windres;
            dbg          : dbg_stabs;
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
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 4;
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            stacksize    : 262144;
            DllScanSupported:true;
            use_function_relative_addresses : true
          );


  implementation

initialization
{$ifdef CPU86}
  {$ifdef WIN32}
    {$ifndef WDOSX}
      set_source_info(system_i386_win32_info);
    {$endif WDOSX}
  {$endif WIN32}
  {$ifdef WINCE}
    set_source_info(system_i386_wince_info);
  {$endif WINCE}
{$endif CPU86}

{$ifdef CPUX86_64}
  {$ifdef WIN64}
    {$ifndef WDOSX}
      set_source_info(system_x64_win64_info);
    {$endif WDOSX}
  {$endif WIN64}
{$endif CPUX86_64}

{$ifdef CPUARM}
  {$ifdef WINCE}
    set_source_info(system_arm_wince_info);
  {$endif WINCE}
{$endif CPUARM}
end.
