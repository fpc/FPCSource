{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for OS/2

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
{ This unit implements support information structures for OS/2. }
unit i_os2;

  interface

    uses
       systems;

    const
       res_emxbind_info : tresinfo =
          (
            id     : res_emxbind;
            resbin : 'emxbind';
            rescmd : '-b -r $RES $OBJ'
            (* Not really used - see TLinkeros2.SetDefaultInfo in t_os2.pas. *)
          );

       system_i386_os2_info : tsysteminfo =
          (
            system       : system_i386_OS2;
            name         : 'OS/2 via EMX';
            shortname    : 'OS2';
            flags        : [tf_need_export];
            cpu          : cpu_i386;
            unit_env     : 'OS2UNITS';
            extradefines : '';
            sourceext    : '.pas';
            pasext       : '.pp';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.cmd';
            smartext     : '.sl';
            unitext      : '.ppo';
            unitlibext   : '.ppl';
            asmext       : '.so2';
            objext       : '.oo2';
            resext       : '.res';
            resobjext    : '.oor';
            sharedlibext : '.ao2';
            staticlibext : '.ao2';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : 'dll';
            staticClibext : '.a';
            staticClibprefix : '';
            sharedClibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            assem        : as_i386_as_aout;
            assemextern  : as_i386_as_aout;
            link         : nil;
            linkextern   : nil;
            ar           : ar_gnu_ar;
            res          : res_emxbind;
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
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 256*1024;
            DllScanSupported:true;
            use_function_relative_addresses : false
          );


  implementation

initialization
{$ifdef CPU86}
  {$ifdef os2}
    set_source_info(system_i386_os2_info);
    { OS/2 via EMX can be run under DOS as well }
    if (OS_Mode=osDOS) or (OS_Mode=osDPMI) then
      source_info.scriptext := '.bat';
  {$endif os2}
{$endif CPU86}
end.
{
  $Log$
  Revision 1.1  2002-09-06 15:03:51  carl
    * moved files to systems directory

  Revision 1.2  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.1  2002/07/26 21:15:38  florian
    * rewrote the system handling
}
