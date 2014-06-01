{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for MS-DOS

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
{ This unit implements support information structures for go32v2. }
unit i_msdos;

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
       system_i8086_msdos_info : tsysteminfo =
          (
            system       : system_i8086_msdos;
            name         : 'MS-DOS 16-bit real mode';
            shortname    : 'MSDOS';
            flags        : [tf_use_8_3,tf_smartlink_library,tf_smartlink_sections,
                            tf_no_objectfiles_when_smartlinking,tf_cld];
            cpu          : cpu_i8086;
            unit_env     : 'MSDOSUNITS';
            extradefines : '';
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
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            importlibprefix : '';
            importlibext : '.al';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            assem        : as_i8086_nasmobj;
            assemextern  : as_i8086_nasmobj;
            link         : ld_none;
            linkextern   : ld_msdos;
{$ifdef USE_SCRIPTED_WLIB}
            ar           : ar_watcom_wlib_omf_scripted;
{$else}
            ar           : ar_watcom_wlib_omf;
{$endif}
            res          : res_none;
            dbg          : dbg_stabs;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 2;
                loopalign       : 2;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 2;
                varalignmin     : 0;
                varalignmax     : 2;
                localalignmin   : 0;
                localalignmax   : 2;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 2
              );
            first_parm_offset : 4;
            stacksize    : 0;
            stackalign   : 2;
            abi          : abi_default;
          );

  implementation

initialization
{$ifdef cpu8086}
  {$ifdef dos16}
    set_source_info(system_i8086_msdos_info);
  {$endif dos16}
{$endif cpu8086}
end.
