{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support information structures for Win16

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
unit i_win16;

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
       system_i8086_win16_info : tsysteminfo =
          (
            system       : system_i8086_win16;
            name         : 'Win16 for x86';
            shortname    : 'Win16';
            flags        : [tf_use_8_3,
{$ifdef I8086_SMARTLINK_SECTIONS}
                            tf_smartlink_sections,
{$else I8086_SMARTLINK_SECTIONS}
                            tf_smartlink_library,
                            tf_no_objectfiles_when_smartlinking,
{$endif I8086_SMARTLINK_SECTIONS}
                            tf_cld,tf_no_generic_stackcheck,tf_emit_stklen,
                            tf_x86_far_procs_push_odd_bp];
            cpu          : cpu_i8086;
            unit_env     : 'WIN16UNITS';
            extradefines : 'MSWINDOWS;WINDOWS';
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
            sharedlibext : '';
            staticlibext : '.a';
            staticlibprefix : '';
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
            assem        : as_i8086_omf;
            assemextern  : as_i8086_nasmobj;
            link         : ld_win16;
            linkextern   : ld_win16;
{$ifdef USE_SCRIPTED_WLIB}
            ar           : ar_watcom_wlib_omf_scripted;
{$else}
            ar           : ar_watcom_wlib_omf;
{$endif}
            res          : res_none;
            dbg          : dbg_dwarf2;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 1;
                loopalign       : 1;
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
            llvmdatalayout : 'todo';
          );

  implementation

initialization
{$ifdef cpu8086}
  {$ifdef win16}
    set_source_info(system_i8086_win16_info);
  {$endif win16}
{$endif cpu8086}
end.
