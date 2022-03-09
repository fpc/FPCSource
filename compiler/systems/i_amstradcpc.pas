{
    Copyright (c) 2022 by the Free Pascal development team

    This unit implements support information structures for the Amstrad CPC

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
unit i_amstradcpc;

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const
       system_z80_amstradcpc_info : tsysteminfo =
          (
            system       : system_z80_amstradcpc;
            name         : 'Amstrad CPC';
            shortname    : 'amstradcpc';
            flags        : [
                            tf_under_development,
{$ifdef Z80_SMARTLINK_SECTIONS}
                            tf_smartlink_sections,
{$else Z80_SMARTLINK_SECTIONS}
                            tf_smartlink_library,
                            tf_no_objectfiles_when_smartlinking,
{$endif Z80_SMARTLINK_SECTIONS}
                            tf_cld,tf_no_generic_stackcheck,tf_emit_stklen];
            cpu          : cpu_z80;
            unit_env     : 'CPCUNITS';
            extradefines : '';
            exeext       : '.com';
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
            Cprefix      : '';
            newline      : #13#10;
            dirsep       : '\';
            assem        : as_z80_rel;
            assemextern  : as_sdcc_sdasz80;
            link         : ld_int_msxdos;
            linkextern   : ld_msxdos;
            ar           : ar_sdcc_sdar;
            res          : res_none;
            dbg          : dbg_dwarf2;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign            : 1;
                loopalign            : 1;
                jumpalign            : 0;
                jumpalignskipmax     : 0;
                coalescealign        : 0;
                coalescealignskipmax : 0;
                constalignmin        : 0;
                constalignmax        : 1;
                varalignmin          : 0;
                varalignmax          : 1;
                localalignmin        : 0;
                localalignmax        : 1;
                recordalignmin       : 0;
                recordalignmax       : 1;
                maxCrecordalign      : 1
              );
            first_parm_offset : 4;
            stacksize    : 1024;
            stackalign   : 1;
            abi          : abi_default;
            llvmdatalayout : 'todo';
          );

  implementation

initialization
{$ifdef cpuz80}
  {$ifdef amstradcpc}
    set_source_info(system_z80_amstradcpc_info);
  {$endif amstradcpc}
{$endif cpuz80}
end.
