{
    This unit implements support information structures for the FPC
    Oric-1 / Oric Atmos / Oric NOVA 64 / Pravetz 8D target

    Copyright (c) 2025 by Nikolay Nikolov

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
unit i_oric;

{$i fpcdefs.inc}

  interface

    uses
       systems;

    const
       system_mos6502_oric_info : tsysteminfo =
          (
            system       : system_mos6502_oric;
            name         : 'Oric-1 / Oric Atmos / Oric NOVA 64 / Pravetz 8D';
            shortname    : 'oric';
            flags        : [
{$ifdef MOS6502_SMARTLINK_SECTIONS}
                            tf_smartlink_sections,
{$else MOS6502_SMARTLINK_SECTIONS}
                            tf_smartlink_library,
                            tf_no_objectfiles_when_smartlinking,
{$endif MOS6502_SMARTLINK_SECTIONS}
                            tf_needs_symbol_size,tf_files_case_sensitive];
            cpu          : cpu_mos6502;
            unit_env     : '';
            extradefines : '';
            exeext       : '.tap';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.rel';
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
            assem        : as_sdcc_sdas6500;
            assemextern  : as_sdcc_sdas6500;
            link         : ld_none;
            linkextern   : ld_oric;
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
            abi : abi_default;
            llvmdatalayout : 'todo';
          );

 implementation

initialization
{$ifdef CPUMOS6502}
  {$ifdef oric}
    set_source_info(system_mos6502_oric_info);
  {$endif oric}
{$endif CPUMOS6502}
end.

