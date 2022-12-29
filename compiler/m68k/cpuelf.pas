{
    Copyright (c) 2022 by the Free Pascal Development team

    Includes ELF-related code specific to m68k

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
unit cpuelf;

{$i fpcdefs.inc}

interface

implementation

  uses
    verbose,elfbase,
    systems,ogbase,ogelf,assemble;

  const
    {* Relocation types. *}
    R_68K_NONE = 0;
    R_68K_32 = 1;
    R_68K_16 = 2;
    R_68K_8 = 3;
    R_68K_PC32 = 4;
    R_68K_PC16 = 5;
    R_68K_PC8 = 6;
    R_68K_GOT32 = 7;
    R_68K_GOT16 = 8;
    R_68K_GOT8 = 9;
    R_68K_GOT32O = 10;
    R_68K_GOT16O = 11;
    R_68K_GOT8O = 12;
    R_68K_PLT32 = 13;
    R_68K_PLT16 = 14;
    R_68K_PLT8 = 15;
    R_68K_PLT32O = 16;
    R_68K_PLT16O = 17;
    R_68K_PLT8O = 18;
    R_68K_COPY = 19;
    R_68K_GLOB_DAT = 20;
    R_68K_JMP_SLOT = 21;
    R_68K_RELATIVE = 22;

    R_68K_GNU_VTINHERIT = 23;
    R_68K_GNU_VTENTRY = 24;

    R_68K_TLS_GD32 = 25;
    R_68K_TLS_GD16 = 26;
    R_68K_TLS_GD8 = 27;
    R_68K_TLS_LDM32 = 28;
    R_68K_TLS_LDM16 = 29;
    R_68K_TLS_LDM8 = 30;
    R_68K_TLS_LDO32 = 31;
    R_68K_TLS_LDO16 = 32;
    R_68K_TLS_LDO8 = 33;
    R_68K_TLS_IE32 = 34;
    R_68K_TLS_IE16 = 35;
    R_68K_TLS_IE8 = 36;
    R_68K_TLS_LE32 = 37;
    R_68K_TLS_LE16 = 38;
    R_68K_TLS_LE8 = 39;
    R_68K_TLS_DTPMOD32 = 40;
    R_68K_TLS_DTPREL32 = 41;
    R_68K_TLS_TPREL32 = 42;

{****************************************************************************
                               ELF Target methods
****************************************************************************}

   function elf_m68k_encodereloc(objrel:TObjRelocation):byte;
     begin
       case objrel.typ of
         RELOC_NONE :
           result:=R_68K_NONE;
         RELOC_ABSOLUTE :
           result:=R_68K_32;
         { TODO }
       else
         result:=0;
         InternalError(2022122901);
       end;
     end;


   function elf_m68k_relocname(reltyp:byte):string;
     begin
       result:='TODO';
     end;


   procedure elf_m68k_loadreloc(objrel:TObjRelocation);
     begin
     end;



{*****************************************************************************
                                    Initialize
*****************************************************************************}

  const
    elf_target_m68k: TElfTarget =
      (
        max_page_size:     $8000; // fixme
        exe_image_base:    $8000; // fixme
        machine_code:      EM_M68K;
        relocs_use_addend: false;
        dyn_reloc_codes: (
          R_68K_RELATIVE,
          R_68K_GLOB_DAT,
          R_68K_JMP_SLOT,
          R_68K_COPY,
          0      // IRELATIVE is absent(?)
        );
        relocname:         @elf_m68k_relocName;
        encodereloc:       @elf_m68k_encodeReloc;
        loadreloc:         @elf_m68k_loadReloc;
        loadsection:       nil;
        encodeflags:       nil;
      );

    as_m68k_elf32_info : tasminfo =
       (
         id     : as_m68k_elf32;
         idtxt  : 'ELF';
         asmbin : '';
         asmcmd : '';
         supported_targets : [system_m68k_amiga,system_m68k_embedded];
         flags : [af_outputbinary,af_smartlink_sections];
         labelprefix : '.L';
         labelmaxlen : -1;
         comment : '';
         dollarsign: '$';
       );

initialization
  RegisterAssembler(as_m68k_elf32_info,TElfAssembler);
  ElfTarget:=elf_target_m68k;
end.

