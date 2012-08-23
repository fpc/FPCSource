{
    Copyright (c) 1998-2006 by Peter Vreman

    Includes ELF-related code specific to i386

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
    verbose,
    systems,ogbase,ogelf,assemble;

  type
    TElfObjOutput386=class(TElfObjectOutput)
      function encodereloc(objrel:TObjRelocation):byte;override;
    end;

    TElfAssembler386=class(TInternalAssembler)
      constructor create(smart:boolean);override;
    end;

  const
    { Relocation types }
    R_386_32 = 1;                    { ordinary absolute relocation }
    R_386_PC32 = 2;                  { PC-relative relocation }
    R_386_GOT32 = 3;                 { an offset into GOT }
    R_386_PLT32 = 4;                 { a PC-relative offset into PLT }
    R_386_COPY = 5;
    R_386_GLOB_DAT = 6;
    R_386_JUMP_SLOT = 7;
    R_386_RELATIVE = 8;
    R_386_GOTOFF = 9;                { an offset from GOT base }
    R_386_GOTPC = 10;                { a PC-relative offset _to_ GOT }

    R_386_TLS_TPOFF = 14;
    R_386_TLS_IE = 15;
    R_386_TLS_GOTIE = 16;
    R_386_TLS_LE = 17;
    R_386_TLS_GD = 18;
    R_386_TLS_LDM = 19;
    R_386_16 = 20;
    R_386_PC16 = 21;
    R_386_8 = 22;
    R_386_PC8 = 23;
    R_386_TLS_GD_32 = 24;
    R_386_TLS_GD_PUSH = 25;
    R_386_TLS_GD_CALL = 26;
    R_386_TLS_GD_POP = 27;
    R_386_TLS_LDM_32 = 28;
    R_386_TLS_LDM_PUSH = 29;
    R_386_TLS_LDM_CALL = 30;
    R_386_TLS_LDM_POP = 31;
    R_386_TLS_LDO_32 = 32;
    R_386_TLS_IE_32 = 33;
    R_386_TLS_LE_32 = 34;
    R_386_TLS_DTPMOD32 = 35;
    R_386_TLS_DTPOFF32 = 36;
    R_386_TLS_TPOFF32 = 37;
    { 38 is unused }
    R_386_TLS_GOTDESC = 39;
    R_386_TLS_DESC_CALL = 40;
    R_386_TLS_DESC = 41;
    R_386_IRELATIVE = 42;
    R_386_GNU_VTINHERIT = 250;
    R_386_GNU_VTENTRY = 251;


{****************************************************************************
                               TElfObjOutput386
****************************************************************************}

  function TElfObjOutput386.encodereloc(objrel:TObjRelocation):byte;
    begin
      case objrel.typ of
        RELOC_RELATIVE :
          result:=R_386_PC32;
        RELOC_ABSOLUTE :
          result:=R_386_32;
        RELOC_GOT32 :
          result:=R_386_GOT32;
        RELOC_GOTPC :
          result:=R_386_GOTPC;
        RELOC_PLT32 :
          result:=R_386_PLT32;
      else
        result:=0;
        InternalError(2012082301);
      end;
    end;

{****************************************************************************
                               TElfAssembler386
****************************************************************************}

  constructor TElfAssembler386.create(smart:boolean);
    begin
      inherited Create(smart);
      CObjOutput:=TElfObjOutput386;
    end;


{*****************************************************************************
                                    Initialize
*****************************************************************************}

  const
    as_i386_elf32_info : tasminfo =
       (
         id     : as_i386_elf32;
         idtxt  : 'ELF';
         asmbin : '';
         asmcmd : '';
         supported_targets : [system_i386_linux,system_i386_beos,
                              system_i386_freebsd,system_i386_haiku,
                              system_i386_openbsd,system_i386_netbsd,
                              system_i386_Netware,system_i386_netwlibc,
                              system_i386_solaris,system_i386_embedded];
         flags : [af_outputbinary,af_smartlink_sections,af_supports_dwarf];
         labelprefix : '.L';
         comment : '';
         dollarsign: '$';
       );

initialization
  RegisterAssembler(as_i386_elf32_info,TElfAssembler386);

end.

