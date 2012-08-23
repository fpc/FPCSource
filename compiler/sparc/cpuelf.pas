{
    Copyright (c) 1998-2006 by Peter Vreman

    Includes ELF-related code specific to SPARC

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
    TElfObjOutputSparc=class(TElfObjectOutput)
      function encodereloc(objrel:TObjRelocation):byte;override;
    end;

    TElfAssemblerSparc=class(TInternalAssembler)
      constructor create(smart:boolean);override;
    end;

  const
    { Relocation types }
    R_SPARC_32 = 3;
    R_SPARC_WDISP30 = 7;
    R_SPARC_HI22 = 9;
    R_SPARC_LO10 = 12;
    R_SPARC_GNU_VTINHERIT = 250;
    R_SPARC_GNU_VTENTRY = 251;


{****************************************************************************
                               TElfObjOutputSparc
****************************************************************************}

   function TElfObjOutputSparc.encodereloc(objrel:TObjRelocation):byte;
     begin
       case objrel.typ of
         RELOC_ABSOLUTE :
           result:=R_SPARC_32;
         { TODO }
       else
         result:=0;
         InternalError(2012082303);
       end;
     end;


{****************************************************************************
                               TElfAssemblerSparc
****************************************************************************}

  constructor TElfAssembleri386.create(smart:boolean);
    begin
      inherited Create(smart);
      CObjOutput:=TElfObjOutputSparc;
    end;


{*****************************************************************************
                                    Initialize
*****************************************************************************}

  const
    as_sparc_elf32_info : tasminfo =
       (
         id     : as_sparc_elf32;
         idtxt  : 'ELF';
         asmbin : '';
         asmcmd : '';
         supported_targets : [];
//            flags : [af_outputbinary,af_smartlink_sections];
         flags : [af_outputbinary,af_supports_dwarf];
         labelprefix : '.L';
         comment : '';
         dollarsign: '$';
       );

initialization
  RegisterAssembler(as_sparc_elf32_info,TElfAssemblerSparc);

end.

