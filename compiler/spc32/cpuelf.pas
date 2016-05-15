{
    Copyright (c) 1998-2006 by Peter Vreman

    Includes ELF-related code specific to SPC32

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
    globtype,cclasses,
    verbose,elfbase,
    systems,aasmbase,ogbase,ogelf,assemble;

  type
    TElfExeOutputSpc32=class(TElfExeOutput)
    private
      procedure MaybeWriteGOTEntry(reltyp:byte;relocval:aint;objsym:TObjSymbol);
    protected
      procedure WriteFirstPLTEntry;override;
      procedure WritePLTEntry(exesym:TExeSymbol);override;
      procedure WriteIndirectPLTEntry(exesym:TExeSymbol);override;
      procedure GOTRelocPass1(objsec:TObjSection;var idx:longint);override;
      procedure DoRelocationFixup(objsec:TObjSection);override;
    end;

  const
    { Relocation types }
    R_SPC32_NONE = 0;
    R_SPC32_32 = 1;
    R_SPC32_PCREL = 2;
    R_SPC32_LO16 = 3;
    R_SPC32_HI16 = 4;


{****************************************************************************
                               ELF Target methods
****************************************************************************}

  function elf_spc32_encodereloc(objrel:TObjRelocation):byte;
    begin
      case objrel.typ of
        RELOC_NONE :
          result:=R_SPC32_NONE;
        RELOC_RELATIVE :
          result:=R_SPC32_PCREL;
        RELOC_ABSOLUTE :
          result:=R_SPC32_32;
        RELOC_LO16:
          result:=R_SPC32_LO16;
        RELOC_HI16:
          result:=R_SPC32_HI16;
      else
        result:=0;
        InternalError(2012082301);
      end;
    end;


  procedure elf_spc32_loadreloc(objrel:TObjRelocation);
    begin
    end;


  function elf_spc32_relocname(reltyp:byte):string;
    begin
      result:='TODO';
    end;

{****************************************************************************
                               TElfExeOutput386
****************************************************************************}


  procedure TElfExeOutputSpc32.WriteFirstPLTEntry;
    begin
    end;


  procedure TElfExeOutputSpc32.WritePLTEntry(exesym:TExeSymbol);
    var
      got_offset: aword;
      tmp:pint;
    begin
    end;


  procedure TElfExeOutputSpc32.WriteIndirectPLTEntry(exesym:TExeSymbol);
    begin
      // TODO
      inherited WriteIndirectPLTEntry(exesym);
    end;


  procedure TElfExeOutputSpc32.GOTRelocPass1(objsec:TObjSection;var idx:longint);
    var
      objsym:TObjSymbol;
      objreloc:TObjRelocation;
      reltyp:byte;
    begin
    end;


  procedure TElfExeOutputSpc32.MaybeWriteGOTEntry(reltyp:byte;relocval:aint;objsym:TObjSymbol);
    var
      gotoff,tmp:aword;
    begin
    end;


  procedure TElfExeOutputSpc32.DoRelocationFixup(objsec:TObjSection);
    var
      i,zero:longint;
      objreloc: TObjRelocation;
      address,
      relocval : aint;
      relocsec : TObjSection;
      data: TDynamicArray;
      reltyp: byte;
      PC: aword;
    begin
      data:=objsec.data;
      for i:=0 to objsec.ObjRelocations.Count-1 do
        begin
          objreloc:=TObjRelocation(objsec.ObjRelocations[i]);
          case objreloc.typ of
            RELOC_NONE:
              continue;
            RELOC_ZERO:
              begin
                data.Seek(objreloc.dataoffset);
                zero:=0;
                data.Write(zero,2);
                continue;
              end;
          end;

          if (objreloc.flags and rf_raw)=0 then
            reltyp:=ElfTarget.encodereloc(objreloc)
          else
            reltyp:=objreloc.ftype;

          if ElfTarget.relocs_use_addend then
            address:=objreloc.orgsize
          else
            begin
              data.Seek(objreloc.dataoffset);
              data.Read(address,2);
            end;

          if assigned(objreloc.symbol) then
            begin
              relocsec:=objreloc.symbol.objsection;
              relocval:=objreloc.symbol.address;
            end
          else if assigned(objreloc.objsection) then
            begin
              relocsec:=objreloc.objsection;
              relocval:=objreloc.objsection.mempos
            end
          else
            internalerror(2012060702);

          { Only debug sections are allowed to have relocs pointing to unused sections }
          if assigned(relocsec) and not (relocsec.used and assigned(relocsec.exesection)) and
             not (oso_debug in objsec.secoptions) then
            begin
              writeln(objsec.fullname,' references ',relocsec.fullname);
              internalerror(2012060703);
            end;

          PC:=objsec.mempos+objreloc.dataoffset;
          { TODO: if relocsec=nil, relocations must be copied to .rel.dyn section }
          if (relocsec=nil) or (relocsec.used) then
            case reltyp of
              R_SPC32_PCREL:
                begin
                  if (objreloc.flags and rf_dynamic)<>0 then
                    WriteDynRelocEntry(PC,R_SPC32_PCREL,objreloc.symbol.exesymbol.dynindex,0)
                  else
                    address:=address+relocval-PC;
                end;

              R_SPC32_32:
                begin
                  if (objreloc.flags and rf_dynamic)<>0 then
                    begin
                      if (objreloc.symbol=nil) or
                         (objreloc.symbol.exesymbol=nil) or
                         (objreloc.symbol.exesymbol.dynindex=0) then
                        begin
                          address:=address+relocval;
                          WriteDynRelocEntry(PC,R_SPC32_PCREL,0,address);
                        end
                      else
                        { Don't modify address in this case, as it serves as addend for RTLD }
                        WriteDynRelocEntry(PC,R_SPC32_32,objreloc.symbol.exesymbol.dynindex,0);
                    end
                  else
                    address:=address+relocval;
                end;

              R_SPC32_LO16:
                address:=address+relocval;
              R_SPC32_HI16:
                address:=address+(relocval shr 16);

              else
                begin
                  writeln(reltyp);
                  internalerror(200604014);
                end;
            end
          else           { not relocsec.Used }
            address:=0;  { Relocation in debug section points to unused section, which is eliminated by linker }

          data.Seek(objreloc.dataoffset);
          data.Write(address,2);
        end;
    end;


{*****************************************************************************
                                    Initialize
*****************************************************************************}

  const
    elf_target_spc32 : TElfTarget =
      (
        max_page_size:     $1000;
        exe_image_base:    $00000000;
        machine_code:      EM_SPC32;
        relocs_use_addend: false;
        dyn_reloc_codes: (
          R_SPC32_PCREL,
          R_SPC32_NONE,
          R_SPC32_NONE,
          R_SPC32_NONE,
          R_SPC32_NONE
        );
        relocname:         @elf_spc32_relocname;
        encodereloc:       @elf_spc32_encodeReloc;
        loadreloc:         @elf_spc32_loadReloc;
        loadsection:       nil;
        encodeflags:       nil;
      );

    as_spc32_elf32_info : tasminfo =
       (
         id     : as_spc32_elf32;
         idtxt  : 'ELF';
         asmbin : '';
         asmcmd : '';
         supported_targets : [system_spc32_embedded];
         flags : [af_outputbinary,af_smartlink_sections,af_supports_dwarf];
         labelprefix : '.L';
         comment : '';
         dollarsign: '$';
       );

initialization
  RegisterAssembler(as_spc32_elf32_info,TElfAssembler);
  ElfExeOutputClass:=TElfExeOutputSpc32;
  ElfTarget:=elf_target_spc32;

end.

