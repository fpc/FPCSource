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
    globtype,cclasses,
    verbose,elfbase,
    systems,aasmbase,ogbase,ogelf,assemble;

  type
    TElfExeOutput386=class(TElfExeOutput)
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
    R_386_NONE = 0;
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
                               ELF Target methods
****************************************************************************}

  function elf_i386_encodereloc(objrel:TObjRelocation):byte;
    begin
      case objrel.typ of
        RELOC_NONE :
          result:=R_386_NONE;
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
        RELOC_GOTOFF:
          result:=R_386_GOTOFF;
      else
        result:=0;
        InternalError(2012082301);
      end;
    end;


  procedure elf_i386_loadreloc(objrel:TObjRelocation);
    begin
    end;


  function elf_i386_relocname(reltyp:byte):string;
    begin
      result:='TODO';
    end;

{****************************************************************************
                               TElfExeOutput386
****************************************************************************}


  procedure TElfExeOutput386.WriteFirstPLTEntry;
    begin
      if IsSharedLibrary then
        // push 4(%ebx);  jmp  *8(%ebx)
        pltobjsec.writeBytes(#$FF#$B3#$04#$00#$00#$00#$FF#$A3#$08#$00#$00#$00)
      else
        begin
          pltobjsec.writeBytes(#$FF#$35);         // push got+4
          pltobjsec.writeReloc_internal(gotpltobjsec,sizeof(pint),4,RELOC_ABSOLUTE);
          pltobjsec.writeBytes(#$FF#$25);         // jmp  *got+8
          pltobjsec.writeReloc_internal(gotpltobjsec,2*sizeof(pint),4,RELOC_ABSOLUTE);
        end;
      pltobjsec.writeBytes(#$90#$90#$90#$90);     // nop
    end;


  procedure TElfExeOutput386.WritePLTEntry(exesym:TExeSymbol);
    var
      got_offset: aword;
      tmp:pint;
    begin
      got_offset:=gotpltobjsec.size;
      if IsSharedLibrary then
        begin
          pltobjsec.writeBytes(#$FF#$A3);   // jmp got+x(%ebx)
          pltobjsec.write(got_offset,4);
        end
      else
        begin
          pltobjsec.writeBytes(#$FF#$25);   // jmp *got+x
          pltobjsec.writeReloc_internal(gotpltobjsec,got_offset,4,RELOC_ABSOLUTE);
        end;
      pltobjsec.writeBytes(#$68);           // push  $index
      tmp:=pltrelocsec.size;
      pltobjsec.write(tmp,4);

      pltobjsec.writeBytes(#$E9);           // jmp   .plt
      tmp:=-(4+pltobjsec.Size);
      pltobjsec.write(tmp,4);

      { write a .got.plt slot pointing back to the 'push' instruction }
      gotpltobjsec.writeReloc_internal(pltobjsec,pltobjsec.size-(16-6),sizeof(pint),RELOC_ABSOLUTE);

      { write a .rel.plt entry }
      pltrelocsec.writeReloc_internal(gotpltobjsec,got_offset,sizeof(pint),RELOC_ABSOLUTE);
      got_offset:=(exesym.dynindex shl 8) or R_386_JUMP_SLOT;
      pltrelocsec.write(got_offset,sizeof(pint));
      if ElfTarget.relocs_use_addend then
        pltrelocsec.writezeros(sizeof(pint));
    end;


  procedure TElfExeOutput386.WriteIndirectPLTEntry(exesym:TExeSymbol);
    begin
      // TODO
      inherited WriteIndirectPLTEntry(exesym);
    end;


  procedure TElfExeOutput386.GOTRelocPass1(objsec:TObjSection;var idx:longint);
    var
      objsym:TObjSymbol;
      objreloc:TObjRelocation;
      reltyp:byte;
    begin
      objreloc:=TObjRelocation(objsec.ObjRelocations[idx]);
      if (ObjReloc.flags and rf_raw)=0 then
        reltyp:=ElfTarget.encodereloc(ObjReloc)
      else
        reltyp:=ObjReloc.ftype;
      case reltyp of

        R_386_PLT32:
          begin
            objsym:=objreloc.symbol.exesymbol.ObjSymbol;
            objsym.refs:=objsym.refs or symref_plt;
          end;

        R_386_32:
          if (oso_executable in objsec.SecOptions) or
            not (oso_write in objsec.SecOptions) then
            begin
              if assigned(objreloc.symbol) and assigned(objreloc.symbol.exesymbol) then
                begin
                  objsym:=objreloc.symbol.exesymbol.ObjSymbol;
                  objsym.refs:=objsym.refs or symref_from_text;
                end;
            end;
      end;

      case reltyp of

        R_386_TLS_IE:
          begin

            AllocGOTSlot(objreloc.symbol);
          end;

        R_386_GOT32:
          begin
            AllocGOTSlot(objreloc.symbol);
          end;

        R_386_32:
          begin
            { TODO: How to handle absolute relocation to *weak* external symbol
              from executable? See test/tweaklib2, symbol test2, ld handles it
              differently for PIC and non-PIC code. In non-PIC code it drops
              dynamic relocation altogether. }
            if not IsSharedLibrary then
              exit;
            if (oso_executable in objsec.SecOptions) or
               not (oso_write in objsec.SecOptions) then
              hastextrelocs:=True;
            dynrelocsec.alloc(dynrelocsec.shentsize);
            objreloc.flags:=objreloc.flags or rf_dynamic;
          end;

        R_386_PC32:
          begin
            if not IsSharedLibrary then
              exit;
            { In shared library PC32 reloc to external symbol cannot be redirected
              to PLT entry, because PIC PLT relies on ebx register set properly. }
            if assigned(objreloc.symbol) and
              (
                (objreloc.symbol.objsection=nil) or
                (oso_plt in objreloc.symbol.objsection.SecOptions)
              ) then
              begin
                { Must be a dynamic symbol }
                if not (assigned(objreloc.symbol.exesymbol) and
                   (objreloc.symbol.exesymbol.dynindex<>0)) then
                  InternalError(2012101201);
                if (oso_executable in objsec.SecOptions) or
                  not (oso_write in objsec.SecOptions) then
                  hastextrelocs:=True;
                dynrelocsec.alloc(dynrelocsec.shentsize);
                objreloc.flags:=objreloc.flags or rf_dynamic;
              end;
          end;
      end;
    end;


  procedure TElfExeOutput386.MaybeWriteGOTEntry(reltyp:byte;relocval:aint;objsym:TObjSymbol);
    var
      gotoff,tmp:aword;
    begin
      gotoff:=objsym.exesymbol.gotoffset;
      if gotoff=0 then
        InternalError(2012060902);

      { the GOT slot itself, and a dynamic relocation for it }
      { TODO: only data symbols must get here }
      if gotoff=gotobjsec.Data.size+sizeof(pint) then
        begin
          gotobjsec.write(relocval,sizeof(pint));

          tmp:=gotobjsec.mempos+gotoff-sizeof(pint);
          if (objsym.exesymbol.dynindex>0) then
            begin
              if (reltyp=R_386_TLS_IE) then
                if IsSharedLibrary then
                  WriteDynRelocEntry(tmp,R_386_TLS_TPOFF,objsym.exesymbol.dynindex,0)
                else
              else
                WriteDynRelocEntry(tmp,R_386_GLOB_DAT,objsym.exesymbol.dynindex,0)
            end
          else if IsSharedLibrary then
            WriteDynRelocEntry(tmp,R_386_RELATIVE,0,relocval);
        end;
    end;


  procedure TElfExeOutput386.DoRelocationFixup(objsec:TObjSection);
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
                data.Write(zero,4);
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
              data.Read(address,4);
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
              R_386_PC32:
                begin
                  if (objreloc.flags and rf_dynamic)<>0 then
                    WriteDynRelocEntry(PC,R_386_PC32,objreloc.symbol.exesymbol.dynindex,0)
                  else
                    address:=address+relocval-PC;
                end;

              R_386_PLT32:
                begin
                  { If target is in current object, treat as RELOC_RELATIVE }
                  address:=address+relocval-PC;
                end;

              R_386_32:
                begin
                  if (objreloc.flags and rf_dynamic)<>0 then
                    begin
                      if (objreloc.symbol=nil) or
                         (objreloc.symbol.exesymbol=nil) or
                         (objreloc.symbol.exesymbol.dynindex=0) then
                        begin
                          address:=address+relocval;
                          WriteDynRelocEntry(PC,R_386_RELATIVE,0,address);
                        end
                      else
                        { Don't modify address in this case, as it serves as addend for RTLD }
                        WriteDynRelocEntry(PC,R_386_32,objreloc.symbol.exesymbol.dynindex,0);
                    end
                  else
                    address:=address+relocval;
                end;

              R_386_GOTPC:
                begin
                  address:=address+gotsymbol.address-PC;
                end;

              R_386_GOT32:
                begin
                  MaybeWriteGOTEntry(reltyp,relocval,objreloc.symbol);

                  relocval:=gotobjsec.mempos+objreloc.symbol.exesymbol.gotoffset-sizeof(pint)-gotsymbol.address;
                  address:=address+relocval;
                end;

              R_386_GOTOFF:
                begin
                  address:=address+relocval-gotsymbol.address;
                end;


              R_386_TLS_IE:
                begin
                  relocval:=-(tlsseg.MemPos+tlsseg.MemSize-relocval);
                  MaybeWriteGOTEntry(reltyp,relocval,objreloc.symbol);

                  { Resolves to *absolute* offset of GOT slot }
                  relocval:=gotobjsec.mempos+objreloc.symbol.exesymbol.gotoffset-sizeof(pint);
                  address:=address+relocval;
                end;

              R_386_TLS_LE_32,
              R_386_TLS_LE:
                begin
                  if IsSharedLibrary then
                    begin
                      {
                      if reltyp=R_386_TLS_LE_32 then
                        begin
                          WriteDynRelocEntry(PC,R_386_TLS_TPOFF32,symbol.exesymbol.dynindex,0);
                          address:=tlsseg.MemPos-relocval;
                        end;
                      else
                        begin
                          WriteDynRelocEntry(PC,R_386_TLS_TPOFF,symbol.exesymbol.dynindex,0);
                          address:=address-tlsseg.MemPos;
                        end;
                       }
                    end
                  else if (reltyp=R_386_TLS_LE) then
                    address:=-(tlsseg.MemPos+tlsseg.MemSize-relocval)
                  else
                    address:=tlsseg.MemPos+tlsseg.MemSize-relocval;
                end;

              else
                begin
                  writeln(reltyp);
                  internalerror(200604014);
                end;
            end
          else           { not relocsec.Used }
            address:=0;  { Relocation in debug section points to unused section, which is eliminated by linker }

          data.Seek(objreloc.dataoffset);
          data.Write(address,4);
        end;
    end;


{*****************************************************************************
                                    Initialize
*****************************************************************************}

  const
    elf_target_i386 : TElfTarget =
      (
        max_page_size:     $1000;
        exe_image_base:    $8048000;
        machine_code:      EM_386;
        relocs_use_addend: false;
        dyn_reloc_codes: (
          R_386_RELATIVE,
          R_386_GLOB_DAT,
          R_386_JUMP_SLOT,
          R_386_COPY,
          R_386_IRELATIVE
        );
        relocname:         @elf_i386_relocName;
        encodereloc:       @elf_i386_encodeReloc;
        loadreloc:         @elf_i386_loadReloc;
        loadsection:       nil;
        encodeflags:       nil;
      );

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
                              system_i386_solaris,system_i386_embedded,
                              system_i386_android,system_i386_aros];
         flags : [af_outputbinary,af_smartlink_sections,af_supports_dwarf];
         labelprefix : '.L';
         comment : '';
         dollarsign: '$';
       );

initialization
  RegisterAssembler(as_i386_elf32_info,TElfAssembler);
  ElfExeOutputClass:=TElfExeOutput386;
  ElfTarget:=elf_target_i386;

end.

