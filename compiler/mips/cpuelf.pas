{
    Copyright (c) 2012 by Sergei Gorelkin

    Includes ELF-related code specific to MIPS

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

interface

{$i fpcdefs.inc}

implementation

  uses
    globtype,sysutils,cutils,cclasses,
    verbose, elfbase,
    systems,aasmbase,ogbase,ogelf,assemble;

  type
    TElfExeOutputMIPS=class(TElfExeOutput)
    private
      gpdispsym: TObjSymbol;
      gnugpsym: TObjSymbol;
      dt_gotsym_value: longint;
      dt_local_gotno_value: longint;
      dt_local_gotno_offset: aword;
      local_got_relocs: TFPObjectList;
      local_got_slots: TFPHashObjectList;
      got_content: array of pint;
      procedure MaybeWriteGOTEntry(reltyp:byte;relocval:aint;objsym:TObjSymbol);
    protected
      procedure PrepareGOT;override;
      function AllocGOTSlot(objsym:TObjSymbol):boolean;override;
      procedure CreateGOTSection;override;
      procedure CreatePLT;override;
      procedure WriteTargetDynamicTags;override;
    //  procedure WriteFirstPLTEntry;override;
      procedure WritePLTEntry(exesym:TExeSymbol);override;
    //  procedure WriteIndirectPLTEntry(exesym:TExeSymbol);override;
      procedure GOTRelocPass1(objsec:TObjSection;var idx:longint);override;
      procedure DoRelocationFixup(objsec:TObjSection);override;
      procedure Do_Mempos;override;
    public
      constructor Create;override;
      destructor Destroy;override;
      procedure FixupRelocations;override;
    end;

  const
    { section types }
    SHT_MIPS_LIBLIST  = $70000000;
    SHT_MIPS_CONFLICT = $70000002;
    SHT_MIPS_GPTAB    = $70000003;
    SHT_MIPS_UCODE    = $70000004;
    SHT_MIPS_DEBUG    = $70000005;
    SHT_MIPS_REGINFO  = $70000006;

    { section flags }
    SHF_MIPS_GPREL    = $10000000;

    { relocations }
    R_MIPS_NONE  = 0;
    R_MIPS_16    = 1;
    R_MIPS_32    = 2;
    R_MIPS_REL32 = 3;
    R_MIPS_26    = 4;
    R_MIPS_HI16  = 5;
    R_MIPS_LO16  = 6;
    R_MIPS_GPREL16 = 7;
    R_MIPS_LITERAL = 8;
    R_MIPS_GOT16   = 9;
    R_MIPS_PC16    = 10;
    R_MIPS_CALL16  = 11;
    R_MIPS_GPREL32 = 12;
    R_MIPS_GOT_HI16 = 21;
    R_MIPS_GOT_LO16 = 22;
    R_MIPS_CALL_HI16 = 30;
    R_MIPS_CALL_LO16 = 31;
    R_MIPS_JALR    = 37;

    { dynamic tags }
    DT_MIPS_RLD_VERSION  = $70000001;
    DT_MIPS_TIME_STAMP   = $70000002;
    DT_MIPS_ICHECKSUM    = $70000003;
    DT_MIPS_IVERSION     = $70000004;
    DT_MIPS_FLAGS        = $70000005;
    DT_MIPS_BASE_ADDRESS = $70000006;
    DT_MIPS_CONFLICT     = $70000008;
    DT_MIPS_LIBLIST      = $70000009;
    DT_MIPS_LOCAL_GOTNO  = $7000000A;
    DT_MIPS_CONFLICTNO   = $7000000B;
    DT_MIPS_LIBLISTNO    = $70000010;
    DT_MIPS_SYMTABNO     = $70000011;
    DT_MIPS_UNREFEXTNO   = $70000012;
    DT_MIPS_GOTSYM       = $70000013;
    DT_MIPS_HIPAGENO     = $70000014;
    DT_MIPS_RLD_MAP      = $70000016;

    { values of DT_MIPS_FLAGS }
    RHF_QUICKSTART = 1;
    RHF_NOTPOT     = 2;


  type
    TElfReginfo=record
      ri_gprmask: longword;
      ri_cprmask: array[0..3] of longword;
      ri_gp_value: longint;     // signed
    end;


  procedure MaybeSwapElfReginfo(var h:TElfReginfo);
    var
      i: longint;
    begin
      if source_info.endian<>target_info.endian then
        begin
          h.ri_gprmask:=swapendian(h.ri_gprmask);
          for i:=0 to 3 do
            h.ri_cprmask[i]:=swapendian(h.ri_cprmask[i]);
          h.ri_gp_value:=swapendian(h.ri_gp_value);
        end;
    end;

{****************************************************************************
                              ELF Target methods
****************************************************************************}

  function elf_mips_encodereloc(objrel:TObjRelocation):byte;
    begin
      case objrel.typ of
        RELOC_NONE:
          result:=R_MIPS_NONE;
        RELOC_ABSOLUTE:
          result:=R_MIPS_32;
      else
        result:=0;
        InternalError(2012110602);
      end;
    end;


  function elf_mips_relocname(reltyp:byte):string;
    begin
      result:='TODO';
    end;


  procedure elf_mips_loadreloc(objrel:TObjRelocation);
    begin
    end;


  function elf_mips_loadsection(objinput:TElfObjInput;objdata:TObjData;const shdr:TElfsechdr;shindex:longint):boolean;
    begin
      case shdr.sh_type of
        SHT_MIPS_REGINFO:
          result:=true;
      else
        writeln('elf_mips_loadsection: ',hexstr(shdr.sh_type,8),' ',objdata.name);
        result:=false;
      end;
    end;


{*****************************************************************************
                                 TElfExeOutputMIPS
*****************************************************************************}

  constructor TElfExeOutputMIPS.Create;
    begin
      inherited Create;
      local_got_relocs:=TFPObjectList.Create(False);
      local_got_slots:=TFPHashObjectList.Create(True);
    end;


  destructor TElfExeOutputMIPS.Destroy;
    begin
      local_got_slots.Free;
      local_got_relocs.Free;
      inherited Destroy;
    end;


  procedure TElfExeOutputMIPS.CreateGOTSection;
    var
      tmp: longword;
    begin
      gotobjsec:=TElfObjSection.create_ext(internalObjData,'.got',
        SHT_PROGBITS,SHF_ALLOC or SHF_WRITE or SHF_MIPS_GPREL,sizeof(pint),sizeof(pint));
      gotobjsec.SecOptions:=[oso_keep];
      { gotpltobjsec is what's pointed to by DT_PLTGOT }
      { TODO: this is not correct; under some circumstances ld can generate PLTs for MIPS,
        using classic model. We'll need to support it, too. }
      gotpltobjsec:=TElfObjSection(gotobjsec);

      internalObjData.SetSection(gotobjsec);
      { TODO: must be an absolute symbol; binutils use linker script to define it  }
      gotsymbol:=internalObjData.SymbolDefine('_gp',AB_GLOBAL,AT_NONE);
      gotsymbol.offset:=$7ff0;
      { also define _gp_disp and __gnu_local_gp }
      gpdispsym:=internalObjData.SymbolDefine('_gp_disp',AB_GLOBAL,AT_NONE);
      gnugpsym:=internalObjData.SymbolDefine('__gnu_local_gp',AB_GLOBAL,AT_NONE);
      { reserved entries }
      gotobjsec.WriteZeros(sizeof(pint));
      tmp:=$80000000;
      if target_info.endian<>source_info.endian then
        tmp:=swapendian(tmp);
      gotobjsec.Write(tmp,sizeof(pint));
    end;


  procedure TElfExeOutputMIPS.CreatePLT;
    begin
      pltobjsec:=TElfObjSection.create_ext(internalObjData,'.plt',
        SHT_PROGBITS,SHF_ALLOC or SHF_EXECINSTR,4,16);
      pltobjsec.SecOptions:=[oso_keep];
    end;


  procedure TElfExeOutputMIPS.WriteTargetDynamicTags;
    begin
      writeDynTag(DT_MIPS_RLD_VERSION,1);
      if not IsSharedLibrary then
        {writeDynTag(DT_MIPS_RLD_MAP,rldmapsec)};
      writeDynTag(DT_MIPS_FLAGS,RHF_NOTPOT);
      if IsSharedLibrary then
        writeDynTag(DT_MIPS_BASE_ADDRESS,0)
      else
        writeDynTag(DT_MIPS_BASE_ADDRESS,ElfTarget.exe_image_base);
      dt_local_gotno_offset:=dynamicsec.size;
      writeDynTag(DT_MIPS_LOCAL_GOTNO,dt_local_gotno_value);
      writeDynTag(DT_MIPS_SYMTABNO,dynsymlist.count+1);
      { ABI says: "Index of first external dynamic symbol not referenced locally" }
      { What the hell is this? BFD writes number of output sections(!!),
        the values found in actual files do not match even that,
        and don't seem to be connected to reality at all... }
      //writeDynTag(DT_MIPS_UNREFEXTNO,0);
      {Index of first dynamic symbol in GOT }
      writeDynTag(DT_MIPS_GOTSYM,dt_gotsym_value+1);
    end;


  procedure TElfExeOutputMIPS.WritePLTEntry(exesym: TExeSymbol);
    begin

    end;


  function TElfExeOutputMIPS.AllocGOTSlot(objsym:TObjSymbol):boolean;
    var
      exesym: TExeSymbol;
    begin
      { MIPS has quite a different way of allocating GOT slots and dynamic relocations }
      result:=false;
      exesym:=objsym.exesymbol;

      if (exesym=nil) then
        InternalError(2013030406);
      if exesym.GotOffset>0 then
        exit;
      make_dynamic_if_undefweak(exesym);

      if (exesym.dynindex>0) and (exesym.ObjSymbol.ObjSection=nil) then
        begin
          { External symbols must be located at the end of GOT, here just
            mark them for dealing later. }
          exesym.GotOffset:=high(aword);
          exit;
        end;
      gotobjsec.alloc(sizeof(pint));
      exesym.GotOffset:=gotobjsec.size;
      result:=true;
    end;


  function put_externals_last(p1,p2:pointer):longint;
    var
      sym1: TExeSymbol absolute p1;
      sym2: TExeSymbol absolute p2;
    begin
      result:=ord(sym1.gotoffset=high(aword))-ord(sym2.gotoffset=high(aword));
    end;


  function address_ascending(p1,p2:pointer):longint;
    var
      reloc1: TObjRelocation absolute p1;
      reloc2: TObjRelocation absolute p2;
    begin
      result:=(reloc1.symbol.address+reloc1.orgsize)-(reloc2.symbol.address+reloc2.orgsize);
    end;


  procedure TElfExeOutputMIPS.PrepareGOT;
    var
      i: longint;
      exesym: TExeSymbol;
    begin
      inherited PrepareGOT;
      { !! maybe incorrect, where do 'unmapped globals' belong? }
      dt_local_gotno_value:=gotobjsec.size div sizeof(pint);

      if not dynamiclink then
        exit;
      { make room for first R_MIPS_NONE entry }
      if dynrelsize>0 then
        begin
          dynrelocsec.alloc(dynrelocsec.shentsize);
          inc(dynrelsize,dynrelocsec.shentsize);
        end;
      dynsymlist.sort(@put_externals_last);
      { reindex, as sorting could changed the order }
      for i:=0 to dynsymlist.count-1 do
        TExeSymbol(dynsymlist[i]).dynindex:=i+1;
      { find the symbol to be written as DT_GOTSYM }
      for i:=dynsymlist.count-1 downto 0 do
        begin
          exesym:=TExeSymbol(dynsymlist[i]);
          if exesym.gotoffset<>high(aword) then
            begin
              dt_gotsym_value:=i+1;
              break;
            end;
        end;

      { actually allocate GOT slots for imported symbols }
      for i:=dt_gotsym_value to dynsymlist.count-1 do
        begin
          exesym:=TExeSymbol(dynsymlist[i]);
          gotobjsec.alloc(sizeof(pint));
          exesym.GotOffset:=gotobjsec.size;
        end;
      gotsize:=gotobjsec.size;
    end;


  procedure TElfExeOutputMIPS.Do_Mempos;
    var
      i:longint;
      objrel:TObjRelocation;
      addr,page:aword;
      numpages,tmp:longint;
      objsym:TObjSymbol;
      exesym:TExeSymbol;
      got_local_area_start:aword;
    begin
      inherited Do_Mempos;
      { determine required amount of 64k page entries }
      local_got_relocs.Sort(@address_ascending);
      numpages:=0;
      page:=high(aword);
      for i:=0 to local_got_relocs.count-1 do
        begin
          objrel:=TObjRelocation(local_got_relocs[i]);
          addr:=objrel.symbol.address+objrel.orgsize;
          addr:=addr-smallint(addr);
          if (page<>addr shr 16) then
            inc(numpages);
          page:=addr shr 16;
        end;

      if (numpages=0) then
        exit;

      { An additional page may be consumed when we add slots to GOT }
      inc(numpages);

      { Make space in GOT }
      got_local_area_start:=dt_local_gotno_value;
      inc(gotsize,numpages*sizeof(pint));
      gotobjsec.alloc(numpages*sizeof(pint));

      { Redo layout }
      inherited Do_Mempos;

      { Now assign GOT offsets to local slots }
      SetLength(got_content,numpages);
      page:=high(aword);
      tmp:=-1;
      objsym:=nil;
      for i:=0 to local_got_relocs.count-1 do
        begin
          objrel:=TObjRelocation(local_got_relocs[i]);
          addr:=objrel.symbol.address+objrel.orgsize;
          { the contents of slot }
          addr:=addr-smallint(addr);
          if (page<>addr) then
            begin
              Inc(tmp);
              if (tmp>=numpages) then
                InternalError(2013030402);
              { replace relocation symbol with one pointing to GOT slot }
              objsym:=TObjSymbol.Create(local_got_slots,hexstr(addr,8));
              objsym.offset:=(got_local_area_start+tmp+1)*sizeof(pint);
              objsym.bind:=AB_LOCAL;
              if (source_info.endian=target_info.endian) then
                got_content[tmp]:=addr
              else
                got_content[tmp]:=swapendian(addr);
              page:=addr;
            end;
          objrel.symbol:=objsym;
        end;

      if dynamiclink then
        begin
          { Patch DT_LOCAL_GOTNO value }
          if (dt_local_gotno_offset=0) then
            InternalError(2013030401);
          i:=dynamicsec.size;
          dynamicsec.Data.Seek(dt_local_gotno_offset);
          writeDynTag(DT_MIPS_LOCAL_GOTNO,dt_local_gotno_value+numpages);
          dynamicsec.size:=i;

          { Increase gotoffset of exesymbols that come after dt_gotsym }
          for i:=dt_gotsym_value to dynsymlist.count-1 do
            begin
              exesym:=TExeSymbol(dynsymlist[i]);
              exesym.GotOffset:=exesym.GotOffset+(numpages*sizeof(pint));
            end;
        end;
    end;


  procedure TElfExeOutputMIPS.FixupRelocations;
    begin
      if dynrelsize>0 then
        WriteDynRelocEntry(0,R_MIPS_NONE,0,0);

      inherited FixupRelocations;
      { Since we omit GOT slots for imported symbols during inherited PrepareGOT, they don't
        get written in FixupRelocations either. This must be compensated here. }
      gotobjsec.write(got_content[0],length(got_content)*sizeof(pint));

      { TODO: shouldn't be zeroes, but address of stubs if address taken, etc. }
      gotobjsec.writeZeros(gotsize-gotobjsec.size);
    end;

  procedure TElfExeOutputMIPS.MaybeWriteGOTEntry(reltyp:byte;relocval:aint;objsym:TObjSymbol);
    var
      gotoff:aword;
    begin
      if (objsym.bind=AB_LOCAL) then
        InternalError(2013030403);
      gotoff:=objsym.exesymbol.gotoffset;
      if gotoff=0 then
        InternalError(2012060902);

      { On MIPS, GOT does not need dynamic relocations }
      if gotoff=gotobjsec.Data.size+sizeof(pint) then
        begin
          if source_info.endian<>target_info.endian then
            relocval:=swapendian(relocval);
          gotobjsec.write(relocval,sizeof(pint));
        end;
    end;

  procedure TElfExeOutputMIPS.GOTRelocPass1(objsec:TObjSection;var idx:longint);
    var
      objreloc:TObjRelocation;
      lowreloc:TObjRelocation;
      reltyp:byte;
      externsym:boolean;
      found:boolean;
      i:longint;
      lopart,hipart:longword;
    begin
      objreloc:=TObjRelocation(objsec.ObjRelocations[idx]);
      if (ObjReloc.flags and rf_raw)=0 then
        reltyp:=ElfTarget.encodereloc(ObjReloc)
      else
        reltyp:=ObjReloc.ftype;

      case reltyp of
        R_MIPS_32:
          begin
            externsym:=assigned(objreloc.symbol) and
              assigned(objreloc.symbol.exesymbol) and
              (objreloc.symbol.exesymbol.dynindex<>0);

            if IsSharedLibrary then
              begin
                dynrelocsec.alloc(dynrelocsec.shentsize);
                objreloc.flags:=objreloc.flags or rf_dynamic;
                if (not externsym) then
                  Inc(relative_reloc_count);
              end
          end;

        R_MIPS_CALL16,
        R_MIPS_GOT16:
          begin
            if objreloc.symbol.bind<>AB_LOCAL then
              AllocGOTSlot(objreloc.symbol)
            else
              begin
                { Extract the addend, which is stored split between this relocation and
                  the following (maybe not immediately) R_MIPS_LO16 one. }
                found:=false;
                for i:=idx+1 to objsec.ObjRelocations.Count-1 do
                  begin
                    lowreloc:=TObjRelocation(objsec.ObjRelocations[i]);
                    if (lowreloc.flags and rf_raw)=0 then
                      InternalError(2013030101);
                    if (lowreloc.ftype=R_MIPS_LO16) then
                      begin;
                        found:=true;
                        break;
                      end;
                  end;
                if not found then
                  InternalError(2013030102);
                objsec.Data.Seek(objreloc.DataOffset);
                objsec.Data.Read(hipart,sizeof(hipart));
                objsec.Data.Seek(lowreloc.DataOffset);
                objsec.Data.Read(lopart,sizeof(lopart));
                if (source_info.endian<>target_info.endian) then
                  begin
                    hipart:=swapendian(hipart);
                    lopart:=swapendian(lopart);
                  end;
                objreloc.orgsize:=(hipart shl 16)+SmallInt(lopart);
                local_got_relocs.add(objreloc);
              end;
          end;
      end;
    end;

  type
    PRelocData=^TRelocData;
    TRelocData=record
      next:PRelocData;
      objsec:TObjSection;
      objrel:TObjRelocation;
      addend:aint;
    end;

  procedure TElfExeOutputMIPS.DoRelocationFixup(objsec:TObjSection);
    var
      i,zero:longint;
      objreloc: TObjRelocation;
      AHL_S,
      tmp,
      address,
      relocval : aint;
      relocsec : TObjSection;
      data: TDynamicArray;
      reltyp: byte;
      curloc: aword;
      reloclist,hr: PRelocData;
      is_gp_disp: boolean;
    begin
      data:=objsec.data;
      reloclist:=nil;
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
              if source_info.endian<>target_info.endian then
                address:=swapendian(address);
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

          curloc:=objsec.mempos+objreloc.dataoffset;
          if (relocsec=nil) or (relocsec.used) then
            case reltyp of

            R_MIPS_32:
              begin
                address:=address+relocval;
                if (objreloc.flags and rf_dynamic)<>0 then
                  begin
                    if (objreloc.symbol=nil) or
                       (objreloc.symbol.exesymbol=nil) or
                       (objreloc.symbol.exesymbol.dynindex=0) then
                      WriteDynRelocEntry(curloc,R_MIPS_REL32,0,address)
                    else
                      dynreloclist.add(TObjRelocation.CreateRaw(curloc,objreloc.symbol,R_MIPS_REL32));
                  end;
               end;

            R_MIPS_26:
              begin
                tmp:=(address and $03FFFFFF) shl 2;
                tmp:=((tmp or (curloc and $F0000000))+relocval) shr 2;
                address:=(address and $FC000000) or (tmp and $3FFFFFF);
              end;

            R_MIPS_HI16:
              begin
                { This relocation can be handled only after seeing a matching LO16 one,
                  moreover BFD supports any number of HI16 to precede a single LO16.
                  So just add it to a queue. }
                new(hr);
                hr^.next:=reloclist;
                hr^.objrel:=objreloc;
                hr^.objsec:=objsec;
                hr^.addend:=address;  //TODO: maybe it can be saved in objrel.orgsize field
                reloclist:=hr;
              end;

            R_MIPS_LO16:
              begin
                { LO16 may be without pair, e.g. in following sequence:
                    lui   $v0, %hi(foo)
                    lw    $a0, %lo(foo)($v0)
                    lw    $a1, %lo(foo+4)($v0)
                }
                AHL_S:=SmallInt(address)+relocval;
                is_gp_disp:=false;
                while assigned(reloclist) do
                  begin
                    hr:=reloclist;
                    reloclist:=hr^.next;
                    // if relocval<>hr^.relocval then  // must be the same symbol
                    //   InternalError();
                    { _gp_disp and __gnu_local_gp magic }
                    if assigned(hr^.objrel.symbol) and
                      (hr^.objrel.symbol.bind<>AB_LOCAL) then
                      begin
                        is_gp_disp:=(hr^.objrel.symbol.exesymbol.objsymbol=gpdispsym);
                        if (hr^.objrel.symbol.exesymbol.objsymbol=gnugpsym) then
                          relocval:=gotsymbol.address;
                      end;

                    { in case of _gp_disp, non-zero addend is not possible? }
                    { 4 must be added right here, so possible overflow in low half
                      is propagated into high one (e.g if displacement is $37ffc,
                      high part must be 4, not 3) }
                    if is_gp_disp then
                      relocval:=gotsymbol.address-curloc+4;
                    AHL_S:=(hr^.addend shl 16)+SmallInt(address)+relocval;

                    case hr^.objrel.ftype of

                      R_MIPS_HI16:
                        tmp:=(AHL_S-SmallInt(AHL_S)) shr 16;

                      R_MIPS_GOT16:
                        tmp:=-(gotsymbol.offset-(hr^.objrel.symbol.offset-sizeof(pint)));

                    else
                      InternalError(2013030404);
                    end;

                    tmp:=(hr^.addend and $FFFF0000) or (tmp and $FFFF);
                    data.seek(hr^.objrel.dataoffset);
                    if source_info.endian<>target_info.endian then
                      tmp:=swapendian(tmp);
                    data.Write(tmp,4);
                    dispose(hr);
                  end;
                address:=(address and $FFFF0000) or (AHL_S and $FFFF);
              end;

            R_MIPS_CALL16,
            R_MIPS_GOT16:
              begin
                { GOT16 relocations against local symbols are followed by LO16 }
                if (objreloc.symbol.bind=AB_LOCAL) then
                  begin
                    new(hr);
                    hr^.next:=reloclist;
                    hr^.objrel:=objreloc;
                    hr^.objsec:=objsec;
                    hr^.addend:=address;  //TODO: maybe it can be saved in objrel.orgsize field
                    reloclist:=hr;
                    continue;
                  end;
                MaybeWriteGOTEntry(reltyp,relocval,objreloc.symbol);
                // !! this is correct only while _gp symbol is defined relative to .got !!
                relocval:=-(gotsymbol.offset-(objreloc.symbol.exesymbol.gotoffset-sizeof(pint)));
                // TODO: check overflow
                address:=(address and $FFFF0000) or (relocval and $FFFF);
              end;

            R_MIPS_PC16:
              //TODO: check overflow
              address:=(address and $FFFF0000) or ((((SmallInt(address) shl 2)+relocval-curloc) shr 2) and $FFFF);

            R_MIPS_JALR: {optimization hint, ignore for now }
              ;
          else
            begin
              writeln(objsec.fullname,'+',objreloc.dataoffset,' ',objreloc.ftype);
              internalerror(200604014);
            end;
          end
        else           { not relocsec.Used }
          address:=0;  { Relocation in debug section points to unused section, which is eliminated by linker }

        data.Seek(objreloc.dataoffset);
        if source_info.endian<>target_info.endian then
          address:=swapendian(address);
        data.Write(address,4);
      end;
    end;

{*****************************************************************************
                                    Initialize
*****************************************************************************}

  const
    elf_target_mips: TElfTarget =
      (
        max_page_size:     $10000;
        exe_image_base:    $400000;
        machine_code:      EM_MIPS;
        relocs_use_addend: false;
        dyn_reloc_codes: (
          0,
          0,
          0,
          0,
          0
        );
        relocname:         @elf_mips_relocName;
        encodereloc:       @elf_mips_encodeReloc;
        loadreloc:         @elf_mips_loadReloc;
        loadsection:       @elf_mips_loadSection;
      );

initialization
  ElfTarget:=elf_target_mips;
  ElfExeOutputClass:=TElfExeOutputMIPS;

end.

