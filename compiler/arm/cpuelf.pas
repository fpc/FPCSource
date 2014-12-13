{
    Copyright (c) 2012 by Sergei Gorelkin

    Includes ELF-related code specific to ARM

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
    globtype,cutils,cclasses,
    verbose, elfbase,
    systems,aasmbase,ogbase,ogelf,assemble;

  type
    TElfExeOutputARM=class(TElfExeOutput)
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
    R_ARM_NONE  = 0;
    R_ARM_PC24  = 1;       // deprecated
    R_ARM_ABS32 = 2;
    R_ARM_REL32 = 3;
    R_ARM_LDR_PC_G0 = 4;
    R_ARM_ABS16 = 5;
    R_ARM_ABS12 = 6;
    R_ARM_THM_ABS5 = 7;
    R_ARM_ABS8  = 8;
    R_ARM_SBREL32 = 9;
    R_ARM_THM_CALL = 10;
    R_ARM_THM_PC8 = 11;
    R_ARM_BREL_ADJ = 12;
    R_ARM_TLS_DESC = 13;
    { 14,15,16 are obsolete }
    R_ARM_TLS_DTPMOD32 = 17;
    R_ARM_TLS_DTPOFF32 = 18;
    R_ARM_TLS_TPOFF32 = 19;
    R_ARM_COPY = 20;
    R_ARM_GLOB_DAT = 21;
    R_ARM_JUMP_SLOT = 22;
    R_ARM_RELATIVE = 23;
    R_ARM_GOTOFF32 = 24;
    R_ARM_BASE_PREL = 25;
    R_ARM_GOT_BREL = 26;
    R_ARM_PLT32 = 27;      // deprecated
    R_ARM_CALL = 28;
    R_ARM_JUMP24 = 29;
    R_ARM_THM_JUMP24 = 30;
    R_ARM_BASE_ABS = 31;
    { 32,33,34 are obsolete }
    R_ARM_LDR_SBREL_11_0 = 35;    // deprecated
    R_ARM_ALU_SBREL_19_12 = 36;   // deprecated
    R_ARM_ALU_SBREL_27_20 = 37;   // deprecated
    R_ARM_TARGET1 = 38;
    R_ARM_SBREL31 = 39;           // deprecated
    R_ARM_V4BX = 40;
    R_ARM_TARGET2 = 41;
    R_ARM_PREL31 = 42;
    R_ARM_MOVW_ABS_NC = 43;
    R_ARM_MOVT_ABS = 44;
    R_ARM_MOVW_PREL_NC = 45;
    R_ARM_MOVT_PREL = 46;
    R_ARM_THM_MOVW_ABS_NC = 47;
    R_ARM_THM_MOVT_ABS    = 48;
    R_ARM_THM_MOVW_PREL_NC = 49;
    R_ARM_THM_MOVT_PREL = 50;
    R_ARM_THM_JUMP19 = 51;
    R_ARM_THM_JUMP6 = 52;
    R_ARM_THM_ALU_PREL_11_0 = 53;
    R_ARM_THM_PC12     = 54;
    R_ARM_ABS32_NOI    = 55;
    R_ARM_REL32_NOI    = 56;
    R_ARM_ALU_PC_G0_NC = 57;
    R_ARM_ALU_PC_G0    = 58;
    R_ARM_ALU_PC_G1_NC = 59;
    R_ARM_ALU_PC_G1    = 60;
    R_ARM_ALU_PC_G2    = 61;
    R_ARM_LDR_PC_G1    = 62;
    R_ARM_LDR_PC_G2    = 63;
    R_ARM_LDRS_PC_G0   = 64;
    R_ARM_LDRS_PC_G1   = 65;
    R_ARM_LDRS_PC_G2   = 66;
    R_ARM_LDC_PC_G0    = 67;
    R_ARM_LDC_PC_G1    = 68;
    R_ARM_LDC_PC_G2    = 69;
    R_ARM_ALU_SB_G0_NC = 70;
    R_ARM_ALU_SB_G0    = 71;
    R_ARM_ALU_SB_G1_NC = 72;
    R_ARM_ALU_SB_G1    = 73;
    R_ARM_ALU_SB_G2    = 74;
    R_ARM_LDR_SB_G0    = 75;
    R_ARM_LDR_SB_G1    = 76;
    R_ARM_LDR_SB_G2    = 77;
    R_ARM_LDRS_SB_G0   = 78;
    R_ARM_LDRS_SB_G1   = 79;
    R_ARM_LDRS_SB_G2   = 80;
    R_ARM_LDC_SB_G0    = 81;
    R_ARM_LDC_SB_G1    = 82;
    R_ARM_LDC_SB_G2    = 83;
    R_ARM_MOVW_BREL_NC = 84;
    R_ARM_MOVT_BREL    = 85;
    R_ARM_MOVW_BREL    = 86;
    R_ARM_THM_MOVW_BREL_NC = 87;
    R_ARM_THM_MOVT_BREL = 88;
    R_ARM_THM_MOVW_BREL = 89;
    R_ARM_TLS_GOTDESC   = 90;
    R_ARM_TLS_CALL      = 91;
    R_ARM_TLS_DESCSEQ   = 92;
    R_ARM_THM_TLS_CALL  = 93;
    R_ARM_PLT32_ABS     = 94;
    R_ARM_GOT_ABS = 95;
    R_ARM_GOT_PREL = 96;
    R_ARM_GOT_BREL12 = 97;
    R_ARM_GOTOFF12 = 98;
    R_ARM_GOTRELAX = 99;
    R_ARM_GNU_VTENTRY = 100;   // deprecated - old C++ abi
    R_ARM_GNU_VTINHERIT = 101; // deprecated - old C++ abi
    R_ARM_THM_JUMP11 = 102;
    R_ARM_THM_JUMP8  = 103;
    R_ARM_TLS_GD32   = 104;
    R_ARM_TLS_LDM32  = 105;
    R_ARM_TLS_LDO32  = 106;
    R_ARM_TLS_IE32   = 107;
    R_ARM_TLS_LE32   = 108;
    R_ARM_TLS_LDO12  = 109;
    R_ARM_TLS_LE12   = 110;
    R_ARM_TLS_IE12GP = 111;
    { 112-127 are for private experiments }
    { 128 is obsolete }
    R_ARM_THM_TLS_DESCSEQ = 129;
    R_ARM_IRELATIVE = 160;

    { Section types }
    SHT_ARM_EXIDX          = $70000001;
    SHT_ARM_PREEMPTMAP     = $70000002;
    SHT_ARM_ATTRIBUTES     = $70000003;
    SHT_ARM_DEBUGOVERLAY   = $70000004;
    SHT_ARM_OVERLAYSECTION = $70000005;

    TCB_SIZE = 8;

    { ELF header e_flags }
    EF_ARM_BE8          = $00800000;
    EF_ARM_EABIMASK     = $FF000000;
    EF_ARM_EABI_UNKNOWN = $00000000;
    EF_ARM_EABI_VER1    = $01000000;
    EF_ARM_EABI_VER2    = $02000000;
    EF_ARM_EABI_VER3    = $03000000;
    EF_ARM_EABI_VER4    = $04000000;
    EF_ARM_EABI_VER5    = $05000000;

  { Using short identifiers to save typing. This ARM thing has more relocations
    than it has instructions... }
  const
    g0=1;
    g1=2;
    g2=3;
    gpmask=3;
    pc=4;
    nc=8;
    thm=16;

  type
    TArmRelocProp=record
      name: PChar;
      flags: byte;      // bits 0,1: group, bit 2: PC-relative, bit 3: unchecked,
                        // bit 4: THUMB
    end;

  const
    relocprops: array[0..111] of TArmRelocProp = (
      (name: 'R_ARM_NONE';     flags: 0),                //
      (name: 'R_ARM_PC24';     flags: pc),               //
      (name: 'R_ARM_ABS32';    flags: 0),                //
      (name: 'R_ARM_REL32';    flags: pc),               //
      (name: 'R_ARM_LDR_PC_G0'; flags: g0+pc),           //
      (name: 'R_ARM_ABS16';    flags: 0),
      (name: 'R_ARM_ABS12';    flags: 0),
      (name: 'R_ARM_THM_ABS5'; flags: thm),
      (name: 'R_ARM_ABS8';     flags: 0),
      (name: 'R_ARM_SBREL32';  flags: 0),
      (name: 'R_ARM_THM_CALL'; flags: thm),
      (name: 'R_ARM_THM_PC8';  flags: pc+thm),
      (name: 'R_ARM_BREL_ADJ'; flags: 0),
      (name: 'R_ARM_TLS_DESC'; flags: 0),
      (name: 'obsolete(14)';   flags: 0),
      (name: 'obsolete(15)';   flags: 0),
      (name: 'obsolete(16)';   flags: 0),
      (name: 'R_ARM_TLS_DTPMOD32'; flags: 0),
      (name: 'R_ARM_TLS_DTPOFF32'; flags: 0),
      (name: 'R_ARM_TLS_TPOFF32';  flags: 0),
      (name: 'R_ARM_COPY';     flags: 0),
      (name: 'R_ARM_GLOB_DAT'; flags: 0),
      (name: 'R_ARM_JUMP_SLOT'; flags: 0),
      (name: 'R_ARM_RELATIVE'; flags: 0),
      (name: 'R_ARM_GOTOFF32'; flags: 0),
      (name: 'R_ARM_BASE_PREL'; flags: pc),              //
      (name: 'R_ARM_GOT_BREL'; flags: 0),                //
      (name: 'R_ARM_PLT32';    flags: pc),               //
      (name: 'R_ARM_CALL';     flags: pc),               //
      (name: 'R_ARM_JUMP24';   flags: pc),               //
      (name: 'R_ARM_THM_JUMP24'; flags: thm),
      (name: 'R_ARM_BASE_ABS'; flags: 0),
      (name: 'obsolete(32)';   flags: 0),
      (name: 'obsolete(33)';   flags: 0),
      (name: 'obsolete(34)';   flags: 0),
      (name: 'R_ARM_LDR_SBREL_11_0'; flags: g0),
      (name: 'R_ARM_ALU_SBREL_19_12'; flags: g1),
      (name: 'R_ARM_ALU_SBREL_27_20'; flags: g2),
      (name: 'R_ARM_TARGET1';  flags: 0),
      (name: 'R_ARM_SBREL31';  flags: 0),
      (name: 'R_ARM_V4BX';     flags: 0),
      (name: 'R_ARM_TARGET2';  flags: 0),
      (name: 'R_ARM_PREL31';   flags: 0),
      (name: 'R_ARM_MOVW_ABS_NC'; flags: nc),
      (name: 'R_ARM_MOVT_ABS'; flags: 0),
      (name: 'R_ARM_MOVW_PREL_NC'; flags: nc),
      (name: 'R_ARM_MOVT_PREL'; flags: 0),
      (name: 'R_ARM_THM_MOVW_ABS_NC';  flags: nc+thm),
      (name: 'R_ARM_THM_MOVT_ABS';     flags: thm),
      (name: 'R_ARM_THM_MOVW_PREL_NC'; flags: nc+thm),
      (name: 'R_ARM_THM_MOVT_PREL';    flags: thm),
      (name: 'R_ARM_THM_JUMP19';       flags: thm),
      (name: 'R_ARM_THM_JUMP6';        flags: thm),
      (name: 'R_ARM_THM_ALU_PREL_11_0'; flags: thm+pc),
      (name: 'R_ARM_THM_PC12';         flags: thm+pc),
      (name: 'R_ARM_ABS32_NOI';    flags: 0),
      (name: 'R_ARM_REL32_NOI';    flags: pc),
      (name: 'R_ARM_ALU_PC_G0_NC'; flags: pc+g0+nc),     //
      (name: 'R_ARM_ALU_PC_G0';    flags: pc+g0),        //
      (name: 'R_ARM_ALU_PC_G1_NC'; flags: pc+g1+nc),     //
      (name: 'R_ARM_ALU_PC_G1';    flags: pc+g1),        //
      (name: 'R_ARM_ALU_PC_G2';    flags: pc+g2),        //
      (name: 'R_ARM_LDR_PC_G1';    flags: pc+g1),        //
      (name: 'R_ARM_LDR_PC_G2';    flags: pc+g2),        //
      (name: 'R_ARM_LDRS_PC_G0';   flags: pc+g0),        //
      (name: 'R_ARM_LDRS_PC_G1';   flags: pc+g1),        //
      (name: 'R_ARM_LDRS_PC_G2';   flags: pc+g2),        //
      (name: 'R_ARM_LDC_PC_G0';    flags: pc+g0),        //
      (name: 'R_ARM_LDC_PC_G1';    flags: pc+g1),        //
      (name: 'R_ARM_LDC_PC_G2';    flags: pc+g2),        //
      (name: 'R_ARM_ALU_SB_G0_NC'; flags: g0+nc),        //
      (name: 'R_ARM_ALU_SB_G0';    flags: g0),           //
      (name: 'R_ARM_ALU_SB_G1_NC'; flags: g1+nc),        //
      (name: 'R_ARM_ALU_SB_G1';    flags: g1),           //
      (name: 'R_ARM_ALU_SB_G2';    flags: g2),           //
      (name: 'R_ARM_LDR_SB_G0';    flags: g0),           //
      (name: 'R_ARM_LDR_SB_G1';    flags: g1),           //
      (name: 'R_ARM_LDR_SB_G2';    flags: g2),           //
      (name: 'R_ARM_LDRS_SB_G0';   flags: g0),           //
      (name: 'R_ARM_LDRS_SB_G1';   flags: g1),           //
      (name: 'R_ARM_LDRS_SB_G2';   flags: g2),           //
      (name: 'R_ARM_LDC_SB_G0';    flags: g0),           //
      (name: 'R_ARM_LDC_SB_G1';    flags: g1),           //
      (name: 'R_ARM_LDC_SB_G2';    flags: g2),           //
      (name: 'R_ARM_MOVW_BREL_NC'; flags: nc),
      (name: 'R_ARM_MOVT_BREL';    flags: 0),
      (name: 'R_ARM_MOVW_BREL';    flags: 0),
      (name: 'R_ARM_THM_MOVW_BREL_NC'; flags: nc+thm),
      (name: 'R_ARM_THM_MOVT_BREL'; flags: thm),
      (name: 'R_ARM_THM_MOVW_BREL'; flags: thm),
      (name: 'R_ARM_TLS_GOTDESC';   flags: 0),
      (name: 'R_ARM_TLS_CALL';      flags: 0),
      (name: 'R_ARM_TLS_DESCSEQ';   flags: 0),
      (name: 'R_ARM_THM_TLS_CALL';  flags: 0),
      (name: 'R_ARM_PLT32_ABS';     flags: 0),
      (name: 'R_ARM_GOT_ABS';       flags: 0),
      (name: 'R_ARM_GOT_PREL';      flags: pc),          //
      (name: 'R_ARM_GOT_BREL12';    flags: 0),
      (name: 'R_ARM_GOTOFF12';      flags: 0),
      (name: 'R_ARM_GOTRELAX';      flags: 0),
      (name: 'R_ARM_GNU_VTENTRY';   flags: 0),
      (name: 'R_ARM_GNU_VTINHERIT'; flags: 0),
      (name: 'R_ARM_THM_JUMP11';    flags: thm),
      (name: 'R_ARM_THM_JUMP8';     flags: thm),
      (name: 'R_ARM_TLS_GD32';      flags: 0),
      (name: 'R_ARM_TLS_LDM32';     flags: 0),
      (name: 'R_ARM_TLS_LDO32';     flags: 0),
      (name: 'R_ARM_TLS_IE32';      flags: 0),
      (name: 'R_ARM_TLS_LE32';      flags: 0),
      (name: 'R_ARM_TLS_LDO12';     flags: 0),
      (name: 'R_ARM_TLS_LE12';      flags: 0),
      (name: 'R_ARM_TLS_IE12GP';    flags: 0)
    );

{****************************************************************************
                              ELF Target methods
****************************************************************************}

  function elf_arm_encodereloc(objrel:TObjRelocation):byte;
    begin
      case objrel.typ of
        RELOC_NONE:
          result:=R_ARM_NONE;
        RELOC_ABSOLUTE:
          result:=R_ARM_ABS32;
        RELOC_RELATIVE:
          result:=R_ARM_REL32;
        RELOC_RELATIVE_24:
          result:=R_ARM_JUMP24;
        RELOC_RELATIVE_24_THUMB:
          result:=R_ARM_CALL;
      else
        result:=0;
        writeln(objrel.typ);
        InternalError(2012110602);
      end;
    end;

  function elf_arm_relocname(reltyp:byte):string;
    begin
      if reltyp<=high(relocprops) then
        result:=relocprops[reltyp].name
      else
        case reltyp of
          112..127:
            result:='R_ARM_PRIVATE_'+tostr(reltyp-112);
          R_ARM_THM_TLS_DESCSEQ:
            result:='R_ARM_THM_TLS_DESCSEQ';
          R_ARM_IRELATIVE:
            result:='R_ARM_IRELATIVE';
        else
          result:='unknown ('+tostr(reltyp)+')';
        end;
    end;

  procedure elf_arm_loadreloc(objrel:TObjRelocation);
    begin
      if (objrel.ftype=R_ARM_V4BX) then
        objrel.flags:=objrel.flags or rf_nosymbol;
    end;

  function elf_arm_loadsection(objinput:TElfObjInput;objdata:TObjData;const shdr:TElfsechdr;shindex:longint):boolean;
    var
      secname:string;
    begin
      case shdr.sh_type of
        SHT_ARM_EXIDX,
        SHT_ARM_PREEMPTMAP,
        SHT_ARM_ATTRIBUTES:
          begin
            objinput.CreateSection(shdr,shindex,objdata,secname);
            result:=true;
          end;
      else
        writeln(hexstr(shdr.sh_type,8));
        result:=false;
      end;
    end;

{****************************************************************************
                              TELFExeOutputARM
****************************************************************************}

  function group_reloc_mask(value:longword;n:longint;out final_residual:longword):longword;
    var
      i:longint;
      g_n:longword;
      shift:longint;
    begin
      result:=0;
      for i:=0 to n do
        begin
          if (value=0) then
            shift:=0
          else
            { MSB in the residual, aligned to a 2-bit boundary }
            shift:=max(0,(bsrdword(value) and (not 1))-6);

          { Calculate plain g_n and encode it into constant+rotation form }
          g_n:=value and ($ff shl shift);
          result:=(g_n shr shift);
          if (g_n>$FF) then
            result:=result or ((32-shift) div 2) shl 8;

          { Mask away the processed part of residual }
          value:=value and (not g_n);
        end;
      final_residual:=value;
    end;


  procedure TElfExeOutputARM.MaybeWriteGOTEntry(reltyp:byte;relocval:aint;objsym:TObjSymbol);
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
              WriteDynRelocEntry(tmp,R_ARM_GLOB_DAT,objsym.exesymbol.dynindex,0)
            end
          else if IsSharedLibrary then
            WriteDynRelocEntry(tmp,R_ARM_RELATIVE,0,relocval);
        end;
    end;


  procedure TElfExeOutputARM.WriteFirstPLTEntry;
    begin
      pltobjsec.WriteBytes(
        #$04#$E0#$2D#$E5+       // str   lr, [sp, #-4]!
        #$04#$E0#$9F#$E5+       // ldr   lr, [pc, #4]
        #$0E#$E0#$8F#$E0+       // add   lr, pc, lr
        #$08#$F0#$BE#$E5);      // ldr   pc, [lr, #8]!
                                // .long _GLOBAL_OFFSET_TABLE-.
      pltobjsec.writeReloc_internal(gotpltobjsec,0,4,RELOC_RELATIVE);
    end;


  procedure TElfExeOutputARM.WritePLTEntry(exesym: TExeSymbol);
    var
      tmp: longword;
      sym:TObjSymbol;
    begin
      { TODO: it may be beneficial to postpone processing until after mempos pass,
        and calculate instructions directly, instead of messing with complex relocations. }
      { Group relocation to "section+offset" with REL-style is impossible, because the
        offset has be encoded into instructions, and it is only possible for offsets
        representable as shifter constants. Therefore we need to define a symbol
        (and risk a name conflict, to some degree) }
      internalobjdata.setsection(gotpltobjsec);
      sym:=internalobjdata.SymbolDefine(exesym.name+'_ptr',AB_LOCAL,AT_DATA);
      pltobjsec.WriteBytes(
        #$08#$C0#$4F#$E2+      // add ip,pc,#:pc_g0_nc:sym-8
        #$04#$C0#$4C#$E2+      // add ip,ip,#:pc_g1_nc:sym-4
        #$00#$F0#$BC#$E5);     // ldr pc,[ip,#:pc_g2:sym]!

      pltobjsec.addrawReloc(pltobjsec.size-12,sym,R_ARM_ALU_PC_G0_NC);
      pltobjsec.addrawReloc(pltobjsec.size-8,sym,R_ARM_ALU_PC_G1_NC);
      pltobjsec.addrawReloc(pltobjsec.size-4,sym,R_ARM_LDR_PC_G2);

      { .got.plt slot initially points to the first PLT entry }
      gotpltobjsec.writeReloc_internal(pltobjsec,0,sizeof(pint),RELOC_ABSOLUTE);
      { write a .rel.plt entry (Elf32_rel record) }
      pltrelocsec.writeReloc_internal(gotpltobjsec,gotpltobjsec.size-sizeof(pint),sizeof(pint),RELOC_ABSOLUTE);
      tmp:=(exesym.dynindex shl 8) or R_ARM_JUMP_SLOT;
      pltrelocsec.write(tmp,sizeof(tmp));
      if ElfTarget.relocs_use_addend then
        pltrelocsec.writezeros(sizeof(pint));
    end;


  procedure TElfExeOutputARM.WriteIndirectPLTEntry(exesym: TExeSymbol);
    begin
      inherited WriteIndirectPLTEntry(exesym);
    end;


  procedure TElfExeOutputARM.GOTRelocPass1(objsec:TObjSection;var idx:longint);
    var
      objreloc:TObjRelocation;
      exesym:TExeSymbol;
      objsym:TObjSymbol;
      reltyp:byte;
    begin
      objreloc:=TObjRelocation(objsec.ObjRelocations[idx]);
      if (ObjReloc.flags and rf_raw)=0 then
        reltyp:=ElfTarget.encodereloc(ObjReloc)
      else
        reltyp:=ObjReloc.ftype;

      case reltyp of
        // Any call or jump can go through PLT, no x86-like segregation here.
        R_ARM_PC24,
        R_ARM_CALL,
        R_ARM_JUMP24,
        R_ARM_PREL31,
        R_ARM_THM_CALL,
        R_ARM_THM_JUMP24,
        R_ARM_THM_JUMP19,
        R_ARM_PLT32:
          begin
            if (objreloc.symbol=nil) or (objreloc.symbol.exesymbol=nil) then
              exit;
            exesym:=objreloc.symbol.exesymbol;
            exesym.objsymbol.refs:=exesym.objsymbol.refs or symref_plt;
          end;

        R_ARM_ABS32:
          if Assigned(ObjReloc.symbol.exesymbol) then
            begin
              objsym:=ObjReloc.symbol.exesymbol.ObjSymbol;
              if (oso_executable in objsec.SecOptions) or
                not (oso_write in objsec.SecOptions) then
                  objsym.refs:=objsym.refs or symref_from_text;
            end;
      end;

      case reltyp of
        R_ARM_ABS32:
          begin
            if not IsSharedLibrary then
              exit;
            if (oso_executable in objsec.SecOptions) or
               not (oso_write in objsec.SecOptions) then
              hastextrelocs:=True;
            dynrelocsec.alloc(dynrelocsec.shentsize);
            objreloc.flags:=objreloc.flags or rf_dynamic;
          end;

        //R_ARM_GOT_ABS,
        //R_ARM_GOT_PREL,
        //R_ARM_GOT_BREL12,
        R_ARM_GOT_BREL:
          begin
            AllocGOTSlot(objreloc.symbol);
          end;

        R_ARM_TLS_IE32:
          AllocGOTSlot(objreloc.symbol);

      end;
    end;


  procedure TElfExeOutputARM.DoRelocationFixup(objsec:TObjSection);
  var
    i,zero:longint;
    objreloc: TObjRelocation;
    tmp,
    address,
    relocval : aint;
    relocsec : TObjSection;
    data: TDynamicArray;
    reltyp: byte;
    group:longint;
    rotation:longint;
    residual,g_n:longword;
    curloc: aword;
    bit_S,bit_I1,bit_I2: aint;
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

        { TODO: TARGET1 and TARGET2 are intended to be configured via commandline }
        if (reltyp=R_ARM_TARGET1) then
          reltyp:=R_ARM_ABS32;             { may be ABS32 or REL32 }
        if (reltyp=R_ARM_TARGET2) then
          reltyp:=R_ARM_ABS32;             { may be ABS32,REL32 or GOT_PREL }

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
        else if (reltyp=R_ARM_V4BX) then
          continue        // ignore for now
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

            R_ARM_ABS32:
              begin
                if (objreloc.flags and rf_dynamic)<>0 then
                  begin
                    if (objreloc.symbol=nil) or
                       (objreloc.symbol.exesymbol=nil) or
                       (objreloc.symbol.exesymbol.dynindex=0) then
                      begin
                        address:=address+relocval;
                        WriteDynRelocEntry(objreloc.dataoffset+objsec.mempos,R_ARM_RELATIVE,0,address);
                      end
                    else
                      { Don't modify address in this case, as it serves as addend for RTLD }
                      WriteDynRelocEntry(objreloc.dataoffset+objsec.mempos,R_ARM_ABS32,objreloc.symbol.exesymbol.dynindex,0);
                  end
                else
                  address:=address+relocval;
              end;

            R_ARM_REL32:
              begin
                address:=address+relocval-curloc;
              end;

            R_ARM_PC24,
            R_ARM_PLT32,
            R_ARM_JUMP24,
            R_ARM_CALL:
              begin
                { R_ARM_PC24 is deprecated in favour of R_ARM_JUMP24 and R_ARM_CALL,
                  which allow to distinguish opcodes without examining them.
                  Difference is:
                  1) when target is Thumb, BL can be changed to BLX, while B has
                  to go via thunking code.
                  2) when target is unresolved weak symbol, CALL must be changed to NOP,
                  while JUMP24 behavior is unspecified. }
                tmp:=sarlongint((address and $00FFFFFF) shl 8,6);
                tmp:=tmp+relocval;
                if odd(tmp) then    { dest is Thumb? }
                  begin
                    if (reltyp=R_ARM_CALL) then
                      { change BL to BLX, dest bit 1 goes to instruction bit 24 }
                      address:=(address and $FE000000) or (((tmp-curloc) and 2) shl 23) or $F0000000
                    else
                      InternalError(2014092001);
                  end
                else if (address and $FF000000)=$FA000000 then
                  begin
                    { Change BLX to BL }
                    address:=(address and $EA000000) or $01000000;
                  end;
                tmp:=tmp-curloc;
                // TODO: check overflow
                address:=(address and $FF000000) or ((tmp and $3FFFFFE) shr 2);
              end;

            R_ARM_BASE_PREL:    { GOTPC }
              address:=address+gotsymbol.address-curloc;

            R_ARM_GOT_BREL:     { GOT32 }
              begin
                MaybeWriteGOTEntry(reltyp,relocval,objreloc.symbol);
                address:=address+gotobjsec.mempos+objreloc.symbol.exesymbol.gotoffset-sizeof(pint)-gotsymbol.address;
              end;

            R_ARM_GOTOFF32:
              address:=address+relocval-gotsymbol.address;

            R_ARM_ALU_PC_G0_NC,
            R_ARM_ALU_PC_G1_NC,
            R_ARM_ALU_PC_G0,
            R_ARM_ALU_PC_G1,
            R_ARM_ALU_PC_G2,
            R_ARM_ALU_SB_G0_NC,
            R_ARM_ALU_SB_G1_NC,
            R_ARM_ALU_SB_G0,
            R_ARM_ALU_SB_G1,
            R_ARM_ALU_SB_G2:
              begin
                group:=(relocprops[reltyp].flags and gpmask)-1;
                if group<0 then
                  InternalError(2012112601);

                if (not ElfTarget.relocs_use_addend) then
                  begin
                    { initial addend must be determined by parsing the instruction }
                    tmp:=address and $FF;
                    rotation:=(address and $F00) shr 7;  { is in multpile of 2 bits }
                    if rotation<>0 then
                      tmp:=RorDword(tmp,rotation);
                    case (address and $1E00000) of
                      1 shl 23: ;           { ADD instruction }
                      1 shl 22: tmp:=-tmp;  { SUB instruction }
                    else
                      Comment(v_error,'Group ALU relocations are permitted only for ADD or SUB instructions');
                      continue;
                    end;
                  end
                else  { TODO: must read the instruction anyway!! }
                  tmp:=address;

                if (relocprops[reltyp].flags and pc)<>0 then
                  tmp:=tmp+relocval-curloc
                else
                  tmp:=tmp+relocval{-SB};  { assuming zero segment base }

                g_n:=group_reloc_mask(abs(tmp),group,residual);
                {TODO: check for overflow}

                address:=address and $FF1FF000 or g_n;
                { set opcode depending on the sign of resulting value }
                if tmp<0 then
                  address:=address or (1 shl 22)
                else
                  address:=address or (1 shl 23);
              end;

            R_ARM_LDR_PC_G0,
            R_ARM_LDR_PC_G1,
            R_ARM_LDR_PC_G2,
            R_ARM_LDR_SB_G0,
            R_ARM_LDR_SB_G1,
            R_ARM_LDR_SB_G2:
              begin
                group:=(relocprops[reltyp].flags and gpmask)-1;
                if group<0 then
                  InternalError(2012112602);

                if (not ElfTarget.relocs_use_addend) then
                  begin
                    tmp:=(address and $FFF);
                    if (address and (1 shl 23))=0 then
                      tmp:=-tmp;
                  end
                else   { TODO: must read the instruction anyway }
                  tmp:=address;

                if (relocprops[reltyp].flags and pc)<>0 then
                  tmp:=tmp+relocval-curloc
                else
                  tmp:=tmp+relocval{-SB};  { assuming zero segment base }

                group_reloc_mask(abs(tmp),group-1,residual);
                if residual>$FFF then
                  InternalError(2012112603);  { TODO: meaningful overflow error message }

                address:=address and $FF7FF000 or residual;
                if tmp>=0 then
                  address:=address or (1 shl 23);
              end;

            R_ARM_LDRS_PC_G0,
            R_ARM_LDRS_PC_G1,
            R_ARM_LDRS_PC_G2,
            R_ARM_LDRS_SB_G0,
            R_ARM_LDRS_SB_G1,
            R_ARM_LDRS_SB_G2:
              begin
                group:=(relocprops[reltyp].flags and gpmask)-1;
                if group<0 then
                  InternalError(2012112606);

                if (not ElfTarget.relocs_use_addend) then
                  begin
                    tmp:=((address and $F00) shr 4) or (address and $F);
                    if (address and (1 shl 23))=0 then
                      tmp:=-tmp;
                  end
                else { TODO: must read the instruction anyway }
                  tmp:=address;

                if (relocprops[reltyp].flags and pc)<>0 then
                  tmp:=tmp+relocval-curloc
                else
                  tmp:=tmp+relocval{-SB};  { assuming zero segment base }

                group_reloc_mask(abs(tmp),group-1,residual);
                if (residual>$FF) then
                  InternalError(2012112607); { TODO: meaningful overflow error message }

                address:=address and $FF7FF0F0 or ((residual and $F0) shl 4) or (residual and $F);
                if tmp>=0 then
                  address:=address or (1 shl 23);
              end;

            R_ARM_LDC_PC_G0,
            R_ARM_LDC_PC_G1,
            R_ARM_LDC_PC_G2,
            R_ARM_LDC_SB_G0,
            R_ARM_LDC_SB_G1,
            R_ARM_LDC_SB_G2:
              begin
                group:=(relocprops[reltyp].flags and gpmask)-1;
                if group<0 then
                  InternalError(2012112604);

                if (not ElfTarget.relocs_use_addend) then
                  begin
                    tmp:=(address and $FF) shl 2;
                    if (address and (1 shl 23))=0 then
                      tmp:=-tmp;
                  end
                else { TODO: must read the instruction anyway }
                  tmp:=address;

                if (relocprops[reltyp].flags and pc)<>0 then
                  tmp:=tmp+relocval-curloc
                else
                  tmp:=tmp+relocval{-SB};  { assuming zero segment base }

                group_reloc_mask(abs(tmp),group-1,residual);
                { residual must be divisible by 4 and fit into 8 bits after having been divided }
                if ((residual and 3)<>0) or (residual>$3FF) then
                  InternalError(2012112605);  { TODO: meaningful overflow error message }

                address:=address and $FF7FFF00 or (residual shr 2);
                if tmp>=0 then
                  address:=address or (1 shl 23);
              end;

            R_ARM_THM_CALL:
              begin
                if (not ElfTarget.relocs_use_addend) then
                  begin
                    address:=((address and $ffff) shl 16) or word(address shr 16);
                    bit_S:=(address shr 26) and 1;
                    bit_I1:=(bit_S xor ((address shr 13) and 1)) xor 1;
                    bit_I2:=(bit_S xor ((address shr 11) and 1)) xor 1;
                    tmp:=((-bit_S) shl 24) or (bit_I1 shl 23) or (bit_I2 shl 22) or (((address shr 16) and $3ff) shl 12) or ((address and $7ff) shl 1);
                  end
                else  { TODO: must read the instruction anyway }
                  tmp:=address;
                tmp:=tmp+relocval;       { dest address }
                if odd(tmp) then         { if it's Thumb code, change possible BLX to BL }
                  address:=address or $1800;
                tmp:=tmp-curloc;         { now take PC-relative }
                { TODO: overflow check, different limit for Thumb and Thumb-2 }

                { now encode this mess back }
                if (address and $5000)=$4000 then
                  tmp:=(tmp+2) and (not 3);

                bit_S:=(tmp shr 31) and 1;
                address:=(address and $F800D000) or
                  (bit_S shl 26) or
                  (((tmp shr 12) and $3ff) shl 16) or
                  ((tmp shr 1) and $7FF) or
                  ((((tmp shr 23) and 1) xor 1 xor bit_S) shl 13) or
                  ((((tmp shr 22) and 1) xor 1 xor bit_S) shl 11);
                address:=((address and $ffff) shl 16) or word(address shr 16);
              end;

            R_ARM_TLS_IE32:
              begin
                relocval:=relocval-tlsseg.mempos+align_aword(TCB_SIZE,tlsseg.align);
                MaybeWriteGOTEntry(reltyp,relocval,objreloc.symbol);
                { resolves to PC-relative offset to GOT slot }
                relocval:=gotobjsec.mempos+objreloc.symbol.exesymbol.gotoffset-sizeof(pint);
                address:=address+relocval-curloc;
              end;

            R_ARM_TLS_LE32:
              if IsSharedLibrary then
                { TODO: error message saying "recompile with -Cg" isn't correct. Or is it? }
                ReportNonDSOReloc(reltyp,objsec,objreloc)
              else
                address:=relocval-tlsseg.mempos+align_aword(TCB_SIZE,tlsseg.align);

          else
            begin
              writeln(objreloc.ftype);
              internalerror(200604014);
            end;
          end
        else           { not relocsec.Used }
          address:=0;  { Relocation in debug section points to unused section, which is eliminated by linker }

        data.Seek(objreloc.dataoffset);
        data.Write(address,4);
      end;
    end;


  function elf_arm_encodeflags: longword;
    begin
      result:=EF_ARM_EABI_VER5;
    end;

{*****************************************************************************
                                    Initialize
*****************************************************************************}

  const
    elf_target_arm: TElfTarget =
      (
        max_page_size:     $8000;
        exe_image_base:    $8000;
        machine_code:      EM_ARM;
        relocs_use_addend: false;
        dyn_reloc_codes: (
          R_ARM_RELATIVE,
          R_ARM_GLOB_DAT,
          R_ARM_JUMP_SLOT,
          R_ARM_COPY,
          R_ARM_IRELATIVE
        );
        relocname:         @elf_arm_relocName;
        encodereloc:       @elf_arm_encodeReloc;
        loadreloc:         @elf_arm_loadReloc;
        loadsection:       @elf_arm_loadSection;
        encodeflags:       @elf_arm_encodeflags;
      );

    as_arm_elf32_info : tasminfo =
       (
         id     : as_arm_elf32;
         idtxt  : 'ELF';
         asmbin : '';
         asmcmd : '';
         supported_targets : [system_arm_embedded,system_arm_darwin,
                              system_arm_linux,system_arm_gba,
                              system_arm_nds];
         flags : [af_outputbinary,af_smartlink_sections,af_supports_dwarf];
         labelprefix : '.L';
         comment : '';
         dollarsign: '$';
       );

initialization
  RegisterAssembler(as_arm_elf32_info,TElfAssembler);
  ElfTarget:=elf_target_arm;
  ElfExeOutputClass:=TElfExeOutputARM;

end.

