{
    $Id: ogcoff.pas,v 1.34 2005/02/14 17:13:06 peter Exp $
    Copyright (c) 1998-2002 by Peter Vreman and Pierre Muller

    Contains the binary coff reader and writer

    * This code was inspired by the NASM sources
      The Netwide Assembler is copyright (C) 1996 Simon Tatham and
      Julian Hall. All rights reserved.

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
unit ogcoff;

{$i fpcdefs.inc}

interface

    uses
       { common }
       cclasses,globtype,
       { target }
       systems,
       { assembler }
       cpuinfo,cpubase,aasmbase,assemble,link,
       { output }
       ogbase,ogmap;

    type
       TCoffObjectSection = class(TAsmSection)
       private
         orgmempos,
         coffrelocs,
         coffrelocpos : longint;
       public
         flags    : cardinal;
         constructor create(const Aname:string;Atype:TAsmSectionType;Aalign:longint;Aoptions:TAsmSectionOptions);override;
         procedure addsymsizereloc(ofs:longint;p:tasmsymbol;size:longint;relative:TAsmRelocationType);
         procedure fixuprelocs;override;
       end;

       TDjCoffObjectSection = class(TCoffObjectSection)
         constructor create(const Aname:string;Atype:TAsmSectionType;Aalign:longint;Aoptions:TAsmSectionOptions);override;
       end;

       TPECoffObjectSection = class(TCoffObjectSection)
         constructor create(const Aname:string;Atype:TAsmSectionType;Aalign:longint;Aoptions:TAsmSectionOptions);override;
       end;

       tcoffobjectdata = class(TAsmObjectData)
       private
         win32      : boolean;
         procedure section_mempos(p:tnamedindexitem;arg:pointer);
       public
         constructor createcoff(const n:string;awin32:boolean;acasmsection:TAsmSectionClass);
         destructor  destroy;override;
         function  sectionname(atype:tasmsectiontype;const aname:string):string;override;
         procedure writereloc(data,len:aint;p:tasmsymbol;relative:TAsmRelocationType);override;
         procedure writesymbol(p:tasmsymbol);override;
         procedure writestabs(offset:aint;p:pchar;nidx,nother,line:longint;reloc:boolean);override;
         procedure writesymstabs(offset:aint;p:pchar;ps:tasmsymbol;nidx,nother,line:longint;reloc:boolean);override;
         procedure beforealloc;override;
         procedure beforewrite;override;
         procedure afteralloc;override;
       end;

       tdjcoffobjectdata = class(TCoffObjectData)
         constructor create(const n:string);override;
       end;

       tpecoffobjectdata = class(TCoffObjectData)
         constructor create(const n:string);override;
       end;

       tcoffobjectoutput = class(tobjectoutput)
       private
         win32   : boolean;
         initsym : longint;
         FCoffStrs : tdynamicarray;
         procedure write_symbol(const name:string;value,section,typ,aux:longint);
         procedure section_write_symbol(p:tnamedindexitem;arg:pointer);
         procedure section_write_relocs(p:tnamedindexitem;arg:pointer);
         procedure write_symbols(data:TAsmObjectData);
         procedure section_set_secsymidx(p:tnamedindexitem;arg:pointer);
         procedure section_set_datapos(p:tnamedindexitem;arg:pointer);
         procedure section_set_reloc_datapos(p:tnamedindexitem;arg:pointer);
         procedure section_write_header(p:tnamedindexitem;arg:pointer);
         procedure section_write_data(p:tnamedindexitem;arg:pointer);
       protected
         function writedata(data:TAsmObjectData):boolean;override;
       public
         constructor createdjgpp(smart:boolean);
         constructor createwin32(smart:boolean);
         function newobjectdata(const n:string):TAsmObjectData;override;
       end;

       tcoffexeoutput = class(texeoutput)
       private
         FCoffsyms,
         FCoffStrs : tdynamicarray;
         win32   : boolean;
         nsects,
         nsyms,
         sympos : longint;
         procedure write_symbol(const name:string;value,section,typ,aux:longint);
         procedure write_symbols;
       protected
         function writedata:boolean;override;
       public
         constructor createdjgpp;
         constructor createwin32;
         function  newobjectinput:tobjectinput;override;
         procedure CalculateMemoryMap;override;
         procedure GenerateExecutable(const fn:string);override;
       end;

       ttasmsymbolrec = record
         sym : tasmsymbol;
         orgsize : longint;
       end;
       ttasmsymbolarray = array[0..high(word)] of ttasmsymbolrec;

       tcoffobjectinput = class(tobjectinput)
       private
         Fidx2sec  : array[0..255] of TAsmSection;
         FCoffsyms,
         FCoffStrs : tdynamicarray;
         FSymTbl   : ^ttasmsymbolarray;
         win32     : boolean;
         procedure read_relocs(s:TCoffObjectSection);
         procedure handle_symbols(data:TAsmObjectData);
       protected
         function  readobjectdata(data:TAsmObjectData):boolean;override;
       public
         constructor createdjgpp;
         constructor createwin32;
         function newobjectdata(const n:string):TAsmObjectData;override;
       end;

       tcoffassembler = class(tinternalassembler)
         constructor create(smart:boolean);override;
       end;

       tpecoffassembler = class(tinternalassembler)
         constructor create(smart:boolean);override;
       end;

       tcofflinker = class(tinternallinker)
         constructor create;override;
       end;


implementation

    uses
       strings,
       cutils,verbose,
       globals,fmodule;

    const
       COFF_FLAG_NORELOCS = $0001;
       COFF_FLAG_EXE      = $0002;
       COFF_FLAG_NOLINES  = $0004;
       COFF_FLAG_NOLSYMS  = $0008;
       COFF_FLAG_AR16WR   = $0080; { 16bit little endian }
       COFF_FLAG_AR32WR   = $0100; { 32bit little endian }
       COFF_FLAG_AR32W    = $0200; { 32bit big endian }
       COFF_FLAG_DLL      = $2000;

       COFF_SYM_GLOBAL   = 2;
       COFF_SYM_LOCAL    = 3;
       COFF_SYM_LABEL    = 6;
       COFF_SYM_FUNCTION = 101;
       COFF_SYM_FILE     = 103;
       COFF_SYM_SECTION  = 104;

    type
       { Structures which are written directly to the output file }
       coffheader=packed record
         mach   : word;
         nsects : word;
         time   : longint;
         sympos : longint;
         syms   : longint;
         opthdr : word;
         flag   : word;
       end;
       coffoptheader=packed record
         magic  : word;
         vstamp : word;
         tsize  : longint;
         dsize  : longint;
         bsize  : longint;
         entry  : longint;
         text_start : longint;
         data_start : longint;
       end;
       coffsechdr=packed record
         name     : array[0..7] of char;
         vsize    : longint;
         rvaofs   : longint;
         datasize : longint;
         datapos  : longint;
         relocpos : longint;
         lineno1  : longint;
         nrelocs  : word;
         lineno2  : word;
         flags    : cardinal;
       end;
       coffsectionrec=packed record
         len     : longint;
         nrelocs : word;
         empty   : array[0..11] of char;
       end;
       coffreloc=packed record
         address  : longint;
         sym      : longint;
         relative : word;
       end;
       coffsymbol=packed record
         name    : array[0..3] of char; { real is [0..7], which overlaps the strpos ! }
         strpos  : longint;
         value   : longint;
         section : smallint;
         empty   : smallint;
         typ     : byte;
         aux     : byte;
       end;
       coffstab=packed record
         strpos  : longint;
         ntype   : byte;
         nother  : byte;
         ndesc   : word;
         nvalue  : longint;
       end;

     const
       symbolresize = 200*sizeof(coffsymbol);
       strsresize   = 8192;


const go32v2stub : array[0..2047] of byte=(
  $4D,$5A,$00,$00,$04,$00,$00,$00,$20,$00,$27,$00,$FF,$FF,$00,
  $00,$60,$07,$00,$00,$54,$00,$00,$00,$00,$00,$00,$00,$0D,$0A,
  $73,$74,$75,$62,$2E,$68,$20,$67,$65,$6E,$65,$72,$61,$74,$65,
  $64,$20,$66,$72,$6F,$6D,$20,$73,$74,$75,$62,$2E,$61,$73,$6D,
  $20,$62,$79,$20,$64,$6A,$61,$73,$6D,$2C,$20,$6F,$6E,$20,$54,
  $68,$75,$20,$44,$65,$63,$20,$20,$39,$20,$31,$30,$3A,$35,$39,
  $3A,$33,$31,$20,$31,$39,$39,$39,$0D,$0A,$54,$68,$65,$20,$53,
  $54,$55,$42,$2E,$45,$58,$45,$20,$73,$74,$75,$62,$20,$6C,$6F,
  $61,$64,$65,$72,$20,$69,$73,$20,$43,$6F,$70,$79,$72,$69,$67,
  $68,$74,$20,$28,$43,$29,$20,$31,$39,$39,$33,$2D,$31,$39,$39,
  $35,$20,$44,$4A,$20,$44,$65,$6C,$6F,$72,$69,$65,$2E,$20,$0D,
  $0A,$50,$65,$72,$6D,$69,$73,$73,$69,$6F,$6E,$20,$67,$72,$61,
  $6E,$74,$65,$64,$20,$74,$6F,$20,$75,$73,$65,$20,$66,$6F,$72,
  $20,$61,$6E,$79,$20,$70,$75,$72,$70,$6F,$73,$65,$20,$70,$72,
  $6F,$76,$69,$64,$65,$64,$20,$74,$68,$69,$73,$20,$63,$6F,$70,
  $79,$72,$69,$67,$68,$74,$20,$0D,$0A,$72,$65,$6D,$61,$69,$6E,
  $73,$20,$70,$72,$65,$73,$65,$6E,$74,$20,$61,$6E,$64,$20,$75,
  $6E,$6D,$6F,$64,$69,$66,$69,$65,$64,$2E,$20,$0D,$0A,$54,$68,
  $69,$73,$20,$6F,$6E,$6C,$79,$20,$61,$70,$70,$6C,$69,$65,$73,
  $20,$74,$6F,$20,$74,$68,$65,$20,$73,$74,$75,$62,$2C,$20,$61,
  $6E,$64,$20,$6E,$6F,$74,$20,$6E,$65,$63,$65,$73,$73,$61,$72,
  $69,$6C,$79,$20,$74,$68,$65,$20,$77,$68,$6F,$6C,$65,$20,$70,
  $72,$6F,$67,$72,$61,$6D,$2E,$0A,$0D,$0A,$24,$49,$64,$3A,$20,
  $73,$74,$75,$62,$2E,$61,$73,$6D,$20,$62,$75,$69,$6C,$74,$20,
  $31,$32,$2F,$30,$39,$2F,$39,$39,$20,$31,$30,$3A,$35,$39,$3A,
  $33,$31,$20,$62,$79,$20,$64,$6A,$61,$73,$6D,$20,$24,$0A,$0D,
  $0A,$40,$28,$23,$29,$20,$73,$74,$75,$62,$2E,$61,$73,$6D,$20,
  $62,$75,$69,$6C,$74,$20,$31,$32,$2F,$30,$39,$2F,$39,$39,$20,
  $31,$30,$3A,$35,$39,$3A,$33,$31,$20,$62,$79,$20,$64,$6A,$61,
  $73,$6D,$0A,$0D,$0A,$1A,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$67,$6F,$33,$32,$73,$74,$75,$62,$2C,$20,$76,$20,$32,
  $2E,$30,$32,$54,$00,$00,$00,$00,$00,$08,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$43,$57,$53,$44,$50,
  $4D,$49,$2E,$45,$58,$45,$00,$00,$00,$00,$00,$0E,$1F,$8C,$1E,
  $24,$00,$8C,$06,$60,$07,$FC,$B4,$30,$CD,$21,$3C,$03,$73,$08,
  $B0,$6D,$BA,$A7,$05,$E9,$D4,$03,$A2,$69,$08,$BE,$20,$00,$8B,
  $04,$09,$C0,$75,$02,$B4,$FE,$BB,$70,$08,$39,$C3,$73,$02,$89,
  $C3,$89,$1C,$FE,$C7,$B9,$04,$FF,$D3,$EB,$B4,$4A,$CD,$21,$73,
  $08,$D3,$E3,$FE,$CF,$89,$1C,$EB,$D8,$26,$8E,$06,$2C,$00,$31,
  $FF,$30,$C0,$A9,$F2,$AE,$26,$81,$3D,$50,$41,$75,$15,$AF,$26,
  $81,$3D,$54,$48,$75,$0D,$AF,$26,$80,$3D,$3D,$75,$06,$47,$89,
  $3E,$8C,$04,$4F,$AE,$75,$DF,$AF,$B4,$3E,$BB,$13,$00,$CD,$21,
  $B4,$3E,$BB,$12,$00,$CD,$21,$06,$57,$31,$C9,$74,$12,$B0,$6E,
  $BA,$7E,$05,$E9,$5E,$03,$09,$C9,$75,$F4,$41,$E8,$A1,$03,$72,
  $EE,$B8,$87,$16,$CD,$2F,$09,$C0,$75,$ED,$80,$E3,$01,$74,$E8,
  $89,$3E,$00,$06,$8C,$06,$02,$06,$89,$36,$04,$06,$5F,$07,$E8,
  $D3,$02,$89,$3E,$2A,$00,$89,$36,$62,$07,$80,$3E,$2C,$00,$00,
  $74,$23,$B9,$08,$00,$BF,$2C,$00,$8A,$05,$47,$08,$C0,$74,$05,
  $88,$07,$43,$E2,$F4,$66,$C7,$07,$2E,$45,$58,$45,$83,$C3,$04,
  $C6,$07,$00,$89,$1E,$62,$07,$B8,$00,$3D,$BA,$64,$07,$CD,$21,
  $0F,$82,$B3,$02,$A3,$06,$06,$89,$C3,$B9,$06,$00,$BA,$B5,$07,
  $B4,$3F,$CD,$21,$31,$D2,$31,$C9,$A1,$B5,$07,$3D,$4C,$01,$74,
  $1B,$3D,$4D,$5A,$0F,$85,$98,$02,$8B,$16,$B9,$07,$C1,$E2,$09,
  $8B,$1E,$B7,$07,$09,$DB,$74,$05,$80,$EE,$02,$01,$DA,$89,$16,
  $BB,$07,$89,$0E,$BD,$07,$B8,$00,$42,$8B,$1E,$06,$06,$CD,$21,
  $B9,$A8,$00,$BA,$BF,$07,$B4,$3F,$CD,$21,$3D,$A8,$00,$75,$06,
  $81,$3E,$BF,$07,$4C,$01,$0F,$85,$61,$02,$66,$A1,$E3,$07,$66,
  $A3,$10,$06,$66,$8B,$0E,$BB,$07,$66,$A1,$03,$08,$66,$01,$C8,
  $66,$A3,$08,$06,$66,$A1,$2B,$08,$66,$01,$C8,$66,$A3,$0C,$06,
  $66,$8B,$1E,$4B,$08,$66,$A1,$4F,$08,$66,$01,$C3,$66,$B8,$01,
  $00,$01,$00,$66,$39,$C3,$73,$03,$66,$89,$C3,$66,$81,$C3,$FF,
  $FF,$00,$00,$31,$DB,$66,$89,$1E,$1C,$00,$E8,$F5,$02,$8B,$1E,
  $04,$06,$09,$DB,$74,$0A,$B4,$48,$CD,$21,$0F,$82,$15,$02,$8E,
  $C0,$E8,$08,$03,$B8,$01,$00,$FF,$1E,$00,$06,$0F,$82,$0F,$02,
  $8C,$06,$26,$00,$8C,$0E,$28,$00,$8C,$D8,$A3,$22,$00,$8E,$C0,
  $31,$C0,$B9,$01,$00,$CD,$31,$72,$07,$A3,$14,$06,$31,$C0,$CD,
  $31,$0F,$82,$F3,$01,$A3,$16,$06,$66,$8B,$0E,$1C,$00,$B8,$01,
  $05,$8B,$1E,$1E,$00,$CD,$31,$0F,$82,$E5,$01,$89,$1E,$1A,$06,
  $89,$0E,$18,$06,$89,$36,$1A,$00,$89,$3E,$18,$00,$B8,$07,$00,
  $8B,$1E,$14,$06,$8B,$0E,$1A,$06,$8B,$16,$18,$06,$CD,$31,$B8,
  $09,$00,$8C,$C9,$83,$E1,$03,$C1,$E1,$05,$51,$81,$C9,$9B,$C0,
  $CD,$31,$B8,$08,$00,$8B,$0E,$1E,$00,$49,$BA,$FF,$FF,$CD,$31,
  $B8,$07,$00,$8B,$1E,$16,$06,$8B,$0E,$1A,$06,$8B,$16,$18,$06,
  $CD,$31,$B8,$09,$00,$59,$81,$C9,$93,$C0,$CD,$31,$B8,$08,$00,
  $8B,$0E,$1E,$00,$49,$BA,$FF,$FF,$CD,$31,$B8,$00,$01,$BB,$00,
  $0F,$CD,$31,$73,$10,$3D,$08,$00,$0F,$85,$73,$01,$B8,$00,$01,
  $CD,$31,$0F,$82,$6A,$01,$A3,$1C,$06,$89,$16,$1E,$06,$C1,$E3,
  $04,$89,$1E,$20,$06,$66,$8B,$36,$08,$06,$66,$8B,$3E,$FB,$07,
  $66,$8B,$0E,$FF,$07,$E8,$49,$00,$66,$8B,$36,$0C,$06,$66,$8B,
  $3E,$23,$08,$66,$8B,$0E,$27,$08,$E8,$37,$00,$8E,$06,$16,$06,
  $66,$8B,$3E,$4B,$08,$66,$8B,$0E,$4F,$08,$66,$31,$C0,$66,$C1,
  $E9,$02,$67,$F3,$66,$AB,$B4,$3E,$8B,$1E,$06,$06,$CD,$21,$B8,
  $01,$01,$8B,$16,$1E,$06,$CD,$31,$1E,$0F,$A1,$8E,$1E,$16,$06,
  $66,$64,$FF,$2E,$10,$06,$66,$89,$F0,$66,$25,$FF,$01,$00,$00,
  $66,$01,$C1,$29,$C6,$66,$29,$C7,$66,$89,$0E,$26,$06,$66,$89,
  $3E,$22,$06,$E8,$0F,$01,$89,$36,$3E,$06,$66,$C1,$EE,$10,$89,
  $36,$42,$06,$8B,$1E,$06,$06,$89,$1E,$3A,$06,$C7,$06,$46,$06,
  $00,$42,$E8,$03,$01,$A1,$1C,$06,$A3,$4E,$06,$C7,$06,$3E,$06,
  $00,$00,$C6,$06,$47,$06,$3F,$A1,$28,$06,$09,$C0,$75,$09,$A1,
  $26,$06,$3B,$06,$20,$06,$76,$03,$A1,$20,$06,$A3,$42,$06,$E8,
  $D9,$00,$66,$31,$C9,$8B,$0E,$46,$06,$66,$8B,$3E,$22,$06,$66,
  $01,$0E,$22,$06,$66,$29,$0E,$26,$06,$66,$31,$F6,$C1,$E9,$02,
  $1E,$06,$8E,$06,$16,$06,$8E,$1E,$1E,$06,$67,$F3,$66,$A5,$07,
  $1F,$66,$03,$0E,$26,$06,$75,$AF,$C3,$3C,$3A,$74,$06,$3C,$2F,
  $74,$02,$3C,$5C,$C3,$BE,$64,$07,$89,$F3,$26,$8A,$05,$47,$88,
  $04,$38,$E0,$74,$0E,$08,$C0,$74,$0A,$46,$E8,$DE,$FF,$75,$EC,
  $89,$F3,$74,$E8,$C3,$B0,$66,$BA,$48,$05,$EB,$0C,$B0,$67,$BA,
  $55,$05,$EB,$05,$B0,$68,$BA,$5F,$05,$52,$8B,$1E,$62,$07,$C6,
  $07,$24,$BB,$64,$07,$EB,$28,$E8,$F5,$00,$B0,$69,$BA,$99,$05,
  $EB,$1A,$B0,$6A,$BA,$B2,$05,$EB,$13,$B0,$6B,$BA,$C4,$05,$EB,
  $0C,$B0,$6C,$BA,$D6,$05,$EB,$05,$B0,$69,$BA,$99,$05,$52,$BB,
  $3B,$05,$E8,$15,$00,$5B,$E8,$11,$00,$BB,$67,$04,$E8,$0B,$00,
  $B4,$4C,$CD,$21,$43,$50,$B4,$02,$CD,$21,$58,$8A,$17,$80,$FA,
  $24,$75,$F2,$C3,$0D,$0A,$24,$50,$51,$57,$31,$C0,$BF,$2A,$06,
  $B9,$19,$00,$F3,$AB,$5F,$59,$58,$C3,$B8,$00,$03,$BB,$21,$00,
  $31,$C9,$66,$BF,$2A,$06,$00,$00,$CD,$31,$C3,$00,$00,$30,$E4,
  $E8,$4E,$FF,$89,$DE,$8B,$3E,$8C,$04,$EB,$17,$B4,$3B,$E8,$41,
  $FF,$81,$FE,$64,$07,$74,$12,$8A,$44,$FF,$E8,$2A,$FF,$74,$04,
  $C6,$04,$5C,$46,$E8,$03,$00,$72,$E4,$C3,$E8,$34,$00,$BB,$44,
  $00,$8A,$07,$88,$04,$43,$46,$08,$C0,$75,$F6,$06,$57,$1E,$07,
  $E8,$9B,$FF,$BB,$2A,$06,$8C,$5F,$04,$89,$5F,$02,$BA,$64,$07,
  $B8,$00,$4B,$CD,$21,$5F,$07,$72,$09,$B4,$4D,$CD,$21,$2D,$00,
  $03,$F7,$D8,$EB,$28,$80,$3E,$69,$08,$05,$72,$20,$B8,$00,$58,
  $CD,$21,$A2,$67,$08,$B8,$02,$58,$CD,$21,$A2,$68,$08,$B8,$01,
  $58,$BB,$80,$00,$CD,$21,$B8,$03,$58,$BB,$01,$00,$CD,$21,$C3,
  $9C,$80,$3E,$69,$08,$05,$72,$1A,$50,$53,$B8,$03,$58,$8A,$1E,
  $68,$08,$30,$FF,$CD,$21,$B8,$01,$58,$8A,$1E,$67,$08,$30,$FF,
  $CD,$21,$5B,$58,$9D,$C3,$4C,$6F,$61,$64,$20,$65,$72,$72,$6F,
  $72,$3A,$20,$24,$3A,$20,$63,$61,$6E,$27,$74,$20,$6F,$70,$65,
  $6E,$24,$3A,$20,$6E,$6F,$74,$20,$45,$58,$45,$24,$3A,$20,$6E,
  $6F,$74,$20,$43,$4F,$46,$46,$20,$28,$43,$68,$65,$63,$6B,$20,
  $66,$6F,$72,$20,$76,$69,$72,$75,$73,$65,$73,$29,$24,$6E,$6F,
  $20,$44,$50,$4D,$49,$20,$2D,$20,$47,$65,$74,$20,$63,$73,$64,
  $70,$6D,$69,$2A,$62,$2E,$7A,$69,$70,$24,$6E,$6F,$20,$44,$4F,
  $53,$20,$6D,$65,$6D,$6F,$72,$79,$24,$6E,$65,$65,$64,$20,$44,
  $4F,$53,$20,$33,$24,$63,$61,$6E,$27,$74,$20,$73,$77,$69,$74,
  $63,$68,$20,$6D,$6F,$64,$65,$24,$6E,$6F,$20,$44,$50,$4D,$49,
  $20,$73,$65,$6C,$65,$63,$74,$6F,$72,$73,$24,$6E,$6F,$20,$44,
  $50,$4D,$49,$20,$6D,$65,$6D,$6F,$72,$79,$24,$90,$90,$90,$90,
  $90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,
  $90,$90,$90,$90,$90,$90,$90,$90);


{****************************************************************************
                               TCoffObjectSection
****************************************************************************}

    constructor tcoffobjectsection.create(const aname:string;atype:tasmsectiontype;aalign:longint;aoptions:TAsmSectionOptions);
      begin
        inherited create(aname,atype,aalign,aoptions);
        Flags:=0;
      end;


    procedure TCoffObjectSection.addsymsizereloc(ofs:longint;p:tasmsymbol;size:longint;relative:TAsmRelocationType);
      begin
        relocations.concat(tasmrelocation.createsymbolsize(ofs,p,size,relative));
      end;


    procedure TCoffObjectSection.fixuprelocs;
      var
        r : TAsmRelocation;
        address,
        relocval : longint;
      begin
        r:=TAsmRelocation(relocations.first);
        if assigned(r) and
           (not assigned(data)) then
          internalerror(200205183);
        while assigned(r) do
          begin
            if assigned(r.symbol) then
              relocval:=r.symbol.address
            else
              internalerror(200205183);
            data.Seek(r.address);
            data.Read(address,4);
            case r.typ of
              RELOC_RELATIVE  :
                begin
                  dec(address,mempos);
                  inc(address,relocval);
                end;
              RELOC_RVA,
              RELOC_ABSOLUTE :
                begin
                  if r.symbol.section.sectype=sec_common then
                   dec(address,r.orgsize)
                  else
                   begin
                     { fixup address when the symbol was known in defined object }
                     if (r.symbol.section<>nil) and
                        (r.symbol.owner=owner) then
                       dec(address,TCoffObjectSection(r.symbol.section).orgmempos);
                   end;
                  inc(address,relocval);
                end;
            end;
            data.Seek(r.address);
            data.Write(address,4);
            { goto next reloc }
            r:=TAsmRelocation(r.next);
          end;
      end;



{****************************************************************************
                               TDjCoffObjectSection
****************************************************************************}

    constructor tdjcoffobjectsection.create(const aname:string;atype:tasmsectiontype;aalign:longint;aoptions:TAsmSectionOptions);
      begin
        inherited create(aname,atype,aalign,aoptions);
        case atype of
          sec_code :
            begin
              Flags:=$20;
              addralign:=16;
            end;
          sec_data :
            begin
              Flags:=$40;
              addralign:=16;
            end;
          sec_bss :
            begin
              Flags:=$80;
              addralign:=16;
            end;
        end;
      end;


{****************************************************************************
                               TPECoffObjectSection
****************************************************************************}

    constructor tpecoffobjectsection.create(const aname:string;atype:tasmsectiontype;aalign:longint;aoptions:TAsmSectionOptions);
      begin
        inherited create(aname,atype,aalign,aoptions);
        case atype of
          sec_code :
            begin
              Flags:=$60000020;
              addralign:=16;
            end;
          sec_data :
            begin
              Flags:=$c0300040;
              addralign:=16;
            end;
          sec_bss :
            begin
              Flags:=$c0300080;
              addralign:=16;
            end;
          sec_idata2,
          sec_idata4,
          sec_idata5,
          sec_idata6,
          sec_idata7 :
            begin
              Flags:=$40000000;
            end;
          sec_edata :
            begin
              Flags:=$c0300040;
            end;
        end;
      end;


{****************************************************************************
                                tcoffobjectdata
****************************************************************************}

    constructor tcoffobjectdata.createcoff(const n:string;awin32:boolean;acasmsection:TAsmSectionClass);
      begin
        inherited create(n);
        CAsmSection:=ACAsmSection;
        win32:=awin32;
        { we need at least the following 3 sections }
        createsection(sec_code,'',0,[]);
        createsection(sec_data,'',0,[]);
        createsection(sec_bss,'',0,[]);
        if (cs_gdb_lineinfo in aktglobalswitches) or
           (cs_debuginfo in aktmoduleswitches) then
         begin
           stabssec:=createsection(sec_stab,'',0,[]);
           stabstrsec:=createsection(sec_stabstr,'',0,[]);
         end;
      end;


    destructor tcoffobjectdata.destroy;
      begin
        inherited destroy;
      end;


    function TCoffObjectData.sectionname(atype:tasmsectiontype;const aname:string):string;
      const
        secnames : array[tasmsectiontype] of string[16] = ('',
          '.text','.data','.data','.bss',
          'common',
          '.note',
          '.stab','.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame'
        );
      begin
        { No support for named sections, because section names are limited to 8 chars }
        result:=secnames[atype];
      end;


    procedure tcoffobjectdata.writesymbol(p:tasmsymbol);
      begin
        if currsec=nil then
          internalerror(200403071);
        { already written ? }
        if p.indexnr<>-1 then
         exit;
        { calculate symbol index }
        if (p.currbind<>AB_LOCAL) then
         begin
           { insert the symbol in the local index, the indexarray
             will take care of the numbering }
           symbols.insert(p);
         end
        else
         p.indexnr:=-2; { local }
      end;


    procedure tcoffobjectdata.writereloc(data,len:aint;p:tasmsymbol;relative:TAsmRelocationType);
      var
        curraddr,
        symaddr : longint;
      begin
        if currsec=nil then
          internalerror(200403072);
        if assigned(p) then
         begin
           { current address }
           curraddr:=currsec.mempos+currsec.datasize;
           { external/common symbols don't have a fixed memory position yet }
           if (p.currbind=AB_COMMON) then
             begin
               { For go32v2 we need to use the size as address }
               if not win32 then
                 symaddr:=p.size
               else
                 symaddr:=0;
             end
           else
             begin
               symaddr:=p.address;
               if assigned(p.section) then
                 inc(symaddr,p.section.mempos);
             end;
           { no symbol relocation need inside a section }
           if (p.section=currsec) and
              (p.currbind<>AB_COMMON) then
             begin
               case relative of
                 RELOC_ABSOLUTE :
                   begin
                     currsec.addsectionreloc(curraddr,currsec,RELOC_ABSOLUTE);
                     inc(data,symaddr);
                   end;
                 RELOC_RELATIVE :
                   begin
                     inc(data,symaddr-len-currsec.datasize);
                   end;
                 RELOC_RVA :
                   begin
                     currsec.addsectionreloc(curraddr,currsec,RELOC_RVA);
                     inc(data,symaddr);
                   end;
               end;
             end
           else
             begin
               writesymbol(p);
               if (p.section<>nil) and
                  (p.currbind<>AB_COMMON) and
                  (relative<>RELOC_RELATIVE) then
                 currsec.addsectionreloc(curraddr,p.section,relative)
               else
                 currsec.addsymreloc(curraddr,p,relative);
               if (not win32) or
                  ((relative<>RELOC_RELATIVE) and (p.section<>nil)) then
                 inc(data,symaddr);
               if relative=RELOC_RELATIVE then
                begin
                  if win32 then
                    dec(data,len-4)
                  else
                    dec(data,len+currsec.datasize);
                end;
            end;
         end;
        currsec.write(data,len);
      end;


    procedure tcoffobjectdata.writestabs(offset:aint;p:pchar;nidx,nother,line:longint;reloc : boolean);
      var
        stab : coffstab;
        curraddr : longint;
      begin
        { local var can be at offset -1 !! PM }
        if reloc then
         begin
           if (offset=-1) then
            begin
              if currsec=nil then
               offset:=0
              else
               offset:=currsec.datasize;
            end;
           if (currsec<>nil) then
            inc(offset,currsec.datapos);
         end;
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=stabstrsec.datasize;
           stabstrsec.write(p^,strlen(p)+1);
         end
        else
         stab.strpos:=0;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        StabsSec.write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         begin
           { current address }
           curraddr:=StabsSec.mempos+StabsSec.datasize;
           if DLLSource and RelocSection then
           { avoid relocation in the .stab section
             because it ends up in the .reloc section instead }
             StabsSec.addsectionreloc(curraddr-4,currsec,RELOC_RVA)
           else
             StabsSec.addsectionreloc(curraddr-4,currsec,RELOC_ABSOLUTE);
         end;
      end;


    procedure tcoffobjectdata.writesymstabs(offset:aint;p:pchar;ps:tasmsymbol;nidx,nother,line:longint;reloc:boolean);
      var
        stab : coffstab;
        curraddr : longint;
      begin
        { do not use the size stored in offset field
         this is DJGPP specific ! PM }
        if win32 then
          offset:=0;
        { local var can be at offset -1 !! PM }
        if reloc then
         begin
           if (offset=-1) then
            begin
              if currsec=nil then
               offset:=0
              else
               offset:=currsec.datasize;
            end;
           if (currsec<>nil) then
            inc(offset,currsec.mempos);
         end;
        if assigned(p) and (p[0]<>#0) then
         begin
           stab.strpos:=StabStrSec.datasize;
           StabStrSec.write(p^,strlen(p)+1);
         end
        else
         stab.strpos:=0;
        stab.ntype:=nidx;
        stab.ndesc:=line;
        stab.nother:=nother;
        stab.nvalue:=offset;
        StabsSec.write(stab,sizeof(stab));
        { when the offset is not 0 then write a relocation, take also the
          hdrstab into account with the offset }
        if reloc then
         begin
           { current address }
           curraddr:=StabsSec.mempos+StabsSec.datasize;
           if DLLSource and RelocSection then
            { avoid relocation in the .stab section
              because it ends up in the .reloc section instead }
            StabsSec.addsymreloc(curraddr-4,ps,RELOC_RVA)
           else
            StabsSec.addsymreloc(curraddr-4,ps,RELOC_ABSOLUTE);
         end;
      end;


    procedure tcoffobjectdata.section_mempos(p:tnamedindexitem;arg:pointer);
      begin
        tcoffobjectsection(p).memsize:=tcoffobjectsection(p).datasize;
        { memory position is in arg }
        if not win32 then
         begin
           tcoffobjectsection(p).mempos:=plongint(arg)^;
           inc(plongint(arg)^,align(tcoffobjectsection(p).memsize,tcoffobjectsection(p).addralign));
         end;
      end;


    procedure tcoffobjectdata.beforealloc;
      begin
        { create stabs sections if debugging }
        if (cs_debuginfo in aktmoduleswitches) then
          begin
            StabsSec.Alloc(sizeof(coffstab));
            StabStrSec.Alloc(length(SplitFileName(current_module.mainsource^))+2);
          end;
      end;


    procedure tcoffobjectdata.beforewrite;
      var
        s : string;
      begin
        { create stabs sections if debugging }
        if (cs_debuginfo in aktmoduleswitches) then
         begin
           writestabs(0,nil,0,0,0,false);
           { write zero pchar and name together (PM) }
           s:=#0+SplitFileName(current_module.mainsource^)+#0;
           stabstrsec.write(s[1],length(s));
         end;
      end;


    procedure tcoffobjectdata.afteralloc;
      var
        mempos : longint;
      begin
        { if debug then also count header stab }
        if (cs_debuginfo in aktmoduleswitches) then
          begin
            StabsSec.Alloc(sizeof(coffstab));
            StabStrSec.Alloc(length(SplitFileName(current_module.mainsource^))+2);
          end;
        { calc mempos }
        mempos:=0;
        sects.foreach(@section_mempos,@mempos);
      end;


{****************************************************************************
                                tdjcoffobjectdata
****************************************************************************}

    constructor tdjcoffobjectdata.create(const n:string);
      begin
        inherited createcoff(n,false,tdjcoffobjectsection);
      end;


{****************************************************************************
                                tpecoffobjectdata
****************************************************************************}

    constructor tpecoffobjectdata.create(const n:string);
      begin
        inherited createcoff(n,true,tpecoffobjectsection);
      end;


{****************************************************************************
                                tcoffobjectoutput
****************************************************************************}

    constructor tcoffobjectoutput.createdjgpp(smart:boolean);
      begin
        inherited create(smart);
        win32:=false;
      end;


    constructor tcoffobjectoutput.createwin32(smart:boolean);
      begin
        inherited create(smart);
        win32:=true;
      end;


    function tcoffobjectoutput.newobjectdata(const n:string):TAsmObjectData;
      begin
        if win32 then
         result:=tpecoffobjectdata.create(n)
        else
         result:=tdjcoffobjectdata.create(n);
      end;


    procedure tcoffobjectoutput.write_symbol(const name:string;value,section,typ,aux:longint);
      var
        sym : coffsymbol;
      begin
        FillChar(sym,sizeof(sym),0);
        { symbolname }
        if length(name)>8 then
         begin
           sym.strpos:=FCoffStrs.size+4;
           FCoffStrs.writestr(name);
           FCoffStrs.writestr(#0);
         end
        else
         move(name[1],sym.name,length(name));
        sym.value:=value;
        sym.section:=section;
        sym.typ:=typ;
        sym.aux:=aux;
        FWriter.write(sym,sizeof(sym));
      end;


    procedure tcoffobjectoutput.section_write_symbol(p:tnamedindexitem;arg:pointer);
      var
        secrec : coffsectionrec;
      begin
        write_symbol(tasmsection(p).name,tasmsection(p).mempos,tasmsection(p).secsymidx,3,1);
        fillchar(secrec,sizeof(secrec),0);
        secrec.len:=tasmsection(p).aligneddatasize;
        secrec.nrelocs:=tasmsection(p).relocations.count;
        FWriter.write(secrec,sizeof(secrec));
      end;


    procedure tcoffobjectoutput.section_write_relocs(p:tnamedindexitem;arg:pointer);
      var
        rel  : coffreloc;
        r    : TAsmRelocation;
      begin
        r:=TasmRelocation(tasmsection(p).relocations.first);
        while assigned(r) do
         begin
           rel.address:=r.address;
           if assigned(r.symbol) then
            begin
              if (r.symbol.currbind=AB_LOCAL) then
               rel.sym:=2*r.symbol.section.secsymidx
              else
               begin
                 if r.symbol.indexnr=-1 then
                   internalerror(4321);
                 { indexnr starts with 1, coff starts with 0 }
                 rel.sym:=r.symbol.indexnr+initsym-1;
               end;
            end
           else
            begin
              if r.section<>nil then
               rel.sym:=2*r.section.secsymidx
              else
               rel.sym:=0;
            end;
           case r.typ of
             RELOC_RELATIVE :
               rel.relative:=$14;
             RELOC_ABSOLUTE :
               rel.relative:=$6;
             RELOC_RVA :
               rel.relative:=$7;
           end;
           FWriter.write(rel,sizeof(rel));
           r:=TAsmRelocation(r.next);
         end;
      end;


    procedure tcoffobjectoutput.write_symbols(data:TAsmObjectData);
      var
        filename  : string[18];
        value     : longint;
        sectionval,
        globalval : byte;
        p         : tasmsymbol;
      begin
        with tcoffobjectdata(data) do
         begin
           { The `.file' record, and the file name auxiliary record }
           write_symbol('.file', 0, -2, $67, 1);
           fillchar(filename,sizeof(filename),0);
           filename:=SplitFileName(current_module.mainsource^);
           FWriter.write(filename[1],sizeof(filename)-1);
           { The section records, with their auxiliaries, also store the
             symbol index }
           Sects.foreach(@section_write_symbol,nil);
           { The symbols used }
           p:=Tasmsymbol(symbols.First);
           while assigned(p) do
            begin
              if assigned(p.section) and
                 (p.currbind<>AB_COMMON) then
               sectionval:=p.section.secsymidx
              else
               sectionval:=0;
              if p.currbind=AB_LOCAL then
               globalval:=3
              else
               globalval:=2;
              { if local of global then set the section value to the address
                of the symbol }
              if p.currbind in [AB_LOCAL,AB_GLOBAL] then
               value:=p.address+p.section.mempos
              else
               value:=p.size;
              { symbolname }
              write_symbol(p.name,value,sectionval,globalval,0);
              p:=tasmsymbol(p.indexnext);
            end;
         end;
      end;


    procedure tcoffobjectoutput.section_set_secsymidx(p:tnamedindexitem;arg:pointer);
      begin
        inc(plongint(arg)^);
        tasmsection(p).secsymidx:=plongint(arg)^;
      end;


    procedure tcoffobjectoutput.section_set_datapos(p:tnamedindexitem;arg:pointer);
      begin
        tasmsection(p).datapos:=plongint(arg)^;
        if not(aso_alloconly in tasmsection(p).secoptions) then
          inc(plongint(arg)^,tasmsection(p).aligneddatasize);
      end;


    procedure tcoffobjectoutput.section_set_reloc_datapos(p:tnamedindexitem;arg:pointer);
      begin
        TCoffObjectSection(p).coffrelocpos:=plongint(arg)^;
        inc(plongint(arg)^,sizeof(coffreloc)*tasmsection(p).relocations.count);
      end;


    procedure tcoffobjectoutput.section_write_header(p:tnamedindexitem;arg:pointer);
      var
        sechdr   : coffsechdr;
        s        : string;
      begin
        fillchar(sechdr,sizeof(sechdr),0);
        s:=tasmsection(p).name;
        { section names are limited to 8 chars }
        if length(s)>8 then
          internalerror(200403312);
        move(s[1],sechdr.name,length(s));
        if not win32 then
          begin
            sechdr.rvaofs:=tasmsection(p).mempos;
            sechdr.vsize:=tasmsection(p).mempos;
          end
        else
          begin
            if tasmsection(p).sectype=sec_bss then
              sechdr.vsize:=tasmsection(p).aligneddatasize;
          end;
        sechdr.datasize:=tasmsection(p).aligneddatasize;
        if (tasmsection(p).datasize>0) and
           not(aso_alloconly in tasmsection(p).secoptions) then
          sechdr.datapos:=tasmsection(p).datapos;
        sechdr.nrelocs:=tasmsection(p).relocations.count;
        sechdr.relocpos:=TCoffObjectSection(p).coffrelocpos;
        sechdr.flags:=TCoffObjectSection(p).flags;
        FWriter.write(sechdr,sizeof(sechdr));
      end;


    procedure tcoffobjectoutput.section_write_data(p:tnamedindexitem;arg:pointer);
      var
        hp       : pdynamicblock;
      begin
        if (aso_alloconly in tasmsection(p).secoptions) then
          exit;
        if tasmsection(p).data=nil then
          internalerror(200403073);
        tasmsection(p).alignsection;
        hp:=tasmsection(p).data.firstblock;
        while assigned(hp) do
          begin
            FWriter.write(hp^.data,hp^.used);
            hp:=hp^.next;
          end;
      end;


    function tcoffobjectoutput.writedata(data:TAsmObjectData):boolean;
      var
        orgdatapos,
        datapos,
        nsects,
        sympos,i : longint;
        hstab    : coffstab;
        gotreloc : boolean;
        header   : coffheader;
        empty    : array[0..15] of byte;
        hp       : pdynamicblock;
        s        : string;
      begin
        result:=false;
        FCoffStrs:=TDynamicArray.Create(strsresize);
        with tcoffobjectdata(data) do
         begin
         { calc amount of sections we have }
           fillchar(empty,sizeof(empty),0);
           nsects:=0;
           Sects.foreach(@section_set_secsymidx,@nsects);
           initsym:=2+nsects*2;   { 2 for the file }
         { For the stab section we need an HdrSym which can now be
           calculated more easily }
           if StabsSec<>nil then
            begin
              { first stabs for main source }
              writestabs(0,nil,0,0,0,false);
              s:=#0+SplitFileName(current_module.mainsource^)+#0;
              stabstrsec.write(s[1],length(s));
              { header stab }
              hstab.strpos:=1;
              hstab.ntype:=0;
              hstab.nother:=0;
              hstab.ndesc:=(StabsSec.datasize div sizeof(coffstab))-1{+1 according to gas output PM};
              hstab.nvalue:=StabStrSec.datasize;
              StabsSec.data.seek(0);
              StabsSec.data.write(hstab,sizeof(hstab));
            end;
         { Calculate the filepositions }
           datapos:=sizeof(coffheader)+sizeof(coffsechdr)*nsects;
           { sections first }
           Sects.foreach(@section_set_datapos,@datapos);
           { relocs }
           orgdatapos:=datapos;
           Sects.foreach(@section_set_reloc_datapos,@datapos);
           gotreloc:=(orgdatapos<>datapos);
           { symbols }
           sympos:=datapos;
         { COFF header }
           fillchar(header,sizeof(coffheader),0);
           header.mach:=$14c;
           header.nsects:=nsects;
           header.sympos:=sympos;
           header.syms:=symbols.count+initsym;
           header.flag:=COFF_FLAG_AR32WR or COFF_FLAG_NOLINES;
           if not gotreloc then
            header.flag:=header.flag or COFF_FLAG_NORELOCS;
           FWriter.write(header,sizeof(header));
         { Section headers }
           Sects.foreach(@section_write_header,nil);
         { Sections }
           Sects.foreach(@section_write_data,nil);
           { Relocs }
           Sects.foreach(@section_write_relocs,nil);
           { Symbols }
           write_symbols(data);
           { Strings }
           i:=FCoffStrs.size+4;
           FWriter.write(i,4);
           hp:=FCoffStrs.firstblock;
           while assigned(hp) do
            begin
              FWriter.write(hp^.data,hp^.used);
              hp:=hp^.next;
            end;
         end;
        FCoffStrs.Free;
      end;


{****************************************************************************
                              tcoffexeoutput
****************************************************************************}

    constructor tcoffexeoutput.createdjgpp;
      begin
        inherited create;
        win32:=false;
      end;


    constructor tcoffexeoutput.createwin32;
      begin
        inherited create;
        win32:=true;
      end;


    function tcoffexeoutput.newobjectinput:tobjectinput;
      begin
        if win32 then
         result:=tcoffobjectinput.createwin32
        else
         result:=tcoffobjectinput.createdjgpp;
      end;


    procedure tcoffexeoutput.write_symbol(const name:string;value,section,typ,aux:longint);
      var
        sym : coffsymbol;
      begin
        FillChar(sym,sizeof(sym),0);
        if length(name)>8 then
         begin
           sym.strpos:=FCoffStrs.size+4;
           FCoffStrs.writestr(name);
           FCoffStrs.writestr(#0);
         end
        else
         move(name[1],sym.name,length(name));
        sym.value:=value;
        sym.section:=section;
        sym.typ:=typ;
        sym.aux:=aux;
        FWriter.write(sym,sizeof(sym));
      end;


    procedure tcoffexeoutput.write_symbols;
      var
        value,
        sectionval,
        globalval : byte;
        objdata   : TAsmObjectData;
        p         : tasmsymbol;
      begin
{$ifdef internallinker}
        objdata:=TAsmObjectData(objdatalist.first);
        while assigned(objdata) do
         begin
           with tcoffobjectdata(objdata) do
            begin
              { The symbols used }
              p:=Tasmsymbol(symbols.First);
              while assigned(p) do
               begin
                 if p.section=sec_common then
                  sectionval:=sections[sec_bss].secsymidx
                 else
                  sectionval:=sections[p.section.name].secsymidx;
                 if p.currbind=AB_LOCAL then
                  globalval:=3
                 else
                  globalval:=2;
                 { if local of global then set the section value to the address
                   of the symbol }
                 if p.currbind in [AB_LOCAL,AB_GLOBAL] then
                  value:=p.address
                 else
                  value:=p.size;
                 { symbolname }
                 s:=p.name;
                 if length(s)>8 then
                  begin
                    nameidx:=FCoffStrs.size+4;
                    FCoffStrs.writestr(s);
                    FCoffStrs.writestr(#0);
                  end
                 else
                  begin
                    nameidx:=-1;
                    namestr:=s;
                  end;
                 write_symbol(namestr,nameidx,value,sectionval,globalval,0);
                 p:=tasmsymbol(p.indexnext);
               end;
            end;
           objdata:=TAsmObjectData(objdata.next);
         end;
{$endif internallinker}
      end;


    procedure tcoffexeoutput.CalculateMemoryMap;
      var
        objdata : TAsmObjectData;
        secsymidx,
        mempos,
        datapos : longint;
      begin
{$ifdef internallinker}
        { retrieve amount of sections }
        nsects:=0;
        secsymidx:=0;
        for sec:=low(TSection) to high(TSection) do
         begin
           if sections[sec].available then
            begin
              inc(nsects);
              inc(secsymidx);
              sections[sec].secsymidx:=secsymidx;
            end;
         end;
        { calculate start positions after the headers }
        datapos:=sizeof(coffheader)+sizeof(coffoptheader)+sizeof(coffsechdr)*nsects;
        mempos:=sizeof(coffheader)+sizeof(coffoptheader)+sizeof(coffsechdr)*nsects;
        if not win32 then
         inc(mempos,sizeof(go32v2stub)+$1000);
        { add sections }
        MapObjectdata(datapos,mempos);
        { end symbol }
        AddGlobalSym('_etext',sections[sec_code].mempos+sections[sec_code].memsize);
        AddGlobalSym('_edata',sections[sec_data].mempos+sections[sec_data].memsize);
        AddGlobalSym('end',mempos);
        { symbols }
        nsyms:=0;
        sympos:=0;
        if not(cs_link_strip in aktglobalswitches) then
         begin
           sympos:=datapos;
           objdata:=TAsmObjectData(objdatalist.first);
           while assigned(objdata) do
            begin
              inc(nsyms,objdata.symbols.count);
              objdata:=TAsmObjectData(objdata.next);
            end;
         end;
{$endif internallinker}
      end;


    function tcoffexeoutput.writedata:boolean;
      var
        i         : longint;
        header    : coffheader;
        optheader : coffoptheader;
        sechdr    : coffsechdr;
        hp        : pdynamicblock;
        objdata   : TAsmObjectData;
        hsym      : tasmsymbol;
      begin
        result:=false;
{$ifdef internallinker}
        FCoffSyms:=TDynamicArray.Create(symbolresize);
        FCoffStrs:=TDynamicArray.Create(strsresize);
        { Stub }
        if not win32 then
         FWriter.write(go32v2stub,sizeof(go32v2stub));
        { COFF header }
        fillchar(header,sizeof(header),0);
        header.mach:=$14c;
        header.nsects:=nsects;
        header.sympos:=sympos;
        header.syms:=nsyms;
        header.opthdr:=sizeof(coffoptheader);
        header.flag:=COFF_FLAG_AR32WR or COFF_FLAG_EXE or COFF_FLAG_NORELOCS or COFF_FLAG_NOLINES;
        FWriter.write(header,sizeof(header));
        { Optional COFF Header }
        fillchar(optheader,sizeof(optheader),0);
        optheader.magic:=$10b;
        optheader.tsize:=sections[sec_code].memsize;
        optheader.dsize:=sections[sec_data].memsize;
        optheader.bsize:=sections[sec_bss].memsize;
        hsym:=tasmsymbol(globalsyms.search('start'));
        if not assigned(hsym) then
         begin
           Comment(V_Error,'Entrypoint "start" not defined');
           exit;
         end;
        optheader.entry:=hsym.address;
        optheader.text_start:=sections[sec_code].mempos;
        optheader.data_start:=sections[sec_data].mempos;
        FWriter.write(optheader,sizeof(optheader));
        { Section headers }
        for sec:=low(TSection) to high(TSection) do
         if sections[sec].available then
          begin
            fillchar(sechdr,sizeof(sechdr),0);
            move(target_asm.secnames[sec][1],sechdr.name,length(target_asm.secnames[sec]));
            if not win32 then
             begin
               sechdr.rvaofs:=sections[sec].mempos;
               sechdr.vsize:=sections[sec].mempos;
             end
            else
             begin
               if sec=sec_bss then
                sechdr.vsize:=sections[sec].memsize;
             end;
            if sec=sec_bss then
             sechdr.datasize:=sections[sec].memsize
            else
             begin
               sechdr.datasize:=sections[sec].datasize;
               sechdr.datapos:=sections[sec].datapos;
             end;
            sechdr.nrelocs:=0;
            sechdr.relocpos:=0;
            sechdr.flags:=sections[sec].flags;
            FWriter.write(sechdr,sizeof(sechdr));
          end;
        { Sections }
        for sec:=low(TSection) to high(TSection) do
         if sections[sec].available then
          begin
            { update objectfiles }
            objdata:=TAsmObjectData(objdatalist.first);
            while assigned(objdata) do
             begin
               if assigned(objdata.sects[sec]) and
                  assigned(objdata.sects[sec].data) then
                begin
                  FWriter.WriteZeros(objdata.sects[sec].dataalignbytes);
                  hp:=objdata.sects[sec].data.firstblock;
                  while assigned(hp) do
                   begin
                     FWriter.write(hp^.data,hp^.used);
                     hp:=hp^.next;
                   end;
                end;
               objdata:=TAsmObjectData(objdata.next);
             end;
          end;
        { Optional symbols }
        if not(cs_link_strip in aktglobalswitches) then
         begin
           { Symbols }
           write_symbols;
           { Strings }
           i:=FCoffStrs.size+4;
           FWriter.write(i,4);
           hp:=FCoffStrs.firstblock;
           while assigned(hp) do
            begin
              FWriter.write(hp^.data,hp^.used);
              hp:=hp^.next;
            end;
         end;
        { Release }
        FCoffStrs.Free;
        FCoffSyms.Free;
        result:=true;
{$endif internallinker}
      end;


    procedure tcoffexeoutput.GenerateExecutable(const fn:string);
      begin
        AddGlobalSym('_etext',0);
        AddGlobalSym('_edata',0);
        AddGlobalSym('end',0);
        if not CalculateSymbols then
         exit;
        CalculateMemoryMap;
        FixupSymbols;
        FixupRelocations;
        writeexefile(fn);
      end;


{****************************************************************************
                                tcoffobjectinput
****************************************************************************}

    constructor tcoffobjectinput.createdjgpp;
      begin
        inherited create;
        win32:=false;
      end;


    constructor tcoffobjectinput.createwin32;
      begin
        inherited create;
        win32:=true;
      end;


    function tcoffobjectinput.newobjectdata(const n:string):TAsmObjectData;
      begin
        if win32 then
         result:=tpecoffobjectdata.create(n)
        else
         result:=tdjcoffobjectdata.create(n);
      end;


    procedure tcoffobjectinput.read_relocs(s:TCoffObjectSection);
      var
        rel      : coffreloc;
        rel_type : TAsmRelocationType;
        i        : longint;
        p        : tasmsymbol;
      begin
        for i:=1 to s.coffrelocs do
         begin
           FReader.read(rel,sizeof(rel));
           case rel.relative of
             $14 : rel_type:=RELOC_RELATIVE;
             $06 : rel_type:=RELOC_ABSOLUTE;
             $07 : rel_type:=RELOC_RVA;
           else
             begin
               Comment(V_Error,'Error reading coff file');
               exit;
             end;
           end;

           p:=FSymTbl^[rel.sym].sym;
           if assigned(p) then
            begin
              s.addsymsizereloc(rel.address-s.mempos,p,FSymTbl^[rel.sym].orgsize,rel_type);
            end
           else
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
         end;
      end;


    procedure tcoffobjectinput.handle_symbols(data:TAsmObjectData);
      var
        size,
        address,
        i,nsyms,
        symidx    : longint;
        sym       : coffsymbol;
        strname   : string;
        p         : tasmsymbol;
        bind      : Tasmsymbind;
        auxrec    : array[0..17] of byte;
      begin
{$ifdef internallinker}
        with tcoffobjectdata(data) do
         begin
           nsyms:=FCoffSyms.Size div sizeof(CoffSymbol);
           { Allocate memory for symidx -> tasmsymbol table }
           GetMem(FSymTbl,nsyms*sizeof(ttasmsymbolrec));
           FillChar(FSymTbl^,nsyms*sizeof(ttasmsymbolrec),0);
           { Loop all symbols }
           FCoffSyms.Seek(0);
           symidx:=0;
           while (symidx<nsyms) do
            begin
              FCoffSyms.Read(sym,sizeof(sym));
              if plongint(@sym.name)^<>0 then
               begin
                 move(sym.name,strname[1],8);
                 strname[9]:=#0;
               end
              else
               begin
                 FCoffStrs.Seek(sym.strpos-4);
                 FCoffStrs.Read(strname[1],255);
                 strname[255]:=#0;
               end;
              strname[0]:=chr(strlen(@strname[1]));
              if strname='' then
               Internalerror(200205172);
              bind:=AB_EXTERNAL;
              sec:=sec_none;
              size:=0;
              address:=0;
              case sym.typ of
                COFF_SYM_GLOBAL :
                  begin
                    if sym.section=0 then
                     begin
                       if sym.value=0 then
                        bind:=AB_EXTERNAL
                       else
                        begin
                          bind:=AB_COMMON;
                          size:=sym.value;
                        end;
                     end
                    else
                     begin
                       bind:=AB_GLOBAL;
                       sec:=Fidx2sec[sym.section];
                       if assigned(sects[sec]) then
                        begin
                          if sym.value>=sects[sec].mempos then
                           address:=sym.value-sects[sec].mempos
                          else
                           internalerror(432432432);
                        end
                       else
                        internalerror(34243214);
                     end;
                    p:=TAsmSymbol.Create(strname,bind,AT_FUNCTION);
                    p.SetAddress(0,sec,address,size);
                    p.objectdata:=data;
                    symbols.insert(p);
                  end;
                COFF_SYM_LABEL,
                COFF_SYM_LOCAL :
                  begin
                    { do not add constants (section=-1) }
                    if sym.section<>-1 then
                     begin
                       bind:=AB_LOCAL;
                       sec:=Fidx2sec[sym.section];
                       if assigned(sects[sec]) then
                        begin
                          if sym.value>=sects[sec].mempos then
                           address:=sym.value-sects[sec].mempos
                          else
                           internalerror(432432432);
                        end
                       else
                        internalerror(34243214);
                       p:=TAsmSymbol.Create(strname,bind,AT_FUNCTION);
                       p.SetAddress(0,sec,address,size);
                       p.objectdata:=data;
                       symbols.insert(p);
                     end;
                  end;
                COFF_SYM_SECTION,
                COFF_SYM_FUNCTION,
                COFF_SYM_FILE :
                  ;
                else
                  internalerror(4342343);
              end;
              FSymTbl^[symidx].sym:=p;
              FSymTbl^[symidx].orgsize:=size;
              { read aux records }
              for i:=1 to sym.aux do
               begin
                 FCoffSyms.Read(auxrec,sizeof(auxrec));
                 inc(symidx);
               end;
              inc(symidx);
            end;
         end;
{$endif internallinker}
      end;


    function  tcoffobjectinput.readobjectdata(data:TAsmObjectData):boolean;
      var
        strsize,
        i        : longint;
        header   : coffheader;
        sechdr   : coffsechdr;
        secname  : array[0..15] of char;
      begin
        result:=false;
{$ifdef internallinker}
        FCoffSyms:=TDynamicArray.Create(symbolresize);
        FCoffStrs:=TDynamicArray.Create(strsresize);
        with tcoffobjectdata(data) do
         begin
           FillChar(Fidx2sec,sizeof(Fidx2sec),0);
           { Read COFF header }
           if not reader.read(header,sizeof(coffheader)) then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
           if header.mach<>$14c then
            begin
              Comment(V_Error,'Not a coff file');
              exit;
            end;
           if header.nsects>255 then
            begin
              Comment(V_Error,'To many sections');
              exit;
            end;
           { Section headers }
           for i:=1 to header.nsects do
            begin
              if not reader.read(sechdr,sizeof(sechdr)) then
               begin
                 Comment(V_Error,'Error reading coff file');
                 exit;
               end;

              move(sechdr.name,secname,8);
              secname[8]:=#0;
              sec:=str2sec(strpas(secname));
              if sec<>sec_none then
               begin
                 Fidx2sec[i]:=sec;
                 createsection(sec);
                 if not win32 then
                  sects[sec].mempos:=sechdr.rvaofs;
                 TCoffObjectSection(sects[sec]).coffrelocs:=sechdr.nrelocs;
                 TCoffObjectSection(sects[sec]).coffrelocpos:=sechdr.relocpos;
                 sects[sec].datapos:=sechdr.datapos;
                 sects[sec].datasize:=sechdr.datasize;
                 sects[sec].memsize:=sechdr.datasize;
                 TCoffObjectSection(sects[sec]).orgmempos:=sects[sec].mempos;
                 sects[sec].flags:=sechdr.flags;
               end
              else
               Comment(V_Warning,'skipping unsupported section '+strpas(sechdr.name));
            end;
           { Symbols }
           Reader.Seek(header.sympos);
           if not Reader.ReadArray(FCoffSyms,header.syms*sizeof(CoffSymbol)) then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
           { Strings }
           if not Reader.Read(strsize,4) then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
           if strsize<4 then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
           if not Reader.ReadArray(FCoffStrs,Strsize-4) then
            begin
              Comment(V_Error,'Error reading coff file');
              exit;
            end;
           { Insert all symbols }
           handle_symbols(data);
           { Sections }
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) and
               (sec<>sec_bss) then
             begin
               Reader.Seek(sects[sec].datapos);
               if not Reader.ReadArray(sects[sec].data,sects[sec].datasize) then
                begin
                  Comment(V_Error,'Error reading coff file');
                  exit;
                end;
             end;
           { Relocs }
           for sec:=low(TSection) to high(TSection) do
            if assigned(sects[sec]) and
               (TCoffObjectSection(sects[sec]).coffrelocs>0) then
             begin
               Reader.Seek(TCoffObjectSection(sects[sec]).coffrelocpos);
               read_relocs(TCoffObjectSection(sects[sec]));
             end;
         end;
        FCoffStrs.Free;
        FCoffSyms.Free;
        result:=true;
{$endif internallinker}
      end;


{****************************************************************************
                                 TCoffAssembler
****************************************************************************}

    constructor TCoffAssembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        objectoutput:=tcoffobjectoutput.createdjgpp(smart);
      end;


{****************************************************************************
                               TPECoffAssembler
****************************************************************************}

    constructor TPECoffAssembler.Create(smart:boolean);
      begin
        inherited Create(smart);
        objectoutput:=tcoffobjectoutput.createwin32(smart);
      end;


{****************************************************************************
                                  TCoffLinker
****************************************************************************}

    constructor TCoffLinker.Create;
      begin
        inherited Create;
        exeoutput:=tcoffexeoutput.createdjgpp;
      end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_i386_coff_info : tasminfo =
          (
            id     : as_i386_coff;
            idtxt  : 'COFF';
            asmbin : '';
            asmcmd : '';
            supported_target : system_i386_go32v2;
            flags : [af_outputbinary];
            labelprefix : '.L';
            comment : '';
          );

    const
       as_i386_pecoff_info : tasminfo =
          (
            id     : as_i386_pecoff;
            idtxt  : 'PECOFF';
            asmbin : '';
            asmcmd : '';
            supported_target : system_i386_win32;
            flags : [af_outputbinary];
            labelprefix : '.L';
            comment : '';
          );

       as_i386_pecoffwdosx_info : tasminfo =
          (
            id     : as_i386_pecoffwdosx;
            idtxt  : 'PEWDOSX';
            asmbin : '';
            asmcmd : '';
            supported_target : system_i386_wdosx;
            flags : [af_outputbinary];
            labelprefix : '.L';
            comment : '';
          );


initialization
  RegisterAssembler(as_i386_coff_info,TCoffAssembler);
  RegisterAssembler(as_i386_pecoff_info,TPECoffAssembler);
  RegisterAssembler(as_i386_pecoffwdosx_info,TPECoffAssembler);
end.
{
  $Log: ogcoff.pas,v $
  Revision 1.34  2005/02/14 17:13:06  peter
    * truncate log

}
