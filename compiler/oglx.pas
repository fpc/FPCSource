{
    $Id$
    Copyright (c) 2002 by Daniel Mantione, Peter Vreman

    Contains the binary reader and writer for the linear executable
    format used by OS/2

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
unit oglx;

{$i fpcdefs.inc}

interface

uses
       { common }
       cclasses,
       { target }
       systems,
       { assembler }
       cpubase,aasmbase,assemble,link,
       { output }
       ogbase,ogmap,ogcoff;

type    Tlxheader = packed record
	    magic:word;				{'LX'}
	    byteorder:byte;			{0 = little 1 = big endian}
	    wordorder:byte;
	    cpu_type:word;
	    os_type:word;
	    module_version:cardinal;
	    module_flags:cardinal;
	    module_page_count:cardinal;
	    eip_object,eip:cardinal;
	    esp_object,esp:cardinal;
	    page_size,page_shift:cardinal;
	    fixup_sect_size:cardinal;
	    fixup_sect_checksum:cardinal;
	    loader_sect_size:cardinal;
	    loader_sect_chksum:cardinal;
	    object_table_offset:cardinal;
	    object_count:cardinal;
	    object_pagetable_ofs:cardinal;
	    object_iterpages_ofs:cardinal;
	    resource_table_ofs:cardinal;
	    resource_count:cardinal;
	    
	end;

	Tlxexeoutput = class(texeoutput)
	private
{       	FCoffsyms,
         FCoffStrs : tdynamicarray;
         win32   : boolean;}
         nsects,
         nsyms,
         sympos : longint;
         procedure write_symbol(const name:string;strpos,value,section,typ,aux:longint);
         procedure write_symbols;
       protected
         function writedata:boolean;override;
       public
         constructor createos2;
         function  newobjectinput:tobjectinput;override;
         procedure CalculateMemoryMap;override;
         procedure GenerateExecutable(const fn:string);override;
       end;

       tlxlinker = class(tinternallinker)
         constructor create;override;
       end;


implementation

uses
{$ifdef delphi}
       sysutils,
{$else}
       strings,
{$endif}
       cutils,verbose,
       globtype,globals,fmodule;


{****************************************************************************
                              tcoffexeoutput
****************************************************************************}

    constructor Tlxexeoutput.createos2;
      begin
        inherited create;
      end;


    function Tlxexeoutput.newobjectinput:tobjectinput;
      begin
         result:=tcoffobjectinput.createdjgpp;
      end;


    procedure Tlxexeoutput.write_symbol(const name:string;strpos,value,section,typ,aux:longint);
{      var
        sym : coffsymbol;}
      begin
{        FillChar(sym,sizeof(sym),0);
        if strpos=-1 then
         move(name[1],sym.name,length(name))
        else
         sym.strpos:=strpos;
        sym.value:=value;
        sym.section:=section;
        sym.typ:=typ;
        sym.aux:=aux;
        FWriter.write(sym,sizeof(sym));}
      end;


    procedure Tlxexeoutput.write_symbols;
{      var
        filename  : string[18];
        sec       : TSection;
        namestr   : string[8];
        nameidx,
        value,
        sectionval,
        i         : longint;
        globalval : byte;
        secrec    : coffsectionrec;
        objdata   : TAsmObjectData;
        p         : tasmsymbol;
        s         : string;}
      begin
(*        objdata:=TAsmObjectData(objdatalist.first);
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
                  sectionval:=sections[p.section].secsymidx;
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
         end;*)
      end;


    procedure Tlxexeoutput.CalculateMemoryMap;
{      var
        objdata : TAsmObjectData;
        secsymidx,
        mempos,
        datapos : longint;
        sec     : TSection;
        sym     : tasmsymbol;
        s       : TAsmSection;}
      begin
(*        { retrieve amount of sections }
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
         end;*)
      end;


    function Tlxexeoutput.writedata:boolean;
{      var
        datapos,
        secsymidx,
        i         : longint;
        hstab     : coffstab;
        gotreloc  : boolean;
        sec       : TSection;
        header    : coffheader;
        optheader : coffoptheader;
        sechdr    : coffsechdr;
        empty     : array[0..15] of byte;
        hp        : pdynamicblock;
        objdata   : TAsmObjectData;
        hsym      : tasmsymbol;}
      begin
(*        result:=false;
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
                  WriteZeros(objdata.sects[sec].dataalignbytes);
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
        result:=true;*)
      end;


    procedure Tlxexeoutput.GenerateExecutable(const fn:string);
      begin
{        AddGlobalSym('_etext',0);
        AddGlobalSym('_edata',0);
        AddGlobalSym('end',0);
        if not CalculateSymbols then
         exit;
        CalculateMemoryMap;
        FixupSymbols;
        FixupRelocations;
        writeexefile(fn);}
      end;

{****************************************************************************
                                  TCoffLinker
****************************************************************************}

    constructor Tlxlinker.Create;
      begin
        inherited Create;
        exeoutput:=Tlxexeoutput.createos2;
      end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}


begin
{  RegisterAssembler(as_i386_coff_info,TCoffAssembler);
  RegisterAssembler(as_i386_pecoff_info,TPECoffAssembler);
  RegisterAssembler(as_i386_pecoffwdosx_info,TPECoffAssembler);}

  RegisterLinker(ld_i386_coff,Tlxlinker);
end.
{
  $Log$
  Revision 1.1  2002-07-08 19:22:22  daniel
  + OS/2 lx format support: Copied ogcoff and started to modify it

}
