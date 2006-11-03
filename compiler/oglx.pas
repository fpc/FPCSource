{
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

{ An LX executable is called a module; it can be either an executable
  or a DLL.

  A module consists of objects. In other executable formats, these
  are usually called sections.

  Objects consist of pages.

  The objects are numbered, numbers do not have any special meaning. The
  pages of the object are loaded into memory with the access rights specified
  the object table entry. (DM)}


{ For the operating system the object numbers have no special meaning.
  However, for Free Pascal generated executables, I define: (DM)}

const   code_object = 0;
    data_object = 1;
    bss_object  = 2;
    stack_object    = 3;
    heap_object = 4;

type    Tlxheader = packed record
        magic:word;             {'LX'}
        byteorder:byte;         {0 = little 1 = big endian.}
        wordorder:byte;         {0 = little 1 = big endian.}
        format_level:cardinal;      {Nothing else than LX level
                         0 has ever been defined.}
        cpu_type:word;          {1 = 286, 2 = 386, 3 = 486,
                         4 = pentium.}
        os_type:word;           {1 = OS/2, 2 = Windows,
                         3 = Siemens MS-Dos 4.0,
                         4 = Windows 386.}
        module_version:cardinal;        {Version of executable,
                         defined by user.}
        module_flags:cardinal;      {Flags.}
        module_page_count:cardinal;     {Amount of pages in module.}
        eip_object,eip:cardinal;        {Initial EIP, object nr and
                         offset within object.}
        esp_object,esp:cardinal;        {Initial ESP, object nr and
                         offset within object.}
        page_size,page_shift:cardinal;  {Page size, in bytes and
                         1 << pageshift.}
        fixup_sect_size:cardinal;
        fixup_sect_checksum:cardinal;
        loader_sect_size:cardinal;
        loader_sect_chksum:cardinal;
        object_table_offset:cardinal;   {Location of object table.}
        object_count:cardinal;      {Amount of objects in module.}
        object_pagetable_ofs:cardinal;  {Location of object page
                         table.}
        object_iterpages_ofs:cardinal;
        resource_table_ofs:cardinal;    {Location of resource table.}
        resource_count:cardinal;        {Amount of resources in
                         resource table.}
        resid_name_tbl_ofs:cardinal;
        entry_table_offset:cardinal;
        module_dir_offset:cardinal;
        module_dir_count:cardinal;
        fixup_pagetab_ofs:cardinal;
        fixup_recrab_ofs:cardinal;
        import_modtab_ofs:cardinal;
        import_modtab_count:cardinal;
        data_pages_offset:cardinal;
        preload_page_count:cardinal;
        nonresid_table_ofs:cardinal;
        nonresid_table_len:cardinal;
        nonresid_tbl_chksum:cardinal;
        auto_ds_object_no:cardinal;     {Not used by OS/2.}
        debug_info_offset:cardinal;
        inst_preload_count:cardinal;
        inst_demand_count:cardinal;
        heapsize:cardinal;          {Only used for 16-bit programs.}
    end;

    Tlxobject_flags = (ofreadable,ofwriteable,ofexecutable,ofresource,
               ofdiscardable,ofshared,ofpreload,ofinvalid,
               ofzerofilled);
    Tlxobject_flag_set = set of Tlxobject_flags;

    Tlxobject_table_entry = packed record
        virtual_size:cardinal;
        reloc_base_addr:cardinal;
        object_flags:Tlxobject_flag_set;
        page_table_index:cardinal;
        page_count:cardinal;
        reserved:cardinal;
    end;

    Tlxexeoutput = class(texeoutput)
    private
{           FCoffsyms,
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

       Tlxlinker = class(tinternallinker)
         constructor create;override;
       end;


implementation

uses
       strings,
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
        objdata   : TObjData;
        p         : tasmsymbol;
        s         : string;}
      begin
(*        objdata:=TObjData(objdatalist.first);
        while assigned(objdata) do
         begin
           with tcoffObjData(objdata) do
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
           objdata:=TObjData(objdata.next);
         end;*)
      end;


    procedure Tlxexeoutput.CalculateMemoryMap;
{      var
        objdata : TObjData;
        secsymidx,
        mempos,
        datapos : longint;
        sec     : TSection;
        sym     : tasmsymbol;
        s       : TObjSection;}
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
        MapObjData(datapos,mempos);
        { end symbol }
        AddGlobalSym('_etext',sections[sec_code].mempos+sections[sec_code].memsize);
        AddGlobalSym('_edata',sections[sec_data].mempos+sections[sec_data].memsize);
        AddGlobalSym('end',mempos);
        { symbols }
        nsyms:=0;
        sympos:=0;
        if not(cs_link_strip in current_settings.globalswitches) then
         begin
           sympos:=datapos;
           objdata:=TObjData(objdatalist.first);
           while assigned(objdata) do
            begin
              inc(nsyms,objdata.symbols.count);
              objdata:=TObjData(objdata.next);
            end;
         end;*)
      end;

function gen_section_header(sec:Tsection;obj:cardinal):Tlxobject_table_entry;
        virtual_size:cardinal;
        reloc_base_addr:cardinal;
        object_flags:Tlxobject_flag_set;
        page_table_index:cardinal;
        page_count:cardinal;
        reserved:cardinal;

begin
    gen_section_header.virtual_size:=sections[sec.memsize];

end;

function Tlxexeoutput.writedata:boolean;

var header:Tlxheader;
    hsym:Tasmsymbol;
    code_object_header,data_object_header,bss_object_header,stack_object_header,
     heap_object_header:Tlxobject_table_entry;


begin
    result:=false;
    fillchar(header,sizeof(header),0);
    header.magic:=$584c;        {'LX'}
    header.cpu_type:=2;         {Intel 386}
    header.os_type:=1;          {OS/2}
    {Set the initial EIP.}
    header.eip_object:=code_object;
    hsym:=tasmsymbol(globalsyms.Find('start'));
    if not assigned(hsym) then
    begin
        comment(V_Error,'Entrypoint "start" not defined');
        exit;
    end;
    header.eip:=hsym.address-sections[sec_code].mempos;
    {Set the initial ESP.}
    header.esp_object:=stack_object;
    header.esp:=stacksize;
    Fwriter.write(header,sizeof(header));
    for sec:=low(Tsection) to high(Tsection) do
    if sections[sec].available then
        if not(sec in [sec_code,sec_data,sec_bss,sec_stab,sec_stabstr]) then
            begin
            result:=false;
            exit;
        end;
    code_object_header:=gen_section_header(sec_code,code_object);
    data_object_header:=gen_section_header(sec_data,data_object);
    bss_object_header:=gen_section_header(sec_bss,bss_object);
    result:=true;
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
  RegisterAssembler(as_i386_pecoffwdosx_info,TPECoffAssembler);

  RegisterLinker(ld_i386_coff,Tlxlinker);}
end.
