{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an abstract asmoutput class for all processor types

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
{ @abstract(This unit implements an abstract asm output class for all processor types)
  This unit implements an abstract assembler output class for all processors, these
  are then overriden for each assembler writer to actually write the data in these
  classes to an assembler file.
}

unit aasmbase;

{$i fpcdefs.inc}

interface

    uses
       cutils,cclasses,
       globtype,globals,systems;

  { asm symbol functions }
    type
       TAsmsymbind=(AB_NONE,AB_EXTERNAL,AB_COMMON,AB_LOCAL,AB_GLOBAL);

       TAsmsymtype=(AT_NONE,AT_FUNCTION,AT_DATA,AT_SECTION);

       TAsmRelocationType = (RELOC_ABSOLUTE,RELOC_RELATIVE,RELOC_RVA);

       TAsmSectionSizes = array[TSection] of longint;

       TAsmSymbol = class(TNamedIndexItem)
         defbind,
         currbind  : TAsmsymbind;
         typ       : TAsmsymtype;
         { the next fields are filled in the binary writer }
         section : TSection;
         address,
         size    : longint;
         { this need to be incremented with every symbol loading into the
           paasmoutput, thus in loadsym/loadref/const_symbol (PFV) }
         refs    : longint;
         {# Alternate symbol which can be used for 'renaming' needed for
           inlining }
         altsymbol : tasmsymbol;
         objectdata : pointer;
         {# TRUE if the symbol is local for a procedure/function }
         proclocal : boolean;
         {# Is the symbol in the used list }
         inusedlist : boolean;
         { assembler pass label is set, used for detecting multiple labels }
         pass : byte;
         ppuidx : longint;
         constructor create(const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
         procedure reset;
         function  is_used:boolean;
         procedure setaddress(_pass:byte;sec:TSection;offset,len:longint);
       end;

       TAsmLabel = class(TAsmSymbol)
         { this is set by the tai_label.Init }
         is_set,
         { is the label only there for getting an address (e.g. for i/o }
         { checks -> true) or is it a jump target (false)               }
         is_addr : boolean;
         labelnr : longint;
         constructor create(nr:longint);
         constructor createdata(nr:longint);
         constructor createaddr(nr:longint);
         function getname:string;override;
       end;

       TAsmRelocation = class(TLinkedListItem)
          address,
          orgsize  : longint;  { original size of the symbol to relocate, required for COFF }
          symbol   : tasmsymbol;
          section  : TSection; { only used if symbol=nil }
          typ      : TAsmRelocationType;
          constructor CreateSymbol(Aaddress:longint;s:Tasmsymbol;Atyp:TAsmRelocationType);
          constructor CreateSymbolSize(Aaddress:longint;s:Tasmsymbol;Aorgsize:longint;Atyp:TAsmRelocationType);
          constructor CreateSection(Aaddress:longint;sec:TSection;Atyp:TAsmRelocationType);
       end;

       TAsmSection = class(TLinkedListItem)
         name      : string[32];
         secsymidx : longint;   { index for the section in symtab }
         addralign : longint;   { alignment of the section }
         flags     : cardinal;  { section flags }
         { size of the data and in the file }
         dataalignbytes : longint;
         data      : TDynamicArray;
         datasize  : longint;
         datapos   : longint;
         { size and position in memory, set by seTSectionsize }
         memsize,
         mempos    : longint;
         { relocation }
         relocations : TLinkedList;
         constructor create(const Aname:string;Aalign:longint;alloconly:boolean);
         destructor  destroy;override;
         function  write(var d;l:longint):longint;
         function  writestr(const s:string):longint;
         procedure writealign(l:longint);
         function  aligneddatasize:longint;
         procedure alignsection;
         procedure alloc(l:longint);
         procedure addsymreloc(ofs:longint;p:tasmsymbol;relative:TAsmRelocationType);
         procedure addsectionreloc(ofs:longint;sec:TSection;relative:TAsmRelocationType);
       end;

       TAsmObjectAlloc = class
         currsec : TSection;
         secsize : TAsmSectionSizes;
         constructor create;
         destructor  destroy;override;
         procedure seTSection(sec:TSection);
         function  sectionsize:longint;
         procedure sectionalloc(l:longint);
         procedure sectionalign(l:longint);
         procedure staballoc(p:pchar);
         procedure resetSections;
       end;

       TAsmObjectData = class(TLinkedListItem)
       public
         name      : string[80];
         currsec   : TSection;
         sects     : array[TSection] of TAsmSection;
         symbols   : tindexarray; { contains symbols that will be defined in object file }
         constructor create(const n:string);
         destructor  destroy;override;
         procedure createsection(sec:TSection);virtual;
         procedure defaulTSection(sec:TSection);
         function  sectionsize(s:TSection):longint;
         function  currsectionsize:longint;
         procedure setsectionsizes(var s:TAsmSectionSizes);virtual;
         procedure alloc(len:longint);
         procedure allocalign(len:longint);
         procedure writebytes(var data;len:longint);
         procedure writereloc(data,len:longint;p:tasmsymbol;relative:TAsmRelocationType);virtual;abstract;
         procedure writesymbol(p:tasmsymbol);virtual;abstract;
         procedure writestabs(section:TSection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;abstract;
         procedure writesymstabs(section:TSection;offset:longint;p:pchar;ps:tasmsymbol;nidx,nother,line:longint;reloc:boolean);virtual;abstract;
         procedure fixuprelocs;virtual;
       end;

       tasmsymbolidxarr = array[0..$7fffffff div sizeof(pointer)] of tasmsymbol;
       pasmsymbolidxarr = ^tasmsymbolidxarr;

       TAsmLibraryData = class(TLinkedListItem)
       private
         nextaltnr   : longint;
         nextlabelnr : longint;
       public
         name      : string[80];
         symbolsearch : tdictionary; { contains ALL assembler symbols }
         usedasmsymbollist : tsinglelist;
         { ppu }
         asmsymbolppuidx : longint;
         asmsymbolidx : pasmsymbolidxarr; { used for translating ppu index->asmsymbol }
         constructor create(const n:string);
         destructor  destroy;override;
         procedure Freeasmsymbolidx;
         procedure DerefAsmsymbol(var s:tasmsymbol);
         { asmsymbol }
         function  newasmsymbol(const s : string) : tasmsymbol;
         function  newasmsymboltype(const s : string;_bind:TAsmSymBind;_typ:TAsmsymtype) : tasmsymbol;
         function  getasmsymbol(const s : string) : tasmsymbol;
         function  renameasmsymbol(const sold, snew : string):tasmsymbol;
         {# create a new assembler label }
         procedure getlabel(var l : tasmlabel);
         { make l as a new label and flag is_addr }
         procedure getaddrlabel(var l : tasmlabel);
         { make l as a new label and flag is_data }
         procedure getdatalabel(var l : tasmlabel);
         {# return a label number }
         procedure getlabelnr(var l : longint);
         procedure CreateUsedAsmSymbolList;
         procedure DestroyUsedAsmSymbolList;
         procedure UsedAsmSymbolListInsert(p:tasmsymbol);
         { generate an alternative (duplicate) symbol }
         procedure GenerateAltSymbol(p:tasmsymbol);
         { reset alternative symbol information }
         procedure UsedAsmSymbolListResetAltSym;
         procedure UsedAsmSymbolListReset;
         procedure UsedAsmSymbolListCheckUndefined;
       end;


    var
      current_library : tasmlibrarydata;


implementation

    uses
{$ifdef delphi}
      sysutils,
{$else}
      strings,
{$endif}
      fmodule,verbose;

    const
      symbolsgrow = 100;


{*****************************************************************************
                                 TAsmSymbol
*****************************************************************************}

    constructor tasmsymbol.create(const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
      begin;
        inherited createname(s);
        reset;
        defbind:=_bind;
        typ:=_typ;
        inusedlist:=false;
        pass:=255;
        ppuidx:=-1;
        { mainly used to remove unused labels from the codesegment }
        refs:=0;
      end;

    procedure tasmsymbol.reset;
      begin
        { reset section info }
        section:=sec_none;
        address:=0;
        size:=0;
        indexnr:=-1;
        pass:=255;
        currbind:=AB_EXTERNAL;
        proclocal:=false;
      end;

    function tasmsymbol.is_used:boolean;
      begin
        is_used:=(refs>0);
      end;

    procedure tasmsymbol.setaddress(_pass:byte;sec:TSection;offset,len:longint);
      begin
        if (_pass=pass) then
         begin
           Message1(asmw_e_duplicate_label,name);
           exit;
         end;
        pass:=_pass;
        section:=sec;
        address:=offset;
        size:=len;
        { when the bind was reset to External, set it back to the default
          bind it got when defined }
        if (currbind=AB_EXTERNAL) and (defbind<>AB_NONE) then
         currbind:=defbind;
      end;


{*****************************************************************************
                                 TAsmLabel
*****************************************************************************}

    constructor tasmlabel.create(nr:longint);
      begin;
        labelnr:=nr;
        inherited create(target_asm.labelprefix+tostr(labelnr),AB_LOCAL,AT_FUNCTION);
        proclocal:=true;
        is_set:=false;
        is_addr := false;
      end;


    constructor tasmlabel.createdata(nr:longint);
      begin;
        labelnr:=nr;
        if (cs_create_smart in aktmoduleswitches) or
           target_asm.labelprefix_only_inside_procedure then
          inherited create('_$'+current_module.modulename^+'$_L'+tostr(labelnr),AB_GLOBAL,AT_DATA)
        else
          inherited create(target_asm.labelprefix+tostr(labelnr),AB_LOCAL,AT_DATA);
        is_set:=false;
        is_addr := false;
        { write it always }
        refs:=1;
      end;

    constructor tasmlabel.createaddr(nr:longint);
      begin;
        create(nr);
        is_addr := true;
      end;

    function tasmlabel.getname:string;
      begin
        getname:=inherited getname;
        inc(refs);
      end;


{****************************************************************************
                                TAsmObjectAlloc
****************************************************************************}

    constructor TAsmObjectAlloc.create;
      begin
      end;


    destructor TAsmObjectAlloc.destroy;
      begin
      end;


    procedure TAsmObjectAlloc.seTSection(sec:TSection);
      begin
        currsec:=sec;
      end;


    procedure TAsmObjectAlloc.reseTSections;
      begin
        FillChar(secsize,sizeof(secsize),0);
      end;


    procedure TAsmObjectAlloc.sectionalloc(l:longint);
      begin
        inc(secsize[currsec],l);
      end;


    procedure TAsmObjectAlloc.sectionalign(l:longint);
      begin
        if (secsize[currsec] mod l)<>0 then
          inc(secsize[currsec],l-(secsize[currsec] mod l));
      end;


    procedure TAsmObjectAlloc.staballoc(p:pchar);
      begin
        inc(secsize[sec_stab]);
        if assigned(p) and (p[0]<>#0) then
          inc(secsize[sec_stabstr],strlen(p)+1);
      end;


    function TAsmObjectAlloc.sectionsize:longint;
      begin
        sectionsize:=secsize[currsec];
      end;


{****************************************************************************
                              TAsmRelocation
****************************************************************************}

    constructor TAsmRelocation.CreateSymbol(Aaddress:longint;s:Tasmsymbol;Atyp:TAsmRelocationType);
      begin
        Address:=Aaddress;
        Symbol:=s;
        OrgSize:=0;
        Section:=Sec_none;
        Typ:=Atyp;
      end;


    constructor TAsmRelocation.CreateSymbolSize(Aaddress:longint;s:Tasmsymbol;Aorgsize:longint;Atyp:TAsmRelocationType);
      begin
        Address:=Aaddress;
        Symbol:=s;
        OrgSize:=Aorgsize;
        Section:=Sec_none;
        Typ:=Atyp;
      end;


    constructor TAsmRelocation.CreateSection(Aaddress:longint;sec:TSection;Atyp:TAsmRelocationType);
      begin
        Address:=Aaddress;
        Symbol:=nil;
        OrgSize:=0;
        Section:=sec;
        Typ:=Atyp;
      end;


{****************************************************************************
                              TAsmSection
****************************************************************************}

    constructor TAsmSection.create(const Aname:string;Aalign:longint;alloconly:boolean);
      begin
        inherited create;
        name:=Aname;
        secsymidx:=0;
        addralign:=Aalign;
        { data }
        datasize:=0;
        datapos:=0;
        if alloconly then
         data:=nil
        else
         Data:=TDynamicArray.Create(8192);
        { position }
        mempos:=0;
        memsize:=0;
        { relocation }
        relocations:=TLinkedList.Create;
      end;


    destructor TAsmSection.destroy;
      begin
        if assigned(Data) then
          Data.Free;
        relocations.free;
      end;


    function TAsmSection.write(var d;l:longint):longint;
      begin
        write:=datasize;
        if not assigned(Data) then
         Internalerror(3334441);
        Data.write(d,l);
        inc(datasize,l);
      end;


    function TAsmSection.writestr(const s:string):longint;
      begin
        writestr:=datasize;
        if not assigned(Data) then
         Internalerror(3334441);
        Data.write(s[1],length(s));
        inc(datasize,length(s));
      end;


    procedure TAsmSection.writealign(l:longint);
      var
        i : longint;
        empty : array[0..63] of char;
      begin
        { no alignment needed for 0 or 1 }
        if l<=1 then
         exit;
        i:=datasize mod l;
        if i>0 then
         begin
           if assigned(data) then
            begin
              fillchar(empty,sizeof(empty),0);
              Data.write(empty,l-i);
            end;
           inc(datasize,l-i);
         end;
      end;


    function TAsmSection.aligneddatasize:longint;
      begin
        aligneddatasize:=align(datasize,addralign);
      end;


    procedure TAsmSection.alignsection;
      begin
        writealign(addralign);
      end;


    procedure TAsmSection.alloc(l:longint);
      begin
        if assigned(Data) then
         Internalerror(3334442);
        inc(datasize,l);
      end;


    procedure TAsmSection.addsymreloc(ofs:longint;p:tasmsymbol;relative:TAsmRelocationType);
      var
        r : TAsmRelocation;
      begin
        r:=TAsmRelocation.Create;
        r.address:=ofs;
        r.orgsize:=0;
        r.symbol:=p;
        r.section:=sec_none;
        r.typ:=relative;
        relocations.concat(r);
      end;


    procedure TAsmSection.addsectionreloc(ofs:longint;sec:TSection;relative:TAsmRelocationType);
      var
        r : TAsmRelocation;
      begin
        r:=TAsmRelocation.Create;
        r.address:=ofs;
        r.symbol:=nil;
        r.orgsize:=0;
        r.section:=sec;
        r.typ:=relative;
        relocations.concat(r);
      end;


{****************************************************************************
                                TAsmObjectData
****************************************************************************}

    constructor TAsmObjectData.create(const n:string);
      begin
        inherited create;
        name:=n;
        { sections }
        FillChar(Sects,sizeof(Sects),0);
        { symbols }
        symbols:=tindexarray.create(symbolsgrow);
        symbols.noclear:=true;
      end;


    destructor TAsmObjectData.destroy;
      var
        sec : TSection;
      begin
        { free memory }
        for sec:=low(TSection) to high(TSection) do
         if assigned(sects[sec]) then
          sects[sec].free;
        symbols.free;
      end;


    procedure TAsmObjectData.createsection(sec:TSection);
      begin
        sects[sec]:=TAsmSection.create(target_asm.secnames[sec],1,(sec=sec_bss));
      end;


    function TAsmObjectData.sectionsize(s:TSection):longint;
      begin
        if assigned(sects[s]) then
         sectionsize:=sects[s].datasize
        else
         sectionsize:=0;
      end;


    function TAsmObjectData.currsectionsize:longint;
      begin
        if assigned(sects[currsec]) then
         currsectionsize:=sects[currsec].datasize
        else
         currsectionsize:=0;
      end;


    procedure TAsmObjectData.seTSectionsizes(var s:TAsmSectionSizes);
      begin
      end;


    procedure TAsmObjectData.defaulTSection(sec:TSection);
      begin
        currsec:=sec;
      end;


    procedure TAsmObjectData.writebytes(var data;len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec].write(data,len);
      end;


    procedure TAsmObjectData.alloc(len:longint);
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        sects[currsec].alloc(len);
      end;


    procedure TAsmObjectData.allocalign(len:longint);
      var
        modulo : longint;
      begin
        if not assigned(sects[currsec]) then
         createsection(currsec);
        modulo:=sects[currsec].datasize mod len;
        if modulo > 0 then
          sects[currsec].alloc(len-modulo);
      end;


    procedure TAsmObjectData.fixuprelocs;
      begin
        { no relocation support by default }
      end;


{****************************************************************************
                                TAsmLibraryData
****************************************************************************}

    constructor TAsmLibraryData.create(const n:string);
      begin
        inherited create;
        name:=n;
        { symbols }
        symbolsearch:=tdictionary.create;
        symbolsearch.usehash;
        { labels }
        nextaltnr:=1;
        nextlabelnr:=1;
        { ppu }
        asmsymbolppuidx:=0;
        asmsymbolidx:=nil;
      end;


    destructor TAsmLibraryData.destroy;
      begin
        symbolsearch.free;
        Freeasmsymbolidx;
      end;


    procedure TAsmLibraryData.Freeasmsymbolidx;
      begin
        if assigned(asmsymbolidx) then
         begin
           Freemem(asmsymbolidx);
           asmsymbolidx:=nil;
         end;
      end;


    procedure TAsmLibraryData.DerefAsmsymbol(var s:tasmsymbol);
      begin
        if not assigned(asmsymbolidx) then
          internalerror(200208072);
        if longint(pointer(s))>=asmsymbolppuidx then
          internalerror(200208073);
        s:=asmsymbolidx^[longint(pointer(s))];
      end;


    function TAsmLibraryData.newasmsymbol(const s : string) : tasmsymbol;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(symbolsearch.search(s));
        if not assigned(hp) then
         begin
           { Not found, insert it as an External }
           hp:=tasmsymbol.create(s,AB_EXTERNAL,AT_FUNCTION);
           symbolsearch.insert(hp);
         end;
        newasmsymbol:=hp;
      end;


    function TAsmLibraryData.newasmsymboltype(const s : string;_bind:TAsmSymBind;_typ:Tasmsymtype) : tasmsymbol;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(symbolsearch.search(s));
        if assigned(hp) then
         hp.defbind:=_bind
        else
         begin
           { Not found, insert it as an External }
           hp:=tasmsymbol.create(s,_bind,_typ);
           symbolsearch.insert(hp);
         end;
        newasmsymboltype:=hp;
      end;


    function TAsmLibraryData.getasmsymbol(const s : string) : tasmsymbol;
      begin
        getasmsymbol:=tasmsymbol(symbolsearch.search(s));
      end;


    function TAsmLibraryData.renameasmsymbol(const sold, snew : string):tasmsymbol;
      begin
        renameasmsymbol:=tasmsymbol(symbolsearch.rename(sold,snew));
      end;


    procedure TAsmLibraryData.CreateUsedAsmSymbolList;
      begin
        if assigned(usedasmsymbollist) then
         internalerror(78455782);
        usedasmsymbollist:=TSingleList.create;
      end;


    procedure TAsmLibraryData.DestroyUsedAsmSymbolList;
      begin
        usedasmsymbollist.destroy;
        usedasmsymbollist:=nil;
      end;


    procedure TAsmLibraryData.UsedAsmSymbolListInsert(p:tasmsymbol);
      begin
        if not p.inusedlist then
         usedasmsymbollist.insert(p);
        p.inusedlist:=true;
      end;


    procedure TAsmLibraryData.GenerateAltSymbol(p:tasmsymbol);
      begin
        if not assigned(p.altsymbol) then
         begin
           p.altsymbol:=tasmsymbol.create(name+'_'+tostr(nextaltnr),p.defbind,p.typ);
           { also copy the amount of references }
           p.altsymbol.refs:=p.refs;
           inc(nextaltnr);
           { add to the usedasmsymbollist, that list is used to reset the
             altsymbol }
           if not p.inusedlist then
            usedasmsymbollist.insert(p);
           p.inusedlist:=true;
         end;
      end;


    procedure TAsmLibraryData.UsedAsmSymbolListReset;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(usedasmsymbollist.first);
        while assigned(hp) do
         begin
           with hp do
            begin
              reset;
              inusedlist:=false;
            end;
           hp:=tasmsymbol(hp.listnext);
         end;
      end;


    procedure TAsmLibraryData.UsedAsmSymbolListResetAltSym;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(usedasmsymbollist.first);
        while assigned(hp) do
         begin
           with hp do
            begin
              altsymbol:=nil;
              inusedlist:=false;
            end;
           hp:=tasmsymbol(hp.listnext);
         end;
      end;


    procedure TAsmLibraryData.UsedAsmSymbolListCheckUndefined;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(usedasmsymbollist.first);
        while assigned(hp) do
         begin
           with hp do
            begin
              if (refs>0) and
                 (section=Sec_none) and
                 not(currbind in [AB_EXTERNAL,AB_COMMON]) then
               Message1(asmw_e_undefined_label,name);
            end;
           hp:=tasmsymbol(hp.listnext);
         end;
      end;


    procedure TAsmLibraryData.getlabel(var l : tasmlabel);
      begin
        l:=tasmlabel.create(nextlabelnr);
        inc(nextlabelnr);
        symbolsearch.insert(l);
      end;


    procedure TAsmLibraryData.getdatalabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createdata(nextlabelnr);
        inc(nextlabelnr);
        symbolsearch.insert(l);
      end;


    procedure TAsmLibraryData.getaddrlabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createaddr(nextlabelnr);
        inc(nextlabelnr);
        symbolsearch.insert(l);
      end;


    procedure TAsmLibraryData.getlabelnr(var l : longint);
      begin
         l:=nextlabelnr;
         inc(nextlabelnr);
      end;


end.
{
  $Log$
  Revision 1.4  2002-08-11 13:24:10  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.3  2002/07/10 07:24:40  jonas
    * memory leak fixes from Sergey Korshunoff

  Revision 1.2  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.1  2002/07/01 18:46:20  peter
    * internal linker
    * reorganized aasm layer

}
