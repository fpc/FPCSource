{
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
       globtype,globals,systems
       ;

    type
       TAsmsymbind=(AB_NONE,AB_EXTERNAL,AB_COMMON,AB_LOCAL,AB_GLOBAL);

       TAsmsymtype=(AT_NONE,AT_FUNCTION,AT_DATA,AT_SECTION,AT_LABEL);

       TAsmSectiontype=(sec_none,
         sec_code,
         sec_data,
         sec_rodata,
         sec_bss,
         sec_threadvar,
         { used for darwin import stubs }
         sec_stub,
         { stabs }
         sec_stab,sec_stabstr,
         { win32 }
         sec_idata2,sec_idata4,sec_idata5,sec_idata6,sec_idata7,sec_edata,
         { C++ exception handling unwinding (uses dwarf) }
         sec_eh_frame,
         { dwarf }
         sec_debug_frame,
         sec_debug_info,
         sec_debug_line,
         sec_debug_abbrev,
         { ELF resources }
         sec_fpc,
         { Table of contents section }
         sec_toc
       );

       TAsmSymbol = class(TNamedIndexItem)
       private
         { this need to be incremented with every symbol loading into the
           taasmoutput with loadsym/loadref/const_symbol (PFV) }
         refs       : longint;
       public
         bind       : TAsmsymbind;
         typ        : TAsmsymtype;
         { Alternate symbol which can be used for 'renaming' needed for
           asm inlining. Also used for external and common solving during linking }
         altsymbol  : TAsmSymbol;
         { Cached objsymbol }
         cachedobjsymbol : TObject;
         constructor create(const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
         function  is_used:boolean;
         procedure increfs;
         procedure decrefs;
         function getrefs: longint;
       end;

       { is the label only there for getting an DataOffset (e.g. for i/o
         checks -> alt_addr) or is it a jump target (alt_jump), for debug
         info alt_dbgline and alt_dbgfile }
       TAsmLabelType = (alt_jump,alt_addr,alt_data,alt_dbgline,alt_dbgfile,alt_dbgtype,alt_dbgframe);

       TAsmLabel = class(TAsmSymbol)
         labelnr   : longint;
         labeltype : TAsmLabelType;
         is_set    : boolean;
         constructor createlocal(nr:longint;ltyp:TAsmLabelType);
         constructor createglobal(const modulename:string;nr:longint;ltyp:TAsmLabelType);
         function getname:string;override;
       end;

       tasmsymbolidxarr = array[0..($7fffffff div sizeof(pointer))-1] of tasmsymbol;
       pasmsymbolidxarr = ^tasmsymbolidxarr;

       TObjLibraryData = class(TLinkedListItem)
       private
         nextaltnr   : longint;
         nextlabelnr : array[Tasmlabeltype] of longint;
       public
         name,
         realname     : string[80];
         symbolsearch : tdictionary; { contains ALL assembler symbols }
         AltSymbollist : TFPObjectList;
         constructor create(const n:string);
         destructor  destroy;override;
         { asmsymbol }
         function  newasmsymbol(const s : string;_bind:TAsmSymBind;_typ:TAsmsymtype) : tasmsymbol;
         function  getasmsymbol(const s : string) : tasmsymbol;
         function  newasmlabel(nr:longint;alt:tasmlabeltype;is_global:boolean) : tasmlabel;
         {# create a new assembler label }
         procedure getlabel(var l : tasmlabel;alt:tasmlabeltype);
         {# create a new assembler label for jumps }
         procedure getjumplabel(var l : tasmlabel);
         { make l as a new label and flag is_addr }
         procedure getaddrlabel(var l : tasmlabel);
         { make l as a new label and flag is_data }
         procedure getdatalabel(var l : tasmlabel);
         {# return a label number }
         { generate an alternative (duplicate) symbol }
         procedure GenerateAltSymbol(p:tasmsymbol);
         procedure ResetAltSymbols;
       end;

    function LengthUleb128(a: aword) : byte;
    function LengthSleb128(a: aint) : byte;

    const
      { alt_jump,alt_addr,alt_data,alt_dbgline,alt_dbgfile }
      asmlabeltypeprefix : array[tasmlabeltype] of char = ('j','a','d','l','f','t','c');

    var
      objectlibrary : TObjLibraryData;


implementation

    uses
      strings,
      verbose;

    const
      sectsgrow   = 100;
      symbolsgrow = 100;


    function LengthUleb128(a: aword) : byte;
      var
        b: byte;
      begin
        result:=0;
        repeat
          b := a and $7f;
          a := a shr 7;
          if a<>0 then
            b := b or $80;
          inc(result);
          if a=0 then
            break;
        until false;
      end;


    function LengthSleb128(a: aint) : byte;
      var
        b, size: byte;
        neg, more: boolean;
      begin
        more := true;
        neg := a < 0;
        size := sizeof(a)*8;
        result:=0;
        repeat
          b := a and $7f;
          a := a shr 7;
          if neg then
            a := a or -(1 shl (size - 7));
          if (((a = 0) and
               (b and $40 = 0)) or
              ((a = -1) and
               (b and $40 <> 0))) then
            more := false
          else
            b := b or $80;
          inc(result);
          if not(more) then
            break;
        until false;
      end;


{*****************************************************************************
                                 TAsmSymbol
*****************************************************************************}

    constructor tasmsymbol.create(const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
      begin;
        inherited createname(s);
        bind:=_bind;
        typ:=_typ;
        { used to remove unused labels from the al_procedures }
        refs:=0;
      end;


    function tasmsymbol.is_used:boolean;
      begin
        is_used:=(refs>0);
      end;


    procedure tasmsymbol.increfs;
      begin
        inc(refs);
      end;


    procedure tasmsymbol.decrefs;
      begin
        dec(refs);
        if refs<0 then
          internalerror(200211121);
      end;


    function tasmsymbol.getrefs: longint;
      begin
        getrefs := refs;
      end;


{*****************************************************************************
                                 TAsmLabel
*****************************************************************************}

    constructor tasmlabel.createlocal(nr:longint;ltyp:TAsmLabelType);
      begin;
        inherited create(target_asm.labelprefix+asmlabeltypeprefix[ltyp]+tostr(nr),AB_LOCAL,AT_LABEL);
        labelnr:=nr;
        labeltype:=ltyp;
        is_set:=false;
      end;


    constructor tasmlabel.createglobal(const modulename:string;nr:longint;ltyp:TAsmLabelType);
      begin;
        inherited create('_$'+modulename+'$_L'+asmlabeltypeprefix[ltyp]+tostr(nr),AB_GLOBAL,AT_DATA);
        labelnr:=nr;
        labeltype:=ltyp;
        is_set:=false;
        { write it always }
        increfs;
      end;


    function tasmlabel.getname:string;
      begin
        getname:=inherited getname;
        increfs;
      end;


{****************************************************************************
                                TObjLibraryData
****************************************************************************}

    constructor TObjLibraryData.create(const n:string);
      var
        alt : TAsmLabelType;
      begin
        inherited create;
        realname:=n;
        name:=upper(n);
        { symbols }
        symbolsearch:=tdictionary.create;
        symbolsearch.usehash;
        AltSymbollist:=TFPObjectList.Create(false);
        { labels }
        nextaltnr:=1;
        for alt:=low(TAsmLabelType) to high(TAsmLabelType) do
          nextlabelnr[alt]:=1;
      end;


    destructor TObjLibraryData.destroy;
      begin
        AltSymbollist.free;
        symbolsearch.free;
      end;


    function TObjLibraryData.newasmsymbol(const s : string;_bind:TAsmSymBind;_typ:Tasmsymtype) : tasmsymbol;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(symbolsearch.search(s));
        if assigned(hp) then
         begin
           {$IFDEF EXTDEBUG}
           if (_typ <> AT_NONE) and
              (hp.typ <> _typ) and
              not(cs_compilesystem in aktmoduleswitches) and
              (target_info.system <> system_powerpc_darwin) then
             begin
               //Writeln('Error symbol '+hp.name+' type is ',Ord(_typ),', should be ',Ord(hp.typ));
               InternalError(2004031501);
             end;
           {$ENDIF}
           if (_bind<>AB_EXTERNAL) then
             hp.bind:=_bind
         end
        else
         begin
           { Not found, insert it. }
           hp:=tasmsymbol.create(s,_bind,_typ);
           symbolsearch.insert(hp);
         end;
        newasmsymbol:=hp;
      end;


    function TObjLibraryData.getasmsymbol(const s : string) : tasmsymbol;
      begin
        getasmsymbol:=tasmsymbol(symbolsearch.search(s));
      end;


    procedure TObjLibraryData.GenerateAltSymbol(p:tasmsymbol);
      begin
        if not assigned(p.altsymbol) then
         begin
           p.altsymbol:=tasmsymbol.create(p.name+'_'+tostr(nextaltnr),p.bind,p.typ);
           symbolsearch.insert(p.altsymbol);
           AltSymbollist.Add(p);
         end;
      end;


    procedure TObjLibraryData.ResetAltSymbols;
      var
        i  : longint;
      begin
        for i:=0 to AltSymbollist.Count-1 do
          tasmsymbol(AltSymbollist[i]).altsymbol:=nil;
        AltSymbollist.Clear;
      end;


    function  TObjLibraryData.newasmlabel(nr:longint;alt:tasmlabeltype;is_global:boolean) : tasmlabel;
      var
        hp : tasmlabel;
      begin
        if is_global then
         hp:=tasmlabel.createglobal(name,nr,alt)
       else
         hp:=tasmlabel.createlocal(nr,alt);
        symbolsearch.insert(hp);
        newasmlabel:=hp;
      end;


    procedure TObjLibraryData.getlabel(var l : tasmlabel;alt:tasmlabeltype);
      begin
        l:=tasmlabel.createlocal(nextlabelnr[alt],alt);
        inc(nextlabelnr[alt]);
        symbolsearch.insert(l);
      end;


    procedure TObjLibraryData.getjumplabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createlocal(nextlabelnr[alt_jump],alt_jump);
        inc(nextlabelnr[alt_jump]);
        symbolsearch.insert(l);
      end;


    procedure TObjLibraryData.getdatalabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createglobal(name,nextlabelnr[alt_data],alt_data);
        inc(nextlabelnr[alt_data]);
        symbolsearch.insert(l);
      end;


    procedure TObjLibraryData.getaddrlabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createlocal(nextlabelnr[alt_addr],alt_addr);
        inc(nextlabelnr[alt_addr]);
        symbolsearch.insert(l);
      end;

end.
