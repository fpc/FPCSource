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
{# @abstract(This unit implements an abstract asm output class for all processor types)
  This unit implements an abstract assembler output class for all processors, these
  are then overriden for each assembler writer to actually write the data in these
  classes to an assembler file.
}

unit aasm;

interface

    uses
       cutils,cclasses,
       globtype,globals,systems;

    type
       tait = (
          ait_none,
          ait_direct,
          ait_string,
          ait_label,
          ait_comment,
          ait_instruction,
          ait_datablock,
          ait_symbol,
          ait_symbol_end, { needed to calc the size of a symbol }
          ait_const_32bit,
          ait_const_16bit,
          ait_const_8bit,
          ait_const_symbol,
          ait_real_80bit,
          ait_real_64bit,
          ait_real_32bit,
          ait_comp_64bit,
          ait_align,
          ait_section,
          { the following is only used by the win32 version of the compiler }
          { and only the GNU AS Win32 is able to write it                   }
          ait_const_rva,
          ait_stabn,
          ait_stabs,
          ait_force_line,
          ait_stab_function_name,
          ait_cut, { used to split into tiny assembler files }
          ait_regalloc, { for register,temp allocation debugging }
          ait_tempalloc,
          ait_marker,
{$ifdef alpha}
          { the follow is for the DEC Alpha }
          ait_frame,
          ait_ent,
{$endif alpha}
{$ifdef m68k}
          ait_labeled_instruction,
{$endif m68k}
{$ifdef ia64}
          ait_bundle,
          ait_stop,
{$endif ia64}
{$ifdef SPARC}
          ait_labeled_instruction,
{$endif SPARC}
          { never used, makes insertation of new ait_ easier to type }
          { lazy guy !!!! ;-) (FK) }
          ait_dummy);


{ ait_* types which don't result in executable code or which don't influence   }
{ the way the program runs/behaves, but which may be encountered by the        }
{ optimizer (= if it's sometimes added to the exprasm list). Update if you add }
{ a new ait type!                                                              }
    const
      SkipInstr = [ait_comment, ait_symbol,ait_force_line,ait_section
{$ifdef GDB}
                   ,ait_stabs, ait_stabn, ait_stab_function_name
{$endif GDB}
                   ,ait_regalloc, ait_tempalloc, ait_symbol_end
  ];


  { asm symbol functions }
    type
       TAsmsymbind=(AB_NONE,AB_EXTERNAL,AB_COMMON,AB_LOCAL,AB_GLOBAL);

       TAsmsymtype=(AT_NONE,AT_FUNCTION,AT_DATA,AT_SECTION);

       tasmsymbol = class(TNamedIndexItem)
         defbind,
         bind      : TAsmsymbind;
         typ       : TAsmsymtype;
         { the next fields are filled in the binary writer }
         section : tsection;
         idx     : longint;
         address,
         size    : longint;
         { this need to be incremented with every symbol loading into the
           paasmoutput, thus in loadsym/loadref/const_symbol (PFV) }
         refs    : longint;
         {# Alternate symbol which can be used for 'renaming' needed for
           inlining }
         altsymbol : tasmsymbol;
         {# TRUE if the symbol is local for a procedure/function }
         proclocal : boolean;
         {# Is the symbol in the used list }
         inusedlist : boolean;
         { assembler pass label is set, used for detecting multiple labels }
         pass : byte;
         constructor create(const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
         procedure reset;
         function  is_used:boolean;
         procedure setaddress(_pass:byte;sec:tsection;offset,len:longint);
         procedure GenerateAltSymbol;
       end;

       tasmlabel = class(tasmsymbol)
         { this is set by the tai_label.Init }
         is_set,
         { is the label only there for getting an address (e.g. for i/o }
         { checks -> true) or is it a jump target (false)               }
         is_addr : boolean;
         labelnr : longint;
         constructor create;
         constructor createdata;
         constructor createaddr;
         function getname:string;override;
       end;


       { the short name makes typing easier }
       tai = class(tlinkedlistitem)
          { pointer to record with optimizer info about this tai object }
          optinfo  : pointer;
          fileinfo : tfileposinfo;
          typ      : tait;
          constructor Create;
       end;

       tai_string = class(tai)
          str : pchar;
          { extra len so the string can contain an \0 }
          len : longint;
          constructor Create(const _str : string);
          constructor Create_pchar(_str : pchar);
          constructor Create_length_pchar(_str : pchar;length : longint);
          destructor Destroy;override;
       end;

       { generates a common label }
       tai_symbol = class(tai)
          is_global : boolean;
          sym : tasmsymbol;
          size : longint;
          constructor Create(_sym:tasmsymbol;siz:longint);
          constructor Createname(const _name : string;siz:longint);
          constructor Createname_global(const _name : string;siz:longint);
          constructor Createdataname(const _name : string;siz:longint);
          constructor Createdataname_global(const _name : string;siz:longint);
       end;

       tai_symbol_end = class(tai)
          sym : tasmsymbol;
          constructor Create(_sym:tasmsymbol);
          constructor Createname(const _name : string);
       end;

       tai_label = class(tai)
          is_global : boolean;
          l : tasmlabel;
          constructor Create(_l : tasmlabel);
       end;

       tai_direct = class(tai)
          str : pchar;
          constructor Create(_str : pchar);
          destructor Destroy; override;
       end;

       { to insert a comment into the generated assembler file }
       tai_asm_comment = class(tai)
          str : pchar;
          constructor Create(_str : pchar);
          destructor Destroy; override;
       end;


       { Insert a section/segment directive }
       tai_section = class(tai)
          sec : tsection;
          constructor Create(s : tsection);
       end;


       { generates an uninitializised data block }
       tai_datablock = class(tai)
          is_global : boolean;
          sym  : tasmsymbol;
          size : longint;
          constructor Create(const _name : string;_size : longint);
          constructor Create_global(const _name : string;_size : longint);
       end;


       { generates a long integer (32 bit) }
       tai_const = class(tai)
          value : longint;
          constructor Create_32bit(_value : longint);
          constructor Create_16bit(_value : word);
          constructor Create_8bit(_value : byte);
       end;

       tai_const_symbol = class(tai)
          sym    : tasmsymbol;
          offset : longint;
          constructor Create(_sym:tasmsymbol);
          constructor Create_offset(_sym:tasmsymbol;ofs:longint);
          constructor Create_rva(_sym:tasmsymbol);
          constructor Createname(const name:string);
          constructor Createname_offset(const name:string;ofs:longint);
          constructor Createname_rva(const name:string);
       end;

       { generates a single (32 bit real) }
       tai_real_32bit = class(tai)
          value : ts32real;
          constructor Create(_value : ts32real);
       end;

       { generates a double (64 bit real) }
       tai_real_64bit = class(tai)
          value : ts64real;
          constructor Create(_value : ts64real);
       end;

       { generates an extended (80 bit real) }
       tai_real_80bit = class(tai)
          value : ts80real;
          constructor Create(_value : ts80real);
       end;

       { generates an comp (integer over 64 bits) }
       tai_comp_64bit = class(tai)
          value : ts64comp;
          constructor Create(_value : ts64comp);
       end;

       { insert a cut to split into several smaller files }

       tcutplace=(cut_normal,cut_begin,cut_end);

       tai_cut = class(tai)
          place : tcutplace;
          constructor Create;
          constructor Create_begin;
          constructor Create_end;
       end;

       TMarker = (NoPropInfoStart, NoPropInfoEnd,
         AsmBlockStart, AsmBlockEnd,
         InlineStart,InlineEnd
       );

       tai_marker = class(tai)
         Kind: TMarker;
         Constructor Create(_Kind: TMarker);
       end;

       taitempalloc = class(tai)
          allocation : boolean;
          temppos,
          tempsize   : longint;
          constructor alloc(pos,size:longint);
          constructor dealloc(pos,size:longint);
       end;

{ for each processor define the best precision }
{ bestreal is defined in globals }
{$ifdef x86}
const
       ait_bestreal = ait_real_80bit;
type
       tai_bestreal = tai_real_80bit;
{$endif x86}
{$ifdef m68k}
const
       ait_bestreal = ait_real_32bit;
type
       tai_bestreal = tai_real_32bit;
{$endif m68k}

       taasmoutput = class(tlinkedlist)
         function getlasttaifilepos : pfileposinfo;
       end;

    const
    { maximum of aasmoutput lists there will be }
      maxoutputlists = 10;

    var
    { temporary lists }
      exprasmlist,
    { default lists }
      datasegment,codesegment,bsssegment,
      debuglist,withdebuglist,consts,
      importssection,exportssection,
      resourcesection,rttilist,
      resourcestringlist         : taasmoutput;
    { asm symbol list }
      asmsymbollist : tdictionary;
      usedasmsymbollist : tsinglelist;

    const
      nextaltnr   : longint = 1;
      nextlabelnr : longint = 1;
      countlabelref : boolean = true;

    {# create a new assembler label }
    procedure getlabel(var l : tasmlabel);
    { make l as a new label and flag is_addr }
    procedure getaddrlabel(var l : tasmlabel);
    { make l as a new label and flag is_data }
    procedure getdatalabel(var l : tasmlabel);
    {# return a label number }
    procedure getlabelnr(var l : longint);

    function  newasmsymbol(const s : string) : tasmsymbol;
    function  newasmsymboltype(const s : string;_bind:TAsmSymBind;_typ:TAsmsymtype) : tasmsymbol;
    function  getasmsymbol(const s : string) : tasmsymbol;
    function  renameasmsymbol(const sold, snew : string):tasmsymbol;

    procedure CreateUsedAsmSymbolList;
    procedure DestroyUsedAsmSymbolList;
    procedure UsedAsmSymbolListInsert(p:tasmsymbol);
    procedure UsedAsmSymbolListReset;
    procedure UsedAsmSymbolListResetAltSym;
    procedure UsedAsmSymbolListCheckUndefined;


implementation

uses
{$ifdef delphi}
  sysutils,
{$else}
  strings,
{$endif}
  fmodule,verbose;

{****************************************************************************
                             TAI
 ****************************************************************************}

    constructor tai.Create;
      begin
        optinfo := nil;
        fileinfo:=aktfilepos;
      end;

{****************************************************************************
                             TAI_SECTION
 ****************************************************************************}

    constructor tai_section.Create(s : tsection);
      begin
         inherited Create;
         typ:=ait_section;
         sec:=s;
      end;


{****************************************************************************
                             TAI_DATABLOCK
 ****************************************************************************}

    constructor tai_datablock.Create(const _name : string;_size : longint);

      begin
         inherited Create;
         typ:=ait_datablock;
         sym:=newasmsymboltype(_name,AB_LOCAL,AT_DATA);
         { keep things aligned }
         if _size<=0 then
           _size:=4;
         size:=_size;
         is_global:=false;
      end;


    constructor tai_datablock.Create_global(const _name : string;_size : longint);
      begin
         inherited Create;
         typ:=ait_datablock;
         sym:=newasmsymboltype(_name,AB_GLOBAL,AT_DATA);
         { keep things aligned }
         if _size<=0 then
           _size:=4;
         size:=_size;
         is_global:=true;
      end;


{****************************************************************************
                               TAI_SYMBOL
 ****************************************************************************}

    constructor tai_symbol.Create(_sym:tasmsymbol;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=_sym;
         size:=siz;
         is_global:=(sym.defbind=AB_GLOBAL);
      end;

    constructor tai_symbol.Createname(const _name : string;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=newasmsymboltype(_name,AB_LOCAL,AT_FUNCTION);
         size:=siz;
         is_global:=false;
      end;

    constructor tai_symbol.Createname_global(const _name : string;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=newasmsymboltype(_name,AB_GLOBAL,AT_FUNCTION);
         size:=siz;
         is_global:=true;
      end;

    constructor tai_symbol.Createdataname(const _name : string;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=newasmsymboltype(_name,AB_LOCAL,AT_DATA);
         size:=siz;
         is_global:=false;
      end;

    constructor tai_symbol.Createdataname_global(const _name : string;siz:longint);
      begin
         inherited Create;
         typ:=ait_symbol;
         sym:=newasmsymboltype(_name,AB_GLOBAL,AT_DATA);
         size:=siz;
         is_global:=true;
      end;


{****************************************************************************
                               TAI_SYMBOL
 ****************************************************************************}

    constructor tai_symbol_end.Create(_sym:tasmsymbol);
      begin
         inherited Create;
         typ:=ait_symbol_end;
         sym:=_sym;
      end;

    constructor tai_symbol_end.Createname(const _name : string);
      begin
         inherited Create;
         typ:=ait_symbol_end;
         sym:=newasmsymboltype(_name,AB_GLOBAL,AT_NONE);
      end;


{****************************************************************************
                               TAI_CONST
 ****************************************************************************}

    constructor tai_const.Create_32bit(_value : longint);

      begin
         inherited Create;
         typ:=ait_const_32bit;
         value:=_value;
      end;

    constructor tai_const.Create_16bit(_value : word);

      begin
         inherited Create;
         typ:=ait_const_16bit;
         value:=_value;
      end;

    constructor tai_const.Create_8bit(_value : byte);

      begin
         inherited Create;
         typ:=ait_const_8bit;
         value:=_value;
      end;


{****************************************************************************
                               TAI_CONST_SYMBOL_OFFSET
 ****************************************************************************}

    constructor tai_const_symbol.Create(_sym:tasmsymbol);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=_sym;
         offset:=0;
         { update sym info }
         inc(sym.refs);
      end;

    constructor tai_const_symbol.Create_offset(_sym:tasmsymbol;ofs:longint);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=_sym;
         offset:=ofs;
         { update sym info }
         inc(sym.refs);
      end;

    constructor tai_const_symbol.Create_rva(_sym:tasmsymbol);
      begin
         inherited Create;
         typ:=ait_const_rva;
         sym:=_sym;
         offset:=0;
         { update sym info }
         inc(sym.refs);
      end;

    constructor tai_const_symbol.Createname(const name:string);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=newasmsymbol(name);
         offset:=0;
         { update sym info }
         inc(sym.refs);
      end;

    constructor tai_const_symbol.Createname_offset(const name:string;ofs:longint);
      begin
         inherited Create;
         typ:=ait_const_symbol;
         sym:=newasmsymbol(name);
         offset:=ofs;
         { update sym info }
         inc(sym.refs);
      end;

    constructor tai_const_symbol.Createname_rva(const name:string);
      begin
         inherited Create;
         typ:=ait_const_rva;
         sym:=newasmsymbol(name);
         offset:=0;
         { update sym info }
         inc(sym.refs);
      end;


{****************************************************************************
                               TAI_real_32bit
 ****************************************************************************}

    constructor tai_real_32bit.Create(_value : ts32real);

      begin
         inherited Create;
         typ:=ait_real_32bit;
         value:=_value;
      end;

{****************************************************************************
                               TAI_real_64bit
 ****************************************************************************}

    constructor tai_real_64bit.Create(_value : ts64real);

      begin
         inherited Create;
         typ:=ait_real_64bit;
         value:=_value;
      end;

{****************************************************************************
                               TAI_real_80bit
 ****************************************************************************}

    constructor tai_real_80bit.Create(_value : ts80real);

      begin
         inherited Create;
         typ:=ait_real_80bit;
         value:=_value;
      end;

{****************************************************************************
                               Tai_comp_64bit
 ****************************************************************************}

    constructor tai_comp_64bit.Create(_value : ts64comp);

      begin
         inherited Create;
         typ:=ait_comp_64bit;
         value:=_value;
      end;


{****************************************************************************
                               TAI_STRING
 ****************************************************************************}

     constructor tai_string.Create(const _str : string);

       begin
          inherited Create;
          typ:=ait_string;
          getmem(str,length(_str)+1);
          strpcopy(str,_str);
          len:=length(_str);
       end;

     constructor tai_string.Create_pchar(_str : pchar);

       begin
          inherited Create;
          typ:=ait_string;
          str:=_str;
          len:=strlen(_str);
       end;

    constructor tai_string.Create_length_pchar(_str : pchar;length : longint);

       begin
          inherited Create;
          typ:=ait_string;
          str:=_str;
          len:=length;
       end;

    destructor tai_string.destroy;

      begin
         { you can have #0 inside the strings so }
         if str<>nil then
           freemem(str,len+1);
         inherited Destroy;
      end;


{****************************************************************************
                               TAI_LABEL
 ****************************************************************************}

    constructor tai_label.create(_l : tasmlabel);
      begin
        inherited Create;
        typ:=ait_label;
        l:=_l;
        l.is_set:=true;
        is_global:=(l.defbind=AB_GLOBAL);
      end;


{****************************************************************************
                              TAI_DIRECT
 ****************************************************************************}

     constructor tai_direct.Create(_str : pchar);

       begin
          inherited Create;
          typ:=ait_direct;
          str:=_str;
       end;

    destructor tai_direct.destroy;

      begin
         strdispose(str);
         inherited Destroy;
      end;

{****************************************************************************
          TAI_ASM_COMMENT  comment to be inserted in the assembler file
 ****************************************************************************}

     constructor tai_asm_comment.Create(_str : pchar);

       begin
          inherited Create;
          typ:=ait_comment;
          str:=_str;
       end;

    destructor tai_asm_comment.destroy;

      begin
         strdispose(str);
         inherited Destroy;
      end;

{****************************************************************************
                              TAI_CUT
 ****************************************************************************}

     constructor tai_cut.Create;
       begin
          inherited Create;
          typ:=ait_cut;
          place:=cut_normal;
       end;


     constructor tai_cut.Create_begin;
       begin
          inherited Create;
          typ:=ait_cut;
          place:=cut_begin;
       end;


     constructor tai_cut.Create_end;
       begin
          inherited Create;
          typ:=ait_cut;
          place:=cut_end;
       end;


{****************************************************************************
                             Tai_Marker
 ****************************************************************************}

     Constructor Tai_Marker.Create(_Kind: TMarker);
     Begin
       Inherited Create;
       typ := ait_marker;
       Kind := _Kind;
     End;

{*****************************************************************************
                                TaiTempAlloc
*****************************************************************************}

    constructor taitempalloc.alloc(pos,size:longint);
      begin
        inherited Create;
        typ:=ait_tempalloc;
        allocation:=true;
        temppos:=pos;
        tempsize:=size;
      end;


    constructor taitempalloc.dealloc(pos,size:longint);
      begin
        inherited Create;
        typ:=ait_tempalloc;
        allocation:=false;
        temppos:=pos;
        tempsize:=size;
      end;



{*****************************************************************************
                                  AsmSymbol
*****************************************************************************}

    constructor tasmsymbol.create(const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
      begin;
        inherited createname(s);
        reset;
        defbind:=_bind;
        typ:=_typ;
        inusedlist:=false;
        pass:=255;
        { mainly used to remove unused labels from the codesegment }
        refs:=0;
      end;

    procedure tasmsymbol.GenerateAltSymbol;
      begin
        if not assigned(altsymbol) then
         begin
           altsymbol:=tasmsymbol.create(name+'_'+tostr(nextaltnr),defbind,typ);
           { also copy the amount of references }
           altsymbol.refs:=refs;
           inc(nextaltnr);
         end;
      end;

    procedure tasmsymbol.reset;
      begin
        { reset section info }
        section:=sec_none;
        address:=0;
        size:=0;
        idx:=-1;
        pass:=255;
        bind:=AB_EXTERNAL;
        proclocal:=false;
      end;

    function tasmsymbol.is_used:boolean;
      begin
        is_used:=(refs>0);
      end;

    procedure tasmsymbol.setaddress(_pass:byte;sec:tsection;offset,len:longint);
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
        if (bind=AB_EXTERNAL) and (defbind<>AB_NONE) then
         bind:=defbind;
      end;


{*****************************************************************************
                                  AsmLabel
*****************************************************************************}

    constructor tasmlabel.create;
      begin;
        labelnr:=nextlabelnr;
        inc(nextlabelnr);
        inherited create(target_asm.labelprefix+tostr(labelnr),AB_LOCAL,AT_FUNCTION);
        proclocal:=true;
        is_set:=false;
        is_addr := false;
      end;


    constructor tasmlabel.createdata;
      begin;
        labelnr:=nextlabelnr;
        inc(nextlabelnr);
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

    constructor tasmlabel.createaddr;
      begin;
        create;
        is_addr := true;
      end;

    function tasmlabel.getname:string;
      begin
        getname:=inherited getname;
        inc(refs);
      end;


{*****************************************************************************
                              AsmSymbolList helpers
*****************************************************************************}

    function newasmsymbol(const s : string) : tasmsymbol;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(asmsymbollist.search(s));
        if not assigned(hp) then
         begin
           { Not found, insert it as an External }
           hp:=tasmsymbol.create(s,AB_EXTERNAL,AT_FUNCTION);
           asmsymbollist.insert(hp);
         end;
        newasmsymbol:=hp;
      end;


    function newasmsymboltype(const s : string;_bind:TAsmSymBind;_typ:Tasmsymtype) : tasmsymbol;
      var
        hp : tasmsymbol;
      begin
        hp:=tasmsymbol(asmsymbollist.search(s));
        if assigned(hp) then
         hp.defbind:=_bind
        else
         begin
           { Not found, insert it as an External }
           hp:=tasmsymbol.create(s,_bind,_typ);
           asmsymbollist.insert(hp);
         end;
        newasmsymboltype:=hp;
      end;


    function getasmsymbol(const s : string) : tasmsymbol;
      begin
        getasmsymbol:=tasmsymbol(asmsymbollist.search(s));
      end;


    { renames an asmsymbol }
    function renameasmsymbol(const sold, snew : string):tasmsymbol;
      begin
        renameasmsymbol:=tasmsymbol(asmsymbollist.rename(sold,snew));
      end;


{*****************************************************************************
                              Used AsmSymbolList
*****************************************************************************}

    procedure CreateUsedAsmSymbolList;
      begin
        if assigned(usedasmsymbollist) then
         internalerror(78455782);
        usedasmsymbollist:=TSingleList.create;
      end;


    procedure DestroyUsedAsmSymbolList;
      begin
        usedasmsymbollist.destroy;
        usedasmsymbollist:=nil;
      end;


    procedure UsedAsmSymbolListInsert(p:tasmsymbol);
      begin
        if not p.inusedlist then
         usedasmsymbollist.insert(p);
        p.inusedlist:=true;
      end;


    procedure UsedAsmSymbolListReset;
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


    procedure UsedAsmSymbolListResetAltSym;
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


    procedure UsedAsmSymbolListCheckUndefined;
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
                 not(bind in [AB_EXTERNAL,AB_COMMON]) then
               Message1(asmw_e_undefined_label,name);
            end;
           hp:=tasmsymbol(hp.listnext);
         end;
      end;


{*****************************************************************************
                              Label Helpers
*****************************************************************************}

    procedure getlabel(var l : tasmlabel);
      begin
        l:=tasmlabel.create;
        asmsymbollist.insert(l);
      end;


    procedure getdatalabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createdata;
        asmsymbollist.insert(l);
      end;

    procedure getaddrlabel(var l : tasmlabel);
      begin
        l:=tasmlabel.createaddr;
        asmsymbollist.insert(l);
      end;

    procedure getlabelnr(var l : longint);
      begin
         l:=nextlabelnr;
         inc(nextlabelnr);
      end;


{*****************************************************************************
                                 TAAsmOutput
*****************************************************************************}

    function taasmoutput.getlasttaifilepos : pfileposinfo;
      begin
         if assigned(last) then
           getlasttaifilepos:=@tai(last).fileinfo
         else
           getlasttaifilepos:=nil;
      end;

end.
{
  $Log$
  Revision 1.29  2002-07-04 20:43:00  florian
    * first x86-64 patches

  Revision 1.28  2002/07/01 18:46:20  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.27  2002/05/18 13:34:04  peter
    * readded missing revisions

  Revision 1.25  2002/05/14 19:34:38  peter
    * removed old logs and updated copyright year

  Revision 1.24  2002/05/14 17:28:08  peter
    * synchronized cpubase between powerpc and i386
    * moved more tables from cpubase to cpuasm
    * tai_align_abstract moved to tainst, cpuasm must define
      the tai_align class now, which may be empty

  Revision 1.23  2002/04/15 18:54:34  carl
  - removed tcpuflags

  Revision 1.22  2002/04/07 13:18:19  carl
  + more documentation

  Revision 1.21  2002/04/07 10:17:40  carl
  - remove packenumfixed (requires version 1.0.2 or later to compile now!)
  + changing some comments so its commented automatically

  Revision 1.20  2002/03/24 19:04:31  carl
  + patch for SPARC from Mazen NEIFER

}