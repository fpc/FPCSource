{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit aasm;

{$i defines.inc}

interface

    uses
       cutils,cobjects,
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

          { the follow is for the DEC Alpha }
          ait_frame,
          ait_ent,
{$ifdef m68k}
          ait_labeled_instruction,
{$endif m68k}
          { never used, makes insertation of new ait_ easier to type }
          { lazy guy !!!! ;-) (FK) }
          ait_dummy);

       tcpuflags = (cf_64bitaddr);
       tcpuflagset = set of tcpuflags;

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

       pasmsymbol = ^tasmsymbol;
       tasmsymbol = object(tnamedindexobject)
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
         { alternate symbol which can be used for 'renaming' needed for
           inlining }
         altsymbol : pasmsymbol;
         { is the symbol local for a procedure/function }
         proclocal : boolean;
         { is the symbol in the used list }
         inusedlist : boolean;
         constructor init(const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
         procedure reset;
         function  is_used:boolean;
         procedure setaddress(sec:tsection;offset,len:longint);
         procedure GenerateAltSymbol;
       end;

       pasmlabel = ^tasmlabel;
       tasmlabel = object(tasmsymbol)
{$ifdef PACKENUMFIXED}
         { this is set by the pai_label.init }
         is_set,
         { is the label only there for getting an address (e.g. for i/o }
         { checks -> true) or is it a jump target (false)               }
         is_addr : boolean;
{$endif}
         labelnr : longint;
{$ifndef PACKENUMFIXED}
         is_set,
         is_addr : boolean;
{$endif}
         constructor init;
         constructor initdata;
         constructor initaddr;
         function name:string;virtual;
       end;


       { the short name makes typing easier }
       pai = ^tai;
       tai = object(tlinkedlist_item)
{$ifndef PACKENUMFIXED}
          typ      : tait;
{$endif}
          { pointer to record with optimizer info about this tai object }
          optinfo  : pointer;
          fileinfo : tfileposinfo;
{$ifdef PACKENUMFIXED}
          { still 3 bytes left after the next field }
          typ      : tait;
{$endif}
          constructor init;
       end;

       pai_string = ^tai_string;
       tai_string = object(tai)
          str : pchar;
          { extra len so the string can contain an \0 }
          len : longint;
          constructor init(const _str : string);
          constructor init_pchar(_str : pchar);
          constructor init_length_pchar(_str : pchar;length : longint);
          destructor done;virtual;
       end;

       { generates a common label }
       pai_symbol = ^tai_symbol;
       tai_symbol = object(tai)
{$ifdef PACKENUMFIXED}
          is_global : boolean;
{$endif}
          sym : pasmsymbol;
          size : longint;
{$ifndef PACKENUMFIXED}
          is_global : boolean;
{$endif}
          constructor init(_sym:PAsmSymbol;siz:longint);
          constructor initname(const _name : string;siz:longint);
          constructor initname_global(const _name : string;siz:longint);
          constructor initdataname(const _name : string;siz:longint);
          constructor initdataname_global(const _name : string;siz:longint);
       end;

       pai_symbol_end = ^tai_symbol_end;
       tai_symbol_end = object(tai)
          sym : pasmsymbol;
          constructor init(_sym:PAsmSymbol);
          constructor initname(const _name : string);
       end;

       pai_label = ^tai_label;
       tai_label = object(tai)
{$ifdef PACKENUMFIXED}
          is_global : boolean;
{$endif}
          l : pasmlabel;
{$ifndef PACKENUMFIXED}
          is_global : boolean;
{$endif}
          constructor init(_l : pasmlabel);
       end;

       pai_direct = ^tai_direct;
       tai_direct = object(tai)
          str : pchar;
          constructor init(_str : pchar);
          destructor done; virtual;
       end;


       { to insert a comment into the generated assembler file }
       pai_asm_comment = ^tai_asm_comment;
       tai_asm_comment = object(tai)
          str : pchar;
          constructor init(_str : pchar);
          destructor done; virtual;
       end;


       { alignment for operator }

{$ifdef i386}
       pai_align_abstract = ^tai_align_abstract;
       tai_align_abstract = object(tai)
{$else i386}
       pai_align = ^tai_align;
       tai_align = object(tai)
{$endif i386}
          buf       : array[0..63] of char; { buf used for fill }
          aligntype : byte;   { 1 = no align, 2 = word align, 4 = dword align }
          fillsize  : byte;   { real size to fill }
          fillop    : byte;   { value to fill with - optional }
          use_op    : boolean;
          constructor init(b:byte);
          constructor init_op(b: byte; _op: byte);
          function getfillbuf:pchar;
       end;

       { Insert a section/segment directive }
       pai_section = ^tai_section;
       tai_section = object(tai)
          sec : tsection;
          constructor init(s : tsection);
       end;


       { generates an uninitializised data block }
       pai_datablock = ^tai_datablock;
       tai_datablock = object(tai)
{$ifdef PACKENUMFIXED}
          is_global : boolean;
{$endif}
          sym  : pasmsymbol;
          size : longint;
{$ifndef PACKENUMFIXED}
          is_global : boolean;
{$endif}
          constructor init(const _name : string;_size : longint);
          constructor init_global(const _name : string;_size : longint);
       end;


       { generates a long integer (32 bit) }
       pai_const = ^tai_const;
       tai_const = object(tai)
          value : longint;
          constructor init_32bit(_value : longint);
          constructor init_16bit(_value : word);
          constructor init_8bit(_value : byte);
       end;

       pai_const_symbol = ^tai_const_symbol;
       tai_const_symbol = object(tai)
          sym    : pasmsymbol;
          offset : longint;
          constructor init(_sym:PAsmSymbol);
          constructor init_offset(_sym:PAsmSymbol;ofs:longint);
          constructor init_rva(_sym:PAsmSymbol);
          constructor initname(const name:string);
          constructor initname_offset(const name:string;ofs:longint);
          constructor initname_rva(const name:string);
       end;

       { generates a single (32 bit real) }
       pai_real_32bit = ^tai_real_32bit;
       tai_real_32bit = object(tai)
          value : ts32real;
          constructor init(_value : ts32real);
       end;

       { generates a double (64 bit real) }
       pai_real_64bit = ^tai_real_64bit;
       tai_real_64bit = object(tai)
          value : ts64real;
          constructor init(_value : ts64real);
       end;

       { generates an extended (80 bit real) }
       pai_real_80bit = ^tai_real_80bit;
       tai_real_80bit = object(tai)
          value : ts80real;
          constructor init(_value : ts80real);
       end;

       { generates an comp (integer over 64 bits) }
       pai_comp_64bit = ^tai_comp_64bit;
       tai_comp_64bit = object(tai)
          value : ts64comp;
          constructor init(_value : ts64comp);
       end;

       { insert a cut to split into several smaller files }

       tcutplace=(cut_normal,cut_begin,cut_end);

       pai_cut = ^tai_cut;
       tai_cut = object(tai)
          place : tcutplace;
          constructor init;
          constructor init_begin;
          constructor init_end;
       end;

       TMarker = (NoPropInfoStart, NoPropInfoEnd,
         AsmBlockStart, AsmBlockEnd,
         InlineStart,InlineEnd);
       pai_marker = ^tai_marker;
       tai_marker = object(tai)
         Kind: TMarker;
         Constructor init(_Kind: TMarker);
       end;

       paitempalloc = ^taitempalloc;
       taitempalloc = object(tai)
{$ifdef PACKENUMFIXED}
          allocation : boolean;
{$endif}
          temppos,
          tempsize   : longint;
{$ifndef PACKENUMFIXED}
          allocation : boolean;
{$endif}
          constructor alloc(pos,size:longint);
          constructor dealloc(pos,size:longint);
       end;

{ for each processor define the best precision }
{ bestreal is defined in globals }
{$ifdef i386}
const
       ait_bestreal = ait_real_80bit;
type
       pai_bestreal = pai_real_80bit;
       tai_bestreal = tai_real_80bit;
{$endif i386}
{$ifdef m68k}
const
       ait_bestreal = ait_real_32bit;
type
       pai_bestreal = pai_real_32bit;
       tai_bestreal = tai_real_32bit;
{$endif m68k}


       paasmoutput = ^taasmoutput;
       taasmoutput = object(tlinkedlist)
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
      resourcestringlist         : paasmoutput;
    { asm symbol list }
      asmsymbollist : pdictionary;
      usedasmsymbollist : psinglelist;

    const
      nextaltnr   : longint = 1;
      nextlabelnr : longint = 1;
      countlabelref : boolean = true;

    { make l as a new label }
    procedure getlabel(var l : pasmlabel);
    { make l as a new label and flag is_addr }
    procedure getaddrlabel(var l : pasmlabel);
    { make l as a new label and flag is_data }
    procedure getdatalabel(var l : pasmlabel);
    {just get a label number }
    procedure getlabelnr(var l : longint);

    function  newasmsymbol(const s : string) : pasmsymbol;
    function  newasmsymboltype(const s : string;_bind:TAsmSymBind;_typ:TAsmsymtype) : pasmsymbol;
    function  getasmsymbol(const s : string) : pasmsymbol;
    function  renameasmsymbol(const sold, snew : string):pasmsymbol;

    procedure InitUsedAsmSymbolList;
    procedure DoneUsedAsmSymbolList;
    procedure UsedAsmSymbolListInsert(p:pasmsymbol);
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

    constructor tai.init;
      begin
        optinfo := nil;
        fileinfo:=aktfilepos;
      end;

{****************************************************************************
                             TAI_SECTION
 ****************************************************************************}

    constructor tai_section.init(s : tsection);
      begin
         inherited init;
         typ:=ait_section;
         sec:=s;
      end;


{****************************************************************************
                             TAI_DATABLOCK
 ****************************************************************************}

    constructor tai_datablock.init(const _name : string;_size : longint);

      begin
         inherited init;
         typ:=ait_datablock;
         sym:=newasmsymboltype(_name,AB_LOCAL,AT_DATA);
         { keep things aligned }
         if _size<=0 then
           _size:=4;
         size:=_size;
         is_global:=false;
      end;


    constructor tai_datablock.init_global(const _name : string;_size : longint);
      begin
         inherited init;
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

    constructor tai_symbol.init(_sym:PAsmSymbol;siz:longint);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=_sym;
         size:=siz;
         is_global:=(sym^.defbind=AB_GLOBAL);
      end;

    constructor tai_symbol.initname(const _name : string;siz:longint);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=newasmsymboltype(_name,AB_LOCAL,AT_FUNCTION);
         size:=siz;
         is_global:=false;
      end;

    constructor tai_symbol.initname_global(const _name : string;siz:longint);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=newasmsymboltype(_name,AB_GLOBAL,AT_FUNCTION);
         size:=siz;
         is_global:=true;
      end;

    constructor tai_symbol.initdataname(const _name : string;siz:longint);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=newasmsymboltype(_name,AB_LOCAL,AT_DATA);
         size:=siz;
         is_global:=false;
      end;

    constructor tai_symbol.initdataname_global(const _name : string;siz:longint);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=newasmsymboltype(_name,AB_GLOBAL,AT_DATA);
         size:=siz;
         is_global:=true;
      end;


{****************************************************************************
                               TAI_SYMBOL
 ****************************************************************************}

    constructor tai_symbol_end.init(_sym:PAsmSymbol);
      begin
         inherited init;
         typ:=ait_symbol_end;
         sym:=_sym;
      end;

    constructor tai_symbol_end.initname(const _name : string);
      begin
         inherited init;
         typ:=ait_symbol_end;
         sym:=newasmsymboltype(_name,AB_GLOBAL,AT_NONE);
      end;


{****************************************************************************
                               TAI_CONST
 ****************************************************************************}

    constructor tai_const.init_32bit(_value : longint);

      begin
         inherited init;
         typ:=ait_const_32bit;
         value:=_value;
      end;

    constructor tai_const.init_16bit(_value : word);

      begin
         inherited init;
         typ:=ait_const_16bit;
         value:=_value;
      end;

    constructor tai_const.init_8bit(_value : byte);

      begin
         inherited init;
         typ:=ait_const_8bit;
         value:=_value;
      end;


{****************************************************************************
                               TAI_CONST_SYMBOL_OFFSET
 ****************************************************************************}

    constructor tai_const_symbol.init(_sym:PAsmSymbol);
      begin
         inherited init;
         typ:=ait_const_symbol;
         sym:=_sym;
         offset:=0;
         { update sym info }
         inc(sym^.refs);
      end;

    constructor tai_const_symbol.init_offset(_sym:PAsmSymbol;ofs:longint);
      begin
         inherited init;
         typ:=ait_const_symbol;
         sym:=_sym;
         offset:=ofs;
         { update sym info }
         inc(sym^.refs);
      end;

    constructor tai_const_symbol.init_rva(_sym:PAsmSymbol);
      begin
         inherited init;
         typ:=ait_const_rva;
         sym:=_sym;
         offset:=0;
         { update sym info }
         inc(sym^.refs);
      end;

    constructor tai_const_symbol.initname(const name:string);
      begin
         inherited init;
         typ:=ait_const_symbol;
         sym:=newasmsymbol(name);
         offset:=0;
         { update sym info }
         inc(sym^.refs);
      end;

    constructor tai_const_symbol.initname_offset(const name:string;ofs:longint);
      begin
         inherited init;
         typ:=ait_const_symbol;
         sym:=newasmsymbol(name);
         offset:=ofs;
         { update sym info }
         inc(sym^.refs);
      end;

    constructor tai_const_symbol.initname_rva(const name:string);
      begin
         inherited init;
         typ:=ait_const_rva;
         sym:=newasmsymbol(name);
         offset:=0;
         { update sym info }
         inc(sym^.refs);
      end;


{****************************************************************************
                               TAI_real_32bit
 ****************************************************************************}

    constructor tai_real_32bit.init(_value : ts32real);

      begin
         inherited init;
         typ:=ait_real_32bit;
         value:=_value;
      end;

{****************************************************************************
                               TAI_real_64bit
 ****************************************************************************}

    constructor tai_real_64bit.init(_value : ts64real);

      begin
         inherited init;
         typ:=ait_real_64bit;
         value:=_value;
      end;

{****************************************************************************
                               TAI_real_80bit
 ****************************************************************************}

    constructor tai_real_80bit.init(_value : ts80real);

      begin
         inherited init;
         typ:=ait_real_80bit;
         value:=_value;
      end;

{****************************************************************************
                               Tai_comp_64bit
 ****************************************************************************}

    constructor tai_comp_64bit.init(_value : ts64comp);

      begin
         inherited init;
         typ:=ait_comp_64bit;
         value:=_value;
      end;


{****************************************************************************
                               TAI_STRING
 ****************************************************************************}

     constructor tai_string.init(const _str : string);

       begin
          inherited init;
          typ:=ait_string;
          getmem(str,length(_str)+1);
          strpcopy(str,_str);
          len:=length(_str);
       end;

     constructor tai_string.init_pchar(_str : pchar);

       begin
          inherited init;
          typ:=ait_string;
          str:=_str;
          len:=strlen(_str);
       end;

    constructor tai_string.init_length_pchar(_str : pchar;length : longint);

       begin
          inherited init;
          typ:=ait_string;
          str:=_str;
          len:=length;
       end;

    destructor tai_string.done;

      begin
         { you can have #0 inside the strings so }
         if str<>nil then
           freemem(str,len+1);
         inherited done;
      end;


{****************************************************************************
                               TAI_LABEL
 ****************************************************************************}

    constructor tai_label.init(_l : pasmlabel);
      begin
        inherited init;
        typ:=ait_label;
        l:=_l;
        l^.is_set:=true;
        is_global:=(l^.defbind=AB_GLOBAL);
      end;


{****************************************************************************
                              TAI_DIRECT
 ****************************************************************************}

     constructor tai_direct.init(_str : pchar);

       begin
          inherited init;
          typ:=ait_direct;
          str:=_str;
       end;

    destructor tai_direct.done;

      begin
         strdispose(str);
         inherited done;
      end;

{****************************************************************************
          TAI_ASM_COMMENT  comment to be inserted in the assembler file
 ****************************************************************************}

     constructor tai_asm_comment.init(_str : pchar);

       begin
          inherited init;
          typ:=ait_comment;
          str:=_str;
       end;

    destructor tai_asm_comment.done;

      begin
         strdispose(str);
         inherited done;
      end;

{****************************************************************************
                              TAI_ALIGN
 ****************************************************************************}

{$ifdef i386}
     constructor tai_align_abstract.init(b: byte);
{$else i386}
     constructor tai_align.init(b: byte);
{$endif i386}
       begin
          inherited init;
          typ:=ait_align;
          if b in [1,2,4,8,16,32] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=0;
          use_op:=false;
       end;


{$ifdef i386}
     constructor tai_align_abstract.init_op(b: byte; _op: byte);
{$else i386}
     constructor tai_align.init_op(b: byte; _op: byte);
{$endif i386}
       begin
          inherited init;
          typ:=ait_align;
          if b in [1,2,4,8,16,32] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=_op;
          use_op:=true;
          fillchar(buf,sizeof(buf),_op)
       end;


{$ifdef i386}
     function tai_align_abstract.getfillbuf:pchar;
{$else i386}
     function tai_align.getfillbuf:pchar;
{$endif i386}
       begin
         getfillbuf:=@buf;
       end;

{****************************************************************************
                              TAI_CUT
 ****************************************************************************}

     constructor tai_cut.init;
       begin
          inherited init;
          typ:=ait_cut;
          place:=cut_normal;
       end;


     constructor tai_cut.init_begin;
       begin
          inherited init;
          typ:=ait_cut;
          place:=cut_begin;
       end;


     constructor tai_cut.init_end;
       begin
          inherited init;
          typ:=ait_cut;
          place:=cut_end;
       end;


{****************************************************************************
                             Tai_Marker
 ****************************************************************************}

     Constructor Tai_Marker.Init(_Kind: TMarker);
     Begin
       Inherited Init;
       typ := ait_marker;
       Kind := _Kind;
     End;

{*****************************************************************************
                                TaiTempAlloc
*****************************************************************************}

    constructor taitempalloc.alloc(pos,size:longint);
      begin
        inherited init;
        typ:=ait_tempalloc;
        allocation:=true;
        temppos:=pos;
        tempsize:=size;
      end;


    constructor taitempalloc.dealloc(pos,size:longint);
      begin
        inherited init;
        typ:=ait_tempalloc;
        allocation:=false;
        temppos:=pos;
        tempsize:=size;
      end;



{*****************************************************************************
                                  AsmSymbol
*****************************************************************************}

    constructor tasmsymbol.init(const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
      begin;
        inherited initname(s);
        reset;
        defbind:=_bind;
        typ:=_typ;
        inusedlist:=false;
        { mainly used to remove unused labels from the codesegment }
        refs:=0;
      end;

    procedure tasmsymbol.GenerateAltSymbol;
      begin
        if not assigned(altsymbol) then
         begin
           new(altsymbol,init(name+'_'+tostr(nextaltnr),bind,typ));
           { also copy the amount of references }
           altsymbol^.refs:=refs;
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
        bind:=AB_EXTERNAL;
        proclocal:=false;
      end;

    function tasmsymbol.is_used:boolean;
      begin
        is_used:=(refs>0);
      end;

    procedure tasmsymbol.setaddress(sec:tsection;offset,len:longint);
      begin
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

    constructor tasmlabel.init;
      begin;
        labelnr:=nextlabelnr;
        inc(nextlabelnr);
        inherited init(target_asm.labelprefix+tostr(labelnr),AB_LOCAL,AT_FUNCTION);
        proclocal:=true;
        is_set:=false;
        is_addr := false;
      end;


    constructor tasmlabel.initdata;
      begin;
        labelnr:=nextlabelnr;
        inc(nextlabelnr);
        if (cs_create_smart in aktmoduleswitches) then
          inherited init('_$'+current_module^.modulename^+'$_L'+tostr(labelnr),AB_GLOBAL,AT_DATA)
        else
          inherited init(target_asm.labelprefix+tostr(labelnr),AB_LOCAL,AT_DATA);
        is_set:=false;
        is_addr := false;
        { write it always }
        refs:=1;
      end;

    constructor tasmlabel.initaddr;
      begin;
        init;
        is_addr := true;
      end;

    function tasmlabel.name:string;
      begin
        name:=inherited name;
        inc(refs);
      end;


{*****************************************************************************
                              AsmSymbolList helpers
*****************************************************************************}

    function newasmsymbol(const s : string) : pasmsymbol;
      var
        hp : pasmsymbol;
      begin
        hp:=pasmsymbol(asmsymbollist^.search(s));
        if not assigned(hp) then
         begin
           { Not found, insert it as an External }
           hp:=new(pasmsymbol,init(s,AB_EXTERNAL,AT_FUNCTION));
           asmsymbollist^.insert(hp);
         end;
        newasmsymbol:=hp;
      end;


    function newasmsymboltype(const s : string;_bind:TAsmSymBind;_typ:Tasmsymtype) : pasmsymbol;
      var
        hp : pasmsymbol;
      begin
        hp:=pasmsymbol(asmsymbollist^.search(s));
        if assigned(hp) then
         hp^.defbind:=_bind
        else
         begin
           { Not found, insert it as an External }
           hp:=new(pasmsymbol,init(s,_bind,_typ));
           asmsymbollist^.insert(hp);
         end;
        newasmsymboltype:=hp;
      end;


    function getasmsymbol(const s : string) : pasmsymbol;
      begin
        getasmsymbol:=pasmsymbol(asmsymbollist^.search(s));
      end;


    { renames an asmsymbol }
    function renameasmsymbol(const sold, snew : string):pasmsymbol;
      begin
        renameasmsymbol:=pasmsymbol(asmsymbollist^.rename(sold,snew));
      end;


{*****************************************************************************
                              Used AsmSymbolList
*****************************************************************************}

    procedure InitUsedAsmSymbolList;
      begin
        if assigned(usedasmsymbollist) then
         internalerror(78455782);
        new(usedasmsymbollist,init);
      end;


    procedure DoneUsedAsmSymbolList;
      begin
        dispose(usedasmsymbollist,done);
        usedasmsymbollist:=nil;
      end;


    procedure UsedAsmSymbolListInsert(p:pasmsymbol);
      begin
        if not p^.inusedlist then
         usedasmsymbollist^.insert(p);
        p^.inusedlist:=true;
      end;


    procedure UsedAsmSymbolListReset;
      var
        hp : pasmsymbol;
      begin
        hp:=pasmsymbol(usedasmsymbollist^.first);
        while assigned(hp) do
         begin
           with hp^ do
            begin
              reset;
              inusedlist:=false;
            end;
           hp:=pasmsymbol(hp^.listnext);
         end;
      end;


    procedure UsedAsmSymbolListResetAltSym;
      var
        hp : pasmsymbol;
      begin
        hp:=pasmsymbol(usedasmsymbollist^.first);
        while assigned(hp) do
         begin
           with hp^ do
            begin
              altsymbol:=nil;
              inusedlist:=false;
            end;
           hp:=pasmsymbol(hp^.listnext);
         end;
      end;


    procedure UsedAsmSymbolListCheckUndefined;
      var
        hp : pasmsymbol;
      begin
        hp:=pasmsymbol(usedasmsymbollist^.first);
        while assigned(hp) do
         begin
           with hp^ do
            begin
              if (refs>0) and
                 (section=Sec_none) and
                 not(bind in [AB_EXTERNAL,AB_COMMON]) then
               Message1(asmw_e_undefined_label,name);
            end;
           hp:=pasmsymbol(hp^.listnext);
         end;
      end;


{*****************************************************************************
                              Label Helpers
*****************************************************************************}

    procedure getlabel(var l : pasmlabel);
      begin
        l:=new(pasmlabel,init);
        asmsymbollist^.insert(l);
      end;


    procedure getdatalabel(var l : pasmlabel);
      begin
        l:=new(pasmlabel,initdata);
        asmsymbollist^.insert(l);
      end;

    procedure getaddrlabel(var l : pasmlabel);
      begin
        l:=new(pasmlabel,initaddr);
        asmsymbollist^.insert(l);
      end;

    procedure RegenerateLabel(var l : pasmlabel);
      begin
        if l^.proclocal then
         getlabel(pasmlabel(l^.altsymbol))
        else
         getdatalabel(pasmlabel(l^.altsymbol));
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
           getlasttaifilepos:=@pai(last)^.fileinfo
         else
           getlasttaifilepos:=nil;
      end;

end.
{
  $Log$
  Revision 1.13  2000-09-24 15:06:10  peter
    * use defines.inc

  Revision 1.12  2000/08/27 20:19:38  peter
    * store strings with case in ppu, when an internal symbol is created
      a '$' is prefixed so it's not automatic uppercased

  Revision 1.11  2000/08/27 16:11:48  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.10  2000/08/20 17:38:21  peter
    * smartlinking fixed for linux (merged)

  Revision 1.9  2000/08/16 18:33:53  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.8  2000/08/12 19:14:58  peter
    * ELF writer works now also with -g
    * ELF writer is default again for linux

  Revision 1.7  2000/08/12 15:34:21  peter
    + usedasmsymbollist to check and reset only the used symbols (merged)

  Revision 1.6  2000/08/09 19:49:44  peter
    * packenumfixed things so it compiles with 1.0.0 again

  Revision 1.5  2000/08/05 13:25:06  peter
    * packenum 1 fixes (merged)

  Revision 1.4  2000/07/21 15:14:01  jonas
    + added is_addr field for labels, if they are only used for getting the address
       (e.g. for io checks) and corresponding getaddrlabel() procedure

  Revision 1.3  2000/07/13 12:08:24  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:28  michael
  + removed logs

}
