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

  interface

    uses
       globtype,systems,cobjects,globals;

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

{$ifdef newcg}
    const
      SkipInstr = [ait_comment, ait_align, ait_symbol
{$ifdef GDB}
                   ,ait_stabs, ait_stabn, ait_stab_function_name
{$endif GDB}
                   ,ait_regalloc, ait_tempalloc
  ];
{$endif newcg}


  { asm symbol functions }
    type
       TAsmsymtype=(AS_NONE,AS_EXTERNAL,AS_LOCAL,AS_GLOBAL);

       pasmsymbol = ^tasmsymbol;
       tasmsymbol = object(tnamedindexobject)
         orgtyp,
         typ     : TAsmsymtype;
         proclocal : boolean;
         { this need to be incremented with every symbol loading into the
           paasmoutput, thus in loadsym/loadref/const_symbol (PFV) }
         refs    : longint;
         { the next fields are filled in the binary writer }
         idx     : longint;
         section : tsection;
         address,
         size    : longint;
         { alternate symbol which can be used for 'renaming' needed for
           inlining }
         altsymbol : pasmsymbol;
         constructor init(const s:string;_typ:TAsmsymtype);
         procedure reset;
         function  is_used:boolean;
         procedure settyp(t:tasmsymtype);
         procedure setaddress(sec:tsection;offset,len:longint);
         procedure GenerateAltSymbol;
       end;

       pasmlabel = ^tasmlabel;
       tasmlabel = object(tasmsymbol)
         labelnr : longint;
         { this is set by the pai_label.init }
         is_set  : boolean;
         constructor init;
         constructor initdata;
         function name:string;virtual;
       end;


       pasmsymbollist = ^tasmsymbollist;
       tasmsymbollist = object(tdictionary)
       end;

       { the short name makes typing easier }
       pai = ^tai;
       tai = object(tlinkedlist_item)
          typ      : tait;
          { pointer to record with optimizer info about this tai object }
          optinfo  : pointer;
          fileinfo : tfileposinfo;
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
          sym : pasmsymbol;
          is_global : boolean;
          size : longint;
          constructor init(_sym:PAsmSymbol;siz:longint);
          constructor initname(const _name : string;siz:longint);
          constructor initname_global(const _name : string;siz:longint);
       end;

       pai_symbol_end = ^tai_symbol_end;
       tai_symbol_end = object(tai)
          sym : pasmsymbol;
          constructor init(_sym:PAsmSymbol);
          constructor initname(const _name : string);
       end;

       pai_label = ^tai_label;
       tai_label = object(tai)
          l : pasmlabel;
          is_global : boolean;
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

       pai_align_abstract = ^tai_align_abstract;
       tai_align_abstract = object(tai)
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
          sym  : pasmsymbol;
          size : longint;
          is_global : boolean;
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

       TMarker = (NoPropInfoStart, NoPropInfoEnd, AsmBlockStart, AsmBlockEnd);
       pai_marker = ^tai_marker;
       tai_marker = object(tai)
         Kind: TMarker;
         Constructor init(_Kind: TMarker);
       end;

       paitempalloc = ^taitempalloc;
       taitempalloc = object(tai)
          allocation : boolean;
          temppos,
          tempsize   : longint;
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
      debuglist,consts,
      importssection,exportssection,
      resourcesection,rttilist,
      resourcestringlist         : paasmoutput;
    { asm symbol list }
      asmsymbollist : pasmsymbollist;

    const
      nextaltnr   : longint = 1;
      nextlabelnr : longint = 1;
      countlabelref : boolean = true;

    { make l as a new label }
    procedure getlabel(var l : pasmlabel);
    { make l as a new label and flag is_data }
    procedure getdatalabel(var l : pasmlabel);
    {just get a label number }
    procedure getlabelnr(var l : longint);

    function  newasmsymbol(const s : string) : pasmsymbol;
    function  newasmsymboltyp(const s : string;_typ:TAsmSymType) : pasmsymbol;
    function  getasmsymbol(const s : string) : pasmsymbol;
    function  renameasmsymbol(const sold, snew : string):pasmsymbol;

    procedure ResetAsmsymbolList;
    procedure ResetAsmSymbolListAltSymbol;
    procedure CheckAsmSymbolListUndefined;

implementation

uses
  strings,files,verbose;

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
         sym:=newasmsymboltyp(_name,AS_LOCAL);
         size:=_size;
         is_global:=false;
      end;


    constructor tai_datablock.init_global(const _name : string;_size : longint);
      begin
         inherited init;
         typ:=ait_datablock;
         sym:=newasmsymboltyp(_name,AS_GLOBAL);
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
         is_global:=(sym^.typ=AS_GLOBAL);
      end;

    constructor tai_symbol.initname(const _name : string;siz:longint);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=newasmsymboltyp(_name,AS_LOCAL);
         size:=siz;
         is_global:=false;
      end;

    constructor tai_symbol.initname_global(const _name : string;siz:longint);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=newasmsymboltyp(_name,AS_GLOBAL);
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
         sym:=newasmsymboltyp(_name,AS_GLOBAL);
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
        is_global:=(l^.typ=AS_GLOBAL);
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

     constructor tai_align_abstract.init(b: byte);
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


     constructor tai_align_abstract.init_op(b: byte; _op: byte);
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


     function tai_align_abstract.getfillbuf:pchar;
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

    constructor tasmsymbol.init(const s:string;_typ:TAsmsymtype);
      begin;
        inherited initname(s);
        reset;
        typ:=_typ;
      end;

    procedure tasmsymbol.GenerateAltSymbol;
      begin
        if not assigned(altsymbol) then
         begin
           new(altsymbol,init(name+'_'+tostr(nextaltnr),typ));
           { also copy the amount of references }
           altsymbol^.refs:=refs;
           inc(nextaltnr);
         end;
      end;

    procedure tasmsymbol.reset;
      begin
        { save the original typ if not done yet }
        if orgtyp=AS_NONE then
         orgtyp:=typ;
        { reset section info }
        section:=sec_none;
        address:=0;
        size:=0;
        idx:=-1;
        typ:=AS_EXTERNAL;
        proclocal:=false;
        { mainly used to remove unused labels from the codesegment }
        refs:=0;
      end;

    function tasmsymbol.is_used:boolean;
      begin
        is_used:=(refs>0);
      end;

    procedure tasmsymbol.settyp(t:tasmsymtype);
      begin
        typ:=t;
        orgtyp:=t;
      end;

    procedure tasmsymbol.setaddress(sec:tsection;offset,len:longint);
      begin
        section:=sec;
        address:=offset;
        size:=len;
        { when the typ was reset to External, set it back to the original
          type it got when defined }
        if (typ=AS_EXTERNAL) and (orgtyp<>AS_NONE) then
         typ:=orgtyp;
      end;


{*****************************************************************************
                                  AsmLabel
*****************************************************************************}

    constructor tasmlabel.init;
      begin;
        labelnr:=nextlabelnr;
        inc(nextlabelnr);
        inherited init(target_asm.labelprefix+tostr(labelnr),AS_LOCAL);
        proclocal:=true;
        is_set:=false;
      end;


    constructor tasmlabel.initdata;
      begin;
        labelnr:=nextlabelnr;
        inc(nextlabelnr);
        if (cs_create_smart in aktmoduleswitches) then
          inherited init('_$'+current_module^.modulename^+'$_L'+tostr(labelnr),AS_GLOBAL)
        else
          inherited init(target_asm.labelprefix+tostr(labelnr),AS_LOCAL);
        is_set:=false;
        { write it always }
        refs:=1;
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
        if assigned(hp) then
         begin
           newasmsymbol:=hp;
           exit;
         end;
        { Not found, insert it as an External }
        hp:=new(pasmsymbol,init(s,AS_EXTERNAL));
        asmsymbollist^.insert(hp);
        newasmsymbol:=hp;
      end;


    function  newasmsymboltyp(const s : string;_typ:TAsmSymType) : pasmsymbol;
      var
        hp : pasmsymbol;
      begin
        hp:=pasmsymbol(asmsymbollist^.search(s));
        if assigned(hp) then
         begin
           hp^.settyp(_typ);
           newasmsymboltyp:=hp;
           exit;
         end;
        { Not found, insert it as an External }
        hp:=new(pasmsymbol,init(s,_typ));
        asmsymbollist^.insert(hp);
        newasmsymboltyp:=hp;
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


    procedure ResetAsmSym(p:Pnamedindexobject);{$ifndef FPC}far;{$endif}
      begin
        pasmsymbol(p)^.reset;
      end;


    procedure ResetAsmsymbolList;
      begin
        asmsymbollist^.foreach({$ifndef TP}@{$endif}resetasmsym);
      end;


    procedure ResetAltSym(p:Pnamedindexobject);{$ifndef FPC}far;{$endif}
      begin
        pasmsymbol(p)^.altsymbol:=nil;
      end;


    procedure ResetAsmSymbolListAltSymbol;
      begin
        asmsymbollist^.foreach({$ifndef TP}@{$endif}resetaltsym);
      end;


    procedure checkundefinedasmsym(p:Pnamedindexobject);{$ifndef FPC}far;{$endif}
      begin
        if (pasmsymbol(p)^.refs>0) and
           (pasmsymbol(p)^.section=Sec_none) and
           (pasmsymbol(p)^.typ<>AS_EXTERNAL) then
         Message1(asmw_e_undefined_label,pasmsymbol(p)^.name);
      end;

    procedure CheckAsmSymbolListUndefined;
      begin
        asmsymbollist^.foreach({$ifndef TP}@{$endif}checkundefinedasmsym);
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
  Revision 1.71  2000-01-12 10:38:16  peter
    * smartlinking fixes for binary writer
    * release alignreg code and moved instruction writing align to cpuasm,
      but it doesn't use the specified register yet

  Revision 1.70  2000/01/07 01:14:18  peter
    * updated copyright to 2000

  Revision 1.69  1999/12/22 01:01:46  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.68  1999/11/06 14:34:16  peter
    * truncated log to 20 revs

  Revision 1.67  1999/11/05 16:01:45  jonas
    + first implementation of choosing least used register for alignment code
       (not yet working, between ifdef alignreg)

  Revision 1.66  1999/11/02 15:06:56  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.65  1999/10/27 16:11:27  peter
    * insns.dat is used to generate all i386*.inc files

  Revision 1.64  1999/09/20 16:38:51  peter
    * cs_create_smart instead of cs_smartlink
    * -CX is create smartlink
    * -CD is create dynamic, but does nothing atm.

  Revision 1.63  1999/09/16 23:05:51  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.62  1999/09/15 20:35:37  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.61  1999/09/08 15:01:29  jonas
    * some small changes so the noew optimizer is again compilable

  Revision 1.60  1999/08/06 15:30:17  florian
    + cpu flags added, mainly for the new cg

  Revision 1.59  1999/08/05 15:51:01  michael
  * Added ait_frame, ait_ent

  Revision 1.58  1999/08/04 00:39:56  michael
  + Added ait_frame

  Revision 1.57  1999/08/02 21:01:41  michael
  * Moved toperand type back =(

  Revision 1.56  1999/08/02 20:45:47  michael
  * Moved toperand type to aasm

  Revision 1.55  1999/08/01 23:55:55  michael
  * Moved taitempalloc

  Revision 1.54  1999/07/29 20:53:55  peter
    * write .size also

  Revision 1.53  1999/07/22 09:37:28  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.52  1999/07/03 00:26:01  peter
    * ag386bin doesn't destroy the aasmoutput lists anymore

  Revision 1.51  1999/06/02 22:43:57  pierre
   * previous wrong log corrected

  Revision 1.50  1999/06/02 22:25:24  pierre
  * changed $ifdef FPC @ into $ifndef TP

  Revision 1.49  1999/06/01 14:45:41  peter
    * @procvar is now always needed for FPC

  Revision 1.48  1999/05/28 09:11:39  peter
    * also count ref when asmlabel^.name is used

}
