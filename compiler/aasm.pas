{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

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
       globtype,systems,cobjects,files,globals;

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
          { never used, makes insertation of new ait_ easier to type }
          ait_dummy);


  { asm symbol functions }
    type
       TAsmsymtype=(AS_EXTERNAL,AS_LOCAL,AS_GLOBAL);

       pasmsymbol = ^tasmsymbol;
       tasmsymbol = object(tnamedindexobject)
         typ     : TAsmsymtype;
         { this need te incremented with every symbol loading into the
           paasmoutput, thus in loadsym/loadref/const_symbol (PFV) }
         refs    : longint;
         { the next fields are filled in the binary writer }
         idx     : longint;
         section : tsection;
         address,
         size    : longint;
         constructor init(const s:string;_typ:TAsmsymtype);
         procedure reset;
         function  is_used:boolean;
         procedure setaddress(sec:tsection;offset,len:longint);
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
          constructor init(_sym:PAsmSymbol);
          constructor initname(const _name : string);
          constructor initname_global(const _name : string);
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
       pai_align = ^tai_align;
       tai_align = object(tai)
          aligntype : byte;   { 1 = no align, 2 = word align, 4 = dword align }
          fillsize  : byte;   { real size to fill }
          fillop    : byte;   { value to fill with - optional }
          use_op    : boolean;
          constructor init(b:byte);
          constructor init_op(b: byte; _op: byte);
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
       pai_cut = ^tai_cut;
       tai_cut = object(tai)
          endname : boolean;
          constructor init;
          constructor init_end;
       end;

       TMarker = (NoPropInfoStart, NoPropInfoEnd, AsmBlockStart, AsmBlockEnd);
       pai_marker = ^tai_marker;
       tai_marker = object(tai)
         Kind: TMarker;
         Constructor init(_Kind: TMarker);
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
      nextlabelnr : longint = 1;
      countlabelref : boolean = true;

    { make l as a new label }
    procedure getlabel(var l : pasmlabel);
    { make l as a new label and flag is_data }
    procedure getdatalabel(var l : pasmlabel);
    { free a label }
    procedure freelabel(var l : pasmlabel);
    {just get a label number }
    procedure getlabelnr(var l : longint);

    function  newasmsymbol(const s : string) : pasmsymbol;
    function  newasmsymboltyp(const s : string;_typ:TAsmSymType) : pasmsymbol;
    function  getasmsymbol(const s : string) : pasmsymbol;
    function  renameasmsymbol(const sold, snew : string):pasmsymbol;

    procedure ResetAsmsymbolList;


implementation

uses
  strings,verbose;

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

    constructor tai_symbol.init(_sym:PAsmSymbol);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=_sym;
      end;

    constructor tai_symbol.initname(const _name : string);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=newasmsymboltyp(_name,AS_LOCAL);
         is_global:=false;
      end;

    constructor tai_symbol.initname_global(const _name : string);
      begin
         inherited init;
         typ:=ait_symbol;
         sym:=newasmsymboltyp(_name,AS_GLOBAL);
         is_global:=true;
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

     constructor tai_align.init(b: byte);

       begin
          inherited init;
          typ:=ait_align;
          if b in [1,2,4,8,16] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=0;
          use_op:=false;
       end;


     constructor tai_align.init_op(b: byte; _op: byte);

       begin
          inherited init;
          typ:=ait_align;
          if b in [1,2,4,8,16] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=_op;
          use_op:=true;
       end;


{****************************************************************************
                              TAI_CUT
 ****************************************************************************}

     constructor tai_cut.init;
       begin
          inherited init;
          typ:=ait_cut;
          endname:=false;
       end;


     constructor tai_cut.init_end;
       begin
          inherited init;
          typ:=ait_cut;
          endname:=true;
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
                                  AsmSymbol
*****************************************************************************}

    constructor tasmsymbol.init(const s:string;_typ:TAsmsymtype);
      begin;
        inherited initname(s);
        reset;
        typ:=_typ;
      end;

    procedure tasmsymbol.reset;
      begin
        section:=sec_none;
        address:=0;
        size:=0;
        idx:=-1;
        typ:=AS_EXTERNAL;
        { mainly used to remove unused labels from the codesegment }
        refs:=0;
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
      end;


{*****************************************************************************
                                  AsmLabel
*****************************************************************************}

    constructor tasmlabel.init;
      begin;
        labelnr:=nextlabelnr;
        inc(nextlabelnr);
        inherited init(target_asm.labelprefix+tostr(labelnr),AS_LOCAL);
        is_set:=false;
      end;


    constructor tasmlabel.initdata;
      begin;
        labelnr:=nextlabelnr;
        inc(nextlabelnr);
        if (cs_smartlink in aktmoduleswitches) then
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
           hp^.typ:=_typ;
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
{$ifdef nodictonaryrename}
      var
        hpold,hpnew : pasmsymbol;
      begin
        hpnew:=pasmsymbol(asmsymbollist^.search(snew));
        if assigned(hpnew) then
          internalerror(405405);
        hpold:=pasmsymbol(asmsymbollist^.search(sold));
        if not assigned(hpold) then
          internalerror(405406);

        hpnew:=new(pasmsymbol,init(sold));
        { replace the old one }
        { WARNING this heavily depends on the
          feature that tdictionnary.insert does not delete
          the tree element that it replaces !! }
        asmsymbollist^.replace_existing:=true;
        asmsymbollist^.insert(hpnew);
        asmsymbollist^.replace_existing:=false;
        { restore the tree }
        hpnew^.left:=hpold^.left;
        hpnew^.right:=hpold^.right;
        stringdispose(hpold^._name);
        hpold^._name:=stringdup(snew);
        hpold^.speedvalue:=getspeedvalue(snew);
        { now reinsert it at right location !! }
        asmsymbollist^.insert(hpold);
        renameasmsymbol:=hpold;
      end;
{$else}
      begin
        renameasmsymbol:=pasmsymbol(asmsymbollist^.rename(sold,snew));
      end;
{$endif}


    procedure ResetAsmSym(p:Pnamedindexobject);{$ifndef FPC}far;{$endif}
      begin
        pasmsymbol(p)^.reset;
      end;


    procedure ResetAsmsymbolList;
      begin
        asmsymbollist^.foreach({$ifndef TP}@{$endif}resetasmsym);
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


    procedure freelabel(var l : pasmlabel);
      begin
        { nothing to do, the dispose of the asmsymbollist will do it }
        l:=nil;
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
  Revision 1.53  1999-07-22 09:37:28  florian
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

  Revision 1.47  1999/05/27 19:43:55  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.46  1999/05/21 13:54:38  peter
    * NEWLAB for label as symbol

  Revision 1.45  1999/05/20 22:18:51  pierre
   * fix from Peter for double bug reported 20/05/1999

  Revision 1.44  1999/05/12 00:19:34  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.43  1999/05/08 20:38:02  jonas
    * seperate OPTimizer INFO pointer field in tai object

  Revision 1.42  1999/05/06 09:05:05  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.41  1999/05/02 22:41:46  peter
    * moved section names to systems
    * fixed nasm,intel writer

  Revision 1.40  1999/04/21 09:43:28  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.39  1999/04/16 11:49:36  peter
    + tempalloc
    + -at to show temp alloc info in .s file

  Revision 1.38  1999/04/14 09:14:44  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.37  1999/03/10 13:25:42  pierre
    section order changed to get closer output from coff writer

  Revision 1.36  1999/03/08 14:51:04  peter
    + smartlinking for ag386bin

  Revision 1.35  1999/03/05 13:09:48  peter
    * first things for tai_cut support for ag386bin

  Revision 1.34  1999/03/03 11:59:27  pierre
   + getasmsymbol to search for existing assembler symbol only

  Revision 1.33  1999/03/02 02:56:08  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.32  1999/03/01 13:31:59  pierre
   * external used before implemented problem fixed

  Revision 1.31  1999/02/25 21:02:16  peter
    * ag386bin updates
    + coff writer

  Revision 1.30  1999/02/17 10:16:24  peter
    * small fixes for the binary writer

  Revision 1.29  1998/12/29 18:48:24  jonas
    + optimize pascal code surrounding assembler blocks

  Revision 1.28  1998/12/16 00:27:16  peter
    * removed some obsolete version checks

  Revision 1.27  1998/12/11 00:02:37  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.26  1998/12/01 23:36:31  pierre
   * zero padded alignment was buggy

  Revision 1.25  1998/11/30 09:42:52  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.24  1998/11/12 11:19:30  pierre
   * fix for first line of function break

  Revision 1.23  1998/10/14 15:56:37  pierre
    * all references to comp suppressed for m68k

  Revision 1.22  1998/10/12 12:20:38  pierre
    + added tai_const_symbol_offset
      for r : pointer = @var.field;
    * better message for different arg names on implementation
      of function

  Revision 1.21  1998/10/08 17:17:07  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.20  1998/10/06 17:16:31  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.19  1998/10/01 20:19:11  jonas
    + ait_marker support

  Revision 1.18  1998/09/20 17:11:25  jonas
    * released REGALLOC

  Revision 1.17  1998/09/07 18:33:31  peter
    + smartlinking for win95 imports

  Revision 1.16  1998/09/03 17:08:37  pierre
    * better lines for stabs
      (no scroll back to if before else part
      no return to case line at jump outside case)
    + source lines also if not in order

  Revision 1.15  1998/08/11 15:31:36  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.14  1998/08/10 23:56:03  peter
    * fixed extended writing

  Revision 1.13  1998/08/10 14:49:33  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.12  1998/07/14 14:46:36  peter
    * released NEWINPUT

  Revision 1.11  1998/07/07 11:19:50  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.10  1998/06/08 22:59:41  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.9  1998/06/04 23:51:26  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.8  1998/05/23 01:20:53  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.7  1998/05/07 00:16:59  peter
    * smartlinking for sets
    + consts labels are now concated/generated in hcodegen
    * moved some cpu code to cga and some none cpu depended code from cga
      to tree and hcodegen and cleanup of hcodegen
    * assembling .. output reduced for smartlinking ;)

  Revision 1.6  1998/05/06 18:36:53  peter
    * tai_section extended with code,data,bss sections and enumerated type
    * ident 'compiled by FPC' moved to pmodules
    * small fix for smartlink

  Revision 1.5  1998/05/01 07:43:52  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.4  1998/04/29 10:33:40  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/27 23:10:27  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.2  1998/04/09 15:46:37  florian
    + register allocation tracing stuff added

}
