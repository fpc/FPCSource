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
       cobjects,files,globals;

    type
{$ifdef i386}
     {$ifndef VER0_99_5}
       bestreal = extended;
     {$else}
       bestreal = double;
     {$endif}
{$endif}
{$ifdef m68k}
       bestreal = real;
{$endif}
       pbestreal=^bestreal;

       tait = (
          ait_string,
          ait_label,
          ait_direct,
          ait_labeled_instruction,
          ait_comment,
          ait_instruction,
          ait_datablock,
          ait_symbol,
          ait_const_32bit,
          ait_const_16bit,
          ait_const_8bit,
          ait_const_symbol,
          ait_const_symbol_offset,
          ait_real_64bit,
          ait_real_32bit,
          ait_real_extended,
          ait_comp,
          ait_external,
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
          ait_regalloc,
          ait_regdealloc,
          ait_marker,
          { never used, makes insertation of new ait_ easier to type }
          ait_dummy);

     type
       { the short name makes typing easier }
       pai = ^tai;
       tai = object(tlinkedlist_item)
          typ      : tait;
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
          name : pchar;
          is_global : boolean;
          constructor init(const _name : string);
          constructor init_global(const _name : string);
          destructor done;virtual;
       end;

       { external types defined for TASM }
       { EXT_ANY for search purposes     }
       texternal_typ = (EXT_ANY,EXT_NEAR, EXT_FAR, EXT_PROC, EXT_BYTE,
                        EXT_WORD, EXT_DWORD, EXT_CODEPTR, EXT_DATAPTR,
                        EXT_FWORD, EXT_PWORD, EXT_QWORD, EXT_TBYTE, EXT_ABS);

       { generates an symbol which is marked as external }
       pai_external = ^tai_external;
       tai_external = object(tai)
          name : pchar;
          exttyp : texternal_typ;
          constructor init(const _name : string;exttype : texternal_typ);
          destructor done; virtual;
       end;

     { type for a temporary label test if used for dispose of
       unnecessary labels }
       plabel = ^tlabel;
       tlabel = record
                  nb       : longint;
                  is_data  : boolean;
                  is_used  : boolean;
                  is_set   : boolean;
                  refcount : word;
                end;

       pai_label = ^tai_label;
       tai_label = object(tai)
          l : plabel;
          constructor init(_l : plabel);
          destructor done; virtual;
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
          aligntype: byte;   { 1 = no align, 2 = word align, 4 = dword align }
          op: byte;          { value to fill with - optional                 }
          constructor init(b:byte);
          constructor init_op(b: byte; use_op: byte);
          destructor done;virtual;
       end;


       tsection=(sec_none,sec_code,sec_data,sec_bss,sec_idata);

       { Insert a section/segment directive }
       pai_section = ^tai_section;
       tai_section = object(tai)
          sec      : tsection;
          idataidx : longint;
          constructor init(s : tsection);
          constructor init_idata(i:longint);
          destructor done;virtual;
       end;


       { generates an uninitializised data block }
       pai_datablock = ^tai_datablock;
       tai_datablock = object(tai)
          size : longint;
          name : pchar;
          is_global : boolean;
          constructor init(const _name : string;_size : longint);
          constructor init_global(const _name : string;_size : longint);
          destructor done; virtual;
       end;


       { generates a long integer (32 bit) }
       pai_const = ^tai_const;
       tai_const = object(tai)
          value : longint;
          constructor init_32bit(_value : longint);
          constructor init_16bit(_value : word);
          constructor init_8bit(_value : byte);
          constructor init_symbol(p : pchar);
          constructor init_rva(p : pchar);
          destructor done;virtual;
       end;

       pai_const_symbol_offset = ^tai_const_symbol_offset;
       tai_const_symbol_offset= object(tai)
          name : pchar;
          offset : longint;
          constructor init(p : pchar; l : longint);
          destructor done;virtual;
       end;

       { generates a double (64 bit real) }
       pai_double = ^tai_double;
       tai_double = object(tai)
          value : double;
          constructor init(_value : double);
       end;


       { generates an comp (integer over 64 bits) }
       pai_comp = ^tai_comp;
       tai_comp = object(tai)
          value : bestreal;
          constructor init(_value : bestreal);
{$ifdef i386}
          { usefull for 64 bits apps, maybe later  }
          { comp is not defined on m68k processors !! }
          constructor init_comp(_value : comp);
{$endif i386}
       end;


       { generates a single (32 bit real) }
       pai_single = ^tai_single;
       tai_single = object(tai)
          value : single;
          constructor init(_value : single);
       end;


       { generates an extended (80 bit real) }
       pai_extended = ^tai_extended;
       tai_extended = object(tai)
          value : bestreal;
          constructor init(_value : bestreal);
       end;


       { insert a cut to split into several smaller files }
       pai_cut = ^tai_cut;
       tai_cut = object(tai)
          endname : boolean;
          constructor init;
          constructor init_end;
       end;

       TMarker = (NoPropInfoStart, NoPropInfoEnd);
       pai_marker = ^tai_marker;
       tai_marker = object(tai)
         Kind: TMarker;
         Constructor init(_Kind: TMarker);
       end;


{ for each processor define the best precision }
{ bestreal is defined in globals }
{$ifdef i386}
const
       ait_bestreal = ait_real_extended;
type
       pai_bestreal = pai_extended;
       tai_bestreal = tai_extended;
{$endif i386}
{$ifdef m68k}
const
       ait_bestreal = ait_real_32bit;
type
       pai_bestreal = pai_single;
       tai_bestreal = tai_single;
{$endif m68k}


       paasmoutput = ^taasmoutput;
       taasmoutput = object(tlinkedlist)
                   function getlasttaifilepos : pfileposinfo;
                   end;

    var
    { temporary lists }
      exprasmlist,
    { default lists }

      datasegment,codesegment,bsssegment,
      internals,externals,debuglist,consts,
      importssection,exportssection,
      resourcesection,rttilist         : paasmoutput;

  { external symbols without repetition }
    function search_assembler_symbol(pl : paasmoutput;const _name : string;exttype : texternal_typ) : pai_external;
    procedure concat_external(const _name : string;exttype : texternal_typ);
    procedure concat_internal(const _name : string;exttype : texternal_typ);

  { label functions }
    const
      nextlabelnr : longint = 1;
      countlabelref : boolean = true;
    { convert label to string}
    function lab2str(l : plabel) : string;
    { make l as a new label }
    procedure getlabel(var l : plabel);
    { make l as a new label and flag is_data }
    procedure getdatalabel(var l : plabel);
    { frees the label if unused }
    procedure freelabel(var l : plabel);
    { make a new zero label }
    procedure getzerolabel(var l : plabel);
    { reset a label to a zero label }
    procedure setzerolabel(var l : plabel);
    {just get a label number }
    procedure getlabelnr(var l : longint);


implementation

uses
  strings,verbose,systems;

{****************************************************************************
                             TAI
 ****************************************************************************}

    constructor tai.init;
      begin
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
         idataidx:=0;
      end;

    constructor tai_section.init_idata(i:longint);

      begin
         inherited init;
         typ:=ait_section;
         sec:=sec_idata;
         idataidx:=i;
      end;

    destructor tai_section.done;

      begin
         inherited done;
      end;

{****************************************************************************
                             TAI_DATABLOCK
 ****************************************************************************}

    constructor tai_datablock.init(const _name : string;_size : longint);

      begin
         inherited init;
         typ:=ait_datablock;
         name:=strpnew(_name);
         concat_internal(_name,EXT_ANY);
         size:=_size;
         is_global:=false;
      end;

    constructor tai_datablock.init_global(const _name : string;_size : longint);

      begin
         inherited init;
         typ:=ait_datablock;
         name:=strpnew(_name);
         concat_internal(_name,EXT_ANY);
         size:=_size;
         is_global:=true;
      end;

    destructor tai_datablock.done;

      begin
         strdispose(name);
         inherited done;
      end;

{****************************************************************************
                               TAI_SYMBOL
 ****************************************************************************}

    constructor tai_symbol.init(const _name : string);

      begin
         inherited init;
         typ:=ait_symbol;
         name:=strpnew(_name);
         concat_internal(_name,EXT_ANY);
         is_global:=false;
      end;

    constructor tai_symbol.init_global(const _name : string);

      begin
         inherited init;
         typ:=ait_symbol;
         name:=strpnew(_name);
         concat_internal(_name,EXT_ANY);
         is_global:=true;
      end;

    destructor tai_symbol.done;

      begin
         strdispose(name);
         inherited done;
      end;

{****************************************************************************
                               TAI_EXTERNAL
 ****************************************************************************}

    constructor tai_external.init(const _name : string;exttype : texternal_typ);

      begin
         inherited init;
         typ:=ait_external;
         exttyp:=exttype;
         name:=strpnew(_name);
      end;

    destructor tai_external.done;

      begin
         strdispose(name);
         inherited done;
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

    constructor tai_const.init_symbol(p : pchar);

      begin
         inherited init;
         typ:=ait_const_symbol;
         value:=longint(p);
      end;

    constructor tai_const.init_rva(p : pchar);

      begin
         inherited init;
         typ:=ait_const_rva;
         value:=longint(p);
      end;

    destructor tai_const.done;

      begin
         if typ=ait_const_symbol then
           strdispose(pchar(value));
         inherited done;
      end;


{****************************************************************************
                               TAI_CONST_SYMBOL_OFFSET
 ****************************************************************************}

    constructor tai_const_symbol_offset.init(p : pchar; l : longint);

      begin
         inherited init;
         typ:=ait_const_symbol_offset;
         name:=p;
         offset:=l;
      end;

    destructor tai_const_symbol_offset.done;

      begin
         strdispose(name);
         inherited done;
      end;

{****************************************************************************
                               TAI_DOUBLE
 ****************************************************************************}

    constructor tai_double.init(_value : double);

      begin
         inherited init;
         typ:=ait_real_64bit;
         value:=_value;
      end;

{****************************************************************************
                               TAI_SINGLE
 ****************************************************************************}

    constructor tai_single.init(_value : single);

      begin
         inherited init;
         typ:=ait_real_32bit;
         value:=_value;
      end;

{****************************************************************************
                               TAI_EXTENDED
 ****************************************************************************}

    constructor tai_extended.init(_value : bestreal);

      begin
         inherited init;
         typ:=ait_real_extended;
         value:=_value;
      end;

{****************************************************************************
                               TAI_COMP
 ****************************************************************************}

    constructor tai_comp.init(_value : bestreal);

      begin
         inherited init;
         typ:=ait_comp;
         value:=_value;
      end;

{$ifdef i386}
    constructor tai_comp.init_comp(_value : comp);

      begin
         inherited init;
         typ:=ait_comp;
         value:=_value;
      end;
{$endif i386}

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

     constructor tai_label.init(_l : plabel);

       begin
          inherited init;
          typ:=ait_label;
          l:=_l;
          l^.is_set:=true;
       end;

    destructor tai_label.done;

      begin
         if (l^.refcount>0) then
         { can now be disposed by a tai_labeled instruction !! }
           l^.is_set:=false
         else
           dispose(l);
         inherited done;
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
          op:=0;
       end;


     constructor tai_align.init_op(b: byte; use_op: byte);

       begin
          inherited init;
          typ:=ait_align;
          if b in [1,2,4,8,16] then
           aligntype := b
          else
           aligntype := 1;
           op:=use_op;
       end;

    destructor tai_align.done;

      begin
         inherited done;
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
                           External Helpers
*****************************************************************************}

    function search_assembler_symbol(pl : paasmoutput;const _name : string;exttype : texternal_typ) : pai_external;
      var
         p : pai;
      begin
         search_assembler_symbol:=nil;
         if pl=nil then
           internalerror(2001)
         else
           begin
              p:=pai(pl^.first);
              while (p<>nil) and
                    (p<>pai(pl^.last)) do
                { if we get the same name with a different typ }
                { there is probably an error                   }
                if (p^.typ=ait_external) and
                   ((exttype=EXT_ANY) or (pai_external(p)^.exttyp=exttype)) and
                   (strpas(pai_external(p)^.name)=_name) then
                  begin
                     search_assembler_symbol:=pai_external(p);
                     exit;
                  end
                else
                  p:=pai(p^.next);
              if (p<>nil) and
                 (p^.typ=ait_external) and
                 (pai_external(p)^.exttyp=exttype) and
                 (strpas(pai_external(p)^.name)=_name) then
                begin
                   search_assembler_symbol:=pai_external(p);
                   exit;
                end;
           end;
      end;


    { insert each need external only once }
    procedure concat_external(const _name : string;exttype : texternal_typ);
      begin
        if not target_asm.externals then
         exit;
        if search_assembler_symbol(externals,_name,exttype)=nil then
         externals^.concat(new(pai_external,init(_name,exttype)));
      end;


    { insert each need internal only once }
    procedure concat_internal(const _name : string;exttype : texternal_typ);
      begin
        if not target_asm.externals then
         exit;
        if search_assembler_symbol(internals,_name,exttype)=nil then
         internals^.concat(new(pai_external,init(_name,exttype)));
      end;


{*****************************************************************************
                              Label Helpers
*****************************************************************************}

    function lab2str(l : plabel) : string;
      begin
         if (l=nil) or (l^.nb=0) then
           begin
{$ifdef EXTDEBUG}
             lab2str:='ILLEGAL'
{$else EXTDEBUG}
             internalerror(2000);
{$endif EXTDEBUG}
           end
         else
           begin
             if (l^.is_data) and (cs_smartlink in aktmoduleswitches) then
              lab2str:='_$'+current_module^.modulename^+'$_L'+tostr(l^.nb)
             else
              lab2str:=target_asm.labelprefix+tostr(l^.nb);
           end;
         { inside the WriteTree we must not count the refs PM }
{$ifndef HEAPTRC}
         if countlabelref then
           inc(l^.refcount);
{$endif HEAPTRC}
         l^.is_used:=true;
      end;


    procedure getlabel(var l : plabel);
      begin
         new(l);
         l^.nb:=nextlabelnr;
         l^.is_used:=false;
         l^.is_set:=false;
         l^.is_data:=false;
         l^.refcount:=0;
         inc(nextlabelnr);
      end;


    procedure getdatalabel(var l : plabel);
      begin
         new(l);
         l^.nb:=nextlabelnr;
         l^.is_used:=false;
         l^.is_set:=false;
         l^.is_data:=true;
         l^.refcount:=0;
         inc(nextlabelnr);
      end;


    procedure freelabel(var l : plabel);
      begin
         if (l<>nil) and (not l^.is_set) and (not l^.is_used) then
           dispose(l);
         l:=nil;
      end;


    procedure setzerolabel(var l : plabel);
      begin
        with l^ do
         begin
           nb:=0;
           is_used:=false;
           is_set:=false;
           is_data:=false;
           refcount:=0;
         end;
      end;


    procedure getzerolabel(var l : plabel);
      begin
         new(l);
         l^.nb:=0;
         l^.is_used:=false;
         l^.is_set:=false;
         l^.is_data:=false;
         l^.refcount:=0;
      end;


    procedure getlabelnr(var l : longint);
      begin
         l:=nextlabelnr;
         inc(nextlabelnr);
      end;


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
  Revision 1.24  1998-11-12 11:19:30  pierre
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
