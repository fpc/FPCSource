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

{$I version.inc}
    type
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
          ait_const_symbol,
          ait_const_16bit,
          ait_const_8bit,
          ait_real_64bit,
          ait_real_32bit,
          ait_real_extended,
          ait_comp,
          ait_external,
          ait_align,
          { the following is only used by the win32 version of the compiler }
          { and only the GNU AS Win32 is able to write it                   }
          ait_section,
          ait_const_rva,
{$ifdef GDB}
          ait_stabn,
          ait_stabs,
          ait_stab_function_name,
{$endif GDB}
          ait_cut, { used to split into tiny assembler files }
{$ifdef REGALLOC}
          ait_regalloc,
          ait_regdealloc,
{$endif REGALLOC}
          { never used, makes insertation of new ait_ easier to type }
          ait_dummy);

     type
       { the short name makes typing easier }
       pai = ^tai;

       tai = object(tlinkedlist_item)
          typ : tait;
          line : longint;
          infile : pinputfile;
          constructor init;
       end;

       pai_string = ^tai_string;

       tai_string = object(tai)
          str : pchar;
          { extra len so the string can contain an \0 }
          len : longint;
          constructor init(const _str : string);
          constructor init_pchar(_str : pchar);
          destructor done;virtual;
       end;

       pai_symbol = ^tai_symbol;

       { generates a common label }
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

       pai_external = ^tai_external;

       { generates an symbol which is marked as external }
       tai_external = object(tai)
          name : pchar;
          exttyp : texternal_typ;
          constructor init(const _name : string;exttype : texternal_typ);
          destructor done; virtual;
       end;

       { simple temporary label }
       pai_label = ^tai_label;

       { type for a temporary label }
       { test if used for dispose of unnecessary labels }
       tlabel = record
                nb : longint;
                is_used : boolean;
                is_set : boolean;
                refcount : word;
                end;

       plabel = ^tlabel;

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


       { alignment for operator }
       pai_align = ^tai_align;
       tai_align = object(tai)
          aligntype: byte;   { 1 = no align, 2 = word align, 4 = dword align }
          op: byte;          { value to fill with - optional                 }
          constructor init(b:byte);
          constructor init_op(b: byte; use_op: byte);
          destructor done;virtual;
       end;

       pai_section = ^tai_section;

       tai_section = object(tai)
          name : pstring;
          constructor init(const s : string);
          destructor done;virtual;
       end;

       pai_datablock = ^tai_datablock;

       { generates an uninitilizised data block }
       tai_datablock = object(tai)
          size : longint;
          name : pchar;
          is_global : boolean;
          constructor init(const _name : string;_size : longint);
          constructor init_global(const _name : string;_size : longint);
          destructor done; virtual;
       end;

       pai_const = ^tai_const;

       { generates a long integer (32 bit) }
       tai_const = object(tai)
          value : longint;
          constructor init_32bit(_value : longint);
          constructor init_16bit(_value : word);
          constructor init_8bit(_value : byte);
          constructor init_symbol(p : pchar);
          constructor init_rva(p : pchar);
          destructor done;virtual;
       end;

       pai_double = ^tai_double;

       { generates a double (64 bit real) }
       tai_double = object(tai)
          value : double;
          constructor init(_value : double);
       end;

       pai_single = ^tai_single;

       { generates a single (32 bit real) }
       tai_single = object(tai)
          value : single;
          constructor init(_value : single);
       end;

       pai_extended = ^tai_extended;

       { generates an extended (80 bit real) }
       { for version above v0_9_8            }
       { creates a double otherwise          }
       tai_extended = object(tai)
          value : bestreal;
          constructor init(_value : bestreal);
       end;

       pai_cut = ^tai_cut;
       tai_cut = object(tai)
          constructor init;
       end;

{ for each processor define the best precision }
{ bestreal is defined in globals }
{$ifdef i386}
{$ifdef ver_above0_9_8}
const
       ait_bestreal = ait_real_extended;
type
       pai_bestreal = pai_extended;
       tai_bestreal = tai_extended;
{$else ver_above0_9_8}
const
       ait_bestreal = ait_real_64bit;
type
       pai_bestreal = pai_double;
       tai_bestreal = tai_double;
{$endif ver_above0_9_8}
{$endif i386}
{$ifdef m68k}
const
       ait_bestreal = ait_real_32bit;
type
       pai_bestreal = pai_single;
       tai_bestreal = tai_single;
{$endif m68k}

       pai_comp = ^tai_comp;

       { generates an comp (integer over 64 bits) }
       tai_comp = object(tai)
          value : bestreal;
          constructor init(_value : bestreal);
       end;

       paasmoutput = ^taasmoutput;
       taasmoutput = tlinkedlist;

    var
      datasegment,codesegment,bsssegment,
      internals,externals,debuglist,consts,importssection,
      exportssection,resourcesection : paasmoutput;

   { external symbols without repetition }
    function search_assembler_symbol(pl : paasmoutput;const _name : string;exttype : texternal_typ) : pai_external;
    procedure concat_external(const _name : string;exttype : texternal_typ);
    procedure concat_internal(const _name : string;exttype : texternal_typ);

  implementation

  uses strings,verbose;

{****************************************************************************
                             TAI
 ****************************************************************************}

    constructor tai.init;

      begin
{$ifdef GDB}
         infile:=pointer(current_module^.current_inputfile);
         if assigned(infile) then
           line:=current_module^.current_inputfile^.line_no;
{$endif GDB}
      end;
{****************************************************************************
                             TAI_SECTION
 ****************************************************************************}

    constructor tai_section.init(const s : string);

      begin
         inherited init;
         typ:=ait_section;
         name:=stringdup(s);
      end;

    destructor tai_section.done;

      begin
         stringdispose(name);
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

      var
         p : pai_external;

      begin
         p:=search_assembler_symbol(externals,_name,exttype);
         if p=nil then
           externals^.concat(new(pai_external,init(_name,exttype)));
      end;

    { insert each need external only once }
    procedure concat_internal(const _name : string;exttype : texternal_typ);

      var
         p : pai_external;

      begin
         p:=search_assembler_symbol(internals,_name,exttype);
         if p=nil then
           internals^.concat(new(pai_external,init(_name,exttype)));
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
          { suggestion of JM:
            inc(l^.refcount); }
       end;

    destructor tai_label.done;

      begin
         { suggestion of JM:
         dec(l^.refcount);  }
         if (l^.is_used) then
           l^.is_set:=false
         else dispose(l);
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
       end;

end.
{
  $Log$
  Revision 1.3  1998-04-27 23:10:27  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.2  1998/04/09 15:46:37  florian
    + register allocation tracing stuff added

  Revision 1.1.1.1  1998/03/25 11:18:16  root
  * Restored version

  Revision 1.18  1998/03/10 16:27:36  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.17  1998/03/10 01:17:13  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.16  1998/03/02 01:47:56  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.15  1998/02/28 14:43:46  florian
    * final implemenation of win32 imports
    * extended tai_align to allow 8 and 16 byte aligns

  Revision 1.14  1998/02/28 00:20:20  florian
    * more changes to get import libs for Win32 working

  Revision 1.13  1998/02/27 22:27:50  florian
    + win_targ unit
    + support of sections
    + new asmlists: sections, exports and resource

  Revision 1.12  1998/02/24 00:19:08  peter
    * makefile works again (btw. linux does like any char after a \ )
    * removed circular unit with assemble and files
    * fixed a sigsegv in pexpr
    * pmodule init unit/program is the almost the same, merged them

  Revision 1.11  1998/02/13 10:34:29  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.10  1998/02/06 23:08:31  florian
    + endian to targetinfo and sourceinfo added
    + endian independed writing of ppu file (reading missed), a PPU file
      is written with the target endian

  Revision 1.9  1998/01/11 04:14:30  carl
  + correct floating point support for m68k

  Revision 1.6  1997/12/09 13:18:34  carl
  + added pai_align abstract object (required for m68k)
  + renamed ait_real_s80bit --> ait_real_extended

  Revision 1.5  1997/12/01 18:14:32  pierre
      * fixes a bug in nasm output due to my previous changes

  Revision 1.3  1997/11/28 18:14:17  pierre
   working version with several bug fixes

  Revision 1.2  1997/11/28 14:26:18  florian
  Fixed some bugs

  Revision 1.1.1.1  1997/11/27 08:32:50  michael
  FPC Compiler CVS start

  Pre-CVS log:

  FK     Florian Klaempfl
  PM     Pierre Muller
  +      feature added
  -      removed
  *      bug fixed or changed

  History:
      30th september 1996:
         + unit started
      13th november 1997:
         + added pai_single and pai_extended (PM)
      14th november 1997:
         + added bestreal type and pai_bestreal
           to store all real consts with best precision (PM)
           has a drawback for GDB that does not know extended !! (PM)

}
