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

unit aasmtai;

{$i fpcdefs.inc}

interface

    uses
       cutils,cclasses,
       globtype,globals,systems,
       cpuinfo,cpubase,
       aasmbase;

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
{$ifdef GDB}
          ait_stabn,
          ait_stabs,
          ait_force_line,
          ait_stab_function_name,
{$endif GDB}
          ait_cut, { used to split into tiny assembler files }
          ait_regalloc,
          ait_tempalloc,
          ait_marker, { used to mark assembler blocks and inlined functions }
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
      SkipInstr = [ait_comment, ait_symbol,ait_section
{$ifdef GDB}
                   ,ait_stabs, ait_stabn, ait_stab_function_name, ait_force_line
{$endif GDB}
                   ,ait_regalloc, ait_tempalloc, ait_symbol_end];


    type
       { cut type, required for alphanumeric ordering of the assembler filenames }
       TCutPlace=(cut_normal,cut_begin,cut_end);

       TMarker = (NoPropInfoStart,NoPropInfoEnd,
                  AsmBlockStart,AsmBlockEnd,
                  InlineStart,InlineEnd);

       { the short name makes typing easier }
       tai = class(TLinkedListItem)
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
          sec : TSection;
          constructor Create(s : TSection);
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
       tai_cut = class(tai)
          place : tcutplace;
          constructor Create;
          constructor Create_begin;
          constructor Create_end;
       end;

       { insert a marker for assembler and inline blocks }
       tai_marker = class(tai)
         Kind: TMarker;
         Constructor Create(_Kind: TMarker);
       end;

       tai_tempalloc = class(tai)
          allocation : boolean;
          temppos,
          tempsize   : longint;
          constructor alloc(pos,size:longint);
          constructor dealloc(pos,size:longint);
       end;

      tai_regalloc = class(tai)
         allocation : boolean;
         reg        : tregister;
         constructor alloc(r : tregister);
         constructor dealloc(r : tregister);
      end;

      taicpu_abstract = class(tai)
        condition : TAsmCond;
        ops       : longint;
        oper      : array[0..max_operands-1] of toper;
        opcode    : tasmop;
{$ifdef i386}
        segprefix : tregister;
{$endif i386}
        is_jmp    : boolean; { is this instruction a jump? (needed for optimizer) }
        Constructor Create(op : tasmop);
        Destructor Destroy;override;
        function getcopy:TLinkedListItem;override;
        procedure loadconst(opidx:longint;l:aword);
        procedure loadsymbol(opidx:longint;s:tasmsymbol;sofs:longint);
        procedure loadref(opidx:longint;const r:treference);
        procedure loadreg(opidx:longint;r:tregister);
        procedure loadoper(opidx:longint;o:toper);
        procedure SetCondition(const c:TAsmCond);
      end;

      { alignment for operator }
      tai_align_abstract = class(tai)
         buf       : array[0..63] of char; { buf used for fill }
         aligntype : byte;   { 1 = no align, 2 = word align, 4 = dword align }
         fillsize  : byte;   { real size to fill }
         fillop    : byte;   { value to fill with - optional }
         use_op    : boolean;
         constructor Create(b:byte);
         constructor Create_op(b: byte; _op: byte);
         function getfillbuf:pchar;virtual;
      end;

       taasmoutput = class(tlinkedlist)
         function getlasttaifilepos : pfileposinfo;
       end;


    var
    { temporary lists }
      exprasmlist,
    { default lists }
      datasegment,codesegment,bsssegment,
      debuglist,withdebuglist,consts,
      importssection,exportssection,
      resourcesection,rttilist,
      resourcestringlist         : taasmoutput;


implementation

uses
{$ifdef delphi}
  sysutils,
{$else}
  strings,
{$endif}
  verbose;

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

    constructor tai_section.Create(s : TSection);
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
                                tai_tempalloc
*****************************************************************************}

    constructor tai_tempalloc.alloc(pos,size:longint);
      begin
        inherited Create;
        typ:=ait_tempalloc;
        allocation:=true;
        temppos:=pos;
        tempsize:=size;
      end;


    constructor tai_tempalloc.dealloc(pos,size:longint);
      begin
        inherited Create;
        typ:=ait_tempalloc;
        allocation:=false;
        temppos:=pos;
        tempsize:=size;
      end;

{*****************************************************************************
                                 tai_regalloc
*****************************************************************************}

    constructor tai_regalloc.alloc(r : tregister);
      begin
        inherited create;
        typ:=ait_regalloc;
        allocation:=true;
        reg:=r;
      end;


    constructor tai_regalloc.dealloc(r : tregister);
      begin
        inherited create;
        typ:=ait_regalloc;
        allocation:=false;
        reg:=r;
      end;


{*****************************************************************************
                               TaiInstruction
*****************************************************************************}

    constructor taicpu_abstract.Create(op : tasmop);

      begin
         inherited create;
         typ:=ait_instruction;
         is_jmp:=false;
         opcode:=op;
         ops:=0;
         fillchar(condition,sizeof(condition),0);
         fillchar(oper,sizeof(oper),0);
      end;



    destructor taicpu_abstract.Destroy;

      var
        i : longint;
      begin
        for i:=0 to ops-1 do
        case oper[i].typ of
          top_ref:
            dispose(oper[i].ref);
          top_symbol:
            dec(tasmsymbol(oper[i].sym).refs);
        end;
        inherited destroy;
      end;



{ ---------------------------------------------------------------------
    Loading of operands.
  ---------------------------------------------------------------------}



    procedure taicpu_abstract.loadconst(opidx:longint;l:aword);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            dispose(ref);
           val:=l;
           typ:=top_const;
         end;
      end;



    procedure taicpu_abstract.loadsymbol(opidx:longint;s:tasmsymbol;sofs:longint);
      begin
        if not assigned(s) then
         internalerror(200204251);
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            dispose(ref);
           sym:=s;
           symofs:=sofs;
           typ:=top_symbol;
         end;
        inc(s.refs);
      end;



    procedure taicpu_abstract.loadref(opidx:longint;const r:treference);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ<>top_ref then
            new(ref);
           ref^:=r;
{$ifdef i386}
           { We allow this exception for i386, since overloading this would be
             too much of a a speed penalty}
           if not(ref^.segment in [R_DS,R_NO]) then
            segprefix:=ref^.segment;
{$endif}
           typ:=top_ref;
           { mark symbol as used }
           if assigned(ref^.symbol) then
             inc(ref^.symbol.refs);
         end;
      end;



    procedure taicpu_abstract.loadreg(opidx:longint;r:tregister);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            dispose(ref);
           reg:=r;
           typ:=top_reg;
         end;
      end;



    procedure taicpu_abstract.loadoper(opidx:longint;o:toper);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        if oper[opidx].typ=top_ref then
         dispose(oper[opidx].ref);
        oper[opidx]:=o;
        { copy also the reference }
        if oper[opidx].typ=top_ref then
         begin
           new(oper[opidx].ref);
           oper[opidx].ref^:=o.ref^;
         end;
      end;


{ ---------------------------------------------------------------------
    Miscellaneous methods.
  ---------------------------------------------------------------------}

    procedure taicpu_abstract.SetCondition(const c:TAsmCond);
      begin
         condition:=c;
      end;


    Function taicpu_abstract.getcopy:TLinkedListItem;
      var
        i : longint;
        p : TLinkedListItem;
      begin
        p:=inherited getcopy;
        { make a copy of the references }
        for i:=1 to ops do
         if (taicpu_abstract(p).oper[i-1].typ=top_ref) then
          begin
            new(taicpu_abstract(p).oper[i-1].ref);
            taicpu_abstract(p).oper[i-1].ref^:=oper[i-1].ref^;
          end;
        getcopy:=p;
      end;

{****************************************************************************
                              tai_align_abstract
 ****************************************************************************}

     constructor tai_align_abstract.Create(b: byte);
       begin
          inherited Create;
          typ:=ait_align;
          if b in [1,2,4,8,16,32] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=0;
          use_op:=false;
       end;


     constructor tai_align_abstract.Create_op(b: byte; _op: byte);
       begin
          inherited Create;
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
  Revision 1.1  2002-07-01 18:46:20  peter
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
