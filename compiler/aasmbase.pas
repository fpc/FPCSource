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

       { is the label only there for getting an DataOffset (e.g. for i/o
         checks -> alt_addr) or is it a jump target (alt_jump), for debug
         info alt_dbgline and alt_dbgfile, etc. }
       TAsmLabelType = (alt_jump,alt_addr,alt_data,alt_dbgline,alt_dbgfile,alt_dbgtype,alt_dbgframe);

    const
       asmlabeltypeprefix : array[tasmlabeltype] of char = ('j','a','d','l','f','t','c');

    type
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
           TAsmList with loadsym/loadref/const_symbol (PFV) }
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

       TAsmLabel = class(TAsmSymbol)
         labelnr   : longint;
         labeltype : TAsmLabelType;
         is_set    : boolean;
         constructor createlocal(nr:longint;ltyp:TAsmLabelType);
         constructor createglobal(const modulename:string;nr:longint;ltyp:TAsmLabelType);
         function getname:string;override;
       end;

    function  use_smartlink_section:boolean;
    function  maybe_smartlink_symbol:boolean;

    function LengthUleb128(a: aword) : byte;
    function LengthSleb128(a: aint) : byte;
    function EncodeUleb128(a: aword;out buf) : byte;
    function EncodeSleb128(a: aint;out buf) : byte;


implementation

    uses
      strings,
      verbose;


    function use_smartlink_section:boolean;
      begin
        result:=(af_smartlink_sections in target_asm.flags) and
                (tf_smartlink_sections in target_info.flags);
      end;


    function maybe_smartlink_symbol:boolean;
      begin
        result:=(cs_create_smart in aktmoduleswitches) or
                use_smartlink_section;
      end;


    function LengthUleb128(a: aword) : byte;
      begin
        result:=0;
        repeat
          a := a shr 7;
          inc(result);
          if a=0 then
            break;
        until false;
      end;


    function LengthSleb128(a: aint) : byte;
      var
        b, size: byte;
        asign : aint;
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
            begin
              { Use a variable to be sure that the correct or mask is generated }
              asign:=1;
              asign:=asign shl (size - 7);
              a := a or -asign;
            end;
          if (((a = 0) and
               (b and $40 = 0)) or
              ((a = -1) and
               (b and $40 <> 0))) then
            more := false;
          inc(result);
          if not(more) then
            break;
        until false;
      end;


    function EncodeUleb128(a: aword;out buf) : byte;
      var
        b: byte;
        pbuf : pbyte;
      begin
        result:=0;
        pbuf:=@buf;
        repeat
          b := a and $7f;
          a := a shr 7;
          if a<>0 then
            b := b or $80;
          pbuf^:=b;
          inc(pbuf);
          inc(result);
          if a=0 then
            break;
        until false;
      end;


    function EncodeSleb128(a: aint;out buf) : byte;
      var
        b, size: byte;
        asign : aint;
        neg, more: boolean;
        pbuf : pbyte;
      begin
        more := true;
        neg := a < 0;
        size := sizeof(a)*8;
        result:=0;
        pbuf:=@buf;
        repeat
          b := a and $7f;
          a := a shr 7;
          if neg then
            begin
              { Use a variable to be sure that the correct or mask is generated }
              asign:=1;
              asign:=asign shl (size - 7);
              a := a or -asign;
            end;
          if (((a = 0) and
               (b and $40 = 0)) or
              ((a = -1) and
               (b and $40 <> 0))) then
            more := false
          else
            b := b or $80;
          pbuf^:=b;
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

end.
