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
       TAsmsymbind=(
         AB_NONE,AB_EXTERNAL,AB_COMMON,AB_LOCAL,AB_GLOBAL,AB_WEAK_EXTERNAL,
         { global in the current program/library, but not visible outside it }
         AB_PRIVATE_EXTERN);

       TAsmsymtype=(
         AT_NONE,AT_FUNCTION,AT_DATA,AT_SECTION,AT_LABEL,
         {
           the address of this code label is taken somewhere in the code
           so it must be taken care of it when creating pic
         }
         AT_ADDR
         );

       { is the label only there for getting an DataOffset (e.g. for i/o
         checks -> alt_addr) or is it a jump target (alt_jump), for debug
         info alt_dbgline and alt_dbgfile, etc. }
       TAsmLabelType = (alt_jump,alt_addr,alt_data,alt_dbgline,alt_dbgfile,alt_dbgtype,alt_dbgframe);

    const
       asmlabeltypeprefix : array[TAsmLabeltype] of char = ('j','a','d','l','f','t','c');

    type
       TAsmSectiontype=(sec_none,
         sec_code,
         sec_data,
         { read-only, but may contain relocations }
         sec_rodata,
         { read-only and cannot contain relocations }
         sec_rodata_norel,
         sec_bss,
         sec_threadvar,
         { used for wince exception handling }
         sec_pdata,
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
         { Yury: "sec_fpc is intended for storing fpc specific data
                  which must be recognized and processed specially by linker.
                  Currently fpc version string, dummy links to stab sections
                  and elf resources are stored in .fpc sections."
                  "If special .fpc section cannot be used on some target,
                  .text can be used instead." }
         sec_fpc,
         { Table of contents section }
         sec_toc,
         sec_init,
         sec_fini,
         {Objective-C common and fragile ABI }
         sec_objc_class,
         sec_objc_meta_class,
         sec_objc_cat_cls_meth,
         sec_objc_cat_inst_meth,
         sec_objc_protocol,
         sec_objc_string_object,
         sec_objc_cls_meth,
         sec_objc_inst_meth,
         sec_objc_cls_refs,
         sec_objc_message_refs,
         sec_objc_symbols,
         sec_objc_category,
         sec_objc_class_vars,
         sec_objc_instance_vars,
         sec_objc_module_info,
         sec_objc_class_names,
         sec_objc_meth_var_types,
         sec_objc_meth_var_names,
         sec_objc_selector_strs,
         sec_objc_protocol_ext,
         sec_objc_class_ext,
         sec_objc_property,
         sec_objc_image_info,
         sec_objc_cstring_object,
         sec_objc_sel_fixup,
         { Objective-C non-fragile ABI }
         sec_objc_data,
         sec_objc_const,
         sec_objc_sup_refs,
         sec_data_coalesced,
         sec_objc_classlist,
         sec_objc_nlclasslist,
         sec_objc_catlist,
         sec_objc_nlcatlist,
         sec_objc_protolist
       );

       TAsmSectionOrder = (secorder_begin,secorder_default,secorder_end);

       TAsmSymbol = class(TFPHashObject)
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
         constructor Create(AList:TFPHashObjectList;const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
         function getaltcopy(AList:TFPHashObjectList;altnr: longint): TAsmSymbol; virtual;
         function  is_used:boolean;
         procedure increfs;
         procedure decrefs;
         function getrefs: longint;
       end;
       TAsmSymbolClass = class of TAsmSymbol;

       TAsmLabel = class(TAsmSymbol)
       protected
         function getname:string;override;
       public
         labelnr   : longint;
         labeltype : TAsmLabelType;
         is_set    : boolean;
         constructor Createlocal(AList:TFPHashObjectList;nr:longint;ltyp:TAsmLabelType);
         constructor Createglobal(AList:TFPHashObjectList;const modulename:string;nr:longint;ltyp:TAsmLabelType);
         function getaltcopy(AList:TFPHashObjectList;altnr: longint): TAsmSymbol; override;
       end;

    function create_smartlink_sections:boolean;inline;
    function create_smartlink_library:boolean;inline;
    function create_smartlink:boolean;inline;

    function LengthUleb128(a: qword) : byte;
    function LengthSleb128(a: int64) : byte;
    function EncodeUleb128(a: qword;out buf) : byte;
    function EncodeSleb128(a: int64;out buf) : byte;


implementation

    uses
      SysUtils,
      verbose;


    function create_smartlink_sections:boolean;inline;
      begin
        result:=(af_smartlink_sections in target_asm.flags) and
                (tf_smartlink_sections in target_info.flags);
      end;


    function create_smartlink_library:boolean;inline;
      begin
        result:=(cs_Create_smart in current_settings.moduleswitches) and
                (tf_smartlink_library in target_info.flags) and
                not create_smartlink_sections;
      end;


    function create_smartlink:boolean;inline;
      begin
        result:=(
                 (af_smartlink_sections in target_asm.flags) and
                 (tf_smartlink_sections in target_info.flags)
                ) or
                (
                 (cs_Create_smart in current_settings.moduleswitches) and
                 (tf_smartlink_library in target_info.flags)
                );
      end;


    function LengthUleb128(a: qword) : byte;
      begin
        result:=0;
        repeat
          a := a shr 7;
          inc(result);
          if a=0 then
            break;
        until false;
      end;


    function LengthSleb128(a: int64) : byte;
      var
        b, size: byte;
        asign : int64;
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


    function EncodeUleb128(a: qword;out buf) : byte;
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


    function EncodeSleb128(a: int64;out buf) : byte;
      var
        b, size: byte;
        asign : int64;
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
          inc(pbuf);
          inc(result);
          if not(more) then
            break;
        until false;
      end;


{*****************************************************************************
                                 TAsmSymbol
*****************************************************************************}

    constructor TAsmSymbol.Create(AList:TFPHashObjectList;const s:string;_bind:TAsmsymbind;_typ:Tasmsymtype);
      begin;
        inherited Create(AList,s);
        bind:=_bind;
        typ:=_typ;
        { used to remove unused labels from the al_procedures }
        refs:=0;
      end;


    function TAsmSymbol.getaltcopy(AList:TFPHashObjectList;altnr: longint): TAsmSymbol;
      begin
        result := TAsmSymbol(TAsmSymbolClass(classtype).Create(AList,name+'_'+tostr(altnr),bind,typ));
      end;


    function TAsmSymbol.is_used:boolean;
      begin
        is_used:=(refs>0);
      end;


    procedure TAsmSymbol.increfs;
      begin
        inc(refs);
      end;


    procedure TAsmSymbol.decrefs;
      begin
        dec(refs);
        if refs<0 then
          internalerror(200211121);
      end;


    function TAsmSymbol.getrefs: longint;
      begin
        getrefs := refs;
      end;


{*****************************************************************************
                                 TAsmLabel
*****************************************************************************}

    constructor TAsmLabel.Createlocal(AList:TFPHashObjectList;nr:longint;ltyp:TAsmLabelType);
      var
        asmtyp: TAsmsymtype;
      begin
        case ltyp of
          alt_addr:
            asmtyp:=AT_ADDR;
          alt_data:
            asmtyp:=AT_DATA;
          else
            asmtyp:=AT_LABEL;
        end;
        inherited Create(AList,target_asm.labelprefix+asmlabeltypeprefix[ltyp]+tostr(nr),AB_LOCAL,asmtyp);
        labelnr:=nr;
        labeltype:=ltyp;
        is_set:=false;
      end;


    constructor TAsmLabel.Createglobal(AList:TFPHashObjectList;const modulename:string;nr:longint;ltyp:TAsmLabelType);
      begin
        inherited Create(AList,'_$'+modulename+'$_L'+asmlabeltypeprefix[ltyp]+tostr(nr),AB_GLOBAL,AT_DATA);
        labelnr:=nr;
        labeltype:=ltyp;
        is_set:=false;
        { write it always }
        increfs;
      end;


    function TAsmLabel.getaltcopy(AList:TFPHashObjectList;altnr: longint): TAsmSymbol;
      begin;
        result := inherited getaltcopy(AList,altnr);
        TAsmLabel(result).labelnr:=labelnr;
        TAsmLabel(result).labeltype:=labeltype;
        TAsmLabel(result).is_set:=false;
        case bind of
          AB_GLOBAL,
          AB_PRIVATE_EXTERN:
            result.increfs;
          AB_LOCAL:
            ;
          else
            internalerror(2006053101);
        end;
      end;


    function TAsmLabel.getname:string;
      begin
        getname:=inherited getname;
        increfs;
      end;

end.
