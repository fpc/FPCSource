{
    Copyright (c) 2011 by Jonas Maebe

    Generates nodes for typed constant declarations

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
unit njvmtcon;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      node,
      symtype,symdef,
      ngtcon;


    type
      tarrstringdata = record
        arrstring: ansistring;
        arrdatastart, arrdatalen: asizeint;
        arraybase: tnode;
      end;

      tjvmtypedconstbuilder = class(tnodetreetypedconstbuilder)
       private
        procedure tc_flush_arr_strconst(def: tdef);
        procedure tc_emit_arr_strconst_ele(val: int64; def: torddef);
       protected
        arrstringdata: tarrstringdata;
        parsingordarray: boolean;

        procedure parse_arraydef(def: tarraydef); override;
        procedure tc_emit_setdef(def: tsetdef; var node: tnode);override;
        procedure tc_emit_orddef(def: torddef; var node: tnode); override;
      end;

implementation

    uses
      globals,widestr,verbose,constexp,
      tokens,scanner,pexpr,
      defutil,
      nbas,ncal,ncon,ncnv,njvmcon;


    procedure init_arrstringdata(out data: tarrstringdata);
      begin
        data.arrstring:='';
        data.arrdatastart:=0;
        data.arrdatalen:=0;
        data.arraybase:=nil;
      end;


    procedure tjvmtypedconstbuilder.tc_flush_arr_strconst(def: tdef);
      var
        wstr: pcompilerwidestring;
        wc: tcompilerwidechar;
        i: longint;
        procvariant: string[8];
      begin
        // convert ansistring to packed unicodestring
        initwidestring(wstr);
        for i:=1 to length(arrstringdata.arrstring) div 2 do
          begin
            wc:=tcompilerwidechar(ord(arrstringdata.arrstring[i*2-1]) shl 8 or
                                  ord(arrstringdata.arrstring[i*2]));
            concatwidestringchar(wstr,wc);
          end;
        if odd(length(arrstringdata.arrstring)) then
          concatwidestringchar(wstr,
            tcompilerwidechar(ord(arrstringdata.arrstring[length(arrstringdata.arrstring)]) shl 8));


        if is_char(def) then
          procvariant:='ansichar'
        else if is_signed(def) then
          case def.size of
            1: procvariant:='shortint';
            2: procvariant:='smallint';
            4: procvariant:='longint';
            8: procvariant:='int64';
            else
              internalerror(2011111301);
          end
        else
          case def.size of
            1: procvariant:='byte';
            2: procvariant:='word';
            4: procvariant:='cardinal';
            8: procvariant:='qword';
            else
              internalerror(2011111302);
          end;
        // (const s: unicodestring; var arr: array of shortint; startintdex, len: longint);
        addstatement(statmnt,ccallnode.createintern('fpc_tcon_'+procvariant+'_array_from_string',
          ccallparanode.create(genintconstnode(arrstringdata.arrdatalen),
            ccallparanode.create(genintconstnode(arrstringdata.arrdatastart),
              ccallparanode.create(arrstringdata.arraybase.getcopy,
                ccallparanode.create(cstringconstnode.createunistr(wstr),nil))))));

        inc(arrstringdata.arrdatastart,arrstringdata.arrdatalen);
        arrstringdata.arrstring:='';
        arrstringdata.arrdatalen:=0;

        donewidestring(wstr);
      end;


    procedure tjvmtypedconstbuilder.tc_emit_arr_strconst_ele(val: int64; def: torddef);
      var
        elesize: longint;
      begin
        elesize:=def.size;
        inc(arrstringdata.arrdatalen);
        case elesize of
          1:
            arrstringdata.arrstring:=arrstringdata.arrstring+char(val);
          2:
            arrstringdata.arrstring:=arrstringdata.arrstring+char(val shr 8)+char(val and $ff);
          4:
            arrstringdata.arrstring:=arrstringdata.arrstring+char((val shr 24))+
              char((val shr 16) and $ff)+
              char((val shr 8) and $ff)+
              char(val and $ff);
          8:
            arrstringdata.arrstring:=arrstringdata.arrstring+char((val shr 56))+
              char((val shr 48) and $ff)+
              char((val shr 40) and $ff)+
              char((val shr 32) and $ff)+
              char((val shr 24) and $ff)+
              char((val shr 16) and $ff)+
              char((val shr 8) and $ff)+
              char(val and $ff);
        end;
        { we can't use the full 64kb, because inside the Java class file the
          string constant is actually encoded using UTF-8 and it's this UTF-8
          encoding that has to fit inside 64kb (and utf-8 encoding of random
          data can easily blow up its size by about a third) }
        if length(arrstringdata.arrstring)>40000 then
          tc_flush_arr_strconst(def);
      end;


    procedure tjvmtypedconstbuilder.parse_arraydef(def: tarraydef);
      var
        n: tnode;
        i, len: longint;
        ca: pbyte;
        ch: array[0..1] of char;
        old_arrstringdata: tarrstringdata;
        old_parsingordarray: boolean;
      begin
        if is_dynamic_array(def) or
           (not is_char(def.elementdef) and
            (not is_integer(def.elementdef) or
             not(ts_compact_int_array_init in current_settings.targetswitches))) then
          begin
            inherited;
            exit;
          end;
        old_arrstringdata:=arrstringdata;
        init_arrstringdata(arrstringdata);
        arrstringdata.arraybase:=basenode.getcopy;
        old_parsingordarray:=parsingordarray;
        parsingordarray:=true;
        if (token=_LKLAMMER) or
           not is_char(def.elementdef) then
          inherited
        else
          begin
            { array of ansichar -> can be constant char/string; can't use plain
              assignment in this case, because it will result in a codepage
              conversion }
            n:=comp_expr([ef_accept_equal]);
            if n.nodetype=stringconstn then
              begin
                len:=tstringconstnode(n).len;
                if (tstringconstnode(n).cst_type in [cst_unicodestring,cst_widestring]) then
                  inserttypeconv(n,getansistringdef);
                  if n.nodetype<>stringconstn then
                    internalerror(2010033010);
                  ca:=pbyte(tstringconstnode(n).value_str);
                { For tp7 the maximum lentgh can be 255 }
                if (m_tp7 in current_settings.modeswitches) and
                   (len>255) then
                 len:=255;
              end
            else if is_constcharnode(n) then
               begin
                 ch[0]:=chr(tordconstnode(n).value.uvalue and $ff);
                 ca:=@ch;
                 len:=1;
               end
            else if is_constwidecharnode(n) and (current_settings.sourcecodepage<>CP_UTF8) then
               begin
                 inserttypeconv(n,cansichartype);
                 if not is_constcharnode(n) then
                   internalerror(2010033007);
                 ch[0]:=chr(tordconstnode(n).value.uvalue and $ff);
                 ca:=@ch;
                 len:=1;
               end
            else
              begin
                Message(parser_e_illegal_expression);
                len:=0;
                { avoid crash later on }
                ch[0]:=#0;
                ca:=@ch;
              end;
            if len>(def.highrange-def.lowrange+1) then
              Message(parser_e_string_larger_array);
            for i:=0 to def.highrange-def.lowrange do
              begin
                if i<len then
                  begin
                    tc_emit_arr_strconst_ele(pbyte(ca)^,torddef(cansichartype));
                    inc(ca);
                  end
                else
                  {Fill the remaining positions with #0.}
                  tc_emit_arr_strconst_ele(0,torddef(cansichartype));
              end;
            n.free;
          end;
        if length(arrstringdata.arrstring)<>0 then
          tc_flush_arr_strconst(def.elementdef);
        arrstringdata.arraybase.free;
        parsingordarray:=old_parsingordarray;
        arrstringdata:=old_arrstringdata;
      end;


    procedure tjvmtypedconstbuilder.tc_emit_setdef(def: tsetdef; var node: tnode);
      begin
        { indicate that set constant nodes have to be transformed into
          constructors here }
        if node.nodetype=setconstn then
          tjvmsetconstnode(node).setconsttype:=sct_construct;
        inherited tc_emit_setdef(def,node);
      end;


    procedure tjvmtypedconstbuilder.tc_emit_orddef(def: torddef; var node: tnode);
      begin
        if not parsingordarray then
          begin
            inherited;
            exit;
          end;
        if node.nodetype<>ordconstn then
          internalerror(2011111101);
        tc_emit_arr_strconst_ele(tordconstnode(node).value.svalue,def);
        basenode.free;
        basenode:=nil;
        node.free;
        node:=nil;
      end;

begin
  ctypedconstbuilder:=tjvmtypedconstbuilder;
end.
