{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    Reads typed constants

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
unit ptconst;

  interface

   uses symtable;

    { this procedure reads typed constants }
    { sym is only needed for ansi strings  }
    { the assembler label is in the middle (PM) }
    procedure readtypedconst(def : pdef;sym : ptypedconstsym);

  implementation

    uses
       globtype,systems,tokens,
       cobjects,globals,scanner,aasm,tree,pass_1,
       hcodegen,types,verbose
       { parser specific stuff }
       ,pbase,pexpr
       { processor specific stuff }
{$ifdef i386}
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ;

    { this procedure reads typed constants }
    procedure readtypedconst(def : pdef;sym : ptypedconstsym);

      var
{$ifdef m68k}
         j : longint;
{$endif m68k}
         len       : longint;
         p,hp      : ptree;
         i,l,offset,
         strlength : longint;
         ll        : plabel;
         s         : string;
         ca        : pchar;
         aktpos    : longint;
         pd        : pprocdef;
         obj       : pobjectdef;
         symt      : psymtable;
         hp1,hp2   : pdefcoll;
         value     : bestreal;

      procedure check_range;
        begin
           if ((p^.value>porddef(def)^.high) or
               (p^.value<porddef(def)^.low)) then
             begin
                if (cs_check_range in aktlocalswitches) then
                  Message(parser_e_range_check_error)
                else
                  Message(parser_w_range_check_error);
             end;
        end;

      function is_po_equal(o1,o2:longint):boolean;
        begin
        { assembler does not affect }
          is_po_equal:=(o1 and not(poassembler))=
                       (o2 and not(poassembler));
        end;

{$R-}  {Range check creates problem with init_8bit(-1) !!}
      begin
         case def^.deftype of
            orddef:
              begin
                 p:=comp_expr(true);
                 do_firstpass(p);
                 case porddef(def)^.typ of
                    s8bit,
                    u8bit : begin
                               if not is_constintnode(p) then
                               { is't an int expected }
                                 Message(cg_e_illegal_expression)
                               else
                                 begin
                                    datasegment^.concat(new(pai_const,init_8bit(p^.value)));
                                    check_range;
                                 end;
                            end;
                    s32bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression)
                                else
                                  begin
                                     datasegment^.concat(new(pai_const,init_32bit(p^.value)));
                                     check_range;
                                  end;
                            end;
                    u32bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression)
                                else
                                   datasegment^.concat(new(pai_const,init_32bit(p^.value)));
                             end;
                    bool8bit : begin
                                  if not is_constboolnode(p) then
                                    Message(cg_e_illegal_expression);
                                  datasegment^.concat(new(pai_const,init_8bit(p^.value)));
                               end;
                    uchar : begin
                                if not is_constcharnode(p) then
                                  Message(cg_e_illegal_expression);
                                datasegment^.concat(new(pai_const,init_8bit(p^.value)));
                            end;
                    u16bit,
                    s16bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression);
                                datasegment^.concat(new(pai_const,init_16bit(p^.value)));
                                check_range;
                            end;
                 end;
                 disposetree(p);
              end;
         floatdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if is_constrealnode(p) then
                value:=p^.value_real
              else if is_constintnode(p) then
                value:=p^.value
              else
                Message(cg_e_illegal_expression);

              case pfloatdef(def)^.typ of
                 s64real : datasegment^.concat(new(pai_double,init(value)));
                 s32real : datasegment^.concat(new(pai_single,init(value)));
                 s80real : datasegment^.concat(new(pai_extended,init(value)));
                 s64bit  : datasegment^.concat(new(pai_comp,init(value)));
                 f32bit : datasegment^.concat(new(pai_const,init_32bit(trunc(value*65536))));
              else internalerror(18);
              end;
              disposetree(p);
           end;
         pointerdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              { allows horrible ofs(typeof(TButton)^) code !! }
              if (p^.treetype=addrn) and (p^.left^.treetype=derefn) then
                begin
                   hp:=p^.left^.left;
                   p^.left^.left:=nil;
                   disposetree(p);
                   p:=hp;
                end;
              { nil pointer ? }
              if p^.treetype=niln then
                datasegment^.concat(new(pai_const,init_32bit(0)))
              { maybe pchar ? }
              else
                if (ppointerdef(def)^.definition^.deftype=orddef) and
                   (porddef(ppointerdef(def)^.definition)^.typ=uchar) then
                  begin
                    getdatalabel(ll);
                    datasegment^.concat(new(pai_const,init_symbol(strpnew(lab2str(ll)))));
                    consts^.concat(new(pai_label,init(ll)));
                    if p^.treetype=stringconstn then
                      begin
                        getmem(ca,p^.length+2);
                        move(p^.value_str^,ca^,p^.length+1);
                        consts^.concat(new(pai_string,init_length_pchar(ca,p^.length+1)));
                      end
                    else
                      if is_constcharnode(p) then
                        consts^.concat(new(pai_string,init(char(byte(p^.value))+#0)))
                    else
                      Message(cg_e_illegal_expression);
                end
              else
                if p^.treetype=addrn then
                  begin
                    hp:=p^.left;
                    while assigned(hp) and (hp^.treetype=subscriptn) do
                      hp:=hp^.left;
                    if (is_equal(ppointerdef(p^.resulttype)^.definition,ppointerdef(def)^.definition) or
                       (is_equal(ppointerdef(p^.resulttype)^.definition,voiddef)) or
                       (is_equal(ppointerdef(def)^.definition,voiddef))) and
                       (hp^.treetype = loadn) then
                      begin
                        firstpass(p^.left);
                        hp:=p^.left;
                        offset:=0;
                        while assigned(hp) and (hp^.treetype<>loadn) do
                          begin
                             if hp^.treetype=subscriptn then
                               inc(offset,hp^.vs^.address)
                             else
                               Message(cg_e_illegal_expression);
                             hp:=hp^.left;
                          end;
                        datasegment^.concat(new(pai_const_symbol_offset,init(
                               strpnew(hp^.symtableentry^.mangledname),offset)));
                        (*if token=POINT then
                          begin
                             offset:=0;
                             while token=POINT do
                               begin
                                  consume(POINT);
                                  lsym:=pvarsym(precdef(
                                        ppointerdef(p^.resulttype)^.definition)^.symtable^.search(pattern));
                                  if assigned(sym) then
                                    offset:=offset+lsym^.address
                                  else
                                    begin
                                       Message1(sym_e_illegal_field,pattern);
                                    end;
                                  consume(ID);
                               end;
                             datasegment^.concat(new(pai_const_symbol_offset,init(
                               strpnew(p^.left^.symtableentry^.mangledname),offset)));
                          end
                        else
                          begin
                             datasegment^.concat(new(pai_const,init_symbol(
                               strpnew(p^.left^.symtableentry^.mangledname))));
                          end;   *)
                        maybe_concat_external(hp^.symtableentry^.owner,
                          hp^.symtableentry^.mangledname);
                      end
                    else
                      Message(cg_e_illegal_expression);
                  end
              else
              { allow typeof(Object type)}
                if (p^.treetype=inlinen) and
                   (p^.inlinenumber=in_typeof_x) then
                  begin
                    if (p^.left^.treetype=typen) then
                      begin
                        datasegment^.concat(new(pai_const,init_symbol(
                          strpnew(pobjectdef(p^.left^.resulttype)^.vmt_mangledname))));
                        if pobjectdef(p^.left^.resulttype)^.owner^.symtabletype=unitsymtable then
                          concat_external(pobjectdef(p^.left^.resulttype)^.vmt_mangledname,EXT_NEAR);
                      end
                    else
                      Message(cg_e_illegal_expression);
                  end
              else
                Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         setdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if p^.treetype=setconstn then
                begin
                   { we only allow const sets }
                   if assigned(p^.left) then
                     Message(cg_e_illegal_expression)
                   else
                     begin
{$ifdef i386}
                        for l:=0 to def^.savesize-1 do
                          datasegment^.concat(new(pai_const,init_8bit(p^.value_set^[l])));
{$endif}
{$ifdef m68k}
                        j:=0;
                        for l:=0 to ((def^.savesize-1) div 4) do
                        { HORRIBLE HACK because of endian        }
                        { now use intel endian for constant sets }
                         begin
                           datasegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+3])));
                           datasegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+2])));
                           datasegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+1])));
                           datasegment^.concat(new(pai_const,init_8bit(p^.value_set^[j])));
                           Inc(j,4);
                         end;
{$endif}
                     end;
                end
              else
                Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         enumdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if p^.treetype=ordconstn then
                begin
                   if is_equal(p^.resulttype,def) then
                     datasegment^.concat(new(pai_const,init_32bit(p^.value)))
                   else
                     Message(cg_e_illegal_expression);
                end
              else
                Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         stringdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              { first take care of prefixes for long and ansi strings }
              case pstringdef(def)^.string_typ of
                 st_shortstring:
                   begin
                      if p^.treetype=stringconstn then
                        begin
                           if p^.length>=def^.size then
                             strlength:=def^.size-1
                           else
                             strlength:=p^.length;
                           datasegment^.concat(new(pai_const,init_8bit(strlength)));
                           { this can also handle longer strings }
                           getmem(ca,strlength+1);
                           move(p^.value_str^,ca^,strlength);
                           ca[strlength]:=#0;
                           datasegment^.concat(new(pai_string,init_length_pchar(ca,strlength)));
                        end
                      else if is_constcharnode(p) then
                        begin
                           datasegment^.concat(new(pai_string,init(#1+char(byte(p^.value)))));
                           strlength:=1;
                        end
                      else Message(cg_e_illegal_expression);

                      if def^.size>strlength then
                        begin
                           getmem(ca,def^.size-strlength);
                           { def^.size contains also the leading length, so we }
                           { we have to subtract one                           }
                           fillchar(ca[0],def^.size-strlength-1,' ');
                           ca[def^.size-strlength-1]:=#0;
                           { this can also handle longer strings }
                           datasegment^.concat(new(pai_string,init_length_pchar(ca,def^.size-strlength-1)));
                        end;
                   end;
{$ifdef UseLongString}
                 st_longstring:
                   begin
                     if is_constcharnode(p) then
                      strlength:=1
                     else
                      strlength:=p^.length;
                     { first write the maximum size }
                     datasegment^.concat(new(pai_const,init_32bit(strlength)))));
                     { fill byte }
                     datasegment^.concat(new(pai_const,init_8bit(0)));
                     if p^.treetype=stringconstn then
                       begin
                         getmem(ca,strlength+1);
                         move(p^.value_str^,ca^,strlength);
                         ca[strlength]:=#0;
                         generate_pascii(consts,ca,strlength);
                       end
                     else if is_constcharnode(p) then
                       begin
                          consts^.concat(new(pai_const,init_8bit(p^.value)));
                       end
                     else Message(cg_e_illegal_expression);
                     datasegment^.concat(new(pai_const,init_8bit(0)));
                   end;
{$endif UseLongString}
                 st_ansistring:
                   begin
                      { an empty ansi string is nil! }
                      if (p^.treetype=stringconstn) and (p^.length=0) then
                        datasegment^.concat(new(pai_const,init_32bit(0)))
                      else
                        begin
                           if is_constcharnode(p) then
                            strlength:=1
                           else
                            strlength:=p^.length;
                           getdatalabel(ll);
                           datasegment^.concat(new(pai_const,init_symbol(strpnew(lab2str(ll)))));
                           { first write the maximum size }
                           consts^.concat(new(pai_const,init_32bit(strlength)));
                           { second write the real length }
                           consts^.concat(new(pai_const,init_32bit(strlength)));
                           { redondent with maxlength but who knows ... (PM) }
                           { third write use count (set to -1 for safety ) }
                           consts^.concat(new(pai_const,init_32bit(-1)));
                           consts^.concat(new(pai_label,init(ll)));
                           if p^.treetype=stringconstn then
                             begin
                               getmem(ca,strlength+1);
                               move(p^.value_str^,ca^,strlength);
                               ca[strlength]:=#0;
                               consts^.concat(new(pai_string,init_length_pchar(ca,strlength)));
                             end
                           else if is_constcharnode(p) then
                             begin
                                consts^.concat(new(pai_const,init_8bit(p^.value)));
                             end
                           else Message(cg_e_illegal_expression);
                           consts^.concat(new(pai_const,init_8bit(0)));
                        end;
                   end;
              end;
              disposetree(p);
           end;
         arraydef:
           begin
              if token=LKLAMMER then
                begin
                    consume(LKLAMMER);
                    for l:=parraydef(def)^.lowrange to parraydef(def)^.highrange-1 do
                      begin
                         readtypedconst(parraydef(def)^.definition,nil);
                         consume(COMMA);
                      end;
                    readtypedconst(parraydef(def)^.definition,nil);
                    consume(RKLAMMER);
                 end
              else
                begin
                   p:=comp_expr(true);
                   do_firstpass(p);
                   if p^.treetype=stringconstn then
                    begin
                      if p^.length>255 then
                       len:=255
                      else
                       len:=p^.length;
                      {$ifndef TP}
                        {$ifopt H+}
                          setlength(s,len);
                        {$else}
                          s[0]:=chr(len);
                        {$endif}
                      {$else}
                        s[0]:=chr(len);
                      {$endif}
                      move(p^.value_str^,s[1],len);
                    end
                   else
                     if is_constcharnode(p) then
                       s:=char(byte(p^.value))
                   else
                     Message(cg_e_illegal_expression);
                   disposetree(p);
                   l:=length(s);
                   for i:=Parraydef(def)^.lowrange to Parraydef(def)^.highrange do
                     begin
                        if i+1-Parraydef(def)^.lowrange<=l then
                          begin
                             datasegment^.concat(new(pai_const,init_8bit(byte(s[1]))));
                             delete(s,1,1);
                          end
                        else
                          {Fill the remaining positions with #0.}
                          datasegment^.concat(new(pai_const,init_8bit(0)));
                     end;
                   if length(s)>0 then
                     Message(parser_e_invalid_string_size);
                 end;
           end;
         procvardef:
           begin
              { Procvars and pointers are no longer compatible.  }
              { under tp:  =nil or =var under fpc: =nil or =@var }
              if token=_NIL then
                begin
                   datasegment^.concat(new(pai_const,init_32bit(0)));
                   consume(_NIL);
                   exit;
                end
              else
                if not(m_tp_procvar in aktmodeswitches) then
                  if token=KLAMMERAFFE then
                    consume(KLAMMERAFFE);
              getsym(pattern,true);
              consume(ID);
              if srsym^.typ=unitsym then
                begin
                   consume(POINT);
                   getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                   consume(ID);
                end;
              if srsym^.typ<>procsym then
                Message(cg_e_illegal_expression)
              else
                begin
                   pd:=pprocsym(srsym)^.definition;
                   if assigned(pd^.nextoverloaded) then
                     Message(parser_e_no_overloaded_procvars);
                   if is_po_equal(pprocvardef(def)^.options,pd^.options) and
                      is_equal(pprocvardef(def)^.retdef,pd^.retdef) then
                     begin
                       hp1:=pprocvardef(def)^.para1;
                       hp2:=pd^.para1;
                       while assigned(hp1) and assigned(hp2) do
                        begin
                          if not(is_equal(hp1^.data,hp2^.data)) or
                             not(hp1^.paratyp=hp2^.paratyp) then
                            begin
                              Message(type_e_mismatch);
                              break;
                            end;
                           hp1:=hp1^.next;
                           hp2:=hp2^.next;
                         end;
                        if not((hp1=nil) and (hp2=nil)) then
                          Message(type_e_mismatch);
                     end
                   else
                     Message(type_e_mismatch);
                   datasegment^.concat(new(pai_const,init_symbol(strpnew(pd^.mangledname))));
                   if pd^.owner^.symtabletype=unitsymtable then
                     concat_external(pd^.mangledname,EXT_NEAR);
                end;
           end;
         { reads a typed constant record }
         recorddef:
           begin
              consume(LKLAMMER);
              aktpos:=0;
              while token<>RKLAMMER do
                begin
                   s:=pattern;
                   consume(ID);
                   consume(COLON);
                   srsym:=precdef(def)^.symtable^.search(s);
                   if srsym=nil then
                     begin
                        Message1(sym_e_id_not_found,s);
                        consume_all_until(SEMICOLON);
                     end
                   else
                     begin
                        { check position }
                        if pvarsym(srsym)^.address<aktpos then
                          Message(parser_e_invalid_record_const);

                        { if needed fill }
                        if pvarsym(srsym)^.address>aktpos then
                          for i:=1 to pvarsym(srsym)^.address-aktpos do
                            datasegment^.concat(new(pai_const,init_8bit(0)));

                        { new position }
                        aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.definition^.size;

                        { read the data }
                        readtypedconst(pvarsym(srsym)^.definition,nil);

                        if token=SEMICOLON then
                          consume(SEMICOLON)
                        else break;
                     end;
                end;
              for i:=1 to def^.size-aktpos do
                datasegment^.concat(new(pai_const,init_8bit(0)));
              consume(RKLAMMER);
           end;
         { reads a typed object }
         objectdef:
           begin
              if (pobjectdef(def)^.options and (oo_hasvmt or oo_is_class))<>0 then
                begin
                   Message(parser_e_type_const_not_possible);
                   consume_all_until(RKLAMMER);
                end
              else
                begin
                   consume(LKLAMMER);
                   aktpos:=0;
                   while token<>RKLAMMER do
                     begin
                        s:=pattern;
                        consume(ID);
                        consume(COLON);
                        srsym:=nil;
                        obj:=pobjectdef(def);
                        symt:=obj^.publicsyms;
                        while (srsym=nil) and assigned(symt) do
                          begin
                             srsym:=symt^.search(s);
                             if assigned(obj) then
                               obj:=obj^.childof;
                             if assigned(obj) then
                               symt:=obj^.publicsyms
                             else
                               symt:=nil;
                          end;

                        if srsym=nil then
                          begin
                             Message1(sym_e_id_not_found,s);
                             consume_all_until(SEMICOLON);
                          end
                        else
                          begin
                             { check position }
                             if pvarsym(srsym)^.address<aktpos then
                               Message(parser_e_invalid_record_const);

                             { if needed fill }
                             if pvarsym(srsym)^.address>aktpos then
                               for i:=1 to pvarsym(srsym)^.address-aktpos do
                                 datasegment^.concat(new(pai_const,init_8bit(0)));

                             { new position }
                             aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.definition^.size;

                             { read the data }
                             readtypedconst(pvarsym(srsym)^.definition,nil);

                             if token=SEMICOLON then
                               consume(SEMICOLON)
                             else break;
                          end;
                     end;
                   for i:=1 to def^.size-aktpos do
                     datasegment^.concat(new(pai_const,init_8bit(0)));
                   consume(RKLAMMER);
                end;
           end;
         else Message(parser_e_type_const_not_possible);
         end;
      end;

end.
{
  $Log$
  Revision 1.31  1998-12-11 00:03:41  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.30  1998/11/27 14:34:42  peter
    * give error when string[0] decl is found

  Revision 1.29  1998/11/23 18:26:44  pierre
   * fix for bug0182

  Revision 1.28  1998/11/17 10:40:16  peter
    * H+ fixes

  Revision 1.27  1998/11/16 12:12:23  peter
    - generate_pascii which is obsolete

  Revision 1.26  1998/11/10 17:53:06  peter
    * fixed const string

  Revision 1.25  1998/11/10 16:10:47  peter
    * fixed const pchar

  Revision 1.24  1998/11/05 12:02:55  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.23  1998/11/04 10:11:45  peter
    * ansistring fixes

  Revision 1.22  1998/10/20 08:06:56  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.21  1998/10/19 08:55:03  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.20  1998/10/16 08:51:49  peter
    + target_os.stackalignment
    + stack can be aligned at 2 or 4 byte boundaries

  Revision 1.19  1998/10/12 12:20:58  pierre
    + added tai_const_symbol_offset
      for r : pointer = @var.field;
    * better message for different arg names on implementation
      of function

  Revision 1.18  1998/10/12 09:50:05  florian
    + support of <procedure var type>:=<pointer> in delphi mode added

  Revision 1.17  1998/10/09 08:56:29  pierre
    * several memory leaks fixed

  Revision 1.16  1998/09/24 23:49:18  peter
    + aktmodeswitches

  Revision 1.15  1998/09/07 18:46:11  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.14  1998/09/04 08:42:07  peter
    * updated some error messages

  Revision 1.13  1998/09/01 09:05:36  peter
    * fixed string[4]='.library'

  Revision 1.12  1998/08/31 12:26:32  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.11  1998/08/10 14:50:20  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.10  1998/07/21 11:16:25  florian
    * bug0147 fixed

  Revision 1.9  1998/07/20 22:17:16  florian
    * hex constants in numeric char (#$54#$43 ...) are now allowed
    * there was a bug in record_var_dec which prevents the used
      of nested variant records (for example drivers.tevent of tv)

  Revision 1.8  1998/07/20 18:40:15  florian
    * handling of ansi string constants should now work

  Revision 1.7  1998/07/18 22:54:29  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.6  1998/06/08 22:59:52  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.5  1998/06/03 22:49:01  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.4  1998/05/05 12:05:42  florian
    * problems with properties fixed
    * crash fixed:  i:=l when i and l are undefined, was a problem with
      implementation of private/protected

  Revision 1.3  1998/04/29 10:34:00  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions
}
