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
    procedure readtypedconst(def : pdef;sym : ptypedconstsym;no_change_allowed : boolean);

  implementation

    uses
       globtype,systems,tokens,
       cobjects,globals,scanner,aasm,tree,pass_1,
       types,verbose
       { parser specific stuff }
       ,pbase,pexpr
       { processor specific stuff }
{$ifdef i386}
       ,i386base
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       { codegen }
       ,hcodegen,hcgdata
       ;


    { this procedure reads typed constants }
    procedure readtypedconst(def : pdef;sym : ptypedconstsym;no_change_allowed : boolean);

      var
{$ifdef m68k}
         j : longint;
{$endif m68k}
         len,base  : longint;
         p,hp      : ptree;
         i,l,offset,
         strlength : longint;
         curconstsegment : paasmoutput;
         ll     : pasmlabel;
         s       : string;
         ca     : pchar;
         aktpos    : longint;
         pd     : pprocdef;
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
         if no_change_allowed then
           curconstsegment:=consts
         else
           curconstsegment:=datasegment;
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
                                    curconstsegment^.concat(new(pai_const,init_8bit(p^.value)));
                                    check_range;
                                 end;
                            end;
                    s32bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression)
                                else
                                  begin
                                     curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                                     check_range;
                                  end;
                            end;
                    u32bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression)
                                else
                                   curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                             end;
                    bool8bit : begin
                                  if not is_constboolnode(p) then
                                    Message(cg_e_illegal_expression);
                                  curconstsegment^.concat(new(pai_const,init_8bit(p^.value)));
                               end;
                    bool16bit : begin
                                  if not is_constboolnode(p) then
                                    Message(cg_e_illegal_expression);
                                  curconstsegment^.concat(new(pai_const,init_16bit(p^.value)));
                               end;
                    bool32bit : begin
                                  if not is_constboolnode(p) then
                                    Message(cg_e_illegal_expression);
                                  curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                               end;
                    uchar : begin
                                if not is_constcharnode(p) then
                                  Message(cg_e_illegal_expression);
                                curconstsegment^.concat(new(pai_const,init_8bit(p^.value)));
                            end;
                    u16bit,
                    s16bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression);
                                curconstsegment^.concat(new(pai_const,init_16bit(p^.value)));
                                check_range;
                            end;
                    s64bitint,
                    u64bit:
                      begin
                         if not is_constintnode(p) then
                           Message(cg_e_illegal_expression)
                         else
                           begin
                              {!!!!! hmmm, we can write yet only consts til 2^32-1 :( (FK) }
                              curconstsegment^.concat(new(pai_const,init_32bit(p^.value)));
                              curconstsegment^.concat(new(pai_const,init_32bit(0)));
                           end;
                      end;
                    else
                      internalerror(3799);
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
                 s32real : curconstsegment^.concat(new(pai_real_32bit,init(value)));
                 s64real : curconstsegment^.concat(new(pai_real_64bit,init(value)));
                 s80real : curconstsegment^.concat(new(pai_real_80bit,init(value)));
                 s64comp  : curconstsegment^.concat(new(pai_comp_64bit,init(value)));
                 f32bit : curconstsegment^.concat(new(pai_const,init_32bit(trunc(value*65536))));
              else internalerror(18);
              end;
              disposetree(p);
           end;
         pointerdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if (p^.treetype=typeconvn) and
                 ((p^.left^.treetype=addrn) or (p^.left^.treetype=niln)) and
                 is_equal(def,p^.resulttype) then
                begin
                   hp:=p^.left;
                   putnode(p);
                   p:=hp;
                end;
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
                curconstsegment^.concat(new(pai_const,init_32bit(0)))
              { maybe pchar ? }
              else
                if (ppointerdef(def)^.definition^.deftype=orddef) and
                   (porddef(ppointerdef(def)^.definition)^.typ=uchar) and
                   (p^.treetype<>addrn) then
                  begin
                    getdatalabel(ll);
                    curconstsegment^.concat(new(pai_const_symbol,init(ll)));
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
                    while assigned(hp) and (hp^.treetype in [subscriptn,vecn]) do
                      hp:=hp^.left;
                    if (is_equal(ppointerdef(p^.resulttype)^.definition,ppointerdef(def)^.definition) or
                       (is_equal(ppointerdef(p^.resulttype)^.definition,voiddef)) or
                       (is_equal(ppointerdef(def)^.definition,voiddef))) and
                       (hp^.treetype=loadn) then
                      begin
                        do_firstpass(p^.left);
                        hp:=p^.left;
                        offset:=0;
                        while assigned(hp) and (hp^.treetype<>loadn) do
                          begin
                             case hp^.treetype of
                               vecn       :
                                 begin
                                    if (hp^.left^.resulttype^.deftype=stringdef) then
                                      begin
                                         { this seems OK for shortstring and ansistrings PM }
                                         { it is wrong for widestrings !! }
                                         len:=1;
                                         base:=0;
                                      end
                                    else if (hp^.left^.resulttype^.deftype=arraydef) then
                                      begin
                                         len:=parraydef(hp^.left^.resulttype)^.elesize;
                                         base:=parraydef(hp^.left^.resulttype)^.lowrange;
                                      end
                                    else
                                      Message(cg_e_illegal_expression);
                                    if is_constintnode(hp^.right) then
                                      inc(offset,len*(get_ordinal_value(hp^.right)-base))
                                    else
                                      Message(cg_e_illegal_expression);
                                      {internalerror(9779);}
                                 end;

                               subscriptn : inc(offset,hp^.vs^.address)
                             else
                               Message(cg_e_illegal_expression);
                             end;
                             hp:=hp^.left;
                          end;
                        if hp^.symtableentry^.typ=constsym then
                          Message(type_e_variable_id_expected);
                        curconstsegment^.concat(new(pai_const_symbol,initname_offset(hp^.symtableentry^.mangledname,offset)));
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
                             curconstsegment^.concat(new(pai_const_symbol_offset,init(
                               strpnew(p^.left^.symtableentry^.mangledname),offset)));
                          end
                        else
                          begin
                             curconstsegment^.concat(new(pai_const,init_symbol(
                               strpnew(p^.left^.symtableentry^.mangledname))));
                          end;   *)
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
                        curconstsegment^.concat(new(pai_const_symbol,
                          initname(pobjectdef(p^.left^.resulttype)^.vmt_mangledname)));
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
                        for l:=0 to def^.size-1 do
                          curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[l])));
{$endif}
{$ifdef m68k}
                        j:=0;
                        for l:=0 to ((def^.size-1) div 4) do
                        { HORRIBLE HACK because of endian       }
                        { now use intel endian for constant sets }
                         begin
                           curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+3])));
                           curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+2])));
                           curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[j+1])));
                           curconstsegment^.concat(new(pai_const,init_8bit(p^.value_set^[j])));
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
                     curconstsegment^.concat(new(pai_const,init_32bit(p^.value)))
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
                           curconstsegment^.concat(new(pai_const,init_8bit(strlength)));
                           { this can also handle longer strings }
                           getmem(ca,strlength+1);
                           move(p^.value_str^,ca^,strlength);
                           ca[strlength]:=#0;
                           curconstsegment^.concat(new(pai_string,init_length_pchar(ca,strlength)));
                        end
                      else if is_constcharnode(p) then
                        begin
                           curconstsegment^.concat(new(pai_string,init(#1+char(byte(p^.value)))));
                           strlength:=1;
                        end
                      else Message(cg_e_illegal_expression);

                      if def^.size>strlength then
                        begin
                           getmem(ca,def^.size-strlength);
                           { def^.size contains also the leading length, so we }
                           { we have to subtract one                       }
                           fillchar(ca[0],def^.size-strlength-1,' ');
                           ca[def^.size-strlength-1]:=#0;
                           { this can also handle longer strings }
                           curconstsegment^.concat(new(pai_string,init_length_pchar(ca,def^.size-strlength-1)));
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
                     curconstsegment^.concat(new(pai_const,init_32bit(strlength)))));
                     { fill byte }
                     curconstsegment^.concat(new(pai_const,init_8bit(0)));
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
                     curconstsegment^.concat(new(pai_const,init_8bit(0)));
                   end;
{$endif UseLongString}
                 st_ansistring:
                   begin
                      { an empty ansi string is nil! }
                      if (p^.treetype=stringconstn) and (p^.length=0) then
                        curconstsegment^.concat(new(pai_const,init_32bit(0)))
                      else
                        begin
                           if is_constcharnode(p) then
                            strlength:=1
                           else
                            strlength:=p^.length;
                           getdatalabel(ll);
                           curconstsegment^.concat(new(pai_const_symbol,init(ll)));
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
                         readtypedconst(parraydef(def)^.definition,nil,no_change_allowed);
                         consume(COMMA);
                      end;
                    readtypedconst(parraydef(def)^.definition,nil,no_change_allowed);
                    consume(RKLAMMER);
                 end
              else
              { if array of char then we allow also a string }
               if is_char(parraydef(def)^.definition) then
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
                     begin
                       Message(cg_e_illegal_expression);
                       s:='';
                     end;
                   disposetree(p);
                   l:=length(s);
                   for i:=Parraydef(def)^.lowrange to Parraydef(def)^.highrange do
                     begin
                        if i+1-Parraydef(def)^.lowrange<=l then
                          begin
                             curconstsegment^.concat(new(pai_const,init_8bit(byte(s[1]))));
                             delete(s,1,1);
                          end
                        else
                          {Fill the remaining positions with #0.}
                          curconstsegment^.concat(new(pai_const,init_8bit(0)));
                     end;
                   if length(s)>0 then
                     Message(parser_e_string_larger_array);
                end
              else
                begin
                  { we want the ( }
                  consume(LKLAMMER);
                end;
           end;
         procvardef:
           begin
              { Procvars and pointers are no longer compatible.  }
              { under tp:  =nil or =var under fpc: =nil or =@var }
              if token=_NIL then
                begin
                   curconstsegment^.concat(new(pai_const,init_32bit(0)));
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
                   curconstsegment^.concat(new(pai_const_symbol,initname(pd^.mangledname)));
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
                            curconstsegment^.concat(new(pai_const,init_8bit(0)));

                        { new position }
                        aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.definition^.size;

                        { read the data }
                        readtypedconst(pvarsym(srsym)^.definition,nil,no_change_allowed);

                        if token=SEMICOLON then
                          consume(SEMICOLON)
                        else break;
                     end;
                end;
              for i:=1 to def^.size-aktpos do
                curconstsegment^.concat(new(pai_const,init_8bit(0)));
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
                                 curconstsegment^.concat(new(pai_const,init_8bit(0)));

                             { new position }
                             aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.definition^.size;

                             { read the data }
                             readtypedconst(pvarsym(srsym)^.definition,nil,no_change_allowed);

                             if token=SEMICOLON then
                               consume(SEMICOLON)
                             else break;
                          end;
                     end;
                   for i:=1 to def^.size-aktpos do
                     curconstsegment^.concat(new(pai_const,init_8bit(0)));
                   consume(RKLAMMER);
                end;
           end;
         errordef:
           begin
              { try to consume something useful }
              if token=LKLAMMER then
                consume_all_until(RKLAMMER)
              else
                consume_all_until(SEMICOLON);
           end;
         else Message(parser_e_type_const_not_possible);
         end;
      end;

end.
{
  $Log$
  Revision 1.48  1999-07-23 16:05:26  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

  Revision 1.47  1999/07/03 14:14:28  florian
    + start of val(int64/qword)
    * longbool, wordbool constants weren't written, fixed

  Revision 1.46  1999/05/27 19:44:54  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.45  1999/05/23 18:42:13  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.44  1999/05/21 13:55:11  peter
    * NEWLAB for label as symbol

  Revision 1.43  1999/05/12 00:19:54  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.42  1999/05/06 09:05:24  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.41  1999/05/01 13:24:39  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.40  1999/04/25 22:42:17  pierre
   + code for initialized vars in Delphi mode

  Revision 1.39  1999/03/24 23:17:21  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.38  1999/02/25 21:02:45  peter
    * ag386bin updates
    + coff writer

  Revision 1.37  1999/02/22 02:44:13  peter
    * ag386bin doesn't use i386.pas anymore

  Revision 1.36  1999/02/17 10:15:26  peter
    * fixed error messages when parsing typed const array

  Revision 1.35  1999/01/20 14:09:28  pierre
   * fix to bug0191

  Revision 1.34  1999/01/05 08:20:08  florian
    * mainly problem with invalid case ranges fixed (reported by Jonas)

  Revision 1.33  1998/12/15 17:16:01  peter
    * fixed const s : ^string
    * first things for const pchar : @string[1]

  Revision 1.32  1998/12/11 16:50:23  florian
    + typed const int64 and qword
    + unary minus-operator  q1:=-q2;
    + not-operator

  Revision 1.31  1998/12/11 00:03:41  peter
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
