{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
{$ifdef Delphi}
       sysutils,
{$else}
       strings,
{$endif Delphi}
       globtype,systems,tokens,
       cobjects,globals,scanner,
       symconst,aasm,types,verbose,
       tree,pass_1,
       { parser specific stuff }
       pbase,pexpr,
       { processor specific stuff }
       cpubase,
       { codegen }
{$ifdef newcg}
       cgbase,
{$else}
       hcodegen,
{$endif}
       hcgdata;


{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}
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
         ll        : pasmlabel;
         s         : string;
         ca        : pchar;
         aktpos    : longint;
         obj       : pobjectdef;
         symt      : psymtable;
         value     : bestreal;
         strval    : pchar;

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

(*      function is_po_equal(o1,o2:longint):boolean;
        begin
        { assembler does not affect }
          is_po_equal:=(o1 and not(poassembler))=
                       (o2 and not(poassembler));
        end; *)

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
                    uwidechar : begin
                                if not is_constcharnode(p) then
                                  Message(cg_e_illegal_expression);
                                curconstsegment^.concat(new(pai_const,init_16bit(p^.value)));
                            end;
                    u16bit,
                    s16bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression);
                                curconstsegment^.concat(new(pai_const,init_16bit(p^.value)));
                                check_range;
                            end;
                    s64bit,
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
         classrefdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              case p^.treetype of
                 loadvmtn:
                   begin
                      if not(pobjectdef(pclassrefdef(p^.resulttype)^.pointertype.def)^.is_related(
                        pobjectdef(pclassrefdef(def)^.pointertype.def))) then
                        Message(cg_e_illegal_expression);
                      curconstsegment^.concat(new(pai_const_symbol,init(newasmsymbol(pobjectdef(
                        pclassrefdef(p^.resulttype)^.pointertype.def)^.vmt_mangledname))));
                   end;
                 niln:
                   curconstsegment^.concat(new(pai_const,init_32bit(0)));
                 else Message(cg_e_illegal_expression);
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
                if is_char(ppointerdef(def)^.pointertype.def) and
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
                    if (is_equal(ppointerdef(p^.resulttype)^.pointertype.def,ppointerdef(def)^.pointertype.def) or
                       (is_equal(ppointerdef(p^.resulttype)^.pointertype.def,voiddef)) or
                       (is_equal(ppointerdef(def)^.pointertype.def,voiddef))) and
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
                             while token=_POINT do
                               begin
                                  consume(_POINT);
                                  lsym:=pvarsym(precdef(
                                        ppointerdef(p^.resulttype)^.pointertype.def)^.symtable^.search(pattern));
                                  if assigned(sym) then
                                    offset:=offset+lsym^.address
                                  else
                                    begin
                                       Message1(sym_e_illegal_field,pattern);
                                    end;
                                  consume(_ID);
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
              { load strval and strlength of the constant tree }
              if p^.treetype=stringconstn then
                begin
                  strlength:=p^.length;
                  strval:=p^.value_str;
                end
              else if is_constcharnode(p) then
                begin
                  strval:=pchar(@p^.value);
                  strlength:=1
                end
              else if is_constresourcestringnode(p) then
                begin
                  strval:=pchar(pconstsym(p^.symtableentry)^.value);
                  strlength:=pconstsym(p^.symtableentry)^.len;
                end
              else
                begin
                  Message(cg_e_illegal_expression);
                  strlength:=-1;
                end;
              if strlength>=0 then
               begin
                 case pstringdef(def)^.string_typ of
                   st_shortstring:
                     begin
                       if strlength>=def^.size then
                        begin
                          message2(parser_w_string_too_long,strpas(strval),tostr(def^.size-1));
                          strlength:=def^.size-1;
                        end;
                       curconstsegment^.concat(new(pai_const,init_8bit(strlength)));
                       { this can also handle longer strings }
                       getmem(ca,strlength+1);
                       move(strval^,ca^,strlength);
                       ca[strlength]:=#0;
                       curconstsegment^.concat(new(pai_string,init_length_pchar(ca,strlength)));
                       { fillup with spaces if size is shorter }
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
                       { first write the maximum size }
                       curconstsegment^.concat(new(pai_const,init_32bit(strlength)))));
                       { fill byte }
                       curconstsegment^.concat(new(pai_const,init_8bit(0)));
                       getmem(ca,strlength+1);
                       move(strval^,ca^,strlength);
                       ca[strlength]:=#0;
                       generate_pascii(consts,ca,strlength);
                       curconstsegment^.concat(new(pai_const,init_8bit(0)));
                     end;
{$endif UseLongString}
                   st_ansistring:
                     begin
                        { an empty ansi string is nil! }
                        if (strlength=0) then
                          curconstsegment^.concat(new(pai_const,init_32bit(0)))
                        else
                          begin
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
                            getmem(ca,strlength+2);
                            move(strval^,ca^,strlength);
                            { The terminating #0 to be stored in the .data section (JM) }
                            ca[strlength]:=#0;
                            { End of the PChar. The memory has to be allocated because in }
                            { tai_string.done, there is a freemem(len+1) (JM)             } 
                            ca[strlength+1]:=#0;
                            consts^.concat(new(pai_string,init_length_pchar(ca,strlength+1)));
                          end;
                     end;
                 end;
               end;
              disposetree(p);
           end;
         arraydef:
           begin
              if token=_LKLAMMER then
                begin
                    consume(_LKLAMMER);
                    for l:=parraydef(def)^.lowrange to parraydef(def)^.highrange-1 do
                      begin
                         readtypedconst(parraydef(def)^.elementtype.def,nil,no_change_allowed);
                         consume(_COMMA);
                      end;
                    readtypedconst(parraydef(def)^.elementtype.def,nil,no_change_allowed);
                    consume(_RKLAMMER);
                 end
              else
              { if array of char then we allow also a string }
               if is_char(parraydef(def)^.elementtype.def) then
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
                  consume(_LKLAMMER);
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
                  if token=_KLAMMERAFFE then
                    consume(_KLAMMERAFFE);
              getprocvar:=true;
              getprocvardef:=pprocvardef(def);
              p:=comp_expr(true);
              getprocvar:=false;
              do_firstpass(p);
              if codegenerror then
               begin
                 disposetree(p);
                 exit;
               end;
              { convert calln to loadn }
              if p^.treetype=calln then
               begin
                 if (p^.symtableprocentry^.owner^.symtabletype=objectsymtable) and
                    (pobjectdef(p^.symtableprocentry^.owner^.defowner)^.is_class) then
                  hp:=genloadmethodcallnode(pprocsym(p^.symtableprocentry),p^.symtableproc,
                        getcopy(p^.methodpointer))
                 else
                  hp:=genloadcallnode(pprocsym(p^.symtableprocentry),p^.symtableproc);
                 disposetree(p);
                 do_firstpass(hp);
                 p:=hp;
                 if codegenerror then
                  begin
                    disposetree(p);
                    exit;
                  end;
               end
              else if (p^.treetype=addrn) and assigned(p^.left) and
                (p^.left^.treetype=calln) then
                begin
                   if (p^.left^.symtableprocentry^.owner^.symtabletype=objectsymtable) and
                      (pobjectdef(p^.left^.symtableprocentry^.owner^.defowner)^.is_class) then
                    hp:=genloadmethodcallnode(pprocsym(p^.left^.symtableprocentry),
                    p^.left^.symtableproc,getcopy(p^.left^.methodpointer))
                   else
                    hp:=genloadcallnode(pprocsym(p^.left^.symtableprocentry),
                      p^.left^.symtableproc);
                   disposetree(p);
                   do_firstpass(hp);
                   p:=hp;
                   if codegenerror then
                    begin
                       disposetree(p);
                       exit;
                    end;
                end;
              { let type conversion check everything needed }
              p:=gentypeconvnode(p,def);
              do_firstpass(p);
              if codegenerror then
               begin
                 disposetree(p);
                 exit;
               end;
              { remove typeconvn, that will normally insert a lea
                instruction which is not necessary for us }
              if p^.treetype=typeconvn then
               begin
                 hp:=p^.left;
                 putnode(p);
                 p:=hp;
               end;
              { remove addrn which we also don't need here }
              if p^.treetype=addrn then
               begin
                 hp:=p^.left;
                 putnode(p);
                 p:=hp;
               end;
              { we now need to have a loadn with a procsym }
              if (p^.treetype=loadn) and
                 (p^.symtableentry^.typ=procsym) then
               begin
                 curconstsegment^.concat(new(pai_const_symbol,
                   initname(pprocsym(p^.symtableentry)^.definition^.mangledname)));
               end
              else
               Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         { reads a typed constant record }
         recorddef:
           begin
              consume(_LKLAMMER);
              aktpos:=0;
              while token<>_RKLAMMER do
                begin
                   s:=pattern;
                   consume(_ID);
                   consume(_COLON);
                   srsym:=precorddef(def)^.symtable^.search(s);
                   if srsym=nil then
                     begin
                        Message1(sym_e_id_not_found,s);
                        consume_all_until(_SEMICOLON);
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
                        aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.vartype.def^.size;

                        { read the data }
                        readtypedconst(pvarsym(srsym)^.vartype.def,nil,no_change_allowed);

                        if token=_SEMICOLON then
                          consume(_SEMICOLON)
                        else break;
                     end;
                end;
              for i:=1 to def^.size-aktpos do
                curconstsegment^.concat(new(pai_const,init_8bit(0)));
              consume(_RKLAMMER);
           end;
         { reads a typed object }
         objectdef:
           begin
              if ([oo_has_vmt,oo_is_class]*pobjectdef(def)^.objectoptions)<>[] then
                begin
                   Message(parser_e_type_const_not_possible);
                   consume_all_until(_RKLAMMER);
                end
              else
                begin
                   consume(_LKLAMMER);
                   aktpos:=0;
                   while token<>_RKLAMMER do
                     begin
                        s:=pattern;
                        consume(_ID);
                        consume(_COLON);
                        srsym:=nil;
                        obj:=pobjectdef(def);
                        symt:=obj^.symtable;
                        while (srsym=nil) and assigned(symt) do
                          begin
                             srsym:=symt^.search(s);
                             if assigned(obj) then
                               obj:=obj^.childof;
                             if assigned(obj) then
                               symt:=obj^.symtable
                             else
                               symt:=nil;
                          end;

                        if srsym=nil then
                          begin
                             Message1(sym_e_id_not_found,s);
                             consume_all_until(_SEMICOLON);
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
                             aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.vartype.def^.size;

                             { read the data }
                             readtypedconst(pvarsym(srsym)^.vartype.def,nil,no_change_allowed);

                             if token=_SEMICOLON then
                               consume(_SEMICOLON)
                             else break;
                          end;
                     end;
                   for i:=1 to def^.size-aktpos do
                     curconstsegment^.concat(new(pai_const,init_8bit(0)));
                   consume(_RKLAMMER);
                end;
           end;
         errordef:
           begin
              { try to consume something useful }
              if token=_LKLAMMER then
                consume_all_until(_RKLAMMER)
              else
                consume_all_until(_SEMICOLON);
           end;
         else Message(parser_e_type_const_not_possible);
         end;
      end;
{$ifdef fpc}
{$maxfpuregisters default}
{$endif fpc}

end.
{
  $Log$
  Revision 1.2  2000-07-13 11:32:47  michael
  + removed logs

}
