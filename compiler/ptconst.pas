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

{$i defines.inc}

interface

   uses symtype,symsym;

    { this procedure reads typed constants }
    { sym is only needed for ansi strings  }
    { the assembler label is in the middle (PM) }
    procedure readtypedconst(const t:ttype;sym : ptypedconstsym;no_change_allowed : boolean);

implementation

    uses
{$ifdef Delphi}
       sysutils,
{$else}
       strings,
{$endif Delphi}
       globtype,systems,tokens,cpuinfo,
       cutils,globals,scanner,
       symconst,symbase,symdef,aasm,types,verbose,
       { pass 1 }
       node,pass_1,
       nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,
       { parser specific stuff }
       pbase,pexpr,
       { codegen }
       hcodegen
{$ifdef newcg}
       ,cgbase
{$endif}
       ;

{$ifdef fpc}
  {$maxfpuregisters 0}
{$endif fpc}
    { this procedure reads typed constants }
    procedure readtypedconst(const t:ttype;sym : ptypedconstsym;no_change_allowed : boolean);

      var
{$ifdef m68k}
         j : longint;
{$endif m68k}
         len,base  : longint;
         p,hp      : tnode;
         i,l,offset,
         strlength : longint;
         curconstsegment : TAAsmoutput;
         ll        : pasmlabel;
         s         : string;
         ca        : pchar;
         tmpguid   : tguid;
         aktpos    : longint;
         obj       : pobjectdef;
         srsym     : psym;
         symt      : psymtable;
         value     : bestreal;
         strval    : pchar;

      procedure check_range(def:porddef);
        begin
           if ((tordconstnode(p).value>def^.high) or
               (tordconstnode(p).value<def^.low)) then
             begin
                if (cs_check_range in aktlocalswitches) then
                  Message(parser_e_range_check_error)
                else
                  Message(parser_w_range_check_error);
             end;
        end;

{$R-}  {Range check creates problem with init_8bit(-1) !!}
      begin
         if no_change_allowed then
           curconstsegment:=consts
         else
           curconstsegment:=datasegment;
         case t.def^.deftype of
            orddef:
              begin
                 p:=comp_expr(true);
                 case porddef(t.def)^.typ of
                    bool8bit :
                      begin
                         if is_constboolnode(p) then
                           curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value))
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    bool16bit :
                      begin
                         if is_constboolnode(p) then
                           curconstSegment.concat(Tai_const.Create_16bit(tordconstnode(p).value))
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    bool32bit :
                      begin
                         if is_constboolnode(p) then
                           curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value))
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    uchar :
                      begin
                         if is_constcharnode(p) then
                           curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value))
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    uwidechar :
                      begin
                         if is_constcharnode(p) then
                           curconstSegment.concat(Tai_const.Create_16bit(tordconstnode(p).value))
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    s8bit,
                    u8bit :
                      begin
                         if is_constintnode(p) then
                           begin
                              curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value));
                              check_range(porddef(t.def));
                           end
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    u16bit,
                    s16bit :
                      begin
                         if is_constintnode(p) then
                           begin
                             curconstSegment.concat(Tai_const.Create_16bit(tordconstnode(p).value));
                             check_range(porddef(t.def));
                           end
                         else
                           Message(cg_e_illegal_expression);
                     end;
                    s32bit,
                    u32bit :
                      begin
                         if is_constintnode(p) then
                           begin
                              curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value));
                              if porddef(t.def)^.typ<>u32bit then
                               check_range(porddef(t.def));
                           end
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    s64bit,
                    u64bit:
                      begin
                         if is_constintnode(p) then
                           begin
                              {!!!!! hmmm, we can write yet only consts til 2^32-1 :( (FK) }
                              curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value));
                              curconstSegment.concat(Tai_const.Create_32bit(0));
                           end
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    else
                      internalerror(3799);
                 end;
                 p.free;
              end;
         floatdef:
           begin
              p:=comp_expr(true);
              if is_constrealnode(p) then
                value:=trealconstnode(p).value_real
              else if is_constintnode(p) then
                value:=tordconstnode(p).value
              else
                Message(cg_e_illegal_expression);

              case pfloatdef(t.def)^.typ of
                 s32real :
                   curconstSegment.concat(Tai_real_32bit.Create(value));
                 s64real :
                   curconstSegment.concat(Tai_real_64bit.Create(value));
                 s80real :
                   curconstSegment.concat(Tai_real_80bit.Create(value));
                 s64comp :
                   curconstSegment.concat(Tai_comp_64bit.Create(value));
                 else
                   internalerror(18);
              end;
              p.free;
           end;
         classrefdef:
           begin
              p:=comp_expr(true);
              case p.nodetype of
                 loadvmtn:
                   begin
                      if not(pobjectdef(pclassrefdef(p.resulttype.def)^.pointertype.def)^.is_related(
                        pobjectdef(pclassrefdef(t.def)^.pointertype.def))) then
                        Message(cg_e_illegal_expression);
                      curconstSegment.concat(Tai_const_symbol.Create(newasmsymbol(pobjectdef(
                        pclassrefdef(p.resulttype.def)^.pointertype.def)^.vmt_mangledname)));
                   end;
                 niln:
                   curconstSegment.concat(Tai_const.Create_32bit(0));
                 else Message(cg_e_illegal_expression);
              end;
              p.free;
           end;
         pointerdef:
           begin
              p:=comp_expr(true);
              if (p.nodetype=typeconvn) and
                 (ttypeconvnode(p).left.nodetype in [addrn,niln]) and
                 is_equal(t.def,p.resulttype.def) then
                begin
                   hp:=ttypeconvnode(p).left;
                   ttypeconvnode(p).left:=nil;
                   p.free;
                   p:=hp;
                end;
              { allows horrible ofs(typeof(TButton)^) code !! }
              if (p.nodetype=addrn) and
                 (taddrnode(p).left.nodetype=derefn) then
                begin
                   hp:=tderefnode(taddrnode(p).left).left;
                   tderefnode(taddrnode(p).left).left:=nil;
                   p.free;
                   p:=hp;
                end;
              { const pointer ? }
              if (p.nodetype = pointerconstn) then
                curconstsegment.concat(Tai_const.Create_32bit(
                  tpointerconstnode(p).value))
              { nil pointer ? }
              else if p.nodetype=niln then
                curconstSegment.concat(Tai_const.Create_32bit(0))
              { maybe pchar ? }
              else
                if is_char(ppointerdef(t.def)^.pointertype.def) and
                   (p.nodetype<>addrn) then
                  begin
                    getdatalabel(ll);
                    curconstSegment.concat(Tai_const_symbol.Create(ll));
                    Consts.concat(Tai_label.Create(ll));
                    if p.nodetype=stringconstn then
                      begin
                        len:=tstringconstnode(p).len;
                        { For tp7 the maximum lentgh can be 255 }
                        if (m_tp in aktmodeswitches) and
                           (len>255) then
                         len:=255;
                        getmem(ca,len+2);
                        move(tstringconstnode(p).value_str^,ca^,len+1);
                        Consts.concat(Tai_string.Create_length_pchar(ca,len+1));
                      end
                    else
                      if is_constcharnode(p) then
                        Consts.concat(Tai_string.Create(char(byte(tordconstnode(p).value))+#0))
                    else
                      Message(cg_e_illegal_expression);
                end
              else
                if p.nodetype=addrn then
                  begin
                    hp:=taddrnode(p).left;
                    while assigned(hp) and (hp.nodetype in [subscriptn,vecn]) do
                      hp:=tbinarynode(hp).left;
                    if (is_equal(ppointerdef(p.resulttype.def)^.pointertype.def,ppointerdef(t.def)^.pointertype.def) or
                       (is_void(ppointerdef(p.resulttype.def)^.pointertype.def)) or
                       (is_void(ppointerdef(t.def)^.pointertype.def))) and
                       (hp.nodetype=loadn) then
                      begin
                        do_resulttypepass(taddrnode(p).left);
                        hp:=taddrnode(p).left;
                        offset:=0;
                        while assigned(hp) and (hp.nodetype<>loadn) do
                          begin
                             case hp.nodetype of
                               vecn :
                                 begin
                                   case tvecnode(hp).left.resulttype.def^.deftype of
                                     stringdef :
                                       begin
                                          { this seems OK for shortstring and ansistrings PM }
                                          { it is wrong for widestrings !! }
                                          len:=1;
                                          base:=0;
                                       end;
                                     arraydef :
                                       begin
                                          len:=parraydef(tvecnode(hp).left.resulttype.def)^.elesize;
                                          base:=parraydef(tvecnode(hp).left.resulttype.def)^.lowrange;
                                       end
                                     else
                                       Message(cg_e_illegal_expression);
                                   end;
                                   if is_constintnode(tvecnode(hp).right) then
                                     inc(offset,len*(get_ordinal_value(tvecnode(hp).right)-base))
                                   else
                                     Message(cg_e_illegal_expression);
                                 end;
                               subscriptn :
                                 inc(offset,tsubscriptnode(hp).vs^.address)
                               else
                                 Message(cg_e_illegal_expression);
                             end;
                             hp:=tbinarynode(hp).left;
                          end;
                        if tloadnode(hp).symtableentry^.typ=constsym then
                          Message(type_e_variable_id_expected);
                        curconstSegment.concat(Tai_const_symbol.Createname_offset(tloadnode(hp).symtableentry^.mangledname,offset));
                      end
                    else
                      Message(cg_e_illegal_expression);
                  end
              else
              { allow typeof(Object type)}
                if (p.nodetype=inlinen) and
                   (tinlinenode(p).inlinenumber=in_typeof_x) then
                  begin
                    if (tinlinenode(p).left.nodetype=typen) then
                      begin
                        curconstSegment.concat(Tai_const_symbol.createname(
                          pobjectdef(tinlinenode(p).left.resulttype.def)^.vmt_mangledname));
                      end
                    else
                      Message(cg_e_illegal_expression);
                  end
              else
                Message(cg_e_illegal_expression);
              p.free;
           end;
         setdef:
           begin
              p:=comp_expr(true);
              if p.nodetype=setconstn then
                begin
                   { we only allow const sets }
                   if assigned(tsetconstnode(p).left) then
                     Message(cg_e_illegal_expression)
                   else
                     begin
{$ifdef i386}
                        for l:=0 to t.def^.size-1 do
                          curconstSegment.concat(Tai_const.Create_8bit(tsetconstnode(p).value_set^[l]));
{$endif}
{$ifdef m68k}
                        j:=0;
                        for l:=0 to ((def^.size-1) div 4) do
                        { HORRIBLE HACK because of endian       }
                        { now use intel endian for constant sets }
                         begin
                           curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value_set^[j+3]));
                           curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value_set^[j+2]));
                           curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value_set^[j+1]));
                           curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value_set^[j]));
                           Inc(j,4);
                         end;
{$endif}
                     end;
                end
              else
                Message(cg_e_illegal_expression);
              p.free;
           end;
         enumdef:
           begin
              p:=comp_expr(true);
              if p.nodetype=ordconstn then
                begin
                  if is_equal(p.resulttype.def,t.def) or
                     is_subequal(p.resulttype.def,t.def) then
                   begin
                     case p.resulttype.def^.size of
                       1 : curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value));
                       2 : curconstSegment.concat(Tai_const.Create_16bit(tordconstnode(p).value));
                       4 : curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value));
                     end;
                   end
                  else
                   Message2(type_e_incompatible_types,t.def^.typename,p.resulttype.def^.typename);
                end
              else
                Message(cg_e_illegal_expression);
              p.free;
           end;
         stringdef:
           begin
              p:=comp_expr(true);
              { load strval and strlength of the constant tree }
              if p.nodetype=stringconstn then
                begin
                  strlength:=tstringconstnode(p).len;
                  strval:=tstringconstnode(p).value_str;
                end
              else if is_constcharnode(p) then
                begin
                  strval:=pchar(@tordconstnode(p).value);
                  strlength:=1
                end
              else if is_constresourcestringnode(p) then
                begin
                  strval:=pchar(tpointerord(pconstsym(tloadnode(p).symtableentry)^.value));
                  strlength:=pconstsym(tloadnode(p).symtableentry)^.len;
                end
              else
                begin
                  Message(cg_e_illegal_expression);
                  strlength:=-1;
                end;
              if strlength>=0 then
               begin
                 case pstringdef(t.def)^.string_typ of
                   st_shortstring:
                     begin
                       if strlength>=t.def^.size then
                        begin
                          message2(parser_w_string_too_long,strpas(strval),tostr(t.def^.size-1));
                          strlength:=t.def^.size-1;
                        end;
                       curconstSegment.concat(Tai_const.Create_8bit(strlength));
                       { this can also handle longer strings }
                       getmem(ca,strlength+1);
                       move(strval^,ca^,strlength);
                       ca[strlength]:=#0;
                       curconstSegment.concat(Tai_string.Create_length_pchar(ca,strlength));
                       { fillup with spaces if size is shorter }
                       if t.def^.size>strlength then
                        begin
                          getmem(ca,t.def^.size-strlength);
                          { def^.size contains also the leading length, so we }
                          { we have to subtract one                       }
                          fillchar(ca[0],t.def^.size-strlength-1,' ');
                          ca[t.def^.size-strlength-1]:=#0;
                          { this can also handle longer strings }
                          curconstSegment.concat(Tai_string.Create_length_pchar(ca,t.def^.size-strlength-1));
                        end;
                     end;
{$ifdef UseLongString}
                   st_longstring:
                     begin
                       { first write the maximum size }
                       curconstSegment.concat(Tai_const.Create_32bit(strlength))));
                       { fill byte }
                       curconstSegment.concat(Tai_const.Create_8bit(0));
                       getmem(ca,strlength+1);
                       move(strval^,ca^,strlength);
                       ca[strlength]:=#0;
                       generate_pascii(consts,ca,strlength);
                       curconstSegment.concat(Tai_const.Create_8bit(0));
                     end;
{$endif UseLongString}
                   st_ansistring:
                     begin
                        { an empty ansi string is nil! }
                        if (strlength=0) then
                          curconstSegment.concat(Tai_const.Create_32bit(0))
                        else
                          begin
                            getdatalabel(ll);
                            curconstSegment.concat(Tai_const_symbol.Create(ll));
                            { first write the maximum size }
                            Consts.concat(Tai_const.Create_32bit(strlength));
                            { second write the real length }
                            Consts.concat(Tai_const.Create_32bit(strlength));
                            { redondent with maxlength but who knows ... (PM) }
                            { third write use count (set to -1 for safety ) }
                            Consts.concat(Tai_const.Create_32bit(-1));
                            Consts.concat(Tai_label.Create(ll));
                            getmem(ca,strlength+2);
                            move(strval^,ca^,strlength);
                            { The terminating #0 to be stored in the .data section (JM) }
                            ca[strlength]:=#0;
                            { End of the PChar. The memory has to be allocated because in }
                            { tai_string.done, there is a freemem(len+1) (JM)             }
                            ca[strlength+1]:=#0;
                            Consts.concat(Tai_string.Create_length_pchar(ca,strlength+1));
                          end;
                     end;
                 end;
               end;
              p.free;
           end;
         arraydef:
           begin
              if token=_LKLAMMER then
                begin
                    consume(_LKLAMMER);
                    for l:=parraydef(t.def)^.lowrange to parraydef(t.def)^.highrange-1 do
                      begin
                         readtypedconst(parraydef(t.def)^.elementtype,nil,no_change_allowed);
                         consume(_COMMA);
                      end;
                    readtypedconst(parraydef(t.def)^.elementtype,nil,no_change_allowed);
                    consume(_RKLAMMER);
                 end
              else
              { if array of char then we allow also a string }
               if is_char(parraydef(t.def)^.elementtype.def) then
                begin
                   p:=comp_expr(true);
                   if p.nodetype=stringconstn then
                    begin
                      len:=tstringconstnode(p).len;
                      { For tp7 the maximum lentgh can be 255 }
                      if (m_tp in aktmodeswitches) and
                         (len>255) then
                       len:=255;
                      ca:=tstringconstnode(p).value_str;
                    end
                   else
                     if is_constcharnode(p) then
                      begin
                        ca:=pchar(@tordconstnode(p).value);
                        len:=1;
                      end
                   else
                     begin
                       Message(cg_e_illegal_expression);
                       len:=0;
                     end;
                   if len>(Parraydef(t.def)^.highrange-Parraydef(t.def)^.lowrange+1) then
                     Message(parser_e_string_larger_array);
                   for i:=Parraydef(t.def)^.lowrange to Parraydef(t.def)^.highrange do
                     begin
                        if i+1-Parraydef(t.def)^.lowrange<=len then
                          begin
                             curconstSegment.concat(Tai_const.Create_8bit(byte(ca^)));
                             inc(ca);
                          end
                        else
                          {Fill the remaining positions with #0.}
                          curconstSegment.concat(Tai_const.Create_8bit(0));
                     end;
                   p.free;
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
                   curconstSegment.concat(Tai_const.Create_32bit(0));
                   consume(_NIL);
                   exit;
                end
              else
                if not(m_tp_procvar in aktmodeswitches) then
                  if token=_KLAMMERAFFE then
                    consume(_KLAMMERAFFE);
              getprocvar:=true;
              getprocvardef:=pprocvardef(t.def);
              p:=comp_expr(true);
              getprocvar:=false;
              if codegenerror then
               begin
                 p.free;
                 exit;
               end;
              { convert calln to loadn }
              if p.nodetype=calln then
               begin
                 hp:=cloadnode.create(pprocsym(tcallnode(p).symtableprocentry),tcallnode(p).symtableproc);
                 if (tcallnode(p).symtableprocentry^.owner^.symtabletype=objectsymtable) and
                    is_class(pdef(tcallnode(p).symtableprocentry^.owner^.defowner)) then
                  tloadnode(hp).set_mp(tcallnode(p).methodpointer.getcopy);
                 p.free;
                 do_resulttypepass(hp);
                 p:=hp;
                 if codegenerror then
                  begin
                    p.free;
                    exit;
                  end;
               end
              else if (p.nodetype=addrn) and assigned(taddrnode(p).left) and
                (taddrnode(p).left.nodetype=calln) then
                begin
                   hp:=cloadnode.create(pprocsym(tcallnode(taddrnode(p).left).symtableprocentry),
                     tcallnode(taddrnode(p).left).symtableproc);
                   if (tcallnode(taddrnode(p).left).symtableprocentry^.owner^.symtabletype=objectsymtable) and
                      is_class(pdef(tcallnode(taddrnode(p).left).symtableprocentry^.owner^.defowner)) then
                    tloadnode(hp).set_mp(tcallnode(taddrnode(p).left).methodpointer.getcopy);
                   p.free;
                   do_resulttypepass(hp);
                   p:=hp;
                   if codegenerror then
                    begin
                       p.free;
                       exit;
                    end;
                end;
              { let type conversion check everything needed }
              inserttypeconv(p,t);
              if codegenerror then
               begin
                 p.free;
                 exit;
               end;
              { remove typeconvn, that will normally insert a lea
                instruction which is not necessary for us }
              if p.nodetype=typeconvn then
               begin
                 hp:=ttypeconvnode(p).left;
                 ttypeconvnode(p).left:=nil;
                 p.free;
                 p:=hp;
               end;
              { remove addrn which we also don't need here }
              if p.nodetype=addrn then
               begin
                 hp:=taddrnode(p).left;
                 taddrnode(p).left:=nil;
                 p.free;
                 p:=hp;
               end;
              { we now need to have a loadn with a procsym }
              if (p.nodetype=loadn) and
                 (tloadnode(p).symtableentry^.typ=procsym) then
               begin
                 curconstSegment.concat(Tai_const_symbol.createname(
                   pprocsym(tloadnode(p).symtableentry)^.definition^.mangledname));
               end
              else
               Message(cg_e_illegal_expression);
              p.free;
           end;
         { reads a typed constant record }
         recorddef:
           begin
              { KAZ }
              if (precorddef(t.def)=rec_tguid) and
                 ((token=_CSTRING) or (token=_CCHAR) or (token=_ID)) then
                begin
                  p:=comp_expr(true);
                  inserttypeconv(p,cshortstringtype);
                  if p.nodetype=stringconstn then
                    begin
                      s:=strpas(tstringconstnode(p).value_str);
                      p.free;
                      if string2guid(s,tmpguid) then
                        begin
                          curconstSegment.concat(Tai_const.Create_32bit(tmpguid.D1));
                          curconstSegment.concat(Tai_const.Create_16bit(tmpguid.D2));
                          curconstSegment.concat(Tai_const.Create_16bit(tmpguid.D3));
                          for i:=Low(tmpguid.D4) to High(tmpguid.D4) do
                            curconstSegment.concat(Tai_const.Create_8bit(tmpguid.D4[i]));
                        end
                      else
                        Message(parser_e_improper_guid_syntax);
                    end
                  else
                    begin
                      p.free;
                      Message(cg_e_illegal_expression);
                      exit;
                    end;
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
                        srsym:=psym(precorddef(t.def)^.symtable^.search(s));
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
                                 curconstSegment.concat(Tai_const.Create_8bit(0));

                             { new position }
                             aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.vartype.def^.size;

                             { read the data }
                             readtypedconst(pvarsym(srsym)^.vartype,nil,no_change_allowed);

                             if token=_SEMICOLON then
                               consume(_SEMICOLON)
                             else break;
                          end;
                   end;
                 for i:=1 to t.def^.size-aktpos do
                   curconstSegment.concat(Tai_const.Create_8bit(0));
                 consume(_RKLAMMER);
              end;
           end;
         { reads a typed object }
         objectdef:
           begin
              if is_class_or_interface(t.def) then
                begin
                  p:=comp_expr(true);
                  if p.nodetype<>niln then
                    begin
                      Message(parser_e_type_const_not_possible);
                      consume_all_until(_RKLAMMER);
                    end
                  else
                    begin
                      curconstSegment.concat(Tai_const.Create_32bit(0));
                    end;
                  p.free;
                end
              { for objects we allow it only if it doesn't contain a vmt }
              else if (oo_has_vmt in pobjectdef(t.def)^.objectoptions) and
                      not(m_tp in aktmodeswitches) then
                 Message(parser_e_type_const_not_possible)
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
                        obj:=pobjectdef(t.def);
                        symt:=obj^.symtable;
                        while (srsym=nil) and assigned(symt) do
                          begin
                             srsym:=psym(symt^.search(s));
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

                             { check in VMT needs to be added for TP mode }
                             if (m_tp in aktmodeswitches) and
                                (oo_has_vmt in pobjectdef(t.def)^.objectoptions) and
                                (pobjectdef(t.def)^.vmt_offset<pvarsym(srsym)^.address) then
                               begin
                                 for i:=1 to pobjectdef(t.def)^.vmt_offset-aktpos do
                                   curconstsegment.concat(tai_const.create_8bit(0));
                                 curconstsegment.concat(tai_const_symbol.createname(pobjectdef(t.def)^.vmt_mangledname));
                                 { this is more general }
                                 aktpos:=pobjectdef(t.def)^.vmt_offset + target_os.size_of_pointer;
                               end;

                             { if needed fill }
                             if pvarsym(srsym)^.address>aktpos then
                               for i:=1 to pvarsym(srsym)^.address-aktpos do
                                 curconstSegment.concat(Tai_const.Create_8bit(0));

                             { new position }
                             aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.vartype.def^.size;

                             { read the data }
                             readtypedconst(pvarsym(srsym)^.vartype,nil,no_change_allowed);

                             if token=_SEMICOLON then
                               consume(_SEMICOLON)
                             else break;
                          end;
                     end;
                   if (m_tp in aktmodeswitches) and
                      (oo_has_vmt in pobjectdef(t.def)^.objectoptions) and
                      (pobjectdef(t.def)^.vmt_offset>=aktpos) then
                     begin
                       for i:=1 to pobjectdef(t.def)^.vmt_offset-aktpos do
                         curconstsegment.concat(tai_const.create_8bit(0));
                       curconstsegment.concat(tai_const_symbol.createname(pobjectdef(t.def)^.vmt_mangledname));
                       { this is more general }
                       aktpos:=pobjectdef(t.def)^.vmt_offset + target_os.size_of_pointer;
                     end;
                   for i:=1 to t.def^.size-aktpos do
                     curconstSegment.concat(Tai_const.Create_8bit(0));
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
  Revision 1.20  2001-04-04 22:43:53  peter
    * remove unnecessary calls to firstpass

  Revision 1.19  2001/04/02 21:20:34  peter
    * resulttype rewrite

  Revision 1.18  2001/03/11 22:58:50  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.17  2001/02/04 11:12:16  jonas
    * fixed web bug 1377 & const pointer arithmtic

  Revision 1.16  2001/02/03 00:26:35  peter
    * merged fix for bug 1365

  Revision 1.15  2000/12/25 00:07:28  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.14  2000/12/10 20:24:18  peter
    * allow subtypes for enums

  Revision 1.13  2000/11/29 00:30:38  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.12  2000/11/06 15:54:15  florian
    * fixed two bugs to get make cycle work, but it's not enough

  Revision 1.11  2000/11/04 14:25:21  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.10  2000/10/31 22:02:51  peter
    * symtable splitted, no real code changes

  Revision 1.9  2000/10/14 10:14:52  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.8  2000/09/30 13:23:04  peter
    * const array of char and pchar length fixed (merged)

  Revision 1.7  2000/09/24 15:06:25  peter
    * use defines.inc

  Revision 1.6  2000/08/27 16:11:52  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.5  2000/08/24 19:13:18  peter
    * allow nil for class typed consts (merged)

  Revision 1.4  2000/08/16 13:06:06  florian
    + support of 64 bit integer constants

  Revision 1.3  2000/08/05 13:25:06  peter
    * packenum 1 fixes (merged)

  Revision 1.2  2000/07/13 11:32:47  michael
  + removed logs

}
