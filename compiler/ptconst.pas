{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

   uses symtype,symsym;

    { this procedure reads typed constants }
    { sym is only needed for ansi strings  }
    { the assembler label is in the middle (PM) }
    procedure readtypedconst(const t:ttype;sym : ttypedconstsym;writable : boolean);

implementation

    uses
{$ifdef Delphi}
       sysutils,
{$else}
       strings,
{$endif Delphi}
       globtype,systems,tokens,verbose,
       cutils,globals,widestr,scanner,
       symconst,symbase,symdef,aasmbase,aasmtai,aasmcpu,defutil,defcmp,
       { pass 1 }
       node,
       nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,
       { parser specific stuff }
       pbase,pexpr,
       { codegen }
       cpuinfo,cgbase
       ;

{$ifdef fpc}
  {$maxfpuregisters 0}
{$endif fpc}
    { this procedure reads typed constants }
    procedure readtypedconst(const t:ttype;sym : ttypedconstsym;writable : boolean);

      var
         len,base  : longint;
         p,hp,hpstart : tnode;
         i,j,l,offset,
         varalign,
         strlength : longint;
         curconstsegment : TAAsmoutput;
         ll        : tasmlabel;
         s,sorg    : string;
         c         : char;
         ca        : pchar;
         tmpguid   : tguid;
         aktpos    : longint;
         obj       : tobjectdef;
         recsym,
         srsym     : tsym;
         symt      : tsymtable;
         value     : bestreal;
         strval    : pchar;
         pw        : pcompilerwidestring;
         error     : boolean;

      type
         setbytes = array[0..31] of byte;
         Psetbytes = ^setbytes;

      procedure check_range(def:torddef);
        begin
           if ((tordconstnode(p).value>def.high) or
               (tordconstnode(p).value<def.low)) then
             begin
                if (cs_check_range in aktlocalswitches) then
                  Message(parser_e_range_check_error)
                else
                  Message(parser_w_range_check_error);
             end;
        end;

{$R-}  {Range check creates problem with init_8bit(-1) !!}
      begin
         if writable then
           curconstsegment:=datasegment
         else
           curconstsegment:=consts;
         case t.def.deftype of
            orddef:
              begin
                 p:=comp_expr(true);
                 case torddef(t.def).typ of
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
                           inserttypeconv(p,cwidechartype);
                         if is_constwidecharnode(p) then
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
                              check_range(torddef(t.def));
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
                             check_range(torddef(t.def));
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
                              if torddef(t.def).typ<>u32bit then
                               check_range(torddef(t.def));
                           end
                         else
                           Message(cg_e_illegal_expression);
                      end;
                    s64bit,
                    u64bit:
                      begin
                         if is_constintnode(p) then
                           begin
                              if target_info.endian = endian_little then
                                begin
                                  curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value and $ffffffff));
                                  curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value shr 32));
                                end
                              else
                                begin
                                  curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value shr 32));
                                  curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value  and $ffffffff));
                                end;
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

              case tfloatdef(t.def).typ of
                 s32real :
                   curconstSegment.concat(Tai_real_32bit.Create(ts32real(value)));
                 s64real :
                   curconstSegment.concat(Tai_real_64bit.Create(ts64real(value)));
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
                      if not(tobjectdef(tclassrefdef(p.resulttype.def).pointertype.def).is_related(
                        tobjectdef(tclassrefdef(t.def).pointertype.def))) then
                        Message(cg_e_illegal_expression);
                      curconstSegment.concat(Tai_const_symbol.Create(objectlibrary.newasmsymbol(tobjectdef(
                        tclassrefdef(p.resulttype.def).pointertype.def).vmt_mangledname)));
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
                 equal_defs(t.def,p.resulttype.def) then
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
                if is_char(tpointerdef(t.def).pointertype.def) and
                   (p.nodetype<>addrn) then
                  begin
                    objectlibrary.getdatalabel(ll);
                    curconstSegment.concat(Tai_const_symbol.Create(ll));
                    if p.nodetype=stringconstn then
                     varalign:=tstringconstnode(p).len
                    else
                     varalign:=0;
                    varalign:=const_align(varalign);
                    Consts.concat(Tai_align.Create(varalign));
                    Consts.concat(Tai_label.Create(ll));
                    if p.nodetype=stringconstn then
                      begin
                        len:=tstringconstnode(p).len;
                        { For tp7 the maximum lentgh can be 255 }
                        if (m_tp7 in aktmodeswitches) and
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
              { maybe pwidechar ? }
              else
                if is_widechar(tpointerdef(t.def).pointertype.def) and
                   (p.nodetype<>addrn) then
                  begin
                    objectlibrary.getdatalabel(ll);
                    curconstSegment.concat(Tai_const_symbol.Create(ll));
                    Consts.concat(tai_align.create(const_align(pointer_size)));
                    Consts.concat(Tai_label.Create(ll));
                    if (p.nodetype in [stringconstn,ordconstn]) then
                      begin
                        { convert to widestring stringconstn }
                        inserttypeconv(p,cwidestringtype);
                        if (p.nodetype=stringconstn) and
                           (tstringconstnode(p).st_type=st_widestring) then
                         begin
                           pw:=pcompilerwidestring(tstringconstnode(p).value_str);
                           for i:=0 to tstringconstnode(p).len-1 do
                            Consts.concat(Tai_const.Create_16bit(pw^.data[i]));
                           { ending #0 }
                           Consts.concat(Tai_const.Create_16bit(0))
                         end;
                      end
                    else
                      Message(cg_e_illegal_expression);
                end
              else
                if p.nodetype=addrn then
                  begin
                    inserttypeconv(p,t);
                    { if a typeconv node was inserted then check if it was an tc_equal. If
                      true then we remove the node. If not tc_equal then we leave the typeconvn
                      and the nodetype=loadn will always be false and generate the error (PFV) }
                    if (p.nodetype=typeconvn) then
                     begin
                       if (ttypeconvnode(p).convtype=tc_equal) then
                        hpstart:=taddrnode(ttypeconvnode(p).left).left
                       else
                        hpstart:=p;
                     end
                    else
                     hpstart:=taddrnode(p).left;
                    hp:=hpstart;
                    while assigned(hp) and (hp.nodetype in [subscriptn,vecn]) do
                      hp:=tunarynode(hp).left;
                    if (hp.nodetype=loadn) then
                      begin
                        hp:=hpstart;
                        offset:=0;
                        while assigned(hp) and (hp.nodetype<>loadn) do
                          begin
                             case hp.nodetype of
                               vecn :
                                 begin
                                   case tvecnode(hp).left.resulttype.def.deftype of
                                     stringdef :
                                       begin
                                          { this seems OK for shortstring and ansistrings PM }
                                          { it is wrong for widestrings !! }
                                          len:=1;
                                          base:=0;
                                       end;
                                     arraydef :
                                       begin
                                          len:=tarraydef(tvecnode(hp).left.resulttype.def).elesize;
                                          base:=tarraydef(tvecnode(hp).left.resulttype.def).lowrange;
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
                                 inc(offset,tsubscriptnode(hp).vs.address)
                               else
                                 Message(cg_e_illegal_expression);
                             end;
                             hp:=tbinarynode(hp).left;
                          end;
                        srsym:=tloadnode(hp).symtableentry;
                        case srsym.typ of
                          procsym :
                            begin
                              if Tprocsym(srsym).procdef_count>1 then
                                Message(parser_e_no_overloaded_procvars);
                              curconstSegment.concat(Tai_const_symbol.Createname_offset(tprocsym(srsym).first_procdef.mangledname,offset));
                            end;
                          varsym :
                            curconstSegment.concat(Tai_const_symbol.Createname_offset(tvarsym(srsym).mangledname,offset));
                          typedconstsym :
                            curconstSegment.concat(Tai_const_symbol.Createname_offset(ttypedconstsym(srsym).mangledname,offset));
                          else
                            Message(type_e_variable_id_expected);
                        end;
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
                          tobjectdef(tinlinenode(p).left.resulttype.def).vmt_mangledname));
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
                   { be sure to convert to the correct result, else
                     it can generate smallset data instead of normalset (PFV) }
                   inserttypeconv(p,t);
                   { we only allow const sets }
                   if assigned(tsetconstnode(p).left) then
                     Message(cg_e_illegal_expression)
                   else
                     begin
                        { this writing is endian independant   }
                        { untrue - because they are considered }
                        { arrays of 32-bit values CEC          }

                        if source_info.endian = target_info.endian then
                          begin
                            for l:=0 to p.resulttype.def.size-1 do
                            {$ifdef oldset}
                               curconstsegment.concat(tai_const.create_8bit(tsetconstnode(p).value_set^[l]));
                            {$else}
                               curconstsegment.concat(tai_const.create_8bit(Psetbytes(tsetconstnode(p).value_set)^[l]));
                            {$endif}
                          end
                        else
                          begin
                            { store as longint values in swaped format }
                            j:=0;
                            for l:=0 to ((p.resulttype.def.size-1) div 4) do
                              begin
                        {$ifdef oldset}
                                curconstsegment.concat(tai_const.create_8bit(tsetconstnode(p).value_set^[j+3]));
                                curconstsegment.concat(tai_const.create_8bit(tsetconstnode(p).value_set^[j+2]));
                                curconstsegment.concat(tai_const.create_8bit(tsetconstnode(p).value_set^[j+1]));
                                curconstsegment.concat(tai_const.create_8bit(tsetconstnode(p).value_set^[j]));
                        {$else}
                                curconstsegment.concat(tai_const.create_8bit(Psetbytes(tsetconstnode(p).value_set)^[j+3]));
                                curconstsegment.concat(tai_const.create_8bit(Psetbytes(tsetconstnode(p).value_set)^[j+2]));
                                curconstsegment.concat(tai_const.create_8bit(Psetbytes(tsetconstnode(p).value_set)^[j+1]));
                                curconstsegment.concat(tai_const.create_8bit(Psetbytes(tsetconstnode(p).value_set)^[j]));
                        {$endif}
                                Inc(j,4);
                              end;
                          end;
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
                  if equal_defs(p.resulttype.def,t.def) or
                     is_subequal(p.resulttype.def,t.def) then
                   begin
                     case p.resulttype.def.size of
                       1 : curconstSegment.concat(Tai_const.Create_8bit(tordconstnode(p).value));
                       2 : curconstSegment.concat(Tai_const.Create_16bit(tordconstnode(p).value));
                       4 : curconstSegment.concat(Tai_const.Create_32bit(tordconstnode(p).value));
                     end;
                   end
                  else
                   Message2(type_e_incompatible_types,t.def.typename,p.resulttype.def.typename);
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
                  { convert to the expected string type so that
                    for widestrings strval is a pcompilerwidestring }
                  inserttypeconv(p,t);
                  strlength:=tstringconstnode(p).len;
                  strval:=tstringconstnode(p).value_str;
                end
              else if is_constcharnode(p) then
                begin
                  { strval:=pchar(@tordconstnode(p).value);
                    THIS FAIL on BIG_ENDIAN MACHINES PM }
                  c:=chr(tordconstnode(p).value and $ff);
                  strval:=@c;
                  strlength:=1
                end
              else if is_constresourcestringnode(p) then
                begin
                  strval:=pchar(tconstsym(tloadnode(p).symtableentry).value.valueptr);
                  strlength:=tconstsym(tloadnode(p).symtableentry).value.len;
                end
              else
                begin
                  Message(cg_e_illegal_expression);
                  strlength:=-1;
                end;
              if strlength>=0 then
               begin
                 case tstringdef(t.def).string_typ of
                   st_shortstring:
                     begin
                       if strlength>=t.def.size then
                        begin
                          message2(parser_w_string_too_long,strpas(strval),tostr(t.def.size-1));
                          strlength:=t.def.size-1;
                        end;
                       curconstSegment.concat(Tai_const.Create_8bit(strlength));
                       { this can also handle longer strings }
                       getmem(ca,strlength+1);
                       move(strval^,ca^,strlength);
                       ca[strlength]:=#0;
                       curconstSegment.concat(Tai_string.Create_length_pchar(ca,strlength));
                       { fillup with spaces if size is shorter }
                       if t.def.size>strlength then
                        begin
                          getmem(ca,t.def.size-strlength);
                          { def.size contains also the leading length, so we }
                          { we have to subtract one                       }
                          fillchar(ca[0],t.def.size-strlength-1,' ');
                          ca[t.def.size-strlength-1]:=#0;
                          { this can also handle longer strings }
                          curconstSegment.concat(Tai_string.Create_length_pchar(ca,t.def.size-strlength-1));
                        end;
                     end;
                   st_ansistring:
                     begin
                        { an empty ansi string is nil! }
                        if (strlength=0) then
                          curconstSegment.concat(Tai_const.Create_32bit(0))
                        else
                          begin
                            objectlibrary.getdatalabel(ll);
                            curconstSegment.concat(Tai_const_symbol.Create(ll));
                            { the actual structure starts at -12 from start label - CEC }
                            Consts.concat(tai_align.create(const_align(pointer_size)));
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
                   st_widestring:
                     begin
                        { an empty ansi string is nil! }
                        if (strlength=0) then
                          curconstSegment.concat(Tai_const.Create_32bit(0))
                        else
                          begin
                            objectlibrary.getdatalabel(ll);
                            curconstSegment.concat(Tai_const_symbol.Create(ll));
                            { the actual structure starts at -12 from start label - CEC }
                            Consts.concat(tai_align.create(const_align(pointer_size)));
                            Consts.concat(Tai_const.Create_32bit(strlength));
                            Consts.concat(Tai_const.Create_32bit(strlength));
                            Consts.concat(Tai_const.Create_32bit(-1));
                            Consts.concat(Tai_label.Create(ll));
                            for i:=0 to strlength-1 do
                              Consts.concat(Tai_const.Create_16bit(pcompilerwidestring(strval)^.data[i]));
                            { ending #0 }
                            Consts.concat(Tai_const.Create_16bit(0))
                          end;
                     end;
                   st_longstring:
                     begin
                       internalerror(200107081);
                       {curconstSegment.concat(Tai_const.Create_32bit(strlength))));
                       curconstSegment.concat(Tai_const.Create_8bit(0));
                       getmem(ca,strlength+1);
                       move(strval^,ca^,strlength);
                       ca[strlength]:=#0;
                       generate_pascii(consts,ca,strlength);
                       curconstSegment.concat(Tai_const.Create_8bit(0));}
                     end;
                 end;
               end;
              p.free;
           end;
         arraydef:
           begin
              if try_to_consume(_LKLAMMER) then
                begin
                  for l:=tarraydef(t.def).lowrange to tarraydef(t.def).highrange-1 do
                    begin
                      readtypedconst(tarraydef(t.def).elementtype,nil,writable);
                      consume(_COMMA);
                    end;
                  readtypedconst(tarraydef(t.def).elementtype,nil,writable);
                  consume(_RKLAMMER);
                end
              else
              { if array of char then we allow also a string }
               if is_char(tarraydef(t.def).elementtype.def) then
                begin
                   p:=comp_expr(true);
                   if p.nodetype=stringconstn then
                    begin
                      len:=tstringconstnode(p).len;
                      { For tp7 the maximum lentgh can be 255 }
                      if (m_tp7 in aktmodeswitches) and
                         (len>255) then
                       len:=255;
                      ca:=tstringconstnode(p).value_str;
                    end
                   else
                     if is_constcharnode(p) then
                      begin
                        c:=chr(tordconstnode(p).value and $ff);
                        ca:=@c;
                        len:=1;
                      end
                   else
                     begin
                       Message(cg_e_illegal_expression);
                       len:=0;
                     end;
                   if len>(tarraydef(t.def).highrange-tarraydef(t.def).lowrange+1) then
                     Message(parser_e_string_larger_array);
                   for i:=tarraydef(t.def).lowrange to tarraydef(t.def).highrange do
                     begin
                        if i+1-tarraydef(t.def).lowrange<=len then
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
              { dynamic array nil }
               if is_dynamic_array(t.def) then
                begin
                  { Only allow nil initialization }
                  consume(_NIL);
                  curconstSegment.concat(Tai_const.Create_32bit(0));
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
                   if (po_methodpointer in tprocvardef(t.def).procoptions) then
                     curconstSegment.concat(Tai_const.Create_32bit(0));
                   consume(_NIL);
                   exit;
                end;
              { you can't assign a value other than NIL to a typed constant  }
              { which is a "procedure of object", because this also requires }
              { address of an object/class instance, which is not known at   }
              { compile time (JM)                                            }
              if (po_methodpointer in tprocvardef(t.def).procoptions) then
                Message(parser_e_no_procvarobj_const);
                { parse the rest too, so we can continue with error checking }
              getprocvardef:=tprocvardef(t.def);
              p:=comp_expr(true);
              getprocvardef:=nil;
              if codegenerror then
               begin
                 p.free;
                 exit;
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
                 (tloadnode(p).symtableentry.typ=procsym) then
               begin
                 curconstSegment.concat(Tai_const_symbol.createname(
                   tprocsym(tloadnode(p).symtableentry).first_procdef.mangledname));
               end
              else
               Message(cg_e_illegal_expression);
              p.free;
           end;
         { reads a typed constant record }
         recorddef:
           begin
              { KAZ }
              if (trecorddef(t.def)=rec_tguid) and
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
                   sorg:='';
                   aktpos:=0;
                   srsym := tsym(trecorddef(t.def).symtable.symindex.first);
                   recsym := nil;
                   while token<>_RKLAMMER do
                     begin
                        s:=pattern;
                        sorg:=orgpattern;
                        consume(_ID);
                        consume(_COLON);
                        error := false;
                        recsym := tsym(trecorddef(t.def).symtable.search(s));
                        if not assigned(recsym) then
                          begin
                            Message1(sym_e_illegal_field,sorg);
                            error := true;
                          end;
                        if (not error) and
                           (not assigned(srsym) or
                            (s <> srsym.name)) then
                          { possible variant record (JM) }
                          begin
                            { All parts of a variant start at the same offset      }
                            { Also allow jumping from one variant part to another, }
                            { as long as the offsets match                         }
                            if (assigned(srsym) and
                                (tvarsym(recsym).address = tvarsym(srsym).address)) or
                               { srsym is not assigned after parsing w2 in the      }
                               { typed const in the next example:                   }
                               {   type tr = record case byte of                    }
                               {          1: (l1,l2: dword);                        }
                               {          2: (w1,w2: word);                         }
                               {        end;                                        }
                               {   const r: tr = (w1:1;w2:1;l2:5);                  }
                               (tvarsym(recsym).address = aktpos) then
                              srsym := recsym
                            { going backwards isn't allowed in any mode }
                            else if (tvarsym(recsym).address<aktpos) then
                              begin
                                Message(parser_e_invalid_record_const);
                                error := true;
                              end
                            { Delphi allows you to skip fields }
                            else if (m_delphi in aktmodeswitches) then
                              begin
                                Message1(parser_w_skipped_fields_before,sorg);
                                srsym := recsym;
                              end
                            { FPC and TP don't }
                            else
                              begin
                                Message1(parser_e_skipped_fields_before,sorg);
                                error := true;
                              end;
                          end;
                        if error then
                          consume_all_until(_SEMICOLON)
                        else
                          begin

                            { if needed fill (alignment) }
                            if tvarsym(srsym).address>aktpos then
                               for i:=1 to tvarsym(srsym).address-aktpos do
                                 curconstSegment.concat(Tai_const.Create_8bit(0));

                             { new position }
                             aktpos:=tvarsym(srsym).address+tvarsym(srsym).vartype.def.size;

                             { read the data }
                             readtypedconst(tvarsym(srsym).vartype,nil,writable);

                             { keep previous field for checking whether whole }
                             { record was initialized (JM)                    }
                             recsym := srsym;
                             { goto next field }
                             srsym := tsym(srsym.indexnext);

                             if token=_SEMICOLON then
                               consume(_SEMICOLON)
                             else break;
                          end;
                   end;

                 { are there any fields left?                            }
                 if assigned(srsym) and
                    { don't complain if there only come other variant parts }
                    { after the last initialized field                      }
                    ((recsym=nil) or
                     (tvarsym(srsym).address > tvarsym(recsym).address)) then
                   Message1(parser_w_skipped_fields_after,sorg);

                 for i:=1 to t.def.size-aktpos do
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
              else if (oo_has_vmt in tobjectdef(t.def).objectoptions) and
                      (m_fpc in aktmodeswitches) then
                 Message(parser_e_type_const_not_possible)
              else
                begin
                   consume(_LKLAMMER);
                   aktpos:=0;
                   while token<>_RKLAMMER do
                     begin
                        s:=pattern;
                        sorg:=orgpattern;
                        consume(_ID);
                        consume(_COLON);
                        srsym:=nil;
                        obj:=tobjectdef(t.def);
                        symt:=obj.symtable;
                        while (srsym=nil) and assigned(symt) do
                          begin
                             srsym:=tsym(symt.search(s));
                             if assigned(obj) then
                               obj:=obj.childof;
                             if assigned(obj) then
                               symt:=obj.symtable
                             else
                               symt:=nil;
                          end;

                        if srsym=nil then
                          begin
                             Message1(sym_e_id_not_found,sorg);
                             consume_all_until(_SEMICOLON);
                          end
                        else
                          begin
                             { check position }
                             if tvarsym(srsym).address<aktpos then
                               Message(parser_e_invalid_record_const);

                             { check in VMT needs to be added for TP mode }
                             if not(m_fpc in aktmodeswitches) and
                                (oo_has_vmt in tobjectdef(t.def).objectoptions) and
                                (tobjectdef(t.def).vmt_offset<tvarsym(srsym).address) then
                               begin
                                 for i:=1 to tobjectdef(t.def).vmt_offset-aktpos do
                                   curconstsegment.concat(tai_const.create_8bit(0));
                                 curconstsegment.concat(tai_const_symbol.createname(tobjectdef(t.def).vmt_mangledname));
                                 { this is more general }
                                 aktpos:=tobjectdef(t.def).vmt_offset + pointer_size;
                               end;

                             { if needed fill }
                             if tvarsym(srsym).address>aktpos then
                               for i:=1 to tvarsym(srsym).address-aktpos do
                                 curconstSegment.concat(Tai_const.Create_8bit(0));

                             { new position }
                             aktpos:=tvarsym(srsym).address+tvarsym(srsym).vartype.def.size;

                             { read the data }
                             readtypedconst(tvarsym(srsym).vartype,nil,writable);

                             if token=_SEMICOLON then
                               consume(_SEMICOLON)
                             else break;
                          end;
                     end;
                   if not(m_fpc in aktmodeswitches) and
                      (oo_has_vmt in tobjectdef(t.def).objectoptions) and
                      (tobjectdef(t.def).vmt_offset>=aktpos) then
                     begin
                       for i:=1 to tobjectdef(t.def).vmt_offset-aktpos do
                         curconstsegment.concat(tai_const.create_8bit(0));
                       curconstsegment.concat(tai_const_symbol.createname(tobjectdef(t.def).vmt_mangledname));
                       { this is more general }
                       aktpos:=tobjectdef(t.def).vmt_offset + pointer_size;
                     end;
                   for i:=1 to t.def.size-aktpos do
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
  Revision 1.65  2003-03-17 21:42:32  peter
    * allow nil initialization of dynamic array

  Revision 1.64  2003/01/02 20:45:08  peter
    * fix uninited var

  Revision 1.63  2002/12/26 12:34:54  florian
    * fixed support for type widechar consts

  Revision 1.62  2002/12/07 14:15:33  carl
    + add some explicit typecasts to remove some warnings

  Revision 1.61  2002/11/25 18:43:33  carl
   - removed the invalid if <> checking (Delphi is strange on this)
   + implemented abstract warning on instance creation of class with
      abstract methods.
   * some error message cleanups

  Revision 1.60  2002/11/25 17:43:23  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.59  2002/11/22 22:48:10  carl
  * memory optimization with tconstsym (1.5%)

  Revision 1.58  2002/11/09 15:31:57  carl
    + align ansi/wide string constants

  Revision 1.57  2002/09/06 19:58:31  carl
   * start bugfix 1996
   * 64-bit typed constant now work correctly and fully (bugfix 2001)

  Revision 1.56  2002/09/03 16:26:27  daniel
    * Make Tprocdef.defs protected

  Revision 1.55  2002/08/11 14:32:27  peter
    * renamed current_library to objectlibrary

  Revision 1.54  2002/08/11 13:24:13  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.53  2002/07/23 12:34:30  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.52  2002/07/22 11:48:04  daniel
  * Sets are now internally sets.

  Revision 1.51  2002/07/20 11:57:56  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.50  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.49  2002/05/18 13:34:16  peter
    * readded missing revisions

  Revision 1.48  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.46  2002/05/12 16:53:09  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.45  2002/04/23 19:16:35  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.44  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.43  2002/04/15 19:01:53  carl
  + target_info.size_of_pointer -> pointer_Size

  Revision 1.42  2002/04/04 19:06:03  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.41  2002/01/24 18:25:49  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.40  2002/01/06 21:47:32  peter
    * removed getprocvar, use only getprocvardef

}
