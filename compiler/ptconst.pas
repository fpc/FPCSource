{
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

   uses symtype,symsym,aasmdata;

    procedure read_typed_const(list:tasmlist;sym:tstaticvarsym);


implementation

    uses
       SysUtils,
       globtype,systems,tokens,verbose,constexp,
       cutils,globals,widestr,scanner,
       symconst,symbase,symdef,symtable,
       aasmbase,aasmtai,aasmcpu,defutil,defcmp,
       { pass 1 }
       node,htypechk,procinfo,
       nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,
       { parser specific stuff }
       pbase,pexpr,pdecvar,
       { codegen }
       cpuinfo,cgbase,dbgbase
       ;

{$maxfpuregisters 0}

{*****************************************************************************
                          Bitpacked value helpers
*****************************************************************************}

    type
      tbitpackedval = record
        curval, nextval: aword;
        curbitoffset: smallint;
        loadbitsize,packedbitsize: byte;
      end;


    procedure initbitpackval(out bp: tbitpackedval; packedbitsize: byte);
      begin
        bp.curval:=0;
        bp.nextval:=0;
        bp.curbitoffset:=0;
        bp.packedbitsize:=packedbitsize;
        bp.loadbitsize:=packedbitsloadsize(bp.packedbitsize)*8;
      end;


{$ifopt r+}
{$define rangeon}
{$r-}
{$endif}

{$ifopt q+}
{$define overflowon}
{$q-}
{$endif}
    { (values between quotes below refer to fields of bp; fields not         }
    {  mentioned are unused by this routine)                                 }
    { bitpacks "value" as bitpacked value of bitsize "packedbitsize" into    }
    { "curval", which has already been filled up to "curbitoffset", and      }
    { stores the spillover if any into "nextval". It also updates            }
    { curbitoffset to reflect how many bits of currval are now used (can be  }
    { > AIntBits in case of spillover)                                       }
    procedure bitpackval(value: aword; var bp: tbitpackedval);
      var
        shiftcount: longint;
      begin
        if (target_info.endian=endian_big) then
          begin
            { bitpacked format: left-aligned (i.e., "big endian bitness") }
            bp.curval:=bp.curval or ((value shl (AIntBits-bp.packedbitsize)) shr bp.curbitoffset);
            shiftcount:=((AIntBits-bp.packedbitsize)-bp.curbitoffset);
            { carry-over to the next element? }
            if (shiftcount<0) then
              bp.nextval:=(value and ((aword(1) shl (-shiftcount))-1)) shl
                          (AIntBits+shiftcount)
          end
        else
          begin
            { bitpacked format: right aligned (i.e., "little endian bitness") }
            bp.curval:=bp.curval or (value shl bp.curbitoffset);
            { carry-over to the next element? }
            if (bp.curbitoffset+bp.packedbitsize>AIntBits) then
              bp.nextval:=value shr (AIntBits-bp.curbitoffset)
          end;
        inc(bp.curbitoffset,bp.packedbitsize);
      end;

{$ifdef rangeon}
{$r+}
{$undef rangeon}
{$endif}

{$ifdef overflowon}
{$q+}
{$undef overflowon}
{$endif}


    procedure flush_packed_value(list: tasmlist; var bp: tbitpackedval);
      var
        bitstowrite: longint;
        writeval : byte;
      begin
        if (bp.curbitoffset < AIntBits) then
          begin
            { forced flush -> write multiple of loadsize }
            bitstowrite:=align(bp.curbitoffset,bp.loadbitsize);
            bp.curbitoffset:=0;
          end
        else
          begin
            bitstowrite:=AIntBits;
            dec(bp.curbitoffset,AIntBits);
          end;
        while (bitstowrite>=8) do
          begin
            if (target_info.endian=endian_little) then
              begin
                { write lowest byte }
                writeval:=byte(bp.curval);
                bp.curval:=bp.curval shr 8;
              end
            else
              begin
                { write highest byte }
                writeval:=bp.curval shr (AIntBits-8);
                bp.curval:=(bp.curval and (not($ff shl (AIntBits-8)))) shl 8;
              end;
            list.concat(tai_const.create_8bit(writeval));
            dec(bitstowrite,8);
          end;
        bp.curval:=bp.nextval;
        bp.nextval:=0;
      end;


{*****************************************************************************
                             read typed const
*****************************************************************************}


    { this procedure reads typed constants }
    procedure read_typed_const_data(list:tasmlist;def:tdef);

      procedure parse_orddef(list:tasmlist;def:torddef);
        var
          n : tnode;
          intvalue : tconstexprint;

        procedure do_error;
          begin
            if is_constnode(n) then
              IncompatibleTypes(n.resultdef, def)
            else
              Message(parser_e_illegal_expression);
          end;

        begin
           n:=comp_expr(true);
           case def.ordtype of
              bool8bit :
                begin
                   if is_constboolnode(n) then
                     list.concat(Tai_const.Create_8bit(byte(tordconstnode(n).value.svalue)))
                   else
                     do_error;
                end;
              bool16bit :
                begin
                   if is_constboolnode(n) then
                     list.concat(Tai_const.Create_16bit(word(tordconstnode(n).value.svalue)))
                   else
                     do_error;
                end;
              bool32bit :
                begin
                   if is_constboolnode(n) then
                     list.concat(Tai_const.Create_32bit(longint(tordconstnode(n).value.svalue)))
                   else
                     do_error;
                end;
              bool64bit :
                begin
                   if is_constboolnode(n) then
                     list.concat(Tai_const.Create_64bit(int64(tordconstnode(n).value.svalue)))
                   else
                     do_error;
                end;
              uchar :
                begin
                   if is_constcharnode(n) or
                     ((m_delphi in current_settings.modeswitches) and
                      is_constwidecharnode(n) and
                      (tordconstnode(n).value <= 255)) then
                     list.concat(Tai_const.Create_8bit(byte(tordconstnode(n).value.svalue)))
                   else
                     do_error;
                end;
              uwidechar :
                begin
                   if is_constcharnode(n) then
                     inserttypeconv(n,cwidechartype);
                   if is_constwidecharnode(n) then
                     list.concat(Tai_const.Create_16bit(word(tordconstnode(n).value.svalue)))
                   else
                     do_error;
                end;
              s8bit,u8bit,
              u16bit,s16bit,
              s32bit,u32bit,
              s64bit,u64bit :
                begin
                   if is_constintnode(n) then
                     begin
                       testrange(def,tordconstnode(n).value,false);
                       case def.size of
                         1 :
                           list.concat(Tai_const.Create_8bit(byte(tordconstnode(n).value.svalue)));
                         2 :
                           list.concat(Tai_const.Create_16bit(word(tordconstnode(n).value.svalue)));
                         4 :
                           list.concat(Tai_const.Create_32bit(longint(tordconstnode(n).value.svalue)));
                         8 :
                           list.concat(Tai_const.Create_64bit(tordconstnode(n).value.svalue));
                       end;
                     end
                   else
                     do_error;
                end;
              scurrency:
                begin
                   if is_constintnode(n) then
                     intvalue := tordconstnode(n).value
                   { allow bootstrapping }
                   else if is_constrealnode(n) then
                     intvalue:=PInt64(@trealconstnode(n).value_currency)^
                   else
                     begin
                       intvalue:=0;
                       IncompatibleTypes(n.resultdef, def);
                     end;
                  list.concat(Tai_const.Create_64bit(intvalue));
                end;
              else
                internalerror(200611052);
           end;
           n.free;
        end;

        procedure parse_floatdef(list:tasmlist;def:tfloatdef);
        var
          n : tnode;
          value : bestreal;
        begin
          n:=comp_expr(true);
          if is_constrealnode(n) then
            value:=trealconstnode(n).value_real
          else if is_constintnode(n) then
            value:=tordconstnode(n).value
          else
            IncompatibleTypes(n.resultdef, def);

          case def.floattype of
             s32real :
               list.concat(Tai_real_32bit.Create(ts32real(value)));
             s64real :
{$ifdef ARM}
               if is_double_hilo_swapped then
                 list.concat(Tai_real_64bit.Create_hiloswapped(ts64real(value)))
               else
{$endif ARM}
                 list.concat(Tai_real_64bit.Create(ts64real(value)));
             s80real :
               list.concat(Tai_real_80bit.Create(value));
             s64comp :
               { the round is necessary for native compilers where comp isn't a float }
               list.concat(Tai_comp_64bit.Create(round(value)));
             s64currency:
               list.concat(Tai_comp_64bit.Create(round(value*10000)));
             s128real:
               list.concat(Tai_real_128bit.Create(value));
             else
               internalerror(200611053);
          end;
          n.free;
        end;

        procedure parse_classrefdef(list:tasmlist;def:tclassrefdef);
        var
          n : tnode;
        begin
          n:=comp_expr(true);
          case n.nodetype of
            loadvmtaddrn:
              begin
                if not Tobjectdef(tclassrefdef(n.resultdef).pointeddef).is_related(tobjectdef(def.pointeddef)) then
                  IncompatibleTypes(n.resultdef, def);
                list.concat(Tai_const.Create_sym(current_asmdata.RefAsmSymbol(Tobjectdef(tclassrefdef(n.resultdef).pointeddef).vmt_mangledname)));
              end;
             niln:
               list.concat(Tai_const.Create_sym(nil));
             else
               IncompatibleTypes(n.resultdef, def);
          end;
          n.free;
        end;

        procedure parse_pointerdef(list:tasmlist;def:tpointerdef);
        var
          hp,p      : tnode;
          srsym     : tsym;
          pd        : tprocdef;
          ca        : pchar;
          pw        : pcompilerwidestring;
          i,len     : longint;
          base,
          offset    : aint;
          v         : Tconstexprint;
          ll        : tasmlabel;
          varalign  : shortint;
        begin
          p:=comp_expr(true);
          { remove equal typecasts for pointer/nil addresses }
          if (p.nodetype=typeconvn) then
            with Ttypeconvnode(p) do
              if (left.nodetype in [addrn,niln]) and equal_defs(def,p.resultdef) then
                begin
                  hp:=left;
                  left:=nil;
                  p.free;
                  p:=hp;
                end;
          { allows horrible ofs(typeof(TButton)^) code !! }
          if (p.nodetype=addrn) then
            with Taddrnode(p) do
              if left.nodetype=derefn then
                begin
                  hp:=tderefnode(left).left;
                  tderefnode(left).left:=nil;
                  p.free;
                  p:=hp;
               end;
          { const pointer ? }
          if (p.nodetype = pointerconstn) then
            begin
              if sizeof(TConstPtrUInt)=8 then
                list.concat(Tai_const.Create_64bit(int64(tpointerconstnode(p).value)))
              else
                if sizeof(TConstPtrUInt)=4 then
                  list.concat(Tai_const.Create_32bit(longint(tpointerconstnode(p).value)))
              else
                internalerror(200404122);
            end
          { nil pointer ? }
          else if p.nodetype=niln then
            list.concat(Tai_const.Create_sym(nil))
          { maybe pchar ? }
          else
            if is_char(def.pointeddef) and
               (p.nodetype<>addrn) then
              begin
                current_asmdata.getdatalabel(ll);
                list.concat(Tai_const.Create_sym(ll));
                if p.nodetype=stringconstn then
                 varalign:=size_2_align(tstringconstnode(p).len)
                else
                 varalign:=0;
                varalign:=const_align(varalign);
                current_asmdata.asmlists[al_const].concat(Tai_align.Create(varalign));
                current_asmdata.asmlists[al_const].concat(Tai_label.Create(ll));
                if p.nodetype=stringconstn then
                  begin
                    len:=tstringconstnode(p).len;
                    { For tp7 the maximum lentgh can be 255 }
                    if (m_tp7 in current_settings.modeswitches) and
                       (len>255) then
                     len:=255;
                    getmem(ca,len+2);
                    move(tstringconstnode(p).value_str^,ca^,len+1);
                    current_asmdata.asmlists[al_const].concat(Tai_string.Create_pchar(ca,len+1));
                  end
                else
                  if is_constcharnode(p) then
                    current_asmdata.asmlists[al_const].concat(Tai_string.Create(char(byte(tordconstnode(p).value.svalue))+#0))
                else
                  IncompatibleTypes(p.resultdef, def);
            end
          { maybe pwidechar ? }
          else
            if is_widechar(def.pointeddef) and
               (p.nodetype<>addrn) then
              begin
                current_asmdata.getdatalabel(ll);
                list.concat(Tai_const.Create_sym(ll));
                current_asmdata.asmlists[al_typedconsts].concat(tai_align.create(const_align(sizeof(aint))));
                current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(ll));
                if (p.nodetype in [stringconstn,ordconstn]) then
                  begin
                    { convert to widestring stringconstn }
                    inserttypeconv(p,cwidestringtype);
                    if (p.nodetype=stringconstn) and
                       (tstringconstnode(p).cst_type=cst_widestring) then
                     begin
                       pw:=pcompilerwidestring(tstringconstnode(p).value_str);
                       for i:=0 to tstringconstnode(p).len-1 do
                         current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_16bit(pw^.data[i]));
                       { ending #0 }
                       current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_16bit(0))
                     end;
                  end
                else
                  IncompatibleTypes(p.resultdef, def);
            end
          else
            if (p.nodetype=addrn) or
               is_procvar_load(p) then
              begin
                { insert typeconv }
                inserttypeconv(p,def);
                hp:=p;
                while assigned(hp) and (hp.nodetype in [addrn,typeconvn,subscriptn,vecn]) do
                  hp:=tunarynode(hp).left;
                if (hp.nodetype=loadn) then
                  begin
                    hp:=p;
                    offset:=0;
                    while assigned(hp) and (hp.nodetype<>loadn) do
                      begin
                         case hp.nodetype of
                           vecn :
                             begin
                               case tvecnode(hp).left.resultdef.typ of
                                 stringdef :
                                   begin
                                      { this seems OK for shortstring and ansistrings PM }
                                      { it is wrong for widestrings !! }
                                      len:=1;
                                      base:=0;
                                   end;
                                 arraydef :
                                   begin
                                      if not is_packed_array(tvecnode(hp).left.resultdef) then
                                        begin
                                          len:=tarraydef(tvecnode(hp).left.resultdef).elesize;
                                          base:=tarraydef(tvecnode(hp).left.resultdef).lowrange;
                                        end
                                      else
                                        begin
                                          Message(parser_e_packed_dynamic_open_array);
                                          len:=1;
                                          base:=0;
                                        end;
                                   end
                                 else
                                   Message(parser_e_illegal_expression);
                               end;
                               if is_constintnode(tvecnode(hp).right) then
                                 begin
                                   {Prevent overflow.}
                                   v:=get_ordinal_value(tvecnode(hp).right)-base;
                                   if (v<int64(low(offset))) or (v>int64(high(offset))) then
                                     message(parser_e_range_check_error);
                                   if high(offset)-offset div len>v then
                                     inc(offset,len*v.svalue)
                                   else
                                     message(parser_e_range_check_error);
                                 end
                               else
                                 Message(parser_e_illegal_expression);
                             end;
                           subscriptn :
                             inc(offset,tsubscriptnode(hp).vs.fieldoffset);
                           typeconvn :
                             begin
                               if not(ttypeconvnode(hp).convtype in [tc_equal,tc_proc_2_procvar]) then
                                 Message(parser_e_illegal_expression);
                             end;
                           addrn :
                             ;
                           else
                             Message(parser_e_illegal_expression);
                         end;
                         hp:=tunarynode(hp).left;
                      end;
                    srsym:=tloadnode(hp).symtableentry;
                    case srsym.typ of
                      procsym :
                        begin
                          pd:=tprocdef(tprocsym(srsym).ProcdefList[0]);
                          if Tprocsym(srsym).ProcdefList.Count>1 then
                            Message(parser_e_no_overloaded_procvars);
                          if po_abstractmethod in pd.procoptions then
                            Message(type_e_cant_take_address_of_abstract_method)
                          else
                            list.concat(Tai_const.Createname(pd.mangledname,offset));
                        end;
                      staticvarsym :
                        list.concat(Tai_const.Createname(tstaticvarsym(srsym).mangledname,offset));
                      labelsym :
                        list.concat(Tai_const.Createname(tlabelsym(srsym).mangledname,offset));
                      constsym :
                        if tconstsym(srsym).consttyp=constresourcestring then
                          list.concat(Tai_const.Createname(make_mangledname('RESSTR',tconstsym(srsym).owner,tconstsym(srsym).name),sizeof(aint)))
                        else
                          Message(type_e_variable_id_expected);
                      else
                        Message(type_e_variable_id_expected);
                    end;
                  end
                else
                  Message(parser_e_illegal_expression);
              end
          else
          { allow typeof(Object type)}
            if (p.nodetype=inlinen) and
               (tinlinenode(p).inlinenumber=in_typeof_x) then
              begin
                if (tinlinenode(p).left.nodetype=typen) then
                  begin
                    list.concat(Tai_const.createname(
                      tobjectdef(tinlinenode(p).left.resultdef).vmt_mangledname,0));
                  end
                else
                  Message(parser_e_illegal_expression);
              end
          else
            Message(parser_e_illegal_expression);
          p.free;
        end;

        procedure parse_setdef(list:tasmlist;def:tsetdef);
        type
           setbytes = array[0..31] of byte;
           Psetbytes = ^setbytes;
        var
          p : tnode;
          i : longint;
        begin
          p:=comp_expr(true);
          if p.nodetype=setconstn then
            begin
              { be sure to convert to the correct result, else
                it can generate smallset data instead of normalset (PFV) }
              inserttypeconv(p,def);
              { we only allow const sets }
              if assigned(tsetconstnode(p).left) then
                Message(parser_e_illegal_expression)
              else
                begin
                  tsetconstnode(p).adjustforsetbase;
                  { this writing is endian independant   }
                  { untrue - because they are considered }
                  { arrays of 32-bit values CEC          }
                  if source_info.endian = target_info.endian then
                    begin
{$if defined(FPC_NEW_BIGENDIAN_SETS) or defined(FPC_LITTLE_ENDIAN)}
                      for i:=0 to p.resultdef.size-1 do
                        list.concat(tai_const.create_8bit(Psetbytes(tsetconstnode(p).value_set)^[i]));
{$else}
                      for i:=0 to p.resultdef.size-1 do
                        list.concat(tai_const.create_8bit(reverse_byte(Psetbytes(tsetconstnode(p).value_set)^[i xor 3])));
{$endif}
                    end
                  else
                    begin
                      for i:=0 to p.resultdef.size-1 do
                        list.concat(tai_const.create_8bit(reverse_byte(Psetbytes(tsetconstnode(p).value_set)^[i])));
                    end;
                end;
            end
          else
            Message(parser_e_illegal_expression);
          p.free;
        end;

        procedure parse_enumdef(list:tasmlist;def:tenumdef);
        var
          p : tnode;
        begin
          p:=comp_expr(true);
          if p.nodetype=ordconstn then
            begin
              if equal_defs(p.resultdef,def) or
                 is_subequal(p.resultdef,def) then
                begin
                  case longint(p.resultdef.size) of
                    1 : list.concat(Tai_const.Create_8bit(Byte(tordconstnode(p).value.svalue)));
                    2 : list.concat(Tai_const.Create_16bit(Word(tordconstnode(p).value.svalue)));
                    4 : list.concat(Tai_const.Create_32bit(Longint(tordconstnode(p).value.svalue)));
                  end;
                end
              else
                IncompatibleTypes(p.resultdef,def);
            end
          else
            Message(parser_e_illegal_expression);
          p.free;
        end;

        procedure parse_stringdef(list:tasmlist;def:tstringdef);
        var
          n : tnode;
          i : longint;
          strlength : aint;
          strval    : pchar;
          strch     : char;
          ll,ll2    : tasmlabel;
          ca        : pchar;
        begin
          n:=comp_expr(true);
          { load strval and strlength of the constant tree }
          if (n.nodetype=stringconstn) or is_widestring(def) then
            begin
              { convert to the expected string type so that
                for widestrings strval is a pcompilerwidestring }
              inserttypeconv(n,def);
              strlength:=tstringconstnode(n).len;
              strval:=tstringconstnode(n).value_str;
            end
          else if is_constcharnode(n) then
            begin
              { strval:=pchar(@tordconstnode(n).value);
                THIS FAIL on BIG_ENDIAN MACHINES PM }
              strch:=chr(tordconstnode(n).value.svalue and $ff);
              strval:=@strch;
              strlength:=1
            end
          else if is_constresourcestringnode(n) then
            begin
              strval:=pchar(tconstsym(tloadnode(n).symtableentry).value.valueptr);
              strlength:=tconstsym(tloadnode(n).symtableentry).value.len;
            end
          else
            begin
              Message(parser_e_illegal_expression);
              strlength:=-1;
            end;
          if strlength>=0 then
            begin
              case def.stringtype of
                st_shortstring:
                  begin
                    if strlength>=def.size then
                     begin
                       message2(parser_w_string_too_long,strpas(strval),tostr(def.size-1));
                       strlength:=def.size-1;
                     end;
                    list.concat(Tai_const.Create_8bit(strlength));
                    { this can also handle longer strings }
                    getmem(ca,strlength+1);
                    move(strval^,ca^,strlength);
                    ca[strlength]:=#0;
                    list.concat(Tai_string.Create_pchar(ca,strlength));
                    { fillup with spaces if size is shorter }
                    if def.size>strlength then
                     begin
                       getmem(ca,def.size-strlength);
                       { def.size contains also the leading length, so we }
                       { we have to subtract one                       }
                       fillchar(ca[0],def.size-strlength-1,' ');
                       ca[def.size-strlength-1]:=#0;
                       { this can also handle longer strings }
                       list.concat(Tai_string.Create_pchar(ca,def.size-strlength-1));
                     end;
                  end;
                st_ansistring:
                  begin
                     { an empty ansi string is nil! }
                     if (strlength=0) then
                       list.concat(Tai_const.Create_sym(nil))
                     else
                       begin
                         current_asmdata.getdatalabel(ll);
                         list.concat(Tai_const.Create_sym(ll));
                         current_asmdata.getdatalabel(ll2);
                         current_asmdata.asmlists[al_const].concat(tai_align.create(const_align(sizeof(aint))));
                         current_asmdata.asmlists[al_const].concat(Tai_label.Create(ll2));
                         current_asmdata.asmlists[al_const].concat(Tai_const.Create_aint(-1));
                         current_asmdata.asmlists[al_const].concat(Tai_const.Create_aint(strlength));
                         { make sure the string doesn't get dead stripped if the header is referenced }
                         if (target_info.system in systems_darwin) then
                           current_asmdata.asmlists[al_typedconsts].concat(tai_directive.create(asd_reference,ll.name));
                         current_asmdata.asmlists[al_const].concat(Tai_label.Create(ll));
                         { ... and vice versa }
                         if (target_info.system in systems_darwin) then
                           list.concat(tai_directive.create(asd_reference,ll2.name));
                         getmem(ca,strlength+1);
                         move(strval^,ca^,strlength);
                         { The terminating #0 to be stored in the .data section (JM) }
                         ca[strlength]:=#0;
                         current_asmdata.asmlists[al_const].concat(Tai_string.Create_pchar(ca,strlength+1));
                       end;
                  end;
                st_widestring:
                  begin
                     { an empty ansi string is nil! }
                     if (strlength=0) then
                       list.concat(Tai_const.Create_sym(nil))
                     else
                       begin
                         current_asmdata.getdatalabel(ll);
                         list.concat(Tai_const.Create_sym(ll));
                         current_asmdata.getdatalabel(ll2);
                         current_asmdata.asmlists[al_const].concat(tai_align.create(const_align(sizeof(aint))));
                         current_asmdata.asmlists[al_const].concat(Tai_label.Create(ll2));
                         if tf_winlikewidestring in target_info.flags then
                           current_asmdata.asmlists[al_const].concat(Tai_const.Create_32bit(strlength*cwidechartype.size))
                         else
                           begin
                             current_asmdata.asmlists[al_const].concat(Tai_const.Create_aint(-1));
                             current_asmdata.asmlists[al_const].concat(Tai_const.Create_aint(strlength*cwidechartype.size));
                           end;
                         { make sure the string doesn't get dead stripped if the header is referenced }
                         if (target_info.system in systems_darwin) then
                           current_asmdata.asmlists[al_typedconsts].concat(tai_directive.create(asd_reference,ll.name));
                         current_asmdata.asmlists[al_const].concat(Tai_label.Create(ll));
                         { ... and vice versa }
                         if (target_info.system in systems_darwin) then
                           current_asmdata.asmlists[al_typedconsts].concat(tai_directive.create(asd_reference,ll2.name));
                         for i:=0 to strlength-1 do
                           current_asmdata.asmlists[al_const].concat(Tai_const.Create_16bit(pcompilerwidestring(strval)^.data[i]));
                         { ending #0 }
                         current_asmdata.asmlists[al_const].concat(Tai_const.Create_16bit(0))
                       end;
                  end;
                else
                  internalerror(200107081);
              end;
            end;
          n.free;
        end;


        { parse a single constant and add it to the packed const info  }
        { represented by curval etc (see explanation of bitpackval for }
        { what the different parameters mean)                          }
        function parse_single_packed_const(list: tasmlist; def: tdef; var bp: tbitpackedval): boolean;
          var
            n : tnode;
          begin
            result:=true;
            n:=comp_expr(true);
            if (n.nodetype <> ordconstn) or
               not equal_defs(n.resultdef,def) and
               not is_subequal(n.resultdef,def) then
              begin
                n.free;
                incompatibletypes(n.resultdef,def);
                consume_all_until(_SEMICOLON);
                result:=false;
                exit;
              end;
            if (Tordconstnode(n).value<qword(low(Aword))) or (Tordconstnode(n).value>qword(high(Aword))) then
              message(parser_e_range_check_error)
            else
              bitpackval(Tordconstnode(n).value.uvalue,bp);
            if (bp.curbitoffset>=AIntBits) then
              flush_packed_value(list,bp);
            n.free;
          end;


        { parses a packed array constant }
        procedure parse_packed_array_def(list: tasmlist; def: tarraydef);
          var
            i  : aint;
            bp : tbitpackedval;
          begin
            if not(def.elementdef.typ in [orddef,enumdef]) then
              internalerror(2007022010);
            { begin of the array }
            consume(_LKLAMMER);
            initbitpackval(bp,def.elepackedbitsize);
            i:=def.lowrange;
            { can't use for-loop, fails when cross-compiling from }
            { 32 to 64 bit because i is then 64 bit               }
            while (i<def.highrange) do
              begin
                { get next item of the packed array }
                if not parse_single_packed_const(list,def.elementdef,bp) then
                  exit;
                consume(_COMMA);
                inc(i);
              end;
            { final item }
            if not parse_single_packed_const(list,def.elementdef,bp) then
              exit;
            { flush final incomplete value if necessary }
            if (bp.curbitoffset <> 0) then
              flush_packed_value(list,bp);
            consume(_RKLAMMER);
          end;


        procedure parse_arraydef(list:tasmlist;def:tarraydef);
        var
          n : tnode;
          i : longint;
          len : aint;
          ch  : char;
          ca  : pchar;
        begin
          { dynamic array nil }
          if is_dynamic_array(def) then
            begin
              { Only allow nil initialization }
              consume(_NIL);
              list.concat(Tai_const.Create_sym(nil));
            end
          { packed array constant }
          else if is_packed_array(def) and
                  (def.elepackedbitsize mod 8 <> 0)  then
            begin
              parse_packed_array_def(list,def);
            end
          { normal array const between brackets }
          else if try_to_consume(_LKLAMMER) then
            begin
              for i:=def.lowrange to def.highrange-1 do
                begin
                  read_typed_const_data(list,def.elementdef);
                  consume(_COMMA);
                end;
              read_typed_const_data(list,def.elementdef);
              consume(_RKLAMMER);
            end
          { if array of char then we allow also a string }
          else if is_char(def.elementdef) then
            begin
               n:=comp_expr(true);
               if n.nodetype=stringconstn then
                 begin
                   len:=tstringconstnode(n).len;
                   { For tp7 the maximum lentgh can be 255 }
                   if (m_tp7 in current_settings.modeswitches) and
                      (len>255) then
                    len:=255;
                   ca:=tstringconstnode(n).value_str;
                 end
               else
                 if is_constcharnode(n) then
                  begin
                    ch:=chr(tordconstnode(n).value.uvalue and $ff);
                    ca:=@ch;
                    len:=1;
                  end
               else
                 begin
                   Message(parser_e_illegal_expression);
                   len:=0;
                 end;
               if len>(def.highrange-def.lowrange+1) then
                 Message(parser_e_string_larger_array);
               for i:=def.lowrange to def.highrange do
                 begin
                    if i+1-def.lowrange<=len then
                      begin
                         list.concat(Tai_const.Create_8bit(byte(ca^)));
                         inc(ca);
                      end
                    else
                      {Fill the remaining positions with #0.}
                      list.concat(Tai_const.Create_8bit(0));
                 end;
               n.free;
            end
          else
            begin
              { we want the ( }
              consume(_LKLAMMER);
            end;
        end;

        procedure parse_procvardef(list:tasmlist;def:tprocvardef);
        var
          tmpn,n : tnode;
          pd   : tprocdef;
        begin
          { Procvars and pointers are no longer compatible.  }
          { under tp:  =nil or =var under fpc: =nil or =@var }
          if try_to_consume(_NIL) then
            begin
               list.concat(Tai_const.Create_sym(nil));
               if (po_methodpointer in def.procoptions) then
                 list.concat(Tai_const.Create_sym(nil));
               exit;
            end;
          { you can't assign a value other than NIL to a typed constant  }
          { which is a "procedure of object", because this also requires }
          { address of an object/class instance, which is not known at   }
          { compile time (JM)                                            }
          if (po_methodpointer in def.procoptions) then
            Message(parser_e_no_procvarobj_const);
          { parse the rest too, so we can continue with error checking }
          getprocvardef:=def;
          n:=comp_expr(true);
          getprocvardef:=nil;
          if codegenerror then
            begin
              n.free;
              exit;
            end;
          { let type conversion check everything needed }
          inserttypeconv(n,def);
          if codegenerror then
            begin
              n.free;
              exit;
            end;
          { remove typeconvs, that will normally insert a lea
            instruction which is not necessary for us }
          while n.nodetype=typeconvn do
            begin
              tmpn:=ttypeconvnode(n).left;
              ttypeconvnode(n).left:=nil;
              n.free;
              n:=tmpn;
            end;
          { remove addrn which we also don't need here }
          if n.nodetype=addrn then
            begin
              tmpn:=taddrnode(n).left;
              taddrnode(n).left:=nil;
              n.free;
              n:=tmpn;
            end;
          { we now need to have a loadn with a procsym }
          if (n.nodetype=loadn) and
             (tloadnode(n).symtableentry.typ=procsym) then
            begin
              pd:=tprocdef(tprocsym(tloadnode(n).symtableentry).ProcdefList[0]);
              list.concat(Tai_const.createname(pd.mangledname,0));
            end
          else
            Message(parser_e_illegal_expression);
          n.free;
        end;

        procedure parse_recorddef(list:tasmlist;def:trecorddef);
        var
          n       : tnode;
          i,
          symidx  : longint;
          recsym,
          srsym   : tsym;
          hs      : string;
          sorg,s  : TIDString;
          tmpguid : tguid;
          curroffset,
          fillbytes  : aint;
          bp   : tbitpackedval;
          error,
          is_packed: boolean;
        begin
          { GUID }
          if (def=rec_tguid) and
             ((token=_CSTRING) or (token=_CCHAR) or (token=_ID)) then
            begin
              n:=comp_expr(true);
              inserttypeconv(n,cshortstringtype);
              if n.nodetype=stringconstn then
                begin
                  hs:=strpas(tstringconstnode(n).value_str);
                  if string2guid(hs,tmpguid) then
                    begin
                      list.concat(Tai_const.Create_32bit(longint(tmpguid.D1)));
                      list.concat(Tai_const.Create_16bit(tmpguid.D2));
                      list.concat(Tai_const.Create_16bit(tmpguid.D3));
                      for i:=Low(tmpguid.D4) to High(tmpguid.D4) do
                        list.concat(Tai_const.Create_8bit(tmpguid.D4[i]));
                    end
                  else
                    Message(parser_e_improper_guid_syntax);
                end
              else
                Message(parser_e_illegal_expression);
              n.free;
              exit;
            end;
          { bitpacked record? }
          is_packed:=is_packed_record_or_object(def);
          if (is_packed) then
            begin
              { loadbitsize = 8, bitpacked records are always padded to    }
              { a multiple of a byte. packedbitsize will be set separately }
              { for each field                                             }
              initbitpackval(bp,0);
              bp.loadbitsize:=8;
            end;
          { normal record }
          consume(_LKLAMMER);
          curroffset:=0;
          symidx:=0;
          sorg:='';
          srsym:=tsym(def.symtable.SymList[symidx]);
          recsym := nil;
          while token<>_RKLAMMER do
            begin
              s:=pattern;
              sorg:=orgpattern;
              consume(_ID);
              consume(_COLON);
              error := false;
              recsym := tsym(def.symtable.Find(s));
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
                      (tfieldvarsym(recsym).fieldoffset = tfieldvarsym(srsym).fieldoffset)) or
                     { srsym is not assigned after parsing w2 in the      }
                     { typed const in the next example:                   }
                     {   type tr = record case byte of                    }
                     {          1: (l1,l2: dword);                        }
                     {          2: (w1,w2: word);                         }
                     {        end;                                        }
                     {   const r: tr = (w1:1;w2:1;l2:5);                  }
                     (tfieldvarsym(recsym).fieldoffset = curroffset) then
                    begin
                      srsym := recsym;
                      symidx := def.symtable.SymList.indexof(srsym)
                    end
                  { going backwards isn't allowed in any mode }
                  else if (tfieldvarsym(recsym).fieldoffset<curroffset) then
                    begin
                      Message(parser_e_invalid_record_const);
                      error := true;
                    end
                  { Delphi allows you to skip fields }
                  else if (m_delphi in current_settings.modeswitches) then
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
                  if tfieldvarsym(srsym).fieldoffset>curroffset then
                    begin
                      if not(is_packed) then
                        fillbytes:=tfieldvarsym(srsym).fieldoffset-curroffset
                      else
                        begin
                          flush_packed_value(list,bp);
                          { curoffset is now aligned to the next byte }
                          curroffset:=align(curroffset,8);
                          { offsets are in bits in this case }
                          fillbytes:=(tfieldvarsym(srsym).fieldoffset-curroffset) div 8;
                        end;
                      for i:=1 to fillbytes do
                        list.concat(Tai_const.Create_8bit(0))
                    end;

                  { new position }
                  curroffset:=tfieldvarsym(srsym).fieldoffset;
                  if not(is_packed) then
                    inc(curroffset,tfieldvarsym(srsym).vardef.size)
                   else
                     inc(curroffset,tfieldvarsym(srsym).vardef.packedbitsize);

                  { read the data }
                  if not(is_packed) or
                     { only orddefs and enumdefs are bitpacked, as in gcc/gpc }
                     not(tfieldvarsym(srsym).vardef.typ in [orddef,enumdef]) then
                    begin
                      if is_packed then
                        begin
                          flush_packed_value(list,bp);
                          curroffset:=align(curroffset,8);
                        end;
                      read_typed_const_data(list,tfieldvarsym(srsym).vardef);
                    end
                  else
                    begin
                      bp.packedbitsize:=tfieldvarsym(srsym).vardef.packedbitsize;
                      parse_single_packed_const(list,tfieldvarsym(srsym).vardef,bp);
                    end;

                  { keep previous field for checking whether whole }
                  { record was initialized (JM)                    }
                  recsym := srsym;
                  { goto next field }
                  inc(symidx);
                  if symidx<def.symtable.SymList.Count then
                    srsym:=tsym(def.symtable.SymList[symidx])
                  else
                    srsym:=nil;

                  if token=_SEMICOLON then
                    consume(_SEMICOLON)
                  else
                    break;
                end;
            end;

          { are there any fields left, but don't complain if there only
            come other variant parts after the last initialized field }
          if assigned(srsym) and
             (
              (recsym=nil) or
              (tfieldvarsym(srsym).fieldoffset > tfieldvarsym(recsym).fieldoffset)
             ) then
            Message1(parser_w_skipped_fields_after,sorg);

          if not(is_packed) then
            fillbytes:=def.size-curroffset
          else
            begin
              flush_packed_value(list,bp);
              curroffset:=align(curroffset,8);
              fillbytes:=def.size-(curroffset div 8);
            end;
          for i:=1 to fillbytes do
            list.concat(Tai_const.Create_8bit(0));

          consume(_RKLAMMER);
        end;


        procedure parse_objectdef(list:tasmlist;def:tobjectdef);
        var
          n      : tnode;
          i      : longint;
          obj    : tobjectdef;
          srsym  : tsym;
          st     : tsymtable;
          curroffset : aint;
          s,sorg : TIDString;
          vmtwritten : boolean;
        begin
          { no support for packed object }
          if is_packed_record_or_object(def) then
            begin
              Message(type_e_no_const_packed_record);
              exit;
            end;

          { only allow nil for class and interface }
          if is_class_or_interface(def) then
            begin
              n:=comp_expr(true);
              if n.nodetype<>niln then
                begin
                  Message(parser_e_type_const_not_possible);
                  consume_all_until(_SEMICOLON);
                end
              else
                list.concat(Tai_const.Create_sym(nil));
              n.free;
              exit;
            end;

          { for objects we allow it only if it doesn't contain a vmt }
          if (oo_has_vmt in def.objectoptions) and
             (m_fpc in current_settings.modeswitches) then
            begin
              Message(parser_e_type_object_constants);
              exit;
            end;

          consume(_LKLAMMER);
          curroffset:=0;
          vmtwritten:=false;
          while token<>_RKLAMMER do
            begin
              s:=pattern;
              sorg:=orgpattern;
              consume(_ID);
              consume(_COLON);
              srsym:=nil;
              obj:=tobjectdef(def);
              st:=obj.symtable;
              while (srsym=nil) and assigned(st) do
                begin
                  srsym:=tsym(st.Find(s));
                  if assigned(obj) then
                    obj:=obj.childof;
                  if assigned(obj) then
                    st:=obj.symtable
                  else
                    st:=nil;
                end;

              if srsym=nil then
                begin
                  Message1(sym_e_id_not_found,sorg);
                  consume_all_until(_SEMICOLON);
                end
              else
                with tfieldvarsym(srsym) do
                  begin
                    { check position }
                    if fieldoffset<curroffset then
                      message(parser_e_invalid_record_const);

                    { check in VMT needs to be added for TP mode }
                    if not(vmtwritten) and
                       not(m_fpc in current_settings.modeswitches) and
                       (oo_has_vmt in def.objectoptions) and
                       (def.vmt_offset<fieldoffset) then
                      begin
                        for i:=1 to def.vmt_offset-curroffset do
                          list.concat(tai_const.create_8bit(0));
                        list.concat(tai_const.createname(def.vmt_mangledname,0));
                        { this is more general }
                        curroffset:=def.vmt_offset + sizeof(aint);
                        vmtwritten:=true;
                      end;

                    { if needed fill }
                    if fieldoffset>curroffset then
                      for i:=1 to fieldoffset-curroffset do
                        list.concat(Tai_const.Create_8bit(0));

                    { new position }
                    curroffset:=fieldoffset+vardef.size;

                    { read the data }
                    read_typed_const_data(list,vardef);

                    if not try_to_consume(_SEMICOLON) then
                      break;
                  end;
            end;
          if not(m_fpc in current_settings.modeswitches) and
             (oo_has_vmt in def.objectoptions) and
             (def.vmt_offset>=curroffset) then
            begin
              for i:=1 to def.vmt_offset-curroffset do
                list.concat(tai_const.create_8bit(0));
              list.concat(tai_const.createname(def.vmt_mangledname,0));
              { this is more general }
              curroffset:=def.vmt_offset + sizeof(aint);
            end;
          for i:=1 to def.size-curroffset do
            list.concat(Tai_const.Create_8bit(0));
          consume(_RKLAMMER);
        end;

      var
        old_block_type : tblock_type;
      begin
        old_block_type:=block_type;
        block_type:=bt_const;
        case def.typ of
          orddef :
            parse_orddef(list,torddef(def));
          floatdef :
            parse_floatdef(list,tfloatdef(def));
          classrefdef :
            parse_classrefdef(list,tclassrefdef(def));
          pointerdef :
            parse_pointerdef(list,tpointerdef(def));
          setdef :
            parse_setdef(list,tsetdef(def));
          enumdef :
            parse_enumdef(list,tenumdef(def));
          stringdef :
            parse_stringdef(list,tstringdef(def));
          arraydef :
            parse_arraydef(list,tarraydef(def));
          procvardef:
            parse_procvardef(list,tprocvardef(def));
          recorddef:
            parse_recorddef(list,trecorddef(def));
          objectdef:
            parse_objectdef(list,tobjectdef(def));
          errordef:
            begin
               { try to consume something useful }
               if token=_LKLAMMER then
                 consume_all_until(_RKLAMMER)
               else
                 consume_all_until(_SEMICOLON);
            end;
          else
            Message(parser_e_type_const_not_possible);
        end;
        block_type:=old_block_type;
      end;

{$maxfpuregisters default}

    procedure read_typed_const(list:tasmlist;sym:tstaticvarsym);
      var
        storefilepos : tfileposinfo;
        cursectype   : TAsmSectionType;
        valuelist    : tasmlist;
      begin
        { mark the staticvarsym as typedconst }
        include(sym.varoptions,vo_is_typed_const);
        { The variable has a value assigned }
        sym.varstate:=vs_initialised;
        { the variable can't be placed in a register }
        sym.varregable:=vr_none;

        { generate data for typed const }
        storefilepos:=current_filepos;
        current_filepos:=sym.fileinfo;
        if sym.varspez=vs_const then
          cursectype:=sec_rodata
        else
          cursectype:=sec_data;
        maybe_new_object_file(list);
        valuelist:=tasmlist.create;
        read_typed_const_data(valuelist,sym.vardef);

        { Parse hints }
        try_consume_hintdirective(sym.symoptions);

        consume(_SEMICOLON);

        { parse public/external/export/... }
        if (
            (
             (token = _ID) and
             (idtoken in [_EXPORT,_EXTERNAL,_PUBLIC,_CVAR]) and
             (m_cvar_support in current_settings.modeswitches)
            ) or
            (
             (m_mac in current_settings.modeswitches) and
             (
              (cs_external_var in current_settings.localswitches) or
              (cs_externally_visible in current_settings.localswitches)
             )
            )
           ) then
          read_public_and_external(sym);

        { only now add items based on the symbolname, because it may }
        { have been modified by the directives parsed above          }
        new_section(list,cursectype,lower(sym.mangledname),const_align(sym.vardef.alignment));
        if (sym.owner.symtabletype=globalsymtable) or
           create_smartlink or
           (assigned(current_procinfo) and
            (po_inline in current_procinfo.procdef.procoptions)) or
           DLLSource then
          list.concat(Tai_symbol.Createname_global(sym.mangledname,AT_DATA,0))
        else
          list.concat(Tai_symbol.Createname(sym.mangledname,AT_DATA,0));

        { add the parsed value }
        list.concatlist(valuelist);
        valuelist.free;
        list.concat(tai_symbol_end.Createname(sym.mangledname));
        current_filepos:=storefilepos;
      end;

end.
