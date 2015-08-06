{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Routines for the code generation of RTTI data structures

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
unit ncgrtti;

{$i fpcdefs.inc}

interface

    uses
      cclasses,constexp,
      aasmbase,aasmcnst,
      symbase,symconst,symtype,symdef;

    type

      { TRTTIWriter }

      TRTTIWriter=class
      private
        { required internal alignment of the rtti data }
        reqalign: shortint;
        { required packing of all structures except for ttypeinfo and tpropinfo,
          which always use packrecords 1 }
        defaultpacking: shortint;

        procedure fields_write_rtti(st:tsymtable;rt:trttitype);
        procedure params_write_rtti(def:tabstractprocdef;rt:trttitype);
        procedure fields_write_rtti_data(tcb: ttai_typedconstbuilder; def: tabstractrecorddef; rt: trttitype);
        procedure write_rtti_extrasyms(def:Tdef;rt:Trttitype;mainrtti:Tasmsymbol);
        procedure published_write_rtti(st:tsymtable;rt:trttitype);
        function  published_properties_count(st:tsymtable):longint;
        procedure published_properties_write_rtti_data(tcb: ttai_typedconstbuilder; propnamelist: TFPHashObjectList; st: tsymtable);
        procedure collect_propnamelist(propnamelist:TFPHashObjectList;objdef:tobjectdef);
        function  ref_rtti(def:tdef;rt:trttitype):tasmsymbol;
        procedure write_rtti_name(tcb: ttai_typedconstbuilder; def: tdef);
        procedure write_rtti_data(tcb: ttai_typedconstbuilder; def:tdef; rt: trttitype);
        procedure write_child_rtti_data(def:tdef;rt:trttitype);
        procedure write_rtti_reference(tcb: ttai_typedconstbuilder; def: tdef; rt: trttitype);
        procedure write_header(tcb: ttai_typedconstbuilder; def: tdef; typekind: byte);
      public
        constructor create;
        procedure write_rtti(def:tdef;rt:trttitype);
        function  get_rtti_label(def:tdef;rt:trttitype):tasmsymbol;
        function  get_rtti_label_ord2str(def:tdef;rt:trttitype):tasmsymbol;
        function  get_rtti_label_str2ord(def:tdef;rt:trttitype):tasmsymbol;
      end;

    var
      RTTIWriter : TRTTIWriter;


implementation

    uses
       cutils,
       globals,globtype,verbose,systems,
       fmodule, procinfo,
       symtable,symsym,
       aasmtai,aasmdata,
       defutil,
       wpobase
       ;


    const
       rttidefstate : array[trttitype] of tdefstate =
         (ds_rtti_table_written,ds_init_table_written,
         { Objective-C related, does not pass here }
         symconst.ds_none,symconst.ds_none,
         symconst.ds_none,symconst.ds_none);

    type
       TPropNameListItem = class(TFPHashObject)
         propindex : longint;
         propowner : TSymtable;
       end;


{***************************************************************************
                              TRTTIWriter
***************************************************************************}

    procedure TRTTIWriter.write_header(tcb: ttai_typedconstbuilder; def: tdef; typekind: byte);
      var
        name: shortstring;
      begin
        if assigned(def.typesym) then
          name:=ttypesym(def.typesym).realname
        else
          name:='';
        { TTypeInfo, always packed and doesn't need alignment }
        tcb.begin_anonymous_record(
          internaltypeprefixName[itp_rtti_header]+tostr(length(name)),1,1,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        if def.typ=arraydef then
          InternalError(201012211);
        tcb.emit_tai(Tai_const.Create_8bit(typekind),u8inttype);
        tcb.emit_shortstring_const(name);
        tcb.end_anonymous_record;
      end;

    procedure TRTTIWriter.write_rtti_name(tcb: ttai_typedconstbuilder; def: tdef);
      begin
         if is_open_array(def) then
           { open arrays never have a typesym with a name, since you cannot
             define an "open array type". Kylix prints the type of the
             elements in the array in this case (so together with the pfArray
             flag, you can reconstruct the full typename, I assume (JM))
           }
           def:=tarraydef(def).elementdef;
         { name }
         if assigned(def.typesym) then
           tcb.emit_shortstring_const(ttypesym(def.typesym).realname)
         else
           tcb.emit_shortstring_const('');
      end;

    { writes a 32-bit count followed by array of field infos for given symtable }
    procedure TRTTIWriter.fields_write_rtti_data(tcb: ttai_typedconstbuilder; def: tabstractrecorddef; rt: trttitype);
      var
        i   : longint;
        sym : tsym;
        fieldcnt: longint;
        st: tsymtable;
        fields: tfplist;
        parentrtti: boolean;
      begin
        fieldcnt:=0;
        parentrtti:=false;
        st:=def.symtable;
        fields:=tfplist.create;
        fields.capacity:=st.symlist.count+1;
        { For objects, treat parent (if any) as a field with offset 0. This
          provides correct handling of entire instance with RTL rtti routines. }
        if (def.typ=objectdef) and (tobjectdef(def).objecttype=odt_object) and
            Assigned(tobjectdef(def).childof) and
            ((rt=fullrtti) or (tobjectdef(def).childof.needs_inittable)) then
           begin
             parentrtti:=true;
             inc(fieldcnt);
           end;

        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (tsym(sym).typ=fieldvarsym) and
               not(sp_static in tsym(sym).symoptions) and
               (
                (rt=fullrtti) or
                tfieldvarsym(sym).vardef.needs_inittable
               ) and
               not is_objc_class_or_protocol(tfieldvarsym(sym).vardef) then
              begin
                fields.add(tfieldvarsym(sym));
                inc(fieldcnt);
              end;
          end;
        { insert field count before data }
        tcb.emit_ord_const(fieldcnt,u32inttype);
        { parent object? }
        if parentrtti then
          begin
            write_rtti_reference(tcb,tobjectdef(def).childof,rt);
            tcb.emit_ord_const(0,ptruinttype);
          end;
        { fields }
        for i:=0 to fields.count-1 do
          begin
            sym:=tsym(fields[i]);
            write_rtti_reference(tcb,tfieldvarsym(sym).vardef,rt);
            tcb.emit_ord_const(tfieldvarsym(sym).fieldoffset,ptruinttype);
          end;
        fields.free;
      end;


    procedure TRTTIWriter.fields_write_rtti(st:tsymtable;rt:trttitype);
      var
        i   : longint;
        sym : tsym;
      begin
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (tsym(sym).typ=fieldvarsym) and
               not(sp_static in tsym(sym).symoptions) and
               (
                (rt=fullrtti) or
                tfieldvarsym(sym).vardef.needs_inittable
               ) then
              write_rtti(tfieldvarsym(sym).vardef,rt);
          end;
      end;


    procedure TRTTIWriter.params_write_rtti(def:tabstractprocdef;rt:trttitype);
      var
        i   : longint;
        sym : tparavarsym;
      begin
        for i:=0 to def.paras.count-1 do
          begin
            sym:=tparavarsym(def.paras[i]);
            if not (vo_is_hidden_para in sym.varoptions) then
              write_rtti(sym.vardef,rt);
          end;
      end;


    procedure TRTTIWriter.published_write_rtti(st:tsymtable;rt:trttitype);
      var
        i   : longint;
        sym : tsym;
      begin
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (sym.visibility=vis_published) then
              begin
                case tsym(sym).typ of
                  propertysym:
                    write_rtti(tpropertysym(sym).propdef,rt);
                  fieldvarsym:
                    write_rtti(tfieldvarsym(sym).vardef,rt);
                end;
              end;
          end;
      end;


    function TRTTIWriter.published_properties_count(st:tsymtable):longint;
      var
        i   : longint;
        sym : tsym;
      begin
        result:=0;
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (tsym(sym).typ=propertysym) and
               (sym.visibility=vis_published) then
              inc(result);
          end;
      end;


    procedure TRTTIWriter.collect_propnamelist(propnamelist:TFPHashObjectList;objdef:tobjectdef);
      var
        i   : longint;
        sym : tsym;
        pn  : tpropnamelistitem;
      begin
        if assigned(objdef.childof) then
          collect_propnamelist(propnamelist,objdef.childof);
        for i:=0 to objdef.symtable.SymList.Count-1 do
          begin
            sym:=tsym(objdef.symtable.SymList[i]);
            if (tsym(sym).typ=propertysym) and
               (sym.visibility=vis_published) then
              begin
                pn:=TPropNameListItem(propnamelist.Find(tsym(sym).name));
                if not assigned(pn) then
                  begin
                     pn:=tpropnamelistitem.create(propnamelist,tsym(sym).name);
                     pn.propindex:=propnamelist.count-1;
                     pn.propowner:=tsym(sym).owner;
                  end;
             end;
          end;
      end;


    procedure TRTTIWriter.published_properties_write_rtti_data(tcb: ttai_typedconstbuilder; propnamelist:TFPHashObjectList;st:tsymtable);
      var
        i : longint;
        sym : tsym;
        proctypesinfo : byte;
        propnameitem  : tpropnamelistitem;

        procedure writeaccessproc(pap:tpropaccesslisttypes; shiftvalue : byte; unsetvalue: byte);
        var
           typvalue : byte;
           hp : ppropaccesslistitem;
           extnumber: longint;
           address,space : longint;
           def : tdef;
           hpropsym : tpropertysym;
           propaccesslist : tpropaccesslist;
        begin
           hpropsym:=tpropertysym(sym);
           repeat
             propaccesslist:=hpropsym.propaccesslist[pap];
             if not propaccesslist.empty then
               break;
             hpropsym:=hpropsym.overriddenpropsym;
           until not assigned(hpropsym);
           if not(assigned(propaccesslist) and assigned(propaccesslist.firstsym))  then
             begin
               tcb.emit_tai(Tai_const.Create_int_codeptr(unsetvalue),voidcodepointertype);
               typvalue:=3;
             end
           else if propaccesslist.firstsym^.sym.typ=fieldvarsym then
             begin
                address:=0;
                hp:=propaccesslist.firstsym;
                def:=nil;
                while assigned(hp) do
                  begin
                     case hp^.sltype of
                       sl_load :
                         begin
                           def:=tfieldvarsym(hp^.sym).vardef;
                           inc(address,tfieldvarsym(hp^.sym).fieldoffset);
                         end;
                       sl_subscript :
                         begin
                           if not(assigned(def) and
                                  ((def.typ=recorddef) or
                                   is_object(def))) then
                             internalerror(200402171);
                           inc(address,tfieldvarsym(hp^.sym).fieldoffset);
                           def:=tfieldvarsym(hp^.sym).vardef;
                         end;
                       sl_vec :
                         begin
                           if not(assigned(def) and (def.typ=arraydef)) then
                             internalerror(200402172);
                           def:=tarraydef(def).elementdef;
                           {Hp.value is a Tconstexprint, which can be rather large,
                            sanity check for longint overflow.}
                           space:=(high(address)-address) div def.size;
                           if int64(space)<hp^.value then
                             internalerror(200706101);
                           inc(address,int64(def.size*hp^.value));
                         end;
                     end;
                     hp:=hp^.next;
                  end;
                tcb.emit_tai(Tai_const.Create_int_codeptr(address),voidcodepointertype);
                typvalue:=0;
             end
           else
             begin
                { When there was an error then procdef is not assigned }
                if not assigned(propaccesslist.procdef) then
                  exit;
                if not(po_virtualmethod in tprocdef(propaccesslist.procdef).procoptions) or
                   is_objectpascal_helper(tprocdef(propaccesslist.procdef).struct) then
                  begin
                    tcb.emit_procdef_const(tprocdef(propaccesslist.procdef));
                    typvalue:=1;
                  end
                else
                  begin
                    { virtual method, write vmt offset }
                    extnumber:=tprocdef(propaccesslist.procdef).extnumber;
                    tcb.emit_tai(Tai_const.Create_int_codeptr(
                      tobjectdef(tprocdef(propaccesslist.procdef).struct).vmtmethodoffset(extnumber)),
                      voidcodepointertype);
                    { register for wpo }
                    tobjectdef(tprocdef(propaccesslist.procdef).struct).register_vmt_call(extnumber);
                    {$ifdef vtentry}
                    { not sure if we can insert those vtentry symbols safely here }
                    {$error register methods used for published properties}
                    {$endif vtentry}
                    typvalue:=2;
                  end;
             end;
           proctypesinfo:=proctypesinfo or (typvalue shl shiftvalue);
        end;

      begin
        tcb.begin_anonymous_record('',defaultpacking,reqalign,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        tcb.emit_ord_const(published_properties_count(st),u16inttype);
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (sym.typ=propertysym) and
               (sym.visibility=vis_published) then
              begin
                { TPropInfo is a packed record (even on targets that require
                  alignment), but it starts aligned }
                tcb.begin_anonymous_record(
                  internaltypeprefixName[itp_rtti_prop]+tostr(length(tpropertysym(sym).realname)),
                  1,reqalign,
                  targetinfos[target_info.system]^.alignment.recordalignmin,
                  targetinfos[target_info.system]^.alignment.maxCrecordalign);
                if ppo_indexed in tpropertysym(sym).propoptions then
                  proctypesinfo:=$40
                else
                  proctypesinfo:=0;
                write_rtti_reference(tcb,tpropertysym(sym).propdef,fullrtti);
                writeaccessproc(palt_read,0,0);
                writeaccessproc(palt_write,2,0);
                { is it stored ? }
                if not(ppo_stored in tpropertysym(sym).propoptions) then
                  begin
                    { no, so put a constant zero }
                    tcb.emit_tai(Tai_const.Create_nil_codeptr,voidcodepointertype);
                    proctypesinfo:=proctypesinfo or (3 shl 4);
                  end
                else
                  writeaccessproc(palt_stored,4,1); { maybe; if no procedure put a constant 1 (=true) }
                tcb.emit_ord_const(tpropertysym(sym).index,u32inttype);
                tcb.emit_ord_const(tpropertysym(sym).default,u32inttype);
                propnameitem:=TPropNameListItem(propnamelist.Find(tpropertysym(sym).name));
                if not assigned(propnameitem) then
                  internalerror(200512201);
                tcb.emit_ord_const(propnameitem.propindex,u16inttype);
                tcb.emit_ord_const(proctypesinfo,u8inttype);
                tcb.emit_shortstring_const(tpropertysym(sym).realname);
                tcb.end_anonymous_record;
             end;
          end;
        tcb.end_anonymous_record;
      end;


    procedure TRTTIWriter.write_rtti_data(tcb: ttai_typedconstbuilder; def: tdef; rt: trttitype);

        procedure unknown_rtti(def:tstoreddef);
        begin
          tcb.emit_ord_const(tkUnknown,u8inttype);
          write_rtti_name(tcb,def);
        end;

        procedure variantdef_rtti(def:tvariantdef);
        begin
          write_header(tcb,def,tkVariant);
        end;

        procedure stringdef_rtti(def:tstringdef);
        begin
          case def.stringtype of
            st_ansistring:
              begin
                write_header(tcb,def,tkAString);
                { align }
                tcb.begin_anonymous_record(
                  internaltypeprefixName[itp_rtti_ansistr],
                  defaultpacking,reqalign,
                  targetinfos[target_info.system]^.alignment.recordalignmin,
                  targetinfos[target_info.system]^.alignment.maxCrecordalign);
                tcb.emit_ord_const(def.encoding,u16inttype);
                tcb.end_anonymous_record;
              end;

            st_widestring:
              write_header(tcb,def,tkWString);

            st_unicodestring:
              write_header(tcb,def,tkUString);

            st_longstring:
              write_header(tcb,def,tkLString);

            st_shortstring:
              begin
                 write_header(tcb,def,tkSString);
                 tcb.emit_ord_const(def.len,u8inttype);
              end;
          end;
        end;

        procedure enumdef_rtti(def: tenumdef);
        var
           i  : integer;
           hp : tenumsym;
        begin
          write_header(tcb,def,tkEnumeration);
          { align; the named fields are so that we can let the compiler
            calculate the string offsets later on }
          tcb.next_field_name:='size_start_rec';
          tcb.begin_anonymous_record('',defaultpacking,reqalign,
            targetinfos[target_info.system]^.alignment.recordalignmin,
            targetinfos[target_info.system]^.alignment.maxCrecordalign);
          case longint(def.size) of
            1 :
              tcb.emit_ord_const(otUByte,u8inttype);
            2 :
              tcb.emit_ord_const(otUWord,u8inttype);
            4 :
              tcb.emit_ord_const(otULong,u8inttype);
          end;
          { we need to align by Tconstptruint here to satisfy the alignment
            rules set by records: in the typinfo unit we overlay a TTypeData
            record on this data, which at the innermost variant record needs an
            alignment of TConstPtrUint due to e.g. the "CompType" member for
            tkSet (also the "BaseType" member for tkEnumeration).

            We need to adhere to this, otherwise things will break. }
          tcb.next_field_name:='min_max_rec';
          tcb.begin_anonymous_record('',defaultpacking,reqalign,
            targetinfos[target_info.system]^.alignment.recordalignmin,
            targetinfos[target_info.system]^.alignment.maxCrecordalign);
          tcb.emit_ord_const(def.min,s32inttype);
          tcb.emit_ord_const(def.max,s32inttype);
          tcb.next_field_name:='basetype_array_rec';
          { all strings must appear right after each other -> from now on
            packrecords 1 (but the start must still be aligned) }
          tcb.begin_anonymous_record('',1,reqalign,
            targetinfos[target_info.system]^.alignment.recordalignmin,
            targetinfos[target_info.system]^.alignment.maxCrecordalign);
          { write base type }
          write_rtti_reference(tcb,def.basedef,rt);
          for i:=0 to def.symtable.SymList.Count-1 do
            begin
              hp:=tenumsym(def.symtable.SymList[i]);
              if hp.value<def.minval then
                continue
              else
              if hp.value>def.maxval then
                break;
              tcb.next_field_name:='enumname'+tostr(hp.SymId);
              tcb.emit_shortstring_const(hp.realname);
            end;
          { write unit name }
          tcb.emit_shortstring_const(current_module.realmodulename^);
          { write zero which is required by RTL }
          tcb.emit_ord_const(0,u8inttype);
          { terminate all records }
          tcb.end_anonymous_record;
          tcb.end_anonymous_record;
          tcb.end_anonymous_record;
        end;

        procedure orddef_rtti(def:torddef);

          procedure dointeger(typekind: byte);
          const
            trans : array[tordtype] of byte =
              (otUByte{otNone},
               otUByte,otUWord,otULong,otUByte{otNone},
               otSByte,otSWord,otSLong,otUByte{otNone},
               otUByte,otUWord,otULong,otUByte,
               otSByte,otSWord,otSLong,otSByte,
               otUByte,otUWord,otUByte);
          begin
            write_header(tcb,def,typekind);
            tcb.begin_anonymous_record(
              internaltypeprefixName[itp_rtti_ord_outer],
              defaultpacking,reqalign,
              targetinfos[target_info.system]^.alignment.recordalignmin,
              targetinfos[target_info.system]^.alignment.maxCrecordalign);
            tcb.emit_ord_const(byte(trans[def.ordtype]),u8inttype);
            tcb.begin_anonymous_record(
              internaltypeprefixName[itp_rtti_ord_inner],
              defaultpacking,reqalign,
              targetinfos[target_info.system]^.alignment.recordalignmin,
              targetinfos[target_info.system]^.alignment.maxCrecordalign);
            {Convert to longint to smuggle values in high(longint)+1..high(cardinal) into asmlist.}
            tcb.emit_ord_const(longint(def.low.svalue),s32inttype);
            tcb.emit_ord_const(longint(def.high.svalue),s32inttype);
            tcb.end_anonymous_record;
            tcb.end_anonymous_record;
          end;

        begin
          case def.ordtype of
            s64bit :
              begin
                write_header(tcb,def,tkInt64);
                tcb.begin_anonymous_record(
                  internaltypeprefixName[itp_rtti_ord_64bit],
                  defaultpacking,reqalign,
                  targetinfos[target_info.system]^.alignment.recordalignmin,
                  targetinfos[target_info.system]^.alignment.maxCrecordalign);
                { low }
                tcb.emit_ord_const(def.low.svalue,s64inttype);
                { high }
                tcb.emit_ord_const(def.high.svalue,s64inttype);
                tcb.end_anonymous_record;
              end;
            u64bit :
              begin
                write_header(tcb,def,tkQWord);
                tcb.begin_anonymous_record(
                  internaltypeprefixName[itp_rtti_ord_64bit],
                  defaultpacking,reqalign,
                  targetinfos[target_info.system]^.alignment.recordalignmin,
                  targetinfos[target_info.system]^.alignment.maxCrecordalign);
                { use svalue because emit_ord_const accepts int64, prevents
                  range check errors }
                { low }
                tcb.emit_ord_const(def.low.svalue,s64inttype);
                { high }
                tcb.emit_ord_const(def.high.svalue,s64inttype);
                tcb.end_anonymous_record;
              end;
            pasbool8:
                dointeger(tkBool);
            uchar:
                dointeger(tkChar);
            uwidechar:
                dointeger(tkWChar);
            scurrency:
              begin
                write_header(tcb,def,tkFloat);
                tcb.begin_anonymous_record(
                  internaltypeprefixName[itp_1byte],
                  defaultpacking,reqalign,
                  targetinfos[target_info.system]^.alignment.recordalignmin,
                  targetinfos[target_info.system]^.alignment.maxCrecordalign);
                tcb.emit_ord_const(ftCurr,u8inttype);
                tcb.end_anonymous_record;
              end;
            else
              dointeger(tkInteger);
          end;
        end;


        procedure floatdef_rtti(def:tfloatdef);
        const
          {tfloattype = (s32real,s64real,s80real,sc80real,s64bit,s128bit);}
          translate : array[tfloattype] of byte =
             (ftSingle,ftDouble,ftExtended,ftExtended,ftComp,ftCurr,ftFloat128);
        begin
           write_header(tcb,def,tkFloat);
           tcb.begin_anonymous_record(
             internaltypeprefixName[itp_1byte],
             defaultpacking,reqalign,
             targetinfos[target_info.system]^.alignment.recordalignmin,
             targetinfos[target_info.system]^.alignment.maxCrecordalign);
           tcb.emit_ord_const(translate[def.floattype],u8inttype);
           tcb.end_anonymous_record;
        end;


        procedure setdef_rtti(def:tsetdef);
        begin
           write_header(tcb,def,tkSet);
           tcb.begin_anonymous_record(
             internaltypeprefixName[itp_1byte],
             defaultpacking,reqalign,
             targetinfos[target_info.system]^.alignment.recordalignmin,
             targetinfos[target_info.system]^.alignment.maxCrecordalign);
           case def.size of
             1:
               tcb.emit_ord_const(otUByte,u8inttype);
             2:
               tcb.emit_ord_const(otUWord,u8inttype);
             4:
               tcb.emit_ord_const(otULong,u8inttype);
             else
               tcb.emit_ord_const(otUByte,u8inttype);
           end;
           { since this record has an alignment of reqalign, its size will also
             be rounded up to a multiple of reqalign -> the following value will
             also be properly aligned without having to start an extra record }
           tcb.end_anonymous_record;
           write_rtti_reference(tcb,def.elementdef,rt);
        end;


        procedure arraydef_rtti(def:tarraydef);
          var
            i,dimcount: byte;
            totalcount: asizeuint;
            finaldef: tdef;
            curdef:tarraydef;
        begin
           if ado_IsDynamicArray in def.arrayoptions then
             tcb.emit_ord_const(tkDynArray,u8inttype)
           else
             tcb.emit_ord_const(tkArray,u8inttype);
           write_rtti_name(tcb,def);

           if not(ado_IsDynamicArray in def.arrayoptions) then
             begin
               { remember tha last instruction. we will need to insert some
                 calculated values after it }
               finaldef:=def;
               totalcount:=1;
               dimcount:=0;
               repeat
                 curdef:=tarraydef(finaldef);
                 finaldef:=curdef.elementdef;
                 { Dims[i] PTypeInfo }
                 inc(dimcount);
                 totalcount:=totalcount*curdef.elecount;
               until (finaldef.typ<>arraydef) or
                     (ado_IsDynamicArray in tarraydef(finaldef).arrayoptions);
               tcb.begin_anonymous_record(
                 internaltypeprefixName[itp_rtti_normal_array]+tostr(dimcount),
                 defaultpacking,reqalign,
                 targetinfos[target_info.system]^.alignment.recordalignmin,
                 targetinfos[target_info.system]^.alignment.maxCrecordalign);
               { total size = elecount * elesize of the first arraydef }
               tcb.emit_tai(Tai_const.Create_pint(def.elecount*def.elesize),ptruinttype);
               { total element count }
               tcb.emit_tai(Tai_const.Create_pint(pint(totalcount)),ptruinttype);
               { last dimension element type }
               tcb.emit_tai(Tai_const.Create_sym(ref_rtti(curdef.elementdef,rt)),voidpointertype);
               { dimension count }
               tcb.emit_ord_const(dimcount,u8inttype);
               finaldef:=def;
               { ranges of the dimensions }
               for i:=1 to dimcount do
                 begin
                   curdef:=tarraydef(finaldef);
                   finaldef:=curdef.elementdef;
                   { Dims[i] PTypeInfo }
                   write_rtti_reference(tcb,curdef.rangedef,rt);
                 end;
             end
           else
             { write a delphi almost compatible dyn. array entry:
               there are two types, eltype and eltype2, the latter is nil if the element type needs
               no finalization, the former is always valid, delphi has this swapped, but for
               compatibility with older fpc versions we do it different, to be delphi compatible,
               the names are swapped in typinfo.pp
             }
             begin
               tcb.begin_anonymous_record(
                 internaltypeprefixName[itp_rtti_dyn_array],
                 defaultpacking,reqalign,
                 targetinfos[target_info.system]^.alignment.recordalignmin,
                 targetinfos[target_info.system]^.alignment.maxCrecordalign);
               { size of elements }
               tcb.emit_tai(Tai_const.Create_pint(def.elesize),ptruinttype);
               { element type }
               write_rtti_reference(tcb,def.elementdef,rt);
               { variant type }
               tcb.emit_ord_const(tstoreddef(def.elementdef).getvardef,s32inttype);
               { element type }
               if def.elementdef.needs_inittable then
                 write_rtti_reference(tcb,def.elementdef,rt)
               else
                 tcb.emit_tai(Tai_const.Create_nil_dataptr,voidpointertype);
               { write unit name }
               tcb.emit_shortstring_const(current_module.realmodulename^);
             end;
          tcb.end_anonymous_record;
        end;

        procedure classrefdef_rtti(def:tclassrefdef);
        begin
          write_header(tcb,def,tkClassRef);
          { will be aligned thanks to encompassing record }
          write_rtti_reference(tcb,def.pointeddef,rt);
        end;

        procedure pointerdef_rtti(def:tpointerdef);
        begin
          write_header(tcb,def,tkPointer);
          { will be aligned thanks to encompassing record }
          write_rtti_reference(tcb,def.pointeddef,rt);
        end;

        procedure recorddef_rtti(def:trecorddef);
        begin
           write_header(tcb,def,tkRecord);
           { need extra reqalign record, because otherwise the u32 int will
             only be aligned to 4 even on 64 bit target (while the rtti code
             in typinfo expects alignments to sizeof(pointer)) }
           tcb.begin_anonymous_record('',defaultpacking,reqalign,
             targetinfos[target_info.system]^.alignment.recordalignmin,
             targetinfos[target_info.system]^.alignment.maxCrecordalign);
           tcb.emit_ord_const(def.size,u32inttype);
           fields_write_rtti_data(tcb,def,rt);
           tcb.end_anonymous_record;
        end;


        procedure procvardef_rtti(def:tprocvardef);
           const
             ProcCallOptionToCallConv: array[tproccalloption] of byte = (
              { pocall_none       } 0,
              { pocall_cdecl      } 1,
              { pocall_cppdecl    } 5,
              { pocall_far16      } 6,
              { pocall_oldfpccall } 7,
              { pocall_internproc } 8,
              { pocall_syscall    } 9,
              { pocall_pascal     } 2,
              { pocall_register   } 0,
              { pocall_safecall   } 4,
              { pocall_stdcall    } 3,
              { pocall_softfloat  } 10,
              { pocall_mwpascal   } 11,
              { pocall_interrupt  } 12
             );

           procedure write_param_flag(parasym:tparavarsym);
             var
               paraspec : byte;
             begin
               case parasym.varspez of
                 vs_value   : paraspec := 0;
                 vs_const   : paraspec := pfConst;
                 vs_var     : paraspec := pfVar;
                 vs_out     : paraspec := pfOut;
                 vs_constref: paraspec := pfConstRef;
                 else
                   internalerror(2013112904);
               end;
               { Kylix also seems to always add both pfArray and pfReference
                 in this case
               }
               if is_open_array(parasym.vardef) then
                 paraspec:=paraspec or pfArray or pfReference;
               { and these for classes and interfaces (maybe because they
                 are themselves addresses?)
               }
               if is_class_or_interface(parasym.vardef) then
                 paraspec:=paraspec or pfAddress;
               { set bits run from the highest to the lowest bit on
                 big endian systems
               }
               if (target_info.endian = endian_big) then
                 paraspec:=reverse_byte(paraspec);
               { write flags for current parameter }
               tcb.emit_ord_const(paraspec,u8inttype);
             end;

           procedure write_para(parasym:tparavarsym);
             begin
               { only store user visible parameters }
               if not(vo_is_hidden_para in parasym.varoptions) then
                 begin
                   { write flags for current parameter }
                   write_param_flag(parasym);
                   { write name of current parameter }
                   tcb.emit_shortstring_const(parasym.realname);
                   { write name of type of current parameter }
                   write_rtti_name(tcb,parasym.vardef);
                 end;
             end;

           procedure write_procedure_param(parasym:tparavarsym);
             begin
               { only store user visible parameters }
               if not(vo_is_hidden_para in parasym.varoptions) then
                 begin
                   { every parameter is expected to start aligned }
                   tcb.begin_anonymous_record(
                     internaltypeprefixName[itp_rtti_proc_param]+tostr(length(parasym.realname)),
                     defaultpacking,reqalign,
                     targetinfos[target_info.system]^.alignment.recordalignmin,
                     targetinfos[target_info.system]^.alignment.maxCrecordalign);
                   { write flags for current parameter }
                   write_param_flag(parasym);
                   { write param type }
                   write_rtti_reference(tcb,parasym.vardef,fullrtti);
                   { write name of current parameter }
                   tcb.emit_shortstring_const(parasym.realname);
                   tcb.end_anonymous_record;
                 end;
             end;

        var
          methodkind : byte;
          i : integer;
        begin
          if po_methodpointer in def.procoptions then
            begin
               { write method id and name }
               write_header(tcb,def,tkMethod);
               tcb.begin_anonymous_record('',defaultpacking,reqalign,
                 targetinfos[target_info.system]^.alignment.recordalignmin,
                 targetinfos[target_info.system]^.alignment.maxCrecordalign);

               { write kind of method }
               case def.proctypeoption of
                 potype_constructor: methodkind:=mkConstructor;
                 potype_destructor: methodkind:=mkDestructor;
                 potype_class_constructor: methodkind:=mkClassConstructor;
                 potype_class_destructor: methodkind:=mkClassDestructor;
                 potype_operator: methodkind:=mkOperatorOverload;
                 potype_procedure:
                   if po_classmethod in def.procoptions then
                     methodkind:=mkClassProcedure
                   else
                     methodkind:=mkProcedure;
                 potype_function:
                   if po_classmethod in def.procoptions then
                     methodkind:=mkClassFunction
                   else
                     methodkind:=mkFunction;
               else
                 begin
                   if def.returndef = voidtype then
                     methodkind:=mkProcedure
                   else
                     methodkind:=mkFunction;
                 end;
               end;
               tcb.emit_ord_const(methodkind,u8inttype);

               { write parameter info. The parameters must be written in reverse order
                 if this method uses right to left parameter pushing! }
               tcb.emit_ord_const(def.maxparacount,u8inttype);

               for i:=0 to def.paras.count-1 do
                 write_para(tparavarsym(def.paras[i]));

               if (methodkind=mkFunction) or (methodkind=mkClassFunction) then
               begin
                 { write name of result type }
                 write_rtti_name(tcb,def.returndef);
                 { enclosing record takes care of alignment }
                 { write result typeinfo }
                 write_rtti_reference(tcb,def.returndef,fullrtti);
               end;

               { write calling convention }
               tcb.emit_ord_const(ProcCallOptionToCallConv[def.proccalloption],u8inttype);

               { enclosing record takes care of alignment }
               { write params typeinfo }
               for i:=0 to def.paras.count-1 do
                 if not(vo_is_hidden_para in tparavarsym(def.paras[i]).varoptions) then
                   write_rtti_reference(tcb,tparavarsym(def.paras[i]).vardef,fullrtti);
               tcb.end_anonymous_record;
            end
          else
            begin
              write_header(tcb,def,tkProcvar);
              tcb.begin_anonymous_record('',defaultpacking,reqalign,
                targetinfos[target_info.system]^.alignment.recordalignmin,
                targetinfos[target_info.system]^.alignment.maxCrecordalign);

              { flags }
              tcb.emit_ord_const(0,u8inttype);
              { write calling convention }
              tcb.emit_ord_const(ProcCallOptionToCallConv[def.proccalloption],u8inttype);
              { enclosing record takes care of alignment }
              { write result typeinfo }
              write_rtti_reference(tcb,def.returndef,fullrtti);
              { write parameter count }
              tcb.emit_ord_const(def.maxparacount,u8inttype);
              for i:=0 to def.paras.count-1 do
                write_procedure_param(tparavarsym(def.paras[i]));
              tcb.end_anonymous_record;
            end;
        end;


        procedure objectdef_rtti(def: tobjectdef);

          procedure objectdef_rtti_fields(def:tobjectdef);
          begin
            tcb.emit_ord_const(def.size, u32inttype);
            { enclosing record takes care of alignment }
            fields_write_rtti_data(tcb,def,rt);
          end;

          procedure objectdef_rtti_interface_init(def:tobjectdef);
          begin
            tcb.emit_ord_const(def.size, u32inttype);
          end;

          procedure objectdef_rtti_class_full(def:tobjectdef);
          var
            propnamelist : TFPHashObjectList;
          begin
            { Collect unique property names with nameindex }
            propnamelist:=TFPHashObjectList.Create;
            collect_propnamelist(propnamelist,def);

            if not is_objectpascal_helper(def) then
              if (oo_has_vmt in def.objectoptions) then
                tcb.emit_tai(
                  Tai_const.Createname(def.vmt_mangledname,AT_DATA,0),
                  cpointerdef.getreusable(def.vmt_def))
              else
                tcb.emit_tai(Tai_const.Create_nil_dataptr,voidpointertype);

            { write parent typeinfo }
            write_rtti_reference(tcb,def.childof,fullrtti);

            { write typeinfo of extended type }
            if is_objectpascal_helper(def) then
              if assigned(def.extendeddef) then
                write_rtti_reference(tcb,def.extendeddef,fullrtti)
              else
                InternalError(2011033001);

            { total number of unique properties }
            tcb.emit_ord_const(propnamelist.count,u16inttype);

            { write unit name }
            tcb.emit_shortstring_const(current_module.realmodulename^);

            { write published properties for this object }
            published_properties_write_rtti_data(tcb,propnamelist,def.symtable);

            propnamelist.free;
          end;

          procedure objectdef_rtti_interface_full(def:tobjectdef);
          var
            propnamelist : TFPHashObjectList;
            { if changed to a set, make sure it's still a byte large, and
              swap appropriately when cross-compiling
            }
            IntfFlags: byte;
          begin
            { Collect unique property names with nameindex }
            propnamelist:=TFPHashObjectList.Create;
            collect_propnamelist(propnamelist,def);

            { write parent typeinfo }
            write_rtti_reference(tcb,def.childof,fullrtti);

            { interface: write flags, iid and iidstr }
            IntfFlags:=0;
            if assigned(def.iidguid) then
              IntfFlags:=IntfFlags or (1 shl ord(ifHasGuid));
            if assigned(def.iidstr) then
              IntfFlags:=IntfFlags or (1 shl ord(ifHasStrGUID));
            if (def.objecttype=odt_dispinterface) then
              IntfFlags:=IntfFlags or (1 shl ord(ifDispInterface));
            if (target_info.endian=endian_big) then
              IntfFlags:=reverse_byte(IntfFlags);
              {
              ifDispatch, }
            tcb.emit_ord_const(IntfFlags,u8inttype);
            tcb.begin_anonymous_record('',defaultpacking,reqalign,
              targetinfos[target_info.system]^.alignment.recordalignmin,
              targetinfos[target_info.system]^.alignment.maxCrecordalign);

            tcb.emit_guid_const(def.iidguid^);

            { write unit name }
            tcb.emit_shortstring_const(current_module.realmodulename^);

            tcb.begin_anonymous_record('',defaultpacking,reqalign,
              targetinfos[target_info.system]^.alignment.recordalignmin,
              targetinfos[target_info.system]^.alignment.maxCrecordalign);

            { write iidstr }
            if assigned(def.iidstr) then
              tcb.emit_shortstring_const(def.iidstr^)
            else
              tcb.emit_shortstring_const('');

            { write published properties for this object }
            published_properties_write_rtti_data(tcb,propnamelist,def.symtable);

            tcb.end_anonymous_record;
            tcb.end_anonymous_record;

            propnamelist.free;
          end;

        begin
           case def.objecttype of
             odt_class:
               tcb.emit_ord_const(tkclass,u8inttype);
             odt_object:
               tcb.emit_ord_const(tkobject,u8inttype);
             odt_dispinterface,
             odt_interfacecom:
               tcb.emit_ord_const(tkInterface,u8inttype);
             odt_interfacecorba:
               tcb.emit_ord_const(tkinterfaceCorba,u8inttype);
             odt_helper:
               tcb.emit_ord_const(tkhelper,u8inttype);
             else
               internalerror(200611034);
           end;

           { generate the name }
           tcb.emit_shortstring_const(def.objrealname^);

           tcb.begin_anonymous_record('',defaultpacking,reqalign,
             targetinfos[target_info.system]^.alignment.recordalignmin,
             targetinfos[target_info.system]^.alignment.maxCrecordalign);

           case rt of
             initrtti :
               begin
                 if def.objecttype in [odt_class,odt_object,odt_helper] then
                   objectdef_rtti_fields(def)
                 else
                   objectdef_rtti_interface_init(def);
               end;
             fullrtti :
               begin
                 case def.objecttype of
                   odt_helper,
                   odt_class:
                     objectdef_rtti_class_full(def);
                   odt_object:
                     objectdef_rtti_fields(def);
                 else
                   objectdef_rtti_interface_full(def);
                 end;
               end;
           end;
           tcb.end_anonymous_record;
        end;

      begin
        case def.typ of
          variantdef :
            variantdef_rtti(tvariantdef(def));
          stringdef :
            stringdef_rtti(tstringdef(def));
          enumdef :
            enumdef_rtti(tenumdef(def));
          orddef :
            orddef_rtti(torddef(def));
          floatdef :
            floatdef_rtti(tfloatdef(def));
          setdef :
            setdef_rtti(tsetdef(def));
          procvardef :
            procvardef_rtti(tprocvardef(def));
          arraydef :
            begin
              if ado_IsBitPacked in tarraydef(def).arrayoptions then
                unknown_rtti(tstoreddef(def))
              else
                arraydef_rtti(tarraydef(def));
            end;
          recorddef :
            begin
              if trecorddef(def).is_packed then
                unknown_rtti(tstoreddef(def))
              else
                recorddef_rtti(trecorddef(def));
            end;
          objectdef :
            objectdef_rtti(tobjectdef(def));
          classrefdef :
            classrefdef_rtti(tclassrefdef(def));
          pointerdef :
            pointerdef_rtti(tpointerdef(def));
          else
            unknown_rtti(tstoreddef(def));
        end;
      end;


    function enumsym_compare_name(item1, item2: pointer): Integer;
      var
        enum1: tenumsym absolute item1;
        enum2: tenumsym absolute item2;
      begin
        if enum1=enum2 then
          result:=0
        else if enum1.name>enum2.name then
          result:=1
        else
          { there can't be equal names, identifiers are unique }
          result:=-1;
      end;


    function enumsym_compare_value(item1, item2: pointer): Integer;
      var
        enum1: tenumsym absolute item1;
        enum2: tenumsym absolute item2;
      begin
        if enum1.value>enum2.value then
          result:=1
        else if enum1.value<enum2.value then
          result:=-1
        else
          result:=0;
      end;


    procedure TRTTIWriter.write_rtti_extrasyms(def:Tdef;rt:Trttitype;mainrtti:Tasmsymbol);

        type Penumsym = ^Tenumsym;

        { Writes a helper table for accelerated conversion of ordinal enum values to strings.
          If you change something in this method, make sure to adapt the corresponding code
          in sstrings.inc. }
        procedure enumdef_rtti_ord2stringindex(rttidef: trecorddef; const syms: tfplist);

        var rttilab:Tasmsymbol;
            h,i,o,prev_value:longint;
            mode:(lookup,search); {Modify with care, ordinal value of enum is written.}
            r:single;             {Must be real type because of integer overflow risk.}
            tcb: ttai_typedconstbuilder;
            sym_count: integer;
        begin

          {Decide wether a lookup array is size efficient.}
          mode:=lookup;
          sym_count:=syms.count;
          if sym_count>0 then
            begin
              i:=1;
              r:=0;
              h:=tenumsym(syms[0]).value; {Next expected enum value is min.}
              { set prev_value for the first iteration to a value that is
                different from the first one without risking overflow (it's used
                to detect whether two enum values are the same) }
              if h=0 then
                prev_value:=1
              else
                prev_value:=0;
              while i<sym_count do
                begin
                  { if two enum values are the same, we have to create a table }
                  if (prev_value=h) then
                    begin
                      mode:=search;
                      break;
                    end;
                  {Calculate size of hole between values. Avoid integer overflows.}
                  r:=r+(single(tenumsym(syms[i]).value)-single(h))-1;
                  prev_value:=h;
                  h:=tenumsym(syms[i]).value;
                  inc(i);
                end;
              if r>sym_count then
                mode:=search; {Don't waste more than 50% space.}
            end;
          { write rtti data; make sure that the alignment matches the corresponding data structure
            in the code that uses it (if alignment is required). }
          rttilab:=current_asmdata.DefineAsmSymbol(Tstoreddef(def).rtti_mangledname(rt)+'_o2s',AB_GLOBAL,AT_DATA);
          tcb:=ctai_typedconstbuilder.create([tcalo_make_dead_strippable]);
          { use TConstPtrUInt packrecords to ensure good alignment }
          tcb.begin_anonymous_record('',defaultpacking,reqalign,
            targetinfos[target_info.system]^.alignment.recordalignmin,
            targetinfos[target_info.system]^.alignment.maxCrecordalign);
          { now emit the data: first the mode }
          tcb.emit_tai(Tai_const.create_32bit(longint(mode)),u32inttype);
          { align }
          tcb.begin_anonymous_record('',defaultpacking,reqalign,
            targetinfos[target_info.system]^.alignment.recordalignmin,
            targetinfos[target_info.system]^.alignment.maxCrecordalign);
          if mode=lookup then
            begin
              o:=tenumsym(syms[0]).value;  {Start with min value.}
              for i:=0 to sym_count-1 do
                begin
                  while o<tenumsym(syms[i]).value do
                    begin
                      tcb.emit_tai(Tai_const.create_pint(0),ptruinttype);
                      inc(o);
                    end;
                  inc(o);
                  tcb.queue_init(voidpointertype);
                  tcb.queue_subscriptn_multiple_by_name(rttidef,
                    ['size_start_rec',
                      'min_max_rec',
                      'basetype_array_rec',
                      'enumname'+tostr(tenumsym(syms[i]).symid)]
                  );
                  tcb.queue_emit_asmsym(mainrtti,rttidef);
                end;
            end
          else
            begin
              tcb.emit_ord_const(sym_count,u32inttype);
              tcb.begin_anonymous_record('',defaultpacking,reqalign,
                targetinfos[target_info.system]^.alignment.recordalignmin,
                targetinfos[target_info.system]^.alignment.maxCrecordalign);
              for i:=0 to sym_count-1 do
                begin
                  tcb.emit_ord_const(tenumsym(syms[i]).value,s32inttype);
                  tcb.queue_init(voidpointertype);
                  tcb.queue_subscriptn_multiple_by_name(rttidef,
                    ['size_start_rec',
                      'min_max_rec',
                      'basetype_array_rec',
                      'enumname'+tostr(tenumsym(syms[i]).symid)]
                  );
                  tcb.queue_emit_asmsym(mainrtti,rttidef);
                end;
              tcb.end_anonymous_record;
            end;
            tcb.end_anonymous_record;

            current_asmdata.asmlists[al_rtti].concatlist(tcb.get_final_asmlist(
              rttilab,tcb.end_anonymous_record,sec_rodata,
              rttilab.name,const_align(sizeof(pint))));
            tcb.free;
        end;


        { Writes a helper table for accelerated conversion of string to ordinal enum values.
          If you change something in this method, make sure to adapt the corresponding code
          in sstrings.inc. }
        procedure enumdef_rtti_string2ordindex(rttidef: trecorddef; const syms: tfplist);

        var
          tcb: ttai_typedconstbuilder;
          rttilab:Tasmsymbol;
          i:longint;
        begin
          { write rtti data }
          rttilab:=current_asmdata.DefineAsmSymbol(Tstoreddef(def).rtti_mangledname(rt)+'_s2o',AB_GLOBAL,AT_DATA);
          tcb:=ctai_typedconstbuilder.create([tcalo_make_dead_strippable]);
          { begin of Tstring_to_ord }
          tcb.begin_anonymous_record('',defaultpacking,reqalign,
            targetinfos[target_info.system]^.alignment.recordalignmin,
            targetinfos[target_info.system]^.alignment.maxCrecordalign);
          tcb.emit_ord_const(syms.count,s32inttype);
          { begin of "data" array in Tstring_to_ord }
          tcb.begin_anonymous_record('',defaultpacking,reqalign,
            targetinfos[target_info.system]^.alignment.recordalignmin,
            targetinfos[target_info.system]^.alignment.maxCrecordalign);
          for i:=0 to syms.count-1 do
            begin
              tcb.emit_ord_const(tenumsym(syms[i]).value,s32inttype);
              { alignment of pointer value handled by enclosing record already }
              tcb.queue_init(voidpointertype);
              tcb.queue_subscriptn_multiple_by_name(rttidef,
                ['size_start_rec',
                  'min_max_rec',
                  'basetype_array_rec',
                  'enumname'+tostr(tenumsym(syms[i]).SymId)]
              );
              tcb.queue_emit_asmsym(mainrtti,rttidef);
            end;
          tcb.end_anonymous_record;
          current_asmdata.asmlists[al_rtti].concatlist(tcb.get_final_asmlist(
            rttilab,tcb.end_anonymous_record,sec_rodata,
            rttilab.name,const_align(sizeof(pint))));
          tcb.free;
        end;

        procedure enumdef_rtti_extrasyms(def:Tenumdef);
        var
          t:Tenumsym;
          syms:tfplist;
          h,i,p:longint;
          rttitypesym: ttypesym;
          rttidef: trecorddef;
        begin
          { collect enumsyms belonging to this enum type (could be a subsection
            in case of a subrange type) }
          syms:=tfplist.create;
          for i := 0 to def.symtable.SymList.Count - 1 do
            begin
              t:=tenumsym(def.symtable.SymList[i]);
              if t.value<def.minval then
                continue
              else
              if t.value>def.maxval then
                break;
              syms.add(t);
            end;
          { sort the syms by enum name }
          syms.sort(@enumsym_compare_name);
          rttitypesym:=try_search_current_module_type(internaltypeprefixName[itp_rttidef]+def.rtti_mangledname(fullrtti));
          if not assigned(rttitypesym) or
             (ttypesym(rttitypesym).typedef.typ<>recorddef) then
            internalerror(2015071402);
          rttidef:=trecorddef(ttypesym(rttitypesym).typedef);
          enumdef_rtti_string2ordindex(rttidef,syms);
          { sort the syms by enum value }
          syms.sort(@enumsym_compare_value);
          enumdef_rtti_ord2stringindex(rttidef,syms);
          syms.free;
        end;


    begin
      case def.typ of
        enumdef:
          if rt=fullrtti then
            begin
              enumdef_rtti_extrasyms(Tenumdef(def));
            end;
      end;
    end;

    procedure TRTTIWriter.write_child_rtti_data(def:tdef;rt:trttitype);
      begin
        case def.typ of
          enumdef :
            if assigned(tenumdef(def).basedef) then
              write_rtti(tenumdef(def).basedef,rt);
          setdef :
            write_rtti(tsetdef(def).elementdef,rt);
          arraydef :
            begin
              write_rtti(tarraydef(def).rangedef,rt);
              write_rtti(tarraydef(def).elementdef,rt);
            end;
          recorddef :
            fields_write_rtti(trecorddef(def).symtable,rt);
          objectdef :
            begin
              if assigned(tobjectdef(def).childof) then
                write_rtti(tobjectdef(def).childof,rt);
              if (rt=initrtti) or (tobjectdef(def).objecttype=odt_object) then
                fields_write_rtti(tobjectdef(def).symtable,rt)
              else
                published_write_rtti(tobjectdef(def).symtable,rt);
            end;
          classrefdef,
          pointerdef:
            if not is_objc_class_or_protocol(tabstractpointerdef(def).pointeddef) then
              write_rtti(tabstractpointerdef(def).pointeddef,rt);
          procvardef:
            params_write_rtti(tabstractprocdef(def),rt);
        end;
      end;

    procedure TRTTIWriter.write_rtti_reference(tcb: ttai_typedconstbuilder; def: tdef; rt: trttitype);
      begin
        { we don't care about the real type here, because
           a) we don't index into these elements
           b) we may not have the rtti type available at the point that we
              are emitting this data, because of forward definitions etc
           c) if the rtti is emitted in another unit, we won't have the type
              available at all
          For the cases where the type is emitted in the current unit and hence
          the underlying system will detect and complain about symbol def
          mismatches, type conversions will have to be inserted afterwards (like
          in llvm/llvmtype)
        }
        if not assigned(def) or is_void(def) or ((rt<>initrtti) and is_objc_class_or_protocol(def)) then
          tcb.emit_tai(Tai_const.Create_nil_dataptr,voidpointertype)
        else
          tcb.emit_tai(Tai_const.Create_sym(ref_rtti(def,rt)),voidpointertype);
      end;


    function TRTTIWriter.ref_rtti(def:tdef;rt:trttitype):tasmsymbol;
      begin
        result:=current_asmdata.RefAsmSymbol(def.rtti_mangledname(rt),AT_DATA);
        if (cs_create_pic in current_settings.moduleswitches) and
           assigned(current_procinfo) then
          include(current_procinfo.flags,pi_needs_got);
      end;


    procedure TRTTIWriter.write_rtti(def:tdef;rt:trttitype);
      var
        tcb: ttai_typedconstbuilder;
        rttilab: tasmsymbol;
        rttidef: tdef;
      begin
        { only write rtti of definitions from the current module }
        if not findunitsymtable(def.owner).iscurrentunit then
          exit;
        { check if separate initrtti is actually needed }
        if (rt=initrtti) and (not def.needs_separate_initrtti) then
          rt:=fullrtti;
        { prevent recursion }
        if rttidefstate[rt] in def.defstates then
          exit;
        include(def.defstates,rttidefstate[rt]);
        { write first all dependencies }
        write_child_rtti_data(def,rt);
        { write rtti data }
        tcb:=ctai_typedconstbuilder.create([tcalo_make_dead_strippable]);
        rttilab:=current_asmdata.DefineAsmSymbol(tstoreddef(def).rtti_mangledname(rt),AB_GLOBAL,AT_DATA);
        tcb.begin_anonymous_record(
          internaltypeprefixName[itp_rttidef]+rttilab.Name,
          defaultpacking,reqalign,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign
        );
        write_rtti_data(tcb,def,rt);
        rttidef:=tcb.end_anonymous_record;
        current_asmdata.AsmLists[al_rtti].concatList(
          tcb.get_final_asmlist(rttilab,rttidef,sec_rodata,rttilab.name,const_align(sizeof(pint))));
        write_rtti_extrasyms(def,rt,rttilab);
      end;


    constructor TRTTIWriter.create;
      begin
        if tf_requires_proper_alignment in target_info.flags then
          begin
            reqalign:=sizeof(TConstPtrUInt);
            defaultpacking:=C_alignment;
          end
        else
          begin
            reqalign:=1;
            defaultpacking:=1;
          end;
      end;


    function TRTTIWriter.get_rtti_label(def:tdef;rt:trttitype):tasmsymbol;
      begin
        result:=current_asmdata.RefAsmSymbol(def.rtti_mangledname(rt),AT_DATA);
        if (cs_create_pic in current_settings.moduleswitches) and
           assigned(current_procinfo) then
          include(current_procinfo.flags,pi_needs_got);
      end;

    function TRTTIWriter.get_rtti_label_ord2str(def:tdef;rt:trttitype):tasmsymbol;
      begin
        result:=current_asmdata.RefAsmSymbol(def.rtti_mangledname(rt)+'_o2s',AT_DATA);
        if (cs_create_pic in current_settings.moduleswitches) and
           assigned(current_procinfo) then
          include(current_procinfo.flags,pi_needs_got);
      end;

    function TRTTIWriter.get_rtti_label_str2ord(def:tdef;rt:trttitype):tasmsymbol;
      begin
        result:=current_asmdata.RefAsmSymbol(def.rtti_mangledname(rt)+'_s2o',AT_DATA);
        if (cs_create_pic in current_settings.moduleswitches) and
           assigned(current_procinfo) then
          include(current_procinfo.flags,pi_needs_got);
      end;

end.

