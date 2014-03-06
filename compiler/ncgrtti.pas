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
      aasmbase,
      symbase,symconst,symtype,symdef;

    type

      { TRTTIWriter }

      TRTTIWriter=class
      private
        procedure fields_write_rtti(st:tsymtable;rt:trttitype);
        procedure fields_write_rtti_data(def:tabstractrecorddef;rt:trttitype);
        procedure write_rtti_extrasyms(def:Tdef;rt:Trttitype;mainrtti:Tasmsymbol);
        procedure published_write_rtti(st:tsymtable;rt:trttitype);
        function  published_properties_count(st:tsymtable):longint;
        procedure published_properties_write_rtti_data(propnamelist:TFPHashObjectList;st:tsymtable);
        procedure collect_propnamelist(propnamelist:TFPHashObjectList;objdef:tobjectdef);
        function  ref_rtti(def:tdef;rt:trttitype):tasmsymbol;
        procedure write_rtti_name(def:tdef);
        procedure write_rtti_data(def:tdef;rt:trttitype);
        procedure write_child_rtti_data(def:tdef;rt:trttitype);
        procedure write_rtti_reference(def:tdef;rt:trttitype);
        procedure write_header(def: tdef; typekind: byte);
        procedure write_string(const s: string);
        procedure maybe_write_align;
      public
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
       symsym,
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

    procedure TRTTIWriter.maybe_write_align;
      begin
        if (tf_requires_proper_alignment in target_info.flags) then
          current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
      end;

    procedure TRTTIWriter.write_string(const s: string);
      begin
        current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(length(s)));
        current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(s));
      end;

    procedure TRTTIWriter.write_header(def: tdef; typekind: byte);
      begin
        if def.typ=arraydef then
          InternalError(201012211);
        current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(typekind));
        if assigned(def.typesym) then
          write_string(ttypesym(def.typesym).realname)
        else
          current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(#0));
      end;

    procedure TRTTIWriter.write_rtti_name(def:tdef);
      var
         hs : string;
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
           begin
              hs:=ttypesym(def.typesym).realname;
              current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(chr(length(hs))+hs));
           end
         else
           current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(#0));
      end;

    { writes a 32-bit count followed by array of field infos for given symtable }
    procedure TRTTIWriter.fields_write_rtti_data(def:tabstractrecorddef;rt:trttitype);
      var
        i   : longint;
        sym : tsym;
        fieldcnt: longint;
        lastai: TLinkedListItem;
        st: tsymtable;
      begin
        fieldcnt:=0;
        { Count will be inserted at this location. It cannot be nil as we've just
          written header for this symtable owner. But stay safe. }
        lastai:=current_asmdata.asmlists[al_rtti].last;
        if lastai=nil then
          InternalError(201012212);

        { For objects, treat parent (if any) as a field with offset 0. This
          provides correct handling of entire instance with RTL rtti routines. }
        if (def.typ=objectdef) and (tobjectdef(def).objecttype=odt_object) and
            Assigned(tobjectdef(def).childof) and
            ((rt=fullrtti) or (tobjectdef(def).childof.needs_inittable)) then
          begin
            write_rtti_reference(tobjectdef(def).childof,rt);
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_pint(0));
            inc(fieldcnt);
          end;
        st:=def.symtable;
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
                write_rtti_reference(tfieldvarsym(sym).vardef,rt);
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_pint(tfieldvarsym(sym).fieldoffset));
                inc(fieldcnt);
              end;
          end;
        { insert field count before data }
        current_asmdata.asmlists[al_rtti].InsertAfter(Tai_const.Create_32bit(fieldcnt),lastai)
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


    procedure TRTTIWriter.published_properties_write_rtti_data(propnamelist:TFPHashObjectList;st:tsymtable);
      var
        i : longint;
        sym : tsym;
        proctypesinfo : byte;
        propnameitem  : tpropnamelistitem;

        procedure writeaccessproc(pap:tpropaccesslisttypes; shiftvalue : byte; unsetvalue: byte);
        var
           typvalue : byte;
           hp : ppropaccesslistitem;
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
                current_asmdata.asmlists[al_rtti].concat(Tai_const.create(aitconst_ptr,unsetvalue));
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
                current_asmdata.asmlists[al_rtti].concat(Tai_const.create(aitconst_ptr,address));
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
                     current_asmdata.asmlists[al_rtti].concat(Tai_const.createname(tprocdef(propaccesslist.procdef).mangledname,AT_FUNCTION,0));
                     typvalue:=1;
                  end
                else
                  begin
                     { virtual method, write vmt offset }
                     current_asmdata.asmlists[al_rtti].concat(Tai_const.create(aitconst_ptr,
                       tobjectdef(tprocdef(propaccesslist.procdef).struct).vmtmethodoffset(tprocdef(propaccesslist.procdef).extnumber)));
                     { register for wpo }
                     tobjectdef(tprocdef(propaccesslist.procdef).struct).register_vmt_call(tprocdef(propaccesslist.procdef).extnumber);
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
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (sym.typ=propertysym) and
               (sym.visibility=vis_published) then
              begin
                if ppo_indexed in tpropertysym(sym).propoptions then
                  proctypesinfo:=$40
                else
                  proctypesinfo:=0;
                write_rtti_reference(tpropertysym(sym).propdef,fullrtti);
                writeaccessproc(palt_read,0,0);
                writeaccessproc(palt_write,2,0);
                { is it stored ? }
                if not(ppo_stored in tpropertysym(sym).propoptions) then
                  begin
                    { no, so put a constant zero }
                    current_asmdata.asmlists[al_rtti].concat(Tai_const.create(aitconst_ptr,0));
                    proctypesinfo:=proctypesinfo or (3 shl 4);
                  end
                else
                  writeaccessproc(palt_stored,4,1); { maybe; if no procedure put a constant 1 (=true) }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(tpropertysym(sym).index));
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(tpropertysym(sym).default));
                propnameitem:=TPropNameListItem(propnamelist.Find(tpropertysym(sym).name));
                if not assigned(propnameitem) then
                  internalerror(200512201);
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(propnameitem.propindex));
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(proctypesinfo));
                write_string(tpropertysym(sym).realname);
                maybe_write_align;
             end;
          end;
      end;


    procedure TRTTIWriter.write_rtti_data(def:tdef;rt:trttitype);

        procedure unknown_rtti(def:tstoreddef);
        begin
          current_asmdata.asmlists[al_rtti].concat(tai_const.create_8bit(tkUnknown));
          write_rtti_name(def);
        end;

        procedure variantdef_rtti(def:tvariantdef);
        begin
          write_header(def,tkVariant);
        end;

        procedure stringdef_rtti(def:tstringdef);
        begin
          case def.stringtype of
            st_ansistring:
              begin
                write_header(def,tkAString);
                maybe_write_align;
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(def.encoding));
              end;

            st_widestring:
              write_header(def,tkWString);

            st_unicodestring:
              write_header(def,tkUString);

            st_longstring:
              write_header(def,tkLString);

            st_shortstring:
              begin
                 write_header(def,tkSString);
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(def.len));
                 maybe_write_align;  // is align necessary here?
              end;
          end;
        end;

        procedure enumdef_rtti(def:tenumdef);
        var
           i  : integer;
           hp : tenumsym;
        begin
          write_header(def,tkEnumeration);
          maybe_write_align;
          case longint(def.size) of
            1 :
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otUByte));
            2 :
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otUWord));
            4 :
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otULong));
          end;
          { we need to align by Tconstptruint here to satisfy the alignment rules set by
            records: in the typinfo unit we overlay a TTypeData record on this data, which at
            the innermost variant record needs an alignment of TConstPtrUint due to e.g.
            the "CompType" member for tkSet (also the "BaseType" member for tkEnumeration).
            We need to adhere to this, otherwise things will break.
            Note that other code (e.g. enumdef_rtti_calcstringtablestart()) relies on the
            exact sequence too. }
          maybe_write_align;
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.min));
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.max));
          maybe_write_align;  // is align necessary here?
          { write base type }
          write_rtti_reference(def.basedef,rt);
          for i := 0 to def.symtable.SymList.Count - 1 do
            begin
              hp:=tenumsym(def.symtable.SymList[i]);
              if hp.value<def.minval then
                continue
              else
              if hp.value>def.maxval then
                break;
              write_string(hp.realname);
            end;
          { write unit name }
          write_string(current_module.realmodulename^);
          { write zero which is required by RTL }
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(0));
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
            write_header(def,typekind);
            maybe_write_align;
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(byte(trans[def.ordtype])));
            maybe_write_align;
            {Convert to longint to smuggle values in high(longint)+1..high(cardinal) into asmlist.}
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(longint(def.low.svalue)));
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(longint(def.high.svalue)));
          end;

        begin
          case def.ordtype of
            s64bit :
              begin
                write_header(def,tkInt64);
                maybe_write_align;
                { low }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_64bit(def.low.svalue));
                { high }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_64bit(def.high.svalue));
              end;
            u64bit :
              begin
                write_header(def,tkQWord);
                maybe_write_align;
                {use svalue because Create_64bit accepts int64, prevents range checks}
                { low }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_64bit(def.low.svalue));
                { high }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_64bit(def.high.svalue));
              end;
            pasbool8:
                dointeger(tkBool);
            uchar:
                dointeger(tkChar);
            uwidechar:
                dointeger(tkWChar);
            scurrency:
              begin
                write_header(def,tkFloat);
                maybe_write_align;
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(ftCurr));
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
           write_header(def,tkFloat);
           maybe_write_align;
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(translate[def.floattype]));
        end;


        procedure setdef_rtti(def:tsetdef);
        begin
           write_header(def,tkSet);
           maybe_write_align;
           case def.size of
             1:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otUByte));
             2:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otUWord));
             4:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otULong));
             else
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otUByte));
           end;
           maybe_write_align;
           write_rtti_reference(def.elementdef,rt);
        end;


        procedure arraydef_rtti(def:tarraydef);
          var
            lastai: TLinkedListItem;
            dimcount: byte;
            totalcount: asizeuint;
            curdef:tarraydef;
        begin
           if ado_IsDynamicArray in def.arrayoptions then
             current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkdynarray))
           else
             current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkarray));
           write_rtti_name(def);
           maybe_write_align;

           if not(ado_IsDynamicArray in def.arrayoptions) then
             begin
               { remember tha last instruction. we will need to insert some
                 calculated values after it }
               lastai:=current_asmdata.asmlists[al_rtti].last;
               curdef:=def;
               totalcount:=1;
               dimcount:=0;
               while assigned(curdef) do
                 begin
                   { Dims[i] PTypeInfo }
                   write_rtti_reference(curdef.rangedef,rt);
                   inc(dimcount);
                   totalcount:=totalcount*curdef.elecount;
                   { get the next static array }
                   if assigned(curdef.elementdef) and
                      (curdef.elementdef.typ=arraydef) and
                      not(ado_IsDynamicArray in tarraydef(curdef.elementdef).arrayoptions) then
                     curdef:=tarraydef(curdef.elementdef)
                   else
                     break;
                 end;
               if (tf_requires_proper_alignment in target_info.flags) then
                 current_asmdata.asmlists[al_rtti].InsertAfter(cai_align.Create(sizeof(TConstPtrUInt)),lastai);
               { dimension count }
               current_asmdata.asmlists[al_rtti].InsertAfter(Tai_const.Create_8bit(dimcount),lastai);
               { last dimension element type }
               current_asmdata.asmlists[al_rtti].InsertAfter(Tai_const.Create_sym(ref_rtti(curdef.elementdef,rt)),lastai);
               { total element count }
               current_asmdata.asmlists[al_rtti].InsertAfter(Tai_const.Create_pint(pint(totalcount)),lastai);
               { total size = elecount * elesize of the first arraydef }
               current_asmdata.asmlists[al_rtti].InsertAfter(Tai_const.Create_pint(def.elecount*def.elesize),lastai);
             end
           else
             { write a delphi almost compatible dyn. array entry:
               there are two types, eltype and eltype2, the latter is nil if the element type needs
               no finalization, the former is always valid, delphi has this swapped, but for
               compatibility with older fpc versions we do it different, to be delphi compatible,
               the names are swapped in typinfo.pp
             }
             begin
               { size of elements }
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_pint(def.elesize));
               { element type }
               write_rtti_reference(def.elementdef,rt);
               { variant type }
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(tstoreddef(def.elementdef).getvardef));
               { element type }
               if def.elementdef.needs_inittable then
                 write_rtti_reference(def.elementdef,rt)
               else
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_pint(0));
               { write unit name }
               write_string(current_module.realmodulename^);
             end;
        end;

        procedure classrefdef_rtti(def:tclassrefdef);
        begin
          write_header(def,tkClassRef);
          maybe_write_align;
          write_rtti_reference(def.pointeddef,rt);
        end;

        procedure pointerdef_rtti(def:tpointerdef);
        begin
          write_header(def,tkPointer);
          maybe_write_align;
          write_rtti_reference(def.pointeddef,rt);
        end;

        procedure recorddef_rtti(def:trecorddef);
        begin
           write_header(def,tkRecord);
           maybe_write_align;
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.size));
           fields_write_rtti_data(def,rt);
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
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(paraspec));
             end;

           procedure write_para(parasym:tparavarsym);
             begin
               { only store user visible parameters }
               if not(vo_is_hidden_para in parasym.varoptions) then
                 begin
                   { write flags for current parameter }
                   write_param_flag(parasym);
                   { write name of current parameter }
                   write_string(parasym.realname);
                   { write name of type of current parameter }
                   write_rtti_name(parasym.vardef);
                 end;
             end;

           procedure write_procedure_param(parasym:tparavarsym);
             begin
               { only store user visible parameters }
               if not(vo_is_hidden_para in parasym.varoptions) then
                 begin
                   { write flags for current parameter }
                   write_param_flag(parasym);
                   maybe_write_align;
                   { write param type }
                   write_rtti_reference(parasym.vardef,fullrtti);
                   { write name of current parameter }
                   write_string(parasym.realname);
                 end;
             end;

        var
          methodkind : byte;
          i : integer;
        begin
          if po_methodpointer in def.procoptions then
            begin
               { write method id and name }
               write_header(def,tkMethod);
               maybe_write_align;

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
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(methodkind));

               { write parameter info. The parameters must be written in reverse order
                 if this method uses right to left parameter pushing! }
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(def.maxparacount));

               for i:=0 to def.paras.count-1 do
                 write_para(tparavarsym(def.paras[i]));

               if (methodkind=mkFunction) or (methodkind=mkClassFunction) then
               begin
                 { write name of result type }
                 write_rtti_name(def.returndef);
                 maybe_write_align;
                 { write result typeinfo }
                 write_rtti_reference(def.returndef,fullrtti);
               end;

               { write calling convention }
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(ProcCallOptionToCallConv[def.proccalloption]));
               maybe_write_align;

               { write params typeinfo }
               for i:=0 to def.paras.count-1 do
                 if not(vo_is_hidden_para in tparavarsym(def.paras[i]).varoptions) then
                   write_rtti_reference(tparavarsym(def.paras[i]).vardef,fullrtti);
            end
          else
            begin
              write_header(def,tkProcvar);
              maybe_write_align;

              { flags }
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(0));
              //maybe_write_align;     // aligning between bytes is not necessary
              { write calling convention }
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(ProcCallOptionToCallConv[def.proccalloption]));
              maybe_write_align;
              { write result typeinfo }
              write_rtti_reference(def.returndef,fullrtti);
              { write parameter count }
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(def.maxparacount));
              for i:=0 to def.paras.count-1 do
                begin
                  maybe_write_align;
                  write_procedure_param(tparavarsym(def.paras[i]));
                end;
            end;
        end;


        procedure objectdef_rtti(def:tobjectdef);

          procedure objectdef_rtti_fields(def:tobjectdef);
          begin
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.size));
            fields_write_rtti_data(def,rt);
          end;

          procedure objectdef_rtti_interface_init(def:tobjectdef);
          begin
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.size));
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
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Createname(def.vmt_mangledname,AT_DATA,0))
              else
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_nil_dataptr);

            { write parent typeinfo }
            write_rtti_reference(def.childof,fullrtti);

            { write typeinfo of extended type }
            if is_objectpascal_helper(def) then
              if assigned(def.extendeddef) then
                write_rtti_reference(def.extendeddef,fullrtti)
              else
                InternalError(2011033001);

            { total number of unique properties }
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(propnamelist.count));

            { write unit name }
            write_string(current_module.realmodulename^);
            maybe_write_align;

            { write published properties for this object }
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(published_properties_count(def.symtable)));
            maybe_write_align;
            published_properties_write_rtti_data(propnamelist,def.symtable);

            propnamelist.free;
          end;

          procedure objectdef_rtti_interface_full(def:tobjectdef);
          var
            i : longint;
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
            write_rtti_reference(def.childof,fullrtti);

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
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(IntfFlags));
            maybe_write_align;
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(longint(def.iidguid^.D1)));
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(def.iidguid^.D2));
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(def.iidguid^.D3));
            for i:=Low(def.iidguid^.D4) to High(def.iidguid^.D4) do
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(def.iidguid^.D4[i]));

            { write unit name }
            write_string(current_module.realmodulename^);
            maybe_write_align;

            { write iidstr }
            if assigned(def.iidstr) then
              write_string(def.iidstr^)
            else
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(0));
            maybe_write_align;

            { write published properties for this object }
            published_properties_write_rtti_data(propnamelist,def.symtable);

            propnamelist.free;
          end;

        begin
           case def.objecttype of
             odt_class:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkclass));
             odt_object:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkobject));
             odt_dispinterface,
             odt_interfacecom:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkinterface));
             odt_interfacecorba:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkinterfaceCorba));
             odt_helper:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkhelper));
             else
               internalerror(200611034);
           end;

           { generate the name }
           write_string(def.objrealname^);
           maybe_write_align;

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

    procedure TRTTIWriter.write_rtti_extrasyms(def:Tdef;rt:Trttitype;mainrtti:Tasmsymbol);

        type Penumsym = ^Tenumsym;

        function enumdef_rtti_calcstringtablestart(const def : Tenumdef) : integer;
        begin
          { the alignment calls must correspond to the ones used during generating the
            actual data structure created elsewhere in this file }
          result:=1;
          if assigned(def.typesym) then
            inc(result,length(def.typesym.realname)+1)
          else
            inc(result);
          if (tf_requires_proper_alignment in target_info.flags) then
            result:=align(result,sizeof(Tconstptruint));
          inc(result);
          if (tf_requires_proper_alignment in target_info.flags) then
            result:=align(result,sizeof(Tconstptruint));
          inc(result, sizeof(longint) * 2);
          if (tf_requires_proper_alignment in target_info.flags) then
            result:=align(result,sizeof(Tconstptruint));
          inc(result, sizeof(pint));
        end;

        { Writes a helper table for accelerated conversion of ordinal enum values to strings.
          If you change something in this method, make sure to adapt the corresponding code
          in sstrings.inc. }
        procedure enumdef_rtti_ord2stringindex(const sym_count:longint; const offsets:plongint; const syms:Penumsym; const st:longint);

        var rttilab:Tasmsymbol;
            h,i,o,prev_value:longint;
            mode:(lookup,search); {Modify with care, ordinal value of enum is written.}
            r:single;             {Must be real type because of integer overflow risk.}

        begin

          {Decide wether a lookup array is size efficient.}
          mode:=lookup;
          if sym_count>0 then
            begin
              i:=1;
              r:=0;
              h:=syms[0].value; {Next expected enum value is min.}
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
                  r:=r+(single(syms[i].value)-single(h))-1;
                  prev_value:=h;
                  h:=syms[i].value;
                  inc(i);
                end;
              if r>sym_count then
                mode:=search; {Don't waste more than 50% space.}
            end;
          { write rtti data; make sure that the alignment matches the corresponding data structure
            in the code that uses it (if alignment is required). }
          with current_asmdata do
            begin
              rttilab:=defineasmsymbol(Tstoreddef(def).rtti_mangledname(rt)+'_o2s',AB_GLOBAL,AT_DATA);
              maybe_new_object_file(asmlists[al_rtti]);
              new_section(asmlists[al_rtti],sec_rodata,rttilab.name,const_align(sizeof(pint)));
              asmlists[al_rtti].concat(Tai_symbol.create_global(rttilab,0));
              asmlists[al_rtti].concat(Tai_const.create_32bit(longint(mode)));
              if mode=lookup then
                begin
                  maybe_write_align;
                  o:=syms[0].value;  {Start with min value.}
                  for i:=0 to sym_count-1 do
                    begin
                      while o<syms[i].value do
                        begin
                          asmlists[al_rtti].concat(Tai_const.create_pint(0));
                          inc(o);
                        end;
                      inc(o);
                      asmlists[al_rtti].concat(Tai_const.create_sym_offset(mainrtti,st+offsets[i]));
                    end;
                end
              else
                begin
                  maybe_write_align;
                  asmlists[al_rtti].concat(Tai_const.create_32bit(sym_count));
                  for i:=0 to sym_count-1 do
                    begin
                      maybe_write_align;
                      asmlists[al_rtti].concat(Tai_const.create_32bit(syms[i].value));
                      maybe_write_align;
                      asmlists[al_rtti].concat(Tai_const.create_sym_offset(mainrtti,st+offsets[i]));
                    end;
                end;
              asmlists[al_rtti].concat(Tai_symbol_end.create(rttilab));
            end;
        end;

        { Writes a helper table for accelerated conversion of string to ordinal enum values.
          If you change something in this method, make sure to adapt the corresponding code
          in sstrings.inc. }
        procedure enumdef_rtti_string2ordindex(const sym_count:longint; const offsets:plongint; const syms:Penumsym; const st:longint);

        var rttilab:Tasmsymbol;
            i:longint;

        begin
          { write rtti data }
          with current_asmdata do
            begin
              rttilab:=defineasmsymbol(Tstoreddef(def).rtti_mangledname(rt)+'_s2o',AB_GLOBAL,AT_DATA);
              maybe_new_object_file(asmlists[al_rtti]);
              new_section(asmlists[al_rtti],sec_rodata,rttilab.name,const_align(sizeof(pint)));
              asmlists[al_rtti].concat(Tai_symbol.create_global(rttilab,0));
              asmlists[al_rtti].concat(Tai_const.create_32bit(sym_count));
              { need to align the entry record according to the largest member }
              maybe_write_align;
              for i:=0 to sym_count-1 do
                begin
                  if (tf_requires_proper_alignment in target_info.flags) then
                    current_asmdata.asmlists[al_rtti].concat(cai_align.Create(4));  // necessary?
                  asmlists[al_rtti].concat(Tai_const.create_32bit(syms[i].value));
                  maybe_write_align;
                  asmlists[al_rtti].concat(Tai_const.create_sym_offset(mainrtti,st+offsets[i]));
                end;
              asmlists[al_rtti].concat(Tai_symbol_end.create(rttilab));
            end;
        end;

        procedure enumdef_rtti_extrasyms(def:Tenumdef);
        var
          t:Tenumsym;
          syms:Penumsym;
          sym_count,sym_alloc:sizeuint;
          offsets:^longint;
          h,i,p,o,st:longint;
        begin
          {Random access needed, put in array.}
          getmem(syms,64*sizeof(Tenumsym));
          getmem(offsets,64*sizeof(longint));
          sym_count:=0;
          sym_alloc:=64;
          st:=0;
          for i := 0 to def.symtable.SymList.Count - 1 do
            begin
              t:=tenumsym(def.symtable.SymList[i]);
              if t.value<def.minval then
                continue
              else
              if t.value>def.maxval then
                break;
              if sym_count>=sym_alloc then
                begin
                  reallocmem(syms,2*sym_alloc*sizeof(Tenumsym));
                  reallocmem(offsets,2*sym_alloc*sizeof(longint));
                  sym_alloc:=sym_alloc*2;
                end;
              syms[sym_count]:=t;
              offsets[sym_count]:=st;
              inc(sym_count);
              st:=st+length(t.realname)+1;
            end;
          {Sort the syms by enum name}
          if sym_count>=2 then
            begin
              p:=1;
              while 2*p<sym_count do
                p:=2*p;
              while p<>0 do
                begin
                  for h:=p to sym_count-1 do
                    begin
                      i:=h;
                      t:=syms[i];
                      o:=offsets[i];
                      repeat
                        if syms[i-p].name<=t.name then
                          break;
                        syms[i]:=syms[i-p];
                        offsets[i]:=offsets[i-p];
                        dec(i,p);
                      until i<p;
                      syms[i]:=t;
                      offsets[i]:=o;
                    end;
                  p:=p shr 1;
                end;
            end;
          st:=enumdef_rtti_calcstringtablestart(def);
          enumdef_rtti_string2ordindex(sym_count,offsets,syms,st);
          { Sort the syms by enum value }
          if sym_count>=2 then
            begin
              p:=1;
              while 2*p<sym_count do
                p:=2*p;
              while p<>0 do
                begin
                  for h:=p to sym_count-1 do
                    begin
                      i:=h;
                      t:=syms[i];
                      o:=offsets[i];
                      repeat
                        if syms[i-p].value<=t.value then
                          break;
                        syms[i]:=syms[i-p];
                        offsets[i]:=offsets[i-p];
                        dec(i,p);
                      until i<p;
                      syms[i]:=t;
                      offsets[i]:=o;
                    end;
                  p:=p shr 1;
                end;
            end;
          enumdef_rtti_ord2stringindex(sym_count,offsets,syms,st);
          freemem(syms);
          freemem(offsets);
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
        end;
      end;

    procedure TRTTIWriter.write_rtti_reference(def:tdef;rt:trttitype);
      begin
        if not assigned(def) or is_void(def) or ((rt<>initrtti) and is_objc_class_or_protocol(def)) then
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_nil_dataptr)
        else
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_sym(ref_rtti(def,rt)));
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
        rttilab : tasmsymbol;
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
        rttilab:=current_asmdata.DefineAsmSymbol(tstoreddef(def).rtti_mangledname(rt),AB_GLOBAL,AT_DATA);
        maybe_new_object_file(current_asmdata.asmlists[al_rtti]);
        new_section(current_asmdata.asmlists[al_rtti],sec_rodata,rttilab.name,const_align(sizeof(pint)));
        current_asmdata.asmlists[al_rtti].concat(Tai_symbol.Create_global(rttilab,0));
        write_rtti_data(def,rt);
        current_asmdata.asmlists[al_rtti].concat(Tai_symbol_end.Create(rttilab));
        write_rtti_extrasyms(def,rt,rttilab);
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

