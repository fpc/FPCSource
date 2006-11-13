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
      cclasses,
      aasmbase,
      symbase,symconst,symtype,symdef;

    type

      { TRTTIWriter }

      TRTTIWriter=class
      private
        function  fields_count(st:tsymtable;rt:trttitype):longint;
        procedure fields_write_rtti(st:tsymtable;rt:trttitype);
        procedure fields_write_rtti_data(st:tsymtable;rt:trttitype);
        procedure published_write_rtti(st:tsymtable;rt:trttitype);
        function  published_properties_count(st:tsymtable):longint;
        procedure published_properties_write_rtti_data(propnamelist:TFPHashObjectList;st:tsymtable);
        procedure collect_propnamelist(propnamelist:TFPHashObjectList;objdef:tobjectdef);
        procedure write_rtti_name(def:tdef);
        procedure write_rtti_data(def:tdef;rt:trttitype);
        procedure write_child_rtti_data(def:tdef;rt:trttitype);
        function  ref_rtti(def:tdef;rt:trttitype):tasmsymbol;
      public
        procedure write_rtti(def:tdef;rt:trttitype);
        function  get_rtti_label(def:tdef;rt:trttitype):tasmsymbol;
      end;

    var
      RTTIWriter : TRTTIWriter;


implementation

    uses
       cutils,
       globals,globtype,verbose,
       fmodule,
       symsym,
       aasmtai,aasmdata
       ;


    const
       rttidefstate : array[trttitype] of tdefstate = (ds_rtti_table_written,ds_init_table_written);

    type
       TPropNameListItem = class(TFPHashObject)
         propindex : longint;
         propowner : TSymtable;
       end;


{***************************************************************************
                              TRTTIWriter
***************************************************************************}

    procedure TRTTIWriter.write_rtti_name(def:tdef);
      var
         hs : string;
      begin
         { name }
         if assigned(def.typesym) then
           begin
              hs:=ttypesym(def.typesym).realname;
              current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(chr(length(hs))+hs));
           end
         else
           current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(#0));
      end;


    function TRTTIWriter.fields_count(st:tsymtable;rt:trttitype):longint;
      var
        i   : longint;
        sym : tsym;
      begin
        result:=0;
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (rt=fullrtti) or
               (
                (tsym(sym).typ=fieldvarsym) and
                tfieldvarsym(sym).vardef.needs_inittable
               ) then
              inc(result);
          end;
      end;


    procedure TRTTIWriter.fields_write_rtti_data(st:tsymtable;rt:trttitype);
      var
        i   : longint;
        sym : tsym;
      begin
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (rt=fullrtti) or
               (
                (tsym(sym).typ=fieldvarsym) and
                tfieldvarsym(sym).vardef.needs_inittable
               ) then
              begin
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_sym(ref_rtti(tfieldvarsym(sym).vardef,rt)));
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(tfieldvarsym(sym).fieldoffset));
              end;
          end;
      end;


    procedure TRTTIWriter.fields_write_rtti(st:tsymtable;rt:trttitype);
      var
        i   : longint;
        sym : tsym;
      begin
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (rt=fullrtti) or
               (
                (tsym(sym).typ=fieldvarsym) and
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
            if (sp_published in tsym(sym).symoptions) then
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
               (sp_published in tsym(sym).symoptions) then
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
               (sp_published in tsym(sym).symoptions) then
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
           address : longint;
           def : tdef;
           hpropsym : tpropertysym;
           propaccesslist : tpropaccesslist;
        begin
           hpropsym:=tpropertysym(sym);
           repeat
             propaccesslist:=hpropsym.propaccesslist[pap];
             if not propaccesslist.empty then
               break;
             hpropsym:=hpropsym.overridenpropsym;
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
                           if not(assigned(def) and (def.typ=recorddef)) then
                             internalerror(200402171);
                           inc(address,tfieldvarsym(hp^.sym).fieldoffset);
                           def:=tfieldvarsym(hp^.sym).vardef;
                         end;
                       sl_vec :
                         begin
                           if not(assigned(def) and (def.typ=arraydef)) then
                             internalerror(200402172);
                           def:=tarraydef(def).elementdef;
                           inc(address,def.size*hp^.value);
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
                if not(po_virtualmethod in tprocdef(propaccesslist.procdef).procoptions) then
                  begin
                     current_asmdata.asmlists[al_rtti].concat(Tai_const.createname(tprocdef(propaccesslist.procdef).mangledname,0));
                     typvalue:=1;
                  end
                else
                  begin
                     { virtual method, write vmt offset }
                     current_asmdata.asmlists[al_rtti].concat(Tai_const.create(aitconst_ptr,
                       tprocdef(propaccesslist.procdef)._class.vmtmethodoffset(tprocdef(propaccesslist.procdef).extnumber)));
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
               (sp_published in sym.symoptions) then
              begin
                if ppo_indexed in tpropertysym(sym).propoptions then
                  proctypesinfo:=$40
                else
                  proctypesinfo:=0;
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_sym(ref_rtti(tpropertysym(sym).propdef,fullrtti)));
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
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(length(tpropertysym(sym).realname)));
                current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(tpropertysym(sym).realname));
{$ifdef cpurequiresproperalignment}
                current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
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
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkVariant));
        end;

        procedure stringdef_rtti(def:tstringdef);
        begin
          case def.stringtype of
            st_ansistring:
              begin
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkAString));
                 write_rtti_name(def);
              end;
            st_widestring:
              begin
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkWString));
                 write_rtti_name(def);
              end;
            st_longstring:
              begin
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkLString));
                 write_rtti_name(def);
              end;
            st_shortstring:
              begin
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkSString));
                 write_rtti_name(def);
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(def.len));
{$ifdef cpurequiresproperalignment}
                 current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
              end;
          end;
        end;

        procedure enumdef_rtti(def:tenumdef);
        var
           hp : tenumsym;
        begin
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkEnumeration));
          write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
          current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
          case longint(def.size) of
            1 :
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otUByte));
            2 :
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otUWord));
            4 :
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otULong));
          end;
{$ifdef cpurequiresproperalignment}
          current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.min));
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.max));
          if assigned(def.basedef) then
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_sym(ref_rtti(def.basedef,rt)))
          else
            current_asmdata.asmlists[al_rtti].concat(Tai_const.create_sym(nil));
          hp:=tenumsym(def.firstenum);
          while assigned(hp) do
            begin
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(length(hp.realname)));
              current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(hp.realname));
              hp:=hp.nextenum;
            end;
          current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(0));
        end;

        procedure orddef_rtti(def:torddef);

          procedure dointeger;
          const
            trans : array[tordtype] of byte =
              (otUByte{otNone},
               otUByte,otUWord,otULong,otUByte{otNone},
               otSByte,otSWord,otSLong,otUByte{otNone},
               otUByte,otUWord,otULong,otUByte,
               otUByte,otUWord,otUByte);
          begin
            write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(byte(trans[def.ordtype])));
{$ifdef cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(longint(def.low)));
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(longint(def.high)));
          end;

        begin
          case def.ordtype of
            s64bit :
              begin
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkInt64));
                write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
                current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
                { low }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_64bit(int64($80000000) shl 32));
                { high }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_64bit((int64($7fffffff) shl 32) or int64($ffffffff)));
              end;
            u64bit :
              begin
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkQWord));
                write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
                current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
                { low }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_64bit(0));
                { high }
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_64bit(int64((int64($ffffffff) shl 32) or int64($ffffffff))));
              end;
            bool8bit:
              begin
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkBool));
                dointeger;
              end;
            uchar:
              begin
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkChar));
                dointeger;
              end;
            uwidechar:
              begin
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkWChar));
                dointeger;
              end;
            else
              begin
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkInteger));
                dointeger;
              end;
          end;
        end;


        procedure floatdef_rtti(def:tfloatdef);
        const
          {tfloattype = (s32real,s64real,s80real,s64bit,s128bit);}
          translate : array[tfloattype] of byte =
             (ftSingle,ftDouble,ftExtended,ftComp,ftCurr,ftFloat128);
        begin
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkFloat));
           write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(translate[def.floattype]));
        end;


        procedure setdef_rtti(def:tsetdef);
        begin
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkSet));
           write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(otULong));
{$ifdef cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_sym(ref_rtti(def.elementdef,rt)));
        end;


        procedure arraydef_rtti(def:tarraydef);
        begin
           if ado_IsDynamicArray in def.arrayoptions then
             current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkdynarray))
           else
             current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkarray));
           write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
           { size of elements }
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_aint(def.elesize));
           if not(ado_IsDynamicArray in def.arrayoptions) then
             current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_aint(def.elecount));
           { element type }
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_sym(ref_rtti(def.elementdef,rt)));
           { variant type }
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(tstoreddef(def.elementdef).getvardef));
        end;

        procedure recorddef_rtti(def:trecorddef);
        var
          fieldcnt : longint;
        begin
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkrecord));
           write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.size));
           fieldcnt:=fields_count(def.symtable,rt);
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(fieldcnt));
           fields_write_rtti_data(def.symtable,rt);
        end;


        procedure procvardef_rtti(def:tprocvardef);

           procedure write_para(parasym:tparavarsym);
           var
             paraspec : byte;
           begin
             { only store user visible parameters }
             if not(vo_is_hidden_para in parasym.varoptions) then
               begin
                 case parasym.varspez of
                   vs_value: paraspec := 0;
                   vs_const: paraspec := pfConst;
                   vs_var  : paraspec := pfVar;
                   vs_out  : paraspec := pfOut;
                 end;
                 { write flags for current parameter }
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(paraspec));
                 { write name of current parameter }
                 current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(length(parasym.realname)));
                 current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(parasym.realname));
                 { write name of type of current parameter }
                 write_rtti_name(parasym.vardef);
               end;
           end;

        var
          methodkind : byte;
          i : integer;
        begin
          if po_methodpointer in def.procoptions then
            begin
               { write method id and name }
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkmethod));
               write_rtti_name(def);
{$ifdef cpurequiresproperalignment}
               current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}

               { write kind of method (can only be function or procedure)}
               if def.returndef = voidtype then
                 methodkind := mkProcedure
               else
                 methodkind := mkFunction;
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(methodkind));

               { write parameter info. The parameters must be written in reverse order
                 if this method uses right to left parameter pushing! }
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(def.maxparacount));
               if def.proccalloption in pushleftright_pocalls then
                 begin
                   for i:=0 to def.paras.count-1 do
                     write_para(tparavarsym(def.paras[i]));
                 end
               else
                 begin
                   for i:=def.paras.count-1 downto 0 do
                     write_para(tparavarsym(def.paras[i]));
                 end;

               { write name of result type }
               write_rtti_name(def.returndef);
            end
          else
            begin
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkprocvar));
              write_rtti_name(def);
            end;
        end;


        procedure objectdef_rtti(def:tobjectdef);

          procedure objectdef_rtti_class_init(def:tobjectdef);
          begin
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(def.size));
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(fields_count(def.symtable,rt)));
            fields_write_rtti_data(def.symtable,rt);
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

            if (oo_has_vmt in def.objectoptions) then
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Createname(def.vmt_mangledname,0))
            else
              current_asmdata.asmlists[al_rtti].concat(Tai_const.create_sym(nil));

            { write parent typeinfo }
            if assigned(def.childof) and
               (oo_can_have_published in def.childof.objectoptions) then
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_sym(ref_rtti(def.childof,fullrtti)))
            else
              current_asmdata.asmlists[al_rtti].concat(Tai_const.create_sym(nil));

            { total number of unique properties }
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(propnamelist.count));

            { write unit name }
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(length(current_module.realmodulename^)));
            current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(current_module.realmodulename^));
{$ifdef cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}

            { write published properties for this object }
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(published_properties_count(def.symtable)));
{$ifdef cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
            published_properties_write_rtti_data(propnamelist,def.symtable);

            propnamelist.free;
          end;

          procedure objectdef_rtti_interface_full(def:tobjectdef);
          var
            i : longint;
            propnamelist : TFPHashObjectList;
          begin
            { Collect unique property names with nameindex }
            propnamelist:=TFPHashObjectList.Create;
            collect_propnamelist(propnamelist,def);

            { write parent typeinfo }
            if assigned(def.childof) then
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_sym(ref_rtti(def.childof,fullrtti)))
            else
              current_asmdata.asmlists[al_rtti].concat(Tai_const.create_sym(nil));

            { interface: write flags, iid and iidstr }
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(
              { ugly, but working }
              longint([
                TCompilerIntfFlag(ord(ifHasGuid)*ord(assigned(def.iidguid))),
                TCompilerIntfFlag(ord(ifHasStrGUID)*ord(assigned(def.iidstr)))
              ])
              {
              ifDispInterface,
              ifDispatch, }
              ));
{$ifdef cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_32bit(longint(def.iidguid^.D1)));
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(def.iidguid^.D2));
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_16bit(def.iidguid^.D3));
            for i:=Low(def.iidguid^.D4) to High(def.iidguid^.D4) do
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(def.iidguid^.D4[i]));

            { write unit name }
            current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(length(current_module.realmodulename^)));
            current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(current_module.realmodulename^));
{$ifdef cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}

            { write iidstr }
            if assigned(def.iidstr) then
              begin
                current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(length(def.iidstr^)));
                current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(def.iidstr^));
              end
            else
              current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(0));
{$ifdef cpurequiresproperalignment}
            current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}

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
             odt_interfacecom:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkinterface));
             odt_interfacecorba:
               current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(tkinterfaceCorba));
             else
               internalerror(200611034);
           end;

           { generate the name }
           current_asmdata.asmlists[al_rtti].concat(Tai_const.Create_8bit(length(def.objrealname^)));
           current_asmdata.asmlists[al_rtti].concat(Tai_string.Create(def.objrealname^));
{$ifdef cpurequiresproperalignment}
           current_asmdata.asmlists[al_rtti].concat(cai_align.Create(sizeof(TConstPtrUInt)));
{$endif cpurequiresproperalignment}

           case rt of
             initrtti :
               begin
                 if def.objecttype in [odt_class,odt_object] then
                   objectdef_rtti_class_init(def)
                 else
                   objectdef_rtti_interface_init(def);
               end;
             fullrtti :
               begin
                 if def.objecttype in [odt_class,odt_object] then
                   objectdef_rtti_class_full(def)
                 else
                   objectdef_rtti_interface_full(def);
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
          else
            unknown_rtti(tstoreddef(def));
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
            write_rtti(tarraydef(def).elementdef,rt);
          recorddef :
            fields_write_rtti(trecorddef(def).symtable,rt);
          objectdef :
            begin
              if assigned(tobjectdef(def).childof) then
                write_rtti(tobjectdef(def).childof,rt);
              if rt=initrtti then
                fields_write_rtti(tobjectdef(def).symtable,rt)
              else
                published_write_rtti(tobjectdef(def).symtable,rt);
            end;
        end;
      end;


    function TRTTIWriter.ref_rtti(def:tdef;rt:trttitype):tasmsymbol;
      begin
        result:=current_asmdata.RefAsmSymbol(def.rtti_mangledname(rt));
      end;


    procedure TRTTIWriter.write_rtti(def:tdef;rt:trttitype);
      var
        rttilab : tasmsymbol;
      begin
        { only write rtti of definitions from the current module }
        if not findunitsymtable(def.owner).iscurrentunit then
          exit;
        { prevent recursion }
        if rttidefstate[rt] in def.defstates then
          exit;
        include(def.defstates,rttidefstate[rt]);
        { write first all dependencies }
        write_child_rtti_data(def,rt);
        { write rtti data }
        rttilab:=current_asmdata.DefineAsmSymbol(tstoreddef(def).rtti_mangledname(rt),AB_GLOBAL,AT_DATA);
        maybe_new_object_file(current_asmdata.asmlists[al_rtti]);
        new_section(current_asmdata.asmlists[al_rtti],sec_rodata,rttilab.name,const_align(sizeof(aint)));
        current_asmdata.asmlists[al_rtti].concat(Tai_symbol.Create_global(rttilab,0));
        write_rtti_data(def,rt);
        current_asmdata.asmlists[al_rtti].concat(Tai_symbol_end.Create(rttilab));
      end;


    function TRTTIWriter.get_rtti_label(def:tdef;rt:trttitype):tasmsymbol;
      begin
        result:=current_asmdata.RefAsmSymbol(def.rtti_mangledname(rt));
      end;

end.

