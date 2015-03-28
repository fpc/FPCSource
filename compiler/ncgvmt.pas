{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generates VMT for classes/objects and interface wrappers

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
unit ncgvmt;

{$i fpcdefs.inc}

interface

    uses
      aasmdata,aasmbase,aasmcnst,
      symbase,symtype,symdef;

    type
      pprocdeftree = ^tprocdeftree;
      tprocdeftree = record
         data : tprocdef;
         nl   : tasmlabel;
         l,r  : pprocdeftree;
      end;

      TVMTWriter=class
      private
        _Class : tobjectdef;
        { message tables }
        root : pprocdeftree;

        procedure disposeprocdeftree(p : pprocdeftree);
        procedure insertmsgint(p:TObject;arg:pointer);
        procedure insertmsgstr(p:TObject;arg:pointer);
        procedure insertint(p : pprocdeftree;var at : pprocdeftree;var count:longint);
        procedure insertstr(p : pprocdeftree;var at : pprocdeftree;var count:longint);
        function RedirectToEmpty(procdef: tprocdef): boolean;
        procedure writenames(list : TAsmList;p : pprocdeftree);
        procedure writeintentry(tcb: ttai_typedconstbuilder; p: pprocdeftree; entrydef: tdef);
        procedure writestrentry(tcb: ttai_typedconstbuilder; p: pprocdeftree; entrydef: tdef);
{$ifdef WITHDMT}
        { dmt }
        procedure insertdmtentry(p:TObject;arg:pointer);
        procedure writedmtindexentry(p : pprocdeftree);
        procedure writedmtaddressentry(p : pprocdeftree);
{$endif}
        { published methods }
        procedure do_count_published_methods(p:TObject;arg:pointer);
        procedure do_gen_published_methods(p:TObject;arg:pointer);
        { virtual methods }
        procedure writevirtualmethods(List:TAsmList);
        { interface tables }
        function  intf_get_vtbl_name(AImplIntf:TImplementedInterface): string;
        procedure intf_create_vtbl(rawdata: TAsmList;AImplIntf:TImplementedInterface);
        procedure intf_gen_intf_ref(tcb: ttai_typedconstbuilder; AImplIntf: TImplementedInterface; interfaceentrydef, interfaceentrytypedef: tdef);
        function  intf_write_table(list : TAsmList):TAsmLabel;
        { get a table def of the form
            record
              count: countdef;
              elements: array[0..count-1] of elementdef
            end;
          Returns both the outer record and the inner arraydef
        }
        procedure gettabledef(const basename: string; countdef, elementdef: tdef; count: longint; packrecords: shortint; out recdef: trecorddef; out arrdef: tarraydef);
        function getrecorddef(const name: string; const fields: array of tdef; packrecords: shortint): trecorddef;
        { generates the message tables for a class }
        function  genstrmsgtab(list : TAsmList) : tasmlabel;
        function  genintmsgtab(list : TAsmList) : tasmlabel;
        function  genpublishedmethodstable(list : TAsmList) : tasmlabel;
        function  generate_field_table(list : TAsmList) : tasmlabel;
        procedure generate_abstract_stub(list:TAsmList;pd:tprocdef);
{$ifdef WITHDMT}
        { generates a DMT for _class }
        function  gendmt : tasmlabel;
{$endif WITHDMT}
      public
        constructor create(c:tobjectdef); virtual;
        { write the VMT to al_globals }
        procedure writevmt;
        procedure writeinterfaceids(list: TAsmList);
        { should the VMT writer be used at all (e.g., not for the JVM target) }
        class function use_vmt_writer: boolean; virtual;
      end;
      TVMTWriterClass = class of TVMTWriter;

    { generate persistent type information like VMT, RTTI and inittables }
    procedure write_persistent_type_info(st:tsymtable;is_global:boolean);

  var
    CVMTWriter: TVMTWriterClass = TVMTWriter;

implementation

    uses
      cutils,cclasses,
      globtype,globals,verbose,constexp,
      systems,fmodule,
      symconst,symsym,symtable,defutil,
      aasmtai,
      wpobase,
      nobj,
      cgbase,parabase,paramgr,cgobj,cgcpu,hlcgobj,hlcgcpu,
      ncgrtti;


{*****************************************************************************
                                TVMTWriter
*****************************************************************************}

    constructor TVMTWriter.create(c:tobjectdef);
      begin
        inherited Create;
        _Class:=c;
      end;


{**************************************
           Message Tables
**************************************}

    procedure TVMTWriter.disposeprocdeftree(p : pprocdeftree);
      begin
         if assigned(p^.l) then
           disposeprocdeftree(p^.l);
         if assigned(p^.r) then
           disposeprocdeftree(p^.r);
         dispose(p);
      end;


    procedure TVMTWriter.insertint(p : pprocdeftree;var at : pprocdeftree;var count:longint);
      begin
         if at=nil then
           begin
              at:=p;
              inc(count);
           end
         else
           begin
              if p^.data.messageinf.i<at^.data.messageinf.i then
                insertint(p,at^.l,count)
              else if p^.data.messageinf.i>at^.data.messageinf.i then
                insertint(p,at^.r,count)
              else
                Message1(parser_e_duplicate_message_label,tostr(p^.data.messageinf.i));
           end;
      end;


    procedure TVMTWriter.insertstr(p : pprocdeftree;var at : pprocdeftree;var count:longint);
      var
         i : integer;
      begin
         if at=nil then
           begin
              at:=p;
              inc(count);
           end
         else
           begin
              i:=CompareStr(p^.data.messageinf.str^,at^.data.messageinf.str^);
              if i<0 then
                insertstr(p,at^.l,count)
              else if i>0 then
                insertstr(p,at^.r,count)
              else
                Message1(parser_e_duplicate_message_label,p^.data.messageinf.str^);
           end;
      end;


    procedure TVMTWriter.insertmsgint(p:TObject;arg:pointer);
      var
        i  : longint;
        pd : Tprocdef;
        pt : pprocdeftree;
      begin
        if tsym(p).typ<>procsym then
          exit;
        for i:=0 to Tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(Tprocsym(p).ProcdefList[i]);
            if po_msgint in pd.procoptions then
              begin
                new(pt);
                pt^.data:=pd;
                pt^.l:=nil;
                pt^.r:=nil;
                insertint(pt,root,plongint(arg)^);
              end;
          end;
      end;


    procedure TVMTWriter.insertmsgstr(p:TObject;arg:pointer);
      var
        i  : longint;
        pd : Tprocdef;
        pt : pprocdeftree;
      begin
        if tsym(p).typ<>procsym then
          exit;
        for i:=0 to Tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(Tprocsym(p).ProcdefList[i]);
            if po_msgstr in pd.procoptions then
              begin
                new(pt);
                pt^.data:=pd;
                pt^.l:=nil;
                pt^.r:=nil;
                insertstr(pt,root,plongint(arg)^);
              end;
          end;
      end;


    procedure TVMTWriter.writenames(list : TAsmList;p : pprocdeftree);
      var
        ca : pchar;
        len : byte;
        tcb : ttai_typedconstbuilder;
      begin
         current_asmdata.getglobaldatalabel(p^.nl);
         if assigned(p^.l) then
           writenames(list,p^.l);
         tcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);
         len:=length(p^.data.messageinf.str^);
         tcb.maybe_begin_aggregate(getarraydef(cansichartype,len+1));
         tcb.emit_tai(tai_const.create_8bit(len),cansichartype);
         getmem(ca,len+1);
         move(p^.data.messageinf.str^[1],ca^,len);
         ca[len]:=#0;
         tcb.emit_tai(Tai_string.Create_pchar(ca,len),getarraydef(cansichartype,len));
         tcb.maybe_end_aggregate(getarraydef(cansichartype,len+1));
         list.concatList(tcb.get_final_asmlist(p^.nl,getarraydef(cansichartype,len+1),sec_rodata_norel,'',sizeof(pint)));
         tcb.free;
         if assigned(p^.r) then
           writenames(list,p^.r);
      end;

    procedure TVMTWriter.writestrentry(tcb: ttai_typedconstbuilder; p: pprocdeftree; entrydef: tdef);
      begin
         if assigned(p^.l) then
           writestrentry(tcb,p^.l,entrydef);

         { write name label }
         tcb.maybe_begin_aggregate(entrydef);
         tcb.emit_tai(Tai_const.Create_sym(p^.nl),getpointerdef(getarraydef(cansichartype,length(p^.data.messageinf.str^)+1)));
         tcb.queue_init(voidcodepointertype);
         tcb.queue_emit_proc(p^.data);
         tcb.maybe_end_aggregate(entrydef);

         if assigned(p^.r) then
           writestrentry(tcb,p^.r,entrydef);
     end;


    function TVMTWriter.genstrmsgtab(list : TAsmList) : tasmlabel;
      var
         count : longint;
         tcb: ttai_typedconstbuilder;
         msgstrtabdef: trecorddef;
         msgstrentry: tdef;
         msgarraydef: tarraydef;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.SymList.ForEachCall(@insertmsgstr,@count);

         tcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);
         { write all names }
         if assigned(root) then
           writenames(list,root);

         { now start writing of the message string table }
         current_asmdata.getlabel(result,alt_data);
         {
           TStringMessageTable = record
              count : longint;
              msgstrtable : array[0..0] of tmsgstrtable;
           end;

           Instead of 0 as the upper bound, use the actual upper bound
         }
         msgstrentry:=search_system_type('TMSGSTRTABLE').typedef;
         gettabledef('fpc_intern_TStringMessageTable_',s32inttype,msgstrentry,count,0,msgstrtabdef,msgarraydef);
         { outer record (TStringMessageTable) }
         tcb.maybe_begin_aggregate(msgstrtabdef);
         tcb.emit_tai(Tai_const.Create_32bit(count),s32inttype);
         if assigned(root) then
           begin
              { array of TMsgStrTable }
              tcb.maybe_begin_aggregate(msgarraydef);
              writestrentry(tcb,root,msgstrentry);
              tcb.maybe_end_aggregate(msgarraydef);
              disposeprocdeftree(root);
           end;
         tcb.maybe_end_aggregate(msgstrtabdef);
         list.concatList(tcb.get_final_asmlist(result,msgstrtabdef,sec_rodata,'',sizeof(pint)));
         tcb.free;
      end;


    procedure TVMTWriter.writeintentry(tcb: ttai_typedconstbuilder; p: pprocdeftree; entrydef: tdef);
      begin
         if assigned(p^.l) then
           writeintentry(tcb,p^.l,entrydef);

         tcb.maybe_begin_aggregate(entrydef);
         { write integer dispatch number }
         tcb.emit_tai(Tai_const.Create_32bit(p^.data.messageinf.i),u32inttype);
         tcb.queue_init(voidcodepointertype);
         tcb.queue_emit_proc(p^.data);
         tcb.maybe_end_aggregate(entrydef);

         if assigned(p^.r) then
           writeintentry(tcb,p^.r,entrydef);
      end;


    function TVMTWriter.genintmsgtab(list : TAsmList) : tasmlabel;
      var
         r : tasmlabel;
         count : longint;
         tcb: ttai_typedconstbuilder;
         msgintdef: trecorddef;
         msginttabledef: trecorddef;
         msgintarrdef: tarraydef;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.SymList.ForEachCall(@insertmsgint,@count);

         { now start writing of the message string table }

         { from objpas.inc:
             TMsgIntTable = record
                index : dword;
                method : codepointer;
             end;
         }
         msginttabledef:=getrecorddef('fpc_intern_msgint_table',[u32inttype,voidcodepointertype],0);
         { from objpas.inc:
             TMsgInt = record
                count : longint;
                msgs : array[0..0] of TMsgIntTable;
             end;
         }
         current_asmdata.getlabel(r,alt_data);
         tcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);
         genintmsgtab:=r;
         gettabledef('fpc_msgint_table_entries_',s32inttype,msginttabledef,count,0,msgintdef,msgintarrdef);
         tcb.maybe_begin_aggregate(msgintdef);
         tcb.emit_tai(Tai_const.Create_32bit(count),s32inttype);
         if assigned(root) then
           begin
              tcb.maybe_begin_aggregate(msgintarrdef);
              writeintentry(tcb,root,msginttabledef);
              tcb.maybe_end_aggregate(msgintarrdef);
              disposeprocdeftree(root);
           end;
         tcb.maybe_end_aggregate(msgintdef);
         list.concatList(tcb.get_final_asmlist(result,msgintdef,sec_rodata,'',sizeof(pint)));
         tcb.free;
      end;

{$ifdef WITHDMT}

{**************************************
              DMT
**************************************}

    procedure TVMTWriter.insertdmtentry(p:TObject;arg:pointer);

      var
         hp : tprocdef;
         pt : pprocdeftree;

      begin
         if tsym(p).typ=procsym then
           begin
              hp:=tprocsym(p).definition;
              while assigned(hp) do
                begin
                   if (po_msgint in hp.procoptions) then
                     begin
                        new(pt);
                        pt^.p:=hp;
                        pt^.l:=nil;
                        pt^.r:=nil;
                        insertint(pt,root);
                     end;
                   hp:=hp.nextoverloaded;
                end;
           end;
      end;

    procedure TVMTWriter.writedmtindexentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtindexentry(p^.l);
         al_globals.concat(Tai_const.Create_32bit(p^.data.messageinf.i));
         if assigned(p^.r) then
           writedmtindexentry(p^.r);
      end;

    procedure TVMTWriter.writedmtaddressentry(p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writedmtaddressentry(p^.l);
         al_globals.concat(Tai_const_symbol.Createname(p^.data.mangledname,0));
         if assigned(p^.r) then
           writedmtaddressentry(p^.r);
      end;

    function TVMTWriter.gendmt : tasmlabel;

      var
         r : tasmlabel;

      begin
         root:=nil;
         count:=0;
         gendmt:=nil;
         { insert all message handlers into a tree, sorted by number }
         _class.symtable.SymList.ForEachCall(insertdmtentry);

         if count>0 then
           begin
              current_asmdata.getglobaldatalabel(r);
              gendmt:=r;
              al_globals.concat(cai_align.create(const_align(sizeof(pint))));
              al_globals.concat(Tai_label.Create(r));
              { entries for caching }
              al_globals.concat(Tai_const.Create_ptr(0));
              al_globals.concat(Tai_const.Create_ptr(0));

              al_globals.concat(Tai_const.Create_32bit(count));
              if assigned(root) then
                begin
                   writedmtindexentry(root);
                   writedmtaddressentry(root);
                   disposeprocdeftree(root);
                end;
           end;
      end;

{$endif WITHDMT}

{**************************************
        Published Methods
**************************************}

    procedure TVMTWriter.do_count_published_methods(p:TObject;arg:pointer);
      var
        i  : longint;
        pd : tprocdef;
      begin
        if (tsym(p).typ<>procsym) then
          exit;
        for i:=0 to Tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(Tprocsym(p).ProcdefList[i]);
            if (pd.procsym=tsym(p)) and
               (pd.visibility=vis_published) then
              inc(plongint(arg)^);
          end;
      end;


    type
      tvmtasmoutput = record
        pubmethodstcb: ttai_typedconstbuilder;
        list: tasmlist;
        methodnamerec: trecorddef;
      end;
      pvmtasmoutput = ^tvmtasmoutput;

    procedure TVMTWriter.do_gen_published_methods(p:TObject;arg:pointer);
      var
        i  : longint;
        l  : tasmlabel;
        pd : tprocdef;
        lists: pvmtasmoutput absolute arg;
        tcb  : ttai_typedconstbuilder;
        namedef : tdef;
      begin
        if (tsym(p).typ<>procsym) then
          exit;
        for i:=0 to Tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(Tprocsym(p).ProcdefList[i]);
            if (pd.procsym=tsym(p)) and
               (pd.visibility=vis_published) then
              begin
                current_asmdata.getlabel(l,alt_data);
                { l: name_of_method }
                tcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);

                namedef:=tcb.emit_shortstring_const(tsym(p).realname);
                lists^.list.concatList(tcb.get_final_asmlist(l,namedef,sec_rodata_norel,'',sizeof(pint)));
                tcb.free;
                { the tmethodnamerec }
                lists^.pubmethodstcb.maybe_begin_aggregate(lists^.methodnamerec);
                { convert the pointer to the name into a generic pshortstring,
                  so all entries can share the same recorddef }
                lists^.pubmethodstcb.queue_init(charpointertype);
                lists^.pubmethodstcb.queue_emit_asmsym(l,namedef);
                if po_abstractmethod in pd.procoptions then
                  lists^.pubmethodstcb.emit_tai(Tai_const.Create_nil_codeptr,voidcodepointertype)
                else
                  begin
                    { convert the procdef in a generic voidcodepointer, same
                      reason as above }
                    lists^.pubmethodstcb.queue_init(voidcodepointertype);
                    lists^.pubmethodstcb.queue_emit_proc(pd);
                  end;
                lists^.pubmethodstcb.maybe_end_aggregate(lists^.methodnamerec);
              end;
           end;
      end;


    function TVMTWriter.genpublishedmethodstable(list : TAsmList) : tasmlabel;

      var
         l : tasmlabel;
         count : longint;
         lists : tvmtasmoutput;
         pubmethodsdef: trecorddef;
         pubmethodsarraydef: tarraydef;
      begin
         count:=0;
         _class.symtable.SymList.ForEachCall(@do_count_published_methods,@count);
         if count>0 then
           begin
              lists.list:=list;
              { in the list of the published methods (from objpas.inc):
                  tmethodnamerec = packed record
                     name : pshortstring;
                     addr : codepointer;
                  end;
              }
              lists.methodnamerec:=getrecorddef('fpc_intern_tmethodnamerec',[getpointerdef(cshortstringtype),voidcodepointertype],1);
              { from objpas.inc:
                  tmethodnametable = packed record
                    count : dword;
                    entries : packed array[0..0] of tmethodnamerec;
                  end;
               }
              lists.pubmethodstcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);
              current_asmdata.getlabel(l,alt_data);
              gettabledef('fpc_intern_tmethodnametable_',u32inttype,lists.methodnamerec,count,1,pubmethodsdef,pubmethodsarraydef);
              { begin tmethodnametable }
              lists.pubmethodstcb.maybe_begin_aggregate(pubmethodsdef);
              { emit count field }
              lists.pubmethodstcb.emit_tai(Tai_const.Create_32bit(count),u32inttype);
              { begin entries field (array) }
              lists.pubmethodstcb.maybe_begin_aggregate(pubmethodsarraydef);
              { add all entries elements }
              _class.symtable.SymList.ForEachCall(@do_gen_published_methods,@lists);
              { end entries field (array) }
              lists.pubmethodstcb.maybe_end_aggregate(pubmethodsarraydef);
              { end methodnametable }
              lists.pubmethodstcb.maybe_end_aggregate(pubmethodsdef);
              list.concatlist(lists.pubmethodstcb.get_final_asmlist(l,pubmethodsdef,sec_rodata,'',sizeof(pint)));
              lists.pubmethodstcb.free;
              genpublishedmethodstable:=l;
           end
         else
           genpublishedmethodstable:=nil;
      end;


    function TVMTWriter.generate_field_table(list : TAsmList) : tasmlabel;
      var
        i   : longint;
        sym : tsym;
        fieldtable,
        classtable : tasmlabel;
        classindex,
        fieldcount : longint;
        classtablelist : TFPList;
        tcb: ttai_typedconstbuilder;
        packrecords: longint;
        classdef: tobjectdef;
        classtabledef,
        fieldtabledef: trecorddef;
      begin
        classtablelist:=TFPList.Create;
        { retrieve field info fields }
        fieldcount:=0;
        for i:=0 to _class.symtable.SymList.Count-1 do
          begin
            sym:=tsym(_class.symtable.SymList[i]);
            if (sym.typ=fieldvarsym) and
               (sym.visibility=vis_published) then
             begin
                if tfieldvarsym(sym).vardef.typ<>objectdef then
                  internalerror(200611032);
                classindex:=classtablelist.IndexOf(tfieldvarsym(sym).vardef);
                if classindex=-1 then
                  classtablelist.Add(tfieldvarsym(sym).vardef);
                inc(fieldcount);
             end;
          end;

        if fieldcount>0 then
          begin
            current_asmdata.getlabel(fieldtable,alt_data);
            current_asmdata.getlabel(classtable,alt_data);

            if (tf_requires_proper_alignment in target_info.flags) then
              packrecords:=0
            else
              packrecords:=1;

            { generate the class table }
            tcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);
            tcb.begin_anonymous_record('$fpc_intern_classtable_'+tostr(classtablelist.Count-1),
              packrecords,
              targetinfos[target_info.system]^.alignment.recordalignmin,
              targetinfos[target_info.system]^.alignment.maxCrecordalign);
            tcb.emit_tai(Tai_const.Create_16bit(classtablelist.count),u16inttype);
            for i:=0 to classtablelist.Count-1 do
              begin
                classdef:=tobjectdef(classtablelist[i]);
                { type of the field }
                tcb.queue_init(voidpointertype);
                { reference to the vmt }
                tcb.queue_emit_asmsym(
                  current_asmdata.RefAsmSymbol(classdef.vmt_mangledname,AT_DATA),
                  tfieldvarsym(classdef.vmt_field).vardef);
              end;
            classtabledef:=tcb.end_anonymous_record;
            list.concatlist(tcb.get_final_asmlist(classtable,classtabledef,sec_rodata,'',sizeof(pint)));
            tcb.free;

            { write fields }
            {
              TFieldTable =
             $ifndef FPC_REQUIRES_PROPER_ALIGNMENT
              packed
             $endif FPC_REQUIRES_PROPER_ALIGNMENT
              record
                FieldCount: Word;
                ClassTable: Pointer;
                Fields: array[0..0] of TFieldInfo
              end;
            }
            tcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);
            { can't easily specify a name here for reuse of the constructed def,
              since it's full of variable length shortstrings (-> all of those
              lengths and their order would have to incorporated in the name,
              plus there would be very little chance that it could actually be
              reused }
            tcb.begin_anonymous_record('',packrecords,
              targetinfos[target_info.system]^.alignment.recordalignmin,
              targetinfos[target_info.system]^.alignment.maxCrecordalign);
            tcb.emit_tai(Tai_const.Create_16bit(fieldcount),u16inttype);
            tcb.emit_tai(Tai_const.Create_sym(classtable),getpointerdef(classtabledef));
            for i:=0 to _class.symtable.SymList.Count-1 do
              begin
                sym:=tsym(_class.symtable.SymList[i]);
                if (sym.typ=fieldvarsym) and
                  (sym.visibility=vis_published) then
                  begin
                    {
                      TFieldInfo =
                     $ifndef FPC_REQUIRES_PROPER_ALIGNMENT
                      packed
                     $endif FPC_REQUIRES_PROPER_ALIGNMENT
                      record
                        FieldOffset: PtrUInt;
                        ClassTypeIndex: Word;
                        Name: ShortString;
                      end;
                    }
                    tcb.begin_anonymous_record('$fpc_intern_fieldinfo_'+tostr(length(tfieldvarsym(sym).realname)),packrecords,
                      targetinfos[target_info.system]^.alignment.recordalignmin,
                      targetinfos[target_info.system]^.alignment.maxCrecordalign);
                    tcb.emit_tai(Tai_const.Create_pint(tfieldvarsym(sym).fieldoffset),ptruinttype);
                    classindex:=classtablelist.IndexOf(tfieldvarsym(sym).vardef);
                    if classindex=-1 then
                      internalerror(200611033);
                    tcb.emit_tai(Tai_const.Create_16bit(classindex+1),u16inttype);
                    tcb.emit_shortstring_const(tfieldvarsym(sym).realname);
                    tcb.end_anonymous_record;
                  end;
              end;
            fieldtabledef:=tcb.end_anonymous_record;
            list.concatlist(tcb.get_final_asmlist(fieldtable,fieldtabledef,sec_rodata,'',sizeof(pint)));
            tcb.free;

            result:=fieldtable;
          end
        else
          result:=nil;

        classtablelist.free;
      end;


{**************************************
           Interface tables
**************************************}

    function  TVMTWriter.intf_get_vtbl_name(AImplIntf:TImplementedInterface): string;
      begin
        result:=make_mangledname('VTBL',_class.owner,_class.objname^+'_$_'+AImplIntf.IntfDef.objname^);
      end;


    procedure TVMTWriter.intf_create_vtbl(rawdata: TAsmList;AImplIntf:TImplementedInterface);
      var
        pd : tprocdef;
        vtblstr,
        hs : string;
        i  : longint;
      begin
        vtblstr:=intf_get_vtbl_name(AImplIntf);
        rawdata.concat(cai_align.create(const_align(sizeof(pint))));
        rawdata.concat(tai_symbol.createname(vtblstr,AT_DATA,0));
        if assigned(AImplIntf.procdefs) then
          begin
            for i:=0 to AImplIntf.procdefs.count-1 do
              begin
                pd:=tprocdef(AImplIntf.procdefs[i]);
                hs:=make_mangledname('WRPR',_class.owner,_class.objname^+'_$_'+AImplIntf.IntfDef.objname^+'_$_'+
                                     tostr(i)+'_$_'+pd.mangledname);
                { create reference }
                rawdata.concat(Tai_const.Createname(hs,AT_FUNCTION,0));
              end;
           end;
        rawdata.concat(tai_symbol_end.createname(vtblstr));
      end;


    procedure TVMTWriter.intf_gen_intf_ref(tcb: ttai_typedconstbuilder; AImplIntf: TImplementedInterface; interfaceentrydef, interfaceentrytypedef: tdef);
      var
        pd: tprocdef;
      begin
        tcb.maybe_begin_aggregate(interfaceentrydef);
        { GUID (or nil for Corba interfaces) }
        if AImplIntf.IntfDef.objecttype in [odt_interfacecom] then
          tcb.emit_tai(Tai_const.CreateName(
            make_mangledname('IID',AImplIntf.IntfDef.owner,AImplIntf.IntfDef.objname^),AT_DATA,0),getpointerdef(rec_tguid))
        else
          tcb.emit_tai(Tai_const.Create_nil_dataptr,getpointerdef(rec_tguid));

        { VTable }
        tcb.queue_init(voidpointertype);
        tcb.queue_emit_asmsym(current_asmdata.RefAsmSymbol(intf_get_vtbl_name(AImplIntf.VtblImplIntf),AT_DATA),AImplIntf.VtblImplIntf.IntfDef);
        { IOffset field }
        case AImplIntf.VtblImplIntf.IType of
          etFieldValue, etFieldValueClass,
          etStandard:
            tcb.emit_tai(Tai_const.Create_pint(AImplIntf.VtblImplIntf.IOffset),ptruinttype);
          etStaticMethodResult, etStaticMethodClass:
            begin
              pd:=tprocdef(tpropertysym(AImplIntf.ImplementsGetter).propaccesslist[palt_read].procdef);
              tcb.queue_init(ptruinttype);
              tcb.queue_emit_proc(pd);
            end;
          etVirtualMethodResult, etVirtualMethodClass:
            begin
              pd:=tprocdef(tpropertysym(AImplIntf.ImplementsGetter).propaccesslist[palt_read].procdef);
              tcb.emit_tai(Tai_const.Create_pint(tobjectdef(pd.struct).vmtmethodoffset(pd.extnumber)),ptruinttype);
            end;
          else
            internalerror(200802162);
        end;

        { IIDStr }
        tcb.emit_tai(Tai_const.CreateName(
          make_mangledname('IIDSTR',AImplIntf.IntfDef.owner,AImplIntf.IntfDef.objname^),AT_DATA,0),getpointerdef(getarraydef(cansichartype,length(AImplIntf.IntfDef.iidstr^)+1)));
        { IType }
        tcb.emit_ord_const(aint(AImplIntf.VtblImplIntf.IType),interfaceentrytypedef);
        tcb.maybe_end_aggregate(interfaceentrydef);
      end;


    function TVMTWriter.intf_write_table(list : TAsmList):TAsmLabel;
      var
        i        : longint;
        ImplIntf : TImplementedInterface;
        tcb      : ttai_typedconstbuilder;
        tabledef : tdef;
        interfaceentrydef : tdef;
        interfaceentrytypedef: tdef;
        interfacearray: tdef;
      begin
        current_asmdata.getlabel(result,alt_data);
        tcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);
        tcb.begin_anonymous_record('',0,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        tcb.emit_tai(Tai_const.Create_pint(_class.ImplementedInterfaces.count),search_system_type('SIZEUINT').typedef);
        interfaceentrydef:=search_system_type('TINTERFACEENTRY').typedef;
        interfaceentrytypedef:=search_system_type('TINTERFACEENTRYTYPE').typedef;
        interfacearray:=getarraydef(interfaceentrydef,_class.ImplementedInterfaces.count);
        tcb.maybe_begin_aggregate(interfacearray);
        { Write vtbl references }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            intf_gen_intf_ref(tcb,ImplIntf,interfaceentrydef,interfaceentrytypedef);
          end;
        tcb.maybe_end_aggregate(interfacearray);
        tabledef:=tcb.end_anonymous_record;
        list.concatlist(tcb.get_final_asmlist(result,tabledef,sec_rodata,'',tabledef.alignment));

        { Write vtbls }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            if ImplIntf.VtblImplIntf=ImplIntf then
              intf_create_vtbl(list,ImplIntf);
          end;
        tcb.free;
      end;


    procedure TVMTWriter.gettabledef(const basename: string; countdef, elementdef: tdef; count: longint; packrecords: shortint; out recdef: trecorddef; out arrdef: tarraydef);
      var
        fields: tfplist;
        name: TIDString;
        srsym: tsym;
        srsymtable: tsymtable;
      begin
        { already created a message string table with this number of elements
          in this unit -> reuse the def }
        name:=basename+tostr(count);
        if searchsym_type(name,srsym,srsymtable) then
          begin
            recdef:=trecorddef(ttypesym(srsym).typedef);
            arrdef:=tarraydef(trecordsymtable(recdef.symtable).findfieldbyoffset(countdef.size).vardef);
            exit
          end;
        recdef:=crecorddef.create_global_internal('$'+basename+tostr(count),packrecords,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        fields:=tfplist.create;
        fields.add(countdef);
        if count>0 then
          begin
            arrdef:=carraydef.create(0,count-1,ptruinttype);
            arrdef.elementdef:=elementdef;
            fields.add(arrdef);
          end
        else
          arrdef:=nil;
        recdef.add_fields_from_deflist(fields);
        fields.free;
      end;


    function TVMTWriter.getrecorddef(const name: string; const fields: array of tdef; packrecords: shortint): trecorddef;
      var
        fieldlist: tfplist;
        srsym: tsym;
        srsymtable: tsymtable;
        i: longint;
      begin
        if searchsym_type(name,srsym,srsymtable) then
          begin
            result:=trecorddef(ttypesym(srsym).typedef);
            exit
          end;
        fieldlist:=tfplist.create;
        for i:=low(fields) to high(fields) do
          fieldlist.add(fields[i]);
        result:=crecorddef.create_global_internal('$'+name,packrecords,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        result.add_fields_from_deflist(fieldlist);
        fieldlist.free;
      end;


  { Write interface identifiers to the data section }
  procedure TVMTWriter.writeinterfaceids(list: TAsmList);
    var
      s : string;
      tcb : ttai_typedconstbuilder;
      def : tdef;
    begin
      if assigned(_class.iidguid) then
        begin
          s:=make_mangledname('IID',_class.owner,_class.objname^);
          tcb:=ctai_typedconstbuilder.create([tcalo_make_dead_strippable]);
          tcb.emit_guid_const(_class.iidguid^);
          list.concatlist(tcb.get_final_asmlist(
            current_asmdata.DefineAsmSymbol(s,AB_GLOBAL,AT_DATA),
            rec_tguid,
            sec_rodata_norel,
            s,
            const_align(sizeof(pint))));
          tcb.free;
        end;
      s:=make_mangledname('IIDSTR',_class.owner,_class.objname^);
      tcb:=ctai_typedconstbuilder.create([tcalo_make_dead_strippable]);
      def:=tcb.emit_shortstring_const(_class.iidstr^);
      list.concatlist(tcb.get_final_asmlist(
        current_asmdata.DefineAsmSymbol(s,AB_GLOBAL,AT_DATA),
        def,
        sec_rodata_norel,
        s,
        sizeof(pint)));
      tcb.free;
    end;


  class function TVMTWriter.use_vmt_writer: boolean;
    begin
      result:=true;
    end;


    function TVMTWriter.RedirectToEmpty(procdef : tprocdef) : boolean;
      var
        i : longint;
        hp : PCGParaLocation;
      begin
        result:=false;
        if procdef.isempty then
          begin
{$ifdef x86}
            paramanager.create_funcretloc_info(procdef,calleeside);
            if (procdef.funcretloc[calleeside].Location^.loc=LOC_FPUREGISTER) then
              exit;
{$endif x86}
            procdef.init_paraloc_info(callerside);
            { we can redirect the call if no memory parameter is passed }
            for i:=0 to procdef.paras.count-1 do
              begin
                hp:=tparavarsym(procdef.paras[i]).paraloc[callerside].Location;
                while assigned(hp) do
                  begin
                    if not(hp^.Loc in [LOC_REGISTER,LOC_MMREGISTER,LOC_FPUREGISTER]) then
                      exit;
                    hp:=hp^.Next;
                  end;
              end;
            result:=true;
          end;
      end;


    procedure TVMTWriter.generate_abstract_stub(list:TAsmList;pd:tprocdef);
      var
        sym: TAsmSymbol;
      begin
        { Generate stubs for abstract methods, so their symbols are present and
          can be used e.g. to take address (see issue #24536). }
        if (po_global in pd.procoptions) and
           (pd.owner.defowner<>self._class) then
          exit;
        sym:=current_asmdata.GetAsmSymbol(pd.mangledname);
        if assigned(sym) and (sym.bind<>AB_EXTERNAL) then
          exit;
        maybe_new_object_file(list);
        new_section(list,sec_code,lower(pd.mangledname),target_info.alignment.procalign);
        if (po_global in pd.procoptions) then
          begin
            sym:=current_asmdata.DefineAsmSymbol(pd.mangledname,AB_GLOBAL,AT_FUNCTION);
            list.concat(Tai_symbol.Create_global(sym,0));
          end
        else
          begin
            sym:=current_asmdata.DefineAsmSymbol(pd.mangledname,AB_LOCAL,AT_FUNCTION);
            list.concat(Tai_symbol.Create(sym,0));
          end;
        hlcg.g_external_wrapper(list,pd,'FPC_ABSTRACTERROR');
        list.concat(Tai_symbol_end.Create(sym));
      end;


    procedure TVMTWriter.writevirtualmethods(List:TAsmList);
      var
         vmtpd : tprocdef;
         vmtentry : pvmtentry;
         i  : longint;
         procname : TSymStr;
{$ifdef vtentry}
         hs : string;
{$endif vtentry}
      begin
        if not assigned(_class.VMTEntries) then
          exit;
        for i:=0 to _class.VMTEntries.Count-1 do
         begin
           vmtentry:=pvmtentry(_class.vmtentries[i]);
           vmtpd:=vmtentry^.procdef;
           { safety checks }
           if not(po_virtualmethod in vmtpd.procoptions) then
             internalerror(200611082);
           if vmtpd.extnumber<>i then
             internalerror(200611083);
           if (po_abstractmethod in vmtpd.procoptions) then
             begin
               procname:='FPC_ABSTRACTERROR';
               generate_abstract_stub(current_asmdata.AsmLists[al_procedures],vmtpd);
             end
           else if (cs_opt_remove_emtpy_proc in current_settings.optimizerswitches) and RedirectToEmpty(vmtpd) then
             procname:='FPC_EMPTYMETHOD'
           else if not wpoinfomanager.optimized_name_for_vmt(_class,vmtpd,procname) then
             procname:=vmtpd.mangledname;
           List.concat(Tai_const.createname(procname,AT_FUNCTION,0));
{$ifdef vtentry}
           hs:='VTENTRY'+'_'+_class.vmt_mangledname+'$$'+tostr(_class.vmtmethodoffset(i) div sizeof(pint));
           current_asmdata.asmlists[al_globals].concat(tai_symbol.CreateName(hs,AT_DATA,0));
{$endif vtentry}
         end;
      end;


    procedure TVMTWriter.writevmt;
      var
         methodnametable,intmessagetable,
         strmessagetable,classnamelabel,
         fieldtablelabel : tasmlabel;
         hs: string;
{$ifdef WITHDMT}
         dmtlabel : tasmlabel;
{$endif WITHDMT}
         interfacetable : tasmlabel;
         templist : TAsmList;
         tcb: ttai_typedconstbuilder;
         classnamedef: tdef;
      begin
{$ifdef WITHDMT}
         dmtlabel:=gendmt;
{$endif WITHDMT}
         { this code gets executed after the current module's symtable has
           already been removed from the symtablestack -> add it again, so that
           newly created defs here end up in the right unit }
         symtablestack.push(current_module.localsymtable);
         templist:=TAsmList.Create;
         strmessagetable:=nil;
         interfacetable:=nil;
         fieldtablelabel:=nil;
         methodnametable:=nil;
         intmessagetable:=nil;
         classnamelabel:=nil;

         { write tables for classes, this must be done before the actual
           class is written, because we need the labels defined }
         if is_class(_class) then
          begin
            { write class name }
            current_asmdata.getlabel(classnamelabel,alt_data);
            tcb:=ctai_typedconstbuilder.create([tcalo_is_lab]);
            hs:=_class.RttiName;
            classnamedef:=tcb.emit_shortstring_const(_class.RttiName);
            templist.concatlist(tcb.get_final_asmlist(classnamelabel,classnamedef,sec_rodata_norel,'',sizeof(pint)));
            tcb.free;

            { interface table }
            if _class.ImplementedInterfaces.count>0 then
              interfacetable:=intf_write_table(templist);

            methodnametable:=genpublishedmethodstable(templist);
            fieldtablelabel:=generate_field_table(templist);

            { generate message and dynamic tables }
            if (oo_has_msgstr in _class.objectoptions) then
              strmessagetable:=genstrmsgtab(templist);
            if (oo_has_msgint in _class.objectoptions) then
              intmessagetable:=genintmsgtab(templist);
          end;

        { write debug info }
        maybe_new_object_file(current_asmdata.asmlists[al_globals]);
        new_section(current_asmdata.asmlists[al_globals],sec_rodata,_class.vmt_mangledname,const_align(sizeof(pint)));
        current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global(_class.vmt_mangledname,AT_DATA,0));

         { determine the size with symtable.datasize, because }
         { size gives back 4 for classes                    }
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create(aitconst_ptr,tObjectSymtable(_class.symtable).datasize));
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create(aitconst_ptr,-int64(tObjectSymtable(_class.symtable).datasize)));
{$ifdef WITHDMT}
         if _class.classtype=ct_object then
           begin
              if assigned(dmtlabel) then
                current_asmdata.asmlists[al_globals].concat(Tai_const_symbol.Create(dmtlabel)))
              else
                current_asmdata.asmlists[al_globals].concat(Tai_const.Create_ptr(0));
           end;
{$endif WITHDMT}
         { write pointer to parent VMT, this isn't implemented in TP }
         { but this is not used in FPC ? (PM) }
         { it's not used yet, but the delphi-operators as and is need it (FK) }
         { it is not written for parents that don't have any vmt !! }
         if assigned(_class.childof) and
            (oo_has_vmt in _class.childof.objectoptions) then
           current_asmdata.asmlists[al_globals].concat(Tai_const.Createname(_class.childof.vmt_mangledname,AT_DATA,0))
         else
           current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_dataptr);

         { write extended info for classes, for the order see rtl/inc/objpash.inc }
         if is_class(_class) then
          begin
            { pointer to class name string }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(classnamelabel));
            { pointer to dynamic table or nil }
            if (oo_has_msgint in _class.objectoptions) then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(intmessagetable))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_dataptr);
            { pointer to method table or nil }
            if assigned(methodnametable) then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(methodnametable))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_dataptr);
            { pointer to field table }
            if assigned(fieldtablelabel) then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(fieldtablelabel))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_dataptr);
            { pointer to type info of published section }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(RTTIWriter.get_rtti_label(_class,fullrtti)));
            { inittable for con-/destruction }
            if _class.members_need_inittable then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(RTTIWriter.get_rtti_label(_class,initrtti)))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_dataptr);
            { auto table }
            current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_dataptr);
            { interface table }
            if _class.ImplementedInterfaces.count>0 then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(interfacetable))
            else if _class.implements_any_interfaces then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_dataptr)
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(current_asmdata.RefAsmSymbol('FPC_EMPTYINTF',AT_DATA)));
            { table for string messages }
            if (oo_has_msgstr in _class.objectoptions) then
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_sym(strmessagetable))
            else
              current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_dataptr);
          end;
         { write virtual methods }
         writevirtualmethods(current_asmdata.asmlists[al_globals]);
         current_asmdata.asmlists[al_globals].concat(Tai_const.Create_nil_codeptr);
         { write the size of the VMT }
         current_asmdata.asmlists[al_globals].concat(Tai_symbol_end.Createname(_class.vmt_mangledname));
{$ifdef vtentry}
         { write vtinherit symbol to notify the linker of the class inheritance tree }
         hs:='VTINHERIT'+'_'+_class.vmt_mangledname+'$$';
         if assigned(_class.childof) then
           hs:=hs+_class.childof.vmt_mangledname
         else
           hs:=hs+_class.vmt_mangledname;
         current_asmdata.asmlists[al_globals].concat(tai_symbol.CreateName(hs,AT_DATA,0));
{$endif vtentry}
         if is_class(_class) then
           current_asmdata.asmlists[al_globals].concatlist(templist);
        templist.Free;
        symtablestack.pop(current_module.localsymtable);
      end;


    procedure gen_intf_wrapper(list:TAsmList;_class:tobjectdef);
      var
        i,j  : longint;
        tmps : string;
        pd   : TProcdef;
        ImplIntf : TImplementedInterface;
      begin
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            if (ImplIntf=ImplIntf.VtblImplIntf) and
               assigned(ImplIntf.ProcDefs) then
              begin
                for j:=0 to ImplIntf.ProcDefs.Count-1 do
                  begin
                    pd:=TProcdef(ImplIntf.ProcDefs[j]);
                    { we don't track method calls via interfaces yet ->
                      assume that every method called via an interface call
                      is reachable for now }
                    if (po_virtualmethod in pd.procoptions) and
                       not is_objectpascal_helper(tprocdef(pd).struct) then
                      tobjectdef(tprocdef(pd).struct).register_vmt_call(tprocdef(pd).extnumber);
                    tmps:=make_mangledname('WRPR',_class.owner,_class.objname^+'_$_'+
                      ImplIntf.IntfDef.objname^+'_$_'+tostr(j)+'_$_'+pd.mangledname);
                    { create wrapper code }
                    new_section(list,sec_code,tmps,target_info.alignment.procalign);
                    hlcg.init_register_allocators;
                    hlcg.g_intf_wrapper(list,pd,tmps,ImplIntf.ioffset);
                    hlcg.done_register_allocators;
                  end;
              end;
          end;
      end;


    procedure do_write_persistent_type_info(st:tsymtable;is_global:boolean);
      var
        i : longint;
        def : tdef;
        vmtwriter  : TVMTWriter;
      begin
        if not CVMTWriter.use_vmt_writer then
          exit;
        for i:=0 to st.DefList.Count-1 do
          begin
            def:=tdef(st.DefList[i]);
            case def.typ of
              recorddef :
                do_write_persistent_type_info(trecorddef(def).symtable,is_global);
              objectdef :
                begin
                  { Skip generics and forward defs }
                  if ([df_generic,df_genconstraint]*def.defoptions<>[]) or
                     (oo_is_forward in tobjectdef(def).objectoptions) then
                    continue;
                  do_write_persistent_type_info(tobjectdef(def).symtable,is_global);
                  { Write also VMT if not done yet }
                  if not(ds_vmt_written in def.defstates) then
                    begin
                      vmtwriter:=CVMTWriter.create(tobjectdef(def));
                      if is_interface(tobjectdef(def)) then
                        vmtwriter.writeinterfaceids(current_asmdata.AsmLists[al_globals]);
                      if (oo_has_vmt in tobjectdef(def).objectoptions) then
                        vmtwriter.writevmt;
                      vmtwriter.free;
                      include(def.defstates,ds_vmt_written);
                    end;
                  if is_class(def) then
                    gen_intf_wrapper(current_asmdata.asmlists[al_globals],tobjectdef(def));
                end;
              procdef :
                begin
                  if assigned(tprocdef(def).localst) and
                     (tprocdef(def).localst.symtabletype=localsymtable) then
                    do_write_persistent_type_info(tprocdef(def).localst,false);
                  if assigned(tprocdef(def).parast) then
                    do_write_persistent_type_info(tprocdef(def).parast,false);
                end;
            end;
            { generate always persistent tables for types in the interface so it can
              be reused in other units and give always the same pointer location. }
            { Init }
            if (
                assigned(def.typesym) and
                is_global and
                not is_objc_class_or_protocol(def)
               ) or
               is_managed_type(def) or
               (ds_init_table_used in def.defstates) then
              RTTIWriter.write_rtti(def,initrtti);
            { RTTI }
            if (
                assigned(def.typesym) and
                is_global and
                not is_objc_class_or_protocol(def)
               ) or
               (ds_rtti_table_used in def.defstates) then
              RTTIWriter.write_rtti(def,fullrtti);
          end;
      end;

    procedure write_persistent_type_info(st:tsymtable;is_global:boolean);
      begin
        create_hlcodegen;
        do_write_persistent_type_info(st,is_global);
        destroy_hlcodegen;
      end;

end.
