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
      aasmdata,aasmbase,
      symbase,symdef;

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
        procedure writeintentry(list : TAsmList;p : pprocdeftree);
        procedure writestrentry(list : TAsmList;p : pprocdeftree);
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
        procedure intf_gen_intf_ref(rawdata: TAsmList;AImplIntf:TImplementedInterface);
        function  intf_write_table(list : TAsmList):TAsmLabel;
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
      systems,
      symconst,symtype,symsym,symtable,defutil,
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
      begin
         current_asmdata.getdatalabel(p^.nl);
         if assigned(p^.l) then
           writenames(list,p^.l);
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_label.Create(p^.nl));
         len:=length(p^.data.messageinf.str^);
         list.concat(tai_const.create_8bit(len));
         getmem(ca,len+1);
         move(p^.data.messageinf.str^[1],ca^,len);
         ca[len]:=#0;
         list.concat(Tai_string.Create_pchar(ca,len));
         if assigned(p^.r) then
           writenames(list,p^.r);
      end;

    procedure TVMTWriter.writestrentry(list : TAsmList;p : pprocdeftree);

      begin
         if assigned(p^.l) then
           writestrentry(list,p^.l);

         { write name label }
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_const.Create_sym(p^.nl));
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_const.Createname(p^.data.mangledname,AT_FUNCTION,0));

         if assigned(p^.r) then
           writestrentry(list,p^.r);
     end;


    function TVMTWriter.genstrmsgtab(list : TAsmList) : tasmlabel;
      var
         count : longint;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.SymList.ForEachCall(@insertmsgstr,@count);

         { write all names }
         if assigned(root) then
           writenames(list,root);

         { now start writing of the message string table }
         current_asmdata.getlabel(result,alt_data);
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_label.Create(result));
         list.concat(cai_align.create(const_align(sizeof(longint))));
         list.concat(Tai_const.Create_32bit(count));
         list.concat(cai_align.create(const_align(sizeof(pint))));
         if assigned(root) then
           begin
              writestrentry(list,root);
              disposeprocdeftree(root);
           end;
      end;


    procedure TVMTWriter.writeintentry(list : TAsmList;p : pprocdeftree);
      begin
         if assigned(p^.l) then
           writeintentry(list,p^.l);

         { write name label }
         list.concat(cai_align.create(const_align(sizeof(longint))));
         list.concat(Tai_const.Create_32bit(p^.data.messageinf.i));
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_const.Createname(p^.data.mangledname,AT_FUNCTION,0));

         if assigned(p^.r) then
           writeintentry(list,p^.r);
      end;


    function TVMTWriter.genintmsgtab(list : TAsmList) : tasmlabel;
      var
         r : tasmlabel;
         count : longint;
      begin
         root:=nil;
         count:=0;
         { insert all message handlers into a tree, sorted by name }
         _class.symtable.SymList.ForEachCall(@insertmsgint,@count);

         { now start writing of the message string table }
         current_asmdata.getlabel(r,alt_data);
         list.concat(cai_align.create(const_align(sizeof(pint))));
         list.concat(Tai_label.Create(r));
         genintmsgtab:=r;
         list.concat(cai_align.create(const_align(sizeof(longint))));
         list.concat(Tai_const.Create_32bit(count));
         list.concat(cai_align.create(const_align(sizeof(pint))));
         if assigned(root) then
           begin
              writeintentry(list,root);
              disposeprocdeftree(root);
           end;
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
              current_asmdata.getdatalabel(r);
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


    procedure TVMTWriter.do_gen_published_methods(p:TObject;arg:pointer);
      var
        i  : longint;
        l  : tasmlabel;
        pd : tprocdef;
        lists: ^TAsmList absolute arg;
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
                lists[1].concat(cai_align.Create(const_align(sizeof(pint))));
                lists[1].concat(Tai_label.Create(l));
                lists[1].concat(Tai_const.Create_8bit(length(tsym(p).realname)));
                lists[1].concat(Tai_string.Create(tsym(p).realname));

                lists[0].concat(Tai_const.Create_sym(l));
                if po_abstractmethod in pd.procoptions then
                  lists[0].concat(Tai_const.Create_nil_codeptr)
                else
                  lists[0].concat(Tai_const.Createname(pd.mangledname,AT_FUNCTION,0));
              end;
           end;
      end;


    function TVMTWriter.genpublishedmethodstable(list : TAsmList) : tasmlabel;

      var
         l : tasmlabel;
         count : longint;
         lists : array[0..1] of TAsmList;
      begin
         count:=0;
         _class.symtable.SymList.ForEachCall(@do_count_published_methods,@count);
         if count>0 then
           begin
              lists[0]:=list;
              lists[1]:=TAsmList.Create;
              current_asmdata.getlabel(l,alt_data);
              list.concat(cai_align.create(const_align(sizeof(pint))));
              list.concat(Tai_label.Create(l));
              list.concat(Tai_const.Create_32bit(count));
              _class.symtable.SymList.ForEachCall(@do_gen_published_methods,@lists);
              list.concatlist(lists[1]);
              lists[1].Free;
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

            list.concat(cai_align.create(const_align(sizeof(pint))));
            { write fields }
            list.concat(Tai_label.Create(fieldtable));
            list.concat(Tai_const.Create_16bit(fieldcount));
            if (tf_requires_proper_alignment in target_info.flags) then
              list.concat(cai_align.Create(sizeof(TConstPtrUInt)));
            list.concat(Tai_const.Create_sym(classtable));
            for i:=0 to _class.symtable.SymList.Count-1 do
              begin
                sym:=tsym(_class.symtable.SymList[i]);
                if (sym.typ=fieldvarsym) and
                  (sym.visibility=vis_published) then
                  begin
                    if (tf_requires_proper_alignment in target_info.flags) then
                      list.concat(cai_align.Create(sizeof(pint)));
                    list.concat(Tai_const.Create_pint(tfieldvarsym(sym).fieldoffset));
                    classindex:=classtablelist.IndexOf(tfieldvarsym(sym).vardef);
                    if classindex=-1 then
                      internalerror(200611033);
                    list.concat(Tai_const.Create_16bit(classindex+1));
                    list.concat(Tai_const.Create_8bit(length(tfieldvarsym(sym).realname)));
                    list.concat(Tai_string.Create(tfieldvarsym(sym).realname));
                  end;
              end;

            { generate the class table }
            list.concat(cai_align.create(const_align(sizeof(pint))));
            list.concat(Tai_label.Create(classtable));
            list.concat(Tai_const.Create_16bit(classtablelist.count));
            if (tf_requires_proper_alignment in target_info.flags) then
              list.concat(cai_align.Create(sizeof(TConstPtrUInt)));
            for i:=0 to classtablelist.Count-1 do
              list.concat(Tai_const.Createname(tobjectdef(classtablelist[i]).vmt_mangledname,AT_DATA,0));
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


    procedure TVMTWriter.intf_gen_intf_ref(rawdata: TAsmList;AImplIntf:TImplementedInterface);
      var
        pd: tprocdef;
      begin
        { GUID (or nil for Corba interfaces) }
        if AImplIntf.IntfDef.objecttype in [odt_interfacecom] then
          rawdata.concat(Tai_const.CreateName(
            make_mangledname('IID',AImplIntf.IntfDef.owner,AImplIntf.IntfDef.objname^),AT_DATA,0))
        else
          rawdata.concat(Tai_const.Create_nil_dataptr);

        { VTable }
        rawdata.concat(Tai_const.Createname(intf_get_vtbl_name(AImplIntf.VtblImplIntf),AT_DATA,0));
        { IOffset field }
        case AImplIntf.VtblImplIntf.IType of
          etFieldValue, etFieldValueClass,
          etStandard:
            rawdata.concat(Tai_const.Create_pint(AImplIntf.VtblImplIntf.IOffset));
          etStaticMethodResult, etStaticMethodClass:
            rawdata.concat(Tai_const.Createname(
              tprocdef(tpropertysym(AImplIntf.ImplementsGetter).propaccesslist[palt_read].procdef).mangledname,
              AT_FUNCTION,
              0
            ));
          etVirtualMethodResult, etVirtualMethodClass:
            begin
              pd := tprocdef(tpropertysym(AImplIntf.ImplementsGetter).propaccesslist[palt_read].procdef);
              rawdata.concat(Tai_const.Create_pint(tobjectdef(pd.struct).vmtmethodoffset(pd.extnumber)));
            end;
          else
            internalerror(200802162);
        end;

        { IIDStr }
        rawdata.concat(Tai_const.CreateName(
          make_mangledname('IIDSTR',AImplIntf.IntfDef.owner,AImplIntf.IntfDef.objname^),AT_DATA,0));
        { IType }
        rawdata.concat(Tai_const.Create_pint(aint(AImplIntf.VtblImplIntf.IType)));
      end;


    function TVMTWriter.intf_write_table(list : TAsmList):TAsmLabel;
      var
        i        : longint;
        ImplIntf : TImplementedInterface;
      begin
        current_asmdata.getlabel(result,alt_data);
        list.concat(cai_align.create(const_align(sizeof(pint))));
        list.concat(Tai_label.Create(result));
        list.concat(Tai_const.Create_pint(_class.ImplementedInterfaces.count));
        { Write vtbl references }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            intf_gen_intf_ref(list,ImplIntf);
          end;

        { Write vtbls }
        for i:=0 to _class.ImplementedInterfaces.count-1 do
          begin
            ImplIntf:=TImplementedInterface(_class.ImplementedInterfaces[i]);
            if ImplIntf.VtblImplIntf=ImplIntf then
              intf_create_vtbl(list,ImplIntf);
          end;
      end;


  { Write interface identifiers to the data section }
  procedure TVMTWriter.writeinterfaceids(list: TAsmList);
    var
      i : longint;
      s : string;
    begin
      if assigned(_class.iidguid) then
        begin
          s:=make_mangledname('IID',_class.owner,_class.objname^);
          maybe_new_object_file(list);
          new_section(list,sec_rodata_norel,s,const_align(sizeof(pint)));
          list.concat(Tai_symbol.Createname_global(s,AT_DATA,0));
          list.concat(Tai_const.Create_32bit(longint(_class.iidguid^.D1)));
          list.concat(Tai_const.Create_16bit(_class.iidguid^.D2));
          list.concat(Tai_const.Create_16bit(_class.iidguid^.D3));
          for i:=Low(_class.iidguid^.D4) to High(_class.iidguid^.D4) do
            list.concat(Tai_const.Create_8bit(_class.iidguid^.D4[i]));
        end;
      maybe_new_object_file(list);
      s:=make_mangledname('IIDSTR',_class.owner,_class.objname^);
      new_section(list,sec_rodata_norel,s,sizeof(pint));
      list.concat(Tai_symbol.Createname_global(s,AT_DATA,0));
      list.concat(Tai_const.Create_8bit(length(_class.iidstr^)));
      list.concat(Tai_string.Create(_class.iidstr^));
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
        cg.g_external_wrapper(list,pd,'FPC_ABSTRACTERROR');
        list.concat(Tai_symbol_end.Create(sym));
      end;


    procedure TVMTWriter.writevirtualmethods(List:TAsmList);
      var
         vmtpd : tprocdef;
         vmtentry : pvmtentry;
         i  : longint;
         procname : string;
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
      begin
{$ifdef WITHDMT}
         dmtlabel:=gendmt;
{$endif WITHDMT}
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
            templist.concat(cai_align.create(const_align(sizeof(pint))));
            templist.concat(Tai_label.Create(classnamelabel));
            hs:=_class.RttiName;
            templist.concat(Tai_const.Create_8bit(length(hs)));
            templist.concat(Tai_string.Create(hs));

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
                    cg.g_intf_wrapper(list,pd,tmps,ImplIntf.ioffset);
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
