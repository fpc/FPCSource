{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Does object types for Free Pascal

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
unit pdecobj;

{$i defines.inc}

interface

    uses
      globtype,symtype,symdef;

    { parses a object declaration }
    function object_dec(const n : stringid;fd : pobjectdef) : pdef;

implementation

    uses
      cutils,cobjects,
      globals,verbose,systems,tokens,
      aasm,symconst,symbase,symsym,symtable,types,
{$ifdef GDB}
      gdb,
{$endif}
      hcodegen,hcgdata,
      node,nld,ncon,ncnv,pass_1,
      scanner,
      pbase,pexpr,pdecl,psub,pdecsub,pdecvar,ptype;

    function object_dec(const n : stringid;fd : pobjectdef) : pdef;
    { this function parses an object or class declaration }
      var
         actmembertype : tsymoptions;
         there_is_a_destructor : boolean;
         classtype : tobjectdeftype;
         childof : pobjectdef;
         aktclass : pobjectdef;

      procedure constructor_head;

        begin
           consume(_CONSTRUCTOR);
           { must be at same level as in implementation }
           inc(lexlevel);
           parse_proc_head(potype_constructor);
           dec(lexlevel);

           if (cs_constructor_name in aktglobalswitches) and (aktprocsym^.name<>'INIT') then
            Message(parser_e_constructorname_must_be_init);

           include(aktclass^.objectoptions,oo_has_constructor);
           consume(_SEMICOLON);
             begin
                if is_class(aktclass) then
                  begin
                     { CLASS constructors return the created instance }
                     aktprocsym^.definition^.rettype.def:=aktclass;
                  end
                else
                  begin
                     { OBJECT constructors return a boolean }
                     aktprocsym^.definition^.rettype.setdef(booldef);
                  end;
             end;
        end;


      procedure property_dec;

        var
           sym : psym;
           propertyparas : plinkedlist;

        { returns the matching procedure to access a property }
        function get_procdef : pprocdef;

          var
             p : pprocdef;

          begin
             p:=pprocsym(sym)^.definition;
             get_procdef:=nil;
             while assigned(p) do
               begin
                  if equal_paras(p^.para,propertyparas,cp_value_equal_const) then
                    break;
                  p:=p^.nextoverloaded;
               end;
             get_procdef:=p;
          end;

        var
           hp2,datacoll : pparaitem;
           p : ppropertysym;
           overriden : psym;
           hs : string;
           varspez : tvarspez;
           sc : pstringcontainer;
           s : string;
           tt : ttype;
           declarepos : tfileposinfo;
           pp : pprocdef;
           pt : tnode;
           propname : stringid;

        begin
           { check for a class }
           if not(is_class(aktclass)) then
            Message(parser_e_syntax_error);
           consume(_PROPERTY);
           new(propertyparas,init);
           datacoll:=nil;
           if token=_ID then
             begin
                p:=new(ppropertysym,init(orgpattern));
                propname:=pattern;
                consume(_ID);
                { property parameters ? }
                if token=_LECKKLAMMER then
                  begin
                     if (sp_published in current_object_option) then
                       Message(parser_e_cant_publish_that_property);

                     { create a list of the parameters in propertyparas }
                     consume(_LECKKLAMMER);
                     inc(testcurobject);
                     repeat
                       if token=_VAR then
                         begin
                            consume(_VAR);
                            varspez:=vs_var;
                         end
                       else if token=_CONST then
                         begin
                            consume(_CONST);
                            varspez:=vs_const;
                         end
                       else if token=_OUT then
                         begin
                            consume(_OUT);
                            varspez:=vs_out;
                         end
                       else varspez:=vs_value;
                       sc:=idlist;
{$ifdef fixLeaksOnError}
                       strContStack.push(sc);
{$endif fixLeaksOnError}
                       if token=_COLON then
                         begin
                            consume(_COLON);
                            if token=_ARRAY then
                              begin
                                 {
                                 if (varspez<>vs_const) and
                                   (varspez<>vs_var) then
                                   begin
                                      varspez:=vs_const;
                                      Message(parser_e_illegal_open_parameter);
                                   end;
                                 }
                                 consume(_ARRAY);
                                 consume(_OF);
                                 { define range and type of range }
                                 tt.setdef(new(parraydef,init(0,-1,s32bitdef)));
                                 { define field type }
                                 single_type(parraydef(tt.def)^.elementtype,s,false);
                              end
                            else
                              single_type(tt,s,false);
                         end
                       else
                         tt.setdef(cformaldef);
                       repeat
                         s:=sc^.get_with_tokeninfo(declarepos);
                         if s='' then
                          break;
                         new(hp2,init);
                         hp2^.paratyp:=varspez;
                         hp2^.paratype:=tt;
                         propertyparas^.insert(hp2);
                       until false;
{$ifdef fixLeaksOnError}
                       if strContStack.pop <> sc then
                         writeln('problem with strContStack in ptype');
{$endif fixLeaksOnError}
                       dispose(sc,done);
                     until not try_to_consume(_SEMICOLON);
                     dec(testcurobject);
                     consume(_RECKKLAMMER);
                  end;
                { overriden property ?                                 }
                { force property interface, if there is a property parameter }
                if (token=_COLON) or not(propertyparas^.empty) then
                  begin
                     consume(_COLON);
                     single_type(p^.proptype,hs,false);
                     if (idtoken=_INDEX) then
                       begin
                          consume(_INDEX);
                          pt:=comp_expr(true);
                          do_firstpass(pt);
                          if is_ordinal(pt.resulttype) and
                             (not is_64bitint(pt.resulttype)) then
                            p^.index:=tordconstnode(pt).value
                          else
                            begin
                              Message(parser_e_invalid_property_index_value);
                              p^.index:=0;
                            end;
                          p^.indextype.setdef(pt.resulttype);
                          include(p^.propoptions,ppo_indexed);
                          { concat a longint to the para template }
                          new(hp2,init);
                          hp2^.paratyp:=vs_value;
                          hp2^.paratype:=p^.indextype;
                          propertyparas^.insert(hp2);
                          pt.free;
                       end;
                     { the parser need to know if a property has parameters }
                     if not(propertyparas^.empty) then
                       include(p^.propoptions,ppo_hasparameters);
                  end
                else
                  begin
                     { do an property override }
                     overriden:=search_class_member(aktclass,propname);
                     if assigned(overriden) and (overriden^.typ=propertysym) then
                       begin
                         p^.dooverride(ppropertysym(overriden));
                       end
                     else
                       begin
                         p^.proptype.setdef(generrordef);
                         message(parser_e_no_property_found_to_override);
                       end;
                  end;
                if (sp_published in current_object_option) and
                   not(p^.proptype.def^.is_publishable) then
                  Message(parser_e_cant_publish_that_property);

                { create data defcoll to allow correct parameter checks }
                new(datacoll,init);
                datacoll^.paratyp:=vs_value;
                datacoll^.paratype:=p^.proptype;

                if (idtoken=_READ) then
                  begin
                     p^.readaccess^.clear;
                     consume(_READ);
                     sym:=search_class_member(aktclass,pattern);
                     if not(assigned(sym)) then
                       begin
                         Message1(sym_e_unknown_id,pattern);
                         consume(_ID);
                       end
                     else
                       begin
                          consume(_ID);
                          while (token=_POINT) and
                                ((sym^.typ=varsym) and
                                 (pvarsym(sym)^.vartype.def^.deftype=recorddef)) do
                           begin
                             p^.readaccess^.addsym(sym);
                             consume(_POINT);
                             getsymonlyin(precorddef(pvarsym(sym)^.vartype.def)^.symtable,pattern);
                             if not assigned(srsym) then
                               Message1(sym_e_illegal_field,pattern);
                             sym:=srsym;
                             consume(_ID);
                           end;
                       end;

                     if assigned(sym) then
                       begin
                          { search the matching definition }
                          case sym^.typ of
                            procsym :
                              begin
                                 pp:=get_procdef;
                                 if not(assigned(pp)) or
                                    not(is_equal(pp^.rettype.def,p^.proptype.def)) then
                                   Message(parser_e_ill_property_access_sym);
                                 p^.readaccess^.setdef(pp);
                              end;
                            varsym :
                              begin
                                if not(propertyparas^.empty) or
                                   not(is_equal(pvarsym(sym)^.vartype.def,p^.proptype.def)) then
                                  Message(parser_e_ill_property_access_sym);
                              end;
                            else
                              Message(parser_e_ill_property_access_sym);
                          end;
                          p^.readaccess^.addsym(sym);
                       end;
                  end;
                if (idtoken=_WRITE) then
                  begin
                     p^.writeaccess^.clear;
                     consume(_WRITE);
                     sym:=search_class_member(aktclass,pattern);
                     if not(assigned(sym)) then
                       begin
                         Message1(sym_e_unknown_id,pattern);
                         consume(_ID);
                       end
                     else
                       begin
                          consume(_ID);
                          while (token=_POINT) and
                                ((sym^.typ=varsym) and
                                 (pvarsym(sym)^.vartype.def^.deftype=recorddef)) do
                           begin
                             p^.writeaccess^.addsym(sym);
                             consume(_POINT);
                             getsymonlyin(precorddef(pvarsym(sym)^.vartype.def)^.symtable,pattern);
                             if not assigned(srsym) then
                               Message1(sym_e_illegal_field,pattern);
                             sym:=srsym;
                             consume(_ID);
                           end;
                       end;

                     if assigned(sym) then
                       begin
                          { search the matching definition }
                          case sym^.typ of
                            procsym :
                              begin
                                 { insert data entry to check access method }
                                 propertyparas^.insert(datacoll);
                                 pp:=get_procdef;
                                 { ... and remove it }
                                 propertyparas^.remove(datacoll);
                                 if not(assigned(pp)) then
                                   Message(parser_e_ill_property_access_sym);
                                 p^.writeaccess^.setdef(pp);
                              end;
                            varsym :
                              begin
                                 if not(propertyparas^.empty) or
                                    not(is_equal(pvarsym(sym)^.vartype.def,p^.proptype.def)) then
                                   Message(parser_e_ill_property_access_sym);
                              end
                            else
                              Message(parser_e_ill_property_access_sym);
                          end;
                          p^.writeaccess^.addsym(sym);
                       end;
                  end;
                include(p^.propoptions,ppo_stored);
                if (idtoken=_STORED) then
                  begin
                     consume(_STORED);
                     p^.storedaccess^.clear;
                     case token of
                        _ID:
                           { in the case that idtoken=_DEFAULT }
                           { we have to do nothing except      }
                           { setting ppo_stored, it's the same }
                           { as stored true                    }
                           if idtoken<>_DEFAULT then
                             begin
                                sym:=search_class_member(aktclass,pattern);
                                if not(assigned(sym)) then
                                  begin
                                    Message1(sym_e_unknown_id,pattern);
                                    consume(_ID);
                                  end
                                else
                                  begin
                                     consume(_ID);
                                     while (token=_POINT) and
                                           ((sym^.typ=varsym) and
                                            (pvarsym(sym)^.vartype.def^.deftype=recorddef)) do
                                      begin
                                        p^.storedaccess^.addsym(sym);
                                        consume(_POINT);
                                        getsymonlyin(precorddef(pvarsym(sym)^.vartype.def)^.symtable,pattern);
                                        if not assigned(srsym) then
                                          Message1(sym_e_illegal_field,pattern);
                                        sym:=srsym;
                                        consume(_ID);
                                      end;
                                  end;

                                if assigned(sym) then
                                  begin
                                     { only non array properties can be stored }
                                     case sym^.typ of
                                       procsym :
                                         begin
                                           pp:=pprocsym(sym)^.definition;
                                           while assigned(pp) do
                                             begin
                                                { the stored function shouldn't have any parameters }
                                                if pp^.para^.empty then
                                                  break;
                                                 pp:=pp^.nextoverloaded;
                                             end;
                                           { found we a procedure and does it really return a bool? }
                                           if not(assigned(pp)) or
                                              not(is_equal(pp^.rettype.def,booldef)) then
                                             Message(parser_e_ill_property_storage_sym);
                                           p^.storedaccess^.setdef(pp);
                                         end;
                                       varsym :
                                         begin
                                           if not(propertyparas^.empty) or
                                              not(is_equal(pvarsym(sym)^.vartype.def,booldef)) then
                                             Message(parser_e_stored_property_must_be_boolean);
                                         end;
                                       else
                                         Message(parser_e_ill_property_storage_sym);
                                     end;
                                     p^.storedaccess^.addsym(sym);
                                  end;
                             end;
                        _FALSE:
                          begin
                             consume(_FALSE);
                             exclude(p^.propoptions,ppo_stored);
                          end;
                        _TRUE:
                          consume(_TRUE);
                     end;
                  end;
                if (idtoken=_DEFAULT) then
                  begin
                     consume(_DEFAULT);
                     if not(is_ordinal(p^.proptype.def) or
                            is_64bitint(p^.proptype.def) or
                            ((p^.proptype.def^.deftype=setdef) and
                             (psetdef(p^.proptype.def)^.settype=smallset))) or
                        not(propertyparas^.empty) then
                       Message(parser_e_property_cant_have_a_default_value);
                     { Get the result of the default, the firstpass is
                       needed to support values like -1 }
                     pt:=comp_expr(true);
                     do_firstpass(pt);
                     if (p^.proptype.def^.deftype=setdef) and
                        (pt.nodetype=arrayconstructorn) then
                       begin
                         arrayconstructor_to_set(tarrayconstructornode(pt));
                         do_firstpass(pt);
                       end;
                     pt:=gentypeconvnode(pt,p^.proptype.def);
                     do_firstpass(pt);
                     if not(is_constnode(pt)) then
                       Message(parser_e_property_default_value_must_const);

                     if pt.nodetype=setconstn then
                       p^.default:=plongint(tsetconstnode(pt).value_set)^
                     else
                       p^.default:=tordconstnode(pt).value;
                     pt.free;
                  end
                else if (idtoken=_NODEFAULT) then
                  begin
                     consume(_NODEFAULT);
                     p^.default:=0;
                  end;
                symtablestack^.insert(p);
                { default property ? }
                consume(_SEMICOLON);
                if (idtoken=_DEFAULT) then
                  begin
                     consume(_DEFAULT);
                     { overriding a default propertyp is allowed
                     p2:=search_default_property(aktclass);
                     if assigned(p2) then
                       message1(parser_e_only_one_default_property,
                         pobjectdef(p2^.owner^.defowner)^.objname^)
                     else
                     }
                       begin
                          include(p^.propoptions,ppo_defaultproperty);
                          if propertyparas^.empty then
                            message(parser_e_property_need_paras);
                       end;
                     consume(_SEMICOLON);
                  end;
                { clean up }
                if assigned(datacoll) then
                  dispose(datacoll,done);
             end
           else
             begin
                consume(_ID);
                consume(_SEMICOLON);
             end;
           dispose(propertyparas,done);
        end;


      procedure destructor_head;
        begin
           consume(_DESTRUCTOR);
           inc(lexlevel);
           parse_proc_head(potype_destructor);
           dec(lexlevel);
           if (cs_constructor_name in aktglobalswitches) and (aktprocsym^.name<>'DONE') then
            Message(parser_e_destructorname_must_be_done);
           include(aktclass^.objectoptions,oo_has_destructor);
           consume(_SEMICOLON);
           if not(aktprocsym^.definition^.para^.empty) then
             if not (m_tp in aktmodeswitches) then
               Message(parser_e_no_paras_for_destructor);
           { no return value }
           aktprocsym^.definition^.rettype.def:=voiddef;
        end;

      var
         hs      : string;
         pcrd       : pclassrefdef;
         tt     : ttype;
         oldprocinfo : pprocinfo;
         oldprocsym : pprocsym;
         oldparse_only : boolean;
         methodnametable,intmessagetable,
         strmessagetable,classnamelabel,
         fieldtablelabel : pasmlabel;
         storetypecanbeforward : boolean;

      procedure setclassattributes;

        begin
           if classtype=odt_class then
             begin
                aktclass^.objecttype:=odt_class;
                if (cs_generate_rtti in aktlocalswitches) or
                    (assigned(aktclass^.childof) and
                     (oo_can_have_published in aktclass^.childof^.objectoptions)) then
                  begin
                     include(aktclass^.objectoptions,oo_can_have_published);
                     { in "publishable" classes the default access type is published }
                     actmembertype:=[sp_published];
                     { don't know if this is necessary (FK) }
                     current_object_option:=[sp_published];
                  end;
             end;
        end;

     procedure setclassparent;

        begin
           if assigned(fd) then
             aktclass:=fd
           else
             aktclass:=new(pobjectdef,init(classtype,n,nil));
           { is the current class tobject?   }
           { so you could define your own tobject }
           if (cs_compilesystem in aktmoduleswitches) and (classtype=odt_class) and (upper(n)='TOBJECT') then
             class_tobject:=aktclass
           else if (cs_compilesystem in aktmoduleswitches) and (classtype=odt_interfacecom) and (upper(n)='IUNKNOWN') then
             interface_iunknown:=aktclass
           else
             begin
                case classtype of
                  odt_class:
                    childof:=class_tobject;
                  odt_interfacecom:
                    childof:=interface_iunknown;
                end;
                if (oo_is_forward in childof^.objectoptions) then
                  Message1(parser_e_forward_declaration_must_be_resolved,childof^.objname^);
                aktclass^.set_parent(childof);
             end;
         end;

      { generates the vmt for classes as well as for objects }
      procedure writevmt;

        var
           vmtlist : taasmoutput;
{$ifdef WITHDMT}
           dmtlabel : pasmlabel;
{$endif WITHDMT}
           interfacetable : pasmlabel;

        begin
{$ifdef WITHDMT}
           dmtlabel:=gendmt(aktclass);
{$endif WITHDMT}
           { this generates the entries }
           vmtlist.init;
           genvmt(@vmtlist,aktclass);

           { write tables for classes, this must be done before the actual
             class is written, because we need the labels defined }
           if classtype=odt_class then
            begin
              methodnametable:=genpublishedmethodstable(aktclass);
              fieldtablelabel:=aktclass^.generate_field_table;
              { rtti }
              if (oo_can_have_published in aktclass^.objectoptions) then
               aktclass^.generate_rtti;
              { write class name }
              getdatalabel(classnamelabel);
              datasegment^.concat(new(pai_label,init(classnamelabel)));
              datasegment^.concat(new(pai_const,init_8bit(length(aktclass^.objname^))));
              datasegment^.concat(new(pai_string,init(aktclass^.objname^)));
              { generate message and dynamic tables }
              if (oo_has_msgstr in aktclass^.objectoptions) then
                strmessagetable:=genstrmsgtab(aktclass);
              if (oo_has_msgint in aktclass^.objectoptions) then
                intmessagetable:=genintmsgtab(aktclass)
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));
              if aktclass^.implementedinterfaces^.count>0 then
                interfacetable:=genintftable(aktclass);
            end;

          { write debug info }
{$ifdef GDB}
          if (cs_debuginfo in aktmoduleswitches) then
           begin
             do_count_dbx:=true;
             if assigned(aktclass^.owner) and assigned(aktclass^.owner^.name) then
               datasegment^.concat(new(pai_stabs,init(strpnew('"vmt_'+aktclass^.owner^.name^+n+':S'+
                 typeglobalnumber('__vtbl_ptr_type')+'",'+tostr(N_STSYM)+',0,0,'+aktclass^.vmt_mangledname))));
           end;
{$endif GDB}
           datasegment^.concat(new(pai_symbol,initdataname_global(aktclass^.vmt_mangledname,0)));

           { determine the size with symtable^.datasize, because }
           { size gives back 4 for classes                    }
           datasegment^.concat(new(pai_const,init_32bit(aktclass^.symtable^.datasize)));
           datasegment^.concat(new(pai_const,init_32bit(-aktclass^.symtable^.datasize)));
{$ifdef WITHDMT}
           if classtype=ct_object then
             begin
                if assigned(dmtlabel) then
                  datasegment^.concat(new(pai_const_symbol,init(dmtlabel)))
                else
                  datasegment^.concat(new(pai_const,init_32bit(0)));
             end;
{$endif WITHDMT}
           { write pointer to parent VMT, this isn't implemented in TP }
           { but this is not used in FPC ? (PM) }
           { it's not used yet, but the delphi-operators as and is need it (FK) }
           { it is not written for parents that don't have any vmt !! }
           if assigned(aktclass^.childof) and
              (oo_has_vmt in aktclass^.childof^.objectoptions) then
             datasegment^.concat(new(pai_const_symbol,initname(aktclass^.childof^.vmt_mangledname)))
           else
             datasegment^.concat(new(pai_const,init_32bit(0)));

           { write extended info for classes, for the order see rtl/inc/objpash.inc }
           if classtype=odt_class then
            begin
              { pointer to class name string }
              datasegment^.concat(new(pai_const_symbol,init(classnamelabel)));
              { pointer to dynamic table }
              if (oo_has_msgint in aktclass^.objectoptions) then
                datasegment^.concat(new(pai_const_symbol,init(intmessagetable)))
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));
              { pointer to method table }
              if assigned(methodnametable) then
                datasegment^.concat(new(pai_const_symbol,init(methodnametable)))
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));
              { pointer to field table }
              datasegment^.concat(new(pai_const_symbol,init(fieldtablelabel)));
              { pointer to type info of published section }
              if (oo_can_have_published in aktclass^.objectoptions) then
                datasegment^.concat(new(pai_const_symbol,initname(aktclass^.rtti_name)))
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));
              { inittable for con-/destruction }
              {
              if aktclass^.needs_inittable then
              }
              { we generate the init table for classes always, because needs_inittable }
              { for classes is always false, it applies only for objects               }
              datasegment^.concat(new(pai_const_symbol,init(aktclass^.get_inittable_label)));
              {
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));
              }
              { auto table }
              datasegment^.concat(new(pai_const,init_32bit(0)));
              { interface table }
              if aktclass^.implementedinterfaces^.count>0 then
                datasegment^.concat(new(pai_const_symbol,init(interfacetable)))
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));
              { table for string messages }
              if (oo_has_msgstr in aktclass^.objectoptions) then
                datasegment^.concat(new(pai_const_symbol,init(strmessagetable)))
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));
            end;
           datasegment^.concatlist(@vmtlist);
           vmtlist.done;
           { write the size of the VMT }
           datasegment^.concat(new(pai_symbol_end,initname(aktclass^.vmt_mangledname)));
        end;

      procedure setinterfacemethodoptions;

        var
          i: longint;
          defs: pindexarray;
          pd: pprocdef;
        begin
          include(aktclass^.objectoptions,oo_has_virtual);
          defs:=aktclass^.symtable^.defindex;
          for i:=1 to defs^.count do
            begin
              pd:=pprocdef(defs^.search(i));
              if pd^.deftype=procdef then
                begin
                  pd^.extnumber:=aktclass^.lastvtableindex;
                  inc(aktclass^.lastvtableindex);
                  include(pd^.procoptions,po_virtualmethod);
                  pd^.forwarddef:=false;
                end;
            end;
        end;

      function readobjecttype : boolean;

        begin
           readobjecttype:=true;
           { distinguish classes and objects }
           case token of
              _OBJECT:
                begin
                   classtype:=odt_object;
                   consume(_OBJECT)
                end;
              _CPPCLASS:
                begin
                   classtype:=odt_cppclass;
                   consume(_CPPCLASS);
                end;
              _INTERFACE:
                begin
                   if aktinterfacetype=it_interfacecom then
                     classtype:=odt_interfacecom
                   else {it_interfacecorba}
                     classtype:=odt_interfacecorba;
                   consume(_INTERFACE);
                   { forward declaration }
                   if not(assigned(fd)) and (token=_SEMICOLON) then
                     begin
                       { also anonym objects aren't allow (o : object a : longint; end;) }
                       if n='' then
                         Message(parser_f_no_anonym_objects);
                       aktclass:=new(pobjectdef,init(classtype,n,nil));
                       if (cs_compilesystem in aktmoduleswitches) and
                          (classtype=odt_interfacecom) and (upper(n)='IUNKNOWN') then
                         interface_iunknown:=aktclass;
                       aktclass^.objectoptions:=aktclass^.objectoptions+[oo_is_forward];
                     end;
                end;
              _CLASS:
                begin
                   classtype:=odt_class;
                   consume(_CLASS);
                   if not(assigned(fd)) and (token=_OF) then
                     begin
                        { a hack, but it's easy to handle }
                        { class reference type }
                        consume(_OF);
                        single_type(tt,hs,typecanbeforward);

                        { accept hp1, if is a forward def or a class }
                        if (tt.def^.deftype=forwarddef) or
                           is_class(tt.def) then
                          begin
                             pcrd:=new(pclassrefdef,init(tt.def));
                             object_dec:=pcrd;
                          end
                        else
                          begin
                             object_dec:=generrordef;
                             Message1(type_e_class_type_expected,generrordef^.typename);
                          end;
                        typecanbeforward:=storetypecanbeforward;
                        readobjecttype:=false;
                        exit;
                     end
                   { forward class }
                   else if not(assigned(fd)) and (token=_SEMICOLON) then
                     begin
                        { also anonym objects aren't allow (o : object a : longint; end;) }
                        if n='' then
                          Message(parser_f_no_anonym_objects);
                        aktclass:=new(pobjectdef,init(odt_class,n,nil));
                        if (cs_compilesystem in aktmoduleswitches) and (upper(n)='TOBJECT') then
                          class_tobject:=aktclass;
                        aktclass^.objecttype:=odt_class;
                        include(aktclass^.objectoptions,oo_is_forward);
                        { all classes must have a vmt !!  at offset zero }
                        if not(oo_has_vmt in aktclass^.objectoptions) then
                          aktclass^.insertvmt;

                        object_dec:=aktclass;
                        typecanbeforward:=storetypecanbeforward;
                        readobjecttype:=false;
                        exit;
                     end;
                end;
              else
                begin
                   classtype:=odt_class; { this is error but try to recover }
                   consume(_OBJECT);
                end;
           end;
        end;

      procedure readimplementedinterfaces;
        var
          implintf: pobjectdef;
          tt      : ttype;
        begin
          while try_to_consume(_COMMA) do begin
            id_type(tt,pattern,false);
            implintf:=pobjectdef(tt.def);
            if (tt.def^.deftype<>objectdef) then begin
              Message1(type_e_interface_type_expected,tt.def^.typename);
              Continue; { omit }
            end;
            if not is_interface(implintf) then begin
              Message1(type_e_interface_type_expected,implintf^.typename);
              Continue; { omit }
            end;
            if aktclass^.implementedinterfaces^.searchintf(tt.def)<>-1 then
              Message1(sym_e_duplicate_id,tt.def^.name)
            else
              aktclass^.implementedinterfaces^.addintf(tt.def);
          end;
        end;

      procedure readinterfaceiid;
        var
          tt: ttype;
          p : tnode;

        begin
          p:=comp_expr(true);
          do_firstpass(p);
          if p.nodetype=stringconstn then
            begin
              aktclass^.iidstr:=stringdup(strpas(tstringconstnode(p).value_str)); { or upper? }
              p.free;
              aktclass^.isiidguidvalid:=string2guid(aktclass^.iidstr^,aktclass^.iidguid);
              if (classtype=odt_interfacecom) and not aktclass^.isiidguidvalid then
                Message(parser_e_improper_guid_syntax);
            end
          else
            begin
              p.free;
              Message(cg_e_illegal_expression);
            end;
        end;

      procedure readparentclasses;

        begin
           { reads the parent class }
           if token=_LKLAMMER then
             begin
                consume(_LKLAMMER);
                id_type(tt,pattern,false);
                childof:=pobjectdef(tt.def);
                if (not assigned(childof)) or
                   (childof^.deftype<>objectdef) then
                 begin
                   if assigned(childof) then
                    Message1(type_e_class_type_expected,childof^.typename);
                   childof:=nil;
                   aktclass:=new(pobjectdef,init(classtype,n,nil));
                 end
                else
                 begin
                   { a mix of class, interfaces, objects and cppclasses
                     isn't allowed }
                   case classtype of
                      odt_class:
                        if not(is_class(childof)) and
                          not(is_interface(childof)) then
                          Message(parser_e_mix_of_classes_and_objects);
                      odt_interfacecorba,
                      odt_interfacecom:
                        if not(is_interface(childof)) then
                          Message(parser_e_mix_of_classes_and_objects);
                      odt_cppclass:
                        if not(is_cppclass(childof)) then
                          Message(parser_e_mix_of_classes_and_objects);
                      odt_object:
                        if not(is_object(childof)) then
                          Message(parser_e_mix_of_classes_and_objects);
                   end;
                   { the forward of the child must be resolved to get
                     correct field addresses }
                   if assigned(fd) then
                    begin
                      if (oo_is_forward in childof^.objectoptions) then
                       Message1(parser_e_forward_declaration_must_be_resolved,childof^.objname^);
                      aktclass:=fd;
                      { we must inherit several options !!
                        this was missing !!
                        all is now done in set_parent
                        including symtable datasize setting PM }
                      fd^.set_parent(childof);
                    end
                   else
                    aktclass:=new(pobjectdef,init(classtype,n,childof));
                   if aktclass^.objecttype=odt_class then
                    readimplementedinterfaces;
                 end;
                consume(_RKLAMMER);
             end
           { if no parent class, then a class get tobject as parent }
           else if classtype in [odt_class,odt_interfacecom] then
             setclassparent
           else
             aktclass:=new(pobjectdef,init(classtype,n,nil));
           { read GUID }
             if (classtype in [odt_interfacecom,odt_interfacecorba]) and
                try_to_consume(_LECKKLAMMER) then
               begin
                 readinterfaceiid;
                 consume(_RECKKLAMMER);
               end;
        end;

      procedure chkcpp;

        begin
           if is_cppclass(aktclass) then
             begin
                include(aktprocsym^.definition^.proccalloptions,pocall_cppdecl);
                aktprocsym^.definition^.setmangledname(
                  target_os.Cprefix+aktprocsym^.definition^.cplusplusmangledname);
             end;
        end;

      var
        temppd : pprocdef;
      begin
         {Nowadays aktprocsym may already have a value, so we need to save
          it.}
         oldprocsym:=aktprocsym;
         { forward is resolved }
         if assigned(fd) then
           exclude(fd^.objectoptions,oo_is_forward);

         there_is_a_destructor:=false;
         actmembertype:=[sp_public];

         { objects and class types can't be declared local }
         if (symtablestack^.symtabletype<>globalsymtable) and
           (symtablestack^.symtabletype<>staticsymtable) then
           Message(parser_e_no_local_objects);

         storetypecanbeforward:=typecanbeforward;
         { for tp mode don't allow forward types }
         if (m_tp in aktmodeswitches) and
            not (m_delphi in aktmodeswitches) then
           typecanbeforward:=false;

         if not(readobjecttype) then
           exit;

         { also anonym objects aren't allow (o : object a : longint; end;) }
         if n='' then
           Message(parser_f_no_anonym_objects);

         readparentclasses;

         { default access is public }
         actmembertype:=[sp_public];

         { set class flags and inherits published, if necessary? }
         setclassattributes;

         aktobjectdef:=aktclass;
         aktclass^.symtable^.next:=symtablestack;
         symtablestack:=aktclass^.symtable;
         testcurobject:=1;
         curobjectname:=Upper(n);

         { new procinfo }
         oldprocinfo:=procinfo;
         new(procinfo,init);
         procinfo^._class:=aktclass;


         { short class declaration ? }
         if (classtype<>odt_class) or (token<>_SEMICOLON) then
          begin
          { Parse componenten }
            repeat
              if (sp_private in actmembertype) then
                include(aktclass^.objectoptions,oo_has_private);
              if (sp_protected in actmembertype) then
                include(aktclass^.objectoptions,oo_has_protected);
              case token of
              _ID : begin
                      case idtoken of
                       _PRIVATE : begin
                                    consume(_PRIVATE);
                                    actmembertype:=[sp_private];
                                    current_object_option:=[sp_private];
                                  end;
                     _PROTECTED : begin
                                    consume(_PROTECTED);
                                    current_object_option:=[sp_protected];
                                    actmembertype:=[sp_protected];
                                  end;
                        _PUBLIC : begin
                                    consume(_PUBLIC);
                                    current_object_option:=[sp_public];
                                    actmembertype:=[sp_public];
                                  end;
                     _PUBLISHED : begin
                                    if not(oo_can_have_published in aktclass^.objectoptions) then
                                     Message(parser_e_cant_have_published);
                                    consume(_PUBLISHED);
                                    current_object_option:=[sp_published];
                                    actmembertype:=[sp_published];
                                  end;
                      else
                        read_var_decs(false,true,false);
                      end;
                    end;
        _PROPERTY : property_dec;
       _PROCEDURE,
        _FUNCTION,
           _CLASS : begin
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      parse_proc_dec;
{$ifndef newcg}
                      parse_object_proc_directives(aktprocsym);
{$endif newcg}
                      { check if there are duplicates }
                      check_identical_proc(temppd);
                      if (po_msgint in aktprocsym^.definition^.procoptions) then
                        include(aktclass^.objectoptions,oo_has_msgint);

                      if (po_msgstr in aktprocsym^.definition^.procoptions) then
                        include(aktclass^.objectoptions,oo_has_msgstr);

                      if (po_virtualmethod in aktprocsym^.definition^.procoptions) then
                        include(aktclass^.objectoptions,oo_has_virtual);

                      chkcpp;

                      parse_only:=oldparse_only;
                    end;
     _CONSTRUCTOR : begin
                      if not(sp_public in actmembertype) then
                        Message(parser_w_constructor_should_be_public);
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      constructor_head;
{$ifndef newcg}
                      parse_object_proc_directives(aktprocsym);
{$endif newcg}
                      if (po_virtualmethod in aktprocsym^.definition^.procoptions) then
                        include(aktclass^.objectoptions,oo_has_virtual);

                      chkcpp;

                      parse_only:=oldparse_only;
                    end;
      _DESTRUCTOR : begin
                      if there_is_a_destructor then
                        Message(parser_n_only_one_destructor);
                      there_is_a_destructor:=true;
                      if not(sp_public in actmembertype) then
                        Message(parser_w_destructor_should_be_public);
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      destructor_head;
{$ifndef newcg}
                      parse_object_proc_directives(aktprocsym);
{$endif newcg}
                      if (po_virtualmethod in aktprocsym^.definition^.procoptions) then
                        include(aktclass^.objectoptions,oo_has_virtual);

                      chkcpp;

                      parse_only:=oldparse_only;
                    end;
             _END : begin
                      consume(_END);
                      break;
                    end;
              else
               consume(_ID); { Give a ident expected message, like tp7 }
              end;
            until false;
            current_object_option:=[sp_public];
          end;
         testcurobject:=0;
         curobjectname:='';
         typecanbeforward:=storetypecanbeforward;

         { generate vmt space if needed }
         if not(oo_has_vmt in aktclass^.objectoptions) and
            (([oo_has_virtual,oo_has_constructor,oo_has_destructor]*aktclass^.objectoptions<>[]) or
             (classtype in [odt_class])
            ) then
           aktclass^.insertvmt;
         if (cs_create_smart in aktmoduleswitches) then
           datasegment^.concat(new(pai_cut,init));

         if is_interface(aktclass) then
           writeinterfaceids(aktclass);

         if (oo_has_vmt in aktclass^.objectoptions) then
           writevmt;

         if is_interface(aktclass) then
           setinterfacemethodoptions;

         { restore old state }
         symtablestack:=symtablestack^.next;
         aktobjectdef:=nil;
         {Restore procinfo}
         dispose(procinfo,done);
         procinfo:=oldprocinfo;
         {Restore the aktprocsym.}
         aktprocsym:=oldprocsym;

         object_dec:=aktclass;
      end;

end.
{
  $Log$
  Revision 1.11  2000-11-12 23:24:11  florian
    * interfaces are basically running

  Revision 1.10  2000/11/12 22:17:47  peter
    * some realname updates for messages

  Revision 1.9  2000/11/06 23:05:52  florian
    * more fixes

  Revision 1.8  2000/11/06 20:30:55  peter
    * more fixes to get make cycle working

  Revision 1.7  2000/11/04 18:03:57  florian
    * fixed upper/lower case problem

  Revision 1.6  2000/11/04 17:31:00  florian
    * fixed some problems of previous commit

  Revision 1.5  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.4  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/26 21:54:03  peter
    * fixed crash with error in child definition (merged)

  Revision 1.2  2000/10/21 18:16:11  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.1  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

}
