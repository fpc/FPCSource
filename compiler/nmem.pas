{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Type checking and register allocation for memory related nodes

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
unit nmem;

{$i fpcdefs.inc}

interface

    uses
       node,
       symdef,symsym,symtable,symtype;

    type
tloadvmtaddrnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
       end;
       tloadvmtaddrnodeclass = class of tloadvmtaddrnode;

       tloadparentfpnode = class(tunarynode)
          parentpd : tprocdef;
          parentpdderef : tderef;
          constructor create(pd:tprocdef);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function docompare(p: tnode): boolean; override;
          function dogetcopy : tnode;override;
       end;
       tloadparentfpnodeclass = class of tloadparentfpnode;

       taddrnode = class(tunarynode)
          getprocvardef : tprocvardef;
          getprocvardefderef : tderef;
          constructor create(l : tnode);virtual;
          constructor create_internal(l : tnode); virtual;
          constructor create_internal_nomark(l : tnode); virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure mark_write;override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function docompare(p: tnode): boolean; override;
          function dogetcopy : tnode;override;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
         private
          mark_read_written: boolean;
       end;
       taddrnodeclass = class of taddrnode;

       tderefnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          procedure mark_write;override;
       end;
       tderefnodeclass = class of tderefnode;

       tsubscriptnode = class(tunarynode)
          vs : tfieldvarsym;
          vsderef : tderef;
          constructor create(varsym : tsym;l : tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function dogetcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function pass_typecheck:tnode;override;
          procedure mark_write;override;
       end;
       tsubscriptnodeclass = class of tsubscriptnode;

       tvecnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          procedure mark_write;override;
       end;
       tvecnodeclass = class of tvecnode;

       twithnode = class(tunarynode)
          constructor create(l:tnode);
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function dogetcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function pass_typecheck:tnode;override;
       end;
       twithnodeclass = class of twithnode;

    var
       cloadvmtaddrnode : tloadvmtaddrnodeclass;
       cloadparentfpnode : tloadparentfpnodeclass;
       caddrnode : taddrnodeclass;
       cderefnode : tderefnodeclass;
       csubscriptnode : tsubscriptnodeclass;
       cvecnode : tvecnodeclass;
       cwithnode : twithnodeclass;

    function is_big_untyped_addrnode(p: tnode): boolean;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symbase,defutil,defcmp,
      nbas,nutils,
      htypechk,pass_1,ncal,nld,ncon,ncnv,cgbase,procinfo
      ;

{*****************************************************************************
                            TLOADVMTADDRNODE
*****************************************************************************}

    constructor tloadvmtaddrnode.create(l : tnode);
      begin
         inherited create(loadvmtaddrn,l);
      end;


    function tloadvmtaddrnode.pass_typecheck:tnode;
      begin
        result:=nil;
        typecheckpass(left);
        if codegenerror then
         exit;

        case left.resultdef.typ of
          classrefdef :
            resultdef:=left.resultdef;
          objectdef :
            resultdef:=tclassrefdef.create(left.resultdef);
          else
            Message(parser_e_pointer_to_class_expected);
        end;
      end;


    function tloadvmtaddrnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_REGISTER;
         if left.nodetype<>typen then
           firstpass(left)
         { keep track of which classes might be instantiated via a classrefdef }
         else if (left.resultdef.typ=classrefdef) then
           tobjectdef(tclassrefdef(left.resultdef).pointeddef).register_maybe_created_object_type
         else if (left.resultdef.typ=objectdef) then
           tobjectdef(left.resultdef).register_maybe_created_object_type;
      end;


{*****************************************************************************
                        TLOADPARENTFPNODE
*****************************************************************************}

    constructor tloadparentfpnode.create(pd:tprocdef);
      begin
        inherited create(loadparentfpn,nil);
        if not assigned(pd) then
          internalerror(200309288);
        if (pd.parast.symtablelevel>current_procinfo.procdef.parast.symtablelevel) then
          internalerror(200309284);
        parentpd:=pd;
      end;


    constructor tloadparentfpnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(parentpdderef);
      end;


    procedure tloadparentfpnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(parentpdderef);
      end;


    procedure tloadparentfpnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        parentpdderef.build(parentpd);
      end;


    procedure tloadparentfpnode.derefimpl;
      begin
        inherited derefimpl;
        parentpd:=tprocdef(parentpdderef.resolve);
      end;


    function tloadparentfpnode.docompare(p: tnode): boolean;
      begin
        result:=
          inherited docompare(p) and
          (tloadparentfpnode(p).parentpd=parentpd);
      end;


    function tloadparentfpnode.dogetcopy : tnode;
      var
         p : tloadparentfpnode;
      begin
         p:=tloadparentfpnode(inherited dogetcopy);
         p.parentpd:=parentpd;
         dogetcopy:=p;
      end;


    function tloadparentfpnode.pass_typecheck:tnode;
{$ifdef dummy}
      var
        currpi : tprocinfo;
        hsym   : tparavarsym;
{$endif dummy}
      begin
        result:=nil;
        resultdef:=voidpointertype;
{$ifdef dummy}
        { currently parentfps are never loaded in registers (FK) }
        if (current_procinfo.procdef.parast.symtablelevel<>parentpd.parast.symtablelevel) then
          begin
            currpi:=current_procinfo;
            { walk parents }
            while (currpi.procdef.owner.symtablelevel>parentpd.parast.symtablelevel) do
              begin
                currpi:=currpi.parent;
                if not assigned(currpi) then
                  internalerror(2005040602);
                hsym:=tparavarsym(currpi.procdef.parast.Find('parentfp'));
                if not assigned(hsym) then
                  internalerror(2005040601);
                hsym.varregable:=vr_none;
              end;
          end;
{$endif dummy}
      end;


    function tloadparentfpnode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_REGISTER;
      end;


{*****************************************************************************
                             TADDRNODE
*****************************************************************************}

    constructor taddrnode.create(l : tnode);

      begin
         inherited create(addrn,l);
         getprocvardef:=nil;
         mark_read_written := true;
      end;


    constructor taddrnode.create_internal(l : tnode);
      begin
        self.create(l);
        include(flags,nf_internal);
      end;


    constructor taddrnode.create_internal_nomark(l : tnode);
      begin
        self.create_internal(l);
        mark_read_written := false;
      end;


    constructor taddrnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(getprocvardefderef);
      end;


    procedure taddrnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(getprocvardefderef);
      end;

    procedure Taddrnode.mark_write;

    begin
      {@procvar:=nil is legal in Delphi mode.}
      left.mark_write;
    end;

    procedure taddrnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        getprocvardefderef.build(getprocvardef);
      end;


    procedure taddrnode.derefimpl;
      begin
        inherited derefimpl;
        getprocvardef:=tprocvardef(getprocvardefderef.resolve);
      end;


    function taddrnode.docompare(p: tnode): boolean;
      begin
        result:=
          inherited docompare(p) and
          (taddrnode(p).getprocvardef=getprocvardef);
      end;


    function taddrnode.dogetcopy : tnode;
      var
         p : taddrnode;
      begin
         p:=taddrnode(inherited dogetcopy);
         p.getprocvardef:=getprocvardef;
         dogetcopy:=p;
      end;


    function taddrnode.pass_typecheck:tnode;
      var
         hp  : tnode;
         hsym : tfieldvarsym;
         isprocvar : boolean;
      begin
        result:=nil;
        typecheckpass(left);
        if codegenerror then
         exit;

        make_not_regable(left,[ra_addr_regable,ra_addr_taken]);

        { don't allow constants, for internal use we also
          allow taking the address of strings }
        if is_constnode(left) and
           not(
               (nf_internal in flags) and
               (left.nodetype in [stringconstn])
              ) then
         begin
           current_filepos:=left.fileinfo;
           CGMessage(type_e_no_addr_of_constant);
           exit;
         end;

        { Handle @proc special, also @procvar in tp-mode needs
          special handling }
        if (left.resultdef.typ=procdef) or
           (
            (left.resultdef.typ=procvardef) and
            ((m_tp_procvar in current_settings.modeswitches) or
             (m_mac_procvar in current_settings.modeswitches))
           ) then
          begin
            isprocvar:=(left.resultdef.typ=procvardef);

            if not isprocvar then
              begin
                left:=ctypeconvnode.create_proc_to_procvar(left);
                typecheckpass(left);
              end;

            { In tp procvar mode the result is always a voidpointer. Insert
              a typeconversion to voidpointer. For methodpointers we need
              to load the proc field }
            if (m_tp_procvar in current_settings.modeswitches) or
               (m_mac_procvar in current_settings.modeswitches) then
              begin
                if tabstractprocdef(left.resultdef).is_addressonly then
                  begin
                    result:=ctypeconvnode.create_internal(left,voidpointertype);
                    include(result.flags,nf_load_procvar);
                    left:=nil;
                  end
                else
                  begin
                    { For procvars we need to return the proc field of the
                      methodpointer }
                    if isprocvar then
                      begin
                        { find proc field in methodpointer record }
                        hsym:=tfieldvarsym(trecorddef(methodpointertype).symtable.Find('proc'));
                        if not assigned(hsym) then
                          internalerror(200412041);
                        { Load tmehodpointer(left).proc }
                        result:=csubscriptnode.create(
                                     hsym,
                                     ctypeconvnode.create_internal(left,methodpointertype));
                        left:=nil;
                      end
                    else
                      CGMessage(type_e_variable_id_expected);
                  end;
              end
            else
              begin
                { Return the typeconvn only }
                result:=left;
                left:=nil;
              end;
          end
        else
          begin
            { what are we getting the address from an absolute sym? }
            hp:=left;
            while assigned(hp) and (hp.nodetype in [typeconvn,vecn,derefn,subscriptn]) do
              hp:=tunarynode(hp).left;
            if not assigned(hp) then
              internalerror(200412042);
{$ifdef i386}
            if (hp.nodetype=loadn) and
               ((tloadnode(hp).symtableentry.typ=absolutevarsym) and
               tabsolutevarsym(tloadnode(hp).symtableentry).absseg) then
              begin
                if not(nf_typedaddr in flags) then
                  resultdef:=voidfarpointertype
                else
                  resultdef:=tpointerdef.createfar(left.resultdef);
              end
            else
{$endif i386}
              if (nf_internal in flags) or
                 valid_for_addr(left,true) then
                begin
                  if not(nf_typedaddr in flags) then
                    resultdef:=voidpointertype
                  else
                    resultdef:=tpointerdef.create(left.resultdef);
                end
            else
              CGMessage(type_e_variable_id_expected);
          end;

        if mark_read_written then
          begin
            { This is actually only "read", but treat it nevertheless as  }
            { modified due to the possible use of pointers                }
            { To avoid false positives regarding "uninitialised"          }
            { warnings when using arrays, perform it in two steps         }
            set_varstate(left,vs_written,[]);
            { vsf_must_be_valid so it doesn't get changed into }
            { vsf_referred_not_inited                          }
            set_varstate(left,vs_read,[vsf_must_be_valid]);
          end;
      end;


    function taddrnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         { is this right for object of methods ?? }
         expectloc:=LOC_REGISTER;
      end;


{*****************************************************************************
                             TDEREFNODE
*****************************************************************************}

    constructor tderefnode.create(l : tnode);

      begin
         inherited create(derefn,l);

      end;

    function tderefnode.pass_typecheck:tnode;
      begin
         result:=nil;
         typecheckpass(left);
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
          exit;

         { tp procvar support }
         maybe_call_procvar(left,true);

         if left.resultdef.typ=pointerdef then
          resultdef:=tpointerdef(left.resultdef).pointeddef
         else
          CGMessage(parser_e_invalid_qualifier);
      end;

    procedure Tderefnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tderefnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         expectloc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                            TSUBSCRIPTNODE
*****************************************************************************}

    constructor tsubscriptnode.create(varsym : tsym;l : tnode);

      begin
         inherited create(subscriptn,l);
         { vs should be changed to tsym! }
         vs:=tfieldvarsym(varsym);
      end;

    constructor tsubscriptnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(vsderef);
      end;


    procedure tsubscriptnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(vsderef);
      end;


    procedure tsubscriptnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        vsderef.build(vs);
      end;


    procedure tsubscriptnode.derefimpl;
      begin
        inherited derefimpl;
        vs:=tfieldvarsym(vsderef.resolve);
      end;


    function tsubscriptnode.dogetcopy : tnode;
      var
         p : tsubscriptnode;
      begin
         p:=tsubscriptnode(inherited dogetcopy);
         p.vs:=vs;
         dogetcopy:=p;
      end;


    function tsubscriptnode.pass_typecheck:tnode;
      begin
        result:=nil;
        typecheckpass(left);
        { tp procvar support }
        maybe_call_procvar(left,true);
        resultdef:=vs.vardef;

        // don't put records from which we load fields which aren't regable in integer registers
        if (left.resultdef.typ = recorddef) and
           not(tstoreddef(resultdef).is_intregable) then
          make_not_regable(left,[ra_addr_regable]);
      end;

    procedure Tsubscriptnode.mark_write;
      begin
        include(flags,nf_write);
      end;


    function tsubscriptnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         { classes must be dereferenced implicit }
         if is_class_or_interface(left.resultdef) then
           expectloc:=LOC_REFERENCE
         else
           begin
             case left.expectloc of
               LOC_REGISTER,
               LOC_SUBSETREG:
                 // can happen for function results on win32 and darwin/x86
                 if (left.resultdef.size > sizeof(pint)) then
                   expectloc:=LOC_REFERENCE
                 else
                   expectloc:=LOC_SUBSETREG;
               LOC_CREGISTER,
               LOC_CSUBSETREG:
                 expectloc:=LOC_CSUBSETREG;
               LOC_REFERENCE,
               LOC_CREFERENCE:
                 expectloc:=left.expectloc;
               else internalerror(20060521);
              end;
           end;
      end;

    function tsubscriptnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (vs = tsubscriptnode(p).vs);
      end;


{*****************************************************************************
                               TVECNODE
*****************************************************************************}

    constructor tvecnode.create(l,r : tnode);

      begin
         inherited create(vecn,l,r);
      end;


    function tvecnode.pass_typecheck:tnode;
      var
         hightree: tnode;
         htype,elementdef : tdef;
         valid : boolean;
      begin
         result:=nil;
         typecheckpass(left);
         typecheckpass(right);

         { implicitly convert stringconstant to stringdef,
           see tbs/tb0476.pp for a test }
         if (left.nodetype=stringconstn) and
            (tstringconstnode(left).cst_type=cst_conststring) then
           begin
             if tstringconstnode(left).len>255 then
               inserttypeconv(left,cansistringtype)
             else
               inserttypeconv(left,cshortstringtype);
           end;

         { In p[1] p is always valid, it is not possible to
           declared a shortstring or normal array that has
           undefined number of elements. Dynamic array and
           ansi/widestring needs to be valid }
         valid:=is_dynamic_array(left.resultdef) or
                is_ansistring(left.resultdef) or
                is_wide_or_unicode_string(left.resultdef) or
                { implicit pointer dereference -> pointer is read }
                (left.resultdef.typ = pointerdef);
         if valid then
           set_varstate(left,vs_read,[vsf_must_be_valid]);
{
         A vecn is, just like a loadn, always part of an expression with its
         own read/write and must_be_valid semantics. Therefore we don't have
         to do anything else here, just like for loadn's
}
         set_varstate(right,vs_read,[vsf_must_be_valid]);
         if codegenerror then
          exit;

         { maybe type conversion for the index value, but
           do not convert enums,booleans,char
           and do not convert range nodes }
         if (right.nodetype<>rangen) and (is_integer(right.resultdef) or (left.resultdef.typ<>arraydef)) then
           case left.resultdef.typ of
             arraydef:
               if ado_isvariant in Tarraydef(left.resultdef).arrayoptions then
                 {Variant arrays are a special array, can have negative indexes and would therefore
                  need s32bit. However, they should not appear in a vecn, as they are handled in
                  handle_variantarray in pexpr.pas. Therefore, encountering a variant array is an
                  internal error... }
                 internalerror(200707031)
               else if is_special_array(left.resultdef) then
                 {Arrays without a high bound (dynamic arrays, open arrays) are zero based,
                  convert indexes into these arrays to aword.}
                 inserttypeconv(right,uinttype)
               else
                 {Convert array indexes to low_bound..high_bound.}
                 inserttypeconv(right,Torddef.create(Torddef(sinttype).ordtype,
                                                     int64(Tarraydef(left.resultdef).lowrange),
                                                     int64(Tarraydef(left.resultdef).highrange)
                                                    ));
             stringdef:
               if is_open_string(left.resultdef) then
                 inserttypeconv(right,u8inttype)
               else if is_shortstring(left.resultdef) then
                 {Convert shortstring indexes to 0..length.}
                 inserttypeconv(right,Torddef.create(u8bit,0,int64(Tstringdef(left.resultdef).len)))
               else
                 {Convert indexes into dynamically allocated strings to aword.}
                 inserttypeconv(right,uinttype);
             else
               {Others, i.e. pointer indexes to aint.}
               inserttypeconv(right,sinttype);
           end;

         case left.resultdef.typ of
           arraydef :
             begin
               { check type of the index value }
               if (compare_defs(right.resultdef,tarraydef(left.resultdef).rangedef,right.nodetype)=te_incompatible) then
                 IncompatibleTypes(right.resultdef,tarraydef(left.resultdef).rangedef);
               if right.nodetype=rangen then
                 resultdef:=left.resultdef
               else
                 resultdef:=Tarraydef(left.resultdef).elementdef;

               { if we are range checking an open array or array of const, we }
               { need to load the high parameter. If the current procedure is }
               { nested inside the procedure to which the open array/of const }
               { was passed, then the high parameter must not be a regvar.    }
               { So create a loadnode for the high parameter here and         }
               { typecheck it, then the loadnode will make the high parameter }
               { not regable. Otherwise this would only happen inside pass_2, }
               { which is too late since by then the regvars are already      }
               { assigned (pass_1 is also already too late, because then the  }
               { regvars of the parent are also already assigned).            }
               { webtbs/tw8975                                                }
               if (cs_check_range in current_settings.localswitches) and
                  (is_open_array(left.resultdef) or
                   is_array_of_const(left.resultdef)) and
                  { cdecl functions don't have high() so we can not check the range }
                  { (can't use current_procdef, since it may be a nested procedure) }
                  not(tprocdef(tparasymtable(tparavarsym(tloadnode(left).symtableentry).owner).defowner).proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                   begin
                     { load_high_value_node already typechecks }
                     hightree:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry));
                     hightree.free;
                   end;


             end;
           pointerdef :
             begin
               { are we accessing a pointer[], then convert the pointer to
                 an array first, in FPC this is allowed for all pointers
                 (except voidpointer) in delphi/tp7 it's only allowed for pchars. }
               if not is_voidpointer(left.resultdef) and
                  (
                   (m_fpc in current_settings.modeswitches) or
                   is_pchar(left.resultdef) or
                   is_pwidechar(left.resultdef)
                  ) then
                begin
                  { convert pointer to array }
                  htype:=tarraydef.create_from_pointer(tpointerdef(left.resultdef).pointeddef);
                  inserttypeconv(left,htype);
                  if right.nodetype=rangen then
                    resultdef:=htype
                  else
                    resultdef:=tarraydef(htype).elementdef;
                end
               else
                CGMessage(type_e_array_required);
             end;
           stringdef :
             begin
                case tstringdef(left.resultdef).stringtype of
                  st_unicodestring,
                  st_widestring :
                    elementdef:=cwidechartype;
                  st_ansistring :
                    elementdef:=cchartype;
                  st_longstring :
                    elementdef:=cchartype;
                  st_shortstring :
                    elementdef:=cchartype;
                end;
                if right.nodetype=rangen then
                  begin
                    htype:=Tarraydef.create_from_pointer(elementdef);
                    resultdef:=htype;
                  end
                else
                 begin
                   { indexed access to 0 element is only allowed for shortstrings }
                   if (right.nodetype=ordconstn) and
                      (Tordconstnode(right).value.svalue=0) and
                      not is_shortstring(left.resultdef) then
                     CGMessage(cg_e_can_access_element_zero);
                   resultdef:=elementdef;
                 end;
             end;
           variantdef :
             resultdef:=cvarianttype;
           else
             CGMessage(type_e_array_required);
        end;
      end;

    procedure Tvecnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tvecnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         if (nf_callunique in flags) and
            (is_ansistring(left.resultdef) or
             is_unicodestring(left.resultdef) or
            (is_widestring(left.resultdef) and not(tf_winlikewidestring in target_info.flags))) then
           begin
             left := ctypeconvnode.create_internal(ccallnode.createintern('fpc_'+tstringdef(left.resultdef).stringtypname+'_unique',
               ccallparanode.create(
                 ctypeconvnode.create_internal(left,voidpointertype),nil)),
               left.resultdef);
             firstpass(left);
             { double resultdef passes somwhere else may cause this to be }
             { reset though :/                                             }
             exclude(flags,nf_callunique);
           end
         else if is_widestring(left.resultdef) and (tf_winlikewidestring in target_info.flags) then
           exclude(flags,nf_callunique);

         if (not is_packed_array(left.resultdef)) or
            ((tarraydef(left.resultdef).elepackedbitsize mod 8) = 0) then
           if left.expectloc=LOC_CREFERENCE then
             expectloc:=LOC_CREFERENCE
           else
             expectloc:=LOC_REFERENCE
         else
           if left.expectloc=LOC_CREFERENCE then
             expectloc:=LOC_CSUBSETREF
           else
             expectloc:=LOC_SUBSETREF;
      end;


{*****************************************************************************
                               TWITHNODE
*****************************************************************************}

    constructor twithnode.create(l:tnode);
      begin
         inherited create(withn,l);
         fileinfo:=l.fileinfo;
      end;


    destructor twithnode.destroy;
      begin
        inherited destroy;
      end;


    constructor twithnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
      end;


    procedure twithnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
      end;


    function twithnode.dogetcopy : tnode;
      var
         p : twithnode;
      begin
         p:=twithnode(inherited dogetcopy);
         result:=p;
      end;


    function twithnode.pass_typecheck:tnode;
      begin
        result:=nil;
        resultdef:=voidtype;
        if assigned(left) then
          typecheckpass(left);
      end;


    function twithnode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;
      end;


    function twithnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p);
      end;

    function is_big_untyped_addrnode(p: tnode): boolean;
      begin
        is_big_untyped_addrnode:=(p.nodetype=addrn) and
	  not (nf_typedaddr in p.flags) and (taddrnode(p).left.resultdef.size > 1);
      end;

begin
  cloadvmtaddrnode := tloadvmtaddrnode;
  caddrnode := taddrnode;
  cderefnode := tderefnode;
  csubscriptnode := tsubscriptnode;
  cvecnode := tvecnode;
  cwithnode := twithnode;
end.
