{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Type checking and register allocation for load/assignment nodes

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
unit nld;

{$i fpcdefs.inc}

interface

    uses
       node,
       {$ifdef state_tracking}
       nstate,
       {$endif}
       symconst,symbase,symtype,symsym,symdef;

    type
       tloadnode = class(tunarynode)
          symtableentry : tsym;
          symtableentryderef : tderef;
          symtable : tsymtable;
          procdef : tprocdef;
          procdefderef : tderef;
          constructor create(v : tsym;st : tsymtable);virtual;
          constructor create_procvar(v : tsym;d:tprocdef;st : tsymtable);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          procedure set_mp(p:tnode);
          function  is_addr_param_load:boolean;
          function  _getcopy : tnode;override;
          function  pass_1 : tnode;override;
          function  det_resulttype:tnode;override;
          procedure mark_write;override;
          function  docompare(p: tnode): boolean; override;
          procedure printnodedata(var t:text);override;
       end;
       tloadnodeclass = class of tloadnode;

       { different assignment types }
       tassigntype = (at_normal,at_plus,at_minus,at_star,at_slash);

       tassignmentnode = class(tbinarynode)
          assigntype : tassigntype;
          constructor create(l,r : tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function _getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       {$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
       {$endif state_tracking}
          function docompare(p: tnode): boolean; override;
       end;
       tassignmentnodeclass = class of tassignmentnode;

       tarrayconstructorrangenode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tarrayconstructorrangenodeclass = class of tarrayconstructorrangenode;

       tarrayconstructornode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function _getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
          procedure force_type(tt:ttype);
          procedure insert_typeconvs;
       end;
       tarrayconstructornodeclass = class of tarrayconstructornode;

       ttypenode = class(tnode)
          allowed : boolean;
          restype : ttype;
          constructor create(t : ttype);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       ttypenodeclass = class of ttypenode;

       trttinode = class(tnode)
          l1,l2  : longint;
          rttitype : trttitype;
          rttidef : tstoreddef;
          rttidefderef : tderef;
          constructor create(def:tstoreddef;rt:trttitype);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function  _getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       trttinodeclass = class of trttinode;

    var
       cloadnode : tloadnodeclass;
       cassignmentnode : tassignmentnodeclass;
       carrayconstructorrangenode : tarrayconstructorrangenodeclass;
       carrayconstructornode : tarrayconstructornodeclass;
       ctypenode : ttypenodeclass;
       crttinode : trttinodeclass;



implementation

    uses
      cutils,verbose,globtype,globals,systems,
      symnot,
      defutil,defcmp,
      htypechk,pass_1,procinfo,paramgr,
      ncon,ninl,ncnv,nmem,ncal,nutils,
      cgobj,cgbase
      ;

{*****************************************************************************
                             TLOADNODE
*****************************************************************************}

    constructor tloadnode.create(v : tsym;st : tsymtable);
      begin
         inherited create(loadn,nil);
         if not assigned(v) then
          internalerror(200108121);
         symtableentry:=v;
         symtable:=st;
         procdef:=nil;
      end;


    constructor tloadnode.create_procvar(v : tsym;d:tprocdef;st : tsymtable);
      begin
         inherited create(loadn,nil);
         if not assigned(v) then
          internalerror(200108121);
         symtableentry:=v;
         symtable:=st;
         procdef:=d;
      end;


    constructor tloadnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(symtableentryderef);
        symtable:=nil;
        ppufile.getderef(procdefderef);
      end;


    procedure tloadnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(symtableentryderef);
        ppufile.putderef(procdefderef);
      end;


    procedure tloadnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        symtableentryderef.build(symtableentry);
        procdefderef.build(procdef);
      end;


    procedure tloadnode.derefimpl;
      begin
        inherited derefimpl;
        symtableentry:=tsym(symtableentryderef.resolve);
        symtable:=symtableentry.owner;
        procdef:=tprocdef(procdefderef.resolve);
      end;


    procedure tloadnode.set_mp(p:tnode);
      begin
        { typen nodes should not be set }
        if p.nodetype=typen then
          internalerror(200301042);
        left:=p;
      end;


    function tloadnode._getcopy : tnode;
      var
         n : tloadnode;

      begin
         n:=tloadnode(inherited _getcopy);
         n.symtable:=symtable;
         n.symtableentry:=symtableentry;
         n.procdef:=procdef;
         result:=n;
      end;


    function tloadnode.is_addr_param_load:boolean;
      begin
        result:=(symtable.symtabletype=parasymtable) and
                (symtableentry.typ=paravarsym) and
                not(vo_has_local_copy in tparavarsym(symtableentry).varoptions) and
                not(nf_load_self_pointer in flags) and
                paramanager.push_addr_param(tparavarsym(symtableentry).varspez,tparavarsym(symtableentry).vartype.def,tprocdef(symtable.defowner).proccalloption);
      end;


    function tloadnode.det_resulttype:tnode;
      begin
         result:=nil;
         case symtableentry.typ of
           absolutevarsym :
             resulttype:=tabsolutevarsym(symtableentry).vartype;
           constsym:
             begin
               if tconstsym(symtableentry).consttyp=constresourcestring then
                 resulttype:=cansistringtype
               else
                 internalerror(22799);
             end;
           globalvarsym,
           paravarsym,
           localvarsym :
             begin
               inc(tabstractvarsym(symtableentry).refs);
               { Nested variable? The we need to load the framepointer of
                 the parent procedure }
               if assigned(current_procinfo) then
                 begin
                   if (symtable.symtabletype in [localsymtable,parasymtable]) and
                      (symtable.symtablelevel<>current_procinfo.procdef.parast.symtablelevel) then
                     begin
                       if assigned(left) then
                         internalerror(200309289);
                       left:=cloadparentfpnode.create(tprocdef(symtable.defowner));
                       { we can't inline the referenced parent procedure }
                       exclude(tprocdef(symtable.defowner).procoptions,po_inline);
                       { reference in nested procedures, variable needs to be in memory }
                       make_not_regable(self);
                     end;
                   { static variables referenced in procedures or from finalization,
                     variable needs to be in memory.
                     It is too hard and the benefit is too small to detect whether a
                     variable is only used in the finalization to add support for it (PFV) }
                   if (symtable.symtabletype=staticsymtable) and
                      (
                       (symtable.symtablelevel<>current_procinfo.procdef.localst.symtablelevel) or
                       (current_procinfo.procdef.proctypeoption=potype_unitfinalize)
                      ) then
                     make_not_regable(self);
                 end;
               { fix self type which is declared as voidpointer in the
                 definition }
               if vo_is_self in tabstractvarsym(symtableentry).varoptions then
                 begin
                   resulttype.setdef(tprocdef(symtableentry.owner.defowner)._class);
                   if (po_classmethod in tprocdef(symtableentry.owner.defowner).procoptions) or
                      (po_staticmethod in tprocdef(symtableentry.owner.defowner).procoptions) then
                     resulttype.setdef(tclassrefdef.create(resulttype))
                   else if is_object(resulttype.def) and
                           (nf_load_self_pointer in flags) then
                     resulttype.setdef(tpointerdef.create(resulttype));
                 end
               else if vo_is_vmt in tabstractvarsym(symtableentry).varoptions then
                 begin
                   resulttype.setdef(tprocdef(symtableentry.owner.defowner)._class);
                   resulttype.setdef(tclassrefdef.create(resulttype));
                 end
               else
                 resulttype:=tabstractvarsym(symtableentry).vartype;
             end;
           typedconstsym :
             resulttype:=ttypedconstsym(symtableentry).typedconsttype;
           procsym :
             begin
               if not assigned(procdef) then
                begin
                  if Tprocsym(symtableentry).procdef_count>1 then
                   CGMessage(parser_e_no_overloaded_procvars);
                  procdef:=tprocsym(symtableentry).first_procdef;
                end;

               { the result is a procdef, addrn and proc_to_procvar
                 typeconvn need this as resulttype so they know
                 that the address needs to be returned }
               resulttype.setdef(procdef);

               { process methodpointer }
               if assigned(left) then
                 resulttypepass(left);
             end;
           labelsym:
             resulttype:=voidtype;
           else
             internalerror(200104141);
         end;
      end;

    procedure Tloadnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tloadnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_REFERENCE;
         registersint:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         if (cs_create_pic in aktmoduleswitches) and
           not(symtableentry.typ in [paravarsym,localvarsym]) then
           include(current_procinfo.flags,pi_needs_got);

         case symtableentry.typ of
            absolutevarsym :
              ;
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   expectloc:=LOC_CREFERENCE;
              end;
            globalvarsym,
            localvarsym,
            paravarsym :
              begin
                if assigned(left) then
                  firstpass(left);
                if not is_addr_param_load and
                   tabstractvarsym(symtableentry).is_regvar then
                  begin
                    case tabstractvarsym(symtableentry).varregable of
                      vr_intreg :
                        expectloc:=LOC_CREGISTER;
                      vr_fpureg :
                        expectloc:=LOC_CFPUREGISTER;
                      vr_mmreg :
                        expectloc:=LOC_CMMREGISTER;
                    end
                  end
                else
                  if (tabstractvarsym(symtableentry).varspez=vs_const) then
                    expectloc:=LOC_CREFERENCE;
                { we need a register for call by reference parameters }
                if paramanager.push_addr_param(tabstractvarsym(symtableentry).varspez,tabstractvarsym(symtableentry).vartype.def,pocall_default) then
                  registersint:=1;
                if ([vo_is_thread_var,vo_is_dll_var]*tabstractvarsym(symtableentry).varoptions)<>[] then
                  registersint:=1;
                if (target_info.system=system_powerpc_darwin) and
                   ([vo_is_dll_var,vo_is_external] * tabstractvarsym(symtableentry).varoptions <> []) then
                  include(current_procinfo.flags,pi_needs_got);
                { call to get address of threadvar }
                if (vo_is_thread_var in tabstractvarsym(symtableentry).varoptions) then
                  include(current_procinfo.flags,pi_do_call);
                if nf_write in flags then
                  Tabstractvarsym(symtableentry).trigger_notifications(vn_onwrite)
                else
                  Tabstractvarsym(symtableentry).trigger_notifications(vn_onread);
                { count variable references }
                if cg.t_times>1 then
                  inc(tabstractvarsym(symtableentry).refs,cg.t_times-1);
              end;
            typedconstsym :
                ;
            procsym :
                begin
                   { method pointer ? }
                   if assigned(left) then
                     begin
                        expectloc:=LOC_CREFERENCE;
                        firstpass(left);
                        registersint:=max(registersint,left.registersint);
                        registersfpu:=max(registersfpu,left.registersfpu);
 {$ifdef SUPPORT_MMX}
                        registersmmx:=max(registersmmx,left.registersmmx);
 {$endif SUPPORT_MMX}
                     end;
                end;
           labelsym :
             ;
           else
             internalerror(200104143);
         end;
      end;


    function tloadnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (symtableentry = tloadnode(p).symtableentry) and
          (procdef = tloadnode(p).procdef) and
          (symtable = tloadnode(p).symtable);
      end;


    procedure Tloadnode.printnodedata(var t:text);
      begin
        inherited printnodedata(t);
        write(t,printnodeindention,'symbol = ',symtableentry.name);
        if symtableentry.typ=procsym then
          write(t,printnodeindention,'procdef = ',procdef.mangledname);
        writeln(t,'');
      end;


{*****************************************************************************
                             TASSIGNMENTNODE
*****************************************************************************}

    constructor tassignmentnode.create(l,r : tnode);

      begin
         inherited create(assignn,l,r);
         l.mark_write;
         assigntype:=at_normal;
      end;


    constructor tassignmentnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        assigntype:=tassigntype(ppufile.getbyte);
      end;


    procedure tassignmentnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(assigntype));
      end;


    function tassignmentnode._getcopy : tnode;

      var
         n : tassignmentnode;

      begin
         n:=tassignmentnode(inherited _getcopy);
         n.assigntype:=assigntype;
         result:=n;
      end;


    function tassignmentnode.det_resulttype:tnode;
      var
        hp : tnode;
        useshelper : boolean;
        original_size : longint;
      begin
        result:=nil;
        resulttype:=voidtype;
        original_size := 0;

        { must be made unique }
        set_unique(left);

        resulttypepass(left);

        if is_ansistring(left.resulttype.def) then
          begin
            { fold <ansistring>:=<ansistring>+<char|shortstring|ansistring> }
            if (right.nodetype=addn) and
               left.isequal(tbinarynode(right).left) and
               { don't fold multiple concatenations else we could get trouble
                 with multiple uses of s
               }
               (tbinarynode(right).left.nodetype<>addn) and
               (tbinarynode(right).right.nodetype<>addn) then
              begin
                { don't do a resulttypepass(right), since then the addnode }
                { may insert typeconversions that make this optimization   }
                { opportunity quite difficult to detect (JM)               }
                resulttypepass(tbinarynode(right).left);
                resulttypepass(tbinarynode(right).right);
                if (tbinarynode(right).right.nodetype=stringconstn) or
		   is_char(tbinarynode(right).right.resulttype.def) or
                   is_shortstring(tbinarynode(right).right.resulttype.def) or
                   is_ansistring(tbinarynode(right).right.resulttype.def) then
                  begin
                    { remove property flag so it'll not trigger an error }
                    exclude(left.flags,nf_isproperty);
                    { generate call to helper }
                    hp:=ccallparanode.create(tbinarynode(right).right,
                      ccallparanode.create(left,nil));
                    if is_char(tbinarynode(right).right.resulttype.def) then
                      result:=ccallnode.createintern('fpc_'+Tstringdef(left.resulttype.def).stringtypname+'_append_char',hp)
                    else if is_shortstring(tbinarynode(right).right.resulttype.def) then
                      result:=ccallnode.createintern('fpc_'+Tstringdef(left.resulttype.def).stringtypname+'_append_shortstring',hp)
                    else
                      result:=ccallnode.createintern('fpc_'+Tstringdef(left.resulttype.def).stringtypname+'_append_ansistring',hp);
                    tbinarynode(right).right:=nil;
                    left:=nil;
                    exit;
                 end;
              end;
          end
        else
         if is_shortstring(left.resulttype.def) then
          begin
            { fold <shortstring>:=<shortstring>+<shortstring>,
              <shortstring>+<char> is handled by an optimized node }
            if (right.nodetype=addn) and
               left.isequal(tbinarynode(right).left) and
               { don't fold multiple concatenations else we could get trouble
                 with multiple uses of s }
               (tbinarynode(right).left.nodetype<>addn) and
               (tbinarynode(right).right.nodetype<>addn) then
              begin
                { don't do a resulttypepass(right), since then the addnode }
                { may insert typeconversions that make this optimization   }
                { opportunity quite difficult to detect (JM)               }
                resulttypepass(tbinarynode(right).left);
                resulttypepass(tbinarynode(right).right);
                if is_shortstring(tbinarynode(right).right.resulttype.def) then
                  begin
                    { remove property flag so it'll not trigger an error }
                    exclude(left.flags,nf_isproperty);
                    { generate call to helper }
                    hp:=ccallparanode.create(tbinarynode(right).right,
                      ccallparanode.create(left,nil));
                    if is_shortstring(tbinarynode(right).right.resulttype.def) then
                      result:=ccallnode.createintern('fpc_shortstr_append_shortstr',hp);
                    tbinarynode(right).right:=nil;
                    left:=nil;
                    exit;
                 end;
              end;
          end;

        resulttypepass(right);
        set_varstate(right,vs_read,[vsf_must_be_valid]);
        set_varstate(left,vs_written,[]);
        if codegenerror then
          exit;

        { tp procvar support, when we don't expect a procvar
          then we need to call the procvar }
        if (left.resulttype.def.deftype<>procvardef) then
          maybe_call_procvar(right,true);

        { assignments to formaldefs and open arrays aren't allowed }
        if (left.resulttype.def.deftype=formaldef) or
           is_open_array(left.resulttype.def) then
          CGMessage(type_e_operator_not_allowed);

        { test if node can be assigned, properties are allowed }
        valid_for_assignment(left,true);

        { assigning nil to a dynamic array clears the array }
        if is_dynamic_array(left.resulttype.def) and
           (right.nodetype=niln) then
         begin
           hp:=ccallparanode.create(caddrnode.create_internal
                   (crttinode.create(tstoreddef(left.resulttype.def),initrtti)),
               ccallparanode.create(ctypeconvnode.create_internal(left,voidpointertype),nil));
           result := ccallnode.createintern('fpc_dynarray_clear',hp);
           left:=nil;
           exit;
         end;

        { shortstring helpers can do the conversion directly,
          so treat them separatly }
        if (is_shortstring(left.resulttype.def)) then
         begin
           { insert typeconv, except for chars that are handled in
             secondpass and except for ansi/wide string that can
             be converted immediatly }
           if not(is_char(right.resulttype.def) or
                  (right.resulttype.def.deftype=stringdef)) then
             inserttypeconv(right,left.resulttype);
           if right.resulttype.def.deftype=stringdef then
            begin
              useshelper:=true;
              { convert constant strings to shortstrings. But
                skip empty constant strings, that will be handled
                in secondpass }
              if (right.nodetype=stringconstn) then
                begin
                  { verify if range fits within shortstring }
                  { just emit a warning, delphi gives an    }
                  { error, only if the type definition of   }
                  { of the string is less  < 255 characters }
                  if not is_open_string(left.resulttype.def) and
                     (tstringconstnode(right).len > tstringdef(left.resulttype.def).len) then
                     cgmessage(type_w_string_too_long);
                  inserttypeconv(right,left.resulttype);
                  if (tstringconstnode(right).len=0) then
                    useshelper:=false;
                end;
             { rest is done in pass 1 (JM) }
             if useshelper then
               exit;
            end
         end
        else
          begin
           { get the size before the type conversion - check for all nodes }
           if assigned(right.resulttype.def) and
              (right.resulttype.def.deftype in [enumdef,orddef,floatdef]) and
              (right.nodetype in [loadn,vecn,calln]) then
             original_size := right.resulttype.def.size;
           inserttypeconv(right,left.resulttype);
          end;

        { check if the assignment may cause a range check error }
        { if its not explicit, and only if the values are       }
        { ordinals, enumdef and floatdef                        }
        if (right.nodetype = typeconvn) and
           not (nf_explicit in ttypeconvnode(right).flags) then
         begin
            if assigned(left.resulttype.def) and
              (left.resulttype.def.deftype in [enumdef,orddef,floatdef]) and
              not is_boolean(left.resulttype.def) then
              begin
                if (original_size <> 0) and
                   (left.resulttype.def.size < original_size) then
                  begin
                    if (cs_check_range in aktlocalswitches) then
                      Message(type_w_smaller_possible_range_check)
                    else
                      Message(type_h_smaller_possible_range_check);
                  end;
              end;
         end;

        { call helpers for interface }
        if is_interfacecom(left.resulttype.def) then
         begin
           hp:=ccallparanode.create(ctypeconvnode.create_internal
                   (right,voidpointertype),
               ccallparanode.create(ctypeconvnode.create_internal
                   (left,voidpointertype),nil));
           result:=ccallnode.createintern('fpc_intf_assign',hp);
           left:=nil;
           right:=nil;
           exit;
         end;
        { call helpers for variant, they can contain non ref. counted types like
          vararrays which must be really copied }
        if left.resulttype.def.deftype=variantdef then
         begin
           hp:=ccallparanode.create(ctypeconvnode.create_internal(
                 caddrnode.create_internal(right),voidpointertype),
               ccallparanode.create(ctypeconvnode.create_internal(
                 caddrnode.create_internal(left),voidpointertype),
               nil));
           result:=ccallnode.createintern('fpc_variant_copy',hp);
           left:=nil;
           right:=nil;
           exit;
         end;

        { check if local proc/func is assigned to procvar }
        if right.resulttype.def.deftype=procvardef then
          test_local_to_procvar(tprocvardef(right.resulttype.def),left.resulttype.def);
      end;


    function tassignmentnode.pass_1 : tnode;
      var
        hp: tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;

         firstpass(left);
         firstpass(right);
         { assignment to refcounted variable -> inc/decref }
         if (not is_class(left.resulttype.def) and
            left.resulttype.def.needs_inittable) then
           include(current_procinfo.flags,pi_do_call);

         if codegenerror then
           exit;


        if (is_shortstring(left.resulttype.def)) then
          begin
           if right.resulttype.def.deftype=stringdef then
            begin
              if (right.nodetype<>stringconstn) or
                 (tstringconstnode(right).len<>0) then
               begin
                 if (cs_optimize in aktglobalswitches) and
                    (right.nodetype in [calln,blockn]) and
                    (left.nodetype = temprefn) and
                    is_shortstring(right.resulttype.def) and
                    not is_open_string(left.resulttype.def) and
                    (tstringdef(left.resulttype.def).len = 255) then
                   begin
                     { the blocknode case is handled in pass_2 at the temp }
                     { reference level (mainly for callparatemp)  (JM)     }
                     if (right.nodetype = calln) then
                       begin
                         tcallnode(right).funcretnode := left;
                         result := right;
                       end
                     else
                       exit;
                   end
                 else
                   begin
                     hp:=ccallparanode.create
                           (right,
                      ccallparanode.create(cinlinenode.create
                           (in_high_x,false,left.getcopy),nil));
                     result:=ccallnode.createinternreturn('fpc_'+tstringdef(right.resulttype.def).stringtypname+'_to_shortstr',hp,left);
                     firstpass(result);
                   end;
                 left:=nil;
                 right:=nil;
                 exit;
               end;
            end;
           end;

         if (cs_optimize in aktglobalswitches) and
            (right.nodetype = calln) and
            { left must be a temp, since otherwise as soon as you modify the }
            { result, the current left node is modified and that one may     }
            { still be an argument to the function or even accessed in the   }
            { function                                                       }
            (((left.nodetype = temprefn) and
              paramanager.ret_in_param(right.resulttype.def,
                tcallnode(right).procdefinition.proccalloption)) or
             { there's special support for ansi/widestrings in the callnode }
             is_ansistring(right.resulttype.def) or
             is_widestring(right.resulttype.def))  then
           begin
             tcallnode(right).funcretnode := left;
             result := right;
             left := nil;
             right := nil;
             exit;
           end;


         registersint:=left.registersint+right.registersint;
         registersfpu:=max(left.registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
         registersmmx:=max(left.registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
      end;


    function tassignmentnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (assigntype = tassignmentnode(p).assigntype);
      end;

{$ifdef state_tracking}
    function Tassignmentnode.track_state_pass(exec_known:boolean):boolean;

    var se:Tstate_entry;

    begin
        track_state_pass:=false;
        if exec_known then
            begin
                track_state_pass:=right.track_state_pass(exec_known);
                {Force a new resulttype pass.}
                right.resulttype.def:=nil;
                do_resulttypepass(right);
                resulttypepass(right);
                aktstate.store_fact(left.getcopy,right.getcopy);
            end
        else
            aktstate.delete_fact(left);
    end;
{$endif}


{*****************************************************************************
                           TARRAYCONSTRUCTORRANGENODE
*****************************************************************************}

    constructor tarrayconstructorrangenode.create(l,r : tnode);

      begin
         inherited create(arrayconstructorrangen,l,r);
      end;

    function tarrayconstructorrangenode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        resulttypepass(right);
        set_varstate(left,vs_read,[vsf_must_be_valid]);
        set_varstate(right,vs_read,[vsf_must_be_valid]);
        if codegenerror then
         exit;
        resulttype:=left.resulttype;
      end;


    function tarrayconstructorrangenode.pass_1 : tnode;
      begin
        firstpass(left);
        firstpass(right);
        expectloc:=LOC_CREFERENCE;
        calcregisters(self,0,0,0);
        result:=nil;
      end;


{****************************************************************************
                            TARRAYCONSTRUCTORNODE
*****************************************************************************}

    constructor tarrayconstructornode.create(l,r : tnode);
      begin
         inherited create(arrayconstructorn,l,r);
      end;


    function tarrayconstructornode._getcopy : tnode;
      var
         n : tarrayconstructornode;
      begin
         n:=tarrayconstructornode(inherited _getcopy);
         result:=n;
      end;


    function tarrayconstructornode.det_resulttype:tnode;
      var
        htype : ttype;
        hp    : tarrayconstructornode;
        len   : longint;
        varia : boolean;
      begin
        result:=nil;

      { are we allowing array constructor? Then convert it to a set }
        if not allow_array_constructor then
         begin
           hp:=tarrayconstructornode(getcopy);
           arrayconstructor_to_set(tnode(hp));
           result:=hp;
           exit;
         end;

      { only pass left tree, right tree contains next construct if any }
        htype.reset;
        len:=0;
        varia:=false;
        if assigned(left) then
         begin
           hp:=self;
           while assigned(hp) do
            begin
              resulttypepass(hp.left);
              set_varstate(hp.left,vs_read,[vsf_must_be_valid]);
              if (htype.def=nil) then
               htype:=hp.left.resulttype
              else
               begin
                 if ((nf_novariaallowed in flags) or (not varia)) and
                    (not equal_defs(htype.def,hp.left.resulttype.def)) then
                  begin
                    varia:=true;
                  end;
               end;
              inc(len);
              hp:=tarrayconstructornode(hp.right);
            end;
         end;
         { Set the type of empty or varia arrays to void. Also
           do this if the type is array of const/open array
           because those can't be used with setelementtype }
         if not assigned(htype.def) or
            varia or
            is_array_of_const(htype.def) or
            is_open_array(htype.def) then
           htype:=voidtype;
         resulttype.setdef(tarraydef.create(0,len-1,s32inttype));
         tarraydef(resulttype.def).setelementtype(htype);
         tarraydef(resulttype.def).IsConstructor:=true;
         tarraydef(resulttype.def).IsVariant:=varia;
      end;


    procedure tarrayconstructornode.force_type(tt:ttype);
      var
        hp : tarrayconstructornode;
      begin
        tarraydef(resulttype.def).setelementtype(tt);
        tarraydef(resulttype.def).IsConstructor:=true;
        tarraydef(resulttype.def).IsVariant:=false;
        if assigned(left) then
         begin
           hp:=self;
           while assigned(hp) do
            begin
              inserttypeconv(hp.left,tt);
              hp:=tarrayconstructornode(hp.right);
            end;
         end;
      end;


    procedure tarrayconstructornode.insert_typeconvs;
      var
        hp        : tarrayconstructornode;
        dovariant : boolean;
      begin
        dovariant:=(nf_forcevaria in flags) or tarraydef(resulttype.def).isvariant;
        { only pass left tree, right tree contains next construct if any }
        if assigned(left) then
         begin
           hp:=self;
           while assigned(hp) do
            begin
              resulttypepass(hp.left);
              { Insert typeconvs for array of const }
              if dovariant then
               begin
                 case hp.left.resulttype.def.deftype of
                   enumdef :
                     hp.left:=ctypeconvnode.create_internal(hp.left,s32inttype);
                   arraydef :
                     begin
                       if is_chararray(hp.left.resulttype.def) then
                         hp.left:=ctypeconvnode.create_internal(hp.left,charpointertype)
                       else
                         if is_widechararray(hp.left.resulttype.def) then
                           hp.left:=ctypeconvnode.create_internal(hp.left,widecharpointertype)
                       else
                         CGMessagePos1(hp.left.fileinfo,type_e_wrong_type_in_array_constructor,hp.left.resulttype.def.typename);
                     end;
                   orddef :
                     begin
                       if is_integer(hp.left.resulttype.def) and
                          not(is_64bitint(hp.left.resulttype.def)) then
                         hp.left:=ctypeconvnode.create(hp.left,s32inttype);
                     end;
                   floatdef :
                     if not(is_currency(hp.left.resulttype.def)) then
                       hp.left:=ctypeconvnode.create(hp.left,pbestrealtype^);
                   procvardef :
                     hp.left:=ctypeconvnode.create(hp.left,voidpointertype);
                   stringdef,
                   variantdef,
                   pointerdef,
                   classrefdef:
                     ;
                   objectdef :
                     if is_object(hp.left.resulttype.def) then
                       CGMessagePos1(hp.left.fileinfo,type_e_wrong_type_in_array_constructor,hp.left.resulttype.def.typename);
                   else
                     CGMessagePos1(hp.left.fileinfo,type_e_wrong_type_in_array_constructor,hp.left.resulttype.def.typename);
                 end;
               end;
              resulttypepass(hp.left);
              hp:=tarrayconstructornode(hp.right);
            end;
         end;
      end;


    function tarrayconstructornode.pass_1 : tnode;
      var
        hp : tarrayconstructornode;
        do_variant:boolean;
      begin
        do_variant:=(nf_forcevaria in flags) or tarraydef(resulttype.def).isvariant;
        result:=nil;
        { Insert required type convs, this must be
          done in pass 1, because the call must be
          resulttypepassed already }
        if assigned(left) then
          begin
            insert_typeconvs;
            { call firstpass for all nodes }
            hp:=self;
            while assigned(hp) do
              begin
                if hp.left<>nil then
                  begin
                    {This check is pessimistic; a call will happen depending
                     on the location in which the elements will be found in
                     pass 2.}
                    if not do_variant then
                      include(current_procinfo.flags,pi_do_call);
                    firstpass(hp.left);
                  end;
                hp:=tarrayconstructornode(hp.right);
              end;
          end;
        expectloc:=LOC_CREFERENCE;
        calcregisters(self,0,0,0);
      end;


    function tarrayconstructornode.docompare(p: tnode): boolean;

    begin
      docompare:=inherited docompare(p);
    end;


{*****************************************************************************
                              TTYPENODE
*****************************************************************************}

    constructor ttypenode.create(t : ttype);
      begin
         inherited create(typen);
         restype:=t;
         allowed:=false;
      end;


    constructor ttypenode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.gettype(restype);
        allowed:=boolean(ppufile.getbyte);
      end;


    procedure ttypenode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.puttype(restype);
        ppufile.putbyte(byte(allowed));
      end;


    procedure ttypenode.buildderefimpl;
      begin
        inherited buildderefimpl;
        restype.buildderef;
      end;


    procedure ttypenode.derefimpl;
      begin
        inherited derefimpl;
        restype.resolve;
      end;


    function ttypenode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=restype;
        { check if it's valid }
        if restype.def.deftype = errordef then
          CGMessage(parser_e_illegal_expression);
      end;


    function ttypenode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         { a typenode can't generate code, so we give here
           an error. Else it'll be an abstract error in pass_2.
           Only when the allowed flag is set we don't generate
           an error }
         if not allowed then
          Message(parser_e_no_type_not_allowed_here);
      end;


    function ttypenode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p);
      end;


{*****************************************************************************
                              TRTTINODE
*****************************************************************************}


    constructor trttinode.create(def:tstoreddef;rt:trttitype);
      begin
         inherited create(rttin);
         rttidef:=def;
         rttitype:=rt;
      end;


    constructor trttinode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(rttidefderef);
        rttitype:=trttitype(ppufile.getbyte);
      end;


    procedure trttinode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(rttidefderef);
        ppufile.putbyte(byte(rttitype));
      end;


    procedure trttinode.buildderefimpl;
      begin
        inherited buildderefimpl;
        rttidefderef.build(rttidef);
      end;


    procedure trttinode.derefimpl;
      begin
        inherited derefimpl;
        rttidef:=tstoreddef(rttidefderef.resolve);
      end;


    function trttinode._getcopy : tnode;
      var
         n : trttinode;
      begin
         n:=trttinode(inherited _getcopy);
         n.rttidef:=rttidef;
         n.rttitype:=rttitype;
         result:=n;
      end;


    function trttinode.det_resulttype:tnode;
      begin
        { rtti information will be returned as a void pointer }
        result:=nil;
        resulttype:=voidpointertype;
      end;


    function trttinode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_CREFERENCE;
      end;


    function trttinode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (rttidef = trttinode(p).rttidef) and
          (rttitype = trttinode(p).rttitype);
      end;


begin
   cloadnode:=tloadnode;
   cassignmentnode:=tassignmentnode;
   carrayconstructorrangenode:=tarrayconstructorrangenode;
   carrayconstructornode:=tarrayconstructornode;
   ctypenode:=ttypenode;
   crttinode:=trttinode;
end.
