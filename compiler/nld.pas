{
    $Id$
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
       symconst,symppu,symbase,symtype,symsym,symdef;

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
          procedure derefimpl;override;
          procedure set_mp(p:tnode);
          function  getcopy : tnode;override;
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
          function getcopy : tnode;override;
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
          function getcopy : tnode;override;
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
          procedure derefimpl;override;
          function  getcopy : tnode;override;
          function pass_1 : tnode;override;
          procedure pass_2;override;
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


    procedure load_procvar_from_calln(var p1:tnode);
    function load_high_value_node(vs:tvarsym):tnode;
    function load_self_node:tnode;
    function load_result_node:tnode;
    function load_self_pointer_node:tnode;
    function load_vmt_pointer_node:tnode;
    function is_self_node(p:tnode):boolean;


implementation

    uses
      cutils,verbose,globtype,globals,systems,
      symtable,symnot,
      defutil,defcmp,
      htypechk,pass_1,
      ncon,ninl,ncnv,nmem,ncal,
      paramgr,cpubase,rgobj,cgbase,procinfo
      ;

{*****************************************************************************
                                 Helpers
*****************************************************************************}

      procedure load_procvar_from_calln(var p1:tnode);
        var
          p2 : tnode;
        begin
          if p1.nodetype<>calln then
            internalerror(200212251);
          { was it a procvar, then we simply remove the calln and
            reuse the right }
          if assigned(tcallnode(p1).right) then
            begin
              p2:=tcallnode(p1).right;
              tcallnode(p1).right:=nil;
            end
          else
            begin
              p2:=cloadnode.create_procvar(tcallnode(p1).symtableprocentry,
                 tprocdef(tcallnode(p1).procdefinition),tcallnode(p1).symtableproc);
              { when the methodpointer is typen we've something like:
                tobject.create. Then only the address is needed of the
                method without a self pointer }
              if assigned(tcallnode(p1).methodpointer) and
                 (tcallnode(p1).methodpointer.nodetype<>typen) then
               begin
                 tloadnode(p2).set_mp(tcallnode(p1).methodpointer);
                 tcallnode(p1).methodpointer:=nil;
               end;
            end;
          resulttypepass(p2);
          p1.free;
          p1:=p2;
        end;


    function load_high_value_node(vs:tvarsym):tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        srsymtable:=vs.owner;
        srsym:=searchsymonlyin(srsymtable,'high'+vs.name);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            resulttypepass(result);
          end
        else
          CGMessage(cg_e_illegal_expression);
      end;


    function load_self_node:tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        searchsym('self',srsym,srsymtable);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            resulttypepass(result);
          end
        else
          CGMessage(cg_e_illegal_expression);
      end;


    function load_result_node:tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        searchsym('result',srsym,srsymtable);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            resulttypepass(result);
          end
        else
          CGMessage(cg_e_illegal_expression);
      end;


    function load_self_pointer_node:tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        searchsym('self',srsym,srsymtable);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            include(result.flags,nf_load_self_pointer);
            resulttypepass(result);
          end
        else
          CGMessage(cg_e_illegal_expression);
      end;


    function load_vmt_pointer_node:tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        searchsym('vmt',srsym,srsymtable);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            resulttypepass(result);
          end
        else
          CGMessage(cg_e_illegal_expression);
      end;


    function is_self_node(p:tnode):boolean;
      begin
        is_self_node:=(p.nodetype=loadn) and
                      (tloadnode(p).symtableentry.typ=varsym) and
                      (vo_is_self in tvarsym(tloadnode(p).symtableentry).varoptions);
      end;


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
        ppufile.putderef(symtableentry,symtableentryderef);
        ppufile.putderef(procdef,procdefderef);
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


    function tloadnode.getcopy : tnode;
      var
         n : tloadnode;

      begin
         n:=tloadnode(inherited getcopy);
         n.symtable:=symtable;
         n.symtableentry:=symtableentry;
         result:=n;
      end;


    function tloadnode.det_resulttype:tnode;
      begin
         result:=nil;
         { handle first absolute as it will replace the symtableentry }
         if symtableentry.typ=absolutesym then
           begin
             { force the resulttype to the type of the absolute }
             resulttype:=tabsolutesym(symtableentry).vartype;
             { replace the symtableentry when it points to a var, else
               we are finished }
             if (tabsolutesym(symtableentry).abstyp=tovar) then
              begin
                symtableentry:=tabsolutesym(symtableentry).ref;
                symtable:=symtableentry.owner;
                include(flags,nf_absolute);
              end
             else
              exit;
           end;
         case symtableentry.typ of
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   resulttype:=cansistringtype
                 else
                   internalerror(22799);
              end;
            varsym :
              begin
                inc(tvarsym(symtableentry).refs);
                { Nested variable? The we need to load the framepointer of
                  the parent procedure }
                if (symtable.symtabletype in [localsymtable,parasymtable]) and
                   (symtable.symtablelevel<>current_procinfo.procdef.parast.symtablelevel) then
                  begin
                    if assigned(left) then
                      internalerror(200309289);
                    left:=cloadparentfpnode.create(tprocdef(symtable.defowner));
                  end;
                { if it's refered by absolute then it's used }
                if nf_absolute in flags then
                  tvarsym(symtableentry).varstate:=vs_used
                else
                  begin
                    { fix self type which is declared as voidpointer in the
                      definition }
                    if vo_is_self in tvarsym(symtableentry).varoptions then
                      begin
                        resulttype.setdef(tprocdef(symtableentry.owner.defowner)._class);
                        if (po_classmethod in tprocdef(symtableentry.owner.defowner).procoptions) or
                           (po_staticmethod in tprocdef(symtableentry.owner.defowner).procoptions) then
                          resulttype.setdef(tclassrefdef.create(resulttype))
                        else if is_object(resulttype.def) and
                                (nf_load_self_pointer in flags) then
                          resulttype.setdef(tpointerdef.create(resulttype));
                      end
                    else if vo_is_vmt in tvarsym(symtableentry).varoptions then
                      begin
                        resulttype.setdef(tprocdef(symtableentry.owner.defowner)._class);
                        resulttype.setdef(tclassrefdef.create(resulttype));
                      end
                    else
                      resulttype:=tvarsym(symtableentry).vartype;
                  end;
              end;
            typedconstsym :
                if not(nf_absolute in flags) then
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
         registers32:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         case symtableentry.typ of
            absolutesym :
              ;
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   begin
                      include(current_procinfo.flags,pi_needs_implicit_finally);
                      expectloc:=LOC_CREFERENCE;
                   end;
              end;
            varsym :
              begin
                if assigned(left) then
                  firstpass(left);
                if (tvarsym(symtableentry).varspez=vs_const) then
                  expectloc:=LOC_CREFERENCE;
                { we need a register for call by reference parameters }
                if paramanager.push_addr_param(tvarsym(symtableentry).varspez,tvarsym(symtableentry).vartype.def,pocall_default) then
                  registers32:=1;
                if ([vo_is_thread_var,vo_is_dll_var]*tvarsym(symtableentry).varoptions)<>[] then
                  registers32:=1;
                { call to get address of threadvar }
                if (vo_is_thread_var in tvarsym(symtableentry).varoptions) then
                  include(current_procinfo.flags,pi_do_call);
                if nf_write in flags then
                  Tvarsym(symtableentry).trigger_notifications(vn_onwrite)
                else
                  Tvarsym(symtableentry).trigger_notifications(vn_onread);
                { count variable references }
                if rg.t_times>1 then
                  inc(tvarsym(symtableentry).refs,rg.t_times-1);
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
                        registers32:=max(registers32,left.registers32);
                        registersfpu:=max(registersfpu,left.registersfpu);
 {$ifdef SUPPORT_MMX}
                        registersmmx:=max(registersmmx,left.registersmmx);
 {$endif SUPPORT_MMX}
                     end;
                end;
           else
             internalerror(200104143);
         end;
      end;


    function tloadnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (symtableentry = tloadnode(p).symtableentry) and
          (symtable = tloadnode(p).symtable);
      end;


    procedure Tloadnode.printnodedata(var t:text);
      begin
        inherited printnodedata(t);
        writeln(t,printnodeindention,'symbol = ',symtableentry.name);
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


    function tassignmentnode.getcopy : tnode;

      var
         n : tassignmentnode;

      begin
         n:=tassignmentnode(inherited getcopy);
         n.assigntype:=assigntype;
         getcopy:=n;
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
                if (is_char(tbinarynode(right).right.resulttype.def) or
                   is_shortstring(tbinarynode(right).right.resulttype.def) or
                   is_ansistring(tbinarynode(right).right.resulttype.def)) then
                  begin
                    { remove property flag so it'll not trigger an error }
                    exclude(left.flags,nf_isproperty);
                    { generate call to helper }
                    hp:=ccallparanode.create(tbinarynode(right).right,
                      ccallparanode.create(left,nil));
                    if is_char(tbinarynode(right).right.resulttype.def) then
                      result:=ccallnode.createintern('fpc_ansistr_append_char',hp)
                    else if is_shortstring(tbinarynode(right).right.resulttype.def) then
                      result:=ccallnode.createintern('fpc_ansistr_append_shortstring',hp)
                    else
                      result:=ccallnode.createintern('fpc_ansistr_append_ansistring',hp);
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
        set_varstate(left,vs_assigned,false);
        set_varstate(right,vs_used,true);
        if codegenerror then
          exit;

        { assignments to formaldefs and open arrays aren't allowed }
        if (left.resulttype.def.deftype=formaldef) or
           is_open_array(left.resulttype.def) then
          CGMessage(type_e_operator_not_allowed);

        { test if node can be assigned, properties are allowed }
        valid_for_assignment(left);

        { assigning nil to a dynamic array clears the array }
        if is_dynamic_array(left.resulttype.def) and
           (right.nodetype=niln) then
         begin
           hp:=ccallparanode.create(caddrnode.create
                   (crttinode.create(tstoreddef(left.resulttype.def),initrtti)),
               ccallparanode.create(ctypeconvnode.create_explicit(left,voidpointertype),nil));
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
           if assigned(right.resulttype.def) and (right.nodetype in [loadn,vecn,calln]) then
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
              (left.resulttype.def.deftype in [enumdef,orddef,floatdef]) then
              begin
                if (original_size <> 0) and (left.resulttype.def.size < original_size) then
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
           hp:=ccallparanode.create(ctypeconvnode.create_explicit
                   (right,voidpointertype),
               ccallparanode.create(ctypeconvnode.create_explicit
                   (left,voidpointertype),nil));
           result:=ccallnode.createintern('fpc_intf_assign',hp);
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


         registers32:=left.registers32+right.registers32;
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
        set_varstate(left,vs_used,true);
        set_varstate(right,vs_used,true);
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


    function tarrayconstructornode.getcopy : tnode;
      var
         n : tarrayconstructornode;
      begin
         n:=tarrayconstructornode(inherited getcopy);
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
              set_varstate(hp.left,vs_used,true);
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
         if not assigned(htype.def) then
          htype:=voidtype;
         resulttype.setdef(tarraydef.create(0,len-1,s32bittype));
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
        thp,
        chp,
        hp        : tarrayconstructornode;
        dovariant : boolean;
        htype     : ttype;
        orgflags  : tnodeflags;
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
                     hp.left:=ctypeconvnode.create_explicit(hp.left,s32bittype);
                   arraydef :
                     hp.left:=ctypeconvnode.create(hp.left,charpointertype);
                   orddef :
                     begin
                       if is_integer(hp.left.resulttype.def) and
                          not(is_64bitint(hp.left.resulttype.def)) then
                         hp.left:=ctypeconvnode.create(hp.left,s32bittype);
                     end;
                   floatdef :
                     begin
                       { C uses 64bit floats }
                       if nf_cargs in flags then
                         hp.left:=ctypeconvnode.create(hp.left,s64floattype)
                       else
                         hp.left:=ctypeconvnode.create(hp.left,pbestrealtype^);
                     end;
                   stringdef :
                     begin
                       if nf_cargs in flags then
                         hp.left:=ctypeconvnode.create(hp.left,charpointertype);
                     end;
                   procvardef :
                     hp.left:=ctypeconvnode.create(hp.left,voidpointertype);
                   variantdef,
                   pointerdef,
                   classrefdef,
                   objectdef : ;
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
      begin
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
                firstpass(hp.left);
                hp:=tarrayconstructornode(hp.right);
              end;
          end;
        expectloc:=LOC_CREFERENCE;
        calcregisters(self,0,0,0);
      end;


    function tarrayconstructornode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p);
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
          CGMessage(cg_e_illegal_expression);
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
        ppufile.putderef(rttidef,rttidefderef);
        ppufile.putbyte(byte(rttitype));
      end;


    procedure trttinode.derefimpl;
      begin
        inherited derefimpl;
        rttidef:=tstoreddef(rttidefderef.resolve);
      end;


    function trttinode.getcopy : tnode;
      var
         n : trttinode;
      begin
         n:=trttinode(inherited getcopy);
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


    procedure trttinode.pass_2;
      begin
        location_reset(location,LOC_CREFERENCE,OS_NO);
        location.reference.symbol:=rttidef.get_rtti_label(rttitype);
      end;


begin
   cloadnode:=tloadnode;
   cassignmentnode:=tassignmentnode;
   carrayconstructorrangenode:=tarrayconstructorrangenode;
   carrayconstructornode:=tarrayconstructornode;
   ctypenode:=ttypenode;
   crttinode:=trttinode;
end.
{
  $Log$
  Revision 1.110  2003-10-08 19:19:45  peter
    * set_varstate cleanup

  Revision 1.109  2003/10/05 21:21:52  peter
    * c style array of const generates callparanodes
    * varargs paraloc fixes

  Revision 1.108  2003/10/01 20:34:48  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.107  2003/09/28 17:55:03  peter
    * parent framepointer changed to hidden parameter
    * tloadparentfpnode added

  Revision 1.106  2003/09/23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.105  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.104  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.103  2003/07/08 15:20:56  peter
    * don't allow add/assignments for formaldef
    * formaldef size changed to 0

  Revision 1.102  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.101  2003/06/08 20:01:53  jonas
    * optimized assignments with on the right side a function that returns
      an ansi- or widestring

  Revision 1.100  2003/06/08 18:27:15  jonas
    + ability to change the location of a ttempref node with changelocation()
      method. Useful to use instead of copying the contents from one temp to
      another
    + some shortstring optimizations in tassignmentnode that avoid some
      copying (required some shortstring optimizations to be moved from
      resulttype to firstpass, because they work on callnodes and string
      addnodes are only changed to callnodes in the firstpass)
    * allow setting/changing the funcretnode of callnodes after the
      resulttypepass has been done, funcretnode is now a property
    (all of the above should have a quite big effect on callparatemp)

  Revision 1.99  2003/06/07 20:26:32  peter
    * re-resolving added instead of reloading from ppu
    * tderef object added to store deref info for resolving

  Revision 1.98  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.97  2003/06/07 14:39:18  jonas
    * set pi_do_call for accesses to threadvars

  Revision 1.96  2003/05/26 19:38:28  peter
    * generic fpc_shorstr_concat
    + fpc_shortstr_append_shortstr optimization

  Revision 1.95  2003/05/23 17:05:13  peter
    * loadn procsym need to return procdef

  Revision 1.94  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.93  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.92  2003/05/11 14:45:12  peter
    * tloadnode does not support objectsymtable,withsymtable anymore
    * withnode cleanup
    * direct with rewritten to use temprefnode

  Revision 1.91  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.90  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.89  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.88  2003/04/26 00:28:42  peter
    * removed load_funcret

  Revision 1.87  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.86  2003/04/23 20:16:04  peter
    + added currency support based on int64
    + is_64bit for use in cg units instead of is_64bitint
    * removed cgmessage from n386add, replace with internalerrors

  Revision 1.85  2003/04/23 10:10:54  peter
    * procvar is not compared in addrn

  Revision 1.84  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.83  2003/04/11 15:01:23  peter
    * fix bug 2438

  Revision 1.82  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.81  2003/03/11 21:46:24  jonas
    * lots of new regallocator fixes, both in generic and ppc-specific code
      (ppc compiler still can't compile the linux system unit though)

  Revision 1.80  2003/01/07 16:52:58  jonas
    * fixed ansistring+char and ansistring+shortstring optimizations (those
      cases were always handled as ansistring+ansistring due to
      typeconversions inserted by the add-node)

  Revision 1.79  2003/01/05 22:44:14  peter
    * remove a lot of code to support typen in loadn-procsym

  Revision 1.78  2003/01/03 12:15:56  daniel
    * Removed ifdefs around notifications
      ifdefs around for loop optimizations remain

  Revision 1.77  2002/12/31 09:55:58  daniel
   + Notification implementation complete
   + Add for loop code optimization using notifications
     results in 1.5-1.9% speed improvement in nestloop benchmark
     Optimization incomplete, compiler does not cycle yet with
     notifications enabled.

  Revision 1.76  2002/12/30 22:44:53  daniel
  * Some work on notifications

  Revision 1.75  2002/12/27 15:27:25  peter
    * remove property indicator when calling internal helpers

  Revision 1.74  2002/12/24 16:53:19  peter
    * fix for tb0438

  Revision 1.73  2002/12/20 18:14:53  peter
    * fix result of high_tree when high was not available

  Revision 1.72  2002/12/17 22:19:33  peter
    * fixed pushing of records>8 bytes with stdcall
    * simplified hightree loading

  Revision 1.71  2002/12/07 14:27:07  carl
    * 3% memory optimization
    * changed some types
    + added type checking with different size for call node and for
       parameters

  Revision 1.70  2002/12/02 19:38:06  carl
    * fix some errors

  Revision 1.69  2002/11/29 20:02:44  carl
   * warning / hint for possible loss of data in assignment

  Revision 1.68  2002/11/27 20:04:39  peter
    * cdecl array of const fixes

  Revision 1.67  2002/11/27 15:33:47  peter
    * the never ending story of tp procvar hacks

  Revision 1.66  2002/11/25 17:43:20  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.65  2002/11/18 17:31:57  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.64  2002/11/15 01:58:52  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.63  2002/10/17 12:44:09  florian
    + s:=s+<string type> where s is an ansistring is done via calls to append_ansistring_*

  Revision 1.62  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.61  2002/10/03 21:26:08  carl
    + compile-time range checking for strings

  Revision 1.60  2002/09/27 21:13:28  carl
    * low-highval always checked if limit ober 2GB is reached (to avoid overflow)

  Revision 1.59  2002/09/26 15:02:05  florian
    + support of passing variants to "array of const"

  Revision 1.58  2002/09/07 15:25:03  peter
    * old logs removed and tabs fixed

  Revision 1.57  2002/09/03 16:26:26  daniel
    * Make Tprocdef.defs protected

  Revision 1.56  2002/09/01 13:28:37  daniel
   - write_access fields removed in favor of a flag

  Revision 1.55  2002/09/01 08:01:16  daniel
   * Removed sets from Tcallnode.det_resulttype
   + Added read/write notifications of variables. These will be usefull
     for providing information for several optimizations. For example
     the value of the loop variable of a for loop does matter is the
     variable is read after the for loop, but if it's no longer used
     or written, it doesn't matter and this can be used to optimize
     the loop code generation.

  Revision 1.54  2002/08/25 19:25:19  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.53  2002/08/19 19:36:43  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.52  2002/08/18 20:06:23  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.51  2002/08/17 22:09:46  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.50  2002/08/17 09:23:37  florian
    * first part of procinfo rewrite

  Revision 1.49  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.48  2002/07/20 07:44:37  daniel
  * Forgot to add a $ifdef extdebug

  Revision 1.47  2002/07/19 12:55:27  daniel
  * Further developed state tracking in whilerepeatn

  Revision 1.46  2002/07/19 11:41:36  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.45  2002/07/15 18:03:15  florian
    * readded removed changes

  Revision 1.43  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.44  2002/07/14 18:00:44  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

  Revision 1.42  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.41  2002/05/16 19:46:38  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.39  2002/05/12 16:53:07  peter
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

  Revision 1.38  2002/04/25 20:16:39  peter
    * moved more routines from cga/n386util

  Revision 1.37  2002/04/23 19:16:34  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

}
