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
          symtable : tsymtable;
          procdef : tprocdef;
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
       {$ifdef extdebug}
          procedure _dowrite;override;
       {$endif}
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

       tfuncretnode = class(tnode)
          funcretsym : tfuncretsym;
          constructor create(v:tsym);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          procedure mark_write;override;
          function docompare(p: tnode): boolean; override;
       end;
       tfuncretnodeclass = class of tfuncretnode;

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
       cfuncretnode : tfuncretnodeclass;
       carrayconstructorrangenode : tarrayconstructorrangenodeclass;
       carrayconstructornode : tarrayconstructornodeclass;
       ctypenode : ttypenodeclass;
       crttinode : trttinodeclass;


    procedure load_procvar_from_calln(var p1:tnode);
    function load_high_value(vs:tvarsym):tnode;


implementation

    uses
      cutils,verbose,globtype,globals,systems,
      symtable,paramgr,defutil,defcmp,
      htypechk,pass_1,
      ncon,ninl,ncnv,nmem,ncal,cpubase,rgobj,cginfo,cgbase
      ,symnot
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


    function load_high_value(vs:tvarsym):tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        srsymtable:=vs.owner;
        if vo_is_local_copy in vs.varoptions then
         begin
           { next symtable is always the para symtable }
           srsymtable:=srsymtable.next;
           if not(srsymtable.symtabletype in [parasymtable,inlineparasymtable]) then
             internalerror(200212171);
         end;
        srsym:=searchsymonlyin(srsymtable,'high'+vs.name);
        if assigned(srsym) then
          result:=cloadnode.create(srsym,srsymtable)
        else
          CGMessage(cg_e_illegal_expression);
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
        symtableentry:=tsym(ppufile.getderef);
{$ifdef fpc}
{$warning FIXME: No withsymtable support}
{$endif}
        symtable:=nil;
        procdef:=tprocdef(ppufile.getderef);
      end;


    procedure tloadnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(symtableentry);
        ppufile.putderef(procdef);
      end;


    procedure tloadnode.derefimpl;
      begin
        inherited derefimpl;
        resolvesym(pointer(symtableentry));
        symtable:=symtableentry.owner;
        resolvedef(pointer(procdef));
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
      var
        p1 : tnode;
        p  : tprocinfo;
      begin
         result:=nil;
         { optimize simple with loadings }
         if (symtable.symtabletype=withsymtable) and
            (twithsymtable(symtable).direct_with) and
            (symtableentry.typ=varsym) then
           begin
              p1:=tnode(twithsymtable(symtable).withrefnode).getcopy;
              p1:=csubscriptnode.create(tvarsym(symtableentry),p1);
              left:=nil;
              result:=p1;
              exit;
           end;
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
            funcretsym :
              begin
                { find the main funcret for the function }
                p:=procinfo;
                while assigned(p) do
                 begin
                   if assigned(p.procdef.funcretsym) and
                      ((tfuncretsym(symtableentry)=p.procdef.resultfuncretsym) or
                       (tfuncretsym(symtableentry)=p.procdef.funcretsym)) then
                     begin
                       symtableentry:=p.procdef.funcretsym;
                       break;
                     end;
                    p:=p.parent;
                  end;
                { generate funcretnode }
                p1:=cfuncretnode.create(symtableentry);
                resulttypepass(p1);
                { if it's refered as absolute then we need to have the
                  type of the absolute instead of the function return,
                  the function return is then also assigned }
                if nf_absolute in flags then
                 begin
                   tfuncretsym(symtableentry).funcretstate:=vs_assigned;
                   p1.resulttype:=resulttype;
                 end;
                left:=nil;
                result:=p1;
              end;
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   resulttype:=cansistringtype
                 else
                   internalerror(22799);
              end;
            varsym :
              begin
                { if it's refered by absolute then it's used }
                if nf_absolute in flags then
                  tvarsym(symtableentry).varstate:=vs_used
                else
                  resulttype:=tvarsym(symtableentry).vartype;
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

                   { Delphi returns Pointer as type }
                   if not(m_tp_procvar in aktmodeswitches) then
                    resulttype.setdef(procdef)
                   else
                    resulttype:=voidpointertype;

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
            funcretsym :
              internalerror(200104142);
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   begin
                      { we use ansistrings so no fast exit here }
                      if assigned(procinfo) then
                        procinfo.no_fast_exit:=true;
                      expectloc:=LOC_CREFERENCE;
                   end;
              end;
            varsym :
                begin
                  if (symtable.symtabletype in [parasymtable,localsymtable]) and
                      (lexlevel>symtable.symtablelevel) then
                     begin
                       { if the variable is in an other stackframe then we need
                         a register to dereference }
                       if (symtable.symtablelevel)>0 then
                        begin
                          registers32:=1;
                          { further, the variable can't be put into a register }
                          tvarsym(symtableentry).varoptions:=
                            tvarsym(symtableentry).varoptions-[vo_fpuregable,vo_regable];
                        end;
                     end;
                   if (tvarsym(symtableentry).varspez=vs_const) then
                     expectloc:=LOC_CREFERENCE;
                   { we need a register for call by reference parameters }
                   if (tvarsym(symtableentry).varspez in [vs_var,vs_out]) or
                      ((tvarsym(symtableentry).varspez=vs_const) and
                      paramanager.push_addr_param(tvarsym(symtableentry).vartype.def,pocall_none)) or
                      { call by value open arrays are also indirect addressed }
                      is_open_array(tvarsym(symtableentry).vartype.def) then
                     registers32:=1;
                   if symtable.symtabletype in [withsymtable,objectsymtable] then
                     inc(registers32);

                   if ([vo_is_thread_var,vo_is_dll_var]*tvarsym(symtableentry).varoptions)<>[] then
                     registers32:=1;
                    if nf_write in flags then
                      Tvarsym(symtableentry).trigger_notifications(vn_onwrite)
                    else
                      Tvarsym(symtableentry).trigger_notifications(vn_onread);
                   { count variable references }

                     { this will create problem with local var set by
                     under_procedures
                     if (assigned(tvarsym(symtableentry).owner) and assigned(aktprocsym)
                       and ((tvarsym(symtableentry).owner = aktprocdef.localst)
                       or (tvarsym(symtableentry).owner = aktprocdef.localst))) then }
                   if rg.t_times<1 then
                     inc(tvarsym(symtableentry).refs)
                   else
                     inc(tvarsym(symtableentry).refs,rg.t_times);
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

{$ifdef extdebug}
    procedure Tloadnode._dowrite;

    begin
        inherited _dowrite;
        writeln(',');
        system.write(writenodeindention,'symbol = ',symtableentry.name);
    end;
{$endif}


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
          end;

        resulttypepass(right);
        set_varstate(left,false);
        set_varstate(right,true);
        set_funcret_is_valid(left);
        if codegenerror then
          exit;

        { assignments to open arrays aren't allowed }
        if is_open_array(left.resulttype.def) then
          CGMessage(type_e_mismatch);

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
            { test for s:=s+anything ... }
            { the problem is for
              s:=s+s+s;
              this is broken here !! }
{$ifdef newoptimizations2}
            { the above is fixed now, but still problem with s := s + f(); if }
            { f modifies s (bad programming, so only enable if uncertain      }
            { optimizations are on) (JM)                                      }
            if (cs_UncertainOpts in aktglobalswitches) then
              begin
                hp := right;
                while hp.treetype=addn do
                  hp:=hp.left;
                if left.docompare(hp) then
                  begin
                    concat_string:=true;
                    hp:=right;
                    while hp.treetype=addn do
                      begin
                        hp.use_strconcat:=true;
                        hp:=hp.left;
                      end;
                  end;
              end;
{$endif newoptimizations2}

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
              if useshelper then
               begin
                 hp:=ccallparanode.create
                         (right,
                     ccallparanode.create(cinlinenode.create
                         (in_high_x,false,left.getcopy),nil));
                 result:=ccallnode.createinternreturn('fpc_'+tstringdef(right.resulttype.def).stringtypname+'_to_shortstr',hp,left);
                 left:=nil;
                 right:=nil;
                 exit;
               end;
            end;
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


      begin
         result:=nil;
         expectloc:=LOC_VOID;

         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;



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
                                 TFUNCRETNODE
*****************************************************************************}

    constructor tfuncretnode.create(v:tsym);

      begin
         inherited create(funcretn);
         funcretsym:=tfuncretsym(v);
      end;


    constructor tfuncretnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        funcretsym:=tfuncretsym(ppufile.getderef);
      end;


    procedure tfuncretnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(funcretsym);
      end;


    procedure tfuncretnode.derefimpl;
      begin
        inherited derefimpl;
        resolvesym(pointer(funcretsym));
      end;


    function tfuncretnode.getcopy : tnode;
      var
         n : tfuncretnode;
      begin
         n:=tfuncretnode(inherited getcopy);
         n.funcretsym:=funcretsym;
         getcopy:=n;
      end;


    function tfuncretnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=funcretsym.returntype;
      end;

    procedure Tfuncretnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tfuncretnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_REFERENCE;
         if paramanager.ret_in_param(resulttype.def,tprocdef(funcretsym.owner.defowner).proccalloption) or
            (lexlevel<>funcretsym.owner.symtablelevel) then
           registers32:=1;
      end;


    function tfuncretnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (funcretsym = tfuncretnode(p).funcretsym);
      end;


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
        set_varstate(left,true);
        set_varstate(right,true);
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
              set_varstate(hp.left,true);
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


    function tarrayconstructornode.pass_1 : tnode;
      var
        thp,
        chp,
        hp        : tarrayconstructornode;
        dovariant : boolean;
        htype     : ttype;
        orgflags  : tnodeflagset;
      begin
        dovariant:=(nf_forcevaria in flags) or tarraydef(resulttype.def).isvariant;
        result:=nil;
        { only pass left tree, right tree contains next construct if any }
        if assigned(left) then
         begin
           hp:=self;
           while assigned(hp) do
            begin
              firstpass(hp.left);
              { Insert typeconvs for array of const }
              if dovariant then
               begin
                 case hp.left.resulttype.def.deftype of
                   enumdef :
                     begin
                       hp.left:=ctypeconvnode.create_explicit(hp.left,s32bittype);
                       firstpass(hp.left);
                     end;
                   arraydef :
                     begin
                       hp.left:=ctypeconvnode.create(hp.left,charpointertype);
                       firstpass(hp.left);
                     end;
                   orddef :
                     begin
                       if is_integer(hp.left.resulttype.def) and
                         not(is_64bitint(hp.left.resulttype.def)) then
                        begin
                          hp.left:=ctypeconvnode.create(hp.left,s32bittype);
                          firstpass(hp.left);
                        end;
                     end;
                   floatdef :
                     begin
                       { C uses 64bit floats }
                       if nf_cargs in flags then
                         hp.left:=ctypeconvnode.create(hp.left,s64floattype)
                       else
                         hp.left:=ctypeconvnode.create(hp.left,pbestrealtype^);
                       firstpass(hp.left);
                     end;
                   stringdef :
                     begin
                       if nf_cargs in flags then
                        begin
                          hp.left:=ctypeconvnode.create(hp.left,charpointertype);
                          firstpass(hp.left);
                        end;
                     end;
                   procvardef :
                     begin
                       hp.left:=ctypeconvnode.create(hp.left,voidpointertype);
                       firstpass(hp.left);
                     end;
                   variantdef,
                   pointerdef,
                   classrefdef,
                   objectdef : ;
                   else
                     CGMessagePos1(hp.left.fileinfo,type_e_wrong_type_in_array_constructor,hp.left.resulttype.def.typename);
                 end;
               end;
              hp:=tarrayconstructornode(hp.right);
            end;
         { swap the tree for cargs }
           if (nf_cargs in flags) and (not(nf_cargswap in flags)) then
            begin
              chp:=nil;
              { save resulttype }
              htype:=resulttype;
              { we need a copy here, because self is destroyed }
              { by firstpass later                             }
              hp:=tarrayconstructornode(getcopy);
              { we also need a copy of the nf_ forcevaria flag to restore }
              { later) (JM)                                               }
              orgflags := flags * [nf_forcevaria];
              while assigned(hp) do
               begin
                 thp:=tarrayconstructornode(hp.right);
                 hp.right:=chp;
                 chp:=hp;
                 hp:=thp;
               end;
              chp.flags := chp.flags+orgflags;
              include(chp.flags,nf_cargs);
              include(chp.flags,nf_cargswap);
              chp.expectloc:=LOC_CREFERENCE;
              calcregisters(chp,0,0,0);
              chp.resulttype:=htype;
              result:=chp;
              exit;
            end;
         end;
        { C style has pushed everything on the stack, so
          there is no return value }
        if (nf_cargs in flags) then
         expectloc:=LOC_VOID
        else
         expectloc:=LOC_CREFERENCE;
        { Calculate registers }
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
        rttidef:=tstoreddef(ppufile.getderef);
        rttitype:=trttitype(ppufile.getbyte);
      end;


    procedure trttinode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(rttidef);
        ppufile.putbyte(byte(rttitype));
      end;


    procedure trttinode.derefimpl;
      begin
        inherited derefimpl;
        resolvedef(pointer(rttidef));
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
   cfuncretnode:=tfuncretnode;
   carrayconstructorrangenode:=tarrayconstructorrangenode;
   carrayconstructornode:=tarrayconstructornode;
   ctypenode:=ttypenode;
   crttinode:=trttinode;
end.
{
  $Log$
  Revision 1.85  2003-04-23 10:10:54  peter
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
