{
    Common subexpression elimination on base blocks

    Copyright (c) 2005 by Florian Klaempfl

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
unit optcse;

{$i fpcdefs.inc}

{ $define csedebug}
{ $define csestats}

  interface

    uses
      node;

    {
      the function  creates non optimal code so far:
      - call para nodes are cse barriers because they can be reordered and thus the
        temp. creation can be done too late
      - cse's in chained expressions are not recognized: the common subexpression
        in (a1 and b and c) vs. (a2 and b and c) is not recognized because there is no common
        subtree b and c
      - the cse knows nothing about register pressure. In case of high register pressure, cse might
        have a negative impact
      - assignment nodes are currently cse borders: things like a[i,j]:=a[i,j]+1; are not improved
      - the list of cseinvariant node types and inline numbers is not complete yet

      Further, it could be done probably in a faster way though the complexity can't probably not reduced
    }
    function do_optcse(var rootnode : tnode) : tnode;

  implementation

    uses
      globtype,
      cclasses,
      verbose,
      nutils,
      nbas,nld,ninl,ncal,ncnv,nadd,
      pass_1,
      symconst,symtype,symdef,
      defutil,
      optbase;

    const
      cseinvariant : set of tnodetype = [loadn,addn,muln,subn,divn,slashn,modn,andn,orn,xorn,notn,vecn,
        derefn,equaln,unequaln,ltn,gtn,lten,gten,typeconvn,subscriptn,
        inn,symdifn,shrn,shln,ordconstn,realconstn,unaryminusn,pointerconstn,stringconstn,setconstn,
        isn,asn,starstarn,nothingn,temprefn {,callparan}];

    function searchsubdomain(var n:tnode; arg: pointer) : foreachnoderesult;
      begin
        if (n.nodetype in cseinvariant) or
          ((n.nodetype=inlinen) and
            (tinlinenode(n).inlinenumber in [in_assigned_x])
          ) then
          result:=fen_true
        else
          begin
            pboolean(arg)^:=false;
            result:=fen_norecurse_true;
          end;
      end;

    type
      tlists = record
        nodelist : tfplist;
        locationlist : tfplist;
        equalto : tfplist;
        refs : tfplist;
        avail : TDFASet;
      end;

      plists = ^tlists;

    { collectnodes needs the address of itself to call foreachnodestatic,
      so we need a wrapper because @<func> inside <func doesn't work }

    function collectnodes(var n:tnode; arg: pointer) : foreachnoderesult;forward;

    function collectnodes2(var n:tnode; arg: pointer) : foreachnoderesult;
      begin
        result:=collectnodes(n,arg);
      end;

    function collectnodes(var n:tnode; arg: pointer) : foreachnoderesult;

      procedure AddAvail(child : tnode);
        begin
          if assigned(child) and assigned(child.optinfo) then
            DFASetIncludeSet(n.optinfo^.avail,child.optinfo^.avail);
        end;

      var
        i,j : longint;
      begin
        result:=fen_false;
        { don't add the tree below an untyped const parameter: there is
          no information available that this kind of tree actually needs
          to be addresable, this could be improved }
        if ((n.nodetype=callparan) and
          (tcallparanode(n).left.resultdef.typ=formaldef) and
          (tcallparanode(n).parasym.varspez=vs_const)) then
          begin
            result:=fen_norecurse_false;
            exit;
          end;
        { so far, we can handle only nodes being read }
        if (n.flags*[nf_write,nf_modify]=[]) and
          { node possible to add? }
          assigned(n.resultdef) and (tstoreddef(n.resultdef).is_intregable or tstoreddef(n.resultdef).is_fpuregable) and
          { is_int/fpuregable allows arrays and records to be in registers, cse cannot handle this }
          not(n.resultdef.typ in [arraydef,recorddef]) and
          { adding tempref nodes is worthless but their complexity is probably <= 1 anyways }
          not(n.nodetype in [temprefn]) and

          { node worth to add?

            We consider every node because even loading a variables from
            a register instead of memory is more beneficial. This behaviour should
            not increase register pressure because if a variable is already
            in a register, the reg. allocator can merge the nodes. If a variable
            is loaded from memory, loading this variable and spilling another register
            should not add a speed penalty.

            Const nodes however are only considered if their complexity is >1
            This might be the case for the risc architectures if they need
            more than one instruction to load this particular value
          }

          (not(is_constnode(n)) or (node_complexity(n)>1)) then
          begin
            plists(arg)^.nodelist.Add(n);
            plists(arg)^.locationlist.Add(@n);
            plists(arg)^.refs.Add(nil);
            plists(arg)^.equalto.Add(pointer(-1));

            {
            { setup set of available expressions }
            n.allocoptinfo;
            n.optinfo^.avail:=nil;
            DFASetInclude(n.optinfo^.avail,plists(arg)^.nodelist.count-1);
            }
            DFASetInclude(plists(arg)^.avail,plists(arg)^.nodelist.count-1);

            for i:=0 to plists(arg)^.nodelist.count-2 do
              begin
                if tnode(plists(arg)^.nodelist[i]).isequal(n) and DFASetIn(plists(arg)^.avail,i) then
                  begin
                    { use always the first occurence }
                    if ptrint(plists(arg)^.equalto[i])<>-1 then
                      plists(arg)^.equalto[plists(arg)^.nodelist.count-1]:=plists(arg)^.equalto[i]
                    else
                      plists(arg)^.equalto[plists(arg)^.nodelist.count-1]:=pointer(i);
                    plists(arg)^.refs[i]:=pointer(plists(arg)^.refs[i])+1;
                    break;
                  end;
              end;

            { boolean and/or require a special handling: after evaluating the and/or node,
              the expressions of the right side might not be available due to short boolean
              evaluation, so after handling the right side, mark those expressions
              as unavailable }
            if (n.nodetype in [orn,andn]) and is_boolean(taddnode(n).left.resultdef) then
              begin
                foreachnodestatic(pm_postprocess,taddnode(n).left,@collectnodes2,arg);
                j:=plists(arg)^.nodelist.count;
                foreachnodestatic(pm_postprocess,taddnode(n).right,@collectnodes2,arg);
                for i:=j to plists(arg)^.nodelist.count-1 do
                  DFASetExclude(plists(arg)^.avail,i);
                result:=fen_norecurse_false;
              end;
          end;
      end;


    function searchcsedomain(var n: tnode; arg: pointer) : foreachnoderesult;
      var
        csedomain : boolean;
        lists : tlists;
        templist : tfplist;
        i : longint;
        def : tstoreddef;
        nodes : tblocknode;
        creates,
        statements : tstatementnode;
        hp : ttempcreatenode;
      begin
        result:=fen_false;
        if n.nodetype in cseinvariant then
          begin
            csedomain:=true;
            foreachnodestatic(pm_postprocess,n,@searchsubdomain,@csedomain);
            { found a cse domain }
            if csedomain then
              begin
                statements:=nil;
                result:=fen_norecurse_true;
{$ifdef csedebug}
                writeln('============ cse domain ==================');
                printnode(output,n);
                writeln('Complexity: ',node_complexity(n));
{$endif csedebug}
                lists.nodelist:=tfplist.create;
                lists.locationlist:=tfplist.create;
                lists.equalto:=tfplist.create;
                lists.refs:=tfplist.create;
                foreachnodestatic(pm_postprocess,n,@collectnodes,@lists);

                templist:=tfplist.create;
                templist.count:=lists.nodelist.count;

                { check all nodes if one is used more than once }
                for i:=0 to lists.nodelist.count-1 do
                  begin
                    { current node used more than once? }
                    if ptrint(lists.refs[i])<>0 then
                      begin
                        if not(assigned(statements)) then
                          begin
                            nodes:=internalstatements(statements);
                            addstatement(statements,internalstatements(creates));
                          end;

                        def:=tstoreddef(tnode(lists.nodelist[i]).resultdef);
                        templist[i]:=ctempcreatenode.create_value(def,def.size,tt_persistent,
                          def.is_intregable or def.is_fpuregable,tnode(lists.nodelist[i]));
                        addstatement(creates,tnode(templist[i]));

                        hp:=ttempcreatenode(templist[i]);
                        do_firstpass(tnode(hp));
                        templist[i]:=hp;

                        pnode(lists.locationlist[i])^:=ctemprefnode.create(ttempcreatenode(templist[i]));
                        do_firstpass(pnode(lists.locationlist[i])^);
{$ifdef csedebug}
                        printnode(output,statements);
{$endif csedebug}
                      end
                    { current node reference to another node? }
                    else if ptrint(lists.equalto[i])<>-1 then
                      begin
{$if defined(csedebug) or defined(csestats)}
                        printnode(output,tnode(lists.nodelist[i]));
                        writeln(i,'    equals   ',ptrint(lists.equalto[i]));
                        printnode(output,tnode(lists.nodelist[ptrint(lists.equalto[i])]));
{$endif defined(csedebug) or defined(csestats)}
                        templist[i]:=templist[ptrint(lists.equalto[i])];
                        pnode(lists.locationlist[i])^:=ctemprefnode.create(ttempcreatenode(templist[ptrint(lists.equalto[i])]));
                        do_firstpass(pnode(lists.locationlist[i])^);
                      end;
                  end;
                { clean up unused trees }
                for i:=0 to lists.nodelist.count-1 do
                  if ptrint(lists.equalto[i])<>-1 then
                    tnode(lists.nodelist[i]).free;
{$ifdef csedebug}
                writeln('nodes: ',lists.nodelist.count);
                writeln('==========================================');
{$endif csedebug}
                lists.nodelist.free;
                lists.locationlist.free;
                lists.equalto.free;
                lists.refs.free;
                templist.free;

                if assigned(statements) then
                  begin
                    { call para nodes need a special handling because
                      they can be only children nodes of call nodes
                      so the initialization code is inserted below the
                      call para node
                    }
                    if n.nodetype=callparan then
                      begin
                        addstatement(statements,tcallparanode(n).left);
                        tcallparanode(n).left:=nodes;
                        do_firstpass(tcallparanode(n).left);
                      end
                    else
                      begin
                        addstatement(statements,n);
                        n:=nodes;
                        do_firstpass(n);
                      end;
{$ifdef csedebug}
                    printnode(output,nodes);
{$endif csedebug}
                  end;
              end
          end;
      end;


    function do_optcse(var rootnode : tnode) : tnode;
      begin
        foreachnodestatic(pm_postprocess,rootnode,@searchcsedomain,nil);
        result:=nil;
(*
        { create a linear list of nodes }

        { create hash values }

        { sort by hash values, taking care of nf_csebarrier and keeping the
          original order of the nodes }

        { compare nodes with equal hash values }

        { search barrier }
        for i:=0 to nodelist.length-1 do
          begin
            { and then search backward so we get always the largest equal trees }
            j:=i+1;
            { collect equal nodes }
            while (j<=nodelist.length-1) and
              nodelist[i].isequal(nodelist[j]) do
              inc(j);
            dec(j);
            if j>i then
              begin
                { cse found }

                { create temp. location }

                { replace first node by
                  - temp. creation
                  - expression calculation
                  - assignment of expression to temp. }
                tempnode:=ctempcreatenode.create(nodelist[i].resultdef,nodelist[i].resultdef.size,tt_persistent,
                  nodelist[i].resultdef.is_intregable or nodelist[i].resultdef.is_fpuregable);
                addstatement(createstatement,tempnode);
                addstatement(createstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                      caddrnode.create_internal(para.left)));
                    para.left := ctypeconvnode.create_internal(cderefnode.create(ctemprefnode.create(tempnode)),para.left.resultdef);
                    addstatement(deletestatement,ctempdeletenode.create(tempnode));

                { replace next nodes by loading the temp. reference }

                { replace last node by loading the temp. reference and
                  delete the temp. }
              end;
          end;
*)
      end;

end.
