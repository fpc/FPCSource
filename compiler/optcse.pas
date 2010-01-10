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
      nbas,nld,ninl,
      pass_1,
      symtype,symdef;

    const
      cseinvariant : set of tnodetype = [loadn,addn,muln,subn,divn,slashn,modn,andn,orn,xorn,notn,vecn,
        derefn,equaln,unequaln,ltn,gtn,lten,gten,typeconvn,subscriptn,
        inn,symdifn,shrn,shln,ordconstn,realconstn,unaryminusn,pointerconstn,stringconstn,setconstn,
        isn,asn,starstarn,nothingn,temprefn,callparan];

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
      end;

      plists = ^tlists;

    function collectnodes(var n:tnode; arg: pointer) : foreachnoderesult;
      var
        i : longint;
      begin
        result:=fen_false;
        { node worth to add? }
        if (node_complexity(n)>1) and (tstoreddef(n.resultdef).is_intregable or tstoreddef(n.resultdef).is_fpuregable) and
          { adding tempref nodes is worthless but their complexity is probably <= 1 anyways }
          not(n.nodetype in [temprefn]) then
          begin
            plists(arg)^.nodelist.Add(n);
            plists(arg)^.locationlist.Add(@n);
            plists(arg)^.refs.Add(nil);
            plists(arg)^.equalto.Add(pointer(-1));
            for i:=0 to plists(arg)^.nodelist.count-2 do
              begin
                if tnode(plists(arg)^.nodelist[i]).isequal(n) then
                  begin
                    { use always the first occurence }
                    if ptrint(plists(arg)^.equalto[i])<>-1 then
                      plists(arg)^.equalto[plists(arg)^.nodelist.count-1]:=plists(arg)^.equalto[i]
                    else
                      plists(arg)^.equalto[plists(arg)^.nodelist.count-1]:=pointer(i);
                    plists(arg)^.refs[i]:=pointer(plists(arg)^.refs[i])+1;
                    exit;
                  end;
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
                    addstatement(statements,n);
                    n:=nodes;
                    do_firstpass(n);
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
