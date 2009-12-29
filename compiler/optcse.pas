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
      the function is not ready for use yet
      - when handling short boolean evaluation, the cse's are evaluated before the
        whole expression, this is wrong and can lead to crashes

      further, it creates non optimal code:
      - nested cse's in another cse are not recognized
      - cse's in chained expressions are not recognized: the common subexpression
        in (a1 and b and c) vs. (a2 and b and c) is not recognized because there is no common
        subtree b and c
      - the cse knows nothing about register pressure. In case of high register pressure, cse might
        have a negative impact

      - it could be done probably in a faster way, currently the complexity is approx. O(n^4)
    }
    function do_optcse(var rootnode : tnode) : tnode;

  implementation

    uses
      globtype,
      cclasses,
      verbose,
      nutils,
      nbas,nld,
      pass_1,
      symtype,symdef;

    const
      cseinvariant : set of tnodetype = [loadn,addn,muln,subn,divn,slashn,modn,andn,orn,xorn,notn,vecn,
        derefn,equaln,unequaln,ltn,gtn,lten,gten,typeconvn,subscriptn,
        inn,symdifn,shrn,shln,ordconstn,realconstn,unaryminusn,pointerconstn,stringconstn,setconstn,
        isn,asn,starstarn,nothingn,temprefn];

    function searchsubdomain(var n:tnode; arg: pointer) : foreachnoderesult;
      begin
        if not(n.nodetype in cseinvariant) then
          begin
            pboolean(arg)^:=false;
            result:=fen_norecurse_true;
          end
        else
          result:=fen_true;
      end;

    type
      tlists = record
        nodelist : tfplist;
        locationlist : tfplist;
      end;

      plists = ^tlists;

    function collectnodes(var n:tnode; arg: pointer) : foreachnoderesult;
      begin
        result:=fen_false;
        { node worth to add? }
        if (node_complexity(n)>1) and (tstoreddef(n.resultdef).is_intregable or tstoreddef(n.resultdef).is_fpuregable) and
          { adding tempref nodes is worthless but their complexity is probably <= 1 anyways }
          not(n.nodetype in [temprefn]) then
          begin
            plists(arg)^.nodelist.Add(n);
            plists(arg)^.locationlist.Add(@n);
          end;
        {
        else
          result:=fen_norecurse_false;
        }
      end;


    function searchcsedomain(var n: tnode; arg: pointer) : foreachnoderesult;
      var
        restart : boolean;
        csedomain : boolean;
        lists : tlists;
        templist : tfplist;
        i,j : longint;
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
                repeat
                  lists.nodelist:=tfplist.create;
                  lists.locationlist:=tfplist.create;
                  foreachnodestatic(pm_postprocess,n,@collectnodes,@lists);

                  templist:=tfplist.create;
                  templist.count:=lists.nodelist.count;

                  restart:=false;

                  { this is poorly coded, just comparing every node with all other nodes }

                  { the nodes are sorted by size so we'll find the largest matching tree
                    first }
                  for i:=0 to lists.nodelist.count-1 do
                    begin
                      for j:=lists.nodelist.count-1 downto i+1 do
                        begin
                        {
                          writeln(i);
                          writeln(j);
                          writeln(dword(tnode(lists.nodelist[i]).nodetype));
                          writeln(dword(tnode(lists.nodelist[j]).nodetype)); }
                          if not(tnode(lists.nodelist[i]).nodetype in [tempcreaten,temprefn]) and
                            not(tnode(lists.nodelist[j]).nodetype in [tempcreaten,temprefn]) and
                            tnode(lists.nodelist[i]).isequal(tnode(lists.nodelist[j])) then
                            begin
                              if not(assigned(statements)) then
                                begin
                                  nodes:=internalstatements(statements);
                                  addstatement(statements,internalstatements(creates));
                                end;
{$if defined(csedebug) or defined(csestats)}
                              writeln('    ====     ');
                              printnode(output,tnode(lists.nodelist[i]));
                              writeln('    equals   ');
                              printnode(output,tnode(lists.nodelist[j]));
                              writeln('    ====     ');
{$endif defined(csedebug) or defined(csestats)}

                              def:=tstoreddef(tnode(lists.nodelist[i]).resultdef);
                              if assigned(def) then
                                begin
                                  restart:=true;
                                  if assigned(templist[i])  then
                                    begin
                                      templist[j]:=templist[i];
                                      pnode(lists.locationlist[j])^.free;
                                      pnode(lists.locationlist[j])^:=ctemprefnode.create(ttempcreatenode(templist[j]));
                                      do_firstpass(pnode(lists.locationlist[j])^);
                                      lists.nodelist[j]:=pnode(lists.locationlist[j])^;
                                    end
                                  else
                                    begin
                                      templist[i]:=ctempcreatenode.create(def,def.size,tt_persistent,
                                        def.is_intregable or def.is_fpuregable);
                                      addstatement(creates,tnode(templist[i]));

                                      { properties can't be passed by "var" }
                                      hp:=ttempcreatenode(templist[i]);
                                      do_firstpass(tnode(hp));

                                      addstatement(statements,cassignmentnode.create(ctemprefnode.create(ttempcreatenode(templist[i])),
                                        tnode(lists.nodelist[i]).getcopy));
                                      pnode(lists.locationlist[i])^:=ctemprefnode.create(ttempcreatenode(templist[i]));
                                      do_firstpass(pnode(lists.locationlist[i])^);

                                      templist[j]:=templist[i];

                                      pnode(lists.locationlist[j])^.free;
                                      pnode(lists.locationlist[j])^:=ctemprefnode.create(ttempcreatenode(templist[j]));
                                      do_firstpass(pnode(lists.locationlist[j])^);
                                      lists.nodelist[j]:=pnode(lists.locationlist[j])^;
{$ifdef csedebug}
                                      printnode(output,statements);
{$endif csedebug}
                                    end;
                                  end
                                else
                                  internalerror(2007091701);
                            end;
                        end;
                      { if a node in a cse domain has been replaced, we've to restart
                        searching else we could find nested trees of the replaced node
                      }
                      if restart then
                        break;
                    end;
{$ifdef csedebug}
                  writeln('nodes: ',lists.nodelist.count);
                  writeln('==========================================');
{$endif csedebug}
                  lists.nodelist.free;
                  lists.locationlist.free;
                  templist.free;
                until not(restart);
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
