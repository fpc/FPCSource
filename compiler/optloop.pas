{
    Loop optimization

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
unit optloop;

{$i fpcdefs.inc}

  interface

    uses
      node;

    function unroll_loop(node : tnode) : tnode;
    function OptimizeInductionVariables(node : tnode) : boolean;

  implementation

    uses
      cclasses,
      globtype,globals,constexp,
      symdef,symsym,
      cpuinfo,
      nutils,
      nadd,nbas,nflw,ncon,ninl,ncal,nld,
      pass_1,
      optbase,optutils,
      procinfo;

    var
      nodecount : aword;

    function donodecount(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        inc(nodecount);
        result:=fen_false;
      end;


    { rough estimation how large the tree "node" is }
    function countnodes(node : tnode) : aword;
      begin
        nodecount:=0;
        foreachnodestatic(node,@donodecount,nil);
        result:=nodecount;
      end;


    function number_unrolls(node : tnode) : cardinal;
      begin
{$ifdef i386}
        { multiply by 2 for CPUs with a long pipeline }
        if current_settings.cputype in [cpu_Pentium4] then
          number_unrolls:=60 div countnodes(node)
        else
{$endif i386}
          number_unrolls:=30 div countnodes(node);

        if number_unrolls=0 then
          number_unrolls:=1;
      end;


    function unroll_loop(node : tnode) : tnode;
      var
        unrolls,i : cardinal;
        counts : qword;
        unrollstatement,newforstatement : tstatementnode;
        unrollblock : tblocknode;
      begin
        result:=nil;
        if (cs_opt_size in current_settings.optimizerswitches) then
          exit;
        if not(node.nodetype in [forn]) then
          exit;
        unrolls:=number_unrolls(tfornode(node).t2);
        if unrolls>1 then
          begin
            { number of executions known? }
            if (tfornode(node).right.nodetype=ordconstn) and (tfornode(node).t1.nodetype=ordconstn) then
              begin
                if lnf_backward in tfornode(node).loopflags then
                  counts:=tordconstnode(tfornode(node).right).value-tordconstnode(tfornode(node).t1).value+1
                else
                  counts:=tordconstnode(tfornode(node).t1).value-tordconstnode(tfornode(node).right).value+1;

                { don't unroll more than we need }
                if unrolls>counts then
                  unrolls:=counts;

                { create block statement }
                unrollblock:=internalstatements(unrollstatement);

                { let's unroll (and rock of course) }
                for i:=1 to unrolls do
                  begin
                    { create and insert copy of the statement block }
                    addstatement(unrollstatement,tfornode(node).t2.getcopy);

                    { set and insert entry label? }
                    if (counts mod unrolls<>0) and
                      ((counts mod unrolls)=unrolls-i) then
                      begin
                        tfornode(node).entrylabel:=clabelnode.create(cnothingnode.create,tlabelsym.create('$optunrol'));
                        addstatement(unrollstatement,tfornode(node).entrylabel);
                      end;

                    { for itself increases at the last iteration }
                    if i<unrolls then
                      begin
                        { insert incrementation of counter var }
                        addstatement(unrollstatement,
                          geninlinenode(in_inc_x,false,ccallparanode.create(tfornode(node).left.getcopy,nil)));
                      end;
                  end;
                { can we get rid of the for statement? }
                if unrolls=counts then
                  begin
                    { create block statement }
                    result:=internalstatements(newforstatement);
                    { initial assignment }
                    addstatement(newforstatement,cassignmentnode.create(
                      tfornode(node).left.getcopy,tfornode(node).right.getcopy));
                    addstatement(newforstatement,unrollblock);
                  end;
              end
            else
              begin
                { unrolling is a little bit more tricky if we don't know the
                  loop count at compile time, but the solution is to use a jump table
                  which is indexed by "loop count mod unrolls" at run time and which
                  jumps then at the appropriate place inside the loop. Because
                  a module division is expensive, we can use only unroll counts dividable
                  by 2 }
                case unrolls of
                  1..2:
                    ;
                  3:
                    unrolls:=2;
                  4..7:
                    unrolls:=4;
                  { unrolls>4 already make no sense imo, but who knows (FK) }
                  8..15:
                    unrolls:=8;
                  16..31:
                    unrolls:=16;
                  32..63:
                    unrolls:=32;
                  64..$7fff:
                    unrolls:=64;
                  else
                    exit;
                end;
                { we don't handle this yet }
                exit;
              end;
            if not(assigned(result)) then
              begin
                tfornode(node).t2.free;
                tfornode(node).t2:=unrollblock;
              end;
          end;
      end;

    var
      initcode,
      calccode,
      deletecode : tblocknode;
      initcodestatements,
      calccodestatements,
      deletecodestatements: tstatementnode;
      templist : tfplist;
      inductionexprs : tfplist;
      changedforloop,
      containsnestedforloop : boolean;

    function is_loop_invariant(loop : tnode;expr : tnode) : boolean;
      begin
        result:=is_constnode(expr);
        case expr.nodetype of
          loadn:
            begin
              if (pi_dfaavailable in current_procinfo.flags) and
                assigned(loop.optinfo) and
                assigned(expr.optinfo) then
                { no aliasing? }
                result:=not(tabstractvarsym(tloadnode(expr).symtableentry).addr_taken) and
                { no definition in the loop? }
                  not(DFASetIn(loop.optinfo^.defsum,expr.optinfo^.index));
            end;
        end;
      end;


    { checks if the strength of n can be recuded, arg is the tforloop being considered }
    function dostrengthreductiontest(var n: tnode; arg: pointer): foreachnoderesult;

      function findpreviousstrengthreduction : boolean;
        var
          i : longint;
        begin
          result:=false;
          for i:=0 to inductionexprs.count-1 do
            begin
              { do we already maintain one expression? }
              if tnode(inductionexprs[i]).isequal(n) then
                begin
                  n.free;
                  n:=ttemprefnode.create(ttempcreatenode(templist[i]));
                  result:=true;
                  exit;
                end;
            end;
        end;


      procedure CreateNodes;
        begin
          if not assigned(initcode) then
            begin
              initcode:=internalstatements(initcodestatements);
              calccode:=internalstatements(calccodestatements);
              deletecode:=internalstatements(deletecodestatements);
            end;
        end;

      var
        tempnode : ttempcreatenode;
      begin
        result:=fen_false;
        case n.nodetype of
          forn:
            { inform for loop search routine, that it needs to search more deeply }
            containsnestedforloop:=true;
          muln:
            begin
              if (taddnode(n).right.nodetype=loadn) and
                taddnode(n).right.isequal(tfornode(arg).left) and
                { plain read of the loop variable? }
                not(nf_write in taddnode(n).right.flags) and
                not(nf_modify in taddnode(n).right.flags) and
                is_loop_invariant(tfornode(arg),taddnode(n).left) and
                { for now, we can handle only constant lower borders }
                is_constnode(tfornode(arg).right) then
                taddnode(n).swapleftright;

              if (taddnode(n).left.nodetype=loadn) and
                taddnode(n).left.isequal(tfornode(arg).left) and
                { plain read of the loop variable? }
                not(nf_write in taddnode(n).left.flags) and
                not(nf_modify in taddnode(n).left.flags) and
                is_loop_invariant(tfornode(arg),taddnode(n).right) and
                { for now, we can handle only constant lower borders }
                is_constnode(tfornode(arg).right) then
                begin
                  changedforloop:=true;
                  { did we use the same expression before already? }
                  if not(findpreviousstrengthreduction) then
                    begin
                      tempnode:=ctempcreatenode.create(n.resultdef,n.resultdef.size,tt_persistent,
                        tstoreddef(n.resultdef).is_intregable or tstoreddef(n.resultdef).is_fpuregable);

                      templist.Add(tempnode);
                      inductionexprs.Add(n);
                      CreateNodes;

                      if lnf_backward in tfornode(arg).loopflags then
                        addstatement(calccodestatements,
                          geninlinenode(in_dec_x,false,
                          ccallparanode.create(ctemprefnode.create(tempnode),ccallparanode.create(taddnode(n).right.getcopy,nil))))
                      else
                        addstatement(calccodestatements,
                          geninlinenode(in_inc_x,false,
                          ccallparanode.create(ctemprefnode.create(tempnode),ccallparanode.create(taddnode(n).right.getcopy,nil))));
                      addstatement(initcodestatements,tempnode);
                      addstatement(initcodestatements,cassignmentnode.create(ctemprefnode.create(tempnode),
                        caddnode.create(muln,
                          caddnode.create(subn,tfornode(arg).right.getcopy,cordconstnode.create(1,tfornode(arg).right.resultdef,false)),
                          taddnode(n).right.getcopy)
                        )
                      );

                      { finally replace the node by a temp. ref }
                      n:=ctemprefnode.create(tempnode);

                      { ... and add a temp. release node }
                      addstatement(deletecodestatements,ctempdeletenode.create(tempnode));
                    end;
                  { set types }
                  do_firstpass(n);
                  result:=fen_norecurse_false;
                end;
            end;
        end;
      end;


    function OptimizeInductionVariablesSingleForLoop(node : tnode) : tnode;
      var
        loopcode,
        newcode : tblocknode;
        loopcodestatements,
        newcodestatements : tstatementnode;
        fornode : tfornode;
      begin
        result:=nil;
        if node.nodetype<>forn then
          exit;
        templist:=TFPList.Create;
        inductionexprs:=TFPList.Create;
        initcode:=nil;
        calccode:=nil;
        deletecode:=nil;
        initcodestatements:=nil;
        calccodestatements:=nil;
        deletecodestatements:=nil;
        { find all expressions being candidates for strength reduction
          and replace them }
        foreachnodestatic(pm_postprocess,node,@dostrengthreductiontest,node);

        { clue everything together }
        if assigned(initcode) then
          begin
            do_firstpass(initcode);
            do_firstpass(calccode);
            do_firstpass(deletecode);
            { create a new for node, the old one will be released by the compiler }
            with tfornode(node) do
              begin
                fornode:=cfornode.create(left,right,t1,t2,lnf_backward in loopflags);
                left:=nil;
                right:=nil;
                t1:=nil;
                t2:=nil;
              end;
            node:=fornode;

            loopcode:=internalstatements(loopcodestatements);
            addstatement(loopcodestatements,calccode);
            addstatement(loopcodestatements,tfornode(node).t2);
            tfornode(node).t2:=loopcode;
            do_firstpass(node);

            result:=internalstatements(newcodestatements);
            addstatement(newcodestatements,initcode);
            addstatement(newcodestatements,node);
            addstatement(newcodestatements,deletecode);
          end;
        templist.Free;
        inductionexprs.Free;
      end;


    function iterforloops(var n: tnode; arg: pointer): foreachnoderesult;
      var
        hp : tnode;
      begin
        Result:=fen_false;
        if n.nodetype=forn then
          begin
            { do we have DFA available? }
            if pi_dfaavailable in current_procinfo.flags then
              begin
                CalcDefSum(n);
              end;

            containsnestedforloop:=false;
            hp:=OptimizeInductionVariablesSingleForLoop(n);
            if assigned(hp) then
              begin
                n.Free;
                n:=hp;
              end;
            { can we avoid further searching? }
            if not(containsnestedforloop) then
              Result:=fen_norecurse_false;
          end;
      end;


    function OptimizeInductionVariables(node : tnode) : boolean;
      begin
        changedforloop:=false;
        foreachnodestatic(pm_postprocess,node,@iterforloops,nil);
        Result:=changedforloop;
      end;

end.
