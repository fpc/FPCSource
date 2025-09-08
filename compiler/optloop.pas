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

{ $define DEBUG_OPTSTRENGTH}
{ $define DEBUG_OPTFORLOOP}

  interface

    uses
      node;

    function unroll_loop(node : tnode) : tnode;
    function OptimizeInductionVariables(node : tnode) : boolean;
    function OptimizeForLoop(var node : tnode) : boolean;

  implementation

    uses
      cutils,cclasses,compinnr,
      globtype,globals,constexp,
      verbose,
      symdef,symsym,
      defutil,
      cpuinfo,
      nutils,
      nadd,nbas,nflw,ncon,ninl,ncal,nld,nmem,ncnv,
      ncgmem,
      pass_1,
      optbase,optutils,
      procinfo;

    function number_unrolls(node : tnode) : cardinal;
      var
        nodeCount : cardinal;
      begin
        { calculate how often a loop shall be unrolled.

          The term (60*ord(node_count_weighted(node)<15)) is used to get small loops  unrolled more often as
          the counter management takes more time in this case. }
{$ifdef i386}
        { multiply by 2 for CPUs with a long pipeline }
        if current_settings.optimizecputype in [cpu_Pentium4] then
          begin
            { See the common branch below for an explanation. }
            nodeCount:=node_count_weighted(node,41);
            number_unrolls:=round((60+(60*ord(nodeCount<15)))/max(nodeCount,1))
          end
        else
{$endif i386}
          begin
            { If nodeCount >= 15, numerator will be 30,
              and the largest number (starting from 15) that makes sense as its denominator
              (the smallest number that gives number_unrolls = 1) is 21 = trunc(30/1.5+1),
              so there's no point in counting for more than 21 nodes.
              "Long pipeline" variant above is the same with numerator=60 and max denominator = 41. }
            nodeCount:=node_count_weighted(node,21);
            number_unrolls:=round((30+(60*ord(nodeCount<15)))/max(nodeCount,1));
          end;

        if number_unrolls=0 then
          number_unrolls:=1;
      end;

    type
      treplaceinfo = record
        node : tnode;
        value : Tconstexprint;
      end;
      preplaceinfo = ^treplaceinfo;

    function checkcontrollflowstatements(var n:tnode; arg: pointer): foreachnoderesult;
      begin
        if n.nodetype in [breakn,continuen,goton,labeln,exitn,raisen] then
          result:=fen_norecurse_true
        else
          result:=fen_false;
      end;


    function replaceloadnodes(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        if n.isequal(preplaceinfo(arg)^.node) then
          begin
            if n.flags*[nf_modify,nf_write,nf_address_taken]<>[] then
              internalerror(2012090402);
            n.free;
            n:=cordconstnode.create(preplaceinfo(arg)^.value,preplaceinfo(arg)^.node.resultdef,false);
            do_firstpass(n);
          end;
        result:=fen_false;
      end;


    function unroll_loop(node : tnode) : tnode;
      var
        unrolls,i : cardinal;
        counts : qword;
        unrollstatement,newforstatement : tstatementnode;
        unrollblock : tblocknode;
        getridoffor : boolean;
        replaceinfo : treplaceinfo;
        hascontrollflowstatements : boolean;
      begin
        result:=nil;
        if (cs_opt_size in current_settings.optimizerswitches) then
          exit;
        if ErrorCount<>0 then
          exit;
        if not(node.nodetype in [forn]) then
          exit;
        unrolls:=number_unrolls(tfornode(node).t2);
        if (unrolls>1) and
          ((tfornode(node).left.nodetype<>loadn) or
           { the address of the counter variable might be taken if it is passed by constref to a
             subroutine, so really check if it is not taken }
           ((tfornode(node).left.nodetype=loadn) and (tloadnode(tfornode(node).left).symtableentry is tabstractvarsym) and
            not(tabstractvarsym(tloadnode(tfornode(node).left).symtableentry).addr_taken) and
            not(tabstractvarsym(tloadnode(tfornode(node).left).symtableentry).different_scope))
           ) then
          begin
            { number of executions known? }
            if (tfornode(node).right.nodetype=ordconstn) and (tfornode(node).t1.nodetype=ordconstn) then
              begin
                if lnf_backward in tfornode(node).loopflags then
                  counts:=tordconstnode(tfornode(node).right).value-tordconstnode(tfornode(node).t1).value+1
                else
                  counts:=tordconstnode(tfornode(node).t1).value-tordconstnode(tfornode(node).right).value+1;

                hascontrollflowstatements:=foreachnodestatic(tfornode(node).t2,@checkcontrollflowstatements,nil);

                { don't unroll more than we need,

                  multiply unroll by two here because we can get rid
                  of the counter variable completely and replace it by a constant
                  if unrolls=counts }
                if unrolls*2>=counts then
                  unrolls:=counts;

                { create block statement }
                unrollblock:=internalstatements(unrollstatement);

                { can we get rid completly of the for ? }
                getridoffor:=(unrolls=counts) and not(hascontrollflowstatements) and
                  { TP/Macpas allows assignments to the for-variables, so we cannot get rid of the for }
                  ([m_tp7,m_mac]*current_settings.modeswitches=[]);

                if getridoffor then
                  begin
                    replaceinfo.node:=tfornode(node).left;
                    replaceinfo.value:=tordconstnode(tfornode(node).right).value;
                  end
                else
                  { we consider currently unrolling not beneficial, if we cannot get rid of the for completely, this
                    might change if a more sophisticated heuristics is used (FK) }
                  exit;

                { let's unroll (and rock of course) }
                for i:=1 to unrolls do
                  begin
                    { create and insert copy of the statement block }
                    addstatement(unrollstatement,tfornode(node).t2.getcopy);

                    { set and insert entry label? }
                    if (counts mod unrolls<>0) and
                      ((counts mod unrolls)=unrolls-i) then
                      begin
                        tfornode(node).entrylabel:=clabelnode.create(cnothingnode.create,clabelsym.create('$optunrol'));
                        addstatement(unrollstatement,tfornode(node).entrylabel);
                      end;

                    if getridoffor then
                      begin
                        foreachnodestatic(tnode(unrollstatement),@replaceloadnodes,@replaceinfo);
                        if lnf_backward in tfornode(node).loopflags then
                          replaceinfo.value:=replaceinfo.value-1
                        else
                          replaceinfo.value:=replaceinfo.value+1;
                      end
                    else
                      begin
                        { for itself increases at the last iteration }
                        if i<unrolls then
                          begin
                            { insert incr/decrementation of counter var }
                            if lnf_backward in tfornode(node).loopflags then
                              addstatement(unrollstatement,
                                geninlinenode(in_dec_x,false,ccallparanode.create(tfornode(node).left.getcopy,nil)))
                            else
                              addstatement(unrollstatement,
                                geninlinenode(in_inc_x,false,ccallparanode.create(tfornode(node).left.getcopy,nil)));
                          end;
                       end;
                  end;
                { can we get rid of the for statement? }
                if getridoffor then
                  begin
                    { create block statement }
                    result:=internalstatements(newforstatement);
                    addstatement(newforstatement,unrollblock);
                    doinlinesimplify(result);
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


    function checkcontinue(var n:tnode; arg: pointer): foreachnoderesult;
      begin
        if n.nodetype=continuen then
          result:=fen_norecurse_true
        else
          result:=fen_false;
      end;


    function is_loop_invariant(loop : tnode;expr : tnode) : boolean;
      begin
        result:=is_constnode(expr);
        case expr.nodetype of
          loadn:
            begin
              if (pi_dfaavailable in current_procinfo.flags) and
                assigned(loop.optinfo) and
                assigned(expr.optinfo) and
                not(expr.isequal(tfornode(loop).left)) then
                { no aliasing? }
                result:=(([nf_write,nf_modify]*expr.flags)=[]) and not(tabstractvarsym(tloadnode(expr).symtableentry).addr_taken) and
                { no definition in the loop? }
                  not(DFASetIn(tfornode(loop).t2.optinfo^.defsum,expr.optinfo^.index));
            end;
          vecn:
            begin
              result:=((tvecnode(expr).left.nodetype=loadn) or is_loop_invariant(loop,tvecnode(expr).left)) and
                is_loop_invariant(loop,tvecnode(expr).right);
            end;
          typeconvn:
            result:=is_loop_invariant(loop,ttypeconvnode(expr).left);
          addn,subn:
            result:=is_loop_invariant(loop,taddnode(expr).left) and is_loop_invariant(loop,taddnode(expr).right);
          else
            ;
        end;
      end;


    type
      toptimizeinductionvariablescontext = object
        currforloop : tfornode;
        initcode,
        calccode,
        deletecode : tblocknode;
        initcodestatements,
        calccodestatements,
        deletecodestatements: tstatementnode;
        ninductions : sizeint;
        inductions : array of record
          temp : ttempcreatenode;
          expr : tnode;
        end;
        changedforloop,
        containsnestedforloop,
        docalcatend : boolean;
        function findpreviousstrengthreduction(var n: tnode): boolean;
        procedure addinduction(temp : ttempcreatenode; expr : tnode);
        function dostrengthreductiontest(var n: tnode): foreachnoderesult;
        procedure optimizeinductionvariablessingleforloop(var n: tnode);
      end;


    function toptimizeinductionvariablescontext.findpreviousstrengthreduction(var n: tnode): boolean;
      var
        i : longint;
        hp : tnode;
      begin
        result:=false;
        for i:=0 to ninductions-1 do
          begin
            { do we already maintain one expression? }
            if inductions[i].expr.isequal(n) then
              begin
                case n.nodetype of
                  muln:
                    hp:=ctemprefnode.create(inductions[i].temp);
                  vecn:
                    hp:=ctypeconvnode.create_internal(cderefnode.create(ctemprefnode.create(inductions[i].temp)),n.resultdef);
                  else
                    internalerror(200809211);
                end;
                n.free;
                n:=hp;
                exit(true);
              end;
          end;
      end;


    procedure toptimizeinductionvariablescontext.addinduction(temp : ttempcreatenode; expr : tnode);
      begin
        if not assigned(initcode) then
          begin
            initcode:=internalstatements(initcodestatements);
            calccode:=internalstatements(calccodestatements);
            deletecode:=internalstatements(deletecodestatements);
            docalcatend:=not(assigned(currforloop.entrylabel)) and
              not(foreachnodestatic(currforloop.t2,@checkcontinue,nil));
          end;
        if ninductions>=length(inductions) then
          SetLength(inductions,4+ninductions+ninductions shr 1);
        inductions[ninductions].temp:=temp;
        inductions[ninductions].expr:=expr;
        inc(ninductions);
      end;


    { checks if the strength of n can be reduced, currforloop is the tforloop being considered }
    function toptimizeinductionvariablescontext.dostrengthreductiontest(var n: tnode): foreachnoderesult;
      var
        tempnode,startvaltemp : ttempcreatenode;
        dummy : longint;
        nn : tnode;
        nt : tnodetype;
        nflags : tnodeflags;
      begin
        result:=fen_false;
        nflags:=n.flags;
        case n.nodetype of
          forn:
            { inform for loop search routine, that it needs to search more deeply }
            containsnestedforloop:=true;
          muln:
            begin
              if (taddnode(n).right.nodetype=loadn) and
                taddnode(n).right.isequal(currforloop.left) and
                { plain read of the loop variable? }
                not(nf_write in taddnode(n).right.flags) and
                not(nf_modify in taddnode(n).right.flags) and
                is_loop_invariant(currforloop,taddnode(n).left) then
                taddnode(n).swapleftright;

              if (taddnode(n).left.nodetype=loadn) and
                taddnode(n).left.isequal(currforloop.left) and
                { plain read of the loop variable? }
                not(nf_write in taddnode(n).left.flags) and
                not(nf_modify in taddnode(n).left.flags) and
                is_loop_invariant(currforloop,taddnode(n).right) then
                begin
                  changedforloop:=true;
                  { did we use the same expression before already? }
                  if not(findpreviousstrengthreduction(n)) then
                    begin
{$ifdef DEBUG_OPTSTRENGTH}
                      writeln('**********************************************************************************');
                      writeln(parser_current_file, ': Found expression for strength reduction (MUL): ');
                      printnode(n);
                      writeln('**********************************************************************************');
{$endif DEBUG_OPTSTRENGTH}
                      tempnode:=ctempcreatenode.create(n.resultdef,n.resultdef.size,tt_persistent,
                        tstoreddef(n.resultdef).is_intregable or tstoreddef(n.resultdef).is_fpuregable);
                      addinduction(tempnode,n);

                      if lnf_backward in currforloop.loopflags then
                        addstatement(calccodestatements,
                          geninlinenode(in_dec_x,false,
                          ccallparanode.create(ctemprefnode.create(tempnode),ccallparanode.create(taddnode(n).right.getcopy,nil))))
                      else
                        addstatement(calccodestatements,
                          geninlinenode(in_inc_x,false,
                          ccallparanode.create(ctemprefnode.create(tempnode),ccallparanode.create(taddnode(n).right.getcopy,nil))));

                      addstatement(initcodestatements,tempnode);
                      nn:=currforloop.right.getcopy;
                      { If the calculation is not performed at the end
                        it is needed to adjust the starting value }
                      if not docalcatend then
                        begin
                          if lnf_backward in currforloop.loopflags then
                            nt:=addn
                          else
                            nt:=subn;
                          nn:=caddnode.create_internal(nt,nn,
                             cordconstnode.create(1,nn.resultdef,false));
                        end;
                      addstatement(initcodestatements,cassignmentnode.create(ctemprefnode.create(tempnode),
                          caddnode.create(muln,nn,
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
          vecn:
            begin
              { is the index the counter variable? }
              if not(is_special_array(tvecnode(n).left.resultdef)) and
                not(is_packed_array(tvecnode(n).left.resultdef)) and
                (tvecnode(n).right.isequal(currforloop.left) or
                 { fpc usually creates a type cast to access an array }
                 ((tvecnode(n).right.nodetype=typeconvn) and
                  ttypeconvnode(tvecnode(n).right).left.isequal(currforloop.left)
                 )
                ) and
                { plain read of the loop variable? }
                not(nf_write in tvecnode(n).right.flags) and
                not(nf_modify in tvecnode(n).right.flags) and
                { direct array access? }
                ((tvecnode(n).left.nodetype=loadn) or
                { ... or loop invariant expression? }
                is_loop_invariant(currforloop,tvecnode(n).right))
{$if not (defined(cpu16bitalu) or defined(cpu8bitalu))}
                { removing the multiplication is only worth the
                  effort if it's not a simple shift }
                and not(ispowerof2(tcgvecnode(n).get_mul_size,dummy))
{$endif}
                then
                begin
                  changedforloop:=true;
                  { did we use the same expression before already? }
                  if not(findpreviousstrengthreduction(n)) then
                    begin
{$ifdef DEBUG_OPTSTRENGTH}
                      writeln('**********************************************************************************');
                      writeln(parser_current_file,': Found expression for strength reduction (VEC): ');
                      printnode(n);
                      writeln('**********************************************************************************');
{$endif DEBUG_OPTSTRENGTH}
                      tempnode:=ctempcreatenode.create(voidpointertype,voidpointertype.size,tt_persistent,true);
                      addinduction(tempnode,n);

                      if lnf_backward in currforloop.loopflags then
                        addstatement(calccodestatements,
                          cinlinenode.createintern(in_dec_x,false,
                          ccallparanode.create(ctemprefnode.create(tempnode),ccallparanode.create(
                          cordconstnode.create(tcgvecnode(n).get_mul_size,sizeuinttype,false),nil))))
                      else
                        addstatement(calccodestatements,
                          cinlinenode.createintern(in_inc_x,false,
                          ccallparanode.create(ctemprefnode.create(tempnode),ccallparanode.create(
                          cordconstnode.create(tcgvecnode(n).get_mul_size,sizeuinttype,false),nil))));

                      addstatement(initcodestatements,tempnode);

                      startvaltemp:=maybereplacewithtemp(currforloop.right,initcode,initcodestatements,currforloop.right.resultdef.size,true);
                      nn:=caddrnode.create(
                          cvecnode.create(tvecnode(n).left.getcopy,ctypeconvnode.create_internal(currforloop.right.getcopy,tvecnode(n).right.resultdef))
                        );
                      { If the calculation is not performed at the end
                        it is needed to adjust the starting value }
                      if not docalcatend then
                        begin
                          if lnf_backward in currforloop.loopflags then
                            nt:=addn
                          else
                            nt:=subn;
                          nn:=caddnode.create_internal(nt,
                             ctypeconvnode.create_internal(nn,voidpointertype),
                             cordconstnode.create(tcgvecnode(n).get_mul_size,sizeuinttype,false));
                        end;
                      addstatement(initcodestatements,cassignmentnode.create(ctemprefnode.create(tempnode),nn));

                      { finally replace the node by a temp. ref }
                      n:=ctypeconvnode.create_internal(cderefnode.create(ctemprefnode.create(tempnode)),n.resultdef);

                      { ... and add a temp. release node }
                      if startvaltemp<>nil then
                        addstatement(deletecodestatements,ctempdeletenode.create(startvaltemp));
                      addstatement(deletecodestatements,ctempdeletenode.create(tempnode));
                    end;
                  { Copy the nf_write,nf_modify flags to the new deref node of the temp.
                    Othewise assignments to vector elements will be removed. }
                  if nflags*[nf_write,nf_modify]<>[] then
                    begin
                      if (n.nodetype<>typeconvn) or (ttypeconvnode(n).left.nodetype<>derefn) then
                        internalerror(2021091501);
                      ttypeconvnode(n).left.flags:=ttypeconvnode(n).left.flags+nflags*[nf_write,nf_modify];
                    end;
                  { set types }
                  do_firstpass(n);
                  result:=fen_norecurse_false;
                end;
            end;
          else
            ;
        end;
      end;


    function dostrengthreductiontest_callback(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        result:=toptimizeinductionvariablescontext(arg^).dostrengthreductiontest(n);
      end;


    procedure toptimizeinductionvariablescontext.optimizeinductionvariablessingleforloop(var n: tnode);
      var
        loopcode : tblocknode;
        loopcodestatements,
        newcodestatements : tstatementnode;
        newfor,oldn : tnode;
      begin
        { do we have DFA available? }
        if pi_dfaavailable in current_procinfo.flags then
          begin
            CalcDefSum(tfornode(n).t2);
          end;
        currforloop:=tfornode(n);
        initcode:=nil;
        calccode:=nil;
        deletecode:=nil;
        initcodestatements:=nil;
        calccodestatements:=nil;
        deletecodestatements:=nil;
        ninductions:=0;
        docalcatend:=false;
        { find all expressions being candidates for strength reduction
          and replace them }
        foreachnodestatic(pm_postprocess,n,@dostrengthreductiontest_callback,@self);

        { clue everything together }
        if assigned(initcode) then
          begin
            do_firstpass(tnode(initcode));
            do_firstpass(tnode(calccode));
            do_firstpass(tnode(deletecode));
            { create a new for node, the old one will be released by the compiler }
            oldn:=n;
            newfor:=cfornode.create(tfornode(oldn).left,tfornode(oldn).right,tfornode(oldn).t1,tfornode(oldn).t2,lnf_backward in tfornode(oldn).loopflags);
            tfornode(oldn).left:=nil;
            tfornode(oldn).right:=nil;
            tfornode(oldn).t1:=nil;
            tfornode(oldn).t2:=nil;

            loopcode:=internalstatements(loopcodestatements);
            if not docalcatend then
              addstatement(loopcodestatements,calccode);
            addstatement(loopcodestatements,tfornode(newfor).t2);
            if docalcatend then
              addstatement(loopcodestatements,calccode);
            tfornode(newfor).t2:=loopcode;
            do_firstpass(newfor);

            n:=internalstatements(newcodestatements);
            oldn.Free;
            addstatement(newcodestatements,initcode);
            addstatement(newcodestatements,newfor);
            addstatement(newcodestatements,deletecode);
          end;
      end;


    function optimizeinductionvariablessingleforloop_static(var n: tnode; arg: pointer): foreachnoderesult;
      var
        ctx : ^toptimizeinductionvariablescontext absolute arg;
      begin
        Result:=fen_false;
        if n.nodetype<>forn then
          exit;
        ctx^.containsnestedforloop:=false;
        ctx^.optimizeinductionvariablessingleforloop(n);
        { can we avoid further searching? }
        if not(ctx^.containsnestedforloop) then
          Result:=fen_norecurse_false;
      end;


    function OptimizeInductionVariables(node : tnode) : boolean;
      var
        ctx : toptimizeinductionvariablescontext;
      begin
        Result:=false;
        if not(pi_dfaavailable in current_procinfo.flags) then
          exit;
        ctx.changedforloop:=false;
        foreachnodestatic(pm_postprocess,node,@optimizeinductionvariablessingleforloop_static,@ctx);
        Result:=ctx.changedforloop;
      end;


    type
      toptimizeforloopcontext = object
        changedforloop : boolean;
      end;

    function OptimizeForLoop_iterforloops(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        Result:=fen_false;
        if (n.nodetype=forn) and
          not(lnf_backward in tfornode(n).loopflags) and
          (lnf_dont_mind_loopvar_on_exit in tfornode(n).loopflags) and
          is_constintnode(tfornode(n).right) and
          (([cs_check_overflow,cs_check_range]*n.localswitches)=[]) and
          (([cs_check_overflow,cs_check_range]*tfornode(n).left.localswitches)=[]) and
          ((tfornode(n).left.nodetype=loadn) and (tloadnode(tfornode(n).left).symtableentry is tabstractvarsym) and
            not(tabstractvarsym(tloadnode(tfornode(n).left).symtableentry).addr_taken) and
            not(tabstractvarsym(tloadnode(tfornode(n).left).symtableentry).different_scope)) then
          begin
            { do we have DFA available? }
            if pi_dfaavailable in current_procinfo.flags then
              begin
                CalcUseSum(tfornode(n).t2);
                CalcDefSum(tfornode(n).t2);
              end
            else
              Internalerror(2017122801);
            if not(assigned(tfornode(n).left.optinfo)) then
              exit;
            if not(DFASetIn(tfornode(n).t2.optinfo^.usesum,tfornode(n).left.optinfo^.index)) and
              not(DFASetIn(tfornode(n).t2.optinfo^.defsum,tfornode(n).left.optinfo^.index))  then
              begin
                { convert the loop from i:=a to b into i:=b-a+1 to 1 as this simplifies the
                  abort condition }
{$ifdef DEBUG_OPTFORLOOP}
                writeln('**********************************************************************************');
                writeln('Found loop for reverting: ');
                printnode(n);
                writeln('**********************************************************************************');
{$endif DEBUG_OPTFORLOOP}
                include(tfornode(n).loopflags,lnf_backward);
                tfornode(n).right:=ctypeconvnode.create_internal(
                  caddnode.create_internal(addn,caddnode.create_internal(subn,
                    tfornode(n).t1,tfornode(n).right),
                    cordconstnode.create(1,tfornode(n).left.resultdef,false)),
                  tfornode(n).left.resultdef);
                tfornode(n).t1:=cordconstnode.create(1,tfornode(n).left.resultdef,false);
                include(tfornode(n).loopflags,lnf_counter_not_used);
                exclude(n.transientflags,tnf_pass1_done);
                do_firstpass(n);
{$ifdef DEBUG_OPTFORLOOP}
                writeln('Loop reverted: ');
                printnode(n);
                writeln('**********************************************************************************');
{$endif DEBUG_OPTFORLOOP}
                toptimizeforloopcontext(arg^).changedforloop:=true;
              end;
          end;
      end;


    function OptimizeForLoop(var node : tnode) : boolean;
      var
        ctx : toptimizeforloopcontext;
      begin
        ctx.changedforloop:=false;
        if pi_dfaavailable in current_procinfo.flags then
          foreachnodestatic(pm_postprocess,node,@OptimizeForLoop_iterforloops,@ctx);
        Result:=ctx.changedforloop;
      end;

end.

