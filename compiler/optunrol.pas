unit optunrol;

{$i fpcdefs.inc}

  interface

    uses
      node;

    function unroll_loop(node : tnode) : tnode;

  implementation

    uses
      globtype,globals,
      cpuinfo,
      nutils,
      nbas,nflw,ncon,ninl,ncal;

    var
      nodecount : aint;

    function donodecount(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        inc(nodecount);
        result:=fen_false;
      end;


    { rough estimation how large the tree "node" is }
    function countnodes(node : tnode) : aint;
      begin
        nodecount:=0;
        foreachnodestatic(node,@donodecount,nil);
        result:=nodecount;
      end;


    function number_unrolls(node : tnode) : integer;
      begin
{$ifdef i386}
        { multiply by 2 for CPUs with a long pipeline }
        if aktoptprocessor in [ClassPentium4] then
          number_unrolls:=60 div countnodes(node)
        else
{$endif i386}
          number_unrolls:=30 div countnodes(node);

        if number_unrolls=0 then
          number_unrolls:=1;
      end;


    function unroll_loop(node : tnode) : tnode;
      var
        unrolls,i : integer;
        counts : qword;
        unrollstatement : tstatementnode;
        unrollblock : tblocknode;
        entrylabel : tlabelnode;
      begin
        result:=nil;
        if (cs_littlesize in aktglobalswitches) then
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
                    { set and insert entry label? }
                    if (counts mod unrolls<>0) and
                      ((counts mod unrolls)=unrolls-i+1) then
                      begin
                        tfornode(node).entrylabel:=clabelnode.create(cnothingnode.create);
                        addstatement(unrollstatement,tfornode(node).entrylabel);
                      end;
                    { create and insert copy of the statement block }
                    addstatement(unrollstatement,tfornode(tfornode(node).t2).getcopy);

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
                  result:=unrollblock;
              end
            else
              begin
                { for now, we can't handle this }
                exit;
              end;
            if not(assigned(result)) then
              begin
                tfornode(node).t2.free;
                tfornode(node).t2:=unrollblock;
              end;
          end;
      end;

end.
