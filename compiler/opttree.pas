{
    General tree transformations

    Copyright (c) 2013 by Florian Klaempfl

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

{$define DEBUG_NORMALIZE}

{ this unit implements routines to perform all-purpose tree transformations }
unit opttree;

{$i fpcdefs.inc}

  interface

    uses
      node,optutils;

    { tries to bring the tree in a normalized form:
       - expressions are free of control statements
       - callinitblock/callcleanupblocks are converted into statements

      rationale is that this simplifies data flow analysis

      returns true, if this was successful
    }
    function normalize(var n : tnode) : Boolean;

  implementation

    uses
      verbose,
      globtype,
      defutil,
      nbas,nld,ncal,
      nutils,
      pass_1;

    function searchstatements(var n : tnode;arg : pointer) : foreachnoderesult;forward;

    function hasblock(var n : tnode;arg : pointer) : foreachnoderesult;
      begin
        result:=fen_false;
        if n.nodetype=blockn then
          result:=fen_norecurse_true;
      end;

    function searchblock(var n : tnode;arg : pointer) : foreachnoderesult;
      var
        hp,
        statements,
        stmnt : tstatementnode;
        res : pnode;
        tempcreatenode : ttempcreatenode;
        newblock : tnode;
      begin
        result:=fen_true;
        if n.nodetype in [addn,orn] then
          begin
            { so far we cannot fiddle with short boolean evaluations containing blocks }
            if doshortbooleval(n) and foreachnodestatic(n,@hasblock,nil) then
              begin
                result:=fen_norecurse_false;
                exit;
              end;
          end;
        case n.nodetype of
          calln:
            begin
              if assigned(tcallnode(n).callinitblock) then
                begin
                  { create a new statement node and insert it }
                  hp:=cstatementnode.create(tcallnode(n).callinitblock,pnode(arg)^);
                  pnode(arg)^:=hp;
                  { tree moved }
                  tcallnode(n).callinitblock:=nil;
                  { process the newly generated block }
                  foreachnodestatic(pnode(arg)^,@searchstatements,nil);
                end;
              if assigned(tcallnode(n).callcleanupblock) then
                begin
                  { create a new statement node and append it }
                  hp:=cstatementnode.create(tcallnode(n).callcleanupblock,tstatementnode(pnode(arg)^).right);
                  tstatementnode(pnode(arg)^).right:=hp;
                  { tree moved }
                  tcallnode(n).callcleanupblock:=nil;
                  { process the newly generated block }
                  foreachnodestatic(tstatementnode(pnode(arg)^).right,@searchstatements,nil);
                end;
            end;
          blockn:
            begin
              if assigned(tblocknode(n).left) and (tblocknode(n).left.nodetype<>statementn) then
                internalerror(2013120502);

              stmnt:=tstatementnode(tblocknode(n).left);
              { search for the result of the block node }
              if assigned(stmnt) then
                begin
                  res:=nil;
                  hp:=tstatementnode(stmnt);
                  while assigned(hp) do
                    begin
                      if assigned(hp.left) then
                        res:=@hp.left;
                      hp:=tstatementnode(hp.right);
                    end;
                  { did we find a last node? }
                  if assigned(res^) then
                    begin
                      case res^.nodetype of
                        ordconstn,
                        realconstn,
                        stringconstn,
                        pointerconstn,
                        setconstn,
                        temprefn:
                          begin
                            { create a new statement node and insert it }
                            hp:=cstatementnode.create(n,pnode(arg)^);
                            pnode(arg)^:=hp;
                            { use the result node instead of the block node }
                            n:=res^;
                            { the old statement is not used anymore }
                            res^:=cnothingnode.create;
                            { process the newly generated statement }
                            foreachnodestatic(pnode(arg)^,@searchstatements,nil);
                          end
                        else if assigned(res^.resultdef) and not(is_void(res^.resultdef)) then
                          begin
                            { replace the last node of the block by an assignment to a temp, and move the block out
                              of the expression }
                            newblock:=internalstatements(statements);
                            tempcreatenode:=ctempcreatenode.create(res^.resultdef,res^.resultdef.size,tt_persistent,true);
                            addstatement(statements,tempcreatenode);
                            addstatement(statements,n);

                            { replace the old result node of the block by an assignement to the newly generated temp }
                            res^:=cassignmentnode.create_internal(ctemprefnode.create(tempcreatenode),res^);
                            do_firstpass(res^);
                            addstatement(statements,ctempdeletenode.create_normal_temp(tempcreatenode));
                            addstatement(statements,pnode(arg)^);

                            { use the temp. ref instead of the block node }
                            n:=ctemprefnode.create(tempcreatenode);
                            { replace the statement with the block }
                            pnode(arg)^:=newblock;
                            { first pass the newly generated block }
                            do_firstpass(newblock);
                            { ... and the inserted temp. }
                            do_firstpass(n);
                            { process the newly generated block }
                            foreachnodestatic(pnode(arg)^,@searchstatements,nil);
                          end;
                      end;
                    end;
                end;
            end;
          else
            ;
        end;
      end;

    var
      searchstatementsproc : staticforeachnodefunction;

    function searchstatements(var n : tnode;arg : pointer) : foreachnoderesult;
      begin
        if n.nodetype=statementn then
          begin
            if not(foreachnodestatic(tstatementnode(n).left,@searchblock,@n)) then
              begin
                pboolean(arg)^:=false;
                result:=fen_norecurse_false;
                exit;
              end;
            { do not recurse automatically, but continue with the next statement }
            result:=fen_norecurse_false;
            foreachnodestatic(tstatementnode(n).right,searchstatementsproc,arg);
          end
        else
          result:=fen_false;
      end;


    function normalize(var n: tnode) : Boolean;
      var
        success : Boolean;
      begin
        success:=true;
{$ifdef DEBUG_NORMALIZE}
        writeln('******************************************** Before ********************************************');
        printnode(n);
{$endif DEBUG_NORMALIZE}
        searchstatementsproc:=@searchstatements;
        foreachnodestatic(n,@searchstatements,@success);
{$ifdef DEBUG_NORMALIZE}
        if success then
          begin
            writeln('******************************************** After ********************************************');
            printnode(n);
          end
        else
          writeln('************************* Normalization not possible ********************************');
{$endif DEBUG_NORMALIZE}
        Result:=success;
      end;


end.

