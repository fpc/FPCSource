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

{ $define DEBUG_NORMALIZE}

{ this unit implements routines to perform all-purpose tree transformations }
unit opttree;

{$i fpcdefs.inc}

  interface

    uses
      compilerbase,node,nbas,nutils;

type

  { TTreeOptimizations }

  TTreeOptimizations = class
  private
    FCompiler: TCompilerBase;

    { success pointer shared across recursive searchstatements calls so nested
      blockns can be processed with the proper arg (their own statement chain) }
    normalize_success : pboolean;

    function block_extract_tempdelete(block: tblocknode; ti: ptempinfo): ttempdeletenode;
    function searchblock(var n : tnode;arg : pointer) : foreachnoderesult;
    function searchstatements(var n : tnode;arg : pointer) : foreachnoderesult;
    property Compiler: TCompilerBase read FCompiler;
  public
    constructor Create(ACompiler: TCompilerBase);
    { tries to bring the tree in a normalized form:
       - expressions are free of control statements
       - callinitblock/callcleanupblocks are converted into statements

      rationale is that this simplifies data flow analysis

      returns true, if this was successful
    }
    function normalize(var n : tnode) : Boolean;
  end;

  implementation

    uses
      verbose,
      globtype,
      cgbase,
      defutil,
      nld,ncal,
      pass_1,compiler;

    constructor TTreeOptimizations.Create(ACompiler: TCompilerBase);
      begin
        FCompiler:=ACompiler;
      end;

    function hasblock(var n : tnode;arg : pointer) : foreachnoderesult;
      begin
        result:=fen_false;
        if n.nodetype=blockn then
          result:=fen_norecurse_true;
      end;

    { scan the block's top-level statements for a tempdeletenode referring to
      the given tempinfo, unlink it (replace its statement slot with a
      nothingnode) and return it; nil if not found. Matches the flat
      tempcreate/.../tempdelete pattern emitted around internalstatements
      blocks (nested deletes not expected) }
    function TTreeOptimizations.block_extract_tempdelete(block : tblocknode; ti : ptempinfo) : ttempdeletenode;
      var
        s : tstatementnode;
      begin
        result:=nil;
        s:=tstatementnode(block.left);
        while assigned(s) do
          begin
            if assigned(s.left) and
               (s.left.nodetype=tempdeleten) and
               (ttempdeletenode(s.left).tempinfo=ti) then
              begin
                result:=ttempdeletenode(s.left);
                s.left:=compiler.cnothingnode;
                exit;
              end;
            s:=tstatementnode(s.right);
          end;
      end;

    function TTreeOptimizations.searchblock(var n : tnode;arg : pointer) : foreachnoderesult;
      var
        hp,
        stmnt : tstatementnode;
        res : pnode;
        tempcreatenode : ttempcreatenode;
        tempdelnode : ttempdeletenode;
        movedblock : tnode;
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
                  hp:=compiler.cstatementnode(tcallnode(n).callinitblock,pnode(arg)^);
                  pnode(arg)^:=hp;
                  { tree moved }
                  tcallnode(n).callinitblock:=nil;
                  { process the newly generated block }
                  foreachnode(pnode(arg)^,@searchstatements,nil);
                end;
              if assigned(tcallnode(n).callcleanupblock) then
                begin
                  { create a new statement node and append it }
                  hp:=compiler.cstatementnode(tcallnode(n).callcleanupblock,tstatementnode(pnode(arg)^).right);
                  tstatementnode(pnode(arg)^).right:=hp;
                  { tree moved }
                  tcallnode(n).callcleanupblock:=nil;
                  { process the newly generated block }
                  foreachnode(tstatementnode(pnode(arg)^).right,@searchstatements,nil);
                end;
            end;
          blockn:
            begin
              if assigned(tblocknode(n).left) and (tblocknode(n).left.nodetype<>statementn) then
                internalerror(2013120502);

              { blocks used as statements (void result) cannot be extracted -
                replacing the statement slot with a value would produce a
                malformed tree. But their body may contain extractable blockns
                with references to this block's own temps; recurse through the
                statement chain so those inner blockns get normalized at the
                scope where their external temp refs are still valid. }
              if not assigned(tblocknode(n).resultdef) or
                 is_void(tblocknode(n).resultdef) then
                begin
                  if assigned(tblocknode(n).left) then
                    foreachnode(tblocknode(n).left,@searchstatements,normalize_success);
                  { tell the outer foreachnodestatic not to descend again -
                    we've already walked the body with searchstatements }
                  result:=fen_norecurse_true;
                  exit;
                end;

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
                            { when the block ends with temprefn(t) and also
                              contains tempdelete(t), lift the tempdelete out
                              so the extracted tempref outlives its backing
                              temp (otherwise the delete would run before the
                              use and trip IE 200108231); nil for other cases }
                            if res^.nodetype=temprefn then
                              tempdelnode:=block_extract_tempdelete(tblocknode(n),ttemprefnode(res^).tempinfo)
                            else
                              tempdelnode:=nil;
                            { create a new statement node and insert it }
                            hp:=compiler.cstatementnode(n,pnode(arg)^);
                            pnode(arg)^:=hp;
                            { use the result node instead of the block node }
                            n:=res^;
                            { the old statement is not used anymore }
                            res^:=compiler.cnothingnode;
                            { splice the lifted tempdelete after the original
                              statement so the temp outlives the extracted tempref }
                            if assigned(tempdelnode) then
                              tstatementnode(hp.right).right:=
                                compiler.cstatementnode(tempdelnode,tstatementnode(hp.right).right);
                            { process the newly generated statement }
                            foreachnode(pnode(arg)^,@searchstatements,nil);
                          end
                        else if assigned(res^.resultdef) and not(is_void(res^.resultdef)) then
                          begin
                            { Splice into the parent chain:
                                tempcreate -> block (now assigns to temp) -> origstmt -> tempdelete
                              The blockn's slot in the source expression is
                              replaced by a temprefn for the new temp; the
                              statement slot at pnode(arg)^ stays a statementn. }
                            tempcreatenode:=compiler.ctempcreatenode(res^.resultdef,res^.resultdef.size,tt_persistent,true);
                            { pass_1 on tempcreate sets pi_needs_implicit_finally
                              for managed typedefs; must fire before
                              add_entry_exit_code reads that flag }
                            do_firstpass(tnode(tempcreatenode));

                            { rewrite block's last expression: "temp := <expr>" }
                            res^:=compiler.cassignmentnode_internal(compiler.ctemprefnode(tempcreatenode),res^);
                            do_firstpass(res^);
                            { replace blockn in source expression with a temprefn }
                            movedblock:=n;
                            n:=compiler.ctemprefnode(tempcreatenode);
                            do_firstpass(n);

                            { tempdelete after origstmt }
                            hp:=compiler.cstatementnode(
                                  compiler.ctempdeletenode_normal_temp(tempcreatenode),
                                  tstatementnode(pnode(arg)^).right);
                            tstatementnode(pnode(arg)^).right:=hp;
                            { movedblock before origstmt }
                            hp:=compiler.cstatementnode(movedblock,pnode(arg)^);
                            pnode(arg)^:=hp;
                            { tempcreate before movedblock }
                            hp:=compiler.cstatementnode(tempcreatenode,pnode(arg)^);
                            pnode(arg)^:=hp;

                            foreachnode(pnode(arg)^,@searchstatements,nil);
                          end;
                      end;
                    end;
                end;
            end;
          else
            ;
        end;
      end;


    function TTreeOptimizations.searchstatements(var n : tnode;arg : pointer) : foreachnoderesult;
      begin
        if n.nodetype=statementn then
          begin
            if not(foreachnode(tstatementnode(n).left,@searchblock,@n)) then
              begin
                pboolean(arg)^:=false;
                result:=fen_norecurse_false;
                exit;
              end;
            { do not recurse automatically, but continue with the next statement }
            result:=fen_norecurse_false;
            foreachnode(tstatementnode(n).right,@Self.searchstatements,arg);
          end
        else
          result:=fen_false;
      end;


    function TTreeOptimizations.normalize(var n: tnode) : Boolean;
      var
        success : Boolean;
      begin
        success:=true;
{$ifdef DEBUG_NORMALIZE}
        writeln('******************************************** Before ********************************************');
        printnode(output,n);
{$endif DEBUG_NORMALIZE}
        normalize_success:=@success;
        foreachnode(n,@searchstatements,@success);
{$ifdef DEBUG_NORMALIZE}
        if success then
          begin
            writeln('******************************************** After ********************************************');
            printnode(output,n);
          end
        else
          writeln('************************* Normalization not possible ********************************');
{$endif DEBUG_NORMALIZE}
        Result:=success;
      end;


end.

