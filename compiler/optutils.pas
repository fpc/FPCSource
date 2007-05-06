{
    Helper routines for the optimizer

    Copyright (c) 2007 by Florian Klaempfl

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
unit optutils;

{$i fpcdefs.inc}

  interface

    uses
      node;

    type
      { this implementation should be really improved,
        its purpose is to find equal nodes }
      TIndexedNodeSet = class(TFPList)
        function Add(node : tnode) : boolean;
        function Includes(node : tnode) : tnode;
        function Remove(node : tnode) : boolean;
      end;

      TNodeMap = class(TNodeSet)
        function (node : tnode) : boolean;
      end;

    procedure SetNodeSucessors(p : tnode);

  implementation

    uses
      nbas,nflw;

    function TIndexedNodeSet.Add(node : tnode) : boolean;
      var
        i : Integer;
        p : tnode;
      begin
        node.allocoptinfo;
        p:=Includes(node);
        if assigned(p) then
          begin
            result:=false;
            node.optinfo^.index:=p.optinfo^.index;
          end
        else
          begin
            i:=Add(node);
            node.optinfo^.index:=i;
            result:=true;
          end
      end;


    function TIndexedNodeSet.Includes(node : tnode) : tnode;
      var
        i : longint;
      begin
        for i:=0 to FCount-1 do
          if tnode(FList^[i]).isequal(node) then
            begin
              result:=tnode(FList^[i]);
              exit;
            end;
        result:=nil;
      end;


    function TIndexedNodeSet.Remove(node : tnode) : boolean;
      var
        p : tnode;
      begin
        result:=false;
        p:=Includes(node);
        if assigned(p) then
          begin
            if Remove(p)<>-1 then
              result:=true;
          end;
      end;


    procedure PrintIndexedNodeSet(f : text;s : TIndexedNodeSet);
      begin
        for i:=0 to high(s) do
          begin
            writeln(f,'=============================== Node ',i,' ===============================');
            printnode(f,s[i]);
            writeln(f);
          end;
      end;


    procedure SetNodeSucessors(p : tnode);
      var
        Continuestack : TFPList;
        Breakstack : TFPList;
      { sets the successor nodes of a node tree block
        returns the first node of the tree if it's a controll flow node }
      function DoSet(p : tnode;succ : tnode) : tnode;
        var
          hp1,hp2 : tnode;
        begin
          result:=nil;
          case p.nodetype of
            statementn:
              begin
                hp1:=p;
                result:=p;
                while assigned(hp1) do
                  begin
                    if assigned(tstatementnode(hp1).right) then
                      begin
                        hp2:=DoSet(tstatementnode(hp1).statement,tstatementnode(hp1).next);
                        if assigned(hp2) then
                          tstatementnode(hp1).successor:=hp2
                        else
                          tstatementnode(hp1).successor:=tstatementnode(hp1).right;
                      end
                    else
                      begin
                        hp2:=DoSet(tstatementnode(hp1).statement,successor);
                        if assigned(hp2) then
                          tstatementnode(hp1).successor:=hp2
                        else
                          tstatementnode(hp1).successor:=successor;
                      end;
                  end;
              end;
            blockn:
              begin
                result:=DoSet(tblocknode(p).statements,successor);
              end;
            forn:
              begin
                Breakstack.Add(successor);
                Continuestack.Add(p);
                result:=p;
                DoSet(tfornode(p).statements,successor);
                Breakstack.Delete(Count-1);
                Continuestack.Delete(Count-1);
              end;
            breakn:
              begin
                result:=p;
                p.successor:=tnode(Breakstack.Last);
              end;
            continuen:
              begin
                result:=p;
                p.successor:=tnode(Continuestack.Last);
              end;
            { exit is actually a jump to some final. code
            exitn:
              begin
                result:=p;
                p.successor:=nil;
              end;
            }
            ifn,
            whilerepeatn,
            exitn,
            withn,
            casen,
            labeln,
            goton,
            tryexceptn,
            raisen,
            tryfinallyn,
            onn,
            nothingn:
              internalerror(2007050501);
          end;
        end;

      begin
        Breakstack:=TFPList.Create;
        Continuestack:=TFPList.Create;
        DoSet(p,nil);
        Continuestack.Free;
        Breakstack.Free;
      end;

end.

