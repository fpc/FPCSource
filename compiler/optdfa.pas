{
    DFA

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
unit optdfa;

{$i fpcdefs.inc}

  interface

    uses
      node;

    { reset all dfa info, this is required before creating dfa info
      if the tree has been changed without updating dfa }
    procedure resetdfainfo(node : tnode);

    procedure createoptinfo(node : tnode);

  implementation

    uses
      globtype,globals,
      cpuinfo,
      nutils,
      nbas,nflw,ncon,ninl,ncal,
      optutils;


    function initnodes(var n:tnode; arg: pointer) : foreachnoderesult;
      begin
        { node worth to add? }
        if (node_complexity(n)>1) and (tstoreddef(n.resultdef).is_intregable or tstoreddef(n.resultdef).is_fpuregable) then
          begin
            plists(arg)^.nodelist.Add(n);
            plists(arg)^.locationlist.Add(@n);
            result:=fen_false;
          end
        else
          result:=fen_norecurse_false;
      end;


    {
      x:=f;         read: [f]

      while x do    read: []

        a:=b;       read: [a,b,d]  def: [a]       life:  read*def=[a]
          c:=d;     read: [a,d]    def: [a,c]     life:  read*def=[a]
            e:=a;   read: [a]      def: [a,c,e]   life:  read*def=[a]


      function f(b,d,x : type) : type;

        begin
          while x do        alive: b,d,x
            begin
              a:=b;         alive: b,d,x
              c:=d;         alive: a,d,x
              e:=a+c;       alive: a,c,x
              dec(x);       alive: c,e,x
            end;
          result:=c+e;      alive: c,e
        end;                alive: result

    }

    type
      tdfainfo = record
        use : TDFASet;
        def : TDFASet;
        map : TIndexedNodeSet
      end;

    procedure AddDefUse(s : TDFASet;m : ;n : tnode);
      begin
        while true do
          begin
            case n.nodetype of
              typeconvn:
                n:=ttypeconvnode(n).left;
              loadn:
                begin
                  m.Add(n);
                  TDFASetInclude(s,n.optinfo^.index);
                end;
              else
                internalerror(2007050601);
            end;
          end;
      end;


    procedure CreateLifeInfo(node : tnode);

      var
        changed : boolean;

      procedure CreateInfo(node : tnode);

        procedure updatelifeinfo(n : tnode;l : TDFASet);
          begin
            changed:=changed or DFASetNotEqual(l,n.life);
            node.life:=l;
          end;

        procedure calclife(n : tnode);
          var
            l : TDFANode;
          begin
            if assigned(successor) then
              begin
                DFASetDiff(l,successor.optinfo^.life,n.optinfo^.def);
                DFASetIncludeSet(l,n.optinfo^.use);
                updatelifeinfo(n,l);
              end
          end

        var
          dfainfo : tdfainfo;
        begin
          if nf_processing in node.flags then
            exit;
          include(node,nf_processing);

          if assigned(node.successor) then
            CreateInfo(node.successor);

          { life:=succesorlive-definition+use }

          case node.nodetype of
            whilen:
              begin
                { first, do things as usual, get life information from the successor }

                { life:=succesorlive-definition+use }

                { now iterate through the loop }
                CreateInfo(twhilenode(node).left);

                { update while node }
                { life:=life+left.life }

                { ... and a second iteration for fast convergence }
                CreateInfo(twhilenode(node).left);
              end;
            statementn:
              begin
                { actually an expression doing something? }
                case tstatementnode(node).statement.nodetype of
                  assignn:
                    begin
                      tstatementnode(node).allocoptinfo;
                      if not(assigned(tstatementnode(node).optinfo^.def)) or
                        not(assigned(tstatementnode(node).optinfo^.use)) then
                        begin
                          dfainfo.use:=tstatementnode(node).optinfo^.use;
                          dfainfo.def:=tstatementnode(node).optinfo^.def;
                          Foreach
                        end;
                      calclife(node);
                    end;
                end;
              end;
            else
              internalerror(2007050502);
          end;

          exclude(node,nf_processing);
        end;

      begin
        repeat
          changed:=false;
          CreateInfo(node);
        until not(changed);
      end;


    { reset all dfa info, this is required before creating dfa info
      if the tree has been changed without updating dfa }
    procedure resetdfainfo(node : tnode);
      begin
      end;


    procedure createdfainfo(node : tnode);
      begin
        { add controll flow information }
        SetNodeSucessors(node);

        { now, collect life information }
        CreateLifeInfo(node);
      end;


end.
