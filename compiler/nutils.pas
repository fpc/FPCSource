{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Type checking and register allocation for inline nodes

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
unit nutils;

{$i fpcdefs.inc}

interface

  uses
    node;

  type
    { resulttype of functions that process on all nodes in a (sub)tree }
    foreachnoderesult = (
      { false, continue recursion }
      fen_false,
      { false, stop recursion }
      fen_norecurse_false,
      { true, continue recursion }
      fen_true,
      { true, stop recursion }
      fen_norecurse_true
    );


  foreachnodefunction = function(var n: tnode): foreachnoderesult of object;
  staticforeachnodefunction = function(var n: tnode): foreachnoderesult;


  function foreachnode(var n: tnode; f: foreachnodefunction): boolean;
  function foreachnodestatic(var n: tnode; f: staticforeachnodefunction): boolean;

implementation

  uses nflw,nset,ncal;

  function foreachnode(var n: tnode; f: foreachnodefunction): boolean;
    begin
      result := false;
      if not assigned(n) then
        exit;
      case f(n) of
        fen_norecurse_false:
          exit;
        fen_norecurse_true:
          begin
            result := true;
            exit;
          end;
        fen_true:
          result := true;
       { result is already false
        fen_false:
          result := false; }
      end;
      case n.nodetype of
        calln:
          result := foreachnode(tcallnode(n).methodpointer,f) or result;
        procinlinen:
          result := foreachnode(tprocinlinenode(n).inlinetree,f) or result;
        ifn, whilerepeatn, forn:
          begin
            { not in one statement, won't work because of b- }
            result := foreachnode(tloopnode(n).t1,f) or result;
            result := foreachnode(tloopnode(n).t2,f) or result;
          end;
        raisen:
          result := foreachnode(traisenode(n).frametree,f) or result;
        casen:
          result := foreachnode(tcasenode(n). elseblock,f) or result;
      end;
      if n.inheritsfrom(tbinarynode) then
        begin
          result := foreachnode(tbinarynode(n).right,f) or result;
          result := foreachnode(tbinarynode(n).left,f) or result;
        end
      else if n.inheritsfrom(tunarynode) then
        result := foreachnode(tunarynode(n).left,f) or result;
    end;


  function foreachnodestatic(var n: tnode; f: staticforeachnodefunction): boolean;
    begin
      result := false;
      if not assigned(n) then
        exit;
      case f(n) of
        fen_norecurse_false:
          exit;
        fen_norecurse_true:
          begin
            result := true;
            exit;
          end;
        fen_true:
          result := true;
       { result is already false
        fen_false:
          result := false; }
      end;
      case n.nodetype of
        calln:
          result := foreachnodestatic(tcallnode(n).methodpointer,f) or result;
        procinlinen:
          result := foreachnodestatic(tprocinlinenode(n).inlinetree,f) or result;
        ifn, whilerepeatn, forn:
          begin
            { not in one statement, won't work because of b- }
            result := foreachnodestatic(tloopnode(n).t1,f) or result;
            result := foreachnodestatic(tloopnode(n).t2,f) or result;
          end;
        raisen:
          result := foreachnodestatic(traisenode(n).frametree,f) or result;
        casen:
          result := foreachnodestatic(tcasenode(n). elseblock,f) or result;
      end;
      if n.inheritsfrom(tbinarynode) then
        begin
          result := foreachnodestatic(tbinarynode(n).right,f) or result;
          result := foreachnodestatic(tbinarynode(n).left,f) or result;
        end
      else if n.inheritsfrom(tunarynode) then
        result := foreachnodestatic(tunarynode(n).left,f) or result;
    end;


end.

{
  $Log$
  Revision 1.1  2003-04-23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

}