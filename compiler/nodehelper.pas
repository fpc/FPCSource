{
    Copyright (c) 2026 by Nikolay Nikolov
    Member of the Free Pascal development team

    This unit implements the class helper for the TNode class. This allows easy
    access to various objects, such as hlcg (the high level code generator) or
    paramanager in the context of the TNode class.

    Since FPC 3.2.x only supports a single class helper to be active for a given
    class, and since we want to be able to bootstrap with FPC 3.2.x, we only
    define a single helper for TNode, that has references to everything we need.

    To avoid circular unit interface dependencies, we put this helper in a
    separate unit by itself, since this unit may have dependencies on many uses
    in its interface section. On the other hand, this unit will be used by other
    units in their implementation section. So, please don't add anything here,
    that could be used in other unit's interface sections.

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
unit nodehelper;

interface

{$i fpcdefs.inc}

    uses
      node,paramgr,hlcgobj,cgobj,tgobj;

    type

      { tnodehelper }

      tnodehelper = class helper for tnode
      private
        function GetParaManager: TParaManager; inline;
        function GetTG: ttgobj; inline;
      public
        property paramanager: TParaManager read GetParaManager;
        property tg: ttgobj read GetTG;
      end;

implementation

  uses
    compiler;

  { tnodehelper }

  function tnodehelper.GetParaManager: TParaManager; inline;
    begin
      result:=self.compiler.paramanager;
    end;

  function tnodehelper.GetTG: ttgobj;
    begin
      result:=self.compiler.tg;
    end;

end.
