{
    Copyright (c) 2026 by Nikolay Nikolov
    Member of the Free Pascal development team

    This unit implements the class helper for the ttgobj class.

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
unit tghelper;

interface

{$i fpcdefs.inc}

    uses
      tgobj,paramgr,cgobj,hlcgobj;

    type

      { ttgobjhelper }

      ttgobjhelper = class helper for ttgobj
      private
        function GetCG: tcg; inline;
        function GetHLCG: thlcgobj; inline;
        function GetParaManager: tparamanager; inline;
      public
        property cg: tcg read GetCG;
        property hlcg: thlcgobj read GetHLCG;
        property paramanager: tparamanager read GetParaManager;
      end;

implementation

  uses
    compiler;

  { ttgobjhelper }

  function ttgobjhelper.GetCG: tcg; inline;
    begin
      result:=self.compiler.cg;
    end;

  function ttgobjhelper.GetHLCG: thlcgobj; inline;
    begin
      result:=self.compiler.hlcg;
    end;

  function ttgobjhelper.GetParaManager: tparamanager; inline;
    begin
      result:=self.compiler.paramanager;
    end;

end.
