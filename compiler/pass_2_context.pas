{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit handles the codegeneration pass

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
unit pass_2_context;

{$i fpcdefs.inc}

interface

uses
  compilerbase,
  node,hlcgobj;

type

  { tpassgeneratecodecontextimpl }

  tpassgeneratecodecontextimpl = class(tpassgeneratecodecontext)
    hlcg: thlcgobj;
    procedure create_hlcodegen(acompiler: TCompilerBase);
  end;

  { tpassgeneratecodecontexthelper }

  tpassgeneratecodecontexthelper = class helper for tpassgeneratecodecontext
  private
    function GetHlcg: thlcgobj; inline;
  public
    property hlcg: thlcgobj read GetHlcg;
  end;

implementation

uses
  compiler;

{ tpassgeneratecodecontextimpl }

procedure tpassgeneratecodecontextimpl.create_hlcodegen(acompiler: TCompilerBase);
begin
  hlcgobj.create_hlcodegen(acompiler);
  hlcg:=acompiler.hlcg;
end;

{ tpassgeneratecodecontexthelper }

function tpassgeneratecodecontexthelper.GetHlcg: thlcgobj; inline;
begin
  result:=tpassgeneratecodecontextimpl(self).hlcg;
end;

end.
