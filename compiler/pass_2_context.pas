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
  node,hlcgobj,cgobj,tgobj,verbose,systems,globals;

type

  { tpassgeneratecodecontextimpl }

  tpassgeneratecodecontextimpl = class(tpassgeneratecodecontext)
  private
    verbose: TVerbose;
    target: TReadOnlyCompilerTarget;
    globals: TCompilerGlobals;
    hlcg: thlcgobj;
    tg: ttgobj;
    has_parent_tg: Boolean;
  public
    constructor create(acompiler: TCompilerBase; parent_tg: ttgobj);

    procedure create_hlcodegen(acompiler: TCompilerBase);
    procedure create_tempgen(acompiler: TCompilerBase);
  end;

  { tpassgeneratecodecontexthelper }

  tpassgeneratecodecontexthelper = class helper for tpassgeneratecodecontext
  private
    function GetCg: tcg; inline;
    function GetGlobals: TCompilerGlobals; inline;
    function GetHlcg: thlcgobj; inline;
{$ifdef cpu64bitalu}
    function GetCG128: tcg128; inline;
    function GetTarget: TReadOnlyCompilerTarget; inline;
{$else cpu64bitalu}
    function GetCG64: tcg64; inline;
{$endif cpu64bitalu}
    function GetTG: ttgobj;
    function GetVerbose: TVerbose; inline;
  public
    property Verbose: TVerbose read GetVerbose;
    property Target: TReadOnlyCompilerTarget read GetTarget;
    property Globals: TCompilerGlobals read GetGlobals;
    property hlcg: thlcgobj read GetHlcg;
    property cg: tcg read GetCg;
{$ifdef cpu64bitalu}
    property cg128: tcg128 read GetCG128;
{$else cpu64bitalu}
    property cg64: tcg64 read GetCG64;
{$endif cpu64bitalu}
    property tg: ttgobj read GetTG;
  end;

implementation

uses
  compiler;

{ tpassgeneratecodecontextimpl }

constructor tpassgeneratecodecontextimpl.create(acompiler: TCompilerBase; parent_tg: ttgobj);
begin
  verbose:=acompiler.verbose;
  target:=acompiler.target;
  globals:=acompiler.globals;
  tg:=parent_tg;
  has_parent_tg:=(tg<>nil);
end;

procedure tpassgeneratecodecontextimpl.create_hlcodegen(acompiler: TCompilerBase);
begin
  hlcgobj.create_hlcodegen(acompiler);
  hlcg:=acompiler.hlcg;
end;

procedure tpassgeneratecodecontextimpl.create_tempgen(acompiler: TCompilerBase);
begin
  if has_parent_tg then
    internalerror(2026051001);
  tcompiler(acompiler).tg:=tgobjclass.create(acompiler);
  tg:=acompiler.tg;
end;

{ tpassgeneratecodecontexthelper }

function tpassgeneratecodecontexthelper.GetCg: tcg; inline;
begin
  result:=hlcg.CG;
end;

function tpassgeneratecodecontexthelper.GetGlobals: TCompilerGlobals;
begin
  result:=tpassgeneratecodecontextimpl(self).globals;
end;

function tpassgeneratecodecontexthelper.GetHlcg: thlcgobj; inline;
begin
  result:=tpassgeneratecodecontextimpl(self).hlcg;
end;

{$ifdef cpu64bitalu}
function tpassgeneratecodecontexthelper.GetCG128: tcg128; inline;
begin
  result:=cg.cg128;
end;

function tpassgeneratecodecontexthelper.GetTarget: TReadOnlyCompilerTarget; inline;
begin
  result:=tpassgeneratecodecontextimpl(self).target;
end;

{$else cpu64bitalu}
function tpassgeneratecodecontexthelper.GetCG64: tcg64; inline;
begin
  result:=cg.cg64;
end;
{$endif cpu64bitalu}

function tpassgeneratecodecontexthelper.GetTG: ttgobj;
begin
  result:=tpassgeneratecodecontextimpl(self).tg;
end;

function tpassgeneratecodecontexthelper.GetVerbose: TVerbose; inline;
begin
  result:=tpassgeneratecodecontextimpl(self).verbose;
end;

end.
