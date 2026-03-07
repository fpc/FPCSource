{
    Copyright (c) 2026 by Nikolay Nikolov

    This unit defines a class, that contains the optimization modules

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
unit opt;

{$i fpcdefs.inc}

interface

uses
  sysutils,
  compilerbase,
  optconstprop,optcse,optloop;

type

  { TOptimizers }

  TOptimizers = class
  private
    FConstProp: TConstPropOptimizer;
    FCSE: TCSEOptimizer;
    FLoop: TLoopOptimizer;
  public
    constructor Create(ACompiler: TCompilerBase);
    destructor Destroy; override;
    property ConstProp: TConstPropOptimizer read FConstProp;
    property CSE: TCSEOptimizer read FCSE;
    property Loop: TLoopOptimizer read FLoop;
  end;

implementation

{ TOptimizers }

constructor TOptimizers.Create(ACompiler: TCompilerBase);
begin
  FConstProp:=TConstPropOptimizer.Create(ACompiler);
  FCSE:=TCSEOptimizer.Create(ACompiler);
  FLoop:=TLoopOptimizer.Create(ACompiler);
end;

destructor TOptimizers.Destroy;
begin
  FreeAndNil(FLoop);
  FreeAndNil(FCSE);
  FreeAndNil(FConstProp);
  inherited Destroy;
end;

end.
