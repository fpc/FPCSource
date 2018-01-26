{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript precompile class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
   ./testpas2js --suite=TTestPrecompile.TestPC_EmptyUnit
}
unit tcfiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree, PScanner, PasResolver,
  FPPas2Js,
  tcmodules;

type

  { TCustomTestPrecompile }

  TCustomTestPrecompile = Class(TCustomTestModule)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure WriteUnit; virtual;
  public
  end;

  { TTestPrecompile }

  TTestPrecompile = class(TCustomTestPrecompile)
  published
    procedure TestPC_EmptyUnit;
  end;

implementation

{ TCustomTestPrecompile }

procedure TCustomTestPrecompile.SetUp;
begin
  inherited SetUp;

end;

procedure TCustomTestPrecompile.TearDown;
begin

  inherited TearDown;
end;

procedure TCustomTestPrecompile.WriteUnit;
begin

end;

{ TTestPrecompile }

procedure TTestPrecompile.TestPC_EmptyUnit;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  ConvertUnit;
  WriteUnit;
end;

Initialization
  RegisterTests([TTestPrecompile]);
end.

