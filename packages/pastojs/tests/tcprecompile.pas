{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
    ./testpas2js --suite=TTestCLI_Precompile
    ./testpas2js --suite=TTestModule.TestEmptyUnit
}
unit tcprecompile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  tcunitsearch, tcmodules;

type

  { TTestCLI_Precompile }

  TTestCLI_Precompile = class(TCustomTestCLI)
  public
  published
    procedure TestPCU_EmptyUnit;
  end;

implementation

{ TTestCLI_Precompile }

procedure TTestCLI_Precompile.TestPCU_EmptyUnit;
var
  aFile, JSFile: TCLIFile;
  OrigSrc, NewSrc, s: String;
begin
  AddUnit('sub/system.pp',[''],['']);
  AddFile('test1.pas',[
    'begin',
    'end.']);
  AddDir('units');
  // compile, create  .pcu files
  {$IFDEF VerbosePJUFiler}
  writeln('TTestCLI_Precompile.TestPCU_EmptyUnit create pcu files=========================');
  {$ENDIF}
  Compile(['test1.pas','-Jc','-Fusub','-JUpcu','-FUunits']);
  aFile:=FindFile('units/system.pcu');
  AssertNotNull('units/system.pcu',aFile);
  JSFile:=FindFile('units/test1.js');
  AssertNotNull('units/test1.js',JSFile);
  OrigSrc:=JSFile.Source;

  // compile, using .pcu files
  {$IFDEF VerbosePJUFiler}
  writeln('TTestCLI_Precompile.TestPCU_EmptyUnit compile using pcu files==================');
  {$ENDIF}
  Compiler.Reset;
  Compile(['test1.pas','-Jc','-Fuunits']);
  NewSrc:=JSFile.Source;
  if not CheckSrcDiff(OrigSrc,NewSrc,s) then
    Fail('test1.js: '+s);
end;

Initialization
  RegisterTests([TTestCLI_Precompile]);
end.

