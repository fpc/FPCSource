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
  fpcunit, testregistry, Pas2jsFileUtils, Pas2JsFiler,
  tcunitsearch, tcmodules;

type

  { TCustomTestCLI_Precompile }

  TCustomTestCLI_Precompile = class(TCustomTestCLI)
  private
    FFormat: TPas2JSPrecompileFormat;
  protected
    procedure CheckPrecompile(MainFile, UnitPaths: string;
      SharedParams: TStringList = nil;
      FirstRunParams: TStringList = nil;
      SecondRunParams: TStringList = nil);
  public
    constructor Create; override;
    property Format: TPas2JSPrecompileFormat read FFormat write FFormat;
  end;

  { TTestCLI_Precompile }

  TTestCLI_Precompile = class(TCustomTestCLI_Precompile)
  published
    procedure TestPCU_EmptyUnit;
    procedure TestPCU_ParamNS;
    procedure TestPCU_Overloads;
    procedure TestPCU_UnitCycle;
  end;

function LinesToList(const Lines: array of string): TStringList;

implementation

function LinesToList(const Lines: array of string): TStringList;
var
  i: Integer;
begin
  Result:=TStringList.Create;
  for i:=Low(Lines) to High(Lines) do Result.Add(Lines[i]);
end;

{ TCustomTestCLI_Precompile }

procedure TCustomTestCLI_Precompile.CheckPrecompile(MainFile, UnitPaths: string;
  SharedParams: TStringList; FirstRunParams: TStringList;
  SecondRunParams: TStringList);
var
  UnitOutputDir, JSFilename, OrigSrc, NewSrc, s: String;
  JSFile: TCLIFile;
begin
  try
    UnitOutputDir:='units';
    AddDir(UnitOutputDir);
    // compile, create  .pcu files
    {$IFDEF VerbosePCUFiler}
    writeln('TTestCLI_Precompile.CheckPrecompile create pcu files=========================');
    {$ENDIF}
    Params.Clear;
    if SharedParams<>nil then
      Params.Assign(SharedParams);
    if FirstRunParams<>nil then
      Params.AddStrings(FirstRunParams);
    Compile([MainFile,'-Jc','-Fu'+UnitPaths,'-JU'+Format.Ext,'-FU'+UnitOutputDir]);
    AssertFileExists('units/system.'+Format.Ext);
    JSFilename:=UnitOutputDir+PathDelim+ExtractFilenameOnly(MainFile)+'.js';
    AssertFileExists(JSFilename);
    JSFile:=FindFile(JSFilename);
    OrigSrc:=JSFile.Source;
    // compile, using .pcu files
    {$IFDEF VerbosePCUFiler}
    writeln('TTestCLI_Precompile.CheckPrecompile compile using pcu files==================');
    {$ENDIF}
    JSFile.Source:='';
    Compiler.Reset;
    Params.Clear;
    if SharedParams<>nil then
      Params.Assign(SharedParams);
    if SecondRunParams<>nil then
      Params.AddStrings(SecondRunParams);
    Compile([MainFile,'-Jc','-FU'+UnitOutputDir]);
    NewSrc:=JSFile.Source;
    if not CheckSrcDiff(OrigSrc,NewSrc,s) then
    begin
      WriteSources;
      Fail('test1.js: '+s);
    end;
  finally
    SharedParams.Free;
    FirstRunParams.Free;
    SecondRunParams.Free;
  end;
end;

constructor TCustomTestCLI_Precompile.Create;
begin
  inherited Create;
  FFormat:=PrecompileFormats.FindExt('pcu');
end;

{ TTestCLI_Precompile }

procedure TTestCLI_Precompile.TestPCU_EmptyUnit;
begin
  AddUnit('src/system.pp',[''],['']);
  AddFile('test1.pas',[
    'begin',
    'end.']);
  CheckPrecompile('test1.pas','src');
end;

procedure TTestCLI_Precompile.TestPCU_ParamNS;
begin
  AddUnit('src/system.pp',[''],['']);
  AddUnit('src/foo.unit1.pp',['var i: longint;'],['']);
  AddFile('test1.pas',[
    'uses unit1;',
    'begin',
    '  i:=3;',
    'end.']);
  CheckPrecompile('test1.pas','src',LinesToList(['-NSfoo']));
end;

procedure TTestCLI_Precompile.TestPCU_Overloads;
begin
  AddUnit('src/system.pp',['type integer = longint;'],['']);
  AddUnit('src/unit1.pp',
  ['var i: integer;',
   'procedure DoIt(j: integer); overload;',
   'procedure DoIt(b: boolean);'],
  ['procedure DoIt(j: integer);',
   'begin',
   '  i:=j;',
   'end;',
   'procedure DoIt(b: boolean);',
   'begin',
   '  i:=3;',
   'end;']);
  AddUnit('src/unit2.pp',
  ['uses unit1;',
  'procedure DoIt(s: string); overload;'],
  ['procedure DoIt(s: string);',
   'begin',
   '  unit1.i:=j;',
   'end;']);
  AddFile('test1.pas',[
    'uses unit1;',
    'procedure DoIt(d: double); overload;',
    'begin',
    '  unit1.i:=j;',
    'end;',
    'begin',
    '  DoIt(3);',
    '  DoIt(''abc'');',
    '  Do1(true);',
    '  Do1(3.3);',
    'end.']);
  CheckPrecompile('test1.pas','src');
end;

procedure TTestCLI_Precompile.TestPCU_UnitCycle;
begin
  AddUnit('src/system.pp',['type integer = longint;'],['']);
  AddUnit('src/unit1.pp',
  ['var i: integer;',
   'procedure Do1(j: integer);'],
  ['uses unit2;',
   'procedure Do1(j: integer);',
   'begin',
   '  Do2(j);',
   'end;']);
  AddUnit('src/unit2.pp',
  ['uses unit1;',
  'procedure Do2(j: integer);'],
  ['procedure Do2(j: integer);',
   'begin',
   '  unit1.i:=j;',
   'end;']);
  AddFile('test1.pas',[
    'uses unit1;',
    'begin',
    '  Do1(3);',
    'end.']);
  CheckPrecompile('test1.pas','src');
end;

Initialization
  RegisterTests([TTestCLI_Precompile]);
end.

