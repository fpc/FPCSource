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
      SecondRunParams: TStringList = nil; ExpExitCode: integer = 0);
  public
    constructor Create; override;
    property Format: TPas2JSPrecompileFormat read FFormat write FFormat;
  end;

  { TTestCLI_Precompile }

  TTestCLI_Precompile = class(TCustomTestCLI_Precompile)
  published
    procedure TestPCU_EmptyUnit;
    procedure TestPCU_UTF8BOM;
    procedure TestPCU_ParamNS;
    procedure TestPCU_Overloads;
    procedure TestPCU_UnitCycle;
    procedure TestPCU_ClassForward;
    procedure TestPCU_ClassConstructor;
    {$IFDEF EnableInterfaces}
    procedure TestPCU_ClassInterface;
    {$ELSE}
    procedure TestPCU_IgnoreInterface;
    {$ENDIF}
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

procedure TCustomTestCLI_Precompile.CheckPrecompile(MainFile,
  UnitPaths: string; SharedParams: TStringList; FirstRunParams: TStringList;
  SecondRunParams: TStringList; ExpExitCode: integer);
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
    Compile([MainFile,'-Jc','-FU'+UnitOutputDir],ExpExitCode);
    if ExpExitCode=0 then
      begin
      NewSrc:=JSFile.Source;
      if not CheckSrcDiff(OrigSrc,NewSrc,s) then
      begin
        WriteSources;
        Fail('test1.js: '+s);
      end;
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
  FFormat:=PrecompileFormats[0];
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

procedure TTestCLI_Precompile.TestPCU_UTF8BOM;
var
  aFile: TCLIFile;
begin
  aFile:=AddUnit('src/system.pp',
    ['var',
    '  s: string = ''aaaäö'';',
    '  s2: string = ''😊'';', // 1F60A
    ''],
    ['']);
  aFile.Source:=UTF8BOM+aFile.Source;
  aFile:=AddFile('test1.pas',[
    'begin',
    '  s:=''ö😊'';',
    'end.']);
  aFile.Source:=UTF8BOM+aFile.Source;
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
   '  unit1.i:=length(s);',
   'end;']);
  AddFile('test1.pas',[
    'uses unit1, unit2;',
    'procedure DoIt(d: double); overload;',
    'begin',
    '  unit1.i:=4;',
    'end;',
    'begin',
    '  DoIt(3);',
    '  DoIt(''abc'');',
    '  DoIt(true);',
    '  DoIt(3.3);',
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

procedure TTestCLI_Precompile.TestPCU_ClassForward;
begin
  AddUnit('src/system.pp',[
    'type integer = longint;',
    'procedure Writeln; varargs;'],
    ['procedure Writeln; begin end;']);
  AddUnit('src/unit1.pp',
  ['type',
   '  TClass = class of TObject;',
   '  TBirdClass = class of TBird;',
   '  TObject = class',
   '    FBirdClass: TBirdClass;',
   '    constructor Create;',
   '    constructor Create(Id: integer);',
   '    property BirdClass: TBirdClass read FBirdClass;',
   '  end;',
   '  TBird = class',
   '    constructor Create(d: double); overload;',
   '  end;',
   ''],
  ['constructor TObject.Create; begin end;',
   'constructor TObject.Create(Id: integer); begin end;',
   'constructor TBird.Create(d: double); begin end;']);
  AddFile('test1.pas',[
    'uses unit1;',
    'var',
    '  b: TBird;',
    '  c: TClass;',
    'begin',
    '  c:=TObject;',
    '  c:=TBird;',
    '  c:=b.BirdClass;',
    '  b:=TBird.Create;',
    '  b:=TBird.Create(1);',
    '  b:=TBird.Create(3.3);',
    'end.']);
  CheckPrecompile('test1.pas','src');
end;

procedure TTestCLI_Precompile.TestPCU_ClassConstructor;
begin
  AddUnit('src/system.pp',[
    'type integer = longint;',
    'procedure Writeln; varargs;'],
    ['procedure Writeln; begin end;']);
  AddUnit('src/unit1.pp',[
    'type',
    '  TObject = class',
    '    constructor Create;',
    '  end;',
    '  TBird = class',
    '    constructor Create; reintroduce;',
    '  end;',
    '  TCow = class',
    '    constructor Create; reintroduce;',
    '  end;',
    ''],[
    'constructor TObject.Create; begin end;',
    'constructor TBird.Create; begin end;',
    'constructor TCow.Create; begin end;',
    '']);
  AddUnit('src/unit2.pp',[
    'uses unit1;',
    'procedure DoIt;',
    ''],[
    'procedure DoIt;',
    'begin',
    '  TBird.Create;',
    '  TCow.Create;',
    'end;',
    '']);
  AddFile('test1.pas',[
    'uses unit2;',
    'begin',
    '  DoIt;',
    'end.']);
  CheckPrecompile('test1.pas','src');
end;

{$IFDEF EnableInterfaces}
procedure TTestCLI_Precompile.TestPCU_ClassInterface;
begin
  AddUnit('src/system.pp',[
    '{$interfaces corba}',
    'type',
    '  integer = longint;',
    '  IUnknown = interface',
    '  end;',
    'procedure Writeln; varargs;'],
    ['procedure Writeln; begin end;']);
  AddUnit('src/unit1.pp',[
    'type',
    '  IIntf = interface',
    '    function GetItems(Index: longint): longint;',
    '    procedure SetItems(Index: longint; Value: longint);',
    '    property Items[Index: longint]: longint read GetItems write SetItems; default;',
    '  end;',
    ''],[
    '']);
  AddUnit('src/unit2.pp',[
    'uses unit1;',
    'type',
    '  IAlias = IIntf;',
    '  TObject = class end;',
    '  TBird = class(IIntf)',
    '  strict private',
    '    function IIntf.GetItems = FetchItems;',
    '    function FetchItems(Index: longint): longint; virtual; abstract;',
    '    procedure SetItems(Index: longint; Value: longint); virtual; abstract;',
    '  end;',
    ''],[
    '']);
  AddUnit('src/unit3.pp',[
    'uses unit2;',
    'type',
    '  TEagle = class(TBird)',
    '    function FetchItems(Index: longint): longint; override;',
    '    procedure SetItems(Index: longint; Value: longint); override;',
    '  end;',
    '  TFlying = class(IAlias)',
    '  strict private',
    '    FEagle: TEagle;',
    '    property Eagle: TEagle read FEagle implements IAlias;',
    '  public',
    '    constructor Create;',
    '  end;',
    ''],[
    'function TEagle.FetchItems(Index: longint): longint; begin end;',
    'procedure TEagle.SetItems(Index: longint; Value: longint); begin end;',
    'constructor TFlying.Create;',
    'begin',
    '  FEagle:=nil;',
    'end;',
    '']);
  AddFile('test1.pas',[
    'uses unit2, unit3;',
    'type IAlias2 = IAlias;',
    'var',
    '  f: TFlying;',
    '  i: IAlias2;',
    'begin',
    '  f:=TFlying.Create;',
    '  i:=f;',
    '  i[2]:=i[3];',
    'end.']);
  CheckPrecompile('test1.pas','src');
end;
{$ELSE}
procedure TTestCLI_Precompile.TestPCU_IgnoreInterface;
begin
  AddUnit('src/system.pp',[
    'type integer = longint;',
    'procedure Writeln; varargs;'],
    ['procedure Writeln; begin end;']);
  AddUnit('src/unit1.pp',[
    'type',
    '  IIntf = interface',
    '    function GetItems: longint;',
    '    procedure SetItems(Index: longint; Value: longint);',
    '    property Items[Index: longint]: longint read GetItems write SetItems;',
    '  end;',
    ''],[
    '']);
  AddUnit('src/unit2.pp',[
    'uses unit1;',
    'type',
    '  IAlias = IIntf;',
    '  TObject = class end;',
    '  TBird = class(TObject,IIntf) end;',
    ''],[
    '']);
  AddFile('test1.pas',[
    'uses unit2;',
    'type IAlias2 = IAlias;',
    'var b: TBird;',
    'begin',
    '  if b=nil then ;',
    'end.']);
  CheckPrecompile('test1.pas','src');
end;
{$ENDIF}

Initialization
  RegisterTests([TTestCLI_Precompile]);
end.

