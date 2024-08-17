{
    This file is part of the Free Component Library

    Testsuite to load schema from official testsuite
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utOfficialTests;


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson.schema.testutils;

Type
  
  { TSchemaFileTest }

  TSchemaFileTest = class(TAssert)
  Private
    FIgnores: Boolean;
    FTestDef: TTestDef;
  protected
    function GetTestName: string; override;
    function GetTestSuiteName: string; override;
    function GetEnableIgnores: boolean; override;
    procedure SetEnableIgnores(Value: boolean); override;
    procedure SetTestSuiteName(const aName: string); override;
    procedure TestSchema; virtual; abstract;
  Public
    constructor Create(aTestDef : TTestDef);
    function CountTestCases: integer; override;
    procedure Run(AResult: TTestResult); override;
    property TestDef : TTestDef Read FTestDef;
  end;

  TSchemaReaderFileTest = class(TSchemaFileTest)
    procedure TestSchema; override;
  end;

  TSchemaLoaderFileTest = class(TSchemaFileTest)
    procedure TestSchema; override;
  end;


  { TSchemaFileTests }

  TSchemaFileTests = class(TTestSuite)
  private
    FList : TTestDefs;
  Protected
    procedure FindTestFiles(const aDir : String; aFiles : TStrings);
    procedure AddAllTests(const aDir : string);
    procedure DoAddTestDef(aTest : TTestDef); virtual; abstract;
  Public
    Constructor Create(const aTestDir : String); reintroduce;
    destructor destroy; override;
  end;

  { TSchemaFileReaderTests }

  TSchemaFileReaderTests = class(TSchemaFileTests)
  protected
    procedure DoAddTestDef(aTest : TTestDef); override;
  end;

  { TSchemaFileLoaderTests }

  TSchemaFileLoaderTests = class(TSchemaFileTests)
  protected
    procedure DoAddTestDef(aTest : TTestDef); override;
  end;


Procedure RegisterTestFiles(const aDir : String);

implementation

uses fpjson.schema.loader,  fpjson.schema.reader, fpjson.schema.schema;

procedure RegisterTestFiles(const aDir: String);
begin
  RegisterTest('Reader JSONSchema testsuite',TSchemaFileReaderTests.Create(aDir));
  RegisterTest('Loader JSONSchema testsuite',TSchemaFileLoaderTests.Create(aDir));
end;

{ TSchemaFileTest }

function TSchemaFileTest.CountTestCases: integer;
begin
  Result:=1;
end;

function TSchemaFileTest.GetTestName: string;
begin
  Result:=ChangeFileExt(ExtractFileName(FTestDef.FileName),'')+' : '+FTestDef.Description;
end;

function TSchemaFileTest.GetTestSuiteName: string;
begin
  Result:='';
end;

function TSchemaFileTest.GetEnableIgnores: boolean;
begin
  Result:=FIgnores;
end;

procedure TSchemaFileTest.SetEnableIgnores(Value: boolean);
begin
  FIgnores:=Value
end;

procedure TSchemaFileTest.SetTestSuiteName(const aName: string);
begin
  // Do nothing
end;

Procedure DoRun(aTest: TTest; aResult: TTestResult);

begin
  TSchemaFileTest(aTest).TestSchema;
end;

procedure TSchemaFileTest.Run(AResult: TTestResult);
begin
  aResult.StartTest(Self);
  AResult.RunProtected(Self,@DoRun);
  aResult.EndTest(Self);
end;

constructor TSchemaFileTest.Create(aTestDef: TTestDef);
begin
  FTestDef:=aTestDef;
end;

{ TSchemaFileTests }


procedure TSchemaFileTests.FindTestFiles(const aDir: String; aFiles: TStrings);

var
  Info : TSearchRec;
  D : String;

begin
  D:=IncludeTrailingPathDelimiter(aDir);
  if FindFirst(D+'*.json',faNormal,Info)=0 then
    try
      Repeat
        aFiles.Add(D+Info.Name);
      until FindNext(Info)<>0;
    finally
      FindClose(Info);
    end;
end;

procedure TSchemaReaderFileTest.TestSchema;

var
  S : TJSONSchema;
  Reader : TJsonSchemaReader;

begin
  Reader:=Nil;
  S:=TJSONSchema.Create;
  try
    Reader:=TJSONSChemaReader.Create(Nil);
    Reader.ReadFromString(S,TestDef.Schema.AsJSON);
  finally
    S.Free;
    Reader.Free;
  end;
end;

procedure TSchemaLoaderFileTest.TestSchema;

var
  S : TJSONSchema;
  Loader : TJsonSchemaLoader;

begin
  Loader:=Nil;
  S:=TJSONSchema.Create;
  try
    Loader:=TJSONSChemaLoader.Create(Nil);
    Loader.ReadFromJSON(S,TestDef.Schema);
  finally
    S.Free;
    Loader.Free;
  end;
end;


procedure TSchemaFileTests.AddAllTests(const aDir: string);
Var
  aFile : String;
  aFiles : TStrings;
  i : integer;

begin
  if aDir='' then
    Ignore('No test dir specified');
  aFiles:=TStringList.Create;
  try
    FindTestFiles(aDir,aFiles);
    FList.Clear;
    for aFile in aFiles do
      ExtractTestsFromFile(aFile,FList);
    For I:=0 to FList.Count-1 do
      DoAddTestDef(FList[I]);

  finally
    aFiles.Free;
  end;
end;

constructor TSchemaFileTests.Create(const aTestDir: String);
begin
  Inherited Create();
  FList:=TTestDefs.Create(True);
  AddAllTests(aTestDir);
end;

destructor TSchemaFileTests.destroy;
begin
  FreeAndNil(FList);
  inherited destroy;
end;

{ TSchemaFileReaderTests }

procedure TSchemaFileReaderTests.DoAddTestDef(aTest: TTestDef);
begin
  AddTest(TSchemaReaderFileTest.Create(aTest));
end;

{ TSchemaFileLoaderTests }

procedure TSchemaFileLoaderTests.DoAddTestDef(aTest: TTestDef);
begin
  AddTest(TSchemaLoaderFileTest.Create(aTest));
end;

end.

