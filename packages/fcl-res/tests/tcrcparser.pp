{ Tests for resource names and file names in .rc scripts read by TRCResourceReader. }
unit tcRCParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, resource, rcreader;

type

  { TTestRCNames }

  TTestRCNames = class(TTestCase)
  private
    FDir: string;
    FOldDir: string;
    FResources: TResources;
    procedure WriteDataFile(const aFilename, aContent: string);
    procedure ReadRC(const aScript: string);
    function DataOf(aResource: TAbstractResource): string;
    procedure AssertNamedResource(const aMsg, aName, aContent: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDottedNameDottedFile;
    procedure TestPlainNamePlainFile;
    procedure TestPlainNameDottedFile;
    procedure TestPlainNameQuotedFile;
    procedure TestQuotedNameDottedFile;
    procedure TestDottedNameSameAsQuotedName;
    procedure TestNumericNamePlainFile;
    procedure TestDottedNameQuotedFile;
    procedure TestOptionPlainFile;
    procedure TestOptionRawData;
    procedure TestRawData;
  end;

implementation

{ TTestRCNames }

procedure TTestRCNames.SetUp;

begin
  inherited SetUp;
  FOldDir:=GetCurrentDir;
  FDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'trcparser'+IntToStr(GetProcessID)+PathDelim;
  AssertTrue('Create test directory '+FDir,ForceDirectories(FDir));
  AssertTrue('Change to test directory '+FDir,SetCurrentDir(FDir));
  WriteDataFile('test.bin','dotted data');
  WriteDataFile('testbin','plain data');
  FResources:=TResources.Create;
end;


procedure TTestRCNames.TearDown;

begin
  FreeAndNil(FResources);
  SetCurrentDir(FOldDir);
  DeleteFile(FDir+'test.bin');
  DeleteFile(FDir+'testbin');
  RemoveDir(FDir);
  inherited TearDown;
end;


procedure TTestRCNames.WriteDataFile(const aFilename, aContent: string);

var
  lStream: TFileStream;

begin
  lStream:=TFileStream.Create(FDir+aFilename,fmCreate);
  try
    lStream.WriteBuffer(aContent[1],Length(aContent));
  finally
    lStream.Free;
  end;
end;


procedure TTestRCNames.ReadRC(const aScript: string);

var
  lStream: TStringStream;
  lReader: TRCResourceReader;

begin
  lStream:=TStringStream.Create(aScript+LineEnding);
  lReader:=TRCResourceReader.Create;
  try
    FResources.LoadFromStream(lStream,lReader);
  finally
    lReader.Free;
    lStream.Free;
  end;
end;


function TTestRCNames.DataOf(aResource: TAbstractResource): string;

begin
  Result:='';
  SetLength(Result,aResource.RawData.Size);
  aResource.RawData.Position:=0;
  if Length(Result)>0 then
    aResource.RawData.ReadBuffer(Result[1],Length(Result));
end;


procedure TTestRCNames.AssertNamedResource(const aMsg, aName, aContent: string);

begin
  AssertEquals(aMsg+': resource count',1,FResources.Count);
  AssertEquals(aMsg+': resource type',RT_RCDATA,FResources[0]._Type.ID);
  AssertTrue(aMsg+': resource has a name',FResources[0].Name.DescType=dtName);
  AssertEquals(aMsg+': resource name',aName,FResources[0].Name.Name);
  AssertEquals(aMsg+': resource data',aContent,DataOf(FResources[0]));
end;


procedure TTestRCNames.TestDottedNameDottedFile;

begin
  ReadRC('test.bin RCDATA test.bin');
  AssertNamedResource('test.bin RCDATA test.bin','TEST.BIN','dotted data');
end;


procedure TTestRCNames.TestPlainNamePlainFile;

begin
  ReadRC('testbin RCDATA testbin');
  AssertNamedResource('testbin RCDATA testbin','TESTBIN','plain data');
end;


procedure TTestRCNames.TestPlainNameDottedFile;

begin
  ReadRC('test RCDATA test.bin');
  AssertNamedResource('test RCDATA test.bin','TEST','dotted data');
end;


procedure TTestRCNames.TestPlainNameQuotedFile;

begin
  ReadRC('test RCDATA "test.bin"');
  AssertNamedResource('test RCDATA "test.bin"','TEST','dotted data');
end;


procedure TTestRCNames.TestQuotedNameDottedFile;

begin
  ReadRC('"test.bin" RCDATA test.bin');
  AssertNamedResource('"test.bin" RCDATA test.bin','TEST.BIN','dotted data');
end;


procedure TTestRCNames.TestDottedNameSameAsQuotedName;

var
  lQuoted: TResources;

begin
  ReadRC('test.bin RCDATA test.bin');
  lQuoted:=FResources;
  FResources:=TResources.Create;
  try
    ReadRC('"test.bin" RCDATA test.bin');
    AssertEquals('Same name for dotted and quoted resource name',
      lQuoted[0].Name.Name,FResources[0].Name.Name);
  finally
    lQuoted.Free;
  end;
end;


procedure TTestRCNames.TestNumericNamePlainFile;

begin
  ReadRC('1 RCDATA testbin');
  AssertEquals('1 RCDATA testbin: resource count',1,FResources.Count);
  AssertTrue('1 RCDATA testbin: resource has an ID',FResources[0].Name.DescType=dtID);
  AssertEquals('1 RCDATA testbin: resource ID',1,FResources[0].Name.ID);
  AssertEquals('1 RCDATA testbin: resource data','plain data',DataOf(FResources[0]));
end;


procedure TTestRCNames.TestDottedNameQuotedFile;

begin
  ReadRC('test.bin RCDATA "test.bin"');
  AssertNamedResource('test.bin RCDATA "test.bin"','TEST.BIN','dotted data');
end;


procedure TTestRCNames.TestOptionPlainFile;

begin
  ReadRC('test RCDATA DISCARDABLE testbin');
  AssertNamedResource('test RCDATA DISCARDABLE testbin','TEST','plain data');
  AssertTrue('test RCDATA DISCARDABLE testbin: discardable flag',
    (FResources[0].MemoryFlags and MF_DISCARDABLE)<>0);
end;


procedure TTestRCNames.TestOptionRawData;

begin
  ReadRC('test RCDATA DISCARDABLE'+LineEnding+'BEGIN'+LineEnding+'  "abc"'+LineEnding+'END');
  AssertNamedResource('test RCDATA DISCARDABLE BEGIN "abc" END','TEST','abc');
  AssertTrue('test RCDATA DISCARDABLE BEGIN "abc" END: discardable flag',
    (FResources[0].MemoryFlags and MF_DISCARDABLE)<>0);
end;


procedure TTestRCNames.TestRawData;

begin
  ReadRC('test RCDATA { "abc" }');
  AssertNamedResource('test RCDATA { "abc" }','TEST','abc');
end;


initialization
  RegisterTest(TTestRCNames);
end.
