{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 Michael Van Canneyt (michael@freepascal.org)

    Run wasm consortium WIT testsuite tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utcrundirtests;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  WIT.Model,
  WIT.Scanner,
  WIT.Parser;


type
  { TSchemaFileTest }

  { TFileTest }

  TFileTest = class(TAssert)
  Private
    FIgnores: Boolean;
    FFileName : String;
  protected
    function GetTestName: string; override;
    function GetTestSuiteName: string; override;
    function GetEnableIgnores: boolean; override;
    procedure SetEnableIgnores(Value: boolean); override;
    procedure SetTestSuiteName(const aName: string); override;
    procedure TestFile; virtual;
  Public
    constructor Create(const aFileName: String);
    function CountTestCases: integer; override;
    procedure Run(AResult: TTestResult); override;
    property TestFileName : String Read FFileName;
  end;

  { TDirectoryFileTests }
  TDirectoryFileTests = class(TTestSuite)
  public
    Constructor CreateFromDir(const aDir : String);
  end;

implementation

function TFileTest.GetTestName: string;
begin
  Result:=ChangeFileExt(ExtractFileName(FFileName),'')
end;

function TFileTest.GetTestSuiteName: string;
begin
  Result:='';
end;

function TFileTest.GetEnableIgnores: boolean;
begin
  Result:=FIgnores;
end;

procedure TFileTest.SetEnableIgnores(Value: boolean);
begin
  FIgnores:=Value;
end;

procedure TFileTest.SetTestSuiteName(const aName: string);
begin
  // Nothing
end;

procedure TFileTest.TestFile;
var
  lScanner : TWITScanner;
  lParser : TWITParser;
  lStream : TStream;
  lDocument : TWITDocument;
  lErr : string;
begin
  lScanner:=Nil;
  lParser:=Nil;
  lDocument:=Nil;
  lStream:=TFileStream.Create(FFileName,fmOpenRead or fmShareDenyWrite);
  try
    lScanner:=TWITScanner.Create(lStream);
    lParser:=TWITParser.Create(lScanner,True);
    try
      lScanner:=Nil;
      LDocument:=LParser.ParseDocument;
    except
      on E : exception do
        lErr:=E.ClassName+' : '+E.Message;;
    end;
    if lErr<>'' then
      Fail('Failed to parse '+TestFileName+' : '+lErr);
  finally
    LDocument.Free;
    lScanner.Free;
    lParser.Free;
    lStream.Free;
 end;

end;

constructor TFileTest.Create(const aFileName: String);
begin
  FFileName:=aFileName;
end;

function TFileTest.CountTestCases: integer;
begin
  Result:=1;
end;


Procedure DoRun(aTest: TTest; aResult: TTestResult);

begin
  TFileTest(aTest).TestFile;
end;

procedure TFileTest.Run(AResult: TTestResult);
begin
  aResult.StartTest(Self);
  FLastStep:=TTestStep.stRunTest;
  AResult.RunProtected(Self,@DoRun);
  aResult.EndTest(Self);
end;

{ TDirectoryFileTests }

constructor TDirectoryFileTests.CreateFromDir(const aDir: String);
var
  lInfo : TSearchRec;
  lDir : String;
begin
  inherited create('TestDir');
  lDir:=IncludeTrailingPathDelimiter(aDir);
  if FindFirst(lDir+'*.wit',0,lInfo)=0 then
    try
      Repeat
        AddTest(TFileTest.Create(lDir+lInfo.Name));
      until FindNext(lInfo)<>0;
    finally
      FindClose(lInfo);
    end;
end;


end.

