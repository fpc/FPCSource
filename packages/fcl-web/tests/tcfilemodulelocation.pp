{ Tests the containment check of TFPCustomFileModule for registered file locations, with and without a script name. }
unit tcfilemodulelocation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, httpdefs, fpwebfile;

type

  { TLocationTestResponse }

  // Response that sends nothing.
  TLocationTestResponse = class(TResponse)
  protected
    procedure DoSendHeaders(aHeaders: TStrings); override;
    procedure DoSendContent; override;
  end;

  { TTestFileModuleLocation }

  TTestFileModuleLocation = class(TTestCase)
  private
    FRoot: String;
    FOutside: String;
    procedure WriteFile(const aFileName: String);
    function Serve(const aLocation, aScriptName, aPath: String): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestServeWithoutScriptName;
    procedure TestTraversalWithoutScriptName;
    procedure TestServeWithScriptName;
    procedure TestTraversalWithScriptName;
    procedure TestServeOutsideExeDirWithoutScriptName;
    procedure TestServeOutsideExeDirWithScriptName;
  end;

implementation

const
  cLocation = 'tcfmlfiles';
  cOutsideLocation = 'tcfmloutside';
  cScriptName = '/cgi-bin/app.cgi';


{ TLocationTestResponse }

procedure TLocationTestResponse.DoSendHeaders(aHeaders: TStrings);

begin
end;


procedure TLocationTestResponse.DoSendContent;

begin
end;


{ TTestFileModuleLocation }

// Create aFileName with a short text.
procedure TTestFileModuleLocation.WriteFile(const aFileName: String);

var
  lList: TStringList;

begin
  lList:=TStringList.Create;
  try
    lList.Add('test');
    lList.SaveToFile(aFileName);
  finally
    lList.Free;
  end;
end;


// Serve aPath from location aLocation with a file module as TWebHandler.SetBaseURL sets it up; return the status code.
function TTestFileModuleLocation.Serve(const aLocation, aScriptName, aPath: String): Integer;

var
  lModule: TFPCustomFileModule;
  lRequest: TRequest;
  lResponse: TLocationTestResponse;

begin
  lModule:=TFPCustomFileModule.CreateNew(nil,0);
  lRequest:=TRequest.Create;
  lResponse:=TLocationTestResponse.Create(lRequest);
  try
    lRequest.Method:='GET';
    lRequest.ScriptName:=aScriptName;
    lRequest.PathInfo:='/'+aLocation+'/'+aPath;
    if aScriptName='' then
      lModule.BaseURL:=aLocation+'/'
    else
      lModule.BaseURL:=aScriptName+'/'+aLocation+'/';
    lModule.HandleRequest(lRequest,lResponse);
    Result:=lResponse.Code;
  finally
    lResponse.Free;
    lRequest.Free;
    lModule.Free;
  end;
end;


procedure TTestFileModuleLocation.SetUp;

begin
  FRoot:=ExtractFilePath(ExpandFileName(ParamStr(0)))+'tcfml'+PathDelim;
  FOutside:=IncludeTrailingPathDelimiter(ExpandFileName(GetTempDir(False)))+'tcfml-outside'+PathDelim;
  ForceDirectories(FRoot+'public');
  ForceDirectories(FOutside);
  WriteFile(FRoot+'public'+PathDelim+'inside.txt');
  WriteFile(FRoot+'secret.txt');
  WriteFile(FOutside+'inside.txt');
  RegisterFileLocation(cLocation,FRoot+'public');
  RegisterFileLocation(cOutsideLocation,FOutside);
end;


procedure TTestFileModuleLocation.TearDown;

begin
  UnRegisterFileLocation(cLocation);
  UnRegisterFileLocation(cOutsideLocation);
  DeleteFile(FRoot+'public'+PathDelim+'inside.txt');
  DeleteFile(FRoot+'secret.txt');
  RemoveDir(FRoot+'public');
  RemoveDir(FRoot);
  DeleteFile(FOutside+'inside.txt');
  RemoveDir(FOutside);
end;


procedure TTestFileModuleLocation.TestServeWithoutScriptName;

begin
  AssertEquals('File inside the location is served', 200, Serve(cLocation,'','inside.txt'));
end;


procedure TTestFileModuleLocation.TestTraversalWithoutScriptName;

begin
  AssertEquals('File above the location is refused', 403, Serve(cLocation,'','../secret.txt'));
end;


procedure TTestFileModuleLocation.TestServeWithScriptName;

begin
  AssertEquals('File inside the location is served under a script name', 200,
    Serve(cLocation,cScriptName,'inside.txt'));
end;


procedure TTestFileModuleLocation.TestTraversalWithScriptName;

begin
  AssertEquals('File above the location is refused under a script name', 403,
    Serve(cLocation,cScriptName,'../secret.txt'));
end;


procedure TTestFileModuleLocation.TestServeOutsideExeDirWithoutScriptName;

begin
  AssertEquals('File in a location outside the executable directory is served', 200,
    Serve(cOutsideLocation,'','inside.txt'));
end;


procedure TTestFileModuleLocation.TestServeOutsideExeDirWithScriptName;

begin
  AssertEquals('File in a location outside the executable directory is served under a script name', 200,
    Serve(cOutsideLocation,cScriptName,'inside.txt'));
end;

initialization
  RegisterTest(TTestFileModuleLocation);
end.
