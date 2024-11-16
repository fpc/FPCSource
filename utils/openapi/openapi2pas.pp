{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    Open API to pascal code generator

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program openapi2pas;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  CustApp,
  fpopenapi.objects,
  fpopenapi.reader,
  fpopenapi.codegen;

type

  { TGenDTOApplication }

  TGenDTOApplication = class(TCustomApplication)
  private
    FQuiet : Boolean;
    FCodeGen : TOpenAPICodeGen;
    FServiceMapFile,
    FUUIDMapFile : String;
  protected
    Procedure DoLog(EventType : TEventType; const Msg : String); override;
    procedure DoRun; override;
    procedure ReadOpenAPi(const aInputFile: string; aApi: TOpenAPI);
    procedure WriteAPI(aApi: TOpenAPI; const aOutputFile: String);
    procedure WriteConfig(const aFileName: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aMessage : string); virtual;
  end;

{ TGenDTOApplication }

procedure TGenDTOApplication.WriteAPI(aApi: TOpenAPI; const aOutputFile : String) ;

begin
  FCodeGen.OnLog:=@DoLog;
  FCodeGen.BaseOutputFileName:=aOutputFile;
  FCodeGen.API:=aAPI;
  if (FUUIDMapFile<>'') and FileExists(FUUIDMapFile) then
    FCodeGen.UUIDMap.LoadFromFile(FUUIDMapFile);
  if (FServiceMapFile<>'') then
    FCodeGen.ServiceMap.LoadFromFile(FServiceMapFile);
  FCodeGen.Execute;
  if FUUIDMapFile<>'' then
    FCodeGen.UUIDMap.SavetoFile(FUUIDMapFile);
end;

procedure TGenDTOApplication.ReadOpenAPi(const aInputFile : string; aApi: TOpenAPI);

var
  lReader : TOpenAPIReader;

begin
  lReader:=TOpenAPIReader.Create(Self);
  try
    lReader.ReadFromFile(aAPI,aInputFile);
  finally
    lReader.Free;
  end;
end;

procedure TGenDTOApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  if FQuiet then
    exit;
  Writeln(EventType,' : ',Msg);
end;

procedure TGenDTOApplication.WriteConfig(const aFileName : string);

var
  lDir : String;

begin
  lDir:=ExtractFilePath(aFileName);
  if lDir<>'' then
    If not ForceDirectories(lDir) then
      begin
      Writeln(StdErr,'Failed to create directory ',lDir);
      Exit;
      end;
  Log(etInfo,'Writing config file to %s',[aFileName]);
  FCodegen.SaveConfig(aFileName);
end;

procedure TGenDTOApplication.DoRun;

const
  shortOpts = 'hi:o:dequ:s:varcC:bnw:';
  LongOpts : Array of string = ('help','input:','output:','delphi','uuid-map:','quiet','service-map','verbose-header','enumerated','async','server','client','config:','abstract','no-implementation','write-config:');

var
  lAPI : TOpenAPI;
  lConfig, lOutputFile,lInputFile, ErrorMsg : String;

begin
  Terminate;
  ErrorMsg:=CheckOptions(ShortOPts,LongOpts);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  lConfig:=GetOptionValue('C','config');
  if (lConfig<>'') then
    FCodeGen.LoadConfig(lConfig);
  FCodeGen.DelphiCode:=HasOption('d','delphi');
  FCodeGen.VerboseHeader:=HasOption('v','verbose-header');
  FCodeGen.UseEnums:=HasOption('e','enumerated');
  FCodeGen.AsyncService:=HasOption('a','async');
  lInputFile:=GetOptionValue('i','input');
  lOutputFile:=GetOptionValue('o','output');
  FUUIDMapFile:=GetOptionValue('u','uuid-map');
  FServiceMapFile:=GetOptionValue('s','service-map');
  FCodeGen.GenerateServer:=HasOption('r','server');
  FCodeGen.AbstractServiceCalls:=HasOption('b','abstract');
  FCodeGen.SkipServerServiceImplementationModule:=HasOption('n','no-implementation');
  FQuiet:=HasOption('q','quiet');
  if HasOption('w','write-config') then
    WriteConfig(GetOptionValue('w','write-config'))
  else
    begin
    if lOutputFile='' then
      lOutputFile:=ChangeFileExt(lInputFile,'');
    lAPI:=TOpenAPI.Create;
    try
      ReadOpenAPi(lInputFile,lAPI);
      WriteApi(lApi,lOutputFile);
    finally
      lApi.Free;
    end;
    end;
end;

constructor TGenDTOApplication.Create(TheOwner: TComponent);

begin
  inherited Create(TheOwner);
  ExceptionExitCode:=1;
  StopOnException:=True;
  FCodeGen:=TOpenAPICodeGen.Create(Self);
end;

destructor TGenDTOApplication.Destroy;

begin
  FreeAndNil(FCodeGen);
  inherited Destroy;
end;

procedure TGenDTOApplication.Usage(const aMessage: string);

begin
  writeln('Usage: ', ExeName, '[options]');
  Writeln('Where options is one or more of:');
  Writeln('-a --async             Generate asynchronous service calls.');
  Writeln('-b --abstract          Split server in abstract handler and implementation modules (and units).');
  Writeln('-c --client            Generate client-side service.');
  Writeln('-C --config=FILE       Read config file with converter settings.');
  Writeln('-d --delphi            Generate delphi code for DTO/Serializer/Service definitions.');
  Writeln('-e --enumerated        Use enumerateds (default is to keep strings).');
  Writeln('-h --help              This message.');
  Writeln('-i --input=FILE        OpenAPI JSON File to use.');
  Writeln('-n --no-implementation Skip generation of server service module (only useful when -b is used).');
  Writeln('-o --output=FILE       Base filename for output.');
  Writeln('-q --quiet             Be less verbose.');
  Writeln('-s --service-map=FILE  Read service and method name mapping from file.');
  Writeln('-u --uuid-map=FILE     Read (and write) a file with UUIDs for interfaces.');
  Writeln('-v --verbose-header    Add OpenAPI description to unit header.');
  Writeln('-w --write-config=FILE Write a configuration file with current settings and exit.');
  ExitCode:=Ord(aMessage<>'');
end;

var
  Application: TGenDTOApplication;

begin
  Application:=TGenDTOApplication.Create(nil);
  Application.Title:='Generate DTO Application';
  Application.Run;
  Application.Free;
end.

