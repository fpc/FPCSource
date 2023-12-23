{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by the Free Pascal development team

    Class to collect debug output from a pas2js application.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpdebugcapturesvc;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpWeb.Http.Defs, FpWeb.Http.Base, FpJson.Data; 
{$ELSE}
  Classes, SysUtils, httpdefs, fphttp, fpjson;
{$ENDIF}
Type
  TDebugCaptureHandler = Procedure (aSender : TObject; aCapture : TJSONData) of object;
  TDebugCaptureLogHandler = Procedure (EventType : TEventType; const Msg : String) of object;

  { THandlerRegistrationItem }

  THandlerRegistrationItem = Class(TCollectionItem)
  private
    FHandler: TDebugCaptureHandler;
    FName: String;
  Public
    Property Name : String Read FName Write FName;
    Property Handler : TDebugCaptureHandler Read FHandler Write FHandler;
  end;

  { THandlerRegistrationList }

  THandlerRegistrationList = class(TOwnedCollection)
  private
    function GetH(aIndex : Integer): THandlerRegistrationItem;
    procedure SetH(aIndex : Integer; AValue: THandlerRegistrationItem);
  Public
    Function IndexOf(const aName : string) : Integer;
    Function Find(const aName : string) : THandlerRegistrationItem;
    Function Add(const aName : string; aHandler : TDebugCaptureHandler) : THandlerRegistrationItem;
    Property Handlers[aIndex :Integer] : THandlerRegistrationItem Read GetH Write SetH; default;
  end;

  { TDebugCaptureService }

  TDebugCaptureService = class(TComponent)
  Private
    class var _instance : TDebugCaptureService;
  private
    FCaptureToErrorLog: Boolean;
    FCors: TCORSSupport;
    FFileName: string;
    FHandlers: THandlerRegistrationList;
    FLogToConsole: Boolean;
    FOnLog: TDebugCaptureLogHandler;
    FCaptureStream : TStream;
    procedure SetCaptureToErrorLog(AValue: Boolean);
    procedure SetCors(AValue: TCORSSupport);
    procedure SetLogFileName(const AValue: string);
    procedure SetLogToConsole(AValue: Boolean);
    function GetHandlerCount: Integer;
  Protected
    Procedure DoLog(aType : TEventType; const aMsg : String);
    Procedure DoLog(aType : TEventType; const aFmt : String; args : Array of const);
    function GetCaptureJSON(ARequest: TRequest; AResponse: TResponse; var aJSON: TJSONData): TJSONArray;
    procedure DistributeCaptureOutput(aJSON: TJSONData); virtual;
    procedure DoLogToConsole(aSender: TObject; aCapture: TJSONData); virtual;
    procedure DoLogToErrorLog(aSender: TObject; aCapture: TJSONData); virtual;
    procedure DoLogToFile(aSender: TObject; aCapture: TJSONData); virtual;
    Function CreateRegistrationList : THandlerRegistrationList; virtual;
    Property Handlers : THandlerRegistrationList Read FHandlers;
  Public
    Constructor Create(aOwner:TComponent); override;
    Destructor Destroy; Override;
    class constructor init;
    class destructor done;
    class Property Instance : TDebugCaptureService Read _Instance;
    class function JSONDataToString(aJSON: TJSONData): TJSONStringType;
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);
    Procedure RegisterHandler(const aName : String; aHandler: TDebugCaptureHandler);
    Procedure UnregisterHandler(const aName : String);
    Property HandlerCount : Integer Read GetHandlerCount;
    Property LogFileName : string Read FFileName Write SetLogFileName;
    Property LogToConsole : Boolean Read FLogToConsole Write SetLogToConsole;
    Property CaptureToErrorLog : Boolean Read FCaptureToErrorLog Write SetCaptureToErrorLog;
    Property OnLog : TDebugCaptureLogHandler Read FOnLog Write FOnLog;
    Property CORS : TCORSSupport Read FCors Write SetCors;
  end;


implementation

{ THandlerRegistrationList }

function THandlerRegistrationList.GetH(aIndex : Integer): THandlerRegistrationItem;
begin
  Result:=Items[aIndex] as THandlerRegistrationItem;
end;

procedure THandlerRegistrationList.SetH(aIndex : Integer; AValue: THandlerRegistrationItem);
begin
  Items[aIndex]:=aValue;
end;

function THandlerRegistrationList.IndexOf(const aName: string): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(GetH(Result).Name,aName) do
    Dec(Result);
end;

function THandlerRegistrationList.Find(const aName: string): THandlerRegistrationItem;

var
  Idx : integer;

begin
  Result:=Nil;
  Idx:=IndexOf(aName);
  If Idx<>-1 then
    Result:=GetH(Idx);
end;

function THandlerRegistrationList.Add(const aName: string; aHandler: TDebugCaptureHandler): THandlerRegistrationItem;
begin
  Result:=(Inherited Add) as THandlerRegistrationItem;
  Result.Name:=aName;
  Result.Handler:=aHandler;
end;

function TDebugCaptureService.GetCaptureJSON(ARequest: TRequest; AResponse: TResponse; var aJSON: TJSONData): TJSONArray;

var
  aJSONObj : TJSONObject absolute aJSON;
  Cont : String;

begin
  Result:=Nil;
  aJSON:=Nil;
  try
    Cont:=aRequest.Content;
    aJSON:=GetJSON(Cont);
    if aJSON.JSONType<>jtObject then
      Raise EHTTP.Create('No JSON object in capture JSON');
    Result:=aJSONObj.Get('lines',TJSONArray(Nil));
    if Result=Nil then
      begin
      FreeAndNil(aJSON);
      Raise EHTTP.Create('No lines element in capture JSON');
      end;
  except
    On E : Exception do
      begin
      DoLog(etError,Format('Exception %s (%s) : Invalid capture content: not valid JSON: %s',[E.ClassName,E.Message,Copy(Cont,1,255)]));
      aResponse.Code:=400;
      aResponse.CodeText:='INVALID PARAM';
      aResponse.SendResponse;
      end;
  end;
end;

procedure TDebugCaptureService.DoLogToErrorLog(aSender: TObject; aCapture: TJSONData);

var
  S : TJSONStringType;

begin
  S:=JSonDataToString(aCapture);
  DoLog(etInfo,'Capture : '+S);
end;

procedure TDebugCaptureService.DoLogToConsole(aSender: TObject; aCapture: TJSONData);
var
  S : TJSONStringType;

begin
  S:=JSonDataToString(aCapture);
  Try
    Writeln('Debug capture: ',S);
  except
    On E : Exception Do
      DoLog(etError,'Exception %s writing to console: %s',[E.ClassName,E.Message]);
  end;
end;

procedure TDebugCaptureService.DoLogToFile(aSender: TObject; aCapture: TJSONData);

var
  S : TJSONStringType;

begin
  S:=JSonDataToString(aCapture)+sLineBreak;
  if Assigned(FCaptureStream) then
    Try
      FCaptureStream.WriteBuffer(S[1],Length(S)*SizeOf(TJSONCharType));
    except
      On E : Exception Do
        DoLog(etError,'Exception %s writing to file %s: %s',[E.ClassName,LogFileName,E.Message]);
    end;
end;

function TDebugCaptureService.GetHandlerCount: Integer;
begin
  Result:=FHandlers.Count;
end;

Const
  cCaptureToErrorLog = '$ErrorLog';
  cCaptureToFile     = '$File';
  cCaptureToConsole  = '$Console';

procedure TDebugCaptureService.SetCaptureToErrorLog(AValue: Boolean);
begin
  if FCaptureToErrorLog=AValue then Exit;
  FCaptureToErrorLog:=AValue;
  if FCaptureToErrorLog then
    RegisterHandler(cCaptureToErrorLog,@DoLogToErrorLog)
  else
    UnRegisterHandler(cCaptureToErrorLog);
end;

procedure TDebugCaptureService.SetCors(AValue: TCORSSupport);
begin
  if FCors=AValue then Exit;
  FCors.Assign(AValue);
end;

procedure TDebugCaptureService.SetLogFileName(const AValue: string);
begin
  if FFileName=AValue then Exit;
  if Assigned(FCaptureStream) then
    FreeAndNil(FCaptureStream);
  FFileName:=AValue;
  if FFileName<>'' then
    begin
    FCaptureStream:=TFileStream.Create(FFileName,fmCreate or fmShareDenyWrite);
    RegisterHandler(cCaptureToFile,@DoLogToFile)
    end
  else
    UnRegisterHandler(cCaptureToFile);
end;

procedure TDebugCaptureService.SetLogToConsole(AValue: Boolean);
begin
  if FLogToConsole=AValue then Exit;
  FLogToConsole:=AValue;
  if FLogToConsole then
    RegisterHandler(cCaptureToFile,@DoLogToConsole)
  else
    UnRegisterHandler(cCaptureToFile);
end;

procedure TDebugCaptureService.DoLog(aType: TEventType; const aMsg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(aType,aMsg);
end;

procedure TDebugCaptureService.DoLog(aType: TEventType; const aFmt: String; args: array of const);
begin
  if Assigned(FonLog) then
{$IF DECLARED(SafeFormat)}
    FonLog(aType,SafeFormat(aFmt,args));
{$ELSE}
    FonLog(aType,Format(aFmt,args));
{$ENDIF}
end;

function TDebugCaptureService.CreateRegistrationList: THandlerRegistrationList;
begin
  Result:=THandlerRegistrationList.Create(Self,THandlerRegistrationItem);
end;

constructor TDebugCaptureService.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHandlers:=CreateRegistrationList;
  FCors:=TCORSSupport.Create;
end;

destructor TDebugCaptureService.Destroy;
begin
  FreeAndNil(FCors);
  FreeAndNil(FHandlers);
  inherited Destroy;
end;

procedure TDebugCaptureService.DistributeCaptureOutput(aJSON : TJSONData);

var
  I : Integer;
  H : THandlerRegistrationItem;

begin
  For I:=0 to FHandlers.Count-1 do
    Try
      H:=FHandlers[i];
      H.Handler(Self,aJSON);
    except
      On E : Exception do
        DoLog(etError,'Handler %s raised exception %s while handling debug capture: %s',[H.Name,E.ClassName,E.Message]);
    end;
end;

procedure TDebugCaptureService.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  aJSON : TJSONData;
  aArray : TJSONArray;
  I : Integer;

begin
  if CORS.HandleRequest(aRequest,aResponse,[hcDetect,hcSend]) then
    exit;
  aJSON:=Nil;
  aArray:=Nil;
  try
    aArray:=GetCaptureJSON(aRequest,aResponse,aJSON);
    if aArray<>Nil then
      begin
      For I:=0 to aArray.Count-1 do
        DistributeCaptureOutput(aArray[i]);
      aResponse.Code:=200;
      aResponse.CodeText:='OK';
      aResponse.SendResponse;
      end;
  finally
    aJSON.Free;
  end;
end;


procedure TDebugCaptureService.RegisterHandler(const aName: String; aHandler: TDebugCaptureHandler);
begin
  If FHandlers.IndexOf(aName)<>-1  then
    Raise EListError.CreateFmt('Duplicate name: %s',[aName]);
  FHandlers.Add(aName,aHandler);
end;

procedure TDebugCaptureService.UnregisterHandler(const aName: String);

var
  Idx : integer;

begin
  Idx:=FHandlers.IndexOf(aName);
  if Idx<>-1 then
    FHandlers.Delete(Idx);
end;

class function TDebugCaptureService.JSONDataToString(aJSON : TJSONData): TJSONStringType;

begin
  if aJSON.JSONType in StructuredJSONTypes then
    Result:=aJSON.AsJSON
  else if aJSON.JSONType<>jtNull then
    Result:=aJSON.AsString
  else
    Result:='null';
end;

class constructor TDebugCaptureService.init;

begin
  _instance:=TDebugCaptureService.Create(Nil);
end;

class destructor TDebugCaptureService.done;

begin
  FreeAndNil(_instance);
end;

end.

