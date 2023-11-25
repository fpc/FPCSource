{
    This file is part of the Free Component Library

    Client-side JSON-RPC functionality using Invoke.
    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fprpcclient;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.TypInfo, System.Classes, System.SysUtils, FpJson.Data, FpWeb.Client, FpWeb.Client.Http, System.Rtti, FpJson.Value;
{$ELSE FPC_DOTTEDUNITS}
uses
  TypInfo, Classes, SysUtils, fpjson, fpwebclient, fphttpwebclient, rtti, fpjsonvalue;
{$ENDIF FPC_DOTTEDUNITS}

Type
  ERPCClient = Class(Exception);
  TRttiParameterArray = array of TRttiParameter;

  TFPRPCClient = Class;

  { TFPRPCVirtualInterface }

  TFPRPCVirtualInterface = Class(TVirtualInterface)
  private
    FClient: TFPRPCClient;
    FTypeInfo: PTypeInfo;
    FClassName : String;
  Protected
   procedure HandleInvoke(aMethod: TRttiMethod; const aArgs: TValueArray; out aResult: TValue);
  Public
    Constructor Create(aTypeInfo : PTypeInfo; const aClassName : String; aClient : TFPRPCClient);
    Property Client : TFPRPCClient Read FClient;
    Property IntfTypeInfo : PTypeInfo Read FTypeInfo;
  end;

  { TFPRPCClient }
  TRPCClientOption = (rcoObjectParam,rcoNotifications);
  TRPCClientOptions = set of TRPCClientOption;
  TFPRPCClient = Class(TComponent)
  Private
    FBaseURL: String;
    FClient : TAbstractWebClient;
    FInternalClient : TAbstractWebClient;
    FOptions: TRPCClientOptions;
    FRequestID : Int64;
    function GetClient : TAbstractWebClient;
  Protected
    // Override so we can query for all registered types
    function QueryInterface(constref aIID: TGuid; out aObj): LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; override;
    // Create virtual interface. Override this if you want to return something other than TFPRPCVirtualInterface
    function CreateVirtualInterface(IntfType: TRttiInterfaceType; const aName: string): IInterface; virtual;
    // Encode parameters to method call.
    function EncodeParams(aMethod: TRttiMethod; const aArgs: TValueArray; out VarParamCount: Integer): TJSONData;
    // Decode JSON-RPC result to method call result and var/out params.
    function DecodeResult(Response: TJSONObject; aMethod: TRttiMethod; const aArgs: TValueArray; HaveReturnValues: Boolean): TValue;
    // Find registered interfacen return instance in aObj. Return true if successful.
    function DoCreateProxy(constref aIID: TGuid; out aObj): Boolean;
    function DoCreateProxy(const aName: String; out aObj): Boolean;
    // Called from TFPRPCVirtualInterface to actuall handle call.
    procedure HandleInvoke(const aClassName : String; aMethod: TRttiMethod; const aArgs: TValueArray; out aResult: TValue); virtual;
    // Do actual HTTP request.
    function DoRequest(aRequest : TJSONObject) : TJSONObject; virtual;
    // Create JSON-RPC request object.
    function CreateRPCRequest(const aClassName,aMethodName : String; IsNotification : Boolean): TJSONObject; virtual;
    // Client to do request with. If WebClient is set, that is used. Otherwise fallback using TFPHTTPClient is used.
    property Client : TAbstractWebClient Read GetClient;
  Public
    // Create a service by name. Use QueryInterface on the result to get your actual interface
    Function CreateService(const aName : string) : IInterface;
    // Create a service by name, directly return the interface.
    generic Function CreateService<T : IInterface>(const aName : string) : T;
    // Set this to use another webclient other than the default one.
    Property WebClient : TAbstractWebClient Read FClient Write FClient;
    // base URL for JSON-RPC requests
    property BaseURL : String Read FBaseURL Write FBaseURL;
    // Options.
    Property Options : TRPCClientOptions Read FOptions Write FOptions;
  end;

  { TFPRPCServiceRegistry }

  TFPRPCServiceRegistry = class
  Class var
    _instance : TFPRPCServiceRegistry;
  Private
    Type
      { TIntfEntry }
      TIntfEntry = record
        Name: String;
        IntfType : TRttiInterfaceType;
      end;
    Var
      FContext : TRTTIContext;
      fIntfs : Array of TIntfEntry;
      fIntfCount : Integer;
  Protected
  Public
    class var
      SizeDelta : Integer;

  Public
    class constructor Init;
    class destructor done;
    constructor create; virtual;
    procedure Add(aInterfaceInfo : PTypeInfo; const aName : string = '');
    generic procedure Add <T : IInterface>(const aName : string = '');
    function Find(const aName: string; out IntfType: TRttiInterfaceType): Boolean;
    function Find(const aGUID: TGUID; out IntfType: TRttiInterfaceType; out aName : String): Boolean;
    function Get(const aName: string) : TRttiInterfaceType;
    function Get(const aGUID: TGUID; out aName : String) : TRttiInterfaceType;
    class property Instance : TFPRPCServiceRegistry Read _Instance;
  end;


Function RPCServiceRegistry : TFPRPCServiceRegistry;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.JsonRpc.Strings;
{$ELSE FPC_DOTTEDUNITS}
uses fprpcstrings;
{$ENDIF FPC_DOTTEDUNITS}

function IsGUIDEqual(const guid1, guid2: tguid): boolean;
  begin
    IsGUIDEqual:=
      (guid1.D1=guid2.D1) and
      (PDWORD(@guid1.D2)^=PDWORD(@guid2.D2)^) and
      (PDWORD(@guid1.D4[0])^=PDWORD(@guid2.D4[0])^) and
      (PDWORD(@guid1.D4[4])^=PDWORD(@guid2.D4[4])^);
  end;


Function RPCServiceRegistry : TFPRPCServiceRegistry;

begin
  Result:=TFPRPCServiceRegistry.Instance;
end;

{ TFPRPCVirtualInterface }

procedure TFPRPCVirtualInterface.HandleInvoke(aMethod: TRttiMethod; const aArgs: TValueArray; out aResult: TValue);
begin
  FClient.HandleInvoke(FClassName,aMethod,aArgs,aResult);
end;

constructor TFPRPCVirtualInterface.Create(aTypeInfo: PTypeInfo; const aClassName: String; aClient: TFPRPCClient);
begin
  inherited Create(aTypeInfo, @HandleInvoke);
  FTypeInfo:=aTypeInfo;
  FClient:=aClient;
  FClassName:=aClassName;
end;

{ TFPRPCServiceRegistry }

class constructor TFPRPCServiceRegistry.Init;
begin
  SizeDelta:=32;
  _Instance:=TFPRPCServiceRegistry.Create;
end;

class destructor TFPRPCServiceRegistry.done;
begin
  FreeAndNil(_Instance);
end;

constructor TFPRPCServiceRegistry.create;
begin
  SetLength(fIntfs,SizeDelta);
  fIntfCount:=0;
end;

procedure TFPRPCServiceRegistry.Add(aInterfaceInfo: PTypeInfo; const aName: string);
var
  entry: TIntfEntry;

begin
  if aName='' then
    entry.Name:=aInterfaceInfo^.Name
  else
    entry.Name:=aName;
  entry.IntfType := fContext.GetType(aInterfaceInfo) as TRttiInterfaceType;
  if fIntfCount>=Length(fIntfs) then
    SetLength(fIntfs,Length(fIntfs)+SizeDelta);
  fIntfs[fIntfCount]:=entry;
  Inc(fIntfCount);
end;

function TFPRPCServiceRegistry.Find(Const aName: string; out IntfType: TRttiInterfaceType): Boolean;

Var
  Idx : integer;
  Entry : TIntfEntry;

begin
  Result:=False;
  Idx:=fIntfCount-1;
  While (Idx>=0) and not Result do
    begin
    Result:=SameText(fIntfs[Idx].Name,aName);
    if Result then
      begin
      Entry:=fIntfs[Idx];
      IntfType:=Entry.IntfType;
      end;
    Dec(Idx);
    end;
end;

function TFPRPCServiceRegistry.Find(const aGUID: TGUID; out IntfType: TRttiInterfaceType; out aName: String): Boolean;
Var
  Idx : integer;
  Entry : TIntfEntry;

begin
  Result:=False;
  Idx:=fIntfCount-1;
  While (Idx>=0) and not Result do
    begin
    Result:=IsGUIDEqual(fIntfs[Idx].IntfType.GUID,aGUID);
    if Result then
      begin
      Entry:=fIntfs[Idx];
      IntfType:=Entry.IntfType;
      aName:=Entry.Name;
      end;
    Dec(Idx);
    end;
end;


function TFPRPCServiceRegistry.Get(Const aName: string): TRttiInterfaceType;
begin
  if not Find(aName,Result) then
    Raise ERPCClient.CreateFmt(SErrUnknownServiceName ,[aName]);
end;

function TFPRPCServiceRegistry.Get(const aGUID: TGUID; out aName: String): TRttiInterfaceType;
begin
  if not Find(aGuid,Result,aName) then
    raise ERPCClient.CreateFmt(SErrUnknownServiceGUID, [aGuid.ToString]);
end;


generic procedure TFPRPCServiceRegistry.Add <T>(const aName : string = '');
begin
  Add(TypeInfo(T),aName);
end;

{ TFPRPCClient }

function TFPRPCClient.CreateVirtualInterface(IntfType : TRttiInterfaceType; const aName: string) : IInterface;

begin
  Result:=TFPRPCVirtualInterface.Create(IntfType.Handle,aName,Self) as IInterface
end;

function TFPRPCClient.DoCreateProxy(constref aIID: TGuid; out aObj): Boolean;

Var
  IntfType : TRttiInterfaceType;
  aName : string;
  aIntf : IInterface;
begin
  Result:=RPCServiceRegistry.Find(aIID,IntfType,aName);
  if Result then
    begin
    aIntf:=CreateVirtualInterface(IntfType,aName);
    Result:=(aIntf.QueryInterface(aIID,aObj)=S_OK);
    end;
end;

function TFPRPCClient.DoCreateProxy(const aName: String; out aObj): Boolean;

Var
  IntfType : TRttiInterfaceType;
  aIntf : IInterface;
begin
  Result:=RPCServiceRegistry.Find(aName,IntfType);
  if Result then
    begin
    aIntf:=CreateVirtualInterface(IntfType,aName);
    Result:=(aIntf.QueryInterface(IntfType.GUID,aObj)=S_OK);
    end;
end;

function TFPRPCClient.QueryInterface(constref aIID: TGuid; out aObj): LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};


begin
  Result:=Inherited QueryInterface(aIID,aObj);
  if (Result<>S_OK) then
    begin
    if DoCreateProxy(aIID,aObj) then
      Result:=S_OK
    else
      Result:=E_NOINTERFACE;
    end
end;

function TFPRPCClient.GetClient: TAbstractWebClient;
begin
  Result:=FClient;
  if Result=Nil then
    begin
    if FInternalClient=Nil then
      FInternalClient:=TFPHTTPWebClient.Create(Self);
    Result:=FInternalClient;
    end;
end;

function TFPRPCClient.CreateRPCRequest(const aClassName, aMethodName: String; IsNotification: Boolean): TJSONObject;

begin
  Result := TJSONObject.Create;
  try
    Result.Add('method', aMethodName);
    Result.Add('class', aClassName);
    Result.Add('jsonrpc','2.0');
    // In case of notification, do not send an ID
    if Not (IsNotification and (rcoNotifications in Options))  then
      begin
      inc(FRequestID);
      Result.Add('id',FRequestID);
      end;
  except
    Result.Free;
    Raise;
  end;
end;

function TFPRPCClient.CreateService(const aName: string): IInterface;
begin
  if not DoCreateProxy(aName,Result) then
    Raise ERPCClient.CreateFmt(SErrUnknownServiceName,[aName]);
end;

generic function TFPRPCClient.CreateService<T>(const aName: string): T;

Var
  II : IInterface;

begin
  Result:=Nil;
  II:=CreateService(aName);
  if II.QueryInterface(RPCServiceRegistry.Get(aName).GUID,Result)<>S_OK then
    Raise ERPCClient.CreateFmt(SErrSupportedServiceName,[aName]);
end;

Function TFPRPCClient.EncodeParams(aMethod: TRttiMethod; const aArgs: TValueArray; out VarParamCount : Integer) : TJSONData;

var
  UseObj : Boolean;
  args: TRttiParameterArray;
  arg: TRttiParameter;
  I,argIdx: Integer;
  argVal : TJSONData;


begin
  varParamCount:=0;
  UseObj:=rcoObjectParam in Options;
  if UseObj then
    Result := TJSONObject.Create
  else
    Result := TJSONArray.Create;
  try
    argIdx:=1;
    args := aMethod.GetParameters;
    for I:=0 to length(args)-1 do
      begin
      Arg:=args[i];
      if [pfHidden,pfSelf] * arg.Flags <> [] then
        Continue
      else if ([pfVar,pfOut] * arg.Flags)<>[] then
        Inc(VarParamCount);
      argVal:=ValueToJSON(aArgs[argidx], arg.ParamType);
      if UseObj then
        TJSONObject(Result).Add(arg.Name, argVal)
      else
        TJSONArray(Result).Add(argVal);
      Inc(argidx);
      end;
  except
    Result.Free;
    Raise;
  end;
end;

Function TFPRPCClient.DecodeResult(Response : TJSONObject; aMethod: TRttiMethod; const aArgs: TValueArray; HaveReturnValues : Boolean): TValue;

Var
  i,argIdx : Integer;
  args : TRttiParameterArray;
  arg : TRttiParameter;
  resobj : TJSONObject;
  value: TValue;

begin
  Result:=Default(TValue);
  if Assigned(aMethod.ReturnType) or HaveReturnValues then
    if not Assigned(Response) then
      raise ERPCClient.CreateFmt(SErrExpectedReturnButNoServerReturn,[aMethod.Name]);
  if Not HaveReturnValues then
    begin
    if Assigned(aMethod.ReturnType) then
      Result := JSONToValue(response.Elements['result'], aMethod.ReturnType);
    end
  else
    begin
    resObj:=response.Objects['result'];
    if Assigned(aMethod.ReturnType) then
      Result := JSONToValue(resObj.Elements['$result'], aMethod.ReturnType);
    argidx := 1;
    args:=aMethod.GetParameters;
    for i := 0 to High(args) do
      begin
      arg := Args[i];
      if pfHidden in arg.Flags then
        Continue;
      if arg.Flags * [pfOut, pfVar] = [] then
        begin
        Inc(argidx);
        Continue;
        end;
      value := JSONToValue(resObj.Elements[arg.Name], arg.ParamType);
      value.ExtractRawData(aArgs[argidx].GetReferenceToRawData);
      Inc(argidx);
      end;
    end;
end;

procedure TFPRPCClient.HandleInvoke(const aClassName : String; aMethod: TRttiMethod; const aArgs: TValueArray; out aResult: TValue);

var
  request, response: TJSONObject;
  argobj: TJSONData;
  VarParamCount:Integer;

begin
  aResult:=Default(TValue);
  response:=nil;
  Request:=CreateRPCRequest(aClassName,aMethod.Name,Not Assigned(aMethod.ReturnType));
  try
    { skip Self argument }
    argObj:=EncodeParams(aMethod,aArgs,VarParamCount);
    request.Add('params', argobj);
    response := DoRequest(request) as TJSONObject;
    aResult:=DecodeResult(Response,aMethod,aArgs,VarParamCount>0);
 finally
   response.Free;
   request.Free;
 end;
end;

function TFPRPCClient.DoRequest(aRequest: TJSONObject): TJSONObject;

var
  aClient: TAbstractWebClient;
  Req : TWebClientRequest;
  Resp: TWebClientResponse;
  S : TJSONStringType;
  Res : TJSONData;

begin
  Result:=Nil;
  aClient := GetClient;
  Resp:=Nil;
  Req:=aClient.CreateRequest;
  try
    S:=aRequest.AsJSON;
    // Writeln('Request : ',S);
    Req.Content.WriteBuffer(S[1],Length(S));
    Resp:=aClient.ExecuteRequest('POST',FBaseURL,Req);
    // Writeln('Response : ',Resp.GetContentAsString);
    // For notification methods, there is no return !
    if (resp.Content.Size>0) then
      begin
      resp.Content.Position:=0;
      Res:=GetJSON(resp.Content,True);
      if (Res is TJSONObject) then
        Result:=Res as TJSONObject
      else
        begin
        Res.Free;
        Raise ERPCClient.Create(SErrInvalidServerResponse);
        end;
      end;
  finally
    Req.Free;
    Resp.Free;
  end;
end;


end.

