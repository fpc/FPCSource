unit fpextdirect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fpjsonrpc, webjsonrpc, httpdefs,websession;

Const
  DefaultExtDirectOptions = DefaultDispatchOptions + [jdoRequireClass];

Type
  { TCustomExtDirectDispatcher }

  TCustomExtDirectDispatcher = Class(TCustomJSONRPCDispatcher)
  private
    FAPIType: String;
    FNameSpace: String;
    FURL: String;
    function GetNameSpace: String;
    function isNameSpaceStored: boolean;
  Protected
    function FormatResult(const AClassName, AMethodName: TJSONStringType;
      const Params, ID, Return: TJSONData): TJSONData; override;
    // 'tid'
    Class Function TransactionProperty : String; override;
    // 'method'
    Class Function MethodProperty : String; override;
    // 'action'
    Class Function ClassNameProperty : String; override;
    // 'data'
    Class Function ParamsProperty : String; override;
    // Add session support
    Function FindHandler(Const AClassName,AMethodName : TJSONStringType;AContext : TJSONRPCCallContext; Out FreeObject : TComponent) : TCustomJSONRPCHandler; override;
    // Create API
    Function DoAPI : TJSONData; virtual;
    // Namespace for API description. Must be set. Default 'FPWeb'
    Property NameSpace : String Read GetNameSpace Write FNameSpace Stored isNameSpaceStored;
    // URL property for router. Must be set
    Property URL : String Read FURL Write FURL;
    // "type". By default: 'remoting'
    Property APIType : String Read FAPIType Write FAPIType;
  Public
    // Override to set additional opions.
    Constructor Create(AOwner : TComponent); override;
    // Return API description object
    Function API: TJSONData;
    // Return API Description including namespace, as a string
    Function APIAsString : String;
  end;

  { TExtDirectDispatcher }

  TExtDirectDispatcher = Class(TCustomExtDirectDispatcher)
  Published
    Property NameSpace;
    Property URL;
    Property APIType;
    Property OnStartBatch;
    Property OnDispatchRequest;
    Property OnFindHandler;
    Property OnEndBatch;
    Property Options;
  end;

  { TCustomExtDirectContentProducer }

  TCustomExtDirectContentProducer = Class(TCustomJSONRPCContentProducer)
  Protected
    Function GetIDProperty : String; override;
    Procedure DoGetContent(ARequest : TRequest; Content : TStream; Var Handled : Boolean); override;
  end;

  { TExtDirectContentProducer }

  TExtDirectContentProducer = Class(TCustomExtDirectContentProducer)
  private
    FDispatcher: TCustomExtDirectDispatcher;
    procedure SetDispatcher(const AValue: TCustomExtDirectDispatcher);
  Protected
    Function GetDispatcher : TCustomJSONRPCDispatcher; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  Published
    Property Dispatcher :  TCustomExtDirectDispatcher Read FDispatcher Write SetDispatcher;
  end;

  { TCustomExtDirectModule }

  TCustomExtDirectModule = Class(TJSONRPCDispatchModule)
  private
    FAPIPath: String;
    FDispatcher: TCustomExtDirectDispatcher;
    FOptions: TJSONRPCDispatchOptions;
    FRequest: TRequest;
    FResponse: TResponse;
    FRouterPath: String;
    procedure SetDispatcher(const AValue: TCustomExtDirectDispatcher);
  Protected
    // Create API
    procedure CreateAPI(ADispatcher: TCustomExtDirectDispatcher; ARequest: TRequest; AResponse: TResponse); virtual;
    Function CreateDispatcher : TCustomExtDirectDispatcher; virtual;
    Property Dispatcher :  TCustomExtDirectDispatcher Read FDispatcher Write SetDispatcher;
    Property DispatchOptions : TJSONRPCDispatchOptions Read FOptions Write FOptions default DefaultDispatchOptions;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  Public
    Constructor CreateNew(AOwner : TComponent; CreateMode : Integer); override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
    // Access to request
    Property Request: TRequest Read FRequest;
    // Access to response
    Property Response: TResponse Read FResponse;
    // API path/action. Append to BaseURL to get API. Default 'API'
    Property APIPath : String Read FAPIPath Write FAPIPath;
    // Router path/action. Append to baseURL to get router. Default 'router'
    Property RouterPath : String Read FRouterPath Write FRouterPath;
  end;

  TExtDirectModule = Class(TCustomExtDirectModule)
  Published
    Property APIPath;
    Property RouterPath;
  end;

implementation

Resourcestring
  SErrInvalidPath = 'Invalid path';

{ TCustomExtDirectDispatcher }
Const
  DefaultNameSpace = 'FPWeb';

function TCustomExtDirectDispatcher.GetNameSpace: String;
begin
  Result:=FNameSpace;
  If (Result='') then
    Result:=DefaultNameSpace
end;

function TCustomExtDirectDispatcher.isNameSpaceStored: boolean;
begin
  Result:=NameSpace<>DefaultNameSpace;
end;

function TCustomExtDirectDispatcher.FormatResult(Const AClassName, AMethodName: TJSONStringType;
Const Params,ID, Return : TJSONData) : TJSONData;

begin
  Result:=Inherited FormatResult(AClassName,AMethodName,Params,ID,Return);
  TJSONObject(Result).Add('type','rpc');
  TJSONObject(Result).Add('action',AClassName);
  TJSONObject(Result).Add('method',AMethodName);
end;

class function TCustomExtDirectDispatcher.TransactionProperty: String;
begin
  Result:='tid';
end;

class function TCustomExtDirectDispatcher.MethodProperty: String;
begin
  Result:='method';
end;

class function TCustomExtDirectDispatcher.ClassNameProperty: String;
begin
  Result:='action';
end;

class function TCustomExtDirectDispatcher.ParamsProperty: String;
begin
  Result:='data';
end;

function TCustomExtDirectDispatcher.FindHandler(const AClassName,
  AMethodName: TJSONStringType; AContext: TJSONRPCCallContext; out
  FreeObject: TComponent): TCustomJSONRPCHandler;
begin
  Result:=inherited FindHandler(AClassName, AMethodName, AContext, FreeObject);
  If (AContext is TJSONRPCSessionContext) and (FreeObject is TCustomJSONRPCModule) then
    TCustomJSONRPCModule(FreeObject).Session:=TJSONRPCSessionContext(AContext).Session;
end;

function TCustomExtDirectDispatcher.DoAPI: TJSONData;

Var
  A,D : TJSONObject;
  R : TJSONArray;
  N : TJSONStringType;
  H : TCustomJSONRPCHandler;
  I,J : Integer;
  M : TCustomJSONRPCHandlerManager;
  HD : TJSONRPCHandlerDef;

begin
  D:=TJSONObject.Create;
  try
    D.Add('url',URL);
    D.Add('type',APIType);
    A:=TJSONObject.Create;
    D.Add('actions',A);
    R:=Nil;
    N:='';
    If (jdoSearchOwner in Options) and Assigned(Owner) then
      begin
      for I:=Owner.ComponentCount-1 downto 0 do
        If Owner.Components[i] is TCustomJSONRPCHandler then
          begin
          If (R=Nil) then
            begin
            N:=Owner.Name;
            R:=TJSONArray.Create;
            A.Add(N,R);
            end;
          H:=Owner.Components[i] as TCustomJSONRPCHandler;
          R.Add(TJSONObject.Create(['name',H.Name,'len',H.ParamDefs.Count]));
          end;
      end;
    If (jdoSearchRegistry in Options) then
      begin
      M:=JSONRPCHandlerManager;
      For I:=M.HandlerCount-1 downto 0 do
        begin
        HD:=M.HandlerDefs[i];
        If (R=Nil) or (CompareText(N,HD.HandlerClassName)<>0) then
          begin
          N:=HD.HandlerClassName;
          J:=A.IndexOf(R);
          If (J=-1) then
            begin
            R:=TJSONArray.Create;
            A.Add(N,R);
            end
          else
            R:=A.Items[i] as TJSONArray;
          end;
        R.Add(TJSONObject.Create(['name',HD.HandlerMethodName,'len',HD.ArgumentCount]));
        end;
      end;
    Result:=D;
  except
    FreeAndNil(D);
    Raise;
  end;
end;

constructor TCustomExtDirectDispatcher.Create(AOwner: TComponent);

Var
  O : TJSONRPCDispatchOptions;

begin
  inherited Create(AOwner);
  Options:=DefaultExtDirectOptions;
  APIType:='remoting';
end;

function TCustomExtDirectDispatcher.API: TJSONData;
begin
  Result:=DoAPI;
end;

function TCustomExtDirectDispatcher.APIAsString: String;

Var
  A : TJSONData;

begin
  A:=API;
  try
    Result:=NameSpace + ' = ' + A.AsJSON + ';';
  finally
    A.Free;
  end;
end;


{ TCustomExtDirectContentProducer }

function TCustomExtDirectContentProducer.GetIDProperty: String;
begin
  Result:='tid';
end;

procedure TCustomExtDirectContentProducer.DoGetContent(ARequest: TRequest;
  Content: TStream; var Handled: Boolean);

Var
  A,R: String;

begin
  A:=ARequest.GetNextPathInfo;
  If (A<>'router') then
    begin
    R:=TCustomExtDirectDispatcher(GetDispatcher).APIAsString;
    Content.WriteBuffer(R[1],Length(R));
    Handled:=True;
    end
  else
    inherited DoGetContent(ARequest, Content, Handled);
end;

{ TExtDirectContentProducer }

procedure TExtDirectContentProducer.SetDispatcher(
  const AValue: TCustomExtDirectDispatcher);
begin
  if FDispatcher=AValue then exit;
  If Assigned(FDispatcher) then
    FDispatcher.RemoveFreeNotification(Self);
  FDispatcher:=AValue;
  If Assigned(FDispatcher) then
    FDispatcher.FreeNotification(Self);
end;

function TExtDirectContentProducer.GetDispatcher: TCustomJSONRPCDispatcher;
begin
  Result:=FDispatcher;
end;

procedure TExtDirectContentProducer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and (AComponent=FDispatcher) then
    FDispatcher:=Nil;
end;

{ TCustomExtDirectModule }

procedure TCustomExtDirectModule.SetDispatcher(
  const AValue: TCustomExtDirectDispatcher);
begin
  if FDispatcher=AValue then exit;
  If Assigned(FDispatcher) then
    FDispatcher.RemoveFreeNotification(Self);
  FDispatcher:=AValue;
  If Assigned(FDispatcher) then
    FDispatcher.FreeNotification(Self);
end;

function TCustomExtDirectModule.CreateDispatcher: TCustomExtDirectDispatcher;

Var
  E : TExtDirectDispatcher;

begin
  E:=TExtDirectDispatcher.Create(Self);
  E.Options:=DispatchOptions;
  E.URL:=IncludeHTTPPathDelimiter(BaseURL)+RouterPath;
  Result:=E
end;

procedure TCustomExtDirectModule.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and (AComponent=FDispatcher) then
    FDispatcher:=Nil;
end;

constructor TCustomExtDirectModule.CreateNew(AOwner: TComponent;
  CreateMode: Integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FOptions:=DefaultDispatchOptions+[jdoSearchRegistry];
end;

procedure TCustomExtDirectModule.CreateAPI(ADispatcher : TCustomExtDirectDispatcher; ARequest: TRequest;   AResponse: TResponse);


begin
  AResponse.Content:=ADispatcher.APIAsString;
  AResponse.ContentLength:=Length(AResponse.Content);

end;

procedure TCustomExtDirectModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);

Var
  Disp : TCustomExtDirectDispatcher;
  Req,res : TJSONData;
  R : String;

begin
  If (Dispatcher=Nil) then
    Dispatcher:=CreateDispatcher;
  Disp:=Dispatcher as TCustomExtDirectDispatcher;
  R:=ARequest.QueryFields.Values['action'];
  If (R='') then
    ARequest.GetNextPathInfo;
  If (CompareText(R,APIPath)=0) then
    begin
    CreateAPI(Disp,ARequest,AResponse);
    AResponse.SendResponse;
    end
  else if (CompareText(R,RouterPath)=0) then
    begin
    Res:=DispatchRequest(ARequest,Disp);
    try
      If Assigned(Res) then
        AResponse.Content:=Res.AsJSON;
      AResponse.SendResponse;
    finally
      Res.Free;
    end;
    AResponse.SendResponse;
    end
  else
    JSONRPCError(SErrInvalidPath);
end;

end.

