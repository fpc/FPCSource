{
    This file is part of the Free Component Library

    JSON-RPC functionality - http independant (backend) part
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpjsonrpc;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{$inline on}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpJson.Data;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpjson;
{$ENDIF FPC_DOTTEDUNITS}

Type

{ ---------------------------------------------------------------------
  JSON-RPC Handler support
  ---------------------------------------------------------------------}
  TJSONRPCHandlerDef = Class;
  TCustomJSONRPCDispatcher = Class;

  { TJSONParamDef }

  TJSONParamDef = Class(TCollectionItem)
  private
    FName: TJSONStringType;
    FRequired: Boolean;
    FType: TJSONtype;
    procedure SetName(const AValue: TJSONStringType);
  protected
    function GetDisplayName: string; override;
  public
    Constructor Create(ACollection : TCollection); override;
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Name : TJSONStringType Read FName Write SetName;
    Property DataType : TJSONtype Read FType Write FType default jtString;
    Property Required : Boolean Read FRequired Write FRequired default True;
  end;

  { TJSONParamDefs }

  TJSONParamDefs = Class(TOwnedCollection)
  private
    function GetP(AIndex : Integer): TJSONParamDef;
    procedure SetP(AIndex : Integer; const AValue: TJSONParamDef);
  Public
    Function AddParamDef(Const AName : TJSONStringType; AType : TJSONType = jtString; ARequired: Boolean = False) : TJSONParamDef;
    Function IndexOfParamDef(Const AName : TJSONStringType) : Integer;
    Function FindParamDef(Const AName : TJSONStringType) : TJSONParamDef;
    Function ParamDefByName(Const AName : TJSONStringType) : TJSONParamDef;
    Property ParamDefs[AIndex : Integer] : TJSONParamDef Read GetP Write SetP; default;
  end;

  { TCustomJSONRPCHandler }
  TJSONParamErrorEvent = Procedure (Sender : TObject; Const E : Exception; Var Fatal : boolean) of Object;
  TJSONRPCOption = (jroCheckParams,jroObjectParams,jroArrayParams,jroIgnoreExtraFields);
  TJSONRPCOptions = set of TJSONRPCOption;

  { TJSONRPCCallContext }

  TJSONRPCCallContext = Class(TObject)
  private
    FClassName: String;
    FMethod: String;
    FParamDefinitions: TJSONParamDefs;
    FParams: TJSONData;
    FTID: String;
    procedure SetParams(AValue: TJSONData);
  protected
    Property ParamDefinitions : TJSONParamDefs Read FParamDefinitions Write FParamDefinitions;
  Public
    Property Params : TJSONData Read FParams Write SetParams;
    // Action used to call handler.
    Property ClassName : String Read FClassName Write FClassName;
    // Method used to call handler.
    Property Method : String Read FMethod Write FMethod;
    // Transaction in which handler is called.
    Property TID : String Read FTID Write FTID;
  end;

  TCustomJSONRPCHandler = Class(TComponent)
  private
    FAfterExecute: TNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FOnParamError: TJSONParamErrorEvent;
    FOptions: TJSONRPCOptions;
    FParamDefs: TJSONParamDefs;
    FExecParams : TJSONData;
    FResultType: TJSONtype;
    FRPCMethodName : String;
    function GetRPCMethodName: String;
    procedure SetRPCMethodName(AValue: String);
    procedure SetParamDefs(const AValue: TJSONParamDefs);
  Protected
    function CreateParamDefs: TJSONParamDefs; virtual;
    Procedure DoCheckParams(Const Params : TJSONData); virtual;
    Procedure DoCheckParamDefsOnObject(Const ParamObject: TJSONObject); virtual;
    Procedure DoCheckParamArray(const ParamArray: TJSONArray); virtual;
    Function DoExecute(Const Params : TJSONData; AContext : TJSONRPCCallContext): TJSONData; virtual;
    Property BeforeExecute : TNotifyEvent Read FBeforeExecute Write FBeforeExecute;
    Property AfterExecute : TNotifyEvent Read FAfterExecute Write FAfterExecute;
    Property OnParamError :TJSONParamErrorEvent Read FOnParamError Write FONParamError;
    Property Options : TJSONRPCOptions Read FOptions Write FOptions;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure CheckParams(Const Params : TJSONData);
    Function ParamByName(Const AName : String) : TJSONData;
    // Called before execute is called.
    Procedure SetRequestClassAndMethod(const aClassName,aMethodName : String); virtual;
    // Actually call method.
    Function Execute(Const Params : TJSONData; AContext : TJSONRPCCallContext = Nil) : TJSONData;
    // Checked on incoming request
    Property ParamDefs : TJSONParamDefs Read FParamDefs Write SetParamDefs;
    // Used in parameter descriptions
    Property ResultType : TJSONtype Read FResultType Write FResultType;
    Property RPCMethodName : String Read GetRPCMethodName Write SetRPCMethodName;
  end;
  TCustomJSONRPCHandlerClass = Class of TCustomJSONRPCHandler;

  TJSONRPCEvent = Procedure (Sender : TObject; Const Params : TJSONData; Out Res : TJSONData) of object;
  TJSONContextRPCEvent = Procedure (Sender : TObject; aContext : TJSONRPCCallContext; Const Params : TJSONData; Out Res : TJSONData) of object;

  { TJSONRPCHandler }

  TJSONRPCHandler = Class(TCustomJSONRPCHandler)
  private
    FOnExecute: TJSONRPCEvent;
    FOnContextExecute : TJSONContextRPCEvent;
  protected
    Function DoExecute(Const Params : TJSONData; AContext : TJSONRPCCallContext): TJSONData; override;
  Published
    Property OnExecute : TJSONRPCEvent Read FOnExecute Write FOnExecute;
    Property OnContextExecute : TJSONContextRPCEvent Read FOnContextExecute Write FOnContextExecute;
    Property BeforeExecute;
    Property AfterExecute;
    Property OnParamError;
    Property Options;
    Property ParamDefs;
    Property ResultType;
  end;

  { TJSONRPCEcho }

  TJSONRPCEcho = Class(TCustomJSONRPCHandler)
  Protected
    Function DoExecute(Const Params : TJSONData;AContext : TJSONRPCCallContext): TJSONData; override;
  end;

{ ---------------------------------------------------------------------
  JSON-RPC dispatcher support
  ---------------------------------------------------------------------}

  TCreateAPIOption = (caoFormatted,caoFullParams);
  TCreateAPIOptions = set of TCreateAPIOption;

  { TAPIDescriptionCreator }

  TAPIDescriptionCreator = Class(TPersistent)
  private
    FDefaultOptions: TCreateAPIOptions;
    FDispatcher: TCustomJSONRPCDispatcher;
    FNameSpace : String;
    FURL : String;
    FAPIType : String;
    function GetNameSpace: String;
    function isNameSpaceStored: Boolean;
  Protected
    Function GetOwner: TPersistent; override;
    procedure AddParamDefs(O: TJSONObject; Defs: TJSONParamDefs); virtual;
    function CreateParamDef(aDef: TJSONParamDef): TJSONObject; virtual;
    function HandlerToAPIMethod(H: TCustomJSONRPCHandler; aOptions: TCreateAPIOptions): TJSONObject; virtual;
    function HandlerDefToAPIMethod(H: TJSONRPCHandlerDef; aOptions: TCreateAPIOptions): TJSONObject; virtual;
    function DefaultNameSpace: String; virtual;
    Function PublishHandler(H: TCustomJSONRPCHandler): Boolean; virtual;
    function PublishHandlerDef(HD: TJSONRPCHandlerDef): Boolean; virtual;
  Public
    Constructor Create(aDispatcher : TCustomJSONRPCDispatcher); virtual;
    Procedure Assign(Source : TPersistent); override;
    function CreateAPI(aOptions: TCreateAPIOptions): TJSONObject; overload;
    function CreateAPI : TJSONObject; overload;
    Property Dispatcher : TCustomJSONRPCDispatcher Read FDispatcher;
  Published
    // Namespace for API description. Must be set. Default 'FpWeb'
    Property NameSpace : String Read GetNameSpace Write FNameSpace Stored isNameSpaceStored;
    // URL property for API router. Must be set.
    Property URL : String Read FURL Write FURL;
    // "type". By default: 'remoting'
    Property APIType : String Read FAPIType Write FAPIType;
    // Default options for creating an API
    Property DefaultOptions : TCreateAPIOptions Read FDefaultOptions Write FDefaultOptions;
  end;

  TJSONRPCDispatchOption = (jdoSearchRegistry, // Check JSON Handler registry
                            jdoSearchOwner, // Check owner (usually webmodule) for request handler
                            jdoJSONRPC1, // Allow JSON RPC-1
                            jdoJSONRPC2, // Allow JSON RPC-2
                            jdoRequireClass, // Require class name (as in Ext.Direct)
                            jdoNotifications, // Allow JSON Notifications
                            jdoStrictNotifications, // Error if notification returned result. Default is to discard result.
                            jdoAllowAPI, // Allow client to get API description
                            jdoCacheAPI // Cache the API description
                            );
  TJSONRPCDispatchOptions = set of TJSONRPCDispatchOption;

Const
  DefaultDispatchOptions =  [jdoSearchOwner,jdoJSONRPC1,jdoJSONRPC2,jdoNotifications,jdoAllowAPI,jdoCacheAPI];

Type
  TDispatchRequestEvent = Procedure(Sender : TObject; Const AClassName,AMethod : TJSONStringType; Const Params : TJSONData) of object;
  TFindRPCHandlerEvent =  Procedure(Sender : TObject; Const AClassName,AMethod : TJSONStringType; Out Handler : TCustomJSONRPCHandler) of object;

  { TCustomJSONRPCDispatcher }


  TCustomJSONRPCDispatcher = Class(TComponent)
  private
    FAPICreator: TAPIDescriptionCreator;
    FFindHandler: TFindRPCHandlerEvent;
    FOnDispatchRequest: TDispatchRequestEvent;
    FOnEndBatch: TNotifyEvent;
    FOnStartBatch: TNotifyEvent;
    FOptions: TJSONRPCDispatchOptions;
    FCachedAPI : TJSONObject;
    FCachedAPIOptions : TCreateAPIOptions;
    procedure SetAPICreator(AValue: TAPIDescriptionCreator);
  Protected
    // Create TAPIDescriptionCreator instance. Must have self as owner
    Function CreateAPICreator : TAPIDescriptionCreator; virtual;
    // Find handler. If none found, nil is returned. Executes OnFindHandler if needed.
    // On return 'DoFree' must be set to indicate that the hand
    Function FindHandler(Const AClassName,AMethodName : TJSONStringType;AContext : TJSONRPCCallContext; Out FreeObject : TComponent) : TCustomJSONRPCHandler; virtual;
    // Execute handler instance. This can be overridden to implement e.g. authentication globally before actually executing the handler.
    Function ExecuteHandler(H: TCustomJSONRPCHandler; Params, ID: TJSONData; AContext: TJSONRPCCallContext): TJSONData; virtual;
    // Execute method. Finds handler, and returns response.
    Function ExecuteMethod(Const AClassName, AMethodName : TJSONStringType; Params,ID : TJSONData; AContext : TJSONRPCCallContext) : TJSONData; virtual;
    // Check and Execute a single request. Exceptions are caught and converted to request error object.
    function ExecuteRequest(ARequest: TJSONData;AContext : TJSONRPCCallContext): TJSONData;
    // Execute requests, returns responses in same format as requests (single or array)
    Function DoExecute(Requests : TJSONData;AContext : TJSONRPCCallContext) : TJSONData; virtual;
    // Check if single request corresponds to specs.
    // Returns an error object if an error was found.
    // if request is OK, then transaction id, classname, method and params *must* be returned.
    // The returned transaction id, method, classname and params will be ignored if there is an error.
    function CheckRequest(Request: TJSONData; Out AClassName, AMethodName : TJSONStringType; Out ID, Params : TJSONData): TJSONData; virtual;
    // Check if requests are OK (check if JSON2 is allowed for array).
    Function CheckRequests(Requests : TJSONData) : TJSONData; virtual;
    // Format result of a single request. Result is returned to the client, possibly in an array if multiple requests were received in batch.
    Function FormatResult(const AClassName, AMethodName: TJSONStringType;  const Params, ID, Return: TJSONData) : TJSONData; virtual;
    // Format error of a single request. ID will be cloned if non-nil.
    function CreateJSON2Error(Const AMessage : String; Const ACode : Integer; ID : TJSONData = Nil; idname : TJSONStringType = 'id' ) : TJSONObject; virtual;
    function CreateJSON2Error(Const AFormat : String; Args : Array of const; Const ACode : Integer; ID : TJSONData = Nil; idname : TJSONStringType = 'id') : TJSONObject;
    // Hooks for user.
    Property OnStartBatch : TNotifyEvent Read FOnStartBatch Write FOnStartBatch;
    Property OnDispatchRequest : TDispatchRequestEvent Read FOnDispatchRequest Write FOnDispatchRequest;
    Property OnFindHandler : TFindRPCHandlerEvent Read FFindHandler Write FFindHandler;
    Property OnEndBatch : TNotifyEvent Read FOnEndBatch Write FOnEndBatch;
    Property Options : TJSONRPCDispatchOptions Read FOptions Write FOptions default DefaultDispatchOptions;
    Class Function MethodProperty : String; virtual;
    Class Function ClassNameProperty : String; virtual;
    Class Function ParamsProperty : String; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function TransactionProperty : String; virtual;
    // execute request(s) using context
    Function Execute(Requests : TJSONData;AContext : TJSONRPCCallContext = Nil) : TJSONData;
    // Create an API description. If options are not specified, APICreator.DefaultOptions is used.
    Function CreateAPI(aOptions : TCreateAPIOptions): TJSONObject; overload;
    Function CreateAPI : TJSONObject; overload;
    // Return API Description including namespace, as a string. If options are not specified, APICreator.DefaultOptions is used.
    Function APIAsPascal(aOptions : TCreateAPIOptions; aUnitName : string) : String; virtual;
    Function APIAsString(aOptions : TCreateAPIOptions) : TJSONStringType; virtual;
    Function APIAsString : TJSONStringType; virtual;
    Property APICreator : TAPIDescriptionCreator Read FAPICreator Write  SetAPICreator;
  end;

  TJSONRPCDispatcher = Class(TCustomJSONRPCDispatcher)
  Published
    Property OnStartBatch;
    Property OnDispatchRequest;
    Property OnFindHandler;
    Property OnEndBatch;
    Property Options;
    Property APICreator;
  end;


{ ---------------------------------------------------------------------
  Factory support
  ---------------------------------------------------------------------}


   { TJSONRPCHandlerDef }

  TDataModuleClass = Class of TDataModule; // For the time being. As of rev 15343 it is in classes unit

  TBeforeCreateJSONRPCHandlerEvent = Procedure (Sender : TObject; Var AClass : TCustomJSONRPCHandlerClass) of object;
  TJSONRPCHandlerEvent = Procedure (Sender : TObject; AHandler : TCustomJSONRPCHandler) of object;

  TJSONRPCHandlerDef = Class(TCollectionItem)
  private
    FAfterCreate: TJSONRPCHandlerEvent;
    FArgumentCount: Integer;
    FBeforeCreate: TBeforeCreateJSONRPCHandlerEvent;
    FParamDefs: TJSONParamDefs;
    FPClass: TCustomJSONRPCHandlerClass;
    FDataModuleClass : TDataModuleClass;
    FHandlerMethodName: TJSONStringType;
    FHandlerClassName: TJSONStringType;
    FResultType: TJSONType;
    procedure CheckNames(const AClassName, AMethodName: TJSONStringType);
    function GetParamDefs: TJSONParamDefs;
    procedure SetFPClass(const AValue: TCustomJSONRPCHandlerClass);
    procedure SetHandlerClassName(const AValue: TJSONStringType);
    procedure SetHandlerMethodName(const AValue: TJSONStringType);
    procedure SetParamDefs(AValue: TJSONParamDefs);
  protected
    Function CreateInstance(AOwner : TComponent; Out AContainer : TComponent) : TCustomJSONRPCHandler; virtual;
    Property DataModuleClass : TDataModuleClass Read FDataModuleClass;
  Public
    Destructor Destroy; override;
    Function HaveParamDefs : Boolean;
    Property HandlerClassName : TJSONStringType Read FHandlerClassName Write SetHandlerClassName;
    Property HandlerMethodName : TJSONStringType Read FHandlerMethodName Write SetHandlerMethodName;
    Property HandlerClass : TCustomJSONRPCHandlerClass Read FPClass Write SetFPClass;
    Property BeforeCreate : TBeforeCreateJSONRPCHandlerEvent Read FBeforeCreate Write FBeforeCreate;
    Property AfterCreate : TJSONRPCHandlerEvent Read FAfterCreate Write FAfterCreate;
    Property ArgumentCount : Integer Read FArgumentCount Write FArgumentCount;
    Property ParamDefs : TJSONParamDefs Read GetParamDefs Write SetParamDefs;
    Property ResultType : TJSONType Read FResultType Write FResultType;
  end;
  TJSONRPCHandlerDefClass = Class of TJSONRPCHandlerDef;

  { TJSONRPCHandlerDefs }

  TJSONRPCHandlerDefs = Class(TCollection)
  private
    function GetH(Index : Integer): TJSONRPCHandlerDef;
    procedure SetH(Index : Integer; const AValue: TJSONRPCHandlerDef);
  Public
    Function IndexOfHandler(Const AClassName,AMethodName : TJSONStringType) : Integer;
    Function AddHandler(Const AClassName,AMethodName : TJSONStringType) : TJSONRPCHandlerDef; overload;
    Function AddHandler(Const AClassName,AMethodName : TJSONStringType; AClass : TCustomJSONRPCHandlerClass) : TJSONRPCHandlerDef; overload;
    Property HandlerDefs[Index : Integer] : TJSONRPCHandlerDef Read GetH Write SetH; default;
  end;

  { TCustomJSONRPCHandlerManager }

  TCustomJSONRPCHandlerManager = Class(TComponent)
  Private
    FRegistering: Boolean;
{$IFDEF CPU64}
    FHandlerCount : Int64;
{$ELSE}
    FHandlerCount : Integer;
{$ENDIF}
  Protected
    procedure Initialize; virtual;
    procedure DoClear; virtual;
    // Handler support
    Procedure RemoveHandlerDef(Const Index : Integer); virtual; abstract;
    function AddHandlerDef(Const AClassName,AMethodName : TJSONStringType) : TJSONRPCHandlerDef; virtual; abstract;
    function IndexOfHandlerDef(Const AClassName,AMethodName : TJSONStringType) : Integer; virtual; abstract;
    function GetHandlerDef(Index : Integer): TJSONRPCHandlerDef; virtual; abstract;
    function GetHandlerDefCount: Integer; virtual; abstract;
  Public
    // Handler support
    Procedure UnregisterHandler(Const AClassName, AMethodName : TJSONStringType);
    Procedure RegisterDatamodule(Const AClass : TDatamoduleClass; Const AHandlerClassName : TJSONStringType);
    Function RegisterHandler(Const AMethodName : TJSONStringType; AClass : TCustomJSONRPCHandlerClass; AArgumentCount : Integer = 0) : TJSONRPCHandlerDef; overload;
    Function RegisterHandler(Const AClassName,AMethodName : TJSONStringType; AClass : TCustomJSONRPCHandlerClass; AArgumentCount : Integer = 0) : TJSONRPCHandlerDef; overload;
    Function FindHandlerDefByName(Const AClassName,AMethodName : TJSONStringType) : TJSONRPCHandlerDef;
    Function GetHandlerDefByName(Const AClassName,AMethodName : TJSONStringType) : TJSONRPCHandlerDef;
    Function GetHandler(Const ADef : TJSONRPCHandlerDef; AOwner : TComponent; Out AContainer : TComponent): TCustomJSONRPCHandler;
    Function GetHandler(Const AClassName,AMethodName : TJSONStringType; AOwner : TComponent; Out AContainer : TComponent): TCustomJSONRPCHandler;
    Procedure GetClassNames (List : TStrings); // Should be a stringlist of TJSONStringType
    Procedure GetMethodsOfClass(Const AClassName : TJSONStringType; List : TStrings); // Should be a stringlist of TJSONStringType
    Procedure Clear;
    // properties
    Property Registering : Boolean Read FRegistering;
    Property HandlerCount : Integer Read GetHandlerDefCount;
    Property HandlerDefs[Index : Integer] : TJSONRPCHandlerDef Read GetHandlerDef;
  end;
  TCustomJSONRPCHandlerManagerClass = Class of TCustomJSONRPCHandlerManager;

  { TJSONRPCHandlerManager }

  TJSONRPCHandlerManager = Class(TCustomJSONRPCHandlerManager)
  Private
    FHandlerDefs : TJSONRPCHandlerDefs;
  Protected
    procedure DoClear; override;
    Function CreateDefs : TJSONRPCHandlerDefs; virtual;
    Procedure RemoveHandlerDef(Const Index : Integer); override;
    function AddHandlerDef(Const AClassName,AMethodName : TJSONStringType) : TJSONRPCHandlerDef; override;
    function IndexOfHandlerDef(Const AClassName,AMethodName : TJSONStringType) : Integer; override;
    function GetHandlerDef(Index : Integer): TJSONRPCHandlerDef; override;
    function GetHandlerDefCount: Integer; override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  end;


{ ---------------------------------------------------------------------
  Auxiliary stuff
  ---------------------------------------------------------------------}


  EJSONRPC = Class(Exception);
  TJSONErrorObject = Class(TJSONObject);

// Raise EJSONRPC exceptions.
Procedure JSONRPCError(const Msg : String);
Procedure JSONRPCError(const Fmt : String; const Args : Array of const);
Procedure JSONRPCParamError(const Msg: String);
Procedure JSONRPCParamError(const Fmt: String; const Args: array of const);

// Create an 'Error' object for an error response.
function CreateJSONErrorObject(Const AMessage : String; Const ACode : Integer) : TJSONObject;

// Create a JSON RPC 2 error response object containing an 'Error' object.
// Result is of type TJSONErrorObject
// ID is cloned if it is non-nil.
function CreateJSON2ErrorResponse(Const AMessage : String; Const ACode : Integer; ID : TJSONData = Nil; idname : TJSONStringType = 'id' ) : TJSONObject;
function CreateJSON2ErrorResponse(Const AFormat : String; Args : Array of const; Const ACode : Integer; ID : TJSONData = Nil; idname : TJSONStringType = 'id') : TJSONObject;
// Examines Req (request) and returns Error or an array of clones of Error)
Function CreateErrorForRequest(Const Req,Error : TJSONData) : TJSONData;

// Return TCustomJSONRPCHandlerManager instance to use for managing JSON-RPC handler.

Function JSONRPCHandlerManager : TCustomJSONRPCHandlerManager;

Var
  // Class that will be created. Must be set before first call to JSONRPCHandlerManager.
  JSONRPCHandlerManagerClass : TCustomJSONRPCHandlerManagerClass = TJSONRPCHandlerManager;
  // Class of Defs that will be created by TJSONRPCHandlerManager. Must be set before first call to JSONRPCHandlerManager.
  DefaultJSONRPCHandlerDefClass : TJSONRPCHandlerDefClass = TJSONRPCHandlerDef;

Const
  // JSON RPC 2.0 error codes
  EJSONRPCParseError     = -32700;
  EJSONRPCInvalidRequest = -32600;
  EJSONRPCMethodNotFound = -32601;
  EJSONRPCInvalidParams  = -32602;
  EJSONRPCInternalError  = -32603;

implementation


{$IFDEF FPC_DOTTEDUNITS}
uses {$IFDEF WMDEBUG}System.Dbugintf, {$ENDIF} FpWeb.JsonRpc.Codegen, FpWeb.JsonRpc.Strings;
{$ELSE FPC_DOTTEDUNITS}
uses {$IFDEF WMDEBUG}dbugintf, {$ENDIF} fprpccodegen, fprpcstrings;
{$ENDIF FPC_DOTTEDUNITS}

function CreateJSONErrorObject(const AMessage: String; const ACode: Integer
  ): TJSONObject;

begin
  Result:=TJSONErrorObject.Create(['code',ACode,'message',AMessage])
end;

function CreateJSON2ErrorResponse(const AMessage: String; const ACode: Integer;
  ID: TJSONData; idname: TJSONStringType): TJSONObject;

begin
  If (ID=Nil) then
    ID:=TJSONNull.Create
  else
    ID:=ID.Clone;
  Result:=TJSONErrorObject.Create(['jsonrpc','2.0','error',CreateJSONErrorObject(AMessage,ACode),idname,ID]);
end;

function CreateJSON2ErrorResponse(const AFormat: String; Args: array of const;
  const ACode: Integer; ID: TJSONData; idname: TJSONStringType): TJSONObject;

begin
  If (ID=Nil) then
    ID:=TJSONNull.Create
  else
    ID:=ID.Clone;
  Result:=TJSONErrorObject.Create(['jsonrpc','2.0','error',CreateJSONErrorObject(Format(AFormat,Args),ACode),idname,ID]);
end;

function CreateErrorForRequest(const Req, Error: TJSONData): TJSONData;

Var
  I : Integer;

begin
  if Req is TJSONArray then
    begin
    Result:=TJSONArray.Create;
    TJSONArray(Result).Add(Error);
    For I:=1 to TJSONArray(Req).Count-1 do
      TJSONArray(Result).Add(Error.Clone);
    end
  else
    Result:=Error;
end;


Var
  TheHandler : TCustomJSONRPCHandlerManager;

function JSONRPCHandlerManager: TCustomJSONRPCHandlerManager;
begin
  If (TheHandler=Nil) then
    TheHandler:=JSONRPCHandlerManagerClass.Create(Nil);
  JSONRPCHandlerManager:=TheHandler;
end;

procedure JSONRPCError(const Msg: String);

begin
  Raise EJSONRPC.Create(Msg);
end;

procedure JSONRPCError(const Fmt: String; const Args: array of const);

begin
  Raise EJSONRPC.CreateFmt(Fmt,Args);
end;

procedure JSONRPCParamError(const Msg: String);
begin
  raise EJSONRPC.CreateFmt(SErrParams, [Msg]);
end;

procedure JSONRPCParamError(const Fmt: String; const Args: array of const);
begin
  raise EJSONRPC.CreateFmt(SErrParams, [Format(Fmt, Args)]);
end;

{ TJSONRPCCallContext }

procedure TJSONRPCCallContext.SetParams(AValue: TJSONData);
begin
  if FParams=AValue then Exit;
  FParams:=AValue;
end;

{ TAPIDescriptionCreator }

function TAPIDescriptionCreator.GetOwner: TPersistent;
begin
  Result:=FDispatcher;
end;

constructor TAPIDescriptionCreator.Create(aDispatcher: TCustomJSONRPCDispatcher);
begin
  FDispatcher:=aDispatcher;
  DefaultOptions:=[caoFullParams];
end;

procedure TAPIDescriptionCreator.Assign(Source: TPersistent);

Var
  C : TAPIDescriptionCreator absolute Source;

begin
  if Source is TAPIDescriptionCreator then
    begin
    URL:=C.URL;
    NameSpace:=C.FNameSpace;
    FAPIType:=C.APIType;
    DefaultOptions:=C.DefaultOptions;
    end
  else
    inherited Assign(Source);
end;


{ TJSONParamDef }

procedure TJSONParamDef.SetName(const AValue: TJSONStringType);

Var
  D: TJSONParamDef;

begin
  if FName=AValue then exit;
  If Assigned(Collection) and (Collection is TJSONParamDefs) then
    begin
    D:=(Collection as TJSONParamDefs).FindParamDef(AValue);
    If (D<>Nil) and (D<>Self) then
      JSONRPCError(SErrDuplicateParam,[AValue]);
    end;
  FName:=AValue;
end;

function TJSONParamDef.GetDisplayName: string;
begin
  Result:=FName;
  If (Result='') then
    Result:=Inherited GetDisplayName;
end;

constructor TJSONParamDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FType:=jtString;
  FRequired:=True;
end;

procedure TJSONParamDef.Assign(Source: TPersistent);

Var
  P : TJSONParamDef;

begin
  If Source is TJSONParamDef then
    begin
    P:=TJSONParamDef(Source);
    FType:=P.DataType;
    FName:=P.Name;
    FRequired:=P.Required;
    end
  else
    inherited Assign(Source);
end;

{ TJSONParamDefs }

function TJSONParamDefs.GetP(AIndex : Integer): TJSONParamDef;
begin
  Result:=TJSONParamDef(Items[AIndex]);
end;

procedure TJSONParamDefs.SetP(AIndex : Integer; const AValue: TJSONParamDef);
begin
  Items[AIndex]:=AValue;
end;

function TJSONParamDefs.AddParamDef(const AName: TJSONStringType;
  AType: TJSONType; ARequired: Boolean): TJSONParamDef;
begin
  Result:=Add as TJSONParamDef;
  try
    Result.Name:=AName;
    Result.DataType:=Atype;
    Result.Required:=ARequired;
  except
    FReeAndNil(Result);
    Raise;
  end;
end;

function TJSONParamDefs.IndexOfParamDef(const AName: TJSONStringType): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(AName,GetP(result).Name)<>0) do
    Dec(Result);
end;

function TJSONParamDefs.FindParamDef(const AName: TJSONStringType): TJSONParamDef;

Var
  I : integer;

begin
  I:=IndexOfParamDef(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetP(I);
end;

function TJSONParamDefs.ParamDefByName(const AName: TJSONStringType): TJSONParamDef;
begin
  Result:=FindParamDef(AName);
  If (Result=Nil) then
    JSONRPCError(SErrUnknownParamDef,[AName]);
end;

{ TCustomJSONRPCHandler }

procedure TCustomJSONRPCHandler.CheckParams(const Params: TJSONData);

Var
  B : Boolean;

begin
  Try
    DoCheckParams(Params);
  Except
    On E : Exception do
      begin
      B:=True;
      If Assigned(FonParamError) then
        FonParamError(Self,E,B);
      If B then
        Raise;
      end;
  end;
end;

function TCustomJSONRPCHandler.ParamByName(const AName: String): TJSONData;

Var
  I : Integer;
  N : String;

begin
  If (FExecParams=Nil) or Not (FExecParams.JSONType in [jtArray,jtObject]) then
    Result:=Nil
  else
    begin
    I:=ParamDefs.IndexOfParamDef(AName);
    If (I=-1) then
      N:=AName
    else
      N:=ParamDefs[i].Name; // Search with original defined name.
    If (FExecParams is TJSONObject) then
      Result:=TJSONObject(FExecParams).Elements[N]
    else if (FExecParams is TJSONArray) then
      begin
      If (I=-1) or (I>=FExecParams.Count) then
        JSONRPCError(SErrUnknownParamDef,[AName]);
      Result:=TJSONArray(FExecParams).Items[i];
      end;
    end;
end;

procedure TCustomJSONRPCHandler.SetRequestClassAndMethod(const aClassName, aMethodName: String);
begin
  // Do nothing
  if aClassName=aMethodName then;
end;

procedure TCustomJSONRPCHandler.SetParamDefs(const AValue: TJSONParamDefs);
begin
  if FParamDefs=AValue then exit;
  FParamDefs.Assign(AValue);
end;

function TCustomJSONRPCHandler.GetRPCMethodName: String;
begin
  Result:=FRPCMethodName;
  if Result='' then
    Result:=Name;
end;

procedure TCustomJSONRPCHandler.SetRPCMethodName(AValue: String);
begin
  if aValue=FRPCMethodName then
    Exit;
  FRPCMethodName:=aValue;
end;

procedure TCustomJSONRPCHandler.DoCheckParams(const Params: TJSONData);

Var
  I,ReqdCount : Integer;
  def: TJSONParamDef;

begin
  if (Params is TJSONObject) then
  begin
    if (jroArrayParams in Options) then
      JSONRPCParamError(SErrParamsMustBeArray);

    DoCheckParamDefsOnObject(Params as TJSONObject);
  end else
  if (Params is TJSONArray) then
  begin
    If (jroObjectParams in Options) then
      JSONRPCParamError(SErrParamsMustBeArray);

    DoCheckParamArray(Params as TJSONArray);
  end
  else
    begin
    if (Params=Nil) then
      begin
      ReqdCount:=0;
      for TCollectionItem(def) in ParamDefs do
        if Def.Required then
          Inc(ReqdCount);
      if ReqdCount>0 then
        JSONRPCParamError(SErrParamsRequiredParams,[ReqdCount])
      end;
    end;
end;

procedure TCustomJSONRPCHandler.DoCheckParamDefsOnObject(
  const ParamObject: TJSONObject);
var
  def: TJSONParamDef;
  Param: TJSONData;
  PropEnum: TJSONEnum;
begin
  for TCollectionItem(def) in ParamDefs do
  begin
    // assert the typecast in for loop
    Assert(def is TJSONParamDef,'Unexpected ParamDef item class.');

    Param:=ParamObject.Find(def.Name);
    // check required parameters
    if not Assigned(Param) then
    begin
      if def.Required then
        JSONRPCParamError(SErrParamsRequiredParamNotFound,[def.Name])
      else
        Continue;
    end;

    // jtUnkown accepts all data types
    if (def.DataType<>jtUnknown) and not (Param.JSONType=def.DataType) then
      JSONRPCParamError(SErrParamsDataTypeMismatch,[def.Name,JSONTypeName(def.DataType),JSONTypeName(Param.JSONType)]);
  end;

  // check if additional parameters are given
  if not (jroIgnoreExtraFields in Options) then
  begin
    for PropEnum in ParamObject do
    begin
      // only check for name is required other specs are checked before
      if ParamDefs.FindParamDef(PropEnum.Key)=nil then
        JSONRPCParamError(SErrParamsNotAllowd,[PropEnum.Key]);
    end;
  end;
end;

procedure TCustomJSONRPCHandler.DoCheckParamArray(const ParamArray: TJSONArray);

var
  I : Integer;
  Param: TJSONData;
  Def : TJSONParamDef;

begin
  for I:=0 to ParamDefs.Count-1 do
    begin
    Def:=ParamDefs[i];
    if I>=ParamArray.Count then
      begin
      if ParamDefs[i].Required then
        JSONRPCParamError(SErrParamsRequiredParamNotFound,[def.Name]);
      end
    else
      begin  
      Param:=ParamArray[i];
      // jtUnkown accepts all data types
      if (def.DataType<>jtUnknown) and not (Param.JSONType=def.DataType) then
        JSONRPCParamError(SErrParamsDataTypeMismatch,[def.Name,JSONTypeName(def.DataType),JSONTypeName(Param.JSONType)]);
      end;  
    end;
end;

function TCustomJSONRPCHandler.DoExecute(Const Params: TJSONData;AContext : TJSONRPCCallContext): TJSONData;
begin
  Result:=Nil;
end;

constructor TCustomJSONRPCHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParamDefs:=CreateParamDefs;
end;

destructor TCustomJSONRPCHandler.Destroy;
begin
  FreeAndNil(FParamDefs);
  inherited Destroy;
end;

function TCustomJSONRPCHandler.CreateParamDefs : TJSONParamDefs;

begin
  Result:=TJSONParamDefs.Create(Self,TJSONParamDef);
end;

function TCustomJSONRPCHandler.Execute(Const Params: TJSONData;AContext : TJSONRPCCallContext = Nil): TJSONData;
begin
  If Assigned(FBeforeExecute) then
    FBeforeExecute(Self);
  if (jroCheckParams in Options) then
    CheckParams(Params);
  FExecParams:=Params;
  try
    Result:=DoExecute(Params,AContext);
  finally
    FExecParams:=Nil;
  end;
  If Assigned(FAfterExecute) then
    FAfterExecute(Self);
end;

{ TJSONRPCHandler }

function TJSONRPCHandler.DoExecute(const Params: TJSONData;AContext : TJSONRPCCallContext): TJSONData;
begin
  Result:=Nil;
  If Assigned(FOnContextExecute) then
    FOnContextExecute(Self,aContext,Params,Result)
  else If Assigned(FOnExecute) then
    FOnExecute(Self,Params,Result);
end;

{ TJSONRPCEcho }

function TJSONRPCEcho.DoExecute(const Params: TJSONData;AContext : TJSONRPCCallContext): TJSONData;

Var
  I : Integer;
  A : TJSONArray;
  S : TJSONStringType;

begin
  If Params is TJSONArray then
    begin
    A:=Params as TJSONArray;
    S:='';
    For I:=0 to A.Count-1 do
      begin
      if (S<>'') then
        S:=S+' ';
      S:=S+A.Items[i].AsString;
      end;
    end
  else If Params.JSONType in [jtObject,jtNumber] then
    S:=Params.AsJSON
  else
    S:=Params.AsString;
  Result:=TJSONString.Create(S);
end;

{ TCustomJSONRPCDispatcher }

// Create API method description

Function TAPIDescriptionCreator.CreateParamDef(aDef: TJSONParamDef) : TJSONObject;

begin
  With aDef do
    Result:=TJSONObject.Create(['name',Name,'type',JSONTypeName(DataType),'required',Required]);
end;

procedure TAPIDescriptionCreator.AddParamDefs(O: TJSONObject; Defs: TJSONParamDefs);

Var
  A : TJSONArray;
  I : Integer;

begin
  A:=TJSONArray.Create;
  O.Add('paramdefs',A);
  For I:=0 to Defs.Count-1 do
    A.Add(CreateParamDef(Defs[i]));
end;

Function TAPIDescriptionCreator.HandlerToAPIMethod (H: TCustomJSONRPCHandler; aOptions : TCreateAPIOptions): TJSONObject;

begin
  Result:=TJSONObject.Create(['name',H.Name,'len',H.ParamDefs.Count]);
  if Not (caoFullParams in aOptions) then exit;
  Result.Add('resulttype',JSONTypeName(H.ResultType));
  if (H.ParamDefs.Count>0) then
    AddParamDefs(Result,H.ParamDefs);
end;

Function TAPIDescriptionCreator.HandlerDefToAPIMethod (H: TJSONRPCHandlerDef; aOptions: TCreateAPIOptions): TJSONObject;

begin
  Result:=TJSONObject.Create(['name',H.HandlerMethodName,'len',H.ArgumentCount]);
  if Not (caoFullParams in aOptions) then exit;
  Result.Add('resulttype',JSONTypeName(H.ResultType));
  if (H.ParamDefs.Count>0) then
    AddParamDefs(Result,H.ParamDefs);
end;

function TAPIDescriptionCreator.GetNameSpace: String;
begin
  Result:=FNameSpace;
  If (Result='') then
    Result:=DefaultNameSpace
end;

function TAPIDescriptionCreator.isNameSpaceStored: Boolean;
begin
  Result:=NameSpace<>DefaultNameSpace;
end;

function TAPIDescriptionCreator.DefaultNameSpace: String;
begin
  Result:='';
end;

function TAPIDescriptionCreator.PublishHandler(H: TCustomJSONRPCHandler): Boolean;
begin
  Result:=(H<>Nil)
end;

Function TAPIDescriptionCreator.PublishHandlerDef(HD: TJSONRPCHandlerDef): Boolean;

begin
  Result:=(HD<>Nil)
end;

function TAPIDescriptionCreator.CreateAPI(aOptions: TCreateAPIOptions): TJSONObject;

Var
  A,D : TJSONObject;
  R : TJSONArray;
  N : TJSONStringType;
  H : TCustomJSONRPCHandler;
  I,J : Integer;
  M : TCustomJSONRPCHandlerManager;
  HD : TJSONRPCHandlerDef;
  search : Boolean;
  C : TComponent;

begin
  D:=TJSONObject.Create;
  try
    D.Add('url',URL);
    D.Add('type',APIType);
    A:=TJSONObject.Create;
    D.Add('actions',A);
    R:=Nil;
    N:='';
    Search:=assigned(Dispatcher) and (jdoSearchOwner in Dispatcher.Options);
    C:=Dispatcher.Owner;
    If Search and Assigned(C) then
      begin
      for I:=C.ComponentCount-1 downto 0 do
        If C.Components[i] is TCustomJSONRPCHandler then
          begin
          H:=C.Components[i] as TCustomJSONRPCHandler;
          if PublishHandler(H) then
            begin
            If (R=Nil) then
              begin
              N:=C.Name;
              R:=TJSONArray.Create;
              A.Add(N,R);
              end;
            R.Add(HandlerToAPIMethod(H,aOptions));
            end;
          end;
      end;
    Search:=assigned(Dispatcher) and (jdoSearchRegistry in Dispatcher.Options);
    If Search then
      begin
      M:=JSONRPCHandlerManager;
      For I:=M.HandlerCount-1 downto 0 do
        begin
        HD:=M.HandlerDefs[i];
        if PublishHandlerDef(HD) then
          begin
          If (R=Nil) or (CompareText(N,HD.HandlerClassName)<>0) then
            begin
            N:=HD.HandlerClassName;
            J:=A.IndexOfName(N);
            If (J=-1) then
              begin
              R:=TJSONArray.Create;
              A.Add(N,R);
              end
            else
              R:=A.Items[J] as TJSONArray;
            end;
          R.Add(HandlerDefToAPIMethod(HD,aOptions));
          end;
        end;
      end;
      Result:=D;
  except
    FreeAndNil(D);
    Raise;
  end;
end;

function TAPIDescriptionCreator.CreateAPI: TJSONObject;
begin
  Result:=CreateAPI(DefaultOptions);
end;

procedure TCustomJSONRPCDispatcher.SetAPICreator(AValue: TAPIDescriptionCreator);
begin
  if FAPICreator=AValue then Exit;
  FAPICreator.Assign(AValue);
end;

function TCustomJSONRPCDispatcher.CreateAPICreator: TAPIDescriptionCreator;
begin
  Result:=TAPIDescriptionCreator.Create(Self);
end;


function TCustomJSONRPCDispatcher.FindHandler(const AClassName, AMethodName: TJSONStringType;AContext : TJSONRPCCallContext;Out FreeObject : TComponent): TCustomJSONRPCHandler;

Var
  C : TComponent;
  D : TJSONRPCHandlerDef;
  I : Integer;


begin
  Result:=Nil;
  FreeObject:=Nil;
  If Assigned(Owner) and ((AClassName='') or (CompareText(AClassName,Owner.name)=0)) then
    begin
    I:=0;
    While (Result=Nil) and (I<Owner.ComponentCount) do
      begin
      C:=Owner.Components[i];
      If (C is TCustomJSONRPCHandler) and SameText(TCustomJSONRPCHandler(C).RPCMethodName,aMethodName) then
        Result:=TCustomJSONRPCHandler(C);
      Inc(I);
      end;
    end;
  If (Result=Nil) and (jdoSearchRegistry in Options) then
    begin
    D:=JSONRPCHandlerManager.FindHandlerDefByName(AClassName,AMethodName);
    If Assigned(D) then
      Result:=JSONRPCHandlerManager.GetHandler(D,Self,FreeObject);
    end;
 If (Result=Nil) and Assigned(FFindHandler) then
    begin
    FFindhandler(Self,AClassName,AMethodName,Result);
    FreeObject:=Nil
    end;
end;

Function TCustomJSONRPCDispatcher.ExecuteHandler(H : TCustomJSONRPCHandler; Params,ID: TJSONData;AContext : TJSONRPCCallContext): TJSONData;
begin
  Result:=H.Execute(Params,AContext);
end;

function TCustomJSONRPCDispatcher.ExecuteMethod(Const AClassName,AMethodName: TJSONStringType;
  Params,ID: TJSONData;AContext : TJSONRPCCallContext): TJSONData;

Var
  H : TCustomJSONRPCHandler;
  FreeObject : TComponent;

begin
  H:=FindHandler(AClassName,AMethodName,AContext,FreeObject);
  If (H=Nil) then
    begin
    // ID is cloned by CreateJSON2Error
    if (AClassName='') then
      Exit(CreateJSON2Error(SErrInvalidMethodName,[AMethodName],EJSONRPCMethodNotFound,ID,transactionProperty))
    else
      Exit(CreateJSON2Error(SErrInvalidClassMethodName,[AClassName,AMethodName],EJSONRPCMethodNotFound,ID,transactionProperty));
    end;
  H.SetRequestClassAndMethod(aClassName,aMethodName);
  try
    If Assigned(FOndispatchRequest) then
      FOndispatchRequest(Self,AClassName,AMethodName,Params);
    Result:=ExecuteHandler(H,Params,ID,AContext);
  finally
    If Assigned(FreeObject) then
      FreeAndNil(FreeObject);
  end;
end;

function TCustomJSONRPCDispatcher.FormatResult(Const AClassName, AMethodName: TJSONStringType;
Const Params,ID, Return : TJSONData) : TJSONData;

begin
  Result:=TJSONObject.Create(['result',Return,transactionproperty,ID.Clone]);
  if jdoJSONRPC2 in options then
    TJSONObject(Result).Add('jsonrpc','2.0')
  else
    TJSONObject(Result).Add('error',TJSonNull.Create);
end;

function TCustomJSONRPCDispatcher.CreateJSON2Error(const AMessage: String;
  const ACode: Integer; ID: TJSONData; idname: TJSONStringType): TJSONObject;
begin
  Result:=CreateJSON2ErrorResponse(AMessage,ACode,ID,IDName);
end;

function TCustomJSONRPCDispatcher.CreateJSON2Error(const AFormat: String;
  Args: array of const; const ACode: Integer; ID: TJSONData;
  idname: TJSONStringType): TJSONObject;
begin
  Result:=CreateJSON2Error(Format(AFormat,Args),ACode,ID,IDName);
end;

function TCustomJSONRPCDispatcher.ExecuteRequest(ARequest: TJSONData;AContext : TJSONRPCCallContext): TJSONData;

Var
  C,M : TJSONStringType;
  Id,P : TJSONData;


begin
  Result:=Nil;
  try
    Result:=CheckRequest(ARequest,C,M,ID,P);
    If (Result=Nil) then
      begin
      If Assigned(AContext) then
        begin
        AContext.ClassName:=C;
        AContext.Method:=M;
        if Assigned(ID) then
          AContext.TID:=ID.AsJSON;
        end;
      Result:=ExecuteMethod(C,M,P,ID,AContext);
      // Do some sanity checks.
      If (Result=Nil) then
        begin
        // No response, and a response was expected.
        if (ID<>Nil) and (jdoStrictNotifications in Options) then
          Result:=CreateJSON2Error(SErrNoResponse,[M],EJSONRPCInternalError,ID,transactionProperty);
        end
      else
        begin
        // A response was received, and no response was expected.
        if ((ID=Nil) or (ID is TJSONNull))  and (jdoStrictNotifications in Options) then
          Result:=CreateJSON2Error(SErrResponseFromNotification,[M],EJSONRPCInternalError,Nil,transactionProperty);
        If (ID=Nil) or (ID is TJSONNull) then // Notification method, discard result.
          FreeAndNil(Result);
        end;
      end;
    If Assigned(Result) and not (Result is TJSONErrorObject) then
        Result:=FormatResult(C,M,P,ID,Result)
  except
    // Something went really wrong if we end up here.
    On E : Exception do
      begin
      If (Result<>Nil) then
        FreeAndNil(Result);
      If Assigned(ID) and not (ID is TJSONNull) then
        Result:=CreateJSON2Error(E.Message,EJSONRPCInternalError,ID,transactionproperty)
      else
        Result:=CreateJSON2Error(E.Message,EJSONRPCInternalError,Nil,transactionproperty);
      end;
  end;
end;

function TCustomJSONRPCDispatcher.DoExecute(Requests: TJSONData;AContext : TJSONRPCCallContext): TJSONData;

Var
  A : TJSONArray;
  Res : TJSONData;
  I : Integer;

begin
  Result:=Nil;
  If Requests is TJSONArray then
    begin
    A:=Requests as TJSONArray;
    Result:=TJSONArray.Create();
    For I:=0 to A.Count-1 do
      begin
      Res:=ExecuteRequest(A[i],AContext);
      If (Res<>Nil) then
        TJSONArray(Result).Add(Res);
      end;
    end
  else
    Result:=ExecuteRequest(Requests,ACOntext);
end;


function TCustomJSONRPCDispatcher.CheckRequest(Request: TJSONData; Out AClassName,AMethodName : TJSONStringType; Out ID, Params : TJSONData): TJSONData;

Var
  O : TJSONObject;
  I : Integer;
  D : TJSONData;
  OJ2 : Boolean;


begin
  AMethodName:='';
  AClassName:='';
  ID:=Nil;
  Params:=Nil;
  Result:=Nil;
  If Not (Request is TJSONObject) then
    Exit(CreateJSON2Error(SErrRequestMustBeObject,EJSONRPCInvalidRequest,Nil,transactionproperty));
  O:=TJSONObject(Request);
  // Get ID object, if it exists.
  I:=O.IndexOfName(TransactionProperty);
  If (I<>-1) then
    ID:=O.Items[i];
  // Check ID
  If (ID=Nil) and not (jdoNotifications in Options) then
    Exit(CreateJSON2Error(SErrNoIDProperty,EJSONRPCInvalidRequest,Nil,transactionproperty));
  OJ2:=(jdoJSONRPC2 in Options) and not (jdoJSONRPC1 in Options);
  If OJ2 then
    begin
    if Assigned(ID) and not (ID.JSONType in [jtNull,jtString,jtNumber]) then
      Exit(CreateJSON2Error(SErrINvalidIDProperty,EJSONRPCInvalidRequest,Nil,transactionproperty));
    // Check presence and value of jsonrpc property
    I:=O.IndexOfName('jsonrpc');
    If (I=-1) then
      Exit(CreateJSON2Error(SErrNoJSONRPCProperty,EJSONRPCInvalidRequest,ID,transactionproperty));
    If (O.Items[i].JSONType<>jtString) or (O.Items[i].AsString<>'2.0') then
      Exit(CreateJSON2Error(SErrInvalidJSONRPCProperty,EJSONRPCInvalidRequest,ID,transactionproperty));
    end;
  // Get method name, if it exists.
  I:=O.IndexOfName(MethodProperty);
  If (I<>-1) then
    D:=O.Items[i]
  else
    Exit(CreateJSON2Error(SErrNoMethodName,[MethodProperty],EJSONRPCInvalidRequest,ID,transactionproperty));
  // Check if it is a string
  if Not (D is TJSONString) then
    Exit(CreateJSON2Error(SErrInvalidMethodType,[MethodProperty],EJSONRPCInvalidRequest,ID,transactionproperty));
  AMethodName:=D.AsString;
  If (AMethodName='') then
    Exit(CreateJSON2Error(SErrNoMethodName,[MethodProperty],EJSONRPCInvalidRequest,ID,transactionproperty));
  // Get class name, if it exists and is required
  If (ClassNameProperty<>'') then
    begin
    I:=O.IndexOfName(ClassNameProperty);
    If (I<>-1) then
      D:=O.Items[i]
    else if (jdoRequireClass in options) then
      Exit(CreateJSON2Error(SErrNoClassName,[ClassNameProperty],EJSONRPCInvalidRequest,ID,transactionproperty))
    else
      D:=Nil;  
    if Assigned(D) then
     begin  
      // Check if it is a string
      if Not (D is TJSONString) then
        Exit(CreateJSON2Error(SErrInvalidClassNameType,[ClassNameProperty],EJSONRPCInvalidRequest,ID,transactionproperty));
      AClassName:=D.AsString;
      If (AClassName='') and (jdoRequireClass in options)  then
        Exit(CreateJSON2Error(SErrNoClassName,[ClassNameProperty],EJSONRPCInvalidRequest,ID,transactionproperty));
      end;  
    end;
  // Get params, if they exist
  I:=O.IndexOfName(ParamsProperty);
  If (I=-1) then
    begin
    D:=Nil;
    if Not OJ2 then
      Exit(CreateJSON2Error(SErrNoParams,[ParamsProperty],EJSONRPCInvalidParams,ID,transactionproperty));
    end
  else
    begin
    D:=O.Items[i];
    if OJ2 then
      begin
      // Allow array or object
      If Not (D.JSONType in [jtArray,jtObject]) then
        Exit(CreateJSON2Error(SErrParamsMustBeArrayorObject,EJSONRPCInvalidParams,ID,transactionproperty));
      end
    else if not (jdoJSONRPC2 in Options) then
      begin
      // Allow only array
      If Not (D.JSONType in [jtArray]) then
        Exit(CreateJSON2Error(SErrParamsMustBeArray,EJSONRPCInvalidParams,ID,transactionproperty));
      end;
    end;
  Params:=D;
end;

function TCustomJSONRPCDispatcher.CheckRequests(Requests: TJSONData): TJSONData;

Var
  A : TJSONArray;
  O : TJSONObject;
  ID : TJSONData;
  I,J : Integer;

begin
  Result:=Nil;
  If (Requests is TJSONArray) then
    begin
    A:=Requests as TJSONArray;
    If Not (jdoJSONRPC2 in Options) then
      begin
      Result:=TJSONArray.Create;
      For I:=0 to A.Count-1 do
        begin
        ID:=Nil;
        If (A.Items[i] is TJSONObject) then
          begin
          O:=A.Objects[i];
          J:=O.IndexOfName('id');
          if (J<>-1) then
            ID:=O.Items[J];
          end;
        TJSONArray(Result).Add(CreateJSON2ErrorResponse(SErrJSON2NotAllowed,EJSONRPCInvalidRequest,ID,transactionproperty));
        end;
      end;
    end
  else
    If not (Requests is TJSONObject) then
      Result:=CreateJSON2ErrorResponse(SErrRequestMustBeObject,EJSONRPCInvalidRequest,Nil,transactionproperty);
end;

class function TCustomJSONRPCDispatcher.TransactionProperty: String;
begin
  Result:='id'; // Do not localize
end;

class function TCustomJSONRPCDispatcher.MethodProperty: String;
begin
  Result:='method'; // Do not localize
end;

class function TCustomJSONRPCDispatcher.ClassNameProperty: String;
begin
  Result:='class'; // Do not localize
end;

class function TCustomJSONRPCDispatcher.ParamsProperty: String;
begin
  Result:='params'; // Do not localize
end;

constructor TCustomJSONRPCDispatcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAPICreator:=CreateAPICreator;
  FOptions:=DefaultDispatchOptions;
end;

destructor TCustomJSONRPCDispatcher.Destroy;
begin
  FreeAndNil(FAPICreator);
  FreeAndNil(FCachedAPI);
  inherited Destroy;
end;

function TCustomJSONRPCDispatcher.Execute(Requests: TJSONData;AContext : TJSONRPCCallContext = Nil): TJSONData;
begin
  If Assigned(FOnStartBatch) then
    FOnStartBatch(Self);
  Result:=CheckRequests(Requests);
  if (Result=Nil) then // Form is OK and allowed.
    Result:=DoExecute(Requests,AContext);
  If Assigned(FOnEndBatch) then
    FOnEndBatch(Self);
end;

function TCustomJSONRPCDispatcher.CreateAPI(aOptions: TCreateAPIOptions): TJSONObject;

Var
  CAO : TCreateAPIOptions;

begin
  CAO:=aOptions-[caoFormatted];
  Result:=Nil;
  if (jdoCacheAPI in Options)
     and (FCachedAPI<>Nil)
     and (CAO=FCachedAPIOptions) then
    Result:=TJSONObject(FCachedAPI.Clone)
  else
    begin
    Result:=APICreator.CreateAPI(aOptions);
    if (jdoCacheAPI in Options) then
      begin
      FCachedAPI:=TJSONObject(Result.Clone);
      FCachedAPIOptions:=CAO;
      end;
    end;
end;

function TCustomJSONRPCDispatcher.CreateAPI: TJSONObject;
begin
  Result:=CreateAPI(APICreator.DefaultOptions);
end;

function TCustomJSONRPCDispatcher.APIAsPascal(aOptions: TCreateAPIOptions; aUnitName: string): String;

Var
  J : TJSONObject;
  Gen : TAPIClientCodeGen;

begin
  J:=APICreator.CreateAPI(aOptions);
  try
    Gen:=TAPIClientCodeGen.Create(Self);
    if aUnitName='' then
      aUnitName:='services';
    Gen.OutputUnitName:=aUnitName;
    Gen.API:=J;
    Gen.Execute;
    Result:=Gen.Source.Text;
  finally
    Gen.Free;
    J.Free;
  end;
end;

function TCustomJSONRPCDispatcher.APIAsString(aOptions: TCreateAPIOptions): TJSONStringType;

Var
  S : TJSONObject;

begin
  S:=CreateAPI(aOptions);
  try
    if caoFormatted in aOptions then
      Result:=S.FormatJSON()
    else
      Result:=S.AsJSON;
    if APICreator.NameSpace<>'' then
      Result:=APICreator.NameSpace+' = '+Result;
  finally
    S.Free;
  end;
end;

function TCustomJSONRPCDispatcher.APIAsString: TJSONStringType;
begin
  Result:=APIAsString(APICreator.DefaultOptions);
end;

{ TJSONRPCHandlerDef }

procedure TJSONRPCHandlerDef.SetFPClass(const AValue: TCustomJSONRPCHandlerClass
  );
begin
  FPClass:=AValue;
end;

procedure TJSONRPCHandlerDef.CheckNames(const AClassName,
  AMethodName: TJSONStringType);

Var
  I : Integer;

begin
  If Assigned(Collection) and (Collection is TJSONRPCHandlerDefs) then
    begin
    I:=TJSONRPCHandlerDefs(Collection).IndexOfHandler(AClassName,AMethodName);
    If (I<>-1) and (Collection.Items[i]<>Self) then
      if (AClassName<>'') then
        JSONRPCError(SErrDuplicateJSONRPCClassHandlerName,[AClassName,AMethodName])
      else
        JSONRPCError(SErrDuplicateJSONRPCHandlerName,[AClassName,AMethodName]);
    end;
end;

function TJSONRPCHandlerDef.GetParamDefs: TJSONParamDefs;
begin
  IF (FParamDefs=Nil) then
    FParamDefs:=TJSONParamDefs.Create(Self,TJSONParamDef);
  Result:=FParamDefs;
end;

procedure TJSONRPCHandlerDef.SetHandlerClassName(const AValue: TJSONStringType);
begin
  if FHandlerClassName=AValue then exit;
  CheckNames(AValue,HandlerMethodName);
  FHandlerClassName:=AValue;
end;

procedure TJSONRPCHandlerDef.SetHandlerMethodName(const AValue: TJSONStringType
  );
begin
  if FHandlerMethodName=AValue then exit;
  CheckNames(HandlerClassName,AValue);
  FHandlerMethodName:=AValue;
end;

procedure TJSONRPCHandlerDef.SetParamDefs(AValue: TJSONParamDefs);
begin
  if FParamDefs=AValue then Exit;
  IF (FParamDefs=Nil) then
    FParamDefs:=TJSONParamDefs.Create(Self,TJSONParamDef);
  if (AValue<>Nil) then
    FParamDefs.Assign(AValue)
  else
    FreeAndNil(FParamDefs);
end;

function TJSONRPCHandlerDef.CreateInstance(AOwner: TComponent; out
  AContainer: TComponent): TCustomJSONRPCHandler;

Var
  AClass : TCustomJSONRPCHandlerClass;
  DM : TDataModule;
  C : TComponent;
  I : Integer;

begin
  Result:=Nil;
  {$ifdef wmdebug}SendDebug(Format('Creating instance for %s',[Self.HandlerMethodName]));{$endif}
  If Assigned(FDataModuleClass) then
    begin
    {$ifdef wmdebug}SendDebug(Format('Creating datamodule from class %d ',[Ord(Assigned(FDataModuleClass))]));{$endif}
    DM:=FDataModuleClass.Create(Nil);
    {$ifdef wmdebug}SendDebug(Format('Created datamodule from class %s ',[DM.ClassName]));{$endif}
    I:=0;
    While (Result=Nil) and (I<DM.ComponentCount) do
      begin
      C:=DM.Components[i];
      If (C is TCustomJSONRPCHandler) and SameText(TCustomJSONRPCHandler(C).RPCMethodName,FHandlerMethodName) then
        Result:=TCustomJSONRPCHandler(C);
      inc(I);
      end;
    If Result=Nil then
      begin
      FreeAndNil(DM);
      JSONRPCError(SErrUnknownJSONRPCMethodHandler,[FHandlerMethodName]);
      end;
    end
  else
    DM:=TDataModule.CreateNew(Nil,0);
  AContainer:=DM;
  If (Result=Nil) then
    begin
    {$ifdef wmdebug}SendDebug(Format('Creating from class pointer %d ',[Ord(Assigned(FPClass))]));{$endif}
    AClass:=FPCLass;
    If Assigned(FBeforeCreate) then
      FBeforeCreate(Self,AClass);
    Result:=AClass.Create(AContainer);
    end;
  If Assigned(FAfterCreate) then
    FAfterCreate(Self,Result);
end;

destructor TJSONRPCHandlerDef.Destroy;
begin
  FreeAndNil(FParamDefs);
  inherited Destroy;
end;

function TJSONRPCHandlerDef.HaveParamDefs: Boolean;
begin
  Result:=Assigned(FParamDefs);
end;

{ TJSONRPCHandlerDefs }

function TJSONRPCHandlerDefs.GetH(Index: Integer): TJSONRPCHandlerDef;
begin
  Result:=TJSONRPCHandlerDef(Items[Index]);
end;

procedure TJSONRPCHandlerDefs.SetH(Index: Integer;
  const AValue: TJSONRPCHandlerDef);
begin
  Items[Index]:=AValue;
end;

function TJSONRPCHandlerDefs.AddHandler(const AClassName,
  AMethodName: TJSONStringType): TJSONRPCHandlerDef;
begin
  If (IndexOfHandler(AClassName,AMethodName)<>-1) then
    If (AClassName<>'') then
      JSONRPCError(SErrDuplicateJSONRPCClassHandlerName,[AClassName,AMethodName])
    else
      JSONRPCError(SErrDuplicateJSONRPCHandlerName,[AMethodName]);
  Result:=TJSONRPCHandlerDef(Add);
  Result.FHandlerClassName:=AClassName;
  Result.FHandlerMethodName:=AMethodName;
end;

function TJSONRPCHandlerDefs.AddHandler(const AClassName,
  AMethodName: TJSONStringType; AClass: TCustomJSONRPCHandlerClass
  ): TJSONRPCHandlerDef;
begin
  Result:=AddHandler(AClassName,AMethodName);
  Result.HandlerClass:=AClass;
end;

function TJSONRPCHandlerDefs.IndexOfHandler(const AClassName,
  AMethodName: TJSONStringType): Integer;

  Function IsMatch(Index : Integer) : Boolean; inline;

  Var
    D : TJSONRPCHandlerDef;

  begin
    D:=GetH(Index);
    Result:=((AClassName='') or
             (CompareText(D.HandlerClassName,AClassName)=0)) and
            (CompareText(AMethodName,D.HandlerMethodName)=0)
  end;

begin
  Result:=Count-1;
  While (Result>=0) and Not IsMatch(Result) do
    Dec(Result)
end;

{ TCustomJSONRPCHandlerManager }

procedure TCustomJSONRPCHandlerManager.Initialize;
begin
  // Do nothing
end;

procedure TCustomJSONRPCHandlerManager.DoClear;
Var
  I : Integer;
  D : TJSONRPCHandlerDef;
  C,M : String;
begin
  For I:=HandlerCount-1 downto 0 do
    begin
    D:=HandlerDefs[i];
    C:=D.HandlerClassName;
    M:=D.HandlerMethodName;
    UnregisterHandler(C,M);
    end;
end;

Procedure TCustomJSONRPCHandlerManager.UnregisterHandler(Const AClassName,
  AMethodName: TJSONStringType);

Var
  I : Integer;

begin
  I:=IndexOfHandlerDef(AClassName,AMethodName);
  If (I<>-1) then
    RemoveHandlerDef(I)
  else
    If (AClassName<>'') then
      JSONRPCError(SErrUnknownJSONRPCClassMethodHandler,[AClassName,AMethodName])
    else
      JSONRPCError(SErrUnknownJSONRPCMethodHandler,[AMethodName]);
end;

Procedure TCustomJSONRPCHandlerManager.RegisterDatamodule(
  Const AClass: TDatamoduleClass; Const AHandlerClassName: TJSONStringType);

Var
  DM : TDatamodule;
  I,J : Integer;
  C : TComponent;
  H : TCustomJSONRPCHandler absolute C;
  D : TJSONRPCHandlerDef;
  B : Boolean;
  CN : TJSONStringType;

begin
  B:=FRegistering;
  try
    FRegistering:=True;
    DM:=AClass.Create(Self);
    try
      If (AHandlerClassName='') then
        CN:=DM.Name
      else
        CN:=AHandlerClassName;
      For I:=0 to DM.ComponentCount-1 do
        begin
        C:=DM.Components[i];
        if C is TCustomJSONRPCHandler then
          begin
          J:=IndexOfHandlerDef(CN,H.RPCMethodName);
          If (J<>-1) then
             JSONRPCError(SErrDuplicateRPCCLassMethodHandler,[CN,H.RPCMethodName]);
          D:=AddHandlerDef(CN,H.RPCMethodName);
          D.ArgumentCount:=H.ParamDefs.Count;
          D.ParamDefs:=H.ParamDefs;
          D.ResultType:=H.ResultType;
          {$ifdef wmdebug}SendDebug('Registering provider '+C.Name);{$endif}
          D.FDataModuleClass:=TDataModuleClass(DM.ClassType);
          end;
        end;
    finally
      DM.Free;
    end;
  finally
    FRegistering:=B;
  end;
end;

Function TCustomJSONRPCHandlerManager.RegisterHandler(
  Const AMethodName: TJSONStringType; AClass: TCustomJSONRPCHandlerClass;
  AArgumentCount: Integer): TJSONRPCHandlerDef;

begin
  Result:=RegisterHandler('',AMethodName,AClass,AArgumentCount);
end;

Function TCustomJSONRPCHandlerManager.RegisterHandler(Const AClassName,
  AMethodName: TJSONStringType; AClass: TCustomJSONRPCHandlerClass;
  AArgumentCount: Integer): TJSONRPCHandlerDef;

Var
  I : Integer;
  B : Boolean;
  H : TCustomJSONRPCHandler;

begin
  B:=FRegistering;
  try
    FRegistering:=True;
    I:=IndexOfHandlerDef(AClassName,AMethodname);
    If (I<>-1) then
      If (AClassName<>'') then
        JSONRPCError(SErrDuplicateRPCCLassMethodHandler,[AClassName,AMethodName])
      else
        JSONRPCError(SErrDuplicateRPCMethodHandler,[AMethodName]);
    Result:=AddHandlerDef(AClassName,AMEthodName);
    Result.HandlerClass:=AClass;
    Result.ArgumentCount:=AArgumentCount;
    H:=Aclass.Create(Nil);
    try
      Result.ParamDefs:=H.ParamDefs;
      Result.ResultType:=H.ResultType;
    finally
      H.Free;
    end;
  finally
    FRegistering:=B;
  end;
end;

Function TCustomJSONRPCHandlerManager.FindHandlerDefByName(Const AClassName,
  AMethodName: TJSONStringType): TJSONRPCHandlerDef;

Var
  I : integer;

begin
  I:=IndexOfHandlerDef(AClassName,AMethodName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetHandlerDef(I);
end;

Function TCustomJSONRPCHandlerManager.GetHandlerDefByName(Const AClassName,
  AMethodName: TJSONStringType): TJSONRPCHandlerDef;
begin
  Result:=FindHandlerDefByName(AClassName,AMethodName);
  If (Result=Nil) then
    If (AClassName<>'') then
      JSONRPCError(SErrUnknownJSONRPCClassMethodHandler,[AClassName,AMethodName])
    else
      JSONRPCError(SErrUnknownJSONRPCMethodHandler,[AMethodName]);
end;

Function TCustomJSONRPCHandlerManager.GetHandler(
  Const ADef: TJSONRPCHandlerDef; AOwner: TComponent; Out AContainer: TComponent
  ): TCustomJSONRPCHandler;

Var
  N : String;
  O : TComponent;

begin
  If AOwner=Nil then
    O:=Self
  else
    O:=AOwner;
  Result:=ADef.CreateInstance(Nil,AContainer);
  N:=aContainer.Name;
  if N='' then
    N:=aContainer.ClassName;
{$IFDEF CPU64}
  N:=N+IntToStr(InterlockedIncrement64(FHandlerCount));
{$ELSE}
  N:=N+IntToStr(InterlockedIncrement(FHandlerCount));
{$ENDIF}
  aContainer.Name:=N;
  O.InsertComponent(aContainer);
end;

Function TCustomJSONRPCHandlerManager.GetHandler(Const AClassName,
  AMethodName: TJSONStringType; AOwner: TComponent; Out AContainer: TComponent
  ): TCustomJSONRPCHandler;

Var
  D : TJSONRPCHandlerDef;

begin
  D:=GetHandlerDefByname(AClassName,AMEthodName);
  Result:=GetHandler(D,AOwner,AContainer);
end;

Procedure TCustomJSONRPCHandlerManager.GetClassNames(List: TStrings);

Var
  D : TJSONRPCHandlerDef;
  I : Integer;

begin
  For I:=0 to HandlerCount-1 do
    begin
    D:=HandlerDefs[i];
    If List.IndexOf(D.HandlerClassName)=-1 then
      List.Add(D.HandlerClassName);
    end;
end;

Procedure TCustomJSONRPCHandlerManager.GetMethodsOfClass(
  Const AClassName: TJSONStringType; List: TStrings);
Var
  D : TJSONRPCHandlerDef;
  I : Integer;

begin
  For I:=0 to HandlerCount-1 do
    begin
    D:=HandlerDefs[i];
    If AClassName=D.HandlerClassName then
      List.Add(D.HandlerMethodName);
    end;
end;

Procedure TCustomJSONRPCHandlerManager.Clear;
begin
  DoClear;
end;

{ TJSONRPCHandlerManager }

procedure TJSONRPCHandlerManager.DoClear;
begin
  FHandlerDefs.Clear;
end;

Function TJSONRPCHandlerManager.CreateDefs: TJSONRPCHandlerDefs;
begin
  Result:=TJSONRPCHandlerDefs.Create(DefaultJSONRPCHandlerDefClass);
end;

Procedure TJSONRPCHandlerManager.RemoveHandlerDef(Const Index: Integer);
begin
  FHandlerDefs.Delete(Index);
end;

function TJSONRPCHandlerManager.AddHandlerDef(Const AClassName,
  AMethodName: TJSONStringType): TJSONRPCHandlerDef;
begin
  Result:=FHandlerDefs.AddHandler(AClassName,AMethodName);
end;

function TJSONRPCHandlerManager.IndexOfHandlerDef(Const AClassName,
  AMethodName: TJSONStringType): Integer;
begin
  Result:=FHandlerDefs.IndexOfHandler(AClassName,AMethodName);
end;

function TJSONRPCHandlerManager.GetHandlerDef(Index: Integer
  ): TJSONRPCHandlerDef;
begin
  Result:=FHandlerDefs[Index];
end;

function TJSONRPCHandlerManager.GetHandlerDefCount: Integer;
begin
  Result:=FHandlerDefs.Count;
end;

Constructor TJSONRPCHandlerManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandlerDefs:=CreateDefs;
end;

Destructor TJSONRPCHandlerManager.Destroy;
begin
  FreeAndNil(FHandlerDefs);
  inherited Destroy;
end;


Initialization
Finalization
  FreeAndNil(TheHandler);
end.

