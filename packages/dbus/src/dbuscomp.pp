{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2010 by Michael Van Canneyt, member of the
   Free Pascal development team

   DBUS component layer on top of the DBUS library.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
unit dbuscomp;

{$mode objfpc}
{$h+}
interface

uses
  Classes, SysUtils, ctypes,dbus;

Type

  { TDBusMessage }
  TBooleanArray = Array of boolean;
  TByteArray = Array of Byte;
  TSmallIntArray = Array of SmallInt;
  TWordArray = Array of Word;
  TIntegerArray = Array of Integer;
  TCardinalArray = Array of Cardinal;
  TInt64Array = Array of Int64;
  TQWordArray = Array of QWord;
  TDoubleArray = Array of Double;
  TStringArray = Array of String;

  TDBUSDictionary = Class;
  { TDBusMessageIterator }

  TDBusMessageIterator = Class(TObject)
  private
    FIter : DBUSMessageIter;
  protected
    Procedure Error(Const Msg : String);
    Procedure Error(Const Fmt : String; Args : Array of Const);
  Public
    Constructor Create(AIter : DBUSMessageIter);
    Function GetFixedArray(Const AElementType : cint; Var P : Pointer) : cInt;
    Function GetArgumentType: cint;
    Function GetElementType: cint;
    Function Recurse: TDBusMessageIterator;
    Function HasNext : Boolean;
    Procedure Next;
    Procedure Get(AType : cInt; Var Value);
    Procedure GetArgument(Var Arg : Byte);
    Procedure GetArgument(Var Arg : Boolean);
    Procedure GetArgument(Var Arg : SmallInt);
    Procedure GetArgument(Var Arg : Word);
    Procedure GetArgument(Var Arg : Integer);
    Procedure GetArgument(Var Arg : Cardinal);
    Procedure GetArgument(var Arg : Int64);
    Procedure GetArgument(Var Arg : QWord);
    Procedure GetArgument(var Arg : Double);
    Procedure GetArgument(var Arg : String);
    Procedure GetArgument(Var Arg : TByteArray);
    Procedure GetArgument(Var Arg : TBooleanArray);
    Procedure GetArgument(Var Arg : TSmallIntArray);
    Procedure GetArgument(Var Arg : TWordArray);
    Procedure GetArgument(Var Arg : TIntegerArray);
    Procedure GetArgument(Var Arg : TCardinalArray);
    Procedure GetArgument(var Arg : TInt64Array);
    Procedure GetArgument(Var Arg : TQWordArray);
    Procedure GetArgument(var Arg : TDoubleArray);
    Procedure GetArgument(var Arg : TStringArray);
    Procedure GetArgument(var Arg : TStringList);
    Procedure GetArgument(Var Arg : Variant);
    Procedure GetArgument(Const  Arg : TDBUSDictionary);
    Procedure Append(AType : cInt; Const Value);
    Procedure AppendArgument(Const Arg : Byte);
    Procedure AppendArgument(Const Arg : Boolean);
    Procedure AppendArgument(Const Arg : SmallInt);
    Procedure AppendArgument(Const Arg : Word);
    Procedure AppendArgument(Const Arg : Integer);
    Procedure AppendArgument(Const Arg : Cardinal);
    Procedure AppendArgument(Const Arg : Int64);
    Procedure AppendArgument(Const Arg : QWord);
    Procedure AppendArgument(Const Arg : Double);
    Procedure AppendArgument(Const Arg : String);
  end;

  TDBUSDictItem = Class(TCollectionItem)
  Protected
    Procedure Load(I : TDBUSMessageIterator); virtual; abstract;
    Procedure Save(I : TDBUSMessageIterator); virtual; abstract;
  end;

  { TDBUSDictionary }

  TDBUSDictionary = Class(TCollection)
  Protected
    Function AddDictItem : TDBUSDictItem;
  end;

  TDBusMessage = Class(TObject)
  private
    FFromSource: Boolean;
    FMessage: PDBUSMessage;
    FAppendCount : Integer;
    FAppendIterator : TDBUSMessageIterator;
    FGetCount : Integer;
    FGetIterator : TDBUSMessageIterator;
    procedure BeginAppend;
    procedure BeginGet;
    function BeginGetFixedArray(const AElementType: cint; var P: Pointer): cInt;
    procedure EndAppend;
    procedure EndGet;
    function GetMessage: PDBUSMessage;
    function GetReplySerial: dbus_uint32_t;
    function GetSerial: dbus_uint32_t;
    procedure SetReplySerial(const AValue: dbus_uint32_t);
  Protected
    Class function MessageType : cint; virtual; abstract;
    Procedure AllocateMessage; virtual; abstract;
    Procedure CheckNotFromSource;
    Procedure CheckNotAllocated;
    Procedure CheckAllocated;
    Function Allocated : boolean;
    Function Copy : TDBUSMessage;
    Procedure Error(Const Msg : String);
    Procedure Error(Const Fmt : String; Args : Array of Const);
    Property Message : PDBUSMessage Read GetMessage;
    Property FromSource : Boolean Read FFromSource;
    Property Serial : dbus_uint32_t Read GetSerial;
    Property ReplySerial : dbus_uint32_t Read GetReplySerial Write SetReplySerial;
  Public
    Constructor Create(ASource : PDBusMessage); virtual;
    Destructor Destroy; override;
    Procedure Append(AType : cInt; Const Value);
    Procedure AppendArgument(Const Arg : Byte);
    Procedure AppendArgument(Const Arg : Boolean);
    Procedure AppendArgument(Const Arg : SmallInt);
    Procedure AppendArgument(Const Arg : Word);
    Procedure AppendArgument(Const Arg : Integer);
    Procedure AppendArgument(Const Arg : Cardinal);
    Procedure AppendArgument(Const Arg : Int64);
    Procedure AppendArgument(Const Arg : QWord);
    Procedure AppendArgument(Const Arg : Double);
    Procedure AppendArgument(Const Arg : String);
    Procedure Get(AType : cInt; Var Value);
    Procedure GetArgument(Var Arg : Byte);
    Procedure GetArgument(Var Arg : Boolean);
    Procedure GetArgument(Var Arg : SmallInt);
    Procedure GetArgument(Var Arg : Word);
    Procedure GetArgument(Var Arg : Integer);
    Procedure GetArgument(Var Arg : Cardinal);
    Procedure GetArgument(var Arg : Int64);
    Procedure GetArgument(Var Arg : QWord);
    Procedure GetArgument(var Arg : Double);
    Procedure GetArgument(var Arg : String);
    Procedure GetArgument(Var Arg : TByteArray);
    Procedure GetArgument(Var Arg : TBooleanArray);
    Procedure GetArgument(Var Arg : TSmallIntArray);
    Procedure GetArgument(Var Arg : TWordArray);
    Procedure GetArgument(Var Arg : TIntegerArray);
    Procedure GetArgument(Var Arg : TCardinalArray);
    Procedure GetArgument(var Arg : TInt64Array);
    Procedure GetArgument(Var Arg : TQWordArray);
    Procedure GetArgument(var Arg : TDoubleArray);
    Procedure GetArgument(var Arg : TStringArray);
    Procedure GetArgument(var Arg : TStringList);
    Procedure GetArgument(Var Arg : Variant);
    Procedure GetArgument(Const Arg : TDBUSDictionary);
    Function GetNextArgumentType : cInt;
    Function GetArrayElementType : cInt;
    Function HasPath(Const APath : String) : boolean; virtual;
    Function HasSender(Const ASender : String) : boolean; virtual;
    Function HasSignature(Const ASignature : String) : boolean; virtual;
    Function IsError(Const AError : string) : Boolean; virtual;
  end;

  TDBUSGUID = Array[1..32] of Byte;

  { TDBUSInvalidMessage }

  TDBUSInvalidMessage = Class(TDBusMessage)
    Class function MessageType : cint; override;
    Procedure AllocateMessage; override;
  end;
  TDBUSInterfaceMessage = Class(TDBusMessage)
  Private
    FInterface: String;
    FPath: String;
    procedure SetInterface(const AValue: String);
    function GetInterface: String;
    procedure SetPath(const AValue: String);
  Public
    Function HasPath(Const APath : String) : boolean; override;
    Property ObjectPath : String Read FPath Write SetPath;
    Property InterfaceName : String Read GetInterface Write SetInterface;
  end;


  { TDBusMethodCallMessage }

  TDBusMethodCallMessage = Class(TDBUSInterfaceMessage)
  private
    FDestination: String;
    FMethod: String;
    procedure SetDestination(const AValue: String);
    procedure SetMethod(const AValue: String);
  Protected
    Class function MessageType : cint; override;
    Procedure AllocateMessage; override;
  Public
    Constructor Create(Const ADestination,AObjectPath,AInterface,AMethod : String); virtual; overload;
    Property Destination : String Read FDestination Write SetDestination;
    Property MethodName : String Read FMethod Write SetMethod;
  end;

  { TDBusSignalMessage }

  TDBusSignalMessage = Class(TDBusInterfaceMessage)
  private
    FName: String;
    procedure SetName(const AValue: String);
  Protected
    Class function MessageType : cint; override;
    Procedure AllocateMessage; override;
  Public
    Constructor Create(Const AObjectPath,AInterface,AName : String); virtual; overload;
    Property Name : String Read FName Write SetName;
  end;

  { TDBusReplyToMessage }
  TDBusReplyToMessage = Class(TDBUSMessage)
  Private
    FReplyTo: TDBUSMessage;
    procedure SetReplyto(const AValue: TDBUSMessage);
  Public
    Constructor Create(Const AReplyTo : TDBUSMessage); overload;
    Property ReplyTo : TDBUSMessage Read FReplyTo Write SetReplyto;
  end;
  { TDBusErrorMessage }

  TDBusErrorMessage = Class(TDBusReplyToMessage)
  private
    FErrorMessage: String;
    FErrorName: String;
    FName: String;
    procedure SetErrorMessage(const AValue: String);
    procedure SetErrorName(const AValue: String);
  Protected
    Class function MessageType : cint; override;
    Procedure AllocateMessage; override;
  Public
    Constructor Create(Const AReplyTo : TDBUSMessage; Const AErrorName,AErrorMessage : String); overload;
    Constructor Create(Const AReplyTo : TDBUSMessage; Const AErrorName,AFormat : String; Args : Array of const); overload;
    Property ErrorName : String Read FErrorName Write SetErrorName;
    Property ErrorMessage : String Read FErrorMessage Write SetErrorMessage;
  end;

  { TDBusMethodReturnMessage }

  TDBusMethodReturnMessage = Class(TDBusReplyToMessage)
  Protected
    Class function MessageType : cint; override;
    Procedure AllocateMessage; override;
  end;

  { TDBusPendingCall }

  TDBusPendingCall = Class(TObject)
  Protected
    FSource : PDBusPendingCall;
  Public
    Constructor Create(Const ASource : PDBusPendingCall);
    Destructor Destroy; override;
  end;

  TCustomDBUSConnection = Class;
  TDBUSFilterItem = Class;
  TDBusMessageHandler = Procedure (Sender : TObject; Msg : TDBUSMessage; Var AResult : DBusHandlerResult) of Object;

  { TDBUSMessageItem }
  TDBUSMessageItem = Class(TCollectionItem)
  private
    FRegistered: Boolean;
    FENabled: Boolean;
    FOnMessage: TDBusMessageHandler;
    function HaveHandle: Boolean;
  Private
    procedure SetEnabled(const AValue: Boolean);
    procedure SetOnMessage(const AValue: TDBusMessageHandler);
  Protected
    procedure MaybeRegister;
    Function ConnHandle : PDBUSConnection;
    function AllowRegister : Boolean; virtual;
    Procedure Register; virtual; abstract;
    Procedure Unregister; virtual; abstract;
  Public
    Procedure Assign(Source : TPersistent); override;
    Property Registered : Boolean Read FRegistered;
  Published
    Property Enabled : Boolean Read FENabled Write SetEnabled;
    Property OnMessage : TDBusMessageHandler Read FOnMessage Write SetOnMessage;
  end;
  TDBUSMessageItemClass = Class of TDBUSMessageItem;

  { TDBUSMessages }

  TDBUSMessages = Class(TCollection)
  private
    FConnection: TCustomDBUSConnection;
  Public
    Constructor Create(Const AConnection : TCustomDBUSConnection; AItemClass : TDBUSMessageItemClass);
    Property Connection : TCustomDBUSConnection Read FConnection;
  end;

  { TDBUSFilterItem }

  TDBUSFilterItem = Class(TDbusMessageItem)
  Public
    Procedure Register; override;
    Procedure Unregister; override;
  end;
  TDBUSFilterItemClass = Class of TDBUSFilterItem;

  { TDBUSFilters }

  TDBUSFilters = Class(TDBUSMessages)
  Private
    function GetF(AIndex : Integer): TDBUSFilterItem;
    procedure SetF(AIndex : Integer; const AValue: TDBUSFilterItem);
  Public
    Constructor Create(Const AConnection : TCustomDBUSConnection);
    Property Filters[AIndex : Integer] : TDBUSFilterItem Read GetF Write SetF; default;
  end;

  { TDBUSObjectItem }

  TDBUSObjectItem = Class(TDbusMessageItem)
  private
    FFallBack: Boolean;
    FPath: String;
    procedure SetFallback(const AValue: Boolean);
    procedure SetPath(const AValue: String);
  Public
    Procedure Register; override;
    Procedure Unregister; override;
    function AllowRegister : Boolean; override;
  Published
    Property Path : String Read FPath Write SetPath;
    Property FallBack : Boolean Read FFallBack Write SetFallback;
  end;
  TDBUSObjectItemClass = Class of TDBUSObjectItem;

  { TDBUSObjectItem }

  { TDBUSObjects }

  TDBUSObjects = Class(TDBUSMessages)
  Private
    function GetO(AIndex : Integer): TDBUSObjectItem;
    procedure SetO(AIndex : Integer; const AValue: TDBUSObjectItem);
  Public
    Constructor Create(Const AConnection : TCustomDBUSConnection);
    Property Objects[AIndex : Integer] : TDBUSObjectItem Read GetO Write SetO; default;
  end;

  { TCustomDBUSConnection }
  TConnectionKind = (ckCustom,ckSystem,ckSession,ckStarter);
  TCustomDBUSConnection = Class(TComponent)
  private
    FConn : PDBusConnection;
    FErr : DBusError;
    FFilters: TDBUSFilters;
    FKind: TConnectionKind;
    FLoadConnected : Boolean;
    FMaxReceivedSize : clong;
    FMaxMessageSize: clong;
    FObjects: TDBUSObjects;
    FPath: String;
    FShared: Boolean;
    function GetAnonymous: boolean;
    function GetAuthenticated: boolean;
    function GetConnected: Boolean;
    function GetDispatchStatus: DBusDispatchStatus;
    function GetMaxMessageSize: clong;
    function GetMaxReceivedSize: clong;
    function GetOutgoingSize: clong;
    function GetServerID: String;
    procedure SetConnected(const AValue: Boolean);
    procedure SetFilters(const AValue: TDBUSFilters);
    procedure SetKind(const AValue: TConnectionKind);
    procedure SetMaxMessageSize(const AValue: clong);
    procedure SetMaxReceivedSize(const AValue: clong);
    procedure SetObjects(const AValue: TDBUSObjects);
    procedure SetPath(const AValue: String);
    procedure SetShared(const AValue: Boolean);
  Protected
    Procedure CheckError;
    procedure CheckDisconnected;
    procedure CheckConnected;
    procedure Loaded; override;
    Function FilterClass : TDBUSFilterItemClass;
    Function ObjectClass : TDBUSObjectItemClass;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Error(Const Msg : String);
    Procedure ResetError;
    procedure Connect;
    procedure Disconnect;
    procedure RegisterFilters;
    procedure UnRegisterFilters;
    procedure RegisterObjects;
    procedure UnRegisterObjects;
    Function PreAllocateSend : PDBusPreallocatedSend;
    Procedure FreePreAllocatedSend(Allocated : PDBusPreallocatedSend);
    Procedure SendPreallocated(Allocated : PDBusPreallocatedSend; AMessage : TDBusMessage; out ASerial : dbus_uint32_t);
    Procedure SendPreallocated(Allocated : PDBusPreallocatedSend; AMessage : TDBusMessage);
    function Send(AMessage : TDBusMessage; out ASerial : dbus_uint32_t) : boolean;
    function Send(AMessage : TDBusMessage) : boolean;
    Function SendWithReply(AMessage : TDBusMessage; Out PendingCall : TDBUSPendingCall; Const TimeOut : cInt) : boolean;
    Function SendWithReplyAndBlock(AMessage : TDBusMessage; Const TimeOut : cInt) : TDBusMessage;
    Procedure Flush;
    Function ReadWriteDispatch(Const ATimeOut : cInt): Boolean;
    Function ReadWrite(Const ATimeOut : cInt): Boolean;
    Function BorrowMessage : TDBUSMessage;
    Function GetUnixFileDescriptor(Var fd : cInt) : Boolean;
    Function GetUnixProcessID(Var ID : CUlong) : Boolean;
    Function GetUnixUser(Var UID : CUlong) : Boolean;
    Function GetWindowsUser(Var SID : String) : Boolean;
    Function GetSocket(Var SD : cint) : Boolean;
    Function GetObjectPathData(Const Path : String; DoCheck : Boolean = False) : TDBUSObjectItem;
    Procedure SetAllowAnonymous(AValue : Boolean);
    Procedure SetRoutePeerMessages(AValue : Boolean);
    Procedure ReturnMessage(var AMessage : TDBUSMessage);
    Procedure StealBorrowedMessage(var AMessage : TDBUSMessage);
    Procedure ListRegistered(Const APath : String; AList : TStrings);
    Class Function AllocateDataSlot(Var slot : dbus_int32_t) : Boolean;
    Class procedure FreeDataSlot(Var slot : dbus_int32_t);
    Function SetData(Const Slot: dbus_int32_t; Const Data : Pointer; Const FreeFunction : DBUSFreeFunction) : Boolean;
    Function GetData(Const Slot: dbus_int32_t) : Pointer;
    Function PopMessage : TDBUSMessage;
    Function Dispatch : DBusDispatchStatus;
    Property DispatchStatus : DBusDispatchStatus Read GetDispatchStatus;
  Protected
    Property Connected : Boolean Read GetConnected Write SetConnected;
    Property Kind : TConnectionKind Read FKind Write SetKind;
    Property Shared : Boolean read FShared Write SetShared default true;
    Property Path : String Read FPath Write SetPath;
    Property MaxMessageSize : clong Read GetMaxMessageSize Write SetMaxMessageSize;
    Property MaxReceiveSize : clong Read GetMaxReceivedSize Write SetMaxReceivedSize;
    Property OutgoingSize : clong Read GetOutgoingSize;
    Property Authenticated : boolean Read GetAuthenticated;
    Property Anonymous : boolean Read GetAnonymous;
    Property ServerID : String Read GetServerID;
    Property Filters : TDBUSFilters Read FFilters Write SetFilters;
    Property Objects : TDBUSObjects Read FObjects Write SetObjects;
  end;

  TDBUSConnection = Class(TCustomDBUSConnection)
  Public
    Property OutgoingSize;
    Property Authenticated;
    Property Anonymous;
    Property ServerID;
  Published
    Property Connected;
    Property Kind;
    Property Shared;
    Property Path;
    Property MaxMessageSize;
    Property MaxReceiveSize;
    Property Filters;
    Property Objects;
  end;
  { EDBus }

  EDBus = Class(Exception)
  private
    FName: String;
  Public
    Property Name : String Read FName;
  end;

Function CreateMessageFromSource(M : PDbusMessage) : TDBusMessage;
Procedure RaiseDBUSError(Const AName,AMsg : String);
Procedure RaiseDBUSError(Const AName,Fmt : String; Args : Array of const);

implementation

resourcestring
   SErrInvalidOperationWhileConnected = 'Cannot perform this operation when connected to the bus.';
   SErrInvalidOperationWhileDisconnected = 'Cannot perform this operation when disconnected from the bus.';
   SErrNoDBUSPath = 'No DBUS Address to connect to.';
   SErrEmptymessage = 'Source message is Nil';
   SerrWrongMessageType = 'Wrong message type. Expected %d, got %d';
   SErrUnknownMessageType = 'Unknown message type: %d';
   SErrInvalidMessageType = 'Cannot create unknown message type';
   SErrInvalidOperationFromSource = 'This operation cannot be performed on a message coming from the DBUS';
   SErrInvalidOperationWhenAllocated = 'This operation cannot be performed when a message was allocated';
   SErrInvalidOperationWhenNotAllocated = 'This operation cannot be performed when the is not allocated';
   SErrEmptyPath = 'Cannot allocate method call message: no path specified';
   SErrEmptyMethod = 'Cannot allocate method call message: no method specified';
   SErrEmptyName = 'Cannot allocate signal message: no signal name specified';
   SErrNoErrorName = 'Cannot allocate error message: no error name specified';
   SErrNoReplyTo = 'Cannot allocate error message: no reply to message specified';
   SErrObjectWithoutPath = 'Cannot (un)register an object without path';
   SErrCouldNotSetReplySerial = 'Could not set reply serial';
   SErrInitIter = 'Could not initialize iterator';
   SErrAppendFailed = 'Append of argument to message failed';
   SErrNoMoreArguments = 'No more arguments available';
   SErrInvalidArgumentType = 'Invalid argument type. Expected %s, got %s.';
   SErrInvalidArrayElementType = 'Invalid array element type. Expected %s got %s';
   SErrInvalidVariantType = 'Invalid VARIANT type';

Function CreateMessageFromSource(M : PDbusMessage) : TDBusMessage;

begin
  If M=Nil then
    Result:=Nil
  else
    case dbus_message_get_type(M) of
      DBUS_MESSAGE_TYPE_INVALID : Result:=TDBUSInvalidMessage.Create(M);
      DBUS_MESSAGE_TYPE_METHOD_CALL : Result:=TDBUSMethodCallmessage.Create(M);
      DBUS_MESSAGE_TYPE_METHOD_RETURN : Result:=TDBUSMethodReturnMessage.Create(M);
      DBUS_MESSAGE_TYPE_ERROR : Result:=TDBUSErrorMessage.Create(M);
      DBUS_MESSAGE_TYPE_SIGNAL : Result:=TDBUSSignalMessage.Create(M);
    else
      Raise EDBUS.CreateFmt(SErrUnknownMessageType,[dbus_message_get_type(M)]);
    end
end;

procedure RaiseDBUSError(const AName, AMsg: String);

Var
  E : EDBUS;

begin
  E:=EDBUS.Create(Amsg);
  E.FName:=AName;
  Raise E;
end;

procedure RaiseDBUSError(const AName, Fmt: String; Args: array of const);
begin
  RaiseDBUSError(AName,Format(Fmt,Args));
end;

{ TCustomDBUSConnection }

function TCustomDBUSConnection.GetConnected: Boolean;
begin
  Result:=(FConn<>Nil);
  If Result then
    begin
    result:=dbus_connection_get_is_connected(FConn)<>0;
    If not Result then
      Disconnect;
    end;
end;

function TCustomDBUSConnection.GetAnonymous: boolean;
begin
  CheckConnected;
  Result:=False;
  result:=(0<>dbus_connection_get_is_anonymous(FConn));
end;


function TCustomDBUSConnection.GetAuthenticated: boolean;
begin
  CheckConnected;
  result:=(0<>dbus_connection_get_is_authenticated(FConn));
end;

function TCustomDBUSConnection.GetMaxMessageSize: clong;
begin
  if Connected then
    Result:=dbus_connection_get_max_message_size(FConn)
  else
    Result:=FMaxMessageSize;
end;

function TCustomDBUSConnection.GetMaxReceivedSize: clong;
begin
  if Connected then
    Result:=dbus_connection_get_max_received_size(FConn)
  else
    Result:=FMaxReceivedSize;
end;

function TCustomDBUSConnection.GetOutgoingSize: clong;
begin
  CheckConnected;
  Result:=dbus_connection_get_outgoing_size(fconn);
end;

function TCustomDBUSConnection.GetServerID: String;

Var
  p : pchar;

begin
  CheckConnected;
  p:=nil;
  p:=dbus_connection_get_server_id(Fconn);
  If p<>nil then
    Result:=strpas(p);
end;

procedure TCustomDBUSConnection.SetConnected(const AValue: Boolean);
begin
  If (AValue=GetConnected) then exit;
  If (csLoading in ComponentState) then
    FLoadConnected:=AValue
  else
    If AValue then
      Connect
    else
      DisConnect;
end;

procedure TCustomDBUSConnection.SetFilters(const AValue: TDBUSFilters);
begin
  if (FFilters=AValue) then exit;
  FFilters.Assign(AValue);
end;

procedure TCustomDBUSConnection.SetKind(const AValue: TConnectionKind);
begin
  if (Kind<>AValue) then
    CheckDisconnected;
  FKind:=AValue;
end;

procedure TCustomDBUSConnection.SetMaxMessageSize(const AValue: clong);
begin
  FMaxMessageSize:=AValue;
  If Connected and (AValue<>0) then
    dbus_connection_set_max_message_size(fconn,AValue);
end;

procedure TCustomDBUSConnection.SetMaxReceivedSize(const AValue: clong);
begin
  FMaxMessageSize:=AValue;
  If Connected and (AValue<>0) then
    dbus_connection_set_max_received_size(fconn,AValue);
end;

procedure TCustomDBUSConnection.SetObjects(const AValue: TDBUSObjects);
begin
  if FObjects=AValue then exit;
  FObjects.Assign(AValue);
end;

procedure TCustomDBUSConnection.SetPath(const AValue: String);
begin
  if FPath=AValue then exit;
  CheckDisconnected;
  FPath:=AValue;
end;

procedure TCustomDBUSConnection.SetShared(const AValue: Boolean);
begin
  if (AValue=FShared) then exit;
  CheckDisconnected;
  FShared:=AValue;
end;

procedure TCustomDBUSConnection.CheckError;

Var
  E : EDBUS;

begin
  If (dbus_error_is_set(@FErr)<>0) then
    begin
    E:=EDBUS.Create(strpas(FErr.Message));
    E.FName:=StrPas(FErr.Name);
    ResetError;
    Raise E;
    end;
end;

procedure TCustomDBUSConnection.CheckDisconnected;

begin
  If Connected then
    Error(SErrInvalidOperationWhileConnected);
end;

procedure TCustomDBUSConnection.CheckConnected;
begin
  If not Connected then
    Error(SErrInvalidOperationWhileDisconnected);
end;

procedure TCustomDBUSConnection.Loaded;
begin
  If FLoadConnected then
    Connect;
end;

function TCustomDBUSConnection.FilterClass: TDBUSFilterItemClass;
begin
  Result:=TDBUSFilterItem;
end;

function TCustomDBUSConnection.ObjectClass: TDBUSObjectItemClass;
begin
  Result:=TDBUSObjectItem;
end;

constructor TCustomDBUSConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  dbus_error_init(@FErr);
  FShared:=True;
  FFilters:=TDBUSFilters.Create(Self);
  FObjects:=TDBUSObjects.Create(Self);
end;

destructor TCustomDBUSConnection.Destroy;
begin
  Disconnect;
  FreeAndNil(FFilters);
  FreeAndNil(FObjects);
  inherited Destroy;
end;

procedure TCustomDBUSConnection.Error(Const Msg: String);

Var
  E : EDBUS;

begin
  E:=EDBUS.Create(Msg);
  E.FName:=Self.Name;
  Raise E;
end;

procedure TCustomDBUSConnection.ResetError;
begin
  dbus_error_free(@FErr);
end;

procedure TCustomDBUSConnection.Connect;

Const
  CFlags : Array[TConnectionKind] of DBUSBusType
         = (DBUS_BUS_SYSTEM,DBUS_BUS_SYSTEM,DBUS_BUS_SESSION,DBUS_BUS_STARTER);

begin
  if GetConnected then exit;
  case kind of
    ckCustom :
      begin
      If (FPath='') then
        Error(SErrNoDBUSPath);
      if Shared then
        fconn:=dbus_connection_open(pchar(FPath),@Ferr)
      else
        fconn:=dbus_connection_open_private(pchar(FPath),@Ferr);
      CheckError;
      end;
    ckSystem,
    ckSession,
    ckStarter :
      begin
      If Shared then
        FConn:=dbus_bus_get(CFlags[kind],@Ferr)
      else
        FConn:=dbus_bus_get_private(CFlags[kind],@Ferr);
      CheckError;
      if Shared then
        dbus_connection_set_exit_on_disconnect(FConn,Ord(False));
      end;
  end;
  If (FMaxMessageSize<>0) then
    dbus_connection_set_max_message_size(fconn,FMaxMessageSize);
  RegisterFilters;
end;

procedure TCustomDBUSConnection.Disconnect;
begin
  UnregisterFilters;
  UnregisterObjects;
  if Not Shared then
    dbus_connection_close(FConn)
  else
    dbus_connection_unref(FConn);
  FConn:=Nil;
end;

procedure TCustomDBUSConnection.RegisterFilters;

Var
  i : integer;

begin
  For I:=0 to FFilters.Count-1 do
    FFilters[i].MaybeRegister;
end;

procedure TCustomDBUSConnection.UnRegisterFilters;
Var
  i : integer;
begin
  For I:=0 to FFilters.Count-1 do
    If FFilters[i].Registered then
      FFilters[i].UnRegister;
end;

procedure TCustomDBUSConnection.RegisterObjects;

Var
  i : integer;

begin
  For I:=0 to FObjects.Count-1 do
    FObjects[i].MaybeRegister;
end;

procedure TCustomDBUSConnection.UnRegisterObjects;
Var
  i : integer;
begin
  For I:=0 to FObjects.Count-1 do
    If FObjects[i].Registered then
      FObjects[i].UnRegister;
end;

function TCustomDBUSConnection.PreallocateSend: PDBusPreallocatedSend;
begin
  CheckConnected;
  Result:=dbus_connection_preallocate_send(FConn);
end;

procedure TCustomDBUSConnection.FreePreAllocatedSend(
  Allocated: PDBusPreallocatedSend);
begin
  CheckConnected;
  dbus_connection_free_preallocated_send(FConn,Allocated);
end;

procedure TCustomDBUSConnection.SendPreallocated(
  Allocated: PDBusPreallocatedSend; AMessage: TDBusMessage;
  out ASerial: dbus_uint32_t);
begin
  CheckConnected;
  dbus_connection_send_preallocated(FConn,Allocated,AMessage.Message,@ASerial);
end;

procedure TCustomDBUSConnection.SendPreallocated(
  Allocated: PDBusPreallocatedSend; AMessage: TDBusMessage);

Var
  s : dbus_uint32_t;

begin
  SendPreallocated(Allocated,AMessage,S);
end;

function TCustomDBUSConnection.Send(AMessage: TDBusMessage; out
  ASerial: dbus_uint32_t): boolean;
begin
  CheckConnected;
  Result:=dbus_connection_send(FConn,AMessage.Message,@ASerial)<>0;
end;

function TCustomDBUSConnection.Send(AMessage: TDBusMessage): boolean;
Var
  s : dbus_uint32_t;

begin
  Result:=Send(AMessage,S);
end;

function TCustomDBUSConnection.SendWithReply(AMessage: TDBusMessage; out
  PendingCall: TDBUSPendingCall; const TimeOut: cInt): boolean;

Var
  P : PDBusPendingCall;

begin
  CheckConnected;
  PendingCall:=Nil;
  Result:=dbus_connection_send_with_reply(FConn,AMessage.Message,@P,TimeOut)<>0;
  if Result then
    if (P<>Nil) then
      PendingCall:=TDBUSPendingCall.Create(P);
end;

function TCustomDBUSConnection.SendWithReplyAndBlock(AMessage: TDBusMessage;
  const TimeOut: cInt): TDBusMessage;

Var
  M : PDBusMessage;

begin
  CheckConnected;
  M:=dbus_connection_send_with_reply_and_block(FConn,AMessage.Message,TimeOut,@FErr);
  CheckError;
  Result:=CreateMessageFromSource(M);
end;

procedure TCustomDBUSConnection.Flush;
begin
  CheckConnected;
  dbus_connection_flush(FConn);
end;

function TCustomDBUSConnection.ReadWriteDispatch(Const ATimeOut : cInt): Boolean;
begin
  CheckConnected;
  Result:=dbus_connection_read_write_dispatch(FConn,ATimeOut)<>0;
end;

function TCustomDBUSConnection.ReadWrite(const ATimeOut: cInt): Boolean;
begin
  CheckConnected;
  Result:=dbus_connection_read_write(FConn,ATimeOut)<>0;
end;

function TCustomDBUSConnection.BorrowMessage: TDBUSMessage;
begin
  CheckConnected;
  Result:=CreateMessageFromSource(dbus_connection_borrow_message(FConn));
end;

function TCustomDBUSConnection.GetUnixFileDescriptor(Var fd: cInt) : Boolean;
begin
  CheckConnected;
  Result:=dbus_connection_get_unix_fd(FConn,@fd)<>0;
end;

function TCustomDBUSConnection.GetUnixProcessID(var ID: CUlong): Boolean;
begin
  checkconnected;
  Result:=dbus_connection_get_unix_process_id(FConn,@ID)<>0;
end;

function TCustomDBUSConnection.GetUnixUser(var UID: CUlong): Boolean;
begin
  checkconnected;
  Result:=dbus_connection_get_unix_user(FConn,@UID)<>0;
end;

function TCustomDBUSConnection.GetWindowsUser(var SID: String): Boolean;

Var
  P : PChar;

begin
  checkconnected;
  Result:=dbus_connection_get_windows_user(FConn,@P)<>0;
  If Result and (P<>Nil) then
    SID:=StrPas(P)
  else
    Sid:='';
end;

function TCustomDBUSConnection.GetSocket(var SD: cint): Boolean;
begin
  checkconnected;
  Result:=dbus_connection_get_socket(FConn,@SD)<>0;
end;

function TCustomDBUSConnection.GetObjectPathData(const Path : String; DoCheck : Boolean = False): TDBUSObjectItem;

Var
  P : Pointer;
  I : integer;

begin
  CheckConnected;
  dbus_connection_get_object_path_data(FConn,Pchar(Path),@P);
  Result:=Nil;
  If (P<>Nil) then
    if DoCheck then
      begin
      I:=FObjects.Count-1;
      While (Result=Nil) and (I>=0) do
        begin
        If (Pointer(FObjects[i])=P) then
          Result:=TDBUSObjectItem(P);
        Dec(I);
        end;
      end
    else
      Result:=TDBUSObjectItem(P);
end;

procedure TCustomDBUSConnection.SetAllowAnonymous(AValue: Boolean);
begin
  CheckConnected;
  dbus_connection_set_allow_anonymous(FConn,Ord(AValue));
end;

procedure TCustomDBUSConnection.SetRoutePeerMessages(AValue: Boolean);
begin
  CheckConnected;
  dbus_connection_set_route_peer_messages(FConn,Ord(AValue));
end;

procedure TCustomDBUSConnection.ReturnMessage(var AMessage: TDBUSMessage);
begin
  CheckConnected;
  dbus_connection_return_message(FConn,AMessage.Message);
  AMessage.FMessage:=Nil;
  FreeAndNil(AMessage);
end;

procedure TCustomDBUSConnection.StealBorrowedMessage(var AMessage: TDBUSMessage
  );
begin
  CheckConnected;
  dbus_connection_steal_borrowed_message(FConn,AMessage.Message);
end;

procedure TCustomDBUSConnection.ListRegistered(const APath: String;
  AList: TStrings);

Var
  P : PPchar;

begin
  CheckConnected;
  AList.Clear;
  if (dbus_connection_list_registered(FConn,PChar(APath),@P)<>0) then
    If (P<>Nil) then
      begin
      While (P^<>Nil) do
        begin
        AList.Add(StrPas(P^));
        Inc(P);
        end;
      dbus_free_string_array(P);
      end;
end;

class function TCustomDBUSConnection.AllocateDataSlot(var slot: dbus_int32_t
  ): Boolean;
begin
  Result:=dbus_connection_allocate_data_slot(@slot)<>0;
end;

class procedure TCustomDBUSConnection.FreeDataSlot(var slot: dbus_int32_t);
begin
  dbus_connection_free_data_slot(@slot);
end;

function TCustomDBUSConnection.SetData(const Slot: dbus_int32_t;
  const Data: Pointer; const FreeFunction: DBUSFreeFunction): Boolean;
begin
  CheckConnected;
  Result:=0<>dbus_connection_set_Data(FConn,Slot,Data,FreeFunction);
end;

function TCustomDBUSConnection.GetData(const Slot: dbus_int32_t): Pointer;
begin
  CheckConnected;
  Result:=dbus_connection_get_Data(FConn,Slot);
end;

function TCustomDBUSConnection.PopMessage: TDBUSMessage;
begin
  CheckConnected;
  Result:=CreateMessageFromSource(dbus_connection_pop_message(FConn));
end;

function TCustomDBUSConnection.Dispatch: DBusDispatchStatus;
begin
  CheckConnected;
  Result:=dbus_connection_dispatch(FConn);
end;

function TCustomDBUSConnection.GetDispatchStatus: DBusDispatchStatus;
begin
  CheckConnected;
  Result:=dbus_connection_get_dispatch_status(FConn);
end;

{ TDBusMessage }

function TDBusMessage.GetMessage: PDBUSMessage;
begin
  If (FMessage=Nil) then
    AllocateMessage;
  Result:=FMessage;
end;

function TDBusMessage.GetReplySerial: dbus_uint32_t;
begin
  CheckAllocated;
  result:=dbus_message_get_reply_serial(message);
end;

function TDBusMessage.GetSerial: dbus_uint32_t;
begin
  CheckAllocated;
  result:=dbus_message_get_serial(message);
end;

procedure TDBusMessage.SetReplySerial(const AValue: dbus_uint32_t);
begin
  CheckAllocated;
  if dbus_message_set_reply_serial(message,AVAlue)<>0 then
    Raise EDBUS.Create(SErrCouldNotSetReplySerial);
end;

procedure TDBusMessage.CheckNotFromSource;
begin
  If FFromSource then
    Error(SErrInvalidOperationFromSource);
end;

procedure TDBusMessage.CheckNotAllocated;
begin
  If Allocated then
    Error(SErrInvalidOperationWhenAllocated);
end;

procedure TDBusMessage.CheckAllocated;
begin
  If Allocated then
    Error(SErrInvalidOperationWhenNotAllocated);
end;

procedure TDBusMessage.BeginAppend;

Var
  A : DBUSMessageIter;

begin
  If (FAppendCount=0) then
    begin
    dbus_message_iter_init_append(message,@A);
    FAppendIterator:=TDBUSmessageIterator.Create(A);
    end;
  Inc(FAppendCount);
end;

procedure TDBusMessage.Append(AType: cInt; const Value);
begin
  BeginAppend;
  try
    FAppendIterator.Append(Atype,Value);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.EndAppend;
begin
  Dec(FAppendCount);
end;

function TDBusMessage.Allocated: boolean;
begin
  Result:=(FMessage<>Nil);
end;

function TDBusMessage.Copy: TDBUSMessage;
begin
  CheckAllocated;
  Result:=CreateMessageFromSource(dbus_message_copy(message));
end;

procedure TDBusMessage.Error(const Msg: String);
begin
  RaiseDBusError(ClassName,Msg);
end;

procedure TDBusMessage.Error(const Fmt: String; Args: array of const);
begin
  RaiseDBUSError(ClassName,Fmt,Args);
end;

constructor TDBusMessage.Create(ASource: PDBusMessage);

Var
  t : cint;

begin
  If (ASource=Nil) then
    Error(SErrEmptymessage);
  t:=dbus_message_get_type(ASource);
  If (t<>MessageType) then
    Error(SerrWrongMessageType,[MessageType,T]);
  FMessage:=ASource;
  FFromSource:=True;
end;

destructor TDBusMessage.Destroy;
begin
  if Allocated then
    dbus_message_unref(FMessage);
  inherited Destroy;
end;

procedure TDBusMessage.AppendArgument(const Arg: Byte);
begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(Const Arg: Boolean);

begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(const Arg: SmallInt);
begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(const Arg: Word);
begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(Const Arg: Integer);
begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(const Arg: Cardinal);
begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(const Arg: Int64);
begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(const Arg: QWord);
begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(const Arg: Double);
begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.AppendArgument(Const Arg: String);

begin
  BeginAppend;
  try
    FAppendIterator.AppendArgument(Arg);
  finally
    EndAppend;
  end;
end;

procedure TDBusMessage.BeginGet;

Var
  AIter : DBUSMessageIter;

begin
  If (FGetCount=0) then
    begin
    if (0=dbus_message_iter_init(message,@AIter)) then
      Error(SErrInitIter);
    FGetIterator:=TDBUSMessageIterator.Create(AIter);
    end;
  Inc(FGetCount);
end;

procedure TDBusMessage.EndGet;
begin
  Dec(FGetCount);
end;

Function TDBusMessage.GetNextArgumentType: cInt;
begin
  BeginGet;
  try
    Result:=FGetIterator.GetArgumentType;
  finally
    EndGet;
  end;
end;

function TDBusMessage.GetArrayElementType: cInt;
begin
  BeginGet;
  try
    Result:=FGetIterator.GetArgumentType;
    If (Result<>DBUS_TYPE_ARRAY) then
      Error(SErrInvalidArgumentType,[Char(DBUS_TYPE_ARRAY),Char(Result)]);
    Result:=FGetIterator.GetElementType;
  finally
    EndGet;
  end;
end;

procedure TDBusMessage.Get(AType: cInt; var Value);
begin
  BeginGet;
  try
    FGetIterator.Get(AType,Value);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: Byte);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: Boolean);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: SmallInt);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: Word);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: Integer);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: Cardinal);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: Int64);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: QWord);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: Double);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: String);

begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

Function TDBusMessage.BeginGetFixedArray(Const AElementType : cint; Var P : Pointer) : cInt;

begin
  BeginGet;
  try
    Result:=FGetIterator.GetFixedArray(AElementType,P);
  finally
    Endget
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TByteArray);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TBooleanArray);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TSmallIntArray);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TWordArray);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TIntegerArray);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TCardinalArray);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TInt64array);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TQWordArray);
begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TDoubleArray);

begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TStringArray);

begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: TStringList);

begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(var Arg: Variant);

begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    Endget;
  end;
end;

procedure TDBusMessage.GetArgument(Const Arg: TDBUSDictionary);

Var
  I : Integer;

begin
  BeginGet;
  try
    FGetIterator.GetArgument(Arg);
  finally
    EndGet;
  end;
end;

function TDBusMessage.HasPath(const APath: String): boolean;
begin
  Result:=Allocated;
  if Result then
    Result:=dbus_message_has_path(Message,PChar(APath))<>0;
end;

function TDBusMessage.HasSender(const ASender: String): boolean;
begin
  Result:=Allocated;
  if Result then
    Result:=dbus_message_has_sender(message,PChar(ASender))<>0;
end;

function TDBusMessage.HasSignature(const ASignature: String): boolean;
begin
  Result:=Allocated;
  If Result then
    Result:=dbus_message_has_signature(Message,Pchar(ASignature))<>0;
end;

function TDBusMessage.IsError(const AError : string): Boolean;
begin
  Result:=Allocated;
  if Result then
    Result:=dbus_message_is_error(message,Pchar(AError))<>0;
end;

{ TDBusMethodCallMessage }

procedure TDBusMethodCallMessage.SetDestination(const AValue: String);
begin
  if FDestination=AValue then exit;
  CheckNotAllocated;
  FDestination:=AValue;
end;

function TDBusInterfaceMessage.GetInterface: String;

Var
  p : pchar;
begin
  If not Allocated then
    Result:=FInterface
  else
    begin
    p:=dbus_message_get_interface(message);
    if (P<>Nil) then
      Result:=strpas(p);
    end;
end;

procedure TDBusInterfaceMessage.SetInterface(const AValue: String);
begin
  if FInterface=AValue then exit;
  CheckNotAllocated;
  FInterface:=AValue;
end;

procedure TDBusMethodCallMessage.SetMethod(const AValue: String);
begin
  if FMethod=AValue then exit;
  CheckNotAllocated;
  FMethod:=AValue;
end;

procedure TDBusInterfaceMessage.SetPath(const AValue: String);
begin
  if FPath=AValue then exit;
  CheckNotAllocated;
  FPath:=AValue;
end;

constructor TDBusMethodCallMessage.Create(const ADestination, AObjectPath,
  AInterface, AMethod: String);
begin
  FDestination:=ADestination;
  FPath:=AObjectPath;
  FInterface:=AInterface;
  FMethod:=AMethod;
end;

class function TDBusMethodCallMessage.MessageType: cint;
begin
  Result:=DBUS_MESSAGE_TYPE_METHOD_CALL;
end;

procedure TDBusMethodCallMessage.AllocateMessage;

Var
  d,i : pchar;

begin
  CheckNotAllocated;
  d:=nil;
  i:=nil;
  if (FDestination<>'') then
    d:=PChar(FDestination);
  if (FInterface<>'') then
    i:=PChar(FInterface);
  if (FPath='') then
    Error(SErrEmptyPath);
  if (FMethod='') then
    Error(SErrEmptyMethod);
  FMessage:=dbus_message_new_method_call(d,pchar(FPath),i,pchar(FMethod));
end;

function TDBusInterfaceMessage.HasPath(const APath: String): boolean;
begin
  If Allocated then
    Result:=Inherited  HasPath(APath)
  else
    Result:=(FPath=APath)
end;

{ TDBusPendingCall }

constructor TDBusPendingCall.Create(const ASource: PDBusPendingCall);
begin
  FSource:=ASource;
end;

destructor TDBusPendingCall.Destroy;
begin
  dbus_pending_call_unref(FSource);
  inherited Destroy;
end;

{ TDBUSMessageItem }

procedure TDBUSMessageItem.SetOnMessage(const AValue: TDBusMessageHandler);
begin
  if FOnMessage=AValue then exit;
  FOnMessage:=AValue;
end;

function TDBUSMessageItem.ConnHandle: PDBUSConnection;
begin
  if HaveHandle then
    Result:=TDBUSFilters(Collection).Connection.FConn
  else
    Result:=Nil
end;

function TDBUSMessageItem.AllowRegister: Boolean;
begin
  Result:=(FEnabled and Assigned(FOnMessage))
end;

procedure TDBUSMessageItem.MaybeRegister;

begin
  If AllowRegister and not Registered then
    Register
  else
    UnRegister;
end;

procedure TDBUSMessageItem.SetEnabled(const AValue: Boolean);
begin
  If AValue=FEnabled then exit;
  FEnabled:=AValue;
  MaybeRegister
end;

Function TDBUSMessageItem.HaveHandle : Boolean;

begin
  Result:=Assigned(Collection)
          and (Collection is TDBUSMessages)
          and (Assigned(TDBUSMessages(Collection).Connection))
          and (TDBUSMessages(Collection).Connection.Connected);
end;

procedure TDBUSMessageItem.Assign(Source: TPersistent);

Var
  F : TDBUSMessageItem;
begin
  if (Source is TDBUSMessageItem) then
    begin
    F:=Source as TDBUSMessageItem;
    OnMessage:=F.OnMessage;
    Enabled:=F.Enabled;
    end;
  inherited Assign(Source);
end;

{ TDBUSFilterItem }

function FilterHandler(connection: PDBusConnection;
                       message_: PDBusMessage;
                       user_data: Pointer): DBusHandlerResult;cdecl;

Var
  F : TDBUSFilterItem;
  M : TDBUSMessage;

begin
  F:=TDBUSFilterItem(user_data);
  If (Connection<>F.ConnHandle) then
    result:=DBUS_HANDLER_RESULT_NOT_YET_HANDLED
  else
    begin
    M:=CreateMessageFromSource(Message_);
    try
      F.OnMessage(F,M,Result);
    except
      result:=DBUS_HANDLER_RESULT_NOT_YET_HANDLED
    end;
    M.Free;
    end;
end;

procedure TDBUSFilterItem.Register;
begin
  if HaveHandle then
    FRegistered:=0<>dbus_connection_add_filter(ConnHandle,
                                               @FilterHandler,Self,Nil);
end;

procedure TDBUSFilterItem.Unregister;
begin
  if HaveHandle then
    dbus_connection_remove_filter(ConnHandle,@FilterHandler,Self);
  FRegistered:=False;
end;


{ TDBUSFilters }

function TDBUSFilters.GetF(AIndex : Integer): TDBUSFilterItem;
begin
  Result:=TDBUSFilterItem(Items[AIndex])
end;

procedure TDBUSFilters.SetF(AIndex : Integer; const AValue: TDBUSFilterItem);
begin
  Items[AIndex]:=AValue;
end;

Constructor TDBUSFilters.Create(Const AConnection: TCustomDBUSConnection);

Var
  C : TDBUSMessageItemClass;

begin
  C:=TDBUSFilterItem;
  If Assigned(AConnection) then
    C:=AConnection.FilterClass;
  Inherited Create(AConnection,C);
end;

{ TDBUSMessages }

constructor TDBUSMessages.Create(const AConnection: TCustomDBUSConnection;
  AItemClass: TDBUSMessageItemClass);
begin
  Inherited Create(AItemClass);
  FConnection:=AConnection;
end;

{ TDBUSObjectItem }

// in fact, the same handler could be used as in filter...

function ObjectHandler(connection: PDBusConnection;
                       message_: PDBusMessage;
                       user_data: Pointer): DBusHandlerResult;cdecl;

Var
  O : TDBUSObjectItem;
  M : TDBUSMessage;

begin
  O:=TDBUSObjectItem(user_data);
  If (Connection<>O.ConnHandle) then
    result:=DBUS_HANDLER_RESULT_NOT_YET_HANDLED
  else
    begin
    M:=CreateMessageFromSource(Message_);
    try
      O.OnMessage(O,M,Result);
    except
      result:=DBUS_HANDLER_RESULT_NOT_YET_HANDLED
    end;
    M.Free;
    end;
end;

var
  ObjectVTable : DBusObjectPathVTable = (
    unregister_function: Nil;
    message_function: @ObjectHandler;
    dbus_internal_pad1:Nil;
    dbus_internal_pad2:Nil;
    dbus_internal_pad3:Nil;
    dbus_internal_pad4:Nil
  );

procedure TDBUSObjectItem.SetPath(const AValue: String);
begin
  If (FPath=AValue) then exit;
  FPath:=AValue;
  If (FPath<>'') then
    MaybeRegister;
end;

procedure TDBUSObjectItem.SetFallback(const AValue: Boolean);
begin
  if FFallBack=AValue then exit;
  FFallBack:=AValue;
  If Registered then
    Unregister;
  MaybeRegister;
end;

procedure TDBUSObjectItem.Register;
begin
  If Path='' then
    Raise Exception.Create(SErrObjectWithoutPath);
  If HaveHandle then
    if FallBack then
      FRegistered:=0<>dbus_connection_register_fallback(ConnHandle,Pchar(Path),@ObjectVTable,Self)
    else
      FRegistered:=0<>dbus_connection_register_object_path(ConnHandle,Pchar(Path),@ObjectVTable,Self);
end;

procedure TDBUSObjectItem.Unregister;
begin
  If Path='' then
    Raise Exception.Create(SErrObjectWithoutPath);
  if HaveHandle then
   FRegistered:=0=dbus_connection_unregister_object_path(ConnHandle,Pchar(Path));
end;

function TDBUSObjectItem.AllowRegister: Boolean;
begin
  Result:=inherited AllowRegister and (Path<>'');
end;

{ TDBUSObjects }

function TDBUSObjects.GetO(AIndex: Integer): TDBUSObjectItem;
begin
  Result:=TDBUSObjectItem(Items[AIndex]);
end;

procedure TDBUSObjects.SetO(AIndex: Integer; const AValue: TDBUSObjectItem);
begin
  Items[AIndex]:=AValue;
end;

constructor TDBUSObjects.Create(const AConnection: TCustomDBUSConnection);
Var
  C : TDBUSMessageItemClass;

begin
  C:=TDBUSObjectItem;
  If Assigned(AConnection) then
    C:=AConnection.ObjectClass;
  Inherited Create(AConnection,C);
end;

{ TDBusSignalMessage }

procedure TDBusSignalMessage.SetName(const AValue: String);
begin
  if FName=AValue then exit;
  CheckNotAllocated;
  FName:=AValue;
end;

class function TDBusSignalMessage.MessageType: cint;
begin
  Result:= DBUS_MESSAGE_TYPE_SIGNAL;
end;

procedure TDBusSignalMessage.AllocateMessage;

Var
  i : pchar;

begin
  CheckNotAllocated;
  i:=nil;
  if (FInterface<>'') then
    i:=PChar(FInterface);
  if (FPath='') then
    Error(SErrEmptyPath);
  if (FName='') then
    Error(SErrEmptyName);
  FMessage:=dbus_message_new_signal(PChar(FPath),I,Pchar(Name));
end;

constructor TDBusSignalMessage.Create(const AObjectPath, AInterface,
  AName: String);
begin
  FPath:=AObjectPath;
  FInterface:=AInterface;
  FName:=AName;
end;

{ TDBusErrorMessage }

procedure TDBusErrorMessage.SetErrorMessage(const AValue: String);
begin
  if FErrorMessage=AValue then exit;
  CheckNotAllocated;
  FErrorMessage:=AValue;
end;

procedure TDBusErrorMessage.SetErrorName(const AValue: String);
begin
  if FErrorName=AValue then exit;
  CheckNotAllocated;
  FErrorName:=AValue;
end;

procedure TDBusReplyToMessage.SetReplyto(const AValue: TDBUSMessage);
begin
  if FReplyTo=AValue then exit;
  CheckNotAllocated;
  FReplyTo:=AValue;
end;

constructor TDBusReplyToMessage.Create(const AReplyTo: TDBUSMessage);
begin
  FReplyTo:=AReplyTo;
end;

class function TDBusErrorMessage.MessageType: cint;
begin
  Result:=DBUS_MESSAGE_TYPE_ERROR;
end;

procedure TDBusErrorMessage.AllocateMessage;

Var
  P : PChar;

begin
  If (ErrorName='') then
    Error(SErrNoErrorName);
  If (ReplyTo=Nil) then
    Error(SErrNoReplyTo);
  P:=Nil;
  If (ErrorMessage<>'') then
    P:=Pchar(ErrorMessage);
  FMessage:=dbus_message_new_error(ReplyTo.Message,Pchar(FErrorName),P);
end;

constructor TDBusErrorMessage.Create(const AReplyTo: TDBUSMessage;
  const AErrorName, AErrorMessage: String);
begin
  Inherited Create(AReplyto);
  FErrorName:=AErrorName;
  FErrorMessage:=AErrorMessage;
end;

constructor TDBusErrorMessage.Create(const AReplyTo: TDBUSMessage;
  const AErrorName, AFormat: String; Args: array of const);
begin
  Inherited Create(AReplyTo);
  FErrorName:=AErrorName;
  FErrorMessage:=Format(AFormat,Args);
end;

{ TDBusMethodReturnMessage }

class function TDBusMethodReturnMessage.MessageType: cint;
begin
  Result:=DBUS_MESSAGE_TYPE_METHOD_RETURN;
end;

procedure TDBusMethodReturnMessage.AllocateMessage;
begin
  If (FReplyTo=Nil) then
    Error(SErrNoReplyTo);
  FMessage:=dbus_message_new_method_return(FReplyTo.Message);
end;

{ TDBUSInvalidMessage }

class function TDBUSInvalidMessage.MessageType: cint;
begin
  Result:=DBUS_MESSAGE_TYPE_INVALID;
end;

procedure TDBUSInvalidMessage.AllocateMessage;
begin
  Error(SErrInvalidMessageType);
end;

{ TDBusMessageIterator }

procedure TDBusMessageIterator.Error(const Msg: String);
begin
  RaiseDBusError(ClassName,Msg);
end;

procedure TDBusMessageIterator.Error(const Fmt: String; Args: array of const);
begin
  RaiseDBUSError(ClassName,Fmt,Args);
end;

constructor TDBusMessageIterator.Create(AIter: DBUSMessageIter);
begin
  FIter:=AIter;
end;

function TDBusMessageIterator.GetFixedArray(const AElementType: cint;
  var P: Pointer): cInt;
Var
  A : cInt;
  AI : DBUSMessageIter;

begin
  A:=dbus_message_iter_get_arg_type(@FIter);
  If (A<>DBUS_TYPE_ARRAY) then
    Error(SErrInvalidArgumentType,[Char(DBUS_TYPE_ARRAY),Char(A)]);
  A:=dbus_message_iter_get_element_type(@FIter);
  If (A<>AElementType) then
    Error(SErrInvalidArrayElementType,[Char(AElementType),Char(A)]);
  dbus_message_iter_recurse(@FIter, @AI);
  dbus_message_iter_get_fixed_array(@AI,@P,@Result);
end;

function TDBusMessageIterator.GetArgumentType: cint;
begin
  Result:=dbus_message_iter_get_arg_type(@FIter);
end;

function TDBusMessageIterator.GetElementType: cint;
begin
  Result:=dbus_message_iter_get_element_type(@FIter);
end;

function TDBusMessageIterator.Recurse: TDBusMessageIterator;

Var
  AI : DBUSMessageIter;

begin
  dbus_message_iter_recurse(@Fiter, @AI);
  Result:=TDBusMessageIterator.Create(AI);
end;

function TDBusMessageIterator.HasNext: Boolean;
begin
  Result:=dbus_message_iter_has_next(@Fiter)<>0;
end;

procedure TDBusMessageIterator.Next;
begin
  dbus_message_iter_next(@FIter);
end;

procedure TDBusMessageIterator.Get(AType: cInt; var Value);

Var
  A : cInt;

begin
  A:=dbus_message_iter_get_arg_type(@fIter);
  if (A=DBUS_TYPE_INVALID) then
    Error(SErrNoMoreArguments);
  if (A<>AType) then
    Error(SErrInvalidArgumentType,[Char(AType),Char(A)]);
  dbus_message_iter_get_basic(@FIter,@value);
  next;
end;

procedure TDBusMessageIterator.GetArgument(var Arg: Byte);
begin
  Get(DBUS_TYPE_BYTE,Arg);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: Boolean);

Var
  B : DBUS_BOOL_T;

begin
  Get(DBUS_TYPE_BOOLEAN,B);
  Arg:=(B<>0);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: SmallInt);
begin
  Get(DBUS_TYPE_INT16,Arg);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: Word);
begin
  Get(DBUS_TYPE_UINT16,Arg);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: Integer);
begin
  Get(DBUS_TYPE_INT32,Arg);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: Cardinal);
begin
  Get(DBUS_TYPE_UINT32,Arg);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: Int64);
begin
  Get(DBUS_TYPE_INT64,Arg);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: QWord);
begin
  Get(DBUS_TYPE_UINT64,Arg);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: Double);
begin
  Get(DBUS_TYPE_DOUBLE,Arg);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: String);
Var
  P : Pchar;

begin
  p:=Nil;
  Get(DBUS_TYPE_STRING,P);
  if (P=Nil) then
    Arg:=''
  else
    Arg:=StrPas(P);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TByteArray);

Var
  P : Pointer;
  N : cInt;
  It : DBUSMessageIter;

begin
  n:=GetFixedArray(DBUS_TYPE_BYTE,p);
  SetLength(Arg,n);
  If (N>0) then
    Move(P^,Arg[0],N*sizeOf(Byte));
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TBooleanArray);

Var
  P : ^DBUS_BOOL_T;
  N : cInt;

begin
  n:=GetFixedArray(DBUS_TYPE_BOOLEAN,P);
  SetLength(Arg,n);
  While (N>0) do
    begin
    Dec(N);
    Arg[N]:=(P[N]<>0);
    end;
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TSmallIntArray);

Var
  P : Pointer;
  N : cInt;

begin
  n:=GetFixedArray(DBUS_TYPE_int16,P);
  SetLength(Arg,n);
  If (N>0) then
    Move(P^,Arg[0],N*sizeOf(SmallInt));
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TWordArray);

Var
  P : Pointer;
  N : cInt;

begin
  n:=GetFixedArray(DBUS_TYPE_Uint16,P);
  SetLength(Arg,n);
  If (N>0) then
    Move(P^,Arg[0],N*sizeOf(Word));
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TIntegerArray);

Var
  P : Pointer;
  N : cInt;

begin
  n:=GetFixedArray(DBUS_TYPE_int32,P);
  SetLength(Arg,n);
  If (N>0) then
    Move(P^,Arg[0],N*sizeOf(Integer));
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TCardinalArray);
Var
  P : Pointer;
  N : cInt;

begin
  n:=GetFixedArray(DBUS_TYPE_Uint32,P);
  SetLength(Arg,n);
  If (N>0) then
    Move(P^,Arg[0],N*sizeOf(Cardinal));
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TInt64Array);
Var
  P : Pointer;
  N : cInt;

begin
  n:=GetFixedArray(DBUS_TYPE_int64,P);
  SetLength(Arg,n);
  If (N>0) then
    Move(P^,Arg[0],N*sizeOf(Int64));
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TQWordArray);
Var
  P : Pointer;
  N : cInt;

begin
  n:=GetFixedArray(DBUS_TYPE_Uint64,P);
  SetLength(Arg,n);
  If (N>0) then
    Move(P^,Arg[0],N*sizeOf(QWord));
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TDoubleArray);

Var
  P : Pointer;
  N : cInt;

begin
  n:=GetFixedArray(DBUS_TYPE_DOUBLE,P);
  SetLength(Arg,n);
  If (N>0) then
    Move(P^,Arg[0],N*sizeOf(Double));
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TStringArray);

Var
  A : cInt;
  AI : DBUSMessageIter;
  l : integer;
  p : Pchar;

begin
  A:=dbus_message_iter_get_arg_type(@fIter);
  If (A<>DBUS_TYPE_ARRAY) then
    Error(SErrInvalidArgumentType,[Char(DBUS_TYPE_ARRAY),Char(A)]);
   A:=dbus_message_iter_get_element_type(@fIter);
  If (A<>DBUS_TYPE_STRING) then
      Error(SErrInvalidArrayElementType,[Char(DBUS_TYPE_STRING),Char(A)]);
  dbus_message_iter_recurse(@Fiter, @AI);
  setlength(Arg,0);
  l:=0;
  while (dbus_message_iter_get_arg_type(@AI)<>DBUS_TYPE_INVALID) do
     begin
     Inc(l);
     If Length(Arg)<L then
       SetLength(Arg,L+10);
     dbus_message_iter_get_basic(@AI,@p);
     If P<>Nil then
       Arg[l-1]:=StrPas(P)
     else
       Arg[l-1]:='';
     dbus_message_iter_next(@ai)
     end;
  dbus_message_iter_next (@FIter);
  If Length(Arg)<>L then
    SetLength(Arg,L);
end;

procedure TDBusMessageIterator.GetArgument(var Arg: TStringList);

Var
  A : cInt;
  AI : DBUSMessageIter;
  p : Pchar;

begin
  A:=GetArgumentType;
  If (A<>DBUS_TYPE_ARRAY) then
    Error(SErrInvalidArgumentType,[Char(DBUS_TYPE_ARRAY),Char(A)]);
  A:=GetElementType;
  If (A<>DBUS_TYPE_STRING) then
      Error(SErrInvalidArrayElementType,[Char(DBUS_TYPE_STRING),Char(A)]);
  dbus_message_iter_recurse(@FIter, @AI);
  Arg.Clear;
  while (dbus_message_iter_get_arg_type(@AI)<>DBUS_TYPE_INVALID) do
     begin
     dbus_message_iter_get_basic(@AI,@p);
     If P<>Nil then
       Arg.Add(StrPas(P))
     else
       Arg.Add('');
     dbus_message_iter_next(@ai);
     end;
  Next;
end;

procedure TDBusMessageIterator.GetArgument(var Arg: Variant);
Var
  A : cInt;
  AI : DBUSMessageIter;
  p : Pchar;
  By : Byte;
  Boo : Boolean;
  S : smallint;
  W : word;
  I : Integer;
  C : Cardinal;
  I64 : Int64;
  Q : QWord;
  D : Double;
  St : String;
  IR : TDBusMessageIterator;

begin
  A:=GetArgumentType;
  If (A<>DBUS_TYPE_VARIANT) then
    Error(SErrInvalidArgumentType,[Char(DBUS_TYPE_VARIANT),Char(A)]);
  IR:=Recurse;
  try
    A:=IR.GetArgumentType;
    case A of
    DBUS_TYPE_BYTE :
      begin
      IR.GetArgument(By);
      Arg:=by;
      end;
    DBUS_TYPE_BOOLEAN :
      begin
      IR.GetArgument(Boo);
      Arg:=Boo;
      end;
    DBUS_TYPE_INT16 :
      begin
      IR.GetArgument(S);
      Arg:=S;
      end;
    DBUS_TYPE_UINT16 :
      begin
      IR.GetArgument(W);
      Arg:=W;
      end;
    DBUS_TYPE_INT32 :
      begin
      IR.GetArgument(I);
      Arg:=I;
      end;
    DBUS_TYPE_UINT32 :
      begin
      IR.GetArgument(C);
      Arg:=C;
      end;
    DBUS_TYPE_INT64 :
      begin
      IR.GetArgument(I64);
      Arg:=I64;
      end;
    DBUS_TYPE_UINT64 :
      begin
      IR.GetArgument(Q);
      Arg:=Q;
      end;
    DBUS_TYPE_DOUBLE :
      begin
      IR.GetArgument(D);
      Arg:=D;
      end;
    DBUS_TYPE_STRING :
      begin
      IR.GetArgument(St);
      Arg:=St;
      end;
    else
      Error(SErrInvalidVariantType,[Char(DBUS_TYPE_VARIANT),Char(A)]);
    end;
  finally
    IR.free;
  end;
  Next;
end;

procedure TDBusMessageIterator.GetArgument(Const Arg: TDBUSDictionary);

Var
  A : cInt;
  I : TDBusMessageIterator;

begin
  A:=GetArgumentType;
  If (A<>DBUS_TYPE_ARRAY) then
      Error(SErrInvalidArgumentType,[Char(DBUS_TYPE_ARRAY),Char(A)]);
  A:=GetElementType;
  If (A<>DBUS_TYPE_DICT_ENTRY) then
    Error(SErrInvalidArrayElementType,[Char(DBUS_TYPE_DICT_ENTRY),Char(A)]);
  I:=Recurse;
  try
    While I.HasNext do
      Arg.AddDictItem.Load(I);
  finally
    I.Free;
  end;
end;

procedure TDBusMessageIterator.Append(AType: cInt; const Value);
begin
  if (0=dbus_message_iter_append_basic(@FIter,AType,@Value)) then
    RaiseDbusError(ClassName,SErrAppendFailed);
end;

procedure TDBusMessageIterator.AppendArgument(const Arg: Byte);
begin
  Append(DBUS_TYPE_BYTE,Arg);
end;

procedure TDBusMessageIterator.AppendArgument(Const Arg: Boolean);

Var
  B : dbus_bool_t;
begin
  B:=Ord(Arg);
  Append(dbus.DBUS_TYPE_BOOLEAN,B);
end;

procedure TDBusMessageIterator.AppendArgument(const Arg: SmallInt);
begin
  Append(DBUS_TYPE_INT16,Arg);
end;

procedure TDBusMessageIterator.AppendArgument(const Arg: Word);
begin
  Append(DBUS_TYPE_UINT16,Arg);

end;

procedure TDBusMessageIterator.AppendArgument(Const Arg: Integer);
begin
  Append(DBUS_TYPE_INT32,Arg);
end;

procedure TDBusMessageIterator.AppendArgument(const Arg: Cardinal);
begin
  Append(DBUS_TYPE_UINT32,Arg);
end;

procedure TDBusMessageIterator.AppendArgument(const Arg: Int64);
begin
  Append(DBUS_TYPE_INT64,Arg);
end;

procedure TDBusMessageIterator.AppendArgument(const Arg: QWord);
begin
  Append(DBUS_TYPE_UINT64,Arg);
end;

procedure TDBusMessageIterator.AppendArgument(const Arg: Double);
begin
  Append(DBUS_TYPE_DOUBLE,Arg);
end;

procedure TDBusMessageIterator.AppendArgument(Const Arg: String);

Var
  P : PChar;

begin
  P:=Pchar(Arg);
  Append(DBUS_TYPE_STRING,P);
end;

{ TDBUSDictionary }

function TDBUSDictionary.AddDictItem: TDBUSDictItem;
begin
  Result:=Add as TDBUSDictItem;
end;

end.

