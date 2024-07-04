{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2023 the Free Pascal development team

   Generic messaging service class.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit System.Messaging;

{$MODE OBJFPC}
{$H+}
{$modeswitch functionreferences}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Generics.Collections;
{$ELSE}  
  SysUtils, Classes, Generics.Collections;
{$ENDIF}

type

  TMessageBase = class abstract;
  // TMessage = TMessageBase;

  generic TMessage<T> = class (TMessageBase)
  protected
    FValue: T;
  public
    constructor Create(const AValue: T);
    destructor Destroy; override;
    property Value: T read FValue;
  end;

  generic TObjectMessage<T: class> = class(specialize TMessage<T>)
  protected
    FOwnsObject: Boolean;
  public
    constructor Create(const AValue: T; aOwnsObject: Boolean = True);
    destructor Destroy; override;
  end;

  TMessageListener = reference to procedure(const Sender: TObject; const M: TMessageBase);
  TMessageListenerMethod = procedure (const Sender: TObject; const M: TMessageBase) of object;
  {$IFNDEF CPU64}
  TMessageSubscriptionId = LongInt;
  {$ELSE}
  TMessageSubscriptionId = Int64;
  {$ENDIF}
  TBaseMessageManager = Class;
  TBaseMessageManagerClass = Class of TBaseMessageManager;

  { TBaseMessageManager }

  TBaseMessageManager = class
  Private
    FNextID : TMessageSubscriptionId;
  Private
    class var _instance: TBaseMessageManager;
    class function GetInstance: TBaseMessageManager; static;
  Public
    Class Destructor Done;
    class var DefaultManagerClass: TBaseMessageManagerClass;
  Protected
    Function GenerateClientID : TMessageSubscriptionId;
  Public
    Constructor Create; virtual;
    function SubscribeToMessage(const aMessageClass: TClass; const aListener: TMessageListener): TMessageSubscriptionId; virtual; abstract; overload;
    function SubscribeToMessage(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod): TMessageSubscriptionId; virtual; abstract; overload;
    procedure Unsubscribe(const aMessageClass: TClass; SubscriptionId: TMessageSubscriptionId; Immediate: Boolean = False); virtual; abstract; overload;
    procedure Unsubscribe(const aMessageClass: TClass; const aListener: TMessageListener; Immediate: Boolean = False); virtual; abstract; overload;
    procedure Unsubscribe(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); virtual; abstract; overload;
    procedure SendMessage(const Sender: TObject; AMessage: TMessageBase); overload;
    procedure SendMessage(const Sender: TObject; AMessage: TMessageBase; ADispose: Boolean); virtual; abstract;  overload;
    class property DefaultManager: TBaseMessageManager read GetInstance;
  end;


  { TMessageManager }
  // Default, delphi compatible implementation

  TMessageManager = class(TBaseMessageManager)
  protected
  type

    { TListenerWithId }

    TListenerWithId = class
      Id: TMessageSubscriptionId;
      Listener: TMessageListener;
      ListenerMethod: TMessageListenerMethod;
      MarkedAsRemoved : Boolean;
    Public
      Function Matches(aListener : TMessageListener) : Boolean; inline;
      Function Matches(aListener : TMessageListenerMethod) : Boolean; inline;
      Function Matches(aID : TMessageSubscriptionId) : Boolean; inline;
      constructor Create(const AId: TMessageSubscriptionId; const AListenerMethod: TMessageListenerMethod); overload;
      constructor Create(const AId: TMessageSubscriptionId; const AListener: TMessageListener); overload;
      procedure MarkAsRemoved;
    end;
    PListenerWithId = ^TListenerWithId;
    TListenerWithIdList = specialize TObjectList<TListenerWithId>;

    { TListenerList }

    TListenerList = class
    Private
      FList : TListenerWithIdList;
      FUpdateCount : Integer;
      FUnSubscribeCount : Integer;
      Procedure BeginUpdate; inline;
      Procedure EndUpdate; inline;
      function Updating : Boolean; inline;
      procedure DoUnsubscribe(Index: Integer);
    Public
      constructor Create;
      destructor destroy; override;
      procedure RemoveEmpty;
      procedure CheckRemoveEmpty; inline;
      function Subscribe(const AId: TMessageSubscriptionId; const AListener: TMessageListener): TMessageSubscriptionId; overload;
      function Subscribe(const AId: TMessageSubscriptionId; const AListenerMethod: TMessageListenerMethod): TMessageSubscriptionId; overload;
      procedure Unsubscribe(Index: TMessageSubscriptionId);
      procedure Unsubscribe(aListener: TMessageListener);
      procedure Unsubscribe(aListener: TMessageListenerMethod);
      procedure SendMessage(const Sender: TObject; const AMessage: TMessageBase);
    end;

    TListenerRegistry = specialize TObjectDictionary<TClass, TListenerList>;
  private
  protected
    FListeners: TListenerRegistry;
    function Add(const aMessageClass: TClass;
      const aListener: TMessageListener; aListenerMethod: TMessageListenerMethod
      ): Integer;
    procedure RegisterMessageClass(const AMessageClass: TClass);
  public
    constructor Create; override;
    destructor Destroy; override;
    function SubscribeToMessage(const aMessageClass: TClass; const aListener: TMessageListener): TMessageSubscriptionId; override;
    function SubscribeToMessage(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod): TMessageSubscriptionId; override;
    procedure Unsubscribe(const aMessageClass: TClass; SubscriptionId: TMessageSubscriptionId; Immediate: Boolean = False); override;
    procedure Unsubscribe(const aMessageClass: TClass; const aListener: TMessageListener; Immediate: Boolean = False); override;
    procedure Unsubscribe(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); override;
    procedure SendMessage(const Sender: TObject; AMessage: TMessageBase; ADispose: Boolean); override;
  end;
  TMessageManagerClass = class of TMessageManager;

{ ---------------------------------------------------------------------
  TMessageClientList
  ---------------------------------------------------------------------}

   // FPC implementation, designed to be extensible.
   // Used as default.
   // Set TBaseMessageManager.DefaultManagerClass if you want to change the default.

  { TMessageClient }

  TMessageClient = class (TCollectionItem)
  Public
    Disabled : boolean; // Unsubscribed but not yet deleted...
    ClientID : TMessageSubscriptionId;
  Public
    constructor Create(aCollection : TCollection; aClientID : TMessageSubscriptionId); overload;
    Procedure CallNotify(Sender : TObject; aMessage : TMessageBase); virtual; abstract;
  end;

  { TMessageListenerClient }

  TMessageListenerClient = class(TMessageClient)
  Private
    FListener: TMessageListener;
  Protected
    Property Listener : TMessageListener Read FListener;
  Public
    constructor Create(aCollection : TCollection; aClientID : TMessageSubscriptionId; aListener: TMessageListener); overload;
    Procedure CallNotify(Sender : TObject; aMessage : TMessageBase); override;
  end;

  { TMessageListenerMethodClient }

  TMessageListenerMethodClient = class(TMessageClient)
  Private
    FListener: TMessageListenerMethod;
  Protected
    Property Listener : TMessageListenerMethod Read FListener;
  Public
    constructor Create(aCollection : TCollection; aClientID : TMessageSubscriptionId; aListener: TMessageListenerMethod); overload;
    Procedure CallNotify(Sender : TObject; aMessage : TMessageBase); override;
  end;

  { TMessageClientList }

  TMessageClientList = class(TCollection)
  private
    FBusy : Boolean;
  Protected
    Procedure Update(aItem: TCollectionItem); override;
    procedure RemoveDisabled; virtual;
    Property Busy : Boolean Read FBusy Write FBusy;
  public
    constructor Create(aItemClass : TCollectionItemClass);
    function Add(aId : Integer; const aListener: TMessageListener) : TMessageClient; virtual;
    function Add(aId : Integer; const aListenerMethod: TMessageListenerMethod): TMessageClient; virtual;
    procedure NotifyClients(const Sender: TObject; const aMessage: TMessageBase);
    // These should be improved to be faster ?
    function IndexOf(const aClientID: TMessageSubscriptionId) : integer; virtual; overload;
    function IndexOf(const aListener: TMessageListener): integer; virtual; overload;
    function IndexOf(const aListenerMethod: TMessageListenerMethod): integer; virtual; overload;
    procedure Remove(aIndex : Integer);
  end;

  { TSimpleMessageManager }

  TSimpleMessageManager = class(TBaseMessageManager)
  protected
    Type
       TMessageClientListDict = specialize TObjectDictionary<TClass, TMessageClientList>;
  Private
    FMessageClients: TMessageClientListDict;
  Protected
    function CreateMessageTypeDict: TMessageClientListDict; virtual;
    function CreateMessageClientList: TMessageClientList; virtual;
    Function GetList(const aMessageClass: TClass; Out aList : TMessageClientList) : Boolean;
    Function GetOrCreateList(const aMessageClass: TClass) : TMessageClientList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function SubscribeToMessage(const aMessageClass: TClass; const aListener: TMessageListener): TMessageSubscriptionId; override;
    function SubscribeToMessage(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod): TMessageSubscriptionId; override;
    // Immediate not used, it will break during sending of message
    procedure Unsubscribe(const aMessageClass: TClass; SubscriptionId: TMessageSubscriptionId; Immediate: Boolean = False); override;
    procedure Unsubscribe(const aMessageClass: TClass; const aListener: TMessageListener; Immediate: Boolean = False); override;
    procedure Unsubscribe(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); override;
    procedure SendMessage(const Sender: TObject; aMessage: TMessageBase; aDispose: Boolean); override;
  end;

implementation

{ TMessageListenerMethodClient }

constructor TMessageListenerMethodClient.Create(aCollection: TCollection;  aClientID: TMessageSubscriptionId; aListener: TMessageListenerMethod);
begin
  Inherited Create(aCollection,aClientID);
  FListener:=aListener;
end;

procedure TMessageListenerMethodClient.CallNotify(Sender: TObject;
  aMessage: TMessageBase);
begin
  FListener(Sender,aMessage);
end;

{ TMessageListenerClient }

constructor TMessageListenerClient.Create(aCollection: TCollection;
  aClientID: TMessageSubscriptionId; aListener: TMessageListener);
begin
  Inherited Create(aCollection,aClientID);
  FListener:=aListener;
end;

procedure TMessageListenerClient.CallNotify(Sender: TObject; aMessage: TMessageBase);
begin
  FListener(Sender,aMessage);
end;

{ TSimpleMessageManager }

constructor TSimpleMessageManager.Create;
begin
  FMessageClients:=CreateMessageTypeDict;
end;

destructor TSimpleMessageManager.Destroy;
begin
  FreeAndNil(FMessageClients);
  inherited;
end;

function TSimpleMessageManager.GetList(const aMessageClass: TClass; out
  aList: TMessageClientList): Boolean;

begin
  aList:=Nil;
  Result:=FMessageClients.TryGetValue(aMessageClass,aList);
end;

function TSimpleMessageManager.CreateMessageTypeDict: TMessageClientListDict;
begin
  Result:=TMessageClientListDict.Create([doOwnsValues]);
end;

function TSimpleMessageManager.CreateMessageClientList :TMessageClientList;

begin
  Result:=TMessageClientList.Create(TMessageClient);
end;

function TSimpleMessageManager.GetOrCreateList(const aMessageClass: TClass): TMessageClientList;

begin
  if GetList(aMessageClass,Result) then
    exit;
  Result:=CreateMessageClientList;
  FMessageClients.Add(AMessageClass, Result);
end;


function TSimpleMessageManager.SubscribeToMessage(const aMessageClass: TClass; const aListener: TMessageListener) : TMessageSubscriptionId;

var
  Clients: TMessageClientList;

begin
  Clients:=GetOrCreateList(aMessageClass);
  Result:=GenerateClientID;
  Clients.Add(Result,AListener);
end;

function TSimpleMessageManager.SubscribeToMessage(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod): TMessageSubscriptionId;

var
  Clients: TMessageClientList;

begin
  Clients:=GetOrCreateList(aMessageClass);
  Result:=GenerateClientID;
  Clients.Add(Result,AListenerMethod);
end;

procedure TSimpleMessageManager.Unsubscribe(const aMessageClass: TClass; const aListener: TMessageListener; Immediate: Boolean);

var
  Clients : TMessageClientList;
  Idx : Integer;

begin
  if Not FMessageClients.TryGetValue(aMessageClass,Clients) then
    exit;
  Idx:=Clients.IndexOf(aListener);
  if Idx<>-1 then
    Clients.Remove(Idx);
end;

procedure TSimpleMessageManager.Unsubscribe(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod; Immediate: Boolean);

var
  Clients : TMessageClientList;
  Idx : Integer;

begin
  if Not FMessageClients.TryGetValue(aMessageClass,Clients) then
    exit;
  Idx:=Clients.IndexOf(aListenerMethod);
  if Idx<>-1 then
    Clients.Remove(Idx);
end;

procedure TSimpleMessageManager.Unsubscribe(const aMessageClass: TClass; SubscriptionId: TMessageSubscriptionId; Immediate: Boolean);
var
  Clients : TMessageClientList;
  Idx : Integer;

begin
  if Not FMessageClients.TryGetValue(aMessageClass,Clients) then
    exit;
  Idx:=Clients.IndexOf(SubscriptionId);
  Clients.Remove(Idx);
end;

procedure TSimpleMessageManager.SendMessage(const Sender: TObject;
  aMessage: TMessageBase; aDispose: Boolean);

var
  Clients: TMessageClientList;
begin
  if (AMessage=nil) then exit;
  try
    if not GetList(aMessage.ClassType,Clients) then
      exit;
    Clients.NotifyClients(Sender,AMessage);
  finally
    if ADispose then
      AMessage.Free;
  end;
end;

{ TClientList }

procedure TMessageClientList.Update(aItem: TCollectionItem);
begin
  inherited Update(aItem);
  if (aItem=Nil) and not Busy then
    RemoveDisabled;
end;

constructor TMessageClientList.Create(aItemClass : TCollectionItemClass);
begin
  Inherited Create(aItemClass);
end;

function TMessageClientList.Add(aId: Integer; const aListener: TMessageListener ): TMessageClient;

begin
  FBusy:=True;// Prevent cleaning
  try
    Result:=TMessageListenerClient.Create(Self,aId,aListener);
  finally
    FBusy:=False;
  end;
end;

function TMessageClientList.Add(aId: Integer; const aListenerMethod: TMessageListenerMethod): TMessageClient;

begin
  FBusy:=True;// Prevent cleaning
  try
    Result:=TMessageListenerMethodClient.Create(Self,aID,aListenerMethod);
  finally
    FBusy:=False;
  end;
end;

procedure TMessageClientList.NotifyClients(const Sender: TObject;
  const aMessage: TMessageBase);

var
  Listener: TMessageClient;
  I : integer;

begin
  BeginUpdate;
  try
    for I:=0 to Count-1 do
      begin
      Listener:=TMessageClient(Items[I]);
      if Not Listener.Disabled then
        Listener.CallNotify(Sender, AMessage)
      end;
  finally
    EndUpdate;
  end;
end;

function TMessageClientList.IndexOf(const aClientID: TMessageSubscriptionId): integer;
begin
  Result:=Count-1;
  While (Result>=0) and (TMessageClient(Items[Result]).ClientID<>aClientID) do
    Dec(Result);
end;

function TMessageClientList.IndexOf(const aListener: TMessageListener): integer;

  Function IsMatch(C : TMessageClient) : Boolean;

  var
    L : TMessageListenerClient absolute C;

  begin
    Result:=(C is TMessageListenerClient) and (L.Listener=aListener);
  end;

begin
  Result:=Count-1;
  While (Result>=0) and Not IsMatch(TMessageClient(Items[Result])) do
    Dec(Result);
end;

function TMessageClientList.IndexOf(const aListenerMethod: TMessageListenerMethod): integer;
  Function IsMatch(C : TMessageClient) : Boolean;

  var
    L : TMessageListenerMethodClient absolute C;

  begin
    Result:=(C is TMessageListenerMethodClient) and (L.Listener=aListenerMethod);
  end;

begin
  Result:=Count-1;
  While (Result>=0) and Not IsMatch(TMessageClient(Items[Result])) do
    Dec(Result);
end;

procedure TMessageClientList.Remove(aIndex: Integer);

{
  We cannot just remove clients at once: when sending messages they must be
  sent in order of listener registration.
  But sending a message can result in a listener being deleted.
  This can change the indexes in the list if done incorrectly.

  So we can only delete when all messages have been processed.
  We use the standard TCollection Begin/EndUdpate mechansim for this.
}

begin
  if (aIndex<0) or (aIndex>=Count) then exit;
  BeginUpdate;
  try
    TMessageClient(Items[aIndex]).Disabled:=True;
  finally
    EndUpdate;
  end;
end;

procedure TMessageClientList.RemoveDisabled;

var
  I : Integer;

begin
  FBusy:=True;
  BeginUpdate;
  try
    for I:=Count-1 downto 0 do
      if TMessageClient(Items[I]).Disabled then
        Delete(I);
  finally
    EndUpdate;
    FBusy:=False;
  end;
end;

{ TClient }


constructor TMessageClient.Create(aCollection: TCollection; aClientID: TMessageSubscriptionId);
begin
  Disabled:=False; // Safety: set before inherited, make sure cleanup does not happen.
  Inherited Create(aCollection);
  ClientID:=aClientID;
end;

{ TBaseMessageManager }

class function TBaseMessageManager.GetInstance: TBaseMessageManager;
begin
  if _Instance=Nil then
    begin
    if DefaultManagerClass=Nil then
      DefaultManagerClass:=TSimpleMessageManager;
    _Instance:=DefaultManagerClass.Create;
    end;
  Result:=_Instance;
end;


class destructor TBaseMessageManager.Done;

begin
  FreeAndNil(_Instance);
end;

function TBaseMessageManager.GenerateClientID: TMessageSubscriptionId;
begin
  Result:=AtomicIncrement(FNextID);
end;

constructor TBaseMessageManager.Create;
begin
  // Do nothing. Need virtual constructor
end;

procedure TBaseMessageManager.SendMessage(const Sender: TObject;
  AMessage: TMessageBase);
begin
  SendMessage(Sender,aMessage,True);
end;

{ TMessageManager.TListenerWithId }

constructor TMessageManager.TListenerWithId.Create(const aId: TMessageSubscriptionId; const aListenerMethod: TMessageListenerMethod);

begin
  Id:=aID;
  ListenerMethod:=aListenerMethod;
end;

constructor TMessageManager.TListenerWithId.Create(const AId: TMessageSubscriptionId; const AListener: TMessageListener); 

begin
  Id:=aId;
  Listener:=aListener;
end;

Function TMessageManager.TListenerWithId.Matches(aID : TMessageSubscriptionId) : Boolean;
begin
  Result:=(aId=ID);
end;

function TMessageManager.TListenerWithId.Matches(aListener: TMessageListener): Boolean;
begin
  Result:=(Pointer(aListener)=Pointer(Listener));
end;

function TMessageManager.TListenerWithId.Matches(aListener: TMessageListenerMethod): Boolean;
Var
  M1,M2 : TMethod;
begin
  M1:=TMethod(aListener);
  M2:=TMethod(ListenerMethod);
  Result:=(M1.Code=M2.Code) and (M2.Data=M2.Data);
end;

procedure TMessageManager.TListenerWithId.MarkAsRemoved;

begin
  MarkedAsRemoved:=True;
  Id:=0;
  Listener:=Nil;
  ListenerMethod:=Nil;
end;

{ TMessageManager.TListenerList }

procedure TMessageManager.TListenerList.BeginUpdate;
begin
  AtomicIncrement(FUpdateCount);
end;

procedure TMessageManager.TListenerList.EndUpdate;
begin
  AtomicDecrement(FUpdateCount);
end;

function TMessageManager.TListenerList.Updating: Boolean;
begin
  Result:=(FUpdateCount>0);
end;

constructor TMessageManager.TListenerList.Create;
begin
  FList:=TListenerWithIdList.Create(True);
end;

destructor TMessageManager.TListenerList.destroy;
begin
  FreeAndNil(Flist);
  inherited destroy;
end;

procedure TMessageManager.TListenerList.SendMessage(const Sender: TObject; const AMessage: TMessageBase);

var
  I : Integer;
  L : TListenerWithId;

begin
  BeginUpdate;
  try
    for I:=0 to FList.Count-1 do
    begin
      L:=FList[I];
      if L.MarkedAsRemoved then
        continue;
      if Assigned(L.Listener) then
        L.Listener(Sender, AMessage)
      else if Assigned(L.ListenerMethod) then
        L.ListenerMethod(Sender,AMessage);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TMessageManager.TListenerList.CheckRemoveEmpty;

begin
  if FUnSubscribeCount>10 then
    RemoveEmpty;
end;

function TMessageManager.TListenerList.Subscribe(const AId: TMessageSubscriptionId; const AListener: TMessageListener): TMessageSubscriptionId;

var
  Obj : TListenerWithId;

begin
  Obj:=TListenerWithId.Create(aId,aListener);
  FList.Add(Obj);
  Result:=Obj.Id;
end;

function TMessageManager.TListenerList.Subscribe(const AId: TMessageSubscriptionId; const AListenerMethod: TMessageListenerMethod): TMessageSubscriptionId;
var
  Obj : TListenerWithId;

begin
  Obj:=TListenerWithId.Create(aId,aListenerMethod);
  FList.Add(Obj);
  Result:=Obj.Id;
end;

procedure TMessageManager.TListenerList.Unsubscribe(Index: TMessageSubscriptionId);

var
  Idx : integer;

begin
  Idx:=FList.Count-1;
  While (Idx>=0) and not FList[Idx].Matches(Index) do
    Dec(Idx);
  If Idx>0 then
    DoUnsubscribe(Idx);
end;

procedure TMessageManager.TListenerList.Unsubscribe(aListener: TMessageListener);

var
  Idx : integer;

begin
  Idx:=FList.Count-1;
  While (Idx>=0) and not FList[Idx].Matches(aListener) do
    Dec(Idx);
  If Idx>0 then
    DoUnsubscribe(Idx);
end;

procedure TMessageManager.TListenerList.Unsubscribe(aListener: TMessageListenerMethod);
var
  Idx : integer;

begin
  Idx:=FList.Count-1;
  While (Idx>=0) and not FList[Idx].Matches(aListener) do
    Dec(Idx);
  If Idx>0 then
    DoUnsubscribe(Idx);
end;

procedure TMessageManager.TListenerList.DoUnsubscribe(Index: Integer);

begin
  FList[Index].MarkAsRemoved;
  Inc(FUnSubscribeCount);
  if Not Updating then
    CheckRemoveEmpty;
end;

procedure TMessageManager.TListenerList.RemoveEmpty;
var
  I, N: Integer;
  L : TListenerWithId;
begin
  N:=0;
  for I:=0 to FList.Count-1 do
    begin
    L:=FList[I];
    if Not L.MarkedAsRemoved then
      begin
      if N<I then
        FList[N]:=L;
      Inc(N);
      end;
    end;
  FList.Count:=N;
  FUnSubscribeCount:=0;
end;


{ TMessageManager }

constructor TMessageManager.Create;
begin
  FListeners := TListenerRegistry.Create([doOwnsValues]);
end;

destructor TMessageManager.Destroy;
begin
  FListeners.Free;
  inherited;
end;


procedure TMessageManager.RegisterMessageClass(const aMessageClass: TClass);
begin
  if not FListeners.ContainsKey(AMessageClass) then
    FListeners.Add(AMessageClass, TListenerList.Create);
end;

function TMessageManager.Add(const aMessageClass: TClass; const aListener: TMessageListener; aListenerMethod: TMessageListenerMethod) : Integer;

var
  List: TListenerList;

begin
  Result := -1;
  RegisterMessageClass(aMessageClass);
  if Not FListeners.TryGetValue(aMessageClass,List) then
    Exit;
  Result:=GenerateClientID;
  If Assigned(aListener) then
    List.Subscribe(Result,aListener)
  else
    List.SubScribe(Result,aListenerMethod);
end;

function TMessageManager.SubscribeToMessage(const aMessageClass: TClass; const aListener: TMessageListener) : TMessageSubscriptionID;
begin
  Result:=Add(aMessageClass,aListener,Nil);
end;

function TMessageManager.SubscribeToMessage(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod): TMessageSubscriptionID;

begin
  Result:=Add(aMessageClass,Nil,aListenerMethod);
end;

procedure TMessageManager.Unsubscribe(const aMessageClass: TClass; const aListener: TMessageListener; Immediate: Boolean);

var
  List : TListenerList;

begin
  if Not FListeners.TryGetValue(AMessageClass,List) then
    Exit;
  List.Unsubscribe(AListener);
end;

procedure TMessageManager.Unsubscribe(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod; Immediate: Boolean);

var
  List : TListenerList;

begin
  if Not FListeners.TryGetValue(AMessageClass,List) then
    Exit;
  List.Unsubscribe(aListenerMethod);
end;

procedure TMessageManager.Unsubscribe(const aMessageClass: TClass; SubscriptionId: TMessageSubscriptionId; Immediate: Boolean);

var
  List: TListenerList;

begin
  if not FListeners.TryGetValue(AMessageClass,List) then
    Exit;
  List.Unsubscribe(SubscriptionID);
end;

procedure TMessageManager.SendMessage(const Sender: TObject; AMessage: TMessageBase; ADispose: Boolean);

var
  List: TListenerList;

begin
  if aMessage=nil then
    Exit;
  try
    if FListeners.TryGetValue(aMessage.ClassType,List) then
      List.SendMessage(Sender,aMessage);
  finally
    if aDispose then
      aMessage.Free;
  end
end;


constructor TMessage.Create(const aValue: T);
begin
  FValue := AValue;
end;

destructor TMessage.Destroy;
begin
  inherited;
end;

{ TObjectMessage<T> }

constructor TObjectMessage.Create(const aValue: T; aOwnsObject: Boolean);
begin
  inherited Create(AValue);
  FOwnsObject:=aOwnsObject;
end;

destructor TObjectMessage.Destroy;
begin
  if FOwnsObject then
    FValue.Free;
  inherited Destroy;
end;


end.

