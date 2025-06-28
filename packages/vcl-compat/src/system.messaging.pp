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

{.$DEFINE DEBUG_SYSTEM_MESSAGING}
{.$DEFINE DECLARE_COMPATIBLEMANAGER}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs;
{$ELSE}  
  SysUtils, Classes, Generics.Collections, syncobjs;
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


{$ifdef DECLARE_COMPATIBLEMANAGER}
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
{$ENDIF DECLARE_COMPATIBLEMANAGER}

{ ---------------------------------------------------------------------
  TMessageClientList
  ---------------------------------------------------------------------}

   // FPC implementation, designed to be extensible.
   // Used as default.
   // Set TBaseMessageManager.DefaultManagerClass if you want to change the default.

  TMessageClientList = class;

  { TMessageClient }

  TMessageClient = class
  Protected
    Disabled : Boolean; // Unsubscribed but not yet deleted...
    ClientID : TMessageSubscriptionId;
  Public
    constructor Create(aClientID : TMessageSubscriptionId);

    function SameListener(const aListener: TMessageListener) : Boolean; virtual; abstract;
    function SameListenerMethod(const aListenerMethod: TMessageListenerMethod) : Boolean; virtual; abstract;

    Procedure CallNotify(Sender : TObject; aMessage : TMessageBase); virtual; abstract;
  end;

  { TMessageListenerClient }

  TMessageListenerClient = class(TMessageClient)
  Private
    FListener: TMessageListener;
  Protected
    Property Listener : TMessageListener Read FListener;
  Public
    constructor Create(aClientID : TMessageSubscriptionId; aListener: TMessageListener);

    function SameListener(const aListener: TMessageListener) : Boolean; override;
    function SameListenerMethod(const aListenerMethod: TMessageListenerMethod) : Boolean; override;

    Procedure CallNotify(Sender : TObject; aMessage : TMessageBase); override;
  end;

  { TMessageListenerMethodClient }

  TMessageListenerMethodClient = class(TMessageClient)
  Private
    FListenerMethod: TMessageListenerMethod;
    {$ifdef DEBUG_SYSTEM_MESSAGING}
    FListenerClassname : shortstring;
    {$endif}
  Protected
    Property ListenerMethod : TMessageListenerMethod Read FListenerMethod;
  Public
    constructor Create(aClientID : TMessageSubscriptionId; aListenerMethod: TMessageListenerMethod);

    function SameListener(const aListener: TMessageListener) : Boolean; override;
    function SameListenerMethod(const aListenerMethod: TMessageListenerMethod) : Boolean; override;

    Procedure CallNotify(Sender : TObject; aMessage : TMessageBase); override;
  end;

  { TMessageClientList }

  (*
  List with a delayed released of its items

  When an item is "removed" it's marked as Disabled and FDisabledCount is increased
  When FDisabledCount goes above cRemoveDisabledTreshold the RemoveDisabled
  method is triggered which performs the actual release & packs the internal list

  Class isn't thread-safe and should be protected by message manager

  *)
  TMessageClientList = class
  private
    FItems : array of TMessageClient;
    FCount : Integer;
    FUpdateCount : Integer;
    FDisabledCount : Integer;

  protected
    function GetItems(aIndex : Integer) : TMessageClient;
    procedure AddClient(aClient : TMessageClient);

    // Wait until at least that many items have been removed before packing
    const cRemoveDisabledTreshold = 10;

    procedure RemoveDisabled;

  public
    constructor Create(aItemClass : TClass);
    destructor Destroy; override;

    // Use Begin/EndUpdate to protect a loop that's susceptible of triggering Remove
    procedure BeginUpdate;
    procedure EndUpdate;

    property Items[aIndex : Integer] : TMessageClient read GetItems;
    property Count : Integer read FCount;

    function Add(aId : Integer; const aListener: TMessageListener) : TMessageClient; virtual;
    function Add(aId : Integer; const aListenerMethod: TMessageListenerMethod): TMessageClient; virtual;
    procedure NotifyClients(const Sender: TObject; const aMessage: TMessageBase);
    // These should be improved to be faster ?
    function IndexOf(const aClientID: TMessageSubscriptionId) : integer; virtual; overload;
    function IndexOf(const aListener: TMessageListener): integer; virtual; overload;
    function IndexOf(const aListenerMethod: TMessageListenerMethod): integer; virtual; overload;

    procedure Remove(aIndex : Integer);
    procedure Clear;
  end;

  { TSimpleMessageManager }

  TSimpleMessageManager = class(TBaseMessageManager)
  protected
    type
       TMessageClientListDict = specialize TObjectDictionary<TClass, TMessageClientList>;

  private
    FMessageClients: TMessageClientListDict;
    FLock : TCriticalSection;

  protected
    FLockCount : Integer;

    function CreateMessageTypeDict: TMessageClientListDict; virtual;
    function CreateMessageClientList: TMessageClientList; virtual;
    function GetList(const aMessageClass: TClass; out aList : TMessageClientList) : Boolean;
    function GetOrCreateList(const aMessageClass: TClass) : TMessageClientList;

    procedure Lock;
    procedure UnLock;

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

  {$ifndef DECLARE_COMPATIBLEMANAGER}
  TMessageManager = class (TSimpleMessageManager) end;
  {$endif}

implementation

{$ifdef DEBUG_SYSTEM_MESSAGING}
uses fresnel.wasm.api;
{$endif}

// SameMethode
//
function SameMethod(const aMethod1, aMethod2 : TMessageListenerMethod) : Boolean; inline;
begin
  Result :=     (TMethod(aMethod1).Code = TMethod(aMethod2).Code)
            and (TMethod(aMethod1).Data = Tmethod(aMethod2).Data);
end;

{ TMessageListenerMethodClient }

constructor TMessageListenerMethodClient.Create(aClientID: TMessageSubscriptionId; aListenerMethod: TMessageListenerMethod);
begin
  inherited Create(aClientID);
  FListenerMethod := aListenerMethod;
  {$ifdef DEBUG_SYSTEM_MESSAGING}
  FListenerClassname := TObject(TMethod(aListenerMethod).Data).ClassName;
  {$endif}
end;

function TMessageListenerMethodClient.SameListener(const aListener: TMessageListener): Boolean;
begin
  Result := False;
end;

function TMessageListenerMethodClient.SameListenerMethod(const aListenerMethod: TMessageListenerMethod): Boolean;
begin
  Result := SameMethod(aListenerMethod, FListenerMethod);
end;

procedure TMessageListenerMethodClient.CallNotify(Sender: TObject; aMessage: TMessageBase);
begin
  {$ifdef DEBUG_SYSTEM_MESSAGING}
  if Disabled then
  begin
    __fresnel_console_log('TMessageListenerMethodClient.CallNotify DISABLED');
    __fresnel_console_log(FListenerClassname);
  end;
  {$endif}
  FListenerMethod(Sender, aMessage);
end;

{ TMessageListenerClient }

constructor TMessageListenerClient.Create(aClientID: TMessageSubscriptionId; aListener: TMessageListener);
begin
  inherited Create(aClientID);
  FListener := aListener;
end;

function TMessageListenerClient.SameListener(const aListener: TMessageListener): Boolean;
begin
  Result := (aListener = FListener);
end;

function TMessageListenerClient.SameListenerMethod(const aListenerMethod: TMessageListenerMethod): Boolean;
begin
  Result := False;
end;

procedure TMessageListenerClient.CallNotify(Sender: TObject; aMessage: TMessageBase);
begin
  FListener(Sender, aMessage);
end;

{ TSimpleMessageManager }

constructor TSimpleMessageManager.Create;
begin
  FMessageClients:=CreateMessageTypeDict;
  FLock:=TCriticalSection.Create;
end;

destructor TSimpleMessageManager.Destroy;
begin
  FreeAndNil(FLock);
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
  if GetList(aMessageClass, Result) then
    Exit;

  Result := CreateMessageClientList;
  FMessageClients.Add(AMessageClass, Result);
end;

procedure TSimpleMessageManager.Lock;
begin
  {$ifdef DEBUG_SYSTEM_MESSAGING}
  if FLockCount > 0 then
    __fresnel_console_log('ALREADY LOCKED');
  {$endif}
  FLock.Enter;
  Inc(FLockCount);
end;

procedure TSimpleMessageManager.UnLock;
begin
  Dec(FLockCount);
  FLock.Leave;
end;


function TSimpleMessageManager.SubscribeToMessage(const aMessageClass: TClass; const aListener: TMessageListener) : TMessageSubscriptionId;

var
  Clients: TMessageClientList;

begin
  Lock;
  try
    Clients:=GetOrCreateList(aMessageClass);
    Result:=GenerateClientID;
    Clients.Add(Result,AListener);
  finally
    UnLock;
  end;
end;

function TSimpleMessageManager.SubscribeToMessage(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod): TMessageSubscriptionId;

var
  Clients: TMessageClientList;
begin
  Lock;
  try
    Clients:=GetOrCreateList(aMessageClass);
    Result:=GenerateClientID;
    Clients.Add(Result,AListenerMethod);
  finally
    UnLock;
  end;
end;

procedure TSimpleMessageManager.Unsubscribe(const aMessageClass: TClass; const aListener: TMessageListener; Immediate: Boolean);

var
  Clients : TMessageClientList;
  Idx : Integer;

begin
  Lock;
  try
    if Not FMessageClients.TryGetValue(aMessageClass,Clients) then
      exit;
    Idx:=Clients.IndexOf(aListener);
    if Idx >= 0 then
      Clients.Remove(Idx);
  finally
    UnLock;
  end;
end;

procedure TSimpleMessageManager.Unsubscribe(const aMessageClass: TClass; const aListenerMethod: TMessageListenerMethod; Immediate: Boolean);

var
  Clients : TMessageClientList;
  Idx : Integer;
begin
  Lock;
  try
    if not FMessageClients.TryGetValue(aMessageClass,Clients) then
      Exit;

    Idx:=Clients.IndexOf(aListenerMethod);
    if Idx >= 0 then
      Clients.Remove(Idx)
    else
    begin
      {$ifdef DEBUG_SYSTEM_MESSAGING}
      __fresnel_console_log('TSimpleMessageManager.Unsubscribe NOT FOUND for ' + aMessageClass.ClassName);
      {$endif}
    end;
  finally
    UnLock;
  end;
end;

procedure TSimpleMessageManager.Unsubscribe(const aMessageClass: TClass; SubscriptionId: TMessageSubscriptionId; Immediate: Boolean);
var
  Clients : TMessageClientList;
  Idx : Integer;

begin
  Lock;
  try
     if Not FMessageClients.TryGetValue(aMessageClass,Clients) then
       exit;
     Idx:=Clients.IndexOf(SubscriptionId);
     if Idx >= 0 then
       Clients.Remove(Idx);
  finally
    UnLock;
  end;
end;

procedure TSimpleMessageManager.SendMessage(const Sender: TObject;
  aMessage: TMessageBase; aDispose: Boolean);

var
  Clients: TMessageClientList;
begin
  if (AMessage=nil) then exit;

  Lock;
  try
    try
      if GetList(aMessage.ClassType, Clients) then
        Clients.NotifyClients(Sender, aMessage);
    finally
      if ADispose then
        AMessage.Free;
    end;
  finally
    UnLock;
  end;
end;

{ TClientList }

constructor TMessageClientList.Create(aItemClass : TClass);
begin
  Inherited Create;
  Assert(aItemClass.InheritsFrom(TMessageClient));  // for backward compatibility
end;

destructor TMessageClientList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMessageClientList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TMessageClientList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    if FUpdateCount < 0 then
      raise EListError.Create('TMessageClientList.EndUpdate unbalanced');
    if FDisabledCount > cRemoveDisabledTreshold then
      RemoveDisabled;
  end;
end;

function TMessageClientList.Add(aId: Integer; const aListener: TMessageListener ): TMessageClient;
begin
  Result := TMessageListenerClient.Create(aId, aListener);
  AddClient(Result);
end;

function TMessageClientList.Add(aId: Integer; const aListenerMethod: TMessageListenerMethod): TMessageClient;
begin
  Result:=TMessageListenerMethodClient.Create(aID, aListenerMethod);
  AddClient(Result);
end;

procedure TMessageClientList.NotifyClients(const Sender: TObject; const aMessage: TMessageBase);
var
  lMessageClient: TMessageClient;
  i : Integer;
begin
  BeginUpdate;
  try
    for i:=0 to Count-1 do
    begin
      lMessageClient := FItems[i];
      if not lMessageClient.Disabled then
        lMessageClient.CallNotify(Sender, AMessage)
    end;
  finally
    EndUpdate;
  end;
end;

function TMessageClientList.IndexOf(const aClientID: TMessageSubscriptionId): integer;
var
  i : Integer;
  lItem : TMessageClient;
begin
  for i := 0 to Count-1 do
  begin
    lItem := Items[i];
    if (not lItem.Disabled) and (lItem.ClientID = aClientID) then
      Exit(i);
  end;
  Result := -1;
end;

function TMessageClientList.IndexOf(const aListener: TMessageListener): integer;
var
  i : Integer;
  lItem : TMessageClient;
begin
  for i := 0 to Count-1 do
  begin
    lItem := Items[i];
    if (not lItem.Disabled) and lItem.SameListener(aListener) then
      Exit(i);
  end;
  Result := -1;
end;

function TMessageClientList.IndexOf(const aListenerMethod: TMessageListenerMethod): integer;
var
  i : Integer;
  lItem : TMessageClient;
begin
  for i := 0 to Count-1 do
  begin
    lItem := Items[i];
    if (not lItem.Disabled) and lItem.SameListenerMethod(aListenerMethod) then
      Exit(i);
  end;
  Result := -1;
end;

procedure TMessageClientList.Remove(aIndex: Integer);

{
  We cannot just remove clients at once: when sending messages they must be
  sent in order of listener registration.
  But sending a message can result in a listener being deleted.
  This can change the indexes in the list if done incorrectly.

  So we can only delete when all messages have been processed.
}

begin
  if Cardinal(aIndex) >= Cardinal(Count) then
    raise ERangeError.CreateFmt('TMessageClientList.Remove invalid index (%d)', [ aIndex ]);

  // Begin/EnUpdate is here to "less delay" the delayed release
  // not sure if it's worth it, can probably be dropped
  BeginUpdate;
  try
    if FItems[aIndex].Disabled then
      raise EListError.CreateFmt('TMessageClientList.Remove already removed (%d)', [ aIndex ]);
    FItems[aIndex].Disabled := True;
    Inc(FDisabledCount);
  finally
    EndUpdate;
  end;
end;

procedure TMessageClientList.Clear;
var
  i : Integer;
begin
  if FUpdateCount > 0 then
    raise Exception.Create('TMessageClientList.Clear while an update is ongoing (NOT supported yet)');

  for i := 0 to Count-1 do
    FreeAndNil(FItems[i]);
  SetLength(FItems, 0);
  FDisabledCount := 0;
end;

function TMessageClientList.GetItems(aIndex: Integer): TMessageClient;
begin
  Result := FItems[aIndex];
end;

procedure TMessageClientList.AddClient(aClient: TMessageClient);
var
  lCapacity : Integer;
begin
  lCapacity := Length(FItems);
  if Count = lCapacity then
  begin
    lCapacity := lCapacity + (lCapacity shr 2) + 8;
    SetLength(FItems, lCapacity);
  end;

  FItems[FCount] := aClient;
  Inc(FCount);
end;

procedure TMessageClientList.RemoveDisabled;
var
  iSrc, iDest : Integer;
begin
  if FUpdateCount > 0 then
    raise Exception.Create('TMessageClientList.RemoveDisabled while an update is ongoing');

  iDest := 0;
  for iSrc := 0 to Count-1 do
  begin
    if FItems[iSrc].Disabled then
    begin
      FreeAndNil(FItems[iSrc]);
      Dec(FCount);
      Dec(FDisabledCount);
    end
    else
    begin
      if iSrc <> iDest then
        FItems[iDest] := FItems[iSrc];
      Inc(iDest);
    end;
  end;

  // if less 25% used, relinquish 50%
  if iDest < (Length(FItems) shr 2) then
  begin
    SetLength(FItems, Length(FItems) shr 1);
  end;
end;

{ TClient }

constructor TMessageClient.Create(aClientID: TMessageSubscriptionId);
begin
  inherited Create;
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

{$ifdef DECLARE_COMPATIBLEMANAGER}

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
begin
  Result := SameMethod(aListener, ListenerMethod);
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
  If Idx >= 0 then
    DoUnsubscribe(Idx);
end;

procedure TMessageManager.TListenerList.Unsubscribe(aListener: TMessageListener);

var
  Idx : integer;

begin
  Idx:=FList.Count-1;
  While (Idx>=0) and not FList[Idx].Matches(aListener) do
    Dec(Idx);
  If Idx >= 0 then
    DoUnsubscribe(Idx);
end;

procedure TMessageManager.TListenerList.Unsubscribe(aListener: TMessageListenerMethod);
var
  Idx : integer;

begin
  Idx:=FList.Count-1;
  While (Idx>=0) and not FList[Idx].Matches(aListener) do
    Dec(Idx);
  If Idx >= 0 then
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

{$endif DECLARE_COMPATIBLEMANAGER}

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

