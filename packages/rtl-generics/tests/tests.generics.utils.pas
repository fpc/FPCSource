{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2014 by Maciej Izak (hnb)
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit tests.generics.utils;

{$mode delphi}

interface

uses
  fpcunit, testutils, testregistry,
  Classes, SysUtils, Generics.Collections;

type
  TNotificationRec<T> = record
    Sender: TObject;
    Item: T;
    Action: TCollectionNotification;
    Executed: boolean;
  end;

  TNotificationNodeRec<TValue, TInfo> = record
    Sender: TObject;
    Key: string;
    Value: TValue;
    IgnoreNodePtr: boolean;
    Node: TCustomAVLTreeMap<string, TValue, TInfo>.PNode;
    Action: TCollectionNotification;
    Dispose: boolean;
    Executed: boolean;
  end;

  TTestCollections = class(TTestCase)
  private type
    TNotificationRec_String = TNotificationRec<string>;
    TNotificationRec_TObject = TNotificationRec<TObject>;
    TNotificationNodeRec_String = TNotificationNodeRec<string, TEmptyRecord>;
    TNotificationNodeRec_Empty = TNotificationNodeRec<TEmptyRecord, TEmptyRecord>;
    PNode_String = TCustomAVLTreeMap<string, string, TEmptyRecord>.PNode;
    PNode_Empty = TCustomAVLTreeMap<string, TEmptyRecord, TEmptyRecord>.PNode;
  private
    NotificationsListNode_String: TList<TNotificationNodeRec_String>;
    NotificationsListNode_Empty: TList<TNotificationNodeRec_Empty>;
    NotificationsListStr: TList<TNotificationRec_String>;
    NotificationsListObj: TList<TNotificationRec_TObject>;
    NotificationsIndex, NotificationsNodesIndex: Integer;
  protected
    procedure NotificationAdd(ASender: TObject; const AKey, AValue: string; ANode: PNode_String;
      AAction: TCollectionNotification; ADispose, AIgnoreNodePtr: boolean); overload;
    procedure NotificationAdd(ASender: TObject; const AKeys, AValues: array of string;
      const ANodes: array of PNode_String; AAction: TCollectionNotification; ADispose, AIgnoreNodePtr: boolean); overload;
    procedure NotificationAdd(ASender: TObject; const AKey: string; ANode: PNode_Empty;
      AAction: TCollectionNotification; ADispose, AIgnoreNodePtr: boolean); overload;
    procedure NotificationAdd(ASender: TObject; const AKeys: array of string;
      const ANodes: array of PNode_Empty; AAction: TCollectionNotification; ADispose, AIgnoreNodePtr: boolean); overload;
    procedure NotificationAdd(ASender: TObject; const AItem: string;
      AAction: TCollectionNotification); overload;
    procedure NotificationAdd(ASender: TObject; const AItems: array of string;
      AAction: TCollectionNotification); overload;
    procedure NotificationAdd(ASender: TObject; const AItem: TObject;
      AAction: TCollectionNotification); overload;
    procedure NotificationAdd(ASender: TObject; const AItems: array of TObject;
      AAction: TCollectionNotification); overload;
    procedure AssertNotificationsExecutedNodeStr;
    procedure ClearNotificationsNodeStr;
    procedure AssertNotificationsExecutedNodeEmpty;
    procedure ClearNotificationsNodeEmpty;
    procedure AssertNotificationsExecutedStr;
    procedure ClearNotificationsStr;
    procedure AssertNotificationsExecutedObj;
    procedure ClearNotificationsObj;
    procedure NotifyTestNodeStr(ASender: TObject; ANode: PNode_String; AAction: TCollectionNotification; ADispose: boolean);
    procedure NotifyTestNodeEmpty(ASender: TObject; ANode: PNode_Empty; AAction: TCollectionNotification; ADispose: boolean);
    procedure NotifyTestStr(ASender: TObject; const AItem: string; AAction: TCollectionNotification);
    procedure NotifyTestObj(ASender: TObject; const AItem: TObject; AAction: TCollectionNotification);

    procedure CreateObjects(var AArray: TArray<TObject>; ACount: Integer);
    procedure FreeObjects(AArray: TArray<TObject>);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TStringList = TList<string>;

  { TStringsEnumerator }

  TStringsEnumerator = class(TInterfacedObject, IEnumerator<string>)
  private
    FEnumerator: TStringList.TEnumerator;
    FCollection: TStringList;

    function GetCurrent: string;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: string read GetCurrent;

    constructor Create(AEnumerator: TStringList.TEnumerator; ACollection: TStringList);
    destructor Destroy; override;
  end;

  { TStringsEnumerable }

  TStringsEnumerable = class(TInterfacedObject, IEnumerable<string>)
  private
    FEnumerable: TStringList;

    function GetEnumerator: IEnumerator<string>;

    constructor Create(const AItems: array of string);
  end;

  TObjectList = TList<TObject>;

  { TObjectEnumerator }

  TObjectEnumerator = class(TInterfacedObject, IEnumerator<TObject>)
  private
    FEnumerator: TObjectList.TEnumerator;
    FCollection: TObjectList;

    function GetCurrent: TObject;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;

    constructor Create(AEnumerator: TObjectList.TEnumerator; ACollection: TObjectList);
    destructor Destroy; override;
  end;

  { TObjectEnumerable }

  TObjectEnumerable = class(TInterfacedObject, IEnumerable<TObject>)
  private
    FEnumerable: TObjectList;

    function GetEnumerator: IEnumerator<TObject>;

    constructor Create(const AItems: array of TObject);
  end;

function EnumerableStringsIntf(const AItems: array of string): IEnumerable<string>;
function EnumerableStringsObj(const AItems: array of string): TEnumerable<string>;
function EnumerableObjectsIntf(const AItems: array of TObject): IEnumerable<TObject>;
function EnumerableObjectsObj(const AItems: array of TObject): TEnumerable<TObject>;

implementation

function EnumerableStringsIntf(const AItems: array of string): IEnumerable<string>;
begin
  Result := TStringsEnumerable.Create(AItems);
end;

function EnumerableStringsObj(const AItems: array of string): TEnumerable<string>;
begin
  Result := TStringList.Create;
  TStringList(Result).AddRange(AItems);
end;

function EnumerableObjectsIntf(const AItems: array of TObject): IEnumerable<TObject>;
begin
  Result := TObjectEnumerable.Create(AItems);
end;

function EnumerableObjectsObj(const AItems: array of TObject): TEnumerable<TObject>;
begin
  Result := TObjectList.Create;
  TObjectList(Result).AddRange(AItems);
end;

{ TTestCollections }

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AKey, AValue: string; ANode: PNode_String; AAction: TCollectionNotification; ADispose, AIgnoreNodePtr: boolean);
var
  LNotification: TNotificationNodeRec_String;
begin
  LNotification.Sender := ASender;
  LNotification.Key := AKey;
  LNotification.Value := AValue;
  LNotification.IgnoreNodePtr := AIgnoreNodePtr;
  LNotification.Node := ANode;
  LNotification.Action := AAction;
  LNotification.Dispose := ADispose;
  LNotification.Executed := False;
  NotificationsListNode_String.Add(LNotification);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AKeys, AValues: array of string; const ANodes: array of PNode_String; AAction: TCollectionNotification; ADispose, AIgnoreNodePtr: boolean);
var
  i: Integer;
begin
  Assert(Length(AKeys) = Length(ANodes));
  for i := 0 to High(AKeys) do
    NotificationAdd(ASender, AKeys[i], AValues[i], ANodes[i], AAction, ADispose, AIgnoreNodePtr);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AKey: string; ANode: PNode_Empty; AAction: TCollectionNotification; ADispose, AIgnoreNodePtr: boolean);
var
  LNotification: TNotificationNodeRec_Empty;
begin
  LNotification.Sender := ASender;
  LNotification.Key := AKey;
  LNotification.Value := EmptyRecord;
  LNotification.IgnoreNodePtr := AIgnoreNodePtr;
  LNotification.Node := ANode;
  LNotification.Action := AAction;
  LNotification.Dispose := ADispose;
  LNotification.Executed := False;
  NotificationsListNode_Empty.Add(LNotification);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AKeys: array of string; const ANodes: array of PNode_Empty; AAction: TCollectionNotification; ADispose, AIgnoreNodePtr: boolean);
var
  i: Integer;
begin
  Assert(Length(AKeys) = Length(ANodes));
  for i := 0 to High(AKeys) do
    NotificationAdd(ASender, AKeys[i], ANodes[i], AAction, ADispose, AIgnoreNodePtr);
end;


procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AItem: string; AAction: TCollectionNotification);
var
  LNotification: TNotificationRec_String;
begin
  LNotification.Sender := ASender;
  LNotification.Item := AItem;
  LNotification.Action := AAction;
  LNotification.Executed := False;
  NotificationsListStr.Add(LNotification);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AItems: array of string; AAction: TCollectionNotification);
var
  s: string;
begin
  for s in AItems do
    NotificationAdd(ASender, s, AAction);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AItem: TObject; AAction: TCollectionNotification);
var
  LNotification: TNotificationRec_TObject;
begin
  LNotification.Sender := ASender;
  LNotification.Item := AItem;
  LNotification.Action := AAction;
  LNotification.Executed := False;
  NotificationsListObj.Add(LNotification);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AItems: array of TObject; AAction: TCollectionNotification);
var
  o: TObject;
begin
  for o in AItems do
    NotificationAdd(ASender, o, AAction);
end;

procedure TTestCollections.AssertNotificationsExecutedNodeStr;
var
  p: ^TNotificationNodeRec_String;
begin
  for p in NotificationsListNode_String.Ptr^ do
    AssertTrue(p^.Executed);
  AssertEquals(NotificationsNodesIndex, NotificationsListNode_String.Count);
  ClearNotificationsStr;
end;

procedure TTestCollections.ClearNotificationsNodeStr;
begin
  NotificationsListNode_String.Clear;
  NotificationsNodesIndex := 0;
end;

procedure TTestCollections.AssertNotificationsExecutedNodeEmpty;
var
  p: ^TNotificationNodeRec_Empty;
begin
  for p in NotificationsListNode_Empty.Ptr^ do
    AssertTrue(p^.Executed);
  AssertEquals(NotificationsNodesIndex, NotificationsListNode_Empty.Count);
  ClearNotificationsStr;
end;

procedure TTestCollections.ClearNotificationsNodeEmpty;
begin
  NotificationsListNode_Empty.Clear;
  NotificationsNodesIndex := 0;
end;

procedure TTestCollections.AssertNotificationsExecutedStr;
var
  p: ^TNotificationRec_String;
begin
  for p in NotificationsListStr.Ptr^ do
    AssertTrue(p^.Executed);
  AssertEquals(NotificationsIndex, NotificationsListStr.Count);
  ClearNotificationsStr;
end;

procedure TTestCollections.ClearNotificationsStr;
begin
  NotificationsListStr.Clear;
  NotificationsIndex := 0;
end;

procedure TTestCollections.AssertNotificationsExecutedObj;
var
  p: ^TNotificationRec_TObject;
begin
  for p in NotificationsListObj.Ptr^ do
    AssertTrue(p^.Executed);
  AssertEquals(NotificationsIndex, NotificationsListObj.Count);
  ClearNotificationsObj;
end;

procedure TTestCollections.ClearNotificationsObj;
begin
  NotificationsListObj.Clear;
  NotificationsIndex := 0;
end;

procedure TTestCollections.NotifyTestNodeStr(ASender: TObject; ANode: PNode_String; AAction: TCollectionNotification; ADispose: boolean);
var
  LNotification: TNotificationNodeRec_String;
begin
  AssertTrue(NotificationsNodesIndex < NotificationsListNode_String.Count);
  LNotification := NotificationsListNode_String[NotificationsNodesIndex];
  AssertTrue(ASender = LNotification.Sender);
  AssertEquals(ANode.Key, LNotification.Key);
  AssertEquals(ANode.Value, LNotification.Value);
  if not LNotification.IgnoreNodePtr then
    AssertSame(ANode, LNotification.Node);
  AssertTrue(AAction = LNotification.Action);
  AssertEquals(ADispose, LNotification.Dispose);
  AssertFalse(LNotification.Executed);
  LNotification.Executed := True;
  NotificationsListNode_String[NotificationsNodesIndex] := LNotification;
  Inc(NotificationsNodesIndex)
end;

procedure TTestCollections.NotifyTestNodeEmpty(ASender: TObject; ANode: PNode_Empty; AAction: TCollectionNotification; ADispose: boolean);
var
  LNotification: TNotificationNodeRec_Empty;
begin
  AssertTrue(NotificationsNodesIndex < NotificationsListNode_Empty.Count);
  LNotification := NotificationsListNode_Empty[NotificationsNodesIndex];
  AssertTrue(ASender = LNotification.Sender);
  AssertEquals(ANode.Key, LNotification.Key);
  if not LNotification.IgnoreNodePtr then
    AssertSame(ANode, LNotification.Node);
  AssertTrue(AAction = LNotification.Action);
  AssertEquals(ADispose, LNotification.Dispose);
  AssertFalse(LNotification.Executed);
  LNotification.Executed := True;
  NotificationsListNode_Empty[NotificationsNodesIndex] := LNotification;
  Inc(NotificationsNodesIndex)
end;

procedure TTestCollections.NotifyTestStr(ASender: TObject; const AItem: string; AAction: TCollectionNotification);
var
  LNotification: TNotificationRec_String;
begin
  AssertTrue(NotificationsIndex < NotificationsListStr.Count);
  LNotification := NotificationsListStr[NotificationsIndex];
  AssertTrue(ASender = LNotification.Sender);
  AssertEquals(AItem, LNotification.Item);
  AssertTrue(AAction = LNotification.Action);
  AssertFalse(LNotification.Executed);
  LNotification.Executed := True;
  NotificationsListStr[NotificationsIndex] := LNotification;
  Inc(NotificationsIndex)
end;

procedure TTestCollections.NotifyTestObj(ASender: TObject; const AItem: TObject; AAction: TCollectionNotification);
var
  LNotification: TNotificationRec_TObject;
begin
  AssertTrue(NotificationsIndex < NotificationsListObj.Count);
  LNotification := NotificationsListObj[NotificationsIndex];
  AssertTrue(ASender = LNotification.Sender);
  AssertTrue(AItem = LNotification.Item);
  AssertTrue(AAction = LNotification.Action);
  AssertFalse(LNotification.Executed);
  LNotification.Executed := True;
  NotificationsListObj[NotificationsIndex] := LNotification;
  Inc(NotificationsIndex)
end;

procedure TTestCollections.CreateObjects(var AArray: TArray<TObject>; ACount: Integer);
var
  i: Integer;
begin
  SetLength(AArray, ACount);
  for i := 0 to ACount - 1 do
    AArray[i] := TObject.Create;
end;

procedure TTestCollections.FreeObjects(AArray: TArray<TObject>);
var
  o: TObject;
begin
  for o in AArray do
    o.Free;
end;

constructor TTestCollections.Create;
begin
  inherited;
  NotificationsListStr := TList<TNotificationRec_String>.Create;
  NotificationsListObj := TList<TNotificationRec_TObject>.Create;
  NotificationsListNode_String := TList<TNotificationNodeRec_String>.Create;
  NotificationsListNode_Empty := TList<TNotificationNodeRec_Empty>.Create;
end;

destructor TTestCollections.Destroy;
begin
  NotificationsListNode_Empty.Free;
  NotificationsListNode_String.Free;
  NotificationsListObj.Free;
  NotificationsListStr.Free;
  inherited;
end;

{ TStringsEnumerable }

function TStringsEnumerable.GetEnumerator: IEnumerator<string>;
begin
  Result := TStringsEnumerator.Create(FEnumerable.GetEnumerator, FEnumerable);
end;

constructor TStringsEnumerable.Create(const AItems: array of string);
begin
  FEnumerable := TStringList.Create;
  FEnumerable.AddRange(AItems);
end;

{ TStringsEnumerator }

function TStringsEnumerator.GetCurrent: string;
begin
  Result := FEnumerator.Current;
end;

function TStringsEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

procedure TStringsEnumerator.Reset;
begin
  FEnumerator.Free;
  FEnumerator := FCollection.GetEnumerator;
end;

constructor TStringsEnumerator.Create(AEnumerator: TStringList.TEnumerator; ACollection: TStringList);
begin
  FEnumerator := AEnumerator;
  FCollection := ACollection;
end;

destructor TStringsEnumerator.Destroy;
begin
  FEnumerator.Free;
  inherited Destroy;
end;

{ TObjectEnumerable }

function TObjectEnumerable.GetEnumerator: IEnumerator<TObject>;
begin
  Result := TObjectEnumerator.Create(FEnumerable.GetEnumerator, FEnumerable);
end;

constructor TObjectEnumerable.Create(const AItems: array of TObject);
begin
  FEnumerable := TObjectList.Create;
  FEnumerable.AddRange(AItems);
end;

{ TObjectEnumerator }

function TObjectEnumerator.GetCurrent: TObject;
begin
  Result := FEnumerator.Current;
end;

function TObjectEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

procedure TObjectEnumerator.Reset;
begin
  FEnumerator.Free;
  FEnumerator := FCollection.GetEnumerator;
end;

constructor TObjectEnumerator.Create(AEnumerator: TObjectList.TEnumerator; ACollection: TObjectList);
begin
  FEnumerator := AEnumerator;
  FCollection := ACollection;
end;

destructor TObjectEnumerator.Destroy;
begin
  FEnumerator.Free;
  inherited Destroy;
end;

end.

