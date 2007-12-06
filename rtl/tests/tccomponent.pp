unit tccomponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  { TEventSink }

  TEventSink = Class(TObject)
    FEventCount : Integer;
    FLastSender : TObject;
    Procedure Event(Sender : TObject); virtual;
    Procedure ResetEvent;
  end;

  { TNotification }
  
  TNotification = Class(TCollectionItem)
  Public
    ASender,
    AComponent : TComponent;
    AOperation : TOperation;
  end;

  { TNotificationSink }

  TNotificationSink = Class(TObject)
  private
    Fevents : TCollection;
    function GetNot(Index : Integer): TNotification;
  Public
    Destructor Destroy; override;
    procedure Notification(Sender, AComponent: TComponent; Operation: TOperation); virtual;
    Procedure Reset;
    Function EventCount : Integer;
    Property Notifications [Index : Integer] : TNotification Read GetNot;
  end;

  { TMyComponent }

  TNotificationEvent = procedure (Sender : TComponent; AComponent: TComponent; Operation: TOperation) of object;

  TMyComponent = Class(TComponent)
  private
    FOnDestroy: TNotifyEvent;
    FOnNotify: TNotificationEvent;
  Public
    Destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Property OnDestroy : TNotifyEvent Read FOnDestroy Write FOnDestroy;
    Property OnNotification : TNotificationEvent Read FOnNotify Write FOnNotify;
  end;

  { TTestTComponentBase }

  TTestTComponentBase = class(TTestCase)
  protected
    FRoot : TMyComponent;
    Procedure CreateComponents(ACount : Integer);
    Procedure CreateComponents(ACount : Integer; Const BaseName : String);
    Procedure CreateComponents(ACount : Integer; AClass : TComponentClass);
    Procedure CreateComponents(ACount : Integer; AClass : TComponentClass; Const BaseName : String);
    procedure SetUp; override;
    procedure TearDown; override; 
  end;
  
  { TTestTComponent }

  TTestTComponent = Class(TTestTComponentBase)
  private
    procedure TestDoubleName;
    procedure TestTextName;
    procedure TestNumberName;
    procedure TestNumberTextName;
  Published
    Procedure TestCreate;
    Procedure TestName;
    procedure TestIdentiFierName;
    procedure TestIdentiFierNameTwo;
    procedure TestIdentiFierNameThree;
    procedure TestIdentiFierNameFour;
    procedure TestOwner;
    procedure TestChildren;
    Procedure TestDestroyChild;
    Procedure TestDestroyChildren;
    Procedure TestUniqueName;
    Procedure TestRemoveComponent;
  end;
  
  { TTestTComponentNotifies }

  TTestTComponentNotifies = Class(TTestTComponentBase)
  Protected
    N : TNotificationSink;
    procedure SetUp; override;
    procedure TearDown; override;
  Published
    Procedure TestInsertNotification;
    Procedure TestRemoveNotification;
  end;


implementation

procedure TTestTComponentBase.CreateComponents(ACount: Integer);
begin
  CreateComponents(ACount,'');
end;

procedure TTestTComponentBase.CreateComponents(ACount: Integer;
  const BaseName: String);
begin
  CreateComponents(ACount,TMyComponent,BaseName);
end;

procedure TTestTComponentBase.CreateComponents(ACount: Integer;
  AClass: TComponentClass);
begin
  CreateComponents(ACount,AClass,'');
end;

procedure TTestTComponentBase.CreateComponents(ACount: Integer;
  AClass: TComponentClass; const BaseName: String);
  
Var
  I : Integer;
  C : TComponent;
  
begin
  For I:=0 to ACount-1 do
    begin
    C:=TMyComponent.Create(FRoot);
    If (BaseName<>'') then
      C.Name:=BaseName+IntToStr(I+1);
    end;
end;

procedure TTestTComponentBase.SetUp; 
begin
  FRoot:=TMyComponent.Create(Nil);
  FRoot.Name:='Root';
end; 

procedure TTestTComponentBase.TearDown; 
begin
  FreeAndNil(FRoot);
end; 

{ TTestTComponent }

procedure TTestTComponent.TestCreate;
begin
  FreeAndNil(Froot);
  FRoot:=TMyComponent.Create(Nil);
  AssertEquals('Empty name','',FRoot.Name);
  AssertEquals('No owned components',0,FRoot.ComponentCount);
  If (FRoot.ComponentState<>[]) then
    Fail('Componentstate is not empty');
  If (FRoot.Owner<>Nil) then
    Fail('Owner is not nil');
end;

procedure TTestTComponent.TestName;
begin
  AssertEquals('Name is Root','Root',FRoot.Name);
end;

procedure TTestTComponent.TestOwner;

Var
  C : TComponent;

begin
  C:=TComponent.Create(FRoot);
  If (C.Owner<>FRoot) then
    Fail('Owner not saved after create');
end;

procedure TTestTComponent.TestChildren;
begin
  CreateComponents(3,'Child');
  AssertEquals('Componentcount is 3',3,FRoot.ComponentCount);
  AssertEquals('Child component 0 is child1','Child1',FRoot.Components[0].Name);
  AssertEquals('Child component 1 is child2','Child2',FRoot.Components[1].Name);
  AssertEquals('Child component 2 is child3','Child3',FRoot.Components[2].Name);
end;

procedure TTestTComponent.TestDestroyChild;

Var
  S : TEventSink;

begin
  CreateComponents(1);
  S:=TEventSink.Create;
  try
    TMyComponent(FRoot.Components[0]).OnDestroy:=@S.Event;
    FreeAndNil(FRoot);
    AssertEquals('One child destroyed',1,S.FEventcount);
    If (S.FLastSender=Nil) then
      Fail('No sender passed');
  finally
    S.Free;
  end;
end;

procedure TTestTComponent.TestDestroyChildren;

Var
  S : TEventSink;
  I : Integer;

begin
  CreateComponents(3);
  S:=TEventSink.Create;
  try
    For I:=0 to 2 do
      TMyComponent(FRoot.Components[I]).OnDestroy:=@S.Event;
    FreeAndNil(FRoot);
    AssertEquals('One child destroyed',3,S.FEventcount);
    If (S.FLastSender=Nil) then
      Fail('No sender passed');
  finally
    S.Free;
  end;
end;

procedure TTestTComponent.TestDoubleName;

begin
  FRoot.Components[1].Name:='Child1';
end;

procedure TTestTComponent.TestUniqueName;
begin
  CreateComponents(3,'Child');
  AssertException('Unique name',EComponentError,@TestDoubleName);
end;

procedure TTestTComponent.TestRemoveComponent;

Var
  C : TComponent;

begin
  CreateComponents(1);
  C:=FRoot.Components[0];
  FRoot.RemoveComponent(C);
  Try
    AssertEquals('No components left',0,FRoot.ComponentCount);
    AssertSame('Component has no owner',Nil,C.Owner);
  Finally
    C.Free;
  end;
end;


procedure TTestTComponent.TestTextName;

begin
  FRoot.Name:='Child 1';
end;

procedure TTestTComponent.TestNumberName;
begin
  FRoot.Name:='1';
end;

procedure TTestTComponent.TestNumberTextName;
begin
  FRoot.Name:='1Too';
end;

procedure TTestTComponent.TestIdentiFierName;
begin
  AssertException('Identifier name',EComponentError,@TestTextName);
end;

procedure TTestTComponent.TestIdentiFierNameTwo;

begin
  AssertException('Identifier name',EComponentError,@TestNumberTextName);
end;

procedure TTestTComponent.TestIdentiFierNameThree;
begin
  AssertException('Identifier name',EComponentError,@TestNumberName);
end;

procedure TTestTComponent.TestIdentiFierNameFour;

Var
  Failed : Boolean;

begin
  Failed:=False;
  Try
    FRoot.Name:='Some1';
  except
    Failed:=True;
  end;
  If Failed then
    Fail('No identifier ending on 1 accepted ?');
end;

{ TMyComponent }

destructor TMyComponent.Destroy;
begin
  If Assigned(FOnDestroy) then
    FOnDestroy(Self);
  inherited Destroy;
end;

procedure TMyComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  If Assigned(FOnNotify) then
    FOnNotify(Self, AComponent, Operation);
  inherited Notification(AComponent, Operation);
end;

{ TEventSink }

procedure TEventSink.Event(Sender: TObject);
begin
  Inc(FEventCount);
  FLastSender:=Sender;
end;

procedure TEventSink.ResetEvent;
begin
  FLastSender:=Nil;
  FEventCount:=0;
end;

{ TNotificationSink }

function TNotificationSink.GetNot(Index : Integer): TNotification;
begin
  If Assigned(FEvents) then
    Result:=Nil
  else
    Result:=TNotification(FEvents.Items[Index]);
end;

destructor TNotificationSink.Destroy;
begin
  FreeAndNil(FEvents);
  inherited Destroy;
end;

procedure TNotificationSink.Notification(Sender, AComponent: TComponent;
  Operation: TOperation);
  
Var
  N : TNotification;
  
begin
  If (Fevents=Nil) then
    FEvents:=TCollection.Create(TNotification);
  N:=FEvents.Add as TNotification;
  N.AComponent:=AComponent;
  N.ASender:=Sender;
  N.AOperation:=Operation;
end;

procedure TNotificationSink.Reset;
begin
  FreeAndNil(FEvents);
end;

function TNotificationSink.EventCount: Integer;
begin
  If (Fevents<>Nil) then
    Result:=FEvents.Count
  else
    Result:=0;
end;

{ TTestTComponentNotifies }

procedure TTestTComponentNotifies.SetUp;
begin
  inherited SetUp;
  N:=TNotificationSink.Create;
  FRoot.OnNotification:=@N.Notification;
end;

procedure TTestTComponentNotifies.TearDown;
begin
  FreeAndNil(N);
  inherited TearDown;
end;

procedure TTestTComponentNotifies.TestInsertNotification;

Var
  C : TComponent;
  E : TNotification;

begin
  CreateComponents(1);
  AssertEquals('One notification received',1,N.EventCount);
  E:=N.Notifications[0];
  AssertEquals('Insert notification received',Ord(opInsert),Ord(E.AOperation));
end;

procedure TTestTComponentNotifies.TestRemoveNotification;

Var
  C : TComponent;
  E : TNotification;

begin
  CreateComponents(1);
  N.Reset;
  C:=FRoot.Components[0];
  FRoot.RemoveComponent(C);
  Try
    AssertEquals('One notification received',1,N.EventCount);
    E:=N.Notifications[0];
  Finally
    C.Free;
  end;
end;


initialization

  RegisterTests([TTestTComponent,TTestTComponentNotifies]);
end.

