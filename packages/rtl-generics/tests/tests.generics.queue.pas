unit tests.generics.queue;

{$mode objfpc}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Generics.Defaults, Generics.Collections;


Type
  TMySimpleQueue = Class(Specialize TQueue<String>);
{$IFDEF FPC}
  EList = EListError;
{$ENDIF}

  { TTestSimpleQueue }

  TTestSimpleQueue = Class(TTestCase)
  Private
    FQueue : TMySimpleQueue;
    FnotifyMessage : String;
    FCurrentValueNotify : Integer;
    FExpectValues : Array of String;
    FExpectValueAction: Array of TCollectionNotification;
    procedure DoAdd(aCount: Integer; aOffset: Integer=0);
    procedure DoAdd2;
    Procedure DoneExpectValues;
    procedure DoGetValue(Match: String; ExceptionClass: TClass=nil);
    procedure DoValueNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: String; AAction: TCollectionNotification);
  Public
    Procedure SetExpectValues(aMessage : string; AKeys : Array of String; AActions : Array of TCollectionNotification; DoReverse : Boolean = False);
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Queue : TMySimpleQueue Read FQueue;
  Published
    Procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestClear;
    Procedure TestGetValue;
    Procedure TestPeek;
    Procedure TestDequeue;
    Procedure TestToArray;
    Procedure TestEnumerator;
    procedure TestValueNotification;
    procedure TestValueNotificationDelete;
  end;

  { TMyObject }

  TMyObject = Class(TObject)
  Private
    fOnDestroy : TNotifyEvent;
    FID : Integer;
  public
    Constructor Create(aID : Integer; aOnDestroy : TNotifyEvent);
    destructor destroy; override;
    Property ID : Integer Read FID;
  end;

  TSingleObjectQueue = Class(Specialize TObjectQueue<TMyObject>);

  { TTestSingleObjectQueue }

  TTestSingleObjectQueue = Class(TTestCase)
  private
    FOQueue: TSingleObjectQueue;
    FList : TFPList;
    procedure DoAdd(aID: Integer);
    procedure DoDestroy(Sender: TObject);
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Queue : TSingleObjectQueue Read FOQueue;
  Published
    Procedure TestEmpty;
    Procedure TestFreeOnDequeue;
    Procedure TestNoFreeOnDeQueue;
  end;

implementation

{ TTestSingleObjectQueue }

procedure TTestSingleObjectQueue.SetUp;
begin
  FOQueue:=TSingleObjectQueue.Create(True);
  FList:=TFPList.Create;
  inherited SetUp;
end;

procedure TTestSingleObjectQueue.TearDown;
begin
  FreeAndNil(FOQueue);
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TTestSingleObjectQueue.TestEmpty;
begin
  AssertNotNull('Have object',Queue);
  AssertEquals('Have empty object',0,Queue.Count);
end;

procedure TTestSingleObjectQueue.DoAdd(aID : Integer);

Var
  O :  TMyObject;

begin
  O:=TMyObject.Create(aID,@DoDestroy);
  FOQueue.EnQueue(O);
  FList.Add(O);
end;

procedure TTestSingleObjectQueue.DoDestroy(Sender: TObject);

Var
  I : Integer;

begin
  I:=FList.IndexOf(Sender);
  AssertTrue('Have object in Queue',I<>-1);
  FList.Delete(I);
end;

procedure TTestSingleObjectQueue.TestFreeOnDeQueue;

begin
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  Queue.Dequeue;
  AssertEquals('Have no obj',0,FList.Count);
end;

procedure TTestSingleObjectQueue.TestNoFreeOnDeQueue;
begin
  Queue.OwnsObjects:=False;
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  Queue.DeQueue;
  AssertEquals('Have  obj',1,FList.Count);
end;


{ TMyObject }

constructor TMyObject.Create(aID: Integer; aOnDestroy: TNotifyEvent);
begin
  FOnDestroy:=aOnDestroy;
  FID:=AID;
end;

destructor TMyObject.destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  inherited destroy;
end;

{ TTestSimpleQueue }

procedure TTestSimpleQueue.SetUp;
begin
  inherited SetUp;
  FQueue:=TMySimpleQueue.Create;
  FCurrentValueNotify:=0;
  FExpectValues:=[];
  FExpectValueAction:=[];
end;

procedure TTestSimpleQueue.TearDown;
begin
  // So we don't get clear messages
  FQueue.OnNotify:=Nil;
  FreeAndNil(FQueue);
  inherited TearDown;
end;

procedure TTestSimpleQueue.TestEmpty;
begin
  AssertNotNull('Have dictionary',Queue);
  AssertEquals('empty dictionary',0,Queue.Count);
end;

procedure TTestSimpleQueue.DoAdd(aCount : Integer; aOffset : Integer=0);

Var
  I : Integer;

begin
  if aOffset=-1 then
    aOffset:=Queue.Count;
  For I:=aOffset+1 to aOffset+aCount do
    Queue.EnQueue(IntToStr(i));
end;

procedure TTestSimpleQueue.TestAdd;

begin
  DoAdd(1);
  AssertEquals('Count OK',1,Queue.Count);
  DoAdd(1,1);
  AssertEquals('Count OK',2,Queue.Count);
end;

procedure TTestSimpleQueue.TestClear;
begin
  DoAdd(3);
  AssertEquals('Count OK',3,Queue.Count);
  Queue.Clear;
  AssertEquals('Count after clear OK',0,Queue.Count);
end;

procedure TTestSimpleQueue.DoGetValue(Match: String; ExceptionClass: TClass);

Var
  EC : TClass;
  A,EM : String;

begin
  EC:=Nil;
  try
    A:=Queue.DeQueue;
  except
    On E : Exception do
      begin
      EC:=E.ClassType;
      EM:=E.Message;
      end
  end;
  if ExceptionClass=Nil then
    begin
    if EC<>Nil then
      Fail('Got exception '+EC.ClassName+' with message: '+EM);
    AssertEquals('Value is correct',Match,A)
    end
  else
    begin
    if EC=Nil then
      Fail('Expected exception '+ExceptionClass.ClassName+' but got none');
    if EC<>ExceptionClass then
      Fail('Expected exception class '+ExceptionClass.ClassName+' but got '+EC.ClassName+' with message '+EM);
    end;
end;

procedure TTestSimpleQueue.DoValueNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: String; AAction: TCollectionNotification);
begin
//  Writeln(FnotifyMessage+' value Notification',FCurrentValueNotify);
  AssertSame(FnotifyMessage+' value Correct sender', FQueue,aSender);
  if (FCurrentValueNotify>=Length(FExpectValues)) then
    Fail(FnotifyMessage+' Too many value notificiations');
  AssertEquals(FnotifyMessage+' Notification value no '+IntToStr(FCurrentValueNotify),FExpectValues[FCurrentValueNotify],aItem);
  Inc(FCurrentValueNotify);
end;


procedure TTestSimpleQueue.SetExpectValues(aMessage: string; AKeys: array of String;
  AActions: array of TCollectionNotification; DoReverse: Boolean);
Var
  I,L : integer;

begin
  FnotifyMessage:=aMessage;
  FCurrentValueNotify:=0;
  L:=Length(aKeys);
  AssertEquals('SetExpectValues: Lengths arrays equal',l,Length(aActions));
  SetLength(FExpectValues,L);
  SetLength(FExpectValueAction,L);
  Dec(L);
  if DoReverse then
    For I:=0 to L do
      begin
      FExpectValues[L-i]:=AKeys[i];
      FExpectValueAction[L-i]:=AActions[I];
      end
  else
    For I:=0 to L do
      begin
      FExpectValues[i]:=AKeys[i];
      FExpectValueAction[i]:=AActions[I];
      end;
end;

procedure TTestSimpleQueue.TestGetValue;

Var
  I : integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    DoGetValue(IntToStr(I));
  DoGetValue('4',EArgumentOutOfRangeException);
end;

procedure TTestSimpleQueue.TestPeek;
Var
  I : integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    begin
    AssertEquals('Peek ',IntToStr(I),FQueue.Peek);
    DoGetValue(IntToStr(I));
    end;
end;


procedure TTestSimpleQueue.DoAdd2;

begin
  Queue.Enqueue('A new 2');
end;

procedure TTestSimpleQueue.DoneExpectValues;
begin
  AssertEquals(FnotifyMessage+' Expected number of values seen',Length(FExpectValues),FCurrentValueNotify);
end;

procedure TTestSimpleQueue.TestDequeue;

begin
  DoAdd(3);
  AssertEquals('1',Queue.Dequeue);
  AssertEquals('Count',2,Queue.Count);
end;

procedure TTestSimpleQueue.TestToArray;

Var
  A : specialize TArray<String>;

  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=Queue.ToArray;
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('Value '+SI,SI,A[i-1]);
    end;
end;


procedure TTestSimpleQueue.TestEnumerator;

Var
  A : String;
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  I:=1;
  For A in Queue do
    begin
    SI:=IntToStr(I);
    AssertEquals('Value '+SI,SI,A);
    Inc(I);
    end;
end;

procedure TTestSimpleQueue.TestValueNotification;
begin
  Queue.OnNotify:=@DoValueNotify;
  SetExpectValues('Add',['1','2','3'],[cnAdded,cnAdded,cnAdded]);
  DoAdd(3);
  DoneExpectValues;
end;

procedure TTestSimpleQueue.TestValueNotificationDelete;
begin
  DoAdd(3);
  Queue.OnNotify:=@DoValueNotify;
  SetExpectValues('Clear',['1','2','3'],[cnRemoved,cnRemoved,cnRemoved]);
  Queue.Clear;
  DoneExpectValues;
end;

begin
  RegisterTests([ TTestSimpleQueue,TTestSingleObjectQueue]);
end.

