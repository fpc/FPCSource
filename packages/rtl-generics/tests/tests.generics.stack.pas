unit tests.generics.stack;

{$mode objfpc}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Generics.Defaults, Generics.Collections;


Type
  TMySimpleStack = Class(Specialize TStack<String>);
{$IFDEF FPC}
  EList = EListError;
{$ENDIF}

  { TTestSimpleStack }

  TTestSimpleStack = Class(TTestCase)
  Private
    FStack : TMySimpleStack;
    FnotifyMessage : String;
    FCurrentValueNotify : Integer;
    FExpectValues : Array of String;
    FExpectValueAction: Array of TCollectionNotification;
    procedure DoAdd(aCount: Integer);
    procedure DoAdd2;
    Procedure DoneExpectValues;
    procedure DoGetValue(Match: String; ExceptionClass: TClass=nil);
    procedure DoValueNotify(ASender: TObject; const AItem: String; AAction: TCollectionNotification);
  Public
    Procedure SetExpectValues(aMessage : string; AKeys : Array of String; AActions : Array of TCollectionNotification; DoReverse : Boolean = False);
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Stack : TMySimpleStack Read FStack;
  Published
    Procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestClear;
    Procedure TestGetValue;
    Procedure TestPeek;
    Procedure TestPop;
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

  TSingleObjectStack = Class(Specialize TObjectStack<TMyObject>);

  { TTestSingleObjectStack }

  TTestSingleObjectStack = Class(TTestCase)
  private
    FOStack: TSingleObjectStack;
    FList : TFPList;
    procedure DoAdd(aID: Integer);
    procedure DoDestroy(Sender: TObject);
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Stack : TSingleObjectStack Read FOStack;
  Published
    Procedure TestEmpty;
    Procedure TestFreeOnPop;
    Procedure TestNoFreeOnPop;
  end;

implementation

{ TTestSingleObjectStack }

procedure TTestSingleObjectStack.SetUp;
begin
  FOStack:=TSingleObjectStack.Create(True);
  FList:=TFPList.Create;
  inherited SetUp;
end;

procedure TTestSingleObjectStack.TearDown;

Var
  I : integer;
  A : TObject;

begin
  FreeAndNil(FOStack);
  for I:=0 to FList.Count-1 do
    begin
    A:=TObject(FList[i]);
    A.Free;
    end;
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TTestSingleObjectStack.TestEmpty;
begin
  AssertNotNull('Have object',Stack);
  AssertEquals('Have empty object',0,Stack.Count);
end;

procedure TTestSingleObjectStack.DoAdd(aID : Integer);

Var
  O :  TMyObject;

begin
  O:=TMyObject.Create(aID,@DoDestroy);
  FOStack.Push(O);
  FList.Add(O);
end;

procedure TTestSingleObjectStack.DoDestroy(Sender: TObject);

Var
  I : Integer;

begin
  I:=FList.IndexOf(Sender);
  AssertTrue('Have object in Stack',I<>-1);
  FList.Delete(I);
end;

procedure TTestSingleObjectStack.TestFreeOnPop;

begin
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  Stack.Pop;
  AssertEquals('Have no obj',0,FList.Count);
end;

procedure TTestSingleObjectStack.TestNoFreeOnPop;
begin
  Stack.OwnsObjects:=False;
  DoAdd(1);
  AssertEquals('Have obj',1,FList.Count);
  Stack.Pop;
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

{ TTestSimpleStack }

procedure TTestSimpleStack.SetUp;
begin
  inherited SetUp;
  FStack:=TMySimpleStack.Create;
  FCurrentValueNotify:=0;
  FExpectValues:=[];
  FExpectValueAction:=[];
end;

procedure TTestSimpleStack.TearDown;
begin
  // So we don't get clear messages
  FStack.OnNotify:=Nil;
  FreeAndNil(FStack);
  inherited TearDown;
end;

procedure TTestSimpleStack.TestEmpty;
begin
  AssertNotNull('Have dictionary',Stack);
  AssertEquals('empty dictionary',0,Stack.Count);
end;

procedure TTestSimpleStack.DoAdd(aCount : Integer);

Var
  I : Integer;

begin
  For I:=1 to aCount do
    Stack.Push(IntToStr(i));
end;

procedure TTestSimpleStack.TestAdd;

begin
  DoAdd(1);
  AssertEquals('Count OK',1,Stack.Count);
  DoAdd(1);
  AssertEquals('Count OK',2,Stack.Count);
end;

procedure TTestSimpleStack.TestClear;
begin
  DoAdd(3);
  AssertEquals('Count OK',3,Stack.Count);
  Stack.Clear;
  AssertEquals('Count after clear OK',0,Stack.Count);
end;

procedure TTestSimpleStack.DoGetValue(Match: String; ExceptionClass: TClass);

Var
  EC : TClass;
  A,EM : String;

begin
  EC:=Nil;
  try
    A:=Stack.Pop;
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

procedure TTestSimpleStack.DoValueNotify(ASender: TObject; const AItem: String; AAction: TCollectionNotification);
begin
//  Writeln(FnotifyMessage+' value Notification',FCurrentValueNotify);
  AssertSame(FnotifyMessage+' value Correct sender', FStack,aSender);
  if (FCurrentValueNotify>=Length(FExpectValues)) then
    Fail(FnotifyMessage+' Too many value notificiations');
  AssertEquals(FnotifyMessage+' Notification value no '+IntToStr(FCurrentValueNotify),FExpectValues[FCurrentValueNotify],aItem);
  Inc(FCurrentValueNotify);
end;


procedure TTestSimpleStack.SetExpectValues(aMessage: string; AKeys: array of String;
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

procedure TTestSimpleStack.TestGetValue;

Var
  I : integer;

begin
  DoAdd(3);
  For I:=3 downto 1 do
    DoGetValue(IntToStr(I));
  DoGetValue('4',EArgumentOutOfRangeException);
end;

procedure TTestSimpleStack.TestPeek;
Var
  I : integer;

begin
  DoAdd(3);
  For I:=3 downto 1 do
    begin
    AssertEquals('Peek ',IntToStr(I),FStack.Peek);
    DoGetValue(IntToStr(I));
    end;
end;


procedure TTestSimpleStack.DoAdd2;

begin
  Stack.Push('A new 2');
end;

procedure TTestSimpleStack.DoneExpectValues;
begin
  AssertEquals(FnotifyMessage+' Expected number of values seen',Length(FExpectValues),FCurrentValueNotify);
end;

procedure TTestSimpleStack.TestPop;

Var
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  For I:=3 downto 1 do
    begin
    SI:=IntToStr(I);
    AssertEquals('Value '+SI,SI,FStack.Pop);
    end;
  AssertEquals('Count',0,Stack.Count);
end;

procedure TTestSimpleStack.TestToArray;

Var
  A : specialize TArray<String>;
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=Stack.ToArray;
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('Value '+SI,SI,A[i-1]);
    end;
end;


procedure TTestSimpleStack.TestEnumerator;

Var
  A : String;
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  I:=1;
  For A in Stack do
    begin
    SI:=IntToStr(i);
    AssertEquals('Value '+SI,SI,A);
    Inc(I);
    end;
end;

procedure TTestSimpleStack.TestValueNotification;
begin
  Stack.OnNotify:=@DoValueNotify;
  SetExpectValues('Add',['1','2','3'],[cnAdded,cnAdded,cnAdded]);
  DoAdd(3);
  DoneExpectValues;
end;

procedure TTestSimpleStack.TestValueNotificationDelete;
begin
  DoAdd(3);
  Stack.OnNotify:=@DoValueNotify;
  SetExpectValues('Clear',['3','2','1'],[cnRemoved,cnRemoved,cnRemoved],False);
  Stack.Clear;
  DoneExpectValues;
end;

begin
  RegisterTests([ TTestSimpleStack,TTestSingleObjectStack]);
end.

