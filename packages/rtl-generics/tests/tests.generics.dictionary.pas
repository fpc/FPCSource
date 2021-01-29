unit tests.generics.dictionary;

{$mode objfpc}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Generics.Defaults, Generics.Collections;

Type
  TMySimpleDict = Class(Specialize TDictionary<Integer,String>);
{$IFDEF FPC}
  EDictionary = EListError;
  TMyPair = specialize TPair<Integer,String>;
{$ENDIF}
  { TTestSimpleDictionary }

  TTestSimpleDictionary = Class(TTestCase)
  Private
    FDict : TMySimpleDict;
    FnotifyMessage : String;
    FCurrentKeyNotify : Integer;
    FCurrentValueNotify : Integer;
    FExpectKeys : Array of Integer;
    FExpectValues : Array of String;
    FExpectValueAction,
    FExpectKeyAction: Array of TCollectionNotification;
    procedure DoAdd(aCount: Integer; aOffset: Integer=0);
    procedure DoAdd2;
    Procedure DoneExpectKeys;
    Procedure DoneExpectValues;
    procedure DoGetValue(aKey: Integer; Match: String; ExceptionClass: TClass=nil);
    procedure DoKeyNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: Integer; AAction: TCollectionNotification);
    procedure DoValueNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: String; AAction: TCollectionNotification);
  Public
    Procedure SetExpectKeys(aMessage : string; AKeys : Array of Integer; AActions : Array of TCollectionNotification; DoReverse : Boolean = False);
    Procedure SetExpectValues(aMessage : string; AKeys : Array of String; AActions : Array of TCollectionNotification; DoReverse : Boolean = False);
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Dict : TMySimpleDict Read FDict;
  Published
    Procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestClear;
    Procedure TestTryGetValue;
    Procedure TestGetValue;
    Procedure TestSetValue;
    Procedure TestAddDuplicate;
    Procedure TestAddOrSet;
    Procedure TestTryAdd;
    Procedure TestContainsKey;
    Procedure TestContainsValue;
    Procedure TestDelete;
    Procedure TestToArray;
    procedure TestKeys;
    Procedure TestValues;
    Procedure TestEnumerator;
    Procedure TestNotification;
    procedure TestNotificationDelete;
    procedure TestValueNotification;
    procedure TestValueNotificationDelete;
    procedure TestKeyValueNotificationSet;
  end;

implementation

{ TTestSimpleDictionary }

procedure TTestSimpleDictionary.SetUp;
begin
  inherited SetUp;
  FDict:=TMySimpleDict.Create;
  FCurrentKeyNotify:=0;
  FCurrentValueNotify:=0;
  FExpectKeys:=[];
  FExpectKeyAction:=[];
  FExpectValues:=[];
  FExpectValueAction:=[];
end;

procedure TTestSimpleDictionary.TearDown;
begin
  // So we don't get clear messages
  FDict.OnKeyNotify:=Nil;
  FDict.OnValueNotify:=Nil;
  FreeAndNil(FDict);
  inherited TearDown;
end;

procedure TTestSimpleDictionary.TestEmpty;
begin
  AssertNotNull('Have dictionary',Dict);
  AssertEquals('empty dictionary',0,Dict.Count);
end;

procedure TTestSimpleDictionary.DoAdd(aCount : Integer; aOffset : Integer=0);

Var
  I : Integer;

begin
  if aOffset=-1 then
    aOffset:=Dict.Count;
  For I:=aOffset+1 to aOffset+aCount do
    Dict.Add(I,IntToStr(i));
end;

procedure TTestSimpleDictionary.TestAdd;

begin
  DoAdd(1);
  AssertEquals('Count OK',1,Dict.Count);
  AssertTrue('Has added value',Dict.ContainsKey(1));
  DoAdd(1,1);
  AssertEquals('Count OK',2,Dict.Count);
  AssertTrue('Has added value',Dict.ContainsKey(2));
end;

procedure TTestSimpleDictionary.TestClear;
begin
  DoAdd(3);
  AssertEquals('Count OK',3,Dict.Count);
  Dict.Clear;
  AssertEquals('Count after clear OK',0,Dict.Count);
end;

procedure TTestSimpleDictionary.TestTryGetValue;

Var
  I : integer;
  SI,A : string;

begin
  DoAdd(3);
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertTrue('Have value '+SI,Dict.TryGetValue(I,A));
    AssertEquals('Value is correct '+SI,SI,A);
    end;
  AssertFalse('Have no value 4',Dict.TryGetValue(4,A));
end;

procedure TTestSimpleDictionary.DoGetValue(aKey: Integer; Match: String; ExceptionClass: TClass);

Var
  EC : TClass;
  A,EM : String;

begin
  EC:=Nil;
  try
    A:=Dict.Items[aKey];
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
    AssertEquals('Value is correct for '+IntToStr(aKey),Match,A)
    end
  else
    begin
    if EC=Nil then
      Fail('Expected exception '+ExceptionClass.ClassName+' but got none');
    if EC<>ExceptionClass then
      Fail('Expected exception class '+ExceptionClass.ClassName+' but got '+EC.ClassName+' with message '+EM);
    end;
end;

procedure TTestSimpleDictionary.DoKeyNotify(ASender: TObject;  {$ifdef fpc}constref{$else}const{$endif}  AItem: Integer; AAction: TCollectionNotification);
begin
  Writeln(FnotifyMessage+' Notification',FCurrentKeyNotify);
  AssertSame(FnotifyMessage+' Correct sender', FDict,aSender);
  if (FCurrentKeyNotify>=Length(FExpectKeys)) then
    Fail(FnotifyMessage+' Too many notificiations');
  AssertEquals(FnotifyMessage+' Notification Key no '+IntToStr(FCurrentKeyNotify),FExpectKeys[FCurrentKeyNotify],aItem);
  Inc(FCurrentKeyNotify);
end;

procedure TTestSimpleDictionary.DoValueNotify(ASender: TObject; {$ifdef fpc}constref{$else}const{$endif} AItem: String; AAction: TCollectionNotification);
begin
  Writeln(FnotifyMessage+' value Notification',FCurrentValueNotify);
  AssertSame(FnotifyMessage+' value Correct sender', FDict,aSender);
  if (FCurrentValueNotify>=Length(FExpectValues)) then
    Fail(FnotifyMessage+' Too many value notificiations');
  AssertEquals(FnotifyMessage+' Notification value no '+IntToStr(FCurrentValueNotify),FExpectValues[FCurrentValueNotify],aItem);
  Inc(FCurrentValueNotify);
end;

procedure TTestSimpleDictionary.SetExpectKeys(aMessage: string; AKeys: array of Integer;
  AActions: array of TCollectionNotification; DoReverse: Boolean = False);

Var
  I,L : integer;

begin
  FnotifyMessage:=aMessage;
  FCurrentKeyNotify:=0;
  L:=Length(aKeys);
  AssertEquals('SetExpectkeys: Lengths arrays equal',l,Length(aActions));
  SetLength(FExpectKeys,L);
  SetLength(FExpectKeyAction,L);
  Dec(L);
  if DoReverse then
    For I:=0 to L do
      begin
      FExpectKeys[L-i]:=AKeys[i];
      FExpectKeyAction[L-i]:=AActions[I];
      end
  else
    For I:=0 to L do
      begin
      FExpectKeys[i]:=AKeys[i];
      FExpectKeyAction[i]:=AActions[I];
      end;
end;

procedure TTestSimpleDictionary.SetExpectValues(aMessage: string; AKeys: array of String;
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

procedure TTestSimpleDictionary.TestGetValue;

Var
  I : integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    DoGetValue(I,IntToStr(I));
  DoGetValue(4,'4',EDictionary);
end;

procedure TTestSimpleDictionary.TestSetValue;
begin
  TestGetValue;
  Dict.Items[3]:='Six';
  DoGetValue(3,'Six');
end;

procedure TTestSimpleDictionary.DoAdd2;

begin
  Dict.Add(2,'A new 2');
end;

procedure TTestSimpleDictionary.DoneExpectKeys;
begin
  AssertEquals(FnotifyMessage+' Expected number of keys seen',Length(FExpectKeys),FCurrentKeyNotify);
end;

procedure TTestSimpleDictionary.DoneExpectValues;
begin
  AssertEquals(FnotifyMessage+' Expected number of values seen',Length(FExpectValues),FCurrentValueNotify);
end;

procedure TTestSimpleDictionary.TestAddDuplicate;
begin
  DoAdd(3);
  AssertException('Cannot add duplicate',EDictionary,@DoAdd2);
end;

procedure TTestSimpleDictionary.TestAddOrSet;

begin
  DoAdd(3);
  Dict.AddOrSetValue(2,'a new 2');
  DoGetValue(2,'a new 2');
end;

procedure TTestSimpleDictionary.TestTryAdd;
begin
  AssertTrue(Dict.TryAdd(1, 'Foobar'));
  AssertFalse(Dict.TryAdd(1, 'Foo'));
  AssertTrue(Dict.TryAdd(2, 'Bar'));
end;

procedure TTestSimpleDictionary.TestContainsKey;

Var
  I : Integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    AssertTrue('Has '+IntToStr(i),Dict.ContainsKey(I));
  AssertFalse('Has not 4',Dict.ContainsKey(4));
end;

procedure TTestSimpleDictionary.TestContainsValue;

Var
  I : Integer;

begin
  DoAdd(3);
  For I:=1 to 3 do
    AssertTrue('Has '+IntToStr(i),Dict.ContainsValue(IntToStr(i)));
  AssertFalse('Has not 4',Dict.ContainsValue('4'));
end;

procedure TTestSimpleDictionary.TestDelete;

begin
  DoAdd(3);
  Dict.Remove(2);
  AssertEquals('Count',2,Dict.Count);
  AssertFalse('Has not 2',Dict.ContainsKey(2));
end;

procedure TTestSimpleDictionary.TestToArray;

Var
{$ifdef fpc}
  A : specialize TArray<TMyPair>;
{$else}
  A : specialize TArray<TMySimpleDict.TMyPair>;
{$endif}
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=Dict.ToArray;
  specialize TArrayHelper<TMyPair>.Sort(A{$ifndef fpc}, specialize TComparer<TMySimpleDict.TMyPair>.Default{$endif});
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('key '+SI,I,A[i-1].Key);
    AssertEquals('Value '+SI,SI,A[i-1].Value);
    end;
end;

procedure TTestSimpleDictionary.TestKeys;

Var
  A : Array of Integer;
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=Dict.Keys.ToArray;
  specialize TArrayHelper<Integer>.Sort(A{$ifndef fpc}, specialize TComparer<Integer>.Default{$endif});
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('key '+SI,I,A[i-1]);
    end;
end;

procedure TTestSimpleDictionary.TestValues;
Var
  A : Array of String;
  I : Integer;
  SI : String;

begin
  DoAdd(3);
  A:=Dict.Values.ToArray;
  specialize TArrayHelper<String>.Sort(A{$ifndef fpc}, specialize TComparer<String>.Default{$endif});
  AssertEquals('Length Ok',3,Length(A));
  For I:=1 to 3 do
    begin
    SI:=IntToStr(I);
    AssertEquals('Value '+SI,SI,A[i-1]);
    end;
end;

procedure TTestSimpleDictionary.TestEnumerator;
type
  TStringList = specialize TList<String>;
  TIntegerList = specialize TList<Integer>;

Var
{$ifdef fpc}
  A : TMyPair;
{$else}
  A : TMySimpleDict.TMyPair;
{$endif}
  I,J : Integer;
  SI : String;
  IL: TIntegerList;
  SL: TStringList;
begin
  DoAdd(3);
  IL:=Nil;
  SL:=TStringList.Create;
  try
    IL:=TIntegerList.Create;
    for I:=1 to 3 do begin
      IL.Add(I);
      SL.Add(IntToStr(I));
    end;
    I:=1;
    For A in Dict do
      begin
      SI:=IntToStr(I);
      J:=IL.IndexOf(A.Key);
      AssertTrue('key '+SI,J>=0);
      IL.Delete(J);
      J:=SL.IndexOf(A.Value);
      AssertTrue('value '+SI,J>=0);
      SL.Delete(J);
      Inc(I);
      end;
  finally
    IL.Free;
    SL.Free;
  end;
end;

procedure TTestSimpleDictionary.TestNotification;
begin
  Dict.OnKeyNotify:=@DoKeyNotify;
  SetExpectKeys('Add',[1,2,3],[cnAdded,cnAdded,cnAdded]);
  DoAdd(3);
  DoneExpectKeys;
end;

procedure TTestSimpleDictionary.TestNotificationDelete;

begin
  DoAdd(3);
  Dict.OnKeyNotify:=@DoKeyNotify;
  SetExpectKeys('Clear',[1,2,3],[cnRemoved,cnRemoved,cnRemoved],{$IFDEF FPC}true{$ELSE}False{$endif});
  Dict.Clear;
  DoneExpectKeys;
end;

procedure TTestSimpleDictionary.TestValueNotification;
begin
  Dict.OnValueNotify:=@DoValueNotify;
  SetExpectValues('Add',['1','2','3'],[cnAdded,cnAdded,cnAdded]);
  DoAdd(3);
  DoneExpectValues;
end;

procedure TTestSimpleDictionary.TestValueNotificationDelete;
begin
  DoAdd(3);
  Dict.OnValueNotify:=@DoValueNotify;
  SetExpectValues('Clear',['1','2','3'],[cnRemoved,cnRemoved,cnRemoved],{$IFDEF FPC}true{$ELSE}False{$endif});
  Dict.Clear;
  DoneExpectValues;
end;

procedure TTestSimpleDictionary.TestKeyValueNotificationSet;
begin
  DoAdd(3);
  Dict.OnValueNotify:=@DoValueNotify;
  Dict.OnKeyNotify:=@DoKeyNotify;
  SetExpectValues('Set',['2','Six'],[cnRemoved,cnAdded]);
  SetExpectKeys('Set',[],[]);
  Dict[2]:='Six';
  DoneExpectKeys;
  DoneExpectValues;
end;

begin
  RegisterTest(TTestSimpleDictionary);
end.

