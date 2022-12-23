{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2018 by Maciej Izak (hnb),
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types, tests and major refactoring of entire library

    Thanks to Castle Game Engine (https://castle-engine.sourceforge.io)
    Part of tests for this module was copied from Castle Game Engine tests

 **********************************************************************}

unit tests.generics.hashmaps;

{$mode delphi}
{$MACRO ON}

interface

uses
  fpcunit, testregistry, testutils, tests.generics.utils,
  typinfo, Classes, SysUtils, StrUtils, Generics.Collections, Generics.Defaults;

type
  PCollectionNotification = ^TCollectionNotification;

  { TTestHashMaps }

  TTestHashMaps= class(TTestCollections)
  private
    procedure CountAsKey_Check(const AWhat: string; AValue, AExpectedValue: Integer;
      AAction: PCollectionNotification);
    procedure CountAsKey_Notify(const AKind: string; ASender: TObject; const AItem: Integer; AAction: TCollectionNotification);
    procedure CountAsKey_NotifyValue(ASender: TObject; const AItem: Integer; AAction: TCollectionNotification);
    procedure CountAsKey_NotifyKey(ASender: TObject; const AItem: Integer; AAction: TCollectionNotification);
  published
    procedure Test_CountAsKey_OpenAddressingLP;
    procedure Test_CountAsKey_OpenAddressingLPT;
    procedure Test_CountAsKey_OpenAddressingQP;
    procedure Test_CountAsKey_OpenAddressingDH;
    procedure Test_CountAsKey_CuckooD2;
    procedure Test_CountAsKey_CuckooD4;
    procedure Test_CountAsKey_CuckooD6;

    procedure Test_OpenAddressingLP_Notification;
    procedure Test_OpenAddressingLPT_Notification;
    procedure Test_OpenAddressingQP_Notification;
    procedure Test_OpenAddressingDH_Notification;
    procedure Test_CuckooD2_Notification;
    procedure Test_CuckooD4_Notification;
    procedure Test_CuckooD6_Notification;

    procedure Test_OpenAddressingLP_TrimExcess;
    procedure Test_CuckooD2_TrimExcess;

    procedure Test_ObjectDictionary;

    procedure Test_TryAddOrSetOrGetValue;
    procedure Test_TryGetValueEmpty_xxHash32;
    procedure Test_TryGetValueEmpty_xxHash32Pascal;
  end;

implementation

{ TTestHashMaps }

procedure TTestHashMaps.CountAsKey_Check(const AWhat: string; AValue, AExpectedValue: Integer;
    AAction: PCollectionNotification);
var
  LCollectionNotificationStr: string;
begin
  if Assigned(AAction) then
    LCollectionNotificationStr := GetEnumName(TypeInfo(TCollectionNotification), Ord(AAction^));

  AssertEquals(AWhat + LCollectionNotificationStr, AExpectedValue, AValue);
end;

procedure TTestHashMaps.CountAsKey_Notify(const AKind: string; ASender: TObject; const
  AItem: Integer; AAction: TCollectionNotification);
var
  LCount: Integer;
begin
  CountAsKey_Check('Item ('+AKind+')', AItem, 0, @AAction);
  LCount := TCustomDictionary<Integer, Integer, TDefaultHashFactory>(ASender).Count;
  case AAction of
    cnAdded:
      CountAsKey_Check('Count', LCount, 1, @AAction);
    cnRemoved:
      CountAsKey_Check('Count', LCount, 0, @AAction);
    cnExtracted: Halt(4);
  end;
end;

procedure TTestHashMaps.CountAsKey_NotifyValue(ASender: TObject; const AItem: Integer;
  AAction: TCollectionNotification);
begin
  CountAsKey_Notify('Value', ASender, AItem, AAction);
end;

procedure TTestHashMaps.CountAsKey_NotifyKey(ASender: TObject; const AItem: Integer;
  AAction: TCollectionNotification);
begin
  CountAsKey_Notify('Key', ASender, AItem, AAction);
end;

{$DEFINE TEST_COUNT_AS_KEY :=
  LDictionary.OnKeyNotify := CountAsKey_NotifyKey;
  LDictionary.OnValueNotify := CountAsKey_NotifyValue;
  CountAsKey_Check('Count', LDictionary.Count, 0, nil);
  LDictionary.Add(LDictionary.Count,LDictionary.Count);
  CountAsKey_Check('Item', LDictionary[0], 0, nil);
  LDictionary.Free
}

procedure TTestHashMaps.Test_CountAsKey_OpenAddressingLP;
var
  LDictionary: TOpenAddressingLP<Integer, Integer>;
begin
  // TOpenAddressingLP
  LDictionary := TOpenAddressingLP<Integer, Integer>.Create;
  TEST_COUNT_AS_KEY;
end;

procedure TTestHashMaps.Test_CountAsKey_OpenAddressingLPT;
var
  LDictionary: TOpenAddressingLPT<Integer, Integer>;
begin
  // TOpenAddressingLPT
  LDictionary := TOpenAddressingLPT<Integer, Integer>.Create;
  TEST_COUNT_AS_KEY;
end;

procedure TTestHashMaps.Test_CountAsKey_OpenAddressingQP;
var
  LDictionary: TOpenAddressingQP<Integer, Integer>;
begin
  // TOpenAddressingQP
  LDictionary := TOpenAddressingQP<Integer, Integer>.Create;
  TEST_COUNT_AS_KEY;
end;

procedure TTestHashMaps.Test_CountAsKey_OpenAddressingDH;
var
  LDictionary: TOpenAddressingDH<Integer, Integer>;
begin
  // TOpenAddressingDH
  LDictionary := TOpenAddressingDH<Integer, Integer>.Create;
  TEST_COUNT_AS_KEY;
end;

procedure TTestHashMaps.Test_CountAsKey_CuckooD2;
var
  LDictionary: TCuckooD2<Integer, Integer>;
begin
  // TCuckooD2
  LDictionary := TCuckooD2<Integer, Integer>.Create;
  TEST_COUNT_AS_KEY;
end;

procedure TTestHashMaps.Test_CountAsKey_CuckooD4;
var
  LDictionary: TCuckooD4<Integer, Integer>;
begin
  // TCuckooD4
  LDictionary := TCuckooD4<Integer, Integer>.Create;
  TEST_COUNT_AS_KEY;
end;

procedure TTestHashMaps.Test_CountAsKey_CuckooD6;
var
  LDictionary: TCuckooD6<Integer, Integer>;
begin
  // TCuckooD6
  LDictionary := TCuckooD6<Integer, Integer>.Create;
  TEST_COUNT_AS_KEY;
end;

{$DEFINE TEST_NOTIFICATIONS :=
try
  LDictionary.OnKeyNotify := NotifyTestStr;
  LDictionary.OnValueNotify := NotifyTestStr;

  // Add
  NotificationAdd(LDictionary, ['Aaa', 'Bbb', 'Ccc', 'Ddd', 'Eee', 'Fff'], cnAdded);
  LDictionary.Add('Aaa', 'Bbb');
  LDictionary.Add('Ccc', 'Ddd');
  LDictionary.Add('Eee', 'Fff');
  AssertNotificationsExecutedStr;

  // Remove and ExtractPair
  NotificationAdd(LDictionary, ['Ccc', 'Ddd'], cnRemoved);
  LDictionary.Remove('Ccc');
  AssertNotificationsExecutedStr;

  NotificationAdd(LDictionary, ['Aaa', 'Bbb'], cnExtracted);
  with LDictionary.ExtractPair('Aaa') do
  begin
    AssertEquals(Key, 'Aaa');
    AssertEquals(Value, 'Bbb');
  end;
  AssertNotificationsExecutedStr;

  // Clear
  NotificationAdd(LDictionary, ['Eee', 'Fff'], cnRemoved);
  LDictionary.Clear;
  AssertNotificationsExecutedStr;

  // SetItem
  NotificationAdd(LDictionary, ['FPC', 'Polandball'], cnAdded);
  LDictionary.AddOrSetValue('FPC', 'Polandball');
  AssertNotificationsExecutedStr;
  NotificationAdd(LDictionary, 'Polandball', cnRemoved);
  NotificationAdd(LDictionary, 'xD', cnAdded);
  NotificationAdd(LDictionary, 'xD', cnRemoved);
  NotificationAdd(LDictionary, 'Polandball', cnAdded);
  LDictionary['FPC'] := 'xD';
  LDictionary.AddOrSetValue('FPC', 'Polandball');
  AssertNotificationsExecutedStr;
finally
  NotificationAdd(LDictionary, ['FPC', 'Polandball'], cnRemoved);
  LDictionary.Free;
  AssertNotificationsExecutedStr;
end
}

procedure TTestHashMaps.Test_OpenAddressingLP_Notification;
var
  LDictionary: TOpenAddressingLP<string, string>;
begin
  LDictionary := TOpenAddressingLP<string, string>.Create;
  TEST_NOTIFICATIONS;
end;

procedure TTestHashMaps.Test_OpenAddressingLPT_Notification;
var
  LDictionary: TOpenAddressingLPT<string, string>;
begin
  LDictionary := TOpenAddressingLPT<string, string>.Create;
  TEST_NOTIFICATIONS;
end;

procedure TTestHashMaps.Test_OpenAddressingQP_Notification;
var
  LDictionary: TOpenAddressingQP<string, string>;
begin
  LDictionary := TOpenAddressingQP<string, string>.Create;
  TEST_NOTIFICATIONS;
end;

procedure TTestHashMaps.Test_OpenAddressingDH_Notification;
var
  LDictionary: TOpenAddressingDH<string, string>;
begin
  LDictionary := TOpenAddressingDH<string, string>.Create;
  TEST_NOTIFICATIONS;
end;

procedure TTestHashMaps.Test_CuckooD2_Notification;
var
  LDictionary: TCuckooD2<string, string>;
begin
  LDictionary := TCuckooD2<string, string>.Create;
  TEST_NOTIFICATIONS;
end;

procedure TTestHashMaps.Test_CuckooD4_Notification;
var
  LDictionary: TCuckooD4<string, string>;
begin
  LDictionary := TCuckooD4<string, string>.Create;
  TEST_NOTIFICATIONS;
end;

procedure TTestHashMaps.Test_CuckooD6_Notification;
var
  LDictionary: TCuckooD6<string, string>;
begin
  LDictionary := TCuckooD6<string, string>.Create;
  TEST_NOTIFICATIONS;
end;

{$DEFINE TEST_TRIMEXCESS :=
  try
    for i := 1 to 8 do
      LDictionary.Add(i, EmptyRecord);
    LDictionary.Remove(1);

    CheckNotEquals(LDictionary.Capacity, LDictionary.Count);
    LDictionary.TrimExcess;
    AssertEquals(LDictionary.Capacity, 8);
  finally
    LDictionary.Free;
  end;
}

procedure TTestHashMaps.Test_OpenAddressingLP_TrimExcess;
var
  LDictionary: TOpenAddressingLP<Integer, TEmptyRecord>;
  i: Integer;
begin
  LDictionary := TOpenAddressingLP<Integer, TEmptyRecord>.Create;
  TEST_TRIMEXCESS;
end;

procedure TTestHashMaps.Test_CuckooD2_TrimExcess;
var
  LDictionary: TCuckooD2<Integer, TEmptyRecord>;
  i: Integer;
begin
  LDictionary := TCuckooD2<Integer, TEmptyRecord>.Create;
  TEST_TRIMEXCESS;
end;

procedure TTestHashMaps.Test_ObjectDictionary;
begin
  with TObjectOpenAddressingLP<TGUID, TGUID>.Create do Free;
  with TObjectCuckooD2<TGUID, TGUID>.Create do Free;
end;

procedure TTestHashMaps.Test_TryAddOrSetOrGetValue;
// modified test from Castle Game Engine (https://castle-engine.sourceforge.io)
var
  LObjects: TDictionary<string, TObject>;
  LObject, LFoundObject: TObject;
begin
  LObjects := TDictionary<string, TObject>.Create;
  try
    LObjects.TryGetValue('blah', LFoundObject);
    AssertTrue(nil = LFoundObject);

    LObject := TObject.Create;
    LObjects.AddOrSetValue('nope', LObject);

    LObjects.TryGetValue('blah', LFoundObject);
    AssertTrue(nil = LFoundObject);

    LObject := TObject.Create;
    LObjects.AddOrSetValue('blah', LObject);

    LObjects.TryGetValue('blah', LFoundObject);
    AssertTrue(LObject = LFoundObject);

    LObjects.Remove('blah');

    LObject.Free;

    LObjects.TryGetValue('blah', LFoundObject);
    AssertTrue(nil = LFoundObject);

    LObjects['nope'].Free;
  finally
    FreeAndNil(LObjects)
  end;
end;

// modified test from Castle Game Engine (https://castle-engine.io/)
{$DEFINE TEST_TRYGETEMPTYVALUE :=
  try
    Map.AddOrSetValue('some key', 'some value');

    B := Map.TryGetValue('some key', V);
    AssertTrue(B);
    AssertEquals('some value', V);

    B := Map.TryGetValue('some other key', V);
    AssertFalse(B);

    B := Map.TryGetValue('', V);
    AssertFalse(B);
  finally
    FreeAndNil(Map)
  end;
}

procedure TTestHashMaps.Test_TryGetValueEmpty_xxHash32;
var
  Map: TOpenAddressingLP<string, string, TxxHash32HashFactory>;
  V: String; B: Boolean;
begin
  Map := TOpenAddressingLP<string, string, TxxHash32HashFactory>.Create;
  TEST_TRYGETEMPTYVALUE;
end;

procedure TTestHashMaps.Test_TryGetValueEmpty_xxHash32Pascal;
var
  Map: TOpenAddressingLP<string, string, TxxHash32PascalHashFactory>;
  V: String; B: Boolean;
begin
  Map := TOpenAddressingLP<string, string, TxxHash32PascalHashFactory>.Create;
  TEST_TRYGETEMPTYVALUE;
end;

begin
  RegisterTest(TTestHashMaps);
end.

