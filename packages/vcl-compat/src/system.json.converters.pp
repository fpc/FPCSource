{
    This file is part of the Free Component Library
    Copyright (c) 2026 by Michael Van Canneyt michael@freepascal.org

    Delphi-compatible JSON serializer converters unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit System.JSON.Converters;

{$mode delphi}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
   System.Generics.Collections, System.TypInfo, System.Rtti, System.SysUtils,
  {$ELSE}
  Generics.Collections, TypInfo, Rtti, SysUtils,
  {$ENDIF}
  System.JSON.Writers, System.JSON.Readers, System.JSON.Serializers;

type

  { TJsonEnumNameConverter }

  TJsonEnumNameConverter = class(TJsonConverter)
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
    function CanConvert(ATypeInf: PTypeInfo): Boolean; override;
  end;

  { TJsonSetNamesConverter }

  TJsonSetNamesConverter = class(TJsonConverter)
  private
    function ExtractSetValue(ATypeInf: PTypeInfo; const AValue: TValue): Int64;
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
    function CanConvert(ATypeInf: PTypeInfo): Boolean; override;
  end;

  { TJsonCustomCreationConverter }

  TJsonCustomCreationConverter<T> = class(TJsonConverter)
  protected
    function CreateInstance(ATypeInf: PTypeInfo): TValue; virtual; abstract;
  public
    constructor Create;
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
    function CanConvert(ATypeInf: PTypeInfo): Boolean; override;
    function CanWrite: Boolean; override;
  end;



  { TJsonCustomObjectConverter }

  TJsonCustomObjectConverter<T: class {$ifndef inlazide}, constructor{$ENDIF}> = class(TJsonConverter)
  protected
    function CreateInstance: T; overload; virtual;
    function CreateInstance(ATypeInf: PTypeInfo; const ASerializer: TJsonSerializer): T; overload;
  public
    function CanConvert(ATypeInf: PTypeInfo): Boolean; override;
  end;

  
  
  
  { TJsonListHelperConverter }

  TJsonListHelperConverter = class(TJsonConverter)
  public
    constructor Create; overload;
    constructor Create(const ARttiCtx: TRttiContext); overload;
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
    function CanConvert(ATypeInf: PTypeInfo): Boolean; override;
    class function ShouldEdit(ATypeInf: PTypeInfo): Boolean; static;
    function ShouldIncludeMember(constref AMember: TRttiMember): Boolean;
  end;

  
  
  
  { TJsonListConverter }

  TJsonListConverter<V> = class(TJsonCustomObjectConverter<TList<V>>)
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
  end;

  
  
  
  { TJsonStackConverter }

  TJsonStackConverter<V> = class(TJsonCustomObjectConverter<TStack<V>>)
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
  end;

  
  
  
  { TJsonQueueConverter }

  TJsonQueueConverter<V> = class(TJsonCustomObjectConverter<TQueue<V>>)
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
  end;

  
  
  
  { TJsonDictionaryConverter }

  TJsonDictionaryConverter<K, V> = class(TJsonCustomObjectConverter<TDictionary<K, V>>)
  protected
    function PropertyToKey(const APropertyName: string): K; virtual; abstract;
    function KeyToProperty(const AKey: K): string; virtual; abstract;
    function ReadKey(const AReader: TJsonReader; const ASerializer: TJsonSerializer): string;
    function ReadValue(const AReader: TJsonReader; const ASerializer: TJsonSerializer): V; virtual;
    procedure WriteValue(const AWriter: TJsonWriter; const AValue: V; const ASerializer: TJsonSerializer); virtual;
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
  end;

  
  
  
  { TJsonStringDictionaryConverter }

  TJsonStringDictionaryConverter<V> = class(TJsonDictionaryConverter<string, V>)
  protected
    function PropertyToKey(const APropertyName: string): string; override;
    function KeyToProperty(const AKey: string): string; override;
  end;

  
  
  
  { TJsonGUIDConverter }

  TJsonGUIDConverter = class(TJsonConverter)
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
    function CanConvert(ATypeInf: PTypeInfo): Boolean; override;
  end;

  
  
  
  { TJsonHashSetConverter }

  TJsonHashSetConverter<V> = class(TJsonCustomObjectConverter<THashSet<V>>)
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
  end;

implementation

uses
  System.JSON.Types, System.JSON.Utils, System.JSONConsts;

{ TJsonEnumNameConverter }

procedure TJsonEnumNameConverter.WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer);
begin
  AWriter.WriteValue(GetEnumName(AValue.TypeInfo, AValue.AsOrdinal));
end;

function TJsonEnumNameConverter.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  S: string;
  Ord: Integer;
begin
  S := AReader.Value.AsString;
  Ord := GetEnumValue(ATypeInf, S);
  if Ord < 0 then
    raise EJsonSerializationException.CreateFmt('Unknown enum value: %s', [S]);
  Result := TValue.FromOrdinal(ATypeInf, Ord);
end;

function TJsonEnumNameConverter.CanConvert(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf^.Kind = tkEnumeration;
end;

{ TJsonSetNamesConverter }

function TJsonSetNamesConverter.ExtractSetValue(ATypeInf: PTypeInfo; const AValue: TValue): Int64;
var
  SetData: array[0..31] of Byte;
  Size: Integer;
begin
  Result := 0;
  Size := GetTypeData(ATypeInf)^.SetSize;
  FillChar(SetData, SizeOf(SetData), 0);
  AValue.ExtractRawData(@SetData);
  case Size of
    1: Result := SetData[0];
    2: Result := PWord(@SetData)^;
    4: Result := PLongWord(@SetData)^;
    8: Result := PInt64(@SetData)^;
  else
    Move(SetData, Result, Size);
  end;
end;

procedure TJsonSetNamesConverter.WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer);
var
  SetVal: Int64;
  CompTypeInf: PTypeInfo;
  TD: PTypeData;
  I, MinVal, MaxVal: Integer;
  S: string;
begin
  SetVal := ExtractSetValue(AValue.TypeInfo, AValue);
  TD := GetTypeData(AValue.TypeInfo);
  CompTypeInf := TD^.CompType;
  MinVal := GetTypeData(CompTypeInf)^.MinValue;
  MaxVal := GetTypeData(CompTypeInf)^.MaxValue;
  S := '';
  for I := MinVal to MaxVal do
  begin
    if (SetVal and (Int64(1) shl I)) <> 0 then
    begin
      if S <> '' then
        S := S + ', ';
      S := S + GetEnumName(CompTypeInf, I);
    end;
  end;
  AWriter.WriteValue(S);
end;

function TJsonSetNamesConverter.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  S, Name: string;
  Parts: TArray<string>;
  CompTypeInf: PTypeInfo;
  TD: PTypeData;
  SetVal: Int64;
  Ord, I: Integer;
  SetData: array[0..31] of Byte;
  Size: Integer;
begin
  S := AReader.Value.AsString;
  TD := GetTypeData(ATypeInf);
  CompTypeInf := TD^.CompType;
  Size := TD^.SetSize;
  SetVal := 0;
  if S <> '' then
  begin
    Parts := S.Split([',']);
    for I := 0 to High(Parts) do
    begin
      Name := Trim(Parts[I]);
      Ord := GetEnumValue(CompTypeInf, Name);
      if Ord >= 0 then
        SetVal := SetVal or (Int64(1) shl Ord);
    end;
  end;
  FillChar(SetData, SizeOf(SetData), 0);
  Move(SetVal, SetData, Size);
  TValue.Make(@SetData, ATypeInf, Result);
end;

function TJsonSetNamesConverter.CanConvert(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf^.Kind = tkSet;
end;

{ TJsonCustomCreationConverter }

constructor TJsonCustomCreationConverter<T>.Create;
begin
  inherited Create;
end;

procedure TJsonCustomCreationConverter<T>.WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
  const ASerializer: TJsonSerializer);
begin
  ASerializer.Serialize(AWriter, AValue);
end;

function TJsonCustomCreationConverter<T>.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  Val: TValue;
begin
  Val := CreateInstance(ATypeInf);
  ASerializer.Populate(AReader, Val, False);
  Result := Val;
end;

function TJsonCustomCreationConverter<T>.CanConvert(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf = TypeInfo(T);
end;

function TJsonCustomCreationConverter<T>.CanWrite: Boolean;
begin
  Result := False;
end;

{ TJsonCustomObjectConverter }

function TJsonCustomObjectConverter<T>.CreateInstance: T;
begin
  Result := T.Create;
end;

function TJsonCustomObjectConverter<T>.CreateInstance(ATypeInf: PTypeInfo; const ASerializer: TJsonSerializer): T;
begin
  Result := CreateInstance;
end;

function TJsonCustomObjectConverter<T>.CanConvert(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf = TypeInfo(T);
end;

{ TJsonListHelperConverter }

constructor TJsonListHelperConverter.Create;
begin
  inherited Create;
end;

constructor TJsonListHelperConverter.Create(const ARttiCtx: TRttiContext);
begin
  inherited Create;
end;

procedure TJsonListHelperConverter.WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer);
var
  Len, I: Integer;
  ElemValue: TValue;
begin
  AWriter.WriteStartArray;
  Len := AValue.GetArrayLength;
  for I := 0 to Len - 1 do
  begin
    ElemValue := AValue.GetArrayElement(I);
    ASerializer.Serialize(AWriter, ElemValue);
  end;
  AWriter.WriteEndArray;
end;

function TJsonListHelperConverter.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  ElemTypeInf: PTypeInfo;
  Elements: TList<TValue>;
  ElemArr: TArray<TValue>;
  ElemValue: TValue;
begin
  ElemTypeInf := GetTypeData(ATypeInf)^.ElType2;
  Elements := TList<TValue>.Create;
  try
    if AReader.TokenType = TJsonToken.None then
      AReader.Read;
    if AReader.TokenType <> TJsonToken.StartArray then
      raise EJsonSerializationException.Create('Expected start of array');
    while AReader.Read do
    begin
      if AReader.TokenType = TJsonToken.EndArray then
        Break;
      ElemValue := ASerializer.Deserialize(AReader, ElemTypeInf);
      Elements.Add(ElemValue);
    end;
    ElemArr := Elements.ToArray;
    Result := TValue.FromArray(ATypeInf, ElemArr);
  finally
    Elements.Free;
  end;
end;

function TJsonListHelperConverter.CanConvert(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf^.Kind = tkDynArray;
end;

class function TJsonListHelperConverter.ShouldEdit(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf^.Kind = tkDynArray;
end;

function TJsonListHelperConverter.ShouldIncludeMember(constref AMember: TRttiMember): Boolean;
begin
  Result := not SameText(AMember.Name, 'FItems') and
            not SameText(AMember.Name, 'FCount') and
            not SameText(AMember.Name, 'FCapacity');
end;

{ TJsonListConverter }

procedure TJsonListConverter<V>.WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer);
var
  List: TList<V>;
  Item: V;
  Tmp: TValue;
begin
  List := AValue.AsObject as TList<V>;
  AWriter.WriteStartArray;
  if List <> nil then
    for Item in List do
    begin
      TValue.Make(@Item, TypeInfo(V), Tmp);
      ASerializer.Serialize(AWriter, Tmp);
    end;
  AWriter.WriteEndArray;
end;

function TJsonListConverter<V>.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  List: TList<V>;
  Item: V;
  ElemVal: TValue;
begin
  List := TList<V>.Create;
  if AReader.TokenType = TJsonToken.None then
    AReader.Read;
  if AReader.TokenType <> TJsonToken.StartArray then
    raise EJsonSerializationException.Create('Expected start of array');
  while AReader.Read do
  begin
    if AReader.TokenType = TJsonToken.EndArray then
      Break;
    ElemVal := ASerializer.Deserialize(AReader, TypeInfo(V));
    ElemVal.ExtractRawData(@Item);
    List.Add(Item);
  end;
  TValue.Make(@List, TypeInfo(TList<V>), Result);
end;

{ TJsonStackConverter }

procedure TJsonStackConverter<V>.WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer);
var
  Stack: TStack<V>;
  Arr: TArray<V>;
  I: Integer;
  Tmp: TValue;
begin
  Stack := AValue.AsObject as TStack<V>;
  AWriter.WriteStartArray;
  if Stack <> nil then
  begin
    Arr := Stack.ToArray;
    for I := 0 to High(Arr) do
    begin
      TValue.Make(@Arr[I], TypeInfo(V), Tmp);
      ASerializer.Serialize(AWriter, Tmp);
    end;
  end;
  AWriter.WriteEndArray;
end;

function TJsonStackConverter<V>.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  Stack: TStack<V>;
  Items: TList<V>;
  Item: V;
  I: Integer;
  ElemVal: TValue;
begin
  Stack := TStack<V>.Create;
  Items := TList<V>.Create;
  try
    if AReader.TokenType = TJsonToken.None then
      AReader.Read;
    if AReader.TokenType <> TJsonToken.StartArray then
      raise EJsonSerializationException.Create('Expected start of array');
    while AReader.Read do
    begin
      if AReader.TokenType = TJsonToken.EndArray then
        Break;
      ElemVal := ASerializer.Deserialize(AReader, TypeInfo(V));
      ElemVal.ExtractRawData(@Item);
      Items.Add(Item);
    end;
    for I := 0 to Items.Count - 1 do
      Stack.Push(Items[I]);
  finally
    Items.Free;
  end;
  TValue.Make(@Stack, TypeInfo(TStack<V>), Result);
end;

{ TJsonQueueConverter }

procedure TJsonQueueConverter<V>.WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer);
var
  Queue: TQueue<V>;
  Arr: TArray<V>;
  I: Integer;
  Tmp: TValue;
begin
  Queue := AValue.AsObject as TQueue<V>;
  AWriter.WriteStartArray;
  if Queue <> nil then
  begin
    Arr := Queue.ToArray;
    for I := 0 to High(Arr) do
    begin
      TValue.Make(@Arr[I], TypeInfo(V), Tmp);
      ASerializer.Serialize(AWriter, Tmp);
    end;
  end;
  AWriter.WriteEndArray;
end;

function TJsonQueueConverter<V>.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  Queue: TQueue<V>;
  Item: V;
  ElemVal: TValue;
begin
  Queue := TQueue<V>.Create;
  if AReader.TokenType = TJsonToken.None then
    AReader.Read;
  if AReader.TokenType <> TJsonToken.StartArray then
    raise EJsonSerializationException.Create('Expected start of array');
  while AReader.Read do
  begin
    if AReader.TokenType = TJsonToken.EndArray then
      Break;
    ElemVal := ASerializer.Deserialize(AReader, TypeInfo(V));
    ElemVal.ExtractRawData(@Item);
    Queue.Enqueue(Item);
  end;
  TValue.Make(@Queue, TypeInfo(TQueue<V>), Result);
end;

{ TJsonDictionaryConverter }

function TJsonDictionaryConverter<K, V>.ReadKey(const AReader: TJsonReader; const ASerializer: TJsonSerializer): string;
begin
  Result := AReader.Value.AsString;
end;

function TJsonDictionaryConverter<K, V>.ReadValue(const AReader: TJsonReader; const ASerializer: TJsonSerializer): V;
var
  ElemVal: TValue;
begin
  ElemVal := ASerializer.Deserialize(AReader, TypeInfo(V));
  ElemVal.ExtractRawData(@Result);
end;

procedure TJsonDictionaryConverter<K, V>.WriteValue(const AWriter: TJsonWriter; const AValue: V; const ASerializer: TJsonSerializer
  );
var
  Tmp: TValue;
begin
  TValue.Make(@AValue, TypeInfo(V), Tmp);
  ASerializer.Serialize(AWriter, Tmp);
end;

procedure TJsonDictionaryConverter<K, V>.WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
  const ASerializer: TJsonSerializer);
var
  Dict: TDictionary<K, V>;
  Pair: TPair<K, V>;
begin
  Dict := AValue.AsObject as TDictionary<K, V>;
  AWriter.WriteStartObject;
  if Dict <> nil then
    for Pair in Dict do
    begin
      AWriter.WritePropertyName(KeyToProperty(Pair.Key));
      WriteValue(AWriter, Pair.Value, ASerializer);
    end;
  AWriter.WriteEndObject;
end;

function TJsonDictionaryConverter<K, V>.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  Dict: TDictionary<K, V>;
  KeyStr: string;
  Key: K;
  Val: V;
begin
  Dict := TDictionary<K, V>.Create;
  if AReader.TokenType = TJsonToken.None then
    AReader.Read;
  if AReader.TokenType <> TJsonToken.StartObject then
    raise EJsonSerializationException.Create('Expected start of object');
  while AReader.Read do
  begin
    if AReader.TokenType = TJsonToken.EndObject then
      Break;
    if AReader.TokenType = TJsonToken.PropertyName then
    begin
      KeyStr := ReadKey(AReader, ASerializer);
      Key := PropertyToKey(KeyStr);
      AReader.Read;
      Val := ReadValue(AReader, ASerializer);
      Dict.Add(Key, Val);
    end;
  end;
  TValue.Make(@Dict, TypeInfo(TDictionary<K, V>), Result);
end;

{ TJsonStringDictionaryConverter }

function TJsonStringDictionaryConverter<V>.PropertyToKey(const APropertyName: string): string;
begin
  Result := APropertyName;
end;

function TJsonStringDictionaryConverter<V>.KeyToProperty(const AKey: string): string;
begin
  Result := AKey;
end;

{ TJsonGUIDConverter }

procedure TJsonGUIDConverter.WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer);
var
  G: TGUID;
begin
  G := AValue.AsType<TGUID>;
  AWriter.WriteValue(GUIDToString(G));
end;

function TJsonGUIDConverter.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  G: TGUID;
begin
  G := StringToGUID(AReader.Value.AsString);
  Result := TValue.From<TGUID>(G);
end;

function TJsonGUIDConverter.CanConvert(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf = TypeInfo(TGUID);
end;

{ TJsonHashSetConverter }

procedure TJsonHashSetConverter<V>.WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer);
var
  HashSet: THashSet<V>;
  Item: V;
  Tmp: TValue;
begin
  HashSet := AValue.AsObject as THashSet<V>;
  AWriter.WriteStartArray;
  if HashSet <> nil then
    for Item in HashSet do
    begin
      TValue.Make(@Item, TypeInfo(V), Tmp);
      ASerializer.Serialize(AWriter, Tmp);
    end;
  AWriter.WriteEndArray;
end;

function TJsonHashSetConverter<V>.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  HashSet: THashSet<V>;
  Item: V;
  ElemVal: TValue;
begin
  HashSet := THashSet<V>.Create;
  if AReader.TokenType = TJsonToken.None then
    AReader.Read;
  if AReader.TokenType <> TJsonToken.StartArray then
    raise EJsonSerializationException.Create('Expected start of array');
  while AReader.Read do
  begin
    if AReader.TokenType = TJsonToken.EndArray then
      Break;
    ElemVal := ASerializer.Deserialize(AReader, TypeInfo(V));
    ElemVal.ExtractRawData(@Item);
    HashSet.Add(Item);
  end;
  TValue.Make(@HashSet, TypeInfo(THashSet<V>), Result);
end;


initialization
  TJsonConverterRegistry.RegisterConverter(TJsonEnumNameConverter);
  TJsonConverterRegistry.RegisterConverter(TJsonSetNamesConverter);
  TJsonConverterRegistry.RegisterConverter(TJsonGUIDConverter);
  TJsonConverterRegistry.RegisterConverter(TJsonListHelperConverter);

end.
