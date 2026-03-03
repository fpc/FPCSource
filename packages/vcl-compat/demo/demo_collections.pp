program demo_collections;
{$mode delphi}{$H+}
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  SysUtils, TypInfo, Rtti, Generics.Collections,
  System.JSON.Types,
  System.JSON.Writers, System.JSON.Readers,
  System.JSON.Serializers, System.JSON.Converters;

type
  TItem = record
    Name: string;
    Value: Integer;
  end;

  { Type aliases needed because FPC can't parse nested <> in generic method calls }
  TIntegerList = TList<Integer>;
  TStringList = TList<string>;
  TIntegerStack = TStack<Integer>;
  TStringQueue = TQueue<string>;
  TStringIntDict = TDictionary<string, Integer>;
  TStringIntPair = TPair<string, Integer>;
  TIntArray = array of Integer;
  TItemArray = array of TItem;

procedure Section(const Title: string);
begin
  WriteLn;
  WriteLn('=== ', Title, ' ===');
end;

procedure DemoDynArrays(Ser: TJsonSerializer);
var
  Ints: TIntArray;
  Items: TItemArray;
  Json: string;
  I: Integer;
begin
  Section('Dynamic Arrays (Built-in via TJsonListHelperConverter)');

  Ints := TIntArray.Create(100, 200, 300);
  Json := Ser.Serialize<TIntArray>(Ints);
  WriteLn('  Integer array: ', Json);

  SetLength(Items, 2);
  Items[0].Name := 'sword';  Items[0].Value := 150;
  Items[1].Name := 'shield'; Items[1].Value := 100;
  Json := Ser.Serialize<TItemArray>(Items);
  WriteLn('  Record array:  ', Json);

  Items := Ser.Deserialize<TItemArray>(Json);
  WriteLn;
  WriteLn('  Deserialized record array:');
  for I := 0 to High(Items) do
    WriteLn('    [', I, '] Name=', Items[I].Name, ', Value=', Items[I].Value);
end;

procedure DemoList(Ser: TJsonSerializer);
var
  List: TIntegerList;
  Json: string;
  I: Integer;
begin
  Section('TList<Integer>');

  Ser.Converters.Add(TJsonListConverter<Integer>.Create);

  List := TIntegerList.Create;
  try
    List.Add(10);
    List.Add(20);
    List.Add(30);
    List.Add(40);

    Json := Ser.Serialize<TIntegerList>(List);
    WriteLn('  Serialized: ', Json);
  finally
    List.Free;
  end;

  WriteLn;
  List := Ser.Deserialize<TIntegerList>(Json);
  try
    Write('  Deserialized (', List.Count, ' items): [');
    for I := 0 to List.Count - 1 do begin
      if I > 0 then Write(', ');
      Write(List[I]);
    end;
    WriteLn(']');
  finally
    List.Free;
  end;
end;

procedure DemoStringListCol(Ser: TJsonSerializer);
var
  List: TStringList;
  Json: string;
  I: Integer;
begin
  Section('TList<string>');

  Ser.Converters.Add(TJsonListConverter<string>.Create);

  List := TStringList.Create;
  try
    List.Add('alpha');
    List.Add('beta');
    List.Add('gamma');

    Json := Ser.Serialize<TStringList>(List);
    WriteLn('  Serialized: ', Json);
  finally
    List.Free;
  end;

  WriteLn;
  List := Ser.Deserialize<TStringList>(Json);
  try
    Write('  Deserialized: [');
    for I := 0 to List.Count - 1 do begin
      if I > 0 then Write(', ');
      Write('"', List[I], '"');
    end;
    WriteLn(']');
  finally
    List.Free;
  end;
end;

procedure DemoStack(Ser: TJsonSerializer);
var
  Stk: TIntegerStack;
  Json: string;
begin
  Section('TStack<Integer>');

  Ser.Converters.Add(TJsonStackConverter<Integer>.Create);

  Stk := TIntegerStack.Create;
  try
    Stk.Push(1);
    Stk.Push(2);
    Stk.Push(3);

    Json := Ser.Serialize<TIntegerStack>(Stk);
    WriteLn('  Pushed 1, 2, 3');
    WriteLn('  Serialized: ', Json);
  finally
    Stk.Free;
  end;

  WriteLn;
  Stk := Ser.Deserialize<TIntegerStack>(Json);
  try
    WriteLn('  Deserialized stack (', Stk.Count, ' items):');
    while Stk.Count > 0 do
      WriteLn('    Pop: ', Stk.Pop);
  finally
    Stk.Free;
  end;
end;

procedure DemoQueue(Ser: TJsonSerializer);
var
  Q: TStringQueue;
  Json: string;
begin
  Section('TQueue<string>');

  Ser.Converters.Add(TJsonQueueConverter<string>.Create);

  Q := TStringQueue.Create;
  try
    Q.Enqueue('first');
    Q.Enqueue('second');
    Q.Enqueue('third');

    Json := Ser.Serialize<TStringQueue>(Q);
    WriteLn('  Enqueued: first, second, third');
    WriteLn('  Serialized: ', Json);
  finally
    Q.Free;
  end;

  WriteLn;
  Q := Ser.Deserialize<TStringQueue>(Json);
  try
    WriteLn('  Deserialized queue (', Q.Count, ' items):');
    while Q.Count > 0 do
      WriteLn('    Dequeue: ', Q.Dequeue);
  finally
    Q.Free;
  end;
end;

procedure DemoDictionary(Ser: TJsonSerializer);
var
  Dict: TStringIntDict;
  Json: string;
  Pair: TStringIntPair;
begin
  Section('TDictionary<string, Integer>');

  Ser.Converters.Add(TJsonStringDictionaryConverter<Integer>.Create);

  Dict := TStringIntDict.Create;
  try
    Dict.Add('apples', 5);
    Dict.Add('bananas', 12);
    Dict.Add('cherries', 30);

    Json := Ser.Serialize<TStringIntDict>(Dict);
    WriteLn('  Serialized as JSON object:');
    WriteLn('    ', Json);
  finally
    Dict.Free;
  end;

  WriteLn;
  Dict := Ser.Deserialize<TStringIntDict>(Json);
  try
    WriteLn('  Deserialized (', Dict.Count, ' entries):');
    for Pair in Dict do
      WriteLn('    ', Pair.Key, ' = ', Pair.Value);
  finally
    Dict.Free;
  end;
end;

var
  Ser: TJsonSerializer;
begin
  Ser := TJsonSerializer.Create;
  try
    WriteLn('Demo: Generic Collection Serialization');
    WriteLn('=======================================');

    DemoDynArrays(Ser);
    DemoList(Ser);
    DemoStringListCol(Ser);
    DemoStack(Ser);
    DemoQueue(Ser);
    DemoDictionary(Ser);

    WriteLn;
    WriteLn('Done.');
  finally
    Ser.Free;
  end;
end.
