unit utc.opf.json;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Json, Opf.Connections;

type

  { TTestOPSerializer }

  TTestOPSerializer = class(TTestCase)
  private
    FList: TSQLDBConnectionList;
    procedure CallSerializeNil;
    procedure CallDeserializeNil;
    procedure CallDeserializeMalformed;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property List: TSQLDBConnectionList read FList;
  published
    procedure TestSerializeCollection;
    procedure TestDeserializeCollection;
    procedure TestSerializeEmptyCollection;
    procedure TestCustomSerializer;
    procedure TestStreamRoundTrip;
    procedure TestDeserializeMalformedJSON;
    procedure TestSerializeNil;
    procedure TestDeserializeNil;
  end;

  { TTestCustomSerializer }

  TTestCustomSerializer = class(TInterfacedObject, IOPSerializer)
  public
    LastSerializedClass: TClass;
    function Serialize(aObject: TObject): UTF8String;
    procedure Deserialize(const aJSON: UTF8String; aObject: TObject);
  end;

implementation

{ TTestCustomSerializer }

function TTestCustomSerializer.Serialize(aObject: TObject): UTF8String;
begin
  LastSerializedClass := aObject.ClassType;
  Result := '[]';
end;

procedure TTestCustomSerializer.Deserialize(const aJSON: UTF8String; aObject: TObject);
begin
  // no-op for test
end;

{ TTestOPSerializer }

procedure TTestOPSerializer.SetUp;
begin
  inherited SetUp;
  FList := TSQLDBConnectionList.Create(TSQLDBConnectionDef);
end;

procedure TTestOPSerializer.TearDown;
begin
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TTestOPSerializer.TestSerializeCollection;
var
  S: UTF8String;
begin
  List.AddConnection('pgsql', 'host1', 'db1', 'user1', 'pass1').Name := 'Conn1';
  List.AddConnection('mysql', 'host2', 'db2', 'user2', 'pass2').Name := 'Conn2';
  S := OPSerializer.Serialize(List);
  AssertTrue('Result not empty', Length(S) > 0);
  AssertTrue('Contains host1', Pos('host1', S) > 0);
  AssertTrue('Contains host2', Pos('host2', S) > 0);
  AssertTrue('Contains db1', Pos('db1', S) > 0);
  AssertTrue('Contains db2', Pos('db2', S) > 0);
  AssertTrue('Contains user1', Pos('user1', S) > 0);
  AssertTrue('Contains user2', Pos('user2', S) > 0);
  AssertTrue('Contains pgsql', Pos('pgsql', S) > 0);
  AssertTrue('Contains mysql', Pos('mysql', S) > 0);
end;

procedure TTestOPSerializer.TestDeserializeCollection;
var
  S: UTF8String;
  List2: TSQLDBConnectionList;
begin
  List.AddConnection('pgsql', 'host1', 'db1', 'user1', 'pass1').Name := 'Conn1';
  List.AddConnection('mysql', 'host2', 'db2', 'user2', 'pass2').Name := 'Conn2';
  S := OPSerializer.Serialize(List);

  List2 := TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    OPSerializer.Deserialize(S, List2);
    AssertEquals('Count after round-trip', 2, List2.Count);
    AssertEquals('Conn1 HostName', 'host1', List2[0].HostName);
    AssertEquals('Conn1 DatabaseName', 'db1', List2[0].DatabaseName);
    AssertEquals('Conn1 UserName', 'user1', List2[0].UserName);
    AssertEquals('Conn1 Password', 'pass1', List2[0].Password);
    AssertEquals('Conn1 ConnectionType', 'pgsql', List2[0].ConnectionType);
    AssertEquals('Conn2 HostName', 'host2', List2[1].HostName);
    AssertEquals('Conn2 DatabaseName', 'db2', List2[1].DatabaseName);
    AssertEquals('Conn2 UserName', 'user2', List2[1].UserName);
    AssertEquals('Conn2 Password', 'pass2', List2[1].Password);
    AssertEquals('Conn2 ConnectionType', 'mysql', List2[1].ConnectionType);
  finally
    List2.Free;
  end;
end;

procedure TTestOPSerializer.TestSerializeEmptyCollection;
var
  S: UTF8String;
  List2: TSQLDBConnectionList;
begin
  S := OPSerializer.Serialize(List);
  List2 := TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    OPSerializer.Deserialize(S, List2);
    AssertEquals('Empty round-trip count', 0, List2.Count);
  finally
    List2.Free;
  end;
end;

procedure TTestOPSerializer.TestCustomSerializer;
var
  Custom: TTestCustomSerializer;
  SavedSerializer: IOPSerializer;
  S: UTF8String;
begin
  SavedSerializer := OPSerializer;
  Custom := TTestCustomSerializer.Create;
  try
    SetOPSerializer(Custom);
    AssertSame('Custom serializer active', TObject(Pointer(IOPSerializer(Custom))),
      TObject(Pointer(OPSerializer)));
    S := OPSerializer.Serialize(List);
    AssertEquals('Custom output', '[]', S);
    AssertEquals('Tracked class', 'TSQLDBConnectionList', Custom.LastSerializedClass.ClassName);
  finally
    SetOPSerializer(SavedSerializer);
  end;
end;

procedure TTestOPSerializer.CallSerializeNil;
begin
  OPSerializer.Serialize(nil);
end;

procedure TTestOPSerializer.CallDeserializeNil;
begin
  OPSerializer.Deserialize('[]', nil);
end;

procedure TTestOPSerializer.CallDeserializeMalformed;
begin
  OPSerializer.Deserialize('{invalid json!!!', List);
end;

procedure TTestOPSerializer.TestStreamRoundTrip;
var
  Stream: TMemoryStream;
  List2: TSQLDBConnectionList;
begin
  List.AddConnection('pgsql', 'host1', 'db1', 'user1', 'pass1').Name := 'Conn1';
  List.AddConnection('mysql', 'host2', 'db2', 'user2', 'pass2').Name := 'Conn2';
  Stream := TMemoryStream.Create;
  try
    List.SaveToStream(Stream);
    AssertTrue('Stream not empty', Stream.Size > 0);
    Stream.Position := 0;
    List2 := TSQLDBConnectionList.Create(TSQLDBConnectionDef);
    try
      List2.LoadFromStream(Stream);
      AssertEquals('Count after stream round-trip', 2, List2.Count);
      AssertEquals('HostName', 'host1', List2[0].HostName);
      AssertEquals('DatabaseName', 'db2', List2[1].DatabaseName);
    finally
      List2.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TTestOPSerializer.TestDeserializeMalformedJSON;
begin
  AssertException('Malformed JSON', EOPFJson, @Self.CallDeserializeMalformed);
end;

procedure TTestOPSerializer.TestSerializeNil;
begin
  AssertException('Nil object', EOPFJson, @Self.CallSerializeNil);
end;

procedure TTestOPSerializer.TestDeserializeNil;
begin
  AssertException('Nil object', EOPFJson, @Self.CallDeserializeNil);
end;

initialization
  RegisterTests([TTestOPSerializer]);
end.
