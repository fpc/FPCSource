{$MODE OBJFPC}
{$H+}
{$C+}
program test;

uses
  contnrs,
  sysutils;

const
  KEYS: array [0..5] of string = (
    'a',
    'b',
    'c',
    'd',
    'e',
    'f'
    );

  TERMINATE_KEY_ID = 2;


procedure DataStaticIterator(Item: Pointer; const Key: string; var Continue: Boolean);
begin
  Assert(Key = String(Item^));
  Continue := TRUE;
end;

procedure DataStaticIteratorTerminated(Item: Pointer; const Key: string; var Continue: Boolean);
begin
  Continue := Key <> KEYS[TERMINATE_KEY_ID];
end;


procedure StringStaticIterator(Item: String; const Key: string; var Continue: Boolean);
begin
  Assert(Key = Item);
  Continue := TRUE;
end;

procedure StringStaticIteratorTerminated(Item: String; const Key: string; var Continue: Boolean);
begin
  Continue := Key <> KEYS[TERMINATE_KEY_ID];
end;


type
  TTestObject = class
  private
    FStr: string;
  public
    constructor Create(const S: string);
    property Str: string read FStr;
  end;

constructor TTestObject.Create(const S: string);
begin
  FStr := S;
end;


procedure ObjectStaticIterator(Item: TObject; const Key: string; var Continue: Boolean);
begin
  Assert(Key = TTestObject(Item).Str);
  Continue := TRUE;
end;

procedure ObjectStaticIteratorTerminated(Item: TObject; const Key: string; var Continue: Boolean);
begin
  Continue := Key <> KEYS[TERMINATE_KEY_ID];
end;


var
  i: integer;
  data_hash_table: TFPDataHashTable;
  last_data: pointer;
  string_hash_table: TFPStringHashTable;
  last_string: string;
  object_hash_table: TFPObjectHashTable;
  last_object: TTestObject;

begin
  data_hash_table := TFPDataHashTable.Create;
  for i := 0 to High(KEYS) do
    data_hash_table.Add(KEYS[i], @KEYS[i]);

  last_data := data_hash_table.Iterate(@DataStaticIterator);
  Assert(last_data = NIL);
  last_data := data_hash_table.Iterate(@DataStaticIteratorTerminated);
  Assert(last_data = @KEYS[TERMINATE_KEY_ID]);

  data_hash_table.Free;

  string_hash_table := TFPStringHashTable.Create;
  for i := 0 to High(KEYS) do
    string_hash_table.Add(KEYS[i], KEYS[i]);

  last_string := string_hash_table.Iterate(@StringStaticIterator);
  Assert(last_string = '');
  last_string := string_hash_table.Iterate(@StringStaticIteratorTerminated);
  Assert(last_string = KEYS[TERMINATE_KEY_ID]);

  string_hash_table.Free;

  object_hash_table := TFPObjectHashTable.Create(TRUE);
  for i := 0 to High(KEYS) do
    object_hash_table.Add(KEYS[i], TTestObject.Create(KEYS[i]));

  last_object := TTestObject(object_hash_table.Iterate(@ObjectStaticIterator));
  Assert(last_object = NIL);
  last_object := TTestObject(object_hash_table.Iterate(@ObjectStaticIteratorTerminated));
  Assert(last_object.Str = KEYS[TERMINATE_KEY_ID]);

  object_hash_table.Free;

  WriteLn('All is OK');
end.
