{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML data & converted unit tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utyamldata;

{$mode ObjFPC}{$H+}

interface

uses
  DateUtils, Classes, SysUtils, fpcunit, testregistry, fpyaml.types, fpyaml.data, fpjson, fpyaml.json;

Type

  { TTestYAMLData }

  TTestYAMLData = class(TTestCase)
  private
    FData: TYAMLData;
  Public
    procedure TearDown; override;
    procedure SetData(aData : TYAMLData);
    procedure AssertScalar(const Msg : string; aData : TYAMLData; aType : TYAMLTagType; const aValue : String);
    Property Data : TYAMLData Read FData Write SetData;
  Published
    Procedure TestHookup;
  end;

  { TTestYAMLScalar }

  TTestYAMLScalar = class(TTestYAMLData)
  private
    FScalar2 : TYAMLScalar;
    function GetScalar: TYAMLScalar;
  Public
    procedure TearDown; override;
    property Scalar : TYAMLScalar Read GetScalar;
    property Scalar2 : TYAMLScalar Read FScalar2 Write FScalar2;
  Published
    Procedure TestString;
    Procedure TestInteger;
    Procedure TestDouble;
    Procedure TestBoolean;
    Procedure TestInt64;
    Procedure TestDateTime;
    Procedure TestNull;
    Procedure TestEqualsBoolean;
    Procedure TestEqualsBooleanNotStrict;
    Procedure TestEqualsString;
    Procedure TestEqualsInteger;
    Procedure TestClone;
  end;

  { TTestYAMLSequence }

  TTestYAMLSequence = class(TTestYAMLData)
  private
    function GetFirst: TYAMLData;
    function GetSequence: TYAMLSequence;
  Public
    procedure Init(aElement: TYAMLData);
    property Sequence : TYAMLSequence Read GetSequence;
    property First : TYAMLData Read GetFirst;
  published
    procedure TestCreate;
    Procedure TestString;
    Procedure TestAddString;
    Procedure TestAddInteger;
    Procedure TestAddBoolean;
    Procedure TestAddFloat;
    Procedure TestDelete;
    Procedure TestExtract;
    Procedure TestEquals;
    Procedure TestClone;
  end;

  { TTestYAMLMapping }

  TTestYAMLMapping = class(TTestYAMLData)
  private
    function GetMapping: TYAMLMapping;
  Public
    procedure Init(aKey,aValue: TYAMLData);
    property Mapping : TYAMLMapping  Read GetMapping;
  Published
    Procedure TestCreate;
    Procedure TestCreate2;
    Procedure TestAddString;
    Procedure TestAddInteger;
    Procedure TestAddBoolean;
    Procedure TestAddFloat;
    Procedure TestDelete;
    Procedure TestExtract;
    Procedure TestEquals;
    Procedure TestEqualsOrder;
    Procedure TestEqualsStrict;
    Procedure TestClone;
  end;

  { TTestConvertToJSON }

  TTestConvertToJSON = Class(TTestYAMLData)
  private
    FJSON: TJSONData;
  Public
    Procedure DoConvert;
    Procedure TearDown; override;
    procedure AssertJSON(const msg: string; aType: TJSONtype; aValue: string);
    Property JSON : TJSONData Read FJSON;
  Published
    Procedure TestNull;
    Procedure TestInteger;
    Procedure TestInt64;
    Procedure TestFloat;
    Procedure TestBoolean;
    Procedure TestString;
    Procedure TestTimeStamp;
    Procedure TestSequence;
    Procedure TestMapping;
    Procedure TestMappingNoCompositeKey;
    Procedure TestMappingNoNullKey;
    Procedure TestDocument;
    Procedure TestDocumentOnly1;
    Procedure TestDocumentNotEmpty;
    Procedure TestStream;
    Procedure TestStreamNotEmpty;
    Procedure TestStreamOnly1;
    Procedure TestVersionOK;
  end;

implementation

{ TTestYAMLData }

procedure TTestYAMLData.TearDown;
begin
  Data:=Nil;
  inherited TearDown;
end;

procedure TTestYAMLData.SetData(aData: TYAMLData);
begin
  FreeAndNil(FData);
  FData:=aData;
end;

procedure TTestYAMLData.AssertScalar(const Msg: string; aData: TYAMLData; aType: TYAMLTagType; const aValue: String);
begin
  AssertNotNull(Msg+': not null',aData);
  AssertEquals(Msg+': scalar',TYAMLScalar,aData.ClassType);
  AssertEquals(Msg+': tag',YAMLTagNames[aType],aData.Tag);
  AssertEquals(Msg+': value',aValue,TYAMLScalar(aData).Value);
end;

procedure TTestYAMLData.TestHookup;
begin
  AssertNull('Data',Data);
end;

{ TTestYAMLScalar }

function TTestYAMLScalar.GetScalar: TYAMLScalar;
begin
  Result:=Data as TYAMLScalar;
end;

procedure TTestYAMLScalar.TearDown;
begin
  FreeAndNil(FScalar2);
  inherited TearDown;
end;

procedure TTestYAMLScalar.TestString;
begin
  Data:=TYAMLScalar.Create('one');
  AssertEquals('Value','one',Scalar.Value);
  AssertEquals('Value typed','one',Scalar.AsString);
  AssertEquals('Tag',YAMLTagNames[yttString],Scalar.Tag);
  AssertFalse('Null',Scalar.IsNull);
end;

procedure TTestYAMLScalar.TestInteger;
begin
  Data:=TYAMLScalar.Create(1);
  AssertEquals('Value','1',Scalar.Value);
  AssertEquals('Value typed',1,Scalar.AsInteger);
  AssertEquals('Tag',YAMLTagNames[yttInteger],Scalar.Tag);
  AssertFalse('Null',Scalar.IsNull);
end;

procedure TTestYAMLScalar.TestDouble;
begin
  Data:=TYAMLScalar.Create(1.2);
  AssertEquals('Value','1.2',Scalar.Value);
  AssertEquals('Typed',1.2,Scalar.AsDouble);
  AssertEquals('Tag',YAMLTagNames[yttFloat],Scalar.Tag);
end;

procedure TTestYAMLScalar.TestBoolean;
begin
  Data:=TYAMLScalar.Create(True);
  AssertEquals('Value','true',Scalar.Value);
  AssertEquals('Typed',true,Scalar.AsBoolean);
  AssertEquals('Tag',YAMLTagNames[yttBoolean],Scalar.Tag);
  AssertFalse('Null',Scalar.IsNull);
end;

procedure TTestYAMLScalar.TestInt64;
begin
  Data:=TYAMLScalar.Create(Int64(1234567891011));
  AssertEquals('Value','1234567891011',Scalar.Value);
  AssertEquals('Typed',1234567891011,Scalar.AsInt64);
  AssertEquals('Tag',YAMLTagNames[yttInteger],Scalar.Tag);
  AssertFalse('Null',Scalar.IsNull);
end;

procedure TTestYAMLScalar.TestDateTime;
begin
  Data:=TYAMLScalar.CreateDateTime(Date);
  AssertEquals('Value',DateToISO8601(Date),Scalar.Value);
  AssertEquals('Typed',Date,Scalar.AsDateTime);
  AssertEquals('Tag',YAMLTagNames[yttTimeStamp],Scalar.Tag);
  AssertFalse('Null',Scalar.IsNull);
end;

procedure TTestYAMLScalar.TestNull;
begin
  Data:=TYAMLScalar.Create(yskPlain);
  AssertEquals('Value','',Scalar.Value);
  AssertTrue('Null',Scalar.IsNull);
end;

procedure TTestYAMLScalar.TestEqualsBoolean;
begin
  Data:=TYAMLScalar.Create(True);
  Scalar2:=TYAMLScalar.Create(True);
  AssertTrue('Equals',Data.Equals(Scalar2,True));
end;

procedure TTestYAMLScalar.TestEqualsBooleanNotStrict;
begin
  Data:=TYAMLScalar.Create(True);
  Scalar2:=TYAMLScalar.Create('true'); // string but with same value
  AssertTrue('Equals',Data.Equals(Scalar2,False));
end;

procedure TTestYAMLScalar.TestEqualsString;
begin
  Data:=TYAMLScalar.Create('a');
  Scalar2:=TYAMLScalar.Create('a');
  AssertTrue('Equals',Data.Equals(Scalar2,True));
  Scalar2.Value:='b';
  AssertFalse('Equals',Data.Equals(Scalar2,True));
end;

procedure TTestYAMLScalar.TestEqualsInteger;
begin
  Data:=TYAMLScalar.Create(1);
  Scalar2:=TYAMLScalar.Create(1);
  AssertTrue('Equals',Data.Equals(Scalar2,True));
  Scalar2.Free;
  Scalar2:=TYAMLScalar.Create('1');
  AssertFalse('Equals (tag diff)',Data.Equals(Scalar2,True));
  AssertTrue('Equals (tag diff, not strict',Data.Equals(Scalar2,False));
end;

procedure TTestYAMLScalar.TestClone;
begin
  Data:=TYAMLScalar.Create(1);
  Scalar2:=Data.Clone as TYAMLScalar;
  AssertEquals('Tag',Data.Tag,Scalar2.Tag);
  AssertEquals('Anchor','',Scalar2.Anchor);
  AssertEquals('Value',Data.AsString,Scalar2.AsString);
  AssertTrue('Kind',Scalar.Kind=Scalar2.Kind);
end;

{ TTestYAMLSequence }


function TTestYAMLSequence.GetFirst: TYAMLData;
begin
  AssertTrue('Have one element',Data.Count>0);
  Result:=Data.Items[0];
end;

function TTestYAMLSequence.GetSequence: TYAMLSequence;
begin
  Result:=Data as TYAMLSequence;
end;

procedure TTestYAMLSequence.Init(aElement: TYAMLData);
begin
  Data:=TYAMLSequence.Create;
  if (aElement<>Nil) then
    Sequence.Add(aElement);
end;

procedure TTestYAMLSequence.TestCreate;
begin
  Init(Nil);
  AssertEquals('Tag',YAMLTagNames[yttSequence],Data.Tag);
end;

procedure TTestYAMLSequence.TestString;
var
  El : TYAMLData;
begin
  Init(TYAMLScalar.Create('one'));
  AssertEquals('Count',1,Sequence.Count);
  El:=Sequence[0];
  AssertNotNull('have Element',El);
  AssertEquals('Element type',TYAMLScalar,El.ClassType);
  AssertEquals('Element value','one',El.AsString);
end;

procedure TTestYAMLSequence.TestAddString;
var
  El : TYAMLData;
begin
  Init(Nil);
  El:=Sequence.Add('abc');
  AssertEquals('Count',1,Sequence.Count);
  AssertNotNull('Have result',EL);
  AssertEquals('Element type',TYAMLScalar,El.ClassType);
  AssertEquals('Element value','abc',El.AsString);
end;

procedure TTestYAMLSequence.TestAddInteger;
var
  El : TYAMLData;
begin
  Init(Nil);
  El:=Sequence.Add(123);
  AssertEquals('Count',1,Sequence.Count);
  AssertNotNull('Have result',EL);
  AssertEquals('Element type',TYAMLScalar,El.ClassType);
  AssertEquals('Element value',123,El.AsInteger);
end;

procedure TTestYAMLSequence.TestAddBoolean;
var
  El : TYAMLData;
begin
  Init(Nil);
  El:=Sequence.Add(True);
  AssertEquals('Count',1,Sequence.Count);
  AssertNotNull('Have result',EL);
  AssertEquals('Element type',TYAMLScalar,El.ClassType);
  AssertEquals('Element value',true,El.AsBoolean);
end;

procedure TTestYAMLSequence.TestAddFloat;
var
  El : TYAMLData;
begin
  Init(Nil);
  El:=Sequence.Add(1.23);
  AssertEquals('Count',1,Sequence.Count);
  AssertNotNull('Have result',EL);
  AssertEquals('Element type',TYAMLScalar,El.ClassType);
  AssertEquals('Element value',1.23,El.AsDouble);
end;

procedure TTestYAMLSequence.TestDelete;
begin
  Init(TYAMLScalar.Create('one'));
  Sequence.Add('two');
  AssertEquals('Count',2,Sequence.Count);
  Sequence.Delete(0);
  AssertEquals('Count',1,Sequence.Count);
  AssertEquals('Remaining','two',Sequence.Items[0].AsString);
end;

procedure TTestYAMLSequence.TestExtract;
var
  El: TYAMLData;
begin
  Init(TYAMLScalar.Create('one'));
  Sequence.Add('two');
  AssertEquals('Count',2,Sequence.Count);
  El:=Sequence.Extract(0);
  try
    AssertEquals('Count',1,Sequence.Count);
    AssertEquals('El','one',EL.AsString);
    AssertEquals('Remaining','two',Sequence.Items[0].AsString);
  finally
    El.Free;
  end;
end;

procedure TTestYAMLSequence.TestEquals;
var
  S2 : TYAMLSequence;

begin
  Init(Nil);
  Sequence.Add('one');
  Sequence.Add('two');
  AssertTrue('Self',Sequence.Equals(Sequence,True));
  S2:=TYAMLSequence.Create;
  try
    S2.Add('one');
    S2.Add('two');
    AssertTrue('S2 - 1 ',Sequence.Equals(S2,True));
    TYAMLScalar(S2[0]).Value:='1';
    AssertFalse('S2 - 2',Sequence.Equals(S2,True));
    TYAMLScalar(S2[0]).Value:='one';
  finally
    S2.Free;
  end;
end;

procedure TTestYAMLSequence.TestClone;
var
  D: TYAMLData;
  S2 : TYAMLSequence absolute d;
begin
  Init(Nil);
  Sequence.Kind:=yckFlow;
  Sequence.Add('one');
  Sequence.Add('two');
  D:=Sequence.Clone;
  try
    AssertEquals('Class',TYAMLSequence,D.ClassType);
    AssertTrue('kind',Sequence.Kind=S2.Kind);
    AssertEquals('Count',Sequence.Count,S2.Count);
    AssertTrue('Item 1',Sequence.Items[0].Equals(S2.Items[0],False));
    AssertEquals('Item 1 tag',Sequence.Items[0].Tag,S2.Items[0].Tag);
    AssertTrue('Item 2',Sequence.Items[1].Equals(S2.Items[1],False));
    AssertEquals('Item 2 tag',Sequence.Items[1].Tag,S2.Items[1].Tag);
  finally
    D.Free;
  end;
end;

{ TTestYAMLMapping }

function TTestYAMLMapping.GetMapping: TYAMLMapping;
begin
  Result:=Data as TYAMLMapping;
end;

procedure TTestYAMLMapping.Init(aKey, aValue: TYAMLData);

begin
  Data:=TYAMLMapping.Create;
  if Assigned(aKey) and Assigned(aValue) then
    Mapping.Add(aKey,aValue);
end;

procedure TTestYAMLMapping.TestCreate;
begin
  Init(Nil,Nil);
  AssertNotNull('Mapping',Mapping);
  AssertEquals('Tag',YAMLTagNames[yttMap],Mapping.Tag);
  AssertEquals('Count',0,Mapping.Count);
end;

procedure TTestYAMLMapping.TestCreate2;

begin
  Init(TYAMLScalar.Create(1),TYAMLScalar.Create(2));
  AssertNotNull('Mapping',Mapping);
  AssertEquals('Count',1,Mapping.Count);
  AssertEquals('Count',1,Mapping.Count);
end;


procedure TTestYAMLMapping.TestAddString;
begin
  Init(Nil,Nil);
  AssertNotNull('Mapping',Mapping);
  Mapping.Add('a','b');
  AssertEquals('Count',1,Mapping.Count);
  AssertScalar('Key',Mapping.Key[0],yttString,'a');
  AssertScalar('Value',Mapping.Items[0],yttString,'b');
end;

procedure TTestYAMLMapping.TestAddInteger;
begin
  Init(Nil,Nil);
  AssertNotNull('Mapping',Mapping);
  Mapping.Add('a',1);
  AssertEquals('Count',1,Mapping.Count);
  AssertScalar('Key',Mapping.Key[0],yttString,'a');
  AssertScalar('Value',Mapping.Items[0],yttInteger,'1');
end;

procedure TTestYAMLMapping.TestAddBoolean;
begin
  Init(Nil,Nil);
  AssertNotNull('Mapping',Mapping);
  Mapping.Add('a',true);
  AssertEquals('Count',1,Mapping.Count);
  AssertScalar('Key',Mapping.Key[0],yttString,'a');
  AssertScalar('Value',Mapping.Items[0],yttBoolean,'true');
end;

procedure TTestYAMLMapping.TestAddFloat;
begin
  Init(Nil,Nil);
  AssertNotNull('Mapping',Mapping);
  Mapping.Add('a',1.2);
  AssertEquals('Count',1,Mapping.Count);
  AssertScalar('Key',Mapping.Key[0],yttString,'a');
  AssertScalar('Value',Mapping.Items[0],yttFloat,'1.2');
end;

procedure TTestYAMLMapping.TestDelete;
begin
  Init(Nil,Nil);
  AssertNotNull('Mapping',Mapping);
  Mapping.Add('a',1.2);
  Mapping.Add('b',1.3);
  Mapping.Add('c',1.4);
  AssertEquals('Count 0',3,Mapping.Count);
  Mapping.Delete(1);
  AssertEquals('Count 1',2,Mapping.Count);
  AssertScalar('Remaining 1 - 0',Mapping.Items[0],yttFloat,'1.2');
  AssertScalar('Remaining 1 - 1',Mapping.Items[1],yttFloat,'1.4');
  Mapping.Delete('c');
  AssertEquals('Count 2',1,Mapping.Count);
  AssertScalar('Remaining 2 - 0',Mapping.Items[0],yttFloat,'1.2');
  Mapping.Delete(Mapping.Key[0]);
  AssertEquals('Count 3',0,Mapping.Count);
end;

procedure TTestYAMLMapping.TestExtract;

var
  Itm: TYAMLData;

begin
  Init(Nil,Nil);
  AssertNotNull('Mapping',Mapping);
  Mapping.Add('a',1.2);
  Mapping.Add('b',1.3);
  Mapping.Add('c',1.4);
  AssertEquals('Count 0',3,Mapping.Count);
  Itm:=Mapping.Extract(1);
  try
    AssertEquals('Count 1',2,Mapping.Count);
    AssertScalar('Remaining 1 - 0',Mapping.Items[0],yttFloat,'1.2');
    AssertScalar('Remaining 1 - 4',Mapping.Items[1],yttFloat,'1.4');
    AssertScalar('Extracted 1', Itm,yttFloat,'1.3');
  finally
    Itm.Free;
  end;
  Itm:=Mapping.Extract('c');
  try
    AssertEquals('Count 2',1,Mapping.Count);
    AssertScalar('Remaining 2 - 0',Mapping.Items[0],yttFloat,'1.2');
    AssertScalar('Extracted 2', Itm,yttFloat,'1.4');
  finally
    Itm.Free;
  end;
  Itm:=Mapping.Extract(Mapping.Key[0]);
  try
    AssertEquals('Count',0,Mapping.Count);
    AssertScalar('Extracted 2', Itm,yttFloat,'1.2');
  finally
    Itm.Free;
  end;
end;

procedure TTestYAMLMapping.TestEquals;

var
  M2 : TYAMLMapping;

begin
  Init(nil,nil);
  Mapping.Add('a','1');
  Mapping.Add('b','2');
  AssertTrue('Self',Mapping.Equals(Mapping,True));
  M2:=TYAMLMapping.Create;
  try
    M2.Add('a','1');
    M2.Add('b','2');
    AssertTrue('M2-1',Mapping.Equals(M2,True));
  finally
    M2.Free;
  end;
end;

procedure TTestYAMLMapping.TestEqualsOrder;
var
  M2 : TYAMLMapping;

begin
  Init(nil,nil);
  Mapping.Add('a','1');
  Mapping.Add('b','2');
  AssertTrue('Self',Mapping.Equals(Mapping,True));
  M2:=TYAMLMapping.Create;
  try
    M2.Add('b','2');
    M2.Add('a','1');
    AssertTrue('M2-1',Mapping.Equals(M2,True));
  finally
    M2.Free;
  end;
end;

procedure TTestYAMLMapping.TestEqualsStrict;
var
  M2 : TYAMLMapping;

begin
  Init(nil,nil);
  Mapping.Add('a','1');
  Mapping.Add('b','2');
  AssertTrue('Self',Mapping.Equals(Mapping,True));
  M2:=TYAMLMapping.Create;
  try
    M2.Add('a','1');
    M2.Add('b',2);
    AssertFalse('M2-1',Mapping.Equals(M2,True));
    AssertTrue('M2-2',Mapping.Equals(M2,False));
  finally
    M2.Free;
  end;
end;

procedure TTestYAMLMapping.TestClone;
var
  D: TYAMLData;
  M2 : TYAMLMapping absolute d;
begin
  Init(Nil,Nil);
  Mapping.Kind:=yckFlow;
  Mapping.Add('one','1');
  Mapping.Add('two','2');
  D:=Mapping.Clone;
  try
    AssertEquals('Class',TYAMLMapping,D.ClassType);
    AssertTrue('kind',Mapping.Kind=M2.Kind);
    AssertEquals('Count',Mapping.Count,M2.Count);
    AssertTrue('Key 1',Mapping.Key[0].Equals(M2.Key[0],False));
    AssertEquals('Key 1 tag',Mapping.Key[0].Tag,M2.Key[0].Tag);
    AssertTrue('Item 1',Mapping.Items[0].Equals(M2.Items[0],False));
    AssertEquals('Item 1 tag',Mapping.Items[0].Tag,M2.Items[0].Tag);
    AssertTrue('Key 2',Mapping.Key[1].Equals(M2.Key[1],False));
    AssertEquals('Key 2 tag',Mapping.Key[1].Tag,M2.Key[1].Tag);
    AssertTrue('Item 2',Mapping.Items[1].Equals(M2.Items[1],False));
    AssertEquals('Item 2 tag',Mapping.Items[1].Tag,M2.Items[1].Tag);
  finally
    D.Free;
  end;
end;

{ TTestConvertToJSON }

procedure TTestConvertToJSON.DoConvert;
begin
  FJSON:=YAMLToJSON(Data);
end;

procedure TTestConvertToJSON.TearDown;
begin
  FreeAndNil(FJSON);
  inherited TearDown;
end;

procedure TTestConvertToJSON.AssertJSON(const msg : string; aType: TJSONtype; aValue: string);
begin
  AssertTrue(Msg+': Correct type',aType=JSON.JSONType);
  if aValue<>'' then
    if aType in StructuredJSONTypes then
      AssertEquals('JSON value : ',aValue,JSON.AsJSON)
    else
      AssertEquals('String value : ',aValue,JSON.AsString);
end;

procedure TTestConvertToJSON.TestNull;
begin
  Data:=TYAMLScalar.Create('',yttNull);
  DoConvert;
  AssertJSON('Null',jtNull,'');
end;

procedure TTestConvertToJSON.TestInteger;
begin
  Data:=TYAMLScalar.Create(123);
  DoConvert;
  AssertJSON('Number',jtNumber,'123');
  AssertTrue('32-bit-Integer',ntInteger=TJSONNumber(JSON).NumberType);
end;

procedure TTestConvertToJSON.TestInt64;
begin
  Data:=TYAMLScalar.Create(123456789101112);
  DoConvert;
  AssertJSON('Number',jtNumber,'123456789101112');
  AssertTrue('64-bit-Integer',ntInt64=TJSONNumber(JSON).NumberType);
end;

procedure TTestConvertToJSON.TestFloat;
begin
  Data:=TYAMLScalar.Create(12.34);
  DoConvert;
  AssertJSON('Number',jtNumber,'');
  AssertEquals('Float',12.34,JSON.AsFloat);
end;

procedure TTestConvertToJSON.TestBoolean;
begin
  Data:=TYAMLScalar.Create(True);
  DoConvert;
  AssertJSON('Boolean',jtBoolean,'True');
end;

procedure TTestConvertToJSON.TestString;
begin
  Data:=TYAMLScalar.Create('abc');
  DoConvert;
  AssertJSON('String',jtString,'abc');
end;

procedure TTestConvertToJSON.TestTimeStamp;

var
  D : TDateTime;
begin
  D:=EncodeDateTime(2024,12,20,21,11,0,0);
  Data:=TYAMLScalar.CreateDateTime(D);
  DoConvert;
  AssertJSON('String',jtString,DateToISO8601(D));
end;

procedure TTestConvertToJSON.TestSequence;

Var
  Seq : TYAMLSequence;

begin
  Seq:=TYAMLSequence.Create;
  Data:=Seq;
  Seq.Add('a');
  Seq.Add(2);
  DoConvert;
  AssertJSON('Array',jtArray,'["a", 2]');
end;

procedure TTestConvertToJSON.TestMapping;
Var
  Map : TYAMLMapping;

begin
  Map:=TYAMLMapping.Create;
  Data:=Map;
  Map.Add('a','1');
  Map.Add('b',2);
  DoConvert;
  AssertJSON('Object',jtObject,'{ "a" : "1", "b" : 2 }');
end;

procedure TTestConvertToJSON.TestMappingNoCompositeKey;
Var
  Map : TYAMLMapping;

begin
  Map:=TYAMLMapping.Create;
  Data:=Map;
  Map.Add(TYAMLSequence.Create(),TYAMLScalar.Create('1'));
  AssertException('No sequence',EConvertError,@DoConvert);
end;

procedure TTestConvertToJSON.TestMappingNoNullKey;
Var
  Map : TYAMLMapping;

begin
  Map:=TYAMLMapping.Create;
  Data:=Map;
  Map.Add(TYAMLScalar.Create('',yttNull),TYAMLScalar.Create('1'));
  AssertException('No null scalar',EConvertError,@DoConvert);
end;

procedure TTestConvertToJSON.TestDocument;
Var
  Doc : TYAMLDocument;

begin
  Doc:=TYAMLDocument.Create;
  Data:=Doc;
  Doc.Add('a');
  DoConvert;
  AssertJSON('string',jtString,'a');
end;

procedure TTestConvertToJSON.TestDocumentOnly1;
Var
  Doc : TYAMLDocument;

begin
  Doc:=TYAMLDocument.Create;
  Data:=Doc;
  Doc.Add('a');
  Doc.Add('b');
  AssertException('Only one element',EConvertError,@DoConvert);
end;

procedure TTestConvertToJSON.TestDocumentNotEmpty;
Var
  Doc : TYAMLDocument;

begin
  Doc:=TYAMLDocument.Create;
  Data:=Doc;
  AssertException('Only one element',EConvertError,@DoConvert);
end;

procedure TTestConvertToJSON.TestStream;
Var
  Stream : TYAMLStream;
  Doc : TYAMLDocument;

begin
  Stream:=TYAMLStream.Create;
  Data:=Stream;
  Doc:=TYAMLDocument.Create;
  Stream.Add(Doc);
  Doc.Add('a');
  DoConvert;
  AssertJSON('string',jtString,'a');
end;

procedure TTestConvertToJSON.TestStreamNotEmpty;
Var
  Stream : TYAMLStream;

begin
  Stream:=TYAMLStream.Create;
  Data:=Stream;
  AssertException('Must have one document',EConvertError,@DoConvert);
end;

procedure TTestConvertToJSON.TestStreamOnly1;
Var
  Stream : TYAMLStream;
  Doc : TYAMLDocument;

begin
  Stream:=TYAMLStream.Create;
  Data:=Stream;
  Doc:=TYAMLDocument.Create;
  Doc.Add('a');
  Stream.Add(Doc);
  Doc:=TYAMLDocument.Create;
  Stream.Add(Doc);
  Doc.Add('B');
  AssertException('Must have only one document',EConvertError,@DoConvert);
end;

procedure TTestConvertToJSON.TestVersionOK;
Var
  Stream : TYAMLStream;
  Doc : TYAMLDocument;
begin
  Stream:=TYAMLStream.Create;
  Data:=Stream;
  Stream.Add(TYAMLTagData.Create('','soso'));
  Doc:=TYAMLDocument.Create;
  Doc.Add('a');
  Stream.Add(Doc);
  DoConvert;
  AssertJSON('string',jtString,'a');
end;

initialization
  RegisterTests([TTestYAMLScalar,TTestYAMLSequence,TTestYAMLMapping,TTestConvertToJSON]);
end.

