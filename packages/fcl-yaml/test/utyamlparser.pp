{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML parser unit tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utyamlparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpyaml.types, fpyaml.parser, fpyaml.data, utyamldata;

type

  { TTestYamlParser }

  TTestYamlParser= class(TTestYAMLData)
  private
    FParser: TYAMLParser;
    function AssertValue(aClass: TYAMLDataClass): TYAMLData;
    function GetDocument: TYAMLDocument;
    function GetStream: TYAMLStream;
    function GetValue: TYAMLData;
  public
    procedure Parse(aContent : Array of string);

    procedure SetUp; override;
    procedure TearDown; override;
    property Parser : TYAMLParser Read FParser;
    property YAML : TYAMLStream Read GetStream;
    property Document: TYAMLDocument Read GetDocument;
    property Value: TYAMLData Read GetValue;
  published
    procedure TestCreate;
    procedure TestEmptyDocument;
    procedure TestVersionEmptyDocument;
    procedure TestMultiDocument;
    procedure TestMultiDocumentNoEnd;
    procedure TestScalar;
    procedure TestAnchoredScalar;
    procedure TestAlias;
    procedure TestBlockSequence;
    procedure TestBlockSequenceTwo;
    procedure TestBlockSequenceThree;
    procedure TestBlockSequenceNested;
    procedure TestBlockSequenceNestedBetween;
    procedure TestFlowSequence;
    procedure TestFlowSequenceTwo;
    procedure TestFlowSequenceThree;
    procedure TestFlowSequenceNestedBetween;
    procedure TestBlockMapping;
    procedure TestBlockMappingTwo;
    procedure TestBlockMappingThree;
    procedure TestBlockMappingNested;
    procedure TestBlockMappingNestedDouble;
    procedure TestBlockMappingUnindentedSequence;
    procedure TestBlockMappingUnindentedSequenceWithIndent;
    procedure TestBlockMappingFlowSequence;
    procedure TestFlowMapping;
    procedure TestFlowMappingOne;
    procedure TestFlowMappingTwo;
    procedure TestFlowMappingNested;
  end;

implementation

procedure TTestYamlParser.TestCreate;
begin
  Parse(['one']);
  AssertNotNull('Parser',Parser);
  AssertNotNull('Data',Data);
  AssertNotNull('YAML',YAML);
  AssertNotNull('Document',Document);
  AssertNotNUll('Value');
end;

procedure TTestYamlParser.TestScalar;
begin
  Parse(['one']);
  AssertNotNull('Parser',Parser);
  AssertNotNull('Data',Data);
  AssertEquals('YAML Stream',TYAMLStream,Data.ClassType);
  AssertEquals('YAML Stream item count',1,YAML.Count);
  AssertNotNull('Document',Document);
  AssertNotNUll('Value');
  AssertEquals('Value ',TYAMLScalar,Value.ClassType);
  AssertEquals('Value ','one',Value.AsString);
end;

procedure TTestYamlParser.TestAnchoredScalar;
begin
  Parse(['&anchor one']);
  AssertNotNull('Parser',Parser);
  AssertNotNull('Data',Data);
  AssertEquals('YAML Stream',TYAMLStream,Data.ClassType);
  AssertEquals('YAML Stream item count',1,YAML.Count);
  AssertNotNull('Document',Document);
  AssertScalar('Value',Value,yttString,'one');
  AssertNotNUll('Value',Value);
  AssertEquals('Value ',TYAMLScalar,Value.ClassType);
  AssertEquals('Value valua ','one',Value.AsString);
  AssertEquals('Value ','anchor',Value.Anchor);
end;

procedure TTestYamlParser.TestAlias;

var
  Seq : TYAMLSequence;
  lItem : TYAMLScalar;
begin
  Parse(['- &anchor one','- *anchor ']);
  Seq:=AssertSequence('Value',Value,2);
  lItem:=AssertScalar('First',Seq[0],yttString,'one');
  AssertEquals('first has anchor','anchor',lItem.Anchor);
  lItem:=AssertScalar('Second',Seq[1],yttString,'one');
  AssertEquals('second has no anchor','',lItem.Anchor);
end;

function TTestYamlParser.AssertValue(aClass : TYAMLDataClass) : TYAMLData;

begin
  AssertNotNull('Data',Data);
  AssertEquals('YAML Stream',TYAMLStream,Data.ClassType);
  AssertEquals('YAML Stream item count',1,YAML.Count);
  AssertNotNull('Document',Document);
  AssertTrue('Document not empty',Document.Count>0);
  AssertEquals('Value class',aClass,Document.Items[0].ClassType);
  Result:=Value;
end;

procedure TTestYamlParser.TestBlockSequence;

var
  Seq : TYAMLSequence;

begin
  Parse(['- one']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertTrue('Correct sequence kind',yckBlock=seq.Kind);
  AssertEquals('element count',1,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
end;

procedure TTestYamlParser.TestBlockSequenceTwo;
var
  Seq : TYAMLSequence;

begin
  Parse(['- one','- two']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertTrue('Correct sequence kind',yckBlock=seq.Kind);
  AssertEquals('Element count',2,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
  AssertScalar('Second',Seq.Items[1],yttString,'two');
end;

procedure TTestYamlParser.TestBlockSequenceThree;
var
  Seq : TYAMLSequence;

begin
  Parse(['- one','- two','- three']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertEquals('Element count',3,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
  AssertScalar('Second',Seq.Items[1],yttString,'two');
  AssertScalar('Third',Seq.Items[2],yttString,'three');
end;

procedure TTestYamlParser.TestBlockSequenceNested;
var
  Seq : TYAMLSequence;

begin
  Parse(['- one','-','  - a','  - b']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertTrue('Correct sequence kind',yckBlock=seq.Kind);
  AssertEquals('Element count',2,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
  AssertEquals('Sequence',TYAMLSequence,Seq.Items[1].ClassType);
  Seq:=TYAMLSequence(Seq.Items[1]);
  AssertEquals('Element count',2,Seq.Count);
  AssertScalar('2 - 1',Seq.Items[0],yttString,'a');
  AssertScalar('2 - 2',Seq.Items[1],yttString,'b');
end;

procedure TTestYamlParser.TestBlockSequenceNestedBetween;
var
  Seq : TYAMLSequence;

begin
  Parse(['- one','-','  - a','  - b','- two']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertTrue('Correct sequence kind',yckBlock=seq.Kind);
  AssertEquals('Element count',3,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
  AssertScalar('Third',Seq.Items[2],yttString,'two');
  AssertEquals('Sequence',TYAMLSequence,Seq.Items[1].ClassType);
  Seq:=TYAMLSequence(Seq.Items[1]);
  AssertEquals('Element count',2,Seq.Count);
  AssertScalar('2 - 1',Seq.Items[0],yttString,'a');
  AssertScalar('2 - 2',Seq.Items[1],yttString,'b');
end;

procedure TTestYamlParser.TestFlowSequence;
var
  Seq : TYAMLSequence;

begin
  Parse(['[one]']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertTrue('Correct sequence kind',yckFlow=seq.Kind);
  AssertEquals('element count',1,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
end;

procedure TTestYamlParser.TestFlowSequenceTwo;
var
  Seq : TYAMLSequence;

begin
  Parse(['[one,two]']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertEquals('element count',2,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
  AssertScalar('Second',Seq.Items[1],yttString,'two');
end;

procedure TTestYamlParser.TestFlowSequenceThree;
var
  Seq : TYAMLSequence;

begin
  Parse(['[one,two,','three]']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertTrue('Correct sequence kind',yckFlow=seq.Kind);
  AssertEquals('element count',3,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
  AssertScalar('Second',Seq.Items[1],yttString,'two');
  AssertScalar('Third',Seq.Items[2],yttString,'three');
end;

procedure TTestYamlParser.TestFlowSequenceNestedBetween;
var
  Seq : TYAMLSequence;
begin
  Parse(['[one ','[a,b]',',two]']);
  Seq:=TYAMLSequence(AssertValue(TYAMLSequence));
  AssertEquals('Element count',3,Seq.Count);
  AssertScalar('First',Seq.Items[0],yttString,'one');
  AssertScalar('Third',Seq.Items[2],yttString,'two');
  AssertEquals('Sequence',TYAMLSequence,Seq.Items[1].ClassType);
  Seq:=TYAMLSequence(Seq.Items[1]);
  AssertEquals('Element count',2,Seq.Count);
  AssertScalar('2 - 1',Seq.Items[0],yttString,'a');
  AssertScalar('2 - 2',Seq.Items[1],yttString,'b');
end;

procedure TTestYamlParser.TestBlockMapping;
var
  Map : TYAMLMapping;
begin
  Parse(['one: two']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckBlock);
  AssertEquals('Element count',1,Map.Count);
  AssertScalar('First key',Map.Key[0],yttString,'one');
  AssertScalar('First value',Map.Items[0],yttString,'two');
end;

procedure TTestYamlParser.TestBlockMappingTwo;
var
  Map : TYAMLMapping;
begin
  Parse(['one: two','three: four']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckBlock);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('First key',Map.Key[0],yttString,'one');
  AssertScalar('First value',Map.Items[0],yttString,'two');
  AssertScalar('Second key',Map.Key[1],yttString,'three');
  AssertScalar('Second value',Map.Items[1],yttString,'four');
end;

procedure TTestYamlParser.TestBlockMappingThree;
var
  Map : TYAMLMapping;
begin
  Parse(['one: two','three: four','five: six']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertEquals('Element count',3,Map.Count);
  AssertScalar('First key',Map.Key[0],yttString,'one');
  AssertScalar('First value',Map.Items[0],yttString,'two');
  AssertScalar('Second key',Map.Key[1],yttString,'three');
  AssertScalar('Second value',Map.Items[1],yttString,'four');
  AssertScalar('third key',Map.Key[2],yttString,'five');
  AssertScalar('third value',Map.Items[2],yttString,'six');
end;

procedure TTestYamlParser.TestBlockMappingNested;
var
  Map : TYAMLMapping;
begin
  Parse(['one: two','three:','  five: six','  seven: eight']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckBlock);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('First key',Map.Key[0],yttString,'one');
  AssertScalar('First value',Map.Items[0],yttString,'two');
  AssertScalar('Second key',Map.Key[1],yttString,'three');
  AssertEquals('Map',TYAMLMapping,Map.Items[1].ClassType);
  Map:=TYAMLMapping(Map.Items[1]);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('2 - First key',Map.Key[0],yttString,'five');
  AssertScalar('2 - first value',Map.Items[0],yttString,'six');
  AssertScalar('2 - Second key',Map.Key[1],yttString,'seven');
  AssertScalar('2 - second value',Map.Items[1],yttString,'eight');
end;

procedure TTestYamlParser.TestBlockMappingNestedDouble;

var
  Map,Map2 : TYAMLMapping;
begin
  Parse(['one: 1',
         'two:',
         '  three:',
         '    four: 4',
         '    five: 5',
         '  six: 6']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckBlock);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('First key',Map.Key[0],yttString,'one');
  AssertScalar('First value',Map.Items[0],yttInteger,'1');
  AssertScalar('Second key',Map.Key[1],yttString,'two');
  AssertEquals('Map',TYAMLMapping,Map.Items[1].ClassType);
  Map:=TYAMLMapping(Map.Items[1]);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('2 - First key',Map.Key[0],yttString,'three');
  AssertEquals('2 - first value is Map',TYAMLMapping,Map.Items[0].ClassType);

  Map2:=TYAMLMapping(Map.Items[0]);
  AssertEquals('Element count',2,Map2.Count);
  AssertScalar('3 - First key',Map2.Key[0],yttString,'four');
  AssertScalar('3 - First value',Map2.items[0],yttInteger,'4');
  AssertScalar('3 - second key',Map2.Key[1],yttString,'five');
  AssertScalar('3 - second value',Map2.items[1],yttInteger,'5');

  AssertScalar('2 - Second key',Map.Key[1],yttString,'six');
  AssertScalar('2 - second value',Map.Items[1],yttInteger,'6');
end;

procedure TTestYamlParser.TestBlockMappingUnindentedSequence;
var
  seq : TYAMLSequence;
  map : TYAMLMapping;
begin
  Parse(['one:',
         '- a',
         '- b',
         'two: c']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckBlock);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('1 - key',Map.Key[0],yttString,'one');
  AssertEquals('1 - item is Map',TYAMLSequence,Map.Items[0].ClassType);
  seq:=TYAMLSequence(Map.Items[0]);
  AssertEquals('> Element count',2,seq.Count);
  AssertScalar('> element - 1',seq.items[0],yttString,'a');
  AssertScalar('> element - 2',seq.items[1],yttString,'b');
  AssertScalar('2 - key',map.key[1],yttString,'two');
  AssertScalar('2 - item',map.items[1],yttString,'c');
end;


procedure TTestYamlParser.TestBlockMappingUnindentedSequenceWithIndent;
var
  seq : TYAMLSequence;
  map1,map2,map3 : TYAMLMapping;
begin
  Parse(['two:', // map1
         '  three:', // map 2
         '    four:', // map 3
         '    - enum', // enum ,
         '  five: a' // Map 2
         ]);
  Map1:=AssertMapping('1',AssertValue(TYAMLMapping));
  Map2:=AssertMapping('2',Map1.Items[0]);
  Map3:=AssertMapping('3',Map2.Items[0]);
  Seq:=AssertSequence('seq',Map3.Items[0]);
  AssertEquals('Seq count',1,Seq.Count);
  AssertEquals('map3 count',1,Map3.Count);
  AssertEquals('map2 count',2,Map2.Count);
  AssertScalar('map2 key 2',Map2.Key[1],yttString,'five');
  AssertScalar('map2 item 2',Map2[1],yttString,'a');
end;

procedure TTestYamlParser.TestBlockMappingFlowSequence;

var
  seq : TYAMLSequence;
  map : TYAMLMapping;

begin
  Parse(['one: []',
         'two: c']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckBlock);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('1 - key',Map.Key[0],yttString,'one');
  AssertEquals('1 - item is Map',TYAMLSequence,Map.Items[0].ClassType);
  seq:=TYAMLSequence(Map.Items[0]);
  AssertEquals('> Element count',0,seq.Count);
//  AssertScalar('> element - 1',seq.items[0],yttString,'a');
//  AssertScalar('> element - 2',seq.items[1],yttString,'b');
//  AssertScalar('2 - key',map.key[1],yttString,'two');
  AssertScalar('2 - key',map.key[1],yttString,'two');
  AssertScalar('2 - item',map.items[1],yttString,'c');
end;

procedure TTestYamlParser.TestFlowMapping;
var
  Map : TYAMLMapping;
begin
  Parse(['{ }']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckFlow);
  AssertEquals('Element count',0,Map.Count);
end;

procedure TTestYamlParser.TestFlowMappingOne;
var
  Map : TYAMLMapping;
begin
  Parse(['{ one : two }']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckFlow);
  AssertEquals('Element count',1,Map.Count);
  AssertScalar('First key',Map.Key[0],yttString,'one');
  AssertScalar('First value',Map.Items[0],yttString,'two');
end;

procedure TTestYamlParser.TestFlowMappingTwo;
var
  Map : TYAMLMapping;
begin
  Parse(['{ one : two, three: four }']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckFlow);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('First key',Map.Key[0],yttString,'one');
  AssertScalar('First value',Map.Items[0],yttString,'two');
  AssertScalar('Second key',Map.Key[1],yttString,'three');
  AssertScalar('Second value',Map.Items[1],yttString,'four');
end;

procedure TTestYamlParser.TestFlowMappingNested;
var
  Map : TYAMLMapping;
begin
  Parse(['{ one: two ,three: { five: six , seven: eight} }']);
  Map:=TYAMLMapping(AssertValue(TYAMLMapping));
  AssertTrue('Correct kind',Map.Kind=yckFlow);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('First key',Map.Key[0],yttString,'one');
  AssertScalar('First value',Map.Items[0],yttString,'two');
  AssertScalar('Second key',Map.Key[1],yttString,'three');
  AssertEquals('Map',TYAMLMapping,Map.Items[1].ClassType);
  Map:=TYAMLMapping(Map.Items[1]);
  AssertEquals('Element count',2,Map.Count);
  AssertScalar('2 - First key',Map.Key[0],yttString,'five');
  AssertScalar('2 - first value',Map.Items[0],yttString,'six');
  AssertScalar('2 - Second key',Map.Key[1],yttString,'seven');
  AssertScalar('2 - second value',Map.Items[1],yttString,'eight');
end;

procedure TTestYamlParser.TestEmptyDocument;
begin
  Parse(['---','...']);
  AssertNotNull('Data',Data);
  AssertEquals('YAML Stream',TYAMLStream,Data.ClassType);
  AssertEquals('YAML Stream item count',1,YAML.Count);
  AssertNotNull('Document',Document);
  AssertEquals('Document empty',0,Document.Count);
end;

procedure TTestYamlParser.TestVersionEmptyDocument;
begin
  Parse(['%YAML 1.2','---','...']);
  AssertNotNull('Data',Data);
  AssertEquals('YAML Stream',TYAMLStream,Data.ClassType);
  AssertEquals('YAML Stream item count',1,YAML.Count);
  AssertNotNull('Document',Document);
  AssertEquals('Major',1,Document.Version.Major);
  AssertEquals('Minor',2,Document.Version.Minor);
  AssertEquals('Document empty',0,Document.Count);
end;

procedure TTestYamlParser.TestMultiDocument;
var
  doc : TYAMLDocument;
begin
  Parse(['%YAML 1.2','---','abc','...','---','def','...']);
  AssertNotNull('Data',Data);
  AssertEquals('YAML Stream',TYAMLStream,Data.ClassType);
  AssertEquals('YAML Stream item count',2,YAML.Count);
  AssertEquals('YAML Stream document count',2,YAML.DocumentCount);
  Doc:=YAML.Documents[0];
  AssertNotNull('Document 1',Doc);
  AssertEquals('Document 1 Major',1,Doc.Version.Major);
  AssertEquals('Document 1 Minor',2,Doc.Version.Minor);
  AssertEquals('Document 1 element count',1,Doc.Count);
  AssertScalar('Document 1 element',Doc[0],yttString,'abc');
  Doc:=YAML.Documents[1];
  AssertNotNull('Document 2',doc);
  AssertEquals('Document 2 element count',1,Doc.Count);
  AssertScalar('Document 2 element',Doc[0],yttString,'def');
end;

procedure TTestYamlParser.TestMultiDocumentNoEnd;
var
  doc : TYAMLDocument;
begin
  Parse(['abc','---','def']);
  AssertNotNull('Data',Data);
  AssertEquals('YAML Stream',TYAMLStream,Data.ClassType);
  AssertEquals('YAML Stream item count',2,YAML.Count);
  AssertEquals('YAML Stream document count',2,YAML.DocumentCount);
  Doc:=YAML.Documents[0];
  AssertNotNull('Document 1',Doc);
  AssertEquals('Document 1 element count',1,Doc.Count);
  AssertScalar('Document 1 element',Doc[0],yttString,'abc');
  Doc:=YAML.Documents[1];
  AssertNotNull('Document 2',doc);
  AssertEquals('Document 2 element count',1,Doc.Count);
  AssertScalar('Document 2 element',Doc[0],yttString,'def');
end;

function TTestYamlParser.GetDocument: TYAMLDocument;

begin
  AssertTrue('Have documents',YAML.DocumentCount>0);
  Result:=YAML.Documents[0];
end;

function TTestYamlParser.GetStream: TYAMLStream;
begin
  AssertTrue('Have stream',Data is  TYAMLStream);
  Result:=Data as TYAMLStream;
end;

function TTestYamlParser.GetValue: TYAMLData;
var
  Doc : TYAMLDocument;
begin
  Doc:=GetDocument;
  AssertTrue('Have data',Doc.Count>0);
  Result:=Doc[0];
end;

procedure TTestYamlParser.Parse(aContent: array of string);
begin
  FParser:=TYAMLParser.Create(aContent);
  SetData(FParser.Parse);
end;

procedure TTestYamlParser.SetUp;
begin
  Inherited;
  FParser:=Nil;
end;

procedure TTestYamlParser.TearDown;
begin
  FreeAndNil(FParser);
  Inherited;
end;

initialization

  RegisterTest(TTestYamlParser);
end.

