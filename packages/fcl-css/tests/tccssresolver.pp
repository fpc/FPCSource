{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022 by Michael Van Canneyt (michael@freepascal.org)

    This file contains the tests for the CSS parser

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcCSSResolver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpcunit, testregistry, fpCSSParser, fpCSSTree,
  fpCSSResolver;

type
  TDemoNodeAttribute = (
    naLeft,
    naTop,
    naWidth,
    naHeight,
    naBorder,
    naDisplay,
    naColor
    );
  TDemoNodeAttributes = set of TDemoNodeAttribute;

const
  DemoAttributeNames: array[TDemoNodeAttribute] of string = (
    // case sensitive!
    'left',
    'top',
    'width',
    'height',
    'border',
    'display',
    'color'
    );

  DemoAttrIDBase = 100;

type
  TDemoPseudoClass = (
    pcActive,
    pcHover
    );
  TDemoPseudoClasses = set of TDemoPseudoClass;

type

  { TDemoNode }

  TDemoNode = class(TComponent,TCSSNode)
  private
    class var FAttributeInitialValues: array[TDemoNodeAttribute] of string;
  private
    FAttributeValues: array[TDemoNodeAttribute] of string;
    FNodes: TFPObjectList; // list of TDemoNode
    FCSSClasses: TStrings;
    FID: string;
    FParent: TDemoNode;
    FStyleElements: TCSSElement;
    FStyle: string;
    function GetAttribute(AIndex: TDemoNodeAttribute): string;
    function GetNodeCount: integer;
    function GetNodes(Index: integer): TDemoNode;
    procedure SetAttribute(AIndex: TDemoNodeAttribute; const AValue: string);
    procedure SetID(const AValue: string);
    procedure SetParent(const AValue: TDemoNode);
    procedure SetStyleElements(const AValue: TCSSElement);
    procedure SetStyle(const AValue: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    class function CSSClassName: string; virtual;
    function GetCSSClassName: string;
    class function CSSTypeID: TCSSNumericalID; virtual;
    function GetCSSTypeID: TCSSNumericalID;
    class function GetAttributeInitialValue(Attr: TDemoNodeAttribute): string; virtual;
    function HasCSSClass(const aClassName: string): boolean; virtual;
    procedure SetCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement); virtual;
    property ID: string read FID write SetID;
    property Parent: TDemoNode read FParent write SetParent;
    property NodeCount: integer read GetNodeCount;
    property Nodes[Index: integer]: TDemoNode read GetNodes; default;
    property CSSClasses: TStrings read FCSSClasses;
    property StyleElements: TCSSElement read FStyleElements write SetStyleElements;
    property Style: string read FStyle write SetStyle;
    // CSS attributes
    property Left: string index naLeft read GetAttribute write SetAttribute;
    property Top: string index naTop read GetAttribute write SetAttribute;
    property Width: string index naWidth read GetAttribute write SetAttribute;
    property Height: string index naHeight read GetAttribute write SetAttribute;
    property Border: string index naBorder read GetAttribute write SetAttribute;
    property Display: string index naDisplay read GetAttribute write SetAttribute;
    property Color: string index naColor read GetAttribute write SetAttribute;
    property Attribute[Attr: TDemoNodeAttribute]: string read GetAttribute write SetAttribute;
  end;

  { TDemoButton }

  TDemoButton = class(TDemoNode)
  public
    class function CSSClassName: string; override;
    class function CSSTypeID: TCSSNumericalID; override;
  end;

  { TDemoDocument }

  TDemoDocument = class(TComponent)
  private
    FNumericalIDs: array[TCSSNumericalIDKind] of TCSSNumericalIDs;
    FCSSResolver: TCSSResolver;
    FStyle: string;
    FStyleElements: TCSSElement;
    function GetNumericalIDs(Kind: TCSSNumericalIDKind): TCSSNumericalIDs;
    procedure SetNumericalIDs(Kind: TCSSNumericalIDKind;
      const AValue: TCSSNumericalIDs);
    procedure SetStyle(const AValue: string);
    procedure SetStyleElements(const AValue: TCSSElement);
  public
    Root: TDemoNode;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyStyle; virtual;
    procedure ApplyStyleToNode(Node: TDemoNode); virtual;

    property NumericalIDs[Kind: TCSSNumericalIDKind]: TCSSNumericalIDs read GetNumericalIDs write SetNumericalIDs;

    property StyleElements: TCSSElement read FStyleElements write SetStyleElements;
    property Style: string read FStyle write SetStyle;

    property CSSResolver: TCSSResolver read FCSSResolver;
  end;

  { TCustomTestCSSResolver }

  TCustomTestCSSResolver = class(TTestCase)
  Private
    FDoc: TDemoDocument;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    property Doc: TDemoDocument read FDoc;
  end;

  { TTestCSSResolver }

  TTestCSSResolver = class(TCustomTestCSSResolver)
  published
    procedure Test_Universal;
  end;

function LinesToStr(Args: array of const): string;

implementation

function LinesToStr(Args: array of const): string;
var
  s: String;
  i: Integer;
begin
  s:='';
  for i:=Low(Args) to High(Args) do
    case Args[i].VType of
      vtChar:         s += Args[i].VChar+LineEnding;
      vtString:       s += Args[i].VString^+LineEnding;
      vtPChar:        s += Args[i].VPChar+LineEnding;
      vtWideChar:     s += AnsiString(Args[i].VWideChar)+LineEnding;
      vtPWideChar:    s += AnsiString(Args[i].VPWideChar)+LineEnding;
      vtAnsiString:   s += AnsiString(Args[i].VAnsiString)+LineEnding;
      vtWidestring:   s += AnsiString(WideString(Args[i].VWideString))+LineEnding;
      vtUnicodeString:s += AnsiString(UnicodeString(Args[i].VUnicodeString))+LineEnding;
    end;
  Result:=s;
end;

{ TCustomTestCSSResolver }

procedure TCustomTestCSSResolver.SetUp;
begin
  inherited SetUp;
  FDoc:=TDemoDocument.Create(nil);
end;

procedure TCustomTestCSSResolver.TearDown;
begin
  FreeAndNil(FDoc);
  inherited TearDown;
end;

{ TTestCSSResolver }

procedure TTestCSSResolver.Test_Universal;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:='* { left: 10px; }';
  Doc.ApplyStyle;
  AssertEquals('left','10px',Doc.Root.Left);
end;

{ TDemoButton }

class function TDemoButton.CSSClassName: string;
begin
  Result:='button';
end;

class function TDemoButton.CSSTypeID: TCSSNumericalID;
begin
  Result:=101;
end;

{ TDemoDocument }

procedure TDemoDocument.SetStyle(const AValue: string);
var
  ss: TStringStream;
  aParser: TCSSParser;
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  FreeAndNil(FStyleElements);
  aParser:=nil;
  ss:=TStringStream.Create(Style);
  try
    aParser:=TCSSParser.Create(ss);
    FStyleElements:=aParser.Parse;
  finally
    aParser.Free;
  end;
end;

function TDemoDocument.GetNumericalIDs(Kind: TCSSNumericalIDKind
  ): TCSSNumericalIDs;
begin
  Result:=FNumericalIDs[Kind];
end;

procedure TDemoDocument.SetNumericalIDs(Kind: TCSSNumericalIDKind;
  const AValue: TCSSNumericalIDs);
begin
  FNumericalIDs[Kind]:=AValue;
end;

procedure TDemoDocument.SetStyleElements(const AValue: TCSSElement);
begin
  if FStyleElements=AValue then Exit;
  FStyleElements.Free;
  FStyleElements:=AValue;
end;

constructor TDemoDocument.Create(AOwner: TComponent);
var
  Attr: TDemoNodeAttribute;
  TypeIDs, AttributeIDs: TCSSNumericalIDs;
  NumKind: TCSSNumericalIDKind;
begin
  inherited Create(AOwner);

  for NumKind in TCSSNumericalIDKind do
    FNumericalIDs[NumKind]:=TCSSNumericalIDs.Create(NumKind);
  TypeIDs:=FNumericalIDs[nikType];
  TypeIDs['*']:=CSSTypeIDUniversal;
  if TypeIDs['*']<>CSSTypeIDUniversal then
    raise Exception.Create('20220909004740');

  TypeIDs[TDemoNode.CSSClassName]:=TDemoNode.CSSTypeID;
  TypeIDs[TDemoButton.CSSClassName]:=TDemoButton.CSSTypeID;

  AttributeIDs:=FNumericalIDs[nikAttribute];
  AttributeIDs['all']:=CSSAttributeIDAll;
  for Attr in TDemoNodeAttribute do
    AttributeIDs[DemoAttributeNames[Attr]]:=ord(Attr)+DemoAttrIDBase;

  FCSSResolver:=TCSSResolver.Create;
  for NumKind in TCSSNumericalIDKind do
    CSSResolver.NumericalIDs[NumKind]:=FNumericalIDs[NumKind];

  Root:=TDemoNode.Create(Self);
  Root.Name:='Root';
end;

destructor TDemoDocument.Destroy;
var
  NumKind: TCSSNumericalIDKind;
begin
  FreeAndNil(FCSSResolver);
  FreeAndNil(Root);
  FreeAndNil(FStyleElements);
  for NumKind in TCSSNumericalIDKind do
    FreeAndNil(FNumericalIDs[NumKind]);
  inherited Destroy;
end;

procedure TDemoDocument.ApplyStyle;

  procedure Traverse(Node: TDemoNode);
  var
    i: Integer;
  begin
    ApplyStyleToNode(Node);
    for i:=0 to Node.NodeCount-1 do
      Traverse(Node[i]);
  end;

begin
  CSSResolver.Style:=StyleElements;
  Traverse(Root);
end;

procedure TDemoDocument.ApplyStyleToNode(Node: TDemoNode);
begin
  CSSResolver.Compute(Node,Node.StyleElements);
end;

{ TDemoNode }

function TDemoNode.GetAttribute(AIndex: TDemoNodeAttribute): string;
begin
  Result:=FAttributeValues[AIndex];
end;

function TDemoNode.GetNodeCount: integer;
begin
  Result:=FNodes.Count;
end;

function TDemoNode.GetNodes(Index: integer): TDemoNode;
begin
  Result:=TDemoNode(FNodes[Index]);
end;

procedure TDemoNode.SetAttribute(AIndex: TDemoNodeAttribute;
  const AValue: string);
begin
  if FAttributeValues[AIndex]=AValue then exit;
  FAttributeValues[AIndex]:=AValue;
end;

procedure TDemoNode.SetID(const AValue: string);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure TDemoNode.SetParent(const AValue: TDemoNode);
begin
  if FParent=AValue then Exit;
  if AValue=Self then
    raise Exception.Create('cycle');

  if FParent<>nil then
  begin
    FParent.FNodes.Remove(Self);
  end;
  FParent:=AValue;
  if FParent<>nil then
  begin
    FParent.FNodes.Add(Self);
    FreeNotification(FParent);
  end;
end;

procedure TDemoNode.SetStyleElements(const AValue: TCSSElement);
begin
  if FStyleElements=AValue then Exit;
  FreeAndNil(FStyleElements);
  FStyleElements:=AValue;
end;

procedure TDemoNode.SetStyle(const AValue: string);
var
  ss: TStringStream;
  aParser: TCSSParser;
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  FreeAndNil(FStyleElements);
  aParser:=nil;
  ss:=TStringStream.Create(Style);
  try
    aParser:=TCSSParser.Create(ss);
    FStyleElements:=aParser.Parse;
  finally
    aParser.Free;
  end;
end;

procedure TDemoNode.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent=Self then exit;
  if Operation=opRemove then
  begin
    if FNodes<>nil then
      FNodes.Remove(AComponent);
  end;
end;

constructor TDemoNode.Create(AOwner: TComponent);
var
  a: TDemoNodeAttribute;
begin
  inherited Create(AOwner);
  FNodes:=TFPObjectList.Create(false);
  FCSSClasses:=TStringList.Create;
  for a in TDemoNodeAttribute do
    FAttributeValues[a]:=FAttributeInitialValues[a];
end;

destructor TDemoNode.Destroy;
begin
  Clear;
  FreeAndNil(FNodes);
  FreeAndNil(FCSSClasses);
  inherited Destroy;
end;

procedure TDemoNode.Clear;
var
  i: Integer;
begin
  FCSSClasses.Clear;
  for i:=NodeCount-1 downto 0 do
    Nodes[i].Parent:=nil;
  FNodes.Clear;
end;

class function TDemoNode.CSSClassName: string;
begin
  Result:='node';
end;

class function TDemoNode.GetAttributeInitialValue(Attr: TDemoNodeAttribute
  ): string;
begin
  case Attr of
    naLeft: Result:='0px';
    naTop: Result:='0px';
    naWidth: Result:='';
    naHeight: Result:='';
    naBorder: Result:='1px';
    naDisplay: Result:='inline';
    naColor: Result:='#000';
  end;
end;

function TDemoNode.HasCSSClass(const aClassName: string): boolean;
var
  i: Integer;
begin
  for i:=0 to CSSClasses.Count-1 do
    if aClassName=CSSClasses[i] then
      exit(true);
  Result:=false;
end;

procedure TDemoNode.SetCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement);
var
  Attr: TDemoNodeAttribute;
  s: TCSSString;
begin
  if (AttrID<DemoAttrIDBase) or (AttrID>ord(High(TDemoNodeAttribute))+DemoAttrIDBase) then
    raise Exception.Create('TDemoNode.SetCSSValue invalid AttrID '+IntToStr(AttrID));
  Attr:=TDemoNodeAttribute(AttrID-DemoAttrIDBase);
  s:=Value.AsString;
  {$IFDEF VerboseCSSResolver}
  writeln('TDemoNode.SetCSSValue ',DemoAttributeNames[Attr],':="',s,'"');
  {$ENDIF}
  Attribute[Attr]:=s;
end;

function TDemoNode.GetCSSClassName: string;
begin
  Result:=CSSClassName;
end;

class function TDemoNode.CSSTypeID: TCSSNumericalID;
begin
  Result:=100;
end;

function TDemoNode.GetCSSTypeID: TCSSNumericalID;
begin
  Result:=CSSTypeID;
end;

initialization
  RegisterTests([TTestCSSResolver]);
end.

