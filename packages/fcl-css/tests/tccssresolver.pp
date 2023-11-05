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
  DemoPseudoClassIDBase = 100;

type
  TDemoPseudoClass = (
    pcActive,
    pcHover
    );
  TDemoPseudoClasses = set of TDemoPseudoClass;

const
  DemoPseudoClassNames: array[TDemoPseudoClass] of string = (
    // case sensitive!
    ':active',
    ':hover'
    );

type

  { TDemoNode }

  TDemoNode = class(TComponent,ICSSNode)
  private
    class var FAttributeInitialValues: array[TDemoNodeAttribute] of string;
  private
    FActive: boolean;
    FAttributeValues: array[TDemoNodeAttribute] of string;
    FHover: boolean;
    FNodes: TFPObjectList; // list of TDemoNode
    FCSSClasses: TStrings;
    FParent: TDemoNode;
    FStyleElements: TCSSElement;
    FStyle: string;
    function GetAttribute(AIndex: TDemoNodeAttribute): string;
    function GetNodeCount: integer;
    function GetNodes(Index: integer): TDemoNode;
    procedure SetActive(const AValue: boolean);
    procedure SetAttribute(AIndex: TDemoNodeAttribute; const AValue: string);
    procedure SetHover(const AValue: boolean);
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
    function GetCSSID: TCSSString; virtual;
    class function CSSTypeName: TCSSString; virtual;
    function GetCSSTypeName: TCSSString;
    class function CSSTypeID: TCSSNumericalID; virtual;
    function GetCSSTypeID: TCSSNumericalID;
    class function GetAttributeInitialValue(Attr: TDemoNodeAttribute): string; virtual;
    function HasCSSClass(const aClassName: TCSSString): boolean; virtual;
    function CheckCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement
      ): boolean; virtual;
    procedure SetCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement); virtual;
    function GetCSSParent: ICSSNode; virtual;
    function GetCSSIndex: integer; virtual;
    function GetCSSNextSibling: ICSSNode; virtual;
    function GetCSSPreviousSibling: ICSSNode; virtual;
    function GetCSSChildCount: integer; virtual;
    function GetCSSChild(const anIndex: integer): ICSSNode; virtual;
    function GetCSSNextOfType: ICSSNode; virtual;
    function GetCSSPreviousOfType: ICSSNode; virtual;
    function GetCSSAttributeClass: TCSSString; virtual;
    function HasCSSAttribute(const AttrID: TCSSNumericalID): boolean; virtual;
    function GetCSSAttribute(const AttrID: TCSSNumericalID): TCSSString; virtual;
    function HasCSSPseudoClass(const {%H-}AttrID: TCSSNumericalID): boolean; virtual;
    function GetCSSEmpty: boolean; virtual;
    function GetCSSDepth: integer; virtual;
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
    // CSS pseudo classes
    property Active: boolean read FActive write SetActive;
    property Hover: boolean read FHover write SetHover;
    function HasPseudoClass(PseudoClass: TDemoPseudoClass): boolean;
  end;
  TDemoNodeClass = class of TDemoNode;

  { TDemoDiv }

  TDemoDiv = class(TDemoNode)
  public
    class function CSSTypeName: TCSSString; override;
    class function CSSTypeID: TCSSNumericalID; override;
  end;

  { TDemoSpan }

  TDemoSpan = class(TDemoNode)
  public
    class function CSSTypeName: TCSSString; override;
    class function CSSTypeID: TCSSNumericalID; override;
  end;

  { TDemoButton }

  TDemoButton = class(TDemoNode)
  private
    FCaption: string;
    procedure SetCaption(const AValue: string);
  public
    class var CSSCaptionID: TCSSNumericalID;
    class function CSSTypeName: TCSSString; override;
    class function CSSTypeID: TCSSNumericalID; override;
    function HasCSSAttribute(const AttrID: TCSSNumericalID): boolean; override;
    function GetCSSAttribute(const AttrID: TCSSNumericalID): TCSSString;
      override;
    procedure SetCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement); override;
    property Caption: string read FCaption write SetCaption;
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
    procedure Test_Selector_Universal;
    procedure Test_Selector_Type;
    // Test list spaces "div, button ,span {}"
    procedure Test_Selector_Id;
    procedure Test_Selector_Class;
    procedure Test_Selector_ClassClass; // AND combinator
    procedure Test_Selector_ClassSpaceClass; // Descendant combinator
    procedure Test_Selector_TypeCommaType; // OR combinator
    procedure Test_Selector_ClassGTClass; // child combinator
    procedure Test_Selector_TypePlusType; // adjacent sibling combinator
    procedure Test_Selector_TypeTildeType; // general sibling combinator
    procedure Test_Selector_HasAttribute;
    procedure Test_Selector_AttributeEquals;
    procedure Test_Selector_AttributeEqualsI;
    procedure Test_Selector_AttributeBeginsWith;
    procedure Test_Selector_AttributeEndsWith;
    procedure Test_Selector_AttributeBeginsWithHyphen;
    procedure Test_Selector_AttributeContainsWord;
    procedure Test_Selector_AttributeContainsSubstring;
    // ToDo: "all"

    // pseudo classes
    procedure Test_Selector_Root;
    procedure Test_Selector_Empty;
    procedure Test_Selector_FirstChild;
    procedure Test_Selector_LastChild;
    procedure Test_Selector_OnlyChild;
    procedure Test_Selector_Not;
    procedure Test_Selector_NthChild;
    procedure Test_Selector_NthLastChild;
    procedure Test_Selector_NthChildOf;
    procedure Test_Selector_FirstOfType;
    procedure Test_Selector_LastOfType;
    procedure Test_Selector_OnlyOfType;
    procedure Test_Selector_NthOfType;
    procedure Test_Selector_NthLastOfType;
    procedure Test_Selector_Is;
    procedure Test_Selector_Where;
    // ToDo: div:has(>img)
    // ToDo: div:has(+img)
    // ToDo: :dir()
    // ToDo: :lang()

    // custom pseudo classes
    procedure Test_Selector_Hover;

    // inline style
    procedure Test_InlineStyle;

    // ToDo: specifity

    // pseudo elements

    // skipping for forward compatibility
    // ToDo: invalid token in selector makes selector invalid

  end;

function LinesToStr(const Args: array of const): string;

implementation

function LinesToStr(const Args: array of const): string;
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

procedure TTestCSSResolver.Test_Selector_Universal;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:='* { left: 10px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','10px',Doc.Root.Left);
end;

procedure TTestCSSResolver.Test_Selector_Type;
var
  Button: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button:=TDemoButton.Create(Doc);
  Button.Parent:=Doc.Root;
  Doc.Style:='button { left: 11px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button.left','11px',Button.Left);
end;

procedure TTestCSSResolver.Test_Selector_Id;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=TDemoButton.Create(Doc);
  Button1.Name:='Button1';
  Button1.Parent:=Doc.Root;
  Doc.Style:='#Button1 { left: 12px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','12px',Button1.Left);
end;

procedure TTestCSSResolver.Test_Selector_Class;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=TDemoButton.Create(Doc);
  Button1.CSSClasses.Add('west');
  Button1.Parent:=Doc.Root;
  Doc.Style:='.west { left: 13px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','13px',Button1.Left);
end;

procedure TTestCSSResolver.Test_Selector_ClassClass;
var
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(Doc);
  Button1.CSSClasses.Add('west');
  Button1.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(Doc);
  Button2.CSSClasses.DelimitedText:='west south';
  AssertEquals('Button2.CSSClasses.Count',2,Button2.CSSClasses.Count);
  Button2.Parent:=Doc.Root;

  Doc.Style:='.west.south { left: 10px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
end;

procedure TTestCSSResolver.Test_Selector_ClassSpaceClass;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.CSSClasses.Add('bird');

  Button1:=TDemoButton.Create(Doc);
  Button1.CSSClasses.Add('west');
  Button1.Parent:=Doc.Root;

  Doc.Style:='.bird .west { left: 10px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','10px',Button1.Left);
end;

procedure TTestCSSResolver.Test_Selector_TypeCommaType;
var
  Button1: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Doc.Style:='div, button { left: 10px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','10px',Button1.Left);
  AssertEquals('Div1.left','10px',Div1.Left);
end;

procedure TTestCSSResolver.Test_Selector_ClassGTClass;
var
  Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.CSSClasses.Add('lvl1');

  Div1:=TDemoDiv.Create(Doc);
  Div1.CSSClasses.Add('lvl2');
  Div1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(Doc);
  Div2.CSSClasses.Add('lvl3');
  Div2.Parent:=Div1;

  Doc.Style:=LinesToStr([
  '.lvl1>.lvl2 { left: 10px; }',
  '.lvl1>.lvl3 { top: 11px; }',
  '.lvl2>.lvl3 { width: 12px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Root.top','',Doc.Root.Top);
  AssertEquals('Root.width','',Doc.Root.Width);
  AssertEquals('Div1.left','10px',Div1.Left);
  AssertEquals('Div1.top','',Div1.Top);
  AssertEquals('Div1.width','',Div1.Width);
  AssertEquals('Div2.left','',Div2.Left);
  AssertEquals('Div2.top','',Div2.Top);
  AssertEquals('Div2.width','12px',Div2.Width);
end;

procedure TTestCSSResolver.Test_Selector_TypePlusType;
var
  Button1, Button2, Button3: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(Doc);
  Button2.Parent:=Doc.Root;

  Button3:=TDemoButton.Create(Doc);
  Button3.Parent:=Doc.Root;

  Doc.Style:='div+button { left: 10px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Div1.left','',Div1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
  AssertEquals('Button3.left','',Button3.Left);
end;

procedure TTestCSSResolver.Test_Selector_TypeTildeType;
var
  Button1, Button2, Button3: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(Doc);
  Button2.Parent:=Doc.Root;

  Button3:=TDemoButton.Create(Doc);
  Button3.Parent:=Doc.Root;

  Doc.Style:='div~button { left: 10px; }';
  Doc.ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Div1.left','',Div1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
  AssertEquals('Button3.left','10px',Button3.Left);
end;

procedure TTestCSSResolver.Test_Selector_HasAttribute;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;
  Button1.Left:='2px';

  Doc.Style:=LinesToStr([
  '[left] { top: 3px; }',
  '[caption] { width: 4px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','3px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','3px',Button1.Top);
  AssertEquals('Button1.Width','4px',Button1.Width);
end;

procedure TTestCSSResolver.Test_Selector_AttributeEquals;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Left:='2px';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;
  Button1.Left:='3px';
  Button1.Color:='maybe black';

  Doc.Style:=LinesToStr([
  '[left=2px] { top: 4px; }',
  '[color="maybe black"] { width: 5px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Button1.Top','',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestCSSResolver.Test_Selector_AttributeEqualsI;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Left:='2px';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;
  Button1.Left:='3px';
  Button1.Color:='maybe Black';

  Doc.Style:=LinesToStr([
  '[left="2Px" i] { top: 4px; }',
  '[color="Maybe bLack" i] { width: 5px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Button1.Top','',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestCSSResolver.Test_Selector_AttributeBeginsWith;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Left:='Foo';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;
  Button1.Left:='Foo Bar';

  Doc.Style:=LinesToStr([
  '[left^=Fo] { top: 4px; }',
  '[left^="Foo B"] { width: 5px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','4px',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestCSSResolver.Test_Selector_AttributeEndsWith;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Left:='Foo';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;
  Button1.Left:='Foo Bar';

  Doc.Style:=LinesToStr([
  '[left$=o] { top: 4px; }',
  '[left$="o Bar"] { width: 5px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestCSSResolver.Test_Selector_AttributeBeginsWithHyphen;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Left:='Foo';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;
  Button1.Left:='Foo-Bar';

  Doc.Style:=LinesToStr([
  '[left|=Foo] { top: 4px; }',
  '[left|="Fo"] { width: 5px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','4px',Button1.Top);
  AssertEquals('Button1.Width','',Button1.Width);
end;

procedure TTestCSSResolver.Test_Selector_AttributeContainsWord;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Left:='One Two Three';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;
  Button1.Left:='Four Five';

  Doc.Style:=LinesToStr([
  '[left~=One] { top: 4px; }',
  '[left~=Two] { width: 5px; }',
  '[left~=Three] { height: 6px; }',
  '[left~="Four Five"] { color: #123; }',  // not one word, so does not match!
  '[left~=our] { display: none; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','5px',Doc.Root.Width);
  AssertEquals('Root.Height','6px',Doc.Root.Height);
  AssertEquals('Root.Color','',Doc.Root.Color);
  AssertEquals('Root.Display','',Doc.Root.Display);
  AssertEquals('Button1.Top','',Button1.Top);
  AssertEquals('Button1.Width','',Button1.Width);
  AssertEquals('Button1.Height','',Button1.Height);
  AssertEquals('Button1.Color','',Button1.Color);
  AssertEquals('Button1.Display','',Button1.Display);
end;

procedure TTestCSSResolver.Test_Selector_AttributeContainsSubstring;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Left:='Foo';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;
  Button1.Left:='Foo Bar';

  Doc.Style:=LinesToStr([
  '[left*=oo] { top: 4px; }',
  '[left*="o B"] { width: 5px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','4px',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestCSSResolver.Test_Selector_Root;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Left:='Foo';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':roOt { top: 4px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Button1.Top','',Button1.Top);
end;

procedure TTestCSSResolver.Test_Selector_Empty;
var
  Div1, Div11, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':eMpty { left: 1px; }',
  'div:emPty { top: 2px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Div11.Left','1px',Div11.Left);
  AssertEquals('Div11.Top','2px',Div11.Top);
  AssertEquals('Div2.Left','1px',Div2.Left);
  AssertEquals('Div2.Top','2px',Div2.Top);
end;

procedure TTestCSSResolver.Test_Selector_FirstChild;
var
  Div1, Div11, Div12, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Div12:=TDemoDiv.Create(Doc);
  Div12.Parent:=Div1;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':first-child { left: 1px; }',
  'div:first-child { top: 2px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','1px',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','1px',Div1.Left);
  AssertEquals('Div1.Top','2px',Div1.Top);
  AssertEquals('Div11.Left','1px',Div11.Left);
  AssertEquals('Div11.Top','2px',Div11.Top);
  AssertEquals('Div12.Left','',Div12.Left);
  AssertEquals('Div12.Top','',Div12.Top);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Div2.Top','',Div2.Top);
end;

procedure TTestCSSResolver.Test_Selector_LastChild;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Button12:=TDemoButton.Create(Doc);
  Button12.Parent:=Div1;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':last-child { left: 6px; }',
  'div:last-child { top: 7px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','6px',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Div11.Left','',Div11.Left);
  AssertEquals('Div11.Top','',Div11.Top);
  AssertEquals('Button12.Left','6px',Button12.Left);
  AssertEquals('Button12.Top','',Button12.Top);
  AssertEquals('Div2.Left','6px',Div2.Left);
  AssertEquals('Div2.Top','7px',Div2.Top);
end;

procedure TTestCSSResolver.Test_Selector_OnlyChild;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Button12:=TDemoButton.Create(Doc);
  Button12.Parent:=Div2;

  Doc.Style:=LinesToStr([
  ':only-child { left: 8px; }',
  'div:only-child { top: 9px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','8px',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Div11.Left','8px',Div11.Left);
  AssertEquals('Div11.Top','9px',Div11.Top);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Div2.Top','',Div2.Top);
  AssertEquals('Button12.Left','8px',Button12.Left);
  AssertEquals('Button12.Top','',Button12.Top);
end;

procedure TTestCSSResolver.Test_Selector_Not;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Button12:=TDemoButton.Create(Doc);
  Button12.Parent:=Div2;

  Doc.Style:=LinesToStr([
  ':not(:only-child) { left: 8px; }',
  ':not(div:only-child) { top: 9px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Root.Top','9px',Doc.Root.Top);
  AssertEquals('Div1.Left','8px',Div1.Left);
  AssertEquals('Div1.Top','9px',Div1.Top);
  AssertEquals('Div11.Left','',Div11.Left);
  AssertEquals('Div11.Top','',Div11.Top);
  AssertEquals('Div2.Left','8px',Div2.Left);
  AssertEquals('Div2.Top','9px',Div2.Top);
  AssertEquals('Button12.Left','',Button12.Left);
  AssertEquals('Button12.Top','9px',Button12.Top);
end;

procedure TTestCSSResolver.Test_Selector_NthChild;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Div3:=TDemoDiv.Create(Doc);
  Div3.Parent:=Doc.Root;

  Div4:=TDemoDiv.Create(Doc);
  Div4.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':nth-child(2n+1) { left: 8px; }',
  ':nth-child(n+3) { border: 6px; }',
  ':nth-child(-n+2) { display: inline; }',
  ':nth-child(even) { top: 3px; }',
  ':nth-child(odd) { width: 4px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Root.Border','',Doc.Root.Border);
  AssertEquals('Root.Display','',Doc.Root.Display);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Div1.Left','8px',Div1.Left);
  AssertEquals('Div1.Border','',Div1.Border);
  AssertEquals('Div1.Display','inline',Div1.Display);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Div1.Width','4px',Div1.Width);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Div2.Border','',Div2.Border);
  AssertEquals('Div2.Display','inline',Div2.Display);
  AssertEquals('Div2.Top','3px',Div2.Top);
  AssertEquals('Div2.Width','',Div2.Width);
  AssertEquals('Div3.Left','8px',Div3.Left);
  AssertEquals('Div3.Border','6px',Div3.Border);
  AssertEquals('Div3.Display','',Div3.Display);
  AssertEquals('Div3.Top','',Div3.Top);
  AssertEquals('Div3.Width','4px',Div3.Width);
  AssertEquals('Div4.Left','',Div4.Left);
  AssertEquals('Div4.Border','6px',Div4.Border);
  AssertEquals('Div4.Display','',Div4.Display);
  AssertEquals('Div4.Top','3px',Div4.Top);
  AssertEquals('Div4.Width','',Div4.Width);
end;

procedure TTestCSSResolver.Test_Selector_NthLastChild;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Div3:=TDemoDiv.Create(Doc);
  Div3.Parent:=Doc.Root;

  Div4:=TDemoDiv.Create(Doc);
  Div4.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':nth-last-child(2n+1) { left: 8px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div2.Left','8px',Div2.Left);
  AssertEquals('Div3.Left','',Div3.Left);
  AssertEquals('Div4.Left','8px',Div4.Left);
end;

procedure TTestCSSResolver.Test_Selector_NthChildOf;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;
  Div2.Top:='3px';

  Div3:=TDemoDiv.Create(Doc);
  Div3.Parent:=Doc.Root;
  Div3.Top:='3px';

  Div4:=TDemoDiv.Create(Doc);
  Div4.Parent:=Doc.Root;
  Div4.Top:='3px';

  Doc.Style:=LinesToStr([
  ':nth-child(2n+1 of [top=3px]) { left: 5px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div2.Left','5px',Div2.Left);
  AssertEquals('Div3.Left','',Div3.Left);
  AssertEquals('Div4.Left','5px',Div4.Left);
end;

procedure TTestCSSResolver.Test_Selector_FirstOfType;
var
  Div1, Div11, Div13, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Button12:=TDemoButton.Create(Doc);
  Button12.Parent:=Div1;

  Div13:=TDemoDiv.Create(Doc);
  Div13.Parent:=Div1;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':first-of-type { left: 6px; }',
  'div:first-of-type { top: 7px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','6px',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','6px',Div1.Left);
  AssertEquals('Div1.Top','7px',Div1.Top);
  AssertEquals('Div11.Left','6px',Div11.Left);
  AssertEquals('Div11.Top','7px',Div11.Top);
  AssertEquals('Button12.Left','6px',Button12.Left);
  AssertEquals('Button12.Top','',Button12.Top);
  AssertEquals('Div13.Left','',Div13.Left);
  AssertEquals('Div13.Top','',Div13.Top);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Div2.Top','',Div2.Top);
end;

procedure TTestCSSResolver.Test_Selector_LastOfType;
var
  Div1, Div11, Div13, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Button12:=TDemoButton.Create(Doc);
  Button12.Parent:=Div1;

  Div13:=TDemoDiv.Create(Doc);
  Div13.Parent:=Div1;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':last-of-type { left: 6px; }',
  'div:last-of-type { top: 7px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','6px',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Div11.Left','',Div11.Left);
  AssertEquals('Div11.Top','',Div11.Top);
  AssertEquals('Button12.Left','6px',Button12.Left);
  AssertEquals('Button12.Top','',Button12.Top);
  AssertEquals('Div13.Left','6px',Div13.Left);
  AssertEquals('Div13.Top','7px',Div13.Top);
  AssertEquals('Div2.Left','6px',Div2.Left);
  AssertEquals('Div2.Top','7px',Div2.Top);
end;

procedure TTestCSSResolver.Test_Selector_OnlyOfType;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Button12:=TDemoButton.Create(Doc);
  Button12.Parent:=Div1;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':only-of-type { left: 6px; }',
  'div:only-of-type { top: 7px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','6px',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Div11.Left','6px',Div11.Left);
  AssertEquals('Div11.Top','7px',Div11.Top);
  AssertEquals('Button12.Left','6px',Button12.Left);
  AssertEquals('Button12.Top','',Button12.Top);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Div2.Top','',Div2.Top);
end;

procedure TTestCSSResolver.Test_Selector_NthOfType;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Div3:=TDemoDiv.Create(Doc);
  Div3.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(Doc);
  Button2.Parent:=Doc.Root;

  Div4:=TDemoDiv.Create(Doc);
  Div4.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':nth-of-type(2n+1) { left: 8px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','8px',Div1.Left);
  AssertEquals('Button1.Left','8px',Button1.Left);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Div3.Left','8px',Div3.Left);
  AssertEquals('Button2.Left','',Button2.Left);
  AssertEquals('Div4.Left','',Div4.Left);
end;

procedure TTestCSSResolver.Test_Selector_NthLastOfType;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Div3:=TDemoDiv.Create(Doc);
  Div3.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(Doc);
  Button2.Parent:=Doc.Root;

  Div4:=TDemoDiv.Create(Doc);
  Div4.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':nth-last-of-type(2n+1) { left: 8px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Button1.Left','',Button1.Left);
  AssertEquals('Div2.Left','8px',Div2.Left);
  AssertEquals('Div3.Left','',Div3.Left);
  AssertEquals('Button2.Left','8px',Button2.Left);
  AssertEquals('Div4.Left','8px',Div4.Left);
end;

procedure TTestCSSResolver.Test_Selector_Is;
var
  Div1, Div2: TDemoDiv;
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;
  Div1.Top:='3px';

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(Doc);
  Button2.Parent:=Doc.Root;
  Button2.Top:='3px';

  Doc.Style:=LinesToStr([
  ':is(div, button)[top=3px] { left: 7px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','7px',Div1.Left);
  AssertEquals('Button1.Left','',Button1.Left);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Button2.Left','7px',Button2.Left);
end;

procedure TTestCSSResolver.Test_Selector_Where;
var
  Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;
  Div1.Top:='3px';

  Div2:=TDemoDiv.Create(Doc);
  Div2.Parent:=Div1;
  Div2.Top:='3px';

  Doc.Style:=LinesToStr([
  ':where(div[top=3px]) { left: 1px; }',
  'div div { left: 2px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','1px',Div1.Left);
  AssertEquals('Div2.Left','2px',Div2.Left);
end;

procedure TTestCSSResolver.Test_Selector_Hover;
var
  Div1, Div11: TDemoDiv;
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;
  Div1.Hover:=true;

  Button1:=TDemoButton.Create(Doc);
  Button1.Parent:=Div1;
  Button1.Hover:=true;

  Div11:=TDemoDiv.Create(Doc);
  Div11.Parent:=Div1;

  Doc.Style:=LinesToStr([
  ':hover { left: 1px; }',
  'button:hover { top: 2px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','1px',Div1.Left);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Button1.Left','1px',Button1.Left);
  AssertEquals('Button1.Top','2px',Button1.Top);
  AssertEquals('Div11.Left','',Div11.Left);
  AssertEquals('Div11.Top','',Div11.Top);
end;

procedure TTestCSSResolver.Test_InlineStyle;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(Doc);
  Div1.Parent:=Doc.Root;
  Div1.Style:='left: 10px; top: 5px';

  Doc.Style:=LinesToStr([
  'div { left: 6px; }',
  '']);
  Doc.ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','10px',Div1.Left);
  AssertEquals('Div1.Top','5px',Div1.Top);
end;

{ TDemoDiv }

class function TDemoDiv.CSSTypeName: TCSSString;
begin
  Result:='div';
end;

class function TDemoDiv.CSSTypeID: TCSSNumericalID;
begin
  Result:=101;
end;

{ TDemoSpan }

class function TDemoSpan.CSSTypeName: TCSSString;
begin
  Result:='span';
end;

class function TDemoSpan.CSSTypeID: TCSSNumericalID;
begin
  Result:=102;
end;

{ TDemoButton }

procedure TDemoButton.SetCaption(const AValue: string);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

class function TDemoButton.CSSTypeName: TCSSString;
begin
  Result:='button';
end;

class function TDemoButton.CSSTypeID: TCSSNumericalID;
begin
  Result:=103;
end;

function TDemoButton.HasCSSAttribute(const AttrID: TCSSNumericalID): boolean;
begin
  Result:=(AttrID=CSSCaptionID) or inherited HasCSSAttribute(AttrID);
end;

function TDemoButton.GetCSSAttribute(const AttrID: TCSSNumericalID): TCSSString;
begin
  if AttrID=CSSCaptionID then
    Result:=Caption
  else
    Result:=inherited GetCSSAttribute(AttrID);
end;

procedure TDemoButton.SetCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement);
begin
  if AttrID=CSSCaptionID then
    SetCaption(Value.AsString)
  else
    inherited SetCSSValue(AttrID, Value);
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
  TypeIDs, AttributeIDs, PseudoClassIDs: TCSSNumericalIDs;
  NumKind: TCSSNumericalIDKind;
  AttrID: TCSSNumericalID;
  PseudoClass: TDemoPseudoClass;
begin
  inherited Create(AOwner);

  for NumKind in TCSSNumericalIDKind do
    FNumericalIDs[NumKind]:=TCSSNumericalIDs.Create(NumKind);

  // register all css types
  TypeIDs:=FNumericalIDs[nikType];
  TypeIDs['*']:=CSSTypeID_Universal;
  if TypeIDs['*']<>CSSTypeID_Universal then
    raise Exception.Create('20220909004740');

  TypeIDs[TDemoNode.CSSTypeName]:=TDemoNode.CSSTypeID;
  TypeIDs[TDemoDiv.CSSTypeName]:=TDemoDiv.CSSTypeID;
  TypeIDs[TDemoButton.CSSTypeName]:=TDemoButton.CSSTypeID;

  // register all css attributes
  AttributeIDs:=FNumericalIDs[nikAttribute];
  AttributeIDs['all']:=CSSAttributeID_All;
  // add basic element attributes
  AttrID:=DemoAttrIDBase;
  for Attr in TDemoNodeAttribute do
  begin
    AttributeIDs[DemoAttributeNames[Attr]]:=AttrID;
    inc(AttrID);
  end;
  // add button caption attribute
  TDemoButton.CSSCaptionID:=AttrID;
  AttributeIDs['caption']:=AttrID;
  inc(AttrID);

  // register css pseudo attributes
  PseudoClassIDs:=FNumericalIDs[nikPseudoClass];
  AttrID:=DemoPseudoClassIDBase;
  for PseudoClass in TDemoPseudoClass do
  begin
    PseudoClassIDs[DemoPseudoClassNames[PseudoClass]]:=AttrID;
    inc(AttrID);
  end;
  if PseudoClassIDs[DemoPseudoClassNames[pcHover]]<>DemoPseudoClassIDBase+ord(pcHover) then
    raise Exception.Create('20231008232201');

  // create the css resolver
  FCSSResolver:=TCSSResolver.Create(nil);
  for NumKind in TCSSNumericalIDKind do
    CSSResolver.NumericalIDs[NumKind]:=FNumericalIDs[NumKind];

  // create a demo root node
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
  if CSSResolver.StyleCount=0 then
    CSSResolver.AddStyle(StyleElements)
  else
    CSSResolver.Styles[0]:=StyleElements;
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

procedure TDemoNode.SetHover(const AValue: boolean);
begin
  if FHover=AValue then Exit;
  FHover:=AValue;
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

procedure TDemoNode.SetActive(const AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
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
    FStyleElements:=aParser.ParseInline;
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
  FCSSClasses.Delimiter:=' ';
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

function TDemoNode.GetCSSID: TCSSString;
begin
  Result:=Name;
end;

class function TDemoNode.CSSTypeName: TCSSString;
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

function TDemoNode.HasCSSClass(const aClassName: TCSSString): boolean;
var
  i: Integer;
begin
  for i:=0 to CSSClasses.Count-1 do
    if aClassName=CSSClasses[i] then
      exit(true);
  Result:=false;
end;

function TDemoNode.CheckCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement
  ): boolean;
begin
  if (AttrID<DemoAttrIDBase) or (AttrID>ord(High(TDemoNodeAttribute))+DemoAttrIDBase) then
    exit(false);
  Result:=Value<>nil;
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

function TDemoNode.GetCSSParent: ICSSNode;
begin
  Result:=Parent;
end;

function TDemoNode.GetCSSIndex: integer;
begin
  if Parent=nil then
    Result:=-1
  else
    Result:=Parent.FNodes.IndexOf(Self);
end;

function TDemoNode.GetCSSNextSibling: ICSSNode;
var
  i: Integer;
begin
  i:=GetCSSIndex;
  if (i<0) or (i+1>=Parent.NodeCount) then
    Result:=nil
  else
    Result:=Parent.Nodes[i+1];
end;

function TDemoNode.GetCSSPreviousSibling: ICSSNode;
var
  i: Integer;
begin
  i:=GetCSSIndex;
  if i<1 then
    Result:=nil
  else
    Result:=Parent.Nodes[i-1];
end;

function TDemoNode.GetCSSChildCount: integer;
begin
  Result:=NodeCount;
end;

function TDemoNode.GetCSSChild(const anIndex: integer): ICSSNode;
begin
  Result:=Nodes[anIndex];
end;

function TDemoNode.GetCSSNextOfType: ICSSNode;
var
  i, Cnt: Integer;
  MyID: TCSSNumericalID;
  aNode: TDemoNode;
begin
  Result:=nil;
  i:=GetCSSIndex;
  if i<0 then exit;
  inc(i);
  MyID:=CSSTypeID;
  Cnt:=Parent.NodeCount;
  while i<Cnt do
  begin
    aNode:=Parent.Nodes[i];
    if aNode.CSSTypeID=MyID then
      exit(aNode);
    inc(i);
  end;
end;

function TDemoNode.GetCSSPreviousOfType: ICSSNode;
var
  i: Integer;
  MyID: TCSSNumericalID;
  aNode: TDemoNode;
begin
  Result:=nil;
  i:=GetCSSIndex;
  if i<0 then exit;
  dec(i);
  MyID:=CSSTypeID;
  while i>=0 do
  begin
    aNode:=Parent.Nodes[i];
    if aNode.CSSTypeID=MyID then
      exit(aNode);
    dec(i);
  end;
end;

function TDemoNode.GetCSSAttributeClass: TCSSString;
begin
  FCSSClasses.Delimiter:=' ';
  Result:=FCSSClasses.DelimitedText;
end;

function TDemoNode.HasCSSAttribute(const AttrID: TCSSNumericalID): boolean;
begin
  Result:=(AttrID>=DemoAttrIDBase) and (AttrID<=DemoAttrIDBase+ord(High(TDemoNodeAttribute)));
end;

function TDemoNode.GetCSSAttribute(const AttrID: TCSSNumericalID): TCSSString;
var
  Attr: TDemoNodeAttribute;
begin
  if (AttrID<DemoAttrIDBase) or (AttrID>DemoAttrIDBase+ord(High(TDemoNodeAttribute))) then
    exit('');
  Attr:=TDemoNodeAttribute(AttrID-DemoAttrIDBase);
  Result:=Attribute[Attr];
end;

function TDemoNode.HasCSSPseudoClass(const AttrID: TCSSNumericalID): boolean;
begin
  if (AttrID>=DemoPseudoClassIDBase) and (AttrID<=DemoPseudoClassIDBase+ord(High(TDemoPseudoClass))) then
    Result:=HasPseudoClass(TDemoPseudoClass(AttrID-DemoPseudoClassIDBase))
  else
    Result:=false;
end;

function TDemoNode.GetCSSEmpty: boolean;
begin
  Result:=NodeCount=0;
end;

function TDemoNode.GetCSSDepth: integer;
var
  Node: TDemoNode;
begin
  Result:=0;
  Node:=Parent;
  while Node<>nil do
  begin
    inc(Result);
    Node:=Node.Parent;
  end;
end;

function TDemoNode.HasPseudoClass(PseudoClass: TDemoPseudoClass): boolean;
begin
  case PseudoClass of
    pcActive: Result:=Active;
    pcHover: Result:=Hover;
  end;
end;

function TDemoNode.GetCSSTypeName: TCSSString;
begin
  Result:=CSSTypeName;
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

