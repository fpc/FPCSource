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
{$IF FPC_FULLVERSION>30300}
{$WARN 6060 off : Case statement does not handle all possible cases}
{$ENDIF}
interface

uses
  Classes, SysUtils, Contnrs, fpcunit, testregistry, fpCSSTree,
  fpCSSResParser, fpCSSResolver;

type
  TDemoNodeAttribute = (
    naNone,
    naLeft,
    naTop,
    naWidth,
    naHeight,
    naBorderWidth,
    naBorderColor,
    naBorder, // shorthand after longhands
    naDisplay,
    naColor,
    naBackground,
    naDirection
    );
  TDemoNodeAttributes = set of TDemoNodeAttribute;

const
  DemoAttributeNames: array[TDemoNodeAttribute] of TCSSString = (
    // case sensitive!
    '?',
    'left',
    'top',
    'width',
    'height',
    'border-width',
    'border-color',
    'border',
    'display',
    'color',
    'background',
    'direction'
    );
  DemoAttributesInherited = [naBackground,naColor,naBorderColor];
  DemoAttributesNotAll = [naDirection];
  DemoAttributeInitialValues: array[TDemoNodeAttribute] of TCSSString = (
    '?',
    'auto', // left
    'auto', // top
    'auto', // width
    'auto', // height
    '0px', // border-width
    'none', // border-color
    'none', // border
    'inline', // display
    'none', // color
    'none', // background
    'auto' // direction
    );

type
  TDemoPseudoClass = (
    pcActive,
    pcHover
    );
  TDemoPseudoClasses = set of TDemoPseudoClass;

const
  DemoPseudoClassNames: array[TDemoPseudoClass] of TCSSString = (
    // case sensitive!
    'active',
    'hover'
    );

type
  TDemoElementType = (
    detNode,
    detDiv,
    detSpan,
    detButton
    );
const
  DemoElementTypeNames: array[TDemoElementType] of TCSSString = (
    // case sensitive!
    'node',
    'div',
    'span',
    'button'
    );

type
  TDemoNode = class;

  { TDemoCSSAttributeDesc }

  TDemoCSSAttributeDesc = class(TCSSAttributeDesc)
  public
    type
      TComputeEvent = procedure(Resolver: TCSSResolver; Node: TDemoNode; Value: TCSSAttributeValue) of object;
  public
    DemoID: TDemoNodeAttribute;
    OnCompute: TComputeEvent;
  end;

  { TDemoCSSPseudoClassDesc }

  TDemoCSSPseudoClassDesc = class(TCSSPseudoClassDesc)
  public
    DemoID: TDemoPseudoClass;
  end;

  { TDemoCSSTypeDesc }

  TDemoCSSTypeDesc = class(TCSSTypeDesc)
  public
    DemoID: TDemoElementType;
  end;

  { TDemoCSSRegistry }

  TDemoCSSRegistry = class(TCSSRegistry)
  private
    // check attribute declarations for validity
    function OnCheck_BorderColor(Resolver: TCSSBaseResolver): boolean;
    function OnCheck_BorderWidth(Resolver: TCSSBaseResolver): boolean;
    function OnCheck_Border(Resolver: TCSSBaseResolver): boolean;
    function OnCheck_Direction(Resolver: TCSSBaseResolver): boolean;
    function OnCheck_Display(Resolver: TCSSBaseResolver): boolean;
    function OnCheck_LeftTop(Resolver: TCSSBaseResolver): boolean;
    function OnCheck_WidthHeight(Resolver: TCSSBaseResolver): boolean;
    // clean up and normalize attribute values
    procedure OnCompute_Direction(Resolver: TCSSResolver; Node: TDemoNode;
      Value: TCSSAttributeValue);
    procedure OnCompute_LeftTop(Resolver: TCSSResolver; Node: TDemoNode;
      Value: TCSSAttributeValue);
    procedure OnCompute_WidthHeight(Resolver: TCSSResolver; Node: TDemoNode;
      Value: TCSSAttributeValue);
    // split shorthands into longhands
    procedure OnSplit_Border(Resolver: TCSSBaseResolver;
      var AttrIDs: TCSSNumericalIDArray; var Values: TCSSStringArray);
  public
    DemoAttrIDBase: TCSSNumericalID;
    DemoPseudoClassIDBase: TCSSNumericalID;
    DemoElementTypeIDBase: TCSSNumericalID;

    DemoAttrs: array[TDemoNodeAttribute] of TDemoCSSAttributeDesc;
    DemoPseudoClasses: array[TDemoPseudoClass] of TDemoCSSPseudoClassDesc;
    DemoTypes: array[TDemoElementType] of TDemoCSSTypeDesc;

    // keywords
    kwRed,
    kwGreen,
    kwBlue,
    kwWhite,
    kwBlack,
    kwNone,
    kwBlock,
    kwInline_Block,
    kwLTR,
    kwRTL: TCSSNumericalID;

    // check parameters
    Chk_BorderWidth: TCSSCheckAttrParams_Dimension;
    Chk_DirectionAllowedKeywordIDs: TCSSNumericalIDArray;
    Chk_DisplayAllowedKeywordIDs: TCSSNumericalIDArray;
    Chk_LeftTop: TCSSCheckAttrParams_Dimension;
    Chk_WidthHeight: TCSSCheckAttrParams_Dimension;

    constructor Create;
    function AddDemoAttr(Attr: TDemoNodeAttribute): TDemoCSSAttributeDesc;
    function AddDemoPseudoClass(PC: TDemoPseudoClass): TDemoCSSPseudoClassDesc;
    function AddDemoType(aType: TDemoElementType): TDemoCSSTypeDesc;
  end;

  { TDemoNode }

  TDemoNode = class(TComponent,ICSSNode)
  private
    class var CSSRegistry: TDemoCSSRegistry;
    class var FDemoNodeTypeID: TCSSNumericalID;
  private
    FNodes: TFPObjectList; // list of TDemoNode
    FCSSClasses: TStrings;
    FParent: TDemoNode;
    FPseudoClasses: array [TDemoPseudoClass] of boolean;
    FInlineStyleElements: TCSSRuleElement;
    FInlineStyle: TCSSString;
    function GetAttribute(DemoAttr: TDemoNodeAttribute): TCSSString;
    function GetNodeCount: integer;
    function GetNodes(Index: integer): TDemoNode;
    function GetPseudoClasses(PseudoClass: TDemoPseudoClass): boolean;
    procedure SetParent(const AValue: TDemoNode);
    procedure SetInlineStyleElements(const AValue: TCSSRuleElement);
    procedure SetInlineStyle(const AValue: TCSSString);
    procedure SetPseudoClasses(PseudoClass: TDemoPseudoClass; const AValue: boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    // computed by resolver:
    Rules: TCSSSharedRuleList; // owned by resolver
    Values: TCSSAttributeValues;
    // explicit attributes: can be queried by CSS, e.g. div[foo=3px]
    ExplicitAttributes: array[TDemoNodeAttribute] of TCSSString;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure ApplyCSS(Resolver: TCSSResolver); virtual;
    class function CSSTypeName: TCSSString; virtual;
    class function GetClassCSSTypeID: TCSSNumericalID; virtual;
    class procedure SetClassCSSTypeID(aID: TCSSNumericalID); virtual;
    class function GetCSSTypeStyle: TCSSString; virtual;

    // ICSSNode interface:
    function GetCSSID: TCSSString; virtual;
    function GetCSSTypeName: TCSSString;
    function GetCSSTypeID: TCSSNumericalID;
    function GetCSSPseudoElementName: TCSSString; virtual;
    function GetCSSPseudoElementID: TCSSNumericalID; virtual;
    function GetCSSParent: ICSSNode; virtual;
    function GetCSSDepth: integer; virtual;
    function GetCSSIndex: integer; virtual;
    function GetCSSNextSibling: ICSSNode; virtual;
    function GetCSSPreviousSibling: ICSSNode; virtual;
    function GetCSSNextOfType: ICSSNode; virtual;
    function GetCSSPreviousOfType: ICSSNode; virtual;
    function GetCSSEmpty: boolean; virtual;
    function GetCSSChildCount: integer; virtual;
    function GetCSSChild(const anIndex: integer): ICSSNode; virtual;
    function HasCSSClass(const aClassName: TCSSString): boolean; virtual;
    function GetCSSAttributeClass: TCSSString; virtual;
    function GetCSSCustomAttribute(const AttrID: TCSSNumericalID): TCSSString; virtual;
    function HasCSSExplicitAttribute(const AttrID: TCSSNumericalID): boolean; virtual;
    function GetCSSExplicitAttribute(const AttrID: TCSSNumericalID): TCSSString; virtual;
    function HasCSSPseudoClass(const {%H-}AttrID: TCSSNumericalID): boolean; virtual;

    property Parent: TDemoNode read FParent write SetParent;
    property NodeCount: integer read GetNodeCount;
    property Nodes[Index: integer]: TDemoNode read GetNodes; default;
    property CSSClasses: TStrings read FCSSClasses;
    property InlineStyleElement: TCSSRuleElement read FInlineStyleElements write SetInlineStyleElements;
    property InlineStyle: TCSSString read FInlineStyle write SetInlineStyle;
    // CSS attributes
    property Left: TCSSString index naLeft read GetAttribute;
    property Top: TCSSString index naTop read GetAttribute;
    property Width: TCSSString index naWidth read GetAttribute;
    property Height: TCSSString index naHeight read GetAttribute;
    property Border: TCSSString index naBorder read GetAttribute;
    property BorderWidth: TCSSString index naBorderWidth read GetAttribute;
    property BorderColor: TCSSString index naBorderColor read GetAttribute;
    property Display: TCSSString index naDisplay read GetAttribute;
    property Color: TCSSString index naColor read GetAttribute;
    property Background: TCSSString index naBackground read GetAttribute;
    property Direction: TCSSString index naDirection read GetAttribute;
    property Attribute[Attr: TDemoNodeAttribute]: TCSSString read GetAttribute;
    // CSS pseudo classes
    property Active: boolean index pcActive read GetPseudoClasses write SetPseudoClasses;
    property Hover: boolean index pcHover read GetPseudoClasses write SetPseudoClasses;
    property HasPseudoClass[PseudoClass: TDemoPseudoClass]: boolean read GetPseudoClasses write SetPseudoClasses;
  end;
  TDemoNodeClass = class of TDemoNode;

  { TDemoPseudoElement }

  TDemoPseudoElement = class(TDemoNode)
  public
    constructor Create(AOwner: TComponent); override;
    function GetCSSTypeName: TCSSString;
    function GetCSSTypeID: TCSSNumericalID;
    function GetCSSParent: ICSSNode; override;
    function GetCSSIndex: integer; override;
    function GetCSSNextSibling: ICSSNode; override;
    function GetCSSPreviousSibling: ICSSNode; override;
    function GetCSSNextOfType: ICSSNode; override;
    function GetCSSPreviousOfType: ICSSNode; override;
    function GetCSSEmpty: boolean; override;
    function GetCSSChildCount: integer; override;
    function GetCSSChild(const anIndex: integer): ICSSNode; override;
    function HasCSSClass(const aClassName: TCSSString): boolean; override;
    function GetCSSAttributeClass: TCSSString; override;
  end;

  { TDemoFirstLine }

  TDemoFirstLine = class(TDemoPseudoElement)
  public
    class var DemoFirstLineID: TCSSNumericalID;
    function GetCSSPseudoElementName: TCSSString; override;
    function GetCSSPseudoElementID: TCSSNumericalID; override;
  end;

  { TDemoDiv }

  TDemoDiv = class(TDemoNode)
  private
    class var FDemoDivTypeID: TCSSNumericalID;
  public
    class function CSSTypeName: TCSSString; override;
    class function GetClassCSSTypeID: TCSSNumericalID; override;
    class procedure SetClassCSSTypeID(aID: TCSSNumericalID); override;
    class function GetCSSTypeStyle: TCSSString; override;
  end;

  { TDemoSpan }

  TDemoSpan = class(TDemoNode)
  private
    class var FDemoSpanTypeID: TCSSNumericalID;
  public
    class function CSSTypeName: TCSSString; override;
    class function GetClassCSSTypeID: TCSSNumericalID; override;
    class procedure SetClassCSSTypeID(aID: TCSSNumericalID); override;
    class function GetCSSTypeStyle: TCSSString; override;
  end;

  { TDemoButton }

  TDemoButton = class(TDemoNode)
  private
    FCaption: TCSSString;
    class var FDemoButtonTypeID: TCSSNumericalID;
    procedure SetCaption(const AValue: TCSSString);
  public
    ExplicitCaption: TCSSString;
    class var CSSCaptionID: TCSSNumericalID;
    class function CSSTypeName: TCSSString; override;
    class function GetClassCSSTypeID: TCSSNumericalID; override;
    class procedure SetClassCSSTypeID(aID: TCSSNumericalID); override;
    class function GetCSSTypeStyle: TCSSString; override;
    function HasCSSExplicitAttribute(const AttrID: TCSSNumericalID): boolean; override;
    function GetCSSExplicitAttribute(const AttrID: TCSSNumericalID): TCSSString; override;
    property Caption: TCSSString read FCaption write SetCaption;
  end;

  { TDemoDocument }

  TDemoDocument = class(TComponent)
  private
    FCSSResolver: TCSSResolver;
    FStyle: TCSSString;
    procedure OnResolverLog(Sender: TObject; Entry: TCSSResolverLogEntry);
  protected
    procedure ApplyTypeStyles; virtual;
    procedure SetStyle(const AValue: TCSSString); virtual;
  public
    Root: TDemoNode;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyStyle; virtual;

    property Style: TCSSString read FStyle write SetStyle;

    property CSSResolver: TCSSResolver read FCSSResolver;
  end;

  { TCustomTestNewCSSResolver }

  TCustomTestNewCSSResolver = class(TTestCase)
  private
    FDoc: TDemoDocument;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ApplyStyle; virtual;
    procedure CheckWarnings; virtual;
  public
    property Doc: TDemoDocument read FDoc;
  end;

  { TTestNewCSSResolver }

  TTestNewCSSResolver = class(TCustomTestNewCSSResolver)
  published
    // invalid attributes while parsing stylesheet
    procedure Test_ParseAttr_Keyword;
    procedure Test_ParseAttr_Keyword_SkipInvalid;
    procedure Test_ParseAttr_Float;
    procedure Test_ParseAttr_Float_SkipInvalid; // todo

    procedure Test_Selector_Universal;
    procedure Test_Selector_Type;
    procedure Test_Selector_Type_Spaces;
    procedure Test_Selector_Id;
    procedure Test_Selector_Class;
    procedure Test_Selector_ClassClass; // AND combinator
    procedure Test_Selector_ClassSpaceClass; // Descendant combinator
    procedure Test_Selector_TypeCommaType; // OR combinator
    procedure Test_Selector_ClassGTClass; // child combinator
    procedure Test_Selector_TypePlusType; // adjacent sibling combinator
    procedure Test_Selector_TypeTildeType; // general sibling combinator

    // explicit attributes, e.g. set by HTML
    procedure Test_Selector_HasAttribute;
    procedure Test_Selector_AttributeEquals;
    procedure Test_Selector_AttributeEqualsI;
    procedure Test_Selector_AttributeBeginsWith;
    procedure Test_Selector_AttributeEndsWith;
    procedure Test_Selector_AttributeBeginsWithHyphen;
    procedure Test_Selector_AttributeContainsWord;
    procedure Test_Selector_AttributeContainsSubstring;

    // pseudo classes and functions
    // test unknown pseudo class
    // test unknown pseudo function
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
    // ToDo: procedure Test_Selector_Is_Descendant;  :is(div button, .hawk .eagle)
    procedure Test_Selector_Where;
    // ToDo: div:has(>img)
    // ToDo: div:has(+img)

    // custom pseudo classes and functions
    procedure Test_Selector_Hover;

    // inline style
    procedure Test_InlineStyle;
    procedure Test_InlineStyle_DisplayNone;

    // Specificity
    procedure Test_Specificity_Id_Class;
    procedure Test_Specificity_Important;
    procedure Test_Specificity_Shorthand_OneRule;
    procedure Test_Specificity_Shorthand_ClassClass;
    procedure Test_Specificity_Longhand_All_Longhand;
    procedure Test_Specificity_Shorthand_All_Shorthand;

    // origin
    procedure Test_Origin_Id_Class;

    // var()
    procedure Test_Var_NoDefault;
    procedure Test_Var_Inline_NoDefault;
    procedure Test_Var_Defaults;

    // skipping for forward compatibility
    // ToDo: invalid token in selector makes selector invalid
    // ToDo: invalid domain in attribute value is skipped
    // ToDo: invalid keyword in attribute value is skipped
    // ToDo: invalid keyword in attribute value is skipped
    // test skip invalid value  color: 3 red;
    // test skip invalid attribute  color: 3;

    // pseudo elements (works like child combinator)
    procedure Test_PseudoElement;
    procedure Test_PseudoElement_Unary;
    procedure Test_PseudoElement_PostfixSelectNothing;
  end;

function LinesToStr(const Args: array of const): TCSSString;

implementation

function LinesToStr(const Args: array of const): TCSSString;
var
  s: TCSSString;
  i: Integer;
begin
  s:='';
  for i:=Low(Args) to High(Args) do
  begin
    case Args[i].VType of
      vtChar:         s += Args[i].VChar+LineEnding;
      vtString:       s += Args[i].VString^+LineEnding;
      vtPChar:        s += Args[i].VPChar+LineEnding;
      vtWideChar:     s += TCSSString(Args[i].VWideChar)+LineEnding;
      vtPWideChar:    s += TCSSString(Args[i].VPWideChar)+LineEnding;
      vtAnsiString:   s += AnsiString(Args[i].VAnsiString)+LineEnding; // FPC uses encoding CP_UTF8 for TVarRec.VAnsiString
      vtWidestring:   s += TCSSString(WideString(Args[i].VWideString))+LineEnding;
      vtUnicodeString:s += TCSSString(UnicodeString(Args[i].VUnicodeString))+LineEnding;
    end;
  end;
  Result:=s;
end;

{ TDemoDiv }

class function TDemoDiv.CSSTypeName: TCSSString;
begin
  Result:=DemoElementTypeNames[detDiv];
end;

class function TDemoDiv.GetClassCSSTypeID: TCSSNumericalID;
begin
  Result:=FDemoDivTypeID;
end;

class procedure TDemoDiv.SetClassCSSTypeID(aID: TCSSNumericalID);
begin
  FDemoDivTypeID:=aID;
end;

class function TDemoDiv.GetCSSTypeStyle: TCSSString;
begin
  Result:='div{ display: block }';
end;

{ TDemoSpan }

class function TDemoSpan.CSSTypeName: TCSSString;
begin
  Result:=DemoElementTypeNames[detSpan];
end;

class function TDemoSpan.GetClassCSSTypeID: TCSSNumericalID;
begin
  Result:=FDemoSpanTypeID;
end;

class procedure TDemoSpan.SetClassCSSTypeID(aID: TCSSNumericalID);
begin
  FDemoSpanTypeID:=aID;
end;

class function TDemoSpan.GetCSSTypeStyle: TCSSString;
begin
  Result:='span{display: inline-block }';
end;

{ TDemoButton }

procedure TDemoButton.SetCaption(const AValue: TCSSString);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

class function TDemoButton.CSSTypeName: TCSSString;
begin
  Result:=DemoElementTypeNames[detButton];
end;

class function TDemoButton.GetClassCSSTypeID: TCSSNumericalID;
begin
  Result:=FDemoButtonTypeID;
end;

class procedure TDemoButton.SetClassCSSTypeID(aID: TCSSNumericalID);
begin
  FDemoButtonTypeID:=aID;
end;

class function TDemoButton.GetCSSTypeStyle: TCSSString;
begin
  Result:='button{display: inline-block }';
end;

function TDemoButton.HasCSSExplicitAttribute(const AttrID: TCSSNumericalID
  ): boolean;
begin
  //writeln('TDemoButton.HasCSSExplicitAttribute ',AttrID,' CSSCaptionID=',CSSCaptionID);
  if AttrID=CSSCaptionID then
    Result:=ExplicitCaption<>''
  else
    Result:=inherited HasCSSExplicitAttribute(AttrID);
end;

function TDemoButton.GetCSSExplicitAttribute(const AttrID: TCSSNumericalID
  ): TCSSString;
begin
  if AttrID=CSSCaptionID then
    Result:=ExplicitCaption
  else
    Result:=inherited GetCSSExplicitAttribute(AttrID);
end;

{ TDemoDocument }

procedure TDemoDocument.SetStyle(const AValue: TCSSString);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
end;

constructor TDemoDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // create the css resolver
  FCSSResolver:=TCSSResolver.Create(nil);
  FCSSResolver.CSSRegistry:=TDemoNode.CSSRegistry;
  FCSSResolver.OnLog:=@OnResolverLog;
end;

destructor TDemoDocument.Destroy;
begin
  FreeAndNil(Root);
  FreeAndNil(FCSSResolver);
  inherited Destroy;
end;

procedure TDemoDocument.ApplyStyle;

  procedure Traverse(Node: TDemoNode);
  var
    i: Integer;
  begin
    Node.ApplyCSS(CSSResolver);
    for i:=0 to Node.NodeCount-1 do
      Traverse(Node[i]);
  end;

begin
  ApplyTypeStyles;

  CSSResolver.AddStyleSheet(cssoAuthor,'test.css',Style);
  CSSResolver.Init;
  Traverse(Root);
end;

procedure TDemoDocument.OnResolverLog(Sender: TObject; Entry: TCSSResolverLogEntry);
begin
  if Sender=nil then ;
  if Entry=nil then ;
end;

procedure TDemoDocument.ApplyTypeStyles;
var
  FoundStyles: array of TDemoNodeClass;

  procedure AddTypeStyle(NodeClass: TDemoNodeClass);
  var
    i: Integer;
    Src, ParentSrc: TCSSString;
    ParentNodeClass: TDemoNodeClass;
  begin
    for i:=0 to length(FoundStyles)-1 do
      if FoundStyles[i]=NodeClass then exit;
    Insert(NodeClass,FoundStyles,length(FoundStyles));

    Src:=NodeClass.GetCSSTypeStyle;
    //writeln('AddTypeStyle ',NodeClass.ClassName,' Src="',Src,'"');
    if Src='' then exit;
    if NodeClass.ClassType<>TDemoNode then
    begin
      ParentNodeClass:=TDemoNodeClass(NodeClass.ClassParent);
      AddTypeStyle(ParentNodeClass);
      ParentSrc:=ParentNodeClass.GetCSSTypeStyle;
      if Src=ParentSrc then exit;
    end;
    //writeln('AddTypeStyle ',NodeClass.ClassName,' [',Src,']');
    FCSSResolver.AddStyleSheet(cssoUserAgent,NodeClass.ClassName,Src);
  end;

  procedure CollectTypeStyles(Node: TDemoNode);
  var
    NodeClass: TDemoNodeClass;
    i: Integer;
  begin
    NodeClass:=TDemoNodeClass(Node.ClassType);
    AddTypeStyle(NodeClass);
    for i:=0 to Node.NodeCount-1 do
      CollectTypeStyles(Node[i]);
  end;

begin
  FoundStyles:=[];
  CollectTypeStyles(Root);
end;

{ TDemoCSSRegistry }

function TDemoCSSRegistry.OnCheck_Border(Resolver: TCSSBaseResolver): boolean;
var
  HasWidth, HasColor: Boolean;
begin
  HasWidth:=false;
  HasColor:=false;
  repeat
    case Resolver.CurComp.Kind of
    rvkFloat:
      if not HasWidth then
        HasWidth:=Resolver.CurComp.FloatUnit in ([cuNONE,cuPERCENT]+cuAllLengths);
    rvkKeyword:
      if not HasColor then
        HasColor:=(Resolver.CurComp.KeywordID>=kwFirstColor) and (Resolver.CurComp.KeywordID<=kwLastColor);
    end;
  until not Resolver.ReadNext;
  Result:=HasWidth or HasColor;
end;

function TDemoCSSRegistry.OnCheck_BorderColor(Resolver: TCSSBaseResolver): boolean;
begin
  Result:=Resolver.CheckAttribute_Color([]);
end;

function TDemoCSSRegistry.OnCheck_BorderWidth(Resolver: TCSSBaseResolver): boolean;
begin
  Result:=Resolver.CheckAttribute_Dimension(Chk_BorderWidth);
end;

procedure TDemoCSSRegistry.OnSplit_Border(Resolver: TCSSBaseResolver;
  var AttrIDs: TCSSNumericalIDArray; var Values: TCSSStringArray);
var
  aWidth, aColor: TCSSString;
begin
  aWidth:='';
  aColor:='';
  repeat
    case Resolver.CurComp.Kind of
    rvkFloat:
      if aWidth='' then begin
        if Resolver.CurComp.FloatUnit in ([cuNONE,cuPERCENT]+cuAllLengths) then
          aWidth:=Resolver.CurComp.FloatAsString;
      end;
    rvkKeyword:
      if aColor='' then
      begin
        if (Resolver.CurComp.KeywordID>=kwFirstColor) and (Resolver.CurComp.KeywordID<=kwLastColor) then
          aColor:=Keywords[Resolver.CurComp.KeywordID];
      end;
    end;
  until not Resolver.ReadNext;
  SetLength(AttrIDs,2);
  SetLength(Values,2);
  AttrIDs[0]:=DemoAttrs[naBorderWidth].Index;
  Values[0]:=aWidth;
  AttrIDs[1]:=DemoAttrs[naBorderColor].Index;
  Values[1]:=aColor;
end;

function TDemoCSSRegistry.OnCheck_Direction(Resolver: TCSSBaseResolver): boolean;
begin
  Result:=Resolver.CheckAttribute_Keyword(Chk_DirectionAllowedKeywordIDs);
end;

function TDemoCSSRegistry.OnCheck_Display(Resolver: TCSSBaseResolver): boolean;
begin
  Result:=Resolver.CheckAttribute_Keyword(Chk_DisplayAllowedKeywordIDs);
end;

function TDemoCSSRegistry.OnCheck_LeftTop(Resolver: TCSSBaseResolver): boolean;
begin
  Result:=Resolver.CheckAttribute_Dimension(Chk_LeftTop);
end;

function TDemoCSSRegistry.OnCheck_WidthHeight(Resolver: TCSSBaseResolver): boolean;
begin
  Result:=Resolver.CheckAttribute_Dimension(Chk_WidthHeight);
end;

procedure TDemoCSSRegistry.OnCompute_Direction(Resolver: TCSSResolver;
  Node: TDemoNode; Value: TCSSAttributeValue);
var
  Invalid: boolean;
begin
  if Resolver.ReadAttribute_Keyword(Invalid,Chk_DirectionAllowedKeywordIDs) then
  begin
    Value.Value:=Keywords[Resolver.CurComp.KeywordID];
    Value.State:=cavsComputed;
  end
  else begin
    Value.Value:='invalid';
    Value.State:=cavsInvalid;
  end;
  if Node=nil then ;
end;

procedure TDemoCSSRegistry.OnCompute_LeftTop(Resolver: TCSSResolver;
  Node: TDemoNode; Value: TCSSAttributeValue);
var
  Invalid: boolean;
begin
  if Resolver.ReadAttribute_Dimension(Invalid,Chk_LeftTop) then
  begin
    case Resolver.CurComp.Kind of
    rvkFloat:
      Value.Value:=Resolver.CurComp.FloatAsString;
    rvkKeyword:
      Value.Value:=Keywords[Resolver.CurComp.KeywordID];
    end;
    Value.State:=cavsComputed;
  end
  else begin
    Value.Value:='invalid';
    Value.State:=cavsInvalid;
  end;
  if Node=nil then ;
end;

procedure TDemoCSSRegistry.OnCompute_WidthHeight(Resolver: TCSSResolver;
  Node: TDemoNode; Value: TCSSAttributeValue);
var
  Invalid: boolean;
begin
  if Resolver.ReadAttribute_Dimension(Invalid,Chk_WidthHeight) then
  begin
    Value.Value:=Resolver.CurComp.FloatAsString;
    Value.State:=cavsComputed;
  end
  else begin
    Value.Value:='invalid';
    Value.State:=cavsInvalid;
  end;
  if Node=nil then ;
end;

constructor TDemoCSSRegistry.Create;

  procedure SetDemoElementTypeID(aClass: TDemoNodeClass);
  var
    Desc: TCSSTypeDesc;
  begin
    Desc:=FindType(aClass.CSSTypeName);
    if Desc=nil then
      raise Exception.Create('20240625190912');
    aClass.SetClassCSSTypeID(Desc.Index);
  end;

  procedure SetCompProps(ShorthandID: TDemoNodeAttribute; Longhands: array of TDemoNodeAttribute);
  var
    i: Integer;
  begin
    SetLength(DemoAttrs[ShorthandID].CompProps,length(Longhands));
    for i:=0 to length(Longhands)-1 do
      DemoAttrs[ShorthandID].CompProps[i]:=DemoAttrs[Longhands[i]];
  end;

var
  Attr: TDemoNodeAttribute;
  PseudoClass: TDemoPseudoClass;
  aType: TDemoElementType;
begin
  inherited Create;
  Init;

  // register demo attributes
  for Attr in TDemoNodeAttribute do
    AddDemoAttr(Attr);
  DemoAttrIDBase:=DemoAttrs[low(TDemoNodeAttribute)].Index;
  if FindAttribute(DemoAttributeNames[naBackground]).Index<>DemoAttrIDBase+ord(naBackground) then
    raise Exception.Create('20240617200337');

  // register demo pseudo classes
  for PseudoClass in TDemoPseudoClass do
    AddDemoPseudoClass(PseudoClass);
  DemoPseudoClassIDBase:=DemoPseudoClasses[low(TDemoPseudoClass)].Index;
  if FindPseudoClass(DemoPseudoClassNames[pcHover]).Index<>DemoPseudoClassIDBase+ord(pcHover) then
    raise Exception.Create('20231008232201');

  // register demo pseudo elements
  TDemoFirstLine.DemoFirstLineID:=AddPseudoElement('first-line').Index;
  AddPseudoElement('selection');

  // register demo element types
  for aType in TDemoElementType do
    AddDemoType(aType);
  DemoElementTypeIDBase:=DemoTypes[low(TDemoElementType)].Index;
  if FindType(DemoElementTypeNames[detButton]).Index<>DemoElementTypeIDBase+ord(detButton) then
    raise Exception.Create('20240625181725');
  SetDemoElementTypeID(TDemoNode);
  SetDemoElementTypeID(TDemoDiv);
  SetDemoElementTypeID(TDemoSpan);
  SetDemoElementTypeID(TDemoButton);

  kwRed:=AddKeyword('red');
  kwFirstColor:=kwRed;
  kwGreen:=AddKeyword('green');
  kwBlue:=AddKeyword('blue');
  kwWhite:=AddKeyword('white');
  kwBlack:=AddKeyword('black');
  kwLastColor:=kwBlack;

  kwNone:=CSSKeywordNone;
  kwBlock:=AddKeyword('block');
  kwInline_Block:=AddKeyword('inline-block');

  kwLTR:=AddKeyword('ltr');
  kwRTL:=AddKeyword('rtl');

  // check attribute values - - - - - - - - - - - - - - - - - - - - - - - -

  // border-color
  DemoAttrs[naBorderColor].OnCheck:=@OnCheck_BorderColor;

  // border-width
  DemoAttrs[naBorderWidth].OnCheck:=@OnCheck_BorderWidth;
  Chk_BorderWidth.AllowedUnits:=[cuNONE,cuPERCENT]+cuAllLengths;
  Chk_BorderWidth.AllowFrac:=true;

  // border shorthand
  SetCompProps(naBorder,[naBorderColor,naBorderWidth]);
  DemoAttrs[naBorder].OnCheck:=@OnCheck_Border;
  DemoAttrs[naBorder].OnSplitShorthand:=@OnSplit_Border;

  // direction
  DemoAttrs[naDirection].OnCheck:=@OnCheck_Direction;
  Chk_DirectionAllowedKeywordIDs:=[kwLTR,kwRTL];
  DemoAttrs[naDirection].OnCompute:=@OnCompute_Direction;

  // display
  DemoAttrs[naDisplay].OnCheck:=@OnCheck_Display;
  Chk_DisplayAllowedKeywordIDs:=[kwNone,kwBlock,kwInline_Block];

  // left, top
  DemoAttrs[naLeft].OnCheck:=@OnCheck_LeftTop;
  DemoAttrs[naLeft].OnCompute:=@OnCompute_LeftTop;
  DemoAttrs[naTop].OnCheck:=@OnCheck_LeftTop;
  DemoAttrs[naTop].OnCompute:=@OnCompute_LeftTop;
  Chk_LeftTop.AllowedUnits:=[cuNONE,cuPERCENT]+cuAllLengths;
  Chk_LeftTop.AllowNegative:=true;
  Chk_LeftTop.AllowFrac:=true;

  // width, height
  DemoAttrs[naWidth].OnCheck:=@OnCheck_WidthHeight;
  DemoAttrs[naWidth].OnCompute:=@OnCompute_WidthHeight;
  DemoAttrs[naHeight].OnCheck:=@OnCheck_WidthHeight;
  DemoAttrs[naHeight].OnCompute:=@OnCompute_WidthHeight;
  Chk_WidthHeight.AllowedUnits:=[cuNONE,cuPERCENT]+cuAllLengths;
  Chk_WidthHeight.AllowFrac:=true;
end;

function TDemoCSSRegistry.AddDemoAttr(Attr: TDemoNodeAttribute
  ): TDemoCSSAttributeDesc;
begin
  Result:=TDemoCSSAttributeDesc(AddAttribute(DemoAttributeNames[Attr],
     DemoAttributeInitialValues[Attr],
     Attr in DemoAttributesInherited,
     not (Attr in DemoAttributesNotAll),
     TDemoCSSAttributeDesc));
  Result.DemoID:=Attr;
  DemoAttrs[Attr]:=Result;
end;

function TDemoCSSRegistry.AddDemoPseudoClass(PC: TDemoPseudoClass
  ): TDemoCSSPseudoClassDesc;
begin
  Result:=TDemoCSSPseudoClassDesc(AddPseudoClass(DemoPseudoClassNames[PC],
     TDemoCSSPseudoClassDesc));
  Result.DemoID:=PC;
  DemoPseudoClasses[PC]:=Result;
end;

function TDemoCSSRegistry.AddDemoType(aType: TDemoElementType
  ): TDemoCSSTypeDesc;
begin
  Result:=TDemoCSSTypeDesc(AddType(DemoElementTypeNames[aType],
     TDemoCSSTypeDesc));
  Result.DemoID:=aType;
  DemoTypes[aType]:=Result;
end;

{ TDemoNode }

function TDemoNode.GetAttribute(DemoAttr: TDemoNodeAttribute): TCSSString;
var
  AttrDesc: TDemoCSSAttributeDesc;
  i: Integer;
begin
  AttrDesc:=CSSRegistry.DemoAttrs[DemoAttr];
  i:=Values.IndexOf(AttrDesc.Index);
  if i>=0 then
    Result:=Values.Values[i].Value
  else
    Result:='';
end;

function TDemoNode.GetNodeCount: integer;
begin
  Result:=FNodes.Count;
end;

function TDemoNode.GetNodes(Index: integer): TDemoNode;
begin
  Result:=TDemoNode(FNodes[Index]);
end;

function TDemoNode.GetPseudoClasses(PseudoClass: TDemoPseudoClass): boolean;
begin
  Result:=FPseudoClasses[PseudoClass];
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

procedure TDemoNode.SetInlineStyleElements(const AValue: TCSSRuleElement);
begin
  if FInlineStyleElements=AValue then Exit;
  FreeAndNil(FInlineStyleElements);
  FInlineStyleElements:=AValue;
end;

procedure TDemoNode.SetInlineStyle(const AValue: TCSSString);
begin
  if FInlineStyle=AValue then Exit;
  FInlineStyle:=AValue;
  FreeAndNil(FInlineStyleElements);
end;

procedure TDemoNode.SetPseudoClasses(PseudoClass: TDemoPseudoClass;
  const AValue: boolean);
begin
  FPseudoClasses[PseudoClass]:=AValue;
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
begin
  inherited Create(AOwner);
  FNodes:=TFPObjectList.Create(false);
  FCSSClasses:=TStringList.Create;
  FCSSClasses.Delimiter:=' ';
end;

destructor TDemoNode.Destroy;
begin
  Clear;
  FreeAndNil(FNodes);
  FreeAndNil(FCSSClasses);
  FParent:=nil;
  inherited Destroy;
end;

procedure TDemoNode.Clear;
var
  i: Integer;
begin
  Rules:=nil;
  FreeAndNil(Values);

  FCSSClasses.Clear;
  FreeAndNil(FInlineStyleElements);

  for i:=NodeCount-1 downto 0 do
    Nodes[i].Free;
  if FNodes.Count>0 then
    raise Exception.Create('20240710174459');
end;

procedure TDemoNode.ApplyCSS(Resolver: TCSSResolver);
var
  AttrDesc: TDemoCSSAttributeDesc;
  i: Integer;
  AttrID: TCSSNumericalID;
  CurValue: TCSSAttributeValue;
  Desc: TCSSAttributeDesc;
begin
  if (InlineStyleElement=nil) and (InlineStyle<>'') then
    InlineStyleElement:=Resolver.ParseInlineStyle(InlineStyle) as TCSSRuleElement;

  Resolver.Compute(Self,InlineStyleElement,Rules,Values);

  {$IFDEF VerboseCSSResolver}
  writeln('TDemoNode.ApplyCSS ',Name,' length(Values)=',length(Values.Values),' All="',CSSRegistry.Keywords[Values.AllValue],'"');
  for i:=0 to length(Values.Values)-1 do begin
    AttrID:=Values.Values[i].AttrID;
    writeln('TDemoNode.ApplyCSS ',Name,' resolved ',CSSRegistry.Attributes[AttrID].Name,'/',AttrID,':="',Values.Values[i].Value,'"');
  end;
  {$ENDIF}
  // compute values
  for i:=0 to length(Values.Values)-1 do
  begin
    CurValue:=Values.Values[i];
    case CurValue.State of
      cavsSource, cavsBaseKeywords:
        begin
          AttrID:=CurValue.AttrID;
          Desc:=Resolver.GetAttributeDesc(AttrID);
          if Desc=nil then
            raise Exception.Create('20240823100115 AttrID='+IntToStr(AttrID));
          if Desc is TDemoCSSAttributeDesc then
          begin
            AttrDesc:=TDemoCSSAttributeDesc(Desc);
            if AttrDesc.OnCompute<>nil then
            begin
              Resolver.CurComp.EndP:=PChar(CurValue.Value);
              Resolver.ReadNext;
              AttrDesc.OnCompute(Resolver,Self,CurValue);
              {$IFDEF VerboseCSSResolver}
              writeln('TDemoNode.ApplyCSS ',Name,' computed ',CSSRegistry.Attributes[AttrID].Name,'/',AttrID,':="',CurValue.Value,'"');
              {$ENDIF}
            end else
              CurValue.State:=cavsComputed;
          end;
        end;
      cavsComputed: ;
      cavsInvalid: ;
    end;
  end;
end;

function TDemoNode.GetCSSID: TCSSString;
begin
  Result:=Name;
end;

class function TDemoNode.CSSTypeName: TCSSString;
begin
  Result:=DemoElementTypeNames[detNode];
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
  MyID:=GetClassCSSTypeID;
  Cnt:=Parent.NodeCount;
  while i<Cnt do
  begin
    aNode:=Parent.Nodes[i];
    if aNode.GetClassCSSTypeID=MyID then
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
  MyID:=GetClassCSSTypeID;
  while i>=0 do
  begin
    aNode:=Parent.Nodes[i];
    if aNode.GetClassCSSTypeID=MyID then
      exit(aNode);
    dec(i);
  end;
end;

function TDemoNode.GetCSSAttributeClass: TCSSString;
begin
  FCSSClasses.Delimiter:=' ';
  Result:=FCSSClasses.DelimitedText;
end;

function TDemoNode.GetCSSCustomAttribute(const AttrID: TCSSNumericalID): TCSSString;
var
  i: Integer;
  El: TDemoNode;
begin
  Result:='';
  El:=Self;
  repeat
    if El.Values<>nil then
    begin
      i:=El.Values.IndexOf(AttrID);
      if i>=0 then
      begin
        Result:=El.Values.Values[i].Value;
        if Result<>'' then exit;
      end;
    end;
    El:=El.Parent;
  until El=nil;
end;

function TDemoNode.HasCSSExplicitAttribute(const AttrID: TCSSNumericalID): boolean;
var
  b: TCSSNumericalID;
  Attr: TDemoNodeAttribute;
begin
  b:=CSSRegistry.DemoAttrIDBase;
  if (AttrID<b) or (AttrID>b+ord(High(TDemoNodeAttribute))) then
    exit(false);
  Attr:=TDemoNodeAttribute(AttrID-b);
  Result:=ExplicitAttributes[Attr]<>'';
end;

function TDemoNode.GetCSSExplicitAttribute(const AttrID: TCSSNumericalID): TCSSString;
var
  Attr: TDemoNodeAttribute;
  b: TCSSNumericalID;
begin
  b:=CSSRegistry.DemoAttrIDBase;
  if (AttrID<b) or (AttrID>b+ord(High(TDemoNodeAttribute))) then
    exit('');
  Attr:=TDemoNodeAttribute(AttrID-b);
  Result:=ExplicitAttributes[Attr];
end;

function TDemoNode.HasCSSPseudoClass(const AttrID: TCSSNumericalID): boolean;
var
  b: TCSSNumericalID;
begin
  b:=CSSRegistry.DemoPseudoClassIDBase;
  if (AttrID>=b) and (AttrID<=b+ord(High(TDemoPseudoClass))) then
    Result:=HasPseudoClass[TDemoPseudoClass(AttrID-b)]
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

function TDemoNode.GetCSSTypeName: TCSSString;
begin
  Result:=CSSTypeName;
end;

class function TDemoNode.GetClassCSSTypeID: TCSSNumericalID;
begin
  Result:=FDemoNodeTypeID;
end;

class procedure TDemoNode.SetClassCSSTypeID(aID: TCSSNumericalID);
begin
  FDemoNodeTypeID:=aID;
end;

function TDemoNode.GetCSSTypeID: TCSSNumericalID;
begin
  Result:=GetClassCSSTypeID;
end;

function TDemoNode.GetCSSPseudoElementName: TCSSString;
begin
  Result:='';
end;

function TDemoNode.GetCSSPseudoElementID: TCSSNumericalID;
begin
  Result:=CSSIDNone;
end;

class function TDemoNode.GetCSSTypeStyle: TCSSString;
begin
  Result:='';
end;

{ TDemoPseudoElement }

constructor TDemoPseudoElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (AOwner is TDemoNode) then
    raise Exception.Create('20250224153414');
end;

function TDemoPseudoElement.GetCSSTypeName: TCSSString;
begin
  Result:='';
end;

function TDemoPseudoElement.GetCSSTypeID: TCSSNumericalID;
begin
  Result:=CSSIDNone;
end;

function TDemoPseudoElement.GetCSSParent: ICSSNode;
begin
  Result:=TDemoNode(Owner);
end;

function TDemoPseudoElement.GetCSSIndex: integer;
begin
  Result:=-1;
end;

function TDemoPseudoElement.GetCSSNextSibling: ICSSNode;
begin
  Result:=nil;
end;

function TDemoPseudoElement.GetCSSPreviousSibling: ICSSNode;
begin
  Result:=nil;
end;

function TDemoPseudoElement.GetCSSNextOfType: ICSSNode;
begin
  Result:=nil;
end;

function TDemoPseudoElement.GetCSSPreviousOfType: ICSSNode;
begin
  Result:=nil;
end;

function TDemoPseudoElement.GetCSSEmpty: boolean;
begin
  Result:=true;
end;

function TDemoPseudoElement.GetCSSChildCount: integer;
begin
  Result:=0;
end;

function TDemoPseudoElement.GetCSSChild(const anIndex: integer): ICSSNode;
begin
  Result:=nil;
  if anIndex=0 then ;
end;

function TDemoPseudoElement.HasCSSClass(const aClassName: TCSSString): boolean;
begin
  Result:=false;
  if aClassName='' then ;
end;

function TDemoPseudoElement.GetCSSAttributeClass: TCSSString;
begin
  Result:='';
end;

{ TDemoFirstLine }

function TDemoFirstLine.GetCSSPseudoElementName: TCSSString;
begin
  Result:='first-line';
end;

function TDemoFirstLine.GetCSSPseudoElementID: TCSSNumericalID;
begin
  Result:=DemoFirstLineID;
end;

{ TCustomTestNewCSSResolver }

procedure TCustomTestNewCSSResolver.SetUp;
var
  AttrDesc: TCSSAttributeDesc;
begin
  inherited SetUp;

  TDemoNode.CSSRegistry:=TDemoCSSRegistry.Create();

  // register button attribute 'caption'
  AttrDesc:=TDemoNode.CSSRegistry.AddAttribute('caption');
  TDemoButton.CSSCaptionID:=AttrDesc.Index;

  FDoc:=TDemoDocument.Create(nil);
end;

procedure TCustomTestNewCSSResolver.TearDown;
begin
  FreeAndNil(FDoc);
  FreeAndNil(TDemoNode.CSSRegistry);
  inherited TearDown;
end;

procedure TCustomTestNewCSSResolver.ApplyStyle;
begin
  Doc.ApplyStyle;
  CheckWarnings;
end;

procedure TCustomTestNewCSSResolver.CheckWarnings;
var
  aResolver: TCSSResolver;
  i: Integer;
  Entry: TCSSResolverLogEntry;
  s: String;
begin
  aResolver:=FDoc.CSSResolver;
  if aResolver.LogCount=0 then exit;
  writeln('TCustomTestNewCSSResolver.CheckWarnings LogCount=',aResolver.LogCount);
  for i:=0 to aResolver.LogCount-1 do
  begin
    Entry:=aResolver.LogEntries[i];
    s:='';
    if Entry.PosEl<>nil then
      s:=' at '+Entry.PosEl.SourceFileName+'('+IntToStr(Entry.PosEl.SourceRow)+','+IntToStr(Entry.PosEl.SourceRow)+')';
    writeln('  ',Entry.MsgType,': ',Entry.ID,' ',Entry.Msg,s);
  end;
end;

{ TTestNewCSSResolver }

procedure TTestNewCSSResolver.Test_ParseAttr_Keyword;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:='* { direction: ltr; }';
  ApplyStyle;
  AssertEquals('Root.direction','ltr',Doc.Root.Direction);
end;

procedure TTestNewCSSResolver.Test_ParseAttr_Keyword_SkipInvalid;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:='* { direction: something ltr; }';
  ApplyStyle;
  AssertEquals('Root.direction','ltr',Doc.Root.Direction);
end;

procedure TTestNewCSSResolver.Test_ParseAttr_Float;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:=
     ':root {'
    +'  left: 10px;'
    +'  top: .1px;'
    +'  width: 3e2em;'
    +'  height: 3e-2px;'
    +'}'
    +'div {'
    +'  left: -4mm;'
    +'  top: -.5pc;'
    +'  width: .6cm;'
    +'  height: 6E+1rem;'
    +'}';
  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  ApplyStyle;
  AssertEquals('Root.Left','10px',Doc.Root.Left);
  AssertEquals('Root.Top','0.1px',Doc.Root.Top);
  AssertEquals('Root.Width','300em',Doc.Root.Width);
  AssertEquals('Root.Height','0.03px',Doc.Root.Height);
  AssertEquals('Div1.Left','-4mm',Div1.Left);
  AssertEquals('Div1.Top','-0.5pc',Div1.Top);
  AssertEquals('Div1.Width','0.6cm',Div1.Width);
  AssertEquals('Div1.Height','60rem',Div1.Height);
end;

procedure TTestNewCSSResolver.Test_ParseAttr_Float_SkipInvalid;
begin
  exit;

  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:=
     ':root {'
    +'  left: something 10px;'
    +'  top: 1 px;' // no space between number
    +'  width: 0 px;' // the px is ignored because of the space, 0 without unit is allowed
    +'  height: -4cm;' // no negative
    +'}';

  ApplyStyle;
  AssertEquals('Root.Left','10px',Doc.Root.Left);
  AssertEquals('Root.Top','invalid',Doc.Root.Top);
  AssertEquals('Root.Width','0',Doc.Root.Width);
  AssertEquals('Root.Height','invalid',Doc.Root.Height);
end;

procedure TTestNewCSSResolver.Test_Selector_Universal;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:='* { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','10px',Doc.Root.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_Type;
var
  Button: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button:=TDemoButton.Create(nil);
  Button.Parent:=Doc.Root;
  Doc.Style:='button { left: 11px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button.left','11px',Button.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_Type_Spaces;
var
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(nil);
  Button2.Parent:=Doc.Root;

  Doc.Style:='div, button ,span { left: 11px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','11px',Button1.Left);
  AssertEquals('Button2.left','11px',Button2.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_Id;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=TDemoButton.Create(nil);
  Button1.Name:='Button1';
  Button1.Parent:=Doc.Root;
  Doc.Style:='#Button1 { left: 12px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','12px',Button1.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_Class;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=TDemoButton.Create(nil);
  Button1.CSSClasses.Add('west');
  Button1.Parent:=Doc.Root;
  Doc.Style:='.west { left: 13px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','13px',Button1.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_ClassClass;
var
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(nil);
  Button1.CSSClasses.Add('west');
  Button1.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(nil);
  Button2.CSSClasses.DelimitedText:='west south';
  AssertEquals('Button2.CSSClasses.Count',2,Button2.CSSClasses.Count);
  Button2.Parent:=Doc.Root;

  Doc.Style:='.west.south { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_ClassSpaceClass;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.CSSClasses.Add('bird');

  Button1:=TDemoButton.Create(nil);
  Button1.CSSClasses.Add('west');
  Button1.Parent:=Doc.Root;

  Doc.Style:='.bird .west { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','10px',Button1.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_TypeCommaType;
var
  Button1: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Doc.Style:='div, button { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','10px',Button1.Left);
  AssertEquals('Div1.left','10px',Div1.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_ClassGTClass;
var
  Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.CSSClasses.Add('lvl1');

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.CSSClasses.Add('lvl2');
  Div1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(nil);
  Div2.Name:='Div2';
  Div2.CSSClasses.Add('lvl3');
  Div2.Parent:=Div1;

  Doc.Style:=LinesToStr([
  '.lvl1>.lvl2 { left: 10px; }', // set
  '.lvl1>.lvl3 { top: 11px; }', // not set, not direct children
  '.lvl2>.lvl3 { width: 12px; }', // set
  '']);
  ApplyStyle;
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

procedure TTestNewCSSResolver.Test_Selector_TypePlusType;
var
  Button1, Button2, Button3: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Button1:=TDemoButton.Create(nil);
  Button1.Name:='Button1';
  Button1.Parent:=Doc.Root;

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(nil);
  Button2.Name:='Button2';
  Button2.Parent:=Doc.Root;

  Button3:=TDemoButton.Create(nil);
  Button3.Name:='Button3';
  Button3.Parent:=Doc.Root;

  Doc.Style:='div+button { left: 10px; }'; // only Button2 has a prev sibling div
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Div1.left','',Div1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
  AssertEquals('Button3.left','',Button3.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_TypeTildeType;
var
  Button1, Button2, Button3: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(nil);
  Button2.Parent:=Doc.Root;

  Button3:=TDemoButton.Create(nil);
  Button3.Parent:=Doc.Root;

  Doc.Style:='div~button { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Div1.left','',Div1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
  AssertEquals('Button3.left','10px',Button3.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_HasAttribute;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.ExplicitAttributes[naLeft]:='100px';

  Button1:=TDemoButton.Create(nil);
  Button1.Name:='Button1';
  Button1.Parent:=Doc.Root;
  Button1.ExplicitAttributes[naLeft]:='2px';
  Button1.ExplicitCaption:='Click Button1';

  Doc.Style:=LinesToStr([
  '[left] { top: 3px; }',
  '[caption] { width: 4px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','3px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','3px',Button1.Top);
  AssertEquals('Button1.Width','4px',Button1.Width);
end;

procedure TTestNewCSSResolver.Test_Selector_AttributeEquals;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='2px';

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;
  Button1.ExplicitAttributes[naLeft]:='3px';
  Button1.ExplicitAttributes[naColor]:='maybe black';

  Doc.Style:=LinesToStr([
  '[left=2px] { top: 4px; }',
  '[color="maybe black"] { width: 5px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Button1.Top','',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestNewCSSResolver.Test_Selector_AttributeEqualsI;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='2px';

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;
  Button1.ExplicitAttributes[naLeft]:='3px';
  Button1.ExplicitAttributes[naColor]:='maybe Black';

  Doc.Style:=LinesToStr([
  '[left="2Px" i] { top: 4px; }',
  '[color="Maybe bLack" i] { width: 5px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Button1.Top','',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestNewCSSResolver.Test_Selector_AttributeBeginsWith;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;
  Button1.ExplicitAttributes[naLeft]:='Foo Bar';

  Doc.Style:=LinesToStr([
  '[left^=Fo] { top: 4px; }',
  '[left^="Foo B"] { width: 5px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','4px',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestNewCSSResolver.Test_Selector_AttributeEndsWith;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;
  Button1.ExplicitAttributes[naLeft]:='Foo Bar';

  Doc.Style:=LinesToStr([
  '[left$=o] { top: 4px; }',
  '[left$="o Bar"] { width: 5px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestNewCSSResolver.Test_Selector_AttributeBeginsWithHyphen;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;
  Button1.ExplicitAttributes[naLeft]:='Foo-Bar';

  Doc.Style:=LinesToStr([
  '[left|=Foo] { top: 4px; }',
  '[left|="Fo"] { width: 5px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','4px',Button1.Top);
  AssertEquals('Button1.Width','',Button1.Width);
end;

procedure TTestNewCSSResolver.Test_Selector_AttributeContainsWord;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.ExplicitAttributes[naLeft]:='One Two Three';

  Button1:=TDemoButton.Create(nil);
  Button1.Name:='Button1';
  Button1.Parent:=Doc.Root;
  Button1.ExplicitAttributes[naLeft]:='Four Five';

  Doc.Style:=LinesToStr([
  '[left~=One] { top: 4px; }',
  '[left~=Two] { width: 5px; }',
  '[left~=Three] { height: 6px; }',
  '[left~="Four Five"] { color: #123; }',  // not one word, so does not match!
  '[left~=our] { display: none; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','5px',Doc.Root.Width);
  AssertEquals('Root.Height','6px',Doc.Root.Height);
  AssertEquals('Root.Color','',Doc.Root.Color);
  AssertEquals('Root.Display','',Doc.Root.Display);
  AssertEquals('Button1.Top','',Button1.Top);
  AssertEquals('Button1.Width','',Button1.Width);
  AssertEquals('Button1.Height','',Button1.Height);
  AssertEquals('Button1.Color','',Button1.Color);
  AssertEquals('Button1.Display','inline-block',Button1.Display);
end;

procedure TTestNewCSSResolver.Test_Selector_AttributeContainsSubstring;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;
  Button1.ExplicitAttributes[naLeft]:='Foo Bar';

  Doc.Style:=LinesToStr([
  '[left*=oo] { top: 4px; }',
  '[left*="o B"] { width: 5px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Top','4px',Button1.Top);
  AssertEquals('Button1.Width','5px',Button1.Width);
end;

procedure TTestNewCSSResolver.Test_Selector_Root;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':roOt { top: 4px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Button1.Top','',Button1.Top);
end;

procedure TTestNewCSSResolver.Test_Selector_Empty;
var
  Div1, Div11, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(nil);
  Div11.Parent:=Div1;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':eMpty { left: 1px; }',
  'div:emPty { top: 2px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Div11.Left','1px',Div11.Left);
  AssertEquals('Div11.Top','2px',Div11.Top);
  AssertEquals('Div2.Left','1px',Div2.Left);
  AssertEquals('Div2.Top','2px',Div2.Top);
end;

procedure TTestNewCSSResolver.Test_Selector_FirstChild;
var
  Div1, Div11, Div12, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(nil);
  Div11.Parent:=Div1;

  Div12:=TDemoDiv.Create(nil);
  Div12.Parent:=Div1;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':first-child { left: 1px; }',
  'div:first-child { top: 2px; }',
  '']);
  ApplyStyle;
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

procedure TTestNewCSSResolver.Test_Selector_LastChild;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(nil);
  Div11.Parent:=Div1;

  Button12:=TDemoButton.Create(nil);
  Button12.Parent:=Div1;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':last-child { left: 6px; }',
  'div:last-child { top: 7px; }',
  '']);
  ApplyStyle;
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

procedure TTestNewCSSResolver.Test_Selector_OnlyChild;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(nil);
  Div11.Name:='Div11';
  Div11.Parent:=Div1;

  Div2:=TDemoDiv.Create(nil);
  Div2.Name:='Div2';
  Div2.Parent:=Doc.Root;

  Button12:=TDemoButton.Create(nil);
  Button12.Name:='Button12';
  Button12.Parent:=Div2;

  Doc.Style:=LinesToStr([
  ':only-child { left: 8px; }',
  'div:only-child { top: 9px; }',
  '']);
  ApplyStyle;
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

procedure TTestNewCSSResolver.Test_Selector_Not;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(nil);
  Div11.Name:='Div11';
  Div11.Parent:=Div1;

  Div2:=TDemoDiv.Create(nil);
  Div2.Name:='Div2';
  Div2.Parent:=Doc.Root;

  Button12:=TDemoButton.Create(nil);
  Button12.Name:='Button12';
  Button12.Parent:=Div2;

  Doc.Style:=LinesToStr([
  ':not(:only-child) { left: 8px; }',
  ':not(div:only-child) { top: 9px; }',
  '']);
  ApplyStyle;
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

procedure TTestNewCSSResolver.Test_Selector_NthChild;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Div3:=TDemoDiv.Create(nil);
  Div3.Parent:=Doc.Root;

  Div4:=TDemoDiv.Create(nil);
  Div4.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  'div:nth-child(2n+1) { left: 8px; }',
  'div:nth-child(n+3) { border-width: 6px; }',
  'div:nth-child(-n+2) { height: 7em; }',
  'div:nth-child(even) { top: 3px; }',
  'div:nth-child(odd) { width: 4px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Root.BorderWidth','',Doc.Root.BorderWidth);
  AssertEquals('Root.Height','',Doc.Root.Height);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Div1.Left','8px',Div1.Left);
  AssertEquals('Div1.BorderWidth','',Div1.BorderWidth);
  AssertEquals('Div1.Height','7em',Div1.Height);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Div1.Width','4px',Div1.Width);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Div2.BorderWidth','',Div2.BorderWidth);
  AssertEquals('Div2.Height','7em',Div2.Height);
  AssertEquals('Div2.Top','3px',Div2.Top);
  AssertEquals('Div2.Width','',Div2.Width);
  AssertEquals('Div3.Left','8px',Div3.Left);
  AssertEquals('Div3.BorderWidth','6px',Div3.BorderWidth);
  AssertEquals('Div3.Height','',Div3.Height);
  AssertEquals('Div3.Top','',Div3.Top);
  AssertEquals('Div3.Width','4px',Div3.Width);
  AssertEquals('Div4.Left','',Div4.Left);
  AssertEquals('Div4.BorderWidth','6px',Div4.BorderWidth);
  AssertEquals('Div4.Height','',Div4.Height);
  AssertEquals('Div4.Top','3px',Div4.Top);
  AssertEquals('Div4.Width','',Div4.Width);
end;

procedure TTestNewCSSResolver.Test_Selector_NthLastChild;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Div3:=TDemoDiv.Create(nil);
  Div3.Parent:=Doc.Root;

  Div4:=TDemoDiv.Create(nil);
  Div4.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':nth-last-child(2n+1) { left: 8px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div2.Left','8px',Div2.Left);
  AssertEquals('Div3.Left','',Div3.Left);
  AssertEquals('Div4.Left','8px',Div4.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_NthChildOf;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(nil);
  Div2.Name:='Div2';
  Div2.Parent:=Doc.Root;
  Div2.ExplicitAttributes[naTop]:='3px';

  Div3:=TDemoDiv.Create(nil);
  Div3.Name:='Div3';
  Div3.Parent:=Doc.Root;
  Div3.ExplicitAttributes[naTop]:='3px';

  Div4:=TDemoDiv.Create(nil);
  Div4.Name:='Div4';
  Div4.Parent:=Doc.Root;
  Div4.ExplicitAttributes[naTop]:='3px';

  Doc.Style:=LinesToStr([
  ':nth-child(2n+1 of [top=3px]) { left: 5px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div2.Left','5px',Div2.Left);
  AssertEquals('Div3.Left','',Div3.Left);
  AssertEquals('Div4.Left','5px',Div4.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_FirstOfType;
var
  Div1, Div11, Div13, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(nil);
  Div11.Parent:=Div1;

  Button12:=TDemoButton.Create(nil);
  Button12.Parent:=Div1;

  Div13:=TDemoDiv.Create(nil);
  Div13.Parent:=Div1;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':first-of-type { left: 6px; }',
  'div:first-of-type { top: 7px; }',
  '']);
  ApplyStyle;
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

procedure TTestNewCSSResolver.Test_Selector_LastOfType;
var
  Div1, Div11, Div13, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(nil);
  Div11.Parent:=Div1;

  Button12:=TDemoButton.Create(nil);
  Button12.Parent:=Div1;

  Div13:=TDemoDiv.Create(nil);
  Div13.Parent:=Div1;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':last-of-type { left: 6px; }',
  'div:last-of-type { top: 7px; }',
  '']);
  ApplyStyle;
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

procedure TTestNewCSSResolver.Test_Selector_OnlyOfType;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Div11:=TDemoDiv.Create(nil);
  Div11.Parent:=Div1;

  Button12:=TDemoButton.Create(nil);
  Button12.Parent:=Div1;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':only-of-type { left: 6px; }',
  'div:only-of-type { top: 7px; }',
  '']);
  ApplyStyle;
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

procedure TTestNewCSSResolver.Test_Selector_NthOfType;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Button1:=TDemoButton.Create(nil);
  Button1.Name:='Button1';
  Button1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(nil);
  Div2.Name:='Div2';
  Div2.Parent:=Doc.Root;

  Div3:=TDemoDiv.Create(nil);
  Div3.Name:='Div3';
  Div3.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(nil);
  Button2.Name:='Button2';
  Button2.Parent:=Doc.Root;

  Div4:=TDemoDiv.Create(nil);
  Div4.Name:='Div4';
  Div4.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':nth-of-type(2n+1) { left: 8px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','8px',Div1.Left);
  AssertEquals('Button1.Left','8px',Button1.Left);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Div3.Left','8px',Div3.Left);
  AssertEquals('Button2.Left','',Button2.Left);
  AssertEquals('Div4.Left','',Div4.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_NthLastOfType;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;

  Button1:=TDemoButton.Create(nil);
  Button1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Div3:=TDemoDiv.Create(nil);
  Div3.Parent:=Doc.Root;

  Button2:=TDemoButton.Create(nil);
  Button2.Parent:=Doc.Root;

  Div4:=TDemoDiv.Create(nil);
  Div4.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  ':nth-last-of-type(2n+1) { left: 8px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Button1.Left','',Button1.Left);
  AssertEquals('Div2.Left','8px',Div2.Left);
  AssertEquals('Div3.Left','',Div3.Left);
  AssertEquals('Button2.Left','8px',Button2.Left);
  AssertEquals('Div4.Left','8px',Div4.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_Is;
var
  Div1, Div2: TDemoDiv;
  Button1, Button2: TDemoButton;
  Span1: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.ExplicitAttributes[naTop]:='3px';

  Button1:=TDemoButton.Create(nil);
  Button1.Name:='Button1';
  Button1.Parent:=Doc.Root;

  Div2:=TDemoDiv.Create(nil);
  Div2.Parent:=Doc.Root;

  Span1:=TDemoSpan.Create(nil);
  Span1.Parent:=Doc.Root;
  Span1.ExplicitAttributes[naTop]:='3px';

  Button2:=TDemoButton.Create(nil);
  Button2.Parent:=Doc.Root;
  Button2.ExplicitAttributes[naTop]:='3px';

  Doc.Style:=LinesToStr([
  ':is(div, button)[top=3px] { left: 7px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','7px',Div1.Left);
  AssertEquals('Button1.Left','',Button1.Left);
  AssertEquals('Div2.Left','',Div2.Left);
  AssertEquals('Span1.Left','',Div2.Left);
  AssertEquals('Button2.Left','7px',Button2.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_Where;
var
  Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.ExplicitAttributes[naTop]:='3px';

  Div2:=TDemoDiv.Create(nil);
  Div2.Name:='Div2';
  Div2.Parent:=Div1;
  Div2.ExplicitAttributes[naTop]:='3px';

  Doc.Style:=LinesToStr([
  ':where(div[top=3px]) { left: 1px; }',
  'div div { left: 2px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','1px',Div1.Left);
  AssertEquals('Div2.Left','2px',Div2.Left);
end;

procedure TTestNewCSSResolver.Test_Selector_Hover;
var
  Div1, Div11: TDemoDiv;
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.Hover:=true;

  Button1:=TDemoButton.Create(nil);
  Button1.Name:='Button1';
  Button1.Parent:=Div1;
  Button1.Hover:=true;

  Div11:=TDemoDiv.Create(nil);
  Div11.Name:='Div11';
  Div11.Parent:=Div1;

  Doc.Style:=LinesToStr([
  ':hover { left: 1px; }',
  'button:hover { top: 2px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Root.Top','',Doc.Root.Top);
  AssertEquals('Div1.Left','1px',Div1.Left);
  AssertEquals('Div1.Top','',Div1.Top);
  AssertEquals('Button1.Left','1px',Button1.Left);
  AssertEquals('Button1.Top','2px',Button1.Top);
  AssertEquals('Div11.Left','',Div11.Left);
  AssertEquals('Div11.Top','',Div11.Top);
end;

procedure TTestNewCSSResolver.Test_InlineStyle;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;
  Div1.InlineStyle:='left: 10px; top: 5px';

  Doc.Style:=LinesToStr([
  'div { left: 6px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','10px',Div1.Left);
  AssertEquals('Div1.Top','5px',Div1.Top);
end;

procedure TTestNewCSSResolver.Test_InlineStyle_DisplayNone;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Parent:=Doc.Root;
  Div1.InlineStyle:='display:none';

  ApplyStyle;
  AssertEquals('Div1.Display','none',Div1.Display);
end;

procedure TTestNewCSSResolver.Test_Specificity_Id_Class;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.CSSClasses.Add('bird');

  Doc.Style:=LinesToStr([
  '.bird { left: 6px; }',
  '#Div1 { left: 7px; top: 8px; }', // id has higher Specificity, no matter if before or after a .class
  '.bird { top: 9px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','7px',Div1.Left);
  AssertEquals('Div1.Top','8px',Div1.Top);
end;

procedure TTestNewCSSResolver.Test_Specificity_Important;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.CSSClasses.Add('bird');

  Doc.Style:=LinesToStr([
  '.bird { left: 6px !important; }',
  '#Div1 { left: 7px; top: 8px; }',
  '.bird { top: 9px ! important; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','6px',Div1.Left);
  AssertEquals('Div1.Top','9px',Div1.Top);
end;

procedure TTestNewCSSResolver.Test_Specificity_Shorthand_OneRule;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.CSSClasses.Add('bird');

  Doc.Style:='.bird { border-color: blue; border: 6px red; border-width: 7px; }';
  ApplyStyle;
  AssertEquals('Div1.BorderColor','red',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','7px',Div1.BorderWidth);
end;

procedure TTestNewCSSResolver.Test_Specificity_Shorthand_ClassClass;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.CSSClasses.Add('bird');
  Div1.CSSClasses.Add('eagle');

  Doc.Style:=LinesToStr([
  '.bird.eagle { border-color: blue; }',
  '.bird { border-width: 6px; }',
  '.bird { border: 7px red; }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.BorderColor','blue',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','7px',Div1.BorderWidth);
end;

procedure TTestNewCSSResolver.Test_Specificity_Longhand_All_Longhand;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.CSSClasses.Add('bird');
  Div1.CSSClasses.Add('eagle');

  Doc.Style:=LinesToStr([
  '.bird.eagle { border-color: blue; }',
  '.bird { border-width: 7px; }',
  '.bird { all: initial; }',
  '.bird { background: red; }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.BorderColor','blue',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','',Div1.BorderWidth);
  AssertEquals('Div1.Background','red',Div1.Background);
end;

procedure TTestNewCSSResolver.Test_Specificity_Shorthand_All_Shorthand;
var
  Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.CSSClasses.Add('bird');

  Div2:=TDemoDiv.Create(nil);
  Div2.Name:='Div2';
  Div2.Parent:=Doc.Root;
  Div2.CSSClasses.Add('eagle');

  Doc.Style:=LinesToStr([
  '.bird { border: 7px blue; }',
  '.bird { all: initial; }',
  '.eagle { all: initial; }',
  '.eagle { border: 8px red; }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.BorderColor','',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','',Div1.BorderWidth);
  AssertEquals('Div2.BorderColor','red',Div2.BorderColor);
  AssertEquals('Div2.BorderWidth','8px',Div2.BorderWidth);
end;

procedure TTestNewCSSResolver.Test_Origin_Id_Class;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Doc.CSSResolver.AddStyleSheet(cssoUserAgent,'testagent',
  '#Div1 { border-width: 2px;'
  +' border-color: blue !important;'
  +' background: green; }'
  );

  Doc.Style:=LinesToStr([
  'div { border-width: 3px; ', // although class has lower spec than id, author origin wins
  ' border-color: orange;', // former important always wins
  '}']);
  ApplyStyle;
  AssertEquals('Div1.BorderColor','blue',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','3px',Div1.BorderWidth);
  AssertEquals('Div1.Background','green',Div1.Background);
end;

procedure TTestNewCSSResolver.Test_Var_NoDefault;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  'div {',
  '  --bird-color: red;',
  '}',
  'div {',
  '  border-color: var(--bird-color);',
  '  border-width: var(--bird-width);',
  '  color: var(--bird-nothing);',
  '}',
  'div {',
  '  --bird-width: 3px;',
  '}']);
  ApplyStyle;
  AssertEquals('Div1.BorderColor','red',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','3px',Div1.BorderWidth);
  AssertEquals('Div1.Color','',Div1.Color);
end;

procedure TTestNewCSSResolver.Test_Var_Inline_NoDefault;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Doc.Style:=LinesToStr([
  'div {',
  '  --bird-color: red;',
  '}']);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.InlineStyle:='--bird-width: 3px; border-color: var(--bird-color); border-width: var(--bird-width);';

  ApplyStyle;
  AssertEquals('Div1.BorderColor','red',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','3px',Div1.BorderWidth);
end;

procedure TTestNewCSSResolver.Test_Var_Defaults;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  'div {',
  '  --def-color:blue;',
  '}',
  'div {',
  '  color: var(--bird-color,);',
  '  border-color: var(--bird-border-color,var(--def-color));',
  '  border-width: var(--bird-border-width,3px);',
  '}']);
  ApplyStyle;
  AssertEquals('Div1.BorderColor','blue',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','3px',Div1.BorderWidth);
  AssertEquals('Div1.Color','',Div1.Color);
end;

procedure TTestNewCSSResolver.Test_PseudoElement;
var
  Div1: TDemoDiv;
  FirstLine: TDemoFirstLine;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  'div {',
  '  border-color:red;',
  '}',
  '#Div1::first-line {',
  '  color:red;',
  '  border-color:white;',
  '}',
  'div {',
  '  color: blue;',
  '}']);
  ApplyStyle;
  FirstLine:=TDemoFirstLine.Create(Div1);
  FirstLine.ApplyCSS(Doc.CSSResolver);

  AssertEquals('Div1.BorderColor','red',Div1.BorderColor);
  AssertEquals('Div1.Color','blue',Div1.Color);
  AssertEquals('Div1::first-line.BorderColor','white',FirstLine.BorderColor);
  AssertEquals('Div1::first-line.Color','red',FirstLine.Color);
end;

procedure TTestNewCSSResolver.Test_PseudoElement_Unary;
var
  Div1: TDemoDiv;
  FirstLine: TDemoFirstLine;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;

  Doc.Style:=LinesToStr([
  '::first-line {',
  '  color:red;',
  '}',
  'div {',
  '  color: blue;',
  '}']);
  ApplyStyle;
  FirstLine:=TDemoFirstLine.Create(Div1);
  FirstLine.ApplyCSS(Doc.CSSResolver);

  AssertEquals('Div1.Color','blue',Div1.Color);
  AssertEquals('Div1::first-line.Color','red',FirstLine.Color);
end;

procedure TTestNewCSSResolver.Test_PseudoElement_PostfixSelectNothing;
var
  Div1: TDemoDiv;
  FirstLine: TDemoFirstLine;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=TDemoDiv.Create(nil);
  Div1.Name:='Div1';
  Div1.Parent:=Doc.Root;
  Div1.CSSClasses.Add('Big');

  Doc.Style:=LinesToStr([
  'div::first-line#Bird {',
  '  color:red;',
  '}',
  'div::first-line.Big {',
  '  border-color:red;',
  '}',
  'div {',
  '  color: blue;',
  '  border-color: blue;',
  '}']);
  ApplyStyle;
  FirstLine:=TDemoFirstLine.Create(Div1);
  FirstLine.ApplyCSS(Doc.CSSResolver);

  AssertEquals('Div1.Color','blue',Div1.Color);
  AssertEquals('Div1.BorderColor','blue',Div1.BorderColor);
  AssertEquals('Div1::first-line.Color','',FirstLine.Color);
  AssertEquals('Div1::first-line.BorderColor','',FirstLine.BorderColor);
end;

initialization
  RegisterTests([TTestNewCSSResolver]);

end.

