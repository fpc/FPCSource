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
  Classes, SysUtils, Math, Contnrs, fpcunit, testregistry, fpCSSTree,
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
      var AttrIDs: TCSSNumericalIDArray; var Values: TBytesArray);
  public

    const
      // keywords
      kwNone=CSSKeywordNone;
      kwRed=CSSKeyword_LastResolver+1;
      kwGreen=kwRed+1;
      kwBlue=kwGreen+1;
      kwWhite=kwBlue+1;
      kwBlack=kwWhite+1;
      kwBlock=kwBlack+1;
      kwInline_Block=kwBlock+1;
      kwLTR=kwInline_Block+1;
      kwRTL=kwLTR+1;
      kwScreen=kwRTL+1;
      kwOrientation=kwScreen+1;
      kwPortrait=kwOrientation+1;
      kwAspectRatio=kwPortrait+1;
      kwWidth=kwAspectRatio+1;
      kwHeight=kwWidth+1;
    var

    DemoAttrIDBase: TCSSNumericalID;
    DemoPseudoClassIDBase: TCSSNumericalID;
    DemoElementTypeIDBase: TCSSNumericalID;

    DemoAttrs: array[TDemoNodeAttribute] of TDemoCSSAttributeDesc;
    DemoPseudoClasses: array[TDemoPseudoClass] of TDemoCSSPseudoClassDesc;
    DemoTypes: array[TDemoElementType] of TDemoCSSTypeDesc;

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

  TDemoNode = class(TComponent,ICSSNode,IFPObserver)
  private
    class var CSSRegistry: TDemoCSSRegistry;
    class var FDemoNodeTypeID: TCSSNumericalID;
  private
    FNodes: TFPObjectList; // list of TDemoNode
    FCSSClasses: TStrings;
    FCSSClassArr: TCSSNumericalIDArray; // cached class ids of FCSSClasses, valid if FCSSClassArrValid and FCSSClassArrStamp=Resolver.CSSClassIDStamp
    FCSSClassArrValid: boolean;
    FCSSClassArrStamp: TCSSNumericalID; // Resolver.CSSClassIDStamp when FCSSClassArr was built
    FResolver: TCSSResolver; // last resolver, used to map class names to ids
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
    // IFPObserver: FCSSClasses changed -> invalidate FCSSClassArr cache
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
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
    function GetCSSID: TCSSNumericalID; virtual;
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
    function HasCSSClass(const aClassID: TCSSNumericalID): boolean; virtual;
    function GetCSSClasses: TCSSNumericalIDArray; virtual;
    function GetCSSAttributeClass: TCSSString; virtual;
    function GetCSSAttributeID: TCSSString; virtual;
    function GetCSSCustomAttribute(const AttrID: TCSSNumericalID): TBytes; virtual;
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
    function HasCSSClass(const aClassID: TCSSNumericalID): boolean; override;
    function GetCSSClasses: TCSSNumericalIDArray; override;
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

  TDemoMediaRangeType = (
    dmrtNone,
    dmrtLength,
    dmrtRatio
    );
  TDemoMediaRangeTypes = set of TDemoMediaRangeType;

  { TDemoDocument }

  TDemoDocument = class(TComponent)
  private
    FCSSResolver: TCSSResolver;
    FHeight: integer;
    FMediaEvalCount: integer;
    FStyle: TCSSString;
    FWidth: integer;
    function HasMediaBoolean(aResolver: TCSSBaseResolver; KW: TCSSNumericalID): boolean;
    function IsMediaPlain(aResolver: TCSSBaseResolver; KW: TCSSNumericalID;
      const aValue: TCSSResCompValue): boolean;
    function MediaCompare(aResolver: TCSSBaseResolver; KW: TCSSNumericalID;
      const aValue: TCSSResCompValue; out Cmp: integer): boolean;
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
    property MediaEvalCount: integer read FMediaEvalCount;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
  end;

  { TCustomTestCSSResolver }

  TCustomTestCSSResolver = class(TTestCase)
  private
    FDoc: TDemoDocument;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ApplyStyle; virtual;
    procedure CheckWarnings; virtual;
    function AddButton(const aName: string; aParent: TDemoNode): TDemoButton;
    function AddDiv(const aName: string; aParent: TDemoNode): TDemoDiv;
    function AddSpan(const aName: string; aParent: TDemoNode): TDemoSpan;
    function AddSpan_Class(const aName, aClass: string; aParent: TDemoNode): TDemoSpan;
  public
    property Doc: TDemoDocument read FDoc;
  end;

  { TTestCSSResolver }

  TTestCSSResolver = class(TCustomTestCSSResolver)
  private
    procedure CheckTokenize(const Title, aValue, Expected: string);
    procedure CheckTokenizeInvalid(const Title, aValue: string);
    procedure CheckRoundtrip(const Title, aValue, Expected: string);
    // locate a declaration in the author 'test.css' sheet by top-level selector
    // and property name, e.g. FindAuthorDecl('.bird','left')
    function FindAuthorDecl(const aSelector, aProp: string): TCSSDeclarationElement;
  published
    // invalid attributes while parsing stylesheet
    procedure TestRes_ParseAttr_Keyword;
    procedure TestRes_ParseAttr_Float;

    // low level tokenizer
    procedure TestRes_Tokenize_Empty;
    procedure TestRes_Tokenize_Keyword;
    procedure TestRes_Tokenize_Identifier;
    procedure TestRes_Tokenize_Float;
    procedure TestRes_Tokenize_Whitespace;
    procedure TestRes_Tokenize_Symbols;
    procedure TestRes_Tokenize_PlusMinus;
    procedure TestRes_Tokenize_Function;
    procedure TestRes_Tokenize_Brackets;
    procedure TestRes_Tokenize_HexColor;
    procedure TestRes_Tokenize_Strings;
    procedure TestRes_Tokenize_Invalid;
    procedure TestRes_Detokenize;

    procedure TestRes_Selector_Universal;
    procedure TestRes_Selector_Type;
    procedure TestRes_Selector_Type_Spaces;
    procedure TestRes_Selector_Id;
    procedure TestRes_Selector_Class;
    procedure TestRes_CSSClassID_Numbering;
    procedure TestRes_CSSClassID_Unknown;
    procedure TestRes_CSSClassID_CaseSensitive;
    procedure TestRes_CSSClassID_ResolvedElement;
    procedure TestRes_CSSClassID_AddClassToStylesheet;
    procedure TestRes_CSSClassID_DeleteClassFromStylesheet;
    procedure TestRes_CSSClassID_ChangeNodeClass; // add/delete a class on the node between two ApplyStyle
    procedure TestRes_CSSID_Numbering;
    procedure TestRes_CSSID_Unknown;
    procedure TestRes_CSSID_CaseSensitive;
    procedure TestRes_CSSID_ResolvedElement;
    procedure TestRes_CSSID_AddIdToStylesheet;
    procedure TestRes_CSSID_DeleteIdFromStylesheet;
    procedure TestRes_CSSID_ChangeNodeId; // rename a node's id between two ApplyStyle
    procedure TestRes_Selector_ClassClass; // AND combinator
    procedure TestRes_Selector_ClassSpaceClass; // Descendant combinator
    procedure TestRes_Selector_TypeCommaType; // OR combinator
    procedure TestRes_Selector_ClassGTClass; // child combinator
    procedure TestRes_Selector_TypePlusType; // adjacent sibling combinator
    procedure TestRes_Selector_TypeTildeType; // general sibling combinator

    // explicit attributes, e.g. set by HTML
    procedure TestRes_Selector_HasAttribute;
    procedure TestRes_Selector_AttributeEquals;
    procedure TestRes_Selector_AttributeEqualsI;
    procedure TestRes_Selector_AttributeBeginsWith;
    procedure TestRes_Selector_AttributeEndsWith;
    procedure TestRes_Selector_AttributeBeginsWithHyphen;
    procedure TestRes_Selector_AttributeContainsWord;
    procedure TestRes_Selector_AttributeContainsSubstring;

    // pseudo classes and functions
    procedure TestRes_Selector_Root;
    procedure TestRes_Selector_Empty;
    procedure TestRes_Selector_FirstChild;
    procedure TestRes_Selector_LastChild;
    procedure TestRes_Selector_OnlyChild;
    procedure TestRes_Selector_Not;
    procedure TestRes_Selector_NthChild;
    procedure TestRes_Selector_NthChild2;
    procedure TestRes_Selector_NthLastChild;
    procedure TestRes_Selector_NthChildOf;
    procedure TestRes_Selector_FirstOfType;
    procedure TestRes_Selector_LastOfType;
    procedure TestRes_Selector_OnlyOfType;
    procedure TestRes_Selector_NthOfType;
    procedure TestRes_Selector_NthLastOfType;
    procedure TestRes_Selector_Is;
    // ToDo: procedure TestRes_Selector_Is_Descendant;  :is(div button, .hawk .eagle)
    procedure TestRes_Selector_Where;
    // ToDo: div:has(>img)
    // ToDo: div:has(+img)

    // custom pseudo classes and functions
    procedure TestRes_Selector_Hover;

    // inline style
    procedure TestRes_InlineStyle;
    procedure TestRes_InlineStyle_DisplayNone;

    // Specificity
    procedure TestRes_Specificity_Id_Class;
    procedure TestRes_Specificity_Important;
    procedure TestRes_Specificity_Shorthand_OneRule;
    procedure TestRes_Specificity_Shorthand_ClassClass;
    procedure TestRes_Specificity_Longhand_All_Longhand;
    procedure TestRes_Specificity_Shorthand_All_Shorthand;

    // disable/enable declarations
    procedure TestRes_Disable_Longhand;
    procedure TestRes_Disable_Shorthand;
    procedure TestRes_Disable_All;
    procedure TestRes_Disable_Restore_AfterReparse;
    procedure TestRes_Disable_QueryMethods;

    // origin
    procedure TestRes_Origin_Id_Class;
    procedure TestRes_Origin_UserBeatsUserAgent; // user normal beats user-agent normal
    procedure TestRes_Origin_AuthorBeatsUser; // author normal beats user normal
    procedure TestRes_Origin_AuthorBeatsUserAgentDespiteSpecificity; // origin trumps selector specificity
    procedure TestRes_Origin_Important; // !important beats normal; among importants the last origin wins
    procedure TestRes_Origin_SourceOrderAcrossOrigins; // rules from all origins collected and sorted

    // InsertStyleSheet / DeleteStyleSheet: within one origin the higher index (later
    // document order) wins the cascade tie-break
    procedure TestRes_InsertStyleSheet_HigherIndexWins;
    procedure TestRes_InsertStyleSheet_MidListOrdered; // mid-list insert beats lower, loses to higher index
    procedure TestRes_DeleteStyleSheet_LowerIndexWinsAfterDelete;
    procedure TestRes_DeleteStyleSheet_RemovesContribution;

    // var()
    procedure TestRes_Var_NoDefault;
    procedure TestRes_Var_Inline_NoDefault;
    procedure TestRes_Var_Defaults;
    procedure TestRes_Var_MixedCase;
    procedure TestRes_Var_StringLiteral;

    // pseudo elements (works like child combinator)
    procedure TestRes_PseudoElement;
    procedure TestRes_PseudoElement_Unary;
    procedure TestRes_PseudoElement_PostfixSelectNothing;

    // nested rules
    procedure TestRes_Nested_Hash; // #id -> Descendant combinator
    procedure TestRes_Nested_Class; // .class -> Descendant combinator
    procedure TestRes_Nested_AndClass; // & AND selector
    procedure TestRes_Nested_AndSpaceClass; // & .class -> Descendant combinator
    procedure TestRes_Nested_ClassCommaClass; // .class,.class: comma: no & is treated as whitespace -> Descendant combinator
    procedure TestRes_Nested_ClassCommaAndClass; // .class,&.class: descendant OR compound
    procedure TestRes_Nested_ClassCommaAndSpaceClass; // .class,& .class: Descendant combinator
    procedure TestRes_Nested_ClassSpaceAnd; // .class & -> append: & is the subject
    procedure TestRes_Nested_AndSpaceType; // & type -> Descendant combinator
    procedure TestRes_Nested_GTClass; // child combinator
    procedure TestRes_Nested_AndGTClass; // & child combinator
    procedure TestRes_Nested_PlusClass; // adjacent sibling combinator
    procedure TestRes_Nested_AndPlusType; // & adjacent sibling combinator
    procedure TestRes_Nested_TildeClass; // general sibling combinator
    procedure TestRes_Nested_AndTildeClass; // & general sibling combinator
    procedure TestRes_Nested_HasAtribute; // [attr]
    procedure TestRes_Nested_AndHasAtribute; // &[attr]
    procedure TestRes_Nested_AndHasSpaceAtribute; // & [attr]

    // @media
    procedure TestRes_Media_Name; // test boolean
    procedure TestRes_Media_NameColonValue; // test plain (name:value)
    procedure TestRes_Media_Range_NameGtValue;
    procedure TestRes_Media_Range_ValueLtName;
    procedure TestRes_Media_Range_NameGtName;
    procedure TestRes_Media_Range_ValueLtNameLtValue;
    procedure TestRes_Media_Range_ValueGtNameGtValue;
    procedure TestRes_Media_Ratio;
    procedure TestRes_Media_And;
    procedure TestRes_Media_Or;
    procedure TestRes_Media_Comma;
    procedure TestRes_Media_Not;
    procedure TestRes_Media_NotAnd;
    procedure TestRes_Media_EvalOncePerInit;
    procedure TestRes_Media_ReplaceStyleSheet;
    // todo procedure TestRes_Media_Only

    // rule buckets: selectors bucketed by their rightmost identifier
    procedure TestRes_Buckets_GetCSSClasses;
    procedure TestRes_Buckets_ClassBucket; // only the .red bucket matches
    procedure TestRes_Buckets_TypeBucket; // only the button bucket matches
    procedure TestRes_Buckets_IdBucket; // only the #id bucket matches
    procedure TestRes_Buckets_IdBucketNumerical; // node id index selects the id bucket
    procedure TestRes_Buckets_OtherBucket; // universal/pseudo-only rules always apply
    procedure TestRes_Buckets_MultiSelectorRuleOnce; // rule with several matching selectors added once
    procedure TestRes_Buckets_NonMatchingSkipped; // wrong class/type/id never apply
    procedure TestRes_Buckets_CompoundRightmost; // div.red bucketed by class .red
    procedure TestRes_Buckets_DescendantRightmost; // div .red bucketed by class .red
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

function TokenStreamToStr(Registry: TCSSRegistry; const Data: TBytes): string;
// Human readable representation of a token stream produced by Tokenize,
// so tests can compare against an expected string.
var
  i: integer;

  function RByte: byte;
  begin
    Result:=Data[i];
    inc(i);
  end;

  function RWord: word;
  begin
    Result:=PWord(@Data[i])^;
    inc(i,2);
  end;

  function RDWord: DWord;
  begin
    Result:=PDWord(@Data[i])^;
    inc(i,4);
  end;

  function RDouble: double;
  begin
    Result:=PDouble(@Data[i])^;
    inc(i,8);
  end;

  function RStr(Count: cardinal): string;
  begin
    Result:='';
    if Count=0 then exit;
    SetLength(Result,Count);
    Move(Data[i],Result[1],Count);
    inc(i,Count);
  end;

var
  Kind: TCSSResTokenKind;
  U: TCSSUnit;
begin
  Result:='';
  i:=0;
  while i<length(Data) do
  begin
    Kind:=TCSSResTokenKind(RByte);
    case Kind of
    rtkWhitespace: Result:=Result+'ws ';
    rtkSymbol: Result:=Result+'sym('+Chr(RByte)+') ';
    rtkLParenthesis: Result:=Result+'( ';
    rtkRParenthesis: Result:=Result+') ';
    rtkLBracket: Result:=Result+'[ ';
    rtkRBracket: Result:=Result+'] ';
    rtkPlus: Result:=Result+'plus ';
    rtkMinus: Result:=Result+'minus ';
    rtkFloat:
      begin
        U:=TCSSUnit(RByte);
        Result:=Result+'float('+FloatToCSSStr(RDouble)+CSSUnitNames[U]+') ';
      end;
    rtkKeyword: Result:=Result+'kw('+Registry.Keywords[RWord]+') ';
    rtkFunction: Result:=Result+'func('+Registry.AttrFunctions[RWord]+') ';
    rtkIdentifier: Result:=Result+'ident('+RStr(RDWord)+') ';
    rtkStringApos: Result:=Result+'apos('+RStr(RDWord)+') ';
    rtkStringQuote: Result:=Result+'quote('+RStr(RDWord)+') ';
    rtkHexColor: Result:=Result+'hex('+RStr(RByte)+') ';
    else
      Result:=Result+'?? ';
      break;
    end;
  end;
  Result:=TrimRight(Result);
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
  FCSSResolver.HasMediaBoolean:=@HasMediaBoolean;
  FCSSResolver.IsMediaPlain:=@IsMediaPlain;
  FCSSResolver.MediaCompare:=@MediaCompare;

  FWidth:=800;
  FHeight:=600;
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
  FMediaEvalCount:=0;
  CSSResolver.Init;
  Traverse(Root);
end;

procedure TDemoDocument.OnResolverLog(Sender: TObject; Entry: TCSSResolverLogEntry);
begin
  if Sender=nil then ;
  if Entry=nil then ;
end;

function TDemoDocument.HasMediaBoolean(aResolver: TCSSBaseResolver; KW: TCSSNumericalID): boolean;
begin
  inc(FMediaEvalCount);
  Result:=false;
  case KW of
  TDemoCSSRegistry.kwHeight,
  TDemoCSSRegistry.kwScreen,
  TDemoCSSRegistry.kwOrientation,
  TDemoCSSRegistry.kwAspectRatio,
  TDemoCSSRegistry.kwWidth
    : Result:=true;
  end;
  if aResolver=nil then ;
end;

function TDemoDocument.IsMediaPlain(aResolver: TCSSBaseResolver; KW: TCSSNumericalID;
  const aValue: TCSSResCompValue): boolean;
var
  Cmp: integer;
begin
  Result:=false;

  case KW of
  TDemoCSSRegistry.kwAspectRatio,
  TDemoCSSRegistry.kwHeight,
  TDemoCSSRegistry.kwWidth:
    begin
      // length
      if not MediaCompare(aResolver,KW,aValue,Cmp) then exit;
      exit(Cmp=0);
    end;
  TDemoCSSRegistry.kwOrientation:
     if (aValue.Kind=rvkKeyword) and (aValue.KeywordID=TDemoCSSRegistry.kwPortrait) then
       Result:=true;
  else exit;
  end;
end;

function TDemoDocument.MediaCompare(aResolver: TCSSBaseResolver; KW: TCSSNumericalID;
  const aValue: TCSSResCompValue; out Cmp: integer): boolean;
// compare length
var
  LeftType: TDemoMediaRangeType;
  LeftValue, RightValue: double;
begin
  Result:=false;

  LeftType:=dmrtNone;
  LeftValue:=NaN;
  case KW of
  TDemoCSSRegistry.kwAspectRatio:
    begin
      LeftType:=dmrtRatio;
      LeftValue:=Width/Height;
    end;
  TDemoCSSRegistry.kwHeight:
    begin
      LeftType:=dmrtLength;
      LeftValue:=Height;
    end;
  TDemoCSSRegistry.kwWidth:
    begin
      LeftType:=dmrtLength;
      LeftValue:=Width;
    end;
  else exit;
  end;

  case LeftType of
  dmrtLength:
    case aValue.Kind of
    rvkFloat:
      case aValue.FloatUnit of
      cu_px: RightValue:=aValue.Float;
      else exit;
      end;
    rvkKeyword:
      case aValue.KeywordID of
      TDemoCSSRegistry.kwAspectRatio: RightValue:=Width/Height;
      TDemoCSSRegistry.kwHeight: RightValue:=Height;
      TDemoCSSRegistry.kwWidth: RightValue:=Width;
      else exit;
      end;
    end;
  dmrtRatio:
    case aValue.Kind of
    rvkFloat:
      case aValue.FloatUnit of
      cuNone: RightValue:=aValue.Float;
      else exit;
      end;
    else exit;
    end;
  else exit;
  end;

  Result:=true;

  if SameValue(LeftValue,RightValue) then
    Cmp:=0
  else if LeftValue>RightValue then
    Cmp:=1
  else
    Cmp:=-1;

  if aResolver=nil then ;
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
    case Resolver.TokenKind of
    rtkFloat:
      if not HasWidth then
        HasWidth:=Resolver.FloatUnit in ([cuNONE,cuPERCENT]+cuAllLengths);
    rtkKeyword:
      if not HasColor then
        HasColor:=(Resolver.KeywordID>=kwFirstColor) and (Resolver.KeywordID<=kwLastColor);
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
  var AttrIDs: TCSSNumericalIDArray; var Values: TBytesArray);
var
  aWidth, aColor: TBytes;
begin
  aWidth:=nil;
  aColor:=nil;
  repeat
    case Resolver.TokenKind of
    rtkFloat:
      if aWidth=nil then begin
        if Resolver.FloatUnit in ([cuNONE,cuPERCENT]+cuAllLengths) then
          aWidth:=Resolver.GetCompTokens;
      end;
    rtkKeyword:
      if aColor=nil then
      begin
        if (Resolver.KeywordID>=kwFirstColor) and (Resolver.KeywordID<=kwLastColor) then
          aColor:=KeywordTokens[Resolver.KeywordID];
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
  Value.Invalid:=not Resolver.ReadAttribute_Keyword(Invalid,Chk_DirectionAllowedKeywordIDs);
  if Node=nil then ;
end;

procedure TDemoCSSRegistry.OnCompute_LeftTop(Resolver: TCSSResolver;
  Node: TDemoNode; Value: TCSSAttributeValue);
var
  Invalid: boolean;
begin
  Value.Invalid:=not Resolver.ReadAttribute_Dimension(Invalid,Chk_LeftTop);
  if Node=nil then ;
end;

procedure TDemoCSSRegistry.OnCompute_WidthHeight(Resolver: TCSSResolver;
  Node: TDemoNode; Value: TCSSAttributeValue);
var
  Invalid: boolean;
begin
  Value.Invalid:=not Resolver.ReadAttribute_Dimension(Invalid,Chk_WidthHeight);
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

  if AddKeyword('red')<>kwRed then
    raise Exception.Create('20260322081212');
  kwFirstColor:=kwRed;
  AddKeyword('green');
  AddKeyword('blue');
  AddKeyword('white');
  if AddKeyword('black')<>kwBlack then
    raise Exception.Create('20260322081247');
  kwLastColor:=kwBlack;

  AddKeyword('block');
  AddKeyword('inline-block');

  AddKeyword('ltr');
  AddKeyword('rtl');
  AddKeyword('screen');
  AddKeyword('orientation');
  AddKeyword('portrait');
  AddKeyword('aspect-ratio');
  AddKeyword('width');
  if AddKeyword('height')<>kwHeight then
    raise Exception.Create('20260322081506');

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
    Result:=FResolver.Detokenize(Values.Values[i].Tokens)
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
  FCSSClasses.FPOAttachObserver(Self);
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
  SiblingMatches: TCSSSiblingMatchList;
begin
  FResolver:=Resolver;

  if (InlineStyleElement=nil) and (InlineStyle<>'') then
    InlineStyleElement:=Resolver.ParseInlineStyle(InlineStyle) as TCSSRuleElement;

  Resolver.Compute(Self,InlineStyleElement,Rules,Values,SiblingMatches);

  {$IFDEF VerboseCSSResolver}
  writeln('TDemoNode.ApplyCSS ',Name,' length(Values)=',length(Values.Values),' All="',CSSRegistry.Keywords[Values.AllValue],'"');
  for i:=0 to length(Values.Values)-1 do begin
    AttrID:=Values.Values[i].AttrID;
    Desc:=Resolver.GetAttributeDesc(AttrID);
    writeln('TDemoNode.ApplyCSS ',Name,' resolved ',Desc.Name,'/',AttrID,':="',Resolver.Detokenize(Values.Values[i].Tokens),'"');
  end;
  {$ENDIF}
  // compute values
  for i:=0 to length(Values.Values)-1 do
  begin
    CurValue:=Values.Values[i];
    if CurValue.Invalid then continue;
    AttrID:=CurValue.AttrID;
    Desc:=Resolver.GetAttributeDesc(AttrID);
    if Desc=nil then
      raise Exception.Create('20240823100115 AttrID='+IntToStr(AttrID));
    if Desc is TDemoCSSAttributeDesc then
    begin
      AttrDesc:=TDemoCSSAttributeDesc(Desc);
      if AttrDesc.OnCompute<>nil then
      begin
        Resolver.InitParseAttr(AttrDesc,CurValue.Tokens);
        AttrDesc.OnCompute(Resolver,Self,CurValue);
        {$IFDEF VerboseCSSResolver}
        writeln('TDemoNode.ApplyCSS ',Name,' computed ',CSSRegistry.Attributes[AttrID].Name,'/',AttrID,':="',Resolver.Detokenize(CurValue.Tokens),'"');
        {$ENDIF}
      end;
    end;
  end;
end;

function TDemoNode.GetCSSID: TCSSNumericalID;
begin
  // map the node name to the resolver's id index (the same index the #name
  // selectors were registered under); CSSIDNone if the name is not used by any selector
  if FResolver=nil then
    exit(CSSIDNone);
  Result:=FResolver.GetCSSIDIndex(Name);
end;

class function TDemoNode.CSSTypeName: TCSSString;
begin
  Result:=DemoElementTypeNames[detNode];
end;

procedure TDemoNode.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender=nil then ;
  if Data=nil then ;
  if Operation in [ooChange,ooAddItem,ooDeleteItem,ooFree] then
    FCSSClassArrValid:=false;
end;

function TDemoNode.HasCSSClass(const aClassID: TCSSNumericalID): boolean;
var
  i: Integer;
  Classes: TCSSNumericalIDArray;
begin
  if aClassID=CSSIDNone then exit(false);
  Classes:=GetCSSClasses;
  for i:=0 to length(Classes)-1 do
    if aClassID=Classes[i] then
      exit(true);
  Result:=false;
end;

function TDemoNode.GetCSSClasses: TCSSNumericalIDArray;
var
  i: Integer;
begin
  // rebuild the cache if the class names changed or the resolver remapped class ids
  if (not FCSSClassArrValid) or (FCSSClassArrStamp<>FResolver.CSSClassIDStamp) then
  begin
    // map the class names to their resolver ids
    SetLength(FCSSClassArr,FCSSClasses.Count);
    for i:=0 to FCSSClasses.Count-1 do
      FCSSClassArr[i]:=FResolver.GetCSSClassID(FCSSClasses[i]);
    FCSSClassArrValid:=true;
    FCSSClassArrStamp:=FResolver.CSSClassIDStamp;
  end;
  Result:=FCSSClassArr;
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

function TDemoNode.GetCSSAttributeID: TCSSString;
begin
  // the demo node uses its component name as the 'id' attribute
  Result:=Name;
end;

function TDemoNode.GetCSSCustomAttribute(const AttrID: TCSSNumericalID): TBytes;
var
  i: Integer;
  El: TDemoNode;
begin
  Result:=nil;
  El:=Self;
  repeat
    if El.Values<>nil then
    begin
      i:=El.Values.IndexOf(AttrID);
      if i>=0 then
      begin
        Result:=El.Values.Values[i].Tokens;
        if length(Result)>0 then exit;
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

function TDemoPseudoElement.HasCSSClass(const aClassID: TCSSNumericalID): boolean;
begin
  Result:=false;
  if aClassID=CSSIDNone then ;
end;

function TDemoPseudoElement.GetCSSClasses: TCSSNumericalIDArray;
begin
  Result:=nil;
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

{ TCustomTestCSSResolver }

procedure TCustomTestCSSResolver.SetUp;
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

procedure TCustomTestCSSResolver.TearDown;
begin
  FreeAndNil(FDoc);
  FreeAndNil(TDemoNode.CSSRegistry);
  inherited TearDown;
end;

procedure TCustomTestCSSResolver.ApplyStyle;
begin
  Doc.ApplyStyle;
  CheckWarnings;
end;

procedure TCustomTestCSSResolver.CheckWarnings;
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

function TCustomTestCSSResolver.AddButton(const aName: string; aParent: TDemoNode): TDemoButton;
begin
  Result:=TDemoButton.Create(nil);
  Result.Name:=aName;
  Result.Parent:=aParent;
end;

function TCustomTestCSSResolver.AddDiv(const aName: string; aParent: TDemoNode): TDemoDiv;
begin
  Result:=TDemoDiv.Create(nil);
  Result.Name:=aName;
  Result.Parent:=aParent;
end;

function TCustomTestCSSResolver.AddSpan(const aName: string; aParent: TDemoNode): TDemoSpan;
begin
  Result:=TDemoSpan.Create(nil);
  Result.Name:=aName;
  Result.Parent:=aParent;
end;

function TCustomTestCSSResolver.AddSpan_Class(const aName, aClass: string; aParent: TDemoNode
  ): TDemoSpan;
begin
  Result:=AddSpan(aName,aParent);
  Result.CSSClasses.Add(aClass);
end;

{ TClassNameCollector }

type
  TClassNameCollector = class(TCSSTreeVisitor)
  public
    Items: array of TCSSResolvedClassNameElement;
    Count: integer;
    procedure Visit(obj: TCSSElement); override;
  end;

procedure TClassNameCollector.Visit(obj: TCSSElement);
begin
  if obj is TCSSResolvedClassNameElement then
  begin
    if Count=length(Items) then
      SetLength(Items,Count*2+8);
    Items[Count]:=TCSSResolvedClassNameElement(obj);
    inc(Count);
  end;
end;

{ THashIdentifierCollector }

type
  THashIdentifierCollector = class(TCSSTreeVisitor)
  public
    Items: array of TCSSResolvedHashIdentifierElement;
    Count: integer;
    procedure Visit(obj: TCSSElement); override;
  end;

procedure THashIdentifierCollector.Visit(obj: TCSSElement);
begin
  if obj is TCSSResolvedHashIdentifierElement then
  begin
    if Count=length(Items) then
      SetLength(Items,Count*2+8);
    Items[Count]:=TCSSResolvedHashIdentifierElement(obj);
    inc(Count);
  end;
end;

{ TTestCSSResolver }

procedure TTestCSSResolver.TestRes_ParseAttr_Keyword;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:='* { direction: ltr; }';
  ApplyStyle;
  AssertEquals('Root.direction','ltr',Doc.Root.Direction);
end;

procedure TTestCSSResolver.TestRes_ParseAttr_Float;
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
  Div1:=AddDiv('Div1',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_Universal;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Style:='* { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','10px',Doc.Root.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_Type;
var
  Button: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button:=AddButton('Button',Doc.Root);
  Doc.Style:='button { left: 11px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button.left','11px',Button.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_Type_Spaces;
var
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Button2:=AddButton('Button2',Doc.Root);

  Doc.Style:='div, button ,span { left: 11px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','11px',Button1.Left);
  AssertEquals('Button2.left','11px',Button2.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_Id;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Doc.Style:='#Button1 { left: 12px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','12px',Button1.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_Class;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('west');
  Doc.Style:='.west { left: 13px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','13px',Button1.Left);
end;

procedure TTestCSSResolver.TestRes_CSSClassID_Numbering;
var
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  AddButton('Button1',Doc.Root);
  // first occurrence in selector order determines the id, beginning at 1
  Doc.Style:='.alpha { left: 1px; }'
            +'.beta { left: 2px; }'
            +'.alpha.gamma { left: 3px; }'; // alpha repeats, gamma is new
  ApplyStyle;
  R:=Doc.CSSResolver;
  AssertEquals('CSSClassNameCount',3,R.CSSClassNameCount);
  AssertEquals('id alpha',1,R.GetCSSClassID('alpha'));
  AssertEquals('id beta',2,R.GetCSSClassID('beta'));
  AssertEquals('id gamma',3,R.GetCSSClassID('gamma'));
  // stable on re-query
  AssertEquals('id alpha again',1,R.GetCSSClassID('alpha'));
  // reverse lookup
  AssertEquals('name 1','alpha',R.GetCSSClassName(1));
  AssertEquals('name 2','beta',R.GetCSSClassName(2));
  AssertEquals('name 3','gamma',R.GetCSSClassName(3));
end;

procedure TTestCSSResolver.TestRes_CSSClassID_Unknown;
var
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  AddButton('Button1',Doc.Root);
  Doc.Style:='.west { left: 1px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  // lookup-only: a class never seen in any selector returns CSSIDNone
  AssertEquals('unknown class',CSSIDNone,R.GetCSSClassID('east'));
  AssertEquals('empty name',CSSIDNone,R.GetCSSClassID(''));
  // querying did not register anything
  AssertEquals('CSSClassNameCount',1,R.CSSClassNameCount);
  AssertEquals('name of unknown id','',R.GetCSSClassName(99));
end;

procedure TTestCSSResolver.TestRes_CSSClassID_CaseSensitive;
var
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  AddButton('Button1',Doc.Root);
  // css class names are case sensitive -> two distinct ids
  Doc.Style:='.Foo { left: 1px; }'
            +'.foo { left: 2px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  AssertEquals('CSSClassNameCount',2,R.CSSClassNameCount);
  AssertEquals('id Foo',1,R.GetCSSClassID('Foo'));
  AssertEquals('id foo',2,R.GetCSSClassID('foo'));
  AssertTrue('distinct ids',R.GetCSSClassID('Foo')<>R.GetCSSClassID('foo'));
end;

procedure TTestCSSResolver.TestRes_CSSClassID_ResolvedElement;
var
  R: TCSSResolver;
  Visitor: TClassNameCollector;
  i: Integer;
  El: TCSSResolvedClassNameElement;
begin
  Doc.Root:=TDemoNode.Create(nil);
  AddButton('Button1',Doc.Root);
  Doc.Style:='.west { left: 1px; }'
            +'.east { left: 2px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;

  Visitor:=TClassNameCollector.Create;
  try
    for i:=0 to R.StyleSheetCount-1 do
      if R.StyleSheets[i].Element<>nil then
        R.StyleSheets[i].Element.Iterate(Visitor);
    // both class selectors got resolved into TCSSResolvedClassNameElement
    AssertEquals('collected class elements',2,Visitor.Count);
    for i:=0 to Visitor.Count-1 do
    begin
      El:=Visitor.Items[i];
      AssertEquals('NumericalID matches GetCSSClassID for '+El.Name,
        R.GetCSSClassID(El.Name),El.NumericalID);
      AssertTrue('NumericalID>=1 for '+El.Name,El.NumericalID>=1);
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestCSSResolver.TestRes_CSSClassID_AddClassToStylesheet;
var
  Button1: TDemoButton;
  R: TCSSResolver;
  OldStamp: TCSSNumericalID;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('west');
  // the stylesheet does not yet contain a .west rule
  Doc.Style:='.east { left: 1px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  // class 'west' is not used by any selector -> not registered yet
  AssertEquals('west unknown before',CSSIDNone,R.GetCSSClassID('west'));
  AssertEquals('Button1.left before','',Button1.Left);

  // add a .west rule: a new class id is registered and the stamp must change
  OldStamp:=R.CSSClassIDStamp;
  Doc.Style:='.east { left: 1px; }.west { left: 22px; }';
  ApplyStyle;
  AssertTrue('CSSClassIDStamp changed after add',R.CSSClassIDStamp<>OldStamp);
  AssertTrue('west registered after add',R.GetCSSClassID('west')>=1);
  // the node noticed the changed stamp and rebuilt its cached class id array,
  // so the freshly added rule now matches
  AssertEquals('Button1.left after','22px',Button1.Left);
end;

procedure TTestCSSResolver.TestRes_CSSClassID_DeleteClassFromStylesheet;
var
  Button1: TDemoButton;
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('west');
  Doc.Style:='.west { left: 22px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  AssertTrue('west registered before',R.GetCSSClassID('west')>=1);
  AssertEquals('Button1.left before','22px',Button1.Left);

  // delete the .west rule from the stylesheet
  Doc.Style:='.east { left: 1px; }';
  ApplyStyle;
  // the rule is gone -> it is no longer applied to the node
  AssertEquals('Button1.left after','',Button1.Left);
end;

procedure TTestCSSResolver.TestRes_CSSClassID_ChangeNodeClass;
var
  Node: TDemoButton;
  R: TCSSResolver;
  RedID: TCSSNumericalID;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Node:=AddButton('Button1',Doc.Root);
  // the stylesheet stays the same; only the node's class list changes
  Doc.Style:='.red { left: 1px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  RedID:=R.GetCSSClassID('red');
  // the node has no class yet -> the .red rule does not match
  AssertFalse('HasCSSClass red before',Node.HasCSSClass(RedID));
  AssertEquals('Button1.left before','',Node.Left);

  // add the class: the IFPObserver invalidates the cached class id array, so
  // GetCSSClasses rebuilds and the .red rule now matches
  Node.CSSClasses.Add('red');
  ApplyStyle;
  AssertTrue('HasCSSClass red after add',Node.HasCSSClass(RedID));
  AssertEquals('Button1.left after add','1px',Node.Left);

  // delete the class again: the rule no longer matches
  Node.CSSClasses.Clear;
  ApplyStyle;
  AssertFalse('HasCSSClass red after delete',Node.HasCSSClass(RedID));
  AssertEquals('Button1.left after delete','',Node.Left);
end;

procedure TTestCSSResolver.TestRes_CSSID_Numbering;
var
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  AddButton('Button1',Doc.Root);
  // first occurrence in selector order determines the id index, beginning at 1
  Doc.Style:='#alpha { left: 1px; }'
            +'#beta { left: 2px; }'
            +'#alpha#gamma { left: 3px; }'; // alpha repeats, gamma is new
  ApplyStyle;
  R:=Doc.CSSResolver;
  AssertEquals('CSSIDCount',3,R.CSSIDCount);
  AssertEquals('index alpha',1,R.GetCSSIDIndex('alpha'));
  AssertEquals('index beta',2,R.GetCSSIDIndex('beta'));
  AssertEquals('index gamma',3,R.GetCSSIDIndex('gamma'));
  // stable on re-query
  AssertEquals('index alpha again',1,R.GetCSSIDIndex('alpha'));
  // reverse lookup
  AssertEquals('name 1','alpha',R.GetCSSIDName(1));
  AssertEquals('name 2','beta',R.GetCSSIDName(2));
  AssertEquals('name 3','gamma',R.GetCSSIDName(3));
end;

procedure TTestCSSResolver.TestRes_CSSID_Unknown;
var
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  AddButton('Button1',Doc.Root);
  Doc.Style:='#west { left: 1px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  // lookup-only: an id never seen in any selector returns CSSIDNone
  AssertEquals('unknown id',CSSIDNone,R.GetCSSIDIndex('east'));
  AssertEquals('empty name',CSSIDNone,R.GetCSSIDIndex(''));
  // querying did not register anything
  AssertEquals('CSSIDCount',1,R.CSSIDCount);
  AssertEquals('name of unknown index','',R.GetCSSIDName(99));
end;

procedure TTestCSSResolver.TestRes_CSSID_CaseSensitive;
var
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  AddButton('Button1',Doc.Root);
  // css ids are case sensitive -> two distinct indices
  Doc.Style:='#Foo { left: 1px; }'
            +'#foo { left: 2px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  AssertEquals('CSSIDCount',2,R.CSSIDCount);
  AssertEquals('index Foo',1,R.GetCSSIDIndex('Foo'));
  AssertEquals('index foo',2,R.GetCSSIDIndex('foo'));
  AssertTrue('distinct indices',R.GetCSSIDIndex('Foo')<>R.GetCSSIDIndex('foo'));
end;

procedure TTestCSSResolver.TestRes_CSSID_ResolvedElement;
var
  R: TCSSResolver;
  Visitor: THashIdentifierCollector;
  i: Integer;
  El: TCSSResolvedHashIdentifierElement;
begin
  Doc.Root:=TDemoNode.Create(nil);
  AddButton('Button1',Doc.Root);
  Doc.Style:='#west { left: 1px; }'
            +'#east { left: 2px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;

  Visitor:=THashIdentifierCollector.Create;
  try
    for i:=0 to R.StyleSheetCount-1 do
      if R.StyleSheets[i].Element<>nil then
        R.StyleSheets[i].Element.Iterate(Visitor);
    // both id selectors got resolved into TCSSResolvedHashIdentifierElement
    AssertEquals('collected id elements',2,Visitor.Count);
    for i:=0 to Visitor.Count-1 do
    begin
      El:=Visitor.Items[i];
      AssertEquals('NumericalID matches GetCSSIDIndex for '+El.Value,
        R.GetCSSIDIndex(El.Value),El.NumericalID);
      AssertTrue('NumericalID>=1 for '+El.Value,El.NumericalID>=1);
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestCSSResolver.TestRes_CSSID_AddIdToStylesheet;
var
  Button1: TDemoButton;
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  // the stylesheet does not yet contain a #Button1 rule
  Doc.Style:='#Other { left: 1px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  // id 'Button1' is not used by any selector -> not registered yet
  AssertEquals('Button1 unknown before',CSSIDNone,R.GetCSSIDIndex('Button1'));
  AssertEquals('Button1.left before','',Button1.Left);

  // add a #Button1 rule: a new id index is registered
  Doc.Style:='#Other { left: 1px; }#Button1 { left: 22px; }';
  ApplyStyle;
  AssertTrue('Button1 registered after add',R.GetCSSIDIndex('Button1')>=1);
  // the node maps its name to the freshly added id index, so the rule now matches
  AssertEquals('Button1.left after','22px',Button1.Left);
end;

procedure TTestCSSResolver.TestRes_CSSID_DeleteIdFromStylesheet;
var
  Button1: TDemoButton;
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Doc.Style:='#Button1 { left: 22px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  AssertTrue('Button1 registered before',R.GetCSSIDIndex('Button1')>=1);
  AssertEquals('Button1.left before','22px',Button1.Left);

  // delete the #Button1 rule from the stylesheet
  Doc.Style:='#Other { left: 1px; }';
  ApplyStyle;
  // the rule is gone -> it is no longer applied to the node
  AssertEquals('Button1.left after','',Button1.Left);
end;

procedure TTestCSSResolver.TestRes_CSSID_ChangeNodeId;
var
  Node: TDemoButton;
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Node:=AddButton('Button1',Doc.Root);
  // the stylesheet stays the same; only the node's id (name) changes
  Doc.Style:='#Button1 { left: 1px; }'
            +'#Other { left: 2px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  // initially the node is #Button1 -> the first rule matches
  AssertEquals('node id is #Button1',R.GetCSSIDIndex('Button1'),Node.GetCSSID);
  AssertEquals('Button1.left before','1px',Node.Left);

  // rename the node to 'Other': its id index now resolves to #Other
  Node.Name:='Other';
  ApplyStyle;
  AssertEquals('node id is #Other',R.GetCSSIDIndex('Other'),Node.GetCSSID);
  AssertTrue('node id changed',Node.GetCSSID<>R.GetCSSIDIndex('Button1'));
  // the freshly matched #Other rule applies, the old #Button1 rule no longer does
  AssertEquals('Other.left after','2px',Node.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_ClassClass;
var
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('west');

  Button2:=AddButton('Button2',Doc.Root);
  Button2.CSSClasses.DelimitedText:='west south';
  AssertEquals('Button2.CSSClasses.Count',2,Button2.CSSClasses.Count);

  Doc.Style:='.west.south { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_ClassSpaceClass;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.CSSClasses.Add('bird');

  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('west');

  Doc.Style:='.bird .west { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','10px',Button1.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_TypeCommaType;
var
  Button1: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=AddButton('',Doc.Root);

  Div1:=AddDiv('',Doc.Root);

  Doc.Style:='div, button { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','10px',Button1.Left);
  AssertEquals('Div1.left','10px',Div1.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_ClassGTClass;
var
  Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.CSSClasses.Add('lvl1');

  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('lvl2');

  Div2:=AddDiv('Div2',Div1);
  Div2.CSSClasses.Add('lvl3');

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

procedure TTestCSSResolver.TestRes_Selector_TypePlusType;
var
  Button1, Button2, Button3: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Button1:=AddButton('Button1',Doc.Root);

  Div1:=AddDiv('Div1',Doc.Root);

  Button2:=AddButton('Button2',Doc.Root);

  Button3:=AddButton('Button3',Doc.Root);

  Doc.Style:='div+button { left: 10px; }'; // only Button2 has a prev sibling div
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Div1.left','',Div1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
  AssertEquals('Button3.left','',Button3.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_TypeTildeType;
var
  Button1, Button2, Button3: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Button1:=AddButton('',Doc.Root);

  Div1:=AddDiv('',Doc.Root);

  Button2:=AddButton('',Doc.Root);

  Button3:=AddButton('',Doc.Root);

  Doc.Style:='div~button { left: 10px; }';
  ApplyStyle;
  AssertEquals('Root.left','',Doc.Root.Left);
  AssertEquals('Button1.left','',Button1.Left);
  AssertEquals('Div1.left','',Div1.Left);
  AssertEquals('Button2.left','10px',Button2.Left);
  AssertEquals('Button3.left','10px',Button3.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_HasAttribute;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.ExplicitAttributes[naLeft]:='100px';

  Button1:=AddButton('Button1',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_AttributeEquals;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='2px';

  Button1:=AddButton('',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_AttributeEqualsI;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='2px';

  Button1:=AddButton('',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_AttributeBeginsWith;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=AddButton('',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_AttributeEndsWith;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=AddButton('',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_AttributeBeginsWithHyphen;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=AddButton('',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_AttributeContainsWord;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.ExplicitAttributes[naLeft]:='One Two Three';

  Button1:=AddButton('Button1',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_AttributeContainsSubstring;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=AddButton('',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_Root;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.ExplicitAttributes[naLeft]:='Foo';

  Button1:=AddButton('',Doc.Root);

  Doc.Style:=LinesToStr([
  ':roOt { top: 4px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Top','4px',Doc.Root.Top);
  AssertEquals('Button1.Top','',Button1.Top);
end;

procedure TTestCSSResolver.TestRes_Selector_Empty;
var
  Div1, Div11, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div11:=AddDiv('',Div1);

  Div2:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_FirstChild;
var
  Div1, Div11, Div12, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div11:=AddDiv('',Div1);

  Div12:=AddDiv('',Div1);

  Div2:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_LastChild;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div11:=AddDiv('',Div1);

  Button12:=AddButton('',Div1);

  Div2:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_OnlyChild;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Div11:=AddDiv('Div11',Div1);

  Div2:=AddDiv('Div2',Doc.Root);

  Button12:=AddButton('Button12',Div2);

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

procedure TTestCSSResolver.TestRes_Selector_Not;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Div11:=AddDiv('Div11',Div1);

  Div2:=AddDiv('Div2',Doc.Root);

  Button12:=AddButton('Button12',Div2);

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

procedure TTestCSSResolver.TestRes_Selector_NthChild;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div2:=AddDiv('',Doc.Root);

  Div3:=AddDiv('',Doc.Root);

  Div4:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_NthChild2;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div2:=AddDiv('',Doc.Root);

  Div3:=AddDiv('',Doc.Root);

  Div4:=AddDiv('',Doc.Root);

  Doc.Style:=LinesToStr([
  'div:nth-child(2) { left: 8px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','',Div1.Left);
  AssertEquals('Div2.Left','8px',Div2.Left);
  AssertEquals('Div3.Left','',Div3.Left);
  AssertEquals('Div4.Left','',Div4.Left);
end;

procedure TTestCSSResolver.TestRes_Selector_NthLastChild;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div2:=AddDiv('',Doc.Root);

  Div3:=AddDiv('',Doc.Root);

  Div4:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_NthChildOf;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Div2:=AddDiv('Div2',Doc.Root);
  Div2.ExplicitAttributes[naTop]:='3px';

  Div3:=AddDiv('Div3',Doc.Root);
  Div3.ExplicitAttributes[naTop]:='3px';

  Div4:=AddDiv('Div4',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_FirstOfType;
var
  Div1, Div11, Div13, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div11:=AddDiv('',Div1);

  Button12:=AddButton('',Div1);

  Div13:=AddDiv('',Div1);

  Div2:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_LastOfType;
var
  Div1, Div11, Div13, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div11:=AddDiv('',Div1);

  Button12:=AddButton('',Div1);

  Div13:=AddDiv('',Div1);

  Div2:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_OnlyOfType;
var
  Div1, Div11, Div2: TDemoDiv;
  Button12: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Div11:=AddDiv('',Div1);

  Button12:=AddButton('',Div1);

  Div2:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_NthOfType;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Button1:=AddButton('Button1',Doc.Root);

  Div2:=AddDiv('Div2',Doc.Root);

  Div3:=AddDiv('Div3',Doc.Root);

  Button2:=AddButton('Button2',Doc.Root);

  Div4:=AddDiv('Div4',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_NthLastOfType;
var
  Div1, Div2, Div3, Div4: TDemoDiv;
  Button1, Button2: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);

  Button1:=AddButton('',Doc.Root);

  Div2:=AddDiv('',Doc.Root);

  Div3:=AddDiv('',Doc.Root);

  Button2:=AddButton('',Doc.Root);

  Div4:=AddDiv('',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Selector_Is;
var
  Div1, Div2: TDemoDiv;
  Button1, Button2: TDemoButton;
  Span1: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);
  Div1.ExplicitAttributes[naTop]:='3px';

  Button1:=AddButton('Button1',Doc.Root);

  Div2:=AddDiv('',Doc.Root);

  Span1:=AddSpan('',Doc.Root);
  Span1.ExplicitAttributes[naTop]:='3px';

  Button2:=AddButton('',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Selector_Where;
var
  Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);
  Div1.ExplicitAttributes[naTop]:='3px';

  Div2:=AddDiv('Div2',Div1);
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

procedure TTestCSSResolver.TestRes_Selector_Hover;
var
  Div1, Div11: TDemoDiv;
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);
  Div1.Hover:=true;

  Button1:=AddButton('Button1',Div1);
  Button1.Hover:=true;

  Div11:=AddDiv('Div11',Div1);

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

procedure TTestCSSResolver.TestRes_InlineStyle;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);
  Div1.InlineStyle:='left: 10px; top: 5px';

  Doc.Style:=LinesToStr([
  'div { left: 6px; }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Left','',Doc.Root.Left);
  AssertEquals('Div1.Left','10px',Div1.Left);
  AssertEquals('Div1.Top','5px',Div1.Top);
end;

procedure TTestCSSResolver.TestRes_InlineStyle_DisplayNone;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('',Doc.Root);
  Div1.InlineStyle:='display:none';

  ApplyStyle;
  AssertEquals('Div1.Display','none',Div1.Display);
end;

procedure TTestCSSResolver.TestRes_Specificity_Id_Class;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Specificity_Important;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Specificity_Shorthand_OneRule;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('bird');

  Doc.Style:='.bird { border-color: blue; border: 6px red; border-width: 7px; }';
  ApplyStyle;
  AssertEquals('Div1.BorderColor','red',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','7px',Div1.BorderWidth);
end;

procedure TTestCSSResolver.TestRes_Specificity_Shorthand_ClassClass;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Specificity_Longhand_All_Longhand;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Specificity_Shorthand_All_Shorthand;
var
  Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('bird');

  Div2:=AddDiv('Div2',Doc.Root);
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

function TTestCSSResolver.FindAuthorDecl(const aSelector, aProp: string
  ): TCSSDeclarationElement;
var
  R: TCSSResolver;
  ShIdx, i, j: Integer;
  Rules: TCSSRuleElementArray;
  Rule: TCSSRuleElement;
  Decl: TCSSDeclarationElement;
begin
  Result:=nil;
  R:=Doc.CSSResolver;
  ShIdx:=R.IndexOfStyleSheetWithName(cssoAuthor,'test.css');
  if ShIdx<0 then exit;
  Rules:=CSSGetTopLevelRules(R.StyleSheets[ShIdx].Element);
  for i:=0 to length(Rules)-1 do
  begin
    Rule:=Rules[i];
    if CSSRuleSelectorsStr(Rule)<>aSelector then continue;
    for j:=0 to Rule.ChildCount-1 do
      if Rule.Children[j] is TCSSDeclarationElement then
      begin
        Decl:=TCSSDeclarationElement(Rule.Children[j]);
        if CSSDeclPropertyName(Decl)=aProp then
          exit(Decl);
      end;
  end;
end;

procedure TTestCSSResolver.TestRes_Disable_Longhand;
var
  Div1: TDemoDiv;
  Decl: TCSSDeclarationElement;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('bird');
  Doc.Style:=LinesToStr(['.bird { left: 10px; }','']);
  ApplyStyle;
  AssertEquals('Div1.Left initial','10px',Div1.Left);

  Decl:=FindAuthorDecl('.bird','left');
  AssertTrue('found .bird left decl',Decl<>nil);
  Doc.CSSResolver.DisableDeclaration(Decl);
  ApplyStyle;
  AssertEquals('Div1.Left disabled','',Div1.Left);

  Doc.CSSResolver.EnableDeclaration(Decl);
  ApplyStyle;
  AssertEquals('Div1.Left enabled','10px',Div1.Left);
end;

procedure TTestCSSResolver.TestRes_Disable_Shorthand;
var
  Div1: TDemoDiv;
  Decl: TCSSDeclarationElement;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('bird');
  Doc.Style:=LinesToStr(['.bird { border: 7px blue; }','']);
  ApplyStyle;
  AssertEquals('Div1.BorderWidth initial','7px',Div1.BorderWidth);
  AssertEquals('Div1.BorderColor initial','blue',Div1.BorderColor);

  Decl:=FindAuthorDecl('.bird','border');
  AssertTrue('found .bird border decl',Decl<>nil);
  Doc.CSSResolver.DisableDeclaration(Decl);
  ApplyStyle;
  AssertEquals('Div1.BorderWidth disabled','',Div1.BorderWidth);
  AssertEquals('Div1.BorderColor disabled','',Div1.BorderColor);

  Doc.CSSResolver.EnableDeclaration(Decl);
  ApplyStyle;
  AssertEquals('Div1.BorderWidth enabled','7px',Div1.BorderWidth);
  AssertEquals('Div1.BorderColor enabled','blue',Div1.BorderColor);
end;

procedure TTestCSSResolver.TestRes_Disable_All;
var
  Div1: TDemoDiv;
  Decl: TCSSDeclarationElement;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('bird');
  Doc.Style:=LinesToStr([
    '.bird { border-width: 7px; }',
    '.bird { all: initial; }',
    '']);
  ApplyStyle;
  // 'all: initial' resets border-width (an All attribute)
  AssertEquals('Div1.BorderWidth initial','',Div1.BorderWidth);

  Decl:=FindAuthorDecl('.bird','all');
  AssertTrue('found .bird all decl',Decl<>nil);
  Doc.CSSResolver.DisableDeclaration(Decl);
  ApplyStyle;
  AssertEquals('Div1.BorderWidth all disabled','7px',Div1.BorderWidth);

  Doc.CSSResolver.EnableDeclaration(Decl);
  ApplyStyle;
  AssertEquals('Div1.BorderWidth all enabled','',Div1.BorderWidth);
end;

procedure TTestCSSResolver.TestRes_Disable_Restore_AfterReparse;
var
  Div1: TDemoDiv;
  Decl: TCSSDeclarationElement;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('bird');
  Doc.Style:=LinesToStr(['.bird { left: 10px; top: 5px; }','']);
  ApplyStyle;
  AssertEquals('Div1.Left initial','10px',Div1.Left);

  Decl:=FindAuthorDecl('.bird','left');
  AssertTrue('found .bird left decl',Decl<>nil);
  Doc.CSSResolver.DisableDeclaration(Decl);
  ApplyStyle;
  AssertEquals('Div1.Left disabled','',Div1.Left);

  // change the source -> reparse replaces all declaration elements;
  // the disabled state must be restored from the path map
  Doc.Style:=LinesToStr(['.bird { left: 10px; top: 8px; }','']);
  ApplyStyle;
  AssertEquals('Div1.Left still disabled after reparse','',Div1.Left);
  AssertEquals('Div1.Top after reparse','8px',Div1.Top);

  // re-enable on the reparsed tree
  Decl:=FindAuthorDecl('.bird','left');
  AssertTrue('found reparsed .bird left decl',Decl<>nil);
  Doc.CSSResolver.EnableDeclaration(Decl);
  ApplyStyle;
  AssertEquals('Div1.Left enabled','10px',Div1.Left);
end;

procedure TTestCSSResolver.TestRes_Disable_QueryMethods;
var
  Div1: TDemoDiv;
  DeclLeft, DeclTop: TCSSDeclarationElement;
  R: TCSSResolver;
  List: TFPList;
  Paths: TStrings;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('bird');
  Doc.Style:=LinesToStr(['.bird { left: 10px; top: 5px; }','']);
  ApplyStyle;
  R:=Doc.CSSResolver;

  DeclLeft:=FindAuthorDecl('.bird','left');
  DeclTop:=FindAuthorDecl('.bird','top');
  AssertTrue('found left',DeclLeft<>nil);
  AssertTrue('found top',DeclTop<>nil);

  // nothing disabled yet
  AssertFalse('left not disabled initially',R.IsDeclarationDisabled(DeclLeft));
  List:=R.GetDisabledDeclarations;
  try
    AssertEquals('disabled count before',0,List.Count);
  finally
    List.Free;
  end;

  // disable left
  R.DisableDeclaration(DeclLeft);
  AssertTrue('left disabled after DisableDeclaration',R.IsDeclarationDisabled(DeclLeft));
  AssertFalse('top still enabled',R.IsDeclarationDisabled(DeclTop));

  List:=R.GetDisabledDeclarations;
  try
    AssertEquals('disabled count',1,List.Count);
    AssertTrue('disabled element is left',TCSSDeclarationElement(List[0])=DeclLeft);
  finally
    List.Free;
  end;

  Paths:=R.GetDisabledDeclarationPaths;
  try
    AssertEquals('paths count',1,Paths.Count);
    AssertTrue('path object is left',TCSSDeclarationElement(Paths.Objects[0])=DeclLeft);
    AssertTrue('path string not empty',Paths[0]<>'');
  finally
    Paths.Free;
  end;

  // after a reparse the element is replaced; the query must return the NEW element
  Doc.Style:=LinesToStr(['.bird { left: 10px; top: 8px; }','']);
  ApplyStyle;
  DeclLeft:=FindAuthorDecl('.bird','left');
  AssertTrue('found reparsed left',DeclLeft<>nil);
  AssertTrue('left still disabled after reparse',R.IsDeclarationDisabled(DeclLeft));
  List:=R.GetDisabledDeclarations;
  try
    AssertEquals('disabled count after reparse',1,List.Count);
    AssertTrue('disabled element is reparsed left',TCSSDeclarationElement(List[0])=DeclLeft);
  finally
    List.Free;
  end;

  // enabling clears it
  R.EnableDeclaration(DeclLeft);
  AssertFalse('left enabled again',R.IsDeclarationDisabled(DeclLeft));
  List:=R.GetDisabledDeclarations;
  try
    AssertEquals('disabled count after enable',0,List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestCSSResolver.TestRes_Origin_Id_Class;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Origin_UserBeatsUserAgent;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  // same selector in user-agent and user origin: the user origin wins for left,
  // while top (only set by the user-agent sheet) still applies.
  Doc.CSSResolver.AddStyleSheet(cssoUserAgent,'ua.css','div { left: 1px; top: 7px; }');
  Doc.CSSResolver.AddStyleSheet(cssoUser,'user.css','div { left: 2px; }');
  ApplyStyle;
  AssertEquals('Div1.left (user beats user-agent)','2px',Div1.Left);
  AssertEquals('Div1.top (user-agent only)','7px',Div1.Top);
end;

procedure TTestCSSResolver.TestRes_Origin_AuthorBeatsUser;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  // same selector in user and author origin: the author origin wins for left,
  // while top (only set by the user sheet) still applies.
  Doc.CSSResolver.AddStyleSheet(cssoUser,'user.css','div { left: 1px; top: 5px; }');
  Doc.Style:='div { left: 2px; }'; // author
  ApplyStyle;
  AssertEquals('Div1.left (author beats user)','2px',Div1.Left);
  AssertEquals('Div1.top (user only)','5px',Div1.Top);
end;

procedure TTestCSSResolver.TestRes_Origin_AuthorBeatsUserAgentDespiteSpecificity;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  // origin beats selector specificity: although the user-agent #Div1 selector has
  // a higher selector specificity than the author div selector, the author origin
  // base specificity (3000) outweighs the user-agent one (1000).
  Doc.CSSResolver.AddStyleSheet(cssoUserAgent,'ua.css','#Div1 { left: 1px; }');
  Doc.Style:='div { left: 2px; }'; // author
  ApplyStyle;
  AssertEquals('Div1.left (author origin beats higher-specificity UA rule)','2px',Div1.Left);
end;

procedure TTestCSSResolver.TestRes_Origin_Important;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  // left: user-agent !important beats author normal (important outweighs any origin).
  // top: all three origins mark top !important, so the cascade falls back to source
  // position - the last declaration in document order (author) wins.
  Doc.CSSResolver.AddStyleSheet(cssoUserAgent,'ua.css',
    '#Div1 { left: 1px !important; top: 1px !important; }');
  Doc.CSSResolver.AddStyleSheet(cssoUser,'user.css','div { top: 2px !important; }');
  Doc.Style:='div { left: 9px; top: 3px !important; }'; // author
  ApplyStyle;
  AssertEquals('Div1.left (UA important beats author normal)','1px',Div1.Left);
  AssertEquals('Div1.top (author important wins among importants)','3px',Div1.Top);
end;

procedure TTestCSSResolver.TestRes_Origin_SourceOrderAcrossOrigins;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  // rules from every origin are gathered (via the rule hashmaps) and then sorted
  // back into document order. The author origin wins, and within the author origin
  // the later declaration wins (source position tie-break).
  Doc.CSSResolver.AddStyleSheet(cssoUserAgent,'ua.css','div { left: 1px; }');
  Doc.CSSResolver.AddStyleSheet(cssoUser,'user.css','div { left: 2px; }');
  Doc.Style:='div { left: 3px; }'+'div { left: 4px; }'; // two author rules
  ApplyStyle;
  AssertEquals('Div1.left (last author rule wins)','4px',Div1.Left);
  // matching rules: the auto user-agent type style (div{display}), the extra
  // user-agent rule, the user rule and both author rules = 5.
  AssertEquals('rule count across origins',5,length(Div1.Rules.Rules));
end;

procedure TTestCSSResolver.TestRes_InsertStyleSheet_HigherIndexWins;
var
  Div1: TDemoDiv;
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  R:=Doc.CSSResolver;
  // two author sheets, same selector: each sheet is its own anonymous layer within
  // the author origin, so the tie-break falls to document order = the array index.
  // The higher index (b.css) wins for left, while top (only in a.css) still applies.
  R.AddStyleSheet(cssoAuthor,'a.css','div { left: 1px; top: 5px; }');
  R.AddStyleSheet(cssoAuthor,'b.css','div { left: 2px; }');
  AssertEquals('StyleSheetCount',2,R.StyleSheetCount);
  ApplyStyle;
  AssertEquals('Div1.left (higher index b.css wins)','2px',Div1.Left);
  AssertEquals('Div1.top (only a.css sets it)','5px',Div1.Top);
end;

procedure TTestCSSResolver.TestRes_InsertStyleSheet_MidListOrdered;
var
  Div1: TDemoDiv;
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  R:=Doc.CSSResolver;
  R.AddStyleSheet(cssoAuthor,'a.css','div { left: 1px; top: 1px; }'); // index 0
  R.AddStyleSheet(cssoAuthor,'b.css','div { left: 2px; }');          // index 1
  // insert c.css between a.css and b.css: array order becomes a(0), c(1), b(2), and
  // OrderStyleSheetInLayer must move c's fresh layer into the matching slot so the
  // cascade document order follows the array. Then c beats a (lower index) but
  // loses to b (higher index).
  R.InsertStyleSheet(1,cssoAuthor,'c.css','div { left: 3px; top: 3px; }');
  AssertEquals('StyleSheetCount',3,R.StyleSheetCount);
  AssertEquals('sheet[0] is a.css','a.css',R.StyleSheets[0].Name);
  AssertEquals('sheet[1] is c.css','c.css',R.StyleSheets[1].Name);
  AssertEquals('sheet[2] is b.css','b.css',R.StyleSheets[2].Name);
  ApplyStyle;
  AssertEquals('Div1.left (b.css, highest index, wins)','2px',Div1.Left);
  AssertEquals('Div1.top (c.css beats a.css)','3px',Div1.Top);
end;

procedure TTestCSSResolver.TestRes_DeleteStyleSheet_LowerIndexWinsAfterDelete;
var
  Div1: TDemoDiv;
  R: TCSSResolver;
  CntBefore: integer;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  R:=Doc.CSSResolver;
  R.AddStyleSheet(cssoAuthor,'a.css','div { left: 1px; }'); // lower index
  R.AddStyleSheet(cssoAuthor,'b.css','div { left: 2px; }'); // higher index, wins
  // ApplyStyle also adds the user-agent type-style sheets and the author test.css,
  // so query the sheet by name rather than assuming an absolute index/count.
  ApplyStyle;
  AssertEquals('Div1.left before delete (b.css wins)','2px',Div1.Left);

  // delete the winning higher-index sheet: the lower-index a.css now wins
  CntBefore:=R.StyleSheetCount;
  R.DeleteStyleSheet(R.IndexOfStyleSheetWithName(cssoAuthor,'b.css'));
  AssertEquals('StyleSheetCount dropped by one',CntBefore-1,R.StyleSheetCount);
  AssertEquals('b.css is gone',-1,R.IndexOfStyleSheetWithName(cssoAuthor,'b.css'));
  ApplyStyle;
  AssertEquals('Div1.left after delete (a.css wins)','1px',Div1.Left);
end;

procedure TTestCSSResolver.TestRes_DeleteStyleSheet_RemovesContribution;
var
  Div1: TDemoDiv;
  R: TCSSResolver;
  CntBefore: integer;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  R:=Doc.CSSResolver;
  R.AddStyleSheet(cssoAuthor,'a.css','div { left: 1px; top: 1px; }'); // lower index
  R.AddStyleSheet(cssoAuthor,'b.css','div { left: 2px; }');          // higher index
  ApplyStyle;
  AssertEquals('Div1.left before delete','2px',Div1.Left);
  AssertEquals('Div1.top before delete','1px',Div1.Top);

  // delete lower-index a.css: its declarations no longer contribute, so top (only
  // a.css set it) becomes empty, while b.css still sets left.
  CntBefore:=R.StyleSheetCount;
  R.DeleteStyleSheet(R.IndexOfStyleSheetWithName(cssoAuthor,'a.css'));
  AssertEquals('StyleSheetCount dropped by one',CntBefore-1,R.StyleSheetCount);
  AssertEquals('a.css is gone',-1,R.IndexOfStyleSheetWithName(cssoAuthor,'a.css'));
  ApplyStyle;
  AssertEquals('Div1.left after delete (b.css remains)','2px',Div1.Left);
  AssertEquals('Div1.top after delete (a.css gone)','',Div1.Top);
end;

procedure TTestCSSResolver.TestRes_Var_NoDefault;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Var_Inline_NoDefault;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Doc.Style:=LinesToStr([
  'div {',
  '  --bird-color: red;',
  '}']);

  Div1:=AddDiv('Div1',Doc.Root);
  Div1.InlineStyle:='--bird-width: 3px; border-color: var(--bird-color); border-width: var(--bird-width);';

  ApplyStyle;
  AssertEquals('Div1.BorderColor','red',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','3px',Div1.BorderWidth);
end;

procedure TTestCSSResolver.TestRes_Var_Defaults;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

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

procedure TTestCSSResolver.TestRes_Var_MixedCase;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

  // the var() function name is case insensitive, while the custom property
  // name --bird-color is case sensitive
  Doc.Style:=LinesToStr([
  'div {',
  '  --bird-color: red;',
  '  --bird-width: 3px;',
  '  border-color: VAR(--bird-color);',
  '  border-width: Var(--bird-width);',
  '}']);
  ApplyStyle;
  AssertEquals('Div1.BorderColor','red',Div1.BorderColor);
  AssertEquals('Div1.BorderWidth','3px',Div1.BorderWidth);
end;

procedure TTestCSSResolver.TestRes_Var_StringLiteral;
begin
  // plain var() call
  AssertEquals('var(--a)',true,HasCSSValueVarCall('var(--a)'));
  AssertEquals('VAR(--a) case insensitive',true,HasCSSValueVarCall('VAR(--a)'));
  AssertEquals('1px var(--a)',true,HasCSSValueVarCall('1px var(--a)'));

  // no var() call
  AssertEquals('empty',false,HasCSSValueVarCall(''));
  AssertEquals('red',false,HasCSSValueVarCall('red'));
  AssertEquals('var without bracket',false,HasCSSValueVarCall('var'));
  AssertEquals('variable',false,HasCSSValueVarCall('variable'));

  // var() inside a string literal must be skipped
  AssertEquals('"var(--a)" double quoted',false,HasCSSValueVarCall('"var(--a)"'));
  AssertEquals('''var(--a)'' single quoted',false,HasCSSValueVarCall(''''+'var(--a)'+''''));
  AssertEquals('text before quoted var',false,HasCSSValueVarCall('url("var(--a)")'));
  AssertEquals('escaped quote in string',false,HasCSSValueVarCall('"\"var(--a)"'));
  AssertEquals('unterminated string',false,HasCSSValueVarCall('"var(--a)'));

  // a real var() call outside a string is still found
  AssertEquals('quoted then real var',true,HasCSSValueVarCall('"text" var(--a)'));
  AssertEquals('real var then quoted',true,HasCSSValueVarCall('var(--a) "var(--b)"'));
end;

procedure TTestCSSResolver.TestRes_PseudoElement;
var
  Div1: TDemoDiv;
  FirstLine: TDemoFirstLine;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

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

procedure TTestCSSResolver.TestRes_PseudoElement_Unary;
var
  Div1: TDemoDiv;
  FirstLine: TDemoFirstLine;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);

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

procedure TTestCSSResolver.TestRes_PseudoElement_PostfixSelectNothing;
var
  Div1: TDemoDiv;
  FirstLine: TDemoFirstLine;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);
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

procedure TTestCSSResolver.TestRes_Nested_Hash;
var
  Container, Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  // Container is the .Big parent; Div1 is a descendant of it
  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Big');

  Div1:=AddDiv('Div1',Container);

  Doc.Style:=LinesToStr([
  '.Big {',
  '  #Div1 {', // -> descendant combinator: .Big #Div1
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Container.Width','',Container.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_Class;
var
  Container, Div1, Div2: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  // Container is the .Big parent; Div1 is a descendant of it
  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Foo');

  Div1:=AddDiv('Div1',Container);
  Div1.CSSClasses.Add('Bar');

  Div2:=AddDiv('Div2',Container);

  Doc.Style:=LinesToStr([
  '.Foo {',
  '  .Bar {', // descendant combinator: .Foo .Bar
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div2.Width','',Div2.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_AndClass;
var
  Div1, Div2, Div3: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('Bird');
  Div1.CSSClasses.Add('Ant');   // has both classes -> matches

  Div2:=AddDiv('Div2',Doc.Root);
  Div2.CSSClasses.Add('Bird');  // only .Bird, not .Ant -> no match

  Div3:=AddDiv('Div3',Doc.Root);
  Div3.CSSClasses.Add('Ant');   // only .Ant, not .Bird -> no match

  Doc.Style:=LinesToStr([
  '.Bird {',
  '  &.Ant {', // compound: element must match both .Bird and .Ant
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div2.Width','',Div2.Width);
  AssertEquals('Div3.Width','',Div3.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_AndSpaceClass;
var
  Container, Div1: TDemoDiv;
  Span1, Span2, Span3: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Foo');

  Div1:=AddDiv('Div1',Container);

  Span1:=AddSpan('Span1',Container);
  Span1.CSSClasses.Add('Bar');
  Span2:=AddSpan('Span2',Div1);
  Span2.CSSClasses.Add('Bar');
  Span3:=AddSpan('Span3',Span1);
  Span3.CSSClasses.Add('Bar');

  Doc.Style:=LinesToStr([
  '.Foo {',
  '  & .Bar {', // descendant combinator: .Foo .Bar
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','10px',Span2.Width);
  AssertEquals('Span3.Width','10px',Span3.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_ClassCommaClass;
var
  Container, Div1: TDemoDiv;
  Span1, Span2, Span3, Span4: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Red');

  Div1:=AddDiv('Div1',Container);

  Span1:=AddSpan_Class('Span1','Blue',Container);
  Span2:=AddSpan_Class('Span2','Green',Div1);
  Span3:=AddSpan_Class('Span3','Blue',Span1);
  Span4:=AddSpan_Class('Span4','Blue',Doc.Root);

  Doc.Style:=LinesToStr([
  '.Red {',
  '  .Green,.Blue {', // 2x descendant combinator: .Red .Green,.Red .Blue
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','10px',Span2.Width);
  AssertEquals('Span3.Width','10px',Span3.Width);
  AssertEquals('Span4.Width','',Span4.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_ClassCommaAndClass;
var
  Container, Div1, Div2, Div3, Div4: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Ant');

  Div1:=AddDiv('Div1',Container);
  Div1.CSSClasses.Add('Bird');        // descendant of .Ant with .Bird -> matches .Bird

  Div2:=AddDiv('Div2',Container);
  Div2.CSSClasses.Add('Cat');         // descendant of .Ant with .Cat, but not .Ant itself -> no match

  Div3:=AddDiv('Div3',Doc.Root);
  Div3.CSSClasses.Add('Ant');
  Div3.CSSClasses.Add('Cat');         // has both .Ant and .Cat -> matches &.Cat

  Div4:=AddDiv('Div4',Doc.Root);
  Div4.CSSClasses.Add('Bird');        // .Bird but not a descendant of .Ant -> no match

  Doc.Style:=LinesToStr([
  '.Ant {',
  '  .Bird,&.Cat {', // .Bird: descendant of .Ant; &.Cat: element with both .Ant and .Cat
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div2.Width','',Div2.Width);
  AssertEquals('Div3.Width','10px',Div3.Width);
  AssertEquals('Div4.Width','',Div4.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_ClassCommaAndSpaceClass;
var
  Container, Div1: TDemoDiv;
  Span1, Span2, Span3, Span4: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Red');

  Div1:=AddDiv('Div1',Container);

  Span1:=AddSpan_Class('Span1','Blue',Container);
  Span2:=AddSpan_Class('Span2','Green',Div1);
  Span3:=AddSpan_Class('Span3','Blue',Span1);
  Span4:=AddSpan_Class('Span4','Blue',Doc.Root);

  Doc.Style:=LinesToStr([
  '.Red {',
  '  .Green,& .Blue {', // 2x descendant combinator: .Red .Green,.Red .Blue
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','10px',Span2.Width);
  AssertEquals('Span3.Width','10px',Span3.Width);
  AssertEquals('Span4.Width','',Span4.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_ClassSpaceAnd;
var
  Container, Div1, Div2, Div3: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Bird');

  Div1:=AddDiv('Div1',Container);
  Div1.CSSClasses.Add('Ant');         // .Ant descendant of .Bird -> matches .Bird &

  Div2:=AddDiv('Div2',Container);     // descendant of .Bird but not .Ant -> no match

  Div3:=AddDiv('Div3',Doc.Root);
  Div3.CSSClasses.Add('Ant');         // .Ant but not descendant of .Bird -> no match

  Doc.Style:=LinesToStr([
  '.Ant {',
  '  .Bird & {', // append: expands to .Bird .Ant
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div2.Width','',Div2.Width);
  AssertEquals('Div3.Width','',Div3.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_AndSpaceType;
var
  Container, Div1: TDemoDiv;
  Span1, Span2, Span3: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Foo');

  Div1:=AddDiv('Div1',Container);
  Div1.CSSClasses.Add('Bar');

  Span1:=AddSpan('Span1',Container);
  Span2:=AddSpan('Span2',Div1);
  Span3:=AddSpan('Span3',Span1);

  Doc.Style:=LinesToStr([
  '.Foo {',
  '  & span {', // descendant combinator: .Foo span
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','10px',Span2.Width);
  AssertEquals('Span3.Width','10px',Span3.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_GTClass;
var
  Container, Div1: TDemoDiv;
  Span1, Span2, Span3, Span4: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Bird');

  Div1:=AddDiv('Div1',Container);

  Span1:=AddSpan_Class('Span1','Ant',Container);
  Span2:=AddSpan_Class('Span2','Ant',Div1);
  Span3:=AddSpan_Class('Span3','Ant',Span1);
  Span4:=AddSpan_Class('Span4','Ant',Doc.Root);

  Doc.Style:=LinesToStr([
  '.Bird {',
  '  > .Ant {', // child combinator: .Bird > .Ant
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','',Span2.Width);
  AssertEquals('Span3.Width','',Span3.Width);
  AssertEquals('Span4.Width','',Span4.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_AndGTClass;
var
  Container, Div1: TDemoDiv;
  Span1, Span2, Span3, Span4: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Bird');

  Div1:=AddDiv('Div1',Container);

  Span1:=AddSpan_Class('Span1','Ant',Container);
  Span2:=AddSpan_Class('Span2','Ant',Div1);
  Span3:=AddSpan_Class('Span3','Ant',Span1);
  Span4:=AddSpan_Class('Span4','Ant',Doc.Root);

  Doc.Style:=LinesToStr([
  '.Bird {',
  '  & > .Ant {', // child combinator: .Bird > .Ant
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','',Span2.Width);
  AssertEquals('Span3.Width','',Span3.Width);
  AssertEquals('Span4.Width','',Span4.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_PlusClass;
var
  Container: TDemoDiv;
  Span1, Span2, Span3: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Bird');

  Span1:=AddSpan_Class('Span1','Ant',Doc.Root);   // immediate sibling of .Bird -> matches
  Span2:=AddSpan_Class('Span2','Ant',Doc.Root);   // sibling but not adjacent to .Bird -> no match
  Span3:=AddSpan_Class('Span3','Ant',Container);  // child of .Bird, not sibling -> no match

  Doc.Style:=LinesToStr([
  '.Bird {',
  '  + .Ant {', // adjacent sibling combinator: .Bird + .Ant
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','',Span2.Width);
  AssertEquals('Span3.Width','',Span3.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_AndPlusType;
var
  Container: TDemoDiv;
  Span1, Span2, Span3: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Bird');

  Span1:=AddSpan_Class('Span1','Ant',Doc.Root);   // immediate sibling of .Bird -> matches
  Span2:=AddSpan_Class('Span2','Ant',Doc.Root);   // sibling but not adjacent to .Bird -> no match
  Span3:=AddSpan_Class('Span3','Ant',Container);  // child of .Bird, not sibling -> no match

  Doc.Style:=LinesToStr([
  '.Bird {',
  '  & + .Ant {', // adjacent sibling combinator: .Bird + .Ant
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','',Span2.Width);
  AssertEquals('Span3.Width','',Span3.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_TildeClass;
var
  Container: TDemoDiv;
  Span1, Span2, Span3: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Bird');

  Span1:=AddSpan_Class('Span1','Ant',Doc.Root);   // sibling after .Bird -> matches
  Span2:=AddSpan_Class('Span2','Ant',Doc.Root);   // sibling after .Bird (not adjacent) -> matches
  Span3:=AddSpan_Class('Span3','Ant',Container);  // child of .Bird, not sibling -> no match

  Doc.Style:=LinesToStr([
  '.Bird {',
  '  ~ .Ant {', // general sibling combinator: .Bird ~ .Ant
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','10px',Span2.Width);
  AssertEquals('Span3.Width','',Span3.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_AndTildeClass;
var
  Container: TDemoDiv;
  Span1, Span2, Span3: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);

  Container:=AddDiv('Container',Doc.Root);
  Container.CSSClasses.Add('Bird');

  Span1:=AddSpan_Class('Span1','Ant',Doc.Root);   // sibling after .Bird -> matches
  Span2:=AddSpan_Class('Span2','Ant',Doc.Root);   // sibling after .Bird (not adjacent) -> matches
  Span3:=AddSpan_Class('Span3','Ant',Container);  // child of .Bird, not sibling -> no match

  Doc.Style:=LinesToStr([
  '.Bird {',
  '  & ~ .Ant {', // general sibling combinator: .Bird ~ .Ant
  '    width:10px;',
  '  }',
  '}']);
  ApplyStyle;

  AssertEquals('Container.Width','',Container.Width);
  AssertEquals('Span1.Width','10px',Span1.Width);
  AssertEquals('Span2.Width','10px',Span2.Width);
  AssertEquals('Span3.Width','',Span3.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_HasAtribute;
var
  Button1, Button2: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.ExplicitAttributes[naLeft]:='100px';

  Button1:=AddButton('Button1',Doc.Root);
  Button1.ExplicitCaption:='Click Button1';

  Div1:=AddDiv('Div1',Doc.Root);

  Button2:=AddButton('Button2',Div1);
  Button2.ExplicitCaption:='Click Button2';

  Doc.Style:=LinesToStr([
  '[left] { [caption] { width: 3px;} }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Width','3px',Button1.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Button2.Width','3px',Button2.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_AndHasAtribute;
var
  Button1, Button2: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.ExplicitAttributes[naLeft]:='100px';

  Button1:=AddButton('Button1',Doc.Root);
  Button1.ExplicitAttributes[naLeft]:='100px';
  Button1.ExplicitCaption:='Click Button1';

  Div1:=AddDiv('Div1',Doc.Root);

  Button2:=AddButton('Button2',Div1);
  Button2.ExplicitCaption:='Click Button2';

  Doc.Style:=LinesToStr([
  '[left] { &[caption] { width: 3px;} }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Width','3px',Button1.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Button2.Width','',Button2.Width);
end;

procedure TTestCSSResolver.TestRes_Nested_AndHasSpaceAtribute;
var
  Button1, Button2: TDemoButton;
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';
  Doc.Root.ExplicitAttributes[naLeft]:='100px';

  Button1:=AddButton('Button1',Doc.Root);
  Button1.ExplicitCaption:='Click Button1';

  Div1:=AddDiv('Div1',Doc.Root);

  Button2:=AddButton('Button2',Div1);
  Button2.ExplicitCaption:='Click Button2';

  Doc.Style:=LinesToStr([
  '[left] { & [caption] { width: 3px;} }',
  '']);
  ApplyStyle;
  AssertEquals('Root.Width','',Doc.Root.Width);
  AssertEquals('Button1.Width','3px',Button1.Width);
  AssertEquals('Div1.Width','',Div1.Width);
  AssertEquals('Button2.Width','3px',Button2.Width);
end;

procedure TTestCSSResolver.TestRes_Media_Name;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media screen { div{ width: 10px; } }',
  '@media (screen) { div{ height: 11px; } }',
  '@media print { div{ width: 20px; } }',
  '@media (print) { div{ width: 21px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','11px',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_NameColonValue;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media (orientation: portrait) { div{ width: 10px; } }',
  '@media (orientation: print) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_Range_NameGtValue;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media (width > 400px) { div{ width: 10px; } }',
  '@media (height < 1000px) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','11px',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_Range_ValueLtName;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media (1000px >= width) { div{ width: 10px; } }',
  '@media (300px <= height) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','11px',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_Range_NameGtName;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media (width > height) { div{ width: 10px; } }',
  '@media (height = width) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_Range_ValueLtNameLtValue;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media (100px <= width < 1000px) { div{ width: 10px; } }',
  '@media (300px < height <= 900px) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','11px',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_Range_ValueGtNameGtValue;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media (1000px >= width > 100px) { div{ width: 10px; } }',
  '@media (900px > height >= 300px) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','11px',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_And;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media (screen and screen) { div{ width: 10px; } }',
  '@media (screen and (width > 400px)) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','11px',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_Or;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media ((width > 9000px) or screen) { div{ width: 10px; } }',
  '@media ((width > 9000px) or (height > 90px)) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div1.Height','11px',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_Comma;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media (width > 9000px), screen { div{ width: 10px; } }',
  '@media (width > 9000px), (height > 9000px) { div{ height: 11px; } }',
  '@media invalid, screen { div{ color: red; } }',
  '']);
  ApplyStyle;
  // first rule: first selector fails, second matches -> match
  AssertEquals('Div1.Width','10px',Div1.Width);
  // second rule: both selectors fail -> no match
  AssertEquals('Div1.Height','',Div1.Height);
  // third rule: parsing recovers after the comma, so screen matches -> match
  AssertEquals('Div1.Color','red',Div1.Color);
end;

procedure TTestCSSResolver.TestRes_Media_Not;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media not (width > 9000px) { div{ width: 10px; } }',
  '@media not screen { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  // width > 9000px is false -> not false -> match
  AssertEquals('Div1.Width','10px',Div1.Width);
  // screen is true -> not true -> no match
  AssertEquals('Div1.Height','',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_NotAnd;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:=LinesToStr([
  '@media not print and not (width > 1000px) { div{ width: 10px; } }',
  '@media not screen and not (width > 1000px) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  // not print (print unknown -> false -> inverted -> match)
  // and not (800 > 1000px) (false -> inverted -> match) -> both match
  AssertEquals('Div1.Width','10px',Div1.Width);
  // not screen (screen true -> inverted -> no match) -> AND short-circuits -> no match
  AssertEquals('Div1.Height','',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_Ratio;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  // Doc.Width=800, Doc.Height=600, aspect-ratio = 800/600 = 4/3 < 3/2
  Doc.Style:=LinesToStr([
  '@media (aspect-ratio < 3/2) { div{ width: 10px; } }',
  '@media (aspect-ratio > 3/2) { div{ height: 11px; } }',
  '']);
  ApplyStyle;
  // 4/3 < 3/2 -> match
  AssertEquals('Div1.Width','10px',Div1.Width);
  // 4/3 > 3/2 -> no match
  AssertEquals('Div1.Height','',Div1.Height);
end;

procedure TTestCSSResolver.TestRes_Media_EvalOncePerInit;
var
  Div1, Div2, Div3: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);
  Div2:=AddDiv('Div2',Doc.Root);
  Div3:=AddDiv('Div3',Doc.Root);

  // one @media rule with one boolean selector -> HasMediaBoolean called once in Init
  Doc.Style:='@media screen { div{ width: 10px; } }';
  ApplyStyle;
  AssertEquals('Div1.Width','10px',Div1.Width);
  AssertEquals('Div2.Width','10px',Div2.Width);
  AssertEquals('Div3.Width','10px',Div3.Width);
  AssertEquals('MediaEvalCount',1,Doc.MediaEvalCount);
end;

procedure TTestCSSResolver.TestRes_Media_ReplaceStyleSheet;
var
  Div1: TDemoDiv;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Doc.Root.Name:='root';

  Div1:=AddDiv('Div1',Doc.Root);

  Doc.Style:='@media screen { div{ width: 10px; } }';
  ApplyStyle;
  AssertEquals('Div1.Width before','10px',Div1.Width);

  // replace stylesheet: print does not match -> width not set
  Doc.Style:='@media print { div{ width: 20px; } }';
  ApplyStyle;
  AssertEquals('Div1.Width after','',Div1.Width);
end;

procedure TTestCSSResolver.TestRes_Buckets_GetCSSClasses;
var
  Button1: TDemoButton;
  R: TCSSResolver;
  Classes: TCSSNumericalIDArray;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('red');
  Button1.CSSClasses.Add('big');
  Doc.Style:='.red { left: 1px; } .big { top: 2px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;

  Classes:=Button1.GetCSSClasses;
  AssertEquals('class count',2,length(Classes));
  AssertEquals('class[0] is red id',R.GetCSSClassID('red'),Classes[0]);
  AssertEquals('class[1] is big id',R.GetCSSClassID('big'),Classes[1]);
  // GetCSSClasses is consistent with HasCSSClass
  AssertTrue('HasCSSClass red',Button1.HasCSSClass(R.GetCSSClassID('red')));
  AssertTrue('HasCSSClass big',Button1.HasCSSClass(R.GetCSSClassID('big')));
  AssertFalse('not HasCSSClass unknown',Button1.HasCSSClass(R.GetCSSClassID('green')));
end;

procedure TTestCSSResolver.TestRes_Buckets_ClassBucket;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('red');
  // Note: every node also matches its user-agent type style (button{display:..})
  // via the type bucket, so the rule counts below include that one extra rule.
  // .blue does not match -> only .red applies (+ the UA button rule)
  Doc.Style:='.red { left: 1px; }'
            +'.blue { top: 2px; }';
  ApplyStyle;
  AssertEquals('Button1.left','1px',Button1.Left);
  AssertEquals('Button1.top','',Button1.Top);
  AssertEquals('Button1.display (UA)','inline-block',Button1.Display);
  AssertEquals('rule count',2,length(Button1.Rules.Rules));
end;

procedure TTestCSSResolver.TestRes_Buckets_TypeBucket;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  // span does not match the button -> only button applies (+ the UA button rule)
  Doc.Style:='button { left: 1px; }'
            +'span { top: 2px; }';
  ApplyStyle;
  AssertEquals('Button1.left','1px',Button1.Left);
  AssertEquals('Button1.top','',Button1.Top);
  AssertEquals('rule count',2,length(Button1.Rules.Rules));
end;

procedure TTestCSSResolver.TestRes_Buckets_IdBucket;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  // #Other does not match -> only #Button1 applies (+ the UA button rule)
  Doc.Style:='#Button1 { left: 1px; }'
            +'#Other { top: 2px; }';
  ApplyStyle;
  AssertEquals('Button1.left','1px',Button1.Left);
  AssertEquals('Button1.top','',Button1.Top);
  AssertEquals('rule count',2,length(Button1.Rules.Rules));
end;

procedure TTestCSSResolver.TestRes_Buckets_IdBucketNumerical;
var
  Button1: TDemoButton;
  R: TCSSResolver;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Doc.Style:='#Button1 { left: 1px; }'
            +'#Other { top: 2px; }';
  ApplyStyle;
  R:=Doc.CSSResolver;
  // the node reports its id as a numerical index, equal to the index the
  // #Button1 selector was registered under (and different from #Other)
  AssertTrue('node id index>=1',Button1.GetCSSID>=1);
  AssertEquals('node id index = #Button1 index',R.GetCSSIDIndex('Button1'),Button1.GetCSSID);
  AssertTrue('node id index <> #Other index',Button1.GetCSSID<>R.GetCSSIDIndex('Other'));
  // and only the matching #Button1 rule applies (+ the UA button rule)
  AssertEquals('Button1.left','1px',Button1.Left);
  AssertEquals('Button1.top','',Button1.Top);
  AssertEquals('rule count',2,length(Button1.Rules.Rules));
end;

procedure TTestCSSResolver.TestRes_Buckets_OtherBucket;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('red');
  // the universal selector goes to the Other bucket and must always be checked;
  // *, .red and button all match -> 3 author rules + the UA button rule
  Doc.Style:='* { left: 1px; }'
            +'.red { top: 2px; }'
            +'button { width: 3px; }';
  ApplyStyle;
  AssertEquals('Button1.left','1px',Button1.Left);
  AssertEquals('Button1.top','2px',Button1.Top);
  AssertEquals('Button1.width','3px',Button1.Width);
  AssertEquals('rule count',4,length(Button1.Rules.Rules));
end;

procedure TTestCSSResolver.TestRes_Buckets_MultiSelectorRuleOnce;
var
  Button1: TDemoButton;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Button1:=AddButton('Button1',Doc.Root);
  Button1.CSSClasses.Add('red');
  // all three selectors of this single rule match the node (class, type, id)
  // -> the rule lands in three buckets but must be applied only once.
  // rule count = 1 author rule + the UA button rule (would be 3 if duplicated).
  Doc.Style:='.red, button, #Button1 { left: 1px; }';
  ApplyStyle;
  AssertEquals('Button1.left','1px',Button1.Left);
  AssertEquals('rule count',2,length(Button1.Rules.Rules));
end;

procedure TTestCSSResolver.TestRes_Buckets_NonMatchingSkipped;
var
  Span1: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Span1:=AddSpan_Class('Span1','blue',Doc.Root);
  // none of these author rules match a span.blue with id Span1
  // -> only the UA span type style applies
  Doc.Style:='.red { left: 1px; }'
            +'button { top: 2px; }'
            +'#Other { width: 3px; }';
  ApplyStyle;
  AssertEquals('Span1.left','',Span1.Left);
  AssertEquals('Span1.top','',Span1.Top);
  AssertEquals('Span1.width','',Span1.Width);
  AssertEquals('Span1.display (UA)','inline-block',Span1.Display);
  AssertEquals('rule count',1,length(Span1.Rules.Rules));
end;

procedure TTestCSSResolver.TestRes_Buckets_CompoundRightmost;
var
  Div1: TDemoDiv;
  Span1: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Div1:=AddDiv('Div1',Doc.Root);
  Div1.CSSClasses.Add('red');
  Span1:=AddSpan_Class('Span1','red',Doc.Root);
  // compound div.red: rightmost identifier is the class .red, but the type div
  // must still be verified -> only the div matches, not the span
  Doc.Style:='div.red { left: 1px; }';
  ApplyStyle;
  AssertEquals('Div1.left','1px',Div1.Left);
  AssertEquals('Span1.left','',Span1.Left);
end;

procedure TTestCSSResolver.TestRes_Buckets_DescendantRightmost;
var
  Div1: TDemoDiv;
  Span1, Span2: TDemoSpan;
begin
  Doc.Root:=TDemoNode.Create(nil);
  Div1:=AddDiv('Div1',Doc.Root);
  Span1:=AddSpan_Class('Span1','red',Div1); // inside the div
  Span2:=AddSpan_Class('Span2','red',Doc.Root); // not inside a div
  // descendant combinator div .red: rightmost is the class .red, ancestor div required
  Doc.Style:='div .red { left: 1px; }';
  ApplyStyle;
  AssertEquals('Span1.left','1px',Span1.Left);
  AssertEquals('Span2.left','',Span2.Left);
  if Div1=nil then ;
end;

procedure TTestCSSResolver.CheckTokenize(const Title, aValue, Expected: string);
var
  Data: TBytes;
  Resolver: TCSSBaseResolver;
begin
  Resolver:=Doc.CSSResolver;
  AssertEquals(Title+' valid',true,Resolver.Tokenize(aValue,Data));
  AssertEquals(Title,Expected,TokenStreamToStr(Resolver.CSSRegistry,Data));
end;

procedure TTestCSSResolver.CheckTokenizeInvalid(const Title, aValue: string);
var
  Data: TBytes;
begin
  AssertEquals(Title+' invalid',false,Doc.CSSResolver.Tokenize(aValue,Data));
end;

procedure TTestCSSResolver.CheckRoundtrip(const Title, aValue, Expected: string);
var
  Data: TBytes;
  Resolver: TCSSBaseResolver;
begin
  Resolver:=Doc.CSSResolver;
  AssertEquals(Title+' valid',true,Resolver.Tokenize(aValue,Data));
  AssertEquals(Title,Expected,Resolver.Detokenize(Data));
end;

procedure TTestCSSResolver.TestRes_Tokenize_Empty;
begin
  // empty and whitespace-only values are invalid (Tokenize returns false)
  CheckTokenizeInvalid('empty','');
  CheckTokenizeInvalid('only whitespace','   ');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Keyword;
begin
  CheckTokenize('keyword red','red','kw(red)');
  CheckTokenize('keyword auto','auto','kw(auto)');
  CheckTokenize('keyword inline-block','inline-block','kw(inline-block)');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Identifier;
begin
  CheckTokenize('custom ident','--my-var','ident(--my-var)');
  CheckTokenize('custom ident short','--x','ident(--x)');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Float;
begin
  CheckTokenize('int','12','float(12)');
  CheckTokenize('px','5px','float(5px)');
  CheckTokenize('percent','50%','float(50%)');
  CheckTokenize('frac','1.5em','float(1.5em)');
  CheckTokenize('dot frac','.25','float(0.25)');
  CheckTokenize('exponent','2e3','float(2000)');
  CheckTokenize('plus number','+7','float(7)');
  CheckTokenize('minus number','-3px','float(-3px)');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Whitespace;
begin
  CheckTokenize('two keywords','red blue','kw(red) ws kw(blue)');
  CheckTokenize('collapsed','red    blue','kw(red) ws kw(blue)');
  // leading and trailing whitespace is trimmed (TrimEnclosingSpace=true)
  CheckTokenize('leading','  red','kw(red)');
  CheckTokenize('trailing','red ','kw(red)');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Symbols;
begin
  CheckTokenize('comma','red,blue','kw(red) sym(,) kw(blue)');
  CheckTokenize('colon','red:blue','kw(red) sym(:) kw(blue)');
  CheckTokenize('semicolon','red;','kw(red) sym(;)');
  CheckTokenize('div','red/blue','kw(red) sym(/) kw(blue)');
  CheckTokenize('star','red*blue','kw(red) sym(*) kw(blue)');
  CheckTokenize('dot','red.blue','kw(red) sym(.) kw(blue)');
end;

procedure TTestCSSResolver.TestRes_Tokenize_PlusMinus;
begin
  CheckTokenize('plus operator','+ red','plus ws kw(red)');
  CheckTokenize('minus operator','- red','minus ws kw(red)');
  CheckTokenize('plus var','+var(--x)','plus func(var) ident(--x) )');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Function;
begin
  CheckTokenize('var','var(--x)','func(var) ident(--x) )');
  CheckTokenize('var fallback','var(--x, red)',
    'func(var) ident(--x) sym(,) ws kw(red) )');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Brackets;
begin
  CheckTokenize('parenthesis','(red)','( kw(red) )');
  CheckTokenize('brackets','[red]','[ kw(red) ]');
  CheckTokenize('nested','([red])','( [ kw(red) ] )');
end;

procedure TTestCSSResolver.TestRes_Tokenize_HexColor;
begin
  CheckTokenize('rgb','#fff','hex(fff)');
  CheckTokenize('rgba','#abcd','hex(abcd)');
  CheckTokenize('rrggbb','#ff0000','hex(ff0000)');
  CheckTokenize('rrggbbaa','#11223344','hex(11223344)');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Strings;
begin
  CheckTokenize('apos','''hello''','apos(hello)');
  CheckTokenize('quote','"hello"','quote(hello)');
  CheckTokenize('empty apos','''''','apos()');
end;

procedure TTestCSSResolver.TestRes_Tokenize_Invalid;
begin
  CheckTokenizeInvalid('unknown keyword','footastic');
  CheckTokenizeInvalid('unknown function','footastic(1)');
  CheckTokenizeInvalid('unknown unit','5foo');
  CheckTokenizeInvalid('unknown symbol','red=blue');
  CheckTokenizeInvalid('unbalanced open paren','var(--x');
  CheckTokenizeInvalid('unbalanced close paren','red)');
  CheckTokenizeInvalid('unbalanced bracket','[red');
  CheckTokenizeInvalid('mismatched bracket','(red]');
  CheckTokenizeInvalid('unterminated apos','''hello');
  CheckTokenizeInvalid('unterminated quote','"hello');
  CheckTokenizeInvalid('bad hex','#ab');
end;

procedure TTestCSSResolver.TestRes_Detokenize;
begin
  // values that survive tokenize+detokenize unchanged
  CheckRoundtrip('keyword','red','red');
  CheckRoundtrip('identifier','--my-var','--my-var');
  CheckRoundtrip('float','5px','5px');
  CheckRoundtrip('two keywords','red blue','red blue');
  CheckRoundtrip('symbols','red,blue','red,blue');
  CheckRoundtrip('function','var(--x, red)','var(--x, red)');
  CheckRoundtrip('brackets','([red])','([red])');
  CheckRoundtrip('hex','#ff0000','#ff0000');
  CheckRoundtrip('apos','''hello''','''hello''');
  CheckRoundtrip('quote','"hello"','"hello"');
  CheckRoundtrip('plus operator','+ red','+ red');
  // normalizations: whitespace collapses, numbers are normalized
  CheckRoundtrip('collapsed whitespace','red    blue','red blue');
  CheckRoundtrip('normalized number','.50','0.5');
  CheckRoundtrip('normalized exponent','2e3','2000');
end;

initialization
  RegisterTests([TTestCSSResolver]);

end.

