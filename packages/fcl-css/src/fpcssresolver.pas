{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022 by Michael Van Canneyt (michael@freepascal.org)

    This file contains CSS utility class

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Works:

Selector 	Example 	Example description
.class  	.intro  	Selects all elements with class="intro"
.class1.class2 	.name1.name2 	Selects all elements with both name1 and name2 set within its class attribute
.class1 .class2 	.name1 .name2 	Selects all elements with name2 that is a descendant of an element with name1
#id 	#firstname 	Selects the element with name="firstname"
* 	* 	Selects all elements
type 	p 	Selects all <p> elements
type.class 	p.intro 	Selects all <p> elements with class="intro"
type,type 	div, p 	Selects all <div> elements and all <p> elements
type type 	div p 	Selects all <p> elements inside <div> elements
type>type 	div > p 	Selects all <p> elements where the parent is a <div> element
type+type 	div + p 	Selects the first <p> element that is placed immediately after a <div> element
element1~element2 	p ~ ul 	Selects every <ul> element that is preceded by a <p> element
[attribute] 	[target] 	Selects all elements with a target attribute
[attribute=value] 	[target=_blank] 	Selects all elements with target="_blank"
[attribute~=value] 	[title~=flower] 	Selects all elements with a title attribute containing the *word* "flower"
[attribute|=value] 	[lang|=en] 	Selects all elements with a lang attribute value equal to "en" or starting with "en-" (hyphen)
[attribute^=value] 	a[href^="https"] 	Selects every <a> element whose href attribute value begins with "https"
[attribute$=value] 	a[href$=".pdf"] 	Selects every <a> element whose href attribute value ends with ".pdf"
[attribute*=value] 	a[href*="w3schools"] 	Selects every <a> element whose href attribute value contains the substring "w3schools"
:root 	:root 	Selects the document's root element
:empty 	p:empty 	Selects every <p> element that has no children (including text nodes)
:first-child 	p:first-child 	Selects every <p> element that is the first child of its parent
:first-of-type 	p:first-of-type 	Selects every <p> element that is the first <p> element of its parent
:last-child 	p:last-child 	Selects every <p> element that is the last child of its parent
:last-of-type 	p:last-of-type 	Selects every <p> element that is the last <p> element of its parent
:not(selector) 	:not(p) 	Selects every element that is not a <p> element
:nth-child(n) 	p:nth-child(2) 	Selects every <p> element that is the second child of its parent. n can be a number, a keyword (odd or even), or a formula (like an + b).
:nth-last-child(n) 	p:nth-last-child(2) 	Selects every <p> element that is the second child of its parent, counting from the last child
:nth-last-of-type(n) 	p:nth-last-of-type(2) 	Selects every <p> element that is the second <p> element of its parent, counting from the last child
:nth-of-type(n) 	p:nth-of-type(2) 	Selects every <p> element that is the second <p> element of its parent
:only-of-type 	p:only-of-type 	Selects every <p> element that is the only <p> element of its parent
:only-child 	p:only-child 	Selects every <p> element that is the only child of its parent
:is()
:where()

Specificity:
important: 10000
inline: 1000
id: 100 #menu
class+attribute selectors: 10 .button, :hover, [href]
element/type: 1 p, :before
*: 0

ToDo:
- 'all' attribute: resets all properties, except direction, unicode bidi and custom css properties
- :has()
- namespaces
- layers
- --varname, var(), inherits
- counter-reset
- counter-increment
- @rules:-----------------------------------------------------------------------
  - @media
  - @font-face
  - @keyframes
  - @property
- Functions and Vars:-----------------------------------------------------------
  - attr() 	Returns the value of an attribute of the selected element
            attr(title)
            attr(src url)
            attr(data-width px, inherit);
  - calc() 	Allows you to perform calculations to determine CSS property values  calc(100% - 100px)
  - max() min() minmax(minvalue,maxvalue)   min(50%, 50px)
             keyword values max-content, min-content, or auto
  - clamp(minvalue,preferred,maxvalue) = max(MIN, min(VAL, MAX))
  - var()   usable in property values, query custom css properties, inherits
            var(--name), var(--name, --default-name), var(--name, var(--foo, #FF0000))
- Pseudo-elements - not case sensitive:-----------------------------------------
  - ::first-letter 	p::first-letter 	Selects the first letter of every <p> element
  - ::first-line 	p::first-line 	Selects the first line of every <p> element
  - ::selection 	::selection 	Selects the portion of an element that is selected by a user
- Altering:---------------------------------------------------------------------
  - ::after 	p::after 	Insert something after the content of each <p> element
  - ::before 	p::before 	Insert something before the content of each <p> element
- grid-structural-selectors:----------------------------------------------------
  - columns combinator ||     col.selected || td
  - :nth-col()
  - :nth-last-col()

}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSResolver;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}
{$ModeSwitch AdvancedRecords}
{$IF FPC_FULLVERSION>30300}
{$WARN 6060 off} // Case statement does not handle all possible cases
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, System.Contnrs, System.StrUtils,
  Fcl.AVLTree, FpCss.Tree, FpCss.ValueParser;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, types, Contnrs, AVL_Tree, StrUtils, fpCSSTree, fpCSSResParser;
{$ENDIF FPC_DOTTEDUNITS}

const
  CSSSpecificityInvalid = -2;
  CSSSpecificityNoMatch = -1;
  CSSSpecificityUniversal = 0;
  CSSSpecificityType = 1;
  CSSSpecificityClass = 10; // includes attribute selectors e.g. [href]
  CSSSpecificityIdentifier = 100;
  CSSSpecificityUserAgent = 1000;
  CSSSpecificityUser = 2000;
  CSSSpecificityAuthor = 3000;
  CSSSpecificityInline = 10000;
  CSSSpecificityImportant = 100000;

type
  TCSSSpecificity = integer; // see CSSSpecificityInvalid..CSSSpecificityImportant

  TCSSOrigin = (
    cssoUserAgent,
    cssoUser,
    cssoAuthor
    );
const
  CSSOriginToSpecifity: array[TCSSOrigin] of TCSSNumericalID = (
    CSSSpecificityUserAgent,
    CSSSpecificityUser,
    CSSSpecificityAuthor
    );

type

  { ECSSResolver }

  ECSSResolver = class(ECSSException)
  end;

  TCSSAttributeMatchKind = (
    camkEqual,
    camkContains,
    camkContainsWord,
    camkBegins,
    camkEnds
    );
  TCSSAttributeMatchKinds = set of TCSSAttributeMatchKind;

  { ICSSNode }

  ICSSNode = interface
    function GetCSSID: TCSSString;
    function GetCSSTypeName: TCSSString;
    function GetCSSTypeID: TCSSNumericalID;
    function GetCSSPseudoElementName: TCSSString;
    function GetCSSPseudoElementID: TCSSNumericalID;
    // parent
    function GetCSSParent: ICSSNode;
    function GetCSSDepth: integer;
    function GetCSSIndex: integer; // node index in parent's children
    // siblings
    function GetCSSNextSibling: ICSSNode;
    function GetCSSPreviousSibling: ICSSNode;
    function GetCSSNextOfType: ICSSNode;
    function GetCSSPreviousOfType: ICSSNode;
    // children
    function GetCSSEmpty: boolean;
    function GetCSSChildCount: integer;
    function GetCSSChild(const anIndex: integer): ICSSNode;
    // attributes
    function HasCSSClass(const aClassName: TCSSString): boolean;
    function GetCSSAttributeClass: TCSSString; // get the 'class' attribute
    function GetCSSCustomAttribute(const AttrID: TCSSNumericalID): TCSSString;
    function HasCSSExplicitAttribute(const AttrID: TCSSNumericalID): boolean; // e.g. if the HTML has the attribute
    function GetCSSExplicitAttribute(const AttrID: TCSSNumericalID): TCSSString;
    function HasCSSPseudoClass(const AttrID: TCSSNumericalID): boolean;
  end;

type

  { TCSSResCustomAttributeDesc }

  TCSSResCustomAttributeDesc = class(TCSSAttributeDesc)
  public
  end;
  TCSSResCustomAttributeDescArray = array of TCSSResCustomAttributeDesc;

  { TCSSResolvedAttribute - used for shared rule lists, merged by the cascade algorithm, not yet computed  }

  TCSSResolvedAttribute = record
    AttrID: TCSSNumericalID;
    Specificity: TCSSSpecificity;
    DeclEl: TCSSDeclarationElement;
  end;
  TCSSResolvedAttributeArray = array of TCSSResolvedAttribute;
  PCSSResolvedAttribute = ^TCSSResolvedAttribute;

  TCSSSharedRule = record
    Rule: TCSSRuleElement;
    Specificity: TCSSSpecificity;
  end;
  PCSSSharedRule = ^TCSSSharedRule;
  TCSSSharedRuleArray = array of TCSSSharedRule;

  { TCSSSharedRuleList - elements with same CSS rules share the base attributes }

  TCSSSharedRuleList = class
    AllDecl: TCSSDeclarationElement;
    AllSpecificity: TCSSSpecificity;
    Rules: TCSSSharedRuleArray; // sorted ascending for Specificity, secondary for source position
    Values: TCSSResolvedAttributeArray; // not sorted, merged, not computed
    destructor Destroy; override;
    procedure Clear;
    function Clone: TCSSSharedRuleList;
    function IndexOfAttr(AttrId: TCSSNumericalID; ForInsert: boolean = false): integer;
  end;

  { TCSSAttributeValue }

  TCSSAttributeValue = class
  public
    type
      TState = (
        cavsSource, // value from CSS - simple normalization, e.g. no comments, some spaces differ, floats
        cavsBaseKeywords, // base keywords resolved e.g. "initial" or "inherit"
        cavsComputed, // has final result
        cavsInvalid // skip this value
        );
  public
    AttrID: TCSSNumericalID; // the resolver has substituted all shorthands
    State: TState;
    Value: TCSSString; // the resolver has substituted all var() calls
  end;
  TCSSAttributeValueArray = array of TCSSAttributeValue;

  { TCSSAttributeValues }

  TCSSAttributeValues = class
  public
    AllValue: TCSSNumericalID;
    Values: TCSSAttributeValueArray; // the resolver sorts them ascending for AttrID, shorthands are already replaced with longhands
    procedure SortValues; virtual; // ascending AttrID
    procedure SwapValues(Index1, Index2: integer);
    function IndexOf(AttrID: TCSSNumericalID): integer;
    procedure SetComputedValue(AttrID: TCSSNumericalID; const aValue: TCSSString);
    destructor Destroy; override;
  end;

  TCSSResolverNthChildParamsCacheItem = record
    TypeID: TCSSNumericalID;
    ChildIDs: TIntegerDynArray;
    Cnt: integer; // = length(ChildIDs), used during creation
  end;
  PCSSNthChildParamsCacheItem = ^TCSSResolverNthChildParamsCacheItem;
  TCSSResolverNthChildParamsCacheItems = array of TCSSResolverNthChildParamsCacheItem;

  TCSSResolverNthChildParams = class;

  TCSSResolverNthChildParamsCache = class
  public
    Owner: TCSSResolverNthChildParams;
    Parent: ICSSNode;
    OfSelector: TCSSElement;
    StackDepth: integer;
    Items: TCSSResolverNthChildParamsCacheItems;
  end;
  TCSSResolverNthChildParamsCaches = array of TCSSResolverNthChildParamsCache;

  { TCSSResolverNthChildParams }

  TCSSResolverNthChildParams = class(TCSSNthChildParams)
  public
    StackCache: TCSSResolverNthChildParamsCaches;
    destructor Destroy; override;
  end;

  TCSSResolverOption = (
    croErrorOnUnknownName
    );
  TCSSResolverOptions = set of TCSSResolverOption;

  { TCSSResolverLogEntry }

  TCSSResolverLogEntry = class
  public
    MsgType: TEventType;
    ID: TCSSMsgID;
    Msg: TCSSString;
    PosEl: TCSSElement;
  end;
  TCSSResolverLogEntryClass = class of TCSSResolverLogEntry;
  TCSSResolverLogEntryArray = array of TCSSResolverLogEntry;

  TCSSResolverLogEvent = procedure(Sender: TObject; Entry: TCSSResolverLogEntry) of object;

  TCSSResStringComparison = (
    crscDefault,
    crscCaseInsensitive,
    crscCaseSensitive
    );
  TCSSResStringComparisons = set of TCSSResStringComparison;

  { TCSSResolver }

  TCSSResolver = class(TCSSBaseResolver)
  public
    type
      TStyleSheet = class
        Source: TCSSString;
        Name: TCSSString; // case sensitive
        Origin: TCSSOrigin;
        Element: TCSSElement;
        Parsed: boolean;
      end;
      TStyleSheets = array of TStyleSheet;

      TLayerElement = record
        Src: TStyleSheet;
        Element: TCSSElement;
      end;
      TLayerElements = array of TLayerElement;

      TLayer = record
        Name: TCSSString;
        Origin: TCSSOrigin;
        Elements: TLayerElements;
        ElementCount: integer;
      end;
      TLayerArray = array of TLayer;
  private
    FLayers: TLayerArray; // sorted for Origin, named layers before anonymous layers
    FOnLog: TCSSResolverLogEvent;
    FOptions: TCSSResolverOptions;
    FStringComparison: TCSSResStringComparison;
    FStyleSheets: TStyleSheets;
    FStyleSheetCount: integer;
    function GetCustomAttributes(Index: TCSSNumericalID): TCSSAttributeDesc;
    function GetLogCount: integer;
    function GetLogEntries(Index: integer): TCSSResolverLogEntry;
    function GetStyleSheets(Index: integer): TStyleSheet;
    procedure SetOptions(const AValue: TCSSResolverOptions);
  protected
    type

      { TMergedAttribute }

      TMergedAttribute = record
        Stamp: Integer; // only valid if equal to FMergedAttributesStamp
        Specificity: TCSSSpecificity;
        DeclEl: TCSSDeclarationElement; // can be nil if set by a shorthand
        Value: TCSSString;
        Complete: boolean;
        Prev, Next: TCSSNumericalID; // valid if >0, see below FMergedAttributeFirst
      end;
      PMergedAttribute = ^TMergedAttribute;
      TMergedAttributeArray = array of TMergedAttribute;

  protected
    FCustomAttributes: TCSSResCustomAttributeDescArray;
    FCustomAttributeCount: TCSSNumericalID;
    FCustomAttributeNameToDesc: TFPHashList;
    FElRules: TCSSSharedRuleArray;
    FElRuleCount: integer;
    FNode: ICSSNode;
    FLogEntries: TFPObjectList; // list of TCSSResolverLogEntry
    FSharedRuleLists: TAVLTree; // tree of TCSSSharedRuleList sorted for rules
    FMergedAttributes: TMergedAttributeArray;
    FMergedAttributesStamp: integer;
    FMergedAttributeFirst, FMergedAttributeLast: TCSSNumericalID; // first, last index in FMergedAttributes of linked list of attributes with current stamp
    FMergedAllDecl: TCSSDeclarationElement;
    FMergedAllSpecificity: TCSSSpecificity;
    FSourceSpecificity: TCSSSpecificity;
    FCSSRegistryStamp: TCSSNumericalID;

    // parse stylesheets
    procedure ParseSource(Index: integer); virtual;
    function ParseCSSSource(const Src: TCSSString; Inline: boolean): TCSSElement; virtual;
    procedure ClearElements; virtual;
    procedure ClearCustomAttributes; virtual;

    // resolving rules
    procedure ComputeElement(El: TCSSElement); virtual;
    procedure ComputeRule(aRule: TCSSRuleElement); virtual;
    function SelectorMatches(aSelector: TCSSElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorIdentifierMatches(Identifier: TCSSResolvedIdentifierElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorHashIdentifierMatches(Identifier: TCSSHashIdentifierElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorClassNameMatches(aClassName: TCSSClassNameElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorPseudoClassMatches(aPseudoClass: TCSSResolvedPseudoClassElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorListMatches(aList: TCSSListElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorUnaryMatches(aUnary: TCSSUnaryElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorPseudoElementMatches(aLeft, aRight: TCSSElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorArrayMatches(anArray: TCSSArrayElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorCallMatches(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function Call_Not(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function Call_Is(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function Call_Where(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function Call_NthChild(PseudoFuncID: TCSSNumericalID; aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function CollectSiblingsOf(PseudoFuncID: TCSSNumericalID; TestNode: ICSSNode;
      Params: TCSSResolverNthChildParams): TIntegerDynArray; virtual;
    function GetSiblingOfIndex(SiblingIDs: TIntegerDynArray; Index: integer): integer; virtual;
    function ComputeValue(El: TCSSElement): TCSSString; virtual;
    function SameValueText(const A, B: TCSSString): boolean; virtual;
    function SameValueText(A: PCSSChar; ALen: integer; B: PCSSChar; BLen: integer): boolean; virtual;
    function PosSubString(const SearchStr, Str: TCSSString): integer; virtual;
    function PosWord(const SearchWord, Words: TCSSString): integer; virtual;
    function GetSiblingCount(aNode: ICSSNode): integer; virtual;

    // resolving identifiers
    function ResolveIdentifier(El: TCSSResolvedIdentifierElement; Kind: TCSSNumericalIDKind): TCSSNumericalID; virtual;

    // shared rules
    procedure ClearSharedRuleLists; virtual;
    procedure FindMatchingRules; virtual; // create FElRules for current FNode
    procedure AddRule(aRule: TCSSRuleElement; Specificity: TCSSSpecificity); // add rule to current array (FElRules)
    function FindSharedRuleList(const Rules: TCSSSharedRuleArray): TCSSSharedRuleList; virtual;
    function CreateSharedRuleList: TCSSSharedRuleList; virtual; // using FElRules, sets FMergedAttributes

    // merge properties
    procedure ClearMerge; virtual;
    procedure InitMerge; virtual;
    procedure SetMergedAttribute(AttrID, aSpecificity: TCSSNumericalID; DeclEl: TCSSDeclarationElement);
    procedure RemoveMergedAttribute(AttrID: TCSSNumericalID);
    procedure MergeAttribute(El: TCSSElement; aSpecificity: TCSSSpecificity); virtual;
    procedure SaveSharedMergedAttributes(SharedMerged: TCSSSharedRuleList); virtual;
    procedure LoadSharedMergedAttributes(SharedMerged: TCSSSharedRuleList); virtual;
    procedure WriteMergedAttributes(const Title: TCSSString); virtual;

    // var() and shorthands
    procedure LoadMergedValues; virtual; // load Value strings from css elements and remove longhand placeholders
    procedure SubstituteVarCalls; virtual; // replace all var()
    procedure ApplyShorthands; virtual; // replace all shorthands with their longhands
    function CreateValueList: TCSSAttributeValues; virtual; // from FMergedAttributes
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Init; virtual; // call after adding stylesheets and before computing all nodes
    function GetElPath(El: TCSSElement): TCSSString; virtual;
    function GetElPos(El: TCSSElement): TCSSString; virtual;
    function ParseInlineStyle(const Src: TCSSString): TCSSRuleElement; virtual; // must be freed by caller
    procedure Compute(Node: ICSSNode;
      InlineStyle: TCSSRuleElement; // inline style of Node
      out Rules: TCSSSharedRuleList {owned by resolver};
      out Values: TCSSAttributeValues
      ); virtual;
    // attributes
    property CustomAttributes[Index: TCSSNumericalID]: TCSSAttributeDesc read GetCustomAttributes;
    property CustomAttributeCount: TCSSNumericalID read FCustomAttributeCount;
    function GetAttributeID(const aName: TCSSString; AutoCreate: boolean = false): TCSSNumericalID; override;
    function GetAttributeDesc(AttrId: TCSSNumericalID): TCSSAttributeDesc; override;
    function GetDeclarationValue(Decl: TCSSDeclarationElement): TCSSString; virtual;
  public
    property Options: TCSSResolverOptions read FOptions write SetOptions;
    property StringComparison: TCSSResStringComparison read FStringComparison;
  public
    // stylesheets
    procedure ClearStyleSheets; virtual;
    function AddStyleSheet(anOrigin: TCSSOrigin; const aName: TCSSString; const aSource: TCSSString): TStyleSheet; virtual;
    procedure ReplaceStyleSheet(Index: integer; const NewSource: TCSSString); virtual;
    function IndexOfStyleSheetWithElement(El: TCSSElement): integer;
    function IndexOfStyleSheetWithName(anOrigin: TCSSOrigin; const aName: TCSSString): integer;
    function FindStyleSheetWithElement(El: TCSSElement): TStyleSheet;
    property StyleSheetCount: integer read FStyleSheetCount;
    property StyleSheets[Index: integer]: TStyleSheet read GetStyleSheets;
    property Layers: TLayerArray read FLayers;
  public
    // logging
    procedure Log(MsgType: TEventType; const ID: TCSSMsgID; const Msg: TCSSString; PosEl: TCSSElement); virtual;
    procedure LogWarning(IsError: boolean; const ID: TCSSMsgID; const Msg: TCSSString; PosEl: TCSSElement); virtual;
    property LogCount: integer read GetLogCount;
    property LogEntries[Index: integer]: TCSSResolverLogEntry read GetLogEntries;
    property OnLog: TCSSResolverLogEvent read FOnLog write FOnLog;
  end;

function ComparePointer(Data1, Data2: Pointer): integer;
function CompareCSSSharedRuleArrays(const Rules1, Rules2: TCSSSharedRuleArray): integer;
function CompareCSSSharedRuleLists(A, B: Pointer): integer;
function CompareRulesArrayWithCSSSharedRuleList(RuleArray, SharedRuleList: Pointer): integer;


implementation

function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

function CompareCSSSharedRuleArrays(const Rules1, Rules2: TCSSSharedRuleArray): integer;
var
  Len1, Len2, i: Integer;
  R1, R2: PCSSSharedRule;
begin
  Len1:=length(Rules1);
  Len2:=length(Rules2);
  if Len1>Len2 then exit(1)
  else if Len1<Len2 then exit(-1);
  if Len1=0 then exit(0);
  R1:=@Rules1[0];
  R2:=@Rules2[0];
  for i:=0 to Len1-1 do
  begin
    if R1^.Specificity>R2^.Specificity then exit(1)
    else if R1^.Specificity<R2^.Specificity then exit(-1);
    Result:=ComparePointer(R1^.Rule,R2^.Rule);
    if Result<>0 then exit;
    inc(R1);
    inc(R2);
  end;
  Result:=0;
end;

function CompareCSSSharedRuleLists(A, B: Pointer): integer;
var
  List1: TCSSSharedRuleList absolute A;
  List2: TCSSSharedRuleList absolute B;
begin
  Result:=CompareCSSSharedRuleArrays(List1.Rules,List2.Rules);
end;

function CompareRulesArrayWithCSSSharedRuleList(RuleArray,
  SharedRuleList: Pointer): integer;
var
  Arr: TCSSSharedRuleArray absolute RuleArray;
  List: TCSSSharedRuleList absolute SharedRuleList;
begin
  Result:=CompareCSSSharedRuleArrays(Arr,List.Rules);
end;

{ TCSSResolverNthChildParams }

destructor TCSSResolverNthChildParams.Destroy;
var
  i: Integer;
begin
  for i:=0 to high(StackCache) do
    StackCache[i].Free;
  inherited Destroy;
end;

{ TCSSSharedRuleList }

destructor TCSSSharedRuleList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCSSSharedRuleList.Clear;
begin
  Rules:=nil;
end;

function TCSSSharedRuleList.Clone: TCSSSharedRuleList;
var
  l: SizeInt;
begin
  Result:=TCSSSharedRuleList.Create;
  Result.AllDecl:=AllDecl;
  Result.AllSpecificity:=AllSpecificity;

  l:=length(Rules);
  if l>0 then
  begin
    SetLength(Result.Rules,l);
    System.Move(Rules[0],Result.Rules[0],SizeOf(TCSSSharedRule)*l);
  end;

  l:=length(Values);
  if l>0 then
  begin
    SetLength(Result.Values,l);
    System.Move(Values[0],Result.Values[0],SizeOf(TCSSResolvedAttribute)*l);
  end;
end;

function TCSSSharedRuleList.IndexOfAttr(AttrId: TCSSNumericalID;
  ForInsert: boolean): integer;
var
  Cnt, l, r: Integer;
  CurAttrID: TCSSNumericalID;
begin
  Cnt:=length(Values);
  l:=0;
  r:=Cnt-1;
  while r>=l do
  begin
    Result:=(l+r) shr 1;
    CurAttrID:=Values[Result].AttrID;
    if CurAttrID>AttrId then
      l:=Result+1
    else if CurAttrID<AttrId then
      r:=Result-1
    else
      exit;
  end;
  if ForInsert then
    Result:=l
  else
    Result:=-1;
end;

{ TCSSAttributeValues }

procedure TCSSAttributeValues.SortValues;

  procedure QuickSort(L, R : integer);
  var
    I, J, PivotIdx : integer;
    AttrP: TCSSNumericalID;
    V: TCSSAttributeValue;
  begin
    repeat
      I := L;
      J := R;
      PivotIdx := L + ((R - L) shr 1); { same as ((L + R) div 2), but without the possibility of overflow }
      AttrP := Values[PivotIdx].AttrID;
      repeat
        while (I < PivotIdx) and (AttrP > Values[i].AttrID) do
          Inc(I);
        while (J > PivotIdx) and (AttrP < Values[J].AttrID) do
          Dec(J);
        if I < J then
        begin
          V := Values[I];
          Values[I] := Values[J];
          Values[J] := V;
          if PivotIdx = I then
          begin
            PivotIdx := J;
            Inc(I);
          end
          else if PivotIdx = J then
          begin
            PivotIdx := I;
            Dec(J);
          end
          else
          begin
            Inc(I);
            Dec(J);
          end;
        end;
      until I >= J;
      // sort the smaller range recursively
      // sort the bigger range via the loop
      // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
      if (PivotIdx - L) < (R - PivotIdx) then
      begin
        if (L + 1) < PivotIdx then
          QuickSort(L, PivotIdx - 1);
        L := PivotIdx + 1;
      end
      else
      begin
        if (PivotIdx + 1) < R then
          QuickSort(PivotIdx + 1, R);
        if (L + 1) < PivotIdx then
          R := PivotIdx - 1
        else
          exit;
      end;
    until L >= R;
  end;

var
  l: SizeInt;
  i, j: Integer;
begin
  l:=length(Values);
  if l<6 then
  begin
    for i:=0 to l-2 do
      for j:=i+1 to l-1 do
        if Values[i].AttrID>Values[j].AttrID then
          SwapValues(i,j);
  end else begin
    //for i:=0 to l-1 do
    //  writeln('TCSSAttributeValues.SortValues ',i,' ',Values[i]<>nil);
    QuickSort(0,l-1);
    for i:=0 to l-2 do
      if Values[i].AttrID>=Values[i+1].AttrID then
        raise Exception.Create('20240816160749');
  end;
end;

procedure TCSSAttributeValues.SwapValues(Index1, Index2: integer);
var
  A, B: TCSSAttributeValue;
  AttrId: TCSSNumericalID;
  Value: TCSSString;
  aState: TCSSAttributeValue.TState;
begin
  A:=Values[Index1];
  B:=Values[Index2];

  AttrId:=A.AttrID;
  A.AttrID:=B.AttrID;
  B.AttrID:=AttrID;

  Value:=A.Value;
  A.Value:=B.Value;
  B.Value:=Value;

  aState:=A.State;
  A.State:=B.State;
  B.State:=aState;
end;

function TCSSAttributeValues.IndexOf(AttrID: TCSSNumericalID): integer;
var
  l, r, m: Integer;
  Diff: TCSSNumericalID;
begin
  l:=0;
  r:=length(Values)-1;
  while l<=r do
  begin
    m:=(l+r) shr 1;
    Diff:=Values[m].AttrID-AttrID;
    if Diff>0 then
      r:=m-1
    else if Diff<0 then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;

procedure TCSSAttributeValues.SetComputedValue(AttrID: TCSSNumericalID; const aValue: TCSSString);

  procedure AddNew;
  var
    Item: TCSSAttributeValue;
    i, l: integer;
  begin
    l:=length(Values);
    i:=l;
    while (i>0) and (Values[i-1].AttrID>AttrID) do dec(i);
    Item:=TCSSAttributeValue.Create;
    Item.AttrID:=AttrID;
    Item.State:=cavsComputed;
    Item.Value:=aValue;
    System.Insert(Item,Values,i);
  end;

var
  i: Integer;
begin
  if AttrID<=CSSAttributeID_All then
    raise Exception.Create('20240729084610');
  if Values=nil then
  begin
    AddNew;
  end else begin
    i:=IndexOf(AttrID);
    if i>=0 then
    begin
      Values[i].State:=cavsComputed;
      Values[i].Value:=aValue;
    end else begin
      AddNew;
    end;
  end;
end;

destructor TCSSAttributeValues.Destroy;
var
  i: Integer;
begin
  for i:=0 to length(Values)-1 do
    Values[i].Free;
  Values:=nil;
  inherited Destroy;
end;

{ TCSSResolver }

function TCSSResolver.GetLogCount: integer;
begin
  Result:=FLogEntries.Count;
end;

function TCSSResolver.GetCustomAttributes(Index: TCSSNumericalID): TCSSAttributeDesc;
begin
  Result:=FCustomAttributes[Index];
end;

function TCSSResolver.GetLogEntries(Index: integer): TCSSResolverLogEntry;
begin
  Result:=TCSSResolverLogEntry(FLogEntries[Index]);
end;

function TCSSResolver.GetStyleSheets(Index: integer): TStyleSheet;
begin
  Result:=FStyleSheets[Index];
end;

procedure TCSSResolver.SetOptions(const AValue: TCSSResolverOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

procedure TCSSResolver.ParseSource(Index: integer);

  procedure AddOrigin(LayerIndex: integer; Origin: TCSSOrigin);
  // inserts a anonymous layer
  var
    aLayer: TLayer;
  begin
    aLayer:=Default(TLayer);
    aLayer.Origin:=Origin;
    System.Insert(aLayer,FLayers,LayerIndex);
  end;

var
  Src: TCSSString;
  El: TCSSElement;
  LayerIndex: Integer;
  Cnt: SizeInt;
  aStyleSheet: TStyleSheet;
begin
  aStyleSheet:=FStyleSheets[Index];
  if aStyleSheet.Parsed then exit;
  aStyleSheet.Parsed:=true;
  if aStyleSheet.Element<>nil then
    raise ECSSResolver.Create('20240624200924');

  // parse
  Src:=aStyleSheet.Source;
  if Src='' then
    exit;
  //writeln('TCSSResolver.ParseSource [',Src,'] ',StringCodePage(Src));
  El:=ParseCSSSource(Src,false);
  if El=nil then exit;
  aStyleSheet.Element:=El;

  // find last layer with this Origin or lower
  LayerIndex:=length(FLayers);
  while (LayerIndex>0) and (FLayers[LayerIndex-1].Origin>aStyleSheet.Origin) do
    dec(LayerIndex);
  if (LayerIndex=length(FLayers)) or (FLayers[LayerIndex].Origin<>aStyleSheet.Origin) then
    AddOrigin(LayerIndex,aStyleSheet.Origin);

  with FLayers[LayerIndex] do
  begin
    Cnt:=length(Elements);
    if Cnt=ElementCount then
    begin
      if Cnt<8 then
        Cnt:=8
      else
        Cnt:=Cnt*2;
      SetLength(Elements,Cnt);
      FillByte(Elements[ElementCount],SizeOf(TLayerElement)*(Cnt-ElementCount),0);
    end;
    Elements[ElementCount].Src:=aStyleSheet;
    Elements[ElementCount].Element:=El;
    inc(ElementCount);
  end;
end;

function TCSSResolver.ParseCSSSource(const Src: TCSSString; Inline: boolean
  ): TCSSElement;
var
  ms: TMemoryStream;
  aParser: TCSSResolverParser;
begin
  Result:=nil;
  if Src='' then
    exit;
  if CSSRegistry=nil then
    raise ECSSResolver.Create('20240630203634');

  if (FCSSRegistryStamp>0) then
  begin
    if (FCSSRegistryStamp<>CSSRegistry.Stamp) then
      raise ECSSResolver.Create('20240822143309 Clear was not called after changing CSSRegistry');
  end else
    FCSSRegistryStamp:=CSSRegistry.Stamp;

  aParser:=nil;
  ms:=TMemoryStream.Create;
  try
    ms.Write(Src[1],length(Src)*SizeOf(TCSSChar));
    ms.Position:=0;
    aParser:=TCSSResolverParser.Create(ms); // ss is freed by the parser
    aParser.Resolver:=Self;
    aParser.OnLog:=@Log;
    aParser.CSSNthChildParamsClass:=TCSSResolverNthChildParams;
    if Inline then
      Result:=aParser.ParseInline
    else
      Result:=aParser.Parse;
  finally
    aParser.Free;
    ms.Free;
  end;
end;

procedure TCSSResolver.ClearElements;
var
  i: Integer;
begin
  FLogEntries.Clear;

  ClearMerge;
  ClearSharedRuleLists;
  ClearCustomAttributes;

  // clear layers
  for i:=0 to length(FLayers)-1 do
  begin
    FLayers[i].ElementCount:=0;
    FLayers[i].Elements:=nil;
    FLayers[i].Name:='';
  end;
  FLayers:=nil;

  for i:=0 to FStyleSheetCount-1 do
    FreeAndNil(FStyleSheets[i].Element);

  // not referencing CSSRegistry anymore
  FCSSRegistryStamp:=0;
end;

procedure TCSSResolver.ClearCustomAttributes;
var
  i: Integer;
begin
  for i:=0 to FCustomAttributeCount-1 do
    FreeAndNil(FCustomAttributes[i]);
  FCustomAttributeCount:=0;
  FCustomAttributeNameToDesc.Clear;
end;

procedure TCSSResolver.AddRule(aRule: TCSSRuleElement; Specificity: TCSSSpecificity
  );
var
  l: SizeInt;
  i: Integer;
begin
  if aRule=nil then
    raise ECSSResolver.Create('20231110202417');
  l:=length(FElRules);
  if FElRuleCount=l then
  begin
    if l<8 then
      l:=8
    else
      l:=l*2;
    Setlength(FElRules,l);
  end;
  i:=FElRuleCount;
  FElRules[i].Rule:=aRule;
  FElRules[i].Specificity:=Specificity;
  inc(FElRuleCount);
end;

procedure TCSSResolver.ComputeElement(El: TCSSElement);
var
  C: TClass;
  Compound: TCSSCompoundElement;
  i: Integer;
begin
  if El=nil then exit;
  C:=El.ClassType;
  {$IFDEF VerboseCSSResolver}
  //writeln('TCSSResolver.ComputeElement ',GetCSSPath(El));
  {$ENDIF}
  if C=TCSSCompoundElement then
  begin
    Compound:=TCSSCompoundElement(El);
    //writeln('TCSSResolver.ComputeElement Compound.ChildCount=',Compound.ChildCount);
    for i:=0 to Compound.ChildCount-1 do
      ComputeElement(Compound.Children[i]);
  end else if C=TCSSRuleElement then
    ComputeRule(TCSSRuleElement(El))
  else
    Log(etWarning,20220908150252,'TCSSResolver.ComputeElement: Unknown CSS element',El);
end;

procedure TCSSResolver.ComputeRule(aRule: TCSSRuleElement);
var
  i: Integer;
  BestSpecificity, Specificity: TCSSSpecificity;
  aSelector: TCSSElement;
begin
  BestSpecificity:=CSSSpecificityNoMatch;
  for i:=0 to aRule.SelectorCount-1 do
  begin
    aSelector:=aRule.Selectors[i];
    Specificity:=SelectorMatches(aSelector,FNode,false);
    if Specificity>BestSpecificity then
      BestSpecificity:=Specificity;
  end;
  if BestSpecificity>=0 then
  begin
    // match -> add rule to ruleset
    AddRule(aRule,BestSpecificity);
  end;
end;

function TCSSResolver.FindSharedRuleList(const Rules: TCSSSharedRuleArray
  ): TCSSSharedRuleList;
var
  Node: TAVLTreeNode;
begin
  Node:=FSharedRuleLists.FindKey(Pointer(Rules),@CompareRulesArrayWithCSSSharedRuleList);
  if Node<>nil then
    Result:=TCSSSharedRuleList(Node.Data)
  else
    Result:=nil;
end;

function TCSSResolver.CreateSharedRuleList: TCSSSharedRuleList;
var
  i, j: Integer;
  RuleArr: TCSSSharedRule;
  Rule: TCSSRuleElement;
  Specificity: TCSSSpecificity;
  RuleI, RuleJ: PCSSSharedRule;
begin
  SetLength(FElRules,FElRuleCount); // needed by FindSharedRuleList

  // sort ascending for Specificity
  for i:=0 to FElRuleCount-2 do
  begin
    RuleI:=@FElRules[i];
    for j:=i+1 to FElRuleCount-1 do
    begin
      RuleJ:=@FElRules[j];
      if RuleI^.Specificity>RuleJ^.Specificity then
      begin
        Specificity:=RuleI^.Specificity;
        RuleI^.Specificity:=RuleJ^.Specificity;
        RuleJ^.Specificity:=Specificity;
        Rule:=RuleI^.Rule;
        RuleI^.Rule:=RuleJ^.Rule;
        RuleJ^.Rule:=Rule;
      end;
    end;
  end;

  Result:=FindSharedRuleList(FElRules);
  if Result<>nil then
  begin
    // already exists
    LoadSharedMergedAttributes(Result);
  end else begin
    // add new shared rule list
    Result:=TCSSSharedRuleList.Create;
    Result.Rules:=copy(FElRules,0,FElRuleCount);
    FSharedRuleLists.Add(Result);

    // merge shared properties
    ClearMerge;
    for i:=0 to length(Result.Rules)-1 do
    begin
      RuleArr:=Result.Rules[i];
      Rule:=RuleArr.Rule;
      Specificity:=RuleArr.Specificity;
      for j:=0 to Rule.ChildCount-1 do
        MergeAttribute(Rule.Children[j],Specificity);
    end;
    SaveSharedMergedAttributes(Result);
  end;
end;

procedure TCSSResolver.ClearMerge;
var
  i: Integer;
begin
  if FMergedAttributesStamp=high(FMergedAttributesStamp) then
  begin
    FMergedAttributesStamp:=1;
    for i:=0 to length(FMergedAttributes)-1 do
      FMergedAttributes[i].Stamp:=0;
  end else
    inc(FMergedAttributesStamp);
  FMergedAllDecl:=nil;
  FMergedAllSpecificity:=CSSSpecificityInvalid;
  FMergedAttributeFirst:=0;
  FMergedAttributeLast:=0;
end;

procedure TCSSResolver.InitMerge;
var
  OldLen, NewLen: TCSSNumericalID;
begin
  if FCustomAttributeCount>0 then
  begin
    if FCustomAttributes[0].Index<>CSSRegistry.AttributeCount then
      raise ECSSResolver.Create('20240822142652');
  end;

  OldLen:=length(FMergedAttributes);
  NewLen:=CSSRegistry.AttributeCount+FCustomAttributeCount;
  if NewLen>OldLen then
  begin
    SetLength(FMergedAttributes,NewLen);
    FillByte(FMergedAttributes[OldLen],(NewLen-OldLen)*SizeOf(TMergedAttribute),0);
  end;
end;

procedure TCSSResolver.SetMergedAttribute(AttrID, aSpecificity: TCSSNumericalID;
  DeclEl: TCSSDeclarationElement);
var
  AttrP: PMergedAttribute;
begin
  if AttrID<=0 then
    raise ECSSResolver.Create('20240701120038');
  if AttrID>=length(FMergedAttributes) then
    raise ECSSResolver.Create('20240823095544');

  AttrP:=@FMergedAttributes[AttrID];
  AttrP^.Specificity:=aSpecificity;
  AttrP^.DeclEl:=DeclEl;
  if AttrP^.Stamp<>FMergedAttributesStamp then
  begin
    if FMergedAttributeFirst>0 then
    begin
      // append to double linked list
      FMergedAttributes[FMergedAttributeLast].Next:=AttrID;
      AttrP^.Prev:=FMergedAttributeLast;
      FMergedAttributeLast:=AttrID;
    end else begin
      // start double linked list
      FMergedAttributeFirst:=AttrID;
      FMergedAttributeLast:=AttrID;
      AttrP^.Prev:=0;
    end;
    AttrP^.Next:=0;
    AttrP^.Stamp:=FMergedAttributesStamp;
  end;
end;

procedure TCSSResolver.RemoveMergedAttribute(AttrID: TCSSNumericalID);
var
  AttrP: PMergedAttribute;
begin
  AttrP:=@FMergedAttributes[AttrID];
  if AttrP^.Stamp<>FMergedAttributesStamp then exit;
  AttrP^.Stamp:=0;
  if FMergedAttributeFirst=AttrID then
    FMergedAttributeFirst:=AttrP^.Next;
  if FMergedAttributeLast=AttrID then
    FMergedAttributeLast:=AttrP^.Prev;
  if AttrP^.Next>0 then
    FMergedAttributes[AttrP^.Next].Prev:=AttrP^.Prev;
  if AttrP^.Prev>0 then
    FMergedAttributes[AttrP^.Prev].Next:=AttrP^.Next;
  AttrP^.Next:=0;
  AttrP^.Prev:=0;
end;

function TCSSResolver.SelectorMatches(aSelector: TCSSElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  C: TClass;
begin
  Result:=CSSSpecificityInvalid;
  //writeln('TCSSResolver.SelectorMatches ',aSelector.ClassName,' ',TestNode.GetCSSTypeName);
  C:=aSelector.ClassType;
  if C=TCSSResolvedIdentifierElement then
    Result:=SelectorIdentifierMatches(TCSSResolvedIdentifierElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSHashIdentifierElement then
    Result:=SelectorHashIdentifierMatches(TCSSHashIdentifierElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSClassNameElement then
    Result:=SelectorClassNameMatches(TCSSClassNameElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSResolvedPseudoClassElement then
    Result:=SelectorPseudoClassMatches(TCSSResolvedPseudoClassElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSUnaryElement then
    Result:=SelectorUnaryMatches(TCSSUnaryElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSBinaryElement then
    Result:=SelectorBinaryMatches(TCSSBinaryElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSArrayElement then
    Result:=SelectorArrayMatches(TCSSArrayElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSListElement then
    Result:=SelectorListMatches(TCSSListElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSResolvedCallElement then
    Result:=SelectorCallMatches(TCSSResolvedCallElement(aSelector),TestNode,OnlySpecificity)
  else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20240625131810,'Unknown CSS selector element '+aSelector.ClassName,aSelector);
    {$ENDIF}
  end;
end;

function TCSSResolver.SelectorIdentifierMatches(
  Identifier: TCSSResolvedIdentifierElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
var
  TypeID: TCSSNumericalID;
begin
  Result:=CSSSpecificityNoMatch;
  TypeID:=Identifier.NumericalID;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorIdentifierMatches ',Identifier.Value,' TypeId=',TypeID,' Node=',TestNode.GetCSSTypeID);
  {$ENDIF}
  if TypeID=CSSTypeID_Universal then
    // universal selector
    Result:=CSSSpecificityUniversal+FSourceSpecificity
  else if OnlySpecificity then
    Result:=CSSSpecificityType+FSourceSpecificity
  else if TypeID=CSSIDNone then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20240625153922,'Unknown type ',Identifier);
    {$ENDIF}
    Result:=CSSSpecificityInvalid;
  end else if TypeID=TestNode.GetCSSTypeID then
    Result:=CSSSpecificityType+FSourceSpecificity;
end;

function TCSSResolver.SelectorHashIdentifierMatches(
  Identifier: TCSSHashIdentifierElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
var
  aValue: TCSSString;
begin
  if OnlySpecificity then
    exit(CSSSpecificityIdentifier+FSourceSpecificity);
  Result:=CSSSpecificityNoMatch;
  aValue:=Identifier.Value;
  if TestNode.GetCSSID=aValue then
    Result:=CSSSpecificityIdentifier+FSourceSpecificity;
end;

function TCSSResolver.SelectorClassNameMatches(
  aClassName: TCSSClassNameElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
var
  aValue: TCSSString;
begin
  if OnlySpecificity then
    exit(CSSSpecificityClass+FSourceSpecificity);
  aValue:=aClassName.Name;
  if TestNode.HasCSSClass(aValue) then
    Result:=CSSSpecificityClass+FSourceSpecificity
  else
    Result:=CSSSpecificityNoMatch;
  //writeln('TCSSResolver.SelectorClassNameMatches ',aValue,' ',Result);
end;

function TCSSResolver.SelectorPseudoClassMatches(aPseudoClass: TCSSResolvedPseudoClassElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  PseudoID: TCSSNumericalID;
begin
  if OnlySpecificity then
    exit(CSSSpecificityClass+FSourceSpecificity);
  Result:=CSSSpecificityNoMatch;
  PseudoID:=aPseudoClass.NumericalID;
  case PseudoID of
  CSSIDNone:
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20240625153950,'Unknown pseudo class',aPseudoClass);
      {$ENDIF}
    end;
  CSSPseudoID_Root:
    if TestNode.GetCSSParent=nil then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  CSSPseudoID_Empty:
    if TestNode.GetCSSEmpty then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  CSSPseudoID_FirstChild:
    if TestNode.GetCSSPreviousSibling=nil then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  CSSPseudoID_LastChild:
    if TestNode.GetCSSNextSibling=nil then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  CSSPseudoID_OnlyChild:
    if (TestNode.GetCSSNextSibling=nil)
        and (TestNode.GetCSSPreviousSibling=nil) then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  CSSPseudoID_FirstOfType:
    if TestNode.GetCSSPreviousOfType=nil then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  CSSPseudoID_LastOfType:
    if TestNode.GetCSSNextOfType=nil then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  CSSPseudoID_OnlyOfType:
    if (TestNode.GetCSSNextOfType=nil)
        and (TestNode.GetCSSPreviousOfType=nil) then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  else
    if TestNode.HasCSSPseudoClass(PseudoID) then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  end;
end;

function TCSSResolver.SelectorListMatches(aList: TCSSListElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  i: Integer;
  El: TCSSElement;
  C: TClass;
  Specificity: TCSSSpecificity;
  aNode: ICSSNode;
begin
  Result:=0;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorListMatches ChildCount=',aList.ChildCount);
  {$ENDIF}
  aNode:=TestNode;
  for i:=0 to aList.ChildCount-1 do
  begin
    El:=aList.Children[i];
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolver.SelectorListMatches ',i,' ',GetCSSObj(El),' AsString=',El.AsString);
    {$ENDIF}
    C:=El.ClassType;
    if (C=TCSSResolvedIdentifierElement) and (i>0) then
    begin
      if OnlySpecificity then
        exit(0);
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20240625154031,'Type selector must be first',aList);
      {$ENDIF}
      exit(CSSSpecificityInvalid);
    end else
      Specificity:=SelectorMatches(El,aNode,OnlySpecificity);
    if Specificity<0 then
      exit(Specificity);
    inc(Result,Specificity);
  end;
end;

function TCSSResolver.SelectorUnaryMatches(aUnary: TCSSUnaryElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
begin
  Result:=CSSSpecificityInvalid;
  case aUnary.Operation of
  uoDoubleColon:
    begin
      // ::PseudoElement
      if OnlySpecificity then
        // treat as Type::PseudoElement
        Result:=CSSSpecificityType+FSourceSpecificity
               +CSSSpecificityType+FSourceSpecificity
      else
        Result:=SelectorPseudoElementMatches(nil,aUnary.Right,TestNode);
    end;
  else
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20250225103026,'Invalid CSS unary selector '+UnaryOperators[aUnary.Operation],aUnary);
    {$ENDIF}
  end;
end;

function TCSSResolver.SelectorBinaryMatches(aBinary: TCSSBinaryElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  aParent, Sibling: ICSSNode;
  aSpecificity: TCSSSpecificity;
begin
  if OnlySpecificity then
  begin
    Result:=SelectorMatches(aBinary.Left,TestNode,true);
    inc(Result,SelectorMatches(aBinary.Right,TestNode,true));
    exit;
  end;

  Result:=CSSSpecificityInvalid;
  case aBinary.Operation of
  boGT:
    begin
      // child combinator >
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      aParent:=TestNode.GetCSSParent;
      if aParent=nil then
        exit(CSSSpecificityNoMatch);
      aSpecificity:=SelectorMatches(aBinary.Left,aParent,false);
      if aSpecificity<0 then
        exit(aSpecificity);
      inc(Result,aSpecificity);
    end;
  boPlus:
    begin
      // adjacent sibling combinator +
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      Sibling:=TestNode.GetCSSPreviousSibling;
      if Sibling=nil then
        exit(CSSSpecificityNoMatch);
      aSpecificity:=SelectorMatches(aBinary.Left,Sibling,false);
      if aSpecificity<0 then
        exit(aSpecificity);
      inc(Result,aSpecificity);
    end;
  boTilde:
    begin
      // general sibling combinator ~
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      Sibling:=TestNode.GetCSSPreviousSibling;
      while Sibling<>nil do
      begin
        aSpecificity:=SelectorMatches(aBinary.Left,Sibling,false);
        if aSpecificity=CSSSpecificityInvalid then
          exit(aSpecificity)
        else if aSpecificity>=0 then
        begin
          inc(Result,aSpecificity);
          exit;
        end;
        Sibling:=Sibling.GetCSSPreviousSibling;
      end;
      Result:=CSSSpecificityNoMatch;
    end;
  boWhiteSpace:
    begin
      // descendant combinator
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      aParent:=TestNode;
      repeat
        aParent:=aParent.GetCSSParent;
        if aParent=nil then
          exit(CSSSpecificityNoMatch);
        aSpecificity:=SelectorMatches(aBinary.Left,aParent,false);
        if aSpecificity>=0 then
        begin
          inc(Result,aSpecificity);
          exit;
        end
        else if aSpecificity=CSSSpecificityInvalid then
          exit(CSSSpecificityInvalid);
      until false;
    end;
  boDoubleColon:
    Result:=SelectorPseudoElementMatches(aBinary.Left,aBinary.Right,TestNode);
  else
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20240625154050,'Invalid CSS binary selector '+BinaryOperators[aBinary.Operation],aBinary);
    {$ENDIF}
  end;
end;

function TCSSResolver.SelectorPseudoElementMatches(aLeft, aRight: TCSSElement;
  const TestNode: ICSSNode): TCSSSpecificity;
// pseudo element (function)
var
  ID: TCSSNumericalID;
  aParent: ICSSNode;
  aSpecificity: TCSSSpecificity;
begin
  Result:=CSSSpecificityInvalid;
  if aRight is TCSSResolvedIdentifierElement then
  begin
    // pseudo element
    ID:=TCSSResolvedIdentifierElement(aRight).NumericalID;
    if ID<=0 then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20250224211914,'Invalid CSS pseudo element',aRight);
      {$ENDIF}
      exit;
    end;
    if ID<>TestNode.GetCSSPseudoElementID then
      exit(CSSSpecificityNoMatch);
    Result:=CSSSpecificityIdentifier;
  end else if aRight is TCSSResolvedCallElement then begin
    // pseudo element function
    ID:=TCSSResolvedCallElement(aRight).NameNumericalID;
    if ID<0 then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20250224212143,'Invalid CSS pseudo element function',aRight);
      {$ENDIF}
      exit;
    end;
    if ID<>TestNode.GetCSSPseudoElementID then
      exit(CSSSpecificityNoMatch);
    // todo: check parameters
    Result:=CSSSpecificityIdentifier;
  end else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20250224212301,'Invalid CSS pseudo element',aRight);
    {$ENDIF}
  end;

  if aLeft=nil then
    exit; // unary ::Name

  // test left side
  aParent:=TestNode.GetCSSParent;
  if aParent=nil then
    exit(CSSSpecificityNoMatch);
  aSpecificity:=SelectorMatches(aLeft,aParent,false);
  if aSpecificity<0 then
    exit(aSpecificity);
  inc(Result,aSpecificity);
end;

function TCSSResolver.SelectorArrayMatches(anArray: TCSSArrayElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  {$IFDEF VerboseCSSResolver}
  i: integer;
  {$ENDIF}
  El: TCSSElement;
  C: TClass;
  AttrID: TCSSNumericalID;
  OldStringComparison: TCSSResStringComparison;
  aValue: TCSSString;
begin
  if OnlySpecificity then
    exit(CSSSpecificityClass+FSourceSpecificity);

  Result:=CSSSpecificityInvalid;
  if anArray.Prefix<>nil then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910154004,'Invalid CSS attribute selector prefix',anArray.Prefix);
    {$ENDIF}
    exit;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayMatches Prefix=',GetCSSObj(anArray.Prefix),' ChildCount=',anArray.ChildCount);
  for i:=0 to anArray.ChildCount-1 do
    writeln('TCSSResolver.SelectorArrayMatches ',i,' ',GetCSSObj(anArray.Children[i]));
  {$ENDIF}
  if anArray.ChildCount<1 then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910154033,'Invalid CSS attribute selector',anArray);
    {$ENDIF}
    exit;
  end;
  OldStringComparison:=StringComparison;
  try
    if anArray.ChildCount>1 then
    begin
      El:=anArray.Children[1];
      C:=El.ClassType;
      if C=TCSSResolvedIdentifierElement then
      begin
        aValue:=TCSSResolvedIdentifierElement(El).Value;
        case aValue of
        'i': FStringComparison:=crscCaseInsensitive;
        's': FStringComparison:=crscCaseSensitive;
        else
          // already warned by parser
          {$IFDEF VerboseCSSResolver}
          LogWarning(croErrorOnUnknownName in Options,20220914174409,'Invalid attribute modifier "'+aValue+'"',El);
          {$ENDIF}
          exit;
        end;
      end else begin
        // already warned by parser
        {$IFDEF VerboseCSSResolver}
        Log(etWarning,20220914173643,'Invalid CSS attribute modifier',El);
        {$ENDIF}
        exit;
      end;
    end;
    if (anArray.ChildCount>2) then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20220914174550,'Invalid CSS attribute modifier',anArray.Children[2]);
      {$ENDIF}
    end;

    El:=anArray.Children[0];
    C:=El.ClassType;
    if C=TCSSResolvedIdentifierElement then
    begin
      // [name]  ->  has explicit attribute
      AttrID:=TCSSResolvedIdentifierElement(El).NumericalID;
      case AttrID of
      CSSIDNone:
        Result:=CSSSpecificityNoMatch;
      CSSAttributeID_ID,
      CSSAttributeID_Class:
        // id and class are always defined
        Result:=CSSSpecificityClass+FSourceSpecificity;
      CSSAttributeID_All:
        // special CSS attributes without a value
        Result:=CSSSpecificityNoMatch;
      else
        if TestNode.HasCSSExplicitAttribute(AttrID) then
          Result:=CSSSpecificityClass+FSourceSpecificity
        else
          Result:=CSSSpecificityNoMatch;
      end;
    end else if C=TCSSBinaryElement then
      Result:=SelectorArrayBinaryMatches(TCSSBinaryElement(El),TestNode)
    else begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      LogWarning(croErrorOnUnknownName in Options,20220910153725,'Invalid CSS array selector',El);
      {$ENDIF}
    end;
  finally
    FStringComparison:=OldStringComparison;
  end;
end;

function TCSSResolver.SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement;
  const TestNode: ICSSNode): TCSSSpecificity;
var
  Left, Right: TCSSElement;
  AttrID: TCSSNumericalID;
  LeftValue, RightValue: TCSSString;
  C: TClass;
begin
  Result:=CSSSpecificityNoMatch;
  Left:=aBinary.Left;
  if Left.ClassType<>TCSSResolvedIdentifierElement then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910164353,'Invalid CSS array selector, expected attribute',Left);
    {$ENDIF}
    exit;
  end;
  AttrID:=TCSSResolvedIdentifierElement(Left).NumericalID;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches AttrID=',AttrID,' Value=',TCSSResolvedIdentifierElement(Left).Value);
  {$ENDIF}
  case AttrID of
  CSSIDNone: exit(CSSSpecificityNoMatch);
  CSSAttributeID_ID:
    LeftValue:=TestNode.GetCSSID;
  CSSAttributeID_Class:
    LeftValue:=TestNode.GetCSSAttributeClass;
  CSSAttributeID_All:
    exit(CSSSpecificityNoMatch);
  else
    LeftValue:=TestNode.GetCSSExplicitAttribute(AttrID);
  end;

  Right:=aBinary.Right;
  C:=Right.ClassType;
  if (C=TCSSStringElement) or (C=TCSSIntegerElement) or (C=TCSSFloatElement)
      or (C=TCSSResolvedIdentifierElement) then
    // ok
  else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910164921,'Invalid CSS array selector, expected string',Right);
    {$ENDIF}
    exit;
  end;
  RightValue:=ComputeValue(Right);

  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Left="',LeftValue,'" Right="',RightValue,'" Op=',aBinary.Operation);
  {$ENDIF}
  case aBinary.Operation of
  boEquals:
    if SameValueText(LeftValue,RightValue) then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  boSquaredEqual:
    // begins with
    if (RightValue<>'') and SameValueText(LeftStr(LeftValue,length(RightValue)),RightValue) then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  boDollarEqual:
    // ends with
    if (RightValue<>'') and SameValueText(RightStr(LeftValue,length(RightValue)),RightValue) then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  boPipeEqual:
    // equal to or starts with name-hyphen
    if (RightValue<>'')
        and (SameValueText(LeftValue,RightValue)
          or SameValueText(LeftStr(LeftValue,length(RightValue)+1),RightValue+'-')) then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  boStarEqual:
    // contains substring
    if (RightValue<>'') and (Pos(RightValue,LeftValue)>0) then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  boTildeEqual:
    // contains word
    if PosWord(RightValue,LeftValue)>0 then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  else
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910164356,'Invalid CSS array selector operator',aBinary);
    {$ENDIF}
    Result:=CSSSpecificityInvalid;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Result=',Result);
  {$ENDIF}
end;

function TCSSResolver.SelectorCallMatches(aCall: TCSSResolvedCallElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  CallID: TCSSNumericalID;
begin
  Result:=CSSSpecificityNoMatch;
  CallID:=aCall.NameNumericalID;
  //writeln('TCSSResolver.SelectorCallMatches ',CallID,' ',aCall.AsString);
  case CallID of
  CSSCallID_Not:
    Result:=Call_Not(aCall,TestNode,OnlySpecificity);
  CSSCallID_Is:
    Result:=Call_Is(aCall,TestNode,OnlySpecificity);
  CSSCallID_Where:
    Result:=Call_Where(aCall,TestNode,OnlySpecificity);
  CSSCallID_NthChild,
  CSSCallID_NthLastChild,
  CSSCallID_NthOfType,
  CSSCallID_NthLastOfType:
    Result:=Call_NthChild(CallID,aCall,TestNode,OnlySpecificity);
  else
    if OnlySpecificity then
      Result:=0
    else
      Result:=CSSSpecificityInvalid;
  end;
end;

function TCSSResolver.Call_Not(aCall: TCSSResolvedCallElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
// :not(arg1, arg2, ...)
// :not(args) has the same Specificity as :not(:is(args))
var
  i: Integer;
  Specificity: TCSSSpecificity;
  HasMatch: Boolean;
begin
  Result:=0;
  HasMatch:=false;
  for i:=0 to aCall.ArgCount-1 do
  begin
    Specificity:=SelectorMatches(aCall.Args[i],TestNode,OnlySpecificity);
    //writeln('TCSSResolver.Call_Not ',i,' ',TestNode.GetCSSTypeName,' Spec=',Specificity);
    if Specificity>=0 then
      HasMatch:=true
    else begin
      // the Specificity of ":not" is the highest, independent of matching (forgiving)
      if not OnlySpecificity then
        Specificity:=SelectorMatches(aCall.Args[i],TestNode,true);
    end;
    if Specificity>Result then
      Result:=Specificity;
  end;
  if OnlySpecificity then
    // return best
  else if HasMatch then
    Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.Call_Is(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
var
  i: Integer;
  Specificity: TCSSSpecificity;
  ok: Boolean;
begin
  Result:=0;
  //writeln('TCSSResolver.Call_Is START ',TestNode.GetCSSID,' ArgCount=',aCall.ArgCount);
  ok:=false;
  for i:=0 to aCall.ArgCount-1 do
  begin
    Specificity:=SelectorMatches(aCall.Args[i],TestNode,OnlySpecificity);
    //writeln('TCSSResolver.Call_Is i=',i,' ',TestNode.GetCSSID,' ',aCall.Args[i].AsString,' Spec=',Specificity);
    if Specificity>=0 then
      ok:=true
    else begin
      // the Specificity of :is is the highest, independent of matching (forgiving)
      if not OnlySpecificity then
        Specificity:=SelectorMatches(aCall.Args[i],TestNode,true);
    end;
    if Specificity>Result then
      Result:=Specificity;
  end;
  if (not ok) and (not OnlySpecificity) then
    Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.Call_Where(aCall: TCSSResolvedCallElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  i: Integer;
begin
  Result:=0;
  if OnlySpecificity then
    exit;
  for i:=0 to aCall.ArgCount-1 do
  begin
    if SelectorMatches(aCall.Args[i],TestNode,false)>=0 then
      // Note: :where is forgiving, so invalid arguments are ignored
      exit;
  end;
  Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.Call_NthChild(PseudoFuncID: TCSSNumericalID;
  aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean
  ): TCSSSpecificity;
var
  i: Integer;
  Params: TCSSResolverNthChildParams;
  ChildIDs: TIntegerDynArray;
begin
  Params:=aCall.Params as TCSSResolverNthChildParams;
  if Params=nil then
    exit(CSSSpecificityInvalid);

  if OnlySpecificity then
    Result:=CSSSpecificityClass+FSourceSpecificity
  else
    Result:=CSSSpecificityInvalid;

  if OnlySpecificity then
  begin
    if Params.OfSelector<>nil then
      inc(Result,SelectorMatches(Params.OfSelector,TestNode,true));
    exit;
  end;

  Result:=CSSSpecificityNoMatch;
  if Params.Modulo=0 then
    exit;
  i:=TestNode.GetCSSIndex;
  if Params.HasOf then
  begin
    ChildIDs:=CollectSiblingsOf(PseudoFuncID,TestNode,Params);
    i:=GetSiblingOfIndex(ChildIDs,i);
  end else
    ChildIDs:=nil;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.Call_NthChild CallID=',PseudoFuncID,' Node=',TestNode.GetCSSID,' ',Params.Modulo,' * N + ',Params.Start,' Index=',TestNode.GetCSSIndex,' i=',i,' HasOf=',Params.HasOf,' OfSelector=',GetCSSObj(Params.OfSelector));
  {$ENDIF}
  if i<0 then
    exit;
  if PseudoFuncID in [CSSCallID_NthLastChild,CSSCallID_NthLastOfType] then
  begin
    if Params.HasOf then
      i:=length(ChildIDs)-i
    else
      i:=GetSiblingCount(TestNode)-i;
  end else
  begin
    i:=i+1;
  end;
  dec(i,Params.Start);
  if i mod Params.Modulo = 0 then
  begin
    i:=i div Params.Modulo;
    if i>=0 then
      Result:=CSSSpecificityClass+FSourceSpecificity;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.Call_NthChild Node=',TestNode.GetCSSID,' ',Params.Modulo,' * N + ',Params.Start,' Index=',TestNode.GetCSSIndex+1,' i=',i,' Result=',Result);
  {$ENDIF}
end;

function TCSSResolver.CollectSiblingsOf(PseudoFuncID: TCSSNumericalID;
  TestNode: ICSSNode; Params: TCSSResolverNthChildParams): TIntegerDynArray;
var
  i, Depth, ChildCount, j: Integer;
  aTypeID: TCSSNumericalID;
  aParent, aNode: ICSSNode;
  aSelector: TCSSElement;
  StackDepth: SizeInt;
  Cache: TCSSResolverNthChildParamsCache;
  Item: PCSSNthChildParamsCacheItem;
  NeedTypeID: Boolean;
begin
  Result:=nil;
  aParent:=TestNode.GetCSSParent;
  {$IFDEF VerboseCSSResolver}
  //writeln('TCSSResolver.CollectSiblingsOf HasParent=',aParent<>nil);
  {$ENDIF}
  if aParent=nil then exit;
  ChildCount:=aParent.GetCSSChildCount;
  if ChildCount=0 then exit;

  Depth:=aParent.GetCSSDepth;
  StackDepth:=length(Params.StackCache);
  if StackDepth<=Depth then
  begin
    SetLength(Params.StackCache,Depth+1);
    for i:=StackDepth to Depth do
      Params.StackCache[i]:=nil;
  end;
  Cache:=Params.StackCache[Depth];
  if Cache=nil then
  begin
    Cache:=TCSSResolverNthChildParamsCache.Create;
    Params.StackCache[Depth]:=Cache;
    Cache.Owner:=Params;
    Cache.StackDepth:=Depth;
  end;

  NeedTypeID:=PseudoFuncID in [CSSCallID_NthOfType,CSSCallID_NthLastOfType];
  aSelector:=Params.OfSelector;

  if (Cache.Parent<>aParent) or (Cache.OfSelector<>aSelector) then
  begin
    // build cache
    Cache.Parent:=aParent;
    Cache.OfSelector:=aSelector;
    SetLength(Cache.Items,0);
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolver.CollectSiblingsOf Depth=',Depth,' Candidates=',ChildCount);
    {$ENDIF}
    for i:=0 to ChildCount-1 do
    begin
      aNode:=aParent.GetCSSChild(i);
      if (aSelector<>nil) and (SelectorMatches(aSelector,aNode,false)<0) then
        continue;

      // put
      if NeedTypeID then
        aTypeID:=aNode.GetCSSTypeID
      else
        aTypeID:=0;
      j:=length(Cache.Items)-1;
      while (j>=0) and (Cache.Items[j].TypeID<>aTypeID) do dec(j);
      if j<0 then
      begin
        j:=length(Cache.Items);
        SetLength(Cache.Items,j+1);
        Item:=@Cache.Items[j];
        Item^.TypeID:=aTypeID;
        Item^.Cnt:=0;
        SetLength(Item^.ChildIDs,ChildCount);
      end else
        Item:=@Cache.Items[j];
      Item^.ChildIDs[Item^.Cnt]:=i;
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.CollectSiblingsOf Sel=',GetCSSObj(aSelector),' CSSTypeID=',aNode.GetCSSTypeID,' ',Item^.Cnt,'=>',i);
      {$ENDIF}
      inc(Item^.Cnt);
    end;

    for i:=0 to high(Cache.Items) do
      with Cache.Items[i] do
        SetLength(ChildIDs,Cnt);
  end;

  // use cache
  if NeedTypeID then
  begin
    aTypeID:=TestNode.GetCSSTypeID;
    for i:=0 to high(Cache.Items) do
      if Cache.Items[i].TypeID=aTypeID then
        exit(Cache.Items[i].ChildIDs);
  end else if length(Cache.Items)>0 then
    Result:=Cache.Items[0].ChildIDs;
end;

function TCSSResolver.GetSiblingOfIndex(SiblingIDs: TIntegerDynArray;
  Index: integer): integer;
// searches the position of Index in a sorted array
var
  l, r, m: Integer;
begin
  l:=0;
  r:=length(SiblingIDs)-1;
  while l<=r do
  begin
    m:=(l+r) div 2;
    Result:=SiblingIDs[m];
    if Index<Result then
      r:=m-1
    else if Index>Result then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;

function TCSSResolver.ComputeValue(El: TCSSElement): TCSSString;
var
  ElData: TObject;
  C: TClass;
  StrEl: TCSSStringElement;
  IntEl: TCSSIntegerElement;
  FloatEl: TCSSFloatElement;
begin
  C:=El.ClassType;
  if C=TCSSResolvedIdentifierElement then
    Result:=TCSSResolvedIdentifierElement(El).Value
  else if (C=TCSSStringElement)
      or (C=TCSSIntegerElement)
      or (C=TCSSFloatElement) then
  begin
    ElData:=El.CustomData;
    if ElData is TCSSValueData then
      exit(TCSSValueData(ElData).NormValue);
    if C=TCSSStringElement then
    begin
      StrEl:=TCSSStringElement(El);
      Result:=StrEl.Value;
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.ComputeValue String=[',Result,']');
      {$ENDIF}
    end
    else if C=TCSSIntegerElement then
    begin
      IntEl:=TCSSIntegerElement(El);
      Result:=IntEl.AsString;
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.ComputeValue Integer=[',Result,']');
      {$ENDIF}
    end else if C=TCSSFloatElement then
    begin
      FloatEl:=TCSSFloatElement(El);
      Result:=FloatEl.AsString;
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.ComputeValue Float=[',Result,']');
      {$ENDIF}
    end;
    ElData:=TCSSValueData.Create;
    TCSSValueData(ElData).NormValue:=Result;
    El.CustomData:=ElData;
  end else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    LogWarning(croErrorOnUnknownName in Options,20220910235106,'TCSSResolver.ComputeValue not supported',El);
    {$ENDIF}
  end;
end;

function TCSSResolver.SameValueText(const A, B: TCSSString): boolean;
begin
  if StringComparison=crscCaseInsensitive then
    Result:=SameText(A,B)
  else
    Result:=A=B;
end;

function TCSSResolver.SameValueText(A: PCSSChar; ALen: integer; B: PCSSChar; BLen: integer
  ): boolean;
var
  AC, BC: TCSSChar;
  i: Integer;
begin
  if ALen<>BLen then exit(false);
  if ALen=0 then exit(true);
  if StringComparison=crscCaseInsensitive then
  begin
    for i:=0 to ALen-1 do
    begin
      AC:=A^;
      BC:=B^;
      if (AC<>BC) then
      begin
        if (AC in ['a'..'z']) then AC:=TCSSChar(ord(AC)-32);
        if (BC in ['a'..'z']) then BC:=TCSSChar(ord(BC)-32);
        if AC<>BC then
          exit(false);
      end;
      inc(A);
      inc(B);
    end;
    Result:=true;
  end else
    Result:=CompareMem(A,B,ALen);
end;

function TCSSResolver.PosSubString(const SearchStr, Str: TCSSString): integer;
var
  SearchLen: SizeInt;
  i: Integer;
  SearchP, StrP: PCSSChar;
  AC, BC: TCSSChar;
begin
  Result:=0;
  if SearchStr='' then exit;
  if Str='' then exit;
  if StringComparison=crscCaseInsensitive then
  begin
    SearchP:=PCSSChar(SearchStr);
    StrP:=PCSSChar(Str);
    SearchLen:=length(SearchStr);
    AC:=SearchP^;
    if AC in ['a'..'z'] then AC:=TCSSChar(ord(AC)-32);
    for i:=0 to length(Str)-SearchLen do
    begin
      BC:=StrP^;
      if BC in ['a'..'z'] then BC:=TCSSChar(ord(BC)-32);
      if (AC=BC) and SameValueText(SearchP,SearchLen,StrP,SearchLen) then
        exit(i+1);
      inc(StrP);
    end;
  end else begin
    Result:=Pos(SearchStr,Str);
  end;
end;

function TCSSResolver.PosWord(const SearchWord, Words: TCSSString): integer;
// attribute selector ~=
const
  Whitespace = [#9,#10,#12,#13,' '];
var
  WordsLen, SearchLen: SizeInt;
  p, WordStart: Integer;
begin
  Result:=0;
  if SearchWord='' then exit;
  if Words='' then exit;
  WordsLen:=length(Words);
  SearchLen:=length(SearchWord);
  //writeln('TCSSResolver.PosWord "',SearchWord,'" Words="',words,'"');
  p:=1;
  repeat
    repeat
      if p>WordsLen then
        exit(0);
      if not (Words[p] in Whitespace) then
        break;
      inc(p);
    until false;
    WordStart:=p;
    while (p<=WordsLen) and not (Words[p] in Whitespace) do
      inc(p);
    //writeln('TCSSResolver.PosWord start=',WordStart,' p=',p);
    if SameValueText(@SearchWord[1],SearchLen,@Words[WordStart],p-WordStart) then
      exit(WordStart);
  until p>WordsLen;
end;

function TCSSResolver.GetSiblingCount(aNode: ICSSNode): integer;
var
  aParent, CurNode: ICSSNode;
begin
  if aNode=nil then
    exit(0);
  aParent:=aNode.GetCSSParent;
  if aParent<>nil then
    exit(aParent.GetCSSChildCount);
  Result:=0;
  CurNode:=aNode;
  while CurNode<>nil do
  begin
    inc(Result);
    CurNode:=CurNode.GetCSSPreviousSibling;
  end;
  CurNode:=aNode.GetCSSNextSibling;
  while CurNode<>nil do
  begin
    inc(Result);
    CurNode:=CurNode.GetCSSNextSibling;
  end;
end;

procedure TCSSResolver.MergeAttribute(El: TCSSElement;
  aSpecificity: TCSSSpecificity);
var
  C: TClass;
  Decl: TCSSDeclarationElement;
  aKey: TCSSElement;
  AnAttrID, NextAttrID, SubAttrID: TCSSNumericalID;
  AttrDesc: TCSSAttributeDesc;
  KeyData: TCSSAttributeKeyData;
begin
  C:=El.ClassType;
  if C<>TCSSDeclarationElement then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220908232359,'Unknown property',El);
    {$ENDIF}
    exit;
  end;
  Decl:=TCSSDeclarationElement(El);

  if Decl.KeyCount<>1 then
  begin
    if Decl.KeyCount<1 then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20231112135955,'missing keys in declaration',Decl);
      {$ENDIF}
    end;
    if Decl.KeyCount>1 then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20231112140722,'too many keys in declaration',Decl);
      {$ENDIF}
    end;
    exit;
  end;

  if Decl.ChildCount=0 then
    exit;
  if Decl.IsImportant then
    aSpecificity:=CSSSpecificityImportant;

  aKey:=Decl.Keys[0];
  C:=aKey.ClassType;
  if C<>TCSSResolvedIdentifierElement then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220908232359,'Unknown CSS key',aKey);
    {$ENDIF}
    exit;
  end;
  AnAttrID:=TCSSResolvedIdentifierElement(aKey).NumericalID;
  if AnAttrID<=CSSIDNone then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220909000932,'Unknown CSS property "'+TCSSResolvedIdentifierElement(aKey).Name+'"',aKey);
    {$ENDIF}
    exit;
  end;

  KeyData:=TCSSAttributeKeyData(aKey.CustomData);
  if KeyData.Invalid then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    //Log(etWarning,20240710162139,'Invalid CSS property "'+El.AsString+'"',aKey);
    {$ENDIF}
    exit;
  end;

  if AnAttrID=CSSAttributeID_All then
  begin
    // 'all' sets almost all attributes to a value
    if FMergedAllSpecificity>aSpecificity then
      exit;
    FMergedAllSpecificity:=aSpecificity;
    FMergedAllDecl:=Decl;

    SubAttrID:=FMergedAttributeFirst;
    while SubAttrID>=1 do
    begin
      NextAttrID:=FMergedAttributes[SubAttrID].Next;
      AttrDesc:=GetAttributeDesc(SubAttrID);
      if AttrDesc.All then
        RemoveMergedAttribute(SubAttrID);
      SubAttrID:=NextAttrID;
    end;
  end
  else begin
    // set property
    AttrDesc:=GetAttributeDesc(AnAttrID);

    if (FMergedAllSpecificity>aSpecificity) and AttrDesc.All then
      exit; // a former 'all' has higher Specificity

    with FMergedAttributes[AnAttrID] do
    begin
      if (Stamp=FMergedAttributesStamp) and (Specificity>aSpecificity) then
        exit; // a former attribute has higher Specificity
    end;
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolver.MergeAttribute Node=',FNode.GetCSSID,' AttrID=',AnAttrID,' ',AttrDesc.Name,' Spec=',aSpecificity,' Decl="',Decl.AsString,'"');
    {$ENDIF}
    SetMergedAttribute(AnAttrID,aSpecificity,Decl);

    if (AttrDesc<>nil) and (length(AttrDesc.CompProps)>0) then
    begin
      // shorthand -> set longhands
      // Note: order matters when same Specificity, so longhands must be done during the cascade
      for NextAttrID:=0 to length(AttrDesc.CompProps)-1 do
      begin
        SubAttrID:=AttrDesc.CompProps[NextAttrID].Index;
        with FMergedAttributes[SubAttrID] do
        begin
          if (Stamp=FMergedAttributesStamp) and (Specificity>aSpecificity) then
            continue; // a former attribute has higher Specificity
          SetMergedAttribute(SubAttrID,aSpecificity,nil);
          {$IFDEF VerboseCSSResolver}
          writeln('TCSSResolver.MergeAttribute Longhand Node=',FNode.GetCSSID,' Shorthand=',AttrDesc.Name,' Spec=',aSpecificity,' Decl="',Decl.AsString,'" Longhand=',GetAttributeDesc(SubAttrID).Name);
          {$ENDIF}
        end;
      end;
    end;

    //WriteMergedAttributes('TCSSResolver.MergeAttribute');
  end;
end;

procedure TCSSResolver.SaveSharedMergedAttributes(SharedMerged: TCSSSharedRuleList);
var
  Cnt: Integer;
  AttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
begin
  SharedMerged.AllDecl:=FMergedAllDecl;
  SharedMerged.AllSpecificity:=FMergedAllSpecificity;

  // count attributes (skip longhands set by shorthands DeclEl=nil)
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    AttrP:=@FMergedAttributes[AttrID];
    if AttrP^.DeclEl<>nil then
      inc(Cnt);
    AttrID:=AttrP^.Next;
  end;
  SetLength(SharedMerged.Values,Cnt);

  // save attributes
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    AttrP:=@FMergedAttributes[AttrID];
    if AttrP^.DeclEl<>nil then
    begin
      SharedMerged.Values[Cnt].AttrID:=AttrID;
      SharedMerged.Values[Cnt].DeclEl:=AttrP^.DeclEl;
      SharedMerged.Values[Cnt].Specificity:=AttrP^.Specificity;
      inc(Cnt);
    end;
    AttrID:=AttrP^.Next;
  end;
end;

procedure TCSSResolver.LoadSharedMergedAttributes(
  SharedMerged: TCSSSharedRuleList);
var
  i: Integer;
begin
  ClearMerge;
  FMergedAllDecl:=SharedMerged.AllDecl;
  FMergedAllSpecificity:=SharedMerged.AllSpecificity;
  for i:=0 to length(SharedMerged.Values)-1 do
  begin
    with SharedMerged.Values[i] do
      SetMergedAttribute(AttrID,Specificity,DeclEl);
  end;
end;

procedure TCSSResolver.WriteMergedAttributes(const Title: TCSSString);
var
  AttrID, NextAttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
  Cnt: Integer;
  AttrDesc: TCSSAttributeDesc;
begin
  writeln('TCSSResolver.WriteMergedAttributes START ',Title);
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    NextAttrID:=FMergedAttributes[AttrID].Next;
    AttrP:=@FMergedAttributes[AttrID];
    AttrDesc:=GetAttributeDesc(AttrID);
    writeln('  ',Cnt,' AttrID=',AttrID,' ',AttrDesc.Name,' Spec=',AttrP^.Specificity,' Value="',AttrP^.Value,'" Complete=',AttrP^.Complete,' Decl=',AttrP^.DeclEl<>nil);
    inc(Cnt);
    AttrID:=NextAttrID;
  end;
  writeln('TCSSResolver.WriteMergedAttributes END Count=',Cnt);
end;

procedure TCSSResolver.LoadMergedValues;
var
  AttrID, NextAttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
  Key: TCSSElement;
  KeyData: TCSSAttributeKeyData;
  Value: TCSSString;
begin
  // load value strings from css elements
  // and remove longhand placeholders set by shorthands
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    NextAttrID:=FMergedAttributes[AttrID].Next;
    AttrP:=@FMergedAttributes[AttrID];
    if AttrP^.DeclEl=nil then
      // remove longhand placeholder set by shorthand
      RemoveMergedAttribute(AttrID)
    else begin
      Key:=AttrP^.DeclEl.Keys[0];
      KeyData:=Key.CustomData as TCSSAttributeKeyData;
      Value:=KeyData.Value;
      //writeln('TCSSResolver.LoadMergedValues AttrID=',AttrID,' Decl=',AttrP^.DeclEl.Classname,' Key=',(AttrP^.DeclEl.Keys[0] as TCSSResolvedIdentifierElement).Name,' Value=',Value);
      AttrP^.Value:=Value;
      if TCSSResolverParser.IsWhiteSpace(Value) then
        RemoveMergedAttribute(AttrID)
      else
        AttrP^.Complete:=KeyData.Complete;
    end;
    AttrID:=NextAttrID;
  end;
end;

procedure TCSSResolver.SubstituteVarCalls;
// called after CSS attribute values have been merged by cascade rules
// before replacing shorthands
const
  ReplaceMax = 10;
var
  AttrID, NextAttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
  p: PCSSChar;
  ReplaceCnt: integer;

  procedure SkipEscape;
  begin
    inc(p);
    if p^>#0 then inc(p);
  end;

  procedure SkipString;
  var
    c: TCSSChar;
  begin
    c:=p^;
    repeat
      inc(p);
      if p^=#0 then exit;
      if p^=c then
      begin
        inc(p);
        exit;
      end;
    until false;
  end;

  procedure SkipIdentifier;
  begin
    while p^ in ['-','_','a'..'z','A'..'Z'] do inc(p);
  end;

  procedure SkipWhiteSpace;
  begin
    while p^ in [' ',#9,#10,#13] do inc(p);
  end;

  function ReplaceVarsInRightString: boolean;
  var
    OldP, Lvl: integer;
    VarStartP, NameStartP, NameEndP, ValueStartP, BracketCloseP: PCSSChar;
    aValue, s: TCSSString;
    {$IF SIZEOF(CHAR)=2}
    varname: UnicodeString;
    {$ELSE}
    VarName: ShortString;
    {$ENDIF}
    Desc: TCSSResCustomAttributeDesc;
    aParentNode: ICSSNode;
  begin
    {$IFDEF VerboseCSSVar}
    writeln('ReplaceVarsInRightString p="',p,'"');
    {$ENDIF}
    Result:=true;
    repeat
      case p^ of
      #0: break;
      '"','''': SkipString;
      '\': SkipEscape;
      '@','#':
        begin
          inc(p);
          SkipIdentifier;
        end;
      '-':
        begin
          inc(p);
          if (p^ in ['a'..'z','A'..'Z','_','-']) then
            SkipIdentifier;
        end;
      'a'..'z','A'..'Z','_':
        if (p^='v') and (p[1]='a') and (p[2]='r') and (p[3]='(') then
        begin
          // var() found

          inc(ReplaceCnt);
          if ReplaceCnt=ReplaceMax then
          begin
            // maybe a loop
            exit(false);
          end;

          VarStartP:=p;
          inc(p,4);
          SkipWhiteSpace;

          // replace var() in parameter
          OldP:=p-PCSSChar(AttrP^.Value);
          if not ReplaceVarsInRightString then
            exit(false);
          p:=PCSSChar(AttrP^.Value)+OldP;

          NameStartP:=p;
          NameEndP:=nil;
          ValueStartP:=nil;
          if (p^<>'-') or (p[1]<>'-') then
          begin
            {$IFDEF VerboseCSSVar}
            writeln('ReplaceVarsInRightString invalid VarName (must start with --): ',NameStartP);
            {$ENDIF}
            exit(false);
          end;
          inc(p,2);
          while p^ in ['a'..'z','A'..'Z','_','-'] do inc(p);
          NameEndP:=p;
          if NameEndP-NameStartP>255 then
          begin
            {$IFDEF VerboseCSSVar}
            writeln('ReplaceVarsInRightString invalid VarName (too long): ',NameStartP);
            {$ENDIF}
            exit(false);
          end;
          SkipWhiteSpace;
          if p^=',' then
          begin
            inc(p);
            SkipWhiteSpace;
            ValueStartP:=p;
          end;

          // skip to round bracket close
          Lvl:=1;
          BracketCloseP:=nil;
          repeat
            case p^ of
            #0:
              begin
                // syntax error
                {$IFDEF VerboseCSSVar}
                writeln('ReplaceVarsInRightString missing closing bracket: ',NameStartP);
                {$ENDIF}
                exit(false);
              end;
            '"','''': SkipString;
            '\': SkipEscape;
            '(':
              begin
                inc(Lvl);
                inc(p);
              end;
            ')':
              if Lvl=1 then
              begin
                BracketCloseP:=p;
                inc(p);
                break;
              end else begin
                dec(Lvl);
                inc(p);
              end;
            else
              inc(p);
            end;
          until false;

          // fetch value from node
          SetString(VarName,NameStartP,NameEndP-NameStartP);
          {$IF SIZEOF(CHAR)=2}
          Desc:=TCSSResCustomAttributeDesc(FCustomAttributeNameToDesc.Find(UTF8Encode(VarName)));
          {$ELSE}
          Desc:=TCSSResCustomAttributeDesc(FCustomAttributeNameToDesc.Find(VarName));
          {$ENDIF}
          if Desc<>nil then
          begin
            {$IFDEF VerboseCSSVar}
            writeln('ReplaceVarsInRightString VarName="',VarName,'" AttrID=',Desc.Index);
            {$ENDIF}
            if FMergedAttributes[Desc.Index].Stamp=FMergedAttributesStamp then
              aValue:=FMergedAttributes[Desc.Index].Value
            else
              aValue:='';
            if aValue='' then
            begin
              aParentNode:=FNode.GetCSSParent;
              if aParentNode<>nil then
                aValue:=aParentNode.GetCSSCustomAttribute(Desc.Index);
            end;
          end else begin
            {$IFDEF VerboseCSSVar}
            writeln('ReplaceVarsInRightString VarName="',VarName,'" never declared');
            {$ENDIF}
            aValue:='';
          end;

          if aValue='' then
          begin
            // use default value
            if ValueStartP<>nil then
              SetString(aValue,ValueStartP,BracketCloseP-ValueStartP);
          end;
          {$IFDEF VerboseCSSVar}
          writeln('ReplaceVarsInRightString VarName="',VarName,'" Value="',aValue,'"');
          {$ENDIF}

          // replace
          p:=PCSSChar(AttrP^.Value);
          OldP:=VarStartP-p;
          s:=AttrP^.Value;
          AttrP^.Value:=LeftStr(s,VarStartP-p)+aValue+copy(s,BracketCloseP-p+2,length(s));
          {$IFDEF VerboseCSSVar}
          writeln('ReplaceVarsInRightString New AttrP^.Value="',AttrP^.Value,'"');
          {$ENDIF}

          // continue parsing
          p:=PCSSChar(AttrP^.Value)+OldP;
        end else
          SkipIdentifier;
      else
        inc(p);
      end;
    until false;
  end;

begin
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    NextAttrID:=FMergedAttributes[AttrID].Next;
    AttrP:=@FMergedAttributes[AttrID];
    if not AttrP^.Complete then
    begin
      // check attribute
      if Pos('var(',AttrP^.Value)>0 then
      begin
        // can have var() calls -> parse
        p:=PCSSChar(AttrP^.Value);
        {$IFDEF VerboseCSSVar}
        writeln('TCSSResolver.SubstituteVarCalls ',GetAttributeDesc(AttrID).Name,': "',AttrP^.Value,'"');
        {$ENDIF}
        ReplaceCnt:=0;
        if not ReplaceVarsInRightString then
          AttrP^.Value:='';
      end;

      if AttrP^.Value='' then
        RemoveMergedAttribute(AttrID);
    end;
    AttrID:=NextAttrID;
  end;
end;

procedure TCSSResolver.ApplyShorthands;
// called after all var() have been substituted
var
  AttrID, NextAttrID, SubAttrID: TCSSNumericalID;
  AttrP, SubAttrP: PMergedAttribute;
  AttrDesc, SubAttrDesc: TCSSAttributeDesc;
  LHAttrIDs: TCSSNumericalIDArray;
  LHValues: TCSSStringArray;
  i: Integer;
begin
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    NextAttrID:=FMergedAttributes[AttrID].Next;
    AttrP:=@FMergedAttributes[AttrID];
    AttrDesc:=GetAttributeDesc(AttrID);
    //writeln('TCSSResolver.ApplyShorthands ',AttrID,' ',AttrDesc.Name,' ShortHand=',AttrDesc.OnSplitShorthand<>nil);
    if Assigned(AttrDesc.OnSplitShorthand) then
    begin
      RemoveMergedAttribute(AttrID);
      if AttrP^.Value>'' then
      begin
        // replace shorthand with longhands, keep already set longhands
        LHAttrIDs:=[];
        LHValues:=[];
        InitParseAttr(AttrDesc,nil,AttrP^.Value);
        if not (CurComp.Kind in [rvkNone,rvkInvalid]) then
        begin
          AttrDesc.OnSplitShorthand(Self,LHAttrIDs,LHValues);
          for i:=0 to length(LHAttrIDs)-1 do
          begin
            SubAttrID:=LHAttrIDs[i];
            SubAttrDesc:=GetAttributeDesc(SubAttrID);
            if SubAttrDesc=nil then
              raise ECSSResolver.Create('20240709194135');
            if SubAttrDesc.OnSplitShorthand<>nil then
              raise ECSSResolver.Create('20240709194634');
            SubAttrP:=@FMergedAttributes[SubAttrID];
            if (SubAttrP^.Stamp=FMergedAttributesStamp) and (SubAttrP^.Specificity>=AttrP^.Specificity) then
            begin
              // longhand already exists -> keep
            end else begin
              SetMergedAttribute(SubAttrID,AttrP^.Specificity,nil);
              SubAttrP^.Value:=LHValues[i];
              if SubAttrP^.Value='' then
                SubAttrP^.Value:=SubAttrDesc.InitialValue;
              SubAttrP^.Complete:=false;
              // Note: if NextAttrID=0 then this was the last shorthand
            end;
          end;
        end;
      end;
    end;
    AttrID:=NextAttrID;
  end;
end;

function TCSSResolver.CreateValueList: TCSSAttributeValues;
var
  Cnt: Integer;
  AttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
  AttrValue: TCSSAttributeValue;
begin
  Result:=TCSSAttributeValues.Create;

  // all
  if FMergedAllDecl<>nil then
  begin
    // set Result.AllValue
    InitParseAttr(CSSRegistry.Attributes[CSSAttributeID_All],nil,GetDeclarationValue(FMergedAllDecl));
    if (CurComp.Kind=rvkKeyword) and IsBaseKeyword(CurComp.KeywordID) then
    begin
      Result.AllValue:=CurComp.KeywordID;
    end;
  end;

  // count and allocate attributes
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    //writeln('TCSSResolver.CreateValueList Cnt=',Cnt,' AttrID=',AttrID);
    inc(Cnt);
    AttrID:=FMergedAttributes[AttrID].Next;
  end;
  SetLength(Result.Values,Cnt);

  // copy
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    AttrP:=@FMergedAttributes[AttrID];
    AttrValue:=TCSSAttributeValue.Create;
    Result.Values[Cnt]:=AttrValue;
    AttrValue.AttrID:=AttrID;
    AttrValue.Value:=AttrP^.Value;
    //writeln('TCSSResolver.CreateValueList ',Cnt,' ',AttrID,' "',AttrValue.Value,'"');
    AttrID:=AttrP^.Next;
    inc(Cnt);
  end;

  // sort
  Result.SortValues;
end;

function TCSSResolver.ResolveIdentifier(El: TCSSResolvedIdentifierElement;
  Kind: TCSSNumericalIDKind): TCSSNumericalID;
var
  aName: TCSSString;
begin
  Result:=El.NumericalID;
  if Result=CSSIDNone then
  begin
    // not yet resolved
    aName:=El.Name;
    if Kind in [nikPseudoClass,nikPseudoElement] then
    begin
      // pseudo attributes and elements are ASCII case insensitive
      System.Delete(aName,1,1);
      aName:=lowercase(aName);
    end;

    Result:=CSSRegistry.IndexOfNamedItem(Kind,aName);
    if Result=CSSIDNone then
    begin
      El.NumericalID:=-1;
      Log(etWarning,20240625160211,'unknown '+CSSNumericalIDKindNames[Kind]+' "'+aName+'"',El);
    end else begin
      El.NumericalID:=Result;
      El.Kind:=Kind;
    end;
  end else if Result=-1 then
    Result:=CSSIDNone // name not found
  else if El.Kind<>Kind then
    raise ECSSResolver.Create('20240701105839');
end;

procedure TCSSResolver.LogWarning(IsError: boolean; const ID: TCSSMsgID;
  const Msg: TCSSString; PosEl: TCSSElement);
var
  MsgType: TEventType;
begin
  if IsError then
    MsgType:=etError
  else
    MsgType:=etWarning;
  Log(MsgType,ID,Msg,PosEl);
end;

procedure TCSSResolver.Log(MsgType: TEventType; const ID: TCSSMsgID;
  const Msg: TCSSString; PosEl: TCSSElement);
var
  Entry: TCSSResolverLogEntry;
begin
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.Log ',MsgType,' ID=',ID,' ',GetElPos(PosEl),': "',Msg,'"');
  {$ENDIF}
  if Assigned(OnLog) then
  begin
    Entry:=TCSSResolverLogEntry.Create;
    Entry.MsgType:=MsgType;
    Entry.ID:=ID;
    Entry.Msg:=Msg;
    Entry.PosEl:=PosEl;
    FLogEntries.Add(Entry);
    OnLog(Self,Entry);
  end;
  if MsgType=etError then
  begin
    raise ECSSResolver.Create('['+IntToStr(ID)+'] '+Msg+' at '+GetElPos(PosEl));
  end;
end;

function TCSSResolver.GetElPos(El: TCSSElement): TCSSString;
begin
  if El=nil then
    Result:='no element'
  else begin
    Result:=El.SourceFileName+'('+IntToStr(El.SourceCol)+','+IntToStr(El.SourceCol)+')';
    {$IFDEF VerboseCSSResolver}
    Result:='['+GetElPath(El)+']'+Result;
    {$ENDIF}
  end;
end;

function TCSSResolver.ParseInlineStyle(const Src: TCSSString): TCSSRuleElement;
begin
  Result:=ParseCSSSource(Src,true) as TCSSRuleElement;
end;

function TCSSResolver.GetElPath(El: TCSSElement): TCSSString;
begin
  Result:=GetCSSPath(El);
end;

constructor TCSSResolver.Create(AOwner: TComponent);
begin
  inherited;
  FLogEntries:=TFPObjectList.Create(true);
  FSharedRuleLists:=TAVLTree.Create(@CompareCSSSharedRuleLists);
  FCustomAttributeNameToDesc:=TFPHashList.Create;
end;

destructor TCSSResolver.Destroy;
begin
  Clear;
  FreeAndNil(FCustomAttributeNameToDesc);
  FreeAndNil(FSharedRuleLists);
  FreeAndNil(FLogEntries);
  inherited Destroy;
end;

procedure TCSSResolver.Clear;
begin
  ClearStyleSheets;
end;

procedure TCSSResolver.Init;
var
  i: Integer;
begin
  if CSSRegistry.Modified then
  begin
    CSSRegistry.ConsistencyCheck;
    CSSRegistry.Modified:=false;
  end;

  // todo: if CSSRegistry has changed, reparse all stylesheets

  FMergedAttributesStamp:=1;
  for i:=0 to length(FMergedAttributes)-1 do
    FMergedAttributes[i].Stamp:=0;
end;

procedure TCSSResolver.ClearSharedRuleLists;
begin
  FSharedRuleLists.FreeAndClear;
end;

procedure TCSSResolver.Compute(Node: ICSSNode; InlineStyle: TCSSRuleElement;
  out Rules: TCSSSharedRuleList; out Values: TCSSAttributeValues);
var
  i: Integer;
begin
  Rules:=nil;
  FNode:=Node;
  try
    InitMerge;

    FindMatchingRules;

    // create a shared rule list and merge attributes
    Rules:=CreateSharedRuleList;

    // apply inline attributes
    if InlineStyle<>nil then
    begin
      for i:=0 to InlineStyle.ChildCount-1 do
        MergeAttribute(InlineStyle.Children[i],CSSSpecificityInline);
    end;

    LoadMergedValues;
    SubstituteVarCalls; // replace var() calls
    ApplyShorthands;

    // create sorted map AttrId to Value
    Values:=CreateValueList;
  finally
    FNode:=nil;
  end;
end;

function TCSSResolver.GetAttributeID(const aName: TCSSString; AutoCreate: boolean): TCSSNumericalID;
var
  Desc: TCSSResCustomAttributeDesc;
  Cnt: TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfAttributeName(aName);
  if Result<0 then
  begin
    Desc:=TCSSResCustomAttributeDesc(FCustomAttributeNameToDesc.Find(aName));
    if Desc<>nil then
      exit(Desc.Index);

    if AutoCreate
        and (length(aName)>2) and (aName[1]='-') and (aName[2]='-')
        and (length(aName)<256) then
    begin
      // create custom attribute
      Cnt:=FCustomAttributeCount;
      if Cnt=length(FCustomAttributes) then
      begin
        if Cnt<32 then
          Cnt:=32
        else
          Cnt:=Cnt*2;
        SetLength(FCustomAttributes,Cnt);
        FillByte(FCustomAttributes[FCustomAttributeCount],SizeOf(Pointer)*(Cnt-FCustomAttributeCount),0);
      end;

      Desc:=TCSSResCustomAttributeDesc.Create;
      Desc.Name:=aName;
      Desc.Index:=CSSRegistry.AttributeCount+FCustomAttributeCount;
      Desc.Inherits:=true;
      FCustomAttributes[FCustomAttributeCount]:=Desc;
      FCustomAttributeNameToDesc.Add(aName,Desc);

      inc(FCustomAttributeCount);

      Result:=Desc.Index;
      Cnt:=GetAttributeID(aName);
      if Cnt<>Result then
        raise ECSSResolver.Create('20240822173412');

      if GetAttributeDesc(Result)<>Desc then
        raise ECSSResolver.Create('20240822174053');
    end;
  end;
end;

procedure TCSSResolver.FindMatchingRules;
var
  aLayerIndex, i: Integer;
begin
  FElRuleCount:=0;

  // find all matching rules in all stylesheets
  for aLayerIndex:=0 to length(FLayers)-1 do
    with FLayers[aLayerIndex] do begin
      FSourceSpecificity:=CSSOriginToSpecifity[Origin];
      for i:=0 to ElementCount-1 do
        ComputeElement(Elements[i].Element);
    end;
end;

function TCSSResolver.GetAttributeDesc(AttrId: TCSSNumericalID
  ): TCSSAttributeDesc;
begin
  Result:=nil;
  if AttrID<CSSRegistry.AttributeCount then
    Result:=CSSRegistry.Attributes[AttrId]
  else begin
    dec(AttrID,CSSRegistry.AttributeCount);
    if AttrID<FCustomAttributeCount then
      Result:=FCustomAttributes[AttrId];
  end;
end;

function TCSSResolver.GetDeclarationValue(Decl: TCSSDeclarationElement): TCSSString;
var
  KeyData: TCSSAttributeKeyData;
begin
  Result:='';
  if Decl=nil then exit;
  if Decl.KeyCount=0 then exit;
  KeyData:=TCSSAttributeKeyData(Decl.Keys[0].CustomData);
  if KeyData=nil then exit;
  Result:=KeyData.Value;
end;

procedure TCSSResolver.ClearStyleSheets;
var
  i: Integer;
begin
  ClearElements;

  // clear stylesheets
  for i:=0 to FStyleSheetCount-1 do
  begin
    FreeAndNil(FStyleSheets[i].Element);
    FreeAndNil(FStyleSheets[i]);
  end;
  FStyleSheetCount:=0;
end;

function TCSSResolver.AddStyleSheet(anOrigin: TCSSOrigin; const aName: TCSSString;
  const aSource: TCSSString): TStyleSheet;
var
  Cnt, i: SizeInt;
begin
  if aName>'' then
  begin
    i:=IndexOfStyleSheetWithName(anOrigin,aName);
    if i>=0 then
    begin
      ReplaceStyleSheet(i,aSource);
      exit;
    end;
  end;

  Cnt:=length(FStyleSheets);
  if Cnt=FStyleSheetCount then
  begin
    if Cnt<32 then
      Cnt:=32
    else
      Cnt:=Cnt*2;
    SetLength(FStyleSheets,Cnt);
    FillByte(FStyleSheets[FStyleSheetCount],SizeOf(Pointer)*(Cnt-FStyleSheetCount),0);
  end;
  Result:=FStyleSheets[FStyleSheetCount];
  if Result=nil then
  begin
    Result:=TStyleSheet.Create;
    FStyleSheets[FStyleSheetCount]:=Result;
  end;
  inc(FStyleSheetCount);

  with Result do begin
    Name:=aName;
    Origin:=anOrigin;
    Source:=aSource;
    Parsed:=false;
    if Element<>nil then
      FreeAndNil(Element);
  end;

  ParseSource(FStyleSheetCount-1);
end;

procedure TCSSResolver.ReplaceStyleSheet(Index: integer; const NewSource: TCSSString);
var
  Sheet: TStyleSheet;
begin
  Sheet:=StyleSheets[Index];
  if NewSource=Sheet.Source then exit;
  ClearMerge;
  ClearSharedRuleLists;
  FreeAndNil(Sheet.Element);
  Sheet.Parsed:=false;
  Sheet.Source:=NewSource;

  ParseSource(Index);
end;

function TCSSResolver.IndexOfStyleSheetWithElement(El: TCSSElement): integer;
var
  aParent: TCSSElement;
  i: Integer;
begin
  Result:=-1;
  if El=nil then exit;
  repeat
    aParent:=El.Parent;
    if aParent=nil then break;
    El:=aParent
  until false;
  for i:=0 to FStyleSheetCount-1 do
    if FStyleSheets[i].Element=El then
      exit(i);
end;

function TCSSResolver.IndexOfStyleSheetWithName(anOrigin: TCSSOrigin; const aName: TCSSString
  ): integer;
var
  Sheet: TStyleSheet;
begin
  for Result:=0 to FStyleSheetCount-1 do
  begin
    Sheet:=FStyleSheets[Result];
    if (Sheet.Origin=anOrigin) and (aName=Sheet.Name) then
      exit;
  end;
  Result:=-1;
end;

function TCSSResolver.FindStyleSheetWithElement(El: TCSSElement): TStyleSheet;
var
  i: Integer;
begin
  i:=IndexOfStyleSheetWithElement(El);
  if i>=0 then
    Result:=FStyleSheets[i]
  else
    Result:=nil;
end;

end.

