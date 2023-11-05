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

Specifity:
important: 10000
inline: 1000
id: 100 #menu
class+attribute selectors: 10 .button, :hover, [href]
element/type: 1 p, :before
*: 0

ToDo:
- 'all' attribute: resets all properties, except direction and unicode bidi
- :has()
- TCSSResolver.FindComputedAttribute  use binary search for >8 elements
- TCSSNumericalIDs: once initialized sort and use binary search
- namespaces
- layers
- --varname
- counter-reset
- counter-increment
- @rules:-----------------------------------------------------------------------
  - @media
  - @font-face
  - @keyframes
- Pseudo-elements - not case sensitive:-----------------------------------------
  - ::first-letter 	p::first-letter 	Selects the first letter of every <p> element
  - ::first-line 	p::first-line 	Selects the first line of every <p> element
  - ::selection 	::selection 	Selects the portion of an element that is selected by a user
- Altering:---------------------------------------------------------------------
  - ::after 	p::after 	Insert something after the content of each <p> element
  - ::before 	p::before 	Insert something before the content of each <p> element
- Functions and Vars:-----------------------------------------------------------
  - attr() 	Returns the value of an attribute of the selected element
  - calc() 	Allows you to perform calculations to determine CSS property values  calc(100% - 100px)
  - max() min()  min(50%, 50px)
- columns:----------------------------------------------------------------------
  - columns combinator ||     col.selected || td
  - :nth-col()
  - :nth-last-col()

}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSResolver;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}
{$WARN 6060 off} // Case statement does not handle all possible cases

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, System.Contnrs, System.StrUtils, FPCSS.Tree;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, types, Contnrs, StrUtils, fpCSSTree;
{$ENDIF FPC_DOTTEDUNITS}

const
  CSSSpecifityInvalid = -2;
  CSSSpecifityNoMatch = -1;
  CSSSpecifityUniversal = 0;
  CSSSpecifityType = 1;
  CSSSpecifityClass = 10; // includes attribute selectors e.g. [href]
  CSSSpecifityIdentifier = 100;
  CSSSpecifityInline = 1000;
  CSSSpecifityImportant = 10000;

  CSSIDNone = 0;
  // built-in type IDs
  CSSTypeID_Universal = 1; // id of type '*'
  CSSLastTypeID = CSSTypeID_Universal;
  // built-in attribute IDs
  CSSAttributeID_ID = 1; // id of attribute key 'id'
  CSSAttributeID_Class = 2; // id of attribute key 'class'
  CSSAttributeID_All = 3; // id of attribute key 'all'
  CSSLastAttributeID = CSSAttributeID_All;
  // pseudo attribute and call IDs
  CSSPseudoID_Root = 1; // :root
  CSSPseudoID_Empty = CSSPseudoID_Root+1; // :empty
  CSSPseudoID_FirstChild = CSSPseudoID_Empty+1; // :first-child
  CSSPseudoID_LastChild = CSSPseudoID_FirstChild+1; // :last-child
  CSSPseudoID_OnlyChild = CSSPseudoID_LastChild+1; // :only-child
  CSSPseudoID_FirstOfType = CSSPseudoID_OnlyChild+1; // :first-of-type
  CSSPseudoID_LastOfType = CSSPseudoID_FirstOfType+1; // :last-of-type
  CSSPseudoID_OnlyOfType = CSSPseudoID_LastOfType+1; // :only-of-type
  CSSCallID_Not = CSSPseudoID_OnlyOfType+1; // :not()
  CSSCallID_Is = CSSCallID_Not+1; // :is()
  CSSCallID_Where = CSSCallID_Is+1; // :where()
  CSSCallID_Has = CSSCallID_Where+1; // :has()
  CSSCallID_NthChild = CSSCallID_Has+1; // :nth-child(n)
  CSSCallID_NthLastChild = CSSCallID_NthChild+1; // :nth-last-child(n)
  CSSCallID_NthOfType = CSSCallID_NthLastChild+1; // :nth-of-type(n)
  CSSCallID_NthLastOfType = CSSCallID_NthOfType+1; // :nth-last-of-type(n)
  CSSLastPseudoID = CSSCallID_NthLastOfType;

const
  CSSPseudoNames: array[0..CSSLastPseudoID] of string = (
    '?',
    ':root',
    ':empty',
    ':first-child',
    ':last-child',
    ':only-child',
    ':first-of-type',
    ':last-of-type',
    ':only-of-type',
    ':not()',
    ':is()',
    ':where()',
    ':has()',
    ':nth-child(n)',
    ':nth-last-child(n)',
    ':nth-of-type(n)',
    ':nth-last-of-type(n)'
    );

type
  TCSSMsgID = int64;
  TCSSNumericalID = integer;
  TCSSSpecifity = integer;

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
    function HasCSSClass(const aClassName: TCSSString): boolean;
    function GetCSSAttributeClass: TCSSString;
    function GetCSSParent: ICSSNode;
    function GetCSSIndex: integer; // node index in parent's children
    function GetCSSNextSibling: ICSSNode;
    function GetCSSPreviousSibling: ICSSNode;
    function GetCSSChildCount: integer;
    function GetCSSChild(const anIndex: integer): ICSSNode;
    function GetCSSNextOfType: ICSSNode;
    function GetCSSPreviousOfType: ICSSNode;
    function HasCSSAttribute(const AttrID: TCSSNumericalID): boolean;
    function GetCSSAttribute(const AttrID: TCSSNumericalID): TCSSString;
    function HasCSSPseudoClass(const AttrID: TCSSNumericalID): boolean;
    function GetCSSEmpty: boolean;
    function GetCSSDepth: integer;
    procedure SetCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement);
    function CheckCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement): boolean;
  end;

type
  TCSSNumericalIDKind = (
    nikType,
    nikAttribute,
    nikPseudoClass
    );
  TCSSNumericalIDKinds = set of TCSSNumericalIDKind;

const
  CSSNumericalIDKindNames: array[TCSSNumericalIDKind] of TCSSString = (
    'Type',
    'Attribute',
    'PseudoClass'
    );

type

  { TCSSNumericalIDs }

  TCSSNumericalIDs = class
  private
    FKind: TCSSNumericalIDKind;
    fList: TFPHashList;
    function GetCount: Integer;
    function GetIDs(const aName: TCSSString): TCSSNumericalID;
    procedure SetIDs(const aName: TCSSString; const AValue: TCSSNumericalID);
  public
    constructor Create(aKind: TCSSNumericalIDKind);
    destructor Destroy; override;
    procedure Clear;
    property IDs[const aName: TCSSString]: TCSSNumericalID read GetIDs write SetIDs; default;
    property Kind: TCSSNumericalIDKind read FKind;
    property Count: Integer read GetCount;
  end;

  TCSSComputedAttribute = record
    AttrID: TCSSNumericalID;
    Specifity: TCSSSpecifity;
    Value: TCSSElement;
  end;
  TCSSComputedAttributeArray = array of TCSSComputedAttribute;
  PCSSComputedAttribute = ^TCSSComputedAttribute;

  TCSSElResolverData = class
  public
    Element: TCSSElement;
    Next, Prev: TCSSElResolverData;
  end;

  TCSSValueValidity = (
    cvvNone,
    cvvValid,
    cvvInvalid
    );
  TCSSValueValidities = set of TCSSValueValidity;

  TCSSIdentifierData = class(TCSSElResolverData)
  public
    NumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
    ValueValid: TCSSValueValidity;
  end;

  TCSSValueData = class(TCSSElResolverData)
  public
    NormValue: string;
  end;

  { TCSSCallData }

  TCSSCallData = class(TCSSElResolverData)
  public
    NumericalID: TCSSNumericalID;
    Params: TObject;
    destructor Destroy; override;
  end;

  TCSSCallNthChildParams = class;

  TCSSCallNthChildParamsCacheItem = record
    TypeID: TCSSNumericalID;
    ChildIDs: TIntegerDynArray;
    Cnt: integer; // = length(ChildIDs), used during creation
  end;
  PCSSCallNthChildParamsCacheItem = ^TCSSCallNthChildParamsCacheItem;
  TCSSCallNthChildParamsCacheItems = array of TCSSCallNthChildParamsCacheItem;

  TCSSCallNthChildParamsCache = class
  public
    Owner: TCSSCallNthChildParams;
    Parent: ICSSNode;
    StackDepth: integer;
    Items: TCSSCallNthChildParamsCacheItems;
  end;
  TCSSCallNthChildParamsCaches = array of TCSSCallNthChildParamsCache;

  { TCSSCallNthChildParams }

  TCSSCallNthChildParams = class
    Modulo: integer;
    Start: integer;
    HasOf: boolean;
    OfSelector: TCSSElement;
    StackCache: TCSSCallNthChildParamsCaches;
    destructor Destroy; override;
  end;

  TCSSResolverOption = (
    croErrorOnUnknownName
    );
  TCSSResolverOptions = set of TCSSResolverOption;

  TCSSComputeOption = (
    ccoCommit
    );
  TCSSComputeOptions = set of TCSSComputeOption;

const
  DefaultCSSComputeOptions = [ccoCommit];

type
  TCSSResolverLogEntry = class
  public
    MsgType: TEventType;
    ID: TCSSMsgID;
    Msg: string;
    PosEl: TCSSElement;
  end;

  TCSSResolverLogEvent = procedure(Sender: TObject; Entry: TCSSResolverLogEntry) of object;

  TCSSResStringComparison = (
    crscDefault,
    crscCaseInsensitive,
    crscCaseSensitive
    );
  TCSSResStringComparisons = set of TCSSResStringComparison;

  { TCSSResolver }

  TCSSResolver = class(TComponent)
  private
    FNumericalIDs: array[TCSSNumericalIDKind] of TCSSNumericalIDs;
    FOnLog: TCSSResolverLogEvent;
    FOptions: TCSSResolverOptions;
    FStringComparison: TCSSResStringComparison;
    FStyles: TCSSElementArray;
    FOwnsStyle: boolean;
    FFirstElData: TCSSElResolverData;
    FLastElData: TCSSElResolverData;
    function GetAttributes(Index: integer): PCSSComputedAttribute;
    function GetLogCount: integer;
    function GetLogEntries(Index: integer): TCSSResolverLogEntry;
    function GetNumericalIDs(Kind: TCSSNumericalIDKind): TCSSNumericalIDs;
    function GetStyleCount: integer;
    function GetStyles(Index: integer): TCSSElement;
    procedure SetNumericalIDs(Kind: TCSSNumericalIDKind;
      const AValue: TCSSNumericalIDs);
    procedure SetOptions(const AValue: TCSSResolverOptions);
    procedure SetStyles(Index: integer; const AValue: TCSSElement);
  protected
    FAttributes: TCSSComputedAttributeArray;
    FAttributeCount: integer;
    FNode: ICSSNode;
    FLogEntries: TFPObjectList; // list of TCSSResolverLogEntry
    procedure ComputeElement(El: TCSSElement); virtual;
    procedure ComputeRule(aRule: TCSSRuleElement); virtual;
    procedure ComputeInline(El: TCSSElement); virtual;
    procedure ComputeInlineRule(aRule: TCSSRuleElement); virtual;
    function SelectorMatches(aSelector: TCSSElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function SelectorIdentifierMatches(Identifier: TCSSIdentifierElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function SelectorHashIdentifierMatches(Identifier: TCSSHashIdentifierElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function SelectorClassNameMatches(aClassName: TCSSClassNameElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function SelectorPseudoClassMatches(aPseudoClass: TCSSPseudoClassElement; var TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function SelectorListMatches(aList: TCSSListElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function SelectorBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function SelectorArrayMatches(anArray: TCSSArrayElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: ICSSNode): TCSSSpecifity; virtual;
    function SelectorCallMatches(aCall: TCSSCallElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function Call_Not(aCall: TCSSCallElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function Call_Is(aCall: TCSSCallElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function Call_Where(aCall: TCSSCallElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function Call_NthChild(CallID: TCSSNumericalID; aCall: TCSSCallElement; const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity; virtual;
    function CollectSiblingsOf(CallID: TCSSNumericalID; TestNode: ICSSNode;
      Params: TCSSCallNthChildParams): TIntegerDynArray; virtual;
    function GetSiblingOfIndex(SiblingIDs: TIntegerDynArray; Index: integer): integer; virtual;
    function ComputeValue(El: TCSSElement): TCSSString; virtual;
    function SameValueText(const A, B: TCSSString): boolean; virtual;
    function SameValueText(A: PAnsiChar; ALen: integer; B: PAnsiChar; BLen: integer): boolean; virtual;
    function PosSubString(const SearchStr, Str: TCSSString): integer; virtual;
    function PosWord(const SearchWord, Words: TCSSString): integer; virtual;
    function GetSiblingCount(aNode: ICSSNode): integer; virtual;
    procedure MergeProperty(El: TCSSElement; Specifity: TCSSSpecifity); virtual;
    function CheckAttrValueValidity(AttrID: TCSSNumericalID; aKey, aValue: TCSSElement): boolean; virtual;
    function ResolveIdentifier(El: TCSSIdentifierElement; Kind: TCSSNumericalIDKind): TCSSNumericalID; virtual;
    function ResolveCall(El: TCSSCallElement): TCSSNumericalID; virtual;
    procedure AddElData(El: TCSSElement; ElData: TCSSElResolverData); virtual;
    function AddElValueData(El: TCSSElement; const aValue: TCSSString): TCSSValueData; virtual;
    function FindComputedAttribute(AttrID: TCSSNumericalID): PCSSComputedAttribute;
    function AddComputedAttribute(TheAttrID: TCSSNumericalID; aSpecifity: TCSSSpecifity;
                          aValue: TCSSElement): PCSSComputedAttribute;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetElPath(El: TCSSElement): string; virtual;
    function GetElPos(El: TCSSElement): string; virtual;
    function IndexOfStyle(aStyle: TCSSElement): integer; virtual;
    procedure AddStyle(aStyle: TCSSElement); virtual;
    procedure Clear; virtual;
    procedure ClearStyleCustomData; virtual;
    procedure ClearStyles; virtual;
    procedure Commit; virtual;
    procedure Compute(Node: ICSSNode; NodeStyle: TCSSElement = nil;
      const CompOptions: TCSSComputeOptions = DefaultCSSComputeOptions); virtual;
    procedure DeleteStyle(aIndex: integer); virtual;
    procedure Log(MsgType: TEventType; const ID: TCSSMsgID; Msg: string; PosEl: TCSSElement); virtual;
    procedure LogWarning(IsError: boolean; const ID: TCSSMsgID; Msg: string; PosEl: TCSSElement); virtual;
    procedure RemoveStyle(aStyle: TCSSElement); virtual;
    property AttributeCount: integer read FAttributeCount;
    property Attributes[Index: integer]: PCSSComputedAttribute read GetAttributes;
    property LogCount: integer read GetLogCount;
    property LogEntries[Index: integer]: TCSSResolverLogEntry read GetLogEntries;
    property NumericalIDs[Kind: TCSSNumericalIDKind]: TCSSNumericalIDs read GetNumericalIDs write SetNumericalIDs;
    property OnLog: TCSSResolverLogEvent read FOnLog write FOnLog;
    property Options: TCSSResolverOptions read FOptions write SetOptions;
    property OwnsStyle: boolean read FOwnsStyle write FOwnsStyle default false;
    property StringComparison: TCSSResStringComparison read FStringComparison;
    property StyleCount: integer read GetStyleCount;
    property Styles[Index: integer]: TCSSElement read GetStyles write SetStyles;
  end;

implementation

{ TCSSCallNthChildParams }

destructor TCSSCallNthChildParams.Destroy;
var
  i: Integer;
begin
  for i:=0 to high(StackCache) do
    StackCache[i].Free;
  inherited Destroy;
end;

{ TCSSCallData }

destructor TCSSCallData.Destroy;
begin
  FreeAndNil(Params);
  inherited Destroy;
end;

{ TCSSNumericalIDs }

function TCSSNumericalIDs.GetIDs(const aName: TCSSString): TCSSNumericalID;
begin
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
  Result:=TCSSNumericalID(fList.Find(aName));
  {$WARN 4056 on}
end;

function TCSSNumericalIDs.GetCount: Integer;
begin
  Result:=fList.Count;
end;

procedure TCSSNumericalIDs.SetIDs(const aName: TCSSString;
  const AValue: TCSSNumericalID);
var
  i: Integer;
begin
  i:=fList.FindIndexOf(aName);
  if i>=0 then
    fList.Delete(i);
  if AValue=CSSIDNone then
    exit;
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
  fList.Add(aName,Pointer(AValue));
  {$WARN 4056 on}
end;

constructor TCSSNumericalIDs.Create(aKind: TCSSNumericalIDKind);
begin
  FKind:=aKind;
  fList:=TFPHashList.Create;
end;

destructor TCSSNumericalIDs.Destroy;
begin
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure TCSSNumericalIDs.Clear;
begin
  fList.Clear;
end;

{ TCSSResolver }

function TCSSResolver.GetNumericalIDs(Kind: TCSSNumericalIDKind
  ): TCSSNumericalIDs;
begin
  Result:=FNumericalIDs[Kind];
end;

function TCSSResolver.GetStyleCount: integer;
begin
  Result:=length(FStyles);
end;

function TCSSResolver.GetStyles(Index: integer): TCSSElement;
begin
  Result:=FStyles[Index];
end;

function TCSSResolver.GetAttributes(Index: integer): PCSSComputedAttribute;
begin
  if (Index<0) or (Index>=FAttributeCount) then
    raise ECSSResolver.Create('TCSSResolver.GetAttributes index out of bounds');
  Result:=@FAttributes[Index];
end;

function TCSSResolver.GetLogCount: integer;
begin
  Result:=FLogEntries.Count;
end;

function TCSSResolver.GetLogEntries(Index: integer): TCSSResolverLogEntry;
begin
  Result:=TCSSResolverLogEntry(FLogEntries[Index]);
end;

procedure TCSSResolver.SetNumericalIDs(Kind: TCSSNumericalIDKind;
  const AValue: TCSSNumericalIDs);
begin
  FNumericalIDs[Kind]:=AValue;
end;

procedure TCSSResolver.SetOptions(const AValue: TCSSResolverOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

procedure TCSSResolver.SetStyles(Index: integer; const AValue: TCSSElement);
begin
  if (Index<0) or (Index>=length(FStyles)) then
    raise ECSSResolver.Create('TCSSResolver.SetStyles index '+IntToStr(Index)+' out of bounds '+IntToStr(length(FStyles)));
  if FStyles[Index]=AValue then exit;
  if OwnsStyle then
    FStyles[Index].Free;
  FStyles[Index]:=AValue;
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
  BestSpecifity, Specifity: TCSSSpecifity;
  aSelector: TCSSElement;
begin
  BestSpecifity:=CSSSpecifityNoMatch;
  for i:=0 to aRule.SelectorCount-1 do
  begin
    aSelector:=aRule.Selectors[i];
    Specifity:=SelectorMatches(aSelector,FNode,false);
    if Specifity>BestSpecifity then
      BestSpecifity:=Specifity;
  end;
  if BestSpecifity>=0 then
  begin
    // match -> apply properties
    for i:=0 to aRule.ChildCount-1 do
      MergeProperty(aRule.Children[i],BestSpecifity);
  end;
end;

procedure TCSSResolver.ComputeInline(El: TCSSElement);
var
  C: TClass;
begin
  if El=nil then exit;
  C:=El.ClassType;
  if C=TCSSRuleElement then
    ComputeInlineRule(TCSSRuleElement(El))
  else
    Log(etWarning,20220915140402,'TCSSResolver.ComputeInline Not yet supported inline element',El);
end;

procedure TCSSResolver.ComputeInlineRule(aRule: TCSSRuleElement);
var
  i: Integer;
begin
  if aRule.SelectorCount>0 then
    exit;
  for i:=0 to aRule.ChildCount-1 do
    MergeProperty(aRule.Children[i],CSSSpecifityInline);
end;

function TCSSResolver.SelectorMatches(aSelector: TCSSElement;
  const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity;

  procedure MatchPseudo;
  var
    aNode: ICSSNode;
  begin
    aNode:=TestNode;
    Result:=SelectorPseudoClassMatches(TCSSPseudoClassElement(aSelector),aNode,OnlySpecifity);
  end;

var
  C: TClass;
begin
  Result:=CSSSpecifityInvalid;
  //writeln('TCSSResolver.SelectorMatches ',aSelector.ClassName,' ',TestNode.GetCSSTypeName);
  C:=aSelector.ClassType;
  if C=TCSSIdentifierElement then
    Result:=SelectorIdentifierMatches(TCSSIdentifierElement(aSelector),TestNode,OnlySpecifity)
  else if C=TCSSHashIdentifierElement then
    Result:=SelectorHashIdentifierMatches(TCSSHashIdentifierElement(aSelector),TestNode,OnlySpecifity)
  else if C=TCSSClassNameElement then
    Result:=SelectorClassNameMatches(TCSSClassNameElement(aSelector),TestNode,OnlySpecifity)
  else if C=TCSSPseudoClassElement then
    MatchPseudo
  else if C=TCSSBinaryElement then
    Result:=SelectorBinaryMatches(TCSSBinaryElement(aSelector),TestNode,OnlySpecifity)
  else if C=TCSSArrayElement then
    Result:=SelectorArrayMatches(TCSSArrayElement(aSelector),TestNode,OnlySpecifity)
  else if C=TCSSListElement then
    Result:=SelectorListMatches(TCSSListElement(aSelector),TestNode,OnlySpecifity)
  else if C=TCSSCallElement then
    Result:=SelectorCallMatches(TCSSCallElement(aSelector),TestNode,OnlySpecifity)
  else
    Log(etWarning,20220908230152,'Unknown CSS selector element',aSelector);
end;

function TCSSResolver.SelectorIdentifierMatches(
  Identifier: TCSSIdentifierElement; const TestNode: ICSSNode;
  OnlySpecifity: boolean): TCSSSpecifity;
var
  TypeID: TCSSNumericalID;
begin
  Result:=CSSSpecifityNoMatch;
  TypeID:=ResolveIdentifier(Identifier,nikType);
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorIdentifierMatches ',Identifier.Value,' TypeId=',TypeID);
  {$ENDIF}
  if TypeID=CSSTypeID_Universal then
  begin
    // universal selector
    Result:=CSSSpecifityUniversal;
  end else if OnlySpecifity then
    Result:=CSSSpecifityType
  else if TypeID=CSSIDNone then
  begin
    LogWarning(croErrorOnUnknownName in Options,20220911230224,'Unknown CSS selector type name "'+Identifier.Name+'"',Identifier);
    Result:=CSSSpecifityInvalid;
  end else if TypeID=TestNode.GetCSSTypeID then
    Result:=CSSSpecifityType;
end;

function TCSSResolver.SelectorHashIdentifierMatches(
  Identifier: TCSSHashIdentifierElement; const TestNode: ICSSNode;
  OnlySpecifity: boolean): TCSSSpecifity;
var
  aValue: TCSSString;
begin
  if OnlySpecifity then
    exit(CSSSpecifityIdentifier);
  Result:=CSSSpecifityNoMatch;
  aValue:=Identifier.Value;
  if TestNode.GetCSSID=aValue then
    Result:=CSSSpecifityIdentifier;
end;

function TCSSResolver.SelectorClassNameMatches(
  aClassName: TCSSClassNameElement; const TestNode: ICSSNode;
  OnlySpecifity: boolean): TCSSSpecifity;
var
  aValue: TCSSString;
begin
  if OnlySpecifity then
    exit(CSSSpecifityClass);
  aValue:=aClassName.Name;
  if TestNode.HasCSSClass(aValue) then
    Result:=CSSSpecifityClass
  else
    Result:=CSSSpecifityNoMatch;
  //writeln('TCSSResolver.SelectorClassNameMatches ',aValue,' ',Result);
end;

function TCSSResolver.SelectorPseudoClassMatches(
  aPseudoClass: TCSSPseudoClassElement; var TestNode: ICSSNode;
  OnlySpecifity: boolean): TCSSSpecifity;
var
  PseudoID: TCSSNumericalID;
begin
  if OnlySpecifity then
    exit(CSSSpecifityClass);
  Result:=CSSSpecifityNoMatch;
  PseudoID:=ResolveIdentifier(aPseudoClass,nikPseudoClass);
  case PseudoID of
  CSSIDNone:
    LogWarning(croErrorOnUnknownName in Options,20220911205605,'Unknown CSS selector pseudo attribute name "'+aPseudoClass.Name+'"',aPseudoClass);
  CSSPseudoID_Root:
    if TestNode.GetCSSParent=nil then
      Result:=CSSSpecifityClass;
  CSSPseudoID_Empty:
    if TestNode.GetCSSEmpty then
      Result:=CSSSpecifityClass;
  CSSPseudoID_FirstChild:
    if TestNode.GetCSSPreviousSibling=nil then
      Result:=CSSSpecifityClass;
  CSSPseudoID_LastChild:
    if TestNode.GetCSSNextSibling=nil then
      Result:=CSSSpecifityClass;
  CSSPseudoID_OnlyChild:
    if (TestNode.GetCSSNextSibling=nil)
        and (TestNode.GetCSSPreviousSibling=nil) then
      Result:=CSSSpecifityClass;
  CSSPseudoID_FirstOfType:
    if TestNode.GetCSSPreviousOfType=nil then
      Result:=CSSSpecifityClass;
  CSSPseudoID_LastOfType:
    if TestNode.GetCSSNextOfType=nil then
      Result:=CSSSpecifityClass;
  CSSPseudoID_OnlyOfType:
    if (TestNode.GetCSSNextOfType=nil)
        and (TestNode.GetCSSPreviousOfType=nil) then
      Result:=CSSSpecifityClass;
  else
    if TestNode.HasCSSPseudoClass(PseudoID) then
      Result:=CSSSpecifityClass;
  end;
end;

function TCSSResolver.SelectorListMatches(aList: TCSSListElement;
  const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity;
var
  i: Integer;
  El: TCSSElement;
  C: TClass;
  Specifity: TCSSSpecifity;
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
    if (C=TCSSIdentifierElement) and (i>0) then
    begin
      if OnlySpecifity then
        exit(0);
      Log(etWarning,20220914163218,'Type selector must be first',aList);
      exit(CSSSpecifityInvalid);
    end
    else if C=TCSSPseudoClassElement then
    begin
      Specifity:=SelectorPseudoClassMatches(TCSSPseudoClassElement(El),aNode,OnlySpecifity);
    end else
      Specifity:=SelectorMatches(El,aNode,OnlySpecifity);
    if Specifity<0 then
      exit(Specifity);
    inc(Result,Specifity);
    end;
end;

function TCSSResolver.SelectorBinaryMatches(aBinary: TCSSBinaryElement;
  const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity;
var
  aParent, Sibling: ICSSNode;
  aSpecifity: TCSSSpecifity;
begin
  if OnlySpecifity then
  begin
    Result:=SelectorMatches(aBinary.Left,TestNode,true);
    inc(Result,SelectorMatches(aBinary.Right,TestNode,true));
    exit;
  end;

  Result:=CSSSpecifityInvalid;
  case aBinary.Operation of
  boGT:
    begin
      // child combinator >
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      aParent:=TestNode.GetCSSParent;
      if aParent=nil then
        exit(CSSSpecifityNoMatch);
      aSpecifity:=SelectorMatches(aBinary.Left,aParent,false);
      if aSpecifity<0 then
        exit(aSpecifity);
      inc(Result,aSpecifity);
    end;
  boPlus:
    begin
      // adjacent sibling combinator +
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      Sibling:=TestNode.GetCSSPreviousSibling;
      if Sibling=nil then
        exit(CSSSpecifityNoMatch);
      aSpecifity:=SelectorMatches(aBinary.Left,Sibling,false);
      if aSpecifity<0 then
        exit(aSpecifity);
      inc(Result,aSpecifity);
    end;
  boTilde:
    begin
      // general sibling combinator ~
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      Sibling:=TestNode.GetCSSPreviousSibling;
      while Sibling<>nil do
      begin
        aSpecifity:=SelectorMatches(aBinary.Left,Sibling,false);
        if aSpecifity=CSSSpecifityInvalid then
          exit(aSpecifity)
        else if aSpecifity>=0 then
        begin
          inc(Result,aSpecifity);
          exit;
        end;
        Sibling:=Sibling.GetCSSPreviousSibling;
      end;
      Result:=CSSSpecifityNoMatch;
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
        exit(CSSSpecifityNoMatch);
      aSpecifity:=SelectorMatches(aBinary.Left,aParent,false);
      if aSpecifity>=0 then
      begin
        inc(Result,aSpecifity);
        exit;
      end
      else if aSpecifity=CSSSpecifityInvalid then
        exit(CSSSpecifityInvalid);
    until false;
    end
  else
    LogWarning(croErrorOnUnknownName in Options,20220910123724,'Invalid CSS binary selector '+BinaryOperators[aBinary.Operation],aBinary);
  end;
end;

function TCSSResolver.SelectorArrayMatches(anArray: TCSSArrayElement;
  const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity;
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
  if OnlySpecifity then
    exit(CSSSpecifityClass);

  Result:=CSSSpecifityInvalid;
  if anArray.Prefix<>nil then
  begin
    Log(etWarning,20220910154004,'Invalid CSS attribute selector prefix',anArray.Prefix);
    exit;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayMatches Prefix=',GetCSSObj(anArray.Prefix),' ChildCount=',anArray.ChildCount);
  for i:=0 to anArray.ChildCount-1 do
    writeln('TCSSResolver.SelectorArrayMatches ',i,' ',GetCSSObj(anArray.Children[i]));
  {$ENDIF}
  if anArray.ChildCount<1 then
  begin
    Log(etWarning,20220910154033,'Invalid CSS attribute selector',anArray);
    exit;
  end;
  OldStringComparison:=StringComparison;
  try
    if anArray.ChildCount>1 then
    begin
      El:=anArray.Children[1];
      C:=El.ClassType;
      if C=TCSSIdentifierElement then
      begin
        aValue:=TCSSIdentifierElement(El).Value;
        case aValue of
        'i': FStringComparison:=crscCaseInsensitive;
        's': FStringComparison:=crscCaseSensitive;
        else
          LogWarning(croErrorOnUnknownName in Options,20220914174409,'Invalid attribute modifier "'+aValue+'"',El);
          exit;
        end;
      end else begin
        Log(etWarning,20220914173643,'Invalid CSS attribute modifier',El);
        exit;
      end;
    end;
    if (anArray.ChildCount>2) then
      Log(etWarning,20220914174550,'Invalid CSS attribute modifier',anArray.Children[2]);

    El:=anArray.Children[0];
    C:=El.ClassType;
    if C=TCSSIdentifierElement then
    begin
      // [name]  ->  has attribute name
      AttrID:=ResolveIdentifier(TCSSIdentifierElement(El),nikAttribute);
      case AttrID of
      CSSIDNone:
        Result:=CSSSpecifityNoMatch;
      CSSAttributeID_ID,
      CSSAttributeID_Class:
        // basic CSS attributes are always defined
        Result:=CSSSpecifityClass;
      CSSAttributeID_All:
        // special CSS attributes without a value
        Result:=CSSSpecifityNoMatch;
      else
        if TestNode.HasCSSAttribute(AttrID) then
          Result:=CSSSpecifityClass
        else
          Result:=CSSSpecifityNoMatch;
      end;
    end else if C=TCSSBinaryElement then
      Result:=SelectorArrayBinaryMatches(TCSSBinaryElement(El),TestNode)
    else begin
      LogWarning(croErrorOnUnknownName in Options,20220910153725,'Invalid CSS array selector',El);
    end;
  finally
    FStringComparison:=OldStringComparison;
  end;
end;

function TCSSResolver.SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement;
  const TestNode: ICSSNode): TCSSSpecifity;
var
  Left, Right: TCSSElement;
  AttrID: TCSSNumericalID;
  LeftValue, RightValue: TCSSString;
  C: TClass;
begin
  Result:=CSSSpecifityNoMatch;
  Left:=aBinary.Left;
  if Left.ClassType<>TCSSIdentifierElement then
    Log(etError,20220910164353,'Invalid CSS array selector, expected attribute',Left);
  AttrID:=ResolveIdentifier(TCSSIdentifierElement(Left),nikAttribute);
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches AttrID=',AttrID,' Value=',TCSSIdentifierElement(Left).Value);
  {$ENDIF}
  case AttrID of
  CSSIDNone: exit(CSSSpecifityNoMatch);
  CSSAttributeID_ID:
    LeftValue:=TestNode.GetCSSID;
  CSSAttributeID_Class:
    LeftValue:=TestNode.GetCSSAttributeClass;
  CSSAttributeID_All: exit(CSSSpecifityNoMatch);
  else
    LeftValue:=TestNode.GetCSSAttribute(AttrID);
  end;

  Right:=aBinary.Right;
  C:=Right.ClassType;
  if (C=TCSSStringElement) or (C=TCSSIntegerElement) or (C=TCSSFloatElement)
      or (C=TCSSIdentifierElement) then
    // ok
  else
    Log(etError,20220910164921,'Invalid CSS array selector, expected string',Right);
  RightValue:=ComputeValue(Right);

  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Left="',LeftValue,'" Right="',RightValue,'" Op=',aBinary.Operation);
  {$ENDIF}
  case aBinary.Operation of
  boEquals:
    if SameValueText(LeftValue,RightValue) then
      Result:=CSSSpecifityClass;
  boSquaredEqual:
    // begins with
    if (RightValue<>'') and SameValueText(LeftStr(LeftValue,length(RightValue)),RightValue) then
      Result:=CSSSpecifityClass;
  boDollarEqual:
    // ends with
    if (RightValue<>'') and SameValueText(RightStr(LeftValue,length(RightValue)),RightValue) then
      Result:=CSSSpecifityClass;
  boPipeEqual:
    // equal to or starts with name-hyphen
    if (RightValue<>'')
        and (SameValueText(LeftValue,RightValue)
          or SameValueText(LeftStr(LeftValue,length(RightValue)+1),RightValue+'-')) then
      Result:=CSSSpecifityClass;
  boStarEqual:
    // contains substring
    if (RightValue<>'') and (Pos(RightValue,LeftValue)>0) then
      Result:=CSSSpecifityClass;
  boTildeEqual:
    // contains word
    if PosWord(RightValue,LeftValue)>0 then
      Result:=CSSSpecifityClass;
  else
    LogWarning(croErrorOnUnknownName in Options,20220910164356,'Invalid CSS array selector operator',aBinary);
    Result:=CSSSpecifityInvalid;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Result=',Result);
  {$ENDIF}
end;

function TCSSResolver.SelectorCallMatches(aCall: TCSSCallElement;
  const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity;
var
  CallID: TCSSNumericalID;
begin
  Result:=CSSSpecifityNoMatch;
  CallID:=ResolveCall(aCall);
  case CallID of
  CSSCallID_Not:
    Result:=Call_Not(aCall,TestNode,OnlySpecifity);
  CSSCallID_Is:
    Result:=Call_Is(aCall,TestNode,OnlySpecifity);
  CSSCallID_Where:
    Result:=Call_Where(aCall,TestNode,OnlySpecifity);
  CSSCallID_NthChild,CSSCallID_NthLastChild,CSSCallID_NthOfType, CSSCallID_NthLastOfType:
    Result:=Call_NthChild(CallID,aCall,TestNode,OnlySpecifity);
  else
    if OnlySpecifity then
      Result:=0
    else
      Result:=CSSSpecifityInvalid;
  end;
end;

function TCSSResolver.Call_Not(aCall: TCSSCallElement;
  const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity;
// :not(arg1, arg2, ...)
// :not(args) has the same specifity as :not(:is(args))
var
  i: Integer;
  Specifity: TCSSSpecifity;
  HasMatch: Boolean;
begin
  Result:=0;
  HasMatch:=false;
  for i:=0 to aCall.ArgCount-1 do
  begin
    Specifity:=SelectorMatches(aCall.Args[i],TestNode,OnlySpecifity);
    if Specifity>=0 then
      HasMatch:=true
    else begin
      // the specifity of :is is the highest, independent of matching (forgiving)
      Specifity:=SelectorMatches(aCall.Args[i],TestNode,true);
    end;
    if Specifity>Result then
      Result:=Specifity;
  end;
  if OnlySpecifity then
    // return best
  else if HasMatch then
    Result:=CSSSpecifityNoMatch;
end;

function TCSSResolver.Call_Is(aCall: TCSSCallElement; const TestNode: ICSSNode;
  OnlySpecifity: boolean): TCSSSpecifity;
var
  i: Integer;
  Specifity: TCSSSpecifity;
  ok: Boolean;
begin
  Result:=0;
  ok:=false;
  for i:=0 to aCall.ArgCount-1 do
  begin
    Specifity:=SelectorMatches(aCall.Args[i],TestNode,OnlySpecifity);
    if Specifity>=0 then
      ok:=true
    else begin
      // the specifity of :is is the highest, independent of matching (forgiving)
      Specifity:=SelectorMatches(aCall.Args[i],TestNode,true);
    end;
    if Specifity>Result then
      Result:=Specifity;
  end;
  if (not ok) and (not OnlySpecifity) then
    Result:=CSSSpecifityNoMatch;
end;

function TCSSResolver.Call_Where(aCall: TCSSCallElement;
  const TestNode: ICSSNode; OnlySpecifity: boolean): TCSSSpecifity;
var
  i: Integer;
begin
  Result:=0;
  if OnlySpecifity then
    exit;
  for i:=0 to aCall.ArgCount-1 do
  begin
    if SelectorMatches(aCall.Args[i],TestNode,false)>=0 then
      // Note: :where is forgiving, so invalid arguments are ignored
      exit;
  end;
  Result:=CSSSpecifityNoMatch;
end;

function TCSSResolver.Call_NthChild(CallID: TCSSNumericalID;
  aCall: TCSSCallElement; const TestNode: ICSSNode; OnlySpecifity: boolean
  ): TCSSSpecifity;

  procedure NthWarn(const ID: TCSSMsgID; const Msg: string; PosEl: TCSSElement);
  begin
    Log(etWarning,ID,CSSPseudoNames[CallID]+' '+Msg,PosEl);
  end;

var
  i, ArgCount, aModulo, aStart: Integer;
  Arg, OffsetEl: TCSSElement;
  Str: TCSSString;
  UnaryEl, anUnary: TCSSUnaryElement;
  Params: TCSSCallNthChildParams;
  CallData: TCSSCallData;
  ChildIDs: TIntegerDynArray;
begin
  if OnlySpecifity then
    Result:=CSSSpecifityClass
  else
    Result:=CSSSpecifityInvalid;
  CallData:=TCSSCallData(aCall.CustomData);
  Params:=TCSSCallNthChildParams(CallData.Params);
  if Params=nil then
  begin
    ArgCount:=aCall.ArgCount;
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolver.Call_NthChild ',aCall.ArgCount);
    for i:=0 to aCall.ArgCount-1 do
      writeln('TCSSResolver.Call_NthChild ',i,' ',GetCSSObj(aCall.Args[i]),' AsString=',aCall.Args[i].AsString);
    {$ENDIF}
    // An+B[of S], odd, even, An

    i:=0;
    aModulo:=0;
    aStart:=1;
    // check step
    if ArgCount<=i then
    begin
      NthWarn(20220915143843,'missing arguments',aCall);
      exit;
    end;
    Arg:=aCall.Args[i];
    if Arg.ClassType=TCSSIntegerElement then
    begin
      aModulo:=TCSSIntegerElement(Arg).Value;
      inc(i);
      // check n
      if ArgCount<=i then
      begin
        NthWarn(20220915143843,'missing arguments',aCall);
        exit;
      end;
      Arg:=aCall.Args[i];
      if Arg.ClassType<>TCSSIdentifierElement then
      begin
        NthWarn(20220915144312,'expected n',Arg);
        exit;
      end;
      if TCSSIdentifierElement(Arg).Value<>'n' then
      begin
        NthWarn(20220915144359,'expected n',Arg);
        exit;
      end;

    end
    else if Arg.ClassType=TCSSIdentifierElement then
    begin
      Str:=TCSSIdentifierElement(Arg).Value;
      case lowercase(Str) of
      'even':
        begin
        //writeln('TCSSResolver.Call_NthChild EVEN');
        aModulo:=2;
        aStart:=2;
        end;
      'odd':
        begin
        //writeln('TCSSResolver.Call_NthChild ODD');
        aModulo:=2;
        end;
      'n':
        begin
        //writeln('TCSSResolver.Call_NthChild N');
        aModulo:=1;
        end;
      else
        NthWarn(20220915150332,'expected multiplier',Arg);
        exit;
      end
    end else if Arg.ClassType=TCSSUnaryElement then
    begin
      anUnary:=TCSSUnaryElement(Arg);
      case anUnary.Operation of
      uoMinus: aModulo:=-1;
      uoPlus: aModulo:=1;
      else
        NthWarn(20220917080309,'expected multiplier',Arg);
        exit;
      end;
      if (anUnary.Right.ClassType=TCSSIdentifierElement)
          and (SameText(TCSSIdentifierElement(anUnary.Right).Value,'n')) then
      begin
        // ok
      end else begin
        NthWarn(20220917080154,'expected multiplier',Arg);
        exit;
      end;
    end else
    begin
      NthWarn(20220915144056,'expected multiplier',Arg);
      exit;
    end;

    inc(i);
    if ArgCount>i then
    begin
      Arg:=aCall.Args[i];
      if Arg.ClassType=TCSSUnaryElement then
      begin
        UnaryEl:=TCSSUnaryElement(Arg);
        //writeln('TCSSResolver.Call_NthChild UNARY ',UnaryEl.AsString);
        if not (UnaryEl.Operation in [uoMinus,uoPlus]) then
        begin
          NthWarn(20220915151422,'unexpected offset',UnaryEl);
          exit;
        end;
        OffsetEl:=UnaryEl.Right;
        if OffsetEl=nil then
        begin
          NthWarn(20220915151511,'unexpected offset',UnaryEl);
          exit;
        end;
        if OffsetEl.ClassType<>TCSSIntegerElement then
        begin
          NthWarn(20220915151718,'unexpected offset',OffsetEl);
          exit;
        end;
        aStart:=TCSSIntegerElement(OffsetEl).Value;
        if UnaryEl.Operation=uoMinus then
          aStart:=-aStart;
      end else
      begin
        NthWarn(20220915150851,'expected offset',Arg);
        exit;
      end;
    end;

    Params:=TCSSCallNthChildParams.Create;
    CallData.Params:=Params;
    Params.Modulo:=aModulo;
    Params.Start:=aStart;

    inc(i);
    if (i<ArgCount) then
    begin
      Arg:=aCall.Args[i];
      if (Arg.ClassType=TCSSIdentifierElement)
          and (SameText(TCSSIdentifierElement(Arg).Value,'of')) then
      begin
        // An+B of Selector
        inc(i);
        if i=ArgCount then
        begin
          NthWarn(20220915150851,'expected selector',Arg);
          exit;
        end;
        Arg:=aCall.Args[i];
        Params.HasOf:=true;
        Params.OfSelector:=Arg;
      end;
    end;

    if (CallID in [CSSCallID_NthOfType,CSSCallID_NthLastOfType]) then
      Params.HasOf:=true;
  end else begin
    aModulo:=Params.Modulo;
    aStart:=Params.Start;
  end;

  if OnlySpecifity then
  begin
    if Params.OfSelector<>nil then
      inc(Result,SelectorMatches(Params.OfSelector,TestNode,true));
    exit;
  end;

  Result:=CSSSpecifityNoMatch;
  if aModulo=0 then
    exit;
  i:=TestNode.GetCSSIndex;
  if Params.HasOf then
  begin
    ChildIDs:=CollectSiblingsOf(CallID,TestNode,Params);
    i:=GetSiblingOfIndex(ChildIDs,i);
  end else
    ChildIDs:=nil;
  {$IFDEF VerboseCSSResolver}
  //writeln('TCSSResolver.Call_NthChild CallID=',CallID,' ',aModulo,' * N + ',aStart,' Index=',TestNode.GetCSSIndex,' i=',i,' HasOf=',Params.HasOf,' OfChildCount=',length(Params.ChildIDs));
  {$ENDIF}
  if i<0 then
    exit;
  if CallID in [CSSCallID_NthLastChild,CSSCallID_NthLastOfType] then
  begin
    if Params.HasOf then
      i:=length(ChildIDs)-i
    else
      i:=GetSiblingCount(TestNode)-i;
  end else
  begin
    i:=i+1;
  end;
  dec(i,aStart);
  if i mod aModulo = 0 then
  begin
    i:=i div aModulo;
    if i>=0 then
      Result:=CSSSpecifityClass;
  end;
  {$IFDEF VerboseCSSResolver}
  //writeln('TCSSResolver.Call_NthChild ',aModulo,' * N + ',aStart,' Index=',TestNode.GetCSSIndex+1,' Result=',Result);
  {$ENDIF}
end;

function TCSSResolver.CollectSiblingsOf(CallID: TCSSNumericalID;
  TestNode: ICSSNode; Params: TCSSCallNthChildParams): TIntegerDynArray;
var
  i, Depth, ChildCount, j: Integer;
  aTypeID: TCSSNumericalID;
  aParent, aNode: ICSSNode;
  aSelector: TCSSElement;
  StackDepth: SizeInt;
  Cache: TCSSCallNthChildParamsCache;
  Item: PCSSCallNthChildParamsCacheItem;
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
    Cache:=TCSSCallNthChildParamsCache.Create;
    Params.StackCache[Depth]:=Cache;
    Cache.Owner:=Params;
    Cache.StackDepth:=Depth;
  end;

  NeedTypeID:=CallID in [CSSCallID_NthOfType,CSSCallID_NthLastOfType];

  if Cache.Parent<>aParent then
  begin
    // build cache
    Cache.Parent:=aParent;
    SetLength(Cache.Items,0);
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolver.CollectSiblingsOf Depth=',Depth,' Candidates=',ChildCount);
    {$ENDIF}
    aSelector:=Params.OfSelector;
    for i:=0 to ChildCount-1 do
    begin
      aNode:=aParent.GetCSSChild(i);
      if (aSelector<>nil) and (SelectorMatches(aSelector,aNode,false)<0) then
        continue;

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
  end else
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
  if C=TCSSIdentifierElement then
    Result:=TCSSIdentifierElement(El).Value
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
    AddElValueData(El,Result);
  end else begin
    LogWarning(croErrorOnUnknownName in Options,20220910235106,'TCSSResolver.ComputeValue not supported',El);
  end;
end;

function TCSSResolver.SameValueText(const A, B: TCSSString): boolean;
begin
  if StringComparison=crscCaseInsensitive then
    Result:=SameText(A,B)
  else
    Result:=A=B;
end;

function TCSSResolver.SameValueText(A: PAnsiChar; ALen: integer; B: PAnsiChar;
  BLen: integer): boolean;
var
  AC, BC: AnsiChar;
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
      if (AC<>BC) and (UpCase(AC)<>UpCase(BC)) then
        exit(false);
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
  SearchP, StrP: PAnsiChar;
  AC, BC: AnsiChar;
begin
  Result:=0;
  if SearchStr='' then exit;
  if Str='' then exit;
  if StringComparison=crscCaseInsensitive then
  begin
    SearchP:=PAnsiChar(SearchStr);
    StrP:=PAnsiChar(Str);
    SearchLen:=length(SearchStr);
    AC:=SearchP^;
    for i:=0 to length(Str)-SearchLen do
    begin
      BC:=StrP^;
      if (upcase(AC)=upcase(BC)) and SameValueText(SearchP,SearchLen,StrP,SearchLen) then
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

procedure TCSSResolver.MergeProperty(El: TCSSElement; Specifity: TCSSSpecifity);
var
  C: TClass;
  Decl: TCSSDeclarationElement;
  aKey, aValue: TCSSElement;
  AttrID: TCSSNumericalID;
  CompAttr: PCSSComputedAttribute;
begin
  C:=El.ClassType;
  if C=TCSSDeclarationElement then
  begin
    Decl:=TCSSDeclarationElement(El);
    if Decl.KeyCount<>1 then begin
      Log(etWarning,20220908232213,'Not yet implemented CSS declaration with KeyCount='+IntToStr(Decl.KeyCount),El);
      exit;
    end;
    if Decl.ChildCount<>1 then begin
      Log(etWarning,20220908232324,'Not yet implemented CSS declaration with ChildCount='+IntToStr(Decl.ChildCount),El);
      exit;
    end;

    aKey:=Decl.Keys[0];
    aValue:=Decl.Children[0];
    if Decl.IsImportant then
      Specifity:=CSSSpecifityImportant;

    C:=aKey.ClassType;
    if C=TCSSIdentifierElement then
    begin
      AttrID:=ResolveIdentifier(TCSSIdentifierElement(aKey),nikAttribute);
      if AttrID=CSSIDNone then
        Log(etWarning,20220909000932,'Unknown CSS property "'+TCSSIdentifierElement(aKey).Name+'"',aKey)
      else if AttrID=CSSAttributeID_All then
        // 'all'
        Log(etWarning,20220909001019,'Not yet implemented CSS property "'+TCSSIdentifierElement(aKey).Name+'"',aKey)
      else begin
        // set property
        CompAttr:=FindComputedAttribute(AttrID);
        if CompAttr<>nil then
        begin
          if CompAttr^.Specifity>Specifity then
            exit;
          if not CheckAttrValueValidity(AttrID,aKey,aValue) then
            exit;
          CompAttr^.Specifity:=Specifity;
          CompAttr^.Value:=aValue;
        end else begin
          if not CheckAttrValueValidity(AttrID,aKey,aValue) then
            exit;
          AddComputedAttribute(AttrID,Specifity,aValue);
        end;
      end;
    end else
      Log(etWarning,20220908232359,'Unknown CSS key',aKey);
  end else
    Log(etWarning,20220908230855,'Unknown CSS property',El);
end;

function TCSSResolver.CheckAttrValueValidity(AttrID: TCSSNumericalID; aKey,
  aValue: TCSSElement): boolean;
var
  Data: TCSSIdentifierData;
begin
  if not (aKey.CustomData is TCSSIdentifierData) then
    raise Exception.Create('TCSSResolver.CheckAttrValueValidity 20221019173901');
  Data:=TCSSIdentifierData(aKey.CustomData);
  case Data.ValueValid of
  cvvValid: exit(true);
  cvvInvalid: exit(false);
  end;
  Result:=FNode.CheckCSSValue(AttrID,aValue);
  if Result then
    Data.ValueValid:=cvvValid
  else
    Data.ValueValid:=cvvInvalid;
end;

function TCSSResolver.ResolveIdentifier(El: TCSSIdentifierElement;
  Kind: TCSSNumericalIDKind): TCSSNumericalID;
var
  Data: TObject;
  IdentData: TCSSIdentifierData;
  aName: TCSSString;
begin
  Data:=El.CustomData;
  if Data<>nil then
  begin
    IdentData:=TCSSIdentifierData(Data);
    Result:=IdentData.NumericalID;
    {$IFDEF VerboseCSSResolver}
    if IdentData.Kind<>Kind then
      Log(etError,20220908235300,'TCSSResolver.ResolveIdentifier',El);
    {$ENDIF}
  end else
  begin
    aName:=El.Name;
    Result:=CSSIDNone;

    // check built-in names
    case Kind of
    nikType:
      case aName of
      '*': Result:=CSSTypeID_Universal;
      end;
    nikAttribute:
      case aName of
      'id': Result:=CSSAttributeID_ID;
      'class': Result:=CSSAttributeID_Class;
      'all': Result:=CSSAttributeID_All;
      end;
    nikPseudoClass:
      begin
      aName:=lowercase(aName); // pseudo attributes are ASCII case insensitive
      case aName of
      ':root': Result:=CSSPseudoID_Root;
      ':empty': Result:=CSSPseudoID_Empty;
      ':first-child': Result:=CSSPseudoID_FirstChild;
      ':last-child': Result:=CSSPseudoID_LastChild;
      ':only-child': Result:=CSSPseudoID_OnlyChild;
      ':first-of-type': Result:=CSSPseudoID_FirstOfType;
      ':last-of-type': Result:=CSSPseudoID_LastOfType;
      ':only-of-type': Result:=CSSPseudoID_OnlyOfType;
      end;
      end;
    end;

    // resolve user defined names
    //writeln('TCSSResolver.ResolveIdentifier ',Kind,' "',aName,'"');
    if Result=CSSIDNone then
      Result:=FNumericalIDs[Kind][aName];

    if Result=CSSIDNone then
    begin
      LogWarning(croErrorOnUnknownName in FOptions,20220908235919,'TCSSResolver.ResolveIdentifier unknown '+CSSNumericalIDKindNames[Kind]+' "'+El.Name+'"',El);
      exit;
    end;
    IdentData:=TCSSIdentifierData.Create;
    IdentData.Kind:=Kind;
    IdentData.NumericalID:=Result;
    AddElData(El,IdentData);
  end;
end;

function TCSSResolver.ResolveCall(El: TCSSCallElement): TCSSNumericalID;
var
  Data: TObject;
  CallData: TCSSCallData;
  aName: TCSSString;
begin
  Data:=El.CustomData;
  if Data<>nil then
  begin
    CallData:=TCSSCallData(Data);
    Result:=CallData.NumericalID;
  end else
  begin
    aName:=El.Name;
    Result:=CSSIDNone;

    case aName of
    ':not': Result:=CSSCallID_Not;
    ':is': Result:=CSSCallID_Is;
    ':where': Result:=CSSCallID_Where;
    ':has': Result:=CSSCallID_Has;
    ':nth-child': Result:=CSSCallID_NthChild;
    ':nth-last-child': Result:=CSSCallID_NthLastChild;
    ':nth-of-type': Result:=CSSCallID_NthOfType;
    ':nth-last-of-type': Result:=CSSCallID_NthLastOfType;
    else
      LogWarning(croErrorOnUnknownName in FOptions,20220914193946,'TCSSResolver.ResolveCall unknown "'+El.Name+'"',El);
      exit;
    end;
    CallData:=TCSSCallData.Create;
    CallData.NumericalID:=Result;
    AddElData(El,CallData);
  end;
end;

procedure TCSSResolver.AddElData(El: TCSSElement; ElData: TCSSElResolverData);
begin
  El.CustomData:=ElData;
  ElData.Element:=El;
  if FFirstElData=nil then
  begin
    FFirstElData:=ElData;
  end else begin
    FLastElData.Next:=ElData;
    ElData.Prev:=FLastElData;
  end;
  FLastElData:=ElData;
end;

function TCSSResolver.AddElValueData(El: TCSSElement; const aValue: TCSSString
  ): TCSSValueData;
begin
  Result:=TCSSValueData.Create;
  Result.NormValue:=aValue;
  AddElData(El,Result);
end;

function TCSSResolver.FindComputedAttribute(AttrID: TCSSNumericalID
  ): PCSSComputedAttribute;
var
  i: Integer;
begin
  for i:=0 to FAttributeCount-1 do
    if FAttributes[i].AttrID=AttrID then
      exit(@FAttributes[i]);
  Result:=nil;
end;

function TCSSResolver.AddComputedAttribute(TheAttrID: TCSSNumericalID;
  aSpecifity: TCSSSpecifity; aValue: TCSSElement): PCSSComputedAttribute;
var
  NewLength: Integer;
begin
  if FAttributeCount=length(FAttributes) then
  begin
    NewLength:=FAttributeCount*2;
    if NewLength<16 then
      NewLength:=16;
    SetLength(FAttributes,NewLength);
  end;
  with FAttributes[FAttributeCount] do
  begin
    AttrID:=TheAttrID;
    Specifity:=aSpecifity;
    Value:=aValue;
  end;
  Result:=@FAttributes[FAttributeCount];
  inc(FAttributeCount);
end;

procedure TCSSResolver.LogWarning(IsError: boolean; const ID: TCSSMsgID;
  Msg: string; PosEl: TCSSElement);
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
  Msg: string; PosEl: TCSSElement);
var
  Entry: TCSSResolverLogEntry;
  i: Integer;
begin
  if Assigned(OnLog) then
  begin
    for i:=0 to FLogEntries.Count-1 do
    begin
      Entry:=LogEntries[i];
      if (Entry.PosEl=PosEl)
          and (Entry.ID=ID)
          and (Entry.MsgType=MsgType)
          and (Entry.Msg=Msg) then
        exit; // this warning was already logged
    end;
    Entry:=TCSSResolverLogEntry.Create;
    Entry.MsgType:=MsgType;
    Entry.ID:=ID;
    Entry.Msg:=Msg;
    Entry.PosEl:=PosEl;
    FLogEntries.Add(Entry);
    OnLog(Self,Entry);
  end;
  if (MsgType=etError) or (FOnLog=nil) then
  begin
    Msg:='['+IntToStr(ID)+'] '+Msg+' at '+GetElPos(PosEl);
    raise ECSSResolver.Create(Msg);
  end;
end;

function TCSSResolver.GetElPos(El: TCSSElement): string;
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

function TCSSResolver.GetElPath(El: TCSSElement): string;
begin
  Result:=GetCSSPath(El);
end;

constructor TCSSResolver.Create(AOwner: TComponent);
begin
  inherited;
  FLogEntries:=TFPObjectList.Create(true);
end;

destructor TCSSResolver.Destroy;
begin
  Clear;
  FreeAndNil(FLogEntries);
  inherited Destroy;
end;

procedure TCSSResolver.Clear;
begin
  FLogEntries.Clear;
  ClearStyleCustomData;
  ClearStyles;
end;

procedure TCSSResolver.ClearStyleCustomData;
var
  Data: TCSSElResolverData;
begin
  while FLastElData<>nil do
  begin
    Data:=FLastElData;
    FLastElData:=Data.Prev;
    if FLastElData<>nil then
      FLastElData.Next:=nil
    else
      FFirstElData:=nil;
    if Data.Element.CustomData<>Data then
      Log(etError,20220908234726,'TCSSResolver.ClearStyleCustomData',Data.Element);
    Data.Element.CustomData:=nil;
    Data.Free;
  end;
end;

procedure TCSSResolver.Compute(Node: ICSSNode; NodeStyle: TCSSElement;
  const CompOptions: TCSSComputeOptions);
var
  i: Integer;
begin
  FNode:=Node;
  try
    FAttributeCount:=0;
    for i:=0 to high(FStyles) do
      ComputeElement(Styles[i]);
    ComputeInline(NodeStyle);
    if ccoCommit in CompOptions then
      Commit;
  finally
    FNode:=nil;
  end;
end;

procedure TCSSResolver.Commit;
var
  i: Integer;
begin
  //writeln('TCSSResolver.Commit FAttributeCount=',FAttributeCount);
  for i:=0 to FAttributeCount-1 do
    with FAttributes[i] do
      FNode.SetCSSValue(AttrID,Value);
end;

procedure TCSSResolver.AddStyle(aStyle: TCSSElement);
begin
  if aStyle=nil then exit;
  Insert(aStyle,FStyles,length(FStyles));
end;

function TCSSResolver.IndexOfStyle(aStyle: TCSSElement): integer;
begin
  Result:=high(FStyles);
  while (Result>=0) and (FStyles[Result]<>aStyle) do dec(Result);
end;

procedure TCSSResolver.RemoveStyle(aStyle: TCSSElement);
var
  i: Integer;
begin
  i:=IndexOfStyle(aStyle);
  if i<0 then exit;
  DeleteStyle(i);
end;

procedure TCSSResolver.DeleteStyle(aIndex: integer);
begin
  if (aIndex<0) or (aIndex>=length(FStyles)) then
    raise ECSSResolver.Create('TCSSResolver.DeleteStyle index '+IntToStr(aIndex)+' out of bounds '+IntToStr(length(FStyles)));
  if OwnsStyle then
    FStyles[aIndex].Free;
  Delete(FStyles,aIndex,1);
end;

procedure TCSSResolver.ClearStyles;
var
  i: Integer;
begin
  if OwnsStyle then
    for i:=0 to high(FStyles) do
      FStyles[i].Free;
  FStyles:=nil;
end;

end.

