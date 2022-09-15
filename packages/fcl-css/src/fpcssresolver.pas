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

ToDo:
- descendant combinator
- and combinator
- 'all' attribute
- TCSSResolver.FindComputedAttribute  use binary search for >8 elements
- CSSSpecifityInline
- namespaces
- layers

}

unit fpCSSResolver;

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, Contnrs, StrUtils, fpCSSTree;

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
  CSSTypeID_Universal = 1; // id of type '*'
  CSSAttributeID_ID = 1; // id of attribute key 'id'
  CSSAttributeID_Class = 2; // id of attribute key 'class'
  CSSAttributeID_All = 3; // id of attribute key 'all'
  // pseudo attribute IDs
  CSSPseudoID_Root = 1; // :root
  CSSPseudoID_Empty = 2; // :empty
  CSSPseudoID_FirstChild = 3; // :first-child
  CSSPseudoID_LastChild = 4; // :last-child
  CSSPseudoID_OnlyChild = 5; // :only-child
  CSSPseudoID_FirstOfType = 6; // :first-of-type
  CSSPseudoID_LastOfType = 7; // :last-of-type
  CSSPseudoID_OnlyOfType = 8; // :only-of-type
  CSSCallID_NthChild = 9; // :nth-child

type
  TCSSMsgID = int64;
  TCSSNumericalID = integer;
  TCSSSpecifity = integer;

  ECSSResolver = class(Exception)
  end;

  TCSSAttributeMatchKind = (
    camkEqual,
    camkContains,
    camkContainsWord,
    camkBegins,
    camkEnds
    );
  TCSSAttributeMatchKinds = set of TCSSAttributeMatchKind;

  { TCSSNode }

  TCSSNode = interface
    function GetCSSID: TCSSString;
    function GetCSSTypeName: TCSSString;
    function GetCSSTypeID: TCSSNumericalID;
    function HasCSSClass(const aClassName: TCSSString): boolean;
    function GetCSSAttributeClass: TCSSString;
    function GetCSSParent: TCSSNode;
    function GetCSSIndex: integer; // node index in parent's children
    function GetCSSNextSibling: TCSSNode;
    function GetCSSPreviousSibling: TCSSNode;
    function GetCSSChildCount: integer;
    function GetCSSChild(const anIndex: integer): TCSSNode;
    function GetCSSNextOfType: TCSSNode;
    function GetCSSPreviousOfType: TCSSNode;
    function HasCSSAttribute(const AttrID: TCSSNumericalID): boolean;
    function GetCSSAttribute(const AttrID: TCSSNumericalID): TCSSString;
    function HasCSSPseudoAttribute(const AttrID: TCSSNumericalID): boolean;
    function GetCSSPseudoAttribute(const AttrID: TCSSNumericalID): TCSSString;
    function GetCSSEmpty: boolean;
    procedure SetCSSValue(AttrID: TCSSNumericalID; Value: TCSSElement);
  end;

type
  TCSSNumericalIDKind = (
    nikType,
    nikAttribute,
    nikPseudoAttribute
    );
  TCSSNumericalIDKinds = set of TCSSNumericalIDKind;

const
  CSSNumericalIDKindNames: array[TCSSNumericalIDKind] of TCSSString = (
    'Type',
    'Attribute',
    'PseudoAttribute'
    );

type

  { TCSSNumericalIDs }

  TCSSNumericalIDs = class
  private
    FKind: TCSSNumericalIDKind;
    fList: TFPHashList;
    function GetIDs(const aName: TCSSString): TCSSNumericalID;
    procedure SetIDs(const aName: TCSSString; const AValue: TCSSNumericalID);
  public
    constructor Create(aKind: TCSSNumericalIDKind);
    destructor Destroy; override;
    procedure Clear;
    property IDs[const aName: TCSSString]: TCSSNumericalID read GetIDs write SetIDs; default;
    property Kind: TCSSNumericalIDKind read FKind;
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

  TCSSIdentifierData = class(TCSSElResolverData)
  public
    NumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
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

  TCSSCallNthChildParams = class
    Modulo: integer;
    Start: integer;
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
  TCSSResStringComparison = (
    crscDefault,
    crscCaseInsensitive,
    crscCaseSensitive
    );
  TCSSResStringComparisons = set of TCSSResStringComparison;

  TCSSResolverLogEvent = procedure(Sender: TObject; aType: TEventType;
    const ID: TCSSMsgID; const Msg: string; PosEl: TCSSElement) of object;

  { TCSSResolver }

  TCSSResolver = class
  private
    FNumericalIDs: array[TCSSNumericalIDKind] of TCSSNumericalIDs;
    FOnLog: TCSSResolverLogEvent;
    FOptions: TCSSResolverOptions;
    FStringComparison: TCSSResStringComparison;
    FStyle: TCSSElement;
    FOwnsStyle: boolean;
    FFirstElData: TCSSElResolverData;
    FLastElData: TCSSElResolverData;
    function GetAttributes(Index: integer): PCSSComputedAttribute;
    function GetNumericalIDs(Kind: TCSSNumericalIDKind): TCSSNumericalIDs;
    procedure SetNumericalIDs(Kind: TCSSNumericalIDKind;
      const AValue: TCSSNumericalIDs);
    procedure SetOptions(const AValue: TCSSResolverOptions);
  protected
    FAttributes: TCSSComputedAttributeArray;
    FAttributeCount: integer;
    FNode: TCSSNode;
    procedure SetStyle(const AValue: TCSSElement); virtual;
    procedure ComputeElement(El: TCSSElement); virtual;
    procedure ComputeRule(aRule: TCSSRuleElement); virtual;
    procedure ComputeInline(El: TCSSElement); virtual;
    procedure ComputeInlineRule(aRule: TCSSRuleElement); virtual;
    function SelectorMatches(aSelector: TCSSElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorIdentifierMatches(Identifier: TCSSIdentifierElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorHashIdentifierMatches(Identifier: TCSSHashIdentifierElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorClassNameMatches(aClassName: TCSSClassNameElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorPseudoClassMatches(aPseudoClass: TCSSPseudoClassElement; var TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorListMatches(aList: TCSSListElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorArrayMatches(anArray: TCSSArrayElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorCallMatches(aCall: TCSSCallElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function Call_NthChild(aCall: TCSSCallElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function ComputeValue(El: TCSSElement): TCSSString; virtual;
    function SameValueText(const A, B: TCSSString): boolean; virtual;
    function SameValueText(A: PChar; ALen: integer; B: PChar; BLen: integer): boolean; virtual;
    function PosSubString(const SearchStr, Str: TCSSString): integer; virtual;
    function PosWord(const SearchWord, Words: TCSSString): integer; virtual;
    procedure MergeProperty(El: TCSSElement; Specifity: TCSSSpecifity); virtual;
    function ResolveIdentifier(El: TCSSIdentifierElement; Kind: TCSSNumericalIDKind): TCSSNumericalID; virtual;
    function ResolveCall(El: TCSSCallElement): TCSSNumericalID; virtual;
    procedure AddElData(El: TCSSElement; ElData: TCSSElResolverData); virtual;
    function AddElValueData(El: TCSSElement; const aValue: TCSSString): TCSSValueData; virtual;
    function FindComputedAttribute(AttrID: TCSSNumericalID): PCSSComputedAttribute;
    function AddComputedAttribute(TheAttrID: TCSSNumericalID; aSpecifity: TCSSSpecifity;
                          aValue: TCSSElement): PCSSComputedAttribute;
    procedure Log(MsgType: TEventType; const ID: TCSSMsgID; Msg: string; PosEl: TCSSElement); virtual;
    function GetElPos(El: TCSSElement): string; virtual;
    function GetElPath(El: TCSSElement): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearStyleCustomData; virtual;
    procedure Compute(Node: TCSSNode; NodeStyle: TCSSElement = nil;
      const CompOptions: TCSSComputeOptions = DefaultCSSComputeOptions); virtual;
    procedure Commit; virtual;
    property Style: TCSSElement read FStyle write SetStyle;
    property OwnsStyle: boolean read FOwnsStyle write FOwnsStyle default false;
    property NumericalIDs[Kind: TCSSNumericalIDKind]: TCSSNumericalIDs read GetNumericalIDs write SetNumericalIDs;
    property Options: TCSSResolverOptions read FOptions write SetOptions;
    property Attributes[Index: integer]: PCSSComputedAttribute read GetAttributes;
    property AttributeCount: integer read FAttributeCount;
    property StringComparison: TCSSResStringComparison read FStringComparison;
    property OnLog: TCSSResolverLogEvent read FOnLog write FOnLog;
  end;

implementation

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

function TCSSResolver.GetAttributes(Index: integer): PCSSComputedAttribute;
begin
  if (Index<0) or (Index>=FAttributeCount) then
    raise ECSSResolver.Create('TCSSResolver.GetAttributes index out of bounds');
  Result:=@FAttributes[Index];
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

procedure TCSSResolver.SetStyle(const AValue: TCSSElement);
begin
  if FStyle=AValue then Exit;
  if FOwnsStyle then
    FStyle.Free;
  FStyle:=AValue;
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
    for i:=0 to Compound.ChildCount-1 do
      ComputeElement(Compound.Children[i]);
  end else if C=TCSSRuleElement then
    ComputeRule(TCSSRuleElement(El))
  else
    Log(etError,20220908150252,'Unknown CSS element',El);
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
    Specifity:=SelectorMatches(aSelector,FNode);
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
    Log(etError,20220915140402,'TCSSResolver.ComputeInline Not yet supported inline element',El);
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
  const TestNode: TCSSNode): TCSSSpecifity;

  procedure MatchPseudo;
  var
    aNode: TCSSNode;
  begin
    aNode:=TestNode;
    Result:=SelectorPseudoClassMatches(TCSSPseudoClassElement(aSelector),aNode);
  end;

var
  C: TClass;
begin
  Result:=CSSSpecifityInvalid;
  C:=aSelector.ClassType;
  if C=TCSSIdentifierElement then
    Result:=SelectorIdentifierMatches(TCSSIdentifierElement(aSelector),TestNode)
  else if C=TCSSHashIdentifierElement then
    Result:=SelectorHashIdentifierMatches(TCSSHashIdentifierElement(aSelector),TestNode)
  else if C=TCSSClassNameElement then
    Result:=SelectorClassNameMatches(TCSSClassNameElement(aSelector),TestNode)
  else if C=TCSSPseudoClassElement then
    MatchPseudo
  else if C=TCSSBinaryElement then
    Result:=SelectorBinaryMatches(TCSSBinaryElement(aSelector),TestNode)
  else if C=TCSSArrayElement then
    Result:=SelectorArrayMatches(TCSSArrayElement(aSelector),TestNode)
  else if C=TCSSListElement then
    Result:=SelectorListMatches(TCSSListElement(aSelector),TestNode)
  else if C=TCSSCallElement then
    Result:=SelectorCallMatches(TCSSCallElement(aSelector),TestNode)
  else
    Log(etError,20220908230152,'Unknown CSS selector element',aSelector);
end;

function TCSSResolver.SelectorIdentifierMatches(
  Identifier: TCSSIdentifierElement; const TestNode: TCSSNode): TCSSSpecifity;
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
  end else if TypeID=CSSIDNone then
  begin
    if croErrorOnUnknownName in Options then
      Log(etError,20220911230224,'Unknown CSS selector type name "'+Identifier.Name+'"',Identifier);
    Result:=CSSSpecifityInvalid;
  end else if TypeID=TestNode.GetCSSTypeID then
    Result:=CSSSpecifityType;
end;

function TCSSResolver.SelectorHashIdentifierMatches(
  Identifier: TCSSHashIdentifierElement; const TestNode: TCSSNode
  ): TCSSSpecifity;
var
  aValue: TCSSString;
begin
  Result:=CSSSpecifityNoMatch;
  aValue:=Identifier.Value;
  if TestNode.GetCSSID=aValue then
    Result:=CSSSpecifityIdentifier;
end;

function TCSSResolver.SelectorClassNameMatches(
  aClassName: TCSSClassNameElement; const TestNode: TCSSNode): TCSSSpecifity;
var
  aValue: TCSSString;
begin
  aValue:=aClassName.Name;
  if TestNode.HasCSSClass(aValue) then
    Result:=CSSSpecifityClass
  else
    Result:=CSSSpecifityNoMatch;
  //writeln('TCSSResolver.SelectorClassNameMatches ',aValue,' ',Result);
end;

function TCSSResolver.SelectorPseudoClassMatches(
  aPseudoClass: TCSSPseudoClassElement; var TestNode: TCSSNode): TCSSSpecifity;
var
  PseudoID: TCSSNumericalID;
begin
  Result:=CSSSpecifityNoMatch;
  PseudoID:=ResolveIdentifier(aPseudoClass,nikPseudoAttribute);
  case PseudoID of
  CSSIDNone:
    if croErrorOnUnknownName in Options then
      Log(etError,20220911205605,'Unknown CSS selector pseudo attribute name "'+aPseudoClass.Name+'"',aPseudoClass);
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
    if TestNode.GetCSSPseudoAttribute(PseudoID)<>'' then
      Result:=CSSSpecifityClass;
  end;
end;

function TCSSResolver.SelectorListMatches(aList: TCSSListElement;
  const TestNode: TCSSNode): TCSSSpecifity;
var
  i: Integer;
  El: TCSSElement;
  C: TClass;
  Specifity: TCSSSpecifity;
  aNode: TCSSNode;
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
      Log(etError,20220914163218,'Type selector must be first',aList)
    else if C=TCSSPseudoClassElement then
    begin
      Specifity:=SelectorPseudoClassMatches(TCSSPseudoClassElement(El),aNode);
    end else
      Specifity:=SelectorMatches(El,aNode);
    if Specifity<0 then
      exit(Specifity);
    inc(Result,Specifity);
    end;
end;

function TCSSResolver.SelectorBinaryMatches(aBinary: TCSSBinaryElement;
  const TestNode: TCSSNode): TCSSSpecifity;
var
  aParent, Sibling: TCSSNode;
  aSpecifity: TCSSSpecifity;
begin
  Result:=CSSSpecifityInvalid;
  case aBinary.Operation of
  boGT:
    begin
      // child combinator >
      Result:=SelectorMatches(aBinary.Right,TestNode);
      if Result<0 then exit;
      aParent:=TestNode.GetCSSParent;
      if aParent=nil then
        exit(CSSSpecifityNoMatch);
      aSpecifity:=SelectorMatches(aBinary.Left,aParent);
      if aSpecifity<0 then
        exit(aSpecifity);
      inc(Result,aSpecifity);
    end;
  boPlus:
    begin
      // adjacent sibling combinator +
      Result:=SelectorMatches(aBinary.Right,TestNode);
      if Result<0 then exit;
      Sibling:=TestNode.GetCSSPreviousSibling;
      if Sibling=nil then
        exit(CSSSpecifityNoMatch);
      aSpecifity:=SelectorMatches(aBinary.Left,Sibling);
      if aSpecifity<0 then
        exit(aSpecifity);
      inc(Result,aSpecifity);
    end;
  boTilde:
    begin
      // general sibling combinator ~
      Result:=SelectorMatches(aBinary.Right,TestNode);
      if Result<0 then exit;
      Sibling:=TestNode.GetCSSPreviousSibling;
      while Sibling<>nil do
      begin
        aSpecifity:=SelectorMatches(aBinary.Left,Sibling);
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
    Result:=SelectorMatches(aBinary.Right,TestNode);
    if Result<0 then exit;
    aParent:=TestNode;
    repeat
      aParent:=aParent.GetCSSParent;
      if aParent=nil then
        exit(CSSSpecifityNoMatch);
      aSpecifity:=SelectorMatches(aBinary.Left,aParent);
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
    if croErrorOnUnknownName in Options then
      Log(etError,20220910123724,'Invalid CSS binary selector '+BinaryOperators[aBinary.Operation],aBinary);
  end;
end;

function TCSSResolver.SelectorArrayMatches(anArray: TCSSArrayElement;
  const TestNode: TCSSNode): TCSSSpecifity;
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
  Result:=CSSSpecifityInvalid;
  if anArray.Prefix<>nil then
    Log(etError,20220910154004,'Invalid CSS attribute selector prefix',anArray.Prefix);
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayMatches Prefix=',GetCSSObj(anArray.Prefix),' ChildCount=',anArray.ChildCount);
  for i:=0 to anArray.ChildCount-1 do
    writeln('TCSSResolver.SelectorArrayMatches ',i,' ',GetCSSObj(anArray.Children[i]));
  {$ENDIF}
  if anArray.ChildCount<1 then
    Log(etError,20220910154033,'Invalid CSS attribute selector',anArray);
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
          if croErrorOnUnknownName in Options then
            Log(etError,20220914174409,'Invalid attribute modifier "'+aValue+'"',El);
        end;
      end else
        Log(etError,20220914173643,'Invalid CSS attribute modifier',El);
      end;
    if (anArray.ChildCount>2) and (croErrorOnUnknownName in Options) then
      Log(etError,20220914174550,'Invalid CSS attribute modifier',anArray.Children[2]);

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
    else
      Log(etError,20220910153725,'Invalid CSS array selector',El);
  finally
    FStringComparison:=OldStringComparison;
  end;
end;

function TCSSResolver.SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement;
  const TestNode: TCSSNode): TCSSSpecifity;
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
    if croErrorOnUnknownName in Options then
      Log(etError,20220910164356,'Invalid CSS array selector operator',aBinary);
    Result:=CSSSpecifityInvalid;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Result=',Result);
  {$ENDIF}
end;

function TCSSResolver.SelectorCallMatches(aCall: TCSSCallElement;
  const TestNode: TCSSNode): TCSSSpecifity;
var
  CallID: TCSSNumericalID;
begin
  Result:=CSSSpecifityNoMatch;
  CallID:=ResolveCall(aCall);
  case CallID of
  CSSCallID_NthChild:
    Result:=Call_NthChild(aCall,TestNode);
  else
    Result:=CSSSpecifityInvalid;
  end;
end;

function TCSSResolver.Call_NthChild(aCall: TCSSCallElement;
  const TestNode: TCSSNode): TCSSSpecifity;
var
  i, ArgCount, aModulo, aStart: Integer;
  Arg, OffsetEl: TCSSElement;
  Str: TCSSString;
  UnaryEl: TCSSUnaryElement;
  Params: TCSSCallNthChildParams;
  CallData: TCSSCallData;
begin
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
    aStart:=0;
    // check step
    if ArgCount<=i then
    begin
      Log(etWarning,20220915143843,':nth-child missing arguments',aCall);
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
        Log(etWarning,20220915143843,':nth-child missing arguments',aCall);
        exit;
      end;
      Arg:=aCall.Args[i];
      if Arg.ClassType<>TCSSIdentifierElement then
      begin
        Log(etWarning,20220915144312,':nth-child expected n',Arg);
        exit;
      end;
      if TCSSIdentifierElement(Arg).Value<>'n' then
      begin
        Log(etWarning,20220915144359,':nth-child expected n',Arg);
        exit;
      end;

    end
    else if Arg.ClassType=TCSSIdentifierElement then
    begin
      Str:=TCSSIdentifierElement(Arg).Value;
      case lowercase(Str) of
      'even':
        begin
        writeln('TCSSResolver.Call_NthChild EVEN');
        aModulo:=2;
        aStart:=2;
        end;
      'odd':
        begin
        writeln('TCSSResolver.Call_NthChild ODD');
        aModulo:=2;
        aStart:=1;
        end;
      'n':
        begin
        writeln('TCSSResolver.Call_NthChild N');
        aModulo:=1;
        aStart:=1;
        end;
      else
        Log(etWarning,20220915150332,':nth-child expected multiplier',Arg);
        exit;
      end;
    end else
    begin
      Log(etWarning,20220915144056,':nth-child expected multiplier',Arg);
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
          Log(etWarning,20220915151422,':nth-child unexpected offset',UnaryEl);
          exit;
        end;
        OffsetEl:=UnaryEl.Right;
        if OffsetEl=nil then
        begin
          Log(etWarning,20220915151511,':nth-child unexpected offset',UnaryEl);
          exit;
        end;
        if OffsetEl.ClassType<>TCSSIntegerElement then
        begin
          Log(etWarning,20220915151718,':nth-child unexpected offset',OffsetEl);
          exit;
        end;
        aStart:=TCSSIntegerElement(OffsetEl).Value;
        if UnaryEl.Operation=uoMinus then
          aStart:=-aStart;
      end else
      begin
        Log(etWarning,20220915150851,':nth-child expected offset',Arg);
        exit;
      end;
    end;

    Params:=TCSSCallNthChildParams.Create;
    CallData.Params:=Params;
    Params.Modulo:=aModulo;
    Params.Start:=aStart;
  end else begin
    aModulo:=Params.Modulo;
    aStart:=Params.Start;
  end;

  Result:=CSSSpecifityNoMatch;
  if aModulo<1 then
    exit;
  i:=TestNode.GetCSSIndex;
  if i<0 then
    exit;
  i:=i+1-aStart;
  if i mod aModulo = 0 then
  begin
    i:=i div aModulo;
    if i>=0 then
      Result:=CSSSpecifityClass
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.Call_NthChild ',aModulo,' * N + ',aStart,' Index=',TestNode.GetCSSIndex+1,' Result=',Result);
  {$ENDIF}
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
  end else
    Log(etError,20220910235106,'TCSSResolver.ComputeValue not supported',El);
end;

function TCSSResolver.SameValueText(const A, B: TCSSString): boolean;
begin
  if StringComparison=crscCaseInsensitive then
    Result:=SameText(A,B)
  else
    Result:=A=B;
end;

function TCSSResolver.SameValueText(A: PChar; ALen: integer; B: PChar;
  BLen: integer): boolean;
var
  AC, BC: Char;
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
  SearchP, StrP: PChar;
  AC, BC: Char;
begin
  Result:=0;
  if SearchStr='' then exit;
  if Str='' then exit;
  if StringComparison=crscCaseInsensitive then
  begin
    SearchP:=PChar(SearchStr);
    StrP:=PChar(Str);
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
    if Decl.KeyCount<>1 then
      Log(etError,20220908232213,'Not yet implemented CSS declaration with KeyCount='+IntToStr(Decl.KeyCount),El);
    if Decl.ChildCount<>1 then
      Log(etError,20220908232324,'Not yet implemented CSS declaration with ChildCount='+IntToStr(Decl.ChildCount),El);

    aKey:=Decl.Keys[0];
    aValue:=Decl.Children[0];
    if Decl.IsImportant then
      Specifity:=CSSSpecifityImportant;

    C:=aKey.ClassType;
    if C=TCSSIdentifierElement then
    begin
      AttrID:=ResolveIdentifier(TCSSIdentifierElement(aKey),nikAttribute);
      if AttrID=CSSIDNone then
        Log(etError,20220909000932,'Unknown CSS property "'+TCSSIdentifierElement(aKey).Name+'"',aKey)
      else if AttrID=CSSAttributeID_All then
        // 'all'
        Log(etError,20220909001019,'Not yet implemented CSS property "'+TCSSIdentifierElement(aKey).Name+'"',aKey)
      else begin
        // set property
        CompAttr:=FindComputedAttribute(AttrID);
        if CompAttr<>nil then
        begin
          if CompAttr^.Specifity>Specifity then
            exit;
          CompAttr^.Specifity:=Specifity;
          CompAttr^.Value:=aValue;
        end else begin
          AddComputedAttribute(AttrID,Specifity,aValue);
        end;
      end;
    end else
      Log(etError,20220908232359,'Unknown CSS key',aKey);
  end else
    Log(etError,20220908230855,'Unknown CSS property',El);
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
    nikPseudoAttribute:
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
    if Result=CSSIDNone then
      Result:=FNumericalIDs[Kind][aName];

    if Result=CSSIDNone then
    begin
      if croErrorOnUnknownName in FOptions then
        Log(etError,20220908235919,'TCSSResolver.ResolveIdentifier unknown '+CSSNumericalIDKindNames[Kind]+' "'+El.Name+'"',El);
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
    ':nth-child': Result:=CSSCallID_NthChild;
    else
      if croErrorOnUnknownName in FOptions then
        Log(etError,20220914193946,'TCSSResolver.ResolveCall unknown "'+El.Name+'"',El);
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

procedure TCSSResolver.Log(MsgType: TEventType; const ID: TCSSMsgID;
  Msg: string; PosEl: TCSSElement);
begin
  if assigned(OnLog) then
    OnLog(Self,MsgType,ID,Msg,PosEl);
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

constructor TCSSResolver.Create;
begin

end;

destructor TCSSResolver.Destroy;
begin
  if FOwnsStyle then
    FStyle.Free;
  FStyle:=nil;
  inherited Destroy;
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

procedure TCSSResolver.Compute(Node: TCSSNode; NodeStyle: TCSSElement;
  const CompOptions: TCSSComputeOptions);
begin
  FNode:=Node;
  try
    FAttributeCount:=0;
    ComputeElement(Style);
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
  for i:=0 to FAttributeCount-1 do
    with FAttributes[i] do
      FNode.SetCSSValue(AttrID,Value);
end;

end.

