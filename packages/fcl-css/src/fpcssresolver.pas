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
  CSSAttributeID_All = 2; // id of attribute key 'all'
  // pseudo attribute IDs
  CSSPseudoID_Root = 1; // :root
  CSSPseudoID_Empty = 2; // :empty
  CSSPseudoID_FirstChild = 3; // :first-child
  CSSPseudoID_LastChild = 4; // :last-child
  CSSPseudoID_OnlyChild = 5; // :only-child
  CSSPseudoID_FirstOfType = 6; // :first-of-type
  CSSPseudoID_LastOfType = 7; // :last-of-type
  CSSPseudoID_OnlyOfType = 8; // :only-of-type

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

  { TCSSResolver }

  TCSSResolver = class
  private
    FNumericalIDs: array[TCSSNumericalIDKind] of TCSSNumericalIDs;
    FOptions: TCSSResolverOptions;
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
    function SelectorMatches(aSelector: TCSSElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorIdentifierMatches(Identifier: TCSSIdentifierElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorClassNameMatches(aClassName: TCSSClassNameElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorPseudoClassMatches(aPseudoClass: TCSSPseudoClassElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorStringMatches(aString: TCSSStringElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorListMatches(aList: TCSSListElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorArrayMatches(anArray: TCSSArrayElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: TCSSNode): TCSSSpecifity; virtual;
    function ComputeValue(El: TCSSElement): TCSSString; virtual;
    function IsWordBegin(const s: TCSSString; p: integer): boolean; virtual;
    function IsWordEnd(const s: TCSSString; p: integer): boolean; virtual;
    function PosWord(const aSearch, aText: TCSSString): integer; virtual;
    procedure MergeProperty(El: TCSSElement; Specifity: TCSSSpecifity); virtual;
    function ResolveIdentifier(El: TCSSIdentifierElement; Kind: TCSSNumericalIDKind): TCSSNumericalID; virtual;
    procedure AddElData(El: TCSSElement; ElData: TCSSElResolverData); virtual;
    function AddElValueData(El: TCSSElement; const aValue: TCSSString): TCSSValueData; virtual;
    function FindComputedAttribute(AttrID: TCSSNumericalID): PCSSComputedAttribute;
    function AddComputedAttribute(TheAttrID: TCSSNumericalID; aSpecifity: TCSSSpecifity;
                          aValue: TCSSElement): PCSSComputedAttribute;
    procedure DoError(const ID: TCSSMsgID; Msg: string; PosEl: TCSSElement); virtual;
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
  end;

implementation

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
  if C=TCSSCompoundElement then
  begin
    Compound:=TCSSCompoundElement(El);
    for i:=0 to Compound.ChildCount-1 do
      ComputeElement(Compound.Children[i]);
  end else if C=TCSSRuleElement then
    ComputeRule(TCSSRuleElement(El))
  else
    DoError(20220908150252,'Unknown CSS element',El);
end;

procedure TCSSResolver.ComputeRule(aRule: TCSSRuleElement);
var
  i, j: Integer;
  Specifity: TCSSSpecifity;
  aSelector: TCSSElement;
begin
  for i:=0 to aRule.SelectorCount-1 do
  begin
    aSelector:=aRule.Selectors[i];
    Specifity:=SelectorMatches(aSelector,FNode);
    if Specifity<0 then continue;
    // match -> apply properties
    for j:=0 to aRule.ChildCount-1 do
      MergeProperty(aRule.Children[j],Specifity);
  end;
end;

function TCSSResolver.SelectorMatches(aSelector: TCSSElement;
  const TestNode: TCSSNode): TCSSSpecifity;
var
  C: TClass;
begin
  Result:=CSSSpecifityInvalid;
  C:=aSelector.ClassType;
  if C=TCSSIdentifierElement then
    Result:=SelectorIdentifierMatches(TCSSIdentifierElement(aSelector),TestNode)
  else if C=TCSSClassNameElement then
    Result:=SelectorClassNameMatches(TCSSClassNameElement(aSelector),TestNode)
  else if C=TCSSPseudoClassElement then
    Result:=SelectorPseudoClassMatches(TCSSPseudoClassElement(aSelector),TestNode)
  else if C=TCSSStringElement then
    Result:=SelectorStringMatches(TCSSStringElement(aSelector),TestNode)
  else if C=TCSSBinaryElement then
    Result:=SelectorBinaryMatches(TCSSBinaryElement(aSelector),TestNode)
  else if C=TCSSArrayElement then
    Result:=SelectorArrayMatches(TCSSArrayElement(aSelector),TestNode)
  else if C=TCSSListElement then
    Result:=SelectorListMatches(TCSSListElement(aSelector),TestNode)
  else
    DoError(20220908230152,'Unknown CSS selector element',aSelector);
end;

function TCSSResolver.SelectorIdentifierMatches(
  Identifier: TCSSIdentifierElement; const TestNode: TCSSNode): TCSSSpecifity;
var
  TypeID: TCSSNumericalID;
begin
  Result:=CSSSpecifityNoMatch;
  TypeID:=ResolveIdentifier(Identifier,nikType);
  if TypeID=CSSTypeID_Universal then
  begin
    // universal selector
    Result:=CSSSpecifityUniversal;
  end else if TypeID=CSSIDNone then
  begin
    if croErrorOnUnknownName in Options then
      DoError(20220911230224,'Unknown CSS selector type name "'+Identifier.Name+'"',Identifier);
    Result:=CSSSpecifityInvalid;
  end else
  begin
    if TypeID=TestNode.GetCSSTypeID then
      Result:=CSSSpecifityType;
  end;
end;

function TCSSResolver.SelectorClassNameMatches(
  aClassName: TCSSClassNameElement; const TestNode: TCSSNode): TCSSSpecifity;
var
  aValue: TCSSString;
begin
  aValue:=copy(aClassName.Name,2,255);
  if TestNode.HasCSSClass(aValue) then
    Result:=CSSSpecifityClass
  else
    Result:=CSSSpecifityNoMatch;
end;

function TCSSResolver.SelectorPseudoClassMatches(
  aPseudoClass: TCSSPseudoClassElement; const TestNode: TCSSNode
  ): TCSSSpecifity;
var
  PseudoID: TCSSNumericalID;
begin
  Result:=CSSSpecifityNoMatch;
  PseudoID:=ResolveIdentifier(aPseudoClass,nikPseudoAttribute);
  case PseudoID of
  CSSIDNone:
    if croErrorOnUnknownName in Options then
      DoError(20220911205605,'Unknown CSS selector pseudo attribute name "'+aPseudoClass.Name+'"',aPseudoClass);
  CSSPseudoID_Root:
    if TestNode.GetCSSParent=nil then
      Result:=CSSSpecifityClass;
  CSSPseudoID_Empty:
    if TestNode.GetCSSChildCount=0 then
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

function TCSSResolver.SelectorStringMatches(aString: TCSSStringElement;
  const TestNode: TCSSNode): TCSSSpecifity;
// id selector #name
var
  aValue: TCSSString;
begin
  Result:=CSSSpecifityNoMatch;
  if aString.Children.Count>0 then
    DoError(20220910113909,'Invalid CSS string selector',aString.Children[0]);
  aValue:=aString.Value;
  if aValue[1]<>'#' then
    DoError(20220910114014,'Invalid CSS selector',aString);
  System.Delete(aValue,1,1);
  if aValue='' then
    DoError(20220910114133,'Invalid CSS identifier selector',aString);
  if aValue=TestNode.GetCSSID then
    Result:=CSSSpecifityIdentifier;
end;

function TCSSResolver.SelectorListMatches(aList: TCSSListElement;
  const TestNode: TCSSNode): TCSSSpecifity;
var
  i: Integer;
begin
  Result:=CSSSpecifityInvalid;
  writeln('TCSSResolver.SelectorListMatches ChildCount=',aList.ChildCount);
  for i:=0 to aList.ChildCount-1 do
    writeln('TCSSResolver.SelectorListMatches ',i,' ',GetCSSObj(aList.Children[i]),' AsString=',aList.Children[i].AsString);
  DoError(20220910115531,'Invalid CSS list selector',aList);
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
      // child combinator
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
      // adjacent sibling combinator
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
      // general sibling combinator
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
  else
    if croErrorOnUnknownName in Options then
      DoError(20220910123724,'Invalid CSS binary selector '+BinaryOperators[aBinary.Operation],aBinary);
  end;
end;

function TCSSResolver.SelectorArrayMatches(anArray: TCSSArrayElement;
  const TestNode: TCSSNode): TCSSSpecifity;
var
  El: TCSSElement;
  C: TClass;
  AttrID: TCSSNumericalID;
  {$IFDEF VerboseCSSResolver}
  i: integer;
  {$ENDIF}
begin
  Result:=CSSSpecifityInvalid;
  if anArray.Prefix<>nil then
    DoError(20220910154004,'Invalid CSS array selector prefix',anArray.Prefix);
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayMatches Prefix=',GetCSSObj(anArray.Prefix),' ChildCount=',anArray.ChildCount);
  for i:=0 to anArray.ChildCount-1 do
    writeln('TCSSResolver.SelectorArrayMatches ',i,' ',GetCSSObj(anArray.Children[i]));
  {$ENDIF}
  if anArray.ChildCount<>1 then
    DoError(20220910154033,'Invalid CSS array selector',anArray);
  El:=anArray.Children[0];
  C:=El.ClassType;
  if C=TCSSIdentifierElement then
  begin
    // [name]  ->  has attribute name
    AttrID:=ResolveIdentifier(TCSSIdentifierElement(El),nikAttribute);
    case AttrID of
    CSSIDNone,
    CSSAttributeID_All: Result:=CSSSpecifityNoMatch;
    CSSAttributeID_ID:
      Result:=CSSSpecifityClass;
    else
      if TestNode.HasCSSAttribute(AttrID) then
        Result:=CSSSpecifityClass
      else
        Result:=CSSSpecifityNoMatch;
    end;
  end else if C=TCSSBinaryElement then
    Result:=SelectorArrayBinaryMatches(TCSSBinaryElement(El),TestNode)
  else
    DoError(20220910153725,'Invalid CSS array selector',El);
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
    DoError(20220910164353,'Invalid CSS array selector, expected attribute',Left);
  AttrID:=ResolveIdentifier(TCSSIdentifierElement(Left),nikAttribute);
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches AttrID=',AttrID,' Value=',TCSSIdentifierElement(Left).Value);
  {$ENDIF}
  case AttrID of
  CSSIDNone,
  CSSAttributeID_All: exit(CSSSpecifityNoMatch);
  CSSAttributeID_ID:
    LeftValue:=TestNode.GetCSSID;
  else
    LeftValue:=TestNode.GetCSSAttribute(AttrID);
  end;

  Right:=aBinary.Right;
  C:=Right.ClassType;
  if (C=TCSSStringElement) or (C=TCSSIntegerElement) or (C=TCSSFloatElement)
      or (C=TCSSIdentifierElement) then
    // ok
  else
    DoError(20220910164921,'Invalid CSS array selector, expected string',Right);
  RightValue:=ComputeValue(Right);

  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Left="',LeftValue,'" Right="',RightValue,'" Op=',aBinary.Operation);
  {$ENDIF}
  case aBinary.Operation of
  boEquals:
    if LeftValue=RightValue then
      Result:=CSSSpecifityClass;
  boSquaredEqual:
    // begins with
    if LeftStr(LeftValue,length(RightValue))=RightValue then
      Result:=CSSSpecifityClass;
  boDollarEqual:
    // ends with
    if RightStr(LeftValue,length(RightValue))=RightValue then
      Result:=CSSSpecifityClass;
  boPipeEqual:
    // equal to or starts with name-hyphen
    if (LeftValue=RightValue)
        or (LeftStr(LeftValue,length(RightValue)+1)=RightValue+'-') then
      Result:=CSSSpecifityClass;
  boStarEqual:
    // contains substring
    if Pos(RightValue,LeftValue)>0 then
      Result:=CSSSpecifityClass;
  boTileEqual:
    // contains word
    if PosWord(RightValue,LeftValue)>0 then
      Result:=CSSSpecifityClass;
  else
    if croErrorOnUnknownName in Options then
      DoError(20220910164356,'Invalid CSS array selector operator',aBinary);
    Result:=CSSSpecifityInvalid;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Result=',Result);
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
      writeln('TCSSResolver.ComputeValue String=[',Result,']');
    end
    else if C=TCSSIntegerElement then
    begin
      IntEl:=TCSSIntegerElement(El);
      Result:=IntEl.AsString;
    end else if C=TCSSFloatElement then
    begin
      FloatEl:=TCSSFloatElement(El);
      Result:=FloatEl.AsString;
    end;
    writeln('TCSSResolver.ComputeValue Value="',Result,'"');
    AddElValueData(El,Result);
  end else
    DoError(20220910235106,'TCSSResolver.ComputeValue not supported',El);
end;

const
  WordChar = ['a'..'z','A'..'Z','0'..'9',#192..#255];

function TCSSResolver.IsWordBegin(const s: TCSSString; p: integer): boolean;
begin
  Result:=false;
  if p<1 then exit;
  if p>length(s) then exit;
  // simple check. ToDo: check unicode
  if (p>1) and (s[p-1] in WordChar) then exit;
  if not (s[p] in WordChar) then exit;
  Result:=true;
end;

function TCSSResolver.IsWordEnd(const s: TCSSString; p: integer): boolean;
begin
  Result:=false;
  if p<=1 then exit;
  if p>length(s)+1 then exit;
  // simple check. ToDo: check unicode
  if (p>1) and not (s[p-1] in WordChar) then exit;
  if (s[p] in WordChar) then exit;
  Result:=true;
end;

function TCSSResolver.PosWord(const aSearch, aText: TCSSString): integer;
begin
  if aSearch='' then exit(0);
  if aText='' then exit(0);
  Result:=Pos(aSearch,aText);
  while Result>0 do
  begin
    if IsWordBegin(aText,Result) and IsWordEnd(aText,Result+length(aSearch)) then
      exit;
    Result:=PosEx(aSearch,aText,Result+1);
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
    if Decl.KeyCount<>1 then
      DoError(20220908232213,'Not yet implemented CSS declaration with KeyCount='+IntToStr(Decl.KeyCount),El);
    if Decl.ChildCount<>1 then
      DoError(20220908232324,'Not yet implemented CSS declaration with ChildCount='+IntToStr(Decl.ChildCount),El);

    aKey:=Decl.Keys[0];
    aValue:=Decl.Children[0];
    if Decl.IsImportant then
      Specifity:=CSSSpecifityImportant;

    C:=aKey.ClassType;
    if C=TCSSIdentifierElement then
    begin
      AttrID:=ResolveIdentifier(TCSSIdentifierElement(aKey),nikAttribute);
      if AttrID=CSSIDNone then
        DoError(20220909000932,'Unknown CSS property "'+TCSSIdentifierElement(aKey).Name+'"',aKey)
      else if AttrID=CSSAttributeID_All then
        // 'all'
        DoError(20220909001019,'Not yet implemented CSS property "'+TCSSIdentifierElement(aKey).Name+'"',aKey)
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
      DoError(20220908232359,'Unknown CSS key',aKey);
  end else
    DoError(20220908230855,'Unknown CSS property',El);
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
      DoError(20220908235300,'TCSSResolver.ResolveTypeIdentifier',El);
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
    if Result=0 then
      Result:=FNumericalIDs[Kind][aName];

    if Result=CSSIDNone then
    begin
      if croErrorOnUnknownName in FOptions then
        DoError(20220908235919,'TCSSResolver.ResolveTypeIdentifier unknown '+CSSNumericalIDKindNames[Kind]+' "'+El.Name+'"',El);
    end;
    IdentData:=TCSSIdentifierData.Create;
    IdentData.Kind:=Kind;
    IdentData.NumericalID:=Result;
    AddElData(El,IdentData);
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

procedure TCSSResolver.DoError(const ID: TCSSMsgID; Msg: string;
  PosEl: TCSSElement);
begin
  Msg:='['+IntToStr(ID)+'] '+Msg+' at '+GetElPos(PosEl);
  raise ECSSResolver.Create(Msg);
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
      DoError(20220908234726,'TCSSResolver.ClearStyleCustomData',Data.Element);
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
    ComputeElement(NodeStyle);
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

