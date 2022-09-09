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
- TCSSResolver.FindComputedAttribute  use binary search for >8 elements

}

unit fpCSSResolver;

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, Contnrs, fpCSSTree;

const
  CSSSpecifityType = 1;
  CSSSpecifityClass = 10; // includes attribute selectors [href]
  CSSSpecifityIdentifier = 100;
  CSSSpecifityInline = 1000;
  CSSSpecifityImportant = 10000;

  CSSIDNone = 0;
  CSSTypeIDUniversal = 1; // id of type '*'
  CSSAttributeIDAll = 1; // id of attribute key 'all'

type
  TCSSMsgID = int64;
  TCSSNumericalID = integer;
  TCSSSpecifity = integer;

  ECSSResolver = class(Exception)
  end;

  { TCSSNode }

  TCSSNode = interface
    function GetCSSClassName: String;
    function GetCSSTypeID: TCSSNumericalID;
    function HasCSSClass(const aClassName: string): boolean;
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
  CSSNumericalIDKindNames: array[TCSSNumericalIDKind] of string = (
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
    function GetIDs(const aName: string): TCSSNumericalID;
    procedure SetIDs(const aName: string; const AValue: TCSSNumericalID);
  public
    constructor Create(aKind: TCSSNumericalIDKind);
    destructor Destroy; override;
    procedure Clear;
    property IDs[const aName: string]: TCSSNumericalID read GetIDs write SetIDs; default;
    property Kind: TCSSNumericalIDKind read FKind;
  end;

  TCSSComputedAttribute = record
    AttrID: TCSSNumericalID;
    Specifity: TCSSSpecifity;
    Value: TCSSElement;
  end;
  TCSSComputedAttributeArray = array of TCSSComputedAttribute;
  PCSSComputedAttribute = ^TCSSComputedAttribute;

  TCSSIdentifierData = class
  public
    Identifier: TCSSIdentifierElement;
    NumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
    Next, Prev: TCSSIdentifierData;
  end;

  TCSSResolverOption = (
    roErrorOnUnknownName
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
    FFirstIdentifierData: TCSSIdentifierData;
    FLastIdentifierData: TCSSIdentifierData;
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
    procedure MergeProperty(El: TCSSElement; Specifity: TCSSSpecifity); virtual;
    function ResolveIdentifier(El: TCSSIdentifierElement; Kind: TCSSNumericalIDKind): TCSSNumericalID; virtual;
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

function TCSSNumericalIDs.GetIDs(const aName: string): TCSSNumericalID;
begin
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
  Result:=TCSSNumericalID(fList.Find(aName));
  {$WARN 4056 on}
end;

procedure TCSSNumericalIDs.SetIDs(const aName: string;
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
  Identifier: TCSSIdentifierElement;
  TypeID: TCSSNumericalID;
  aClassName: TCSSString;
begin
  Result:=-1;
  C:=aSelector.ClassType;
  if C=TCSSIdentifierElement then
  begin
    Identifier:=TCSSIdentifierElement(aSelector);
    TypeID:=ResolveIdentifier(Identifier,nikType);
    if TypeID=CSSTypeIDUniversal then
    begin
      // universal selector
      Result:=0;
    end else if TypeID<>CSSIDNone then
    begin
      if TypeID=TestNode.GetCSSTypeID then
        Result:=CSSSpecifityType;
    end else
      DoError(20220908230426,'Unknown CSS selector type name "'+Identifier.Name+'"',Identifier);
  end else if C=TCSSClassNameElement then
  begin
    Identifier:=TCSSIdentifierElement(aSelector);
    aClassName:=copy(Identifier.Name,2,255);
    if TestNode.HasCSSClass(aClassName) then
      Result:=CSSSpecifityClass;
  end else
    DoError(20220908230152,'Unknown CSS selector element',aSelector);
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
      else if AttrID=CSSAttributeIDAll then
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
    Result:=FNumericalIDs[Kind][El.Name];
    if Result=CSSIDNone then
    begin
      if roErrorOnUnknownName in FOptions then
        DoError(20220908235919,'TCSSResolver.ResolveTypeIdentifier unknown '+CSSNumericalIDKindNames[Kind]+' "'+El.Name+'"',El);
    end;
    IdentData:=TCSSIdentifierData.Create;
    El.CustomData:=IdentData;
    IdentData.Identifier:=El;
    IdentData.Kind:=Kind;
    IdentData.NumericalID:=Result;
    if FFirstIdentifierData=nil then
    begin
      FFirstIdentifierData:=IdentData;
    end else begin
      FLastIdentifierData.Next:=IdentData;
      IdentData:=FLastIdentifierData;
    end;
    FLastIdentifierData:=IdentData;
  end;
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
  Data: TCSSIdentifierData;
begin
  while FLastIdentifierData<>nil do
  begin
    Data:=FLastIdentifierData;
    FLastIdentifierData:=Data.Prev;
    if FLastIdentifierData<>nil then
      FLastIdentifierData.Next:=nil
    else
      FFirstIdentifierData:=nil;
    if Data.Identifier.CustomData<>Data then
      DoError(20220908234726,'TCSSResolver.ClearStyleCustomData',Data.Identifier);
    Data.Identifier.CustomData:=nil;
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

