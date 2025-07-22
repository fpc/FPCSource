{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt (michael@freepascal.org)

    EBNF AST elements

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ebnf.tree;

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Contnrs, System.TypInfo;
  {$ELSE}
  SysUtils, Classes, Contnrs, typinfo;
  {$ENDIF}

type
  TEBNFElementType = (
    etGrammar,
    etRule,
    etExpression,
    etTerm,
    etFactorIdentifier,
    etFactorStringLiteral,
    etFactorOptional,
    etFactorRepetition,
    etFactorGroup,
    etFactorSpecialSequence
  );

  { TEBNFElement }

  TEBNFElement = class
  private
    FNodeType: TEBNFElementType;
  protected
    function GetChildCount : Integer; virtual;
    function GetChild(aIndex : Integer): TEBNFElement; virtual;
  public
    constructor Create(aNodeType: TEBNFElementType);
    property Child[aIdex : Integer] : TEBNFElement read GetChild;
    property ChildCount : Integer Read GetChildCount;
    property NodeType: TEBNFElementType read FNodeType;
    function ToString: string; override;
  end;

  { TEBNFElementWithChildren }

  TEBNFElementWithChildren = class (TEBNFElement)
  Private
    FChildren: TFPObjectList;
  protected
    function GetChildCount : Integer; override;
    function GetChild(aIndex : Integer): TEBNFElement; override;
  public
    constructor Create(aNodeType: TEBNFElementType);
    destructor Destroy; override;
  end;

  { TEBNFFactor }

  TEBNFFactor = class(TEBNFElement)
  private
    FValue: string;
    FInnerNode: TEBNFElement;
  public
    constructor Create(aNodeType: TEBNFElementType); overload;
    constructor Create(aNodeType: TEBNFElementType; aValue: string); overload;
    constructor Create(aNodeType: TEBNFElementType; aInnerNode: TEBNFElement); overload;
    destructor Destroy; override;
    property Value: string read FValue;
    property InnerNode: TEBNFElement read FInnerNode;
    function ToString: string; override;
  end;

  { TEBNFTerm }

  TEBNFTerm = class(TEBNFElementWithChildren)
  private
    FNewLine: boolean;
    function GetFactor(aIndex : integer) : TEBNFFactor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFactor(aFactor: TEBNFFactor);
    property Factors [aIndex : Integer] : TEBNFFactor read GetFactor;
    function ToString: string; override;
    property newline : boolean Read FNewLine Write FNewLine;
  end;

  { TEBNFExpression }

  TEBNFExpression = class(TEBNFElementWithChildren)
  private
    function GetTerm(aIndex : integer): TEBNFTerm;
  public
    constructor Create;
    procedure AddTerm(aTerm: TEBNFTerm);
    property Terms[aIndex : integer] : TEBNFTerm read GetTerm; default;
    function ToString: string; override;
  end;

  { TEBNFRule }

  TEBNFRule = class(TEBNFElement)
  private
    FIdentifier: string;
    FExpression: TEBNFElement; // Will be TEBNFExpression
  public
    constructor Create(aIdentifier: string; aExpression: TEBNFElement);
    destructor Destroy; override;
    property Identifier: string read FIdentifier;
    property Expression: TEBNFElement read FExpression;
    function ToString: string; override;
  end;

  { TEBNFGrammar }

  TEBNFGrammar = class(TEBNFElementWithChildren)
  private
    FRules: TFPObjectHashTable;
    function GetRule(const aName : string) : TEBNFRule; 
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRule(aRule: TEBNFRule);
    property Rules[aName : string]: TEBNFRule read GetRule;
    function ToString: string; override;
  end;


implementation


{  TEBNFElement }

function TEBNFElement.GetChildCount: Integer;
begin
  Result:=0;
end;

function TEBNFElement.GetChild(aIndex: Integer): TEBNFElement;
begin
  if aIndex<0 then ;
  Result:=Nil;
end;

constructor TEBNFElement.Create(aNodeType: TEBNFElementType);
begin
  FNodeType := aNodeType;
end;

function TEBNFElement.ToString: string;
begin
  Result := Format('Node Type: %s', [GetEnumName(TypeInfo(TEBNFElementType), Ord(FNodeType))]);
end;

{ TEBNFElementWithChildren }

function TEBNFElementWithChildren.GetChildCount: Integer;
begin
  Result:=FChildren.Count;
end;

function TEBNFElementWithChildren.GetChild(aIndex: Integer): TEBNFElement;
begin
  Result:=TEBNFElement(FChildren[aIndex]);
end;

constructor TEBNFElementWithChildren.Create(aNodeType: TEBNFElementType);
begin
  Inherited create(aNodeType);
  FChildren:=TFPObjectList.Create(True);
end;

destructor TEBNFElementWithChildren.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

// --- TEBNFRule Implementation ---

constructor TEBNFRule.Create(aIdentifier: string; aExpression: TEBNFElement);
begin
  inherited Create(etRule);
  FIdentifier := aIdentifier;
  FExpression := aExpression;
end;

destructor TEBNFRule.Destroy;
begin
  FExpression.Free;
  inherited;
end;

function TEBNFRule.ToString: string;
begin
  Result := Format('%s = %s;', [FIdentifier, FExpression.ToString]);
end;

{ TEBNFExpression }

function TEBNFExpression.GetTerm(aIndex : integer): TEBNFTerm;
begin
  Result:=Child[aIndex] as TEBNFTerm;
end;

constructor TEBNFExpression.Create;
begin
  inherited Create(etExpression);
end;

procedure TEBNFExpression.AddTerm(aTerm: TEBNFTerm);
begin
  FChildren.Add(aTerm);
end;

function TEBNFExpression.ToString: string;
var
  lRes : String;
  lTerm : TEBNFElement ;
  I : Integer;
begin
  lRes:='';
  for I:=0 to ChildCount-1 do
    begin
    LTerm:=Terms[i];
    if lRes<>'' then
      begin
      if (LTerm is TEBNFTerm) and TEBNFTerm(lTerm).NewLine then
        lRes:=lRes+sLineBreak;
      lRes:=lRes+' | ';
      end;
    lRes:=lRes+lTerm.ToString;
    end;
   Result:=lRes;
end;

{ TEBNFTerm }

function TEBNFTerm.GetFactor(aIndex: integer): TEBNFFactor;
begin
  Result:=Child[aIndex] as TEBNFFactor
end;

constructor TEBNFTerm.Create;
begin
  inherited Create(etTerm);
end;

destructor TEBNFTerm.Destroy;

begin
  inherited;
end;

procedure TEBNFTerm.AddFactor(aFactor: TEBNFFactor);
begin
  FChildren.Add(aFactor);
end;

function TEBNFTerm.ToString: string;
var
  lRes : String;
  I : Integer;
begin
  lRes:='';
  for I:=0 to ChildCount-1 do
    begin
    if lRes<>'' then
      lRes:=lRes+' ';
    lRes:=lRes+Child[i].ToString;
    end;
  Result:=lRes;
end;

{ TEBNFFactor }

constructor TEBNFFactor.Create(aNodeType: TEBNFElementType);
begin
  inherited Create(aNodeType);
  FValue := '';
  FInnerNode := nil;
end;

constructor TEBNFFactor.Create(aNodeType: TEBNFElementType; aValue: string);
begin
  inherited Create(aNodeType);
  FValue := aValue;
  FInnerNode := nil;
end;

constructor TEBNFFactor.Create(aNodeType: TEBNFElementType; aInnerNode: TEBNFElement);
begin
  inherited Create(aNodeType);
  FValue := '';
  FInnerNode := aInnerNode;
end;

destructor TEBNFFactor.Destroy;
begin
  FInnerNode.Free;
  inherited;
end;

function TEBNFFactor.ToString: string;
begin
  case FNodeType of
    etFactorIdentifier: Result := FValue;
    etFactorStringLiteral: Result := AnsiQuotedStr(FValue, '"');
    etFactorOptional: Result := Format('[%s]', [FInnerNode.ToString]);
    etFactorRepetition: Result := Format('{%s}', [FInnerNode.ToString]);
    etFactorGroup: Result := Format('(%s)', [FInnerNode.ToString]);
    etFactorSpecialSequence: Result := Format('?%s?', [FValue]);
  else
    Result := inherited ToString;
  end;
end;

{ TEBNFGrammar }

function TEBNFGrammar.GetRule(const aName: string): TEBNFRule;
begin
  Result:=TEBNFRule(FRules.Items[aName]);
end;

constructor TEBNFGrammar.Create;
begin
  inherited Create(etGrammar);
  FRules := TFPObjectHashTable.Create(False);
end;

destructor TEBNFGrammar.Destroy;

begin
  FreeAndNil(FRules);
  inherited;
end;

procedure TEBNFGrammar.AddRule(aRule: TEBNFRule);
begin
  if FRules.Find(aRule.Identifier)<>Nil then
    raise Exception.CreateFmt('Duplicate rule identifier: %s', [aRule.Identifier]);
  FChildren.Add(aRule);
  FRules.Add(aRule.Identifier, aRule);
end;

function TEBNFGrammar.ToString: string;
var
  I : integer;
  lList : TStrings;
begin
  Result := '';
  lList:=TStringList.Create;
  try
    for I:=0 to ChildCount-1 do
      begin
      lList.Add(Child[i].ToString);
      lList.Add('');
      end;
    Result:=lList.Text;
  finally
    lList.Free;
  end;
end;

end.
