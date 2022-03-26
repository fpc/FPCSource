{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022- by Michael Van Canneyt (michael@freepascal.org)

    This file contains the implementation of objects representing a CSS AST

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpCSSTree;

{$mode ObjFPC}{$H+}

interface

uses contnrs;


Type
  TCSSUnits = (cuNONE, cuPX,cuPERCENT,cuREM,cuEM,cuPT,cuFR,cuVW,cuVH,cuDEG);
  TCSSType = (csstUNKNOWN, csstINTEGER, csstSTRING, csstFLOAT,
              csstIDENTIFIER, csstCLASSNAME, csstPSEUDOCLASS, csstCOMPOUND, csstRULE,
              csstDECLARATION, csstBINARYOP, csstCALL, csstUNARYOP, csstARRAY, csstURL,
              csstUNICODERANGE,csstLIST);

  TCSSElement = class;

  TCSSTreeVisitor = class
  public
    procedure Visit(obj: TCSSElement); virtual; abstract;
  end;

  { TCSSElement }

  TCSSElement = Class(TObject)
  private
    FCol: Integer;
    FData: TObject;
    FFileName: UTF8String;
    FRow: Integer;
    function GetAsUnFormattedString: UTF8String;
    function GetAsFormattedString: UTF8String;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String; virtual;
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); virtual;
  Public
    Constructor Create(const aFileName : UTF8String; aRow,aCol : Integer); virtual;
    Class function CSSType : TCSSType; virtual;
    Procedure Iterate(aVisitor : TCSSTreeVisitor);
    Property CustomData : TObject Read FData Write FData;
    Property SourceRow : Integer Read FRow;
    Property SourceCol : Integer Read FCol;
    Property SourceFileName : UTF8String Read FFileName;
    Property AsFormattedString : UTF8String Read GetAsFormattedString;
    Property AsString : UTF8String Read GetAsUnformattedString;
  end;
  TCSSElementClass = Class of TCSSElement;
  TCSSElementArray = Array of TCSSElement;

  { TCSSElementList }

  TCSSElementList = Class(TFPObjectList)
  private
    function GetElement(aIndex : Integer): TCSSElement;
  Public
    Procedure Iterate(aVisitor : TCSSTreeVisitor);
    Property Elements[aIndex : Integer] : TCSSElement Read GetElement; default;
  end;

  { TCSSIntegerElement }

  TCSSIntegerElement = class(TCSSElement)
  private
    FisEscaped: Boolean;
    FUnits: TCSSUnits;
    FValue: Integer;
  protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String; override;
  Public
    Class function CSSType : TCSSType; override;
    Property Value : Integer Read FValue Write FValue;
    Property IsEscaped : Boolean Read FisEscaped Write FIsEscaped;
    Property Units : TCSSUnits Read FUnits Write FUnits;
  end;

  { TCSSIntegerElement }

  { TCSSFloatElement }

  TCSSFloatElement = class(TCSSElement)
  private
    FUnits: TCSSUnits;
    FValue: Double;
  protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;override;
  Public
    Class function CSSType : TCSSType; override;
    Property Value : Double Read FValue Write FValue;
    Property Units : TCSSUnits Read FUnits Write FUnits;
  end;


  { TCSSBaseUnaryElement }

  TCSSBaseUnaryElement = Class(TCSSElement)
  private
    FRight: TCSSElement;
    procedure SetRight(AValue: TCSSElement);
  protected
    Procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Destructor Destroy; override;
    Property Right : TCSSElement Read FRight Write SetRight;
  end;

  { TCSSUnaryElement }
  TCSSUnaryOperation = (uoDoubleColon,uoMinus,uoPlus,uoDiv);
  TCSSUnaryElement = Class(TCSSBaseUnaryElement)
  private
    FOperation: TCSSUnaryOperation;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String; override;
  Public
    Class function CSSType : TCSSType; override;
    Property Operation : TCSSUnaryOperation Read FOperation Write FOperation;
  end;

  { TCSSBinaryElement }
  TCSSBinaryOperation = (boEquals,boPlus,boMinus,boAnd,boLT,boGT,boDIV,
                         boStar,boTilde,boColon, boDoubleColon,boSquared);
  TCSSBinaryElement = Class(TCSSBaseUnaryElement)
  private
    FLeft: TCSSElement;
    FOperation: TCSSBinaryOperation;
    procedure SetLeft(AValue: TCSSElement);
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;override;
    procedure IterateChildren(aVisitor: TCSSTreeVisitor); override;
  Public
    Destructor Destroy; override;
    Class function CSSType : TCSSType; override;
    Property Left : TCSSElement Read FLeft Write SetLeft;
    Property Operation : TCSSBinaryOperation Read FOperation Write FOperation;
  end;


  { TCSSStringElement }

  { TCSSBaseStringElement }

  TCSSBaseStringElement = Class(TCSSElement)
  private
    FValue: UTF8String;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String; override;
  Public
    Property Value : UTF8String Read FValue Write FValue;
  end;

  { TCSSUnicodeRangeElement }

  TCSSUnicodeRangeElement = class(TCSSBaseStringElement)
  Public
    Class function CSSType : TCSSType; override;
  end;

  { TCSSURLElement }

  TCSSURLElement = Class(TCSSBaseStringElement)
    Class function CSSType : TCSSType; override;
  end;

  TCSSStringElement = Class(TCSSElement)
  private
    FChildren : TCSSElementList;
    FValue: UTF8String;
    function GetChildren: TCSSElementList;
  protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String; override;
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Class function CSSType : TCSSType; override;
    Destructor Destroy; override;
    Property Children : TCSSElementList Read GetChildren;
    Property Value : UTF8String Read FValue Write FValue;
  end;

  { TCSSIdentifierElement }

  TCSSIdentifierElement = Class(TCSSBaseStringElement)
  private
    function GetName: UTF8String;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String; override;
  Public
    Class function CSSType : TCSSType; override;
    Property Name : UTF8String Read GetName;
  end;

  { TCSSClassNameElement }

  TCSSClassNameElement = Class(TCSSIdentifierElement)
  Public
    Class function CSSType : TCSSType; override;
  end;

  { TCSSPseudoClassElement }

  TCSSPseudoClassElement = Class(TCSSElement)
  private
    FElement: TCSSElement;
    procedure SetElement(AValue: TCSSElement);
  Public
    Class function CSSType : TCSSType; override;
    Destructor Destroy; override;
    Property Element : TCSSElement Read FElement Write SetElement;
  end;


  { TCSSChildrenElement }

  TCSSChildrenElement = Class(TCSSElement)
  private
    FChildren : TCSSElementList;
    function GetChild(aIndex : Integer): TCSSElement;
    function GetChildCount: Integer;
  Protected
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Destructor Destroy; override;
    Procedure AddChild(aChild : TCSSElement); virtual;
    Property Children[aIndex : Integer] : TCSSElement Read GetChild;
    Property ChildCount : Integer Read GetChildCount;
  end;

  { TCSSArrayElement }

  TCSSArrayElement = Class(TCSSChildrenElement)
  private
    FPrefix : TCSSElement;
    procedure SetPrefix(AValue: TCSSElement);
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String; override;
  Public
    Destructor Destroy; override;
    Class function CSSType : TCSSType; override;
    Property Prefix : TCSSElement Read FPrefix Write SetPrefix;
  end;

  { TCSSCallElement }

  TCSSCallElement = Class(TCSSChildrenElement)
  private
    FName: UTF8String;
    function GetArg(aIndex : Integer): TCSSElement;
    function GetArgCount: Integer;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String; override;
  Public
    Class function CSSType : TCSSType; override;
    Procedure AddArg(aArg : TCSSElement); virtual;
    Property Args[aIndex : Integer] : TCSSElement Read GetArg; default;
    Property ArgCount : Integer Read GetArgCount;
    Property Name : UTF8String Read FName Write FName;
  end;

  { TCSSDeclarationElement }

  TCSSDeclarationElement = class(TCSSChildrenElement)
  private
    FIsImportant: Boolean;
    FKeys : TCSSElementList;
    FColon: Boolean;
    function GetKeyCount: Integer;
    function GetKeys(aIndex : Integer): TCSSElement;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;override;
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Class function CSSType : TCSSType; override;
    Destructor Destroy; override;
    Procedure AddKey(aKey : TCSSElement); virtual;
    Property Keys [aIndex : Integer] : TCSSElement Read GetKeys;
    Property KeyCount : Integer Read GetKeyCount;
    Property IsImportant : Boolean Read FIsImportant Write FIsImportant;
    Property Colon : Boolean Read FColon Write FColon;
  end;

  { TCSSListElement }

  TCSSListElement = class(TCSSChildrenElement)
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;override;
  Public
    Function ExtractElement(aIndex : Integer) : TCSSElement;
  end;

  { TCSSCompoundElement }

  TCSSCompoundElement = Class(TCSSChildrenElement)
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;override;
  Public
    Class function CSSType : TCSSType; override;
  end;

  { TCSSRuleElement }

  TCSSRuleElement = class(TCSSChildrenElement)
  Private
    FSelectors : TCSSElementList;
    function GetSelector(aIndex : Integer): TCSSElement;
    function GetSelectorCount: Integer;
  Protected
    function DoGetAsString(const aPrefix : String; aFormat : Boolean; const aIndent : String): UTF8String; virtual;
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;override;
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Class function CSSType : TCSSType; override;
    Destructor Destroy; override;
    Procedure AddSelector(aSelector : TCSSElement);
    Property Selectors [aIndex : Integer] : TCSSElement Read GetSelector;
    Property SelectorCount : Integer Read GetSelectorCount;
  end;

  { TCSSAtRuleElement }

  TCSSAtRuleElement = class(TCSSRuleElement)
  private
    FAtKeyWord: String;
  Public
    function GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;override;
    Property AtKeyWord : String Read FAtKeyWord Write FAtKeyWord;
  end;



Function StringToCSSString(S : UTF8String) : UTF8String;

Const
  CSSUnitNames : Array[TCSSUnits] of string =
        ('','px','%','rem','em','pt','fr','vw','vh','deg');
  UnaryOperators : Array[TCSSUnaryOperation] of string =
        ('::','-','+','/');
  BinaryOperators : Array[TCSSBinaryOperation] of string =
        ('=','+','-','and','<','>','/','*','~',':','::','^');


implementation

uses SysUtils, Classes, rtlConsts;

Const
  sIndent = '  ';

Function  u8length(s : char) : Byte;

const u8_length : Array[0..15] of byte = (
// 0 1 2 3 4 5 6 7 8 9 A B C D E F
   1,1,1,1,1,1,1,1,0,0,0,0,2,2,3,4
) ;

begin
 Result:=u8_length[Ord(S) shr 4];
end;

function StringToCSSString(S: UTF8String): UTF8String;

Var
  iIn,iOut,I : Integer;
  O : String[5];
  u : UTF8String;
  W : widestring;
  C : Char;

  Procedure AddO;
  var
    J : Integer;

  begin
    For J:=1 to Length(O) do
      begin
      Inc(iOut);
      Result[iOut]:=O[J];
      end;
  end;

begin
  Result:='';
  SetLength(Result,4*Length(S));
  iIn:=1;
  iOut:=0;
  While iIn<=Length(S) do
    begin
    C:=S[iIn];
    If C in [#0..' ','"'] then
      begin
      O:='\'+HexStr(Ord(C),2);
      AddO;
      end
    else if Ord(C)<128 then
      begin
      inc(iOut);
      Result[iOut]:=C;
      end
    else
      begin
      I:=U8length(C);
      if (I>0) then
        begin
        U:=Copy(S,iIn,I);
        W:=Utf8Decode(U);
        for I:=1 to Length(W) do
          begin
          O:='\'+HexStr(Ord(W[I]),4);
          AddO;
          end;
        end;
      end;
    Inc(iIn);
    end;
  SetLength(Result,iOut);
  Result:='"'+Result+'"';
end;

{ TCSSListElement }

function TCSSListElement.GetAsString(aFormat: Boolean; const aIndent: String): UTF8String;

Var
  I : integer;

begin
  Result:='';
  For I:=0 to ChildCount-1 do
    begin
    if I>0 then
      Result:=Result+' ';
    Result:=Result+Children[I].GetAsString(aFormat,aIndent);
    end;

end;

function TCSSListElement.ExtractElement(aIndex: Integer): TCSSElement;
begin
  Result:=Children[aIndex];
  FChildren.OwnsObjects:=False;
  try
    FChildren.Delete(aIndex);
  finally
    FChildren.OwnsObjects:=True;
  end;
end;

{ TCSSAtRuleElement }

function TCSSAtRuleElement.GetAsString(aFormat: Boolean; const aIndent: String
  ): UTF8String;
begin
  Result:=DoGetAsString(AtKeyWord+' ',aFormat, aIndent);
end;

{ TCSSBaseStringElement }

function TCSSBaseStringElement.GetAsString(aFormat: Boolean;
  const aIndent: String): UTF8String;
begin
  Result:=Value;
  if aFormat then
    Result:=aIndent+Result;
end;

{ TUnicodeRangeElement }

class function TCSSUnicodeRangeElement.CSSType: TCSSType;
begin
  Result:=csstUNICODERANGE;
end;

{ TCSSURLElement }

class function TCSSURLElement.CSSType: TCSSType;
begin
  Result:=csstURL;
end;


{ TCSSCompoundElement }

function TCSSCompoundElement.GetAsString(aFormat: Boolean; const aIndent: String
  ): UTF8String;

Var
  I : Integer;

begin
  Result:='';
  For I:=0 to ChildCount-1 do
    begin
    if (i>0) and aFormat then
      Result:=Result+sLineBreak+aIndent;
    Result:=Result+Children[I].GetAsString(aFormat,aIndent);
    end;
  if aFormat then
    Result:=aIndent+Result;
end;

class function TCSSCompoundElement.CSSType: TCSSType;
begin
  Result:=csstCompound;
end;

{ TCSSDeclarationElement }

function TCSSDeclarationElement.GetKeyCount: Integer;
begin
  If Assigned(FKeys) then
    Result:=FKeys.Count
  else
    Result:=0;
end;

function TCSSDeclarationElement.GetKeys(aIndex : Integer): TCSSElement;
begin
  if Not Assigned(FKeys) then
    Raise EListError.CreateFmt(SListIndexError,[aIndex]);
  Result:=FKeys[aIndex];
end;

function TCSSDeclarationElement.GetAsString(aFormat: Boolean; const aIndent: String): UTF8String;

var
  I : Integer;

begin
  Result:='';
  For I:=0 to KeyCount-1 do
    begin
    if (I>0) then
      begin
      Result:=Result+','+sLineBreak;
      if aFormat then
        Result:=Result+aIndent;
      end;
    Result:=Result+Keys[I].GetAsString(aFormat,aIndent);
    end;
  Result:=Result+' : ';
  For I:=0 to ChildCount-1 do
    begin
    if (I>0) then
      Result:=Result+', ';
    Result:=Result+Children[I].GetAsString(aFormat,aIndent);
    end;
  Result:=aIndent+Result;
end;

procedure TCSSDeclarationElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  if Assigned(FKeys) then
    FKeys.Iterate(aVisitor);
  inherited IterateChildren(aVisitor);
end;

class function TCSSDeclarationElement.CSSType: TCSSType;
begin
  Result:=csstDECLARATION;
end;

destructor TCSSDeclarationElement.Destroy;
begin
  FreeAndNil(FKeys);
  inherited Destroy;
end;

procedure TCSSDeclarationElement.AddKey(aKey: TCSSElement);
begin
  if Not Assigned(FKeys) then
    FKeys:=TCSSElementList.Create(True);
  FKeys.Add(aKey);
end;

{ TCSSUnaryElement }

class function TCSSUnaryElement.CSSType: TCSSType;
begin
  Result:=csstUNARYOP;
end;

function TCSSUnaryElement.GetAsString(aFormat: Boolean; const aIndent: String): UTF8String;

begin
  Result:=UnaryOperators[Self.Operation];
  if Not (Operation in [uoDoubleColon]) then
    Result:=Result+' ';
  if Assigned(Right) then
    Result:=Result+Right.GetAsString(aFormat,aIndent);
  if aFormat then
    Result:=aIndent+Result;
end;

{ TCSSRuleElement }

function TCSSRuleElement.GetSelector(aIndex : Integer): TCSSElement;

begin
  if not assigned(FSelectors) then
     Raise EListError.CreateFmt(SListIndexError,[aIndex]);
  Result:=FSelectors.Elements[aIndex];
end;


function TCSSRuleElement.GetSelectorCount: Integer;

begin
  if Assigned(FSelectors) then
    Result:=FSelectors.Count
  else
    Result:=0;
end;


function TCSSRuleElement.GetAsString(aFormat: Boolean; const aIndent: String
  ): UTF8String;

begin
  Result:=DoGetAsString('',aFormat,aIndent);
end;

function TCSSRuleElement.DoGetAsString(const aPrefix : String; aFormat: Boolean; const aIndent: String
  ): UTF8String;

var
  I : Integer;
  lIndent : String;

begin
  Result:='';
  For I:=0 to SelectorCount-1 do
    begin
    if (I>0) then
      begin
      Result:=Result+',';
      if aFormat then
        Result:=Result+sLineBreak+aIndent
      else
        Result:=Result+' ';
      end;
    Result:=Result+Selectors[I].GetAsString(aFormat,aIndent);
    end;
  if (ChildCount=0) and (aPrefix<>'') then
    Result:=aIndent+aPrefix+Result+';'
  else
    begin
    if SelectorCount>0 then
      Result:=Result+' ';
    Result:=Result+'{';
    lIndent:=aIndent;
    if aFormat then
      begin
      lIndent:=lIndent+sIndent;
      Result:=Result+sLineBreak;
      end
    else
      Result:=Result+' ';
    For I:=0 to ChildCount-1 do
      begin
      if (I>0) then
        begin
        if aFormat then
          Result:=Result+sLineBreak
        else
          Result:=Result+' ';
        end;
      Result:=Result+Children[I].GetAsString(aFormat,lIndent)+';';
      end;
    if aFormat then
      Result:=Result+sLineBreak+aIndent
    else
      Result:=Result+' ';
    Result:=Result+'}';
    Result:=aPrefix+Result;
    if aFormat then
      Result:=aIndent+Result;
    end;
end;

procedure TCSSRuleElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  if Assigned(FSelectors) then
    FSelectors.Iterate(aVisitor);
  inherited IterateChildren(aVisitor);
end;

class function TCSSRuleElement.CSSType: TCSSType;
begin
  Result:=csstRULE
end;

destructor TCSSRuleElement.Destroy;
begin
  FreeAndNil(FSelectors);
  Inherited Destroy;
end;

procedure TCSSRuleElement.AddSelector(aSelector: TCSSElement);
begin
  if Not Assigned(aSelector) then
      exit;
  if not Assigned(FSelectors) then
    FSelectors:=TCSSElementList.Create(True);
  FSelectors.Add(aSelector);
end;

{ TCSSPseudoClassElement }

procedure TCSSPseudoClassElement.SetElement(AValue: TCSSElement);
begin
  if FElement=AValue then Exit;
  FreeAndNil(FElement);
  FElement:=AValue;
end;

class function TCSSPseudoClassElement.CSSType: TCSSType;
begin
  Result:=csstPSEUDOCLASS;
end;

destructor TCSSPseudoClassElement.Destroy;
begin
  Element:=Nil;
  inherited Destroy;
end;

{ TCSSChildrenElement }

function TCSSChildrenElement.GetChild(aIndex : Integer): TCSSElement;
begin
  if not Assigned(FChildren) then
    Raise EListError.CreateFmt(SListIndexError,[aIndex]);
  Result:=FChildren[AIndex];
end;

function TCSSChildrenElement.GetChildCount: Integer;
begin
  if not Assigned(FChildren) then
    Result:=0
  else
    Result:=FChildren.Count;
end;

procedure TCSSChildrenElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  inherited IterateChildren(aVisitor);
  If Assigned(FChildren) then
    FChildren.Iterate(aVisitor);
end;

destructor TCSSChildrenElement.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;


procedure TCSSChildrenElement.AddChild(aChild: TCSSElement);
begin
  if Not Assigned(aChild) then
    exit;
  if FChildren=Nil then
     FChildren:=TCSSElementList.Create;
  FChildren.Add(aChild);
end;


{ TCSSCallElement }

function TCSSCallElement.GetArg(aIndex : Integer): TCSSElement;
begin
  Result:=Children[AIndex];
end;

function TCSSCallElement.GetArgCount: Integer;
begin
  Result:=ChildCount;
end;

function TCSSCallElement.GetAsString(aFormat: Boolean; const aIndent: String): UTF8String;

Var
  I : Integer;

begin
  Result:=Name+'(';
  For I:=0 to ChildCount-1 do
    begin
    if I>0 then
      Result:=Result+', ';
    Result:=Result+Children[I].GetAsString(aFormat,aIndent);
    end;
  if aFormat then
    Result:=aIndent+Result;
  Result:=Result+')'
end;

class function TCSSCallElement.CSSType: TCSSType;
begin
  Result:=csstCall;
end;

procedure TCSSCallElement.AddArg(aArg: TCSSElement);
begin
  AddChild(aArg);
end;

{ TCSSFloatElement }

function TCSSFloatElement.GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;
begin
  Str(Value:5:2,Result);
  Result:=TrimLeft(Result); // Space for positive numbers
  if aFormat then
    Result:=aIndent+Result;
end;

class function TCSSFloatElement.CSSType: TCSSType;
begin
  Result:=csstFloat;
end;

{ TCSSStringElement }

function TCSSStringElement.GetChildren: TCSSElementList;
begin
  if FChildren=Nil then
     FChildren:=TCSSElementList.Create(True);
  Result:=FChildren;
end;

function TCSSStringElement.GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;
begin
  Result:=StringToCSSString(Value);
  if aFormat then
    Result:=aIndent+Result;
end;

procedure TCSSStringElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  inherited IterateChildren(aVisitor);
  if Assigned(FChildren) then
    FChildren.Iterate(aVisitor);
end;

class function TCSSStringElement.CSSType: TCSSType;
begin
  Result:=csstSTRING;
end;

destructor TCSSStringElement.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

{ TCSSClassNameElement }

class function TCSSClassNameElement.CSSType: TCSSType;
begin
  Result:=csstCLASSNAME;
end;

{ TCSSIdentifierElement }

function TCSSIdentifierElement.GetName: UTF8String;
begin
  Result:=Value;
end;

function TCSSIdentifierElement.GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;
begin
  Result:=Value;
end;

class function TCSSIdentifierElement.CSSType: TCSSType;
begin
  Result:=csstIDENTIFIER;
end;

{ TCSSArrayElement }


procedure TCSSArrayElement.SetPrefix(AValue: TCSSElement);
begin
  if FPrefix=AValue then Exit;
  FreeAndNil(FPrefix);
  FPrefix:=AValue;
end;

function TCSSArrayElement.GetAsString(aFormat: Boolean; const aIndent: String
  ): UTF8String;
var
  I : integer;
begin
  Result:='[';
  For I:=0 to ChildCount-1 do
    begin
    if I>0 then
      Result:=Result+' ';
    Result:=Result+Children[I].GetAsString(aFormat, aIndent);
    end;
  Result:=aIndent+Result+']';
end;

destructor TCSSArrayElement.Destroy;
begin
  Prefix:=Nil;
  inherited Destroy;
end;

class function TCSSArrayElement.CSSType: TCSSType;
begin
  Result:=csstArray;
end;


{ TCSSElementList }

function TCSSElementList.GetElement(aIndex : Integer): TCSSElement;
begin
  Result:=TCSSElement(Items[aIndex]);
end;

procedure TCSSElementList.Iterate(aVisitor: TCSSTreeVisitor);

Var
  I : Integer;

begin
  For I:=0 to Count-1 do
    Elements[i].Iterate(aVisitor);
end;

{ TCSSIntegerElement }

function TCSSIntegerElement.GetAsString(aFormat : Boolean; const aIndent : String): UTF8String;
begin
  Result:=IntToStr(Value);
  if aFormat then
    Result:=aIndent+Result;
end;

class function TCSSIntegerElement.CSSType: TCSSType;
begin
  Result:=csstINTEGER;
end;

{ TCSSBinaryElement }

procedure TCSSBinaryElement.SetLeft(AValue: TCSSElement);
begin
  if FLeft=AValue then Exit;
  FreeAndNil(FLeft);
  FLeft:=AValue;
end;

function TCSSBinaryElement.GetAsString(aFormat: Boolean; const aIndent: String
  ): UTF8String;
begin
  Result:='';
  if Assigned(Left) then
    Result:=Left.GetAsString(aFormat,aIndent);
  if Not (Operation in [boColon,boDoubleColon]) then
    Result:=Result+' '+BinaryOperators[Operation]+' '
  else
    Result:=Result+BinaryOperators[Operation];
  if Assigned(Right) then
    Result:=Result+Right.GetAsString(aFormat,aIndent);
end;

destructor TCSSBinaryElement.Destroy;
begin
  Left:=Nil;
  inherited Destroy;
end;

class function TCSSBinaryElement.CSSType: TCSSType;
begin
  Result:=csstBINARYOP;
end;

procedure TCSSBinaryElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  inherited IterateChildren(aVisitor);
  if Assigned(FLeft) then
    FLeft.Iterate(aVisitor);

end;

{ TCSSUnaryElement }

procedure TCSSBaseUnaryElement.SetRight(AValue: TCSSElement);
begin
  if FRight=AValue then Exit;
  FreeAndNil(FRight);
  FRight:=AValue;
end;


destructor TCSSBaseUnaryElement.Destroy;
begin
  Right:=Nil;
  inherited Destroy;
end;

procedure TCSSBaseUnaryElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  inherited IterateChildren(aVisitor);
  If Assigned(FRight) then
     FRight.Iterate(aVisitor);
end;

{ TCSSElement }

function TCSSElement.GetAsUnFormattedString: UTF8String;
begin
  Result:=GetAsString(False,'');
end;

function TCSSElement.GetAsFormattedString: UTF8String;
begin
  Result:=GetAsString(True,'');
end;

function TCSSElement.GetAsString(aFormat: Boolean; const aIndent : String): UTF8String;
begin
  Result:='';
end;

procedure TCSSElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  if Assigned(aVisitor) then ;
end;

constructor TCSSElement.Create(const aFileName: UTF8String; aRow, aCol: Integer);
begin
  FFileName:=aFileName;
  FRow:=aRow;
  FCol:=aCol;
end;

class function TCSSElement.CSSType: TCSSType;
begin
  Result:=csstUnknown;
end;

procedure TCSSElement.Iterate(aVisitor: TCSSTreeVisitor);
begin
  aVisitor.Visit(Self);
  IterateChildren(aVisitor);
end;

end.

