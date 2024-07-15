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

{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSTree;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.Contnrs, System.RtlConsts, System.SysUtils, System.Classes, System.Math;
{$ELSE FPC_DOTTEDUNITS}
uses Contnrs, RtlConsts, SysUtils, Classes, Math;
{$ENDIF FPC_DOTTEDUNITS}


Type
  ECSSException = class(Exception);

  TCSSString = UTF8String;
  TCSSStringDynArray = array of TCSSString;
  TCSSUnits = (cuNONE, cuPX,cuPERCENT,cuREM,cuEM,cuPT,cuFR,cuVW,cuVH,cuDEG);
  TCSSType = (
    csstUnknown,
    csstInteger, csstString, csstFloat,
    csstIdentifier, // name
    csstHashIdentifier, // #name
    csstClassname, // .name
    csstPseudoClass, // :name, ::name
    csstCompound,
    csstRule,
    csstDeclaration,
    csstBinaryOp,
    csstCall, // name(, :name(, ::name(
    csstUnaryOp,
    csstArray, // []
    csstURL, // url()
    csstUnicodeRange,
    csstList);

  TCSSElement = class;

  TCSSTreeVisitor = class
  public
    procedure Visit(obj: TCSSElement); virtual; abstract;
  end;

  { TCSSVisitorFreeCustomData }

  TCSSVisitorFreeCustomData = class(TCSSTreeVisitor)
  public
    procedure Visit(obj: TCSSElement); override;
  end;

  { TCSSElementOwnedData - base class for TCSSElement.CustomData which automatically freed }

  TCSSElementOwnedData = class
  end;

  { TCSSElement }

  TCSSElement = Class(TObject)
  private
    FCol: Integer;
    FData: TObject;
    FFileName: TCSSString;
    FParent: TCSSElement;
    FRow: Integer;
    function GetAsUnFormattedString: TCSSString;
    function GetAsFormattedString: TCSSString;
  Protected
    procedure SetParent(const AValue: TCSSElement);
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; virtual;
    function SubElEquals(ElA, ElB: TCSSElement): boolean;
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); virtual;
  Public
    Constructor Create(const aFileName : TCSSString; aRow,aCol : Integer); virtual;
    destructor Destroy; override;
    Class function CSSType : TCSSType; virtual;
    function Equals(Obj: TObject): boolean; override;
    Procedure Iterate(aVisitor : TCSSTreeVisitor);
    Procedure FreeCustomData; virtual; // free recursively CustomData
    Property CustomData : TObject Read FData Write FData;
    Property SourceRow : Integer Read FRow;
    Property SourceCol : Integer Read FCol;
    Property SourceFileName : TCSSString Read FFileName;
    Property AsFormattedString : TCSSString Read GetAsFormattedString;
    Property AsString : TCSSString Read GetAsUnformattedString;
    Property Parent: TCSSElement read FParent write SetParent;
  end;
  TCSSElementClass = Class of TCSSElement;
  TCSSElementArray = Array of TCSSElement;

  { TCSSElementList }

  TCSSElementList = Class
  private
    FElementParent: TCSSElement;
    FList: TFPObjectList;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetElement(aIndex : Integer): TCSSElement;
    procedure SetCapacity(const AValue: Integer);
  Public
    constructor Create(ElParent: TCSSElement);
    destructor Destroy; override;
    Function Add(El: TCSSElement): Integer;
    procedure Clear;
    Procedure Delete(Index: Integer);
    function Equals(Obj: TObject): boolean; override;
    Procedure Exchange(Index1, Index2: Integer);
    Function Extract(Index: Integer): TCSSElement; // remove without free
    Function IndexOf(El: TCSSElement): Integer;
    Procedure Insert(Index: Integer; El: TCSSElement);
    Function First: TCSSElement;
    Function Last: TCSSElement;
    Procedure Move(CurIndex, NewIndex: Integer);
    Procedure Assign(aList: TCSSElementList);
    Procedure Pack;
    Procedure Sort(const Compare: TListSortCompare);
    Procedure Iterate(aVisitor : TCSSTreeVisitor);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Elements[aIndex : Integer] : TCSSElement Read GetElement; default;
    property ElementParent: TCSSElement read FElementParent;
  end;

  { TCSSIntegerElement }

  TCSSIntegerElement = class(TCSSElement)
  private
    FIsEscaped: Boolean;
    FUnits: TCSSUnits;
    FValue: Integer;
  protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    Class function CSSType : TCSSType; override;
    function Equals(Obj: TObject): boolean; override;
    Property Value : Integer Read FValue Write FValue;
    Property IsEscaped : Boolean Read FIsEscaped Write FIsEscaped;
    Property Units : TCSSUnits Read FUnits Write FUnits;
  end;
  TCSSIntegerElementClass = class of TCSSIntegerElement;

  { TCSSFloatElement }

  TCSSFloatElement = class(TCSSElement)
  private
    FUnits: TCSSUnits;
    FValue: Double;
  protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString;override;
  Public
    Class function CSSType : TCSSType; override;
    function Equals(Obj: TObject): boolean; override;
    Property Value : Double Read FValue Write FValue;
    Property Units : TCSSUnits Read FUnits Write FUnits;
  end;
  TCSSFloatElementClass = class of TCSSFloatElement;

  { TCSSBaseUnaryElement }

  TCSSBaseUnaryElement = Class(TCSSElement)
  private
    FRight: TCSSElement;
    procedure SetRight(AValue: TCSSElement);
  protected
    Procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    Property Right : TCSSElement Read FRight Write SetRight;
  end;

  { TCSSUnaryElement }
  TCSSUnaryOperation = (uoDoubleColon,uoMinus,uoPlus,uoDiv,uoGT,uoTilde);
  TCSSUnaryElement = Class(TCSSBaseUnaryElement)
  private
    FOperation: TCSSUnaryOperation;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    Class function CSSType : TCSSType; override;
    function Equals(Obj: TObject): boolean; override;
    Property Operation : TCSSUnaryOperation Read FOperation Write FOperation;
  end;
  TCSSUnaryElementClass = class of TCSSUnaryElement;

  { TCSSBinaryElement }
  TCSSBinaryOperation = (boEquals,boPlus,boMinus,boAnd,boLE,boLT,boGE,boGT,boDIV,
                         boStar,boTilde,boColon, boDoubleColon,boSquared,
                         boPipe, boDollar, boWhiteSpace,
                         boStarEqual,boTildeEqual,boSquaredEqual,boPipeEqual,boDollarEqual);
  TCSSBinaryElement = Class(TCSSBaseUnaryElement)
  private
    FLeft: TCSSElement;
    FOperation: TCSSBinaryOperation;
    procedure SetLeft(AValue: TCSSElement);
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString;override;
    procedure IterateChildren(aVisitor: TCSSTreeVisitor); override;
  Public
    Destructor Destroy; override;
    Class function CSSType : TCSSType; override;
    function Equals(Obj: TObject): boolean; override;
    Property Left : TCSSElement Read FLeft Write SetLeft;
    Property Operation : TCSSBinaryOperation Read FOperation Write FOperation;
  end;
  TCSSBinaryElementClass = class of TCSSBinaryElement;

  { TCSSBaseStringElement }

  TCSSBaseStringElement = Class(TCSSElement)
  private
    FValue: TCSSString;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    function Equals(Obj: TObject): boolean; override;
    Property Value : TCSSString Read FValue Write FValue;
  end;

  { TCSSUnicodeRangeElement }

  TCSSUnicodeRangeElement = class(TCSSBaseStringElement)
  Public
    Class function CSSType : TCSSType; override;
  end;
  TCSSUnicodeRangeElementClass = class of TCSSUnicodeRangeElement;

  { TCSSURLElement }

  TCSSURLElement = Class(TCSSBaseStringElement)
  public
    Class function CSSType : TCSSType; override;
  end;
  TCSSURLElementClass = class of TCSSURLElement;

  { TCSSStringElement }

  TCSSStringElement = Class(TCSSBaseStringElement)
  private
    FChildren : TCSSElementList;
    function GetChildren: TCSSElementList;
  protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Class function CSSType : TCSSType; override;
    Destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    Property Children : TCSSElementList Read GetChildren;
  end;
  TCSSStringElementClass = class of TCSSStringElement;

  { TCSSIdentifierElement }

  TCSSIdentifierElement = Class(TCSSBaseStringElement)
  private
    function GetName: TCSSString;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    Class function CSSType : TCSSType; override;
    Property Name : TCSSString Read GetName;
  end;
  TCSSIdentifierElementClass = class of TCSSIdentifierElement;

  { TCSSHashIdentifierElement }

  TCSSHashIdentifierElement = Class(TCSSIdentifierElement)
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    Class function CSSType : TCSSType; override;
  end;
  TCSSHashIdentifierElementClass = class of TCSSHashIdentifierElement;

  { TCSSClassNameElement }

  TCSSClassNameElement = Class(TCSSIdentifierElement)
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    Class function CSSType : TCSSType; override;
  end;
  TCSSClassNameElementClass = class of TCSSClassNameElement;

  { TCSSPseudoClassElement }

  TCSSPseudoClassElement = Class(TCSSIdentifierElement)
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    Class function CSSType : TCSSType; override;
  end;
  TCSSPseudoClassElementClass = class of TCSSPseudoClassElement;

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
    function Equals(Obj: TObject): boolean; override;
    Property Children[aIndex : Integer] : TCSSElement Read GetChild; default;
    Property ChildCount : Integer Read GetChildCount;
  end;

  { TCSSArrayElement }

  TCSSArrayElement = Class(TCSSChildrenElement)
  private
    FPrefix : TCSSElement;
    procedure SetPrefix(AValue: TCSSElement);
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    Destructor Destroy; override;
    Class function CSSType : TCSSType; override;
    function Equals(Obj: TObject): boolean; override;
    Property Prefix : TCSSElement Read FPrefix Write SetPrefix;
  end;
  TCSSArrayElementClass = class of TCSSArrayElement;

  { TCSSCallElement }

  TCSSCallElement = Class(TCSSChildrenElement)
  private
    FName: TCSSString;
    function GetArg(aIndex : Integer): TCSSElement;
    function GetArgCount: Integer;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString; override;
  Public
    Class function CSSType : TCSSType; override;
    Procedure AddArg(aArg : TCSSElement); virtual;
    function Equals(Obj: TObject): boolean; override;
    Property Args[aIndex : Integer] : TCSSElement Read GetArg; default;
    Property ArgCount : Integer Read GetArgCount;
    Property Name : TCSSString Read FName Write FName;
  end;
  TCSSCallElementClass = class of TCSSCallElement;

  { TCSSDeclarationElement }

  TCSSDeclarationElement = class(TCSSChildrenElement)
  private
    FIsImportant: Boolean;
    FKeys : TCSSElementList;
    FColon: Boolean;
    function GetKeyCount: Integer;
    function GetKeys(aIndex : Integer): TCSSElement;
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString;override;
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Class function CSSType : TCSSType; override;
    Destructor Destroy; override;
    Procedure AddKey(aKey : TCSSElement); virtual;
    function Equals(Obj: TObject): boolean; override;
    Property Keys [aIndex : Integer] : TCSSElement Read GetKeys;
    Property KeyCount : Integer Read GetKeyCount;
    Property IsImportant : Boolean Read FIsImportant Write FIsImportant;
    Property Colon : Boolean Read FColon Write FColon;
  end;
  TCSSDeclarationElementClass = class of TCSSDeclarationElement;

  { TCSSListElement }

  TCSSListElement = class(TCSSChildrenElement)
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString;override;
  Public
    Function ExtractElement(aIndex : Integer) : TCSSElement;
  end;
  TCSSListElementClass = class of TCSSListElement;

  { TCSSCompoundElement }

  TCSSCompoundElement = Class(TCSSChildrenElement)
  Protected
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString;override;
  Public
    Class function CSSType : TCSSType; override;
  end;
  TCSSCompoundElementClass = class of TCSSCompoundElement;

  { TCSSRuleElement }

  TCSSRuleElement = class(TCSSChildrenElement)
  Private
    FSelectors : TCSSElementList;
    function GetSelector(aIndex : Integer): TCSSElement;
    function GetSelectorCount: Integer;
  Protected
    function DoGetAsString(const aPrefix : TCSSString; aFormat : Boolean; const aIndent : TCSSString): TCSSString; virtual;
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString;override;
    procedure IterateChildren(aVisitor : TCSSTreeVisitor); override;
  Public
    Class function CSSType : TCSSType; override;
    Destructor Destroy; override;
    Procedure AddSelector(aSelector : TCSSElement);
    function Equals(Obj: TObject): boolean; override;
    Property Selectors [aIndex : Integer] : TCSSElement Read GetSelector;
    Property SelectorCount : Integer Read GetSelectorCount;
  end;
  TCSSRuleElementClass = class of TCSSRuleElement;
  TCSSRuleElementArray = array of TCSSRuleElement;

  { TCSSAtRuleElement }

  TCSSAtRuleElement = class(TCSSRuleElement)
  private
    FAtKeyWord: TCSSString;
  Public
    function GetAsString(aFormat : Boolean; const aIndent : TCSSString): TCSSString;override;
    function Equals(Obj: TObject): boolean; override;
    Property AtKeyWord : TCSSString Read FAtKeyWord Write FAtKeyWord;
  end;
  TCSSAtRuleElementClass = class of TCSSAtRuleElement;


// Convert unicode codepoints to \0000 notation
Function StringToCSSString(const S : TCSSString) : TCSSString;
// Escapes non-identifier characters C to \C
Function StringToIdentifier(const S : TCSSString) : TCSSString;

Function GetCSSObj(El: TCSSElement): TCSSString;
Function GetCSSPath(El: TCSSElement): TCSSString;

Function CSSElementListEquals(ListA, ListB: TCSSElementList): boolean;

Const
  CSSUnitNames : Array[TCSSUnits] of TCSSString =
        ('','px','%','rem','em','pt','fr','vw','vh','deg');
  UnaryOperators : Array[TCSSUnaryOperation] of TCSSString =
        ('::','-','+','/','>','~');
  BinaryOperators : Array[TCSSBinaryOperation] of TCSSString =
        ('=','+','-','and','<=','<','>=','>','/','*','~',':','::','^','|','$',' ',
         '*=','~=','^=','|=','$=');

implementation

Const
  sIndent = '  ';

Function  u8length(s : AnsiChar) : Byte;

const u8_length : Array[0..15] of byte = (
// 0 1 2 3 4 5 6 7 8 9 A B C D E F
   1,1,1,1,1,1,1,1,0,0,0,0,2,2,3,4
) ;

begin
 Result:=u8_length[Ord(S) shr 4];
end;

function StringToCSSString(const S: TCSSString): TCSSString;

Var
  iIn,iOut,I,L : Integer;
  O : TCSSString;
  u : TCSSString;
  W : Unicodestring;
  C : AnsiChar;

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
  L:=Length(S);
  SetLength(Result,4*L);
  iIn:=1;
  iOut:=0;
  While iIn<=L do
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
        inc(iIn,I);
        continue;
        end;
      end;
    Inc(iIn);
    end;
  SetLength(Result,iOut);
end;

function StringToIdentifier(const S: TCSSString): TCSSString;

Var
  iIn,iOut,L : Integer;
  C : AnsiChar;

begin
  Result:='';
  SetLength(Result,2*Length(S));
  iIn:=1;
  iOut:=0;
  L:=Length(S);
  While iIn<=L do
    begin
    C:=S[iIn];
    If Not (C in ['a'..'z','A'..'Z','_','-','0'..'9']) then
      begin
      inc(iOut);
      Result[iOut]:='\';
      end;
    inc(iOut);
    Result[iOut]:=C;
    Inc(iIn);
    end;
  SetLength(Result,iOut);
end;

function GetCSSObj(El: TCSSElement): TCSSString;
begin
  if El=nil then
    Result:='nil'
  else if El is TCSSIdentifierElement then
    Result:=El.ClassName+'"'+TCSSIdentifierElement(El).Name+'"'
  else
    Result:=El.ClassName;
end;

function GetCSSPath(El: TCSSElement): TCSSString;
begin
  if El=nil then
    exit('nil');
  Result:='';
  while El<>nil do
    begin
    if Result<>'' then
      Result:='.'+Result;
    Result:=GetCSSObj(El)+Result;
    El:=El.Parent;
    end;
end;

function CSSElementListEquals(ListA, ListB: TCSSElementList): boolean;
begin
  if (ListA=nil) or (ListA.Count=0) then
    Result:=(ListB=nil) or (ListB.Count=0)
  else
    begin
    if (ListB=nil) or (ListB.Count=0) then exit(false);
    Result:=ListA.Equals(ListB);
    end;
end;

{ TCSSListElement }

function TCSSListElement.GetAsString(aFormat: Boolean; const aIndent: TCSSString
  ): TCSSString;

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
  Result:=FChildren.Extract(aIndex);
end;

{ TCSSAtRuleElement }

function TCSSAtRuleElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
begin
  Result:=DoGetAsString(AtKeyWord+' ',aFormat, aIndent);
end;

function TCSSAtRuleElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSAtRuleElement absolute Obj;
begin
  if Obj is TCSSAtRuleElement then
    begin
    if FAtKeyWord<>Src.FAtKeyWord then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

{ TCSSBaseStringElement }

function TCSSBaseStringElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
begin
  Result:=Value;
  if aFormat then
    Result:=aIndent+Result;
end;

function TCSSBaseStringElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSBaseStringElement absolute Obj;
begin
  if Obj is TCSSBaseStringElement then
    begin
    if FValue<>Src.FValue then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

{ TUnicodeRangeElement }

class function TCSSUnicodeRangeElement.CSSType: TCSSType;
begin
  Result:=csstUnicodeRange;
end;

{ TCSSURLElement }

class function TCSSURLElement.CSSType: TCSSType;
begin
  Result:=csstURL;
end;


{ TCSSCompoundElement }

function TCSSCompoundElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;

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

function TCSSDeclarationElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;

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
  Result:=csstDeclaration;
end;

destructor TCSSDeclarationElement.Destroy;
begin
  FreeAndNil(FKeys);
  inherited Destroy;
end;

procedure TCSSDeclarationElement.AddKey(aKey: TCSSElement);
begin
  if aKey=Nil then exit;
  if Not Assigned(FKeys) then
    FKeys:=TCSSElementList.Create(Self);
  FKeys.Add(aKey);
end;

function TCSSDeclarationElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSDeclarationElement absolute Obj;
begin
  if Obj is TCSSDeclarationElement then
    begin
    if (FIsImportant<>Src.FIsImportant)
        or (FColon<>Src.FColon)
        or (not CSSElementListEquals(FKeys,Src.FKeys)) then
      exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

{ TCSSUnaryElement }

class function TCSSUnaryElement.CSSType: TCSSType;
begin
  Result:=csstUnaryOp;
end;

function TCSSUnaryElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSUnaryElement absolute Obj;
begin
  if Obj is TCSSUnaryElement then
    begin
    if FOperation<>Src.FOperation then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

function TCSSUnaryElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;

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


function TCSSRuleElement.GetAsString(aFormat: Boolean; const aIndent: TCSSString
  ): TCSSString;

begin
  Result:=DoGetAsString('',aFormat,aIndent);
end;

function TCSSRuleElement.DoGetAsString(const aPrefix: TCSSString;
  aFormat: Boolean; const aIndent: TCSSString): TCSSString;

var
  I : Integer;
  lIndent : TCSSString;

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
  Result:=csstRule;
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
    FSelectors:=TCSSElementList.Create(Self);
  FSelectors.Add(aSelector);
end;

function TCSSRuleElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSRuleElement absolute Obj;
begin
  if Obj is TCSSRuleElement then
    begin
    if not CSSElementListEquals(FSelectors,Src.FSelectors) then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

{ TCSSPseudoClassElement }

function TCSSPseudoClassElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;

Var
  I : Integer;

begin
  if aFormat then ;
  if aIndent='' then ;
  I:=1;
  if (Length(Value)>2) and (Value[2]=':') then
    I:=2;
  Result:=Copy(Value,1,I)+StringToIdentifier(Copy(Value,I+1,Length(Value)-I));
end;

class function TCSSPseudoClassElement.CSSType: TCSSType;
begin
  Result:=csstPseudoClass;
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
    FChildren:=TCSSElementList.Create(Self);
  FChildren.Add(aChild);
end;

function TCSSChildrenElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSChildrenElement absolute Obj;
begin
  if Obj is TCSSChildrenElement then
    begin
    if not CSSElementListEquals(FChildren,Src.FChildren) then exit(false);
    end;
  Result:=inherited Equals(Obj);
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

function TCSSCallElement.GetAsString(aFormat: Boolean; const aIndent: TCSSString
  ): TCSSString;

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
  Result:=Result+')';
  if aFormat then
    Result:=aIndent+Result;
end;

class function TCSSCallElement.CSSType: TCSSType;
begin
  Result:=csstCall;
end;

procedure TCSSCallElement.AddArg(aArg: TCSSElement);
begin
  AddChild(aArg);
end;

function TCSSCallElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSCallElement absolute Obj;
begin
  if Obj is TCSSCallElement then
    begin
    if FName<>Src.FName then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

{ TCSSFloatElement }

function TCSSFloatElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
begin
  Str(Value:5:2,Result);
  Result:=TrimLeft(Result); // Space for positive numbers
  Result:=Result+CSSUnitNames[Units];
  if aFormat then
    Result:=aIndent+Result;
end;

class function TCSSFloatElement.CSSType: TCSSType;
begin
  Result:=csstFloat;
end;

function TCSSFloatElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSFloatElement absolute Obj;
begin
  if Obj is TCSSFloatElement then
    begin
    if (FUnits<>Src.FUnits)
        or (not SameValue(FValue,Src.FValue)) then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

{ TCSSStringElement }

function TCSSStringElement.GetChildren: TCSSElementList;
begin
  if FChildren=Nil then
    FChildren:=TCSSElementList.Create(Self);
  Result:=FChildren;
end;

function TCSSStringElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
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
  Result:=csstString;
end;

destructor TCSSStringElement.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

function TCSSStringElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSStringElement absolute Obj;
begin
  if Obj is TCSSStringElement then
    begin
    if not CSSElementListEquals(FChildren,Src.FChildren) then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

{ TCSSClassNameElement }

function TCSSClassNameElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
begin
  if aFormat then ;
  if aIndent='' then ;
  Result:='.'+StringToIdentifier(Value);
end;

class function TCSSClassNameElement.CSSType: TCSSType;
begin
  Result:=csstClassname;
end;

{ TCSSIdentifierElement }

function TCSSIdentifierElement.GetName: TCSSString;
begin
  Result:=Value;
end;

function TCSSIdentifierElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
begin
  if aFormat then ;
  if aIndent='' then ;
  Result:=StringToIdentifier(Value);
end;

class function TCSSIdentifierElement.CSSType: TCSSType;
begin
  Result:=csstIdentifier;
end;

{ TCSSHashIdentifierElement }

function TCSSHashIdentifierElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
begin
  if aFormat then ;
  if aIndent='' then ;
  Result:='#'+StringToIdentifier(Value);
end;

class function TCSSHashIdentifierElement.CSSType: TCSSType;
begin
  Result:=csstHashIdentifier;
end;

{ TCSSArrayElement }

procedure TCSSArrayElement.SetPrefix(AValue: TCSSElement);
begin
  if FPrefix=AValue then Exit;
  FreeAndNil(FPrefix);
  FPrefix:=AValue;
end;

function TCSSArrayElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
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

function TCSSArrayElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSArrayElement absolute Obj;
begin
  if Obj is TCSSArrayElement then
    begin
    if not SubElEquals(FPrefix,Src.FPrefix) then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;


{ TCSSElementList }

function TCSSElementList.GetElement(aIndex : Integer): TCSSElement;
begin
  Result:=TCSSElement(FList[aIndex]);
end;

function TCSSElementList.GetCapacity: Integer;
begin
  Result:=FList.Capacity;
end;

function TCSSElementList.GetCount: Integer;
begin
  Result:=FList.Count;
end;

procedure TCSSElementList.SetCapacity(const AValue: Integer);
begin
  FList.Capacity:=AValue;
end;

constructor TCSSElementList.Create(ElParent: TCSSElement);
begin
  FElementParent:=ElParent;
  FList:=TFPObjectList.Create(true);
end;

destructor TCSSElementList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TCSSElementList.Clear;
begin
  FList.Clear;
end;

function TCSSElementList.Add(El: TCSSElement): Integer;
begin
  Result:=FList.Add(El);
  El.Parent:=ElementParent;
end;

procedure TCSSElementList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

function TCSSElementList.Equals(Obj: TObject): boolean;
var
  Src: TCSSElementList absolute Obj;
  i: Integer;
begin
  if Obj is TCSSElementList then
    begin
    Result:=false;
    if Count<>Src.Count then
      exit;
    for i:=0 to Count-1 do
      if not Elements[i].Equals(Src.Elements[i]) then
        exit;
    Result:=true;
    end
  else
    Result:=inherited Equals(Obj);
end;

procedure TCSSElementList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TCSSElementList.Extract(Index: Integer): TCSSElement;
begin
  Result:=TCSSElement(FList[Index]);
  Result.Parent:=nil;
  FList.OwnsObjects:=false;
  try
    FList.Delete(Index);
  finally
    FList.OwnsObjects:=true;
  end;
end;

function TCSSElementList.IndexOf(El: TCSSElement): Integer;
begin
  Result:=FList.IndexOf(El);
end;

procedure TCSSElementList.Insert(Index: Integer; El: TCSSElement);
begin
  FList.Insert(Index,El);
end;

function TCSSElementList.First: TCSSElement;
begin
  Result:=TCSSElement(FList.First);
end;

function TCSSElementList.Last: TCSSElement;
begin
  Result:=TCSSElement(FList.Last);
end;

procedure TCSSElementList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex,NewIndex);
end;

procedure TCSSElementList.Assign(aList: TCSSElementList);
begin
  if Self=aList then exit;
  FList.Assign(aList.FList);
end;

procedure TCSSElementList.Pack;
begin
  FList.Pack;
end;

procedure TCSSElementList.Sort(const Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

procedure TCSSElementList.Iterate(aVisitor: TCSSTreeVisitor);

Var
  I : Integer;

begin
  For I:=0 to Count-1 do
    Elements[i].Iterate(aVisitor);
end;

{ TCSSIntegerElement }

function TCSSIntegerElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
begin
  Result:=IntToStr(Value)+CSSUnitNames[Units];
  if aFormat then
    Result:=aIndent+Result;
end;

class function TCSSIntegerElement.CSSType: TCSSType;
begin
  Result:=csstInteger;
end;

function TCSSIntegerElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSIntegerElement absolute Obj;
begin
  if Obj is TCSSIntegerElement then
    begin
    if (FIsEscaped<>Src.FIsEscaped)
        or (FUnits<>Src.FUnits)
        or (FValue<>Src.FValue) then
      exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

{ TCSSBinaryElement }

procedure TCSSBinaryElement.SetLeft(AValue: TCSSElement);
begin
  if FLeft=AValue then Exit;
  if FLeft<>nil then
    FLeft.Parent:=nil;
  FreeAndNil(FLeft);
  FLeft:=AValue;
  if FLeft<>nil then
    FLeft.Parent:=Self;
end;

function TCSSBinaryElement.GetAsString(aFormat: Boolean;
  const aIndent: TCSSString): TCSSString;
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
  Result:=csstBinaryOp;
end;

function TCSSBinaryElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSBinaryElement absolute Obj;
begin
  if Obj is TCSSBinaryElement then
    begin
    if FOperation<>Src.FOperation then exit(false);
    if not SubElEquals(FLeft,Src.FLeft) then exit(false);
    end;
  Result:=inherited Equals(Obj);
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
  if FRight<>nil then
    FRight.Parent:=nil;
  FreeAndNil(FRight);
  FRight:=AValue;
  if FRight<>nil then
    FRight.Parent:=Self;
end;

destructor TCSSBaseUnaryElement.Destroy;
begin
  Right:=Nil;
  inherited Destroy;
end;

function TCSSBaseUnaryElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSBaseUnaryElement absolute Obj;
begin
  if Obj is TCSSBaseUnaryElement then
    begin
    if not SubElEquals(FRight,Src.FRight) then exit(false);
    end;
  Result:=inherited Equals(Obj);
end;

procedure TCSSBaseUnaryElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  inherited IterateChildren(aVisitor);
  If Assigned(FRight) then
    FRight.Iterate(aVisitor);
end;

{ TCSSElement }

function TCSSElement.GetAsUnFormattedString: TCSSString;
begin
  Result:=GetAsString(False,'');
end;

function TCSSElement.GetAsFormattedString: TCSSString;
begin
  Result:=GetAsString(True,'');
end;

procedure TCSSElement.SetParent(const AValue: TCSSElement);
begin
  if FParent=AValue then Exit;
  FParent:=AValue;
end;

function TCSSElement.GetAsString(aFormat: Boolean; const aIndent: TCSSString
  ): TCSSString;
begin
  if aFormat then ;
  if aIndent='' then ;
  Result:='';
end;

function TCSSElement.SubElEquals(ElA, ElB: TCSSElement): boolean;
begin
  if ElA=nil then
    Result:=ElB=nil
  else
    begin
    if ElB=nil then exit(false);
    Result:=ElA.Equals(ElB);
    end;
end;

procedure TCSSElement.IterateChildren(aVisitor: TCSSTreeVisitor);
begin
  if Assigned(aVisitor) then ;
end;

constructor TCSSElement.Create(const aFileName: TCSSString; aRow, aCol: Integer);
begin
  FFileName:=aFileName;
  FRow:=aRow;
  FCol:=aCol;
end;

destructor TCSSElement.Destroy;
begin
  if FData is TCSSElementOwnedData then
  begin
    FData.Free;
    FData:=nil;
  end;
  inherited Destroy;
end;

class function TCSSElement.CSSType: TCSSType;
begin
  Result:=csstUnknown;
end;

function TCSSElement.Equals(Obj: TObject): boolean;
var
  Src: TCSSElement absolute Obj;
begin
  if Obj is TCSSElement then
    begin
    Result:=(FCol=Src.FCol)
        and (FData=Src.FData)
        and (FFileName=Src.FFileName)
        and (FRow=Src.FRow);
    end
  else
    Result:=inherited Equals(Obj);
end;

procedure TCSSElement.Iterate(aVisitor: TCSSTreeVisitor);
begin
  aVisitor.Visit(Self);
  IterateChildren(aVisitor);
end;

procedure TCSSElement.FreeCustomData;
var
  Visitor: TCSSVisitorFreeCustomData;
begin
  Visitor:=TCSSVisitorFreeCustomData.Create;
  try
    Iterate(Visitor);
  finally
    Visitor.Free;
  end;
end;

{ TCSSVisitorFreeCustomData }

procedure TCSSVisitorFreeCustomData.Visit(obj: TCSSElement);
var
  d: TObject;
begin
  if obj.CustomData=nil then exit;
  d:=obj.CustomData;
  obj.CustomData:=nil;
  d.Free;
end;

end.

