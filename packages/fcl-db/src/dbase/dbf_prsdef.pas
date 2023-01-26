unit dbf_prsdef;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Pascal Ganaye,Micha Nelissen and other members of the
    Free Pascal development team

    DBF expression definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
interface

{$I dbf_common.inc}

uses
  SysUtils,
  Classes,
  Db,
  dbf_prssupp;

const
  MaxArg = 6;
  ArgAllocSize = 32;

type
  TExpressionType = (etInteger, etString, etBoolean, etLargeInt, etFloat, etDateTime,
    etLeftBracket, etRightBracket, etComma, etUnknown);

  EParserException = class(Exception);
  PExpressionRec = ^TExpressionRec;
  PDynamicType = ^TDynamicType;
  PDateTimeRec = ^TDateTimeRec;
{$ifdef SUPPORT_INT64}
  PLargeInt = ^Int64;
{$endif}

  TExprWord = class;

  TExprFunc = procedure(Expr: PExpressionRec);

//-----

  TDynamicType = class(TObject)
  private
    FMemory: PPAnsiChar;
    FMemoryPos: PPAnsiChar;
    FSize: PInteger;
  public
    constructor Create(DestMem, DestPos: PPAnsiChar; ASize: PInteger);

    procedure AssureSpace(ASize: Integer);
    procedure Resize(NewSize: Integer; Exact: Boolean);
    procedure Rewind;
    procedure Append(Source: PAnsiChar; Length: Integer);
    procedure AppendInteger(Source: Integer);

    property Memory: PPAnsiChar read FMemory;
    property MemoryPos: PPAnsiChar read FMemoryPos;
    property Size: PInteger read FSize;
  end;

  TExpressionRec = record
    //used both as linked tree and linked list for maximum evaluation efficiency
    Oper: TExprFunc;
    Next: PExpressionRec;
    Res: TDynamicType;
    ExprWord: TExprWord;
    AuxData: pointer;
    ResetDest: boolean;
    WantsFunction: boolean;
    Args: array[0..MaxArg-1] of PAnsiChar;
    ArgsPos: array[0..MaxArg-1] of PAnsiChar;
    ArgsSize: array[0..MaxArg-1] of Integer;
    ArgsType: array[0..MaxArg-1] of TExpressionType;
    ArgList: array[0..MaxArg-1] of PExpressionRec;
  end;

  TExprCollection = class(TNoOwnerCollection)
  public
    procedure Check;
    procedure EraseExtraBrackets;
  end;

  TExprWordRec = record
    Name: PAnsiChar;
    ShortName: PAnsiChar;
    IsOperator: Boolean;
    IsVariable: Boolean;
    IsFunction: Boolean;
    NeedsCopy: Boolean;
    FixedLen: Boolean;
    CanVary: Boolean;
    ResultType: TExpressionType;
    MinArg: Integer;
    MaxArg: Integer;
    TypeSpec: PAnsiChar;
    Description: PAnsiChar;
    ExprFunc: TExprFunc;
  end;

  { TExprWord }

  TExprWord = class(TObject)
  private
    FName: AnsiString;
    FExprFunc: TExprFunc;
  protected
    FRefCount: Cardinal;

    function GetIsOperator: Boolean; virtual;
    function GetIsVariable: Boolean;
    function GetNeedsCopy: Boolean;
    function GetFixedLen: Integer; virtual;
    function GetCanVary: Boolean; virtual;
    function GetResultType: TExpressionType; virtual;
    function GetMinFunctionArg: Integer; virtual;
    function GetMaxFunctionArg: Integer; virtual;
    function GetDescription: AnsiString; virtual;
    function GetTypeSpec: AnsiString; virtual;
    function GetShortName: AnsiString; virtual;
    procedure SetFixedLen(NewLen: integer); virtual;
  public
    constructor Create(const AName: AnsiString; AExprFunc: TExprFunc);

    function AsPointer: PAnsiChar; virtual;
    function LenAsPointer: PInteger; virtual;
    function IsNullAsPointer: PBoolean; virtual;
    function IsFunction: Boolean; virtual;

    property ExprFunc: TExprFunc read FExprFunc;
    property IsOperator: Boolean read GetIsOperator;
    property CanVary: Boolean read GetCanVary;
    property IsVariable: Boolean read GetIsVariable;
    property NeedsCopy: Boolean read GetNeedsCopy;
    property FixedLen: Integer read GetFixedLen write SetFixedLen;
    property ResultType: TExpressionType read GetResultType;
    property MinFunctionArg: Integer read GetMinFunctionArg;
    property MaxFunctionArg: Integer read GetMaxFunctionArg;
    property Name: AnsiString read FName;
    property ShortName: AnsiString read GetShortName;
    property Description: AnsiString read GetDescription;
    property TypeSpec: AnsiString read GetTypeSpec;
  end;

  TExpressShortList = class(TSortedCollection)
  public
    function KeyOf(Item: Pointer): Pointer; override;
    function Compare(Key1, Key2: Pointer): Integer; override;
    procedure FreeItem(Item: Pointer); override;
  end;

  TExpressList = class(TSortedCollection)
  private
    FShortList: TExpressShortList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer); override;
    function  KeyOf(Item: Pointer): Pointer; override;
    function  Compare(Key1, Key2: Pointer): Integer; override;
    function  Search(Key: Pointer; var Index: Integer): Boolean; override;
    procedure FreeItem(Item: Pointer); override;
  end;

  TConstant = class(TExprWord)
  private
    FResultType: TExpressionType;
  protected
    function GetResultType: TExpressionType; override;
  public
    constructor Create(const AName: AnsiString; AVarType: TExpressionType; AExprFunc: TExprFunc);
  end;

  TFloatConstant = class(TConstant)
  private
    FValue: Double;
  public
    // not overloaded to support older Delphi versions
    constructor Create(const AName: AnsiString; const AValue: AnsiString);
    constructor CreateAsDouble(const AName: AnsiString; AValue: Double);

    function AsPointer: PAnsiChar; override;

    property Value: Double read FValue write FValue;
  end;

  TUserConstant = class(TFloatConstant)
  private
    FDescription: AnsiString;
  protected
    function GetDescription: AnsiString; override;
  public
    constructor CreateAsDouble(const AName, Descr: AnsiString; AValue: Double);
  end;

  TStringConstant = class(TConstant)
  private
    FValue: ansistring;
  public
    // Allow undelimited, delimited by single quotes, delimited by double quotes
    // If delimited, allow escaping inside AnsiString with double delimiters
    constructor Create(const AValue: ansistring);

    function AsPointer: PAnsiChar; override;
  end;

  TIntegerConstant = class(TConstant)
  private
    FValue: Integer;
  public
    constructor Create(AValue: Integer);

    function AsPointer: PAnsiChar; override;
  end;

{$ifdef SUPPORT_INT64}
  TLargeIntConstant = class(TConstant)
  private
    FValue: Int64;
  public
    constructor Create(AValue: Int64);

    function AsPointer: PAnsiChar; override;
  end;
{$endif}

  TBooleanConstant = class(TConstant)
  private
    FValue: Boolean;
  public
    // not overloaded to support older Delphi versions
    constructor Create(const AName: AnsiString; AValue: Boolean);

    function AsPointer: PAnsiChar; override;

    property Value: Boolean read FValue write FValue;
  end;

  { TNullConstant }

  TNullConstant = class(TConstant)
  private
  public
    constructor Create;
  end;

  { TVariable }

  TVariable = class(TExprWord)
  private
    FResultType: TExpressionType;
    FIsNull: PBoolean;
  protected
    function GetCanVary: Boolean; override;
    function GetResultType: TExpressionType; override;
  public
    constructor Create(const AName: AnsiString; AVarType: TExpressionType; AExprFunc: TExprFunc);
    function IsNullAsPointer: PBoolean; override;
  end;

  TFloatVariable = class(TVariable)
  private
    FValue: PDouble;
  public
    constructor Create(const AName: AnsiString; AValue: PDouble);

    function AsPointer: PAnsiChar; override;
  end;

  TStringVariable = class(TVariable)
  private
    FValue: PPAnsiChar;
    FFixedLen: Integer;
  protected
    function GetFixedLen: Integer; override;
    procedure SetFixedLen(NewLen: integer); override;
  public
    constructor Create(const AName: AnsiString; AValue: PPAnsiChar; AIsNull: PBoolean);

    function LenAsPointer: PInteger; override;
    function AsPointer: PAnsiChar; override;

    property FixedLen: Integer read FFixedLen;
  end;

  TDateTimeVariable = class(TVariable)
  private
    FValue: PDateTimeRec;
  public
    constructor Create(const AName: AnsiString; AValue: PDateTimeRec);

    function AsPointer: PAnsiChar; override;
  end;

  TIntegerVariable = class(TVariable)
  private
    FValue: PInteger;
  public
    constructor Create(const AName: AnsiString; AValue: PInteger; AIsNull: PBoolean);

    function AsPointer: PAnsiChar; override;
  end;

{$ifdef SUPPORT_INT64}

  TLargeIntVariable = class(TVariable)
  private
    FValue: PLargeInt;
  public
    constructor Create(const AName: AnsiString; AValue: PLargeInt);

    function AsPointer: PAnsiChar; override;
  end;

{$endif}

  TBooleanVariable = class(TVariable)
  private
    FValue: PBoolean;
  public
    constructor Create(const AName: AnsiString; AValue: PBoolean);

    function AsPointer: PAnsiChar; override;
  end;

  TLeftBracket = class(TExprWord)
    function GetResultType: TExpressionType; override;
  end;

  TRightBracket = class(TExprWord)
  protected
    function GetResultType: TExpressionType; override;
  end;

  TComma = class(TExprWord)
  protected
    function GetResultType: TExpressionType; override;
  end;

  TFunction = class(TExprWord)
  private
    FIsOperator: Boolean;
    FOperPrec: Integer;
    FMinFunctionArg: Integer;
    FMaxFunctionArg: Integer;
    FDescription: AnsiString;
    FTypeSpec: AnsiString;
    FShortName: AnsiString;
    FResultType: TExpressionType;
  protected
    function GetDescription: AnsiString; override;
    function GetIsOperator: Boolean; override;
    function GetMinFunctionArg: Integer; override;
    function GetMaxFunctionArg: Integer; override;
    function GetResultType: TExpressionType; override;
    function GetTypeSpec: AnsiString; override;
    function GetShortName: AnsiString; override;

    procedure InternalCreate(const AName, ATypeSpec: AnsiString; AMinFuncArg: Integer; AResultType: TExpressionType;
      AExprFunc: TExprFunc; AIsOperator: Boolean; AOperPrec: Integer);
  public
    constructor Create(const AName, AShortName, ATypeSpec: AnsiString; AMinFuncArg: Integer; AResultType: TExpressionType; AExprFunc: TExprFunc; const Descr: AnsiString);
    // Create operator: name, param types used, result type, func addr, operator precedence
    constructor CreateOper(const AName, ATypeSpec: AnsiString; AResultType: TExpressionType; AExprFunc: TExprFunc; AOperPrec: Integer);

    function IsFunction: Boolean; override;

    property OperPrec: Integer read FOperPrec;
    property TypeSpec: AnsiString read FTypeSpec;
  end;

  TVaryingFunction = class(TFunction)
    // Functions that can vary e.g. random generators
    // should be TVaryingFunction to ensure that they are
    // always evaluated
  protected
    function GetCanVary: Boolean; override;
  end;

const
  ListChar = ','; {the delimiter used with the 'in' operator: e.g.,
  ('a' in 'a,b') =True
  ('c' in 'a,b') =False}

function ExprCharToExprType(ExprChar: AnsiChar): TExpressionType;

implementation

function ExprCharToExprType(ExprChar: AnsiChar): TExpressionType;
begin
  case ExprChar of
    'B': Result := etBoolean;
    'I': Result := etInteger;
    'L': Result := etLargeInt;
    'F': Result := etFloat;
    'D': Result := etDateTime;
    'S': Result := etString;
    'U': Result := etUnknown;
  else
    Result := etUnknown;
  end;
end;

procedure _FloatVariable(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^;
end;

procedure _BooleanVariable(Param: PExpressionRec);
begin
  with Param^ do
    PBoolean(Res.MemoryPos^)^ := PBoolean(Args[0])^;
end;

procedure _StringConstant(Param: PExpressionRec);
begin
  with Param^ do
    Res.Append(Args[0], StrLen(Args[0]));
end;

procedure _StringVariable(Param: PExpressionRec);
var
  length: integer;
begin
  with Param^ do
  begin
    length := PInteger(Args[1])^;
    if length = -1 then
      length := StrLen(PPAnsiChar(Args[0])^);
    Res.Append(PPAnsiChar(Args[0])^, length);
    // NULL indicator (placed after NULL terminated AnsiString)
    length := StrLen(PPAnsiChar(Args[0])^)+1;
    Res.AssureSpace(length+1);
    PBoolean(Res.Memory^+length)^ := Assigned(Args[2]) and PBoolean(Args[2])^;
  end;
end;

procedure _DateTimeVariable(Param: PExpressionRec);
begin
  with Param^ do
    PDateTimeRec(Res.MemoryPos^)^ := PDateTimeRec(Args[0])^;
end;

procedure _IntegerVariable(Param: PExpressionRec);
begin
  with Param^ do begin
    PInteger(Res.MemoryPos^)^ := PInteger(Args[0])^;
    PBoolean(Res.MemoryPos^+8)^ := Assigned(Args[2]) and PBoolean(Args[2])^; // NULL indicator
  end;
end;

{
procedure _SmallIntVariable(Param: PExpressionRec);
begin
  with Param^ do
    PSmallInt(Res.MemoryPos^)^ := PSmallInt(Args[0])^;
end;
}

{$ifdef SUPPORT_INT64}
procedure _LargeIntVariable(Param: PExpressionRec);
begin
  with Param^ do
    PLargeInt(Res.MemoryPos^)^ := PLargeInt(Args[0])^;
end;
{$endif}

procedure _NullConstant(Param: PExpressionRec);
begin
  // NOP
end;

{ TExpressionWord }

constructor TExprWord.Create(const AName: AnsiString; AExprFunc: TExprFunc);
begin
  FName := AName;
  FExprFunc := AExprFunc;
end;

function TExprWord.GetCanVary: Boolean;
begin
  Result := False;
end;

function TExprWord.GetDescription: AnsiString;
begin
  Result := EmptyStr;
end;

function TExprWord.GetShortName: AnsiString;
begin
  Result := EmptyStr;
end;

function TExprWord.GetIsOperator: Boolean;
begin
  Result := False;
end;

function TExprWord.GetIsVariable: Boolean;
begin
  // delphi wants to call the function pointed to by the variable, use '@'
  // fpc simply returns pointer to function, no '@' needed
  Result := (@FExprFunc = @_StringVariable)         or
            (@FExprFunc = @_StringConstant)         or
            (@FExprFunc = @_FloatVariable)          or
            (@FExprFunc = @_IntegerVariable)        or
//            (FExprFunc = @_SmallIntVariable)       or
{$ifdef SUPPORT_INT64}
            (@FExprFunc = @_LargeIntVariable)       or
{$endif}
            (@FExprFunc = @_DateTimeVariable)       or
            (@FExprFunc = @_BooleanVariable);
end;

function TExprWord.GetNeedsCopy: Boolean;
begin
  Result := (@FExprFunc <> @_StringConstant)         and
//            (@FExprFunc <> @_StringVariable)         and
//            (@FExprFunc <> @_StringVariableFixedLen) and
// AnsiString variable cannot be used as normal parameter
// because it is indirectly referenced and possibly
// not null-terminated (fixed len)
            (@FExprFunc <> @_FloatVariable)          and
            (@FExprFunc <> @_IntegerVariable)        and
//            (FExprFunc <> @_SmallIntVariable)       and
{$ifdef SUPPORT_INT64}
            (@FExprFunc <> @_LargeIntVariable)       and
{$endif}
            (@FExprFunc <> @_DateTimeVariable)       and
            (@FExprFunc <> @_BooleanVariable);
end;

function TExprWord.GetFixedLen: Integer;
begin
  // -1 means variable, non-fixed length
  Result := -1;
end;

function TExprWord.GetMinFunctionArg: Integer;
begin
  Result := 0;
end;

function TExprWord.GetMaxFunctionArg: Integer;
begin
  Result := 0;
end;

function TExprWord.GetResultType: TExpressionType;
begin
  Result := etUnknown;
end;

function TExprWord.GetTypeSpec: AnsiString;
begin
  Result := EmptyStr;
end;

function TExprWord.AsPointer: PAnsiChar;
begin
  Result := nil;
end;

function TExprWord.LenAsPointer: PInteger;
begin
  Result := nil;
end;

function TExprWord.IsNullAsPointer: PBoolean;
begin
  Result := nil;
end;

function TExprWord.IsFunction: Boolean;
begin
  Result := False;
end;

procedure TExprWord.SetFixedLen(NewLen: integer);
begin
end;

{ TConstant }

constructor TConstant.Create(const AName: AnsiString; AVarType: TExpressionType; AExprFunc: TExprFunc);
begin
  inherited Create(AName, AExprFunc);

  FResultType := AVarType;
end;

function TConstant.GetResultType: TExpressionType;
begin
  Result := FResultType;
end;

{ TFloatConstant }

constructor TFloatConstant.Create(const AName: AnsiString; const aValue : AnsiString);
begin
  inherited Create(AName, etFloat, _FloatVariable);

  if Length(AValue) > 0 then
    FValue := StrToFloat(AValue)
  else
    FValue := 0.0;
end;

constructor TFloatConstant.CreateAsDouble(const AName: AnsiString; AValue: Double);
begin
  inherited Create(AName, etFloat, _FloatVariable);

  FValue := AValue;
end;

function TFloatConstant.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(@FValue);
end;

{ TUserConstant }

constructor TUserConstant.CreateAsDouble(const AName, Descr: AnsiString; AValue: Double);
begin
  FDescription := Descr;
  inherited CreateAsDouble(AName, AValue);
end;

function TUserConstant.GetDescription: AnsiString;
begin
  Result := FDescription;
end;

{ TStringConstant }

constructor TStringConstant.Create(const AValue: AnsiString);
var
  firstChar, lastChar: AnsiChar;
begin
  inherited Create(AValue, etString, _StringConstant);

  firstChar := AValue[1];
  lastChar := AValue[Length(AValue)];
  if (firstChar = lastChar) and ((firstChar = '''') or (firstChar = '"')) then
  begin
    FValue := Copy(AValue, 2, Length(AValue) - 2);
    FValue := StringReplace(FValue, firstChar+FirstChar, firstChar, [rfReplaceAll,rfIgnoreCase])
  end
  else
    FValue := AValue;
end;

function TStringConstant.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(FValue);
end;

{ TBooleanConstant }

constructor TBooleanConstant.Create(const AName: AnsiString; AValue: Boolean);
begin
  inherited Create(AName, etBoolean, _BooleanVariable);

  FValue := AValue;
end;

function TBooleanConstant.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(@FValue);
end;

{ TIntegerConstant }

constructor TIntegerConstant.Create(AValue: Integer);
begin
  inherited Create(IntToStr(AValue), etInteger, _IntegerVariable);

  FValue := AValue;
end;

function TIntegerConstant.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(@FValue);
end;

{$ifdef SUPPORT_INT64}
{ TLargeIntConstant }

constructor TLargeIntConstant.Create(AValue: Int64);
begin
  inherited Create(IntToStr(AValue), etLargeInt, _LargeIntVariable);

  FValue := AValue;
end;

function TLargeIntConstant.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(@FValue);
end;
{$endif}

{ TNullConstant }

constructor TNullConstant.Create;
begin
  inherited Create('NULL', etUnknown, _NullConstant);
end;

{ TVariable }

constructor TVariable.Create(const AName: AnsiString; AVarType: TExpressionType; AExprFunc: TExprFunc);
begin
  inherited Create(AName, AExprFunc);

  FResultType := AVarType;
end;

function TVariable.GetCanVary: Boolean;
begin
  Result := True;
end;

function TVariable.GetResultType: TExpressionType;
begin
  Result := FResultType;
end;

function TVariable.IsNullAsPointer: PBoolean;
begin
  Result := FIsNull;
end;

{ TFloatVariable }

constructor TFloatVariable.Create(const AName: AnsiString; AValue: PDouble);
begin
  inherited Create(AName, etFloat, _FloatVariable);
  FValue := AValue;
end;

function TFloatVariable.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(FValue);
end;

{ TStringVariable }

constructor TStringVariable.Create(const AName: AnsiString; AValue: PPAnsiChar; AIsNull: PBoolean);
begin
  // variable or fixed length?
  inherited Create(AName, etString, _StringVariable);

  // store pointer to AnsiString
  FValue := AValue;
  FFixedLen := -1;
  FIsNull := AIsNull;
end;

function TStringVariable.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(FValue);
end;

function TStringVariable.GetFixedLen: Integer;
begin
  Result := FFixedLen;
end;

function TStringVariable.LenAsPointer: PInteger;
begin
  Result := @FFixedLen;
end;

procedure TStringVariable.SetFixedLen(NewLen: integer);
begin
  FFixedLen := NewLen;
end;

{ TDateTimeVariable }

constructor TDateTimeVariable.Create(const AName: AnsiString; AValue: PDateTimeRec);
begin
  inherited Create(AName, etDateTime, _DateTimeVariable);
  FValue := AValue;
end;

function TDateTimeVariable.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(FValue);
end;

{ TIntegerVariable }

constructor TIntegerVariable.Create(const AName: AnsiString; AValue: PInteger; AIsNull: PBoolean);
begin
  inherited Create(AName, etInteger, _IntegerVariable);
  FValue := AValue;
  FIsNull := AIsNull;
end;

function TIntegerVariable.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(FValue);
end;

{$ifdef SUPPORT_INT64}

{ TLargeIntVariable }

constructor TLargeIntVariable.Create(const AName: AnsiString; AValue: PLargeInt);
begin
  inherited Create(AName, etLargeInt, _LargeIntVariable);
  FValue := AValue;
end;

function TLargeIntVariable.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(FValue);
end;

{$endif}

{ TBooleanVariable }

constructor TBooleanVariable.Create(const AName: AnsiString; AValue: PBoolean);
begin
  inherited Create(AName, etBoolean, _BooleanVariable);
  FValue := AValue;
end;

function TBooleanVariable.AsPointer: PAnsiChar;
begin
  Result := PAnsiChar(FValue);
end;

{ TLeftBracket }

function TLeftBracket.GetResultType: TExpressionType;
begin
  Result := etLeftBracket;
end;

{ TRightBracket }

function TRightBracket.GetResultType: TExpressionType;
begin
  Result := etRightBracket;
end;

{ TComma }

function TComma.GetResultType: TExpressionType;
begin
  Result := etComma;
end;

{ TExpressList }

constructor TExpressList.Create;
begin
  inherited;

  FShortList := TExpressShortList.Create;
end;

destructor TExpressList.Destroy;
begin
  inherited;
  FShortList.Free;
end;

procedure TExpressList.Add(Item: Pointer);
var
  I: Integer;
begin
  inherited;

  { remember we reference the object }
  Inc(TExprWord(Item).FRefCount);

  { also add ShortName as reference }
  if Length(TExprWord(Item).ShortName) > 0 then
  begin
    FShortList.Search(FShortList.KeyOf(Item), I);
    FShortList.Insert(I, Item);
  end;
end;

function TExpressList.Compare(Key1, Key2: Pointer): Integer;
begin
  Result := StrIComp(PAnsiChar(Key1), PAnsiChar(Key2));
end;

function TExpressList.KeyOf(Item: Pointer): Pointer;
begin
  Result := PAnsiChar(TExprWord(Item).Name);
end;

procedure TExpressList.FreeItem(Item: Pointer);
begin
  Dec(TExprWord(Item).FRefCount);
  FShortList.Remove(Item);
  if TExprWord(Item).FRefCount = 0 then
    inherited;
end;

function TExpressList.Search(Key: Pointer; var Index: Integer): Boolean;
var
  SecIndex: Integer;
begin
  Result := inherited Search(Key, Index);
  if not Result then
  begin
    Result := FShortList.Search(Key, SecIndex);
    if Result then
      Index := IndexOf(FShortList.Items[SecIndex]);
  end;
end;

function TExpressShortList.Compare(Key1, Key2: Pointer): Integer;
begin
  Result := StrIComp(PAnsiChar(Key1), PAnsiChar(Key2));
end;

function TExpressShortList.KeyOf(Item: Pointer): Pointer;
begin
  Result := PAnsiChar(TExprWord(Item).ShortName);
end;

procedure TExpressShortList.FreeItem(Item: Pointer);
begin
end;

{ TExprCollection }

procedure TExprCollection.Check;
var
  brCount, I: Integer;
begin
  brCount := 0;
  for I := 0 to Count - 1 do
  begin
    case TExprWord(Items[I]).ResultType of
      etLeftBracket: Inc(brCount);
      etRightBracket: Dec(brCount);
    end;
  end;
  if brCount <> 0 then
    raise EParserException.Create('Unequal brackets');
end;

procedure TExprCollection.EraseExtraBrackets;
var
  I: Integer;
  brCount: Integer;
begin
  if (TExprWord(Items[0]).ResultType = etLeftBracket) then
  begin
    brCount := 1;
    I := 1;
    while (I < Count) and (brCount > 0) do
    begin
      case TExprWord(Items[I]).ResultType of
        etLeftBracket: Inc(brCount);
        etRightBracket: Dec(brCount);
      end;
      Inc(I);
    end;
    if (brCount = 0) and (I = Count) and (TExprWord(Items[I - 1]).ResultType =
      etRightBracket) then
    begin
      for I := 0 to Count - 3 do
        Items[I] := Items[I + 1];
      Count := Count - 2;
      EraseExtraBrackets; //Check if there are still too many brackets
    end;
  end;
end;

{ TFunction }

constructor TFunction.Create(const AName, AShortName, ATypeSpec: AnsiString; AMinFuncArg: Integer; AResultType: TExpressionType;
  AExprFunc: TExprFunc; const Descr: AnsiString);
begin
  //to increase compatibility don't use default parameters
  FDescription := Descr;
  FShortName := AShortName;
  InternalCreate(AName, ATypeSpec, AMinFuncArg, AResultType, AExprFunc, false, 0);
end;

constructor TFunction.CreateOper(const AName, ATypeSpec: AnsiString; AResultType: TExpressionType;
  AExprFunc: TExprFunc; AOperPrec: Integer);
begin
  InternalCreate(AName, ATypeSpec, -1, AResultType, AExprFunc, true, AOperPrec);
end;

procedure TFunction.InternalCreate(const AName, ATypeSpec: AnsiString; AMinFuncArg: Integer; AResultType: TExpressionType;
  AExprFunc: TExprFunc; AIsOperator: Boolean; AOperPrec: Integer);
begin
  inherited Create(AName, AExprFunc);

  FMaxFunctionArg := Length(ATypeSpec);
  FMinFunctionArg := AMinFuncArg;
  if AMinFuncArg = -1 then
    FMinFunctionArg := FMaxFunctionArg;
  FIsOperator := AIsOperator;
  FOperPrec := AOperPrec;
  FTypeSpec := ATypeSpec;
  FResultType := AResultType;

  // check correctness
  if FMaxFunctionArg > MaxArg then
    raise EParserException.Create('Too many arguments');
end;

function TFunction.GetDescription: AnsiString;
begin
  Result := FDescription;
end;

function TFunction.GetIsOperator: Boolean;
begin
  Result := FIsOperator;
end;

function TFunction.GetMinFunctionArg: Integer;
begin
  Result := FMinFunctionArg;
end;

function TFunction.GetMaxFunctionArg: Integer;
begin
  Result := FMaxFunctionArg;
end;

function TFunction.GetResultType: TExpressionType;
begin
  Result := FResultType;
end;

function TFunction.GetShortName: AnsiString;
begin
  Result := FShortName;
end;

function TFunction.GetTypeSpec: AnsiString;
begin
  Result := FTypeSpec;
end;

function TFunction.IsFunction: Boolean;
begin
  Result := True;
end;

{ TVaryingFunction }

function TVaryingFunction.GetCanVary: Boolean;
begin
  Result := True;
end;

{ TDynamicType }

constructor TDynamicType.Create(DestMem, DestPos: PPAnsiChar; ASize: PInteger);
begin
  inherited Create;

  FMemory := DestMem;
  FMemoryPos := DestPos;
  FSize := ASize;
end;

procedure TDynamicType.Rewind;
begin
  FMemoryPos^ := FMemory^;
end;

procedure TDynamicType.AssureSpace(ASize: Integer);
begin
  // need more memory?
  if ((FMemoryPos^) - (FMemory^) + ASize) > (FSize^) then
    Resize((FMemoryPos^) - (FMemory^) + ASize, False);
end;

procedure TDynamicType.Resize(NewSize: Integer; Exact: Boolean);
var
  tempBuf: PAnsiChar;
  bytesCopy, pos: Integer;
begin
  // if not exact requested make newlength a multiple of ArgAllocSize
  if not Exact then
    NewSize := NewSize div ArgAllocSize * ArgAllocSize + ArgAllocSize;
  // create new buffer
  GetMem(tempBuf, NewSize);
  // copy memory
  bytesCopy := FSize^;
  if bytesCopy > NewSize then
    bytesCopy := NewSize;
  Move(FMemory^^, tempBuf^, bytesCopy);
  // save position in AnsiString
  pos := FMemoryPos^ - FMemory^;
  // delete old mem
  FreeMem(FMemory^);
  // assign new
  FMemory^ := tempBuf;
  FSize^ := NewSize;
  // assign position
  FMemoryPos^ := FMemory^ + pos;
end;

procedure TDynamicType.Append(Source: PAnsiChar; Length: Integer);
begin
  // make room for AnsiString plus null-terminator
  AssureSpace(Length+4);
  // copy
  Move(Source^, FMemoryPos^^, Length);
  Inc(FMemoryPos^, Length);
  // null-terminate
  FMemoryPos^^ := #0;
  Inc(FMemoryPos^);
end;

procedure TDynamicType.AppendInteger(Source: Integer);
begin
  // make room for number
  AssureSpace(12);
  Inc(FMemoryPos^, GetStrFromInt(Source, FMemoryPos^));
  FMemoryPos^^ := #0;
  Inc(FMemoryPos^);
end;

end.

