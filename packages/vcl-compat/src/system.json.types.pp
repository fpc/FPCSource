{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by the Free Pascal development team

    Delphi compatibility unit: Various JSON structures & routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System.JSON.Types;

{$mode objfpc}
{$modeswitch advancedrecords}
{$h+}

interface

{$SCOPEDENUMS ON}

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Generics.Collections, System.Classes, System.StrUtils, System.DateUtils;
  {$ELSE}
  SysUtils, Generics.Collections, Classes, StrUtils, dateutils;
  {$ENDIF}

const
  JsonFalse            = 'false';
  JsonNan              = 'NaN';
  JsonNegativeInfinity = '-Infinity';
  JsonNew              = 'new';
  JsonNull             = 'null';
  JsonPositiveInfinity = 'Infinity';
  JsonTrue             = 'true';
  JsonUndefined        = 'undefined';

  JsonExtBinaryPropertyName     = '$binary';
  JsonExtCodePropertyName       = '$code';
  JsonExtDatePropertyName       = '$date';
  JsonExtDbPropertyName         = '$db';
  JsonExtDecimalPropertyName    = '$numberdecimal';
  JsonExtIdPropertyName         = '$id';
  JsonExtMaxKeyPropertyName     = '$maxkey';
  JsonExtMinKeyPropertyName     = '$minkey';
  JsonExtNumberLongPropertyName = '$numberlong';
  JsonExtOidPropertyName        = '$oid';
  JsonExtOptionsPropertyName    = '$options';
  JsonExtRefPropertyName        = '$ref';
  JsonExtRegexPropertyName      = '$regex';
  JsonExtScopePropertyName      = '$scope';
  JsonExtTypePropertyName       = '$type';
  JsonExtUndefinedPropertyName  = '$undefined';
  JsonExtTimestampPropertyName  = '$timestamp';

  JsonExtMaxPropertyNameLen     = Length(JsonExtNumberLongPropertyName);

  OidBytesCount = 12;

type
  TJsonToken = (
    None, StartObject, StartArray, StartConstructor, PropertyName, Comment,
    Raw, Integer, Float, &String, Boolean, Null, Undefined, EndObject,
    EndArray, EndConstructor, Date, Bytes, Oid, RegEx, DBRef, CodeWScope,
    MinKey, MaxKey, Decimal, TimeStamp
  );


  TJsonContainerType          = (None, &Object, &Array, &Constructor);
  TJsonDateFormatHandling     = (Iso, Unix, FormatSettings);
  TJsonDateParseHandling      = (None, DateTime);
  TJsonDateTimeZoneHandling   = (Local, Utc);
  TJsonDefaultValueHandling   = (Include, Ignore, Populate, IgnoreAndPopulate);
  TJsonEmptyValueHandling     = (Empty, Null);
  TJsonFloatFormatHandling    = (&String, Symbol, DefaultValue);
  TJsonFormatting             = (None, Indented);
  TJsonNullValueHandling      = (Include, Ignore);
  TJsonObjectCreationHandling = (Auto, Reuse, Replace);
  TJsonReferenceLoopHandling  = (Error, Ignore, Serialize);
  TJsonStringEscapeHandling   = (Default, EscapeNonAscii, EscapeHtml);
  TJsonTypeNameHandling       = (None, Objects, Arrays, All, Auto);

const
  JSONPrimitiveTokens = [
    TJsonToken.Integer, TJsonToken.Float, TJsonToken.&String, TJsonToken.Boolean,
    TJsonToken.Undefined, TJsonToken.Null, TJsonToken.Date, TJsonToken.Bytes,
    TJsonToken.Oid, TJsonToken.RegEx, TJsonToken.DBRef, TJsonToken.CodeWScope,
    TJsonToken.MinKey, TJsonToken.MaxKey
    ];

  JSONStartTokens =  [TJsonToken.StartObject,TJsonToken.StartArray,TJsonToken.StartConstructor];
  JSONEndTokens = [TJsonToken.EndObject, TJsonToken.EndArray, TJsonToken.EndConstructor];


Type

  TJsonLineInfo = class
  public
    function GetLineNumber: Integer; virtual;
    function GetLinePosition: Integer; virtual;
    function HasLineInfo: Boolean; virtual;
    property LineNumber: Integer read GetLineNumber;
    property LinePosition: Integer read GetLinePosition;
  end;

  TJsonExtendedJsonMode = (None, StrictMode, MongoShell);

  TJsonBinaryType = (
    Generic = $00,
    &Function = $01,
    BinaryOld = $02,
    UUIDOld = $03,
    UUID = $04,
    MD5 = $05,
    UserDefined = $80
  );


  TJsonPosition = record
  Public
    ContainerType: TJsonContainerType;
    Position: Integer;
    PropertyName: string;
    HasIndex: Boolean;
    constructor Create(AType: TJsonContainerType); overload;
    procedure Clear;
    procedure WriteTo(const Sb: TStringBuilder);
    function AsString(aInitialDot : Boolean) : String;
    class function Create: TJsonPosition; overload; inline; static;
    class function FormatMessage(const aLineInfo: TJsonLineInfo; const aPath, aMsg: string): string; static;
  end;
  TJsonPositionList = specialize TList<TJsonPosition>;

  TEnumerablePositions = specialize TEnumerable<TJsonPosition>;
  TJsonPositionHelper = record helper for TJsonPosition
    class function BuildPath(const aPositions: TEnumerablePositions; aFromIndex: Integer = 0): string; static;
  end;

  TJsonFiler = class(TJsonLineInfo)
  private
    function GetPath : String;
  protected
    FStack: specialize TList<TJsonPosition>;
    FCurrentPosition: TJsonPosition;
    function GetPosition(ADepth: Integer): TJsonPosition;
    function Peek: TJsonContainerType; inline;
    function Pop: TJsonContainerType;
    procedure Push(AValue: TJsonContainerType);
    function GetInsideContainer: Boolean; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function GetPath(AFromDepth: Integer): string; overload;
    procedure Rewind; virtual;
    class function IsEndToken(aToken: TJsonToken): Boolean; static; inline;
    class function IsPrimitiveToken(aToken: TJsonToken): Boolean; static; inline;
    class function IsStartToken(aToken: TJsonToken): Boolean; static; inline;
    property InsideContainer: Boolean read GetInsideContainer;
    property Path: string read GetPath;
  end;

  EJsonException = class(Exception)
  private
    FInnerException: Exception;
  public
    constructor Create(const aMessage: string; const aInnerException: Exception); overload;
    property InnerException: Exception read FInnerException;
  end;

  TOidBytes = Array[0..OidBytesCount-1] of Byte;
  TJsonOid = record
  private
    function GetAsString: String;
    procedure SetAsString(const aValue: String);
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const aValue: TBytes);
  public
    Bytes: TOidBytes;
    constructor Create(const aOid: TOidBytes); overload;
    constructor Create(const aOid: TBytes); overload;
    constructor Create(const aOid: String); overload;
    property AsString: String read GetAsString write SetAsString;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
  end;

  TJsonCodeWScope = record
  public type
    TScopeItem = record
      Ident: String;
      Value: String;
    end;
  public
    Code: String;
    Scope: array of TScopeItem;
    constructor Create(const aCode: String; aScope: TStrings);
  end;

  TJsonDBRef = record
  private
    function GetAsString: String;
    procedure SetAsString(const aValue: String);
  public
    DB: String;
    Ref: String;
    Id: TJsonOid;
    constructor Create(const aDb, aRef, aId: String); overload;
    constructor Create(const aDb, aRef: String; const aId: TJsonOid); overload;
    constructor Create(const aRef, aId: String); overload;
    constructor Create(const aRef: String; const aId: TJsonOid); overload;
    property AsString: String read GetAsString write SetAsString;
  end;

  TJsonDecimal128 = record
  private
    function GetAsString: String;
    procedure SetAsString(const aValue: String);
    function GetAsExtended: Extended;
    procedure SetAsExtended(const aValue: Extended);
    function GetIsNan: Boolean;
    function GetIsNegInfinity: Boolean;
    function GetIsPosInfinity: Boolean;
    function GetIsZero: Boolean;
  public
    type
      TDecOidBytesCount8ToString = function (const aDec: TJsonDecimal128): string of object;
      TStringToDecOidBytesCount8 = function (const aStr: string; var ADec: TJsonDecimal128): Boolean of object;
    class var
      FDecOidBytesCount8ToString: TDecOidBytesCount8ToString;
      FStringToDecOidBytesCount8: TStringToDecOidBytesCount8;
    const
      MaxStrLen = 42;
  public
    lo, hi: UInt64;
    constructor Create(const aValue: string); overload;
    constructor Create(const aValue: Extended); overload;
    property IsNan: Boolean read GetIsNan;
    property IsPosInfinity: Boolean read GetIsPosInfinity;
    property IsNegInfinity: Boolean read GetIsNegInfinity;
    property IsZero: Boolean read GetIsZero;
    property AsString: String read GetAsString write SetAsString;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
  end;

  JsonNameAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const aValue: string);
    property Value: string read FValue;
  end;

  TJsonRegEx = record
  private
    function GetAsString: String;
    procedure SetAsString(const aValue: String);
  public
    RegEx: String;
    Options: String;
    constructor Create(const aRegEx, AOptions: String);
    property AsString: String read GetAsString write SetAsString;
  end;

  { TJsonTimestamp }

  TJsonTimestamp = record
  private
    function GetAsDateTime: TDateTime;
    function GetAsString: string;
    procedure SetAsDateTime(const aValue: TDateTime);
    procedure SetAsString(const aValue: string);
  public
    t: Integer;
    i: Integer;
    constructor Create(aTime: Integer; aInc: Integer);
    property AsString: string read GetAsString write SetAsString;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  end;

var
  JSONFormatSettings: TFormatSettings;
  JSONSerializationVersion: Integer = 36; // as defined in Delphi

implementation


{ ---------------------------------------------------------------------
  Constants
  ---------------------------------------------------------------------}


const
  SErrFormatMessagePath    = 'In %s';
  SErrFormatMessageLinePos = 'at %d:%d';
  SErrInvalidObjectId      = 'Invalid object ID';
  SErrDecimalNotAvailable  = 'No decimal implementation available';
  SErrInvalidDecimalString = 'Invalid decimal string value';

const
  HighOidBytesCount8Zero      = $3040000000000000;
  HighOidBytesCount8PosInfity = $7800000000000000;
  HighOidBytesCount8NegInfity = QWord($F800000000000000);
  HighOidBytesCount8Nan       = $7C00000000000000;


{ ---------------------------------------------------------------------
  EJsonException
  ---------------------------------------------------------------------}

constructor EJsonException.Create(const aMessage: string; const aInnerException: Exception);

begin
  Create(aMessage);
  FInnerException:=aInnerException;
end;


{ ---------------------------------------------------------------------
  TJsonCodeWScope
  ---------------------------------------------------------------------}

constructor TJsonCodeWScope.Create(const aCode: String; aScope: TStrings);

var
  lLen,I: Integer;

begin
  Code:=aCode;
  lLen:=0;
  if (aScope<>nil) then
    lLen:=aScope.Count;
  SetLength(Self.Scope,lLen);
  for I:=0 to lLen-1 do
    With Self.Scope[i] do
      begin
      Ident:=aScope.Names[I];
      Value:=aScope.ValueFromIndex[I];
      end;
end;

{ ---------------------------------------------------------------------
  TJsonDBRef
  ---------------------------------------------------------------------}

constructor TJsonDBRef.Create(const aDb,aRef,aId: String);
begin
  DB:=aDb;
  Ref:=aRef;
  Id.AsString:=aId;
end;

constructor TJsonDBRef.Create(const aDb, aRef: String; const aId: TJsonOid);

begin
  DB:=aDb;
  Ref:=aRef;
  Id:=aId;
end;


constructor TJsonDBRef.Create(const aRef, AId: String);

begin
  Self:=Create('',aRef,AId);
end;


constructor TJsonDBRef.Create(const aRef: String; const aId: TJsonOid);

begin
  Self:=Create('',ARef,AId);
end;


function TJsonDBRef.GetAsString: String;

var
  S : String;

begin
  S:=DB;
  if S<>'' then
    S:=S+'.';
  S:=S+Ref+'.'+Id.AsString;
  Result:=S;
end;


procedure TJsonDBRef.SetAsString(const aValue: String);

var
  lParts: TStringArray;

begin
  lParts:=SplitString(AValue,'.');
  if Length(lParts)<2 then
    Raise EJsonException.CreateFmt('Invalid DB ref: %s',[aValue]);
  if Length(lParts)=2 then
    begin
    DB:='';
    Ref:=lParts[0];
    Id.AsString:=lParts[1];
    end
  else if Length(lParts)=3 then
    begin
    DB:=lParts[0];
    Ref:=lParts[1];
    Id.AsString:=lParts[2];
    end;
end;

{ ---------------------------------------------------------------------
  TJsonDecimal128
  ---------------------------------------------------------------------}

constructor TJsonDecimal128.Create(const aValue: Extended);

begin
  AsExtended:=AValue;
end;


constructor TJsonDecimal128.Create(const aValue: string);

begin
  AsString:=AValue;
end;


function TJsonDecimal128.GetAsString: String;

begin
  if Assigned(FDecOidBytesCount8ToString) then
    Result:=FDecOidBytesCount8ToString(Self)
  else
    raise EJsonException.Create(SErrDecimalNotAvailable);
end;


procedure TJsonDecimal128.SetAsString(const aValue: String);

begin
  if not Assigned(FStringToDecOidBytesCount8) then
    raise EJsonException.Create(SErrDecimalNotAvailable);
  if aValue='' then
    begin
    lo:=0;
    Hi:=HighOidBytesCount8Zero
    end
  else if not FStringToDecOidBytesCount8(aValue, Self) then
    raise EJsonException.CreateFmt(SErrInvalidDecimalString, [aValue]);
end;


function TJsonDecimal128.GetAsExtended: Extended;

begin
  if IsNan then
    Result:=Extended.NaN
  else if IsNegInfinity then
    Result:=Extended.NegativeInfinity
  else if IsPosInfinity then
    Result:=Extended.PositiveInfinity
  else if IsZero then
    Result:=0.0
  else if not TryStrToFloat(AsString,Result,JSONFormatSettings) then
    Result:=Extended.NaN;
end;


procedure TJsonDecimal128.SetAsExtended(const aValue: Extended);

begin
  AsString:=FloatToStr(aValue, JSONFormatSettings);
end;


function TJsonDecimal128.GetIsNan: Boolean;

begin
  Result:=(lo=0) and (hi=HighOidBytesCount8Nan);
end;


function TJsonDecimal128.GetIsNegInfinity: Boolean;

begin
  Result:=(lo=0) and (hi=HighOidBytesCount8NegInfity);
end;


function TJsonDecimal128.GetIsPosInfinity: Boolean;

begin
  Result:=(lo=0) and (hi=HighOidBytesCount8PosInfity);
end;


function TJsonDecimal128.GetIsZero: Boolean;

begin
  Result:=(lo=0) and (hi=HighOidBytesCount8Zero);
end;


{ ---------------------------------------------------------------------
  TJSONFiler
  ---------------------------------------------------------------------}

constructor TJsonFiler.Create;

begin
  inherited Create;
  FStack:=Specialize TList<TJsonPosition>.Create;
  FStack.Capacity:=10;
  FCurrentPosition.Clear;
end;


destructor TJsonFiler.Destroy;

begin
  FStack.Free;
  inherited Destroy;
end;


function TJsonFiler.GetPath: string;

begin
  Result:=GetPath(0);
end;


function TJsonFiler.GetPath(aFromDepth: Integer): string;

var
  I: Integer;

begin
  Result:='';
  if FCurrentPosition.ContainerType=TJsonContainerType.None then
    Exit;
  if AFromDepth < 0 then
    AFromDepth:=0;
  for I:=AFromDepth to FStack.Count - 1 do
    Result:=Result+FStack[I].AsString(Result<>'');
  if InsideContainer and (AFromDepth <= FStack.Count) then
    Result:=Result+FCurrentPosition.AsString(Result<>'');
end;

function TJsonFiler.GetPosition(aDepth: Integer): TJsonPosition;

begin
  if aDepth<FStack.Count then
    Result:=FStack.List[aDepth]
  else
    Result:=FCurrentPosition;
end;


function TJsonFiler.Peek: TJsonContainerType;

begin
  Result:=FCurrentPosition.ContainerType;
end;


function TJsonFiler.Pop: TJsonContainerType;

begin
  Result:=FCurrentPosition.ContainerType;
  if FStack.Count > 0 then
    begin
    FCurrentPosition:=FStack.List[FStack.Count - 1];
    FStack.Delete(FStack.Count - 1);
    end
  else
    begin
    FCurrentPosition.Clear;
    end;
end;

procedure TJsonFiler.Push(AValue: TJsonContainerType);

begin
  if FCurrentPosition.ContainerType <> TJsonContainerType.None then
    FStack.Add(FCurrentPosition);
  FCurrentPosition.Create(AValue);
end;


procedure TJsonFiler.Rewind;

begin
  FStack.Clear;
  FCurrentPosition.Clear;
end;

class function TJsonFiler.IsPrimitiveToken(aToken: TJsonToken): Boolean;

begin
  Result:=aToken in JSONPrimitiveTokens;
end;


class function TJsonFiler.IsStartToken(aToken: TJsonToken): Boolean;

begin
  Result:=aToken in JSONStartTokens;
end;


class function TJsonFiler.IsEndToken(aToken: TJsonToken): Boolean;

begin
  Result:=aToken in JSONEndTokens
end;


{ ---------------------------------------------------------------------
  TJsonLineInfo
  ---------------------------------------------------------------------}


function TJsonLineInfo.GetLineNumber: Integer;

begin
  Result:=0;
end;


function TJsonLineInfo.GetLinePosition: Integer;

begin
  Result:=0;
end;


function TJsonLineInfo.HasLineInfo: Boolean;

begin
  Result:=False;
end;


{ ---------------------------------------------------------------------
  JsonNameAttribute
  ---------------------------------------------------------------------}

constructor JsonNameAttribute.Create(const aValue: string);

begin
  inherited Create;
  FValue:=aValue;
end;


{ ---------------------------------------------------------------------
  TJsonOid
  ---------------------------------------------------------------------}

constructor TJsonOid.Create(const aOid: TOidBytes); overload;

begin
  Bytes:=aOid;
end;

constructor TJsonOid.Create(const aOid: TBytes);
begin
  AsBytes:=AOid;
end;

constructor TJsonOid.Create(const aOid: String);
begin
  AsString:=AOid;
end;

function TJsonOid.GetAsBytes: TBytes;
begin
  SetLength(Result,OidBytesCount);
  Move(bytes[0],Result[0],OidBytesCount);
end;

procedure TJsonOid.SetAsBytes(const aValue: TBytes);
begin
  Case  Length(aValue) of
    0 : Bytes:=Default(TOidBytes);
    OidBytesCount: Move(aValue[0],Bytes[0],OidBytesCount)
  else
    raise EJsonException.Create(SErrInvalidObjectId);
  end;
end;

function TJsonOid.GetAsString: String;
var
  LBytes, LText: TBytes;
begin
  LBytes:=AsBytes;
  SetLength(LText,Length(LBytes)*2);
  BinToHex(LBytes,0,LText,0,OidBytesCount);
  Result:=TEncoding.ANSI.GetString(LText);
end;

procedure TJsonOid.SetAsString(const aValue: String);
var
  LText, LBytes: TBytes;
begin
  LText:=BytesOf(aValue);
  SetLength(LBytes,Length(LText) div 2);
  HexToBin(LText,0,LBytes,0,Length(LBytes));
  SetAsBytes(LBytes);
end;


{ ---------------------------------------------------------------------
  TJsonPosition
  ---------------------------------------------------------------------}


constructor TJsonPosition.Create(aType: TJsonContainerType);

begin
  ContainerType:=Atype;
  Position:=-1;
  PropertyName:='';
  HasIndex:=(atype=TJsonContainerType.&Array) or (atype=TJsonContainerType.&Constructor);
end;

class function TJsonPosition.Create: TJsonPosition;
begin
  Result.Clear;
end;

procedure TJsonPosition.Clear;
begin
  ContainerType:=TJsonContainerType.None;
  HasIndex:=False;
  Position:=-1;
  PropertyName:='';
end;

class function TJsonPosition.FormatMessage(const aLineInfo: TJsonLineInfo; const aPath, aMsg: string): string;
var
  S : String;
begin
  if aMsg.EndsWith(sLineBreak) then
    S:=aMsg
  else
    begin
    S:=Trim(aMsg);
    if not S.EndsWith('.') then
      S:=S+'. '
    else
      S:=S+' ';
    end;
  Result:=S+Format(SErrFormatMessagePath,[aPath]);
  if Assigned(aLineInfo) then
    With aLineInfo do
      if HasLineInfo then
        Result:=Result+Format(SErrFormatMessageLinePos,[LineNumber, LinePosition]);
end;

function TJsonPosition.AsString(aInitialDot : Boolean) : String;

begin
  Result:='';
  case ContainerType of
    TJsonContainerType.&Object:
      begin
      Result:=PropertyName;
      if aInitialDot then
        Result:='.'+Result;
      end;
    TJsonContainerType.&Array,
    TJsonContainerType.&Constructor:
      Result:='['+IntToStr(Position)+']';
  end;
end;

procedure TJsonPosition.WriteTo(const Sb: TStringBuilder);
var
  S : String;
begin
  S:=AsString((ContainerType=TJsonContainerType.&Object) and (Sb.Length>0));
  if (ContainerType<>TJsonContainerType.None) then
    Sb.Append(S);
end;

{ ---------------------------------------------------------------------
  TJsonPositionHelper
  ---------------------------------------------------------------------}

class function TJsonPositionHelper.BuildPath(const aPositions: TEnumerablePositions; aFromIndex: Integer=0): string;

var
  P : TJsonPosition;
  I : Integer;

begin
  Result:='';
  if AFromIndex<0 then
    AFromIndex:=0;
  i:=0;
  for P in aPositions do
    begin
    if I>=aFromIndex then
      Result:=Result+P.AsString(Result<>'');
    inc(i);
    end;
end;

{ ---------------------------------------------------------------------
  TJsonRegEx
  ---------------------------------------------------------------------}


constructor TJsonRegEx.Create(const aRegEx, aOptions: String);
begin
  RegEx:=aRegEx;
  Options:=aOptions;
end;

function TJsonRegEx.GetAsString: String;
begin
  Result:='/'+RegEx+'/'+Options;
end;

procedure TJsonRegEx.SetAsString(const aValue: String);
var
  lParts: TStringArray;
begin
  RegEx:='';
  Options:='';
  lParts:=SplitString(aValue,'/');
  case Length(lParts) of
  1 :  RegEx:=lParts[0];
  2 :  RegEx:=lParts[1];
  3 :
    begin
    RegEx:=lParts[1];
    Options:=lParts[2];
    end;
  end;
end;

{ TJsonTimestamp }

function TJsonTimestamp.GetAsDateTime: TDateTime;
begin
  Result:=UnixToDateTime(t,True);
end;

function TJsonTimestamp.GetAsString: string;
begin
  Result:=DateToISO8601(GetAsDateTime,True);
  if i<>0 then
    Result:=Result+','+IntToStr(i);
end;

procedure TJsonTimestamp.SetAsDateTime(const aValue: TDateTime);
begin
  t:=DateTimeToUnix(aValue,True);
  i:=0;
end;

procedure TJsonTimestamp.SetAsString(const aValue: string);
var
  lTime,lInc : String;
begin
  lTime:=ExtractWord(1,aValue,[',']);
  t:=DateTimeToUnix(ISO8601ToDate(lTime,True),True);
  if WordCount(aValue,[','])<>2 then
    I:=0
  else
    begin
    lInc:=ExtractWord(2,aValue,[',']);
    I:=StrToInt(lInc);
    end;
end;

constructor TJsonTimestamp.Create(aTime: Integer; aInc: Integer);
begin
  t:=aTime;
  i:=aInc;
end;

initialization
  JSONFormatSettings:=TFormatSettings.Invariant;
end.
