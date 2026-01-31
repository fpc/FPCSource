{
    This file is part of the Free Component Library
    Copyright (c) 2026 by Michael Van Canneyt michael@freepascal.org

    Delphi-compatible JSON writer unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System.JSON.Writers;

{$mode objfpc}
{$scopedenums on}
{$modeswitch typehelpers}
{$h+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,  System.Rtti, System.Classes, Fcl.Streams.Extra,
  {$ELSE}
  SysUtils,  Rtti, Classes, StreamEx,
  {$ENDIF}
  System.JSON, System.JSON.Types, System.JSON.Readers;

type
  TJsonWriteState = (Error, Closed, &Object, &Array, &Constructor, &Property, Start);

  { TJsonWriter }

  TJsonWriter = class(TJsonFiler)
  protected type
    TState = (Start, &Property, ObjectStart, &Object, ArrayStart, &Array, ConstructorStart, &Constructor, Closed, Error);

    { TStateHelper }

    TStateHelper = type helper for TState
      Function ToString : string;
    end;

    TStateTransition = Array[TState] of TState;
  const
    StateArrayTemplate: array[0..7] of TStateTransition = (
    // The rows correspond to the first 7 values of TJSONToken and the 8th is a value (None, StartObject, StartArray, StartConstructor, PropertyName, Comment, Raw, Value).
    // The columns correspond to the new state for the current state
    //                            // Start,                &Property,                ObjectStart,          &Object,           ArrayStart,             &Array,                  ConstructorStart,          &Constructor,             Closed,         Error);
    {TJSONToken.None }            (TState.Error           , TState.Error           , TState.Error        , TState.Error     , TState.Error           , TState.Error            , TState.Error            , TState.Error            , TState.Error , TState.Error),
    {TJSONToken.StartObject}      (TState.ObjectStart     , TState.ObjectStart     , TState.Error        , TState.Error     , TState.ObjectStart     , TState.ObjectStart      , TState.ObjectStart      , TState.ObjectStart      , TState.Error , TState.Error),
    {TJSONToken.StartArray}       (TState.ArrayStart      , TState.ArrayStart      , TState.Error        , TState.Error     , TState.ArrayStart      , TState.ArrayStart       , TState.ArrayStart       , TState.ArrayStart       , TState.Error , TState.Error),
    {TJSONToken.StartConstructor} (TState.ConstructorStart, TState.ConstructorStart, TState.Error        , TState.Error     , TState.ConstructorStart, TState.ConstructorStart , TState.ConstructorStart , TState.ConstructorStart , TState.Error , TState.Error),
    {TJSONToken.PropertyName}     (TState.&Property       , TState.Error           , TState.&Property    , TState.&Property , TState.Error           , TState.Error            , TState.Error            , TState.Error            , TState.Error , TState.Error),
    {TJSONToken.Comment}          (TState.Start           , TState.&Property       , TState.ObjectStart  , TState.&Object   , TState.ArrayStart      , TState.&Array           , TState.&Constructor     , TState.&Constructor     , TState.Error , TState.Error),
    {TJSONToken.Raw}              (TState.Start           , TState.&Property       , TState.ObjectStart  , TState.&Object   , TState.ArrayStart      , TState.&Array           , TState.&Constructor     , TState.&Constructor     , TState.Error , TState.Error),
    {values}                      (TState.Start           , TState.&Object         , TState.Error        , TState.Error     , TState.&Array          , TState.&Array           , TState.&Constructor     , TState.&Constructor     , TState.Error , TState.Error)
    );
  private
    FCloseOutput: Boolean;
    FDateTimeZoneHandling: TJsonDateTimeZoneHandling;
    FEmptyValueHandling: TJsonEmptyValueHandling;
    FContainers : Array of TJsonContainerType;
    FContainerCount : Integer;
    FPopping: Boolean;
    Procedure PushContainer(aType : TJsonContainerType);
    Procedure PopContainer(aType : TJsonContainerType);
    // New state for each current state.
    class var StateArray : array[TJSONToken] of TStateTransition;
    function GetContainerPath: string;
    function GetWriteState: TJsonWriteState;
    class procedure BuildStateArray;
  protected
    FCurrentState: TState;
    procedure AutoComplete(TokenBeingWritten: TJsonToken);
    function GetInsideContainer: Boolean; override;
    function GetTop: Integer;
    function GetTopContainer: TJsonContainerType;
    procedure UpdateScopeWithFinishedValue;
    procedure InternalWriteEnd(Container: TJsonContainerType); virtual;
    procedure InternalWritePropertyName(const Name: string);
    procedure InternalWriteStart(Token: TJsonToken; Container: TJsonContainerType);
    procedure InternalWriteValue(Token: TJsonToken);
    procedure InternalWriteComment;
    procedure SetWriteState(Token: TJsonToken; const Value: TValue);
    procedure WriteEnd(ContainerType: TJsonContainerType); overload;
    procedure WriteEnd(const Token: TJsonToken); overload; virtual;
    procedure WriteToken(const aReader: TJsonReader; aWriteChildren, aWriteDateConstructorAsDate: Boolean); overload;
    procedure WriteToken(const aReader: TJsonReader; aInitialDepth: Integer; aWriteChildren, aWriteDateConstructorAsDate: Boolean); overload;
    procedure WriteDateConstructor(const aReader: TJsonReader);
    class procedure WriteValue(const Writer: TJsonWriter; const Value: TValue); overload;

    procedure OnBeforeWriteToken(TokenBeginWriten: TJsonToken); virtual;

  public
    class constructor Init;
    constructor Create;
    destructor Destroy; override;
    procedure Rewind; override;
    procedure Close; virtual;
    procedure Flush; virtual;
    procedure WriteComment(const Comment: string); virtual;
    procedure WriteStartObject; virtual;
    procedure WriteEndObject; virtual;
    procedure WriteStartArray; virtual;
    procedure WriteEndArray; virtual;
    procedure WriteStartConstructor(const Name: string); virtual;
    procedure WriteEndConstructor; virtual;
    procedure WritePropertyName(const Name: string); overload; virtual;
    procedure WriteEnd; overload; virtual;
    procedure WriteNull; virtual;
    procedure WriteRaw(const Json: string); virtual;
    procedure WriteRawValue(const Json: string); virtual;
    procedure WriteUndefined; virtual;
    procedure WriteToken(const Reader: TJsonReader); overload;
    procedure WriteToken(const Reader: TJsonReader; WriteChildren: Boolean); overload;
    procedure WriteValue(const Value: string); overload; virtual;
    procedure WriteValue(const Value: PAnsiChar); overload;
    procedure WriteValue(const Value: PWideChar); overload;
    procedure WriteValue(Value: Integer); overload; virtual;
    procedure WriteValue(Value: UInt32); overload; virtual;
    procedure WriteValue(Value: Int64); overload; virtual;
    procedure WriteValue(Value: UInt64); overload; virtual;
    procedure WriteValue(Value: Single); overload; virtual;
    procedure WriteValue(Value: Double); overload; virtual;
    procedure WriteValue(Value: Extended); overload; virtual;
    procedure WriteValue(Value: Boolean); overload; virtual;
    procedure WriteValue(Value: Char); overload; virtual;
    procedure WriteValue(Value: Byte); overload; virtual;
    procedure WriteValue(Value: TDateTime); overload; virtual;
    procedure WriteValue(const Value: TGUID); overload; virtual;
    procedure WriteValue(const Value: TBytes; BinaryType: TJsonBinaryType = TJsonBinaryType.Generic); overload; virtual;
    procedure WriteValue(const Value: TJsonOid); overload; virtual;
    procedure WriteValue(const Value: TJsonRegEx); overload; virtual;
    procedure WriteValue(const Value: TJsonDBRef); overload; virtual;
    procedure WriteValue(const Value: TJsonCodeWScope); overload; virtual;
    procedure WriteMinKey; overload; virtual;
    procedure WriteMaxKey; overload; virtual;
    procedure WriteValue(const Value: TJsonDecimal128); overload; virtual;
    procedure WriteValue(const Value: TJsonTimestamp); overload; virtual;
    procedure WriteValue(const Value: TValue); overload; virtual;
    property CloseOutput: Boolean read FCloseOutput write FCloseOutput;
    property ContainerPath: string read GetContainerPath;
    property Top: Integer read GetTop;
    property TopContainer: TJsonContainerType read GetTopContainer;
    property WriteState: TJsonWriteState read GetWriteState;
    property EmptyValueHandling: TJsonEmptyValueHandling read FEmptyValueHandling write FEmptyValueHandling;
    property DateTimeZoneHandling: TJsonDateTimeZoneHandling read FDateTimeZoneHandling write FDateTimeZoneHandling;
  end;

  { EJsonWriterException }

  EJsonWriterException = class (EJsonException)
  private
    FPath: string;
  public
    constructor Create(const Msg: string; const Ex: Exception; const APath: string); overload;
    constructor Create(const Writer: TJsonWriter; const Msg: string; const Ex: Exception = nil); overload;
    constructor CreateFmt(const Writer: TJsonWriter; const Fmt: string; const args : array of const; const Ex: Exception = nil); overload;
    constructor Create(const APath, Msg: string; const Ex: Exception); overload;
    property Path: string read FPath;
  end;

  { TASCIIStreamWriter }
  // This is a useless class, all relevant methods are in the base class...
  TASCIIStreamWriter = class(TStreamWriter)
  public
    constructor Create(Stream: TStream; BufferSize: Integer = 4096); overload;
    constructor Create(const Filename: string; Append: Boolean; BufferSize: Integer = 4096); overload;
  end;

  { TJsonTextWriter }

  TJsonTextWriter = class(TJsonWriter)
  private
    FDateFormatHandling: TJsonDateFormatHandling;
    FExtendedJsonMode: TJsonExtendedJsonMode;
    FFloatFormatHandling: TJsonFloatFormatHandling;
    FFormatSettings: TFormatSettings;
    FFormatting: TJsonFormatting;
    FIndentation: Integer;
    FIndentChar: Char;
    FQuoteChar: Char;
    FQuoteName: Boolean;
    FStringEscapeHandling: TJsonStringEscapeHandling;
    FWriter: TTextWriter;
    FOwnsWriter: Boolean;
    function DoQuote(const aString: String): String;
    procedure SetIndentation(aValue: Integer);
    procedure SetQuoteChar(aValue: Char);
    procedure SetStringEscapeHandling(aValue: TJsonStringEscapeHandling);
  protected
    procedure WriteIndent;
    procedure WriteValueDelimiter; inline;
    procedure WriteIndentSpace; inline;
    procedure WriteEnd(const aToken: TJsonToken); override;
    procedure InternalWriteEnd(aContainer: TJsonContainerType); override;
    procedure OnBeforeWriteToken(aToken: TJsonToken); override;
    function EscapeJsonString(const aValue: string): string;
  public
    constructor Create(const aTextWriter: TTextWriter; aOwnsWriter: Boolean); overload;
    constructor Create(const aTextWriter: TTextWriter); overload;
    constructor Create(const aStream: TStream); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure Flush; override;
    procedure WriteComment(const aComment: string); override;
    procedure WriteNull; override;
    procedure WritePropertyName(const aName: string); overload; override;
    procedure WritePropertyName(const aName: string; aEscape: Boolean); overload;
    procedure WriteRaw(const aJson: string); override;
    procedure WriteStartConstructor(const aName: string); override;
    procedure WriteStartObject; override;
    procedure WriteStartArray; override;
    procedure WriteValue(const aValue: string); override;
    procedure WriteValue(aValue: Integer); override;
    procedure WriteValue(aValue: UInt32); override;
    procedure WriteValue(aValue: Int64); override;
    procedure WriteValue(aValue: UInt64); override;
    procedure WriteValue(aValue: Single); override;
    procedure WriteValue(aValue: Double); override;
    procedure WriteValue(aValue: Extended); override;
    procedure WriteValue(aValue: Boolean); override;
    procedure WriteValue(aValue: Char); override;
    procedure WriteValue(aValue: Byte); override;
    procedure WriteValue(aValue: TDateTime); override;
    procedure WriteValue(const aValue: TGUID); override;
    procedure WriteValue(const aValue: TBytes; aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic); override;
    procedure WriteValue(const aValue: TJsonOid); override;
    procedure WriteValue(const aValue: TJsonRegEx); override;
    procedure WriteValue(const aValue: TJsonDBRef); override;
    procedure WriteValue(const aValue: TJsonCodeWScope); override;
    procedure WriteMinKey; override;
    procedure WriteMaxKey; override;
    procedure WriteValue(const aValue: TJsonDecimal128); override;
    procedure WriteValue(const aValue: TJsonTimestamp); override;
    procedure WriteValue(const aValue: TValue); override;
    procedure WriteUndefined; override;
    procedure WriteWhitespace(const aWhiteSpace: string);
    property Writer: TTextWriter read FWriter;
    property Indentation: Integer read FIndentation write SetIndentation;
    property IndentChar: Char read FIndentChar write FIndentChar;
    property QuoteChar: Char read FQuoteChar write SetQuoteChar;
    property QuoteName: Boolean read FQuoteName write FQuoteName;
    property Formatting: TJsonFormatting read FFormatting write FFormatting;
    property FormatSettings: TFormatSettings read FFormatSettings write FFormatSettings;
    property StringEscapeHandling: TJsonStringEscapeHandling read FStringEscapeHandling write SetStringEscapeHandling;
    property DateFormatHandling: TJsonDateFormatHandling read FDateFormatHandling write FDateFormatHandling;
    property FloatFormatHandling: TJsonFloatFormatHandling read FFloatFormatHandling write FFloatFormatHandling;
    property ExtendedJsonMode: TJsonExtendedJsonMode read FExtendedJsonMode write FExtendedJsonMode;
  end;

  { TJsonObjectWriter }

  TJsonObjectWriter = class(TJsonWriter)
  private
    FDateFormatHandling: TJsonDateFormatHandling;
    FOwnValue: Boolean;
    FRoot: TJSONAncestor;
    FContainerStack: TList;
    FCurrentPropertyName: string;
    function GetContainer: TJSONAncestor;
    procedure SetContainer(AValue: TJSONAncestor);
    function GetCurrentContainer: TJSONAncestor;
    procedure AddValueToContainer(AValue: TJSONValue);
    procedure PushContainer(AContainer: TJSONAncestor);
    function PopContainer: TJSONAncestor;
  public
    constructor Create(OwnValue: Boolean = True);
    destructor Destroy; override;
    procedure Rewind; override;
    procedure WriteNull; override;
    procedure WritePropertyName(const Name: string); overload; override;
    procedure WriteStartConstructor(const Name: string); override;
    procedure WriteStartObject; override;
    procedure WriteEndObject; override;
    procedure WriteStartArray; override;
    procedure WriteEndArray; override;
    procedure WriteRaw(const Json: string); override;
    procedure WriteRawValue(const Json: string); override;
    procedure WriteValue(const aValue: string); override;
    procedure WriteValue(aValue: Integer); override;
    procedure WriteValue(aValue: UInt32); override;
    procedure WriteValue(aValue: Int64); override;
    procedure WriteValue(aValue: UInt64); override;
    procedure WriteValue(aValue: Single); override;
    procedure WriteValue(aValue: Double); override;
    procedure WriteValue(aValue: Extended); override;
    procedure WriteValue(aValue: Boolean); override;
    procedure WriteValue(aValue: Char); override;
    procedure WriteValue(aValue: Byte); override;
    procedure WriteValue(aValue: TDateTime); override;
    procedure WriteValue(const aValue: TGUID); override;
    procedure WriteValue(const aValue: TBytes; aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic); override;
    procedure WriteValue(const aValue: TJsonOid); override;
    procedure WriteValue(const aValue: TJsonRegEx); override;
    procedure WriteValue(const aValue: TJsonDBRef); override;
    procedure WriteValue(const aValue: TJsonCodeWScope); override;
    procedure WriteMinKey; override;
    procedure WriteMaxKey; override;
    procedure WriteValue(const aValue: TJsonDecimal128); override;
    procedure WriteValue(const Value: TJsonTimestamp); override;
    procedure WriteUndefined; override;
    property JSON: TJSONAncestor read FRoot;
    property Container: TJSONAncestor read GetContainer write SetContainer;
    property DateFormatHandling: TJsonDateFormatHandling read FDateFormatHandling write FDateFormatHandling;
    property OwnValue: Boolean read FOwnValue write FOwnValue;
  end;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.TypInfo, System.DateUtils, System.Math,
  {$ELSE}
  TypInfo, DateUtils, Math,
  {$ENDIF}
  System.NetEncoding, System.JSONConsts, System.JSON.Utils;

{ TJsonWriter }

procedure TJsonWriter.PushContainer(aType: TJsonContainerType);
begin
  if FContainerCount=Length(FContainers) then
    SetLength(FContainers,FContainerCount+10);
  FContainers[FContainerCount]:=aType;
  Inc(FContainerCount);
end;

procedure TJsonWriter.PopContainer(aType: TJsonContainerType);
var
  lCount : Integer;
begin
  // Avoid writing twice
  if FPopping then
    exit;
  FPopping:=True;
  try
    lCount:=FContainerCount;
    While (lCount>0) and (FContainers[FContainerCount-1]<>aType) do
      Dec(lCount);
    if lCount=0 then
      Raise EJsonWriterException.Create('Cannot pop container: not in container');
    While FContainerCount>=lCount do
      begin
      if FContainerCount=1 then
        FCurrentState := TState.&Start
      else
        case FContainers[FContainerCount-2] of
          TJsonContainerType.&Object:
            FCurrentState := TState.&Object;
          TJsonContainerType.&Array:
            FCurrentState := TState.&Array;
          TJsonContainerType.&Constructor:
            FCurrentState := TState.&Array;
          TJsonContainerType.None:
            FCurrentState := TState.Start;
        end;
      Dec(FContainerCount);
      end;
  finally
    FPopping:=False;
  end;
end;

function TJsonWriter.GetContainerPath: string;
begin
  Result := Path;
end;

function TJsonWriter.GetWriteState: TJsonWriteState;
begin
  case FCurrentState of
    TState.Start: Result := TJsonWriteState.Start;
    TState.&Property: Result := TJsonWriteState.&Property;
    TState.ObjectStart, TState.&Object: Result := TJsonWriteState.&Object;
    TState.ArrayStart, TState.&Array: Result := TJsonWriteState.&Array;
    TState.ConstructorStart, TState.&Constructor: Result := TJsonWriteState.&Constructor;
    TState.Closed: Result := TJsonWriteState.Closed;
    TState.Error: Result := TJsonWriteState.Error;
  else
    Result := TJsonWriteState.Error;
  end;
end;

class procedure TJsonWriter.BuildStateArray;
const
  ValueTokens = [TJsonToken.Integer, TJsonToken.Float, TJsonToken.&String, TJsonToken.Boolean,
                 TJsonToken.Null, TJsonToken.Undefined, TJsonToken.Date, TJsonToken.Bytes, TJsonToken.Oid,
                 TJsonToken.RegEx, TJsonToken.DBRef, TJsonToken.CodeWScope, TJsonToken.MinKey, TJsonToken.MaxKey,
                 TJsonToken.Decimal, TJsonToken.Timestamp];
var
  Token : TJsonToken;

begin
  for Token := Low(TJsonToken) to High(TJsonToken) do
    begin
    if Token in ValueTokens then
      StateArray[Token]:=StateArrayTemplate[7]
    else if Ord(Token)<=Ord(High(TState)) then
      StateArray[Token]:=StateArrayTemplate[Ord(Token)]
    else
      StateArray[Token]:=StateArrayTemplate[0]
    end;
end;

class function TokenToContainerType(aToken : TJsonToken) : TJsonContainerType;
begin
  case aToken of
    TJsonToken.StartObject,
    TJsonToken.EndObject : Result:=TJsonContainerType.&Object;
    TJsonToken.StartArray,
    TJsonToken.EndArray : Result:=TJsonContainerType.&Array;
    TJsonToken.StartConstructor,
    TJsonToken.EndConstructor : Result:=TJsonContainerType.&Constructor;
  else
    Result:=TJsonContainerType.None;
  end;
end;

procedure TJsonWriter.AutoComplete(TokenBeingWritten: TJsonToken);
const
  closetokens = [TJsonToken.EndObject,TJsonToken.EndArray,TJsonToken.EndConstructor];
var
  lOldState,lNewState: TState;
  lTransition : TStateTransition;
  lContainer : TJSONContainerType;
begin
  if FCurrentState = TState.Error then
    Exit;
  OnBeforeWriteToken(TokenBeingWritten);
  lContainer:=TokenToContainerType(TokenBeingWritten);
  if TokenBeingWritten in CloseTokens then
    begin
    PopContainer(lContainer);
    end
  else
    begin
    lTransition:=StateArray[TokenBeingWritten];
    lOldState := FCurrentState;
    lNewState := lTransition[lOldState];
    if lNewState = TState.Error then
      begin
      FCurrentState := TState.Error;
      raise EJsonWriterException.Create(Self, Format('Invalid JSON token "%s" in state %s', [TokenBeingWritten.ToString, lOldState.ToString]));
      end;
    if lContainer<>TJsonContainerType.None then
      PushContainer(lContainer);
    FCurrentState := lNewState;
    end;
end;

function TJsonWriter.GetInsideContainer: Boolean;
begin
  Result := FCurrentState in [TState.&Array, TState.&Object, TState.&Constructor];
end;

function TJsonWriter.GetTop: Integer;
begin
  Result := FStack.Count;
end;

function TJsonWriter.GetTopContainer: TJsonContainerType;
begin
  if FCurrentPosition.ContainerType = TJsonContainerType.None then
    Result := TJsonContainerType.None
  else
    Result := FCurrentPosition.ContainerType;
end;

procedure TJsonWriter.UpdateScopeWithFinishedValue;
begin
  if FCurrentPosition.HasIndex or (FCurrentPosition.ContainerType = TJsonContainerType.&Object) then
    Inc(FCurrentPosition.Position);
end;

procedure TJsonWriter.InternalWriteEnd(Container: TJsonContainerType);
begin
  case Container of
    TJsonContainerType.&Object: AutoComplete(TJsonToken.EndObject);
    TJsonContainerType.&Array: AutoComplete(TJsonToken.EndArray);
    TJsonContainerType.&Constructor: AutoComplete(TJsonToken.EndConstructor);
  end;
  Pop;
  UpdateScopeWithFinishedValue; // Update parent scope after completing this container
end;

procedure TJsonWriter.InternalWritePropertyName(const Name: string);
begin
  FCurrentPosition.PropertyName := Name;
  AutoComplete(TJsonToken.PropertyName);
end;

procedure TJsonWriter.InternalWriteStart(Token: TJsonToken; Container: TJsonContainerType);
begin
  AutoComplete(Token);
  Push(Container);
end;

procedure TJsonWriter.InternalWriteValue(Token: TJsonToken);
begin
  AutoComplete(Token);
  UpdateScopeWithFinishedValue;
end;

procedure TJsonWriter.InternalWriteComment;
begin
  AutoComplete(TJsonToken.Comment);
end;

procedure TJsonWriter.SetWriteState(Token: TJsonToken; const Value: TValue);
begin
  // This method can be used to set internal state based on token and value
  // Default implementation does nothing
end;

procedure TJsonWriter.WriteEnd(ContainerType: TJsonContainerType);
begin
  case ContainerType of
    TJsonContainerType.None: ;
    TJsonContainerType.&Object: WriteEndObject;
    TJsonContainerType.&Array: WriteEndArray;
    TJsonContainerType.&Constructor: WriteEndConstructor;
  end;
end;

procedure TJsonWriter.WriteEnd(const Token: TJsonToken);
begin
  case Token of
    TJsonToken.EndObject: WriteEndObject;
    TJsonToken.EndArray: WriteEndArray;
    TJsonToken.EndConstructor: WriteEndConstructor;
  else
     ;
  end;
end;

procedure TJsonWriter.WriteToken(const aReader: TJsonReader; aWriteChildren, aWriteDateConstructorAsDate: Boolean);
begin
  WriteToken(aReader, 0, aWriteChildren, aWriteDateConstructorAsDate);
end;


procedure TJsonWriter.WriteDateConstructor(const aReader: TJsonReader);

var
  lDate: TDateTime;
  lTimeStamp: Int64;
begin
  if not aReader.Read then
    raise EJsonWriterException.Create(Self,SUnexpectedEndConstructorDate);
  if aReader.TokenType <> TJsonToken.Integer then
    raise EJsonWriterException.CreateFmt(Self, SUnexpectedTokenDateConstructorExpInt, [aReader.TokenType.ToString]);
  lTimeStamp := aReader.Value.AsInt64;
  lDate := IncMilliSecond(621355968000000000 + (lTimeStamp * 1000));
  if not aReader.Read then
    raise EJsonWriterException.Create(Self, SUnexpectedEndConstructorDate);
  if aReader.TokenType <> TJsonToken.EndConstructor then
    raise EJsonWriterException.CreateFmt(Self,SUnexpectedTokenDateConstructorExpEnd, [aReader.TokenType.ToString]);
  WriteValue(lDate);
end;

procedure TJsonWriter.WriteToken(const aReader: TJsonReader; aInitialDepth: Integer; aWriteChildren,
  aWriteDateConstructorAsDate: Boolean);
var
  lTypeData : PTypeData;
  lValue : TValue;
  lToken : TJSONToken;
  lName : string;
  lSingle : Single;
  lDouble : Double;
  lExtended : Extended;
  lDate : TDateTime;
  lDelta : Integer;
  lDepthOK,lContinue : Boolean;

begin
  repeat
    lValue:=aReader.Value;
    lToken:=aReader.TokenType;
    lTypeData:=lValue.TypeData;
    case lToken of
      TJsonToken.None: ;
      TJsonToken.&String: WriteValue(lValue.AsString);
      TJsonToken.Boolean: WriteValue(lValue.AsBoolean);
      TJsonToken.Null: WriteNull;
      TJsonToken.Undefined: WriteUndefined;
      TJsonToken.StartObject: WriteStartObject;
      TJsonToken.EndObject: WriteEndObject;
      TJsonToken.StartArray: WriteStartArray;
      TJsonToken.EndArray: WriteEndArray;
      TJsonToken.EndConstructor: WriteEndConstructor;
      TJsonToken.Bytes: WriteValue(lValue.specialize AsType<TBytes>);
      TJsonToken.Oid: WriteValue(lValue.specialize AsType<TJsonOid>);
      TJsonToken.RegEx: WriteValue(lValue.specialize AsType<TJsonRegEx>);
      TJsonToken.DBRef: WriteValue(lValue.specialize AsType<TJsonDBRef>);
      TJsonToken.CodeWScope: WriteValue(lValue.specialize AsType<TJsonCodeWScope>);
      TJsonToken.MinKey: WriteMinKey;
      TJsonToken.MaxKey: WriteMaxKey;
      TJsonToken.Decimal: WriteValue(lValue.specialize AsType<TJsonDecimal128>);
      TJsonToken.Timestamp: WriteValue(lValue.specialize AsType<TJsonTimestamp>);
      TJsonToken.Comment:
        if lValue.IsEmpty then
          WriteComment('')
        else
          WriteComment(lValue.AsString);
      TJsonToken.StartConstructor:
        begin
        lName:=lValue.AsString;
        // write a JValue date when the constructor is for a date
        if (lName='Date') and aWriteDateConstructorAsDate then
          WriteDateConstructor(aReader)
        else
          WriteStartConstructor(lName);
        end;
      TJsonToken.PropertyName: WritePropertyName(lValue.AsString);
      TJsonToken.Raw: WriteRawValue(lValue.AsString);
      TJsonToken.Integer:
        if lValue.TypeInfo^.Kind = tkInteger then
          begin
          if lTypeData^.OrdType in [otUByte, otUWord, otULong] then
            WriteValue(Cardinal(lValue.AsInteger))
          else
            WriteValue(lValue.AsInteger);
          end
        else
          begin
          if lTypeData^.MinInt64Value > lTypeData^.MaxInt64Value then
            WriteValue(lValue.AsUInt64)
          else
            WriteValue(lValue.AsInt64);
          end;
      TJsonToken.Float:
        begin
        lExtended := lValue.AsExtended;
        case lValue.TypeData^.FloatType of
          ftCurr,
          ftComp,
          ftExtended:
            WriteValue(lExtended);
          ftDouble:
          begin
            // Not sure if a typecast would have the same effect ?
            lDouble := lExtended;
            WriteValue(lDouble);
          end;
          ftSingle:
          begin
            lSingle := lExtended;
            WriteValue(lSingle);
          end;
        end;
        end;
      TJsonToken.Date:
        begin
        lDate := lValue.AsExtended;
        WriteValue(lDate);
        end;
    end;
    lDelta:=Ord(IsEndToken(aReader.TokenType));
    lDepthOK:=((aInitialDepth - 1) < (aReader.Depth - lDelta));
    lContinue:=lDepthOK and aWriteChildren and aReader.Read;
  until not (lContinue);
end;

class procedure TJsonWriter.WriteValue(const Writer: TJsonWriter; const Value: TValue);
begin
  if Writer <> nil then
    Writer.WriteValue(Value);
end;

procedure TJsonWriter.OnBeforeWriteToken(TokenBeginWriten: TJsonToken);
begin
  // Virtual method for subclasses to override
end;

class constructor TJsonWriter.Init;
begin
  BuildStateArray;
end;

constructor TJsonWriter.Create;
begin
  inherited Create;
  FCurrentState := TState.Start;
  FCloseOutput := True;
  FEmptyValueHandling := TJsonEmptyValueHandling.Empty;
  FDateTimeZoneHandling := TJsonDateTimeZoneHandling.Local;
end;

destructor TJsonWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TJsonWriter.Rewind;
begin
  inherited Rewind;
end;

procedure TJsonWriter.Close;
begin
  while GetTop > 0 do
    WriteEnd;
  FCurrentState := TState.Closed;
end;

procedure TJsonWriter.Flush;
begin
  // Base implementation does nothing
end;

procedure TJsonWriter.WriteComment(const Comment: string);
begin
  InternalWriteComment;
end;

procedure TJsonWriter.WriteStartObject;
begin
  InternalWriteStart(TJsonToken.StartObject, TJsonContainerType.&Object);
end;

procedure TJsonWriter.WriteEndObject;
begin
  InternalWriteEnd(TJsonContainerType.&Object);
end;

procedure TJsonWriter.WriteStartArray;
begin
  InternalWriteStart(TJsonToken.StartArray, TJsonContainerType.&Array);
end;

procedure TJsonWriter.WriteEndArray;
begin
  InternalWriteEnd(TJsonContainerType.&Array);
end;

procedure TJsonWriter.WriteStartConstructor(const Name: string);
begin
  InternalWriteStart(TJsonToken.StartConstructor, TJsonContainerType.&Constructor);
end;

procedure TJsonWriter.WriteEndConstructor;
begin
  InternalWriteEnd(TJsonContainerType.&Constructor);
end;

procedure TJsonWriter.WritePropertyName(const Name: string);
begin
  InternalWritePropertyName(Name);
end;

procedure TJsonWriter.WriteEnd;
begin
  case GetTopContainer of
    TJsonContainerType.&Object: WriteEndObject;
    TJsonContainerType.&Array: WriteEndArray;
    TJsonContainerType.&Constructor: WriteEndConstructor;
  end;
end;

procedure TJsonWriter.WriteNull;
begin
  InternalWriteValue(TJsonToken.Null);
end;

procedure TJsonWriter.WriteRaw(const Json: string);
begin
  InternalWriteValue(TJsonToken.Raw);
end;

procedure TJsonWriter.WriteRawValue(const Json: string);
begin
  UpdateScopeWithFinishedValue;
  AutoComplete(TJsonToken.Raw);
end;

procedure TJsonWriter.WriteUndefined;
begin
  InternalWriteValue(TJsonToken.Undefined);
end;

procedure TJsonWriter.WriteToken(const Reader: TJsonReader);
begin
  WriteToken(Reader, True, True);
end;

procedure TJsonWriter.WriteToken(const Reader: TJsonReader; WriteChildren: Boolean);
begin
  WriteToken(Reader, WriteChildren, True);
end;

procedure TJsonWriter.WriteValue(const Value: string);
begin
  if (Value = '') and (FEmptyValueHandling = TJsonEmptyValueHandling.Null) then
    WriteNull
  else
    InternalWriteValue(TJsonToken.&String);
end;

procedure TJsonWriter.WriteValue(const Value: PAnsiChar);
begin
  if Value = nil then
    WriteNull
  else
    WriteValue(string(Value));
end;

procedure TJsonWriter.WriteValue(const Value: PWideChar);
begin
  if Value = nil then
    WriteNull
  else
    WriteValue(string(Value));
end;

procedure TJsonWriter.WriteValue(Value: Integer);
begin
  InternalWriteValue(TJsonToken.Integer);
end;

procedure TJsonWriter.WriteValue(Value: UInt32);
begin
  InternalWriteValue(TJsonToken.Integer);
end;

procedure TJsonWriter.WriteValue(Value: Int64);
begin
  InternalWriteValue(TJsonToken.Integer);
end;

procedure TJsonWriter.WriteValue(Value: UInt64);
begin
  InternalWriteValue(TJsonToken.Integer);
end;

procedure TJsonWriter.WriteValue(Value: Single);
begin
  InternalWriteValue(TJsonToken.Float);
end;

procedure TJsonWriter.WriteValue(Value: Double);
begin
  InternalWriteValue(TJsonToken.Float);
end;

procedure TJsonWriter.WriteValue(Value: Extended);
begin
  InternalWriteValue(TJsonToken.Float);
end;

procedure TJsonWriter.WriteValue(Value: Boolean);
begin
  InternalWriteValue(TJsonToken.Boolean);
end;

procedure TJsonWriter.WriteValue(Value: Char);
begin
  WriteValue(string(Value));
end;

procedure TJsonWriter.WriteValue(Value: Byte);
begin
  WriteValue(Integer(Value));
end;

procedure TJsonWriter.WriteValue(Value: TDateTime);
begin
  case FDateTimeZoneHandling of
    TJsonDateTimeZoneHandling.Utc:
      InternalWriteValue(TJsonToken.Date);
    TJsonDateTimeZoneHandling.Local:
      InternalWriteValue(TJsonToken.Date);
  else
    InternalWriteValue(TJsonToken.Date);
  end;
end;

procedure TJsonWriter.WriteValue(const Value: TGUID);
begin
  WriteValue(GUIDToString(Value));
end;

procedure TJsonWriter.WriteValue(const Value: TBytes; BinaryType: TJsonBinaryType);
begin
  InternalWriteValue(TJsonToken.Bytes);
end;

procedure TJsonWriter.WriteValue(const Value: TJsonOid);
begin
  InternalWriteValue(TJsonToken.Oid);
end;

procedure TJsonWriter.WriteValue(const Value: TJsonRegEx);
begin
  InternalWriteValue(TJsonToken.RegEx);
end;

procedure TJsonWriter.WriteValue(const Value: TJsonDBRef);
begin
  InternalWriteValue(TJsonToken.DBRef);
end;

procedure TJsonWriter.WriteValue(const Value: TJsonCodeWScope);
begin
  InternalWriteValue(TJsonToken.CodeWScope);
end;

procedure TJsonWriter.WriteMinKey;
begin
  InternalWriteValue(TJsonToken.MinKey);
end;

procedure TJsonWriter.WriteMaxKey;
begin
  InternalWriteValue(TJsonToken.MaxKey);
end;

procedure TJsonWriter.WriteValue(const Value: TJsonDecimal128);
begin
  InternalWriteValue(TJsonToken.Decimal);
end;

procedure TJsonWriter.WriteValue(const Value: TJsonTimestamp);
begin

end;

procedure TJsonWriter.WriteValue(const Value: TValue);
begin
  case Value.Kind of
    tkInteger: WriteValue(Value.AsInteger);
    tkInt64: WriteValue(Value.AsInt64);
    tkFloat: WriteValue(Value.AsExtended);
    tkString, tkLString, tkWString, tkUString: WriteValue(Value.AsString);
    tkEnumeration:
      if Value.TypeInfo = TypeInfo(Boolean) then
        WriteValue(Value.AsBoolean)
      else
        WriteValue(Value.AsOrdinal);
  else
    WriteNull;
  end;
end;

{ TJsonWriter.TStateHelper }

function TJsonWriter.TStateHelper.ToString: string;
begin
  Result:=GetEnumName(TypeInfo(TState),Ord(Self));
end;

{ EJsonWriterException }

constructor EJsonWriterException.Create(const Msg: string; const Ex: Exception; const APath: string);
begin
  inherited Create(Msg, Ex);
  FPath := APath;
end;

constructor EJsonWriterException.Create(const Writer: TJsonWriter; const Msg: string; const Ex: Exception);
begin
  if Writer <> nil then
    FPath := Writer.Path
  else
    FPath := '';
  inherited Create(Msg, Ex);
end;

constructor EJsonWriterException.CreateFmt(const Writer: TJsonWriter; const Fmt: string; const args: array of const;
  const Ex: Exception);
begin
  Create(Writer,Format(Fmt,Args),Ex);
end;

constructor EJsonWriterException.Create(const APath, Msg: string; const Ex: Exception);
begin
  inherited Create(Msg, Ex);
  FPath := APath;
end;

{ TASCIIStreamWriter }

constructor TASCIIStreamWriter.Create(Stream: TStream; BufferSize: Integer);
begin
  inherited Create(Stream, TEncoding.ASCII, BufferSize);
end;

constructor TASCIIStreamWriter.Create(const Filename: string; Append: Boolean; BufferSize: Integer);
begin
  inherited Create(Filename, Append, TEncoding.ASCII, BufferSize);
end;

{ TJsonTextWriter }

procedure TJsonTextWriter.SetIndentation(aValue: Integer);
begin
  if FIndentation=AValue then Exit;
  FIndentation:=AValue;
end;

procedure TJsonTextWriter.SetQuoteChar(aValue: Char);
begin
  if FQuoteChar=AValue then Exit;
  FQuoteChar:=AValue;
end;

procedure TJsonTextWriter.SetStringEscapeHandling(aValue: TJsonStringEscapeHandling);
begin
  if FStringEscapeHandling=AValue then Exit;
  FStringEscapeHandling:=AValue;
end;

procedure TJsonTextWriter.WriteIndent;
var
  i: Integer;
  IndentLevel: Integer;
begin
  if FFormatting = TJsonFormatting.Indented then
  begin
    FWriter.WriteLine;
    // GetTop represents current nesting level
    IndentLevel := GetTop;
    for i := 0 to (IndentLevel * FIndentation) - 1 do
      FWriter.Write(FIndentChar);
  end;
end;

procedure TJsonTextWriter.WriteValueDelimiter;
begin
  FWriter.Write(',');
end;

procedure TJsonTextWriter.WriteIndentSpace;
begin
  if FFormatting = TJsonFormatting.Indented then
    FWriter.Write(' ');
end;

procedure TJsonTextWriter.WriteEnd(const aToken: TJsonToken);
begin
  case aToken of
    TJsonToken.EndObject:
      FWriter.Write('}');
    TJsonToken.EndArray:
      FWriter.Write(']');
    TJsonToken.EndConstructor:
      FWriter.Write(')');
  end;
  inherited WriteEnd(aToken);
end;

procedure TJsonTextWriter.InternalWriteEnd(aContainer: TJsonContainerType);
begin
  // Indent before closing token if formatted
  if FFormatting = TJsonFormatting.Indented then
    WriteIndent;

  case aContainer of
    TJsonContainerType.&Object:
      FWriter.Write('}');
    TJsonContainerType.&Array:
      FWriter.Write(']');
    TJsonContainerType.&Constructor:
      FWriter.Write(')');
  end;
  inherited InternalWriteEnd(aContainer);
end;

procedure TJsonTextWriter.OnBeforeWriteToken(aToken: TJsonToken);
begin
  case aToken of
    TJsonToken.StartObject, TJsonToken.StartArray, TJsonToken.StartConstructor:
      begin
        // For nested objects/arrays in arrays, write comma before the element (except for the first)
        if (FCurrentPosition.Position >= 0) and (FCurrentPosition.ContainerType = TJsonContainerType.&Array) then
          WriteValueDelimiter;
        // Only indent if we're inside a container (not the root)
        if (FFormatting = TJsonFormatting.Indented) and (GetTop > 0) then
          WriteIndent;
      end;
    TJsonToken.PropertyName:
      begin
        // Write comma before property name if not the first property in object
        if (FCurrentPosition.Position >= 0) and (FCurrentPosition.ContainerType = TJsonContainerType.&Object) then
          WriteValueDelimiter;
        if FFormatting = TJsonFormatting.Indented then
          WriteIndent;
      end;
    TJsonToken.&String, TJsonToken.Integer, TJsonToken.Float, TJsonToken.Boolean, TJsonToken.Null:
      begin
        // For array elements, write comma before the element (except for the first)
        if (FCurrentPosition.Position >= 0) and (FCurrentPosition.ContainerType = TJsonContainerType.&Array) then
          WriteValueDelimiter;
      end;
  end;
  inherited OnBeforeWriteToken(aToken);
end;

constructor TJsonTextWriter.Create(const aTextWriter: TTextWriter; aOwnsWriter: Boolean);
begin
  inherited Create;
  FWriter := aTextWriter;
  FOwnsWriter:=aOwnsWriter;
  FQuoteChar := '"';
  FQuoteName := True;
  FIndentChar := ' ';
  FIndentation := 2;
  FFormatting := TJsonFormatting.None;
  FStringEscapeHandling := TJsonStringEscapeHandling.Default;
  FDateFormatHandling := TJsonDateFormatHandling.Iso;
  FFloatFormatHandling := TJsonFloatFormatHandling.&String;
  FExtendedJsonMode := TJsonExtendedJsonMode.None;
  FFormatSettings := TFormatSettings.Invariant;
end;

constructor TJsonTextWriter.Create(const aTextWriter: TTextWriter);
begin
  Create(aTextWriter, False);
end;

constructor TJsonTextWriter.Create(const aStream: TStream);
begin
  Create(TStreamWriter.Create(aStream), True);
end;

destructor TJsonTextWriter.Destroy;
begin
  if FOwnsWriter then
    FreeAndNil(FWriter);
  inherited Destroy;
end;

procedure TJsonTextWriter.Close;
begin
  FWriter.Close;
  inherited Close;
end;

procedure TJsonTextWriter.Flush;
begin
  FWriter.Flush;
  inherited Flush;
end;

procedure TJsonTextWriter.WriteComment(const aComment: string);
begin
  FWriter.Write('/*');
  FWriter.Write(aComment);
  FWriter.Write('*/');
end;

procedure TJsonTextWriter.WriteNull;
begin
  inherited WriteNull;
  FWriter.Write('null');
end;

procedure TJsonTextWriter.WritePropertyName(const aName: string);
begin
  inherited WritePropertyName(aName);

  if FQuoteName then
  begin
    FWriter.Write(FQuoteChar);
    FWriter.Write(EscapeJsonString(aName));
    FWriter.Write(FQuoteChar);
  end
  else
    FWriter.Write(aName);
  FWriter.Write(':');
  WriteIndentSpace;
end;

procedure TJsonTextWriter.WritePropertyName(const aName: string; aEscape: Boolean);
begin
  if FQuoteName then
  begin
    FWriter.Write(FQuoteChar);
    if aEscape then
      FWriter.Write(EscapeJsonString(aName))
    else
      FWriter.Write(aName);
    FWriter.Write(FQuoteChar);
  end
  else
    FWriter.Write(aName);
  FWriter.Write(':');
  WriteIndentSpace;
end;

procedure TJsonTextWriter.WriteRaw(const aJson: string);
begin
  FWriter.Write(aJson);
end;

procedure TJsonTextWriter.WriteStartConstructor(const aName: string);
begin
  inherited WriteStartConstructor(aName);
  FWriter.Write('new ');
  FWriter.Write(aName);
  FWriter.Write('(');
end;

procedure TJsonTextWriter.WriteStartObject;
begin
  inherited WriteStartObject;
  FWriter.Write('{');
end;

procedure TJsonTextWriter.WriteStartArray;
begin
  inherited WriteStartArray;
  FWriter.Write('[');
end;

procedure TJsonTextWriter.WriteValue(const aValue: string);
var
  EscapedValue: string;
begin
  inherited WriteValue(aValue);
  EscapedValue := EscapeJsonString(aValue);
  FWriter.Write(DoQuote(EscapedValue));
end;

procedure TJsonTextWriter.WriteValue(aValue: Integer);
begin
  inherited WriteValue(aValue);
  FWriter.Write(IntToStr(aValue));
end;

procedure TJsonTextWriter.WriteValue(aValue: UInt32);
begin
  inherited WriteValue(aValue);
  FWriter.Write(UIntToStr(aValue));
end;

procedure TJsonTextWriter.WriteValue(aValue: Int64);
begin
  inherited WriteValue(aValue);
  FWriter.Write(IntToStr(aValue));
end;

procedure TJsonTextWriter.WriteValue(aValue: UInt64);
begin
  inherited WriteValue(aValue);
  FWriter.Write(UIntToStr(aValue));
end;

procedure TJsonTextWriter.WriteValue(aValue: Single);
begin
  inherited WriteValue(aValue);
  case FFloatFormatHandling of
    TJsonFloatFormatHandling.&String: FWriter.Write(FloatToStr(aValue, FFormatSettings));
    TJsonFloatFormatHandling.Symbol: FWriter.Write(FloatToStr(aValue, FFormatSettings));
    TJsonFloatFormatHandling.DefaultValue: FWriter.Write(FloatToStr(aValue, FFormatSettings));
  end;
end;

procedure TJsonTextWriter.WriteValue(aValue: Double);
begin
  inherited WriteValue(aValue);
  FWriter.Write(FloatToStr(aValue, FFormatSettings));
end;

procedure TJsonTextWriter.WriteValue(aValue: Extended);
begin
  inherited WriteValue(aValue);
  FWriter.Write(FloatToStr(aValue, FFormatSettings));
end;

procedure TJsonTextWriter.WriteValue(aValue: Boolean);
begin
  inherited WriteValue(aValue);
  if aValue then
    FWriter.Write('true')
  else
    FWriter.Write('false');
end;

procedure TJsonTextWriter.WriteValue(aValue: Char);
begin
  inherited WriteValue(aValue);
end;

procedure TJsonTextWriter.WriteValue(aValue: Byte);
begin
  inherited WriteValue(aValue);
end;

procedure TJsonTextWriter.WriteValue(aValue: TDateTime);
begin
  inherited WriteValue(aValue);
  case FDateFormatHandling of
    TJsonDateFormatHandling.Iso:
      WriteRaw(FQuoteChar+FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', aValue)+FQuoteChar);
    TJsonDateFormatHandling.Unix:
      WriteRaw(IntToStr(Trunc((aValue - EncodeDate(1970, 1, 1)) * 86400)));
    TJsonDateFormatHandling.FormatSettings:
      WriteRaw(FQuoteChar+DateTimeToStr(aValue, FFormatSettings)+FQuoteChar);
  end;
end;

procedure TJsonTextWriter.WriteValue(const aValue: TGUID);
begin
  inherited WriteValue(aValue);
end;

function TJsonTextWriter.DoQuote(const aString : String) : String;
begin
  Result:=FQuoteChar+aString+FQuoteChar;
end;

procedure TJsonTextWriter.WriteValue(const aValue: TBytes; aBinaryType: TJsonBinaryType);
var
  lbase64,lWrite :string;
begin
  inherited WriteValue(aValue, aBinaryType);
  if (Length(aValue) = 0) and (EmptyValueHandling = TJsonEmptyValueHandling.Null) then
     Exit;
   LBase64 := DoQuote(TNetEncoding.Base64String.EncodeBytesToString(aValue));
   case ExtendedJsonMode of
     TJsonExtendedJsonMode.None:
       lWrite:=LBase64;
     TJsonExtendedJsonMode.MongoShell:
       lWrite:=Format('BinData(%d,%s)',[Integer(aBinaryType),LBase64]);
     TJsonExtendedJsonMode.StrictMode:
       lWrite:='{'+DoQuote(JsonExtBinaryPropertyName)+':'+lBase64+','
                  +DoQuote(JsonExtTypePropertyName)+':'+DoQuote(IntToStr(Integer(aBinaryType)))+'}';
   end;
   WriteRaw(lWrite);
end;

procedure TJsonTextWriter.WriteValue(const aValue: TJsonOid);
var
  lRaw,lWrite : String;
begin
  inherited WriteValue(aValue);
  lRaw:=DoQuote(aValue.AsString);
  case ExtendedJsonMode of
    TJsonExtendedJsonMode.None:
      lWrite:=lRaw;
    TJsonExtendedJsonMode.StrictMode:
      lWrite:='{'+DoQuote(JsonExtOidPropertyName)+':'+lRaw+'}';
    TJsonExtendedJsonMode.MongoShell:
      lWrite:='ObjectId('+lRaw+')';
  end;
  WriteRaw(lWrite);
end;

procedure TJsonTextWriter.WriteValue(const aValue: TJsonRegEx);
var
  lRaw,lWrite : String;
begin
  inherited WriteValue(aValue);
  lRaw:=DoQuote(EscapeJSONString(aValue.AsString));
  case ExtendedJsonMode of
      TJsonExtendedJsonMode.None,
      TJsonExtendedJsonMode.MongoShell:
        lWrite:=lRaw;
      TJsonExtendedJsonMode.StrictMode:
        lWrite:='{'+DoQuote(JsonExtRegexPropertyName)+':'+lRaw+','+
                    DoQuote(JsonExtOptionsPropertyName)+':'+DoQuote(EscapeJSONString(aValue.Options))+'}';
  end;
  WriteRaw(lWrite);
end;

procedure TJsonTextWriter.WriteValue(const aValue: TJsonDBRef);
var
  lRaw,lWrite : String;
begin
  inherited WriteValue(aValue);
  case ExtendedJsonMode of
    TJsonExtendedJsonMode.None:
      begin
      lRaw:=aValue.Ref+'.'+aValue.Id.AsString;
      if aValue.DB<>'' then
        lRaw:=aValue.DB+'.'+lRaw;
      lWrite:=DoQuote(lRaw);
      end;
    TJsonExtendedJsonMode.StrictMode:
      begin
      lWrite:='{'+DoQuote(JsonExtRefPropertyName)+':'+DoQuote(aValue.Ref)+','
                 +DoQuote(JsonExtIdPropertyName)+':'+DoQuote(aValue.ID.AsString);
      if aValue.DB<>'' then
        lWrite:=lWrite+','+DoQuote(JsonExtDbPropertyName)+':'+DoQuote(aValue.DB);
      lWrite:=lWrite+'}';
      end;
    TJsonExtendedJsonMode.MongoShell:
      lWrite:='DBRef('+DoQuote(aValue.Ref)+','+DoQuote(aValue.ID.AsString)+')';
  end;
  WriteRaw(lWrite);
end;

procedure TJsonTextWriter.WriteValue(const aValue: TJsonCodeWScope);
var
  i: Integer;
  lRaw,lWrite : String;
begin
  Inherited WriteValue(aValue);
  lRaw:=DoQuote(aValue.Code);
  case ExtendedJsonMode of
    TJsonExtendedJsonMode.None,
    TJsonExtendedJsonMode.MongoShell:
      lWrite:=lRaw;
    TJsonExtendedJsonMode.StrictMode:
      begin
      lWrite:='{'+DoQuote(JsonExtCodePropertyName)+':'+lRaw;
      if Length(aValue.Scope)>0 then
        begin
        lWrite:=lWrite+','+DoQuote(JsonExtScopePropertyName)+':{';
        for i:=0 to Length(aValue.Scope)-1 do
          begin
          if i>0 then
            lWrite:=lWrite+',';
          lWrite:=lWrite+DoQuote(aValue.Scope[i].ident)+':'+DoQuote(aValue.Scope[i].value);
          end;
        lWrite:=lWrite+'}';
        end;
      lWrite:=lWrite+'}';
      end;
  end;
  WriteRaw(lWrite);
end;

procedure TJsonTextWriter.WriteMinKey;
var
  lWrite : string;
begin
  inherited WriteMinKey;
  case ExtendedJsonMode of
    TJsonExtendedJsonMode.None,
    TJsonExtendedJsonMode.MongoShell:
      lWrite:='MinKey';
    TJsonExtendedJsonMode.StrictMode:
      lWrite:='{'+DoQuote(JsonExtMinKeyPropertyName)+':1}';
  end;
  WriteRaw(lWrite);
end;

procedure TJsonTextWriter.WriteMaxKey;
var
  lWrite : string;
begin
  inherited WriteMaxKey;
  case ExtendedJsonMode of
    TJsonExtendedJsonMode.None,
    TJsonExtendedJsonMode.MongoShell:
      lWrite:='MaxKey';
    TJsonExtendedJsonMode.StrictMode:
      lWrite:='{'+DoQuote(JsonExtMaxKeyPropertyName)+':1}';
  end;
  WriteRaw(lWrite);
end;

procedure TJsonTextWriter.WriteValue(const aValue: TJsonDecimal128);
begin
  inherited WriteValue(aValue);
end;

procedure TJsonTextWriter.WriteValue(const aValue: TJsonTimestamp);
var
  lWrite : string;
begin
  Inherited;
  case ExtendedJsonMode of
    TJsonExtendedJsonMode.None:
      lWrite:=aValue.t.ToString;
    TJsonExtendedJsonMode.StrictMode:
      lWrite:='{'+DoQuote(JsonExtTimestampPropertyName)+':{'+
                  DoQuote('t')+':'+IntToStr(aValue.t)+','+
                  DoQuote('i')+':'+IntToStr(aValue.i)+'}}';
    TJsonExtendedJsonMode.MongoShell:
      lWrite:='Timestamp('+IntToStr(aValue.i)+','+IntToStr(aValue.t)+')';
  end;
  WriteRaw(lWrite);
end;

procedure TJsonTextWriter.WriteValue(const aValue: TValue);
begin
  inherited WriteValue(aValue);
end;

procedure TJsonTextWriter.WriteUndefined;
begin
  inherited WriteUndefined;
end;

procedure TJsonTextWriter.WriteWhitespace(const aWhiteSpace: string);
begin
  FWriter.Write(aWhiteSpace);
end;

function TJsonTextWriter.EscapeJsonString(const aValue: string): string;
var
  i: Integer;
  Ch: Char;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    for i := 1 to Length(aValue) do
    begin
      Ch := aValue[i];
      case Ch of
        '"': sb.Append('\"');
        '\': sb.Append('\\');
        '/':
          if FStringEscapeHandling = TJsonStringEscapeHandling.EscapeHtml then
            sb.Append('\/')
          else
            sb.Append('/');
        #8: sb.Append('\b');     // Backspace
        #12: sb.Append('\f');    // Form feed
        #10: sb.Append('\n');    // Line feed
        #13: sb.Append('\r');    // Carriage return
        #9: sb.Append('\t');     // Tab
        #0..#7, #11, #14..#31:   // Other control characters
          sb.AppendFormat('\u%0.4x', [Ord(Ch)]);
        #127..#255:              // Extended ASCII
          begin
            if FStringEscapeHandling = TJsonStringEscapeHandling.EscapeNonAscii then
              sb.AppendFormat('\u%0.4x', [Ord(Ch)])
            else
              sb.Append(Ch);
          end;
      else
        sb.Append(Ch);
      end;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{ TJsonObjectWriter }

function TJsonObjectWriter.GetContainer: TJSONAncestor;
begin
  Result := GetCurrentContainer;
  if Result = nil then
    Result := FRoot;
end;

procedure TJsonObjectWriter.SetContainer(AValue: TJSONAncestor);
begin
  if FOwnValue and (FRoot <> nil) then
    FRoot.Free;
  FRoot := AValue;
  FContainerStack.Clear;
  FCurrentPropertyName := '';
end;

function TJsonObjectWriter.GetCurrentContainer: TJSONAncestor;
begin
  if FContainerStack.Count > 0 then
    Result := TJSONAncestor(FContainerStack[FContainerStack.Count - 1])
  else
    Result := FRoot;
end;

procedure TJsonObjectWriter.AddValueToContainer(AValue: TJSONValue);
var
  CurrentCont: TJSONAncestor;
begin
  CurrentCont := GetCurrentContainer;
  if CurrentCont is TJSONObject then
  begin
    if FCurrentPropertyName <> '' then
    begin
      TJSONObject(CurrentCont).AddPair(FCurrentPropertyName, AValue);
      FCurrentPropertyName := '';
    end;
  end
  else if CurrentCont is TJSONArray then
  begin
    TJSONArray(CurrentCont).AddElement(AValue);
  end;
end;

procedure TJsonObjectWriter.PushContainer(AContainer: TJSONAncestor);
begin
  FContainerStack.Add(AContainer);
end;

function TJsonObjectWriter.PopContainer: TJSONAncestor;
begin
  if FContainerStack.Count > 0 then
  begin
    Result := TJSONAncestor(FContainerStack[FContainerStack.Count - 1]);
    FContainerStack.Delete(FContainerStack.Count - 1);
  end
  else
    Result := nil;
end;

constructor TJsonObjectWriter.Create(OwnValue: Boolean);
begin
  inherited Create;
  FOwnValue := OwnValue;
  FDateFormatHandling := TJsonDateFormatHandling.Iso;
  FRoot := nil;
  FContainerStack := TList.Create;
  FCurrentPropertyName := '';
end;

destructor TJsonObjectWriter.Destroy;
begin
  FContainerStack.Free;
  if FOwnValue and (FRoot <> nil) then
    FRoot.Free;
  inherited Destroy;
end;

procedure TJsonObjectWriter.Rewind;
begin
  if FOwnValue and (FRoot <> nil) then
    FRoot.Free;
  FRoot := nil;
  FContainerStack.Clear;
  FCurrentPropertyName := '';
  inherited Rewind;
end;

procedure TJsonObjectWriter.WriteNull;
begin
  inherited WriteNull;
  AddValueToContainer(TJSONNull.Create);
end;

procedure TJsonObjectWriter.WritePropertyName(const Name: string);
begin
  inherited WritePropertyName(Name);
  FCurrentPropertyName := Name;
end;

procedure TJsonObjectWriter.WriteStartConstructor(const Name: string);
begin
  inherited WriteStartConstructor(Name);
  // Constructors are not standard JSON, so we don't add them to the DOM
end;

procedure TJsonObjectWriter.WriteStartObject;
var
  NewObj: TJSONObject;
begin
  inherited WriteStartObject;
  NewObj := TJSONObject.Create;
  if FRoot = nil then
    FRoot := NewObj
  else
    AddValueToContainer(NewObj);
  PushContainer(NewObj);
end;

procedure TJsonObjectWriter.WriteStartArray;
var
  NewArr: TJSONArray;
begin
  inherited WriteStartArray;
  NewArr := TJSONArray.Create;
  if FRoot = nil then
    FRoot := NewArr
  else
    AddValueToContainer(NewArr);
  PushContainer(NewArr);
end;

procedure TJsonObjectWriter.WriteEndObject;
begin
  PopContainer;
  inherited WriteEndObject;
end;

procedure TJsonObjectWriter.WriteEndArray;
begin
  PopContainer;
  inherited WriteEndArray;
end;

procedure TJsonObjectWriter.WriteRaw(const Json: string);
begin
  inherited WriteRaw(Json);
  // Raw JSON is written as a string value
  AddValueToContainer(TJSONString.Create(Json));
end;

procedure TJsonObjectWriter.WriteRawValue(const Json: string);
var
  ParsedValue: TJSONValue;
begin
  inherited WriteRawValue(Json);
  // Try to parse the raw JSON and add to container
  ParsedValue := TJSONValue.ParseJSONValue(Json);
  if ParsedValue <> nil then
    AddValueToContainer(ParsedValue)
  else
    AddValueToContainer(TJSONString.Create(Json));
end;

procedure TJsonObjectWriter.WriteValue(const aValue: string);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONString.Create(aValue));
end;

procedure TJsonObjectWriter.WriteValue(aValue: Integer);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONNumber.Create(aValue));
end;

procedure TJsonObjectWriter.WriteValue(aValue: UInt32);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONNumber.Create(Int64(aValue)));
end;

procedure TJsonObjectWriter.WriteValue(aValue: Int64);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONNumber.Create(aValue));
end;

procedure TJsonObjectWriter.WriteValue(aValue: UInt64);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONNumber.Create(Int64(aValue)));
end;

procedure TJsonObjectWriter.WriteValue(aValue: Single);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONNumber.Create(Double(aValue)));
end;

procedure TJsonObjectWriter.WriteValue(aValue: Double);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONNumber.Create(aValue));
end;

procedure TJsonObjectWriter.WriteValue(aValue: Extended);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONNumber.Create(Double(aValue)));
end;

procedure TJsonObjectWriter.WriteValue(aValue: Boolean);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONBool.Create(aValue));
end;

procedure TJsonObjectWriter.WriteValue(aValue: Char);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONString.Create(string(aValue)));
end;

procedure TJsonObjectWriter.WriteValue(aValue: Byte);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONNumber.Create(Integer(aValue)));
end;

procedure TJsonObjectWriter.WriteValue(aValue: TDateTime);
var
  DateStr: string;
begin
  inherited WriteValue(aValue);
  case FDateFormatHandling of
    TJsonDateFormatHandling.Iso:
      DateStr := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', aValue);
    TJsonDateFormatHandling.Unix:
      DateStr := IntToStr(Trunc((aValue - EncodeDate(1970, 1, 1)) * 86400));
    TJsonDateFormatHandling.FormatSettings:
      DateStr := DateTimeToStr(aValue);
  else
    DateStr := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', aValue);
  end;
  AddValueToContainer(TJSONString.Create(DateStr));
end;

procedure TJsonObjectWriter.WriteValue(const aValue: TGUID);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONString.Create(GUIDToString(aValue)));
end;

procedure TJsonObjectWriter.WriteValue(const aValue: TBytes; aBinaryType: TJsonBinaryType);
begin
  inherited WriteValue(aValue, aBinaryType);
  AddValueToContainer(TJSONString.Create(TNetEncoding.Base64String.EncodeBytesToString(aValue)));
end;

procedure TJsonObjectWriter.WriteValue(const aValue: TJsonOid);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONString.Create(aValue.AsString));
end;

procedure TJsonObjectWriter.WriteValue(const aValue: TJsonRegEx);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONString.Create(aValue.AsString));
end;

procedure TJsonObjectWriter.WriteValue(const aValue: TJsonDBRef);
var
  RefStr: string;
begin
  inherited WriteValue(aValue);
  RefStr := aValue.Ref + '.' + aValue.Id.AsString;
  if aValue.DB <> '' then
    RefStr := aValue.DB + '.' + RefStr;
  AddValueToContainer(TJSONString.Create(RefStr));
end;

procedure TJsonObjectWriter.WriteValue(const aValue: TJsonCodeWScope);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONString.Create(aValue.Code));
end;

procedure TJsonObjectWriter.WriteMinKey;
begin
  inherited WriteMinKey;
  AddValueToContainer(TJSONString.Create('MinKey'));
end;

procedure TJsonObjectWriter.WriteMaxKey;
begin
  inherited WriteMaxKey;
  AddValueToContainer(TJSONString.Create('MaxKey'));
end;

procedure TJsonObjectWriter.WriteValue(const aValue: TJsonDecimal128);
begin
  inherited WriteValue(aValue);
  AddValueToContainer(TJSONString.Create(aValue.AsString));
end;

procedure TJsonObjectWriter.WriteValue(const Value: TJsonTimestamp);
begin
  inherited WriteValue(Value);
  AddValueToContainer(TJSONNumber.Create(Int64(Value.t)));
end;

procedure TJsonObjectWriter.WriteUndefined;
begin
  inherited WriteUndefined;
  AddValueToContainer(TJSONNull.Create);
end;


end.
