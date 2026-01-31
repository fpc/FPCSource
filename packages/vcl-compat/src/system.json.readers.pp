{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$scopedenums on}
{$macro on}

unit System.JSON.Readers;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti, Fcl.Streams.Extra,
  System.TypInfo, FpJson.Scanner,
  {$ELSE}
  SysUtils, Classes, Generics.Collections, Rtti, StreamEx, TypInfo,
  jsonscanner,
  {$ENDIF}
  System.JSON.Utils, System.JSON,  System.NetEncoding, System.JSON.Types;

{$IFDEF FPC_DOTTEDUNITS}
{$define jscan:=FpJson.Scanner}
{$ELSE}
{$define jscan:=jsonscanner}
{$ENDIF}

type
  TJSONAncestorList = specialize TList<TJSONAncestor>;
  TJsonToken = System.JSON.Types.TJsonToken;

  { TJsonTokenHelper }

  TJsonTokenHelper = type helper for TJsonToken
    function ToString : String;
  end;


  TJsonExtendedJsonMode = (None, StrictMode, MongoShell);
  TJsonDateParseHandling = (None, DateTime);
  TJsonDateTimeZoneHandling = (Local, Utc);

  TState = (Start, Complete, &Property, ObjectStart, &Object, arrayStart,
            &Array, Closed, PostValue, ConstructorStart, &Constructor,
            Error, Finished);

  { TJsonReader }
  TJsonReader = class(TJsonFiler)
  public
  type
    TReadType = (Read, ReadAsInteger, ReadAsBytes, ReadAsString, ReadAsDouble,
      ReadAsDateTime, ReadAsOid, ReadAsInt64, ReadAsUInt64);
  private
    FCloseInput: boolean;
    FDateTimeZoneHandling: TJsonDateTimeZoneHandling;
    FFormatSettings: TFormatSettings;
    FMaxDepth: integer;
    FQuoteChar: char;
    FSupportMultipleContent: boolean;
    FTokenType: TJsonToken;
    FValue: TValue;
    function GetDepth: integer;
  protected
    FCurrentState: TState;
    Procedure DoError(const aMsg : String); overload;
    Procedure DoError(const aFmt : String; const aArgs : Array of const); overload;

    function GetInsideContainer: boolean; override;
    procedure SetStateBasedOnCurrent;
    function ReadAsBytesInternal: TBytes;
    function ReadAsDateTimeInternal: TDateTime;
    function ReadAsDoubleInternal: double;
    function ReadAsIntegerInternal: integer;
    function ReadAsInt64Internal: int64;
    function ReadAsUInt64Internal: uint64;
    function ReadAsStringInternal: string;
    function ReadInternal: boolean; virtual; abstract;
    procedure SetPostValueState(aUpdateIndex: boolean);
    procedure SetToken(aNewToken: TJsonToken); overload; inline;
    procedure SetToken(aNewToken: TJsonToken; const aValue: TValue); overload; inline;
    procedure SetToken(aNewToken: TJsonToken; const aValue: TValue;
      aUpdateIndex: boolean); overload; inline;
    generic procedure SetToken<T>(aNewToken: TJsonToken; const aValue: T; aUpdateIndex: boolean);
      overload; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close; virtual;
    procedure Rewind; override;
    function Read: boolean; virtual;
    function ReadAsInteger: integer; virtual;
    function ReadAsInt64: int64; virtual;
    function ReadAsUInt64: uint64; virtual;
    function ReadAsString: string; virtual;
    function ReadAsBytes: TBytes; virtual;
    function ReadAsDouble: double; virtual;
    function ReadAsDateTime: TDateTime; virtual;
    procedure Skip; virtual;
    property CloseInput: boolean read FCloseInput write FCloseInput;
    property CurrentState: TState read FCurrentState;
    property Depth: integer read GetDepth;
    property TokenType: TJsonToken read FTokenType;
    property MaxDepth: integer read FMaxDepth write FMaxDepth;
    property QuoteChar: char read FQuoteChar write FQuoteChar;
    property SupportMultipleContent: boolean
      read FSupportMultipleContent write FSupportMultipleContent;
    property DateTimeZoneHandling: TJsonDateTimeZoneHandling
      read FDateTimeZoneHandling write FDateTimeZoneHandling;
    property FormatSettings: TFormatSettings read FFormatSettings write FFormatSettings;
    property Value: TValue read FValue;
  end;

  { EJsonReaderException }
  EJsonReaderException = class(EJsonException)
  private
    FLineNumber: integer;
    FLinePosition: integer;
    FPath: string;
  public
    constructor Create(const Msg: string; const aPath: string;
      aLineNumber: integer; aLinePosition: integer); overload;
    constructor Create(const Reader: TJsonReader; const Msg: string); overload;
    constructor Create(const LineInfo: TJsonLineInfo; const Path, Msg: string); overload;
    constructor CreateFmt(const Reader: TJsonReader; const Msg: string;
      const args: array of const); overload;
    constructor CreateFmt(const LineInfo: TJsonLineInfo; const Path, Msg: string;
      const args: array of const); overload;
    property LineNumber: integer read FLineNumber;
    property LinePosition: integer read FLinePosition;
    property Path: string read FPath;
  end;

  { TJsonTextReader }
  TJsonTextReader = class(TJsonReader)
  private
    FDateParseHandling: TJsonDateParseHandling;
    FExtendedJsonMode: TJsonExtendedJsonMode;
    FReader: TTextReader;
    FScanner: TJSONScanner;
    FOwnsReader: boolean;
    FContent: string;
  protected
    function ReadInternal: boolean; override;
  public
    constructor Create(const aReader: TTextReader);
    destructor Destroy; override;
    procedure Close; override;
    procedure Rewind; override;
    function GetLineNumber: integer; override;
    function GetLinePosition: integer; override;
    function HasLineInfo: boolean; override;
    property Reader: TTextReader read FReader;
    property LineNumber: integer read GetLineNumber;
    property LinePosition: integer read GetLinePosition;
    property DateParseHandling: TJsonDateParseHandling
      read FDateParseHandling write FDateParseHandling;
    property ExtendedJsonMode: TJsonExtendedJsonMode
      read FExtendedJsonMode write FExtendedJsonMode;
  end;

  { TJsonObjectReader }

  { TJSONAncestorData }


  TJsonObjectReader = class(TJsonReader)
  private
    Type
       TContainerData = Class
         Ancestor : TJSONAncestor;
         CurrentIndex : Integer;
         constructor Create(aAncestor : TJSONAncestor);
       end;
       TJSONAncestorDataList = specialize TObjectList<TContainerData>;
  private
    FRoot: TContainerData;
    FAncestors : TJSONAncestorDataList;
    function GetCurrent: TJSONAncestor;
    function GetCurrentData : TContainerData;
  protected
    function ReadInternal: boolean; override;
  public
    constructor Create(const aRoot: TJSONAncestor);
    destructor Destroy; override;
    procedure Close; override;
    procedure Rewind; override;
    property Current: TJSONAncestor read GetCurrent;
  end;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.DateUtils, System.Math, System.Character,
  {$ELSE}
  DateUtils, Math, Character,
  {$ENDIF}
  System.JSONConsts;

{ TJsonTokenHelper }

function TJsonTokenHelper.ToString: String;
begin
  Result:=GetEnumName(TypeInfo(TJSONToken),Ord(Self));
end;

{ TJsonReader }

function TJsonReader.GetDepth: integer;
begin
  Result:=FStack.Count;
end;

procedure TJsonReader.DoError(const aMsg: String);
begin
  raise EJsonReaderException.Create(Self,aMsg)
end;

procedure TJsonReader.DoError(const aFmt: String; const aArgs: array of const);
begin
  raise EJsonReaderException.CreateFmt(Self,aFmt,aArgs)
end;

function TJsonReader.GetInsideContainer: boolean;
begin
  Result:=(FCurrentState in [TState.&Object, TState.&Array, TState.&Constructor]) and
    (FTokenType <> TJsonToken.PropertyName);
end;

procedure TJsonReader.SetStateBasedOnCurrent;
begin
  case FTokenType of
    TJsonToken.StartObject:
      FCurrentState:=TState.ObjectStart;
    TJsonToken.StartArray:
      FCurrentState:=TState.ArrayStart;
    TJsonToken.StartConstructor:
      FCurrentState:=TState.ConstructorStart;
    TJsonToken.PropertyName:
      FCurrentState:=TState.&Property;
    TJsonToken.Comment:
      ; // No state change for comments
    TJsonToken.EndObject,
    TJsonToken.EndArray,
    TJsonToken.EndConstructor:
      FCurrentState:=TState.PostValue;
    else
      FCurrentState:=TState.PostValue;
  end;
end;

function TJsonReader.ReadAsBytesInternal: TBytes;
begin
  ReadInternal;
  if FTokenType = TJsonToken.Bytes then
    Result:=FValue.specialize AsType<TBytes>
  else if FTokenType = TJsonToken.&String then
    Result:=TNetEncoding.Base64.DecodeStringToBytes(FValue.AsString)
  else if FTokenType = TJsonToken.Null then
    Result:=nil
  else
    DoError(SUnexpectedTokenReadBytes,[GetEnumName(TypeInfo(TJsonToken), Ord(FTokenType))]);
end;

function TJsonReader.ReadAsDateTimeInternal: TDateTime;
var
  S: string;
begin
  ReadInternal;
  if FTokenType = TJsonToken.Date then
    Result:=FValue.specialize AsType<TDateTime>
  else if FTokenType = TJsonToken.&String then
    begin
    S:=FValue.AsString;
    if not TryStrToDateTime(S, Result, FFormatSettings) then
      Result:=0;
    end
  else if FTokenType = TJsonToken.Null then
    Result:=0
  else
    DoError(SUnexpectedTokenDate ,[GetEnumName(TypeInfo(TJsonToken), Ord(FTokenType))]);
end;

function TJsonReader.ReadAsDoubleInternal: double;
var
  S: string;
begin
  ReadInternal;
  case FTokenType of
    TJsonToken.integer:
      Result:=FValue.specialize AsType<integer>;
    TJsonToken.Float:
      Result:=FValue.specialize AsType<double>;
    TJsonToken.&String:
      begin
      S:=FValue.AsString;
      if not TryStrToFloat(S, Result, FFormatSettings) then
        Result:=0.0;
      end;
    TJsonToken.Null:
      Result:=0.0;
    else
      DoError(SInputInvalidDouble,[GetEnumName(TypeInfo(TJsonToken), Ord(FTokenType))]);
  end;
end;

function TJsonReader.ReadAsIntegerInternal: integer;
var
  S: string;
begin
  ReadInternal;
  case FTokenType of
    TJsonToken.integer:
      Result:=FValue.specialize AsType<integer>;
    TJsonToken.Float:
      Result:=Trunc(FValue.Specialize AsType<double>);
    TJsonToken.&String:
      begin
      S:=FValue.AsString;
      if not TryStrToInt(S, Result) then
        Result:=0;
      end;
    TJsonToken.Null:
      Result:=0;
    else
      DoError(SInputInvalidInteger,[GetEnumName(TypeInfo(TJsonToken), Ord(FTokenType))]);
  end;
end;

function TJsonReader.ReadAsInt64Internal: int64;
var
  S: string;
begin
  ReadInternal;
  case FTokenType of
    TJsonToken.integer:
      Result:=FValue.specialize AsType<int64>;
    TJsonToken.Float:
      Result:=Trunc(FValue.specialize AsType<double>);
    TJsonToken.&String:
      begin
      S:=FValue.AsString;
      if not TryStrToInt64(S, Result) then
        Result:=0;
      end;
    TJsonToken.Null:
      Result:=0;
    else
      DoError(SInputInvalidInt64,[GetEnumName(TypeInfo(TJsonToken), Ord(FTokenType))]);
  end;
end;

function TJsonReader.ReadAsUInt64Internal: uint64;
var
  S: string;
begin
  ReadInternal;
  case FTokenType of
    TJsonToken.integer:
      Result:=FValue.specialize AsType<uint64>;
    TJsonToken.Float:
      Result:=Trunc(FValue.specialize AsType<double>);
    TJsonToken.&String:
      begin
      S:=FValue.AsString;
      if not TryStrToUInt64(S, Result) then
        Result:=0;
      end;
    TJsonToken.Null:
      Result:=0;
    else
      DoError(SInputInvalidUInt64,[GetEnumName(TypeInfo(TJsonToken), Ord(FTokenType))]);
  end;
end;

function TJsonReader.ReadAsStringInternal: string;
begin
  ReadInternal;
  case FTokenType of
    TJsonToken.&String,
    TJsonToken.PropertyName:
      Result:=FValue.AsString;
    TJsonToken.integer:
      Result:=IntToStr(FValue.specialize AsType<integer>);
    TJsonToken.Float:
      Result:=FloatToStr(FValue.specialize AsType<double>, FFormatSettings);
    TJsonToken.boolean:
      Result:=BoolToStr(FValue.specialize AsType<boolean>, True);
    TJsonToken.Null:
      Result:='';
    else
      Result:=FValue.AsString;
  end;
end;

procedure TJsonReader.SetPostValueState(aUpdateIndex: boolean);
begin
  if Peek = TJsonContainerType.&Array then
    begin
    FCurrentPosition.Position:=FCurrentPosition.Position + 1;
    FCurrentState:=TState.&Array;
    end
  else if Peek = TJsonContainerType.&Object then
    FCurrentState:=TState.&Object
  else
    FCurrentState:=TState.PostValue;
end;

procedure TJsonReader.SetToken(aNewToken: TJsonToken);
begin
  SetToken(aNewToken, TValue.Empty, True);
end;

procedure TJsonReader.SetToken(aNewToken: TJsonToken; const aValue: TValue);
begin
  SetToken(aNewToken, aValue, True);
end;

procedure TJsonReader.SetToken(aNewToken: TJsonToken; const aValue: TValue;
  aUpdateIndex: boolean);
begin
  FTokenType:=aNewToken;
  FValue:=aValue;

  case aNewToken of
    TJsonToken.StartObject:
      begin
      Push(TJsonContainerType.&Object);
      FCurrentState:=TState.ObjectStart;
      end;
    TJsonToken.StartArray:
      begin
      Push(TJsonContainerType.&Array);
      FCurrentState:=TState.ArrayStart;
      end;
    TJsonToken.StartConstructor:
      begin
      Push(TJsonContainerType.&Constructor);
      FCurrentState:=TState.ConstructorStart;
      end;
    TJsonToken.PropertyName:
      begin
      FCurrentPosition.PropertyName:=aValue.AsString;
      FCurrentState:=TState.&Property;
      end;
    TJsonToken.EndObject:
      begin
      if FCurrentState = TState.&Property then
        DoError(SInvalidState, [GetEnumName(typeInfo(TState), Ord(FCurrentState))]);
      Pop;
      SetPostValueState(aUpdateIndex);
      end;

    TJsonToken.EndArray,
    TJsonToken.EndConstructor:
      begin
      Pop;
      SetPostValueState(aUpdateIndex);
      end;
    else
      SetPostValueState(aUpdateIndex);
  end;
end;

generic procedure TJsonReader.SetToken<T>(aNewToken: TJsonToken; const aValue: T;
  aUpdateIndex: boolean);
begin
  SetToken(aNewToken, TValue.specialize From<T>(aValue), aUpdateIndex);
end;

constructor TJsonReader.Create;
begin
  inherited Create;
  FMaxDepth:=64;
  FQuoteChar:='"';
  FDateTimeZoneHandling:=TJsonDateTimeZoneHandling.Local;
  FFormatSettings:=TFormatSettings.Create;
  FCurrentState:=TState.Start;
  FSupportMultipleContent:=False;
end;

destructor TJsonReader.Destroy;
begin
  inherited Destroy;
end;

procedure TJsonReader.Close;
begin
  FCurrentState:=TState.Closed;
end;

procedure TJsonReader.Rewind;
begin
  inherited Rewind;
end;

function TJsonReader.Read: boolean;
begin
  Result:=ReadInternal;
end;

function TJsonReader.ReadAsInteger: integer;
begin
  Result:=ReadAsIntegerInternal;
end;

function TJsonReader.ReadAsInt64: int64;
begin
  Result:=ReadAsInt64Internal;
end;

function TJsonReader.ReadAsUInt64: uint64;
begin
  Result:=ReadAsUInt64Internal;
end;

function TJsonReader.ReadAsString: string;
begin
  Result:=ReadAsStringInternal;
end;

function TJsonReader.ReadAsBytes: TBytes;
begin
  Result:=ReadAsBytesInternal;
end;

function TJsonReader.ReadAsDouble: double;
begin
  Result:=ReadAsDoubleInternal;
end;

function TJsonReader.ReadAsDateTime: TDateTime;
begin
  Result:=ReadAsDateTimeInternal;
end;

procedure TJsonReader.Skip;
var
  lDepth: integer;
  lState: TState;
begin
  if FTokenType = TJsonToken.PropertyName then
    Read;

  if FTokenType in [TJsonToken.StartObject, TJsonToken.StartArray,
    TJsonToken.StartConstructor] then
    begin
    lDepth:=GetDepth;
    // Skip until we're back to the original depth
    while Read and (GetDepth >= lDepth) do ;
{
      begin
      lState:=CurrentState;
      Writeln(FTokenType,' : ',lState);
      end;
}
    end;
end;

{ EJsonReaderException }

constructor EJsonReaderException.Create(const Msg: string; const aPath: string;
  aLineNumber: integer; aLinePosition: integer);
begin
  inherited Create(Msg);
  FPath:=aPath;
  FLineNumber:=aLineNumber;
  FLinePosition:=aLinePosition;
end;

constructor EJsonReaderException.Create(const Reader: TJsonReader; const Msg: string);
begin
  inherited Create(Msg);
  if assigned(Reader) then
    begin
    FPath:=Reader.Path;
    FLineNumber:=Reader.GetLineNumber;
    FLinePosition:=Reader.GetLinePosition;
    end;
end;

constructor EJsonReaderException.Create(const LineInfo: TJsonLineInfo;
  const Path, Msg: string);
begin
  inherited Create(Msg);
  FPath:=Path;
  if assigned(LineInfo) then
    begin
    FLineNumber:=LineInfo.GetLineNumber;
    FLinePosition:=LineInfo.GetLinePosition;
    end;
end;

constructor EJsonReaderException.CreateFmt(const Reader: TJsonReader;
  const Msg: string; const args: array of const);
begin
  Create(Reader, Format(Msg, args));
end;

constructor EJsonReaderException.CreateFmt(const LineInfo: TJsonLineInfo;
  const Path, Msg: string; const args: array of const);
begin
  Create(LineInfo, Path, Format(Msg, args));
end;

{ TJsonTextReader }

function TJsonTextReader.ReadInternal: boolean;
var
  ScannerToken: jscan.TJSONToken;
  TokenValue: string;
  ExpectingPropertyName: boolean;
begin
  Result:=False;
  if not assigned(FScanner) then
    Exit;

  try
    repeat
      ScannerToken:=FScanner.FetchToken;

      case ScannerToken of
        jscan.tkEOF:
          begin
          SetToken(TJsonToken.None);
          FCurrentState:=TState.Finished;
          Exit;
          end;

        jscan.tkWhitespace,
        jscan.tkComment:
          begin
          // Skip whitespace and comments
          Continue;
          end;

        jscan.tkString:
          begin
          TokenValue:=FScanner.CurTokenString;
          // Determine if this is a property name or string value
          ExpectingPropertyName:=(Peek = TJsonContainerType.&Object) and
            (FCurrentState in
            [TState.ObjectStart, TState.&Object]);

          if ExpectingPropertyName then
            begin
            specialize SetToken<string>(TJsonToken.PropertyName, TokenValue, True);
            // read the colon
            repeat
              ScannerToken:=FScanner.FetchToken;
            until ScannerToken <> jscan.tkWhitespace;

            if ScannerToken <> jscan.tkColon then
              DoError(SParseErrorColonExpected);
            end
          else
            specialize SetToken<string>(TJsonToken.&String, TokenValue, True);
        end;

        jscan.tkNumber:
          begin
          TokenValue:=FScanner.CurTokenString;
          if (Pos('.', TokenValue) > 0) or (Pos('e', LowerCase(TokenValue)) > 0) then
            specialize SetToken<double>(TJsonToken.Float,
              StrToFloatDef(TokenValue, 0.0, FFormatSettings), True)
          else
            specialize SetToken<int64>(TJsonToken.integer, StrToInt64Def(TokenValue, 0), True);
          end;

        jscan.tkTrue:
          specialize SetToken<boolean>(TJsonToken.boolean, True, True);

        jscan.tkFalse:
          specialize SetToken<boolean>(TJsonToken.boolean, False, True);

        jscan.tkNull:
          SetToken(TJsonToken.Null);

        jscan.tkCurlyBraceOpen:
          SetToken(TJsonToken.StartObject);

        jscan.tkCurlyBraceClose:
          SetToken(TJsonToken.EndObject);

        jscan.tkSquaredBraceOpen:
          SetToken(TJsonToken.StartArray);

        jscan.tkSquaredBraceClose:
          SetToken(TJsonToken.EndArray);

        jscan.tkColon:
          // Colon should be consumed when processing property names
          Continue;

        jscan.tkComma:
          // Comma separator, continue to next token
          Continue;

        jscan.tkIdentifier:
          begin
          TokenValue:=FScanner.CurTokenString;
          // In non-strict mode, identifiers can be property names
          ExpectingPropertyName:=(Peek = TJsonContainerType.&Object) and
            (FCurrentState in
            [TState.ObjectStart, TState.&Object]);

          if ExpectingPropertyName then
            begin
            specialize SetToken<string>(TJsonToken.PropertyName, TokenValue, True);
            // Now we need to read the colon
            repeat
              ScannerToken:=FScanner.FetchToken;
            until ScannerToken <> jscan.tkWhitespace;

            if ScannerToken <> jscan.tkColon then
              DoError(SParseErrorColonExpected);
            end
          else
            specialize SetToken<string>(TJsonToken.&String, TokenValue, True);
          end;
        else
          DoError(SUnexpectedToken,[FScanner.CurTokenString]);
      end;

      Break; // Exit the repeat loop when we have a valid token
    until False;

    Result:=True;

  except
    // Convert FPC error to delphi-compatible error
    on E: EScannerError do
      DoError(E.Message);
  end;
end;

constructor TJsonTextReader.Create(const aReader: TTextReader);
begin
  inherited Create;
  FReader:=aReader;
  FOwnsReader:=False;
  FDateParseHandling:=TJsonDateParseHandling.DateTime;
  FExtendedJsonMode:=TJsonExtendedJsonMode.None;

  // Read all content from the TextReader
  // This is not really optimal. Needs changes in the jscan to support a TTextReader.
  FContent:='';
  repeat
  try
    FContent:=FContent + FReader.ReadLine + sLineBreak;
  except
    Break;
  end;
  until FReader.EOF;
  // Create scanner with the content
  FScanner:=TJSONScanner.Create(FContent, [joUTF8]);
end;

destructor TJsonTextReader.Destroy;
begin
  FreeAndNil(FScanner);
  if FOwnsReader then
    FreeAndNil(FReader);
  inherited Destroy;
end;

procedure TJsonTextReader.Close;
begin
  inherited Close;
  if FOwnsReader then
    FreeAndNil(FReader);
end;

procedure TJsonTextReader.Rewind;
begin
  inherited Rewind;
  FCurrentState:=TState.Start;

  // Recreate scanner to reset position
  FreeAndNil(FScanner);
  FScanner:=TJSONScanner.Create(FContent, [joUTF8]);
end;

function TJsonTextReader.GetLineNumber: integer;
begin
  if assigned(FScanner) then
    Result:=FScanner.CurRow
  else
    Result:=0;
end;

function TJsonTextReader.GetLinePosition: integer;
begin
  if assigned(FScanner) then
    Result:=FScanner.CurColumn
  else
    Result:=0;
end;

function TJsonTextReader.HasLineInfo: boolean;
begin
  Result:=assigned(FScanner);
end;

{ TContainerData }

constructor TJsonObjectReader.TContainerData.Create(aAncestor: TJSONAncestor);
begin
  Ancestor:=aAncestor;
end;

{ TJsonObjectReader }

function TJsonObjectReader.GetCurrent: TJSONAncestor;
begin
  Result:=GetCurrentData.Ancestor;
end;

function TJsonObjectReader.GetCurrentData: TContainerData;
begin
  if (FAncestors.Count > 0) then
    Result:=FAncestors[FAncestors.Count - 1]
  else
    Result:=FRoot;
end;

function TJsonObjectReader.ReadInternal: boolean;
var
  lData : TContainerData;
  lCurrent: TJSONAncestor;
  JsonObj: TJSONObject;
  JsonArr: TJSONArray;
  Pair: TJSONPair;
begin
  Result:=False;
  lData:=GetCurrentData;
  lCurrent:=lData.Ancestor;

  if not assigned(lCurrent) then
    begin
    FCurrentState:=TState.Finished;
    SetToken(TJsonToken.None);
    Exit;
    end;

  // Handle different JSON object types
  if lCurrent is TJSONObject then
    begin
    JsonObj:=TJSONObject(lCurrent);
    if FCurrentState = TState.Start then
      begin
      SetToken(TJsonToken.StartObject);
      FCurrentState:=TState.&Object;
      lData.CurrentIndex:=0;
      Result:=True;
      end
    else if lData.CurrentIndex < JsonObj.Count then
      begin
      Pair:=JsonObj.Pairs[lData.CurrentIndex];
      if FCurrentState = TState.&Object then
        begin
        specialize SetToken<UnicodeString>(TJsonToken.PropertyName, Pair.JsonString.Value, True);
        FCurrentState:=TState.&Property;
        end
      else
        begin
        // Push the value for reading
        FAncestors.Add(TContainerData.Create(Pair.JsonValue));
        Inc(lData.CurrentIndex);
        Result:=ReadInternal(); // Read the value
        FAncestors.Delete(FAncestors.Count - 1);
        end;
      Result:=True;
      end
    else
      begin
      SetToken(TJsonToken.EndObject);
      Result:=True;
      end;
    end
  else if lCurrent is TJSONArray then
    begin
    JsonArr:=TJSONArray(lCurrent);
    if FCurrentState = TState.Start then
      begin
      SetToken(TJsonToken.StartArray);
      LData.CurrentIndex:=0;
      Result:=True;
      end
    else if LData.CurrentIndex < JsonArr.Count then
      begin
      FAncestors.Add(TContainerData.Create(JsonArr.Items[LData.CurrentIndex]));
      Inc(LData.CurrentIndex);
      Result:=ReadInternal(); // Read the array element
      FAncestors.Delete(FAncestors.Count - 1);
      Result:=True;
      end
    else
      begin
      SetToken(TJsonToken.EndArray);
      Result:=True;
      end;
    end
  else if lCurrent is TJSONNumber then
    begin
    // Try to determine if it's an integer or float
    if Pos('.', TJSONNumber(lCurrent).Value) > 0 then
      specialize SetToken<double>(TJsonToken.Float, StrToFloatDef(TJSONNumber(lCurrent).Value, 0.0), True)
    else
      specialize SetToken<int64>(TJsonToken.integer, StrToInt64Def(TJSONNumber(lCurrent).Value, 0), True);
    Result:=True;
    end
  else if lCurrent is TJSONString then
    begin
    specialize SetToken<string>(TJsonToken.&String, TJSONString(lCurrent).Value, True);
    Result:=True;
    end
  else if lCurrent is TJSONBool then
    begin
    specialize SetToken<boolean>(TJsonToken.boolean, TJSONBool(lCurrent).Value = 'true', True);
    Result:=True;
    end
  else if lCurrent is TJSONNull then
    begin
    SetToken(TJsonToken.Null);
    Result:=True;
    end;
end;

constructor TJsonObjectReader.Create(const aRoot: TJSONAncestor);
begin
  inherited Create;
  FRoot:=TContainerData.Create(aRoot);
  FAncestors:=TJSONAncestorDataList.Create(True);
end;

destructor TJsonObjectReader.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FAncestors);
  inherited Destroy;
end;

procedure TJsonObjectReader.Close;
begin
  inherited Close;
end;

procedure TJsonObjectReader.Rewind;
begin
  inherited Rewind;
  FAncestors.Clear;
  FCurrentState:=TState.Start;
end;

end.
