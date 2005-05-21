{
    This file is part of the Free Component Library

    SAX 2 (Simple API for XML) implementation
    Copyright (c) 2000 - 2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit SAX;

{ $DEFINE UseDynArrays}

{ $IFDEF UseDynArrays}
  {$MODE objfpc}
{ $ELSE}
  { $MODE Delphi}
{ $ENDIF}
{$H+}


interface

uses SysUtils, Classes;

resourcestring
  SSAXAttributeIndexError = 'Invalid attribute index %d';

const
  XMLNS = 'http://www.w3.org/XML/1998/namespace';

type

  SAXString = WideString;
  SAXChar = WideChar;
  PSAXChar = ^SAXChar;

{ Exceptions }

  ESAXError = class(Exception);

  ESAXAttributeIndexError = class(ESAXError)
  public
    constructor Create(Index: Integer);
  end;

  ESAXParseException = class(ESAXError);


{ TSAXInputSource: A single input source for an XML entity }

  TSAXInputSource = class
  private
    FStream: TStream;
    FEncoding: String;
    FPublicID, FSystemID: SAXString;
  public
    constructor Create; overload;
    constructor Create(AStream: TStream); overload;
    constructor Create(const ASystemID: SAXString); overload;
    property Stream: TStream read FStream write FStream;
    property Encoding: String read FEncoding write FEncoding;
    property PublicID: SAXString read FPublicID write FPublicID;
    property SystemID: SAXString read FSystemID write FSystemID;
  end;


{ TSAXAttributes: List of XML attributes }

  TSAXAttributeData = record
    URI, LocalName, QName, Value: SAXString;
    AttrType: String;
  end;

  {$IFNDEF UseDynArrays}
  PSAXAttributeData = ^TSAXAttributeData;
  {$ENDIF}

  TSAXAttributes = class
  protected
    FLength: Integer;
    {$IFDEF UseDynArrays}
    Data: array of TSAXAttributeData;
    {$ELSE}
    FData: TList;
    function GetData(Index: Integer): PSAXAttributeData;
    property Data[Index:Integer]: PSAXAttributeData read GetData;
    {$ENDIF}
    procedure BadIndex(Index: Integer);
  public
    constructor Create; overload;
    constructor Create(Atts: TSAXAttributes); overload;
    {$IFNDEF UseDynArrays}
    destructor Destroy; override;
    {$ENDIF}

    function GetIndex(const QName: SAXString): Integer; overload;
    function GetIndex(const URI, LocalPart: SAXString): Integer; overload;
    function GetLength: Integer;
    function GetLocalName(Index: Integer): SAXString;
    function GetQName(Index: Integer): SAXString;
    function GetType(Index: Integer): String; overload;
    function GetType(const QName: SAXString): String; overload;
    function GetType(const URI, LocalName: SAXString): String; overload;
    function GetURI(Index: Integer): SAXString;
    function GetValue(Index: Integer): SAXString; overload;
    function GetValue(const QName: SAXString): SAXString; overload;
    function GetValue(const URI, LocalName: SAXString): SAXString; overload;

    // Manipulation methods:
    procedure Clear;
    procedure SetAttributes(Atts: TSAXAttributes);
    procedure AddAttribute(const AURI, ALocalName, AQName: SAXString;
      const AType: String; const AValue: SAXString);
    procedure SetAttribute(Index: Integer;
      const AURI, ALocalName, AQName: SAXString; const AType: String;
      const AValue: SAXString);
    procedure RemoveAttribute(Index: Integer);
    procedure SetURI(Index: Integer; const AURI: SAXString);
    procedure SetLocalName(Index: Integer; const ALocalName: SAXString);
    procedure SetQName(Index: Integer; const AQName: SAXString);
    procedure SetType(Index: Integer; const AType: String);
    procedure SetValue(Index: Integer; const AValue: SAXString);

    property Length: Integer read GetLength;
    property LocalNames[Index: Integer]: SAXString read GetLocalName;
    property QNames[Index: Integer]: SAXString read GetQName;
    property Types[Index: Integer]: String read GetType;
    property URIs[Index: Integer]: SAXString read GetURI;
    property Values[Index: Integer]: SAXString read GetValue;
  end;


{ TSAXReader: Reading an XML document using callbacks }

  TCharactersEvent = procedure(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer) of object;
  TCommentEvent = procedure(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer) of object;
  TEndElementEvent = procedure(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString) of object;
  TEndPrefixMappingEvent = procedure(Sender: TObject; const Prefix: SAXString) of object;
  TIgnorableWhitespaceEvent = procedure(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer) of object;
  TProcessingInstructionEvent = procedure(Sender: TObject; const Target, Data: SAXString) of object;
  TSkippedEntityEvent = procedure(Sender: TObject; const Name: SAXString) of object;
  TStartElementEvent = procedure(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes) of object;
  TStartPrefixMappingEvent = procedure(Sender: TObject; const Prefix, URI: SAXString) of object;
  TNotationDeclEvent = procedure(Sender: TObject; const Name, PublicID, SystemID: SAXString) of object;
  TUnparsedEntityDeclEvent = procedure(Sender: TObject; const Name, PublicID, SystemID, NotationName: SAXString) of object;
  TResolveEntityEvent = function(Sender: TObject; const PublicID, SystemID: SAXString): TSAXInputSource of object;
  TErrorEvent = procedure(Sender: TObject; AException: ESAXParseException) of object;
  TFatalErrorEvent = procedure(Sender: TObject; AException: ESAXParseException) of object;
  TWarningEvent = procedure(Sender: TObject; AException: ESAXParseException) of object;


  TSAXReader = class
  private
    FOnCharacters: TCharactersEvent;
    FOnComment: TCommentEvent;
    FOnEndDocument: TNotifyEvent;
    FOnEndElement: TEndElementEvent;
    FOnEndPrefixMapping: TEndPrefixMappingEvent;
    FOnIgnorableWhitespace: TIgnorableWhitespaceEvent;
    FOnProcessingInstruction: TProcessingInstructionEvent;
    FOnSkippedEntity: TSkippedEntityEvent;
    FOnStartDocument: TNotifyEvent;
    FOnStartElement: TStartElementEvent;
    FOnStartPrefixMapping: TStartPrefixMappingEvent;
    FOnNotationDecl: TNotationDeclEvent;
    FOnUnparsedEntityDecl: TUnparsedEntityDeclEvent;
    FOnResolveEntity: TResolveEntityEvent;
    FOnError: TErrorEvent;
    FOnFatalError: TFatalErrorEvent;
    FOnWarning: TWarningEvent;
  protected
    FCurColumnNumber, FCurLineNumber: Integer;
    FCurPublicID, FCurSystemID: SAXString;

    function GetFeature(const Name: String): Boolean; dynamic; abstract;
    function GetProperty(const Name: String): TObject; dynamic; abstract;
    procedure SetFeature(const Name: String; Value: Boolean); dynamic; abstract;
    procedure SetProperty(const Name: String; Value: TObject); dynamic; abstract;

    // Notification of the content of a document
    procedure DoCharacters(const ch: PSAXChar; AStart, ALength: Integer); dynamic;
    procedure DoComment(const ch: PSAXChar; AStart, ALength: Integer); dynamic;
    procedure DoEndDocument; dynamic;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); dynamic;
    procedure DoEndPrefixMapping(const Prefix: SAXString); dynamic;
    procedure DoIgnorableWhitespace(const ch: PSAXChar; AStart, ALength: Integer); dynamic;
    procedure DoProcessingInstruction(const Target, Data: SAXString); dynamic;
    procedure DoSkippedEntity(const Name: SAXString); dynamic;
    procedure DoStartDocument; dynamic;
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes); dynamic;
    procedure DoStartPrefixMapping(const Prefix, URI: SAXString); dynamic;

    // Notification of basic DTD-related events
    procedure DoNotationDecl(const Name, PublicID, SystemID: SAXString); dynamic;
    procedure DoUnparsedEntityDecl(const Name, PublicID,
      SystemID, NotationName: SAXString); dynamic;

    // Resolving entities
    function DoResolveEntity(const PublicID,
      SystemID: SAXString): TSAXInputSource; dynamic;

    // SAX error handlers
    procedure DoError(AException: ESAXParseException); dynamic;
    procedure DoFatalError(AException: ESAXParseException); dynamic;
    procedure DoWarning(AException: ESAXParseException); dynamic;
  public
    procedure Parse(AInput: TSAXInputSource); dynamic; abstract; overload;
    procedure Parse(const SystemID: SAXString); dynamic; overload;
    procedure ParseStream(AStream: TStream);

    // Current location
    property CurColumnNumber: Integer read FCurColumnNumber;
    property CurLineNumber: Integer read FCurLineNumber;
    property CurPublicID: SAXString read FCurPublicID;
    property CurSystemID: SAXString read FCurSystemID;

    property Features[const Name: String]: Boolean read GetFeature write SetFeature;
    property Properties[const Name: String]: TObject read GetProperty write SetProperty;

    // Content handler callbacks
    property OnCharacters: TCharactersEvent read FOnCharacters write FOnCharacters;
    property OnComment: TCommentEvent read FOnComment write FOnComment;
    property OnEndDocument: TNotifyEvent read FOnEndDocument write FOnEndDocument;
    property OnEndElement: TEndElementEvent read FOnEndElement write FOnEndElement;
    property OnEndPrefixMapping: TEndPrefixMappingEvent read FOnEndPrefixMapping write FOnEndPrefixMapping;
    property OnIgnorableWhitespace: TIgnorableWhitespaceEvent read FOnIgnorableWhitespace write FOnIgnorableWhitespace;
    property OnProcessingInstruction: TProcessingInstructionEvent read FOnProcessingInstruction write FOnProcessingInstruction;
    property OnSkippedEntity: TSkippedEntityEvent read FOnSkippedEntity write FOnSkippedEntity;
    property OnStartDocument: TNotifyEvent read FOnStartDocument write FOnStartDocument;
    property OnStartElement: TStartElementEvent read FOnStartElement write FOnStartElement;
    property OnStartPrefixMapping: TStartPrefixMappingEvent read FOnStartPrefixMapping write FOnStartPrefixMapping;
    // DTD handler callbacks
    property OnNotationDecl: TNotationDeclEvent read FOnNotationDecl write FOnNotationDecl;
    property OnUnparsedEntityDecl: TUnparsedEntityDeclEvent read FOnUnparsedEntityDecl write FOnUnparsedEntityDecl;
    // Entity resolver callbacks
    property OnResolveEntity: TResolveEntityEvent read FOnResolveEntity write FOnResolveEntity;
    // Error handler callbacks
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnFatalError: TFatalErrorEvent read FOnFatalError write FOnFatalError;
    property OnWarning: TWarningEvent read FOnWarning write FOnWarning;
  end;


{ TSAXFilter: XML filter }

  TSAXFilter = class(TSAXReader)
  private
    FParent: TSAXReader;
  protected
    procedure DoCharacters(const ch: PSAXChar; AStart, ALength: Integer); override;
    procedure DoEndDocument; override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
    procedure DoEndPrefixMapping(const Prefix: SAXString); override;
    procedure DoIgnorableWhitespace(const ch: PSAXChar; AStart, ALength: Integer); override;
    procedure DoProcessingInstruction(const Target, Data: SAXString); override;
    procedure DoSkippedEntity(const Name: SAXString); override;
    procedure DoStartDocument; override;
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes); override;
    procedure DoStartPrefixMapping(const Prefix, URI: SAXString); override;
    procedure DoNotationDecl(const Name, PublicID, SystemID: SAXString); override;
    procedure DoUnparsedEntityDecl(const Name, PublicID, SystemID, NotationName: SAXString); override;
    function DoResolveEntity(const PublicID, SystemID: SAXString): TSAXInputSource; override;
    procedure DoError(AException: ESAXParseException); override;
    procedure DoFatalError(AException: ESAXParseException); override;
    procedure DoWarning(AException: ESAXParseException); override;
  public
    property Parent: TSAXReader read FParent write FParent;
  end;


// ===================================================================
// ===================================================================

implementation


constructor ESAXAttributeIndexError.Create(Index: Integer);
begin
  inherited CreateFmt(SSAXAttributeIndexError, [Index]);
end;


{ TSAXInputSource }

constructor TSAXInputSource.Create;
begin
  inherited Create;
end;

constructor TSAXInputSource.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

constructor TSAXInputSource.Create(const ASystemID: SAXString);
begin
  inherited Create;
  FSystemID := ASystemID;
end;


{ TSAXAttributes }

constructor TSAXAttributes.Create;
begin
  inherited Create;
  {$IFNDEF UseDynArrays}
  FData := TList.Create;
  {$ENDIF}
end;

constructor TSAXAttributes.Create(Atts: TSAXAttributes);
begin
  inherited Create;
  {$IFNDEF UseDynArrays}
  FData := TList.Create;
  {$ENDIF}
  SetAttributes(Atts);
end;

{$IFNDEF UseDynArrays}
destructor TSAXAttributes.Destroy;
begin
  Clear;
  FData.Free;
  inherited Destroy;
end;
{$ENDIF}

function TSAXAttributes.GetIndex(const QName: SAXString): Integer;
begin
  Result := 0;
  while Result < FLength do
  begin
    if Data[Result]^.QName = QName then
      exit;
    Inc(Result);
  end;
  Result := -1;
end;

function TSAXAttributes.GetIndex(const URI, LocalPart: SAXString): Integer;
begin
  Result := 0;
  while Result < FLength do
  begin
    if (Data[Result]^.URI = URI) and (Data[Result]^.LocalName = LocalPart) then
      exit;
    Inc(Result);
  end;
  Result := -1;
end;

function TSAXAttributes.GetLength: Integer;
begin
  Result := FLength;
end;

function TSAXAttributes.GetLocalName(Index: Integer): SAXString;
begin
  if (Index >= 0) and (Index < FLength) then
    Result := Data[Index]^.LocalName
  else
    SetLength(Result, 0);
end;

function TSAXAttributes.GetQName(Index: Integer): SAXString;
begin
  if (Index >= 0) and (Index < FLength) then
    Result := Data[Index]^.QName
  else
    SetLength(Result, 0);
end;

function TSAXAttributes.GetType(Index: Integer): String;
begin
  if (Index >= 0) and (Index < FLength) then
    Result := Data[Index]^.AttrType
  else
    SetLength(Result, 0);
end;

function TSAXAttributes.GetType(const QName: SAXString): String;
var
  i: Integer;
begin
  for i := 0 to FLength - 1 do
    if Data[i]^.QName = QName then
    begin
      Result := Data[i]^.AttrType;
      exit;
    end;
  SetLength(Result, 0);
end;

function TSAXAttributes.GetType(const URI, LocalName: SAXString): String;
var
  i: Integer;
begin
  for i := 0 to FLength - 1 do
    if (Data[i]^.URI = URI) and (Data[i]^.LocalName = LocalName) then
    begin
      Result := Data[i]^.AttrType;
      exit;
    end;
  SetLength(Result, 0);
end;

function TSAXAttributes.GetURI(Index: Integer): SAXString;
begin
  if (Index >= 0) and (Index < FLength) then
    Result := Data[Index * 5]^.URI
  else
    SetLength(Result, 0);
end;

function TSAXAttributes.GetValue(Index: Integer): SAXString;
begin
  if (Index >= 0) and (Index < FLength) then
    Result := Data[Index]^.Value
  else
    SetLength(Result, 0);
end;

function TSAXAttributes.GetValue(const QName: SAXString): SAXString;
var
  i: Integer;
begin
  for i := 0 to FLength - 1 do
    if Data[i]^.QName = QName then
    begin
      Result := Data[i]^.Value;
      exit;
    end;
  SetLength(Result, 0);
end;

function TSAXAttributes.GetValue(const URI, LocalName: SAXString): SAXString;
var
  i: Integer;
begin
  for i := 0 to FLength - 1 do
    if (Data[i]^.URI = URI) and (Data[i]^.LocalName = LocalName) then
    begin
      Result := Data[i]^.Value;
      exit;
    end;
  SetLength(Result, 0);
end;

procedure TSAXAttributes.Clear;
{$IFDEF UseDynArrays}
begin
  SetLength(Data, 0);
end;
{$ELSE}
var
  i: Integer;
  p: PSAXAttributeData;
begin
  for i := 0 to FData.Count - 1 do
  begin
    p := PSAXAttributeData(FData[i]);
    Dispose(p);
  end;
end;
{$ENDIF}

procedure TSAXAttributes.SetAttributes(Atts: TSAXAttributes);
var
  i: Integer;
begin
  FLength := Atts.Length;
  {$IFDEF UseDynArrays}
  SetLength(Data, FLength);
  {$ELSE}
  FData.Count := FLength;
  {$ENDIF}
  for i := 0 to FLength - 1 do
    {$IFDEF UseDynArrays}
    with Data[i] do
    {$ELSE}
    with Data[i]^ do
    {$ENDIF}
    begin
      URI := Atts.URIs[i];
      LocalName := Atts.LocalNames[i];
      QName := Atts.QNames[i];
      AttrType := Atts.Types[i];
      Value := Atts.Values[i];
    end;
end;

procedure TSAXAttributes.AddAttribute(const AURI, ALocalName, AQName: SAXString;
  const AType: String; const AValue: SAXString);
{$IFNDEF UseDynArrays}
var
  p: PSAXAttributeData;
{$ENDIF}
begin
  Inc(FLength);
  {$IFDEF UseDynArrays}
  SetLength(Data, FLength);
  {$ELSE}
  New(p);
  FData.Add(p);
  {$ENDIF}
  {$IFDEF UseDynArrays}
  with Data[FLength - 1] do
  {$ELSE}
  with Data[FLength - 1]^ do
  {$ENDIF}
  begin
    URI := AURI;
    LocalName := ALocalName;
    QName := AQName;
    AttrType := AType;
    Value := AValue;
  end;
end;

procedure TSAXAttributes.SetAttribute(Index: Integer;
  const AURI, ALocalName, AQName: SAXString; const AType: String;
  const AValue: SAXString);
begin
  if (Index >= 0) and (Index < FLength) then
    {$IFDEF UseDynArrays}
    with Data[Index] do
    {$ELSE}
    with Data[Index]^ do
    {$ENDIF}
    begin
      URI := AURI;
      LocalName := ALocalName;
      QName := AQName;
      AttrType := AType;
      Value := AValue;
    end
  else
    BadIndex(Index);
end;

procedure TSAXAttributes.RemoveAttribute(Index: Integer);
{$IFDEF UseDynArrays}
var
  i: Integer;
{$ENDIF}
begin
  if (Index >= 0) and (Index < FLength) then
  begin
    {$IFDEF UseDynArrays}
    for i := Index to FLength - 1 do
      Data[i] := Data[i + 1];
    Dec(FLength);
    SetLength(Data, FLength);
    {$ELSE}
    FData.Delete(Index);
    Dec(FLength);
    {$ENDIF}
  end else
    BadIndex(Index);
end;

procedure TSAXAttributes.SetURI(Index: Integer; const AURI: SAXString);
begin
  if (Index >= 0) and (Index < FLength) then
    Data[Index]^.URI := AURI
  else
    BadIndex(Index);
end;

procedure TSAXAttributes.SetLocalName(Index: Integer;
  const ALocalName: SAXString);
begin
  if (Index >= 0) and (Index < FLength) then
    Data[Index]^.LocalName := ALocalName
  else
    BadIndex(Index);
end;

procedure TSAXAttributes.SetQName(Index: Integer; const AQName: SAXString);
begin
  if (Index >= 0) and (Index < FLength) then
    Data[Index]^.QName := AQName
  else
    BadIndex(Index);
end;

procedure TSAXAttributes.SetType(Index: Integer; const AType: String);
begin
  if (Index >= 0) and (Index < FLength) then
    Data[Index]^.AttrType := AType
  else
    BadIndex(Index);
end;

procedure TSAXAttributes.SetValue(Index: Integer; const AValue: SAXString);
begin
  if (Index >= 0) and (Index < FLength) then
    Data[Index]^.Value := AValue
  else
    BadIndex(Index);
end;

{$IFNDEF UseDynArrays}
function TSAXAttributes.GetData(Index: Integer): PSAXAttributeData;
begin
  Result := PSAXAttributeData(FData[Index]);
end;
{$ENDIF}

procedure TSAXAttributes.BadIndex(Index: Integer);
begin
  raise ESAXAttributeIndexError.Create(Index) at pointer(get_caller_addr(get_frame));
end;


{ TSAXReader }

procedure TSAXReader.Parse(const SystemID: SAXString);
var
  Input: TSAXInputSource;
begin
  Input := TSAXInputSource.Create(SystemID);
  try
    Input.Stream := TFileStream.Create(SystemID, fmOpenRead);
    try
      Parse(Input);
    finally
      Input.Stream.Free;
    end;
  finally
    Input.Free;
  end;
end;

procedure TSAXReader.ParseStream(AStream: TStream);
var
  Input: TSAXInputSource;
begin
  Input := TSAXInputSource.Create(AStream);
  try
    Parse(Input);
  finally
    Input.Free;
  end;
end;

function TSAXReader.DoResolveEntity(const PublicID,
  SystemID: SAXString): TSAXInputSource;
begin
  if Assigned(OnResolveEntity) then
    Result := OnResolveEntity(Self, PublicID, SystemID)
  else
    Result := nil;
end;

procedure TSAXReader.DoNotationDecl(const Name, PublicID, SystemID: SAXString);
begin
  if Assigned(OnNotationDecl) then
    OnNotationDecl(Self, Name, PublicID, SystemID);
end;

procedure TSAXReader.DoUnparsedEntityDecl(const Name, PublicID,
  SystemID, NotationName: SAXString);
begin
  if Assigned(OnUnparsedEntityDecl) then
    OnUnparsedEntityDecl(Self, Name, PublicID, SystemID, NotationName);
end;

procedure TSAXReader.DoCharacters(const ch: PSAXChar;
  AStart, ALength: Integer);
begin
  if Assigned(OnCharacters) then
    OnCharacters(Self, ch, AStart, ALength);
end;

procedure TSAXReader.DoComment(const ch: PSAXChar;
  AStart, ALength: Integer);
begin
  if Assigned(OnComment) then
    OnComment(Self, ch, AStart, ALength);
end;

procedure TSAXReader.DoEndDocument;
begin
  if Assigned(OnEndDocument) then
    OnEndDocument(Self);
end;

procedure TSAXReader.DoEndElement(const NamespaceURI,
  LocalName, QName: SAXString);
begin
  if Assigned(OnEndElement) then
    OnEndElement(Self, NamespaceURI, LocalName, QName);
end;

procedure TSAXReader.DoEndPrefixMapping(const Prefix: SAXString);
begin
  if Assigned(OnEndPrefixMapping) then
    OnEndPrefixMapping(Self, Prefix);
end;

procedure TSAXReader.DoIgnorableWhitespace(const ch: PSAXChar;
  AStart, ALength: Integer);
begin
  if Assigned(OnIgnorableWhitespace) then
    OnIgnorableWhitespace(Self, ch, AStart, ALength);
end;

procedure TSAXReader.DoProcessingInstruction(const Target,
  Data: SAXString);
begin
  if Assigned(OnProcessingInstruction) then
    OnProcessingInstruction(Self, Target, Data);
end;

procedure TSAXReader.DoSkippedEntity(const Name: SAXString);
begin
  if Assigned(OnSkippedEntity) then
    OnSkippedEntity(Self, Name);
end;

procedure TSAXReader.DoStartDocument;
begin
  if Assigned(OnStartDocument) then
    OnStartDocument(Self);
end;

procedure TSAXReader.DoStartElement(const NamespaceURI,
  LocalName, QName: SAXString; Atts: TSAXAttributes);
begin
  if Assigned(OnStartElement) then
    OnStartElement(Self, NamespaceURI, LocalName, QName, Atts);
end;

procedure TSAXReader.DoStartPrefixMapping(const Prefix, URI: SAXString);
begin
  if Assigned(OnStartPrefixMapping) then
    OnStartPrefixMapping(Self, Prefix, URI);
end;

procedure TSAXReader.DoError(AException: ESAXParseException);
begin
  if Assigned(OnError) then
    OnError(Self, AException);
  AException.Free;
end;

procedure TSAXReader.DoFatalError(AException: ESAXParseException);
begin
  if Assigned(OnFatalError) then
    OnFatalError(Self, AException)
  else
    raise AException;
  AException.Free;
end;

procedure TSAXReader.DoWarning(AException: ESAXParseException);
begin
  if Assigned(OnWarning) then
    OnWarning(Self, AException);
  AException.Free;
end;


{ TSAXFilter }

function TSAXFilter.DoResolveEntity(const PublicID,
  SystemID: SAXString): TSAXInputSource;
begin
  if Assigned(OnResolveEntity) then
    Result := OnResolveEntity(Self, PublicID, SystemID)
  else if Assigned(Parent) then
    Result := Parent.DoResolveEntity(PublicID, SystemID)
  else
    Result := nil;
end;

procedure TSAXFilter.DoNotationDecl(const Name, PublicID, SystemID: SAXString);
begin
  if Assigned(OnNotationDecl) then
    OnNotationDecl(Self, Name, PublicID, SystemID)
  else if Assigned(Parent) then
    Parent.DoNotationDecl(Name, PublicID, SystemID);
end;

procedure TSAXFilter.DoUnparsedEntityDecl(const Name, PublicID,
  SystemID, NotationName: SAXString);
begin
  if Assigned(OnUnparsedEntityDecl) then
    OnUnparsedEntityDecl(Self, Name, PublicID, SystemID, NotationName)
  else if Assigned(Parent) then
    Parent.DoUnparsedEntityDecl(Name, PublicID, SystemID, NotationName);
end;

procedure TSAXFilter.DoCharacters(const ch: PSAXChar;
  AStart, ALength: Integer);
begin
  if Assigned(OnCharacters) then
    OnCharacters(Self, ch, AStart, ALength)
  else if Assigned(Parent) then
    Parent.DoCharacters(ch, AStart, ALength);
end;

procedure TSAXFilter.DoEndDocument;
begin
  if Assigned(OnEndDocument) then
    OnEndDocument(Self)
  else if Assigned(Parent) then
    Parent.DoEndDocument;
end;

procedure TSAXFilter.DoEndElement(const NamespaceURI,
  LocalName, QName: SAXString);
begin
  if Assigned(OnEndElement) then
    OnEndElement(Self, NamespaceURI, LocalName, QName)
  else if Assigned(Parent) then
    Parent.DoEndElement(NamespaceURI, LocalName, QName);
end;

procedure TSAXFilter.DoEndPrefixMapping(const Prefix: SAXString);
begin
  if Assigned(OnEndPrefixMapping) then
    OnEndPrefixMapping(Self, Prefix)
  else if Assigned(Parent) then
    Parent.DoEndPrefixMapping(Prefix);
end;

procedure TSAXFilter.DoIgnorableWhitespace(const ch: PSAXChar;
  AStart, ALength: Integer);
begin
  if Assigned(OnIgnorableWhitespace) then
    OnIgnorableWhitespace(Self, ch, AStart, ALength)
  else if Assigned(Parent) then
    Parent.DoIgnorableWhitespace(ch, AStart, ALength);
end;

procedure TSAXFilter.DoProcessingInstruction(const Target,
  Data: SAXString);
begin
  if Assigned(OnProcessingInstruction) then
    OnProcessingInstruction(Self, Target, Data)
  else if Assigned(Parent) then
    Parent.DoProcessingInstruction(Target, Data);
end;

procedure TSAXFilter.DoSkippedEntity(const Name: SAXString);
begin
  if Assigned(OnSkippedEntity) then
    OnSkippedEntity(Self, Name)
  else if Assigned(Parent) then
    Parent.DoSkippedEntity(Name);
end;

procedure TSAXFilter.DoStartDocument;
begin
  if Assigned(OnStartDocument) then
    OnStartDocument(Self)
  else if Assigned(Parent) then
    Parent.DoStartDocument;
end;

procedure TSAXFilter.DoStartElement(const NamespaceURI,
  LocalName, QName: SAXString; Atts: TSAXAttributes);
begin
  if Assigned(OnStartElement) then
    OnStartElement(Self, NamespaceURI, LocalName, QName, Atts)
  else if Assigned(Parent) then
    Parent.DoStartElement(NamespaceURI, LocalName, QName, Atts);
end;

procedure TSAXFilter.DoStartPrefixMapping(const Prefix, URI: SAXString);
begin
  if Assigned(OnStartPrefixMapping) then
    OnStartPrefixMapping(Self, Prefix, URI)
  else if Assigned(Parent) then
    Parent.DoStartPrefixMapping(Prefix, URI);
end;

procedure TSAXFilter.DoError(AException: ESAXParseException);
begin
  if Assigned(OnError) then
    OnError(Self, AException)
  else if Assigned(Parent) then
    Parent.DoError(AException);
  AException.Free;
end;

procedure TSAXFilter.DoFatalError(AException: ESAXParseException);
begin
  if Assigned(OnFatalError) then
    OnFatalError(Self, AException)
  else if Assigned(Parent) then
    Parent.DoFatalError(AException)
  else
    raise AException;
  AException.Free;
end;

procedure TSAXFilter.DoWarning(AException: ESAXParseException);
begin
  if Assigned(OnWarning) then
    OnWarning(Self, AException)
  else if Assigned(Parent) then
    Parent.DoWarning(AException);
  AException.Free;
end;


end.
