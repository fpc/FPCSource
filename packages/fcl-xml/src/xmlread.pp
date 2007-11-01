{
    This file is part of the Free Component Library

    XML reading routines.
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org
    Modified in 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit XMLRead;

{$ifdef fpc}
{$MODE objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, DOM;

type
  TErrorSeverity = (esWarning, esError, esFatal);

  EXMLReadError = class(Exception)
  private
    FSeverity: TErrorSeverity;
    FErrorMessage: string;
    FLine: Integer;
    FLinePos: Integer;
  public
    property Severity: TErrorSeverity read FSeverity;
    property ErrorMessage: string read FErrorMessage;
    property Line: Integer read FLine;
    property LinePos: Integer read FLinePos;
  end;

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: Text); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream; const ABaseURI: String); overload;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: Text); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const ABaseURI: String); overload;

procedure ReadDTDFile(out ADoc: TXMLDocument; const AFilename: String);  overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: Text); overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream; const ABaseURI: String); overload;

type
  TDOMParseOptions = class(TObject)
  private
    FValidate: Boolean;
    FPreserveWhitespace: Boolean;
    FExpandEntities: Boolean;
    FIgnoreComments: Boolean;
    FCDSectionsAsText: Boolean;
  public
    property Validate: Boolean read FValidate write FValidate;
    property PreserveWhitespace: Boolean read FPreserveWhitespace write FPreserveWhitespace;
    property ExpandEntities: Boolean read FExpandEntities write FExpandEntities;
    property IgnoreComments: Boolean read FIgnoreComments write FIgnoreComments;
    property CDSectionsAsText: Boolean read FCDSectionsAsText write FCDSectionsAsText;
  end;

  // NOTE: DOM 3 LS ACTION_TYPE enumeration starts at 1
  TXMLContextAction = (xaAppendAsChildren, xaReplaceChildren, xaInsertBefore,
                       xaInsertAfter, xaReplace);

  TXMLErrorEvent = procedure(Error: EXMLReadError) of object;

  // This may be augmented with ByteOffset, UTF8Offset, etc.
  TLocation = record
    Line: Integer;
    LinePos: Integer;
  end;

  TXMLInputSource = class(TObject)
  private
    FStream: TStream;
    FStringData: string;
//    FBaseURI: WideString;
    FSystemID: WideString;
    FPublicID: WideString;
//    FEncoding: string;
  public
    constructor Create(AStream: TStream); overload;
    constructor Create(const AStringData: string); overload;
    property Stream: TStream read FStream;
    property StringData: string read FStringData;
//    property BaseURI: WideString read FBaseURI write FBaseURI;
    property SystemID: WideString read FSystemID write FSystemID;
    property PublicID: WideString read FPublicID write FPublicID;
//    property Encoding: string read FEncoding write FEncoding;
  end;

  TDOMParser = class(TObject)
  private
    FOptions: TDOMParseOptions;
    FOnError: TXMLErrorEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Src: TXMLInputSource; out ADoc: TXMLDocument);
    procedure ParseUri(const URI: WideString; out ADoc: TXMLDocument);
    function ParseWithContext(Src: TXMLInputSource; Context: TDOMNode;
      Action: TXMLContextAction): TDOMNode;
    property Options: TDOMParseOptions read FOptions;
    property OnError: TXMLErrorEvent read FOnError write FOnError;
  end;


// =======================================================

implementation

uses
  UriParser, xmlutils;

const
  PubidChars: TSetOfChar = [' ', #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
    '#', '@', '$', '_', '%'];

type
  TDOMNotationEx = class(TDOMNotation);
  TDOMDocumentTypeEx = class(TDOMDocumentType);
  TDOMElementDef = class;
  TDOMAttrDef = class;

  TDOMEntityEx = class(TDOMEntity)
  protected
    FExternallyDeclared: Boolean;
    FResolved: Boolean;
    FOnStack: Boolean;
    FReplacementText: DOMString;
    FStartLocation: TLocation;
  end;

  TDecoder = class;
  TDecoderRef = class of TDecoder;

  TXMLCharSource = class(TObject)
  private
    FBuf: PChar;
    FBufEnd: PChar;
    FEof: Boolean;
    FSurrogate: WideChar;
    FReader: TObject;   // weak reference
    FParent: TXMLCharSource;
    FEntity: TObject;   // weak reference
    FCursor: TObject;   // weak reference
    FSavedLocation: TLocation;
    FSystemID: WideString;
    FPublicID: WideString;
    function GetSystemID: WideString;
    function GetPublicID: WideString;
  public
    constructor Create(const AData: WideString);
    function NextChar: WideChar; virtual;
    procedure Initialize; virtual;
    function SetEncoding(const AEncoding: string): Boolean; virtual;
    property SystemID: WideString read GetSystemID write FSystemID;
    property PublicID: WideString read GetPublicID write FPublicID;
  end;

  TXMLDecodingSource = class(TXMLCharSource)
  private
    FDecoder: TDecoder;
    FSeenCR: Boolean;
    function InternalNextChar: WideChar;
    procedure DecodingError(const Msg: string);
  protected
    procedure FetchData; virtual;
  public
    destructor Destroy; override;
    function NextChar: WideChar; override;
    function SetEncoding(const AEncoding: string): Boolean; override;
    procedure Initialize; override;
  end;

  TXMLStreamInputSource = class(TXMLDecodingSource)
  private
    FAllocated: PChar;
    FStream: TStream;
    FBufSize: Integer;
    FOwnStream: Boolean;
  public
    constructor Create(AStream: TStream; AOwnStream: Boolean);
    destructor Destroy; override;
    procedure FetchData; override;
  end;

  TXMLFileInputSource = class(TXMLDecodingSource)
  private
    FFile: ^Text;
    FString: string;
  public
    constructor Create(var AFile: Text);
    procedure FetchData; override;
  end;

  TDecoder = class
  private
    FSource: TXMLDecodingSource;
  public
    constructor Create(ASource: TXMLDecodingSource);
    function DecodeNext: WideChar; virtual; abstract;
    class function Supports(const AEncoding: string): Boolean; virtual; abstract;
  end;

  TISO8859_1Decoder = class(TDecoder)
  public
    function DecodeNext: WideChar; override;
    class function Supports(const AEncoding: string): Boolean; override;
  end;

  TUCS2Decoder = class(TDecoder)
  private
    FSwapEndian: Boolean;
    FEncoding: string;
  public
    function DecodeNext: WideChar; override;
    class function Supports(const AEncoding: string): Boolean; override;
  end;

  TUTF8Decoder = class(TDecoder)
  public
    function DecodeNext: WideChar; override;
    class function Supports(const AEncoding: string): Boolean; override;
  end;

  PWideCharBuf = ^TWideCharBuf;
  TWideCharBuf = record
    Buffer: PWideChar;
    Length: Integer;
    MaxLength: Integer;
  end;

  PWideStrWrapper = ^TWideStrWrapper;
  TWideStrWrapper = record
    Value: WideString;
  end;

  TDeclType = (dtNone, dtXml, dtText);

  TCPType = (ctName, ctChoice, ctSeq);
  TCPQuant = (cqOnce, cqZeroOrOnce, cqZeroOrMore, cqOnceOrMore);

  TContentParticle = class(TObject)
  private
    FParent: TContentParticle;
    FChildren: TList;
    FIndex: Integer;
    function GetChildCount: Integer;
    function GetChild(Index: Integer): TContentParticle;
  public
    CPType: TCPType;
    CPQuant: TCPQuant;
    Name: WideString;
    destructor Destroy; override;
    function Add: TContentParticle;
    function IsRequired: Boolean;
    function FindFirst(const aName: DOMString): TContentParticle;
    function FindNext(const aName: DOMString; ChildIdx: Integer): TContentParticle;
    function MoreRequired(ChildIdx: Integer): Boolean;
    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TContentParticle read GetChild;
  end;

  TElementValidator = class(TObject)
  private
    FParent: TElementValidator;
    FElementDef: TDOMElementDef;
    FCurCP: TContentParticle;
    FFailed: Boolean;
  public
    function IsElementAllowed(const aName: DOMString): Boolean;
    function Incomplete: Boolean;
    property Parent: TElementValidator read FParent write FParent;
  end;

  TXMLReader = class
  private
    FSource: TXMLCharSource;
    FCtrl: TDOMParser;
    FCurChar: WideChar;
    FXML11: Boolean;
    FIntSubset: Boolean;
    FDtdParsed: Boolean;
    FInsideRoot: Boolean;
    FRecognizePE: Boolean;
    FHavePERefs: Boolean;
    FDocNotValid: Boolean;
    FValue: TWideCharBuf;
    FName: TWideCharBuf;
    FCopyBuf: PWideCharBuf;
    FAllowedDecl: TDeclType;
    FLocation: TLocation;
    FTokenStart: TLocation;
    FStandalone: Boolean;          // property of Doc ?
    FNamePages: PByteArray;
    FForbiddenAscii: TSetOfChar;
    FDocType: TDOMDocumentTypeEx;  // a shortcut
    FPEMap: TDOMNamedNodeMap;
    FIDRefs: TList;

    FValidate: Boolean;            // parsing options, copy of FCtrl.Options
    FPreserveWhitespace: Boolean;
    FExpandEntities: Boolean;
    FIgnoreComments: Boolean;
    FCDSectionsAsText: Boolean;

    procedure RaiseExpectedQmark;
    procedure GetChar;
    procedure GetCharRaw;
    procedure UngetCurChar;
    procedure Initialize(ASource: TXMLCharSource);
    procedure InitializeRoot(ASource: TXMLCharSource);
    procedure DoParseAttValue(Delim: WideChar);
    procedure DoParseFragment;
    procedure DoParseExtSubset(ASource: TXMLCharSource);
    function ContextPush(AEntity: TDOMEntityEx): Boolean;
    function ContextPop: Boolean;
    procedure XML11_BuildTables;
    function  XML11_CheckName: Boolean;
    procedure ParseQuantity(CP: TContentParticle);
    procedure MarkTokenStart;
    function ValidateAttrSyntax(AttrDef: TDOMAttrDef; const aValue: WideString): Boolean;
    procedure AddIdRef(Buf: PWideChar; Length: Integer);
    procedure ClearIdRefs;
    procedure ValidateIdRefs;
    procedure StandaloneError;
    procedure CallErrorHandler(E: EXMLReadError);
  protected
    FCursor: TDOMNode;
    FValidator: TElementValidator;

    procedure DoError(Severity: TErrorSeverity; const descr: string; AtTokenStart: Boolean=False);
    procedure FatalError(const descr: String; AtTokenStart: Boolean=False); overload;
    procedure FatalError(const descr: string; const args: array of const; AtTokenStart: Boolean=False); overload;
    procedure FatalError(Expected: WideChar); overload;
    function  SkipWhitespace: Boolean;
    function  SkipWhitespaceRaw: Boolean;
    procedure ExpectWhitespace;
    procedure ExpectString(const s: String);
    procedure ExpectChar(wc: WideChar);
    function  CheckForChar(c: WideChar): Boolean;
    procedure SkipString(const ValidChars: TSetOfChar);
    function  GetString(const ValidChars: TSetOfChar): WideString;

    procedure RaiseNameNotFound;
    function  CheckName: Boolean;
    function  CheckNmToken: Boolean;
    function  ExpectName: WideString;                                   // [5]
    function SkipQuotedLiteral: Boolean;
    procedure ExpectAttValue;                                           // [10]
    procedure SkipPubidLiteral;                                         // [12]
    procedure SkipSystemLiteral(out Literal: WideString);
    procedure ParseComment;                                             // [15]
    procedure ParsePI;                                                  // [16]
    procedure ParseCDSect;                                              // [18]
    procedure ParseXmlOrTextDecl(TextDecl: Boolean);
    function  ParseEq: Boolean;                                         // [25]
    procedure ExpectEq;
    procedure ParseDoctypeDecl;                                         // [28]
    procedure ParseMarkupDecl;                                          // [29]
    procedure ParseElement;                                             // [39]
    procedure ParseContent;                                             // [43]
    function  ResolvePredefined(const RefName: WideString): WideChar;
    procedure IncludeEntity(AEntity: TDOMEntityEx; InAttr: Boolean);
    procedure StartPE;
    function  ParseCharRef: Boolean;                                    // [66]
    function  ParseReference: TDOMEntityEx;                             // [67]
    function  ParsePEReference: Boolean;                                // [69]
    function  ParseExternalID(out SysID, PubID: WideString;             // [75]
      SysIdOptional: Boolean): Boolean;
    procedure ProcessTextAndRefs;

    procedure BadPENesting;
    procedure ParseEntityDecl;
    procedure ParseEntityDeclValue(Delim: WideChar);
    procedure ParseAttlistDecl;
    procedure ExpectChoiceOrSeq(CP: TContentParticle);
    procedure ParseElementDecl;
    procedure ParseNotationDecl;
    function ResolveEntity(const SystemID, PublicID: WideString; out Source: TXMLCharSource): Boolean;
    procedure ProcessDefaultAttributes(Element: TDOMElement);

    procedure PushVC(aElDef: TDOMElementDef);
    procedure PopVC;
    function  CurrentElementDef: TDOMElementDef;
    procedure ValidateDTD;
    procedure ValidationError(const Msg: string; const args: array of const);
    procedure CheckNotation(const Name: WideString);
    procedure DoAttrText(ch: PWideChar; Count: Integer);    
    // Some SAX-alike stuff (at a very early stage)
    procedure DoText(ch: PWideChar; Count: Integer; Whitespace: Boolean=False);
    procedure DoComment(ch: PWideChar; Count: Integer);
    procedure DoCDSect(ch: PWideChar; Count: Integer);
    procedure DoNotationDecl(const aName, aPubID, aSysID: WideString);
  public
    doc: TDOMDocument;
    constructor Create; overload;
    constructor Create(AParser: TDOMParser); overload;
    destructor Destroy; override;
    procedure ProcessXML(ASource: TXMLCharSource);                // [1]
    procedure ProcessFragment(ASource: TXMLCharSource; AOwner: TDOMNode);
    procedure ProcessDTD(ASource: TXMLCharSource);               // ([29])
  end;

  // Attribute/Element declarations

  TAttrDefault = (
    adImplied,
    adDefault,
    adRequired,
    adFixed
  );

  TElementContentType = (
    ctEmpty,
    ctAny,
    ctMixed,
    ctChildren
  );

  TDOMAttrDef = class(TDOMAttr)
  protected
    FExternallyDeclared: Boolean;
    FDefault: TAttrDefault;
    FEnumeration: array of WideString;
    function AddEnumToken(const aValue: WideString): Boolean;
    function HasEnumToken(const aValue: WideString): Boolean;
  end;

  TDOMElementDef = class(TDOMElement)
  public
    FExternallyDeclared: Boolean;
    ContentType: TElementContentType;
    HasElementDecl: Boolean;
    RootCP: TContentParticle;
    constructor Create(aOwner: TDOMDocument);
    destructor Destroy; override;
  end;

// TODO: List of registered/supported decoders
function FindDecoder(const Encoding: string): TDecoderRef;
begin
  if TISO8859_1Decoder.Supports(Encoding) then
    Result := TISO8859_1Decoder
  else
    Result := nil;
end;


procedure BufAllocate(var ABuffer: TWideCharBuf; ALength: Integer);
begin
  ABuffer.MaxLength := ALength;
  ABuffer.Length := 0;
  ABuffer.Buffer := AllocMem(ABuffer.MaxLength*SizeOf(WideChar));
end;

procedure BufAppend(var ABuffer: TWideCharBuf; wc: WideChar);
var
  OldLength: Integer;
begin
  if ABuffer.Length >= ABuffer.MaxLength then
  begin
    OldLength := ABuffer.MaxLength;
    ABuffer.MaxLength := ABuffer.MaxLength * 2;
    ReallocMem(ABuffer.Buffer, ABuffer.MaxLength * SizeOf(WideChar));
    FillChar(ABuffer.Buffer[OldLength],(ABuffer.MaxLength-OldLength) * SizeOf(WideChar),0);
  end;
  ABuffer.Buffer[ABuffer.Length] := wc;
  Inc(ABuffer.Length);
end;

{ TXMLInputSource }

constructor TXMLInputSource.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

constructor TXMLInputSource.Create(const AStringData: string);
begin
  inherited Create;
  FStringData := AStringData;
end;

{ TDOMParser }

constructor TDOMParser.Create;
begin
  FOptions := TDOMParseOptions.Create;
end;

destructor TDOMParser.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

procedure TDOMParser.Parse(Src: TXMLInputSource; out ADoc: TXMLDocument);
var
  InputSrc: TXMLCharSource;
begin
  with TXMLReader.Create(Self) do
  try
    InputSrc := nil;
    if Assigned(Src) then
    begin
      if Assigned(Src.FStream) then
        InputSrc := TXMLStreamInputSource.Create(Src.FStream, False)
      else if Src.FStringData <> '' then
        InputSrc := TXMLStreamInputSource.Create(TStringStream.Create(Src.FStringData), True)
      else if (Src.SystemID <> '') then
        ResolveEntity(Src.SystemID, Src.PublicID, InputSrc);
    end;
    if Assigned(InputSrc) then
      ProcessXML(InputSrc)
    else
      FatalError('No input source specified');
  finally
    ADoc := TXMLDocument(doc);
    Free;
  end;
end;

procedure TDOMParser.ParseUri(const URI: WideString; out ADoc: TXMLDocument);
var
  Src: TXMLCharSource;
begin
  ADoc := nil;
  with TXMLReader.Create(Self) do
  try
    if ResolveEntity(URI, '', Src) then
      ProcessXML(Src);
  finally
    ADoc := TXMLDocument(doc);
    Free;
  end;
end;

function TDOMParser.ParseWithContext(Src: TXMLInputSource;
  Context: TDOMNode; Action: TXMLContextAction): TDOMNode;
begin
  // TODO: implement
  Result := nil;
end;

// TODO: These classes still cannot be considered as the final solution...

{ TXMLInputSource }

constructor TXMLCharSource.Create(const AData: WideString);
begin
  inherited Create;
  FBuf := PChar(PWideChar(AData));
  FBufEnd := FBuf + Length(AData) * sizeof(WideChar);
end;

procedure TXMLCharSource.Initialize;
begin
end;

function TXMLCharSource.NextChar: WideChar;
begin
  Result := FSurrogate;
  FSurrogate := #0;
  if Result <> #0 then
    Exit
  else if FBufEnd <= FBuf then
  begin
    Result := #0;
    FEof := True;
  end
  else
  begin
    Result := PWideChar(FBuf)^;
    Inc(FBuf, sizeof(WideChar));
  end;
end;

function TXMLCharSource.SetEncoding(const AEncoding: string): Boolean;
begin
  Result := True; // always succeed
end;

function TXMLCharSource.GetPublicID: WideString;
begin
  if FPublicID <> '' then
    Result := FPublicID
  else if Assigned(FParent) then
    Result := FParent.PublicID
  else
    Result := '';
end;

function TXMLCharSource.GetSystemID: WideString;
begin
  if FSystemID <> '' then
    Result := FSystemID
  else if Assigned(FParent) then
    Result := FParent.SystemID
  else
    Result := '';
end;

{ TXMLDecodingSource }

destructor TXMLDecodingSource.Destroy;
begin
  FDecoder.Free;
  inherited Destroy;
end;

procedure TXMLDecodingSource.DecodingError(const Msg: string);
begin
  TXMLReader(FReader).FatalError(Msg);
end;

procedure TXMLDecodingSource.FetchData;
begin
  FEof := True;
end;

function TXMLDecodingSource.InternalNextChar: WideChar;
begin
  // TODO: find a place for it, finally...
  Result := FSurrogate;
  FSurrogate := #0;
  if Result <> #0 then
    Exit;
  if FBufEnd <= FBuf then
    FetchData;
  if not FEof then
    Result := FDecoder.DecodeNext;
end;

function TXMLDecodingSource.NextChar: WideChar;
begin
  Result := InternalNextChar;
  if FSeenCR then
  begin
    if (Result = #10) or ((Result = #$85) and TXMLReader(FReader).FXML11) then
      Result := InternalNextChar;
    FSeenCR := False;
  end;
  case Result of
    #13: begin
           FSeenCR := True;
           Result := #10;
         end;

    #$85, #$2028:
      if TXMLReader(FReader).FXML11 then
        Result := #10;
  end;
  if (Result < #256) and (char(ord(Result)) in TXMLReader(FReader).FForbiddenAscii) or
    (Result >= #$FFFE) then
    DecodingError('Invalid character');
end;

procedure TXMLDecodingSource.Initialize;
begin
  inherited;
  if FBufEnd-FBuf > 1 then
  repeat
    if (FBuf[0] = #$FE) and (FBuf[1] = #$FF) then         // BE
    begin
      FDecoder := TUCS2Decoder.Create(Self);
      TUCS2Decoder(FDecoder).FEncoding := 'UTF-16BE';
      {$IFNDEF ENDIAN_BIG}
      TUCS2Decoder(FDecoder).FSwapEndian := True;
      {$ENDIF}
      Exit;
    end
    else if (FBuf[0] = #$FF) and (FBuf[1] = #$FE) then    // LE
    begin
      FDecoder := TUCS2Decoder.Create(Self);
      TUCS2Decoder(FDecoder).FEncoding := 'UTF-16LE';
      {$IFDEF ENDIAN_BIG}
      TUCS2Decoder(FDecoder).FSwapEndian := True;
      {$ENDIF}
      Exit;
    end
    else
      Break;
  until False;
  FDecoder := TUTF8Decoder.Create(Self);
end;

function TXMLDecodingSource.SetEncoding(const AEncoding: string): Boolean;
var
  NewDecoder: TDecoderRef;
begin
  Result := True;
  if FDecoder.Supports(AEncoding) then // no change needed
    Exit;
  // hardcoded stuff - special case of UCS2
  if FDecoder is TUCS2Decoder then
  begin
    // check for 'UTF-16LE' or 'UTF-16BE'
    Result := SameText(AEncoding, TUCS2Decoder(FDecoder).FEncoding);
    Exit;
  end;
  NewDecoder := FindDecoder(AEncoding);
  if Assigned(NewDecoder) then
  begin
    FDecoder.Free;
    FDecoder := NewDecoder.Create(Self);
  end
  else
    Result := False;
end;


{ TXMLStreamInputSource }

constructor TXMLStreamInputSource.Create(AStream: TStream; AOwnStream: Boolean);
begin
  FStream := AStream;
  FBufSize := 4096;
  GetMem(FAllocated, FBufSize+8);
  FBuf := FAllocated+8;
  FBufEnd := FBuf;
  FOwnStream := AOwnStream;
  FetchData;
end;

destructor TXMLStreamInputSource.Destroy;
begin
  FreeMem(FAllocated);
  if FOwnStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TXMLStreamInputSource.FetchData;
var
  Remainder, BytesRead: Integer;
  OldBuf: PChar;
begin
  Assert(FBufEnd - FBuf < 8);

  OldBuf := FBuf;
  Remainder := FBufEnd - FBuf;
  FBuf := FAllocated+8-Remainder;
  Move(OldBuf^, FBuf^, Remainder);
  BytesRead := FStream.Read(FAllocated[8], FBufSize);
  if BytesRead = 0 then
    FEof := True;
  FBufEnd := FAllocated + 8 + BytesRead;
end;

{ TXMLFileInputSource }

constructor TXMLFileInputSource.Create(var AFile: Text);
begin
  FFile := @AFile;
  ReadLn(FFile^, FString);
  FBuf := PChar(FString);
  FBufEnd := FBuf + Length(FString);
end;

procedure TXMLFileInputSource.FetchData;
begin
  FEof := Eof(FFile^);
  if not FEof then
  begin
    ReadLn(FFile^, FString);
    FString := FString + #10;    // bad solution...
    FBuf := PChar(FString);
    FBufEnd := FBuf + Length(FString);
  end;
end;


{ TDecoder }

constructor TDecoder.Create(ASource: TXMLDecodingSource);
begin
  inherited Create;
  FSource := ASource;
end;

{ TISO8859_1Decoder}

function TISO8859_1Decoder.DecodeNext: WideChar;
begin
  with FSource do
  begin
    Result := WideChar(ord(FBuf[0]));
    Inc(FBuf);
  end;
end;

class function TISO8859_1Decoder.Supports(const AEncoding: string): Boolean;
begin
  Result := SameText(AEncoding, 'ISO-8859-1') or
            SameText(AEncoding, 'ISO_8859-1') or
            SameText(AEncoding, 'latin1') or
            SameText(AEncoding, 'iso-ir-100') or
            SameText(AEncoding, 'l1') or
            SameText(AEncoding, 'IBM819') or
            SameText(AEncoding, 'CP819') or
            SameText(AEncoding, 'csISOLatin1') or
// This one is not in character-sets.txt, but used in most FPC documentation...
            SameText(AEncoding, 'ISO8859-1');
end;

{ TUCS2Decoder }

function TUCS2Decoder.DecodeNext: WideChar;
begin
  with FSource do
  begin
    Result := PWideChar(FBuf)^;
    Inc(FBuf, sizeof(WideChar));
  end;
  if FSwapEndian then
    Result := WideChar(Swap(Word(Result)));
end;

class function TUCS2Decoder.Supports(const AEncoding: string): Boolean;
begin
  // generic aliases for both LE and BE
  Result := SameText(AEncoding, 'UTF-16') or
            SameText(AEncoding, 'unicode');
end;

{ TUTF8Decoder }

function TUTF8Decoder.DecodeNext: WideChar;
const
  MaxCode: array[0..3] of Cardinal = ($7F, $7FF, $FFFF, $1FFFFF);
var
  Value: Cardinal;
  I, bc: Integer;
begin
  with FSource do
  begin
    Result := WideChar(byte(FBuf[0]));
    Inc(FBuf);
    if Result < #$80 then
      Exit;
    if Word(Result) and $40 = 0 then
      DecodingError('Invalid UTF-8 sequence start byte');
    bc := 1;
    if Word(Result) and $20 <> 0 then
    begin
      Inc(bc);
      if Word(Result) and $10 <> 0 then
      begin
        Inc(bc);
        if Word(Result) and $8 <> 0 then
          DecodingError('UCS4 character out of supported range');
      end;
    end;
    // DONE: (?) check that bc bytes available
    if FBufEnd-FBuf < bc then
      FetchData;

    Value := Byte(Result);
    I := bc;  // note: I is never zero
    while bc > 0 do
    begin
      if ord(FBuf[0]) and $C0 <> $80 then
        DecodingError('Invalid byte in UTF-8 sequence');
      Value := (Value shl 6) or (Cardinal(FBuf[0]) and $3F);
      Inc(FBuf);
      Dec(bc);
    end;
    Value := Value and MaxCode[I];
    // RFC2279 check
    if Value <= MaxCode[I-1] then
      DecodingError('Invalid UTF-8 sequence');
    case Value of
      0..$D7FF, $E000..$FFFF:
        begin
          Result := WideChar(Value);
          Exit;
        end;
      $10000..$10FFFF:
        begin
          Result := WideChar($D7C0 + (Value shr 10));
          FSurrogate := WideChar($DC00 xor (Value and $3FF));
          Exit;
        end;
    end;
    DecodingError('UCS4 character out of supported range');
  end;
end;

class function TUTF8Decoder.Supports(const AEncoding: string): Boolean;
begin
  Result := SameText(AEncoding, 'UTF-8');
end;

{ TXMLReader }

procedure TXMLReader.MarkTokenStart;
begin
  FTokenStart := FLocation;
end;

function TXMLReader.ResolveEntity(const SystemID, PublicID: WideString; out Source: TXMLCharSource): Boolean;
var
  AbsSysID: WideString;
  Filename: string;
  Stream: TStream;
begin
  Result := True;
  if Assigned(FSource) then
    Result := ResolveRelativeURI(FSource.SystemID, SystemID, AbsSysID)
  else
    AbsSysID := SystemID;

  if Result then
  begin
    Source := nil;
    Result := False;
    // TODO: alternative resolvers
    if URIToFilename(AbsSysID, Filename) then
    begin
      try
        Stream := TFileStream.Create(Filename, fmOpenRead + fmShareDenyWrite);
        Source := TXMLStreamInputSource.Create(Stream, True);
        Source.SystemID := AbsSysID;
        Source.PublicID := PublicID;
        Result := True;
      except
        on E: Exception do
          ValidationError('%s', [E.Message]);
      end;
    end;
  end;
end;

procedure TXMLReader.InitializeRoot(ASource: TXMLCharSource);
begin
  Initialize(ASource);
  GetChar;
  // TODO: presence of BOM must prevent UTF-8 encoding from being changed
  if CheckForChar(#$FEFF) then    // skip BOM, if one is present
    Dec(FLocation.LinePos);
end;


procedure TXMLReader.Initialize(ASource: TXMLCharSource);
begin
  FSource := ASource;
  FSource.FReader := Self;
  FLocation.Line := 1;
  FLocation.LinePos := 0;   // TODO: or 1?
  FSource.Initialize;
end;

procedure TXMLReader.GetCharRaw;
begin
  FCurChar := FSource.NextChar;
  if FCurChar = #10 then
  begin
    Inc(FLocation.Line);
    FLocation.LinePos := 0;
  end
  else
    Inc(FLocation.LinePos);
end;

procedure TXMLReader.GetChar;
begin
  GetCharRaw;
  // Used for handling the internal DTD subset
  if Assigned(FCopyBuf) and (FSource.FParent = nil) then
    BufAppend(FCopyBuf^, FCurChar);
  if not FRecognizePE then
    Exit;
  if (FCurChar = #0) and ContextPop then
  begin
    UngetCurChar;
    FCurChar := #32;
  end
  else if FCurChar = '%' then
  begin
    GetCharRaw;
    if not CheckName then
    begin
      UngetCurChar;
      FCurChar := '%';
      Exit;
    end;
    if FCurChar = ';' then // "%pe1;%pe2" - must not recognize pe2 immediately!
      GetCharRaw
    else
      FatalError(WideChar(';'));
    StartPE;
    FCurChar := #32;
  end;
end;

procedure TXMLReader.UngetCurChar;
begin
  FSource.FSurrogate := FCurChar;
end;

procedure TXMLReader.RaiseExpectedQmark;
begin
  FatalError('Expected single or double quote');
end;

procedure TXMLReader.FatalError(Expected: WideChar);
begin
// FIX: don't output what is found - anything may be found, including exploits...
  FatalError('Expected "%1s"', [string(Expected)]);
end;

procedure TXMLReader.FatalError(const descr: String; AtTokenStart: Boolean);
begin
  DoError(esFatal, descr, AtTokenStart);
end;

procedure TXMLReader.FatalError(const descr: string; const args: array of const; AtTokenStart: Boolean);
begin
  DoError(esFatal, Format(descr, args), AtTokenStart);
end;

procedure TXMLReader.ValidationError(const Msg: string; const Args: array of const);
begin
  FDocNotValid := True;
  if FValidate then
  // Seems that validation errors always appear on token boundary (re-check!)
    DoError(esError, Format(Msg, Args), True);
end;

procedure TXMLReader.DoError(Severity: TErrorSeverity; const descr: string; AtTokenStart: Boolean=False);
var
  RealLocation: ^TLocation;
  E: EXMLReadError;
begin
  if AtTokenStart then
    RealLocation := @FTokenStart
  else
    RealLocation := @FLocation;
  E := EXMLReadError.CreateFmt('In ''%s'' (line %d pos %d): %s', [FSource.SystemID, RealLocation^.Line, RealLocation^.LinePos, descr]);
  E.FSeverity := Severity;
  E.FErrorMessage := descr;
  E.FLine := RealLocation^.Line;
  E.FLinePos := RealLocation^.LinePos;
  CallErrorHandler(E);
  // No 'finally'! If user handler raises exception, control should not get here
  E.Free;
end;

function TXMLReader.SkipWhitespace: Boolean;
begin
  Result := False;
  while (FCurChar = #32) or (FCurChar = #10) or (FCurChar = #9) or (FCurChar = #13) do
  begin
    GetChar;
    Result := True;
  end;
end;

function TXMLReader.SkipWhitespaceRaw: Boolean;
begin
  Result := False;
  while (FCurChar = #32) or (FCurChar = #10) or (FCurChar = #9) or (FCurChar = #13) do
  begin
    GetCharRaw;
    Result := True;
  end;
end;

procedure TXMLReader.ExpectWhitespace;
begin
  if not SkipWhitespace then
    FatalError('Expected whitespace');
end;

procedure TXMLReader.ExpectChar(wc: WideChar);
begin
  if FCurChar = wc then
    GetChar
  else
    FatalError(wc);
end;

procedure TXMLReader.ExpectString(const s: String);
var
  I: Integer;
begin
  for I := 1 to Length(s) do
  begin
    if FCurChar <> WideChar(ord(s[i])) then
      FatalError('Expected "%s"', [s]);
    GetChar;
  end;
end;

function TXMLReader.CheckForChar(c: WideChar): Boolean;
begin
  Result := (FCurChar = c);
  if Result then
    GetChar;
end;

procedure TXMLReader.SkipString(const ValidChars: TSetOfChar);
begin
  FValue.Length := 0;
  while (ord(FCurChar) < 256) and (char(ord(FCurChar)) in ValidChars) do
  begin
    BufAppend(FValue, FCurChar);
    GetChar;
  end;
end;

function TXMLReader.GetString(const ValidChars: TSetOfChar): WideString;
begin
  SkipString(ValidChars);
  SetString(Result, FValue.Buffer, FValue.Length);
end;

constructor TXMLReader.Create;
begin
  inherited Create;
  BufAllocate(FName, 128);
  BufAllocate(FValue, 512);
  FIDRefs := TList.Create;

  // Set char rules to XML 1.0
  FNamePages := @NamePages;
  FForbiddenAscii := [#1..#8, #11..#12, #14..#31];
end;

constructor TXMLReader.Create(AParser: TDOMParser);
begin
  Create;
  FCtrl := AParser;
  FValidate := FCtrl.Options.Validate;
  FPreserveWhitespace := FCtrl.Options.PreserveWhitespace;
  FExpandEntities := FCtrl.Options.ExpandEntities;
  FCDSectionsAsText := FCtrl.Options.CDSectionsAsText;
  FIgnoreComments := FCtrl.Options.IgnoreComments;
end;

destructor TXMLReader.Destroy;
begin
  FreeMem(FName.Buffer);
  FreeMem(FValue.Buffer);
  while ContextPop do;     // clean input stack
  FSource.Free;
  FPEMap.Free;
  while Assigned(FValidator) do
    PopVC;
  ClearIDRefs;
  FIDRefs.Free;
  inherited Destroy;
end;

procedure TXMLReader.XML11_BuildTables;
begin
  FNamePages := Xml11NamePages;
  FXML11 := True;
  FForbiddenAscii := [#1..#8, #11..#12, #14..#31, #$7F..#$84, #$86..#$9F];
end;

procedure TXMLReader.ProcessXML(ASource: TXMLCharSource);
begin
  doc := TXMLDocument.Create;
  FCursor := doc;
  InitializeRoot(ASource);
  FAllowedDecl := dtXml;
  DoParseFragment;              // case FCurChar <> #0 is handled

  if doc.DocumentElement = nil then
    FatalError('Root element is missing');

  if FValidate then
  begin
    if Assigned(FDocType) then
    begin
      if doc.DocumentElement.TagName <> FDocType.Name then
        ValidationError('DTD name does not match root element', []);
      ValidateIdRefs;
    end
    else
      ValidationError('Missing DTD', []);
  end;
end;

procedure TXMLReader.ProcessFragment(ASource: TXMLCharSource; AOwner: TDOMNode);
begin
  doc := AOwner.OwnerDocument;
  FCursor := AOwner;
  InitializeRoot(ASource);
  FXML11 := doc.InheritsFrom(TXMLDocument) and (TXMLDocument(doc).XMLVersion = '1.1');
  FAllowedDecl := dtText;
  FInsideRoot := True;
  DoParseFragment;
end;

// XML 1.1 allowed range $10000..$EFFFF is [D800..DB7F] followed by [DC00..DFFF]
function TXMLReader.XML11_CheckName: Boolean;
begin
  if (FCurChar >= #$D800) and (FCurChar <= #$DB7F) then
  begin
    BufAppend(FName, FCurChar);
    // TODO: do I need to update Location here???
    FCurChar := FSource.NextChar;
    Result := (FCurChar >= #$DC00) and (FCurChar <= #$DFFF);
  end
  else
    Result := False;
end;

function TXMLReader.CheckName: Boolean;
begin
  FName.Length := 0;
  Result := (Byte(FCurChar) in NamingBitmap[FNamePages^[hi(Word(FCurChar))]]) or
    (FXML11 and XML11_CheckName);
  if Result then
  repeat
    BufAppend(FName, FCurChar);
    GetChar;
  until not ((Byte(FCurChar) in NamingBitmap[FNamePages^[$100+hi(Word(FCurChar))]]) or
    (FXML11 and XML11_CheckName));
end;

function TXMLReader.CheckNmToken: Boolean;
begin
  FName.Length := 0;
  Result := False;
  while (Byte(FCurChar) in NamingBitmap[FNamePages^[$100+hi(Word(FCurChar))]]) or
    (FXML11 and XML11_CheckName) do
  begin
    BufAppend(FName, FCurChar);
    GetChar;
    Result := True;
  end;
end;

procedure TXMLReader.RaiseNameNotFound;
begin
  // Coming at no cost, this allows more user-friendly error messages
  if (FCurChar = #32) or (FCurChar = #10) or (FCurChar = #9) or (FCurChar = #13) then
    FatalError('Whitespace is not allowed here')
  else
    FatalError('Name starts with invalid character');
end;

function TXMLReader.ExpectName: WideString;
begin
  if not CheckName then
    RaiseNameNotFound;

  SetString(Result, FName.Buffer, FName.Length);
end;

function TXMLReader.ResolvePredefined(const RefName: WideString): WideChar;
begin
  if RefName = 'amp' then
    Result := '&'
  else if RefName = 'apos' then
    Result := ''''
  else if RefName = 'gt' then
    Result := '>'
  else if RefName = 'lt' then
    Result := '<'
  else if RefName = 'quot' then
    Result := '"'
  else
    Result := #0;
end;

function TXMLReader.ParseCharRef: Boolean;           // [66]
var
  Value: Integer;
begin
  GetCharRaw;   // skip '&'
  Result := FCurChar = '#';
  if Result then
  begin
    GetCharRaw;
    Value := 0;
    if CheckForChar('x') then
    repeat
      case FCurChar of
        '0'..'9': Value := Value * 16 + Ord(FCurChar) - Ord('0');
        'a'..'f': Value := Value * 16 + Ord(FCurChar) - (Ord('a') - 10);
        'A'..'F': Value := Value * 16 + Ord(FCurChar) - (Ord('A') - 10);
      else
        Break;
      end;
      GetCharRaw;
    until False
    else
    repeat
      case FCurChar of
        '0'..'9': Value := Value * 10 + Ord(FCurChar) - Ord('0');
      else
        Break;
      end;
      GetCharRaw;
    until False;

    ExpectChar(';');

    case Value of
      $01..$08, $0B..$0C, $0E..$1F:
        if FXML11 then
          BufAppend(FValue, WideChar(Value))
        else
          FatalError('Invalid character reference');
      $09, $0A, $0D, $20..$D7FF, $E000..$FFFD:
        BufAppend(FValue, WideChar(Value));
      $10000..$10FFFF:
        begin
          BufAppend(FValue, WideChar($D7C0 + (Value shr 10)));
          BufAppend(FValue, WideChar($DC00 xor (Value and $3FF)));
        end;
    else
      FatalError('Invalid character reference');
    end;
  end;
end;

procedure TXMLReader.DoParseAttValue(Delim: WideChar);
var
  RefNode: TDOMEntityEx;
begin
  FValue.Length := 0;
  while (FCurChar <> Delim) and (FCurChar <> #0) do
  begin
    if FCurChar = '<' then
      FatalError('Character ''<'' is not allowed in attribute value')
    else if FCurChar <> '&' then
    begin
      if (FCurChar = #10) or (FCurChar = #9) or (FCurChar = #13) then
        FCurChar := #32;
      BufAppend(FValue, FCurChar);
      GetCharRaw;
    end
    else
    begin
      if ParseCharRef then
        Continue;

      RefNode := ParseReference;
      if Assigned(RefNode) then
      begin
        if FValue.Length > 0 then
        begin
          DoAttrText(FValue.Buffer, FValue.Length);
          FValue.Length := 0;
        end;

        if RefNode.SystemID <> '' then
          FatalError('External entity reference is not allowed in attribute value', True);

        IncludeEntity(RefNode, True);
      end;
    end;
  end; // while
  if FValue.Length > 0 then
  begin
    DoAttrText(FValue.Buffer, FValue.Length);
    FValue.Length := 0;
  end;
end;

procedure TXMLReader.DoParseFragment;
begin
  // SAX: ContentHandler.StartDocument() - here?
  ParseContent;
  if FCurChar <> #0 then
    FatalError('Closing tag is not allowed here');
  // SAX: ContentHandler.EndDocument() - here? or somewhere in destructor?  
end;


function TXMLReader.ContextPush(AEntity: TDOMEntityEx): Boolean;
var
  Src: TXMLCharSource;
begin
  if AEntity.SystemID <> '' then
  begin
    Result := ResolveEntity(AEntity.SystemID, AEntity.PublicID, Src);
    if not Result then
      Exit;
{
  TODO: need different handling of TextDecl in external PEs
  it cannot be parsed if PE is referenced INSIDE declaration
  But - is such case ever met in the wild ?? E.g. MSXML fails such things...
}
    FAllowedDecl := dtText;
  end
  else
    Src := TXMLCharSource.Create(AEntity.FReplacementText);

  AEntity.FOnStack := True;
  Src.FEntity := AEntity;

  Src.FParent := FSource;
  Src.FCursor := FCursor;
  UngetCurChar;             // remember FCurChar and current location in previous context
  Src.FSavedLocation := FLocation;

  Initialize(Src);
  Result := True;
end;

function TXMLReader.ContextPop: Boolean;
var
  Src: TXMLCharSource;
  TmpLocation: TLocation;
begin
  Result := Assigned(FSource.FParent);
  if Result then
  begin
    Src := FSource.FParent;
    TmpLocation := FSource.FSavedLocation;
    if Assigned(FSource.FEntity) then
      TDOMEntityEx(FSource.FEntity).FOnStack := False;
    FCursor := TDOMNode(FSource.FCursor);
    FSource.Free;
    FSource := Src;
    GetChar;                       // re-classify - case of "%pe1;%pe2;"
    FLocation := TmpLocation;
  end;
end;

procedure TXMLReader.IncludeEntity(AEntity: TDOMEntityEx; InAttr: Boolean);
var
  Node, Child: TDOMNode;
begin
  if not AEntity.FResolved then
  begin
    if AEntity.FOnStack then
      FatalError('Entity ''%s'' recursively references itself', [AEntity.NodeName]);

    if ContextPush(AEntity) then
    begin
      GetCharRaw;
      if CheckForChar(#$FEFF) then
        Dec(FLocation.LinePos);

      FCursor := AEntity;         // build child node tree for the entity
      try
        if InAttr then
          DoParseAttValue(#0)
        else
          DoParseFragment;
        AEntity.FResolved := True;
      finally
        ContextPop;               // FCursor restored
        FValue.Length := 0;
      end;
    end;
  end;
  Node := FCursor;
  if (not FExpandEntities) or (not AEntity.FResolved) then
  begin
    Node := doc.CreateEntityReference(AEntity.NodeName);
    FCursor.AppendChild(Node);
  end;

  Child := AEntity.FirstChild;  // clone the entity node tree
  while Assigned(Child) do
  begin
    Node.AppendChild(Child.CloneNode(True));
    Child := Child.NextSibling;
  end;
end;

procedure TXMLReader.StartPE;
var
  PEName: WideString;
  PEnt: TDOMEntityEx;
begin
  SetString(PEName, FName.Buffer, FName.Length);
  PEnt := nil;
  if Assigned(FPEMap) then
    PEnt := FPEMap.GetNamedItem(PEName) as TDOMEntityEx;
  if PEnt = nil then    // TODO -cVC: Referencing undefined PE
  begin                 // (These are classified as 'optional errors'...)
//    ValidationError('Undefined parameter entity referenced: %s', [PEName]);
    Exit;
  end;

  if PEnt.FOnStack then
    FatalError('Entity ''%%%s'' recursively references itself', [PEnt.NodeName]);

  ContextPush(PEnt);
end;

function TXMLReader.ParseReference: TDOMEntityEx;
var
  RefName: WideString;
  Predef: WideChar;
begin
  Result := nil;
  MarkTokenStart;
  RefName := ExpectName;
  ExpectChar(';');
  Predef := ResolvePredefined(RefName);
  if Predef <> #0 then
    BufAppend(FValue, Predef)
  else
  begin
    if Assigned(FDocType) then
      Result := FDocType.Entities.GetNamedItem(RefName) as TDOMEntityEx;

    if Result = nil then
    begin
      if FStandalone or (FDocType = nil) or not (FHavePERefs or (FDocType.SystemID <> '')) then
        FatalError('Reference to undefined entity ''%s''', [RefName], True)
      else
        ValidationError('Undefined entity ''%s'' referenced', [RefName]);
    end
    else
    begin
      if FStandalone and Result.FExternallyDeclared then
        FatalError('Standalone constraint violation', True);
      if Result.NotationName <> '' then
        FatalError('Reference to unparsed entity ''%s''', [RefName], True);
    end;
  end;
end;

procedure TXMLReader.ProcessTextAndRefs;
var
  nonWs: Boolean;
  RefNode: TDOMEntityEx;
  ElDef: TDOMElementDef;
begin
  FValue.Length := 0;
  nonWs := False;
  MarkTokenStart;
  while (FCurChar <> '<') and (FCurChar <> #0) do
  begin
    if FCurChar <> '&' then
    begin
      if (FCurChar <> #32) and (FCurChar <> #10) and (FCurChar <> #9) and (FCurChar <> #13) then
        nonWs := True;
      BufAppend(FValue, FCurChar);
      if FCurChar = '>' then
        with FValue do
          if (Length >= 3) and (Buffer[Length-2] = ']') and (Buffer[Length-3] = ']') then
          begin
            Dec(FLocation.LinePos, 2);
            FatalError('Literal '']]>'' is not allowed in text');
          end;
      GetCharRaw;
    end
    else
    begin
      if not FInsideRoot then
        FatalError('Illegal at document level');
      if FValidate then
      begin
        ElDef := CurrentElementDef;
        if Assigned(ElDef) and (ElDef.ContentType = ctEmpty) then
          ValidationError('References are illegal in EMPTY elements', []);
      end;
      if ParseCharRef then
      begin
        nonWs := True; // CharRef to whitespace is not considered whitespace
        Continue;
      end;
      RefNode := ParseReference;
      if Assigned(RefNode) then
      begin
        if (nonWs or FPreserveWhitespace) and (FValue.Length > 0)  then
        begin
          // 'Reference illegal at root' is checked above, no need to check here
          DoText(FValue.Buffer, FValue.Length, not nonWs);
          FValue.Length := 0;
          nonWs := False;
        end;
        IncludeEntity(RefNode, False);
      end;
    end;
  end; // while
  if FInsideRoot then
  begin
    if (nonWs or FPreserveWhitespace) and (FValue.Length > 0)  then
    begin
      DoText(FValue.Buffer, FValue.Length, not nonWs);
      FValue.Length := 0;
    end;
  end
  else if nonWs then
    FatalError('Illegal at document level', True);
end;

procedure TXMLReader.ExpectAttValue;    // [10]
var
  Delim: WideChar;
begin
  if (FCurChar <> '''') and (FCurChar <> '"') then
    RaiseExpectedQmark;
  Delim := FCurChar;
  GetCharRaw;  // skip quote
  DoParseAttValue(Delim);
  GetChar;    // NOTE: not GetCharRaw - when parsing AttDef in DTD,
              // immediately following PERef must be recognized
end;

function TXMLReader.SkipQuotedLiteral: Boolean;
var
  Delim: WideChar;
begin
  Result := (FCurChar = '''') or (FCurChar = '"');
  if Result then
  begin
    Delim := FCurChar;
    GetCharRaw;  // skip quote
    MarkTokenStart;
    FValue.Length := 0;
    while (FCurChar <> Delim) and (FCurChar <> #0) do
    begin
      BufAppend(FValue, FCurChar);
      GetCharRaw;
    end;
    ExpectChar(Delim);  // <-- to check the EOF only
  end;
end;

procedure TXMLReader.SkipPubidLiteral;                 // [12]
var
  I: Integer;
  wc: WideChar;
begin
  if SkipQuotedLiteral then
  begin
    for I := 0 to FValue.Length-1 do
    begin
      wc := FValue.Buffer[I];
      if (wc > #255) or not (Char(ord(wc)) in PubidChars) then
        FatalError('Illegal Public ID literal', True);
      if (wc = #10) or (wc = #13) then
        FValue.Buffer[I] := #32;
    end;
  end
  else
    RaiseExpectedQMark;
end;

procedure TXMLReader.SkipSystemLiteral(out Literal: WideString);
begin
  if SkipQuotedLiteral then
    SetString(Literal, FValue.Buffer, FValue.Length)
  else
    RaiseExpectedQMark;
end;

procedure TXMLReader.ParseComment;    // [15]
begin
  ExpectString('--');
  MarkTokenStart;
  FValue.Length := 0;
  repeat
    BufAppend(FValue, FCurChar);
    GetCharRaw;
    with FValue do
      if (Length >= 2) and (Buffer[Length-1] = '-') and
      (Buffer[Length-2] = '-') then
      begin
        ExpectChar('>');
        Dec(Length, 2);
        DoComment(Buffer, Length);
        Exit;
      end;
  until FCurChar = #0;
  FatalError('Unterminated comment', True);
end;

procedure TXMLReader.ParsePI;                    // [16]
var
  Name, Value: WideString;
  PINode: TDOMProcessingInstruction;
  ElDef: TDOMElementDef;
begin
  GetCharRaw;      // skip '?'
  MarkTokenStart;
  Name := ExpectName;

  with FName do
    if (Length = 3) and
     ((Buffer[0] = 'X') or (Buffer[0] = 'x')) and
     ((Buffer[1] = 'M') or (Buffer[1] = 'm')) and
     ((Buffer[2] = 'L') or (Buffer[2] = 'l')) then
  begin
    if Name <> 'xml' then
      FatalError('''xml'' is a reserved word; it must be lowercase', True);

    // Declaration is allowed only at the very beginning of the _external_ entity
    if (FTokenStart.Line = 1) and (FTokenStart.LinePos = 3) and (FSource.FSystemID <> '') then
    begin
      ParseXmlOrTextDecl(FAllowedDecl = dtText);
      Exit;
    end
    else
      FatalError('XML declaration is not allowed here', True);
  end;

  if FCurChar <> '?' then
    ExpectWhitespace;

  FValue.Length := 0;
  repeat
    BufAppend(FValue, FCurChar);
    GetCharRaw;
    with FValue do
      if (Length >= 2) and (Buffer[Length-1] = '>') and
        (Buffer[Length-2] = '?') then
      begin
        Dec(Length, 2);
        SetString(Value, Buffer, Length);
        // SAX: ContentHandler.ProcessingInstruction(Name, Value);

        if FValidate then
        begin
          ElDef := CurrentElementDef;
          if Assigned(ElDef) and (ElDef.ContentType = ctEmpty) then
            ValidationError('Processing instructions are not allowed within EMPTY elements', []);
        end;

        PINode := Doc.CreateProcessingInstruction(Name, Value);
        if Assigned(FCursor) then
          FCursor.AppendChild(PINode)
        else  // to comply with certain tests, insert PI from DTD before DTD
          Doc.InsertBefore(PINode, FDocType);
        Exit;
      end;
  until FCurChar = #0;
  FatalError('Unterminated processing instruction', True);
end;


// here we come from ParsePI, 'xml' is already consumed
procedure TXMLReader.ParseXmlOrTextDecl(TextDecl: Boolean);
var
  TmpStr: WideString;
  IsXML11: Boolean;
begin
  ExpectWhitespace;
  // VersionInfo: optional in TextDecl, required in XmlDecl
  if (not TextDecl) or (FCurChar = 'v') then
  begin
    ExpectString('version');                              // [24]
    ExpectEq;
    SkipSystemLiteral(TmpStr);
    IsXML11 := False;
    if TmpStr = '1.1' then     // Checking for bad chars is implied
      IsXML11 := True
    else if TmpStr <> '1.0' then
      FatalError('Illegal version number', True);

    if not TextDecl then
    begin
      if doc.InheritsFrom(TXMLDocument) then
        TXMLDocument(doc).XMLVersion := TmpStr;
      if IsXML11 then
        XML11_BuildTables;
    end
    else   // parsing external entity
      if IsXML11 and not FXML11 then
        FatalError('XML 1.0 document cannot invoke XML 1.1 entities', True);

    if FCurChar <> '?' then
      ExpectWhitespace;
  end;

  // EncodingDecl: required in TextDecl, optional in XmlDecl
  if TextDecl or (FCurChar = 'e') then                    // [80]
  begin
    ExpectString('encoding');
    ExpectEq;
    SkipSystemLiteral(TmpStr);

    if not IsValidXmlEncoding(TmpStr) then
      FatalError('Illegal encoding name', True);

    if not FSource.SetEncoding(TmpStr) then  // <-- Wide2Ansi conversion here
      FatalError('Encoding ''%s'' is not supported', [TmpStr], True);
    // getting here means that specified encoding is supported
    // TODO: maybe assign the 'preferred' encoding name?
    if not TextDecl and doc.InheritsFrom(TXMLDocument) then
      TXMLDocument(doc).Encoding := TmpStr;

    if FCurChar <> '?' then
      ExpectWhitespace;
  end;

  // SDDecl: forbidden in TextDecl, optional in XmlDecl
  if (not TextDecl) and (FCurChar = 's') then
  begin
    ExpectString('standalone');
    ExpectEq;
    SkipSystemLiteral(TmpStr);
    if TmpStr = 'yes' then
      FStandalone := True
    else if TmpStr <> 'no' then
      FatalError('Only "yes" or "no" are permitted as values of "standalone"', True);
    SkipWhitespaceRaw;
  end;

  ExpectString('?>');
end;

procedure TXMLReader.ParseDoctypeDecl;    // [28]
var
  IntSubset: TWideCharBuf;
  Src, OldSrc: TXMLCharSource;
begin
  if FDtdParsed then
    FatalError('Markup declaration is not allowed here');

  ExpectString('DOCTYPE');
  ExpectWhitespace;

  FDocType := TDOMDocumentTypeEx(TDOMDocumentType.Create(doc));
  FDtdParsed := True;
  Doc.AppendChild(FDocType);
  FCursor := nil;

  FDocType.FName := ExpectName;
  ExpectWhitespace;
  ParseExternalID(FDocType.FSystemID, FDocType.FPublicID, False);
  SkipWhitespaceRaw;

  if FCurChar = '[' then
  begin
    BufAllocate(IntSubset, 256);
    FCopyBuf := @IntSubset;
    GetChar;      // cause very first char after '[' to be appended
    try
      FIntSubset := True;
      ParseMarkupDecl;
      if IntSubset.Length > 0 then  // sanity check - must at least contain ']'
        SetString(FDocType.FInternalSubset, IntSubset.Buffer, IntSubset.Length-1);
      ExpectChar(']');
    finally
      FIntSubset := False;
      FCopyBuf := nil;
      FreeMem(IntSubset.Buffer);
    end;
    SkipWhitespaceRaw;
  end;
  ExpectChar('>');

  if (FDocType.SystemID <> '') and ResolveEntity(FDocType.SystemID, FDocType.PublicID, Src) then
  begin
    // DTD parsing code assumes that FSource is RootSource,
    // therefore we cannot use ContextPush here...
    OldSrc := FSource;
    UngetCurChar;
    try
      DoParseExtSubset(Src);
    finally
      while ContextPop do;   // Cleanup after possible exceptions
      FSource.Free;
      FSource := OldSrc;
      GetChar;
    end;
  end;
  FCursor := Doc;
  ValidateDTD;
end;

function TXMLReader.ParseEq: Boolean;    // [25]
begin
  SkipWhitespaceRaw;
  Result := FCurChar = '=';
  if Result then
  begin
    GetCharRaw;
    SkipWhitespaceRaw;
  end;
end;

procedure TXMLReader.ExpectEq;
begin
  if not ParseEq then
    FatalError('Expected "="');
end;


{ DTD stuff }

procedure TXMLReader.BadPENesting;
begin
  ValidationError('Parameter entities must be properly nested', []);
end;

procedure TXMLReader.StandaloneError;
begin
  ValidationError('Standalone constriant violation', []);
end;

procedure TXMLReader.CheckNotation(const Name: WideString);
begin
  if FDocType.Notations.GetNamedItem(Name) = nil then
    ValidationError('Notation ''%s'' is not declared', [Name]);
end;

procedure TXMLReader.ParseQuantity(CP: TContentParticle);
begin
  if CheckForChar('?') then
    CP.CPQuant := cqZeroOrOnce
  else if CheckForChar('*') then
    CP.CPQuant := cqZeroOrMore
  else if CheckForChar('+') then
    CP.CPQuant := cqOnceOrMore;
end;

procedure TXMLReader.ExpectChoiceOrSeq(CP: TContentParticle);                  // [49], [50]
var
  Delim: WideChar;
  CurrentEntity: TObject;
  CurrentCP: TContentParticle;
begin
  Delim := #0;
  repeat
    CurrentCP := CP.Add;
    SkipWhitespace;
    if FCurChar = '(' then
    begin
      CurrentEntity := FSource.FEntity;
      GetChar;
      ExpectChoiceOrSeq(CurrentCP);
      if CurrentEntity <> FSource.FEntity then
        BadPENesting;
      GetChar;
    end
    else
      CurrentCP.Name := ExpectName;

    ParseQuantity(CurrentCP);

    SkipWhitespace;
    if FCurChar = ')' then
      Break;
    if Delim = #0 then
    begin
      if (FCurChar = '|') or (FCurChar = ',') then
        Delim := FCurChar
      else
        FatalError('Expected "|" or ","');
    end
    else
      if FCurChar <> Delim then
        FatalError(Delim);
    GetChar; // skip delimiter
  until False;
  if Delim = '|' then
    CP.CPType := ctChoice
  else
    CP.CPType := ctSeq;    // '(foo)' is a sequence!
end;

procedure TXMLReader.ParseElementDecl;            // [45]
var
  ElName: WideString;
  ElDef: TDOMElementDef;
  NeedAsterisk: Boolean;
  CurrentCP: TContentParticle;
  CurrentEntity: TObject;
  I: Integer;
begin
  MarkTokenStart;
  ElName := ExpectName;
  ExpectWhitespace;
  ElDef := TDOMElementDef(FDocType.ElementDefs.GetNamedItem(ElName));
  if Assigned(ElDef) and ElDef.HasElementDecl then
    ValidationError('Duplicate declaration of element ''%s''', [ElName]);
  if ElDef = nil then
  begin
    ElDef := TDOMElementDef.Create(doc);
    ElDef.FNodeName := ElName;
    FDocType.ElementDefs.SetNamedItem(ElDef);
  end;
  ElDef.FExternallyDeclared := not (FIntSubset and (FSource.FParent = nil));
  ElDef.HasElementDecl := True;

  if FCurChar = 'E' then
  begin
    ExpectString('EMPTY');
    ElDef.ContentType := ctEmpty;
  end
  else if FCurChar = 'A' then
  begin
    ExpectString('ANY');
    ElDef.ContentType := ctAny;
  end
  else if FCurChar = '(' then
  begin
    CurrentEntity := FSource.FEntity;
    GetChar;     // starting bracket
    SkipWhitespace;
    if FCurChar = '#' then       // Mixed section [51]
    begin
      ExpectString('#PCDATA');
      SkipWhitespace;
      ElDef.ContentType := ctMixed;
      NeedAsterisk := False;
      while FCurChar <> ')' do
      begin
        ExpectChar('|');
        NeedAsterisk := True;
        SkipWhitespace;

        CurrentCP := ElDef.RootCP.Add;
        CurrentCP.Name := ExpectName;
        // TODO: rethink this
        for I := ElDef.RootCP.ChildCount-2 downto 0 do
          if CurrentCP.Name = ElDef.RootCP.Children[I].Name then
            ValidationError('Duplicate token in mixed section', []);
        SkipWhitespace;
      end;
      if CurrentEntity <> FSource.FEntity then
        BadPENesting;
      GetChar;
      // TODO: does this asterisk have any real meaning?
      if NeedAsterisk then
      begin
        ExpectChar('*');
        ElDef.RootCP.CPQuant := cqZeroOrMore;
      end
      else
        if CheckForChar('*') then
          ElDef.RootCP.CPQuant := cqZeroOrMore;
    end
    else       // Children section [47]
    begin
      ElDef.ContentType := ctChildren;
      ExpectChoiceOrSeq(ElDef.RootCP);
      if CurrentEntity <> FSource.FEntity then
        BadPENesting;
      GetChar;
      ParseQuantity(ElDef.RootCP);
    end;
  end
  else
    FatalError('Invalid content specification');
  // SAX: DeclHandler.ElementDecl(name, model);
end;


procedure TXMLReader.ParseNotationDecl;        // [82]
var
  Name, SysID, PubID: WideString;
begin
  Name := ExpectName;
  ExpectWhitespace;
  if not ParseExternalID(SysID, PubID, True) then
    FatalError('Expected external or public ID');
  DoNotationDecl(Name, PubID, SysID);
end;

procedure TXMLReader.ParseAttlistDecl;         // [52]
var
  SaveCurNode: TDOMNode;
  ValueRequired: Boolean;
  Token: WideString;
  ElDef: TDOMElementDef;
  AttDef: TDOMAttrDef;
begin
  Token := ExpectName;
  ElDef := TDOMElementDef(FDocType.ElementDefs.GetNamedItem(Token));
  if ElDef = nil then
  begin
    // DONE -cVC: must distinguish ElementDef created here from one explicitly declared
    ElDef := TDOMElementDef.Create(doc);
    ElDef.FNodeName := Token;
    FDocType.ElementDefs.SetNamedItem(ElDef);
  end;
  SkipWhitespace;
  while FCurChar <> '>' do
  begin
    AttDef := TDOMAttrDef.Create(doc);
    AttDef.FExternallyDeclared := not (FIntSubset and (FSource.FParent = nil));
    try
      AttDef.FName := ExpectName;
      ExpectWhitespace;
      Token := GetString(['A'..'Z']);     // Get AttType [54], [55], [56]
      if Token = 'CDATA' then
        AttDef.FDataType := dtCdata
      else if Token = 'ID' then
        AttDef.FDataType := dtId
      else if Token = 'IDREF' then
        AttDef.FDataType := dtIdRef
      else if Token = 'IDREFS' then
        AttDef.FDataType := dtIdRefs
      else if Token = 'ENTITY' then
        AttDef.FDataType := dtEntity
      else if Token = 'ENTITIES' then
        AttDef.FDataType := dtEntities
      else if Token = 'NMTOKEN' then
        AttDef.FDataType := dtNmToken
      else if Token = 'NMTOKENS' then
        AttDef.FDataType := dtNmTokens
      else if Token = 'NOTATION' then     // [57], [58]
      begin
        AttDef.FDataType := dtNotation;
        ExpectWhitespace;
        ExpectChar('(');
        repeat
          SkipWhitespace;
          MarkTokenStart;
          if not AttDef.AddEnumToken(ExpectName) then
            ValidationError('Duplicate token in NOTATION attribute declaration',[]);
          SkipWhitespace;
        until not CheckForChar('|');
        ExpectChar(')');
      end
      else
      if CheckForChar('(') then     // [59]
      begin
        AttDef.FDataType := dtNmToken;
        repeat
          SkipWhitespace;
          MarkTokenStart;
          if not CheckNmToken then
            RaiseNameNotFound;      // not completely correct error message
          SetString(Token, FName.Buffer, FName.Length);
          if not AttDef.AddEnumToken(Token) then
            ValidationError('Duplicate token in enumerated attibute declaration', []);
          SkipWhitespace;
        until not CheckForChar('|');
        ExpectChar(')');
      end else
        FatalError('Illegal attribute type', True);

      ExpectWhitespace;

      ValueRequired := False;
      MarkTokenStart;
      if CheckForChar('#') then
      begin
        Token := GetString(['A'..'Z']);
        if Token = 'REQUIRED' then
          AttDef.FDefault := adRequired
        else if Token = 'IMPLIED' then
          AttDef.FDefault := adImplied
        else if Token = 'FIXED' then
        begin
          AttDef.FDefault := adFixed;
          ExpectWhitespace;
          ValueRequired := True;
        end
        else
          FatalError('Illegal attribute default', True);
      end
      else
      begin
        AttDef.FDefault := adDefault;
        ValueRequired := True;
      end;
      
      if ValueRequired then
      begin
        SaveCurNode := FCursor;
        FCursor := AttDef;
// See comments to valid-sa-094: PE expansion should be disabled in AttDef.
// ExpectAttValue() does not recognize PEs anyway, so setting FRecognizePEs isn't needed
        try
          ExpectAttValue;
        finally
          FCursor := SaveCurNode;
        end;
      end;
      // SAX: DeclHandler.AttributeDecl(...)

      // First declaration is binding, subsequent should be ignored
      if Assigned(ElDef.GetAttributeNode(AttDef.Name)) then
        AttDef.Free
      else
        ElDef.SetAttributeNode(AttDef);
    except
      AttDef.Free;
      raise;
    end;
    SkipWhitespace;
  end;
end;

procedure TXMLReader.ParseEntityDeclValue(Delim: WideChar);   // [9]
var
  I: Integer;
  Src: TXMLCharSource;
begin
  Src := FSource;
  // "Included in literal": process until delimiter hit IN SAME context
  while not ((FSource = Src) and CheckForChar(Delim)) do
  if ParsePEReference then
  begin
    if FIntSubset and (FSource.FParent = nil) then
      FatalError('PE references in internal subset are not allowed inside declarations', True);
    StartPE;
    GetCharRaw;
  end
  else if FCurChar = '&' then  // CharRefs: include, EntityRefs: bypass
  begin
    if not ParseCharRef then
    begin
      BufAppend(FValue, '&');
      ExpectName;
      ExpectChar(';');
      for I := 0 to FName.Length-1 do
        BufAppend(FValue, FName.Buffer[I]);
      BufAppend(FValue, ';');
    end;
  end
  else if FCurChar <> #0 then         // Regular character
  begin
    BufAppend(FValue, FCurChar);
    GetCharRaw;
  end
  else if not ContextPop then         // #0
    Break;
end;

procedure TXMLReader.ParseEntityDecl;        // [70]
var
  NDataAllowed: Boolean;
  Delim: WideChar;
  Entity: TDOMEntityEx;
  Map: TDOMNamedNodeMap;
begin
  NDataAllowed := True;
  Map := FDocType.Entities;
  if CheckForChar('%') then                  // [72]
  begin
    ExpectWhitespace;
    NDataAllowed := False;
    if FPEMap = nil then
      FPEMap := TDOMNamedNodeMap.Create(FDocType, ENTITY_NODE);
    Map := FPEMap;
  end;

  Entity := TDOMEntityEx.Create(Doc);
  try
    Entity.FExternallyDeclared := not (FIntSubset and (FSource.FParent = nil));
    Entity.FName := ExpectName;
    ExpectWhitespace;

    if (FCurChar = '"') or (FCurChar = '''') then
    begin
      NDataAllowed := False;
      Delim := FCurChar;
      Entity.FStartLocation := FLocation;
      FRecognizePE := False;   // PERef right after delimiter should not be recognized
      GetCharRaw;              // at char level - we process it 'manually'
      FValue.Length := 0;
      ParseEntityDeclValue(Delim);
      FRecognizePE := not FIntSubset;
      SetString(Entity.FReplacementText, FValue.Buffer, FValue.Length);
    end
    else
      if not ParseExternalID(Entity.FSystemID, Entity.FPublicID, False) then
        FatalError('Expected entity value or external ID');

    if NDataAllowed then                // [76]
    begin
      if FCurChar <> '>' then
        ExpectWhitespace;
      if FCurChar = 'N' then
      begin
        ExpectString('NDATA');
        ExpectWhitespace;
        if not CheckName then
          RaiseNameNotFound;

        SetString(Entity.FNotationName, FName.Buffer, FName.Length);
        // SAX: DTDHandler.UnparsedEntityDecl(...);
      end;
    end;
  except
    Entity.Free;
    raise;
  end;

  // Repeated declarations of same entity are legal but must be ignored
  if Map.GetNamedItem(Entity.NodeName) = nil then
    Map.SetNamedItem(Entity)
  else
    Entity.Free;
end;


procedure TXMLReader.ParseMarkupDecl;        // [29]
var
  Token: WideString;
  IncludeLevel: Integer;
  IgnoreLevel: Integer;
  CurrentEntity: TObject;
begin
  IncludeLevel := 0;
  IgnoreLevel := 0;
  repeat
    SkipWhitespace;

    if ParsePEReference then     // PERef between declarations should always be recognized
    begin
      FHavePERefs := True;
      StartPE;
      GetChar;
      Continue;
    end;

    if (FCurChar = #0) and ContextPop then
      Continue;

    if (FCurChar = ']') and (IncludeLevel > 0) then
    begin
      ExpectString(']]>');
      Dec(IncludeLevel);
      Continue;
    end;

    if FCurChar <> '<' then
      Break;

    CurrentEntity := FSource.FEntity;
    GetChar;

    if CheckForChar('!') then
    begin
      if FCurChar = '-' then
        ParseComment
      else if FCurChar = '[' then
      begin
        if FIntSubset and (FSource.FParent = nil) then
          FatalError('Conditional sections are not allowed in internal subset');

        FRecognizePE := not FIntSubset;
        GetChar; // skip '['
        SkipWhitespace;
        Token := GetString(['A'..'Z']);
        SkipWhitespace;

        if Token = 'INCLUDE' then
          Inc(IncludeLevel)
        else if Token = 'IGNORE' then
          IgnoreLevel := 1
        else
          FatalError('Expected "INCLUDE" or "IGNORE"');
        if CurrentEntity <> FSource.FEntity then
          BadPENesting;
        ExpectChar('[');
        if IgnoreLevel > 0 then
        repeat
          FRecognizePE := False;    // PEs not recognized in IGNORE section
          if CheckForChar('<') and CheckForChar('!') and CheckForChar('[') then
            Inc(IgnoreLevel)
          else if CheckForChar(']') and CheckForChar(']') and CheckForChar('>') then
            Dec(IgnoreLevel)
          else GetChar;
        until (IgnoreLevel=0) or (FCurChar = #0);
      end
      else
      begin
        FRecognizePE := not FIntSubset;
        MarkTokenStart;
        Token := GetString(['A'..'Z']);
        ExpectWhitespace;
        if Token = 'ELEMENT' then
          ParseElementDecl
        else if Token = 'ENTITY' then
          ParseEntityDecl
        else if Token = 'ATTLIST' then
          ParseAttlistDecl
        else if Token = 'NOTATION' then
          ParseNotationDecl
        else
          FatalError('Illegal markup declaration', True);

        SkipWhitespace;
        FRecognizePE := False;  // ! Don't auto-pop context on last markup delimiter
        ExpectChar('>');        //   This enables correct nesting check
      end;
{
  MarkupDecl starting in PE and ending in root is a WFC [28a]
  MarkupDecl starting in root but ending in PE is a VC (erratum 2e-14)
}
      // TODO: what if statrs in PE1 and ends in PE2, and other cases? 
      if CurrentEntity <> FSource.FEntity then
        if Assigned(FSource.FEntity) then { ends in PE }
          BadPENesting
        else
          FatalError('Parameter entities must be properly nested');
    end
    else if FCurChar = '?' then
      ParsePI;
  until False;
  FRecognizePE := False;
  if (IncludeLevel > 0) or (IgnoreLevel > 0) then
    FatalError('Conditional section is not closed');
end;

procedure TXMLReader.DoParseExtSubset(ASource: TXMLCharSource);
begin
  InitializeRoot(ASource);
  FAllowedDecl := dtText;
  ParseMarkupDecl;
  if FCurChar <> #0 then
    FatalError('Illegal character in DTD');
end;

procedure TXMLReader.ProcessDTD(ASource: TXMLCharSource);
begin
  doc := TXMLDocument.Create;
  FDocType := TDOMDocumentTypeEx.Create(doc);
  // TODO: DTD labeled version 1.1 will be rejected - must set FXML11 flag
  // DONE: It's ok to have FCursor=nil now
  doc.AppendChild(FDocType);
  DoParseExtSubset(ASource);
end;

procedure TXMLReader.ParseCDSect;               // [18]
begin
  ExpectString('[CDATA[');
  MarkTokenStart;
  if not FInsideRoot then
    FatalError('Illegal at document level');
  FValue.Length := 0;
  repeat
    BufAppend(FValue, FCurChar);
    GetCharRaw;
    with FValue do
      if (Length >= 3) and (Buffer[Length-1] = '>') and
      (Buffer[Length-2] = ']') and (Buffer[Length-3] = ']') then
    begin
      DoCDSect(Buffer, Length-3);
      Exit;
    end;
  until FCurChar = #0;
  FatalError('Unterminated CDATA section', True);
end;

procedure TXMLReader.ParseContent;
begin
  repeat
    if FCurChar = '<' then
    begin
      GetCharRaw;
      MarkTokenStart;      
      if CheckName then
        ParseElement
      else if FCurChar = '!' then
      begin
        GetCharRaw;
        if FCurChar = '[' then
          ParseCDSect
        else if FCurChar = '-' then
          ParseComment
        else
          ParseDoctypeDecl;
      end
      else if FCurChar = '?' then
        ParsePI
      else
        Exit;
    end
    else
      ProcessTextAndRefs;
  until FCurChar = #0;
end;

// Element name already in FNameBuffer
procedure TXMLReader.ParseElement;    // [39] [40] [44]
var
  NewElem: TDOMElement;
  ElDef: TDOMElementDef;
  IsEmpty: Boolean;
  attr, OldAttr: TDOMNode;
begin
  if (FCursor = doc) and Assigned(doc.DocumentElement) then
    FatalError('Only one top-level element allowed', True)
  else
    FInsideRoot := True;

  NewElem := doc.CreateElementBuf(FName.Buffer, FName.Length);
  // First check if NewElem is allowed in this context
  if FValidate and Assigned(FValidator) and not FValidator.IsElementAllowed(NewElem.TagName) then
    ValidationError('Element ''%s'' is not allowed in this context',[NewElem.TagName]);

  FCursor.AppendChild(NewElem);

  // Then update ElementDef - it is needed to process attributes
  ElDef := nil;
  if Assigned(FDocType) then
  begin
    ElDef := TDOMElementDef(FDocType.ElementDefs.GetNamedItem(NewElem.TagName));
    if (ElDef = nil) or (not ElDef.HasElementDecl) then
      ValidationError('Using undeclared element ''%s''',[NewElem.TagName]);
  end;

  IsEmpty := False;
  if SkipWhitespaceRaw then
  begin
    while (FCurChar <> '>') and (FCurChar <> '/') do
    begin
      MarkTokenStart;
      ExpectName;
      attr := doc.CreateAttributeBuf(FName.Buffer, FName.Length);

      // !!cannot use TDOMElement.SetAttributeNode because it will free old attribute
      OldAttr := NewElem.Attributes.SetNamedItem(Attr);
      if Assigned(OldAttr) then
      begin
        OldAttr.Free;
        FatalError('Duplicate attribute', True);
      end;
      ExpectEq;
      FCursor := attr;
      ExpectAttValue;
      if (FCurChar <> '>') and (FCurChar <> '/') then
        ExpectWhitespace;
    end;   // while
  end;
  if FCurChar = '/' then
  begin
    IsEmpty := True;
    GetCharRaw;
  end;
  ExpectChar('>');

  PushVC(ElDef);
  // SAX: ContentHandler.StartElement(...)
  // SAX: ContentHandler.StartPrefixMapping(...)

  if not IsEmpty then
  begin
    FCursor := NewElem;
    if not FPreserveWhitespace then   // critical for testsuite compliance
      SkipWhitespaceRaw;
    ParseContent;
    if FCurChar = '/' then         // Get ETag [42]
    begin
      GetCharRaw;
      MarkTokenStart;
      if ExpectName <> NewElem.TagName then
        FatalError('Unmatching element end tag (expected "</%s>")', [NewElem.TagName], True);
      SkipWhitespaceRaw;
      ExpectChar('>');
    end
    else if FCurChar <> #0 then
      RaiseNameNotFound
    else // End of stream in content
      FatalError('Closing tag is missing for ''%s''', [NewElem.TagName]);
  end;
  // SAX: ContentHandler.EndElement(...)
  // SAX: ContentHandler.EndPrefixMapping(...)
  FCursor := NewElem.ParentNode;
  if FCursor = doc then
    FInsideRoot := False;
  ProcessDefaultAttributes(NewElem);

  if FValidate and Assigned(FValidator) and FValidator.Incomplete then
    ValidationError('Element ''%s'' is missing required sub-elements', [NewElem.TagName]);

  PopVC;
end;

procedure TXMLReader.AddIdRef(Buf: PWideChar; Length: Integer);
var
  w: PWideStrWrapper;
begin
  New(w);
  SetString(w^.Value, Buf, Length);
  FIDRefs.Add(w);
end;

procedure TXMLReader.ClearIdRefs;
var
  I: Integer;
begin
  for I := 0 to FIDRefs.Count-1 do
    Dispose(PWideStrWrapper(FIDRefs.List^[I]));
  FIDRefs.Clear;  
end;

procedure TXMLReader.ValidateIdRefs;
var
  I: Integer;
begin
  for I := 0 to FIDRefs.Count-1 do
    if Doc.GetElementById(PWideStrWrapper(FIDRefs.List^[I])^.Value) = nil then
      ValidationError('The ID ''%s'' does not match any element', [PWideStrWrapper(FIDRefs.List^[I])^.Value]);
  ClearIDRefs;
end;

procedure TXMLReader.ProcessDefaultAttributes(Element: TDOMElement);
var
  I, L, StartPos, EndPos: Integer;
  ElDef: TDOMElementDef;
  Map: TDOMNamedNodeMap;
  AttDef: TDOMAttrDef;
  Attr: TDOMAttr;
  AttValue: WideString;
  Entity: TDOMEntity;
begin
  ElDef := CurrentElementDef;
  if Assigned(ElDef) and ElDef.HasAttributes then
  begin
    Map := ElDef.Attributes;

    for I := 0 to Map.Length-1 do
    begin
      AttDef := Map[I] as TDOMAttrDef;

      Attr := Element.GetAttributeNode(AttDef.Name);
      if Attr = nil then
      begin
        // attribute needs defaulting
        case AttDef.FDefault of
          adDefault, adFixed: begin
            if FStandalone and AttDef.FExternallyDeclared then
              StandaloneError;
            // Cloning TDOMAttrDef creates TDOMAttr. DataType is copied.
            Attr := TDOMAttr(AttDef.CloneNode(True));
            TDOMAttrDef(Attr).FSpecified := False;  // Dirty hack...
            TDOMAttrDef(Attr).FDeclared := True;
            Element.SetAttributeNode(Attr);
          end;
          adRequired:  ValidationError('Required attribute ''%s'' of element ''%s'' is missing',[AttDef.Name, Element.TagName])
        end;
      end
      else
      begin
        TDOMAttrDef(Attr).FDeclared := True;
        AttValue := Attr.Value; // unnormalized
        // now assign DataType so that value is correctly normalized
        TDOMAttrDef(Attr).FDataType := AttDef.FDataType;
        if FStandalone and AttDef.FExternallyDeclared and (Attr.Value <> AttValue) then
          StandaloneError;
        AttValue := Attr.Value; // recalculate
        // TODO: what about normalization of AttDef.Value? (Currently it IS normalized)
        if (AttDef.FDefault = adFixed) and (AttDef.Value <> AttValue) then
          ValidationError('Value of attribute ''%s'' does not match its fixed default',[AttDef.Name]);

        if not ValidateAttrSyntax(AttDef, AttValue) then
          ValidationError('Attribute ''%s'' type mismatch', [AttDef.Name]);
      end;

      if Attr = nil then
        Continue;
      L := Length(AttValue);
      case Attr.DataType of
        dtId: if not Doc.AddID(Attr) then
                ValidationError('The ID ''%s'' is not unique', [AttValue]);

        dtIdRef, dtIdRefs: begin
          StartPos := 1;
          while StartPos <= L do
          begin
            EndPos := StartPos;
            while (EndPos <= L) and (AttValue[EndPos] <> #32) do
              Inc(EndPos);

            AddIdRef(@AttValue[StartPos], EndPos-StartPos);
            StartPos := EndPos + 1;
          end;
        end;

        dtEntity, dtEntities: begin
          StartPos := 1;
          while StartPos <= L do
          begin
            EndPos := StartPos;
            while (EndPos <= L) and (AttValue[EndPos] <> #32) do
              Inc(EndPos);
            Entity := TDOMEntity(FDocType.Entities.GetNamedItem(Copy(AttValue, StartPos, EndPos-StartPos)));
            if (Entity = nil) or (Entity.NotationName = '') then
              ValidationError('Attribute ''%s'' type mismatch', [Attr.Name]);
            StartPos := EndPos + 1;
          end;
        end;
      end;
    end;
  end;
  // Now report undeclared attributes
  if Assigned(FDocType) and Element.HasAttributes then
  begin
    Map := Element.Attributes;
    for I := 0 to Map.Length-1 do
    begin
      Attr := TDOMAttr(Map[I]);
      if not TDOMAttrDef(Attr).FDeclared then
        ValidationError('Using undeclared attribute ''%s'' on element ''%s''',[Attr.Name, Element.TagName]);
    end;
  end;
end;

function TXMLReader.ParsePEReference: Boolean;    // [69]
begin
  Result := CheckForChar('%');
  if Result then
  begin
    MarkTokenStart;
    if not CheckName then
      RaiseNameNotFound;
    ExpectChar(';');
  end;
end;

function TXMLReader.ParseExternalID(out SysID, PubID: WideString;     // [75]
  SysIdOptional: Boolean): Boolean;
begin
  if FCurChar = 'S' then
  begin
    ExpectString('SYSTEM');
    ExpectWhitespace;
    SkipSystemLiteral(SysID);
    Result := True;
  end
  else if FCurChar = 'P' then
  begin
    ExpectString('PUBLIC');
    ExpectWhitespace;
    SkipPubidLiteral;
    SetString(PubID, FValue.Buffer, FValue.Length);
    NormalizeSpaces(PubID);
    if SysIdOptional then
    begin
      SkipWhitespace;
      if SkipQuotedLiteral then
        SetString(SysID, FValue.Buffer, FValue.Length);
    end
    else
    begin
      ExpectWhitespace;
      SkipSystemLiteral(SysID);
    end;
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.CallErrorHandler(E: EXMLReadError);
begin
  try
    if Assigned(FCtrl) and Assigned(FCtrl.FOnError) then
      FCtrl.FOnError(E);
    if E.Severity = esFatal then
      raise E;
  except
    if ExceptObject <> E then
      E.Free;
    raise;
  end;
end;

function TXMLReader.ValidateAttrSyntax(AttrDef: TDOMAttrDef; const aValue: WideString): Boolean;
begin
  case AttrDef.FDataType of
    dtId, dtIdRef, dtEntity: Result := IsXmlName(aValue, FXML11);
    dtIdRefs, dtEntities: Result := IsXmlNames(aValue, FXML11);
    dtNmToken: Result := IsXmlNmToken(aValue, FXML11) and AttrDef.HasEnumToken(aValue);
    dtNmTokens: Result := IsXmlNmTokens(aValue, FXML11);
    // IsXmlName() not necessary - enum is never empty and contains valid names
    dtNotation: Result := AttrDef.HasEnumToken(aValue);
  else
    Result := True;
  end;
end;

// TODO: this should be method of TDOMDocumentTypeEx, but we must pass ErrorHandler in...
procedure TXMLReader.ValidateDTD;
var
  I, J, K: Integer;
  Entity: TDOMEntity;
  ElDef: TDOMElementDef;
  AttDef: TDOMAttrDef;
  IdFound, NotationFound, HasDefault: Boolean;
begin
  for I := 0 to FDocType.Entities.Length-1 do
  begin
    Entity := TDOMEntity(FDocType.Entities[I]);
    if (Entity.NotationName <> '') then
      CheckNotation(Entity.NotationName);
  end;

  if Assigned(FDocType.FElementDefs) then
  begin
    for I := 0 to FDocType.FElementDefs.Length-1 do
    begin
      ElDef := TDOMElementDef(FDocType.FElementDefs[I]);
      // XML spec permits ATTLIST declarations without corresponding ELEMENT.
      // Such ATTLISTs are useless for validation, so here we might skip or even
      // delete all ElDefs that have HasElementDecl=False. However, doing so
      // breaks some stupid tests, namely sun/id04.
{
      if not ElDef.HasElementDecl then
        Continue;
}
      if not ElDef.HasAttributes then
        Continue;
      IdFound := False;
      NotationFound := False;
      for J := 0 to ElDef.Attributes.Length-1 do
      begin
        AttDef := TDOMAttrDef(ElDef.Attributes[J]);
        HasDefault := AttDef.FDefault in [adDefault, adFixed];
        case AttDef.FDataType of
        dtId: begin
          if IdFound then
            ValidationError('Only one attribute of type ID is allowed per element',[]);
          IdFound := True;
          if HasDefault then
            ValidationError('An attribute of type ID cannot have a default value',[]);
          end;
        dtNotation: begin
            for K := 0 to Length(AttDef.FEnumeration)-1 do
              CheckNotation(AttDef.FEnumeration[K]);
            if NotationFound then
              ValidationError('Only one attribute of type NOTATION is allowed per element',[]);
            NotationFound := True;
            if ElDef.ContentType = ctEmpty then
              ValidationError('NOTATION attributes are not allowed on EMPTY elements',[]);
          end;
        end; // case
        if HasDefault and not ValidateAttrSyntax(AttDef, AttDef.NodeValue) then
          ValidationError('Illegal attribute default', []);
      end;
    end;
  end;
end;

procedure TXMLReader.DoText(ch: PWideChar; Count: Integer; Whitespace: Boolean);
var
  TextNode: TDOMText;
  ElDef: TDOMElementDef;
begin
  // Validating filter part
  // TODO: for testing whitespace CharRefs, they are contained in internal entities.
  //       Parsing first reports them to Entity, and then they are cloned to real parent
  //       so this method isn't called :(

  ElDef := CurrentElementDef;
  if Assigned(ElDef) then
  begin
    case ElDef.ContentType of
      ctChildren:
        if not Whitespace then
          ValidationError('Character data is not allowed in element-only content',[])
        else
          if FStandalone and ElDef.FExternallyDeclared then
            StandaloneError;
      ctEmpty:
        ValidationError('Character data is not allowed in EMPTY elements', []);
    end;
  end;

  // Document builder part
  TextNode := Doc.CreateTextNodeBuf(ch, Count);
  TextNode.MayBeIgnorable := Whitespace;
  FCursor.AppendChild(TextNode);
end;

procedure TXMLReader.DoAttrText(ch: PWideChar; Count: Integer);
begin
  FCursor.AppendChild(Doc.CreateTextNodeBuf(ch, Count));
end;

procedure TXMLReader.DoComment(ch: PWideChar; Count: Integer);
var
  ElDef: TDOMElementDef;
  Node: TDOMComment;
begin
  // validation filter part
  if FValidate then
  begin
    ElDef := CurrentElementDef;
    if Assigned(ElDef) and (ElDef.ContentType = ctEmpty) then
      ValidationError('Comments are not allowed within EMPTY elements', []);
  end;

  // DOM builder part
  if (not FIgnoreComments) then
  begin
    Node := Doc.CreateCommentBuf(ch, Count);
    if Assigned(FCursor) then
      FCursor.AppendChild(Node)
    else
      Doc.InsertBefore(Node, FDocType);
  end;
end;

procedure TXMLReader.DoCDSect(ch: PWideChar; Count: Integer);
var
  s: WideString;
  ElDef: TDOMElementDef;
begin
  if FValidate then
  begin
    ElDef := CurrentElementDef;
    if Assigned(ElDef) and (ElDef.ContentType = ctChildren) then
      ValidationError('CDATA sections are not allowed in element-only content',[]);
  end;    
  if not FCDSectionsAsText then
  begin
    SetString(s, ch, Count);
    // SAX: LexicalHandler.StartCDATA;
    // SAX: ContentHandler.Characters(...);
    FCursor.AppendChild(doc.CreateCDATASection(s));
    // SAX: LexicalHandler.EndCDATA;
  end
  else
    FCursor.AppendChild(doc.CreateTextNodeBuf(ch, Count));
end;

procedure TXMLReader.DoNotationDecl(const aName, aPubID, aSysID: WideString);
var
  Notation: TDOMNotationEx;
begin
  if FDocType.Notations.GetNamedItem(aName) = nil then
  begin
    Notation := TDOMNotationEx(TDOMNotation.Create(doc));
    Notation.FName := aName;
    Notation.FPublicID := aPubID;
    Notation.FSystemID := aSysID;
    FDocType.Notations.SetNamedItem(Notation);
  end
  else
    ValidationError('Duplicate notation declaration: ''%s''', [aName]);
end;

procedure TXMLReader.PushVC(aElDef: TDOMElementDef);
var
  v: TElementValidator;
begin
  v := TElementValidator.Create;
  v.FElementDef := aElDef;
  v.Parent := FValidator;
  FValidator := v;
end;

procedure TXMLReader.PopVC;
var
  v: TElementValidator;
begin
  if Assigned(FValidator) then
  begin
    v := FValidator.Parent;
    FValidator.Free;
    FValidator := v;
  end;
end;

function TXMLReader.CurrentElementDef: TDOMElementDef;
begin
  if Assigned(FValidator) then
    Result := FValidator.FElementDef
  else
    Result := nil;
end;

{ TDOMAttrDef }

function TDOMAttrDef.AddEnumToken(const aValue: WideString): Boolean;
var
  I, L: Integer;
begin
  // TODO: this implementaion is the slowest possible...
  Result := False;
  L := Length(FEnumeration);
  for I := 0 to L-1 do
  begin
    if aValue = FEnumeration[I] then
      Exit;
  end;
  SetLength(FEnumeration, L+1);
  FEnumeration[L] := aValue;
  Result := True;
end;

function TDOMAttrDef.HasEnumToken(const aValue: WideString): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Length(FEnumeration) = 0 then
    Exit;
  for I := 0 to Length(FEnumeration)-1 do
  begin
    if FEnumeration[I] = aValue then
      Exit;
  end;
  Result := False;
end;

{ TElementValidator }

function TElementValidator.IsElementAllowed(const aName: DOMString): Boolean;
var
  I: Integer;
  Next: TContentParticle;
begin
  Result := True;
  // if element is not declared, non-validity has been already reported, no need to report again...
  if Assigned(FElementDef) then
  begin
    case FElementDef.ContentType of
      ctMixed: begin
        for I := 0 to FElementDef.RootCP.ChildCount-1 do
        begin
          if aName = FElementDef.RootCP.Children[I].Name then
          Exit;
        end;
        Result := False;
      end;

      ctEmpty: Result := False;

      ctChildren: begin
        if FCurCP = nil then
          Next := FElementDef.RootCP.FindFirst(aName)
        else
          Next := FCurCP.FindNext(aName, 0); { second arg ignored here }
        Result := Assigned(Next);
        if Result then
          FCurCP := Next
        else
          FFailed := True;  // used to prevent extra error at the end of element
      end;
      // ctAny: returns True by default
    end;
  end;
end;

function TElementValidator.Incomplete: Boolean;
begin
  if Assigned(FElementDef) and (FElementDef.ContentType = ctChildren) and (not FFailed) then
  begin
    if FCurCP <> nil then
      Result := FCurCP.MoreRequired(0) { arg ignored here }
    else
      Result := FElementDef.RootCP.IsRequired;
  end
  else
    Result := False;
end;

{ TContentParticle }

function TContentParticle.Add: TContentParticle;
begin
  if FChildren = nil then
    FChildren := TList.Create;
  Result := TContentParticle.Create;
  Result.FParent := Self;
  Result.FIndex := FChildren.Add(Result);
end;

destructor TContentParticle.Destroy;
var
  I: Integer;
begin
  if Assigned(FChildren) then
    for I := FChildren.Count-1 downto 0 do
      TObject(FChildren[I]).Free;
  FChildren.Free;
  inherited Destroy;
end;

function TContentParticle.GetChild(Index: Integer): TContentParticle;
begin
  Result := TContentParticle(FChildren[Index]);
end;

function TContentParticle.GetChildCount: Integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.Count
  else
    Result := 0;
end;

function TContentParticle.IsRequired: Boolean;
var
  I: Integer;
begin
  Result := (CPQuant = cqOnce) or (CPQuant = cqOnceOrMore);
  // do not return True if all children are optional
  if (CPType <> ctName) and Result then
  begin
    for I := 0 to ChildCount-1 do
    begin
      Result := Children[I].IsRequired;
      if Result then Exit;
    end;
  end;
end;

function TContentParticle.MoreRequired(ChildIdx: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if CPType = ctSeq then
  begin
    for I := ChildIdx + 1 to ChildCount-1 do
    begin
      Result := Children[I].IsRequired;
      if Result then Exit;
    end;
  end;
  if Assigned(FParent) then
    Result := FParent.MoreRequired(FIndex);
end;

function TContentParticle.FindFirst(const aName: DOMString): TContentParticle;
var
  I: Integer;
begin
  Result := nil;
  case CPType of
    ctSeq:
      for I := 0 to ChildCount-1 do with Children[I] do
      begin
        Result := FindFirst(aName);
        if Assigned(Result) or IsRequired then
          Exit;
      end;
    ctChoice:
      for I := 0 to ChildCount-1 do with Children[I] do
      begin
        Result := FindFirst(aName);
        if Assigned(Result) then
          Exit;
      end;
  else // ctName
    if aName = Self.Name then
      Result := Self
  end;
end;

function TContentParticle.FindNext(const aName: DOMString;
  ChildIdx: Integer): TContentParticle;
var
  I: Integer;
begin
  Result := nil;
  if CPType = ctSeq then   // search sequence to its end
  begin
    for I := ChildIdx + 1 to ChildCount-1 do with Children[I] do
    begin
      Result := FindFirst(aName);
      if (Result <> nil) or IsRequired then
        Exit;
    end;
  end;
  if (CPQuant = cqZeroOrMore) or (CPQuant = cqOnceOrMore) then
    Result := FindFirst(aName);
  if (Result = nil) and Assigned(FParent) then
    Result := FParent.FindNext(aName, FIndex);
end;

{ TDOMElementDef }

constructor TDOMElementDef.Create(aOwner: TDOMDocument);
begin
  inherited Create(aOwner);
  RootCP := TContentParticle.Create;
end;

destructor TDOMElementDef.Destroy;
begin
  RootCP.Free;
  inherited Destroy;
end;

{ plain calls }

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: Text);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Src := TXMLFileInputSource.Create(f);
  Src.SystemID := FilenameToURI(TTextRec(f).Name);
  Reader := TXMLReader.Create;
  try
    Reader.ProcessXML(Src);
    ADoc := TXMLDocument(Reader.Doc);
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream; const ABaseURI: String);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessXML(Src);
  finally
    ADoc := TXMLDocument(Reader.doc);
    Reader.Free;
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream);
begin
  ReadXMLFile(ADoc, f, 'stream:');
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);
var
  FileStream: TStream;
begin
  ADoc := nil;
  FileStream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadXMLFile(ADoc, FileStream, FilenameToURI(AFilename));
  finally
    FileStream.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: Text);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  Reader := TXMLReader.Create;
  try
    Src := TXMLFileInputSource.Create(f);
    Src.SystemID := FilenameToURI(TTextRec(f).Name);
    Reader.ProcessFragment(Src, AParentNode);
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const ABaseURI: String);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  Reader := TXMLReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessFragment(Src, AParentNode);
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream);
begin
  ReadXMLFragment(AParentNode, f, 'stream:');
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadXMLFragment(AParentNode, Stream, FilenameToURI(AFilename));
  finally
    Stream.Free;
  end;
end;


procedure ReadDTDFile(out ADoc: TXMLDocument; var f: Text);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLReader.Create;
  try
    Src := TXMLFileInputSource.Create(f);
    Src.SystemID := FilenameToURI(TTextRec(f).Name);
    Reader.ProcessDTD(Src);
    ADoc := TXMLDocument(Reader.doc);
  finally
    Reader.Free;
  end;
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream; const ABaseURI: String);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessDTD(Src);
    ADoc := TXMLDocument(Reader.doc);
  finally
    Reader.Free;
  end;
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream);
begin
  ReadDTDFile(ADoc, f, 'stream:');
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; const AFilename: String);
var
  Stream: TStream;
begin
  ADoc := nil;
  Stream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadDTDFile(ADoc, Stream, FilenameToURI(AFilename));
  finally
    Stream.Free;
  end;
end;




end.
