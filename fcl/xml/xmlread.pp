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
  EXMLReadError = class(Exception);

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

// =======================================================

implementation

uses
  UriParser;

type
  TSetOfChar = set of Char;

const
  Letter = ['A'..'Z', 'a'..'z'];
  Digit = ['0'..'9'];
  PubidChars: TSetOfChar = [' ', #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
    '#', '@', '$', '_', '%'];

type
  TDOMNotationEx = class(TDOMNotation);
  TDOMAttrEx = class(TDOMAttr);

  TXMLInputSource = class;
  TDOMElementDef = class;

  TDOMEntityEx = class(TDOMEntity)
  protected
    FInternal: Boolean;
    FResolved: Boolean;
    FOnStack: Boolean;
    FReplacementText: DOMString;
  end;

  // TODO: Do I need PEMap in DocType? Maybe move it to Reader itself?
  // (memory usage - they are not needed after parsing)
  TDOMDocumentTypeEx = class(TDOMDocumentType)
  private
    FHasPERefs: Boolean;
    FPEMap: TDOMNamedNodeMap;
    FElementDefs: TDOMNamedNodeMap;
    function GetPEMap: TDOMNamedNodeMap;
    function GetElementDefs: TDOMNamedNodeMap;
  protected
    property PEMap: TDOMNamedNodeMap read GetPEMap;
    property ElementDefs: TDOMNamedNodeMap read GetElementDefs;
    property HasPERefs: Boolean read FHasPERefs write FHasPERefs;
  public
    destructor Destroy; override;
  end;

  TXMLReader = class;
  TDecoder = class;
  TDecoderRef = class of TDecoder;

  TXMLInputSource = class
  private
    FBuf: PChar;
    FBufEnd: PChar;
    FEof: Boolean;
    FSurrogate: WideChar;
    FReader: TXMLReader;
    FParent: TXMLInputSource;
    FEntity: TObject;   // weak reference
    FCursor: TObject;   // weak reference
    FLine: Integer;
    FColumn: Integer;
    FSystemID: WideString;
    FPublicID: WideString;
    function GetSystemID: WideString;
    function GetPublicID: WideString;
  protected
    procedure FetchData; virtual;
  public
    constructor Create(const AData: WideString);
    function NextChar: WideChar; virtual;
    procedure Initialize; virtual;
    procedure SetEncoding(const AEncoding: string); virtual;
    property SystemID: WideString read GetSystemID write FSystemID;
    property PublicID: WideString read GetPublicID write FPublicID;
  end;

  TXMLDecodingSource = class(TXMLInputSource)
  private
    FDecoder: TDecoder;
    FSeenCR: Boolean;
    function InternalNextChar: WideChar;
    procedure DecodingError(const Msg: string); overload;
    procedure DecodingError(const Msg: string; const Args: array of const); overload;
  public
    destructor Destroy; override;
    function NextChar: WideChar; override;
    procedure SetEncoding(const AEncoding: string); override;
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

  TEntityResolveEvent = procedure(const PubID, SysID: WideString; var Source: TXMLInputSource) of object;
  TDeclType = (dtNone, dtXml, dtText);

  TXMLReader = class
  private
    FSource: TXMLInputSource;
    FCurChar: WideChar;
    FWhitespace: Boolean;
    FXML11: Boolean;
    FValue: TWideCharBuf;
    FName: TWideCharBuf;
    FCopyBuf: PWideCharBuf;
    FIntSubset: Boolean;
    FAllowedDecl: TDeclType;
    FDtdParsed: Boolean;
    FRecognizePE: Boolean;
    FStandalone: Boolean;          // property of Doc ?
    FInvalid: Boolean;
    // TODO: This array must be stored globally, not per instance
    FNamePages: PByteArray;
    FForbiddenAscii: TSetOfChar;
    FDocType: TDOMDocumentTypeEx;  // a shortcut
    FEntityLevel: Integer;
    FPreserveWhitespace: Boolean;
    FCreateEntityRefs: Boolean;
    procedure RaiseExpectedQmark;
    procedure GetChar;
    procedure GetCharRaw;
    procedure Unget(wc: WideChar);
    procedure Initialize(ASource: TXMLInputSource);
    procedure InitializeRoot(ASource: TXMLInputSource);
    procedure DoParseAttValue(Delim: WideChar);
    procedure DoParseFragment;
    procedure DoParseExtSubset(ASource: TXMLInputSource);
    function ContextPush(AEntity: TDOMEntityEx): Boolean;
    function ContextPop: Boolean;
    procedure XML11_BuildTables;
    function  XML11_CheckName: Boolean;
  protected
    FCursor: TDOMNode;

    procedure RaiseExc(const descr: String); overload;
    procedure RaiseExc(const descr: string; const args: array of const); overload;
    procedure RaiseExc(Expected: WideChar); overload;
    function  SkipWhitespace: Boolean;
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
    procedure SkipName;
    function SkipQuotedLiteral: Boolean;
    procedure ExpectAttValue;                                           // [10]
    procedure SkipPubidLiteral;                                         // [12]
    procedure SkipSystemLiteral(out Literal: WideString; Required: Boolean);
    procedure ParseComment;                                             // [15]
    procedure ParsePI;                                                  // [16]
    procedure ParseCDSect;                                              // [18]
    procedure ParseXmlOrTextDecl(TextDecl: Boolean);
    function  ParseEq: Boolean;                                         // [25]
    procedure ExpectEq;
    procedure ParseMisc;                                                // [27]
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

    procedure AssertPENesting(CurrentLevel: Integer);
    procedure ParseEntityDecl;
    procedure ParseEntityDeclValue(Delim: WideChar);
    procedure ParseAttlistDecl;
    procedure ExpectChoiceOrSeq;
    procedure ParseMixedOrChildren;
    procedure ParseElementDecl;
    procedure ParseNotationDecl;
    function ResolveEntity(const SystemID, PublicID: WideString; out Source: TXMLInputSource): Boolean;
    procedure ProcessDefaultAttributes(Element: TDOMElement);
    procedure ValidationError(const Msg: string; const args: array of const);
  public
    doc: TDOMDocument;
    constructor Create;
    destructor Destroy; override;
    procedure ProcessXML(ASource: TXMLInputSource);                // [1]
    procedure ProcessFragment(ASource: TXMLInputSource; AOwner: TDOMNode);
    procedure ProcessDTD(ASource: TXMLInputSource);               // ([29])
  end;

  // AttDef/ElementDef support
  TAttrDataType = (
    DT_CDATA,
    DT_ID,
    DT_IDREF,
    DT_IDREFS,
    DT_ENTITY,
    DT_ENTITIES,
    DT_NMTOKEN,
    DT_NMTOKENS,
    DT_NOTATION
  );

  TAttrDefault = (
    AD_IMPLIED,
    AD_DEFAULT,
    AD_REQUIRED,
    AD_FIXED
  );

  TDOMAttrDef = class(TDOMAttr)
  protected
    FDataType: TAttrDataType;
    FDefault: TAttrDefault;
    // FEnumeration: TWideStringList? array of WideStrings?
  end;

  TDOMElementDef = class(TDOMElement);
  

{$i names.inc}

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
  GetMem(ABuffer.Buffer, ABuffer.MaxLength*SizeOf(WideChar));
end;

procedure BufAppend(var ABuffer: TWideCharBuf; wc: WideChar);
begin
  if ABuffer.Length >= ABuffer.MaxLength then
  begin
    ABuffer.MaxLength := ABuffer.MaxLength * 2;
    ReallocMem(ABuffer.Buffer, ABuffer.MaxLength * SizeOf(WideChar));
  end;
  ABuffer.Buffer[ABuffer.Length] := wc;
  Inc(ABuffer.Length);
end;

function IsValidEncName(const s: WideString): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (s = '') or (s[1] > #255) or not (char(s[1]) in ['A'..'Z', 'a'..'z']) then
    Exit;
  for I := 2 to Length(s) do
    if (s[I] > #255) or not (char(s[I]) in ['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']) then
      Exit;
  Result := True;
end;

{ TDOMDocumentTypeEx }

destructor TDOMDocumentTypeEx.Destroy;
begin
  FPEMap.Free;
  FElementDefs.Free;
  inherited Destroy;
end;

function TDOMDocumentTypeEx.GetElementDefs: TDOMNamedNodeMap;
begin
  if FElementDefs = nil then
    FElementDefs := TDOMNamedNodeMap.Create(Self, ELEMENT_NODE);
  Result := FElementDefs;
end;

function TDOMDocumentTypeEx.GetPEMap: TDOMNamedNodeMap;
begin
  if FPEMap = nil then
    FPEMap := TDOMNamedNodeMap.Create(Self, ENTITY_NODE);
  Result := FPEMap;
end;


// TODO: These classes still cannot be considered as the final solution...

{ TXMLInputSource }

constructor TXMLInputSource.Create(const AData: WideString);
begin
  inherited Create;
  FBuf := PChar(PWideChar(AData));
  FBufEnd := FBuf + Length(AData) * sizeof(WideChar);
end;

procedure TXMLInputSource.Initialize;
begin
  FLine := 1;
  FColumn := 0;
end;

function TXMLInputSource.NextChar: WideChar;
begin
  if FSurrogate <> #0 then
  begin
    Result := FSurrogate;
    FSurrogate := #0;
  end
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
  // TODO: Column counting - surrogate pair is a single char!
  if Result = #10 then
  begin
    Inc(FLine);
    FColumn := 0;
  end
  else
    Inc(FColumn);
end;

procedure TXMLDecodingSource.DecodingError(const Msg: string);
begin
  FReader.RaiseExc(Msg);
end;

procedure TXMLDecodingSource.DecodingError(const Msg: string;
  const Args: array of const);
begin
  FReader.RaiseExc(Msg, Args);
end;

procedure TXMLInputSource.FetchData;
begin
  FEof := True;
end;

procedure TXMLInputSource.SetEncoding(const AEncoding: string);
begin
  // do nothing
end;

function TXMLInputSource.GetPublicID: WideString;
begin
  if FPublicID <> '' then
    Result := FPublicID
  else if Assigned(FParent) then
    Result := FParent.PublicID
  else
    Result := '';    
end;

function TXMLInputSource.GetSystemID: WideString;
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

function TXMLDecodingSource.InternalNextChar: WideChar;
begin
  // TODO: find a place for it, finally...
  if FSurrogate <> #0 then
  begin
    Result := FSurrogate;
    FSurrogate := #0;
    Exit;
  end;
  if FBufEnd <= FBuf then
    FetchData;
  if not FEof then
    Result := FDecoder.DecodeNext
  else
    Result := #0;
end;

function TXMLDecodingSource.NextChar: WideChar;
begin
  Result := InternalNextChar;
  if FSeenCR then
  begin
    if (Result = #10) or ((Result = #$85) and FReader.FXML11) then
      Result := InternalNextChar;
    FSeenCR := False;
  end;
  case Result of
    #13: begin
           FSeenCR := True;
           Result := #10;
         end;

    #$85, #$2028:
      if FReader.FXML11 then
        Result := #10;
  end;
  if (Result < #256) and (char(Result) in FReader.FForbiddenAscii) or
    ((ord(Result) or 1) = $FFFF) then
    DecodingError('Invalid character');

  // TODO: Column counting - surrogate pair is a single char!
  if Result = #10 then
  begin
    Inc(FLine);
    FColumn := 0;
  end
  else
    Inc(FColumn);
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

procedure TXMLDecodingSource.SetEncoding(const AEncoding: string);
var
  NewDecoder: TDecoderRef;
begin
  if FDecoder.Supports(AEncoding) then // no change needed
    Exit;
  // hardcoded stuff - special case of UCS2
  if FDecoder is TUCS2Decoder then
  begin
    // check for 'UTF-16LE' or 'UTF-16BE'
    if SameText(AEncoding, TUCS2Decoder(FDecoder).FEncoding) then
      Exit
    else
      DecodingError('Current encoding cannot be switched to ''%s''', [AEncoding]);
  end;
  NewDecoder := FindDecoder(AEncoding);
  if Assigned(NewDecoder) then
  begin
    FDecoder.Free;
    FDecoder := NewDecoder.Create(Self);
  end
  else
    DecodingError('Encoding ''%s'' is not supported', [AEncoding]);
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
    Result := WideChar(FBuf[0]);
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
    Result := WideChar(FBuf[0]);
    Inc(FBuf);
    if Result < #$80 then
      Exit;
    if Byte(Result) and $40 = 0 then
      DecodingError('Invalid UTF8 sequence start byte');
    bc := 1;
    if Byte(Result) and $20 <> 0 then
    begin
      Inc(bc);
      if Byte(Result) and $10 <> 0 then
      begin
        Inc(bc);
        if Byte(Result) and $8 <> 0 then
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
        DecodingError('Invalid byte in UTF8 sequence');
      Value := (Value shl 6) or (Cardinal(FBuf[0]) and $3F);
      Inc(FBuf);
      Dec(bc);
    end;
    Value := Value and MaxCode[I];
    // RFC2279 check
    if Value <= MaxCode[I-1] then
      DecodingError('Invalid UTF8 sequence');
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

function TXMLReader.ResolveEntity(const SystemID, PublicID: WideString; out Source: TXMLInputSource): Boolean;
var
  AbsSysID: WideString;
  Filename: string;
  Stream: TStream;
begin
  Result := False;

  if ResolveRelativeURI(FSource.SystemID, SystemID, AbsSysID) then
  begin
    Source := nil;
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

procedure TXMLReader.InitializeRoot(ASource: TXMLInputSource);
begin
  Initialize(ASource);
  GetChar;
  // TODO: presence of BOM must prevent UTF-8 encoding from being changed
  CheckForChar(#$FEFF);   // skip BOM, if one is present
end;


procedure TXMLReader.Initialize(ASource: TXMLInputSource);
begin
  FSource := ASource;
  FSource.FReader := Self;
  FSource.Initialize;
end;

procedure TXMLReader.GetCharRaw;
begin
  FCurChar := FSource.NextChar;
  FWhitespace := (FCurChar = #32) or (FCurChar = #10) or
                  (FCurChar = #9) or (FCurChar = #13);
  // Used for handling the internal DTD subset
  if Assigned(FCopyBuf) and (FSource.FParent = nil) then
    BufAppend(FCopyBuf^, FCurChar);
end;

procedure TXMLReader.GetChar;
begin
  GetCharRaw;
  if not FRecognizePE then
    Exit;
  if (FCurChar = #0) and ContextPop then
  begin
    Unget(FCurChar);
    FCurChar := #32;
    FWhitespace := True;
  end
  else if FCurChar = '%' then
  begin
    FCurChar := FSource.NextChar;
    if not CheckName then
    begin
      Unget(FCurChar);
      FCurChar := '%';
      Exit;
    end;
    if FCurChar = ';' then // "%pe1;%pe2" - must not recognize pe2 immediately!
      GetCharRaw
    else
      RaiseExc(WideChar(';'));
    StartPE;
    FCurChar := #32;
    FWhitespace := True;
  end;  
end;

procedure TXMLReader.Unget(wc: WideChar);
begin
  FSource.FSurrogate := wc;
end;

procedure TXMLReader.RaiseExpectedQmark;
begin
  RaiseExc('Expected single or double quote');
end;

procedure TXMLReader.RaiseExc(Expected: WideChar);
begin
// FIX: don't output what is found - anything may be found, including exploits...
  RaiseExc('Expected "%1s"', [string(Expected)]);
end;

procedure TXMLReader.RaiseExc(const descr: String);
begin
  raise EXMLReadError.CreateFmt('In ''%s'' (line %d pos %d): %s', [FSource.SystemID, FSource.FLine, FSource.FColumn, descr]);
end;

procedure TXMLReader.RaiseExc(const descr: string; const args: array of const);
begin
  RaiseExc(Format(descr, args));
end;

function TXMLReader.SkipWhitespace: Boolean;
begin
  Result := False;
  while FWhitespace do
  begin
    GetChar;
    Result := True;
  end;
end;

procedure TXMLReader.ExpectWhitespace;
begin
  if not SkipWhitespace then
    RaiseExc('Expected whitespace');
end;

procedure TXMLReader.ExpectChar(wc: WideChar);
begin
  if FCurChar = wc then
    GetChar
  else
    RaiseExc(wc);
end;

procedure TXMLReader.ExpectString(const s: String);
var
  I: Integer;
begin
  for I := 1 to Length(s) do
  begin
    if FCurChar <> WideChar(s[i]) then
      RaiseExc('Expected "%s"', [s]);
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
  while (ord(FCurChar) < 256) and (char(FCurChar) in ValidChars) do
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
  // Naming bitmap: Point to static data for XML 1.0,
  // and allocate buffer in XML11_BuildTables when necessary.
  FNamePages := @NamePages;
  BufAllocate(FName, 128);
  BufAllocate(FValue, 512);
  FForbiddenAscii := [#1..#8, #11..#12, #14..#31];
  // TODO: put under user control
  FPreserveWhitespace := True;
  FCreateEntityRefs := True;
end;

destructor TXMLReader.Destroy;
begin
  if FXML11 then
    FreeMem(FNamePages);
  FreeMem(FName.Buffer);
  FreeMem(FValue.Buffer);
  while ContextPop do;     // clean input stack
  FSource.Free;
  inherited Destroy;
end;

procedure TXMLReader.XML11_BuildTables;
var
  I: Integer;
begin
  if not FXML11 then
    GetMem(FNamePages, 512);
  FXML11 := True;
  for I := 0 to 255 do
    FNamePages^[I] := ord(Byte(I) in Xml11HighPages);
  FNamePages^[0] := 2;
  FNamePages^[3] := $2c;
  FNamePages^[$20] := $2a;
  FNamePages^[$21] := $2b;
  FNamePages^[$2f] := $29;
  FNamePages^[$30] := $2d;
  FNamePages^[$fd] := $28;

  Move(FNamePages^, FNamePages^[256], 256);
  FNamePages^[$100] := $19;
  FNamePages^[$103] := $2E;
  FNamePages^[$120] := $2F;
  FForbiddenAscii := [#1..#8, #11..#12, #14..#31, #$7F..#$84, #$86..#$9F];
end;


procedure TXMLReader.ProcessXML(ASource: TXMLInputSource);
begin
  doc := TXMLDocument.Create;
  FCursor := doc;
  InitializeRoot(ASource);

  FAllowedDecl := dtXml;
  ParseMisc;
  FDtdParsed := True;
  if FDocType = nil then
    ValidationError('Missing DTD', []);
  if CheckName then
    ParseElement
  else
    RaiseExc('Expected element');
  ParseMisc;
  if Assigned(FDocType) and (doc.DocumentElement.TagName <> FDocType.Name) then
    ValidationError('DTD name does not match root element', []);

  if FCurChar <> #0 then
    RaiseExc('Text after end of document element found');
end;

procedure TXMLReader.ProcessFragment(ASource: TXMLInputSource; AOwner: TDOMNode);
begin
  doc := AOwner.OwnerDocument;
  FCursor := AOwner;
  InitializeRoot(ASource);
  FXML11 := doc.InheritsFrom(TXMLDocument) and (TXMLDocument(doc).XMLVersion = '1.1');
  FAllowedDecl := dtText;
  DoParseFragment;
end;

// XML 1.1 allowed range $10000..$EFFFF is [D800..DB7F] followed by [DC00..DFFF]
function TXMLReader.XML11_CheckName: Boolean;
begin
  if (FCurChar >= #$D800) and (FCurChar <= #$DB7F) then
  begin
    BufAppend(FName, FCurChar);
    GetCharRaw;
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
  RaiseExc('Name starts with invalid character');
end;

function TXMLReader.ExpectName: WideString;
begin
  if not CheckName then
    RaiseNameNotFound;

  SetString(Result, FName.Buffer, FName.Length);
end;

procedure TXMLReader.SkipName;
begin
  if not CheckName then
    RaiseNameNotFound;
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
          RaiseExc('Invalid character reference');
      $09, $0A, $0D, $20..$D7FF, $E000..$FFFD:
        BufAppend(FValue, WideChar(Value));
      $10000..$10FFFF:
        begin
          BufAppend(FValue, WideChar($D7C0 + (Value shr 10)));
          BufAppend(FValue, WideChar($DC00 xor (Value and $3FF)));
        end;
    else
      RaiseExc('Invalid character reference');
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
      RaiseExc('Literal "<" in attribute value')
    else if FCurChar <> '&' then
    begin
      if FWhitespace then
        FCurChar := #32;
      BufAppend(FValue, FCurChar);
      GetCharRaw;
    end
    else
    begin
      GetCharRaw; // skip '&'
      if ParseCharRef then
        Continue;

      RefNode := ParseReference;
      if Assigned(RefNode) then
      begin
        if FValue.Length > 0 then
        begin
          FCursor.AppendChild(doc.CreateTextNodeBuf(FValue.Buffer, FValue.Length));
          FValue.Length := 0;
        end;

        if RefNode.SystemID <> '' then
          RaiseExc('External entity reference is not allowed in attribute value');

        IncludeEntity(RefNode, True);
      end;
    end;
  end; // while
  if FValue.Length > 0 then
  begin
    FCursor.AppendChild(doc.CreateTextNodeBuf(FValue.Buffer, FValue.Length));
    FValue.Length := 0;
  end;
end;

procedure TXMLReader.DoParseFragment;
begin
  ParseContent;
  if FCurChar <> #0 then
    RaiseExc('Closing tag not allowed here');
end;


function TXMLReader.ContextPush(AEntity: TDOMEntityEx): Boolean;
var
  Src: TXMLInputSource;
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
    Src := TXMLInputSource.Create(AEntity.FReplacementText);

  AEntity.FOnStack := True;
  Src.FEntity := AEntity;

  Src.FParent := FSource;
  Src.FCursor := FCursor;
  Unget(FCurChar);             // remember FCurChar in previous context

  Inc(FEntityLevel);
  Initialize(Src);
  Result := True;
end;

function TXMLReader.ContextPop: Boolean;
var
  Src: TXMLInputSource;
begin
  Result := Assigned(FSource.FParent);
  if Result then
  begin
    Src := FSource.FParent;
    if Assigned(FSource.FEntity) then
      TDOMEntityEx(FSource.FEntity).FOnStack := False;
    FCursor := TDOMNode(FSource.FCursor);
    FSource.Free;
    FSource := Src;
    Dec(FEntityLevel);
    GetChar;                       // re-classify - case of "%pe1;%pe2;"
  end;
end;

procedure TXMLReader.IncludeEntity(AEntity: TDOMEntityEx; InAttr: Boolean);
var
  Node, Child: TDOMNode;
begin
  if not AEntity.FResolved then
  begin
    if AEntity.FOnStack then
      RaiseExc('Entity ''%s'' recursively references itself', [AEntity.NodeName]);

    if ContextPush(AEntity) then
    begin
      GetCharRaw;
      CheckForChar(#$FEFF);

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
  if FCreateEntityRefs or (not AEntity.FResolved) then
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
  PEnt := FDocType.PEMap.GetNamedItem(PEName) as TDOMEntityEx;
  if PEnt = nil then    // TODO -cVC: Referencing undefined PE
  begin                 // (These are classified as 'optional errors'...)
//    ValidationError('Undefined parameter entity referenced: %s', [PEName]);
    Exit;
  end;

  if PEnt.FOnStack then
    RaiseExc('Entity ''%%%s'' recursively references itself', [PEnt.NodeName]);

  ContextPush(PEnt);
end;

function TXMLReader.ParseReference: TDOMEntityEx;
var
  RefName: WideString;
  Predef: WideChar;
begin
  Result := nil;
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
      if FStandalone or (FDocType = nil) or not (FDocType.HasPERefs or (FDocType.SystemID <> '')) then
        RaiseExc('Undefined entity ''%s'' referenced', [RefName])
      else
        ValidationError('Undefined entity ''%s'' referenced', [RefName]);
    end
    else
    begin
      if FStandalone and (not Result.FInternal) then
        RaiseExc('Standalone constraint violation');
      if Result.NotationName <> '' then
        RaiseExc('Reference to unparsed entity ''%s''', [RefName]);
    end;
  end;
end;

procedure TXMLReader.ProcessTextAndRefs;
var
  nonWs: Boolean;
  last: WideChar;
  RefNode: TDOMEntityEx;
begin
  FValue.Length := 0;
  nonWs := False;
  FAllowedDecl := dtNone;
  while (FCurChar <> '<') and (FCurChar <> #0) do
  begin
    if FCurChar <> '&' then
    begin
      if not FWhitespace then
        nonWs := True;
      BufAppend(FValue, FCurChar);
      if FCurChar = '>' then
        with FValue do
          if (Length >= 3) and
          (Buffer[Length-2] = ']') and (Buffer[Length-3] = ']') then
             RaiseExc('Literal '']]>'' is not allowed in text');
      GetCharRaw;
    end
    else
    begin
      GetCharRaw; // skip '&'
      if ParseCharRef then
      begin
        last := FValue.Buffer[FValue.Length-1];
        if (last <> #9) and (last <> #10) and (last <> #13) and (last <> #32) then
          nonWs := True;
        Continue;
      end;
      nonWs := True;
      RefNode := ParseReference;
      if Assigned(RefNode) then
      begin
        if (nonWs or FPreserveWhitespace) and (FValue.Length > 0)  then
        begin
          FCursor.AppendChild(doc.CreateTextNodeBuf(FValue.Buffer, FValue.Length));
          FValue.Length := 0;
          nonWs := False;
        end;
        IncludeEntity(RefNode, False);
      end;
    end;
  end; // while
  if (nonWs or FPreserveWhitespace) and (FValue.Length > 0)  then
  begin
    FCursor.AppendChild(doc.CreateTextNodeBuf(FValue.Buffer, FValue.Length));
    FValue.Length := 0;
  end;
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
begin
  if SkipQuotedLiteral then
  begin
    for I := 0 to FValue.Length-1 do
      if (FValue.Buffer[I] > #255) or not (Char(FValue.Buffer[I]) in PubidChars) then
        RaiseExc('Illegal Public ID literal')
  end
  else
    RaiseExpectedQMark;
end;

procedure TXMLReader.SkipSystemLiteral(out Literal: WideString; Required: Boolean);
begin
  if SkipQuotedLiteral then
    SetString(Literal, FValue.Buffer, FValue.Length)
  else if Required then
    RaiseExpectedQMark;
end;

procedure TXMLReader.ParseComment;    // [15]
begin
  ExpectString('--');
  FValue.Length := 0;
  repeat
    BufAppend(FValue, FCurChar);
    GetCharRaw;
    with FValue do
      if (Length >= 2) and (Buffer[Length-1] = '-') and
      (Buffer[Length-2] = '-') then
      begin
        Dec(Length, 2);
        if Assigned(FCursor) then
          FCursor.AppendChild(doc.CreateCommentBuf(Buffer, Length));
        ExpectChar('>');
        Exit;
      end;
  until FCurChar = #0;
  RaiseExc('Unterminated comment');
end;

procedure TXMLReader.ParsePI;                    // [16]
var
  Name, Value: WideString;
begin
  GetCharRaw;      // skip '?'
  Name := ExpectName;

  with FName do
    if (Length = 3) and
     ((Buffer[0] = 'X') or (Buffer[0] = 'x')) and
     ((Buffer[1] = 'M') or (Buffer[1] = 'm')) and
     ((Buffer[2] = 'L') or (Buffer[2] = 'l')) then
  begin
    if Name <> 'xml' then
      RaiseExc('''xml'' is a reserved word; it must be lowercase');
    if FAllowedDecl <> dtNone then
    begin
      ParseXmlOrTextDecl(FAllowedDecl = dtText);
      FAllowedDecl := dtNone;
      Exit;
    end
    else
      RaiseExc('XML declaration not allowed here');
  end;

  if FCurChar <> '?' then
    ExpectWhitespace;

  FAllowedDecl := dtNone;  
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
        if Assigned(FCursor) then
          FCursor.AppendChild(Doc.CreateProcessingInstruction(Name, Value));
        Exit;
      end;
  until FCurChar = #0;
  RaiseExc('Unterminated processing instruction');
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
    SkipSystemLiteral(TmpStr, True);
    IsXML11 := False;
    if TmpStr = '1.1' then     // Checking for bad chars is implied
      IsXML11 := True
    else if TmpStr <> '1.0' then
      RaiseExc('Illegal version number');

    if not TextDecl then
    begin
      if doc.InheritsFrom(TXMLDocument) then
        TXMLDocument(doc).XMLVersion := TmpStr;
      if IsXML11 then
        XML11_BuildTables;
    end
    else   // parsing external entity
      if IsXML11 and not FXML11 then
        RaiseExc('XML 1.0 document cannot invoke XML 1.1 entities');

    if FCurChar <> '?' then
      ExpectWhitespace;
  end;

  // EncodingDecl: required in TextDecl, optional in XmlDecl
  if TextDecl or (FCurChar = 'e') then                    // [80]
  begin
    ExpectString('encoding');
    ExpectEq;
    SkipSystemLiteral(TmpStr, True);

    if not IsValidEncName(TmpStr) then
      RaiseExc('Illegal encoding name');

    FSource.SetEncoding(TmpStr);  // <-- Wide2Ansi conversion here
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
    SkipSystemLiteral(TmpStr, True);
    if TmpStr = 'yes' then
      FStandalone := True
    else if TmpStr <> 'no' then
      RaiseExc('Only "yes" or "no" are permitted as values of "standalone"');
    SkipWhitespace;
  end;

  ExpectString('?>');
end;

procedure TXMLReader.ParseDoctypeDecl;    // [28]
var
  IntSubset: TWideCharBuf;
  Src, OldSrc: TXMLInputSource;
begin
  FAllowedDecl := dtNone;

  if FDtdParsed then
    RaiseExc('Markup declaration not allowed here');

  ExpectString('DOCTYPE');  // gives possibly incorrect error message
  ExpectWhitespace;

  FDocType := TDOMDocumentTypeEx.Create(doc);
  FDtdParsed := True;
{ To comply with certain output tests, we must insert PIs coming from internal
  subset before DocType node. This looks very synthetic, but let it be...
  Moreover, this code actually duplicates such PIs }
  try
    FDocType.FName := ExpectName;
    ExpectWhitespace;
    ParseExternalID(FDocType.FSystemID, FDocType.FPublicID, False);
    SkipWhitespace;

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
      SkipWhitespace;
    end;
    ExpectChar('>');

    if FDocType.SystemID <> '' then
    begin
      if ResolveEntity(FDocType.SystemID, FDocType.PublicID, Src) then
      begin
        OldSrc := FSource;
        Unget(FCurChar);
        FCursor := nil;
        try
          DoParseExtSubset(Src);
        finally
          while ContextPop do;   // Cleanup after possible exceptions
          FSource.Free;
          FSource := OldSrc;
          GetChar;
          FCursor := Doc;
        end;
      end;
    end;
  finally
    doc.AppendChild(FDocType);
  end;
end;

procedure TXMLReader.ParseMisc;
begin
  repeat
    if SkipWhitespace then
      FAllowedDecl := dtNone;
    if not CheckForChar('<') then
      Break;
    if CheckForChar('!') then
    begin
      FAllowedDecl := dtNone;
      if FCurChar = '-' then
        ParseComment
      else
        ParseDoctypeDecl;
    end
    else
      if FCurChar = '?' then
        ParsePI
      else
        Break;
  until FCurChar = #0;
  FAllowedDecl := dtNone;
end;

function TXMLReader.ParseEq: Boolean;    // [25]
begin
  while FWhitespace do GetCharRaw;
  Result := FCurChar = '=';
  if Result then
  begin
    GetCharRaw;
    while FWhitespace do GetCharRaw;
  end;
end;

procedure TXMLReader.ExpectEq;
begin
  if not ParseEq then
    RaiseExc('Expected "="');
end;


{ DTD stuff }

procedure TXMLReader.AssertPENesting(CurrentLevel: Integer);
begin
  if CurrentLevel <> FEntityLevel then
    ValidationError('Parameter entities must be properly nested', []);
end;

// content model

type
  TElementContentType = (
    ctEmpty,
    ctAny,
    ctMixed,
    ctName,
    ctChoice,
    ctSeq
  );

  TElementContentQuant = (
    cqNone,
    cqOpt,
    cqReq,
    cqPlus
  );

{
  TElementContent = record
    ContentType: TElementContentType;
    ContentQuant: TElementContentQuant;
    Name: WideString;
    Children: array of TElementContent;
  end;
}

procedure TXMLReader.ExpectChoiceOrSeq();                  // [49], [50]
var
  Delim: WideChar;
  PELevel: Integer;
begin
  Delim := #0;
  repeat
    SkipWhitespace;
    if FCurChar = '(' then
    begin
      PELevel := FEntityLevel;
      GetChar;
      ExpectChoiceOrSeq;
      AssertPENesting(PELevel);
      GetChar;
    end
    else
      SkipName;
    if CheckForChar('?') then
    else if CheckForChar('*') then
    else if CheckForChar('+') then;

    SkipWhitespace;
    if FCurChar = ')' then
      Break;
    if Delim = #0 then
    begin
      if (FCurChar = '|') or (FCurChar = ',') then
        Delim := FCurChar
      else
        RaiseExc('Expected "|" or ","');
    end
    else
      if FCurChar <> Delim then
        RaiseExc(Delim);
    GetChar; // skip delimiter
  until False;
end;

procedure TXMLReader.ParseMixedOrChildren;
var
  PELevel: Integer;
  NeedAsterisk: Boolean;
begin
  PELevel := FEntityLevel;
  GetChar;     // starting bracket
  SkipWhitespace;
  if CheckForChar('#') then       // Mixed section [51]
  begin
    ExpectString('PCDATA');
    SkipWhitespace;
    NeedAsterisk := False;
    while FCurChar <> ')' do
    begin
      ExpectChar('|');
      NeedAsterisk := True;
      SkipWhitespace;
      SkipName;
      SkipWhitespace;
    end;
    AssertPENesting(PELevel);
    GetChar;
    if NeedAsterisk then
      ExpectChar('*')
    else
      CheckForChar('*');
  end
  else       // Parse Children section [47]
  begin
    ExpectChoiceOrSeq;
    AssertPENesting(PELevel);
    GetChar;
    if CheckForChar('?') then
    else if CheckForChar('*') then
    else if CheckForChar('+') then;
  end;
end;

procedure TXMLReader.ParseElementDecl;            // [45]
begin
  SkipName;
  ExpectWhitespace;

   // Get contentspec [46]
  if FCurChar = 'E' then
    ExpectString('EMPTY')
  else if FCurChar = 'A' then
    ExpectString('ANY')
  else if FCurChar = '(' then
    ParseMixedOrChildren
  else
    RaiseExc('Invalid content specification');
end;


procedure TXMLReader.ParseNotationDecl;        // [82]
var
  Notation: TDOMNotationEx;
begin
  Notation := TDOMNotationEx(TDOMNotation.Create(Doc));
  try
    Notation.FName := ExpectName;
    ExpectWhitespace;
    if not ParseExternalID(Notation.FSystemID, Notation.FPublicID, True) then
      RaiseExc('Expected external or public ID');
  except
    Notation.Free;
    raise;
  end;

  if FDocType.Notations.GetNamedItem(Notation.FName) = nil then
    FDocType.Notations.SetNamedItem(Notation)
  else
  begin
    ValidationError('Duplicate notation declaration: %s', [Notation.FName]);
    Notation.Free;
  end;
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
    // TODO -cVC: must distinguish ElementDef created here from one explicitly declared
    ElDef := TDOMElementDef.Create(doc);
    ElDef.FNodeName := Token;
    FDocType.ElementDefs.SetNamedItem(ElDef);
  end;
  SkipWhitespace;
  while FCurChar <> '>' do
  begin
    SkipWhitespace;                     { !!! }
    AttDef := TDOMAttrDef.Create(doc);
    try
      AttDef.FName := ExpectName;
      ExpectWhitespace;
      Token := GetString(['A'..'Z']);     // Get AttType [54], [55], [56]
      if Token = 'CDATA' then
        AttDef.FDataType := DT_CDATA
      else if Token = 'ID' then
        AttDef.FDataType := DT_ID
      else if Token = 'IDREF' then
        AttDef.FDataType := DT_IDREF
      else if Token = 'IDREFS' then
        AttDef.FDataType := DT_IDREFS
      else if Token = 'ENTITY' then
        AttDef.FDataType := DT_ENTITY
      else if Token = 'ENTITIES' then
        AttDef.FDataType := DT_ENTITIES
      else if Token = 'NMTOKEN' then
        AttDef.FDataType := DT_NMTOKEN
      else if Token = 'NMTOKENS' then
        AttDef.FDataType := DT_NMTOKENS
      else if Token = 'NOTATION' then     // [57], [58]
      begin
        AttDef.FDataType := DT_NOTATION;
        ExpectWhitespace;
        ExpectChar('(');
        repeat
          SkipWhitespace;
          SkipName;
          SkipWhitespace;
        until not CheckForChar('|');
        ExpectChar(')');
      end
      else
      if CheckForChar('(') then     // [59]
      begin
        AttDef.FDataType := DT_NMTOKEN;
        repeat
          SkipWhitespace;
          if not CheckNmToken then
            RaiseNameNotFound;      // not completely correct error message
          SkipWhitespace;
        until not CheckForChar('|');
        ExpectChar(')');
      end else
        RaiseExc('Invalid tokenized type');

      ExpectWhitespace;

      // Get DefaultDecl [60]
      ValueRequired := False;
      if CheckForChar('#') then
      begin
        Token := GetString(['A'..'Z']);
        if Token = 'REQUIRED' then
          AttDef.FDefault := AD_REQUIRED
        else if Token = 'IMPLIED' then
          AttDef.FDefault := AD_IMPLIED
        else if Token = 'FIXED' then
        begin
          AttDef.FDefault := AD_FIXED;
          ExpectWhitespace;
          ValueRequired := True;
        end
        else
          RaiseExc('Illegal attribute default');
      end
      else
      begin
        AttDef.FDefault := AD_DEFAULT;
        ValueRequired := True;
      end;
      
      if ValueRequired then
      begin
        SaveCurNode := FCursor;
        FCursor := AttDef;
// tricky moment, no tests for that        
{       FRecognizePE := False;  }        // TODO: shall it really be disabled?
        try
          ExpectAttValue;
        finally
          FCursor := SaveCurNode;
{          FRecognizePE := not FIntSubset; }
        end;
        if AttDef.FDataType = DT_ID then
          ValidationError('Attributes of type ID must not have a default value',[]);
      end;

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
  Src: TXMLInputSource;
begin
  Src := FSource;
  // "Included in literal": process until delimiter hit IN SAME context
  while not ((FSource = Src) and CheckForChar(Delim)) do
  if ParsePEReference then
  begin
    if FIntSubset and (FSource.FParent = nil) then
      RaiseExc('PE references in internal subset not allowed inside declarations');
    StartPE;
    GetCharRaw;  
  end
  else if FCurChar = '&' then  // CharRefs: include, EntityRefs: bypass
  begin
    GetCharRaw;
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
    Map := FDocType.PEMap;
  end;

  Entity := TDOMEntityEx.Create(Doc);
  try
    Entity.FInternal := FIntSubset and (FSource.FParent = nil);
    Entity.FName := ExpectName;
    ExpectWhitespace;

    if (FCurChar = '"') or (FCurChar = '''') then
    begin
      NDataAllowed := False;
      Delim := FCurChar;
      FRecognizePE := False;   // PERef right after delimiter should not be recognized
      GetCharRaw;              // at char level - we process it 'manually'
      FValue.Length := 0;
      ParseEntityDeclValue(Delim);
      FRecognizePE := not FIntSubset;
      SetString(Entity.FReplacementText, FValue.Buffer, FValue.Length);
    end
    else
      if not ParseExternalID(Entity.FSystemID, Entity.FPublicID, False) then
        RaiseExc('Expected entity value or external ID');

    if NDataAllowed then                // [76]
    begin
      if FCurChar <> '>' then
        ExpectWhitespace;
      if FCurChar = 'N' then
      begin
        ExpectString('NDATA');
        ExpectWhitespace;
        SkipName;
        // TODO -cVC: Notation declared. Here or after all has been read?
        SetString(Entity.FNotationName, FName.Buffer, FName.Length);
        if FDocType.Notations.GetNamedItem(Entity.NotationName) = nil then
          ValidationError('Reference to undeclared notation ''%s''', [Entity.NotationName]);
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
  PELevel: Integer;
begin
  IncludeLevel := 0;
  IgnoreLevel := 0;
  repeat
    if SkipWhitespace then
      FAllowedDecl := dtNone;

    if ParsePEReference then     // PERef between declarations should always be recognized
    begin
      FAllowedDecl := dtNone;
      if Assigned(FDocType) then
        FDocType.HasPERefs := True;
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

    PELevel := FEntityLevel;
    GetCharRaw;

    if CheckForChar('!') then
    begin
      FAllowedDecl := dtNone;
      if FCurChar = '-' then
        ParseComment
      else if FCurChar = '[' then
      begin
        if FIntSubset and (FSource.FParent = nil) then
          RaiseExc('Conditional sections not allowed in internal subset');

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
          RaiseExc('Expected "INCLUDE" or "IGNORE"');
        AssertPENesting(PELevel);
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
          RaiseExc('Illegal markup declaration');

        SkipWhitespace;
        FRecognizePE := False;  // ! Don't auto-pop context on last markup delimiter
        ExpectChar('>');        //   This enables correct nesting check
      end;
{
  MarkupDecl starting in PE and ending in root is a WFC [28a]
  MarkupDecl starting in root but ending in PE is a VC (erratum 2e-14)
}
      if PELevel > FEntityLevel then
        RaiseExc('Parameter entities must be properly nested')
      else
        AssertPENesting(PELevel);
    end
    else if FCurChar = '?' then
      ParsePI;
  until False;
  FRecognizePE := False;
  if (IncludeLevel > 0) or (IgnoreLevel > 0) then
    RaiseExc('Conditional section not closed');
end;

procedure TXMLReader.DoParseExtSubset(ASource: TXMLInputSource);
begin
  InitializeRoot(ASource);
  FAllowedDecl := dtText;
  ParseMarkupDecl;
  if FCurChar <> #0 then
    RaiseExc('Illegal character in DTD');
end;

procedure TXMLReader.ProcessDTD(ASource: TXMLInputSource);
begin
  doc := TXMLDocument.Create;
  FDocType := TDOMDocumentTypeEx.Create(doc);
  // TODO: DTD labeled version 1.1 will be rejected - must set FXML11 flag
  // TODO: what shall be FCursor? FDocType cannot - it does not accept child nodes
  doc.AppendChild(FDocType);
  DoParseExtSubset(ASource);
end;

procedure TXMLReader.ParseCDSect;               // [18]
var
  name: WideString;
begin
  ExpectString('[CDATA[');
  FValue.Length := 0;
  repeat
    BufAppend(FValue, FCurChar);
    GetCharRaw;
    with FValue do
      if (Length >= 3) and (Buffer[Length-1] = '>') and
      (Buffer[Length-2] = ']') and (Buffer[Length-3] = ']') then
    begin
      Dec(Length, 3);
      SetString(name, Buffer, Length);
      FCursor.AppendChild(doc.CreateCDATASection(name));
      Exit;
    end;
  until FCurChar = #0;
  RaiseExc('Unterminated CDATA section');
end;

procedure TXMLReader.ParseContent;
begin
  repeat
    if FCurChar = '<' then
    begin
      GetCharRaw;
      if CheckName then
        ParseElement
      else if FCurChar = '!' then
      begin
        GetCharRaw;
        FAllowedDecl := dtNone;
        if FCurChar = '[' then
          ParseCDSect
        else if FCurChar = '-' then
          ParseComment
        else
          ParseDoctypeDecl; // actually will raise error
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
  IsEmpty: Boolean;
  attr, OldAttr: TDOMNode;
begin
  NewElem := doc.CreateElementBuf(FName.Buffer, FName.Length);
  FCursor.AppendChild(NewElem);
  Assert(NewElem.ParentNode = FCursor, 'AppendChild did not set ParentNode');
  FCursor := NewElem;

  IsEmpty := False;
  while FCurChar <> '>' do
  begin
    if FCurChar = '/' then
    begin
      GetCharRaw;
      IsEmpty := True;
      FCursor := FCursor.ParentNode;
      Break;
    end;

    // Get Attribute [41]
    ExpectWhitespace;
    if not CheckName then  // allow stuff like <element >, <element />
      Continue;

    attr := doc.CreateAttributeBuf(FName.Buffer, FName.Length);

    // !!cannot use TDOMElement.SetAttributeNode because it will free old attribute
    OldAttr := NewElem.Attributes.SetNamedItem(Attr);
    if Assigned(OldAttr) then
    begin
      OldAttr.Free;
      RaiseExc('Duplicate attribute');
    end;
    ExpectEq;
    Assert(TDOMAttr(attr).OwnerElement = NewElem, 'DOMAttr.OwnerElement not set correctly');
    FCursor := attr;
    ExpectAttValue;
    FCursor := NewElem;
  end;
  ExpectChar('>');
  ProcessDefaultAttributes(NewElem);

  if not IsEmpty then
  begin
   if not FPreserveWhitespace then   // critical for testsuite compliance
      SkipWhitespace;
    ParseContent;
    if FCurChar = '/' then         // Get ETag [42]
    begin
      GetCharRaw;
      if ExpectName <> NewElem.TagName then
        RaiseExc('Unmatching element end tag (expected "</%s>")', [NewElem.TagName]);
      SkipWhitespace;
      ExpectChar('>');
      FCursor := FCursor.ParentNode;
    end
    else if FCurChar <> #0 then
      RaiseNameNotFound
    else // End of stream in content
      RaiseExc('Document element not closed');
  end;
end;

procedure TXMLReader.ProcessDefaultAttributes(Element: TDOMElement);
var
  I: Integer;
  ElDef: TDOMElementDef;
  AttDefs: TDOMNamedNodeMap;
  AttDef: TDOMAttrDef;
  Attr: TDOMAttrEx;
  Spec: Boolean;
begin
  if Assigned(FDocType) then
  begin
    ElDef := TDOMElementDef(FDocType.ElementDefs.GetNamedItem(Element.TagName));
    if Assigned(ElDef) and ElDef.HasAttributes then
    begin
      AttDefs := ElDef.Attributes;
      for I := 0 to AttDefs.Length-1 do
      begin
        AttDef := AttDefs[I] as TDOMAttrDef;
        Spec := True;
        // no validity checking yet; just append default values
        Attr := TDOMAttrEx(Element.GetAttributeNode(AttDef.Name));
        if (AttDef.FDefault in [AD_DEFAULT, AD_FIXED]) and (Attr = nil) then
        begin
          Attr := TDOMAttrEx(AttDef.CloneNode(True));
          Element.SetAttributeNode(Attr);
          Spec := False;
        end;
        if Assigned(Attr) then
        begin
          Attr.FSpecified := Spec;
          Attr.FNormalize := (AttDef.FDataType <> DT_CDATA);
        end;
      end;
    end;
  end;
end;

function TXMLReader.ParsePEReference: Boolean;    // [69]
begin
  Result := CheckForChar('%');
  if Result then
  begin
    SkipName;
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
    SkipSystemLiteral(SysID, True);
    Result := True;
  end
  else if FCurChar = 'P' then
  begin
    ExpectString('PUBLIC');
    ExpectWhitespace;
    SkipPubidLiteral;
    SetString(PubID, FValue.Buffer, FValue.Length);
    if SysIdOptional then
    begin
      SkipWhitespace;
      SkipSystemLiteral(SysID, False);
    end
    else
    begin
      ExpectWhitespace;
      SkipSystemLiteral(SysID, True);
    end;
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ValidationError(const Msg: string;
  const args: array of const);
begin
  // TODO: just a stub now
  FInvalid := True;
end;




procedure ReadXMLFile(out ADoc: TXMLDocument; var f: Text);
var
  Reader: TXMLReader;
  Src: TXMLInputSource;
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
  Src: TXMLInputSource;
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
  Src: TXMLInputSource;
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
  Src: TXMLInputSource;
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
  Src: TXMLInputSource;
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
  Src: TXMLInputSource;
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
