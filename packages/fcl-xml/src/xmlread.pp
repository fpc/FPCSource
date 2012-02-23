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
  SysUtils, Classes, DOM, xmlutils;

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
procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream; const ABaseURI: String); overload;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: Text); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; f: TStream); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; f: TStream; const ABaseURI: String); overload;

procedure ReadDTDFile(out ADoc: TXMLDocument; const AFilename: String);  overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: Text); overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; f: TStream); overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; f: TStream; const ABaseURI: String); overload;

type
  TDOMParseOptions = class(TObject)
  private
    FValidate: Boolean;
    FPreserveWhitespace: Boolean;
    FExpandEntities: Boolean;
    FIgnoreComments: Boolean;
    FCDSectionsAsText: Boolean;
    FResolveExternals: Boolean;
    FNamespaces: Boolean;
    FDisallowDoctype: Boolean;
    FCanonical: Boolean;
    FMaxChars: Cardinal;
    function GetCanonical: Boolean;
    procedure SetCanonical(aValue: Boolean);
  public
    property Validate: Boolean read FValidate write FValidate;
    property PreserveWhitespace: Boolean read FPreserveWhitespace write FPreserveWhitespace;
    property ExpandEntities: Boolean read FExpandEntities write FExpandEntities;
    property IgnoreComments: Boolean read FIgnoreComments write FIgnoreComments;
    property CDSectionsAsText: Boolean read FCDSectionsAsText write FCDSectionsAsText;
    property ResolveExternals: Boolean read FResolveExternals write FResolveExternals;
    property Namespaces: Boolean read FNamespaces write FNamespaces;
    property DisallowDoctype: Boolean read FDisallowDoctype write FDisallowDoctype;
    property MaxChars: Cardinal read FMaxChars write FMaxChars;
    property CanonicalForm: Boolean read GetCanonical write SetCanonical;
  end;

  // NOTE: DOM 3 LS ACTION_TYPE enumeration starts at 1
  TXMLContextAction = (
    xaAppendAsChildren = 1,
    xaReplaceChildren,
    xaInsertBefore,
    xaInsertAfter,
    xaReplace);

  TXMLErrorEvent = procedure(Error: EXMLReadError) of object;

  TXMLInputSource = class(TObject)
  private
    FStream: TStream;
    FStringData: string;
    FBaseURI: XMLString;
    FSystemID: XMLString;
    FPublicID: XMLString;
//    FEncoding: string;
  public
    constructor Create(AStream: TStream); overload;
    constructor Create(const AStringData: string); overload;
    property Stream: TStream read FStream;
    property StringData: string read FStringData;
    property BaseURI: XMLString read FBaseURI write FBaseURI;
    property SystemID: XMLString read FSystemID write FSystemID;
    property PublicID: XMLString read FPublicID write FPublicID;
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
    procedure ParseUri(const URI: XMLString; out ADoc: TXMLDocument);
    function ParseWithContext(Src: TXMLInputSource; Context: TDOMNode;
      Action: TXMLContextAction): TDOMNode;
    property Options: TDOMParseOptions read FOptions;
    property OnError: TXMLErrorEvent read FOnError write FOnError;
  end;

  TDecoder = record
    Context: Pointer;
    Decode: function(Context: Pointer; InBuf: PChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
    Cleanup: procedure(Context: Pointer); stdcall;
  end;

  TGetDecoderProc = function(const AEncoding: string; out Decoder: TDecoder): Boolean; stdcall;

procedure RegisterDecoder(Proc: TGetDecoderProc);

// =======================================================

implementation

uses
  UriParser, dtdmodel;

const
  PubidChars: TSetOfChar = [' ', #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
    '#', '@', '$', '_', '%'];

type
  TDOMDocumentTypeEx = class(TDOMDocumentType);
  TDOMTopNodeEx = class(TDOMNode_TopLevel);

  TXMLSourceKind = (skNone, skInternalSubset, skManualPop);

  TLocation = xmlutils.TLocation;

  TDOMEntityEx = class(TDOMEntity);

  TXMLTextReader = class;

  TXMLCharSource = class(TObject)
  private
    FBuf: PWideChar;
    FBufEnd: PWideChar;
    FReader: TXMLTextReader;
    FParent: TXMLCharSource;
    FEntity: TEntityDecl;
    FLineNo: Integer;
    LFPos: PWideChar;
    FXML11Rules: Boolean;
    FSystemID: XMLString;
    FCharCount: Cardinal;
    FStartNesting: Integer;
    FXMLVersion: TXMLVersion;
    FXMLEncoding: XMLString;
    function GetSystemID: XMLString;
  protected
    function Reload: Boolean; virtual;
  public
    Kind: TXMLSourceKind;
    constructor Create(const AData: XMLString);
    procedure NextChar;
    procedure NewLine; virtual;
    function SkipUntil(var ToFill: TWideCharBuf; const Delim: TSetOfChar;
      wsflag: PBoolean = nil): WideChar; virtual;
    procedure Initialize; virtual;
    function SetEncoding(const AEncoding: string): Boolean; virtual;
    function Matches(const arg: XMLString): Boolean;
    property SystemID: XMLString read GetSystemID write FSystemID;
  end;

  TXMLDecodingSource = class(TXMLCharSource)
  private
    FCharBuf: PChar;
    FCharBufEnd: PChar;
    FBufStart: PWideChar;
    FDecoder: TDecoder;
    FHasBOM: Boolean;
    FFixedUCS2: string;
    FBufSize: Integer;
    procedure DecodingError(const Msg: string);
  protected
    function Reload: Boolean; override;
    procedure FetchData; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function SetEncoding(const AEncoding: string): Boolean; override;
    procedure NewLine; override;
    function SkipUntil(var ToFill: TWideCharBuf; const Delim: TSetOfChar;
      wsflag: PBoolean = nil): WideChar; override;
    procedure Initialize; override;
  end;

  TXMLStreamInputSource = class(TXMLDecodingSource)
  private
    FAllocated: PChar;
    FStream: TStream;
    FCapacity: Integer;
    FOwnStream: Boolean;
    FEof: Boolean;
  public
    constructor Create(AStream: TStream; AOwnStream: Boolean);
    destructor Destroy; override;
    procedure FetchData; override;
  end;

  TXMLFileInputSource = class(TXMLDecodingSource)
  private
    FFile: ^Text;
    FString: string;
    FTmp: string;
  public
    constructor Create(var AFile: Text);
    procedure FetchData; override;
  end;

  PForwardRef = ^TForwardRef;
  TForwardRef = record
    Value: XMLString;
    Loc: TLocation;
  end;

  TElementValidator = object
    FElementDef: TElementDecl;
    FCurCP: TContentParticle;
    FFailed: Boolean;
    FSaViolation: Boolean;
    FContentType: TElementContentType;       // =ctAny when FElementDef is nil
    function IsElementAllowed(Def: TElementDecl): Boolean;
    function Incomplete: Boolean;
  end;

  TNodeDataDynArray = array of TNodeData;
  TValidatorDynArray = array of TElementValidator;

  TXMLReadState = (rsProlog, rsDTD, rsAfterDTD, rsRoot, rsEpilog);

  TCheckNameFlags = set of (cnOptional, cnToken);

  TXMLToken = (xtNone, xtEOF, xtText, xtWhitespace, xtElement, xtEndElement,
    xtCDSect, xtComment, xtPI, xtDoctype, xtEntity, xtEntityEnd, xtPopElement,
    xtPopEmptyElement, xtPushElement, xtPushEntity, xtPopEntity, xtFakeLF);

  TLiteralType = (ltPlain, ltPubid, ltEntity);

  TXMLTextReader = class
  private
    FSource: TXMLCharSource;
    FNameTable: THashTable;
    FCtrl: TDOMParser;
    FXML11: Boolean;
    FState: TXMLReadState;
    FHavePERefs: Boolean;
    FInsideDecl: Boolean;
    FValue: TWideCharBuf;
    FEntityValue: TWideCharBuf;
    FName: TWideCharBuf;
    FTokenStart: TLocation;
    FStandalone: Boolean;
    FNamePages: PByteArray;
    FDocType: TDTDModel;
    FPEMap: THashTable;
    FForwardRefs: TFPList;
    FDTDStartPos: PWideChar;
    FIntSubset: TWideCharBuf;
    FAttrTag: Cardinal;
    FDTDProcessed: Boolean;
    FFragmentMode: Boolean;
    FNext: TXMLToken;
    FCurrEntity: TEntityDecl;
    FIDMap: THashTable;

    FNSHelper: TNSSupport;
    FNsAttHash: TDblHashArray;
    FStdPrefix_xml: PHashItem;
    FStdPrefix_xmlns: PHashItem;
    FStdUri_xml: PHashItem;
    FStdUri_xmlns: PHashItem;

    FColonPos: Integer;
    FValidate: Boolean;            // parsing options, copy of FCtrl.Options
    FPreserveWhitespace: Boolean;
    FExpandEntities: Boolean;
    FIgnoreComments: Boolean;
    FCDSectionsAsText: Boolean;
    FResolveExternals: Boolean;
    FNamespaces: Boolean;
    FDisallowDoctype: Boolean;
    FCanonical: Boolean;
    FMaxChars: Cardinal;

    procedure SetEOFState;
    procedure SkipQuote(out Delim: WideChar; required: Boolean = True);
    procedure Initialize(ASource: TXMLCharSource);
    procedure NSPrepare;
    procedure EntityToSource(AEntity: TEntityDecl; out Src: TXMLCharSource);
    function ContextPush(AEntity: TEntityDecl): Boolean;
    function ContextPop(Forced: Boolean = False): Boolean;
    procedure XML11_BuildTables;
    function ParseQuantity: TCPQuant;
    procedure StoreLocation(out Loc: TLocation);
    function ValidateAttrSyntax(AttrDef: TAttributeDef; const aValue: XMLString): Boolean;
    procedure ValidateAttrValue(AttrDef: TAttributeDef; attrData: PNodeData);
    procedure AddForwardRef(Buf: PWideChar; Length: Integer);
    procedure ClearForwardRefs;
    procedure ValidateIdRefs;
    procedure StandaloneError(LineOffs: Integer = 0);
    procedure CallErrorHandler(E: EXMLReadError);
    function  FindOrCreateElDef: TElementDecl;
    function  SkipUntilSeq(const Delim: TSetOfChar; c1: WideChar): Boolean;
    procedure CheckMaxChars(ToAdd: Cardinal);
    function AllocNodeData(AIndex: Integer): PNodeData;
    function AllocAttributeData: PNodeData;
    procedure AllocAttributeValueChunk(var APrev: PNodeData; Offset: Integer);
    procedure CleanupAttribute(aNode: PNodeData);
    procedure CleanupAttributes;
    procedure SetNodeInfoWithValue(typ: TXMLNodeType; AName: PHashItem = nil);
    function SetupFakeLF(nextstate: TXMLToken): Boolean;
    function AddId(aNodeData: PNodeData): Boolean;
  protected
    FNesting: Integer;
    FCurrNode: PNodeData;
    FAttrCount: Integer;
    FPrefixedAttrs: Integer;
    FSpecifiedAttrs: Integer;
    FNodeStack: TNodeDataDynArray;
    FValidatorNesting: Integer;
    FValidators: TValidatorDynArray;
    FAttrChunks: TFPList;
    FFreeAttrChunk: PNodeData;
    FAttrCleanupFlag: Boolean;

    procedure DoError(Severity: TErrorSeverity; const descr: string; LineOffs: Integer=0);
    procedure DoErrorPos(Severity: TErrorSeverity; const descr: string;
      const ErrPos: TLocation); overload;
    procedure DoErrorPos(Severity: TErrorSeverity; const descr: string;
      const args: array of const; const ErrPos: TLocation); overload;
    procedure FatalError(const descr: String; LineOffs: Integer=0); overload;
    procedure FatalError(const descr: string; const args: array of const; LineOffs: Integer=0); overload;
    procedure FatalError(Expected: WideChar); overload;
    function  SkipWhitespace(PercentAloneIsOk: Boolean = False): Boolean;
    function  SkipS(required: Boolean = False): Boolean;
    procedure ExpectWhitespace;
    procedure ExpectString(const s: String);
    procedure ExpectChar(wc: WideChar);
    function  CheckForChar(c: WideChar): Boolean;

    procedure RaiseNameNotFound;
    function  CheckName(aFlags: TCheckNameFlags = []): Boolean;
    procedure CheckNCName;
    function  ExpectName: XMLString;                                    // [5]
    function ParseLiteral(var ToFill: TWideCharBuf; aType: TLiteralType;
      Required: Boolean): Boolean;
    procedure ExpectAttValue(attrData: PNodeData; NonCDATA: Boolean);   // [10]
    procedure ParseComment(discard: Boolean);                           // [15]
    procedure ParsePI;                                                  // [16]
    function CreatePINode: TDOMNode;
    procedure ParseXmlOrTextDecl(TextDecl: Boolean);
    procedure ExpectEq;
    procedure ParseDoctypeDecl;                                         // [28]
    procedure ParseMarkupDecl;                                          // [29]
    procedure ParseIgnoreSection;
    procedure ParseStartTag;                                            // [39]
    procedure ParseEndTag;                                              // [42]
    function DoStartElement: TDOMElement;
    procedure HandleEntityStart;
    procedure HandleEntityEnd;
    procedure ResolveEntity;
    procedure DoStartEntity;
    procedure ParseAttribute(ElDef: TElementDecl);
    procedure ParseContent(cursor: TDOMNode_WithChildren);              // [43]
    function  ReadTopLevel: Boolean;
    function  Read: Boolean;
    function  ResolvePredefined: Boolean;
    function  EntityCheck(NoExternals: Boolean = False): TEntityDecl;
    procedure LoadEntity(AEntity: TEntityDecl);
    function PrefetchEntity(AEntity: TEntityDecl): Boolean;
    procedure StartPE;
    function  ParseRef(var ToFill: TWideCharBuf): Boolean;              // [67]
    function  ParseExternalID(out SysID, PubID: XMLString;              // [75]
      SysIdOptional: Boolean): Boolean;

    procedure BadPENesting(S: TErrorSeverity = esError);
    procedure ParseEntityDecl;
    procedure ParseAttlistDecl;
    procedure ExpectChoiceOrSeq(CP: TContentParticle; MustEndIn: TObject);
    procedure ParseElementDecl;
    procedure ParseNotationDecl;
    function ResolveResource(const ASystemID, APublicID, ABaseURI: XMLString; out Source: TXMLCharSource): Boolean;
    procedure ProcessDefaultAttributes(ElDef: TElementDecl);
    procedure ProcessNamespaceAtts;
    function AddBinding(attrData: PNodeData): Boolean;

    procedure PushVC(aElDef: TElementDecl);
    procedure PopElement;
    procedure ValidateDTD;
    procedure ValidateCurrentNode;
    procedure ValidationError(const Msg: string; const args: array of const; LineOffs: Integer = -1);
    procedure ValidationErrorWithName(const Msg: string; LineOffs: Integer = -1);
    procedure DTDReloadHook;
    procedure ConvertSource(SrcIn: TXMLInputSource; out SrcOut: TXMLCharSource);
    function DoCDSect(ch: PWideChar; Count: Integer): TDOMNode;
    procedure DoNotationDecl(const aName, aPubID, aSysID: XMLString);
  public
    doc: TDOMDocument;
    constructor Create; overload;
    constructor Create(AParser: TDOMParser); overload;
    destructor Destroy; override;
    procedure ProcessXML(ASource: TXMLCharSource);                // [1]
    procedure ProcessFragment(ASource: TXMLCharSource; AOwner: TDOMNode);
    procedure ProcessDTD(ASource: TXMLCharSource);               // ([29])
  end;

const
  NullLocation: TLocation = (Line: 0; LinePos: 0);

{ Decoders }

var
  Decoders: array of TGetDecoderProc;

procedure RegisterDecoder(Proc: TGetDecoderProc);
var
  L: Integer;
begin
  L := Length(Decoders);
  SetLength(Decoders, L+1);
  Decoders[L] := Proc;
end;

function FindDecoder(const AEncoding: string; out Decoder: TDecoder): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(Decoders) do
    if Decoders[I](AEncoding, Decoder) then
    begin
      Result := True;
      Exit;
    end;
end;


function Is_8859_1(const AEncoding: string): Boolean;
begin
  Result := SameText(AEncoding, 'ISO-8859-1') or
            SameText(AEncoding, 'ISO_8859-1') or
            SameText(AEncoding, 'latin1') or
            SameText(AEncoding, 'iso-ir-100') or
            SameText(AEncoding, 'l1') or
            SameText(AEncoding, 'IBM819') or
            SameText(AEncoding, 'CP819') or
            SameText(AEncoding, 'csISOLatin1') or
// This one is not in character-sets.txt, but was used in FPC documentation,
// and still being used in fcl-registry package
            SameText(AEncoding, 'ISO8859-1');
end;


{ TDOMParseOptions }

function TDOMParseOptions.GetCanonical: Boolean;
begin
  Result := FCanonical and FExpandEntities and FCDSectionsAsText and
  { (not normalizeCharacters) and } FNamespaces and
  { namespaceDeclarations and } FPreserveWhitespace;
end;

procedure TDOMParseOptions.SetCanonical(aValue: Boolean);
begin
  FCanonical := aValue;
  if aValue then
  begin
    FExpandEntities := True;
    FCDSectionsAsText := True;
    FNamespaces := True;
    FPreserveWhitespace := True;
    { normalizeCharacters := False; }
    { namespaceDeclarations := True; }
    { wellFormed := True; }
  end;
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
  with TXMLTextReader.Create(Self) do
  try
    ConvertSource(Src, InputSrc);  // handles 'no-input-specified' case
    ProcessXML(InputSrc)
  finally
    ADoc := TXMLDocument(doc);
    Free;
  end;
end;

procedure TDOMParser.ParseUri(const URI: XMLString; out ADoc: TXMLDocument);
var
  Src: TXMLCharSource;
begin
  ADoc := nil;
  with TXMLTextReader.Create(Self) do
  try
    if ResolveResource(URI, '', '', Src) then
      ProcessXML(Src)
    else
      DoErrorPos(esFatal, 'The specified URI could not be resolved', NullLocation);
  finally
    ADoc := TXMLDocument(doc);
    Free;
  end;
end;

function TDOMParser.ParseWithContext(Src: TXMLInputSource;
  Context: TDOMNode; Action: TXMLContextAction): TDOMNode;
var
  InputSrc: TXMLCharSource;
  Frag: TDOMDocumentFragment;
  node: TDOMNode;
begin
  if Action in [xaInsertBefore, xaInsertAfter, xaReplace] then
    node := Context.ParentNode
  else
    node := Context;
  // TODO: replacing document isn't yet supported
  if (Action = xaReplaceChildren) and (node.NodeType = DOCUMENT_NODE) then
    raise EDOMNotSupported.Create('DOMParser.ParseWithContext');

  if not (node.NodeType in [ELEMENT_NODE, DOCUMENT_FRAGMENT_NODE]) then
    raise EDOMHierarchyRequest.Create('DOMParser.ParseWithContext');

  with TXMLTextReader.Create(Self) do
  try
    ConvertSource(Src, InputSrc);    // handles 'no-input-specified' case
    Frag := Context.OwnerDocument.CreateDocumentFragment;
    try
      ProcessFragment(InputSrc, Frag);
      Result := Frag.FirstChild;
      case Action of
        xaAppendAsChildren: Context.AppendChild(Frag);

        xaReplaceChildren: begin
          Context.TextContent := '';     // removes children
          Context.ReplaceChild(Frag, Context.FirstChild);
        end;
        xaInsertBefore: node.InsertBefore(Frag, Context);
        xaInsertAfter:  node.InsertBefore(Frag, Context.NextSibling);
        xaReplace:      node.ReplaceChild(Frag, Context);
      end;
    finally
      Frag.Free;
    end;
  finally
    Free;
  end;
end;

{ TXMLCharSource }

constructor TXMLCharSource.Create(const AData: XMLString);
begin
  inherited Create;
  FLineNo := 1;
  FBuf := PWideChar(AData);
  FBufEnd := FBuf + Length(AData);
  LFPos := FBuf-1;
  FCharCount := Length(AData);
end;

procedure TXMLCharSource.Initialize;
begin
end;

function TXMLCharSource.SetEncoding(const AEncoding: string): Boolean;
begin
  Result := True; // always succeed
end;

function TXMLCharSource.GetSystemID: XMLString;
begin
  if FSystemID <> '' then
    Result := FSystemID
  else if Assigned(FParent) then
    Result := FParent.SystemID
  else
    Result := '';
end;

function TXMLCharSource.Reload: Boolean;
begin
  Result := False;
end;

procedure TXMLCharSource.NewLine;
begin
  Inc(FLineNo);
  LFPos := FBuf;
end;

function TXMLCharSource.SkipUntil(var ToFill: TWideCharBuf; const Delim: TSetOfChar;
  wsflag: PBoolean): WideChar;
var
  old: PWideChar;
  nonws: Boolean;
begin
  old := FBuf;
  nonws := False;
  repeat
    if FBuf^ = #10 then
      NewLine;
    if (FBuf^ < #255) and (Char(ord(FBuf^)) in Delim) then
      Break;
    if (FBuf^ > #32) or not (Char(ord(FBuf^)) in [#32, #9, #10, #13]) then
      nonws := True;
    Inc(FBuf);
  until False;
  Result := FBuf^;
  BufAppendChunk(ToFill, old, FBuf);
  if Assigned(wsflag) then
    wsflag^ := wsflag^ or nonws;
end;

function TXMLCharSource.Matches(const arg: XMLString): Boolean;
begin
  Result := False;
  if (FBufEnd >= FBuf + Length(arg)) or Reload then
    Result := CompareMem(Pointer(arg), FBuf, Length(arg)*sizeof(WideChar));
  if Result then
  begin
    Inc(FBuf, Length(arg));
    if FBuf >= FBufEnd then
      Reload;
  end;
end;

{ TXMLDecodingSource }

procedure TXMLDecodingSource.AfterConstruction;
begin
  inherited AfterConstruction;
  FBufStart := AllocMem(4096);
  FBuf := FBufStart;
  FBufEnd := FBuf;
  LFPos := FBuf-1;
end;

destructor TXMLDecodingSource.Destroy;
begin
  FreeMem(FBufStart);
  if Assigned(FDecoder.Cleanup) then
    FDecoder.Cleanup(FDecoder.Context);
  inherited Destroy;
end;

procedure TXMLDecodingSource.FetchData;
begin
end;

procedure TXMLDecodingSource.DecodingError(const Msg: string);
begin
// count line endings to obtain correct error location
  while FBuf < FBufEnd do
  begin
    if (FBuf^ = #10) or (FBuf^ = #13) or (FXML11Rules and ((FBuf^ = #$85) or (FBuf^ = #$2028))) then
    begin
      if (FBuf^ = #13) and (FBuf < FBufEnd-1) and
      ((FBuf[1] = #10) or (FXML11Rules and (FBuf[1] = #$85))) then
        Inc(FBuf);
      LFPos := FBuf;
      Inc(FLineNo);
    end;
    Inc(FBuf);
  end;
  FReader.FatalError(Msg);
end;

function TXMLDecodingSource.Reload: Boolean;
var
  Remainder: PtrInt;
  r, inLeft: Cardinal;
  rslt: Integer;
begin
  if Kind = skInternalSubset then
    FReader.DTDReloadHook;
  Remainder := FBufEnd - FBuf;
  if Remainder > 0 then
    Move(FBuf^, FBufStart^, Remainder * sizeof(WideChar));
  Dec(LFPos, FBuf-FBufStart);
  FBuf := FBufStart;
  FBufEnd := FBufStart + Remainder;

  repeat
    inLeft := FCharBufEnd - FCharBuf;
    if inLeft < 4 then                      // may contain an incomplete char
    begin
      FetchData;
      inLeft := FCharBufEnd - FCharBuf;
      if inLeft <= 0 then
        Break;
    end;
    r := FBufStart + FBufSize - FBufEnd;
    if r = 0 then
      Break;
    rslt := FDecoder.Decode(FDecoder.Context, FCharBuf, inLeft, FBufEnd, r);
    { Sanity checks: r and inLeft must not increase. }
    if inLeft + FCharBuf <= FCharBufEnd then
      FCharBuf := FCharBufEnd - inLeft
    else
      DecodingError('Decoder error: input byte count out of bounds');
    if r + FBufEnd <= FBufStart + FBufSize then
      FBufEnd := FBufStart + FBufSize - r
    else
      DecodingError('Decoder error: output char count out of bounds');

    if rslt = 0 then
      Break
    else if rslt < 0 then
      DecodingError('Invalid character in input stream')
    else
      FReader.CheckMaxChars(rslt);
  until False;

  FBufEnd^ := #0;
  Result := FBuf < FBufEnd;
end;

const
  XmlSign: array [0..4] of WideChar = ('<', '?', 'x', 'm', 'l');

procedure TXMLDecodingSource.Initialize;
begin
  inherited;
  FLineNo := 1;
  FDecoder.Decode := @Decode_UTF8;

  FFixedUCS2 := '';
  if FCharBufEnd-FCharBuf > 1 then
  begin
    if (FCharBuf[0] = #$FE) and (FCharBuf[1] = #$FF) then
    begin
      FFixedUCS2 := 'UTF-16BE';
      FDecoder.Decode := {$IFNDEF ENDIAN_BIG} @Decode_UCS2_Swapped {$ELSE} @Decode_UCS2 {$ENDIF};
    end
    else if (FCharBuf[0] = #$FF) and (FCharBuf[1] = #$FE) then
    begin
      FFixedUCS2 := 'UTF-16LE';
      FDecoder.Decode := {$IFDEF ENDIAN_BIG} @Decode_UCS2_Swapped {$ELSE} @Decode_UCS2 {$ENDIF};
    end;
  end;
  FBufSize := 6;             //  possible BOM and '<?xml'
  Reload;
  if FBuf^ = #$FEFF then
  begin
    FHasBOM := True;
    Inc(FBuf);
  end;
  LFPos := FBuf-1;
  if CompareMem(FBuf, @XmlSign[0], sizeof(XmlSign)) then
  begin
    FBufSize := 3;           // don't decode past XML declaration
    Inc(FBuf, Length(XmlSign));
    FReader.ParseXmlOrTextDecl((FParent <> nil) or (FReader.FState <> rsProlog));
  end;
  FBufSize := 2047;
  if FReader.FXML11 then
    FReader.XML11_BuildTables;
end;

function TXMLDecodingSource.SetEncoding(const AEncoding: string): Boolean;
var
  NewDecoder: TDecoder;
begin
  Result := True;
  if (FFixedUCS2 = '') and SameText(AEncoding, 'UTF-8') then
    Exit;
  if FFixedUCS2 <> '' then
  begin
    Result := SameText(AEncoding, FFixedUCS2) or
       SameText(AEncoding, 'UTF-16') or
       SameText(AEncoding, 'unicode');
    Exit;
  end;
// TODO: must fail when a byte-based stream is labeled as word-based.
// see rmt-e2e-61, it now fails but for a completely different reason.
  FillChar(NewDecoder, sizeof(TDecoder), 0);
  if Is_8859_1(AEncoding) then
    FDecoder.Decode := @Decode_8859_1
  else if FindDecoder(AEncoding, NewDecoder) then
    FDecoder := NewDecoder
  else
    Result := False;
end;

procedure TXMLDecodingSource.NewLine;
begin
  case FBuf^ of
    #10: ;
    #13: begin
      // Reload trashes the buffer, it should be consumed beforehand
      if (FBufEnd >= FBuf+2) or Reload then
      begin
        if (FBuf[1] = #10) or (FXML11Rules and (FBuf[1] = #$85)) then
          Inc(FBuf);
      end;
      FBuf^ := #10;
    end;
    #$85, #$2028: if FXML11Rules then
      FBuf^ := #10
    else
      Exit;
  else
    Exit;
  end;
  Inc(FLineNo);
  LFPos := FBuf;
end;

{ TXMLStreamInputSource }

const
  Slack = 16;

constructor TXMLStreamInputSource.Create(AStream: TStream; AOwnStream: Boolean);
begin
  FStream := AStream;
  FCapacity := 4096;
  GetMem(FAllocated, FCapacity+Slack);
  FCharBuf := FAllocated+(Slack-4);
  FCharBufEnd := FCharBuf;
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
  Assert(FCharBufEnd - FCharBuf < Slack-4);
  if FEof then
    Exit;
  OldBuf := FCharBuf;
  Remainder := FCharBufEnd - FCharBuf;
  if Remainder < 0 then
    Remainder := 0;
  FCharBuf := FAllocated+Slack-4-Remainder;
  if Remainder > 0 then
    Move(OldBuf^, FCharBuf^, Remainder);
  BytesRead := FStream.Read(FAllocated[Slack-4], FCapacity);
  if BytesRead < FCapacity then
    FEof := True;
  FCharBufEnd := FAllocated + (Slack-4) + BytesRead;
  { Null-termination has been removed:
    1) Built-in decoders don't need it because they respect the buffer length.
    2) It was causing unaligned access errors on ARM CPUs.
  }
  //PWideChar(FCharBufEnd)^ := #0;
end;

{ TXMLFileInputSource }

constructor TXMLFileInputSource.Create(var AFile: Text);
begin
  FFile := @AFile;
  SystemID := FilenameToURI(TTextRec(AFile).Name);
  FetchData;
end;

procedure TXMLFileInputSource.FetchData;
var
  Remainder: Integer;
begin
  if not Eof(FFile^) then
  begin
    Remainder := FCharBufEnd - FCharBuf;
    if Remainder > 0 then
      SetString(FTmp, FCharBuf, Remainder);
    ReadLn(FFile^, FString);
    FString := FString + #10;    // bad solution...
    if Remainder > 0 then
      Insert(FTmp, FString, 1);
    FCharBuf := PChar(FString);
    FCharBufEnd := FCharBuf + Length(FString);
  end;
end;

{ helper that closes handle upon destruction }
type
  THandleOwnerStream = class(THandleStream)
  public
    destructor Destroy; override;
  end;

destructor THandleOwnerStream.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;

{ TXMLTextReader }

procedure TXMLTextReader.ConvertSource(SrcIn: TXMLInputSource; out SrcOut: TXMLCharSource);
begin
  SrcOut := nil;
  if Assigned(SrcIn) then
  begin
    if Assigned(SrcIn.FStream) then
      SrcOut := TXMLStreamInputSource.Create(SrcIn.FStream, False)
    else if SrcIn.FStringData <> '' then
      SrcOut := TXMLStreamInputSource.Create(TStringStream.Create(SrcIn.FStringData), True)
    else if (SrcIn.SystemID <> '') then
      ResolveResource(SrcIn.SystemID, SrcIn.PublicID, SrcIn.BaseURI, SrcOut);
  end;
  if (SrcOut = nil) and (FSource = nil) then
    DoErrorPos(esFatal, 'No input source specified', NullLocation);
end;

procedure TXMLTextReader.StoreLocation(out Loc: TLocation);
begin
  Loc.Line := FSource.FLineNo;
  Loc.LinePos := FSource.FBuf-FSource.LFPos;
end;

function TXMLTextReader.ResolveResource(const ASystemID, APublicID, ABaseURI: XMLString; out Source: TXMLCharSource): Boolean;
var
  AbsSysID: XMLString;
  Filename: string;
  Stream: TStream;
  fd: THandle;
begin
  Source := nil;
  Result := False;
  if not ResolveRelativeURI(ABaseURI, ASystemID, AbsSysID) then
    Exit;
  { TODO: alternative resolvers
    These may be 'internal' resolvers or a handler set by application.
    Internal resolvers should probably produce a TStream
    ( so that internal classes need not be exported ).
    External resolver will produce TXMLInputSource that should be converted.
    External resolver must NOT be called for root entity.
    External resolver can return nil, in which case we do the default }
  if URIToFilename(AbsSysID, Filename) then
  begin
    fd := FileOpen(Filename, fmOpenRead + fmShareDenyWrite);
    if fd <> THandle(-1) then
    begin
      Stream := THandleOwnerStream.Create(fd);
      Source := TXMLStreamInputSource.Create(Stream, True);
      Source.SystemID := AbsSysID;    // <- Revisit: Really need absolute sysID?
    end;
  end;
  Result := Assigned(Source);
end;

procedure TXMLTextReader.Initialize(ASource: TXMLCharSource);
begin
  ASource.FParent := FSource;
  FSource := ASource;
  FSource.FReader := Self;
  FSource.FStartNesting := FNesting;
  FSource.Initialize;
end;

procedure TXMLTextReader.FatalError(Expected: WideChar);
begin
// FIX: don't output what is found - anything may be found, including exploits...
  FatalError('Expected "%1s"', [string(Expected)]);
end;

procedure TXMLTextReader.FatalError(const descr: String; LineOffs: Integer);
begin
  DoError(esFatal, descr, LineOffs);
end;

procedure TXMLTextReader.FatalError(const descr: string; const args: array of const; LineOffs: Integer);
begin
  DoError(esFatal, Format(descr, args), LineOffs);
end;

procedure TXMLTextReader.ValidationError(const Msg: string; const Args: array of const; LineOffs: Integer);
begin
  if FValidate then
    DoError(esError, Format(Msg, Args), LineOffs);
end;

procedure TXMLTextReader.ValidationErrorWithName(const Msg: string; LineOffs: Integer);
var
  ws: XMLString;
begin
  SetString(ws, FName.Buffer, FName.Length);
  ValidationError(Msg, [ws], LineOffs);
end;

procedure TXMLTextReader.DoError(Severity: TErrorSeverity; const descr: string; LineOffs: Integer);
var
  Loc: TLocation;
begin
  StoreLocation(Loc);
  if LineOffs >= 0 then
  begin
    Dec(Loc.LinePos, LineOffs);
    DoErrorPos(Severity, descr, Loc);
  end
  else
    DoErrorPos(Severity, descr, FTokenStart);
end;

procedure TXMLTextReader.DoErrorPos(Severity: TErrorSeverity; const descr: string;
  const args: array of const; const ErrPos: TLocation);
begin
  DoErrorPos(Severity, Format(descr, args), ErrPos);
end;

procedure TXMLTextReader.DoErrorPos(Severity: TErrorSeverity; const descr: string; const ErrPos: TLocation);
var
  E: EXMLReadError;
  sysid: XMLString;
begin
  if Assigned(FSource) then
  begin
    sysid := FSource.FSystemID;
    if (sysid = '') and Assigned(FSource.FEntity) then
      sysid := FSource.FEntity.FURI;
    E := EXMLReadError.CreateFmt('In ''%s'' (line %d pos %d): %s', [sysid, ErrPos.Line, ErrPos.LinePos, descr]);
  end
  else
    E := EXMLReadError.Create(descr);
  E.FSeverity := Severity;
  E.FErrorMessage := descr;
  E.FLine := ErrPos.Line;
  E.FLinePos := ErrPos.LinePos;
  CallErrorHandler(E);
  // No 'finally'! If user handler raises exception, control should not get here
  // and the exception will be freed in CallErrorHandler (below)
  E.Free;
end;

procedure TXMLTextReader.CheckMaxChars(ToAdd: Cardinal);
var
  src: TXMLCharSource;
  total: Cardinal;
begin
  Inc(FSource.FCharCount, ToAdd);
  if FMaxChars = 0 then
    Exit;
  src := FSource;
  total := 0;
  repeat
    Inc(total, src.FCharCount);
    if total > FMaxChars then
      FatalError('Exceeded character count limit');
    src := src.FParent;
  until src = nil;
end;

procedure TXMLTextReader.CallErrorHandler(E: EXMLReadError);
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

function TXMLTextReader.SkipWhitespace(PercentAloneIsOk: Boolean): Boolean;
begin
  Result := False;
  repeat
    Result := SkipS or Result;
    if FSource.FBuf^ = #0 then
    begin
      Result := True;      // report whitespace upon exiting the PE
      if not ContextPop then
        Break;
    end
    else if FSource.FBuf^ = '%' then
    begin
      if (FState <> rsDTD) or ((FSource.Kind = skInternalSubset) and FInsideDecl) then
        Break;
// This is the only case where look-ahead is needed
      if FSource.FBuf > FSource.FBufEnd-2 then
        FSource.Reload;

      if (not PercentAloneIsOk) or (Byte(FSource.FBuf[1]) in NamingBitmap[FNamePages^[$100+hi(Word(FSource.FBuf[1]))]]) or
        (FXML11 and (FSource.FBuf[1] >= #$D800) and (FSource.FBuf[1] <= #$DB7F)) then
      begin
        Inc(FSource.FBuf);    // skip '%'
        CheckName;
        ExpectChar(';');
        StartPE;
        Result := True;        // report whitespace upon entering the PE
      end
      else Break;
    end
    else
      Break;
  until False;
end;

procedure TXMLTextReader.ExpectWhitespace;
begin
  if not SkipWhitespace then
    FatalError('Expected whitespace');
end;

function TXMLTextReader.SkipS(Required: Boolean): Boolean;
var
  p: PWideChar;
begin
  Result := False;
  repeat
    p := FSource.FBuf;
    repeat
      if (p^ = #10) or (p^ = #13) or (FXML11 and ((p^ = #$85) or (p^ = #$2028))) then
      begin
        FSource.FBuf := p;
        FSource.NewLine;
        p := FSource.FBuf;
      end
      else if (p^ <> #32) and (p^ <> #9) then
        Break;
      Inc(p);
      Result := True;
    until False;
    FSource.FBuf := p;
  until (FSource.FBuf < FSource.FBufEnd) or (not FSource.Reload);
  if (not Result) and Required then
    FatalError('Expected whitespace');
end;

procedure TXMLTextReader.ExpectString(const s: String);
var
  I: Integer;
begin
  for I := 1 to Length(s) do
  begin
    if FSource.FBuf^ <> WideChar(ord(s[i])) then
      FatalError('Expected "%s"', [s], i-1);
    FSource.NextChar;
  end;
end;

function TXMLTextReader.CheckForChar(c: WideChar): Boolean;
begin
  Result := (FSource.FBuf^ = c);
  if Result then
  begin
    Inc(FSource.FBuf);
    if FSource.FBuf >= FSource.FBufEnd then
      FSource.Reload;
  end;  
end;

procedure TXMLTextReader.SkipQuote(out Delim: WideChar; required: Boolean);
begin
  Delim := #0;
  if (FSource.FBuf^ = '''') or (FSource.FBuf^ = '"') then
  begin
    Delim := FSource.FBuf^;
    FSource.NextChar;  // skip quote
    StoreLocation(FTokenStart);
  end
  else if required then
    FatalError('Expected single or double quote');
end;

const
  PrefixDefault: array[0..4] of WideChar = ('x','m','l','n','s');

constructor TXMLTextReader.Create;
begin
  inherited Create;
  BufAllocate(FName, 128);
  BufAllocate(FValue, 512);
  FForwardRefs := TFPList.Create;
  FAttrChunks := TFPList.Create;

  // Set char rules to XML 1.0
  FNamePages := @NamePages;
  SetLength(FNodeStack, 16);
  SetLength(FValidators, 16);
end;

constructor TXMLTextReader.Create(AParser: TDOMParser);
begin
  Create;
  FCtrl := AParser;
  if FCtrl = nil then
    Exit;
  FValidate := FCtrl.Options.Validate;
  FPreserveWhitespace := FCtrl.Options.PreserveWhitespace;
  FExpandEntities := FCtrl.Options.ExpandEntities;
  FCDSectionsAsText := FCtrl.Options.CDSectionsAsText;
  FIgnoreComments := FCtrl.Options.IgnoreComments;
  FResolveExternals := FCtrl.Options.ResolveExternals;
  FNamespaces := FCtrl.Options.Namespaces;
  FDisallowDoctype := FCtrl.Options.DisallowDoctype;
  FCanonical := FCtrl.Options.CanonicalForm;
  FMaxChars := FCtrl.Options.MaxChars;
end;

destructor TXMLTextReader.Destroy;
var
  i: Integer;
begin
  for i := FAttrChunks.Count-1 downto 0 do
    Dispose(PNodeData(FAttrChunks.List^[i]));
  if Assigned(FEntityValue.Buffer) then
    FreeMem(FEntityValue.Buffer);
  FreeMem(FName.Buffer);
  FreeMem(FValue.Buffer);
  if Assigned(FSource) then
    while ContextPop(True) do;     // clean input stack
  FSource.Free;
  FPEMap.Free;
  ClearForwardRefs;
  FNsAttHash.Free;
  FNSHelper.Free;
  FDocType.Release;
  FIDMap.Free;
  FForwardRefs.Free;
  FAttrChunks.Free;
  if doc = nil then
    FNameTable.Free;
  inherited Destroy;
end;

procedure TXMLTextReader.XML11_BuildTables;
begin
  FNamePages := Xml11NamePages;
  FXML11 := True;
  FSource.FXml11Rules := True;
end;

{ Must be executed after doc has been set.
  After introducing own NameTable, merge this into constructor }
procedure TXMLTextReader.NSPrepare;
begin
  if FNamespaces then
  begin
    FNSHelper := TNSSupport.Create;
    FNsAttHash := TDblHashArray.Create;
    FStdPrefix_xml := FNSHelper.GetPrefix(@PrefixDefault, 3);
    FStdPrefix_xmlns := FNSHelper.GetPrefix(@PrefixDefault, 5);

    FStdUri_xmlns := FNameTable.FindOrAdd(stduri_xmlns);
    FStdUri_xml := FNameTable.FindOrAdd(stduri_xml);
  end;
end;

procedure TXMLTextReader.ProcessXML(ASource: TXMLCharSource);
begin
  doc := TXMLDocument.Create;
  doc.documentURI := ASource.SystemID;  // TODO: to be changed to URI or BaseURI
  FNameTable := doc.Names;
  FState := rsProlog;
  FNesting := 0;
  FValidatorNesting := 0;
  FCurrNode := @FNodeStack[0];
  FFragmentMode := False;
  NSPrepare;
  Initialize(ASource);
  if FSource.FXMLVersion <> xmlVersionUnknown then
    TDOMTopNodeEx(TDOMNode(doc)).FXMLVersion := FSource.FXMLVersion;
  TDOMTopNodeEx(TDOMNode(doc)).FXMLEncoding := FSource.FXMLEncoding;
  doc.XMLStandalone := FStandalone;
  FNext := xtText;
  ParseContent(doc);

  if FValidate then
    ValidateIdRefs;

  doc.IDs := FIDMap;
  FIDMap := nil;
end;

procedure TXMLTextReader.ProcessFragment(ASource: TXMLCharSource; AOwner: TDOMNode);
var
  DoctypeNode: TDOMDocumentTypeEx;
begin
  doc := AOwner.OwnerDocument;
  FNameTable := doc.Names;
  FState := rsRoot;
  FNesting := 0;
  FValidatorNesting := 0;
  FCurrNode := @FNodeStack[0];
  FFragmentMode := True;
  FXML11 := doc.XMLVersion = '1.1';
  NSPrepare;
  Initialize(ASource);
  { Get doctype from the owner's document, but only if it is not already assigned
   (It is set directly when parsing children of an Entity, see LoadEntity procedure) }
  if FDocType = nil then
  begin
    DoctypeNode := TDOMDocumentTypeEx(doc.DocType);
    if Assigned(DoctypeNode) then
      FDocType := DocTypeNode.FModel.Reference;
  end;
  if AOwner is TDOMEntity then
  begin
    TDOMTopNodeEx(AOwner).FXMLVersion := FSource.FXMLVersion;
    TDOMTopNodeEx(AOwner).FXMLEncoding := FSource.FXMLEncoding;
  end;
  FNext := xtText;
  ParseContent(aOwner as TDOMNode_WithChildren);
end;

function TXMLTextReader.CheckName(aFlags: TCheckNameFlags): Boolean;
var
  p: PWideChar;
  NameStartFlag: Boolean;
begin
  p := FSource.FBuf;
  FName.Length := 0;
  FColonPos := -1;
  NameStartFlag := not (cnToken in aFlags);

  repeat
    if NameStartFlag then
    begin
      if (Byte(p^) in NamingBitmap[FNamePages^[hi(Word(p^))]]) or
        ((p^ = ':') and (not FNamespaces)) then
        Inc(p)
      else if FXML11 and ((p^ >= #$D800) and (p^ <= #$DB7F) and
        (p[1] >= #$DC00) and (p[1] <= #$DFFF)) then
        Inc(p, 2)
      else
      begin
  // here we come either when first char of name is bad (it may be a colon),
  // or when a colon is not followed by a valid NameStartChar
        FSource.FBuf := p;
        Result := False;
        Break;
      end;
      NameStartFlag := False;
    end;

    if FXML11 then
    repeat
      if Byte(p^) in NamingBitmap[FNamePages^[$100+hi(Word(p^))]] then
        Inc(p)
      else if ((p^ >= #$D800) and (p^ <= #$DB7F) and
        (p[1] >= #$DC00) and (p[1] <= #$DFFF)) then
        Inc(p,2)
      else
        Break;
    until False
    else
    while Byte(p^) in NamingBitmap[FNamePages^[$100+hi(Word(p^))]] do
      Inc(p);

    if p^ = ':' then
    begin
      if (cnToken in aFlags) or not FNamespaces then  // colon has no specific meaning
      begin
        Inc(p);
        if p^ <> #0 then Continue;
      end
      else if FColonPos = -1 then       // this is the first colon, remember it
      begin
        FColonPos := p-FSource.FBuf+FName.Length;
        NameStartFlag := True;
        Inc(p);
        if p^ <> #0 then Continue;
      end;
    end;

    BufAppendChunk(FName, FSource.FBuf, p);
    Result := (FName.Length > 0);

    FSource.FBuf := p;
    if (p^ <> #0) or not FSource.Reload then
      Break;

    p := FSource.FBuf;
  until False;
  if not (Result or (cnOptional in aFlags)) then
    RaiseNameNotFound;
end;

procedure TXMLTextReader.CheckNCName;
begin
  if FNamespaces and (FColonPos <> -1) then
    FatalError('Names of entities, notations and processing instructions may not contain colons', FName.Length);
end;

procedure TXMLTextReader.RaiseNameNotFound;
begin
  if FColonPos <> -1 then
    FatalError('Bad QName syntax, local part is missing')
  else
  // Coming at no cost, this allows more user-friendly error messages
  with FSource do
  if (FBuf^ = #32) or (FBuf^ = #10) or (FBuf^ = #9) or (FBuf^ = #13) then
    FatalError('Whitespace is not allowed here')
  else
    FatalError('Name starts with invalid character');
end;

function TXMLTextReader.ExpectName: XMLString;
begin
  CheckName;
  SetString(Result, FName.Buffer, FName.Length);
end;

function TXMLTextReader.ResolvePredefined: Boolean;
var
  wc: WideChar;
begin
  Result := False;
  with FName do
  begin
    if (Length = 2) and (Buffer[1] = 't') then
    begin
      if Buffer[0] = 'l' then
        wc := '<'
      else if Buffer[0] = 'g' then
        wc := '>'
      else Exit;
    end
    else if Buffer[0] = 'a' then
    begin
      if (Length = 3) and (Buffer[1] = 'm') and (Buffer[2] = 'p') then
        wc := '&'
      else if (Length = 4) and (Buffer[1] = 'p') and (Buffer[2] = 'o') and
       (Buffer[3] = 's') then
        wc := ''''
      else Exit;  
    end
    else if (Length = 4) and (Buffer[0] = 'q') and (Buffer[1] = 'u') and
      (Buffer[2] = 'o') and (Buffer[3] ='t') then
      wc := '"'
    else
      Exit;
  end; // with
  BufAppend(FValue, wc);
  Result := True;
end;

function TXMLTextReader.ParseRef(var ToFill: TWideCharBuf): Boolean;  // [67]
var
  Code: Integer;
begin
  FSource.NextChar;   // skip '&'
  Result := CheckForChar('#');
  if Result then
  begin
    Code := 0;
    if CheckForChar('x') then
    repeat
      case FSource.FBuf^ of
        '0'..'9': Code := Code * 16 + Ord(FSource.FBuf^) - Ord('0');
        'a'..'f': Code := Code * 16 + Ord(FSource.FBuf^) - (Ord('a') - 10);
        'A'..'F': Code := Code * 16 + Ord(FSource.FBuf^) - (Ord('A') - 10);
      else
        Break;
      end;
      FSource.NextChar;
    until Code > $10FFFF
    else
    repeat
      case FSource.FBuf^ of
        '0'..'9': Code := Code * 10 + Ord(FSource.FBuf^) - Ord('0');
      else
        Break;
      end;
      FSource.NextChar;
    until Code > $10FFFF;

    case Code of
      $01..$08, $0B..$0C, $0E..$1F:
        if FXML11 then
          BufAppend(ToFill, WideChar(Code))
        else
          FatalError('Invalid character reference');
      $09, $0A, $0D, $20..$D7FF, $E000..$FFFD:
        BufAppend(ToFill, WideChar(Code));
      $10000..$10FFFF:
        begin
          BufAppend(ToFill, WideChar($D7C0 + (Code shr 10)));
          BufAppend(ToFill, WideChar($DC00 xor (Code and $3FF)));
        end;
    else
      FatalError('Invalid character reference');
    end;
  end
  else CheckName;
  ExpectChar(';');
end;

const
  AttrDelims: TSetOfChar = [#0, '<', '&', '''', '"', #9, #10, #13];
  GT_Delim: TSetOfChar = [#0, '>'];

{ Parse attribute literal, producing plain string value in AttrData.FValueStr.
  If entity references are encountered and FExpandEntities=False, also builds
  a node chain starting from AttrData.FNext. Node chain is built only for the
  first level. If NonCDATA=True, additionally normalizes whitespace in string value. }

procedure TXMLTextReader.ExpectAttValue(AttrData: PNodeData; NonCDATA: Boolean);
var
  wc: WideChar;
  Delim: WideChar;
  ent: TEntityDecl;
  start: TObject;
  curr: PNodeData;
  StartPos: Integer;
  entName: PHashItem;
begin
  SkipQuote(Delim);
  curr := AttrData;
  FValue.Length := 0;
  StartPos := 0;
  start := FSource.FEntity;
  repeat
    wc := FSource.SkipUntil(FValue, AttrDelims);
    if wc = '<' then
      FatalError('Character ''<'' is not allowed in attribute value')
    else if wc = '&' then
    begin
      if ParseRef(FValue) or ResolvePredefined then
        Continue;

      entName := FNameTable.FindOrAdd(FName.Buffer, FName.Length);
      ent := EntityCheck(True);
      if ((ent = nil) or (not FExpandEntities)) and (FSource.FEntity = start) then
      begin
        if FValue.Length > StartPos then
          AllocAttributeValueChunk(curr, StartPos);
        AllocAttributeValueChunk(curr, FValue.Length);
        curr^.FNodeType := ntEntityReference;
        curr^.FQName := entName;
      end;
      StartPos := FValue.Length;
      if Assigned(ent) then
        ContextPush(ent);
    end
    else if wc <> #0 then
    begin
      FSource.NextChar;
      if (wc = Delim) and (FSource.FEntity = start) then
        Break;
      if (wc = #10) or (wc = #9) or (wc = #13) then
        wc := #32;
      BufAppend(FValue, wc);
    end
    else
    begin
      if (FSource.FEntity = start) or not ContextPop then    // #0
        FatalError('Literal has no closing quote', -1);
      StartPos := FValue.Length;
    end;
  until False;
  if Assigned(attrData^.FNext) then
  begin
    FAttrCleanupFlag := True;
    if FValue.Length > StartPos then
      AllocAttributeValueChunk(curr, StartPos);
  end;
  if nonCDATA then
    BufNormalize(FValue, attrData^.FDenormalized)
  else
    attrData^.FDenormalized := False;
  SetString(attrData^.FValueStr, FValue.Buffer, FValue.Length);
end;

const
  PrefixChar: array[Boolean] of string = ('', '%');

procedure TXMLTextReader.EntityToSource(AEntity: TEntityDecl; out Src: TXMLCharSource);
begin
  if AEntity.FOnStack then
    FatalError('Entity ''%s%s'' recursively references itself', [PrefixChar[AEntity.FIsPE], AEntity.FName]);

  if (AEntity.FSystemID <> '') and not AEntity.FPrefetched then
  begin
    if not ResolveResource(AEntity.FSystemID, AEntity.FPublicID, AEntity.FURI, Src) then
    begin
      // TODO: a detailed message like SysErrorMessage(GetLastError) would be great here
      ValidationError('Unable to resolve external entity ''%s''', [AEntity.FName]);
      Src := nil;
      Exit;
    end;
  end
  else
  begin
    Src := TXMLCharSource.Create(AEntity.FReplacementText);
    Src.FLineNo := AEntity.FStartLocation.Line;
    Src.LFPos := Src.FBuf - AEntity.FStartLocation.LinePos;
    // needed in case of prefetched external PE
    if AEntity.FSystemID <> '' then
      Src.SystemID := AEntity.FURI;
  end;

  AEntity.FOnStack := True;
  Src.FEntity := AEntity;
end;

function TXMLTextReader.ContextPush(AEntity: TEntityDecl): Boolean;
var
  Src: TXMLCharSource;
begin
  EntityToSource(AEntity, Src);
  Result := Assigned(Src);
  if Result then
    Initialize(Src);
end;

function TXMLTextReader.ContextPop(Forced: Boolean): Boolean;
var
  Src: TXMLCharSource;
  Error: Boolean;
begin
  Result := Assigned(FSource.FParent) and (Forced or (FSource.Kind = skNone));
  if Result then
  begin
    Src := FSource.FParent;
    Error := False;
    if Assigned(FSource.FEntity) then
    begin
      FSource.FEntity.FOnStack := False;
      FSource.FEntity.FCharCount := FSource.FCharCount;
// [28a] PE that was started between MarkupDecls may not end inside MarkupDecl
      Error := FSource.FEntity.FBetweenDecls and FInsideDecl;
    end;
    FSource.Free;
    FSource := Src;
// correct position of this error is after PE reference
    if Error then
      BadPENesting(esFatal);
  end;
end;

function TXMLTextReader.EntityCheck(NoExternals: Boolean): TEntityDecl;
var
  RefName: XMLString;
  cnt: Integer;
begin
  Result := nil;
  SetString(RefName, FName.Buffer, FName.Length);
  cnt := FName.Length+2;

  if Assigned(FDocType) then
    Result := FDocType.Entities.Get(FName.Buffer, FName.Length) as TEntityDecl;

  if Result = nil then
  begin
    if FStandalone or (FDocType = nil) or not (FHavePERefs or (FDocType.FSystemID <> '')) then
      FatalError('Reference to undefined entity ''%s''', [RefName], cnt)
    else
      ValidationError('Undefined entity ''%s'' referenced', [RefName], cnt);
    Exit;
  end;

  if FStandalone and Result.ExternallyDeclared then
    FatalError('Standalone constraint violation', cnt);
  if Result.FNotationName <> '' then
    FatalError('Reference to unparsed entity ''%s''', [RefName], cnt);

  if NoExternals and (Result.FSystemID <> '') then
    FatalError('External entity reference is not allowed in attribute value', cnt);

  if not Result.FResolved then
    LoadEntity(Result);

  // at this point we know the charcount of the entity being included
  if Result.FCharCount >= cnt then
    CheckMaxChars(Result.FCharCount - cnt);
end;

procedure TXMLTextReader.StartPE;
var
  PEnt: TEntityDecl;
begin
  PEnt := nil;
  if Assigned(FPEMap) then
    PEnt := FPEMap.Get(FName.Buffer, FName.Length) as TEntityDecl;
  if PEnt = nil then
  begin
    ValidationErrorWithName('Undefined parameter entity ''%s'' referenced', FName.Length+2);
    // cease processing declarations, unless document is standalone.
    FDTDProcessed := FStandalone;
    Exit;
  end;

  { cache an external PE so it's only fetched once }
  if (PEnt.FSystemID <> '') and (not PEnt.FPrefetched) and (not PrefetchEntity(PEnt)) then
  begin
    FDTDProcessed := FStandalone;
    Exit;
  end;
  CheckMaxChars(PEnt.FCharCount);

  PEnt.FBetweenDecls := not FInsideDecl;
  ContextPush(PEnt);
  FHavePERefs := True;
end;

function TXMLTextReader.PrefetchEntity(AEntity: TEntityDecl): Boolean;
begin
  Result := ContextPush(AEntity);
  if Result then
  try
    FValue.Length := 0;
    FSource.SkipUntil(FValue, [#0]);
    SetString(AEntity.FReplacementText, FValue.Buffer, FValue.Length);
    AEntity.FCharCount := FValue.Length;
    AEntity.FStartLocation.Line := 1;
    AEntity.FStartLocation.LinePos := 1;
    AEntity.FURI := FSource.SystemID;    // replace base URI with absolute one
  finally
    ContextPop;
    AEntity.FPrefetched := True;
    FValue.Length := 0;
  end;
end;

const
  LiteralDelims: array[TLiteralType] of TSetOfChar = (
    [#0, '''', '"'],                          // ltPlain
    [#0, '''', '"', #13, #10],                // ltPubid
    [#0, '%', '&', '''', '"']                 // ltEntity
  );

function TXMLTextReader.ParseLiteral(var ToFill: TWideCharBuf; aType: TLiteralType;
  Required: Boolean): Boolean;
var
  start: TObject;
  wc, Delim: WideChar;
  dummy: Boolean;
begin
  SkipQuote(Delim, Required);
  Result := (Delim <> #0);
  if not Result then
    Exit;
  ToFill.Length := 0;
  start := FSource.FEntity;
  repeat
    wc := FSource.SkipUntil(ToFill, LiteralDelims[aType]);
    if wc = '%' then       { ltEntity only }
    begin
      FSource.NextChar;
      CheckName;
      ExpectChar(';');
      if FSource.Kind = skInternalSubset then
        FatalError('PE reference not allowed here in internal subset', FName.Length+2);
      StartPE;
    end
    else if wc = '&' then  { ltEntity }
    begin
      if ParseRef(ToFill) then   // charRefs always expanded
        Continue;
      BufAppend(ToFill, '&');
      BufAppendChunk(ToFill, FName.Buffer, FName.Buffer + FName.Length);
      BufAppend(ToFill, ';');
    end
    else if wc <> #0 then
    begin
      FSource.NextChar;
      if (wc = #10) or (wc = #13) then
        wc := #32
      // terminating delimiter must be in the same context as the starting one
      else if (wc = Delim) and (start = FSource.FEntity) then
        Break;
      BufAppend(ToFill, wc);
    end
    else if (FSource.FEntity = start) or not ContextPop then    // #0
      FatalError('Literal has no closing quote', -1);
  until False;
  if aType = ltPubid then
    BufNormalize(ToFill, dummy);
end;

function TXMLTextReader.SkipUntilSeq(const Delim: TSetOfChar; c1: WideChar): Boolean;
var
  wc: WideChar;
begin
  Result := False;
  StoreLocation(FTokenStart);
  repeat
    wc := FSource.SkipUntil(FValue, Delim);
    if wc <> #0 then
    begin
      FSource.NextChar;
      if (FValue.Length > 0) then
      begin
        if (FValue.Buffer[FValue.Length-1] = c1) then
        begin
          Dec(FValue.Length);
          Result := True;
          Exit;
        end;
      end;
      BufAppend(FValue, wc);
    end;
  until wc = #0;
end;

procedure TXMLTextReader.ParseComment(discard: Boolean);    // [15]
var
  SaveLength: Integer;
begin
  ExpectString('--');
  SaveLength := FValue.Length;
  if not SkipUntilSeq([#0, '-'], '-') then
    FatalError('Unterminated comment', -1);
  ExpectChar('>');

  if not discard then
  begin
    FCurrNode := @FNodeStack[FNesting];
    FCurrNode^.FNodeType := ntComment;
    FCurrNode^.FQName := nil;
    FCurrNode^.FValueStart := @FValue.Buffer[SaveLength];
    FCurrNode^.FValueLength := FValue.Length-SaveLength;
  end;
  FValue.Length := SaveLength;
end;

procedure TXMLTextReader.ParsePI;                    // [16]
begin
  FSource.NextChar;      // skip '?'
  CheckName;
  CheckNCName;
  with FName do
    if (Length = 3) and
     ((Buffer[0] = 'X') or (Buffer[0] = 'x')) and
     ((Buffer[1] = 'M') or (Buffer[1] = 'm')) and
     ((Buffer[2] = 'L') or (Buffer[2] = 'l')) then
  begin
    if not BufEquals(FName, 'xml') then
      FatalError('''xml'' is a reserved word; it must be lowercase', FName.Length)
    else
      FatalError('XML declaration is not allowed here', FName.Length);
  end;

  if FSource.FBuf^ <> '?' then
    SkipS(True);

  FValue.Length := 0;
  if not SkipUntilSeq(GT_Delim, '?') then
    FatalError('Unterminated processing instruction', -1);
  SetNodeInfoWithValue(ntProcessingInstruction,
    FNameTable.FindOrAdd(FName.Buffer, FName.Length));
end;

function TXMLTextReader.CreatePINode: TDOMNode;
var
  NameStr, ValueStr: DOMString;
begin
  SetString(NameStr, FName.Buffer, FName.Length);
  SetString(ValueStr, FValue.Buffer, FValue.Length);
  result := Doc.CreateProcessingInstruction(NameStr, ValueStr);
end;

const
  vers: array[Boolean] of TXMLVersion = (xmlVersion10, xmlVersion11);

procedure TXMLTextReader.ParseXmlOrTextDecl(TextDecl: Boolean);
var
  Delim: WideChar;
  buf: array[0..31] of WideChar;
  I: Integer;
begin
  SkipS(True);
  // [24] VersionInfo: optional in TextDecl, required in XmlDecl
  if (not TextDecl) or (FSource.FBuf^ = 'v') then
  begin
    ExpectString('version');
    ExpectEq;
    SkipQuote(Delim);
    I := 0;
    while (I < 3) and (FSource.FBuf^ <> Delim) do
    begin
      buf[I] := FSource.FBuf^;
      Inc(I);
      FSource.NextChar;
    end;
    if (I <> 3) or (buf[0] <> '1') or (buf[1] <> '.') or
      ((buf[2] <> '0') and (buf[2] <> '1')) then
      FatalError('Illegal version number', -1);

    ExpectChar(Delim);
    FSource.FXMLVersion := vers[buf[2] = '1'];

    if TextDecl and (FSource.FXMLVersion = xmlVersion11) and not FXML11 then
      FatalError('XML 1.0 document cannot invoke XML 1.1 entities', -1);

    if TextDecl or (FSource.FBuf^ <> '?') then
      SkipS(True);
  end;

  // [80] EncodingDecl: required in TextDecl, optional in XmlDecl
  if TextDecl or (FSource.FBuf^ = 'e') then
  begin
    ExpectString('encoding');
    ExpectEq;
    SkipQuote(Delim);
    I := 0;
    while (I < 30) and (FSource.FBuf^ <> Delim) and (FSource.FBuf^ < #127) and
      ((Char(ord(FSource.FBuf^)) in ['A'..'Z', 'a'..'z']) or
      ((I > 0) and (Char(ord(FSource.FBuf^)) in ['0'..'9', '.', '-', '_']))) do
    begin
      buf[I] := FSource.FBuf^;
      Inc(I);
      FSource.NextChar;
    end;
    if not CheckForChar(Delim) then
      FatalError('Illegal encoding name', i);

    SetString(FSource.FXMLEncoding, buf, i);
    if not FSource.SetEncoding(FSource.FXMLEncoding) then  // <-- Wide2Ansi conversion here
      FatalError('Encoding ''%s'' is not supported', [FSource.FXMLEncoding], i+1);

    if FSource.FBuf^ <> '?' then
      SkipS(not TextDecl);
  end;

  // [32] SDDecl: forbidden in TextDecl, optional in XmlDecl
  if (not TextDecl) and (FSource.FBuf^ = 's') then
  begin
    ExpectString('standalone');
    ExpectEq;
    SkipQuote(Delim);
    if FSource.Matches('yes') then
      FStandalone := True
    else if not FSource.Matches('no') then
      FatalError('Only "yes" or "no" are permitted as values of "standalone"', -1);
    ExpectChar(Delim);
    SkipS;
  end;

  ExpectString('?>');
  { Switch to 1.1 rules only after declaration is parsed completely. This is to
    ensure that NEL and LSEP within declaration are rejected (rmt-056, rmt-057) }
  if FSource.FXMLVersion = xmlVersion11 then
    FXML11 := True;
end;

procedure TXMLTextReader.DTDReloadHook;
var
  p: PWideChar;
begin
{ FSource converts CR, NEL and LSEP linebreaks to LF, and CR-NEL sequences to CR-LF.
  We must further remove the CR chars and have only LF's left. }
  p := FDTDStartPos;
  while p < FSource.FBuf do
  begin
    while (p < FSource.FBuf) and (p^ <> #13) do
      Inc(p);
    BufAppendChunk(FIntSubset, FDTDStartPos, p);
    if p^ = #13 then
      Inc(p);
    FDTDStartPos := p;
  end;
  FDTDStartPos := TXMLDecodingSource(FSource).FBufStart;
end;

procedure TXMLTextReader.ParseDoctypeDecl;    // [28]
var
  Src: TXMLCharSource;
begin
  if FState >= rsDTD then
    FatalError('Markup declaration is not allowed here');
  if FDisallowDoctype then
    FatalError('Document type is prohibited by parser settings');

  ExpectString('DOCTYPE');
  SkipS(True);

  FDocType := TDTDModel.Create(FNameTable);
  FDTDProcessed := True;    // assume success
  FState := rsDTD;

  FDocType.FName := ExpectName;
  SkipS(True);
  ParseExternalID(FDocType.FSystemID, FDocType.FPublicID, False);
  SkipS;

  if CheckForChar('[') then
  begin
    BufAllocate(FIntSubset, 256);
    FSource.Kind := skInternalSubset;
    try
      FDTDStartPos := FSource.FBuf;
      ParseMarkupDecl;
      DTDReloadHook;     // fetch last chunk
      SetString(FDocType.FInternalSubset, FIntSubset.Buffer, FIntSubset.Length);
    finally
      FreeMem(FIntSubset.Buffer);
      FSource.Kind := skNone;
    end;
    ExpectChar(']');
    SkipS;
  end;
  ExpectChar('>');

  if (FDocType.FSystemID <> '') then
  begin
    if ResolveResource(FDocType.FSystemID, FDocType.FPublicID, FSource.SystemID, Src) then
    begin
      Initialize(Src);
      try
        Src.Kind := skManualPop;
        ParseMarkupDecl;
      finally
        ContextPop(True);
      end;
    end
    else
    begin
      ValidationError('Unable to resolve external DTD subset', []);
      FDTDProcessed := FStandalone;
    end;
  end;
  FState := rsAfterDTD;
  FCurrNode^.FNodeType := ntDocumentType;
end;

procedure TXMLTextReader.ExpectEq;   // [25]
begin
  if FSource.FBuf^ <> '=' then
    SkipS;
  if FSource.FBuf^ <> '=' then
    FatalError('Expected "="');
  FSource.NextChar;
  SkipS;
end;


{ DTD stuff }

procedure TXMLTextReader.BadPENesting(S: TErrorSeverity);
begin
  if (S = esFatal) or FValidate then
    DoError(S, 'Parameter entities must be properly nested');
end;

procedure TXMLTextReader.StandaloneError(LineOffs: Integer);
begin
  ValidationError('Standalone constriant violation', [], LineOffs);
end;

function TXMLTextReader.ParseQuantity: TCPQuant;
begin
  case FSource.FBuf^ of
    '?': Result := cqZeroOrOnce;
    '*': Result := cqZeroOrMore;
    '+': Result := cqOnceOrMore;
  else
    Result := cqOnce;
    Exit;
  end;
  FSource.NextChar;
end;

function TXMLTextReader.FindOrCreateElDef: TElementDecl;
var
  p: PHashItem;
begin
  CheckName;
  p := FNameTable.FindOrAdd(FName.Buffer, FName.Length);
  Result := TElementDecl(p^.Data);
  if Result = nil then
  begin
    Result := TElementDecl.Create;
    p^.Data := Result;
  end;
end;

procedure TXMLTextReader.ExpectChoiceOrSeq(CP: TContentParticle; MustEndIn: TObject);     // [49], [50]
var
  Delim: WideChar;
  CurrentCP: TContentParticle;
begin
  Delim := #0;
  repeat
    CurrentCP := CP.Add;
    SkipWhitespace;
    if CheckForChar('(') then
      ExpectChoiceOrSeq(CurrentCP, FSource.FEntity)
    else
      CurrentCP.Def := FindOrCreateElDef;

    CurrentCP.CPQuant := ParseQuantity;
    SkipWhitespace;
    if FSource.FBuf^ = ')' then
      Break;
    if Delim = #0 then
    begin
      if (FSource.FBuf^ = '|') or (FSource.FBuf^ = ',') then
        Delim := FSource.FBuf^
      else
        FatalError('Expected pipe or comma delimiter');
    end
    else
      if FSource.FBuf^ <> Delim then
        FatalError(Delim);
    FSource.NextChar; // skip delimiter
  until False;
  if MustEndIn <> FSource.FEntity then
    BadPENesting;
  FSource.NextChar;

  if Delim = '|' then
    CP.CPType := ctChoice
  else
    CP.CPType := ctSeq;    // '(foo)' is a sequence!
end;

procedure TXMLTextReader.ParseElementDecl;            // [45]
var
  ElDef: TElementDecl;
  CurrentEntity: TObject;
  I: Integer;
  CP: TContentParticle;
  Typ: TElementContentType;
  ExtDecl: Boolean;
begin
  CP := nil;
  Typ := ctUndeclared;         // satisfy compiler
  ExpectWhitespace;
  ElDef := FindOrCreateElDef;
  if ElDef.ContentType <> ctUndeclared then
    ValidationErrorWithName('Duplicate declaration of element ''%s''', FName.Length);

  ExtDecl := FSource.Kind <> skInternalSubset;

  ExpectWhitespace;
  if FSource.Matches('EMPTY') then
    Typ := ctEmpty
  else if FSource.Matches('ANY') then
    Typ := ctAny
  else if CheckForChar('(') then
  begin
    CP := TContentParticle.Create;
    try
      CurrentEntity := FSource.FEntity;
      SkipWhitespace;
      if FSource.Matches('#PCDATA') then       // Mixed section [51]
      begin
        SkipWhitespace;
        Typ := ctMixed;
        while FSource.FBuf^ <> ')' do
        begin
          ExpectChar('|');
          SkipWhitespace;

          with CP.Add do
          begin
            Def := FindOrCreateElDef;
            for I := CP.ChildCount-2 downto 0 do
              if Def = CP.Children[I].Def then
                ValidationError('Duplicate token in mixed section', [], FName.Length);
          end;
          SkipWhitespace;
        end;
        if CurrentEntity <> FSource.FEntity then
          BadPENesting;
        FSource.NextChar;
        if (not CheckForChar('*')) and (CP.ChildCount > 0) then
          FatalError(WideChar('*'));
        CP.CPQuant := cqZeroOrMore;
        CP.CPType := ctChoice;
      end
      else       // Children section [47]
      begin
        Typ := ctChildren;
        ExpectChoiceOrSeq(CP, CurrentEntity);
        CP.CPQuant := ParseQuantity;
      end;
    except
      CP.Free;
      raise;
    end;
  end
  else
    FatalError('Invalid content specification');
  // SAX: DeclHandler.ElementDecl(name, model);
  if FDTDProcessed and (ElDef.ContentType = ctUndeclared) then
  begin
    ElDef.ExternallyDeclared := ExtDecl;
    ElDef.ContentType := Typ;
    ElDef.RootCP := CP;
  end
  else
    CP.Free;
end;


procedure TXMLTextReader.ParseNotationDecl;        // [82]
var
  NameStr, SysID, PubID: XMLString;
begin
  ExpectWhitespace;
  NameStr := ExpectName;
  CheckNCName;
  ExpectWhitespace;
  if not ParseExternalID(SysID, PubID, True) then
    FatalError('Expected external or public ID');
  if FDTDProcessed then
    DoNotationDecl(NameStr, PubID, SysID);
end;

const
  AttrDataTypeNames: array[TAttrDataType] of XMLString = (
    'CDATA',
    'ID',
    'IDREF',
    'IDREFS',
    'ENTITY',
    'ENTITIES',
    'NMTOKEN',
    'NMTOKENS',
    'NOTATION'
  );

procedure TXMLTextReader.ParseAttlistDecl;         // [52]
var
  ElDef: TElementDecl;
  AttDef: TAttributeDef;
  dt: TAttrDataType;
  Found, DiscardIt: Boolean;
  Offsets: array [Boolean] of Integer;
  attrName: PHashItem;
begin
  ExpectWhitespace;
  ElDef := FindOrCreateElDef;
  SkipWhitespace;
  while FSource.FBuf^ <> '>' do
  begin
    CheckName;
    ExpectWhitespace;
    attrName := FNameTable.FindOrAdd(FName.Buffer, FName.Length);
    AttDef := TAttributeDef.Create(attrName, FColonPos);
    try
      AttDef.ExternallyDeclared := FSource.Kind <> skInternalSubset;
// In case of duplicate declaration of the same attribute, we must discard it,
// not modifying ElDef, and suppressing certain validation errors.
      DiscardIt := (not FDTDProcessed) or Assigned(ElDef.GetAttrDef(attrName));

      if CheckForChar('(') then     // [59]
      begin
        AttDef.DataType := dtNmToken;
        repeat
          SkipWhitespace;
          CheckName([cnToken]);
          if not AttDef.AddEnumToken(FName.Buffer, FName.Length) then
            ValidationError('Duplicate token in enumerated attibute declaration', [], FName.Length);
          SkipWhitespace;
        until not CheckForChar('|');
        ExpectChar(')');
        ExpectWhitespace;
      end
      else
      begin
        StoreLocation(FTokenStart);
        // search topside-up so that e.g. NMTOKENS is matched before NMTOKEN
        for dt := dtNotation downto dtCData do
        begin
          Found := FSource.Matches(AttrDataTypeNames[dt]);
          if Found then
            Break;
        end;
        if Found and SkipWhitespace then
        begin
          AttDef.DataType := dt;
          if (dt = dtId) and not DiscardIt then
          begin
            if Assigned(ElDef.IDAttr) then
              ValidationError('Only one attribute of type ID is allowed per element',[])
            else
              ElDef.IDAttr := AttDef;
          end
          else if dt = dtNotation then          // no test cases for these ?!
          begin
            if not DiscardIt then
            begin
              if Assigned(ElDef.NotationAttr) then
                ValidationError('Only one attribute of type NOTATION is allowed per element',[])
              else
                ElDef.NotationAttr := AttDef;
              if ElDef.ContentType = ctEmpty then
                ValidationError('NOTATION attributes are not allowed on EMPTY elements',[]);
            end;
            ExpectChar('(');
            repeat
              SkipWhitespace;
              StoreLocation(FTokenStart);
              CheckName;
              CheckNCName;
              if not AttDef.AddEnumToken(FName.Buffer, FName.Length) then
                ValidationError('Duplicate token in NOTATION attribute declaration',[], FName.Length);

              if (not DiscardIt) and FValidate then
                AddForwardRef(FName.Buffer, FName.Length);
              SkipWhitespace;
            until not CheckForChar('|');
            ExpectChar(')');
            ExpectWhitespace;
          end;
        end
        else
        begin
          // don't report 'expected whitespace' if token does not match completely
          Offsets[False] := 0;
          Offsets[True] := Length(AttrDataTypeNames[dt]);
          if Found and (FSource.FBuf^ < 'A') then
            ExpectWhitespace
          else
            FatalError('Illegal attribute type for ''%s''', [attrName^.Key], Offsets[Found]);
        end;
      end;
      StoreLocation(FTokenStart);
      if FSource.Matches('#REQUIRED') then
        AttDef.Default := adRequired
      else if FSource.Matches('#IMPLIED') then
        AttDef.Default := adImplied
      else if FSource.Matches('#FIXED') then
      begin
        AttDef.Default := adFixed;
        ExpectWhitespace;
      end
      else
        AttDef.Default := adDefault;

      if AttDef.Default in [adDefault, adFixed] then
      begin
        if AttDef.DataType = dtId then
          ValidationError('An attribute of type ID cannot have a default value',[]);

// See comments to valid-sa-094: PE expansion should be disabled in AttDef.
        ExpectAttValue(AttDef.Data, dt <> dtCDATA);

        if not ValidateAttrSyntax(AttDef, AttDef.Data^.FValueStr) then
          ValidationError('Default value for attribute ''%s'' has wrong syntax', [attrName^.Key]);
      end;
      // SAX: DeclHandler.AttributeDecl(...)
      if DiscardIt then
        AttDef.Free
      else
        ElDef.AddAttrDef(AttDef);
    except
      AttDef.Free;
      raise;
    end;
    SkipWhitespace;
  end;
end;

procedure TXMLTextReader.ParseEntityDecl;        // [70]
var
  IsPE, Exists: Boolean;
  Entity: TEntityDecl;
  Map: THashTable;
  Item: PHashItem;
begin
  if not SkipWhitespace(True) then
    FatalError('Expected whitespace');
  IsPE := CheckForChar('%');
  if IsPE then                  // [72]
  begin
    ExpectWhitespace;
    if FPEMap = nil then
      FPEMap := THashTable.Create(64, True);
    Map := FPEMap;
  end
  else
    Map := FDocType.Entities;

  Entity := TEntityDecl.Create;
  try
    Entity.ExternallyDeclared := FSource.Kind <> skInternalSubset;
    Entity.FIsPE := IsPE;
    CheckName;
    CheckNCName;
    Item := Map.FindOrAdd(FName.Buffer, FName.Length, Exists);
    ExpectWhitespace;

    // remember where the entity is declared
    Entity.FURI := FSource.SystemID;

    if FEntityValue.Buffer = nil then
      BufAllocate(FEntityValue, 256);

    if ParseLiteral(FEntityValue, ltEntity, False) then
    begin
      SetString(Entity.FReplacementText, FEntityValue.Buffer, FEntityValue.Length);
      Entity.FCharCount := FEntityValue.Length;
      Entity.FStartLocation := FTokenStart;
    end
    else
    begin
      if not ParseExternalID(Entity.FSystemID, Entity.FPublicID, False) then
        FatalError('Expected entity value or external ID');

      if not IsPE then                // [76]
      begin
        if FSource.FBuf^ <> '>' then
          ExpectWhitespace;
        if FSource.Matches('NDATA') then
        begin
          ExpectWhitespace;
          StoreLocation(FTokenStart);
          Entity.FNotationName := ExpectName;
          if FValidate then
            AddForwardRef(FName.Buffer, FName.Length);
          // SAX: DTDHandler.UnparsedEntityDecl(...);
        end;
      end;
    end;
  except
    Entity.Free;
    raise;
  end;

  // Repeated declarations of same entity are legal but must be ignored
  if FDTDProcessed and not Exists then
  begin
    Item^.Data := Entity;
    Entity.FName := Item^.Key;
  end
  else
    Entity.Free;
end;

procedure TXMLTextReader.ParseIgnoreSection;
var
  IgnoreLoc: TLocation;
  IgnoreLevel: Integer;
  wc: WideChar;
begin
  StoreLocation(IgnoreLoc);
  IgnoreLevel := 1;
  repeat
    FValue.Length := 0;
    wc := FSource.SkipUntil(FValue, [#0, '<', ']']);
    if FSource.Matches('<![') then
      Inc(IgnoreLevel)
    else if FSource.Matches(']]>') then
      Dec(IgnoreLevel)
    else if wc <> #0 then
      FSource.NextChar
    else // PE's aren't recognized in ignore section, cannot ContextPop()
      DoErrorPos(esFatal, 'IGNORE section is not closed', IgnoreLoc);
  until IgnoreLevel=0;
end;

procedure TXMLTextReader.ParseMarkupDecl;        // [29]
var
  IncludeLevel: Integer;
  CurrentEntity: TObject;
  IncludeLoc: TLocation;
  CondType: (ctUnknown, ctInclude, ctIgnore);
begin
  IncludeLevel := 0;
  repeat
    SkipWhitespace;

    if (FSource.FBuf^ = ']') and (IncludeLevel > 0) then
    begin
      ExpectString(']]>');
      Dec(IncludeLevel);
      Continue;
    end;

    if not CheckForChar('<') then
      Break;

    CurrentEntity := FSource.FEntity;

    if FSource.FBuf^ = '?' then
    begin
      ParsePI;
      if Assigned(doc) then
        doc.AppendChild(CreatePINode);
    end
    else
    begin
      ExpectChar('!');
      if FSource.FBuf^ = '-' then
        ParseComment(True)
      else if CheckForChar('[') then
      begin
        if FSource.Kind = skInternalSubset then
          FatalError('Conditional sections are not allowed in internal subset', 1);

        SkipWhitespace;

        CondType := ctUnknown;  // satisfy compiler
        if FSource.Matches('INCLUDE') then
          CondType := ctInclude
        else if FSource.Matches('IGNORE') then
          CondType := ctIgnore
        else
          FatalError('Expected "INCLUDE" or "IGNORE"');

        SkipWhitespace;
        if CurrentEntity <> FSource.FEntity then
          BadPENesting;
        ExpectChar('[');
        if CondType = ctInclude then
        begin
          if IncludeLevel = 0 then
            StoreLocation(IncludeLoc);
          Inc(IncludeLevel);
        end
        else if CondType = ctIgnore then
          ParseIgnoreSection;
      end
      else
      begin
        FInsideDecl := True;
        if FSource.Matches('ELEMENT') then
          ParseElementDecl
        else if FSource.Matches('ENTITY') then
          ParseEntityDecl
        else if FSource.Matches('ATTLIST') then
          ParseAttlistDecl
        else if FSource.Matches('NOTATION') then
          ParseNotationDecl
        else
          FatalError('Illegal markup declaration');

        SkipWhitespace;

        if CurrentEntity <> FSource.FEntity then
          BadPENesting;
        ExpectChar('>');
        FInsideDecl := False;
      end;
    end;
  until False;
  if IncludeLevel > 0 then
    DoErrorPos(esFatal, 'INCLUDE section is not closed', IncludeLoc);
  if (FSource.Kind = skInternalSubset) and (FSource.FBuf^ = ']') then
    Exit;
  if FSource.FBuf^ <> #0 then
    FatalError('Illegal character in DTD');
end;

procedure TXMLTextReader.ProcessDTD(ASource: TXMLCharSource);
begin
  doc := TXMLDocument.Create;
  FNameTable := doc.Names;
  FDocType := TDTDModel.Create(FNameTable);
  // TODO: DTD labeled version 1.1 will be rejected - must set FXML11 flag
  doc.AppendChild(TDOMDocumentType.Create(doc, FDocType));
  NSPrepare;
  Initialize(ASource);
  ParseMarkupDecl;
end;


procedure TXMLTextReader.LoadEntity(AEntity: TEntityDecl);
var
  InnerReader: TXMLTextReader;
  Src: TXMLCharSource;
  Ent: TDOMEntityEx;
  DoctypeNode: TDOMDocumentType;
begin
  if Assigned(doc) then
    DoctypeNode := doc.DocType
  else
    Exit;
  if DoctypeNode = nil then
    Exit;
  Ent := TDOMEntityEx(DocTypeNode.Entities.GetNamedItem(AEntity.FName));
  if Ent = nil then
    Exit;
  InnerReader := TXMLTextReader.Create(FCtrl);
  try
    InnerReader.FAttrTag := FAttrTag;
    InnerReader.FDocType := FDocType.Reference;
    EntityToSource(AEntity, Src);
    Ent.SetReadOnly(False);
    if Assigned(Src) then
      InnerReader.ProcessFragment(Src, Ent);
    AEntity.FResolved := True;
  finally
    FAttrTag := InnerReader.FAttrTag;
    InnerReader.Free;
    AEntity.FOnStack := False;
    Ent.SetReadOnly(True);
  end;
end;


procedure TXMLTextReader.SetEOFState;
begin
  FCurrNode := @FNodeStack[0];
  Finalize(FCurrNode^);
  FillChar(FCurrNode^, sizeof(TNodeData), 0);
end;

procedure TXMLTextReader.ValidateCurrentNode;
var
  ElDef: TElementDecl;
  AttDef: TAttributeDef;
  attr: PNodeData;
  i: Integer;
begin
  case FCurrNode^.FNodeType of
    ntElement:
      begin
        if (FNesting = 0) and (not FFragmentMode) then
        begin
          if Assigned(FDocType) then
          begin
            if FDocType.FName <> FCurrNode^.FQName^.Key then
              DoErrorPos(esError, 'Root element name does not match DTD', FCurrNode^.FLoc);
          end
          else
            DoErrorPos(esError, 'Missing DTD', FCurrNode^.FLoc);
        end;
        ElDef := TElementDecl(FCurrNode^.FQName^.Data);
        if (ElDef = nil) or (ElDef.ContentType = ctUndeclared) then
          DoErrorPos(esError, 'Using undeclared element ''%s''',[FCurrNode^.FQName^.Key], FCurrNode^.FLoc);

        if not FValidators[FValidatorNesting].IsElementAllowed(ElDef) then
          DoErrorPos(esError, 'Element ''%s'' is not allowed in this context',[FCurrNode^.FQName^.Key], FCurrNode^.FLoc);

        PushVC(ElDef);

        { Validate attributes }
        for i := 1 to FAttrCount do
        begin
          attr := @FNodeStack[FNesting+i];
          AttDef := TAttributeDef(attr^.FTypeInfo);
          if AttDef = nil then
            DoErrorPos(esError, 'Using undeclared attribute ''%s'' on element ''%s''',
              [attr^.FQName^.Key, FCurrNode^.FQName^.Key], attr^.FLoc)
          else if ((AttDef.DataType <> dtCdata) or (AttDef.Default = adFixed)) then
          begin
            if FStandalone and AttDef.ExternallyDeclared then
              { TODO: perhaps should use different and more descriptive messages }
              if attr^.FDenormalized then
                DoErrorPos(esError, 'Standalone constraint violation', attr^.FLoc2)
              else if i > FSpecifiedAttrs then
                DoError(esError, 'Standalone constraint violation');

            // TODO: what about normalization of AttDef.Value? (Currently it IS normalized)
            if (AttDef.Default = adFixed) and (AttDef.Data^.FValueStr <> attr^.FValueStr) then
              DoErrorPos(esError, 'Value of attribute ''%s'' does not match its #FIXED default',[attr^.FQName^.Key], attr^.FLoc2);
            if not ValidateAttrSyntax(AttDef, attr^.FValueStr) then
              DoErrorPos(esError, 'Attribute ''%s'' type mismatch', [attr^.FQName^.Key], attr^.FLoc2);
            ValidateAttrValue(AttDef, attr);
          end;
        end;
      end;

    ntEndElement:
      begin
        if FValidators[FValidatorNesting].Incomplete then
          ValidationError('Element ''%s'' is missing required sub-elements', [FCurrNode^.FQName^.Key], -1);
        if FValidatorNesting > 0 then
          Dec(FValidatorNesting);
      end;

    ntText, ntSignificantWhitespace:
      case FValidators[FValidatorNesting].FContentType of
        ctChildren:
          if FCurrNode^.FNodeType = ntText then
            ValidationError('Character data is not allowed in element-only content',[])
          else
          begin
            if FValidators[FValidatorNesting].FSaViolation then
              StandaloneError(-1);
            FCurrNode^.FNodeType := ntWhitespace;
          end;
        ctEmpty:
          ValidationError('Character data is not allowed in EMPTY elements', []);
      end;

    ntCDATA:
      if FValidators[FValidatorNesting].FContentType = ctChildren then
        ValidationError('CDATA sections are not allowed in element-only content',[]);

    ntProcessingInstruction:
      if FValidators[FValidatorNesting].FContentType = ctEmpty then
        ValidationError('Processing instructions are not allowed within EMPTY elements', []);

    ntComment:
      if FValidators[FValidatorNesting].FContentType = ctEmpty then
        ValidationError('Comments are not allowed within EMPTY elements', []);

    ntDocumentType:
      ValidateDTD;
  end;
end;

procedure TXMLTextReader.HandleEntityStart;
begin
  FCurrNode := @FNodeStack[FNesting];
  FCurrNode^.FNodeType := ntEntityReference;
  FCurrNode^.FQName := FNameTable.FindOrAdd(FName.Buffer, FName.Length);
  FCurrNode^.FValueStart := nil;
  FCurrNode^.FValueLength := 0;
end;

procedure TXMLTextReader.HandleEntityEnd;
begin
  ContextPop(True);
  if FNesting > 0 then Dec(FNesting);
  FCurrNode := @FNodeStack[FNesting];
  FCurrNode^.FNodeType := ntEndEntity;
  // TODO: other properties of FCurrNode
  FNext := xtText;
end;

procedure TXMLTextReader.ResolveEntity;
begin
  if FCurrNode^.FNodeType <> ntEntityReference then
    raise EInvalidOperation.Create('Wrong node type');

  {... here must actually call EntityCheck, but it's called in main loop}

  FNext := xtPushEntity;
end;

procedure TXMLTextReader.DoStartEntity;
var
  src: TXMLCharSource;
begin
  Inc(FNesting);
  FCurrNode := AllocNodeData(FNesting);
  if Assigned(FCurrEntity) then
    ContextPush(FCurrEntity)
  else
  begin
  // Undefined entity -- use a dummy inputsource, in order to get a matching EndEntity event
    src := TXMLCharSource.Create('');
    src.Kind := skManualPop;
    Initialize(src);
  end;
  FNext := xtText;
end;

function TXMLTextReader.DoStartElement: TDOMElement;
var
  Attr: TDOMAttr;
  i: Integer;
begin
  with FCurrNode^.FQName^ do
    Result := doc.CreateElementBuf(PWideChar(Key), Length(Key));
  if Assigned(FCurrNode^.FNsUri) then
    Result.SetNSI(FCurrNode^.FNsUri^.Key, FCurrNode^.FColonPos+1);

  for i := 1 to FAttrCount do
  begin
    Attr := LoadAttribute(doc, @FNodeStack[FNesting+i]);
    Result.SetAttributeNode(Attr);
    // Attach element to ID map entry if necessary
    if Assigned(FNodeStack[FNesting+i].FIDEntry) then
      FNodeStack[FNesting+i].FIDEntry^.Data := Result;
  end;
end;

// The code below does the bulk of the parsing, and must be as fast as possible.
// To minimize CPU cache effects, methods from different classes are kept together

function TXMLDecodingSource.SkipUntil(var ToFill: TWideCharBuf; const Delim: TSetOfChar;
  wsflag: PBoolean): WideChar;
var
  old: PWideChar;
  nonws: Boolean;
  wc: WideChar;
begin
  nonws := False;
  repeat
    old := FBuf;
    repeat
      wc := FBuf^;
      if (wc = #10) or (wc = #13) or (FXML11Rules and ((wc = #$85) or
        (wc = #$2028))) then
      begin
// strictly this is needed only for 2-byte lineendings
        BufAppendChunk(ToFill, old, FBuf);
        NewLine;
        old := FBuf;
        wc := FBuf^
      end
      else if ((wc < #32) and (not ((wc = #0) and (FBuf >= FBufEnd))) and
        (wc <> #9)) or (wc > #$FFFD) or
        (FXML11Rules and (wc >= #$7F) and (wc <= #$9F)) then
             FReader.FatalError('Invalid character');
      if (wc < #255) and (Char(ord(wc)) in Delim) then
        Break;
// the checks above filter away everything below #32 that isn't a whitespace
      if wc > #32 then
        nonws := True;
      Inc(FBuf);
    until False;
    Result := wc;
    BufAppendChunk(ToFill, old, FBuf);
  until (Result <> #0) or (not Reload);
  if Assigned(wsflag) then
    wsflag^ := wsflag^ or nonws;
end;

const
  TextDelims: array[Boolean] of TSetOfChar = (
    [#0, '<', '&', '>'],
    [#0, '>']
  );

  textNodeTypes: array[Boolean] of TXMLNodeType = (
    ntSignificantWhitespace,
    ntText
  );

procedure TXMLTextReader.ParseContent(cursor: TDOMNode_WithChildren);
var
  element: TDOMElement;
begin
  while Read do
  begin
    if FValidate then
      ValidateCurrentNode;

    case FCurrNode^.FNodeType of
      ntText:
        cursor.InternalAppend(doc.CreateTextNodeBuf(FValue.Buffer, FValue.Length, False));

      ntWhitespace, ntSignificantWhitespace:
        if FPreserveWhitespace then
          cursor.InternalAppend(doc.CreateTextNodeBuf(FValue.Buffer, FValue.Length, FCurrNode^.FNodeType = ntWhitespace));

      ntCDATA:
        cursor.InternalAppend(DoCDSect(FValue.Buffer, FValue.Length));

      ntProcessingInstruction:
        cursor.InternalAppend(CreatePINode);

      ntComment:
        if not FIgnoreComments then
          cursor.InternalAppend(doc.CreateCommentBuf(FCurrNode^.FValueStart, FCurrNode^.FValueLength));

      ntElement:
        begin
          element := DoStartElement;
          cursor.InternalAppend(element);
          cursor := element;
        end;

      ntEndElement:
          cursor := TDOMNode_WithChildren(cursor.ParentNode);

      ntDocumentType:
        if not FCanonical then
          cursor.InternalAppend(TDOMDocumentType.Create(doc, FDocType));

      ntEntityReference:
        cursor.InternalAppend(doc.CreateEntityReference(FCurrNode^.FQName^.Key));
    end;
  end;
end;

function TXMLTextReader.ReadTopLevel: Boolean;
var
  nonWs: Boolean;
  wc: WideChar;
  tok: TXMLToken;
begin
  if FNext = xtFakeLF then
  begin
    Result := SetupFakeLF(xtText);
    Exit;
  end;

  StoreLocation(FTokenStart);
  nonWs := False;
  FValue.Length := 0;

  if FNext = xtText then
  repeat
    wc := FSource.SkipUntil(FValue, [#0, '<'], @nonWs);
    if wc = '<' then
    begin
      Inc(FSource.FBuf);
      if FSource.FBufEnd < FSource.FBuf + 2 then
        FSource.Reload;
      if CheckName([cnOptional]) then
        tok := xtElement
      else if FSource.FBuf^ = '!' then
      begin
        Inc(FSource.FBuf);
        if FSource.FBuf^ = '-' then
        begin
          if FIgnoreComments then
          begin
            ParseComment(True);
            Continue;
          end;
          tok := xtComment;
        end
        else
          tok := xtDoctype;
      end
      else if FSource.FBuf^ = '?' then
        tok := xtPI
      else
        RaiseNameNotFound;
    end
    else  // #0
    begin
      if FState < rsRoot then
        FatalError('Root element is missing');
      tok := xtEOF;
    end;
    if nonWs then
      FatalError('Illegal at document level', -1);

    if FCanonical and (FState > rsRoot) and (tok <> xtEOF) then
    begin
      Result := SetupFakeLF(tok);
      Exit;
    end;

    Break;
  until False
  else   // FNext <> xtText
    tok := FNext;

  if FCanonical and (FState < rsRoot) and (tok <> xtDoctype) then
    FNext := xtFakeLF
  else
    FNext := xtText;

  case tok of
    xtElement:
      begin
        if FState > rsRoot then
          FatalError('Only one top-level element allowed', FName.Length)
        else if FState < rsRoot then
        begin
          // dispose notation refs from DTD, if any
          ClearForwardRefs;
          FState := rsRoot;
        end;
        ParseStartTag;
      end;
    xtPI:         ParsePI;
    xtComment:    ParseComment(False);
    xtDoctype:
      begin
        ParseDoctypeDecl;
        if FCanonical then
        begin
          // recurse, effectively ignoring the DTD
          result := ReadTopLevel;
          Exit;
        end;
      end;
    xtEOF:  SetEofState;
  end;
  Result := tok <> xtEOF;
end;

function TXMLTextReader.Read: Boolean;
var
  nonWs: Boolean;
  wc: WideChar;
  InCDATA: Boolean;
  tok: TXMLToken;
begin
  if FNext = xtPopEmptyElement then
  begin
    FNext := xtPopElement;
    FCurrNode^.FNodeType := ntEndElement;
    if FAttrCleanupFlag then
      CleanupAttributes;
    FAttrCount := 0;
    Result := True;
    Exit;
  end;
  if FNext = xtPushElement then
  begin
    if FAttrCleanupFlag then
      CleanupAttributes;
    FAttrCount := 0;
    Inc(FNesting);
    FNext := xtText;
  end
  else if FNext = xtPopElement then
    PopElement
  else if FNext = xtPushEntity then
    DoStartEntity;

  if FState <> rsRoot then
  begin
    Result := ReadTopLevel;
    Exit;
  end;

  InCDATA := (FNext = xtCDSect);
  StoreLocation(FTokenStart);
  nonWs := False;
  FValue.Length := 0;

  if FNext in [xtCDSect, xtText] then
  repeat
    wc := FSource.SkipUntil(FValue, TextDelims[InCDATA], @nonWs);
    if wc = '<' then
    begin
      Inc(FSource.FBuf);
      if FSource.FBufEnd < FSource.FBuf + 2 then
        FSource.Reload;
      if FSource.FBuf^ = '/' then
        tok := xtEndElement
      else if CheckName([cnOptional]) then
        tok := xtElement
      else if FSource.FBuf^ = '!' then
      begin
        Inc(FSource.FBuf);
        if FSource.FBuf^ = '[' then
        begin
          ExpectString('[CDATA[');
          StoreLocation(FTokenStart);
          InCDATA := True;
          if FCDSectionsAsText or (FValue.Length = 0) then
            Continue;
          tok := xtCDSect;
        end
        else if FSource.FBuf^ = '-' then
        begin
        { Ignoring comments is tricky in validating mode; discarding a comment which
          is the only child of an EMPTY element will make that element erroneously appear
          as valid. Therefore, at this point we discard only comments which are preceded
          by some text (since presence of text already renders an EMPTY element invalid).
          Other comments should be reported to validation part and discarded there. }
          if FIgnoreComments and (FValue.Length > 0) then
          begin
            ParseComment(True);
            Continue;
          end;
          tok := xtComment;
        end
        else
          tok := xtDoctype;
      end
      else if FSource.FBuf^ = '?' then
        tok := xtPI
      else
        RaiseNameNotFound;
    end
    else if wc = #0 then
    begin
      if InCDATA then
        FatalError('Unterminated CDATA section', -1);
      if FNesting > FSource.FStartNesting then
        FatalError('End-tag is missing for ''%s''', [FNodeStack[FNesting-1].FQName^.Key]);

      if Assigned(FSource.FParent) then
      begin
        if FExpandEntities and ContextPop then
          Continue
        else
          tok := xtEntityEnd;
      end
      else
        tok := xtEOF;
    end
    else if wc = '>' then
    begin
      BufAppend(FValue, wc);
      FSource.NextChar;

      if (FValue.Length <= 2) or (FValue.Buffer[FValue.Length-2] <> ']') or
        (FValue.Buffer[FValue.Length-3] <> ']') then Continue;

      if InCData then   // got a ']]>' separator
      begin
        Dec(FValue.Length, 3);
        InCDATA := False;
        if FCDSectionsAsText then
          Continue;
        SetNodeInfoWithValue(ntCDATA);
        FNext := xtText;
        Result := True;
        Exit;
      end
      else
        FatalError('Literal '']]>'' is not allowed in text', 3);
    end
    else if wc = '&' then
    begin
      if FValidators[FValidatorNesting].FContentType = ctEmpty then
        ValidationError('References are illegal in EMPTY elements', []);

      if ParseRef(FValue) or ResolvePredefined then
      begin
        nonWs := True; // CharRef to whitespace is not considered whitespace
        Continue;
      end
      else
      begin
        FCurrEntity := EntityCheck;
        if Assigned(FCurrEntity) and FExpandEntities then
        begin
          ContextPush(FCurrEntity);
          Continue;
        end;
        tok := xtEntity;
      end;
    end;
    if FValue.Length <> 0 then
    begin
      SetNodeInfoWithValue(textNodeTypes[nonWs]);
      FNext := tok;
      Result := True;
      Exit;
    end;
    Break;
  until False
  else   // not (FNext in [xtText, xtCDSect])
    tok := FNext;

  FNext := xtText;

  case tok of
    xtEntity:     HandleEntityStart;
    xtEntityEnd:  HandleEntityEnd;
    xtElement:    ParseStartTag;
    xtEndElement: ParseEndTag;
    xtPI:         ParsePI;
    xtDoctype:    ParseDoctypeDecl;
    xtComment:    ParseComment(False);
    xtEOF:        SetEofState;
  end;
  Result := tok <> xtEOF;
end;

procedure TXMLCharSource.NextChar;
begin
  Inc(FBuf);
  if FBuf >= FBufEnd then
    Reload;
end;

procedure TXMLTextReader.ExpectChar(wc: WideChar);
begin
  if FSource.FBuf^ = wc then
    FSource.NextChar
  else
    FatalError(wc);
end;

// Element name already in FNameBuffer
procedure TXMLTextReader.ParseStartTag;    // [39] [40] [44]
var
  ElDef: TElementDecl;
  IsEmpty: Boolean;
  ElName: PHashItem;
  b: TBinding;
begin
  // we're about to process a new set of attributes
  Inc(FAttrTag);

  // Get hash entry for element name
  ElName := FNameTable.FindOrAdd(FName.Buffer, FName.Length);
  // Find declaration for this element
  ElDef := TElementDecl(ElName^.Data);

  IsEmpty := False;
  FAttrCount := 0;
  FPrefixedAttrs := 0;
  FSpecifiedAttrs := 0;

  FCurrNode := AllocNodeData(FNesting);
  FCurrNode^.FQName := ElName;
  FCurrNode^.FNodeType := ntElement;
  FCurrNode^.FColonPos := FColonPos;
  StoreLocation(FCurrNode^.FLoc);
  Dec(FCurrNode^.FLoc.LinePos, FName.Length);

  if FNamespaces then
  begin
    FNSHelper.StartElement;
    if FColonPos > 0 then
      FCurrNode^.FPrefix := FNSHelper.GetPrefix(FName.Buffer, FColonPos);
  end;

  while (FSource.FBuf^ <> '>') and (FSource.FBuf^ <> '/') do
  begin
    SkipS(True);
    if (FSource.FBuf^ = '>') or (FSource.FBuf^ = '/') then
      Break;
    ParseAttribute(ElDef);
  end;

  if FSource.FBuf^ = '/' then
  begin
    IsEmpty := True;
    FSource.NextChar;
  end;
  ExpectChar('>');

  if Assigned(ElDef) and ElDef.NeedsDefaultPass then
    ProcessDefaultAttributes(ElDef);

  // Adding attributes might have reallocated FNodeStack, so restore FCurrNode once again
  FCurrNode := @FNodeStack[FNesting];

  if FNamespaces then
  begin
    { Assign namespace URIs to prefixed attrs }
    if FPrefixedAttrs <> 0 then
      ProcessNamespaceAtts;
    { Expand the element name }
    if Assigned(FCurrNode^.FPrefix) then
    begin
      b := TBinding(FCurrNode^.FPrefix^.Data);
      if not (Assigned(b) and (b.uri <> '')) then
        DoErrorPos(esFatal, 'Unbound element name prefix "%s"', [FCurrNode^.FPrefix^.Key],FCurrNode^.FLoc);
      FCurrNode^.FNsUri := FNameTable.FindOrAdd(b.uri);
    end
    else
    begin
      b := FNSHelper.DefaultNSBinding;
      if Assigned(b) then
        FCurrNode^.FNsUri := FNameTable.FindOrAdd(b.uri);
    end;
  end;

  if not IsEmpty then
  begin
    if not FPreserveWhitespace then   // critical for testsuite compliance
      SkipS;
    FNext := xtPushElement;
  end
  else
    FNext := xtPopEmptyElement;
end;

procedure TXMLTextReader.ParseEndTag;     // [42]
var
  ElName: PHashItem;
begin
  if FNesting <= FSource.FStartNesting then
    FatalError('End-tag is not allowed here');
  if FNesting > 0 then Dec(FNesting);
  Inc(FSource.FBuf);

  FCurrNode := @FNodeStack[FNesting];  // move off the possible child
  FCurrNode^.FNodeType := ntEndElement;
  ElName := FCurrNode^.FQName;

  CheckName;
  if not BufEquals(FName, ElName^.Key) then
    FatalError('Unmatching element end tag (expected "</%s>")', [ElName^.Key], FName.Length);
  if FSource.FBuf^ = '>' then    // this handles majority of cases
    FSource.NextChar
  else
  begin
    SkipS;
    ExpectChar('>');
  end;
  Inc(FTokenStart.LinePos, 2);   // move over '</' chars
  FNext := xtPopElement;
end;

procedure TXMLTextReader.ParseAttribute(ElDef: TElementDecl);
var
  attrName: PHashItem;
  attrData: PNodeData;
  AttDef: TAttributeDef;
  i: Integer;
begin
  CheckName;
  attrName := FNameTable.FindOrAdd(FName.Buffer, FName.Length);
  attrData := AllocAttributeData;
  attrData^.FQName := attrName;
  attrData^.FColonPos := FColonPos;
  StoreLocation(attrData^.FLoc);
  Dec(attrData^.FLoc.LinePos, FName.Length);
  FSpecifiedAttrs := FAttrCount;

  if Assigned(ElDef) then
  begin
    AttDef := ElDef.GetAttrDef(attrName);
    if Assigned(AttDef) then
      AttDef.Tag := FAttrTag;  // indicates that this one is specified
  end
  else
    AttDef := nil;

  attrData^.FTypeInfo := AttDef;
  // check for duplicates
  for i := 1 to FAttrCount-1 do
    if FNodeStack[FNesting+i].FQName = attrName then
      FatalError('Duplicate attribute', FName.Length);

  if FNamespaces then
  begin
    if ((FName.Length = 5) or (FColonPos = 5)) and
      (FName.Buffer[0] = 'x') and (FName.Buffer[1] = 'm') and
      (FName.Buffer[2] = 'l') and (FName.Buffer[3] = 'n') and
      (FName.Buffer[4] = 's') then
    begin
      if FColonPos > 0 then
        attrData^.FPrefix := FStdPrefix_xmlns;
      attrData^.FNsUri := FStdUri_xmlns;
    end
    else if FColonPos > 0 then
    begin
      attrData^.FPrefix := FNSHelper.GetPrefix(FName.Buffer, FColonPos);
      Inc(FPrefixedAttrs);
    end;
  end;

  ExpectEq;
  ExpectAttValue(attrData, Assigned(AttDef) and (AttDef.DataType <> dtCDATA));
  attrData^.FLoc2 := FTokenStart;

  if Assigned(attrData^.FNsUri) then
  begin
    if (not AddBinding(attrData)) and FCanonical then
    begin
      CleanupAttribute(attrData);
      Dec(FAttrCount);
      Dec(FSpecifiedAttrs);
    end;
  end;
end;

procedure TXMLTextReader.AddForwardRef(Buf: PWideChar; Length: Integer);
var
  w: PForwardRef;
begin
  New(w);
  SetString(w^.Value, Buf, Length);
  w^.Loc := FTokenStart;
  FForwardRefs.Add(w);
end;

procedure TXMLTextReader.ClearForwardRefs;
var
  I: Integer;
begin
  for I := 0 to FForwardRefs.Count-1 do
    Dispose(PForwardRef(FForwardRefs.List^[I]));
  FForwardRefs.Clear;
end;

procedure TXMLTextReader.ValidateIdRefs;
var
  I: Integer;
begin
  for I := 0 to FForwardRefs.Count-1 do
    with PForwardRef(FForwardRefs.List^[I])^ do
      if (FIDMap = nil) or (FIDMap.Find(PWideChar(Value), Length(Value)) = nil) then
        DoErrorPos(esError, 'The ID ''%s'' does not match any element', [Value], Loc);
  ClearForwardRefs;
end;

procedure TXMLTextReader.ProcessDefaultAttributes(ElDef: TElementDecl);
var
  I: Integer;
  AttDef: TAttributeDef;
  attrData: PNodeData;
begin
  for I := 0 to ElDef.AttrDefCount-1 do
  begin
    AttDef := ElDef.AttrDefs[I];

    if AttDef.Tag <> FAttrTag then  // this one wasn't specified
    begin
      case AttDef.Default of
        adDefault, adFixed: begin
          attrData := AllocAttributeData;
          attrData^ := AttDef.Data^;
          if FCanonical then
            attrData^.FIsDefault := False;

          if FNamespaces then
          begin
            if AttDef.IsNamespaceDecl then
            begin
              if attrData^.FColonPos > 0 then
                attrData^.FPrefix := FStdPrefix_xmlns;
              attrData^.FNsUri := FStdUri_xmlns;
              if (not AddBinding(attrData)) and FCanonical then
                Dec(FAttrCount);
            end
            else if attrData^.FColonPos > 0 then
            begin
              attrData^.FPrefix := FNSHelper.GetPrefix(PWideChar(attrData^.FQName^.Key), attrData^.FColonPos);
              Inc(FPrefixedAttrs);
            end;
          end;
        end;
        adRequired:
          ValidationError('Required attribute ''%s'' of element ''%s'' is missing',
            [AttDef.Data^.FQName^.Key, FNodeStack[FNesting].FQName^.Key], 0)
      end;
    end;
  end;
end;


function TXMLTextReader.AddBinding(attrData: PNodeData): Boolean;
var
  nsUri, Pfx: PHashItem;
begin
  nsUri := FNameTable.FindOrAdd(attrData^.FValueStr);
  if attrData^.FColonPos > 0 then
    Pfx := FNSHelper.GetPrefix(@attrData^.FQName^.key[7], Length(attrData^.FQName^.key)-6)
  else
    Pfx := FNSHelper.GetPrefix(nil, 0);  { will return the default prefix }
  { 'xml' is allowed to be bound to the correct namespace }
  if ((nsUri = FStduri_xml) <> (Pfx = FStdPrefix_xml)) or
   (Pfx = FStdPrefix_xmlns) or
   (nsUri = FStduri_xmlns) then
  begin
    if (Pfx = FStdPrefix_xml) or (Pfx = FStdPrefix_xmlns) then
      DoErrorPos(esFatal, 'Illegal usage of reserved prefix ''%s''', [Pfx^.Key], attrData^.FLoc)
    else
      DoErrorPos(esFatal, 'Illegal usage of reserved namespace URI ''%s''', [attrData^.FValueStr], attrData^.FLoc2);
  end;

  if (attrData^.FValueStr = '') and not (FXML11 or (Pfx^.Key = '')) then
    DoErrorPos(esFatal, 'Illegal undefining of namespace', attrData^.FLoc2);

  Result := (Pfx^.Data = nil) or (TBinding(Pfx^.Data).uri <> attrData^.FValueStr);
  if Result then
    FNSHelper.BindPrefix(attrData^.FValueStr, Pfx);
end;

procedure TXMLTextReader.ProcessNamespaceAtts;
var
  I, J: Integer;
  Pfx, AttrName: PHashItem;
  attrData: PNodeData;
  b: TBinding;
begin
  FNsAttHash.Init(FPrefixedAttrs);
  for I := 1 to FAttrCount do
  begin
    attrData := @FNodeStack[FNesting+i];
    if (attrData^.FColonPos < 1) or Assigned(attrData^.FNsUri) then
      Continue;

    Pfx := attrData^.FPrefix;
    b := TBinding(Pfx^.Data);
    if not (Assigned(b) and (b.uri <> '')) then
      DoErrorPos(esFatal, 'Unbound attribute name prefix "%s"', [Pfx^.Key], attrData^.FLoc);

    { detect duplicates }
    J := attrData^.FColonPos+1;
    AttrName := attrData^.FQName;

    if FNsAttHash.Locate(@b.uri, @AttrName^.Key[J], Length(AttrName^.Key) - J+1) then
      DoErrorPos(esFatal, 'Duplicate prefixed attribute', attrData^.FLoc);

    attrData^.FNsUri := FNameTable.FindOrAdd(b.uri);
  end;
end;

function TXMLTextReader.ParseExternalID(out SysID, PubID: XMLString;     // [75]
  SysIdOptional: Boolean): Boolean;
var
  I: Integer;
  wc: WideChar;
begin
  Result := False;
  if FSource.Matches('SYSTEM') then
    SysIdOptional := False
  else if FSource.Matches('PUBLIC') then
  begin
    ExpectWhitespace;
    ParseLiteral(FValue, ltPubid, True);
    SetString(PubID, FValue.Buffer, FValue.Length);
    for I := 1 to Length(PubID) do
    begin
      wc := PubID[I];
      if (wc > #255) or not (Char(ord(wc)) in PubidChars) then
        FatalError('Illegal Public ID literal', -1);
    end;
  end
  else
    Exit;

  if SysIdOptional then
    SkipWhitespace
  else
    ExpectWhitespace;
  if ParseLiteral(FValue, ltPlain, not SysIdOptional) then
    SetString(SysID, FValue.Buffer, FValue.Length);
  Result := True;
end;

function TXMLTextReader.ValidateAttrSyntax(AttrDef: TAttributeDef; const aValue: XMLString): Boolean;
begin
  case AttrDef.DataType of
    dtId, dtIdRef, dtEntity: Result := IsXmlName(aValue, FXML11) and
      ((not FNamespaces) or (Pos(WideChar(':'), aValue) = 0));
    dtIdRefs, dtEntities: Result := IsXmlNames(aValue, FXML11) and
      ((not FNamespaces) or (Pos(WideChar(':'), aValue) = 0));
    dtNmToken: Result := IsXmlNmToken(aValue, FXML11) and AttrDef.HasEnumToken(aValue);
    dtNmTokens: Result := IsXmlNmTokens(aValue, FXML11);
    // IsXmlName() not necessary - enum is never empty and contains valid names
    dtNotation: Result := AttrDef.HasEnumToken(aValue);
  else
    Result := True;
  end;
end;

procedure TXMLTextReader.ValidateAttrValue(AttrDef: TAttributeDef; attrData: PNodeData);
var
  L, StartPos, EndPos: Integer;
  Entity: TEntityDecl;
begin
  L := Length(attrData^.FValueStr);
  case AttrDef.DataType of
    dtId: begin
      if not AddID(attrData) then
        DoErrorPos(esError, 'The ID ''%s'' is not unique', [attrData^.FValueStr], attrData^.FLoc2);
    end;

    dtIdRef, dtIdRefs: begin
      StartPos := 1;
      while StartPos <= L do
      begin
        EndPos := StartPos;
        while (EndPos <= L) and (attrData^.FValueStr[EndPos] <> #32) do
          Inc(EndPos);
        if (FIDMap = nil) or (FIDMap.Find(@attrData^.FValueStr[StartPos], EndPos-StartPos) = nil) then
          AddForwardRef(@attrData^.FValueStr[StartPos], EndPos-StartPos);
        StartPos := EndPos + 1;
      end;
    end;

    dtEntity, dtEntities: begin
      StartPos := 1;
      while StartPos <= L do
      begin
        EndPos := StartPos;
        while (EndPos <= L) and (attrData^.FValueStr[EndPos] <> #32) do
          Inc(EndPos);
        Entity := TEntityDecl(FDocType.Entities.Get(@attrData^.FValueStr[StartPos], EndPos-StartPos));
        if (Entity = nil) or (Entity.FNotationName = '') then
          ValidationError('Attribute ''%s'' type mismatch', [attrData^.FQName^.Key], -1);
        StartPos := EndPos + 1;
      end;
    end;
  end;
end;

procedure TXMLTextReader.ValidateDTD;
var
  I: Integer;
begin
  for I := 0 to FForwardRefs.Count-1 do
    with PForwardRef(FForwardRefs[I])^ do
      if FDocType.Notations.Get(PWideChar(Value), Length(Value)) = nil then
        DoErrorPos(esError, 'Notation ''%s'' is not declared', [Value], Loc);
end;


function TXMLTextReader.DoCDSect(ch: PWideChar; Count: Integer): TDOMNode;
var
  s: XMLString;
begin
  Assert(not FCDSectionsAsText, 'Should not be called when CDSectionsAsText=True');

  SetString(s, ch, Count);
  result := doc.CreateCDATASection(s);
end;

procedure TXMLTextReader.DoNotationDecl(const aName, aPubID, aSysID: XMLString);
var
  Notation: TNotationDecl;
  Entry: PHashItem;
begin
  Entry := FDocType.Notations.FindOrAdd(aName);
  if Entry^.Data = nil then
  begin
    Notation := TNotationDecl.Create;
    Notation.FName := aName;
    Notation.FPublicID := aPubID;
    Notation.FSystemID := aSysID;
    Entry^.Data := Notation;
  end
  else
    ValidationError('Duplicate notation declaration: ''%s''', [aName]);
end;

function TXMLTextReader.AddId(aNodeData: PNodeData): Boolean;
var
  e: PHashItem;
begin
  if FIDMap = nil then
    FIDMap := THashTable.Create(256, False);
  e := FIDMap.FindOrAdd(PWideChar(aNodeData^.FValueStr), Length(aNodeData^.FValueStr), Result);
  Result := not Result;
  if Result then
    aNodeData^.FIDEntry := e;
end;

function TXMLTextReader.AllocAttributeData: PNodeData;
begin
  Result := AllocNodeData(FNesting + FAttrCount + 1);
  Result^.FNodeType := ntAttribute;
  Result^.FIsDefault := False;
  Inc(FAttrCount);
end;

function TXMLTextReader.AllocNodeData(AIndex: Integer): PNodeData;
begin
  {make sure we have an extra slot to place child text/comment/etc}
  if AIndex >= Length(FNodeStack)-1 then
    SetLength(FNodeStack, AIndex * 2 + 2);

  Result := @FNodeStack[AIndex];
  Result^.FNext := nil;
  Result^.FPrefix := nil;
  Result^.FNsUri := nil;
  Result^.FIDEntry := nil;
  Result^.FValueStart := nil;
  Result^.FValueLength := 0;
end;

procedure TXMLTextReader.AllocAttributeValueChunk(var APrev: PNodeData; Offset: Integer);
var
  chunk: PNodeData;
begin
  { when parsing DTD, don't take ownership of allocated data }
  chunk := FFreeAttrChunk;
  if Assigned(chunk) and (FState <> rsDTD) then
  begin
    FFreeAttrChunk := chunk^.FNext;
    chunk^.FNext := nil;
  end
  else { no free chunks, create a new one }
  begin
    New(chunk);
    FillChar(chunk^, sizeof(TNodeData), 0);
    if FState <> rsDTD then
      FAttrChunks.Add(chunk);
  end;
  APrev^.FNext := chunk;
  APrev := chunk;
  { assume text node, for entity refs it is overridden later }
  chunk^.FNodeType := ntText;
  chunk^.FQName := nil;
  { without PWideChar typecast and in $T-, FPC treats '@' result as PAnsiChar... }
  SetString(chunk^.FValueStr, PWideChar(@FValue.Buffer[Offset]), FValue.Length-Offset);
end;

procedure TXMLTextReader.CleanupAttributes;
var
  i: Integer;
begin
  {cleanup only specified attributes; default ones are owned by DTD}
  for i := 1 to FSpecifiedAttrs do
    CleanupAttribute(@FNodeStack[FNesting+i]);
  FAttrCleanupFlag := False;
end;

procedure TXMLTextReader.CleanupAttribute(aNode: PNodeData);
var
  chunk: PNodeData;
begin
  if Assigned(aNode^.FNext) then
  begin
    chunk := aNode^.FNext;
    while Assigned(chunk^.FNext) do
      chunk := chunk^.FNext;
    chunk^.FNext := FFreeAttrChunk;
    FFreeAttrChunk := aNode^.FNext;
    aNode^.FNext := nil;
  end;
end;

procedure TXMLTextReader.SetNodeInfoWithValue(typ: TXMLNodeType; AName: PHashItem = nil);
begin
  FCurrNode := @FNodeStack[FNesting];
  FCurrNode^.FNodeType := typ;
  FCurrNode^.FQName := AName;
  FCurrNode^.FValueStart := FValue.Buffer;
  FCurrNode^.FValueLength := FValue.Length;
end;

function TXMLTextReader.SetupFakeLF(nextstate: TXMLToken): Boolean;
begin
  FValue.Buffer[0] := #10;
  FValue.Length := 1;
  SetNodeInfoWithValue(ntWhitespace,nil);
  FNext := nextstate;
  Result := True;
end;

procedure TXMLTextReader.PushVC(aElDef: TElementDecl);
begin
  Inc(FValidatorNesting);
  if FValidatorNesting >= Length(FValidators) then
    SetLength(FValidators, FValidatorNesting * 2);

  with FValidators[FValidatorNesting] do
  begin
    FElementDef := aElDef;
    FCurCP := nil;
    FFailed := False;
    FContentType := ctAny;
    FSaViolation := False;
    if Assigned(aElDef) then
    begin
      FContentType := aElDef.ContentType;
      FSaViolation := FStandalone and aElDef.ExternallyDeclared;
    end;
  end;
end;

procedure TXMLTextReader.PopElement;
begin
  if FNamespaces then
    FNSHelper.EndElement;

  if (FNesting = 0) and (not FFragmentMode) then
    FState := rsEpilog;
  FCurrNode := @FNodeStack[FNesting];
  FNext := xtText;
end;

{ TElementValidator }

function TElementValidator.IsElementAllowed(Def: TElementDecl): Boolean;
var
  Next: TContentParticle;
begin
  Result := True;
  // if element is not declared, non-validity has been already reported, no need to report again...
  if Assigned(Def) and Assigned(FElementDef) then
  begin
    case FElementDef.ContentType of

      ctEmpty: Result := False;

      ctChildren, ctMixed: begin
        if FFailed then     // if already detected a mismatch, don't waste time
          Exit;
        if FCurCP = nil then
          Next := FElementDef.RootCP.FindFirst(Def)
        else
          Next := FCurCP.FindNext(Def, 0); { second arg ignored here }
        Result := Assigned(Next);
        if Result then
          FCurCP := Next
        else
          FFailed := True;  // used to prevent extra error at the end of element
      end;
      // ctAny, ctUndeclared: returns True by default
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

{ plain calls }

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: Text);
var
  Reader: TXMLTextReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Src := TXMLFileInputSource.Create(f);
  Reader := TXMLTextReader.Create;
  try
    Reader.ProcessXML(Src);
  finally
    ADoc := TXMLDocument(Reader.Doc);
    Reader.Free;
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream; const ABaseURI: String);
var
  Reader: TXMLTextReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLTextReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessXML(Src);
  finally
    ADoc := TXMLDocument(Reader.doc);
    Reader.Free;
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream);
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
  Reader: TXMLTextReader;
  Src: TXMLCharSource;
begin
  Reader := TXMLTextReader.Create;
  try
    Src := TXMLFileInputSource.Create(f);
    Reader.ProcessFragment(Src, AParentNode);
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; f: TStream; const ABaseURI: String);
var
  Reader: TXMLTextReader;
  Src: TXMLCharSource;
begin
  Reader := TXMLTextReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessFragment(Src, AParentNode);
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; f: TStream);
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
  Reader: TXMLTextReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLTextReader.Create;
  try
    Src := TXMLFileInputSource.Create(f);
    Reader.ProcessDTD(Src);
  finally
    ADoc := TXMLDocument(Reader.doc);
    Reader.Free;
  end;
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; f: TStream; const ABaseURI: String);
var
  Reader: TXMLTextReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLTextReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessDTD(Src);
  finally
    ADoc := TXMLDocument(Reader.doc);
    Reader.Free;
  end;
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; f: TStream);
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
