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

{off $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  SysUtils, Classes, DOM;

type
  EXMLReadError = class(Exception);


procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: File); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream; const AFilename: String); overload;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: File); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const AFilename: String); overload;

procedure ReadDTDFile(out ADoc: TXMLDocument; const AFilename: String);  overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: File); overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream; const AFilename: String); overload;


// =======================================================

implementation

type
  TSetOfChar = set of Char;

const
  Letter = ['A'..'Z', 'a'..'z'];
  Digit = ['0'..'9'];
  PubidChars: TSetOfChar = [' ', #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
    '#', '@', '$', '_', '%'];

  NmToken: TSetOfChar = Letter + Digit + ['.', '-', '_', ':'];

type
  TXMLReaderDocumentType = class(TDOMDocumentType);

  TXMLReader = class;

  TCharSource = class
  private
    Buf: PChar;
    FReader: TXMLReader;
  public
    constructor Create(AReader: TXMLReader; ABuffer: PChar);
    function NextChar: WideChar; virtual; abstract;
  end;

  TUCS2CharSource = class(TCharSource)
  private
    FSwapEndian: Boolean;
  public
    function NextChar: WideChar; override;
  end;

  TUTF8CharSource = class(TCharSource)
  private
    procedure BadChar;
  public
    function NextChar: WideChar; override;
  end;

  TISO_8859_1CharSource = class(TCharSource)
  public
    function NextChar: WideChar; override;
  end;  

  TXMLReader = class
  private
    FSource: TCharSource;
    FCurChar: WideChar;
    FLine: Integer;                  // <- To Locator
    FColumn: Integer;                // <- To Locator
    FSeenCR: Boolean;
    FWhitespace: Boolean;
    FValue: array of WideChar;
    FValueLength: Integer;
    FName: array of WideChar;
    FNameLength: Integer;
    FInternalSubset: Boolean;
    FPrologParsed: Boolean;
    procedure RaiseExpectedQmark;
    procedure GetChar;
    procedure AppendValue(wc: WideChar);
    procedure AppendName(wc: WideChar);
    procedure DetectEncoding;
  protected
    buf: PChar;                      // <- To InputSource
    Filename: String;                // <- To InputSource
    FCursor: TDOMNode;

    procedure RaiseExc(const descr: String); overload;
    procedure RaiseExc(Expected, Found: WideChar); overload;
    function  SkipWhitespace: Boolean;
    procedure ExpectWhitespace;
    procedure ExpectString(const s: String);
    procedure ExpectChar(wc: WideChar);
    function  CheckForChar(c: WideChar): Boolean;
    procedure SkipString(const ValidChars: TSetOfChar);
    function  GetString(const ValidChars: TSetOfChar): WideString;

    procedure RaiseNameNotFound;
    function  CheckName: Boolean;
    function  ExpectName: WideString;                                   // [5]
    procedure SkipName;
    procedure ExpectAttValue;                                           // [10]
    procedure SkipPubidLiteral;                                         // [12]
    procedure ParseComment;                                             // [15]
    procedure ParsePI;                                                  // [16]
    procedure ExpectProlog;                                             // [22]
    function ParseInternalDtd: Boolean;
    procedure ParseProlog;
    function  ParseEq: Boolean;                                         // [25]
    procedure ExpectEq;
    procedure ParseMisc;                                                // [27]
    function  ParseMarkupDecl(InternalSubset: Boolean): Boolean;        // [29]
    procedure ParseCDSect;                                              // [18]
    function ParseElementContent: Boolean;
    procedure ParseElement;                                             // [39]
    procedure ExpectElement;
    function ResolvePredefined(const RefName: WideString): Boolean;
    function ParseReference: TDOMEntityReference;                       // [67]
    function  ParsePEReference: Boolean;                                // [69]
    function  ParseExternalID(InNotation: Boolean): Boolean;            // [75]
    procedure ExpectExternalID;
    procedure ProcessTextAndRefs(Delim: WideChar; DiscardWS: Boolean);

    procedure ParseEntityDecl;
    procedure ParseAttlistDecl;
    procedure ParseElementDecl;
    procedure ParseNotationDecl;

    procedure ResolveEntities(RootNode: TDOMNode);
  public
    doc: TDOMDocument;
    destructor Destroy; override;
    procedure ProcessXML(ABuf: PChar; const AFilename: String);  // [1]
    procedure ProcessFragment(AOwner: TDOMNode; ABuf: PChar; const AFilename: String);
    procedure ProcessDTD(ABuf: PChar; const AFilename: String);  // ([29])
  end;

{$i names.inc}

// TODO: These CharSource classes still cannot be considered as the final solution...
{ TCharSource }

constructor TCharSource.Create(AReader: TXMLReader; ABuffer: PChar);
begin
  inherited Create;
  FReader := AReader;
  Buf := ABuffer;
end;

{ TUCS2CharSource }

function TUCS2CharSource.NextChar: WideChar;
begin
  Result := PWideChar(buf)^;
  Inc(buf, sizeof(WideChar));
  if FSwapEndian then
    Result := WideChar(Swap(Word(Result)));
end;

{ TUTF8CharSource }

procedure TUTF8CharSource.BadChar;
begin
  FReader.RaiseExc('Invalid character in UTF8 sequence');
end;

function TUTF8CharSource.NextChar: WideChar;
var
  ch2, ch3: Byte;
begin
  Result := WideChar(buf[0]);
  Inc(buf);
  if Result < #128 then                         { ASCII }
    Exit
  else if (Byte(Result) and $E0) = $C0 then     { #$0080 - #$07FF }
  begin
    ch2 := ord(buf[0]); Inc(Buf);
    if (Ch2 and $C0) <> $80 then
      BadChar;
    Result := WideChar((Byte(Result) and $1F) shl 6 + (Ch2 and $3F));
  end
  else if (Byte(Result) and $F0) = $E0 then     { #$0800 - #$FFFF }
  begin
    ch2 := ord(buf[0]); Inc(buf);
    if (Ch2 and $C0) <> $80 then
      BadChar;
    ch3 := ord(buf[0]); Inc(buf);
    if (Ch3 and $C0) <> $80 then
      BadChar;
    Result := WideChar(Word((Byte(Result) and $0F) shl 12) +
      (Ch2 and $3F) shl 6 + (Ch3 and $3F));
  end
  else { if (Byte(Result) and $F8) = $F0) then }      // and $FC = $F8
                                                      // and $FE = $FC
    FReader.RaiseExc('Unsupported UTF8 character');
end;

{ TISO8859_1CharSource }

function TISO_8859_1CharSource.NextChar: WideChar;
begin
  Result := WideChar(buf[0]); Inc(Buf);
end;

{ TXMLReader }

procedure TXMLReader.DetectEncoding;
var
  b: Char;
begin
  b := buf[0];
  if (b = #$FE) and (buf[1] = #$FF) then
  begin
    Inc(buf, 2);
    FSource := TUCS2CharSource.Create(Self, buf);
    {$IFNDEF ENDIAN_BIG}
    TUCS2CharSource(FSource).FSwapEndian := True;
    {$ENDIF}
  end
  else if (b = #$FF) and (buf[1] = #$FE) then
  begin
    Inc(buf, 2);
    FSource := TUCS2CharSource.Create(Self, buf);
    {$IFDEF ENDIAN_BIG}
    TUCS2CharSource(FSource).FSwapEndian := True;
    {$ENDIF}
  end
  else
    FSource := TUTF8CharSource.Create(Self, Buf);

  GetChar;
  if FCurChar = #$FEFF then  // skip BOM, if one is present
    GetChar;
end;

procedure TXMLReader.GetChar;
begin
  FCurChar := FSource.NextChar;
  if FSeenCR then
  begin
    case FCurChar of
      #10, #$85: FCurChar := FSource.NextChar; // #$85 is xml 1.1 specific
    end;
    FSeenCR := False;
  end;
  FWhitespace := False;
  case FCurChar of
    #9, #10, #32: FWhitespace := True;
    #13: begin
           FSeenCR := True;
           FCurChar := #10;
           FWhitespace := True;
         end;
    #$85, #$2028:  // xml 1.1 specific
      FCurChar := #10;

    #1..#8, #11, #12, #14..#31,  // never allowed... btw, #0 is also forbidden
    #$D800..#$DFFF,              // surrogates - should be supported some way
    #$FFFE..#$FFFF:              // never allowed
      RaiseExc('Invalid character');
  end;

  if FCurChar = #10 then
  begin
    Inc(FLine);
    FColumn := 0;
  end
  else
    Inc(FColumn);
end;

procedure TXMLReader.AppendValue(wc: WideChar);
var
  Alloc: Integer;
begin
  Alloc := Length(FValue);
  if FValueLength >= Alloc then
  begin
    if Alloc = 0 then
      Alloc := 512
    else
      Alloc := Alloc * 2;
    SetLength(FValue, Alloc);
  end;
  FValue[FValueLength] := wc;
  Inc(FValueLength);
end;

procedure TXMLReader.AppendName(wc: WideChar);
var
  Alloc: Integer;
begin
  Alloc := Length(FName);
  if FNameLength >= Alloc then
  begin
    if Alloc = 0 then
      Alloc := 128
    else
      Alloc := Alloc * 2;
    SetLength(FName, Alloc);
  end;
  FName[FNameLength] := wc;
  Inc(FNameLength);
end;


procedure TXMLReader.RaiseExpectedQmark;
begin
  RaiseExc('Expected single or double quotation mark');
end;

procedure TXMLReader.RaiseExc(Expected, Found: WideChar);
begin
  RaiseExc('Expected "' + Expected + '", but found "' +  Found + '"');
end;

procedure TXMLReader.RaiseExc(const descr: String);
begin
  raise EXMLReadError.CreateFmt('In %s (line %d pos %d): %s', [Filename, FLine, FColumn, descr]);
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
  if not CheckForChar(wc) then
    RaiseExc(wc, FCurChar);
end;

procedure TXMLReader.ExpectString(const s: String);

  procedure RaiseStringNotFound;
  begin
    RaiseExc('Expected "' + s + '"');
  end;

var
  I: Integer;
begin
  for I := 1 to Length(s) do
  begin
    if FCurChar <> WideChar(s[i]) then
      RaiseStringNotFound;
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
  FValueLength := 0;
  while (ord(FCurChar) < 256) and (char(FCurChar) in ValidChars) do
  begin
    AppendValue(FCurChar);
    GetChar;
  end;
end;

function TXMLReader.GetString(const ValidChars: TSetOfChar): WideString;
begin
  SkipString(ValidChars);
  SetString(Result, PWideChar(@FValue[0]), FValueLength);
end;


destructor TXMLReader.Destroy;
begin
  FSource.Free;
  inherited Destroy;
end;

procedure TXMLReader.ProcessXML(ABuf: PChar; const AFilename: String);    // [1]
begin
  buf := ABuf;
  Filename := AFilename;
  FLine := 1;
  FColumn := 0;

  doc := TXMLDocument.Create;
  FCursor := doc;
  DetectEncoding;
  ExpectProlog;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ProcessXML A');{$ENDIF}
  ExpectElement;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ProcessXML B');{$ENDIF}
  ParseMisc;

  if FCurChar <> #0 then
    RaiseExc('Text after end of document element found');
end;

procedure TXMLReader.ProcessFragment(AOwner: TDOMNode; ABuf: PChar; const AFilename: String);
begin
  buf := ABuf;
  Filename := AFilename;
  FLine := 1;
  FColumn := 0;
  FCursor := AOwner;
  DetectEncoding;

  if not ParseElementContent then
    ;
end;

function TXMLReader.CheckName: Boolean;        // [5]
begin
  Result := (Byte(FCurChar) in NamingBitmap[namePages[hi(Word(FCurChar))]]);
  if Result then
  begin
    FNameLength := 0;
    repeat
      AppendName(FCurChar);
      GetChar;
    until not (Byte(FCurChar) in NamingBitmap[namePages[$100 + hi(Word(FCurChar))]]);
  end;
end;

procedure TXMLReader.RaiseNameNotFound;
begin
  RaiseExc('Name starts with invalid character');
end;

function TXMLReader.ExpectName: WideString;    // [5]
begin
  if not CheckName then
    RaiseNameNotFound;

  SetString(Result, PWideChar(@FName[0]), FNameLength);
end;

procedure TXMLReader.SkipName;
begin
  if not CheckName then
    RaiseNameNotFound;
end;

// ---------------------

function TXMLReader.ResolvePredefined(const RefName: WideString): Boolean;
begin
  Result := True;
  if RefName = 'amp' then
    AppendValue('&')
  else if RefName = 'apos' then
    AppendValue('''')
  else if RefName = 'gt' then
    AppendValue('>')
  else if RefName = 'lt' then
    AppendValue('<')
  else if RefName = 'quot' then
    AppendValue('"')
  else
    Result := False;
end;

function TXMLReader.ParseReference: TDOMEntityReference;
var
  RefName: WideString;
  Radix, Value: Integer;
begin
  Result := nil;
  if CheckForChar('#') then  // character reference [66]
  begin
    if CheckForChar('x') then
      Radix := 16
    else
      Radix := 10;
    Value := 0;
    repeat
      case FCurChar of
        '0'..'9': Value := Value * Radix + Ord(FCurChar) - Ord('0');
        'a'..'f': if Radix = 16 then Value := Value * 16 + Ord(FCurChar) - Ord('a') + 10 else Break;
        'A'..'F': if Radix = 16 then Value := Value * 16 + Ord(FCurChar) - Ord('A') + 10 else Break;
      else
        Break;
      end;
      GetChar;
    until False;
    
    case Value of
      // TODO: in XML1.1, references to $01..$1F are VALID
      $09, $0A, $0D, $20..$D7FF, $E000..$FFFD:
        AppendValue(WideChar(Value));
      $10000..$10FFFF:
        begin
          AppendValue(WideChar($D7C0 + (Value shr 10)));
          AppendValue(WideChar($DC00 xor (Value and $3FF)));
        end;
    else
      RaiseExc('Invalid character reference');
    end;
  end
  else
  begin
    RefName := ExpectName;
    if not ResolvePredefined(RefName) then
    begin
      // TODO: try resolve the entity here
      Result := doc.CreateEntityReference(RefName);
    end;
  end;
  ExpectChar(';');       // reference terminator
end;

procedure TXMLReader.ProcessTextAndRefs(Delim: WideChar; DiscardWS: Boolean);
var
  nonWs: Boolean;
  RefNode: TDOMEntityReference;
begin
  FValueLength := 0;
  nonWs := False;
  while (FCurChar <> Delim) and (FCurChar <> #0) and (FCurChar <> '<') do
  begin
    if not FWhitespace then
      nonWs := True;
    if FCurChar <> '&' then
    begin
      AppendValue(FCurChar);
      if (FValueLength >= 3) and (FValue[FValueLength-1] = '>') and
        (FValue[FValueLength-2] = ']') and (FValue[FValueLength-3] = ']') then
           RaiseExc('Literal '']]>'' is not allowed in text');
      GetChar;
    end
    else
    begin
      GetChar; // skip '&'
      RefNode := ParseReference;
      if Assigned(RefNode) then
      begin
        if FValueLength > 0 then
        begin
          if (not DiscardWs) or nonWs then
            FCursor.AppendChild(doc.CreateTextNodeBuf(@FValue[0], FValueLength));
          FValueLength := 0;
          nonWs := False;
        end;
        FCursor.AppendChild(RefNode);
      end;
    end;
  end; // while
  if ((not DiscardWs) or nonWs) and (FValueLength > 0) then
  begin
    FCursor.AppendChild(doc.CreateTextNodeBuf(@FValue[0], FValueLength));
    FValueLength := 0;
  end;
end;

procedure TXMLReader.ExpectAttValue;    // [10]
var
  Delim: WideChar;
begin
  if (FCurChar <> '''') and (FCurChar <> '"') then
    RaiseExpectedQmark;
  Delim := FCurChar;
  GetChar;  // skip quote

  ProcessTextAndRefs(Delim, False);
  if FCurChar = '<' then
    RaiseExc('"<" is not allowed in attribute value');

  GetChar;  // skip trailing quote
end;

procedure TXMLReader.SkipPubidLiteral;                 // [12]
var
  Delim: WideChar;
begin
  if (FCurChar = '''') or (FCurChar = '"') then
  begin
    Delim := FCurChar;
    GetChar;  // skip quote
    SkipString(PubidChars - [Char(Delim)]);  // <-- PubidChars do not contain `"`
    ExpectChar(Delim);
  end
  else
    RaiseExpectedQMark;
end;

// starting '<!' already consumed, FCurChar = '-'
procedure TXMLReader.ParseComment;    // [15]
begin
  ExpectString('--');
  FValueLength := 0;
  repeat
    AppendValue(FCurChar);
    GetChar;
    if (FValueLength >= 2) and (FValue[FValueLength-1] = '-') and
      (FValue[FValueLength-2] = '-') then
      begin
        Dec(FValueLength, 2);
        Break;
      end;
  until FCurChar = #0;  // should not happen

  if FCurChar = #0 then
    RaiseExc('Unterminated comment');
  ExpectChar('>');

  FCursor.AppendChild(doc.CreateCommentBuf(@FValue[0], FValueLength));
end;

// starting '<?' already consumed
procedure TXMLReader.ParsePI;                    // [16]
var
  Name, Value: WideString;
begin
  Name := ExpectName;

  if (FNameLength = 3) and
     ((FName[0] = 'X') or (FName[0] = 'x')) and
     ((FName[1] = 'M') or (FName[1] = 'm')) and
     ((FName[2] = 'L') or (FName[2] = 'l')) then
  begin
    if Name <> 'xml' then        // FIX: ibm23n04.xml
      RaiseExc('"xml" reserved word must be lowercase');
    if not FPrologParsed then
    begin
      ParseProlog;
      FPrologParsed := True;
      Exit;
    end
    else
      RaiseExc('"<?xml" processing instruction not allowed here');
  end;

  if FCurChar <> '?' then
    ExpectWhitespace;

  FValueLength := 0;
  repeat
    AppendValue(FCurChar);
    GetChar;
    if (FValueLength >= 2) and (FValue[FValueLength-1] = '>') and
      (FValue[FValueLength-2] = '?') then
      begin
        Dec(FValueLength, 2);
        Break;
      end;
  until FCurChar = #0;  // should not happen

  if FCurChar = #0 then
    RaiseExc('Unterminated processing instruction');

  SetString(Value, PWideChar(@FValue[0]), FValueLength);
  FCursor.AppendChild(Doc.CreateProcessingInstruction(Name, Value));
end;

// here we come from ParsePI, 'xml' is already consumed
procedure TXMLReader.ParseProlog;
var
  Delim: WideChar;
  svalue: WideString;
begin
  // '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
  // VersionInfo: S 'version' Eq (' VersionNum ' | " VersionNum ")
  SkipWhitespace;
  ExpectString('version');
  ExpectEq;
  if (FCurChar = '''') or (FCurChar = '"') then
  begin
    Delim := FCurChar;
    GetChar; // skip quote
    if doc.InheritsFrom(TXMLDocument) then
      TXMLDocument(doc).XMLVersion := GetString(NmToken);
    ExpectChar(Delim);
    if FCurChar <> '?' then
      ExpectWhitespace;
  end
  else
    RaiseExpectedQMark;

  if FCurChar = 'e' then                        // [80]
  begin
    ExpectString('encoding');
    ExpectEq;
    if (FCurChar = '''') or (FCurChar = '"') then
    begin
      Delim := FCurChar;
      GetChar; // skip quote
      if not ((ord(FCurChar) < 256) and (char(FCurChar) in ['A'..'Z', 'a'..'z'])) then
        RaiseExc('Expected character (A-Z, a-z)');
      SkipString(['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']);
      // TODO: analyze encoding string, and adjust FSource if needed and possible
      ExpectChar(Delim);
      if FCurChar <> '?' then
        ExpectWhitespace;
    end
    else
      RaiseExpectedQMark;
  end;

  // SDDecl?
  if FCurChar = 's' then
  begin
    ExpectString('standalone');
    ExpectEq;
    if (FCurChar = '''') or (FCurChar = '"') then
    begin
      Delim := FCurChar;
      GetChar; // skip quote
      svalue := ExpectName;
      if (svalue <> 'yes') and (svalue <> 'no') then
        RaiseExc('Standalone attribute may only have value "yes" or "no"');
      ExpectChar(Delim);
    end
    else
      RaiseExpectedQMark;
    SkipWhitespace;
  end;

  ExpectString('?>');
end;

function TXMLReader.ParseInternalDtd: Boolean;
var
  DocType: TXMLReaderDocumentType;
begin
  // Check for "(doctypedecl Misc*)?"    [28]
  Result := (FCurChar = 'D');
  if Result then
  begin
    FPrologParsed := True;
    ExpectString('DOCTYPE');
    // create the DTD object
    DocType := TXMLReaderDocumentType.Create(doc as TXMLDocument);
    if doc.InheritsFrom(TXMLDocument) then
      TXMLDocument(doc).AppendChild(DocType);
    SkipWhitespace;
    DocType.FName := ExpectName;
    SkipWhitespace;
    ParseExternalID(False);    // may be absent, ignore result
    SkipWhitespace;

    if CheckForChar('[') then
    begin
      repeat
        SkipWhitespace;
      until not (ParseMarkupDecl(True) or ParsePEReference);
      ExpectChar(']');
      SkipWhitespace;
    end;
    ExpectChar('>');
    ParseMisc;
    Exit;
  end;
end;

procedure TXMLReader.ExpectProlog;    // [22]
begin
  FPrologParsed := False;
  // Check for "Misc*".
  // ParseMisc() is inlined here and slightly modified
  // because we need to distinguish '<DOC...' from '<!DOC...'
  repeat
    SkipWhitespace;
    if not CheckForChar('<') then
      Break;
    if CheckForChar('!') then
    begin
      if FCurChar = '-' then
        ParseComment
      else
        if ParseInternalDtd then
          Exit;
    end
    else
      if CheckForChar('?') then
        ParsePI
      else
        Break;
  until False;
end;

function TXMLReader.ParseEq: Boolean;    // [25]
begin
  SkipWhitespace;
  Result := CheckForChar('=');
  if Result then
    SkipWhitespace;
end;

procedure TXMLReader.ExpectEq;
begin
  if not ParseEq then
    RaiseExc('Expected "="');
end;


// Parse "Misc*":
//   Misc ::= Comment | PI | S

procedure TXMLReader.ParseMisc;      // [27]
begin
  repeat
    SkipWhitespace;
    if not CheckForChar('<') then
      Break;
    if CheckForChar('!') then
    begin
      if FCurChar = '-' then
        ParseComment
      else
        RaiseExc('Document type declarations not allowed here');
    end
    else
      if CheckForChar('?') then
        ParsePI
      else
        Break;
  until False;
end;

{ DTD stuff }

procedure TXMLReader.ParseElementDecl;            // [45]

  procedure ExpectChoiceOrSeq;                    // [49], [50]

    procedure ExpectCP;                           // [48]
    begin
      if CheckForChar('(') then
        ExpectChoiceOrSeq
      else
        SkipName;
      if CheckForChar('?') then
      else if CheckForChar('*') then
      else if CheckForChar('+') then;
    end;

  var
    Delim: WideChar;
  begin
    SkipWhitespace;
    ExpectCP;
    Delim := #0;
    repeat
      SkipWhitespace;
      if (FCurChar = #0) or CheckForChar(')') then
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
          RaiseExc(Delim, FCurChar);
      GetChar; // skip delimiter
      SkipWhitespace;
      ExpectCP;
    until False;
  end;

begin
  SkipName;
  ExpectWhitespace;

   // Get contentspec [46]
  if FCurChar = 'E' then
    ExpectString('EMPTY')
  else if FCurChar = 'A' then
    ExpectString('ANY')
  else if CheckForChar('(') then
  begin
    SkipWhitespace;
    if CheckForChar('#') then
    begin
      // Parse Mixed section [51]
      ExpectString('PCDATA');
      SkipWhitespace;
      if not CheckForChar(')') then
      begin
        repeat
          ExpectChar('|');
          SkipWhitespace;
          SkipName;
          SkipWhitespace;
        until FCurChar = ')';
        GetChar;
        ExpectChar('*');
      end
      else // 'PCDATA' followed by ')' - fixes valid/P96/ibm69v01.xml
        CheckForChar('*');
    end
    else       // Parse Children section [47]
    begin
      ExpectChoiceOrSeq;

      if CheckForChar('?') then
      else if CheckForChar('*') then
      else if CheckForChar('+') then;
    end;
  end
  else
    RaiseExc('Invalid content specification');

  SkipWhitespace;
  ExpectChar('>');
end;

procedure TXMLReader.ParseNotationDecl;        // [82]
begin
  SkipName;
  ExpectWhitespace;
  // Unclear rule...
  // IE understands 'SYSTEM' followed by literal and 'PUBLIC' followed by 2 literals
  // this is what is handled in ParseExternalID().
  if ParseExternalID(True) then
(*  else if CheckFor('PUBLIC') then
  begin    // [83]
    ExpectWhitespace;
    SkipPubidLiteral;
  end *) else
    RaiseExc('Expected external or public ID');
  SkipWhitespace;
  ExpectChar('>');
end;

procedure TXMLReader.ParseAttlistDecl;         // [52]
var
  SaveCurNode: TDOMNode;
  ValueRequired: Boolean;
  Token: WideString;
begin
  SkipName;
  SkipWhitespace;
  while not CheckForChar('>') do
  begin
    SkipName;
    ExpectWhitespace;
    Token := GetString(['A'..'Z']);     // Get AttType [54], [55], [56]
    if Token = 'CDATA' then
    else if Token = 'ID' then
    else if Token = 'IDREF' then
    else if Token = 'IDREFS' then
    else if Token = 'ENTITY' then
    else if Token = 'ENTITIES' then
    else if Token = 'NMTOKEN' then
    else if Token = 'NMTOKENS' then
    else if Token = 'NOTATION' then     // [57], [58]
    begin
      ExpectWhitespace;
      ExpectChar('(');
      SkipWhitespace;
      SkipName;
      SkipWhitespace;
      while not CheckForChar(')') do
      begin
        ExpectChar('|');
        SkipWhitespace;
        SkipName;
        SkipWhitespace;
      end;
    end
    else
    if CheckForChar('(') then
    begin    // [59]
      SkipWhitespace;
      SkipString(Nmtoken);
      if FValueLength = 0 then   // Fix ibm59n01.xml - name should be present
        RaiseNameNotFound;
      SkipWhitespace;
      while not CheckForChar(')') do
      begin
        ExpectChar('|');
        SkipWhitespace;
        SkipString(Nmtoken);
        SkipWhitespace;
      end;
    end else
      RaiseExc('Invalid tokenized type');

    ExpectWhitespace;

    // Get DefaultDecl [60]
    ValueRequired := False;
    if CheckForChar('#') then
    begin
      Token := GetString(['A'..'Z']);
      if Token = 'REQUIRED' then
      else if Token = 'IMPLIED' then
      else if Token = 'FIXED' then
      begin
        ExpectWhitespace;       // Fix ibm60n05.xml
        ValueRequired := True;
      end
      else
        RaiseExc('Illegal attribute definition'); // Fix sun/not-wf/attlist08.xml
    end
    else
      ValueRequired := True;

    if ValueRequired then
    begin
      SaveCurNode := FCursor;
      FCursor := doc.CreateAttribute('');
      ExpectAttValue;
      FCursor.Free;                         // avoid memory leaks
      FCursor := SaveCurNode;
    end;
    SkipWhitespace;
  end;
end;

procedure TXMLReader.ParseEntityDecl;        // [70]

  function ParseEntityValue: Boolean;        // [9]
  var
    Delim: WideChar;
  begin
    if (FCurChar = '''') or (FCurChar = '"') then
    begin
      Delim := FCurChar;
      GetChar; // skip quote
      while not ((FCurChar = #0) or CheckForChar(Delim)) do
        if ParsePEReference then
        begin
          if FInternalSubset then
            RaiseExc('PE references in internal subset may not occur inside declarations');
        end
        else if CheckForChar('&') then
        begin
          ParseReference().Free;     // may look awful... but avoid memory leaks
        end
        else begin
          GetChar;                   // Normal character
        end;
      Result := True;
    end
    else
      Result := False;
  end;

begin
  if CheckForChar('%') then         // [72]
  begin
    ExpectWhitespace;
    ExpectName;
    ExpectWhitespace;
    // Get PEDef [74]
    if ParseEntityValue then
            // SYSTEM | PUBLIC
    else if ParseExternalID(False) then
    else
      RaiseExc('Expected entity value or external ID');
  end
  else                              // [71]
  begin
    ExpectName;
    ExpectWhitespace;
    // Get EntityDef [73]
    if ParseEntityValue then
    else
    begin
      ExpectExternalID;
      // Get NDataDecl [76]
      if FCurChar <> '>' then
        ExpectWhitespace;     // FIX: ibm76n03.xml: whitespace REQUIRED before NDATA 
      if FCurChar = 'N' then
      begin
        ExpectString('NDATA');
        ExpectWhitespace;
        SkipName;
      end;
    end;
  end;
  SkipWhitespace;
  ExpectChar('>');
end;


function TXMLReader.ParseMarkupDecl(InternalSubset: Boolean): Boolean;    // [29]
var
  Token: WideString;
begin
  Result := False;
  FInternalSubset := InternalSubset;
  repeat
    SkipWhitespace;
    if not CheckForChar('<') then  // condition is true for #0
      Exit;
    if CheckForChar('!') then
    begin
      if FCurChar = '-' then
        ParseComment
      else
      begin
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
          RaiseExc('Wrong declaration type');  
      end;
    end
    else if CheckForChar('?') then
      ParsePI
  until False;
end;

procedure TXMLReader.ProcessDTD(ABuf: PChar; const AFilename: String);
begin
  buf := ABuf;
  Filename := AFilename;
  FLine := 1;
  FColumn := 0;
  DetectEncoding;
  doc := TXMLDocument.Create;
  repeat
    SkipWhitespace;
  until not (ParseMarkupDecl(False) or ParsePEReference);
end;


// starting '<!' already consumed
procedure TXMLReader.ParseCDSect;               // [18]
var
  name: WideString;
begin
  ExpectString('[CDATA[');
  FValueLength := 0;
  repeat
    AppendValue(FCurChar);
    GetChar;
    if (FValueLength >= 3) and (FValue[FValueLength-1] = '>') and
      (FValue[FValueLength-2] = ']') and (FValue[FValueLength-3] = ']') then
    begin
      Dec(FValueLength, 3);
      Break;
    end;
  until FCurChar = #0;

  if FCurChar = #0 then
    RaiseExc('Unterminated CDATA section');

  SetString(name, PWideChar(@FValue[0]), FValueLength);
  FCursor.AppendChild(doc.CreateCDATASection(name));
end;

{
  returns True at end of stream.
     this is ok for fragments but error for document
  returns False when '<' is followed by ([^![?] | NameStartChar)
     this is ok for document (expect ETag then) but error for fragment
}

function TXMLReader.ParseElementContent: Boolean;
begin
  Result := False;
  repeat
    if FCurChar = '<' then
    begin
      GetChar;
      if FCurChar = '!' then
      begin
        GetChar;
        if FCurChar = '[' then
          ParseCDSect
        else if FCurChar = '-' then
          ParseComment
        else
          RaiseExc('Document type declarations not allowed here');
      end
      else if CheckName then
        ParseElement
      else if CheckForChar('?') then
        ParsePI
      else
        Exit;
    end
    else
      ProcessTextAndRefs('<', True);
  until FCurChar = #0;
  Result := True;
end;

// Element name already in FNameBuffer
procedure TXMLReader.ParseElement;    // [39] [40] [44]
var
  NewElem: TDOMElement;
  IsEmpty: Boolean;
  attr, OldAttr: TDOMAttr;
begin
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  ParseElement A');{$ENDIF}

  NewElem := doc.CreateElementBuf(@FName[0], FNameLength);
  FCursor.AppendChild(NewElem);
  Assert(NewElem.ParentNode = FCursor, 'AppendChild did not set ParentNode');
  FCursor := NewElem;

  IsEmpty := False;
  while FCurChar <> '>' do
  begin
    {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  ParseElement E');{$ENDIF}
    if CheckForChar('/') then
    begin
      IsEmpty := True;
      FCursor := FCursor.ParentNode;
      Break;
    end;

    // Get Attribute [41]
    ExpectWhitespace;
    if not CheckName then  // allow stuff like <element >, <element />
      Continue;

    attr := doc.CreateAttributeBuf(@FName[0], FNameLength);

    // WFC: Attribute must be unique
    // !!cannot use TDOMElement.SetAttributeNode because it will free old attribute
    OldAttr := TDOMAttr(NewElem.Attributes.SetNamedItem(Attr));
    if Assigned(OldAttr) then
    begin
      OldAttr.Free;
      RaiseExc('Duplicate attribute');
    end;
    ExpectEq;
    Assert(attr.OwnerElement = NewElem, 'DOMAttr.OwnerElement not set correctly');
    FCursor := attr;
    ExpectAttValue;
    FCursor := NewElem;
  end;
  ExpectChar('>');

  if not IsEmpty then
  begin
    SkipWhitespace;
    if not ParseElementContent then
    begin
      if CheckForChar('/') then         // Get ETag [42]
      begin
        if ExpectName <> NewElem.NodeName then
          RaiseExc('Unmatching element end tag (expected "</' + NewElem.NodeName + '>")');
        SkipWhitespace;
        ExpectChar('>');
        FCursor := FCursor.ParentNode;
      end
      else
        RaiseNameNotFound;
    end
    else // End of stream in content
      RaiseExc('Document element not closed');
  end;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  ParseElement END');{$ENDIF}
end;


procedure TXMLReader.ExpectElement;
begin
  if CheckName then
    ParseElement
  else
    RaiseExc('Expected element');
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


function TXMLReader.ParseExternalID(InNotation: Boolean): Boolean;    // [75]

  function SkipSystemLiteral: Boolean;
  var
    Delim: WideChar;
  begin
    if (FCurChar = '''') or (FCurChar = '"') then
    begin
      Delim := FCurChar;
      GetChar;  // skip quote
      while (FCurChar <> Delim) and (FCurChar <> #0) do
      begin
        GetChar;
      end;
      ExpectChar(Delim);  // <-- to check the EOF only
      Result := True;
    end
    else
      Result := False;
  end;

begin
  if FCurChar = 'S' then
  begin
    ExpectString('SYSTEM');
    ExpectWhitespace;
    if not SkipSystemLiteral then
      RaiseExpectedQMark; // FIX ibm75n06.xml: system literal MUST be present
    Result := True;
  end
  else
  if FCurChar = 'P' then
  begin
    ExpectString('PUBLIC');
    ExpectWhitespace;
    SkipPubidLiteral;
    if InNotation then
    begin
      SkipWhitespace;
      SkipSystemLiteral;
    end
    else
    begin
      ExpectWhitespace;
      if not SkipSystemLiteral then
        RaiseExpectedQMark; // FIX ibm75n06.xml: system literal MUST be present      
    end;  
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ExpectExternalID;
begin
  if not ParseExternalID(False) then
    RaiseExc('Expected external ID');
end;


{ Currently, this method will only resolve the entities which are
  predefined in XML: }

procedure TXMLReader.ResolveEntities(RootNode: TDOMNode);
var
  Node, NextNode: TDOMNode;

  procedure ReplaceEntityRef(EntityNode: TDOMNode; const Replacement: WideString);
  var
    PrevSibling, NextSibling: TDOMNode;
  begin
    PrevSibling := EntityNode.PreviousSibling;
    NextSibling := EntityNode.NextSibling;
    if Assigned(PrevSibling) and (PrevSibling.NodeType = TEXT_NODE) then
    begin
      TDOMCharacterData(PrevSibling).AppendData(Replacement);
      RootNode.RemoveChild(EntityNode);
      if Assigned(NextSibling) and (NextSibling.NodeType = TEXT_NODE) then
      begin
        // next sibling is to be removed, so we can't use it anymore
        NextNode := NextSibling.NextSibling;
        TDOMCharacterData(PrevSibling).AppendData(
        TDOMCharacterData(NextSibling).Data);
        RootNode.RemoveChild(NextSibling);
      end
    end else
      if Assigned(NextSibling) and (NextSibling.NodeType = TEXT_NODE) then
      begin
        TDOMCharacterData(NextSibling).InsertData(0, Replacement);
        RootNode.RemoveChild(EntityNode);
      end else
        RootNode.ReplaceChild(Doc.CreateTextNode(Replacement), EntityNode);
  end;

begin
  Node := RootNode.FirstChild;
  while Assigned(Node) do
  begin
    NextNode := Node.NextSibling;
    if Node.NodeType = ENTITY_REFERENCE_NODE then
      if Node.NodeName = 'amp' then
        ReplaceEntityRef(Node, '&')
      else if Node.NodeName = 'apos' then
        ReplaceEntityRef(Node, '''')
      else if Node.NodeName = 'gt' then
        ReplaceEntityRef(Node, '>')
      else if Node.NodeName = 'lt' then
        ReplaceEntityRef(Node, '<')
      else if Node.NodeName = 'quot' then
        ReplaceEntityRef(Node, '"');
    Node := NextNode;
  end;
end;



procedure ReadXMLFile(out ADoc: TXMLDocument; var f: File);
var
  reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  ADoc := nil;
  BufSize := FileSize(f) + 2; // need double termination for the case of Unicode
  if BufSize <= 2 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 2);
    buf[BufSize - 1] := #0;
    buf[BufSize] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessXML(buf, TFileRec(f).name);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream; const AFilename: String);
var
  reader: TXMLReader;
  buf: PChar;
  StreamSize: Int64;
begin
  ADoc := nil;
  StreamSize := f.Size;    // access to Size causes at least two seeks...
  if StreamSize = 0 then exit;

  GetMem(buf, StreamSize + 2);
  try
    f.Read(buf^, StreamSize);
    buf[StreamSize] := #0;
    buf[StreamSize+1] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessXML(buf, AFilename);
    finally
      ADoc := TXMLDocument(Reader.doc);
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream);
begin
  ReadXMLFile(ADoc, f, '<Stream>');
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);
var
  FileStream: TStream;
begin
  ADoc := nil;
  FileStream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  if FileStream = nil then exit; //? it throws exception if cannot be created...
  try
    ReadXMLFile(ADoc, FileStream, AFilename);
  finally
    FileStream.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: File);
var
  Reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  BufSize := FileSize(f) + 2;
  if BufSize <= 2 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 2);
    buf[BufSize - 1] := #0;
    buf[BufSize] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.Doc := AParentNode.OwnerDocument;
      Reader.ProcessFragment(AParentNode, buf, TFileRec(f).name);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const AFilename: String);
var
  Reader: TXMLReader;
  buf: PChar;
  StreamSize: Int64;
begin
  StreamSize := f.Size;
  if StreamSize = 0 then
    exit;

  GetMem(buf, StreamSize + 2);
  try
    f.Read(buf^, StreamSize);
    buf[StreamSize] := #0;
    buf[StreamSize+1] := #0;
    Reader := TXMLReader.Create;
    Reader.Doc := AParentNode.OwnerDocument;
    try
      Reader.ProcessFragment(AParentNode, buf, AFilename);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream);
begin
  ReadXMLFragment(AParentNode, f, '<Stream>');
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadXMLFragment(AParentNode, Stream, AFilename);
  finally
    Stream.Free;
  end;
end;


procedure ReadDTDFile(out ADoc: TXMLDocument; var f: File);
var
  Reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  ADoc := nil;
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 1);
    buf[BufSize - 1] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessDTD(buf, TFileRec(f).name);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream; const AFilename: String);
var
  Reader: TXMLReader;
  buf: PChar;
begin
  ADoc := nil;
  if f.Size = 0 then
    exit;

  GetMem(buf, f.Size + 1);
  try
    f.Read(buf^, f.Size);
    buf[f.Size] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessDTD(buf, AFilename);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream);
begin
  ReadDTDFile(ADoc, f, '<Stream>');
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; const AFilename: String);
var
  Stream: TStream;
begin
  ADoc := nil;
  Stream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadDTDFile(ADoc, Stream, AFilename);
  finally
    Stream.Free;
  end;
end;


end.
