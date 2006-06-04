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
  WhitespaceChars: TSetOfChar = [#9, #10, #13, ' '];

  NmToken: TSetOfChar = Letter + Digit + ['.', '-', '_', ':'];

type
  TXMLReaderDocument = class(TXMLDocument)
  public
    procedure SetDocType(ADocType: TDOMDocumentType);
  end;

  TXMLReaderDocumentType = class(TDOMDocumentType)
  public
    constructor Create(ADocument: TXMLReaderDocument);
    property Name: DOMString read FNodeName write FNodeName;
  end;

  { supported encodings }
  TEncoding = (enUnknown, enUTF8, enUTF16BE, enUTF16LE);

  TXMLReader = class
  private
    FCurChar: WideChar;
    FLine: Integer;
    FColumn: Integer;
    FEncoding: TEncoding;
    FValue: array of WideChar;
    FValueLength: Integer;
    FPrologParsed: Boolean;
    procedure RaiseExpectedQmark;
    function GetChar: WideChar;
    procedure AppendValue(wc: WideChar);
    procedure DetectEncoding;
  protected
    buf: PChar;
    Filename: String;

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
    procedure ExpectAttValue(attr: TDOMAttr);                           // [10]
    procedure SkipPubidLiteral;                                         // [12]
    procedure ParseComment(AOwner: TDOMNode);                           // [15]
    procedure ParsePI;                                                  // [16]
    procedure ExpectProlog;                                             // [22]
    procedure ParseProlog;
    function  ParseEq: Boolean;                                         // [25]
    procedure ExpectEq;
    procedure ParseMisc(AOwner: TDOMNode);                              // [27]
    function  ParseMarkupDecl: Boolean;                                 // [29]
    procedure ParseCharData(AOwner: TDOMNode);                          // [14]
    procedure ParseCDSect(AOwner: TDOMNode);                            // [18]
    function  ParseElement(AOwner: TDOMNode): Boolean;                  // [39]
    procedure ExpectElement(AOwner: TDOMNode);
    procedure ParseReference(AOwner: TDOMNode);                         // [67]
    function  ParsePEReference: Boolean;                                // [69]
    function  ParseExternalID: Boolean;                                 // [75]
    procedure ExpectExternalID;
    procedure SkipEncodingDecl;                                         // [80]

    procedure ParseEntityDecl;
    procedure ParseAttlistDecl;
    procedure ParseElementDecl;
    procedure ParseNotationDecl;

    procedure ResolveEntities(RootNode: TDOMNode);
  public
    doc: TDOMDocument;
    procedure ProcessXML(ABuf: PChar; const AFilename: String);  // [1]
    procedure ProcessFragment(AOwner: TDOMNode; ABuf: PChar; const AFilename: String);
    procedure ProcessDTD(ABuf: PChar; const AFilename: String);  // ([29])
  end;

{ TXMLReaderDocument }

procedure TXMLReaderDocument.SetDocType(ADocType: TDOMDocumentType);
begin
  FDocType := ADocType;
end;

constructor TXMLReaderDocumentType.Create(ADocument: TXMLReaderDocument);
begin
  inherited Create(ADocument);
end;

// TODO: this and others must use table approach for speed-up
function IsNameStartChar(wc: WideChar): Boolean;   // [4]
begin
  case wc of
    // (note) excludes single $D7, $F7, $37E,
    ':', 'A'..'Z', '_', 'a'..'z', #$C0..#$D6, #$D8..#$F6, #$F8..#$2FF,
    #$370..#$37D, #$37F..#$1FFF, #$200C, #$200D, #$2070..#$218F,
    #$2C00..#$2FEF, #$3001..#$D7FF, #$F900..#$FDCF, #$FDF0..#$FFFD: Result := True;
  else
    Result := False;
  end;
end;

function IsNameChar(wc: WideChar): Boolean;       // [4a]
begin
  Result := IsNameStartChar(wc) or ((wc = '-') or (wc = '.') or ((wc >= '0') and (wc <= '9')) or
            (wc = #$B7) or ((wc >= #$300) and (wc <= #$36F)) or (wc = #$203F) or (wc = #$2040));
end;

function IsWhitespace(wc: WideChar): Boolean;
begin
  Result := (wc = ' ') or (wc = #10) or (wc = #13) or (wc = #9);
end;


{ TXMLReader }

procedure TXMLReader.DetectEncoding;
var
  w: Word;
  Enc: TEncoding;

function CheckByte(value: Byte): Boolean;
var
  cb: Byte;
begin
  cb := ord(buf[0]); Inc(buf);
  Result := (cb = value);
end;

function CheckWord(value: Word): Boolean;
var
  cw: Word;
begin
  cw := PWord(buf)^; Inc(buf, sizeof(Word));
  {$IFDEF ENDIAN_BIG} Swap(cw); {$ENDIF}   // TODO: Is that correct?
  Result := (cw = value);
end;

begin
  Enc := enUnknown;
  w := PWord(Buf)^; Inc(Buf, sizeof(Word));
  {$IFDEF ENDIAN_BIG} Swap(w); {$ENDIF}   // TODO: Is that correct?

  // case of no BOM
  if (w = (ord('?') shl 8 + ord('<'))) { $3F3C } then
    Enc := enUTF8   // not known, in fact, just a default
  else if (w = ord('<')) and CheckWord(ord('?')) then
    Enc := enUTF16LE
  else if (w = ord('<') shl 8) and CheckWord(ord('?') shl 8) then
    Enc := enUTF16BE;

  if Enc <> enUnknown then // any of above tests succeeded, must start from '?'
  begin
    FEncoding := Enc;
    FCurChar := '?';
    Exit;
  end;

  if w = $FFFE then
    FEncoding := enUTF16BE
  else if w = $FEFF then
    FEncoding := enUTF16LE
  else if (w = $BBEF) and CheckByte($BF) then
    FEncoding := enUTF8;

  GetChar;
end;

function TXMLReader.GetChar: WideChar;
var
  ch, ch2, ch3: Byte;

  procedure BadChar;
  begin
    RaiseExc('Invalid character in UTF8 sequence');
  end;

begin
  if FEncoding in [enUnknown, enUTF8] then
  begin
    ch := ord(buf[0]);
    Inc(Buf);
  end
  else
  begin     // Endianness: no swapping here; see below
    FCurChar := PWideChar(Buf)^;
    Inc(Buf, sizeof(WideChar));
  end;

  case FEncoding of
    enUnknown:
      FCurChar := WideChar(Ch);
    enUTF8:
      if Ch < 128 then                        { ASCII }
        FCurChar := WideChar(Ch)
      else if (Ch and $E0) = $C0 then         { #$0080 - #$07FF }
      begin
        ch2 := ord(buf[0]); Inc(Buf);
        if (Ch2 and $C0) <> $80 then
          BadChar;
        FCurChar := WideChar((Ch and $1F) shl 6 + (Ch2 and $3F));
      end
      else if (Ch and $F0) = $E0 then         { #$0800 - #$FFFF }
      begin
        ch2 := ord(buf[0]); Inc(buf);
        if (Ch2 and $C0) <> $80 then
          BadChar;
          ch3 := ord(buf[0]); Inc(buf);
        if (Ch3 and $C0) <> $80 then
          BadChar;
        FCurChar := WideChar(Word((Ch and $0F) shl 12) +
          (Ch2 and $3F) shl 6 + (Ch3 and $3F));
      end
      else
        RaiseExc('Unsupported UTF8 character');
{$IFDEF ENDIAN_BIG}
    enUTF16LE:
{$ELSE}
    enUTF16BE:
{$ENDIF}
      FCurChar :=
        WideChar((Ord(FCurChar) and $FF) shl 8 + (Ord(FCurChar) shr 8));
  end;

  // TODO: Linefeed handling according to W3C
  if FCurChar = #10 then
  begin
    Inc(FLine);
    FColumn := 0;
  end
  else
    Inc(FColumn);

  Result := FCurChar;
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

procedure TXMLReader.RaiseExpectedQmark;
begin
  RaiseExc('Expected single or double quotation mark');
end;

procedure TXMLReader.RaiseExc(Expected, Found: WideChar);
begin
  RaiseExc('Expected "' + Expected + '", but found "' +  Found + '",');
end;

procedure TXMLReader.RaiseExc(const descr: String);
begin
  raise EXMLReadError.CreateFmt('In %s (line %d pos %d): %s', [Filename, FLine, FColumn, descr]);
end;

function TXMLReader.SkipWhitespace: Boolean;
begin
  Result := False;
  while IsWhitespace(FCurChar) do
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


procedure TXMLReader.ProcessXML(ABuf: PChar; const AFilename: String);    // [1]
begin
  buf := ABuf;
  Filename := AFilename;
  FLine := 1;
  FColumn := 0;

  doc := TXMLReaderDocument.Create;
  DetectEncoding;
  ExpectProlog;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ProcessXML A');{$ENDIF}
  ExpectElement(doc);
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ProcessXML B');{$ENDIF}
  ParseMisc(doc);

  if FCurChar <> #0 then
    RaiseExc('Text after end of document element found');
end;

procedure TXMLReader.ProcessFragment(AOwner: TDOMNode; ABuf: PChar; const AFilename: String);
var
  t: WideChar;
begin
  buf := ABuf;
  Filename := AFilename;
  FLine := 1;
  FColumn := 0;
  FEncoding := enUTF8; // TODO: Detect it? Not sure for now...
  GetChar;
  repeat
    SkipWhitespace;
    if FCurChar = '<' then
    begin
      t := GetChar;
      if t = '!' then
      begin
        GetChar;
        if FCurChar = '[' then
          ParseCDSect(AOwner)
        else if FCurChar = '-' then
          ParseComment(AOwner);
      end
      else if t = '?' then
        ParsePI
      else
        ParseElement(AOwner);
    end
    else if FCurChar = '&' then
      ParseReference(AOwner)
    else
      ParseCharData(AOwner);
  until FCurChar = #0;
end;

function TXMLReader.CheckName: Boolean;        // [5]
begin
  Result := IsNameStartChar(FCurChar);
  if Result then
  begin
    FValueLength := 0;
    repeat
      AppendValue(FCurChar);
      GetChar;
    until (FCurChar = #0) or not IsNameChar(FCurChar);
  end;
end;

procedure TXMLReader.RaiseNameNotFound;
begin
  RaiseExc('Expected letter, "_" or ":" for name, found "' + FCurChar + '"');
end;

function TXMLReader.ExpectName: WideString;    // [5]
begin
  if not CheckName then
    RaiseNameNotFound;

  SetString(Result, PWideChar(@FValue[0]), FValueLength);
end;

procedure TXMLReader.SkipName;
begin
  if not CheckName then
    RaiseNameNotFound;
end;

// ---------------------

procedure TXMLReader.ExpectAttValue(attr: TDOMAttr);    // [10]

  procedure FlushStringBuffer;
  var
    s: WideString;
  begin
    if FValueLength > 0 then
    begin
      SetString(s, PWideChar(@FValue[0]),FValueLength);
      FValueLength := 0;
      attr.AppendChild(doc.CreateTextNode(s));
      //SetLength(s, 0);  // cleared implicitly
    end;
  end;

var
  Delim: WideChar;
begin
  if (FCurChar <> '''') and (FCurChar <> '"') then
    RaiseExpectedQmark;
  Delim := FCurChar;
  GetChar;  // skip quote

  FValueLength := 0;
  while (FCurChar <> Delim) and (FCurChar <> #0) do
  begin
    if FCurChar <> '&' then
    begin
      AppendValue(FCurChar);
      GetChar;
    end
    else
    begin
      if FValueLength > 0 then FlushStringBuffer;
      ParseReference(attr);
      FValueLength := 0;
    end;
  end;
  if FValueLength > 0 then FlushStringBuffer;
  GetChar;  // skip trailing quote
  ResolveEntities(Attr);
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
procedure TXMLReader.ParseComment(AOwner: TDOMNode);    // [15]
var
  comment: WideString;
begin
  ExpectString('--');
  FValueLength := 0;
  repeat
    FValue[FValueLength] := FCurChar;
    GetChar;
    if (FValueLength >= 2) and (FValue[FValueLength] = '>') and
      (FValue[FValueLength-1] = '-') and (FValue[FValueLength-2] = '-') then
      begin
        Dec(FValueLength, 2);
        Break;
      end;
    Inc(FValueLength);
  until FCurChar = #0;  // should not happen

  SetString(comment, PWideChar(@FValue[0]), FValueLength);
  AOwner.AppendChild(doc.CreateComment(comment));
end;

// starting '?' contained in FCurChar
procedure TXMLReader.ParsePI;           // [16]
begin
  GetChar;   // skip '?'
  SkipName;

  // ugly but uses no temp string. Need StrLIComp(PWideChar, PWideChar).
  if (FValueLength = 3) and
     ((FValue[0] = 'X') or (FValue[0] = 'x')) and
     ((FValue[1] = 'M') or (FValue[1] = 'm')) and
     ((FValue[2] = 'L') or (FValue[2] = 'l')) then
  begin
    if not FPrologParsed then
    begin
      ParseProlog;
      FPrologParsed := True;
      Exit;
    end
    else
      RaiseExc('"<?xml" processing instruction not allowed here');
  end;

  if SkipWhitespace then
  begin
    FValueLength := 0;
    repeat
      FValue[FValueLength] := FCurChar;
      GetChar;
      if (FValueLength >= 1) and (FValue[FValueLength] = '>') and
        (FValue[FValueLength-1] = '?') then
        begin
          Dec(FValueLength);
          Break;
        end;
      Inc(FValueLength);
    until FCurChar = #0;  // should not happen
  end;
  
end;

// here we come from ParsePI, 'xml' is already consumed
procedure TXMLReader.ParseProlog;
var
  Delim: WideChar;
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
      TXMLDocument(doc).XMLVersion :=
      GetString(NmToken);    {['a'..'z', 'A'..'Z', '0'..'9', '_', '.', ':', '-']}
    ExpectChar(Delim);
  end
  else
    RaiseExpectedQMark;

  // EncodingDecl?
  SkipEncodingDecl;

  // SDDecl?
  SkipWhitespace;
  if CheckForChar('s') then
  begin
    ExpectString('tandalone');
    ExpectEq;
    if (FCurChar = '''') or (FCurChar = '"') then
    begin
      Delim := FCurChar;
      GetChar; // skip quote
      ExpectName;          // TODO: must check for 'yes' or 'no'
      ExpectChar(Delim);
    end
    else
      RaiseExpectedQMark;
    SkipWhitespace;
  end;

  ExpectString('?>');
end;

procedure TXMLReader.ExpectProlog;    // [22]
var
  DocType: TXMLReaderDocumentType;

begin
  FPrologParsed := False;
  // The special case when first chars had been consumed by DetectEncoding()
  if FCurChar = '?' then
    ParsePI;
  // Check for "Misc*"
  ParseMisc(doc);

  // Check for "(doctypedecl Misc*)?"    [28]
  if CheckForChar('D') then
  begin
    ExpectString('OCTYPE');
    // create the DTD object
    DocType := TXMLReaderDocumentType.Create(doc as TXMLReaderDocument);
    if doc.InheritsFrom(TXMLReaderDocument) then
      TXMLReaderDocument(doc).SetDocType(DocType);
    SkipWhitespace;
    DocType.Name := ExpectName;
    SkipWhitespace;
    ParseExternalID;    // may be absent, ignore result
    SkipWhitespace;

    if CheckForChar('[') then
    begin
      repeat
        SkipWhitespace;
      until not (ParseMarkupDecl or ParsePEReference);
      ExpectChar(']');
      SkipWhitespace;
      ExpectChar('>');
    end;
    ParseMisc(doc);
  end;
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

procedure TXMLReader.ParseMisc(AOwner: TDOMNode);    // [27]
begin
  repeat
    SkipWhitespace;
    if (FCurChar = #0) or (FCurChar <> '<') then
      Break;
    GetChar;
    if FCurChar = '!' then
    begin
      GetChar;
      if FCurChar = '-' then
        ParseComment(AOwner)
      else
        Break;
    end
    else
      if FCurChar = '?' then
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
    delimiter: WideChar;
  begin
    SkipWhitespace;
    ExpectCP;
    SkipWhitespace;
    delimiter := #0;
    repeat
      if (FCurChar = #0) or CheckForChar(')') then
        Break;
      if delimiter = #0 then
      begin
        if (FCurChar = '|') or (FCurChar = ',') then
          delimiter := FCurChar
        else
          RaiseExc('Expected "|" or ","');
      end
      else
        if FCurChar <> delimiter then
          RaiseExc(delimiter, FCurChar);
      GetChar; // skip delimiter   
      SkipWhitespace;
      ExpectCP;
    until False;
  end;


begin   // starting '<!E' already consumed
  ExpectString('LEMENT');
  ExpectWhitespace;
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
        repeat
          ExpectChar('|');
          SkipWhitespace;
          SkipName;
         // TODO: verify 
        until (FCurChar = ')') and (GetChar = '*'); //CheckFor(')*');
    end
    else
    begin
      // Parse Children section [47]

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

// starting '<!' already consumed
procedure TXMLReader.ParseNotationDecl;        // [82]
begin
  ExpectString('NOTATION');
  ExpectWhitespace;
  SkipName;
  ExpectWhitespace;
  // Unclear rule...
  // IE understands 'SYSTEM' followed by literal and 'PUBLIC' followed by 2 literals
  // this is what is handled in ParseExternalID().
  if ParseExternalID then
(*  else if CheckFor('PUBLIC') then
  begin    // [83]
    ExpectWhitespace;
    SkipPubidLiteral;
  end *) else
    RaiseExc('Expected external or public ID');
  SkipWhitespace;
  ExpectChar('>');
end;

// starting '<!' already consumed
procedure TXMLReader.ParseAttlistDecl;         // [52]
var
  attr: TDOMAttr;
  ValueRequired: Boolean;
begin
  ExpectString('ATTLIST');
  ExpectWhitespace;
  SkipName;
  SkipWhitespace;
  while not CheckForChar('>') do
  begin
    SkipName;
    ExpectWhitespace;

    // Get AttType [54], [55], [56]
    // TODO: possibly extract all letters and compare with list...
    if FCurChar = 'C' then
      ExpectString('CDATA')
    else if CheckForChar('I') then    // ID, IDREF, IDREFS
    begin
      ExpectChar('D');
      if FCurChar = 'R' then
      begin
        ExpectString('REF');
        CheckForChar('S');
      end;
    end
    else if FCurChar = 'E' then
    begin
      ExpectString('ENTIT');
      if not CheckForChar('Y') then
        ExpectString('IES');
    end
    else if CheckForChar('N') then
    begin
      if FCurChar = 'M' then
      begin
        ExpectString('TOKEN');
        CheckForChar('S');
      end
      else if FCurChar = 'O' then      // [57], [58]
      begin
        ExpectString('OTATION');
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
      end;
    end
    else
    if CheckForChar('(') then
    begin    // [59]
      SkipWhitespace;
      SkipString(Nmtoken);
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
      if FCurChar = 'R' then
        ExpectString('REQUIRED')
      else if FCurChar = 'I' then
        ExpectString('IMPLIED')
      else if FCurChar = 'F' then
      begin
        ExpectString('FIXED');
        SkipWhitespace;
        ValueRequired := True;
      end;
    end
    else
      ValueRequired := True;

    if ValueRequired then
    begin
      attr := doc.CreateAttribute('');
      ExpectAttValue(attr);
    end;
    SkipWhitespace;
  end;
end;

// starting '<!' already consumed
procedure TXMLReader.ParseEntityDecl;        // [70]
var
  NewEntity: TDOMEntity;

  function ParseEntityValue: Boolean;    // [9]
  var
    Delim: WideChar;
  begin
    if (FCurChar = '''') or (FCurChar = '"') then
    begin
      Delim := FCurChar;
      GetChar; // skip quote
      while not CheckForChar(Delim) do
        if ParsePEReference then
        else if FCurChar = '&' then ParseReference(NewEntity)
        else begin
          GetChar;            // Normal character
        end;
      Result := True;
    end
    else
      Result := False;
  end;
  
begin
  ExpectString('NTITY');
  ExpectWhitespace;
  if CheckForChar('%') then         // [72]
  begin
    ExpectWhitespace;
    NewEntity := doc.CreateEntity(ExpectName);
    ExpectWhitespace;
    // Get PEDef [74]
    if ParseEntityValue then
            // SYSTEM | PUBLIC
    else if ParseExternalID then
    else
      RaiseExc('Expected entity value or external ID');
  end
  else                              // [71]
  begin
    NewEntity := doc.CreateEntity(ExpectName);
    ExpectWhitespace;
    // Get EntityDef [73]
    if ParseEntityValue then
    else
    begin
      ExpectExternalID;
      // Get NDataDecl [76]
      SkipWhitespace;
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


function TXMLReader.ParseMarkupDecl: Boolean;    // [29]
begin
  Result := False;
  repeat
    SkipWhitespace;
    if (FCurChar = #0) or (FCurChar <> '<') then
      Exit;
    // '<'
    GetChar;
    if FCurChar = '!' then
    begin
      GetChar;
      if FCurChar = 'E' then       // either ELEMENT or ENTITY
      begin
        GetChar;
        if FCurChar = 'L' then
          ParseElementDecl
        else if FCurChar = 'N' then
          ParseEntityDecl;
      end
      else if FCurChar = 'A' then  // ATTLIST
        ParseAttlistDecl
      else if FCurChar = 'N' then  // NOTATION
        ParseNotationDecl
      else if FCurChar = '-' then
        ParseComment(Doc);
    end
    else if FCurChar = '?' then
      ParsePI;
  until False;
end;

procedure TXMLReader.ProcessDTD(ABuf: PChar; const AFilename: String);
begin
  buf := ABuf;
  Filename := AFilename;
  FLine := 1;
  FColumn := 0;
  FEncoding := enUTF8;  // TODO: Detect? Don't know for sure now...
  GetChar;
  doc := TXMLReaderDocument.Create;
  ParseMarkupDecl;

  {
  if buf[0] <> #0 then begin
    DebugLn('=== Unparsed: ===');
    //DebugLn(buf);
    DebugLn(StrLen(buf), ' chars');
  end;
  }
end;

procedure TXMLReader.ParseCharData(AOwner: TDOMNode);      // [14]
var
  nonWs: Boolean;
  name: WideString;
begin
  FValueLength := 0;
  nonWs := False;
  while not ((FCurChar = #0) or (FCurChar = '<') or (FCurChar = '&')) do
  begin
    if not IsWhitespace(FCurChar) then
      nonWs := True;
    AppendValue(FCurChar);
    GetChar;
  end;

  if nonWs then
  begin
    SetString(name, PWideChar(@FValue[0]), FValueLength);
    AOwner.AppendChild(doc.CreateTextNode(name));
  end;
end;

// starting '<!' already consumed
procedure TXMLReader.ParseCDSect(AOwner: TDOMNode);       // [18]
var
  name: WideString;
begin
  ExpectString('[CDATA[');
  FValueLength := 0;
  repeat
    FValue[FValueLength] := FCurChar;
    GetChar;
    if (FValueLength >= 2) and (FValue[FValueLength] = '>') and
      (FValue[FValueLength-1] = ']') and (FValue[FValueLength-2] = ']') then
    begin
      Dec(FValueLength, 2);
      Break;
    end;
    Inc(FValueLength);
  until FCurChar = #0;

  SetString(name, PWideChar(@FValue[0]), FValueLength);
  AOwner.AppendChild(doc.CreateCDATASection(name));
end;

function TXMLReader.ParseElement(AOwner: TDOMNode): Boolean;    // [39] [40] [44]
var
  NewElem: TDOMElement;

  procedure CreateNameElement;
  var
    IsEmpty: Boolean;
    attr: TDOMAttr;
    name: WideString;
    t: WideChar;
  begin
    {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  CreateNameElement A');{$ENDIF}
    SetString(name, PWideChar(@FValue[0]), FValueLength);

    NewElem := doc.CreateElement(name);
    AOwner.AppendChild(NewElem);

    SkipWhitespace;
    IsEmpty := False;
    while True do
    begin
      {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  CreateNameElement E');{$ENDIF}
      if CheckForChar('>') then
        Break
      else if FCurChar = '/' then
      begin
        GetChar;
        if CheckForChar('>') then
        begin
          IsEmpty := True;
          Break;
        end
           // <-- error: '>' required
      end;
      
      // Get Attribute [41]
      attr := doc.CreateAttribute(ExpectName);
      NewElem.Attributes.SetNamedItem(attr);
      ExpectEq;
      ExpectAttValue(attr);

      SkipWhitespace;
    end;

    if not IsEmpty then           // Get content
    repeat
      SkipWhitespace;
      if FCurChar = '<' then
      begin
        t := GetChar;
        if t = '!' then
        begin
          GetChar;
          if FCurChar = '[' then
            ParseCDSect(NewElem)
          else if FCurChar = '-' then
            ParseComment(NewElem);
        end
        else if t = '?' then
          ParsePI
        else if t = '/' then       // Get ETag [42]
        begin
          GetChar; // skip '/'
          if ExpectName <> NewElem.NodeName then
            RaiseExc('Unmatching element end tag (expected "</' + NewElem.NodeName + '>")');
          SkipWhitespace;
          ExpectChar('>');
          Break;
        end
        else
          ParseElement(NewElem);
      end
      else if FCurChar = '&' then
        ParseReference(NewElem)
      else
        ParseCharData(NewElem);
    until False;
    {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  CreateNameElement END');{$ENDIF}
    ResolveEntities(NewElem);
  end;

begin                // starting '<' is already consumed at this point
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ParseElement A');{$ENDIF}
  if CheckName then
  begin
    CreateNameElement;
    Result := True;
  end
  else
    Result := False;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ParseElement END');{$ENDIF}
end;

procedure TXMLReader.ExpectElement(AOwner: TDOMNode);
begin
  if not ParseElement(AOwner) then
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

// FCurChar = '&' here
procedure TXMLReader.ParseReference(AOwner: TDOMNode);    // [67] [68]
var
  StrBuf: array[0..31] of char;
  StrLength: Integer;
  s: string;
  Value: Integer;
  PrevNode: TDomNode;

  procedure AppendChar(c: WideChar);
  begin
    if StrLength < High(StrBuf) then
    begin
      StrBuf[StrLength] := char(c);
      Inc(StrLength);
    end;
    GetChar;
  end;

begin
  GetChar; // skip '&'
  if CheckForChar('#') then
  begin    // Test for CharRef [66]
    StrLength := 0;
    if CheckForChar('x') then
    begin
      AppendChar('$');
      while ((ord(FCurChar) < 256) and (char(FCurChar) in ['0'..'9', 'a'..'f', 'A'..'F'])) do
        AppendChar(FCurChar);
    end else
      while ((ord(FCurChar) < 256) and (char(FCurChar) in ['0'..'9'])) do
        AppendChar(FCurChar);
    // TODO: get rid of temp string here     
    SetString(s, StrBuf, StrLength);
    // This will handle case of no digits present
    Value := StrToIntDef(s, -1);
    if (Value < 0) or (Value > $FFFF) then
      RaiseExc('Invalid character reference')
    else
    begin
      PrevNode := AOwner.LastChild;
      // TODO: partial solution, check other similar cases
      if Assigned(PrevNode) and (PrevNode.NodeType = TEXT_NODE) then
        TDomCharacterData(PrevNode).AppendData(WideChar(Value))
      else
        AOwner.AppendChild(doc.CreateTextNode(WideChar(Value)));
    end;
  end else
    AOwner.AppendChild(doc.CreateEntityReference(ExpectName));
  ExpectChar(';');
end;


function TXMLReader.ParseExternalID: Boolean;    // [75]

  procedure SkipSystemLiteral;
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
    end;
  end;

begin
  if FCurChar = 'S' then
  begin
    ExpectString('SYSTEM');
    ExpectWhitespace;
    SkipSystemLiteral;
    Result := True;
  end
  else
  if FCurChar = 'P' then
  begin
    ExpectString('PUBLIC');
    ExpectWhitespace;
    SkipPubidLiteral;
    ExpectWhitespace;
    SkipSystemLiteral;
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ExpectExternalID;
begin
  if not ParseExternalID then
    RaiseExc('Expected external ID');
end;


procedure TXMLReader.SkipEncodingDecl;                   // [80]
var
  Delim: WideChar;
begin
  SkipWhitespace;
  if CheckForChar('e') then
  begin
    ExpectString('ncoding');
    ExpectEq;
    if (FCurChar = '''') or (FCurChar = '"') then
    begin
      Delim := FCurChar;
      GetChar; // skip quote
      if not ((ord(FCurChar) < 256) and (char(FCurChar) in ['A'..'Z', 'a'..'z'])) then
        RaiseExc('Expected character (A-Z, a-z)');
      SkipString(['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']);
      ExpectChar(Delim);
    end;
  end;
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
