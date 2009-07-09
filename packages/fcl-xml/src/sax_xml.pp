{
    This file is part of the Free Component Library
    Copyright (c) 2006 by Michael Van Canneyt.
    Based on SAX_HTML implementation from Sebastian Guenther.

    XML parser with SAX interface

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

unit SAX_XML;

interface

uses SysUtils, Classes, SAX, DOM;

type

{ TXMLReader: The XML reader class }

  TXMLScannerContext = (
    scUnknown,
    scWhitespace,       // within whitespace
    scText,             // within text
    scEntityReference,  // within entity reference ("&...;")
    scTag);             // within a start tag or end tag

  TSAXXMLReader = class(TSAXReader)
  private
    FStarted: Boolean;
    FEndOfStream: Boolean;
    FScannerContext: TXMLScannerContext;
    FTokenText: SAXString;
    FCurStringValueDelimiter: Char;
    FAttrNameRead: Boolean;
  protected
    procedure EnterNewScannerContext(NewContext: TXMLScannerContext);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(AInput: TSAXInputSource); override; overload;

    property EndOfStream: Boolean read FEndOfStream;
    property ScannerContext: TXMLScannerContext read FScannerContext;
    property TokenText: SAXString read FTokenText;
  end;


{ TXMLToDOMConverter }

  TXMLNodeType = (ntWhitespace, ntText, ntEntityReference, ntTag);

  TXMLNodeInfo = class
    NodeType: TXMLNodeType;
    DOMNode: TDOMNode;
  end;

  TXMLToDOMConverter = class
  private
    FReader: TSAXXMLReader;
    FDocument: TDOMDocument;
    FElementStack: TList;
    FNodeBuffer: TList;
    IsFragmentMode, FragmentRootSet: Boolean;
    FragmentRoot: TDOMNode;

    procedure ReaderCharacters(Sender: TObject; const ch: PSAXChar;
      Start, Count: Integer);
    procedure ReaderIgnorableWhitespace(Sender: TObject; const ch: PSAXChar;
      Start, Count: Integer);
    procedure ReaderSkippedEntity(Sender: TObject; const Name: SAXString);
    procedure ReaderStartElement(Sender: TObject;
      const NamespaceURI, LocalName, RawName: SAXString; Attr: TSAXAttributes);
    procedure ReaderEndElement(Sender: TObject;
      const NamespaceURI, LocalName, RawName: SAXString);

  public
    constructor Create(AReader: TSAXXMLReader; ADocument: TDOMDocument);
    constructor CreateFragment(AReader: TSAXXMLReader; AFragmentRoot: TDOMNode);
    destructor Destroy; override;
  end;


// Helper functions; these ones are XML equivalents of ReadXML[File|Fragment]

procedure ReadXMLFile(var ADoc: TXMLDocument; const AFilename: String);
procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream);

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream);



implementation

uses htmldefs; // for entities...

const
  WhitespaceChars = [#9, #10, #13, ' '];
  char_lt: SAXChar = '<';
  char_gt: SAXChar = '>';
  char_quot: SAXChar = '"';
  char_apos: SAXChar = '''';
  char_amp: SAXChar = '&';


constructor TSAXXMLReader.Create;
begin
  inherited Create;
  FScannerContext := scUnknown;
end;

destructor TSAXXMLReader.Destroy;
begin
  if FStarted then
    DoEndDocument;
  inherited Destroy;
end;

procedure TSAXXMLReader.Parse(AInput: TSAXInputSource);
const
  MaxBufferSize = 1024;
var
  Buffer: array[0..MaxBufferSize - 1] of Char;
  BufferSize, BufferPos: Integer;
begin
  if not FStarted then
  begin
    FStarted := True;
    DoStartDocument;
  end;

  FEndOfStream := False;
  while True do
  begin
    // Read data into the input buffer
    BufferSize := AInput.Stream.Read(Buffer, MaxBufferSize);
    if BufferSize = 0 then
    begin
      FEndOfStream := True;
      break;
    end;

    BufferPos := 0;
    while BufferPos < BufferSize do
      case ScannerContext of
        scUnknown:
          case Buffer[BufferPos] of
            #9, #10, #13, ' ':
              EnterNewScannerContext(scWhitespace);
            '&':
              begin
                Inc(BufferPos);
                EnterNewScannerContext(scEntityReference);
              end;
            '<':
              begin
                Inc(BufferPos);
                EnterNewScannerContext(scTag);
              end;
            else
              EnterNewScannerContext(scText);
          end;
        scWhitespace:
          case Buffer[BufferPos] of
            #9, #10, #13, ' ':
              begin
                FTokenText := FTokenText + Buffer[BufferPos];
                Inc(BufferPos);
              end;
            '&':
              begin
                Inc(BufferPos);
                EnterNewScannerContext(scEntityReference);
              end;
            '<':
              begin
                Inc(BufferPos);
                EnterNewScannerContext(scTag);
              end;
            else
              FScannerContext := scText
          end;
        scText:
          case Buffer[BufferPos] of
            '&':
              begin
                Inc(BufferPos);
                EnterNewScannerContext(scEntityReference);
              end;
            '<':
              begin
                Inc(BufferPos);
                EnterNewScannerContext(scTag);
              end;
            else
            begin
              FTokenText := FTokenText + Buffer[BufferPos];
              Inc(BufferPos);
            end;
          end;
        scEntityReference:
          if Buffer[BufferPos] = ';' then
          begin
            Inc(BufferPos);
            EnterNewScannerContext(scUnknown);
          end else if not (Buffer[BufferPos] in
            ['a'..'z', 'A'..'Z', '0'..'9', '#']) then
            EnterNewScannerContext(scUnknown)
          else
          begin
            FTokenText := FTokenText + Buffer[BufferPos];
            Inc(BufferPos);
          end;
        scTag:
          case Buffer[BufferPos] of
            '''', '"':
              begin
                if FAttrNameRead then
                begin
                  if FCurStringValueDelimiter = #0 then
                    FCurStringValueDelimiter := Buffer[BufferPos]
                  else if FCurStringValueDelimiter = Buffer[BufferPos] then
                  begin
                    FCurStringValueDelimiter := #0;
                    FAttrNameRead := False;
                  end;
                end;
                FTokenText := FTokenText + Buffer[BufferPos];
                Inc(BufferPos);
              end;
            '=':
              begin
                FAttrNameRead := True;
                FTokenText := FTokenText + Buffer[BufferPos];
                Inc(BufferPos);
              end;
            '>':
              begin
                Inc(BufferPos);
                if FCurStringValueDelimiter = #0 then
                  EnterNewScannerContext(scUnknown);
              end;
            else
            begin
              FTokenText := FTokenText + Buffer[BufferPos];
              Inc(BufferPos);
            end;
          end;
      end;
  end;
end;

procedure TSAXXMLReader.EnterNewScannerContext(NewContext: TXMLScannerContext);

  function SplitTagString(const s: String; var Attr: TSAXAttributes): String;
  var
    i, j: Integer;
    AttrName: String;
    ValueDelimiter: Char;
    DoIncJ: Boolean;
  begin
    Attr := nil;
    i := Pos(' ', s);
    if i <= 0 then
      Result := LowerCase(s)
    else
    begin
      Result := LowerCase(Copy(s, 1, i - 1));
      Attr := TSAXAttributes.Create;

      Inc(i);

      while (i <= Length(s)) and (s[i] in WhitespaceChars) do
        Inc(i);

      SetLength(AttrName, 0);
      j := i;

      while j <= Length(s) do
        if s[j] = '=' then
        begin
          AttrName := LowerCase(Copy(s, i, j - i));
          Inc(j);
          if (j < Length(s)) and ((s[j] = '''') or (s[j] = '"')) then
          begin
            ValueDelimiter := s[j];
            Inc(j);
          end else
            ValueDelimiter := #0;
          i := j;
          DoIncJ := False;
          while j <= Length(s) do
            if ValueDelimiter = #0 then
              if s[j] in WhitespaceChars then
                break
              else
                Inc(j)
            else if s[j] = ValueDelimiter then
            begin
              DoIncJ := True;
              break
            end else
              Inc(j);

          Attr.AddAttribute('', AttrName, '', '', Copy(s, i, j - i));

          if DoIncJ then
            Inc(j);

          while (j <= Length(s)) and (s[j] in WhitespaceChars) do
            Inc(j);
          i := j;
        end
        else if s[j] in WhitespaceChars then
        begin
          Attr.AddAttribute('', Copy(s, i, j - i), '', '', '');
          Inc(j);
          while (j <= Length(s)) and (s[j] in WhitespaceChars) do
            Inc(j);
          i := j;
        end else
          Inc(j);
    end;
  end;

var
  Attr: TSAXAttributes;
  TagName: String;
  Found: Boolean;
  Ent: SAXChar;
  i: Integer;
begin
  case ScannerContext of
    scWhitespace:
      DoIgnorableWhitespace(PSAXChar(TokenText), 1, Length(TokenText));
    scText:
      DoCharacters(PSAXChar(TokenText), 0, Length(TokenText));
    scEntityReference:
      begin
        if (Length(TokenText) >= 2) and (TokenText[1] = '#') and
          (((TokenText[2] >= '0') and (TokenText[2] <= '9')) or (TokenText[2]='x')) and
          // here actually using it to resolve character references
          ResolveHTMLEntityReference(TokenText, Ent) then
            DoCharacters(@Ent, 0, 1)
        else if TokenText = 'lt' then
          DoCharacters(@char_lt, 0, 1)
        else if TokenText = 'gt' then
          DoCharacters(@char_gt, 0, 1)
        else if TokenText = 'amp' then
          DoCharacters(@char_amp, 0, 1)
        else if TokenText = 'quot' then
          DoCharacters(@char_quot, 0, 1)
        else if TokenText = 'apos' then
          DoCharacters(@char_apos, 0, 1)
        else
          DoSkippedEntity(TokenText);
      end;
    scTag:
      if Length(TokenText) > 0 then
      begin
        Attr := nil;
        if TokenText[Length(fTokenText)]='/' then  // handle empty tag
        begin
          setlength(fTokenText,length(fTokenText)-1);
          // Do NOT combine to a single line, as Attr is an output value!
          TagName := SplitTagString(TokenText, Attr);
          DoStartElement('', TagName, '', Attr);
          DoEndElement('', TagName, '');
        end
        else if TokenText[1] = '/' then
        begin
          DoEndElement('',
            SplitTagString(Copy(TokenText, 2, Length(TokenText)), Attr), '');
        end
        else if TokenText[1] <> '!' then
        begin
          // Do NOT combine to a single line, as Attr is an output value!
          TagName := SplitTagString(TokenText, Attr);
          DoStartElement('', TagName, '', Attr);
        end;
        if Assigned(Attr) then
          Attr.Free;
      end;
  end;
  FScannerContext := NewContext;
  SetLength(FTokenText, 0);
  FCurStringValueDelimiter := #0;
  FAttrNameRead := False;
end;


{ TXMLToDOMConverter }

constructor TXMLToDOMConverter.Create(AReader: TSAXXMLReader;
  ADocument: TDOMDocument);
begin
  inherited Create;
  FReader := AReader;
  FReader.OnCharacters := @ReaderCharacters;
  FReader.OnIgnorableWhitespace := @ReaderIgnorableWhitespace;
  FReader.OnSkippedEntity := @ReaderSkippedEntity;
  FReader.OnStartElement := @ReaderStartElement;
  FReader.OnEndElement := @ReaderEndElement;
  FDocument := ADocument;
  FElementStack := TList.Create;
  FNodeBuffer := TList.Create;
end;

constructor TXMLToDOMConverter.CreateFragment(AReader: TSAXXMLReader;
  AFragmentRoot: TDOMNode);
begin
  inherited Create;
  FReader := AReader;
  FReader.OnCharacters := @ReaderCharacters;
  FReader.OnIgnorableWhitespace := @ReaderIgnorableWhitespace;
  FReader.OnSkippedEntity := @ReaderSkippedEntity;
  FReader.OnStartElement := @ReaderStartElement;
  FReader.OnEndElement := @ReaderEndElement;
  FDocument := AFragmentRoot.OwnerDocument;
  FElementStack := TList.Create;
  FNodeBuffer := TList.Create;
  FragmentRoot := AFragmentRoot;
  IsFragmentMode := True;
end;

destructor TXMLToDOMConverter.Destroy;
var
  i: Integer;
begin
  // Theoretically, always exactly one item will remain - the root element:
  for i := 0 to FNodeBuffer.Count - 1 do
    TXMLNodeInfo(FNodeBuffer[i]).Free;
  FNodeBuffer.Free;

  FElementStack.Free;
  inherited Destroy;
end;

procedure TXMLToDOMConverter.ReaderCharacters(Sender: TObject;
  const ch: PSAXChar; Start, Count: Integer);
var
  s: SAXString;
  NodeInfo: TXMLNodeInfo;
begin
  SetLength(s, Count);
  Move(ch^, s[1], Count * SizeOf(SAXChar));

  NodeInfo := TXMLNodeInfo.Create;
  NodeInfo.NodeType := ntText;
  NodeInfo.DOMNode := FDocument.CreateTextNode(s);
  FNodeBuffer.Add(NodeInfo);
end;

procedure TXMLToDOMConverter.ReaderIgnorableWhitespace(Sender: TObject;
  const ch: PSAXChar; Start, Count: Integer);
var
  s: SAXString;
  NodeInfo: TXMLNodeInfo;
begin
  SetLength(s, Count);
  Move(ch^, s[1], Count * SizeOf(SAXChar));

  NodeInfo := TXMLNodeInfo.Create;
  NodeInfo.NodeType := ntWhitespace;
  NodeInfo.DOMNode := FDocument.CreateTextNode(s);
  FNodeBuffer.Add(NodeInfo);
end;

procedure TXMLToDOMConverter.ReaderSkippedEntity(Sender: TObject;
  const Name: SAXString);
var
  NodeInfo: TXMLNodeInfo;
begin
  NodeInfo := TXMLNodeInfo.Create;
  NodeInfo.NodeType := ntEntityReference;
  NodeInfo.DOMNode := FDocument.CreateEntityReference(Name);
  FNodeBuffer.Add(NodeInfo);
end;

procedure TXMLToDOMConverter.ReaderStartElement(Sender: TObject;
  const NamespaceURI, LocalName, RawName: SAXString; Attr: TSAXAttributes);
var
  NodeInfo: TXMLNodeInfo;
  Element: TDOMElement;
  i: Integer;
begin
  // WriteLn('Start: ', LocalName, '. Node buffer before: ', FNodeBuffer.Count, ' elements');
  Element := FDocument.CreateElement(LocalName);
  if Assigned(Attr) then
  begin
    // WriteLn('Attribute: ', Attr.GetLength);
    for i := 0 to Attr.GetLength - 1 do
    begin
      // WriteLn('#', i, ': LocalName = ', Attr.GetLocalName(i), ', Value = ', Attr.GetValue(i));
      Element[Attr.GetLocalName(i)] := Attr.GetValue(i);
    end;
  end;

  NodeInfo := TXMLNodeInfo.Create;
  NodeInfo.NodeType := ntTag;
  NodeInfo.DOMNode := Element;
  if IsFragmentMode then
  begin
    if not FragmentRootSet then
    begin
      FragmentRoot.AppendChild(Element);
      FragmentRootSet := True;
    end;
  end else
    if not Assigned(FDocument.DocumentElement) then
      FDocument.AppendChild(Element);
  FNodeBuffer.Add(NodeInfo);
  // WriteLn('Start: ', LocalName, '. Node buffer after: ', FNodeBuffer.Count, ' elements');
end;

procedure TXMLToDOMConverter.ReaderEndElement(Sender: TObject;
  const NamespaceURI, LocalName, RawName: SAXString);
var
  NodeInfo, NodeInfo2: TXMLNodeInfo;
  i : Integer;

begin
  // WriteLn('End: ', LocalName, '. Node buffer: ', FNodeBuffer.Count, ' elements');
  // Find the matching start tag
  i := FNodeBuffer.Count - 1;
  while i >= 0 do
  begin
    NodeInfo := TXMLNodeInfo(FNodeBuffer.Items[i]);
    if (NodeInfo.NodeType = ntTag) and
      (CompareText(NodeInfo.DOMNode.NodeName, LocalName) = 0) then
    begin
      // We found the matching start tag

      Inc(i);
      while i < FNodeBuffer.Count do
      begin
        NodeInfo2 := TXMLNodeInfo(FNodeBuffer.Items[i]);
        NodeInfo.DOMNode.AppendChild(NodeInfo2.DOMNode);
        NodeInfo2.Free;
        FNodeBuffer.Delete(i);
      end;
      break;
    end;
    Dec(i);
  end;
end;


procedure ReadXMLFile(var ADoc: TXMLDocument; const AFilename: String);
var
  f: TStream;
begin
  ADoc := nil;
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadXMLFile(ADoc, f);
  finally
    f.Free;
  end;
end;

procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream);
var
  Reader: TSAXXMLReader;
  Converter: TXMLToDOMConverter;
begin
  ADoc := TXMLDocument.Create;
  Reader := TSAXXMLReader.Create;
  try
    Converter := TXMLToDOMConverter.Create(Reader, ADoc);
    try
      Reader.ParseStream(f);
    finally
      Converter.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
var
  f: TStream;
begin
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadXMLFragment(AParentNode, f);
  finally
    f.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream);
var
  Reader: TSAXXMLReader;
  Converter: TXMLToDOMConverter;
begin
  Reader := TSAXXMLReader.Create;
  try
    Converter := TXMLToDOMConverter.CreateFragment(Reader, AParentNode);
    try
      Reader.ParseStream(f);
    finally
      Converter.Free;
    end;
  finally
    Reader.Free;
  end;
end;


end.
