{
    This file is part of the Free Component Library

    HTML parser with SAX-like interface
    Copyright (c) 2000-2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{
  Known problems:
  * The whitespace handling does only work for processing the DOM tree.
    Storing the DOM tree to a XML file will result in a quite ugly file.
    (This probably has got much better with recent versions, which do
    decent whitespace converting, but it's not tested really good.)
  * Entity references in attribute values don't get parsed.
}

{$mode objfpc}
{$H+}

unit SAX_HTML;

interface

uses SysUtils, Classes, SAX, DOM, DOM_HTML,htmldefs,xmlutils;

type

{ THTMLReader: The HTML reader class }

  THTMLScannerContext = (
    scUnknown,
    scWhitespace,       // within whitespace
    scText,             // within text
    scEntityReference,  // within entity reference ("&...;")
    scTag,              // within a start tag or end tag
    scComment,
    scScript
  );

  THTMLReader = class(TSAXReader)
  private
    FStarted: Boolean;
    FEndOfStream: Boolean;
    FScannerContext: THTMLScannerContext;
    FTokenText: SAXString;
    FRawTokenText: string;
    FScriptEndTag: string;
    FScriptEndMatchPos: Integer;
    FCurStringValueDelimiter: Char;
    FAttrNameRead: Boolean;
    FStack: array of THTMLElementTag;
    FNesting: Integer;
    procedure AutoClose(const aName: SAXString);
    procedure NamePush(const aName: SAXString);
    procedure NamePop;
  protected
    procedure EnterNewScannerContext(NewContext: THTMLScannerContext);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(AInput: TSAXInputSource); override; overload;

    property EndOfStream: Boolean read FEndOfStream;
    property ScannerContext: THTMLScannerContext read FScannerContext;
    property TokenText: SAXString read FTokenText;
  end;


{ THTMLToDOMConverter }

  THTMLNodeType = (ntWhitespace, ntText, ntEntityReference, ntTag);

  THTMLNodeInfo = class
    NodeType: THTMLNodeType;
    Closed: Boolean;
    DOMNode: TDOMNode;
  end;

  THTMLToDOMConverter = class
  private
    FReader: THTMLReader;
    FDocument: TDOMDocument;
    FNodeBuffer: TList;
    IsFragmentMode, FragmentRootSet: Boolean;
    FragmentRoot: TDOMNode;

    procedure ReaderCharacters(Sender: TObject; const ch: PSAXChar;
      Start, Count: Integer);
    procedure ReaderIgnorableWhitespace(Sender: TObject; const ch: PSAXChar;
      Start, Count: Integer);
    procedure ReaderStartElement(Sender: TObject;
      const NamespaceURI, LocalName, RawName: SAXString; Attr: TSAXAttributes);
    procedure ReaderEndElement(Sender: TObject;
      const NamespaceURI, LocalName, RawName: SAXString);

  public
    constructor Create(AReader: THTMLReader; ADocument: TDOMDocument);
    constructor CreateFragment(AReader: THTMLReader; AFragmentRoot: TDOMNode);
    destructor Destroy; override;
  end;


// Helper functions; these ones are HTML equivalents of ReadXML[File|Fragment]

procedure ReadHTMLFile(out ADoc: THTMLDocument; const AFilename: String);
procedure ReadHTMLFile(out ADoc: THTMLDocument; f: TStream);

procedure ReadHTMLFragment(AParentNode: TDOMNode; const AFilename: String);
procedure ReadHTMLFragment(AParentNode: TDOMNode; f: TStream);



implementation


constructor THTMLReader.Create;
begin
  inherited Create;
  FScannerContext := scUnknown;
  SetLength(FStack, 16);
end;

destructor THTMLReader.Destroy;
begin
  if FStarted then
    DoEndDocument;
  inherited Destroy;
end;

function CheckForName(const Tag: SAXString): Boolean;
var
  p, p1: PSAXChar;
begin
  p := PSAXChar(Tag);
  result := False;
  if p^ <> '!' then
  begin
    if p^ = '/' then Inc(p);
    p1 := p;
    while (p1^ <> #0) and (p1^ <> '/') and not IsXMLWhitespace(p1^) do
      Inc(p1);
    result := IsXMLName(p, p1-p);
  end;
end;

procedure THTMLReader.Parse(AInput: TSAXInputSource);
const
  MaxBufferSize = 1024;
var
  Buffer: array[0..MaxBufferSize - 1] of Char;
  BufferSize, BufferPos: Integer;
  len: Integer;
  ch: Char;
begin
  if not FStarted then
  begin
    FStarted := True;
    DoStartDocument;
  end;

  FEndOfStream := False;
  FStopFlag := False;
  while not FStopFlag do
  begin
    // Read data into the input buffer
    BufferSize := AInput.Stream.Read(Buffer, MaxBufferSize);
    if BufferSize = 0 then
    begin
      FEndOfStream := True;
      break;
    end;

    BufferPos := 0;
    while (BufferPos < BufferSize) and not FStopFlag do
    begin
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
                FRawTokenText := FRawTokenText + Buffer[BufferPos];
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
              FScannerContext := scText;
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
              FRawTokenText := FRawTokenText + Buffer[BufferPos];
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
            FRawTokenText := FRawTokenText + Buffer[BufferPos];
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
                FRawTokenText := FRawTokenText + Buffer[BufferPos];
                Inc(BufferPos);
              end;
            '=':
              begin
                FAttrNameRead := True;
                FRawTokenText := FRawTokenText + Buffer[BufferPos];
                Inc(BufferPos);
              end;
            '>':
              begin
                Inc(BufferPos);
                if FCurStringValueDelimiter = #0 then
                  EnterNewScannerContext(scUnknown);
              end;
            '<':    // either an unclosed tag or unescaped '<' in text; attempt recovery
              begin
                // TODO: this check is hardly complete, probably must also check if
                // tag name is followed by legal attributes.
                if CheckForName(FRawTokenText) then   { <-- ansi to wide conversion here }
                  EnterNewScannerContext(scUnknown)   // assume unclosed tag
                else if (FRawTokenText <> '') and (FRawTokenText[1] <> '!') then
                begin
                  Insert('<', FRawTokenText, 1);         // assume plaintext
                  FScannerContext := scText;
                  EnterNewScannerContext(scUnknown);
                end
                else
                begin  // in comment, ignore
                  FRawTokenText := FRawTokenText + Buffer[BufferPos];
                  Inc(BufferPos);
                end;
              end;
          else
            FRawTokenText := FRawTokenText + Buffer[BufferPos];
            if FRawTokenText='!--' then
            begin
              FScannerContext := scComment;
              FRawTokenText := '';
            end;
            Inc(BufferPos);
          end;
        scComment:
          begin
            FRawTokenText := FRawTokenText + Buffer[BufferPos];
            Inc(BufferPos);

            if (Buffer[BufferPos-1]='>') then
            begin
              len:=length(FRawTokenText);
              if (len>2) and (FRawTokenText[len-1]='-') and (FRawTokenText[len-2]='-') then
              begin
                Delete(FRawTokenText, Length(FRawTokenText)-2, MaxInt);
                EnterNewScannerContext(scUnknown);
              end;
            end;
          end;
        scScript:
          begin
            ch := Buffer[BufferPos];
            if FScriptEndMatchPos <= Length(FScriptEndTag) then
            begin
              if lowercase(ch) = FScriptEndTag[FScriptEndMatchPos] then
                Inc(FScriptEndMatchPos)
              else
                FScriptEndMatchPos := 1;
              FRawTokenText := FRawTokenText + ch;
              Inc(BufferPos);
            end
            else
            begin
              case ch of
                #9,#10,#13,' ':
                  begin
                    FRawTokenText := FRawTokenText + ch;
                    Inc(BufferPos);
                    Inc(FScriptEndMatchPos);
                  end;
                '>':
                  begin
                    Inc(BufferPos);
                    Delete(FRawTokenText, Length(FRawTokenText)-FScriptEndMatchPos+2, MaxInt);
                    EnterNewScannerContext(scUnknown);
                  end;
              else
                FRawTokenText := FRawTokenText + ch;
                Inc(BufferPos);
                FScriptEndMatchPos := 1;
              end;
            end;
          end;
        end;    // case ScannerContext of
    end;        // while not endOfBuffer
  end;
end;

function LookupTag(const aName: SAXString): THTMLElementTag;
var
  j: THTMLElementTag;
  ansiName: string;
begin
  ansiName := aName;
  for j := Low(THTMLElementTag) to High(THTMLElementTag) do
    if SameText(HTMLElementProps[j].Name, ansiName) then
    begin
      Result := j;
      Exit;
    end;
  Result := etUnknown;
end;

procedure THTMLReader.AutoClose(const aName: SAXString);
var
  newTag: THTMLElementTag;
begin
  newTag := LookupTag(aName);
  while (FNesting > 0) and IsAutoClose(newTag, FStack[FNesting-1]) do
  begin
    DoEndElement('', HTMLElementProps[FStack[FNesting-1]].Name, '');
    namePop;
  end;
end;

procedure THTMLReader.NamePush(const aName: SAXString);
var
  tag: THTMLElementTag;
begin
  tag := LookupTag(aName);
  if FNesting >= Length(FStack) then
    SetLength(FStack, FNesting * 2);
  FStack[FNesting] := tag;
  Inc(FNesting);
end;

procedure THTMLReader.NamePop;
begin
  if FNesting <= 0 then
    Exit;
  Dec(FNesting);
  FStack[FNesting] := etUnknown;
end;

function SplitTagString(const s: SAXString; var Attr: TSAXAttributes): SAXString;
var
  i, j, len: Integer;
  AttrName: SAXString;
  ValueDelimiter: WideChar;
  haseq, hasname: Boolean;
begin
  Attr := nil;
  i := 0;
  len := Length(s);
  repeat
    Inc(i)
  until (i > len) or IsXMLWhitespace(s[i]);

  Result := Copy(s, 1, i - 1);
  WStrLower(Result);
  if i > len then
    exit;
  Attr := TSAXAttributes.Create;
  Inc(i);

  repeat
    while (i <= len) and IsXMLWhitespace(s[i]) do
      Inc(i);
    if (i > len) then
      break;
    j := i;
    haseq := false;
    hasname := false;
    ValueDelimiter := #0;
    // Attr Name
    while (j <= len) and not (IsXMLWhitespace(s[j]) or (s[j] = '='))  do
      Inc(j);
    // j points on =, whitespace or after s
    AttrName := Copy(s, i, j - i);
    WStrLower(AttrName);
    hasname := IsXMLName(AttrName);
    // Look for = and following whitespaces (maybe j already points on =)
    while (j <= len) and (IsXMLWhitespace(s[j]) or (s[j] = '=')) do
    begin
      if (s[j] = '=') then
        haseq := true;
      Inc(j);
    end;
    // Value (j points on first nonblank or after end)
    if haseq then
    begin
      if (j > len) then  { terminal case <tag attr=> }
      begin
        if hasname then
          Attr.AddAttribute('', AttrName, '', '', '');
        break;
      end
      else
      begin
        if (s[j]='''') or (s[j]='"') then
        begin
          ValueDelimiter := s[j];
          Inc(j);
        end;
        i := j;
        while (j <= len) do
        begin
          if (s[j]=ValueDelimiter) then
            break;
          if (ValueDelimiter=#0) and IsXMLWhitespace(s[j]) then
            break;
          Inc(j);
        end;
        if hasname then
          Attr.AddAttribute('', AttrName, '', '', Copy(s, i, j - i))
      end;
    end
    else if hasname then   { html boolean-style attribute }
      Attr.AddAttribute('', AttrName, '', '', AttrName);

    { skip closing quote if one is present }
    i := j + ord(ValueDelimiter<>#0);
  until false;
end;

procedure THTMLReader.EnterNewScannerContext(NewContext: THTMLScannerContext);
var
  Attr: TSAXAttributes;
  TagName: SAXString;
  Ent: SAXChar;
  i: Integer;
  elTag: THTMLElementTag;
begin
  FTokenText := FRawTokenText;
  case ScannerContext of
    scWhitespace:
      if (FNesting > 0) and (efPCDataContent in HTMLElementProps[FStack[FNesting-1]].Flags) then
        DoCharacters(PSAXChar(TokenText), 0, Length(TokenText))
      else
        DoIgnorableWhitespace(PSAXChar(TokenText), 0, Length(TokenText));
    scText:
      DoCharacters(PSAXChar(TokenText), 0, Length(TokenText));
    scEntityReference:
      begin
        if ResolveHTMLEntityReference(TokenText, Ent) then
          DoCharacters(@Ent, 0, 1)
        else
          DoCharacters(PSAXChar('&' + TokenText + ';'), 0, Length(TokenText) + 2);
      end;
    scTag:
      if Length(TokenText) > 0 then
      begin
        Attr := nil;
        if TokenText[Length(fTokenText)]='/' then  // handle xml/xhtml style empty tag
        begin
          setlength(fTokenText,length(fTokenText)-1);
          // Do NOT combine to a single line, as Attr is an output value!
          TagName := SplitTagString(TokenText, Attr);
          AutoClose(TagName);
          DoStartElement('', TagName, '', Attr);
          DoEndElement('', TagName, '');
        end
        else if TokenText[1] = '/' then
        begin
          Delete(FTokenText, 1, 1);
          TagName := SplitTagString(TokenText, Attr);
          elTag := LookupTag(TagName);
          i := FNesting-1;
          while (i >= 0) and (FStack[i] <> elTag) and
            (efEndTagOptional in HTMLElementProps[FStack[i]].Flags) do
            Dec(i);
          if (i>=0) and (FStack[i] = elTag) then
            while FStack[FNesting-1] <> elTag do
            begin
              DoEndElement('', HTMLElementProps[FStack[FNesting-1]].Name, '');
              namePop;
            end;

          DoEndElement('', TagName, '');
          namePop;
        end
        else if TokenText[1] <> '!' then
        begin
          // Do NOT combine to a single line, as Attr is an output value!
          TagName := SplitTagString(TokenText, Attr);
          AutoClose(TagName);
          namePush(TagName);
          DoStartElement('', TagName, '', Attr);
          if FStack[FNesting-1] in [etScript,etStyle] then
          begin
            NewContext := scScript;
            FScriptEndTag := '</' + HTMLElementProps[FStack[FNesting-1]].Name;
            FScriptEndMatchPos := 1;
          end;
        end;
        if Assigned(Attr) then
          Attr.Free;
      end;
    scComment:
      begin
        DoComment(PSAXChar(TokenText), 0, Length(TokenText));
      end;
    scScript:
      begin
        DoCharacters(PSAXChar(TokenText), 0, Length(TokenText));
        DoEndElement('', HTMLElementProps[FStack[FNesting-1]].Name, '');
        namePop;
        FScriptEndTag := '';
      end;
  end;
  FScannerContext := NewContext;
  FTokenText := '';
  FRawTokenText := '';
  FCurStringValueDelimiter := #0;
  FAttrNameRead := False;
end;


{ THTMLToDOMConverter }

constructor THTMLToDOMConverter.Create(AReader: THTMLReader;
  ADocument: TDOMDocument);
begin
  inherited Create;
  FReader := AReader;
  FReader.OnCharacters := @ReaderCharacters;
  FReader.OnIgnorableWhitespace := @ReaderIgnorableWhitespace;
  FReader.OnStartElement := @ReaderStartElement;
  FReader.OnEndElement := @ReaderEndElement;
  FDocument := ADocument;
  FNodeBuffer := TList.Create;
end;

constructor THTMLToDOMConverter.CreateFragment(AReader: THTMLReader;
  AFragmentRoot: TDOMNode);
begin
  Create(AReader, AFragmentRoot.OwnerDocument);
  FragmentRoot := AFragmentRoot;
  IsFragmentMode := True;
end;

destructor THTMLToDOMConverter.Destroy;
var
  i: Integer;
begin
  // Theoretically, always exactly one item will remain - the root element:
  for i := 0 to FNodeBuffer.Count - 1 do
    THTMLNodeInfo(FNodeBuffer[i]).Free;
  FNodeBuffer.Free;

  inherited Destroy;
end;

procedure THTMLToDOMConverter.ReaderCharacters(Sender: TObject;
  const ch: PSAXChar; Start, Count: Integer);
var
  NodeInfo: THTMLNodeInfo;
begin
  NodeInfo := THTMLNodeInfo.Create;
  NodeInfo.NodeType := ntText;
  NodeInfo.DOMNode := FDocument.CreateTextNodeBuf(ch, Count, False);
  FNodeBuffer.Add(NodeInfo);
end;

procedure THTMLToDOMConverter.ReaderIgnorableWhitespace(Sender: TObject;
  const ch: PSAXChar; Start, Count: Integer);
var
  NodeInfo: THTMLNodeInfo;
begin
  NodeInfo := THTMLNodeInfo.Create;
  NodeInfo.NodeType := ntWhitespace;
  NodeInfo.DOMNode := FDocument.CreateTextNodeBuf(ch, Count, False);
  FNodeBuffer.Add(NodeInfo);
end;

procedure THTMLToDOMConverter.ReaderStartElement(Sender: TObject;
  const NamespaceURI, LocalName, RawName: SAXString; Attr: TSAXAttributes);
var
  NodeInfo: THTMLNodeInfo;
  Element: TDOMElement;
  i: Integer;
begin
  {$ifdef SAX_HTML_DEBUG}
  WriteLn('Start: ', LocalName, '. Node buffer before: ', FNodeBuffer.Count, ' elements');
  {$endif}
  Element := FDocument.CreateElement(LocalName);
  if Assigned(Attr) then
  begin
    {$ifdef SAX_HTML_DEBUG}
     WriteLn('Attribute: ', Attr.GetLength);
    {$endif}
    for i := 0 to Attr.GetLength - 1 do
    begin
      {$ifdef SAX_HTML_DEBUG}
       WriteLn('#', i, ': LocalName = ', Attr.GetLocalName(i), ', Value = ', Attr.GetValue(i));
      {$endif}
      Element[Attr.GetLocalName(i)] := Attr.GetValue(i);
    end;
  end;

  NodeInfo := THTMLNodeInfo.Create;
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
  {$ifdef SAX_HTML_DEBUG}
    WriteLn('Start: ', LocalName, '. Node buffer after: ', FNodeBuffer.Count, ' elements');
  {$endif}
end;

procedure THTMLToDOMConverter.ReaderEndElement(Sender: TObject;
  const NamespaceURI, LocalName, RawName: SAXString);
var
  NodeInfo, NodeInfo2: THTMLNodeInfo;
  i : Integer;
  j : THTMLElementTag;
  TagInfo: PHTMLElementProps;

begin
  {$ifdef SAX_HTML_DEBUG}
    WriteLn('End: ', LocalName, '. Node buffer: ', FNodeBuffer.Count, ' elements');
  {$endif}
  // Find the matching start tag
  i := FNodeBuffer.Count - 1;
  while i >= 0 do
  begin
    NodeInfo := THTMLNodeInfo(FNodeBuffer.Items[i]);
    if (NodeInfo.NodeType = ntTag) and (not NodeInfo.Closed) and
      (CompareText(NodeInfo.DOMNode.NodeName, LocalName) = 0) then
    begin
      // We found the matching start tag

      TagInfo := nil;
      for j := Low(THTMLElementTag) to High(THTMLElementTag) do
        if CompareText(HTMLElementProps[j].Name, LocalName) = 0 then
        begin
          TagInfo := @HTMLElementProps[j];
          break;
        end;

      Inc(i);
      while i < FNodeBuffer.Count do
      begin
        NodeInfo2 := THTMLNodeInfo(FNodeBuffer.Items[i]);

        if (NodeInfo2.NodeType = ntWhitespace) and Assigned(TagInfo) and
          (not (efPreserveWhitespace in TagInfo^.Flags)) then
          // Handle whitespace, which doesn't need to get preserved...
          if not (efPCDATAContent in TagInfo^.Flags) then
            // No character data allowed within the current element
            NodeInfo2.DOMNode.Free
          else
          begin
            // Character data allowed, so normalize it
            NodeInfo2.DOMNode.NodeValue := ' ';
            NodeInfo.DOMNode.AppendChild(NodeInfo2.DOMNode)
          end
        else
          NodeInfo.DOMNode.AppendChild(NodeInfo2.DOMNode);

        NodeInfo2.Free;
        FNodeBuffer.Delete(i);
      end;
      NodeInfo.Closed := True;
      break;
    end;
    Dec(i);
  end;
end;


procedure ReadHTMLFile(out ADoc: THTMLDocument; const AFilename: String);
var
  f: TStream;
begin
  ADoc := nil;
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadHTMLFile(ADoc, f);
  finally
    f.Free;
  end;
end;

procedure ReadHTMLFile(out ADoc: THTMLDocument; f: TStream);
var
  Reader: THTMLReader;
  Converter: THTMLToDOMConverter;
begin
  ADoc := THTMLDocument.Create;
  Reader := THTMLReader.Create;
  try
    Converter := THTMLToDOMConverter.Create(Reader, ADoc);
    try
      Reader.ParseStream(f);
    finally
      Converter.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure ReadHTMLFragment(AParentNode: TDOMNode; const AFilename: String);
var
  f: TStream;
begin
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadHTMLFragment(AParentNode, f);
  finally
    f.Free;
  end;
end;

procedure ReadHTMLFragment(AParentNode: TDOMNode; f: TStream);
var
  Reader: THTMLReader;
  Converter: THTMLToDOMConverter;
begin
  Reader := THTMLReader.Create;
  try
    Converter := THTMLToDOMConverter.CreateFragment(Reader, AParentNode);
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
