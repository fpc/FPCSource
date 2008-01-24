{
    This file is part of the Free Component Library

    XML writing routines
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org
    Modified in 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit XMLWrite;

{$MODE objfpc}
{$H+}

interface

uses Classes, DOM;

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String); overload;
procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text); overload;
procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream); overload;

procedure WriteXML(Element: TDOMNode; const AFileName: String); overload;
procedure WriteXML(Element: TDOMNode; var AFile: Text); overload;
procedure WriteXML(Element: TDOMNode; AStream: TStream); overload;


// ===================================================================

implementation

uses SysUtils;

type
  TSpecialCharCallback = procedure(c: WideChar) of object;

  TXMLWriter = class(TObject)
  private
    FInsideTextNode: Boolean;
    FIndent: WideString;
    FIndentCount: Integer;
    FBuffer: PChar;
    FBufPos: PChar;
    FCapacity: Integer;
    FLineBreak: string;
    procedure wrtChars(Src: PWideChar; Length: Integer);
    procedure IncIndent;
    procedure DecIndent; {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtStr(const ws: WideString); {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtChr(c: WideChar); {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtLineEnd; {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtIndent; {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtQuotedLiteral(const ws: WideString);
    procedure ConvWrite(const s: WideString; const SpecialChars: TSetOfChar;
      const SpecialCharCallback: TSpecialCharCallback);
    procedure AttrSpecialCharCallback(c: WideChar);
    procedure TextNodeSpecialCharCallback(c: WideChar);
  protected
    procedure Write(const Buffer; Count: Longint); virtual; abstract;
    procedure WriteNode(Node: TDOMNode);
    procedure VisitDocument(Node: TDOMNode);
    procedure VisitElement(Node: TDOMNode);
    procedure VisitText(Node: TDOMNode);
    procedure VisitCDATA(Node: TDOMNode);
    procedure VisitComment(Node: TDOMNode);
    procedure VisitFragment(Node: TDOMNode);
    procedure VisitAttribute(Node: TDOMNode);
    procedure VisitEntityRef(Node: TDOMNode);
    procedure VisitDocumentType(Node: TDOMNode);
    procedure VisitPI(Node: TDOMNode);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTextXMLWriter = Class(TXMLWriter)
  Private
    F : ^Text;
  Protected
    Procedure Write(Const Buffer; Count : Longint);override;
  Public
    constructor Create(var AFile: Text);
  end;

  TStreamXMLWriter = Class(TXMLWriter)
  Private
    F : TStream;
  Protected
    Procedure Write(Const Buffer; Count : Longint);override;
  Public
    constructor Create(AStream: TStream);
  end;

{ ---------------------------------------------------------------------
    TTextXMLWriter
  ---------------------------------------------------------------------}


constructor TTextXMLWriter.Create(var AFile: Text);
begin
  inherited Create;
  f := @AFile;
end;

procedure TTextXMLWriter.Write(const Buffer; Count: Longint);
var
  s: string;
begin
  if Count>0 then
  begin
    SetString(s, PChar(@Buffer), Count);
    system.Write(f^, s);
  end;
end;

{ ---------------------------------------------------------------------
    TStreamXMLWriter
  ---------------------------------------------------------------------}

constructor TStreamXMLWriter.Create(AStream: TStream);
begin
  inherited Create;
  F := AStream;
end;


procedure TStreamXMLWriter.Write(const Buffer; Count: Longint);
begin
  if Count > 0 then
    F.Write(Buffer, Count);
end;


{ ---------------------------------------------------------------------
    TXMLWriter
  ---------------------------------------------------------------------}

constructor TXMLWriter.Create;
var
  I: Integer;
begin
  inherited Create;
  // some overhead - always be able to write at least one extra UCS4
  FBuffer := AllocMem(512+32);
  FBufPos := FBuffer;
  FCapacity := 512;
  // Initialize Indent string
  SetLength(FIndent, 100);
  for I := 1 to 100 do FIndent[I] := ' ';
  FIndentCount := 0;
  // Later on, this may be put under user control
  // for now, take OS setting
  FLineBreak := sLineBreak;
end;

destructor TXMLWriter.Destroy;
begin
  if FBufPos > FBuffer then
    write(FBuffer^, FBufPos-FBuffer);

  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TXMLWriter.wrtChars(Src: PWideChar; Length: Integer);
var
  pb: PChar;
  wc: Cardinal;
  SrcEnd: PWideChar;
  I: Integer;
begin
  pb := FBufPos;
  SrcEnd := Src + Length;
  while Src < SrcEnd do
  begin
    if pb >= @FBuffer[FCapacity] then
    begin
      write(FBuffer^, FCapacity);
      Dec(pb, FCapacity);
      if pb > FBuffer then
        Move(FBuffer[FCapacity], FBuffer^, pb - FBuffer);
    end;

    wc := Cardinal(Src^);  Inc(Src);
    case wc of
      $0A:  for I := 1 to System.Length(FLineBreak) do
            begin
              pb^ := FLineBreak[I]; Inc(pb);
            end;

      0..$09, $0B..$7F:  begin
        pb^ := char(wc); Inc(pb);
      end;

      $80..$7FF: begin
        pb^ := Char($C0 or (wc shr 6));   Inc(pb);
        pb^ := Char($80 or (wc and $3F)); Inc(pb);
      end;

      $D800..$DBFF: begin
        if (Src < SrcEnd) and (Src^ >= #$DC00) and (Src^ <= #$DFFF) then
        begin
          wc := ((wc - $D7C0) shl 10) + (word(Src^) xor $DC00);
          Inc(Src);

          pb^ := Char($F0 or (wc shr 18));           Inc(pb);
          pb^ := Char($80 or ((wc shr 12) and $3F)); Inc(pb);
          pb^ := Char($80 or ((wc shr 6) and $3F));  Inc(pb);
          pb^ := Char($80 or (wc and $3F));          Inc(pb);
        end
        else
          raise EConvertError.Create('High surrogate without low one');
      end;
      $DC00..$DFFF:
        raise EConvertError.Create('Low surrogate without high one');
      else   // $800 >= wc > $FFFF, excluding surrogates
      begin
        pb^ := Char($E0 or (wc shr 12));          Inc(pb);
        pb^ := Char($80 or ((wc shr 6) and $3F)); Inc(pb);
        pb^ := Char($80 or (wc and $3F));         Inc(pb);
      end;
    end;
  end;
  FBufPos := pb;
end;

procedure TXMLWriter.wrtStr(const ws: WideString); { inline }
begin
  wrtChars(PWideChar(ws), Length(ws));
end;

procedure TXMLWriter.wrtChr(c: WideChar); { inline }
begin
  wrtChars(@c,1);
end;

procedure TXMLWriter.wrtLineEnd; { inline }
begin
  // line endings now handled in WrtStr!
  wrtChr(#10);
end;

procedure TXMLWriter.wrtIndent; { inline }
begin
  wrtChars(PWideChar(FIndent), FIndentCount*2);
end;

procedure TXMLWriter.IncIndent;
var
  I, NewLen, OldLen: Integer;
begin
  Inc(FIndentCount);
  if Length(FIndent) < 2 * FIndentCount then
  begin
    OldLen := Length(FIndent);
    NewLen := 4 * FIndentCount;
    SetLength(FIndent, NewLen);
    for I := OldLen to NewLen do
      FIndent[I] := ' ';
  end;
end;

procedure TXMLWriter.DecIndent; { inline }
begin
  if FIndentCount>0 then dec(FIndentCount);
end;

procedure TXMLWriter.wrtQuotedLiteral(const ws: WideString);
var
  Quote: WideChar;
begin
  // TODO: need to check if the string also contains single quote
  // both quotes present is a error
  if Pos('"', ws) > 0 then
    Quote := ''''
  else
    Quote := '"';
  wrtChr(Quote);
  wrtStr(ws);
  wrtChr(Quote);
end;

const
  AttrSpecialChars = ['<', '"', '&', #9, #10, #13];
  TextSpecialChars = ['<', '>', '&'];

procedure TXMLWriter.ConvWrite(const s: WideString; const SpecialChars: TSetOfChar;
  const SpecialCharCallback: TSpecialCharCallback);
var
  StartPos, EndPos: Integer;
begin
  StartPos := 1;
  EndPos := 1;
  while EndPos <= Length(s) do
  begin
    if (s[EndPos] < #255) and (Char(ord(s[EndPos])) in SpecialChars) then
    begin
      wrtChars(@s[StartPos], EndPos - StartPos);
      SpecialCharCallback(s[EndPos]);
      StartPos := EndPos + 1;
    end;
    Inc(EndPos);
  end;
  if StartPos <= length(s) then
    wrtChars(@s[StartPos], EndPos - StartPos);
end;

const
  QuotStr = '&quot;';
  AmpStr = '&amp;';
  ltStr = '&lt;';
  gtStr = '&gt;';

procedure TXMLWriter.AttrSpecialCharCallback(c: WideChar);
begin
  case c of
    '"': wrtStr(QuotStr);
    '&': wrtStr(AmpStr);
    '<': wrtStr(ltStr);
    // Escape whitespace using CharRefs to be consistent with W3 spec § 3.3.3
    #9: wrtStr('&#x9;');
    #10: wrtStr('&#xA;');
    #13: wrtStr('&#xD;');
  else
    wrtChr(c);
  end;
end;

procedure TXMLWriter.TextnodeSpecialCharCallback(c: WideChar);
begin
  case c of
    '<': wrtStr(ltStr);
    '>': wrtStr(gtStr); // Required only in ']]>' literal, otherwise optional
    '&': wrtStr(AmpStr);
  else
    wrtChr(c);
  end;
end;

procedure TXMLWriter.WriteNode(node: TDOMNode);
begin
  case node.NodeType of
    ELEMENT_NODE:                VisitElement(node);
    ATTRIBUTE_NODE:              VisitAttribute(node);
    TEXT_NODE:                   VisitText(node);
    CDATA_SECTION_NODE:          VisitCDATA(node);
    ENTITY_REFERENCE_NODE:       VisitEntityRef(node);
    PROCESSING_INSTRUCTION_NODE: VisitPI(node);
    COMMENT_NODE:                VisitComment(node);
    DOCUMENT_NODE:               VisitDocument(node);
    DOCUMENT_TYPE_NODE:          VisitDocumentType(node);
    ENTITY_NODE,
    DOCUMENT_FRAGMENT_NODE:      VisitFragment(node);
  end;
end;


procedure TXMLWriter.VisitElement(node: TDOMNode);
var
  i: Integer;
  attr, child: TDOMNode;
  SavedInsideTextNode: Boolean;
  IsLeaf: Boolean;
  MixedContent: Boolean;
begin
  if not FInsideTextNode then
    wrtIndent;
  wrtChr('<');
  wrtStr(node.NodeName);
  // FIX: Accessing Attributes was causing them to be created for every element :(
  if node.HasAttributes then
    for i := 0 to node.Attributes.Length - 1 do
    begin
      attr := node.Attributes.Item[i];
      if TDOMAttr(attr).Specified then
        VisitAttribute(attr);
    end;
  Child := node.FirstChild;
  if Child = nil then
    wrtStr('/>')
  else
  begin
    SavedInsideTextNode := FInsideTextNode;
    wrtChr('>');
    MixedContent := False;
    repeat
      if Assigned(Child.PreviousSibling) and
        (Child.PreviousSibling.InheritsFrom(TDOMText) <> Child.InheritsFrom(TDOMText)) then
        MixedContent := True;
      Child := Child.NextSibling;
    until Child = nil;
    Child := node.FirstChild; // restore

    IsLeaf := (Child = node.LastChild) and (Child.FirstChild = nil);
    if not (FInsideTextNode or MixedContent or IsLeaf) then
      wrtLineEnd;

    FInsideTextNode := {FInsideTextNode or} MixedContent or IsLeaf;
    IncIndent;
    repeat
      WriteNode(Child);
      Child := Child.NextSibling;
    until Child = nil;
    DecIndent;
    if not FInsideTextNode then
      wrtIndent;
    FInsideTextNode := SavedInsideTextNode;
    wrtStr('</');
    wrtStr(Node.NodeName);
    wrtChr('>');
  end;
  if not FInsideTextNode then
    wrtLineEnd;
end;

procedure TXMLWriter.VisitText(node: TDOMNode);
begin
  ConvWrite(node.NodeValue, TextSpecialChars, {$IFDEF FPC}@{$ENDIF}TextnodeSpecialCharCallback);
end;

procedure TXMLWriter.VisitCDATA(node: TDOMNode);
begin
  if not FInsideTextNode then
    wrtIndent;
  wrtStr('<![CDATA[');
  wrtStr(node.NodeValue);
  wrtStr(']]>');
  if not FInsideTextNode then
    wrtLineEnd;
end;

procedure TXMLWriter.VisitEntityRef(node: TDOMNode);
begin
  wrtChr('&');
  wrtStr(node.NodeName);
  wrtChr(';');
end;

procedure TXMLWriter.VisitPI(node: TDOMNode);
begin
  if not FInsideTextNode then wrtIndent;
  wrtStr('<?');
  wrtStr(TDOMProcessingInstruction(node).Target);
  wrtChr(' ');
  wrtStr(TDOMProcessingInstruction(node).Data);
  wrtStr('?>');
  if not FInsideTextNode then wrtLineEnd;
end;

procedure TXMLWriter.VisitComment(node: TDOMNode);
begin
  if not FInsideTextNode then wrtIndent;
  wrtStr('<!--');
  wrtStr(node.NodeValue);
  wrtStr('-->');
  if not FInsideTextNode then wrtLineEnd;
end;

procedure TXMLWriter.VisitDocument(node: TDOMNode);
var
  child: TDOMNode;
begin
  wrtStr('<?xml version="');
  // Definitely should not escape anything here
  if Length(TXMLDocument(node).XMLVersion) > 0 then
    wrtStr(TXMLDocument(node).XMLVersion)
  else
    wrtStr('1.0');
  wrtChr('"');
  
// DISABLED - we are only able write in UTF-8 which does not require labeling
// writing incorrect encoding will render xml unreadable...
(*
  if Length(TXMLDocument(node).Encoding) > 0 then
  begin
    wrtStr(' encoding="');
    wrtStr(TXMLDocument(node).Encoding);
    wrtChr('"');
  end;
*)
  wrtStr('?>'#10);

  // TODO: now handled as a regular PI, remove this?
  if Length(TXMLDocument(node).StylesheetType) > 0 then
  begin
    wrtStr('<?xml-stylesheet type="');
    wrtStr(TXMLDocument(node).StylesheetType);
    wrtStr('" href="');
    wrtStr(TXMLDocument(node).StylesheetHRef);
    wrtStr('"?>'#10);
  end;

  child := node.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;
end;

procedure TXMLWriter.VisitAttribute(Node: TDOMNode);
var
  Child: TDOMNode;
begin
  wrtChr(' ');
  wrtStr(Node.NodeName);
  wrtStr('="');
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    if Child.NodeType = ENTITY_REFERENCE_NODE then
      VisitEntityRef(Child)
    else
      ConvWrite(Child.NodeValue, AttrSpecialChars, {$IFDEF FPC}@{$ENDIF}AttrSpecialCharCallback);
    Child := Child.NextSibling;
  end;
  wrtChr('"');
end;

procedure TXMLWriter.VisitDocumentType(Node: TDOMNode);
begin
  wrtStr('<!DOCTYPE ');
  wrtStr(Node.NodeName);
  wrtChr(' ');
  with TDOMDocumentType(Node) do
  begin
    if PublicID <> '' then
    begin
      wrtStr('PUBLIC ');
      wrtQuotedLiteral(PublicID);
      wrtChr(' ');
      wrtQuotedLiteral(SystemID);
    end
    else if SystemID <> '' then
    begin
      wrtStr('SYSTEM ');
      wrtQuotedLiteral(SystemID);
    end;
    if InternalSubset <> '' then
    begin
      wrtChr('[');
      wrtStr(InternalSubset);
      wrtChr(']');
    end;
  end;
  wrtStr('>'#10);
end;

procedure TXMLWriter.VisitFragment(Node: TDOMNode);
var
  Child: TDOMNode;
begin
  // TODO: TextDecl is probably needed
  // Fragment itself should not be written, only its children should...
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;
end;


// -------------------------------------------------------------------
//   Interface implementation
// -------------------------------------------------------------------

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    WriteXMLFile(doc, fs);
  finally
    fs.Free;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
begin
  with TTextXMLWriter.Create(AFile) do
  try
    WriteNode(doc);
  finally
    Free;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream);
begin
  with TStreamXMLWriter.Create(AStream) do
  try
    WriteNode(doc);
  finally
    Free;
  end;
end;

procedure WriteXML(Element: TDOMNode; const AFileName: String);
begin
  WriteXMLFile(TXMLDocument(Element), AFileName);
end;

procedure WriteXML(Element: TDOMNode; var AFile: Text);
begin
  WriteXMLFile(TXMLDocument(Element), AFile);
end;

procedure WriteXML(Element: TDOMNode; AStream: TStream);
begin
  WriteXMLFile(TXMLDocument(Element), AStream);
end;



end.
