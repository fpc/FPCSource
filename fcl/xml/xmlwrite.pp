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

{$ifdef fpc}
{$MODE objfpc}
{$INLINE ON}
{$H+}
{$endif}

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

// -------------------------------------------------------------------
//   Text file and TStream support
// -------------------------------------------------------------------

type
  TOutputProc = procedure(const Buffer; Count: Longint) of object;
  TCharacters = set of Char;
  TSpecialCharCallback = procedure(c: Char) of object;

  TXMLWriter = class(TObject)  // (TAbstractDOMVisitor)?
  private
    FInsideTextNode: Boolean;
    FIndent: string;
    FIndentCount: Integer;
    procedure IncIndent; {$IFDEF FPC} inline; {$ENDIF}
    procedure DecIndent; {$IFDEF FPC} inline; {$ENDIF}
    procedure wrtStr(const s: string);
    procedure wrtChr(c: char);
    procedure wrtLineEnd; {$IFDEF FPC} inline; {$ENDIF}
    procedure wrtIndent;
    procedure ConvWrite(const s: String; const SpecialChars: TCharacters;
      const SpecialCharCallback: TSpecialCharCallback);
    procedure AttrSpecialCharCallback(c: Char);
    procedure TextNodeSpecialCharCallback(c: Char);
  protected
    Procedure Write(Const Buffer; Count : Longint); virtual;Abstract;
    Procedure Writeln(Const Buffer; Count : Longint); virtual;
    procedure WriteNode(Node: TDOMNode);
    procedure VisitDocument(Node: TDOMNode);  // override;
    procedure VisitElement(Node: TDOMNode);
    procedure VisitText(Node: TDOMNode);
    procedure VisitCDATA(Node: TDOMNode);
    procedure VisitComment(Node: TDOMNode);
    procedure VisitFragment(Node: TDOMNode);
    procedure VisitAttribute(Node: TDOMNode);
    procedure VisitEntity(Node: TDOMNode);
    procedure VisitEntityRef(Node: TDOMNode);
    procedure VisitDocumentType(Node: TDOMNode);
    procedure VisitPI(Node: TDOMNode);
    procedure VisitNotation(Node: TDOMNode);
  end;

  TTextXMLWriter = Class(TXMLWriter)
  Private
    F : ^Text;
  Protected  
    Procedure Write(Const Buffer; Count : Longint);override;
  Public  
    procedure WriteXML(Root: TDomNode; var AFile: Text); overload;
  end;
  
  TStreamXMLWriter = Class(TXMLWriter)
  Private
    F : TStream;
  Protected  
    Procedure Write(Const Buffer; Count : Longint);override;
  Public  
    procedure WriteXML(Root: TDomNode; AStream : TStream); overload;
  end;

{ ---------------------------------------------------------------------
    TTextXMLWriter
  ---------------------------------------------------------------------}
  

procedure TTextXMLWriter.Write(const Buffer; Count: Longint);
var
  s: string;
begin
  if Count>0 then
    begin
    SetString(s, PChar(Buffer), Count);
    system.Write(f^, s);
    end;
end;

{ ---------------------------------------------------------------------
    TStreamXMLWriter
  ---------------------------------------------------------------------}

procedure TStreamXMLWriter.Write(const Buffer; Count: Longint);
begin
  if Count > 0 then
    F.Write(Buffer, Count);
end;


{ ---------------------------------------------------------------------
    TXMLWriter
  ---------------------------------------------------------------------}

Procedure TXMLWriter.Writeln(Const Buffer; Count : Longint); 

begin
  Write(buffer,count);
  Wrtstr(slinebreak);
end;


procedure TXMLWriter.wrtStr(const s: string);
begin
  if s<>'' then
    write(s[1],length(s));
end;

procedure TXMLWriter.wrtChr(c: char);
begin
  write(c,1);
end;

procedure TXMLWriter.wrtLineEnd;
begin
  wrtstr(slinebreak);
end;

procedure TXMLWriter.wrtIndent;
var
  I: Integer;
begin
  for I:=1 to FIndentCount do
    wrtStr(FIndent);
end;

procedure TXMLWriter.IncIndent;
begin
  Inc(FIndentCount);
end;

procedure TXMLWriter.DecIndent;
begin
  if FIndentCount>0 then dec(FIndentCount);
end;

const
  AttrSpecialChars = ['<', '>', '"', '&'];
  TextSpecialChars = ['<', '>', '&'];

procedure TXMLWriter.ConvWrite(const s: String; const SpecialChars: TCharacters;
  const SpecialCharCallback: TSpecialCharCallback);
var
  StartPos, EndPos: Integer;
begin
  StartPos := 1;
  EndPos := 1;
  while EndPos <= Length(s) do
  begin
    if s[EndPos] in SpecialChars then
    begin
      write(s[StartPos],EndPos - StartPos);
      SpecialCharCallback(s[EndPos]);
      StartPos := EndPos + 1;
    end;
    Inc(EndPos);
  end;
  if StartPos <= length(s) then
    write(s[StartPos], EndPos - StartPos);
end;

procedure TXMLWriter.AttrSpecialCharCallback(c: Char);
const
  QuotStr = '&quot;';
  AmpStr = '&amp;';
  ltStr = '&lt;';
begin
  if c = '"' then
    wrtStr(QuotStr)
  else if c = '&' then
    wrtStr(AmpStr)
  else if c = '<' then
    wrtStr(ltStr)
  else
    write(c,1);
end;

procedure TXMLWriter.TextnodeSpecialCharCallback(c: Char);
const
  ltStr = '&lt;';
  gtStr = '&gt;';
  AmpStr = '&amp;';
begin
  if c = '<' then
    wrtStr(ltStr)
  else if c = '>' then
    wrtStr(gtStr)
  else if c = '&' then
    wrtStr(AmpStr)
  else
    write(c,1);
end;

procedure TXMLWriter.WriteNode(node: TDOMNode);
begin
  // Must be: node.Accept(Self);
  case node.NodeType of
    ELEMENT_NODE:                VisitElement(node);
    ATTRIBUTE_NODE:              VisitAttribute(node);
    TEXT_NODE:                   VisitText(node);
    CDATA_SECTION_NODE:          VisitCDATA(node);
    ENTITY_REFERENCE_NODE:       VisitEntityRef(node);
    ENTITY_NODE:                 VisitEntity(node);
    PROCESSING_INSTRUCTION_NODE: VisitPI(node);
    COMMENT_NODE:                VisitComment(node);
    DOCUMENT_NODE:               VisitDocument(node);
    DOCUMENT_TYPE_NODE:          VisitDocumentType(node);
    DOCUMENT_FRAGMENT_NODE:      VisitFragment(node);
    NOTATION_NODE:               VisitNotation(node);
  end;
end;


procedure TXMLWriter.VisitElement(node: TDOMNode);
var
  i: Integer;
  attr, child: TDOMNode;
  SavedInsideTextNode: Boolean;
  s: DOMString;
begin
  if not FInsideTextNode then
    wrtIndent;
  wrtChr('<');
  wrtStr(UTF8Encode(node.NodeName));
  for i := 0 to node.Attributes.Length - 1 do
  begin
    attr := node.Attributes.Item[i];
    wrtChr(' ');
    wrtStr(UTF8Encode(attr.NodeName));
    wrtChr('=');
    s := attr.NodeValue;
    // !!!: Replace special characters in "s" such as '&', '<', '>'
    wrtChr('"');
    ConvWrite(UTF8Encode(s), AttrSpecialChars, {$IFDEF FPC}@{$ENDIF}AttrSpecialCharCallback);
    wrtChr('"');
  end;
  Child := node.FirstChild;
  if Child = nil then begin
    wrtChr('/');
    wrtChr('>');
    if not FInsideTextNode then wrtLineEnd;
  end else
  begin
    SavedInsideTextNode := FInsideTextNode;
    wrtChr('>');
    if not (FInsideTextNode or Child.InheritsFrom(TDOMText)) then
      wrtLineEnd;
    IncIndent;
    repeat
      if Child.InheritsFrom(TDOMText) then
        FInsideTextNode := True
      else                      // <-- fix case when CDATA is first child
        FInsideTextNode := False;
      WriteNode(Child);
      Child := Child.NextSibling;
    until child = nil;
    DecIndent;
    if not FInsideTextNode then
      wrtIndent;
    FInsideTextNode := SavedInsideTextNode;
    wrtChr('<');
    wrtChr('/');
    wrtStr(UTF8Encode(node.NodeName));
    wrtChr('>');
    if not FInsideTextNode then
      wrtLineEnd;
  end;
end;

procedure TXMLWriter.VisitText(node: TDOMNode);
begin
  ConvWrite(UTF8Encode(node.NodeValue), TextSpecialChars, {$IFDEF FPC}@{$ENDIF}TextnodeSpecialCharCallback);
end;

procedure TXMLWriter.VisitCDATA(node: TDOMNode);
begin
  if not FInsideTextNode then
    wrtStr('<![CDATA[' + UTF8Encode(node.NodeValue) + ']]>')
  else begin
    wrtIndent;
    wrtStr('<![CDATA[' + UTF8Encode(node.NodeValue) + ']]>');
    wrtLineEnd;
  end;
end;

procedure TXMLWriter.VisitEntityRef(node: TDOMNode);
begin
  wrtChr('&');
  wrtStr(UTF8Encode(node.NodeName));
  wrtChr(';');
end;

procedure TXMLWriter.VisitEntity(node: TDOMNode);
begin

end;

procedure TXMLWriter.VisitPI(node: TDOMNode);
begin
  if not FInsideTextNode then wrtIndent;
  wrtChr('<'); wrtChr('?');
  wrtStr(UTF8Encode(TDOMProcessingInstruction(node).Target));
  wrtChr(' ');
  wrtStr(UTF8Encode(TDOMProcessingInstruction(node).Data));
  wrtChr('?'); wrtChr('>');
  if not FInsideTextNode then wrtLineEnd;
end;

procedure TXMLWriter.VisitComment(node: TDOMNode);
begin
  if not FInsideTextNode then wrtIndent;
  wrtStr('<!--');
  wrtStr(UTF8Encode(node.NodeValue));
  wrtStr('-->');
  if not FInsideTextNode then wrtLineEnd;
end;

procedure TXMLWriter.VisitDocument(node: TDOMNode);
var
  child: TDOMNode;
begin
  wrtStr('<?xml version="');
  if Length(TXMLDocument(node).XMLVersion) > 0 then
    ConvWrite(TXMLDocument(node).XMLVersion, AttrSpecialChars, {$IFDEF FPC}@{$ENDIF}AttrSpecialCharCallback)
  else
    wrtStr('1.0');
  wrtChr('"');
  if Length(TXMLDocument(node).Encoding) > 0 then
  begin
    wrtStr(' encoding="');
    ConvWrite(TXMLDocument(node).Encoding, AttrSpecialChars, {$IFDEF FPC}@{$ENDIF}AttrSpecialCharCallback);
    wrtStr('"');
  end;
  wrtStr('?>');
  wrtLineEnd;

  if Length(TXMLDocument(node).StylesheetType) > 0 then
  begin
    wrtStr('<?xml-stylesheet type="');
    ConvWrite(TXMLDocument(node).StylesheetType, AttrSpecialChars, {$IFDEF FPC}@{$ENDIF}AttrSpecialCharCallback);
    wrtStr('" href="');
    ConvWrite(TXMLDocument(node).StylesheetHRef, AttrSpecialChars, {$IFDEF FPC}@{$ENDIF}AttrSpecialCharCallback);
    wrtStr('"?>');
    wrtLineEnd;
  end;

  FIndent := '  ';
  FIndentCount := 0;

  child := node.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;

  if node=nil then ;
end;

procedure TXMLWriter.VisitAttribute(Node: TDOMNode);
begin

end;

procedure TXMLWriter.VisitDocumentType(Node: TDOMNode);
begin

end;

procedure TXMLWriter.VisitFragment(Node: TDOMNode);
begin
  VisitElement(Node);
end;

procedure TXMLWriter.VisitNotation(Node: TDOMNode);
begin

end;


procedure TStreamXMLWriter.WriteXML(Root: TDOMNode; AStream: TStream);
begin
  F:=AStream;
  WriteNode(Root);
end;

procedure TTextXMLWriter.WriteXML(Root: TDOMNode; var AFile: Text);
begin
  f := @AFile;
  WriteNode(Root);
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
  with TTextXMLWriter.Create do
  try
    WriteXML(doc, AFile);
  finally
    Free;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream);
begin
  with TStreamXMLWriter.Create do
  try
    WriteXML(doc, AStream);
  finally
    Free;
  end;
end;

procedure WriteXML(Element: TDOMNode; const AFileName: String);
begin
  WriteXML(TXMLDocument(Element), AFileName);
end;

procedure WriteXML(Element: TDOMNode; var AFile: Text);
begin
  WriteXML(TXMLDocument(Element), AFile);
end;

procedure WriteXML(Element: TDOMNode; AStream: TStream);
begin
  WriteXML(TXMLDocument(Element), AStream);
end;



end.
