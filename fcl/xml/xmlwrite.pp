{
    $Id$
    This file is part of the Free Component Library

    XML writing routines
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org

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

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
procedure WriteXMLFile(doc: TXMLDocument; var AStream: TStream);

procedure WriteXML(Element: TDOMNode; const AFileName: String);
procedure WriteXML(Element: TDOMNode; var AFile: Text);
procedure WriteXML(Element: TDOMNode; var AStream: TStream);


// ===================================================================

implementation

uses SysUtils;

// -------------------------------------------------------------------
//   Writers for the different node types
// -------------------------------------------------------------------

procedure WriteElement(node: TDOMNode); forward;
procedure WriteAttribute(node: TDOMNode); forward;
procedure WriteText(node: TDOMNode); forward;
procedure WriteCDATA(node: TDOMNode); forward;
procedure WriteEntityRef(node: TDOMNode); forward;
procedure WriteEntity(node: TDOMNode); forward;
procedure WritePI(node: TDOMNode); forward;
procedure WriteComment(node: TDOMNode); forward;
procedure WriteDocument(node: TDOMNode); forward;
procedure WriteDocumentType(node: TDOMNode); forward;
procedure WriteDocumentFragment(node: TDOMNode); forward;
procedure WriteNotation(node: TDOMNode); forward;


type
  TWriteNodeProc = procedure(node: TDOMNode);

const
  WriteProcs: array[ELEMENT_NODE..NOTATION_NODE] of TWriteNodeProc =
    (@WriteElement, @WriteAttribute, @WriteText, @WriteCDATA, @WriteEntityRef,
     @WriteEntity, @WritePI, @WriteComment, @WriteDocument, @WriteDocumentType,
     @WriteDocumentFragment, @WriteNotation);

procedure WriteNode(node: TDOMNode);
begin
  WriteProcs[node.NodeType](node);
end;


// -------------------------------------------------------------------
//   Text file and TStream support
// -------------------------------------------------------------------

type
  TOutputProc = procedure(s: String);

var
  f: ^Text;
  stream: TStream;
  wrt, wrtln: TOutputProc;
  InsideTextNode: Boolean;


procedure Text_Write(s: String);
begin
  Write(f^, s);
end;

procedure Text_WriteLn(s: String);
begin
  WriteLn(f^, s);
end;

procedure Stream_Write(s: String);
begin
  if Length(s) > 0 then
    stream.Write(s[1], Length(s));
end;

procedure Stream_WriteLn(s: String);
begin
  if Length(s) > 0 then
    stream.Write(s[1], Length(s));
  stream.WriteByte(10);
end;


// -------------------------------------------------------------------
//   Indent handling
// -------------------------------------------------------------------

var
  Indent: String;


procedure IncIndent;
begin
  Indent := Indent + '  ';
end;

procedure DecIndent;
begin
  if Length(Indent) >= 2 then
    SetLength(Indent, Length(Indent) - 2);
end;


// -------------------------------------------------------------------
//   String conversion
// -------------------------------------------------------------------

type
  TCharacters = set of Char;
  TSpecialCharCallback = procedure(c: Char);

const
  AttrSpecialChars = ['"', '&'];
  TextSpecialChars = ['<', '>', '&'];


procedure ConvWrite(const s: String; const SpecialChars: TCharacters;
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
      wrt(Copy(s, StartPos, EndPos - StartPos));
      SpecialCharCallback(s[EndPos]);
      StartPos := EndPos + 1;
    end;
    Inc(EndPos);
  end;
  if EndPos > StartPos then
    wrt(Copy(s, StartPos, EndPos - StartPos));
end;

procedure AttrSpecialCharCallback(c: Char);
begin
  if c = '"' then
    wrt('&quot;')
  else if c = '&' then
    wrt('&amp;')
  else
    wrt(c);
end;

procedure TextnodeSpecialCharCallback(c: Char);
begin
  if c = '<' then
    wrt('&lt;')
  else if c = '>' then
    wrt('&gt;')
  else if c = '&' then
    wrt('&amp;')
  else
    wrt(c);
end;


// -------------------------------------------------------------------
//   Node writers implementations
// -------------------------------------------------------------------

procedure WriteElement(node: TDOMNode);
var
  i: Integer;
  attr, child: TDOMNode;
  SavedInsideTextNode: Boolean;
  s: String;
begin
  if not InsideTextNode then
    wrt(Indent);
  wrt('<' + node.NodeName);
  for i := 0 to node.Attributes.Length - 1 do
  begin
    attr := node.Attributes.Item[i];
    wrt(' ' + attr.NodeName + '=');
    s := attr.NodeValue;
    // !!!: Replace special characters in "s" such as '&', '<', '>'
    wrt('"');
    ConvWrite(s, AttrSpecialChars, @AttrSpecialCharCallback);
    wrt('"');
  end;
  Child := node.FirstChild;
  if Child = nil then
    if InsideTextNode then
      wrt('/>')
    else
      wrtln('/>')
  else
  begin
    SavedInsideTextNode := InsideTextNode;
    if InsideTextNode or Child.InheritsFrom(TDOMText) then
      wrt('>')
    else
      wrtln('>');
    IncIndent;
    repeat
      if Child.InheritsFrom(TDOMText) then
        InsideTextNode := True;
      WriteNode(Child);
      Child := Child.NextSibling;
    until child = nil;
    DecIndent;
    if not InsideTextNode then
      wrt(Indent);
    InsideTextNode := SavedInsideTextNode;
    s := '</' + node.NodeName + '>';
    if InsideTextNode then
      wrt(s)
    else
      wrtln(s);
  end;
end;

procedure WriteAttribute(node: TDOMNode);
begin
  WriteLn('WriteAttribute');
end;

procedure WriteText(node: TDOMNode);
begin
  ConvWrite(node.NodeValue, TextSpecialChars, @TextnodeSpecialCharCallback);
end;

procedure WriteCDATA(node: TDOMNode);
begin
  if InsideTextNode then
    wrt('<![CDATA[' + node.NodeValue + ']]>')
  else
    wrtln(Indent + '<![CDATA[' + node.NodeValue + ']]>')
end;

procedure WriteEntityRef(node: TDOMNode);
begin
  wrt('&' + node.NodeName + ';');
end;

procedure WriteEntity(node: TDOMNode);
begin
  WriteLn('WriteEntity');
end;

procedure WritePI(node: TDOMNode);
var
  s: String;
begin
  s := '<!' + TDOMProcessingInstruction(node).Target + ' ' +
    TDOMProcessingInstruction(node).Data + '>';
  if InsideTextNode then
    wrt(s)
  else
    wrtln(Indent + s);
end;

procedure WriteComment(node: TDOMNode);
begin
  if InsideTextNode then
    wrt('<!--' + node.NodeValue + '-->')
  else
    wrtln(Indent + '<!--' + node.NodeValue + '-->')
end;

procedure WriteDocument(node: TDOMNode);
begin
  WriteLn('WriteDocument');
end;

procedure WriteDocumentType(node: TDOMNode);
begin
  WriteLn('WriteDocumentType');
end;

procedure WriteDocumentFragment(node: TDOMNode);
begin
  WriteLn('WriteDocumentFragment');
end;

procedure WriteNotation(node: TDOMNode);
begin
  WriteLn('WriteNotation');
end;


procedure InitWriter;
begin
  InsideTextNode := False;
end;

procedure RootWriter(doc: TXMLDocument);
var
  Child: TDOMNode;
begin
  InitWriter;
  wrt('<?xml version="');
  if Length(doc.XMLVersion) > 0 then
    wrt(doc.XMLVersion)
  else
    wrt('1.0');
  wrt('"');
  if Length(doc.Encoding) > 0 then
    wrt(' encoding="' + doc.Encoding + '"');
  wrtln('?>');

  if Length(doc.StylesheetType) > 0 then
    // !!!: Can't handle with HRefs which contain special chars (" and so on)
    wrtln(Format('<?xml-stylesheet type="%s" href="%s"?>',
      [doc.StylesheetType, doc.StylesheetHRef]));

  indent := '';

  child := doc.FirstChild;
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
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  wrt := @Stream_Write;
  wrtln := @Stream_WriteLn;
  RootWriter(doc);
  Stream.Free;
end;

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
begin
  f := @AFile;
  wrt := @Text_Write;
  wrtln := @Text_WriteLn;
  RootWriter(doc);
end;

procedure WriteXMLFile(doc: TXMLDocument; var AStream: TStream);
begin
  Stream := AStream;
  wrt := @Stream_Write;
  wrtln := @Stream_WriteLn;
  RootWriter(doc);
end;


procedure WriteXML(Element: TDOMNode; const AFileName: String);
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  wrt := @Stream_Write;
  wrtln := @Stream_WriteLn;
  InitWriter;
  WriteNode(Element);
  Stream.Free;
end;

procedure WriteXML(Element: TDOMNode; var AFile: Text);
begin
  f := @AFile;
  wrt := @Text_Write;
  wrtln := @Text_WriteLn;
  InitWriter;
  WriteNode(Element);
end;

procedure WriteXML(Element: TDOMNode; var AStream: TStream);
begin
  stream := AStream;
  wrt := @Stream_Write;
  wrtln := @Stream_WriteLn;
  InitWriter;
  WriteNode(Element);
end;


end.


{
  $Log$
  Revision 1.8  2002-09-20 11:04:21  michael
  + Changed writexml type to TDomNode instead of TDomeElement

  Revision 1.7  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

}
