{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 Sebastian Guenther, sguenther@gmx.de

    XML writing routines
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit xmlwrite;

interface

uses classes, DOM;

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
procedure WriteXMLFile(doc: TXMLDocument; var AStream: TStream);


// =======================================================

implementation


// -------------------------------------------------------
//   Writers for the different node types
// -------------------------------------------------------

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
    (WriteElement, WriteAttribute, WriteText, WriteCDATA, WriteEntityRef,
     WriteEntity, WritePI, WriteComment, WriteDocument, WriteDocumentType,
     WriteDocumentFragment, WriteNotation);

procedure WriteNode(node: TDOMNode);
begin
  WriteProcs[node.NodeType](node);
end;


// -------------------------------------------------------
//   Text file and TStream support
// -------------------------------------------------------

type
  TOutputProc = procedure(s: String);

var
  f: ^Text;
  stream: TStream;
  wrt, wrtln: TOutputProc;


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
  stream.Write(s[1], Length(s));
end;

procedure Stream_WriteLn(s: String);
begin
  stream.Write(s[1], Length(s));
  stream.WriteByte(10);
end;


// -------------------------------------------------------
//   Indent handling
// -------------------------------------------------------

var

  indent: String;


procedure IncIndent;
begin
  indent := indent + '  ';
end;

procedure DecIndent;
begin
  indent := Copy(indent, 1, Length(indent) - 2);
end;


// -------------------------------------------------------
//   Node writers implementations
// -------------------------------------------------------


procedure WriteElement(node: TDOMNode);
var
  i: Integer;
  attr, child: TDOMNode;
begin
  wrt(Indent + '<' + node.NodeName);
  for i := 0 to node.Attributes.Length - 1 do begin
    attr := node.Attributes.Item[i];
    wrt(' ' + attr.NodeName + '="' + attr.NodeValue + '"');
  end;
  child := node.FirstChild;
  if child = nil then
    wrtln('/>')
  else begin
    wrtln('>');
    IncIndent;
    repeat
      WriteNode(child);
      child := child.NextSibling;
    until child = nil;
    DecIndent;
    wrtln(Indent + '</' + node.NodeName + '>');
  end;
end;

procedure WriteAttribute(node: TDOMNode);
begin
  WriteLn('WriteAttribute');
end;

procedure WriteText(node: TDOMNode);
begin
  wrt(node.NodeValue);
end;

procedure WriteCDATA(node: TDOMNode);
begin
  wrtln('<![CDATA[' + node.NodeValue + ']]>');
end;

procedure WriteEntityRef(node: TDOMNode);
begin
  wrt('&' + node.NodeValue + ';');
end;

procedure WriteEntity(node: TDOMNode);
begin
  WriteLn('WriteEntity');
end;

procedure WritePI(node: TDOMNode);
begin
  WriteLn('WritePI');
end;

procedure WriteComment(node: TDOMNode);
begin
  Write('<!--', node.NodeValue, '-->');
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


procedure RootWriter(doc: TXMLDocument);
var
  child: TDOMNode;
begin
  wrt('<?xml version="');
  if doc.XMLVersion <> '' then wrt(doc.XMLVersion)
  else wrt('1.0');
  wrt('"');
  if doc.Encoding <> '' then wrt(' encoding="' + doc.Encoding + '"');
  wrtln('?>');

  indent := '';

  child := doc.FirstChild;
  while child <> nil do begin
    WriteNode(child);
    child := child.NextSibling;
  end;
end;


// -------------------------------------------------------
//   Interface implementation
// -------------------------------------------------------

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
begin
  f := @AFile;
  wrt := @Text_Write;
  wrtln := @Text_WriteLn;
  RootWriter(doc);
end;

procedure WriteXMLFile(doc: TXMLDocument; var AStream: TStream);
begin
  stream := AStream;
  wrt := @Stream_Write;
  wrtln := @Stream_WriteLn;
  RootWriter(doc);
end;

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmCreate);
  WriteXMLFile(doc, stream);
  stream.Free;
end;


end.


{
  $Log$
  Revision 1.5  2000-01-06 01:20:37  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:12  peter
    * moved to packages dir

  Revision 1.3  1999/07/22 15:06:35  michael
  * Fix for stream_write from Sebastian Guenther

  Revision 1.2  1999/07/09 21:05:53  michael
  + fixes from Guenther Sebastian

  Revision 1.1  1999/07/09 08:35:09  michael
  + Initial implementation by Sebastian Guenther

}
