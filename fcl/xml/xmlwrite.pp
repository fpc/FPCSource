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

unit xmlwrite;

interface

uses DOM;

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);


implementation

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
  TWriteProc = procedure(node: TDOMNode);
const
  WriteProcs: array[ELEMENT_NODE..NOTATION_NODE] of TWriteProc =
    (WriteElement, WriteAttribute, WriteText, WriteCDATA, WriteEntityRef,
     WriteEntity, WritePI, WriteComment, WriteDocument, WriteDocumentType,
     WriteDocumentFragment, WriteNotation);

procedure WriteNode(node: TDOMNode);
begin
  WriteProcs[node.NodeType](node);
end;


var
  f: ^Text;
  indent: String;


procedure IncIndent;
begin
  indent := indent + '  ';
end;

procedure DecIndent;
begin
  indent := Copy(indent, 1, Length(indent) - 2);
end;

procedure WriteElement(node: TDOMNode);
var
  i: Integer;
  attr, child: TDOMNode;
begin
  Write(f^, Indent, '<', node.NodeName);
  for i := 0 to node.Attributes.Length - 1 do begin
    attr := node.Attributes.Item[i];
    Write(f^, ' ', attr.NodeName, '="', attr.NodeValue, '"');
  end;
  child := node.FirstChild;
  if child = nil then
    WriteLn(f^, '/>')
  else begin
    WriteLn(f^, '>');
    IncIndent;
    repeat
      WriteNode(child);
      child := child.NextSibling;
    until child = nil;
    DecIndent;
    WriteLn(f^, Indent, '</', node.NodeName, '>');
  end;
end;

procedure WriteAttribute(node: TDOMNode);
begin
  WriteLn('WriteAttribute');
end;

procedure WriteText(node: TDOMNode);
begin
  WriteLn('WriteText');
end;

procedure WriteCDATA(node: TDOMNode);
begin
  WriteLn('WriteCDATA');
end;

procedure WriteEntityRef(node: TDOMNode);
begin
  WriteLn('WriteEntityRef');
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
  WriteLn('WriteComment');
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


procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
var
  child: TDOMNode;
begin
  f := @AFile;
  Write(f^, '<?xml version="');
  if doc.XMLVersion <> '' then Write(f^, doc.XMLVersion)
  else Write(f^, '1.0');
  Write(f^, '"');
  if doc.Encoding <> '' then Write(f^, ' encoding="', doc.Encoding, '"');
  WriteLn(f^, '?>');

  indent := '';

  child := doc.FirstChild;
  while child <> nil do begin
    WriteNode(child);
    child := child.NextSibling;
  end;
end;


end.


{
  $Log$
  Revision 1.1  1999-07-09 08:35:09  michael
  + Initial implementation by Sebastian Guenther

}
