
{$MODE objfpc}
{$H+}

program xmldump;
uses sysutils, DOM, xmlread;
const
  NodeNames: array[ELEMENT_NODE..NOTATION_NODE] of String = (
    'Element',
    'Attribute',
    'Text',
    'CDATA section',
    'Entity reference',
    'Entity',
    'Processing instruction',
    'Comment',
    'Document',
    'Document type',
    'Document fragment',
    'Notation'
  );

procedure DumpNode(node: TDOMNode; spc: String);
var
  i: Integer;
  attr: TDOMNode;
begin
  Write(spc, NodeNames[node.NodeType]);
  if Copy(node.NodeName, 1, 1) <> '#' then
    Write(' "', node.NodeName, '"');
  if node.NodeValue <> '' then
    Write(' "', node.NodeValue, '"');

  if (node.Attributes <> nil) and (node.Attributes.Length > 0) then begin
    Write(',');
    for i := 0 to node.Attributes.Length - 1 do begin
      attr := node.Attributes.Item[i];
      Write(' ', attr.NodeName, ' = "', attr.NodeValue, '"');
    end;
  end;
  WriteLn;

  if node.FirstChild <> nil then
    DumpNode(node.FirstChild, spc + '  ');
  if node.NextSibling <> nil then
    DumpNode(node.NextSibling, spc);
end;

var
  xml: TXMLDocument;
begin
  if ParamCount <> 1 then begin
    WriteLn('xmldump <xml or dtd file>');
    exit;
  end;

  if UpCase(ExtractFileExt(ParamStr(1))) = '.DTD' then
    ReadDTDFile(xml,ParamStr(1))
  else
    ReadXMLFile(xml,ParamStr(1));

  WriteLn('Successfully parsed the document. Structure:');
  WriteLn;
  if Assigned(xml.DocType) then
  begin
    WriteLn('DocType: "', xml.DocType.Name, '"');
    WriteLn;
  end;
  DumpNode(xml, '| ');
  xml.Free;
end.
