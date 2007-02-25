
{
  This program takes an W3 IDL XML file with interface definitions and
  dumps a interface definition.
}

{$MODE objfpc}
{$H+}

program htdump;

uses sysutils, DOM, xmlread;

Var DoImplementation : Boolean;

procedure DumpNode(node: TDOMNode; spc: String);forward;

procedure DumpMethodNode(node: TDOMElement; spc: String);

var N,NN : TDOMNode;
    rettype : DOMString;
    firstparam : boolean;
    i : longint;

begin

   N:=Node.FindNode('returns');
   If N<>Nil then
      rettype:=TDomElement(N).GetAttribute('type');
   If Not DoImplementation then
     Write (spc);
   If RetType='void' then
     Write ('Procedure ')
   else
     Write ('Function ');
   If DoImplementation then
     Write(TDomElement(Node.ParentNode).GetAttribute('name'),'.');
   Write (Node.GetAttribute('name'));
   N:=Node.FindNode('params');
   If N<>Nil then
     begin
     FirstParam:=True;
     for I:=1 to N.ChildNodes.Count-1 do
       begin
       NN:=N.ChildNodes.Item[i];
       If NN.NodeName<>'param' then
         begin
         If Firstparam then
           begin
           Write('(');
           FirstParam:=False
           end
         else
           Write(';');
         writeln (spc,NN.NodeName,' : ',TDOMElement(NN).GetAttribute('type'));
         end;
       end;
     If Not FirstParam then
     Write (')');
     end;
   If RetType <>'void' then
     Write (' : ',Rettype);
   Writeln(';');
   If DoImplementation then
     begin
     Writeln;
     Writeln('Begin');
     Writeln('End;');
     Writeln;
     Writeln;
     end;
end;

procedure DumpAttributeNode(Doprivate: Boolean;node: TDOMElement; spc: String);

Var PropName : DOMString;

begin
  PropName:=Node.GetAttribute('name');
  If DOPrivate then
    Write (spc,'F')
  else
    Write (spc,'Property ');
  Write (PropName,' : ',Node.getAttribute('type'));
  If Not DoPrivate then
    begin
    Write (' Read F',PropName);
    If not(Node.getAttribute('readonly')='yes') then
      Write (' Write F',PropName)
    end;
  Writeln(';');
end;

Procedure DumpInterfaceNode (node : TDomElement; spc : String);

Var N : TDOMNode;
    C : TDOMNodeList;
    I : longint;

begin
  If not DoImplementation then
    begin
    Write(spc,Node.GetAttribute('name'),' = Class');
    N:=Node.Attributes.GetNamedItem('inherits');
    If N<>Nil then
      Write('(',N.NodeValue,')');
    Writeln;
    // Dump Property fields
    Writeln (Spc+'  Private');
    N:=Node.FirstChild;
    While N<>Nil do
      begin
      If N.NodeName='attribute' then
          DumpAttributeNode(True,TDOMElement(N), spc + '    ');
      N:=N.NextSibling;
      end;
    Writeln (Spc,'  Public');
    end;
  N:=Node.FirstChild;
  While N<>Nil do
    begin
    If N.NodeName='method' then
       DumpMethodNode(TDomElement(N), spc + '    ');
    N:=N.NextSibling;
    end;
  If Not DoImplementation then
    begin
    N:=Node.FirstChild;
    While N<>Nil do
      begin
      If N.NodeName='attribute' then
         DumpAttributeNode(False,TDomElement(N), spc + '    ');
      N:=N.NextSibling;
      end;
    writeln (spc,'End;')
    end;
end;

procedure DumpNode(node: TDOMNode; spc: String);

var
  i: Integer;
  attr: TDOMNode;
begin
  If Node.NodeName='interface' then
    DumpInterfaceNode(TDOMElement(Node),Spc)
  else if Node.NodeName='method' then
    DumpMethodNode(TDOMELEMENt(Node),Spc)
  else if Node.NodeName='attribute' then
    DumpAttributeNode(True,TDomelement(node),spc)
  else
    if node.FirstChild <> nil then
      DumpNode(node.FirstChild, spc + '  ');
  if node.NextSibling <> nil then
    DumpNode(node.NextSibling, spc);
end;

var
  i : longint;
  xml: TXMLDocument;

begin
  if (ParamCount <1) or (paramcount>2) then begin
    WriteLn('htdump -m <xml>');
    exit;
  end;
  I:=1;
  If paramstr(1)='-m' then
    begin
    I:=2;
    DoImplementation:=True;
    end;
  ReadXMLFile(xml, ParamStr(i));
  WriteLn ('// Created From file ',paramstr(I));
  DumpNode(xml, '');
end.
